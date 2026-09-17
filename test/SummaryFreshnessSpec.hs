{-# LANGUAGE OverloadedStrings #-}

module SummaryFreshnessSpec (summaryFreshnessTests) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, fromException, try)
import Control.Monad (forM_, void)
import Data.Aeson (Object, Value (..), object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid (DroidError (DroidInvalidEvent))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Configuration qualified as Configuration
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Mission (SubagentInvocationSummary (..), SubagentStatus (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcInvalidParams))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport (ObjectTransport, objectTransport)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

summaryFreshnessTests :: TestTree
summaryFreshnessTests =
  testGroup
    "Child summary freshness"
    [ testCase "late parent snapshots preserve each newer child without rewriting the receipt" $ bounded $ do
        forM_ [(SubagentCompleted, 9, SubagentRunning, 1), (SubagentRunning, 1, SubagentCompleted, 9)] $ \(status, count, oldStatus, oldCount) -> withOwner $ \connection peer -> do
          let fresh = summary "child" status count
              old = summary "child" oldStatus oldCount
              unrelated = summary "other" SubagentCompleted 3
          withAsync (Daemon.loadSessionInfo connection "parent") $ \pending -> do
            frame <- nextLoad peer
            Daemon.setSubagentInvocationSummary connection fresh
            respond peer frame (loadValue "parent" [old, unrelated])
            info <- wait pending
            Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh)
            Daemon.getSubagentInvocationSummary connection "other" >>= (@?= Just unrelated)
            fmap loadedSubagentInvocations (Daemon.daemonLoadedState info) @?= Just (Just [old, unrelated]),
      testCase "a write before capture may be replaced while an equal-valued write after capture wins" $ bounded $ withOwner $ \connection peer -> do
        let fresh = summary "child" SubagentCompleted 9
            old = summary "child" SubagentRunning 1
        Daemon.setSubagentInvocationSummary connection fresh
        withAsync (Daemon.loadSessionInfo connection "parent") $ \pending -> do
          frame <- nextLoad peer
          respond peer frame (loadValue "parent" [old])
          void (wait pending)
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just old)
        Daemon.setSubagentInvocationSummary connection fresh
        withAsync (Daemon.loadSessionInfo connection "parent") $ \pending -> do
          frame <- nextLoad peer
          Daemon.setSubagentInvocationSummary connection fresh
          respond peer frame (loadValue "parent" [old])
          void (wait pending)
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh),
      testCase "overlapping parents use per-child freshness rather than only their own load epochs" $ bounded $ withOwner $ \connection peer ->
        withAsync (Daemon.loadSessionInfo connection "first") $ \first -> do
          firstFrame <- nextLoad peer
          withAsync (Daemon.loadSessionInfo connection "second") $ \second -> do
            secondFrame <- nextLoad peer
            let fresh = summary "child" SubagentCompleted 9
                old = summary "child" SubagentRunning 1
                other = summary "other" SubagentRunning 2
            respond peer firstFrame (loadValue "first" [fresh])
            void (wait first)
            respond peer secondFrame (loadValue "second" [old, other])
            void (wait second)
            Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh)
            Daemon.getSubagentInvocationSummary connection "other" >>= (@?= Just other),
      testCase "guarded duplicate rows keep the first accepted value and unguarded rows keep the last" $ bounded $ withOwner $ \connection peer -> do
        let first = summary "child" SubagentRunning 1
            lastRow = summary "child" SubagentCompleted 9
        withAsync (Daemon.loadSessionInfo connection "parent") $ \pending -> do
          frame <- nextLoad peer
          respond peer frame (loadValue "parent" [first, lastRow])
          void (wait pending)
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just first)
        Daemon.hydrateSubagentInvocationSummaries connection [first, lastRow]
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just lastRow),
      testCase "explicit captured hydration admits untouched children and rejects only changed children" $ bounded $ withOwner $ \connection peer -> do
        revision <- capture connection
        let fresh = summary "child" SubagentCompleted 9
            old = summary "child" SubagentRunning 1
            other = summary "other" SubagentRunning 3
        Daemon.setSubagentInvocationSummary connection fresh
        hydrateAt connection revision [old, other]
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh)
        Daemon.getSubagentInvocationSummary connection "other" >>= (@?= Just other)
        atomically (tryReadTQueue (peerRequests peer)) >>= (@?= Nothing)
        show revision @?= "SubagentSummaryRevision <redacted>",
      testCase "invalid identities neither publish nor consume a captured revision" $ bounded $ withOwner $ \connection peer -> do
        before <- capture connection
        let invalid = summary " \xfeff" SubagentRunning 1
        try @Daemon.DaemonError (Daemon.setSubagentInvocationSummary connection invalid) >>= (@?= Left Daemon.InvalidChildSessionIdentity)
        hydrateAt connection before [invalid]
        capture connection >>= (@?= before)
        let valid = summary " child " SubagentCompleted 4
        hydrateAt connection before [invalid, valid]
        Daemon.getSubagentInvocationSummary connection " child " >>= (@?= Just valid)
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Nothing)
        atomically (tryReadTQueue (peerRequests peer)) >>= (@?= Nothing),
      testCase "availability reactivation protects the live summary from an older parent snapshot" $ bounded $ withOwner $ \connection peer -> do
        let prior = summary "child" SubagentCompleted 9
        Daemon.setSubagentInvocationSummary connection prior
        withAsync (Daemon.loadSessionInfo connection "parent") $ \pending -> do
          frame <- nextLoad peer
          notify peer "parent" (object ["type" .= String "child_session_available", "childSessionId" .= String "child", "timestamp" .= (0 :: Int), "subagentType" .= String "worker", "description" .= String "fixture"])
          settle connection
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just (prior {invocationStatus = SubagentRunning}))
          respond peer frame (loadValue "parent" [prior {invocationToolUseCount = Just 1}])
          void (wait pending)
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just (prior {invocationStatus = SubagentRunning})),
      testCase "tagged completion refreshes advance freshness even when recomputed values are equal" $ bounded $ do
        forM_ [1, 9] $ \count -> withOwner $ \connection peer -> do
          withAsync (Daemon.loadSessionInfo connection "child") $ \child -> do
            frame <- nextLoad peer
            respond peer frame childLoadValue
            void (wait child)
          let prior = summary "child" SubagentRunning count
              refreshed = prior {invocationToolUseCount = Just 9}
          Daemon.setSubagentInvocationSummary connection prior
          withAsync (Daemon.loadSessionInfo connection "parent") $ \parent -> do
            frame <- nextLoad peer
            notify peer "child" completedTurn
            settle connection
            Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just refreshed)
            respond peer frame (loadValue "parent" [summary "child" SubagentCompleted 2])
            void (wait parent)
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just refreshed),
      testCase "cache retirement retains summary freshness independently of cached payloads" $ bounded $ withOwner $ \connection _ -> do
        revision <- capture connection
        let fresh = summary "child" SubagentCompleted 9
        void (Daemon.registerSessionState connection "child" "machine")
        Daemon.setSubagentInvocationSummary connection fresh
        Daemon.removeCachedSession connection "child" >>= (@?= True)
        hydrateAt connection revision [summary "child" SubagentRunning 1]
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh),
      testCase "captures remain logical across physical generations and offline updates win" $ bounded $ Daemon.withDaemonState $ \shared -> do
        revision <- Daemon.captureSubagentInvocationSummaryRevision shared
        first <- newPeer
        connectWith shared first (const (pure ()))
        let accepted = summary "untouched" SubagentCompleted 2
            fresh = summary "changed" SubagentCompleted 9
        Daemon.setDaemonStateSubagentInvocationSummary shared fresh
        Daemon.hydrateSubagentInvocationSummariesWithRevision shared revision [accepted, summary "changed" SubagentRunning 1]
        storedSummary shared "untouched" >>= (@?= Just accepted)
        second <- newPeer
        connectWith shared second $ \connection -> do
          Daemon.getSubagentInvocationSummary connection "untouched" >>= (@?= Just accepted)
          Daemon.getSubagentInvocationSummary connection "changed" >>= (@?= Just fresh),
      testCase "a reset logical owner does not accept a previous owner's capture" $ bounded $ do
        foreignRevision <- withOwner (\connection _ -> capture connection)
        withOwner $ \connection _ -> do
          let incoming = summary "child" SubagentRunning 1
          hydrateAt connection foreignRevision [incoming]
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Nothing)
          own <- capture connection
          hydrateAt connection own [incoming]
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just incoming),
      testCase "failed malformed and cancelled loads do not hydrate summaries or consume freshness" $ bounded $ withOwner $ \connection peer -> do
        let fresh = summary "child" SubagentCompleted 9
            old = summary "child" SubagentRunning 1
        Daemon.setSubagentInvocationSummary connection fresh
        before <- capture connection
        withAsync (try @RpcResultError (void (Daemon.loadSessionInfo connection "rejected"))) $ \pending -> do
          frame <- nextLoad peer
          rejectLoad peer frame
          wait pending >>= \case Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Remote rejection lost"
        withAsync (try @DroidError (void (Daemon.loadSessionInfo connection "malformed"))) $ \pending -> do
          frame <- nextLoad peer
          respond peer frame (object ["sessionId" .= String "malformed", "subagentInvocations" .= [old], "settings" .= Null])
          wait pending >>= (@?= Left DroidInvalidEvent)
        withAsync (Daemon.loadSessionInfo connection "cancelled") $ \pending -> do
          frame <- nextLoad peer
          cancel pending
          waitCatch pending >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled load returned"
          respond peer frame (loadValue "cancelled" [old])
          settle connection
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh)
        capture connection >>= (@?= before),
      testCase "parent generation fencing still protects the rest of a superseded receipt" $ bounded $ withOwner $ \connection peer ->
        withAsync (Daemon.loadSessionInfo connection "parent") $ \old -> do
          oldFrame <- nextLoad peer
          withAsync (Daemon.loadSessionInfo connection "parent") $ \new -> do
            newFrame <- nextLoad peer
            let fresh = summary "child" SubagentCompleted 9
            respond peer newFrame (withCwd "/fresh" (loadValue "parent" [fresh]))
            void (wait new)
            respond peer oldFrame (withCwd "/stale" (loadValue "parent" [summary "child" SubagentRunning 1]))
            waitCatch old >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Superseded load returned"
            Daemon.getWorkingDirectory connection "parent" >>= (@?= Just "/fresh")
            Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh),
      testCase "new-session initialization uses the same captured-summary boundary" $ bounded $ withOwner $ \connection peer -> do
        let base = Daemon.defaultDaemonSessionOptions "/offline"
            parameters = Daemon.daemonSessionParameters base
            configuration = (Configuration.initializeConfiguration parameters) {Configuration.configurationSessionId = Just "created"}
            sessionOptions = base {Daemon.daemonSessionParameters = parameters {Configuration.initializeConfiguration = configuration}}
            fresh = summary "child" SubagentCompleted 9
        withAsync (Daemon.withSessionOn connection sessionOptions (const (pure ()))) $ \pending -> do
          frame <- nextLoad peer
          field "method" frame @?= String "daemon.initialize_session"
          Daemon.setSubagentInvocationSummary connection fresh
          respond peer frame (loadValue "created" [summary "child" SubagentRunning 1])
          wait pending
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just fresh),
      testCase "logical summary administration needs no physical connection" $ bounded $ Daemon.withDaemonState $ \shared -> do
        let fresh = summary "child" SubagentCompleted 9
            old = summary "child" SubagentRunning 1
        revision <- Daemon.captureSubagentInvocationSummaryRevision shared
        Daemon.setDaemonStateSubagentInvocationSummary shared fresh
        Daemon.hydrateSubagentInvocationSummariesWithRevision shared revision [old]
        storedSummary shared "child" >>= (@?= Just fresh)
        Daemon.hydrateDaemonStateSubagentInvocationSummaries shared [fresh, old]
        storedSummary shared "child" >>= (@?= Just old)
        before <- Daemon.captureSubagentInvocationSummaryRevision shared
        try @Daemon.DaemonError (Daemon.setDaemonStateSubagentInvocationSummary shared (summary "" SubagentRunning 1)) >>= (@?= Left Daemon.InvalidChildSessionIdentity)
        Daemon.hydrateDaemonStateSubagentInvocationSummaries shared [summary "" SubagentRunning 1]
        Daemon.captureSubagentInvocationSummaryRevision shared >>= (@?= before),
      testCase "logical revision operations close with their owner and legacy setters with their handle" $ bounded $ do
        (closed, revision, peer) <- withOwner $ \connection peer -> do
          revision <- capture connection
          pure (connection, revision, peer)
        let shared = Daemon.connectionState closed
            incoming = summary "child" SubagentRunning 1
        try @Daemon.DaemonError (Daemon.captureSubagentInvocationSummaryRevision shared) >>= (@?= Left Daemon.DaemonStateClosed)
        try @Daemon.DaemonError (Daemon.hydrateSubagentInvocationSummariesWithRevision shared revision [incoming]) >>= (@?= Left Daemon.DaemonStateClosed)
        try @Daemon.DaemonError (Daemon.setDaemonStateSubagentInvocationSummary shared incoming) >>= (@?= Left Daemon.DaemonStateClosed)
        try @Daemon.DaemonError (Daemon.hydrateDaemonStateSubagentInvocationSummaries shared [incoming]) >>= (@?= Left Daemon.DaemonStateClosed)
        try @RpcChannelError (Daemon.setSubagentInvocationSummary closed incoming) >>= (@?= Left RpcChannelClosed)
        atomically (tryReadTQueue (peerRequests peer)) >>= (@?= Nothing)
    ]

capture :: Daemon.DaemonConnection -> IO Daemon.SubagentSummaryRevision
capture = Daemon.captureSubagentInvocationSummaryRevision . Daemon.connectionState

hydrateAt :: Daemon.DaemonConnection -> Daemon.SubagentSummaryRevision -> [SubagentInvocationSummary] -> IO ()
hydrateAt connection = Daemon.hydrateSubagentInvocationSummariesWithRevision (Daemon.connectionState connection)

storedSummary :: Daemon.DaemonState -> Text -> IO (Maybe SubagentInvocationSummary)
storedSummary shared identifier = do
  snapshot <- Daemon.getDaemonStateSnapshot shared
  pure (Map.lookup identifier (Daemon.daemonStateSessions snapshot) >>= State.sessionInvocationSummary)

summary :: Text -> SubagentStatus -> Scientific -> SubagentInvocationSummary
summary identifier status count = SubagentInvocationSummary identifier status "worker" "fixture" (Just count) Nothing (KeyMap.singleton "future" (Bool False))

data Peer = Peer
  { peerIncoming :: TQueue Object,
    peerLoads :: TQueue Object,
    peerRequests :: TQueue Object
  }

newPeer :: IO Peer
newPeer = Peer <$> newTQueueIO <*> newTQueueIO <*> newTQueueIO

withOwner :: (Daemon.DaemonConnection -> Peer -> IO a) -> IO a
withOwner action = do
  peer <- newPeer
  Daemon.withConnectionOn options (transport peer) (`action` peer)

connectWith :: Daemon.DaemonState -> Peer -> (Daemon.DaemonConnection -> IO a) -> IO a
connectWith shared peer = Daemon.withConnectionStateOn shared options (transport peer)

options :: Daemon.DaemonClientOptions
options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline") {Daemon.daemonClientHydrateChildSessions = False, Daemon.daemonClientRestoreTerminalsOnLoad = False}

transport :: Peer -> ObjectTransport
transport peer = objectTransport send (atomically (readTQueue (peerIncoming peer)))
  where
    send frame = do
      atomically (writeTQueue (peerRequests peer) frame)
      case field "method" frame of
        String "daemon.load_session" -> atomically (writeTQueue (peerLoads peer) frame)
        String "daemon.initialize_session" -> atomically (writeTQueue (peerLoads peer) frame)
        String "daemon.get_proxy_token" -> respond peer frame (object ["token" .= String "OFFLINE_ONLY"])
        _ -> assertFailure "Unexpected freshness fixture request"

nextLoad :: Peer -> IO Object
nextLoad = atomically . readTQueue . peerLoads

respond :: Peer -> Object -> Value -> IO ()
respond peer frame value = case field "id" frame of
  String identifier -> atomically (writeTQueue (peerIncoming peer) (reply identifier value))
  _ -> assertFailure "Missing request ID"

rejectLoad :: Peer -> Object -> IO ()
rejectLoad peer frame = case field "id" frame of
  String identifier -> atomically (writeTQueue (peerIncoming peer) (KeyMap.insert "error" (object ["code" .= RpcInvalidParams, "message" .= String "fixture rejection"]) (KeyMap.delete "result" (reply identifier Null))))
  _ -> assertFailure "Missing request ID"

notify :: Peer -> Text -> Value -> IO ()
notify peer identifier notification = atomically (writeTQueue (peerIncoming peer) (KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= notification]]))

settle :: Daemon.DaemonConnection -> IO ()
settle connection = do
  ready <- newEmptyTMVarIO
  bracket (Daemon.onRequestSettled connection (const (atomically (void (tryPutTMVar ready ()))))) id $ \_ -> do
    void (Daemon.getProxyToken connection)
    atomically (readTMVar ready)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

loadValue :: Text -> [SubagentInvocationSummary] -> Value
loadValue identifier summaries = object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"], "cwd" .= ("/saved/" <> identifier), "subagentInvocations" .= summaries]

withCwd :: Text -> Value -> Value
withCwd cwd (Object fields) = Object (KeyMap.insert "cwd" (String cwd) fields)
withCwd _ _ = error "Expected fixture object"

childLoadValue :: Value
childLoadValue = object ["sessionId" .= String "child", "session" .= object ["messages" .= [object ["id" .= String "assistant", "role" .= String "assistant", "createdAt" .= (1 :: Int), "updatedAt" .= (1 :: Int), "content" .= [object ["type" .= String "tool_use", "id" .= Text.pack (show n), "name" .= String "Read", "input" .= object []] | n <- [1 .. 9 :: Int]]]]], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low", "tags" .= [object ["name" .= String "subagent"]]], "callingSessionId" .= String "parent"]

completedTurn :: Value
completedTurn = object ["type" .= String "agent_turn_completed", "turnId" .= String "turn", "reason" .= String "completed", "tokenUsage" .= object ["inputTokens" .= (0 :: Int), "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]]
