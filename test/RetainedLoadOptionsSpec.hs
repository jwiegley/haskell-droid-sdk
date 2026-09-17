{-# LANGUAGE OverloadedStrings #-}

module RetainedLoadOptionsSpec (retainedLoadOptionsTests) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM
import Control.Exception (try)
import Control.Monad (forM_, void, when)
import Data.Aeson (Object, Value (..), object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as K
import Data.Maybe (fromMaybe)
import Factory.Droid.Daemon qualified as D
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcConflict))
import Factory.Droid.Transport (objectTransport)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

retainedLoadOptionsTests :: TestTree
retainedLoadOptionsTests =
  testGroup
    "Retained load options"
    [ testCase "offline replacement distinguishes absent and empty without registering a session" $ D.withDaemonState $ \state -> do
        before <- D.getDaemonStateSnapshot state
        D.getSessionLoadOptions state "s" >>= (@?= Nothing)
        D.setSessionLoadOptions state "s" oldPolicy
        D.getSessionLoadOptions state "s" >>= (@?= Just oldPolicy)
        D.setSessionLoadOptions state "s" emptyPolicy
        D.getSessionLoadOptions state "s" >>= (@?= Just emptyPolicy)
        D.getDaemonStateSnapshot state >>= (@?= before)
        D.deleteSessionLoadOptions state "s"
        D.deleteSessionLoadOptions state "s"
        D.getSessionLoadOptions state "s" >>= (@?= Nothing),
      testCase "replacement preserves explicit null false empty and opaque values without merging" $ D.withDaemonState $ \state -> do
        D.setSessionLoadOptions state "s" oldPolicy
        D.setSessionLoadOptions state "s" explicitPolicy
        D.getSessionLoadOptions state "s" >>= (@?= Just explicitPolicy)
        D.setSessionLoadOptions state "s" (defaultSessionLoadConfiguration {loadDisableBuiltinSkills = Just True}, defaultDaemonLoadConfiguration)
        D.getSessionLoadOptions state "s" >>= (@?= Just (defaultSessionLoadConfiguration {loadDisableBuiltinSkills = Just True}, defaultDaemonLoadConfiguration)),
      testCase "invalid IDs fail all operations and padded valid IDs remain literal" $ D.withDaemonState $ \state -> do
        forM_ ["", " \t", "\xfeff"] $ \identifier -> do
          try @D.DaemonError (D.setSessionLoadOptions state identifier emptyPolicy) >>= (@?= Left D.InvalidSessionCacheIdentity)
          try @D.DaemonError (D.getSessionLoadOptions state identifier) >>= (@?= Left D.InvalidSessionCacheIdentity)
          try @D.DaemonError (D.deleteSessionLoadOptions state identifier) >>= (@?= Left D.InvalidSessionCacheIdentity)
        D.setSessionLoadOptions state " s " emptyPolicy
        D.getSessionLoadOptions state "s" >>= (@?= Nothing)
        D.getSessionLoadOptions state " s " >>= (@?= Just emptyPolicy),
      testCase "logical closure rejects set get and delete" $ do
        state <- D.withDaemonState pure
        try @D.DaemonError (D.setSessionLoadOptions state "s" emptyPolicy) >>= (@?= Left D.DaemonStateClosed)
        try @D.DaemonError (D.getSessionLoadOptions state "s") >>= (@?= Left D.DaemonStateClosed)
        try @D.DaemonError (D.deleteSessionLoadOptions state "s") >>= (@?= Left D.DaemonStateClosed),
      testCase "ordinary loads capture connection defaults for later replay" $ bounded $ D.withDaemonState $ \state ->
        withPeer state oldPolicy $ \connection peer -> do
          void (D.loadSessionInfo connection "s")
          D.getSessionLoadOptions state "s" >>= (@?= Just oldPolicy)
          request <- nextRequest peer
          field "messageLimit" (params request) @?= Number 7
          field "runtimeSettingsPath" (params request) @?= String "/old",
      testCase "empty retained policy falls back to defaults but explicit resets win on wire" $ bounded $ D.withDaemonState $ \state ->
        withPeer state oldPolicy $ \connection peer -> do
          D.setSessionLoadOptions state "s" emptyPolicy
          void (D.loadSessionInfo connection "s")
          first <- nextRequest peer
          field "messageLimit" (params first) @?= Number 7
          D.setSessionLoadOptions state "s" explicitPolicy
          void (D.loadSessionInfo connection "s")
          second <- nextRequest peer
          field "structuredOutputFormat" (params second) @?= Null
          K.member "structuredOutputFormat" (params second) @?= True
          field "disableInactivityTimeout" (params second) @?= Bool False
          field "disableBuiltinSkills" (params second) @?= Bool False
          field "runtimeSettingsPath" (params second) @?= String ""
          field "taskSubagentProcess" (params second) @?= Bool True
          field "future" (params second) @?= object ["keep" .= (42 :: Int)],
      testCase "retained task-process hints reject non-true values before any load RPC" $
        bounded $
          forM_ [Bool False, Null, Number 0, String "true"] $ \value -> D.withDaemonState $ \state ->
            withPeer state emptyPolicy $ \connection peer -> do
              let policy = (defaultSessionLoadConfiguration {loadAdditionalFields = K.singleton "taskSubagentProcess" value}, defaultDaemonLoadConfiguration)
              D.setSessionLoadOptions state "s" policy
              D.getSessionLoadOptions state "s" >>= (@?= Just policy)
              try @LoadConfigurationError (void (D.loadSessionInfo connection "s")) >>= (@?= Left InvalidLoadParams)
              readTVarIO (peerRequestCount peer) >>= (@?= 0),
      testCase "configured loads still merge while independent replacement drops prior fields" $ bounded $ D.withDaemonState $ \state ->
        withPeer state emptyPolicy $ \connection peer -> do
          D.setSessionLoadOptions state "s" oldPolicy
          let addition = defaultSessionLoadConfiguration {loadDisableBuiltinSkills = Just False}
          void (D.loadSessionInfoWithConfiguration connection "s" addition defaultDaemonLoadConfiguration)
          first <- nextRequest peer
          field "messageLimit" (params first) @?= Number 7
          field "runtimeSettingsPath" (params first) @?= String "/old"
          field "disableBuiltinSkills" (params first) @?= Bool False
          D.setSessionLoadOptions state "s" (addition, defaultDaemonLoadConfiguration)
          void (D.loadSessionInfo connection "s")
          second <- nextRequest peer
          K.member "messageLimit" (params second) @?= False
          K.member "runtimeSettingsPath" (params second) @?= False,
      testCase "forgetting options leaves loaded observations readiness and cache untouched" $ bounded $ D.withDaemonState $ \state ->
        withPeer state emptyPolicy $ \connection peer -> do
          void (uncurry (D.loadSessionInfoWithConfiguration connection "s") oldPolicy)
          void (nextRequest peer)
          before <- D.getDaemonStateSnapshot state
          D.deleteSessionLoadOptions state "s"
          D.getDaemonStateSnapshot state >>= (@?= before)
          D.getSessionLoadOptions state "s" >>= (@?= Nothing)
          D.getCachedSessionIds connection >>= (@?= ["s"])
          readTVarIO (peerRequestCount peer) >>= (@?= 1),
      testCase "in-flight loads keep captured parameters but do not overwrite newer policy" $ bounded $ D.withDaemonState $ \state ->
        withPeer state emptyPolicy $ \connection peer -> do
          D.setSessionLoadOptions state "s" oldPolicy
          atomically (writeTVar (peerHold peer) True)
          withAsync (D.loadSessionInfo connection "s") $ \worker -> do
            request <- nextRequest peer
            field "messageLimit" (params request) @?= Number 7
            D.setSessionLoadOptions state "s" newPolicy
            respondLoad peer request
            void (wait worker)
          D.getSessionLoadOptions state "s" >>= (@?= Just newPolicy)
          atomically (writeTVar (peerHold peer) False)
          void (D.loadSessionInfo connection "s")
          request <- nextRequest peer
          field "messageLimit" (params request) @?= Number 9,
      testCase "failed creation preserves a later policy replacement" $ creationRace $ \state _ -> D.setSessionLoadOptions state "s" newPolicy >> pure (Just newPolicy),
      testCase "failed creation preserves an equal-value policy reassertion" $ creationRace $ \state current -> D.setSessionLoadOptions state "s" current >> pure (Just current),
      testCase "failed creation cannot resurrect independently forgotten options" $ creationRace $ \state _ -> D.deleteSessionLoadOptions state "s" >> pure Nothing,
      testCase "failed creation with no intervening edit still restores the prior policy" $ creationRace $ \_ _ -> pure (Just oldPolicy),
      testCase "automatic cache eviction does not prevent restoring untouched prior policy" $ creationRaceWithEviction True $ \_ _ -> pure (Just oldPolicy),
      testCase "cache eviction preserves intent until independent forgetting" $ bounded $ D.withDaemonState $ \state ->
        withPeer state emptyPolicy $ \connection _ -> do
          D.setSessionLoadOptions state "s" oldPolicy
          void (D.loadSessionInfo connection "s")
          D.removeCachedSession connection "s" >>= (@?= True)
          D.getSessionLoadOptions state "s" >>= (@?= Just oldPolicy)
          D.deleteSessionLoadOptions state "s"
          D.getCachedSessionIds connection >>= (@?= [])
          D.getSessionLoadOptions state "s" >>= (@?= Nothing),
      testCase "retained defaults survive physical generations and can be forgotten offline" $ bounded $ D.withDaemonState $ \state -> do
        withPeer state oldPolicy $ \connection _ -> void (D.loadSessionInfo connection "s")
        D.getSessionLoadOptions state "s" >>= (@?= Just oldPolicy)
        withPeer state newPolicy $ \connection peer -> do
          void (D.loadSessionInfo connection "s")
          request <- nextRequest peer
          field "messageLimit" (params request) @?= Number 7
        D.deleteSessionLoadOptions state "s"
        withPeer state newPolicy $ \connection peer -> do
          void (D.loadSessionInfo connection "s")
          request <- nextRequest peer
          field "messageLimit" (params request) @?= Number 9
    ]

emptyPolicy, oldPolicy, newPolicy, explicitPolicy :: D.DaemonLoadPolicy
emptyPolicy = (defaultSessionLoadConfiguration, defaultDaemonLoadConfiguration)
oldPolicy = (defaultSessionLoadConfiguration {loadMessageLimit = Just 7}, defaultDaemonLoadConfiguration {daemonLoadDisableInactivity = Just True, daemonLoadRuntimeSettingsPath = Just "/old"})
newPolicy = (defaultSessionLoadConfiguration {loadMessageLimit = Just 9}, defaultDaemonLoadConfiguration {daemonLoadRuntimeSettingsPath = Just "/new"})
explicitPolicy = (defaultSessionLoadConfiguration {loadStructuredOutput = Just Nothing, loadDisableBuiltinSkills = Just False, loadAdditionalFields = K.fromList ["taskSubagentProcess" .= True, "future" .= object ["keep" .= (42 :: Int)]]}, defaultDaemonLoadConfiguration {daemonLoadDisableInactivity = Just False, daemonLoadRuntimeSettingsPath = Just "", daemonLoadSkipPermissionsUnsafe = Just False})

creationRace :: (D.DaemonState -> D.DaemonLoadPolicy -> IO (Maybe D.DaemonLoadPolicy)) -> IO ()
creationRace = creationRaceWithEviction False

creationRaceWithEviction :: Bool -> (D.DaemonState -> D.DaemonLoadPolicy -> IO (Maybe D.DaemonLoadPolicy)) -> IO ()
creationRaceWithEviction evict edit = bounded $ D.withDaemonState $ \state -> withPeer state emptyPolicy $ \connection peer -> do
  when evict (void (D.setSessionCacheCapacity connection (Just 0)))
  D.setSessionLoadOptions state "s" oldPolicy
  let initial = defaultSessionConfiguration {configurationSessionId = Just "s"}
      options = (D.defaultDaemonSessionOptions "/offline") {D.daemonSessionParameters = (defaultInitializeSessionParams "fixture" "/offline") {initializeConfiguration = initial}, D.daemonSessionLoadConfiguration = defaultSessionLoadConfiguration {loadMessageLimit = Just 11}}
  withAsync (try @RpcResultError (D.withSessionOn connection options (const (assertFailure "Rejected creation attached")))) $ \worker -> do
    request <- nextRequest peer
    current <- D.getSessionLoadOptions state "s" >>= maybe (assertFailure "Missing creation policy") pure
    expected <- edit state current
    reject peer request
    wait worker >>= \case
      Left (RpcRemoteFailure _) -> pure ()
      _ -> assertFailure "Expected creation rejection"
    D.getSessionLoadOptions state "s" >>= (@?= expected)
  D.getCachedSessionIds connection >>= (@?= [])

data Peer = Peer
  { peerIncoming :: TQueue Object,
    peerRequests :: TQueue Object,
    peerHold :: TVar Bool,
    peerRequestCount :: TVar Int
  }

withPeer :: D.DaemonState -> D.DaemonLoadPolicy -> (D.DaemonConnection -> Peer -> IO a) -> IO a
withPeer state policy action = do
  peer <- Peer <$> newTQueueIO <*> newTQueueIO <*> newTVarIO False <*> newTVarIO 0
  let send request = do
        atomically $ do
          modifyTVar' (peerRequestCount peer) (+ 1)
          writeTQueue (peerRequests peer) request
        case field "method" request of
          String "daemon.load_session" -> readTVarIO (peerHold peer) >>= \held -> if held then pure () else respondLoad peer request
          String "daemon.initialize_session" -> pure ()
          _ -> assertFailure "Unexpected retained-option fixture request"
      options = (D.defaultDaemonClientOptions (D.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline") {D.daemonClientRestoreTerminalsOnLoad = False, D.daemonClientLoadConfiguration = fst policy, D.daemonClientLoadSpawnConfiguration = snd policy}
  D.withConnectionStateOn state options (objectTransport send (atomically (readTQueue (peerIncoming peer)))) $ \connection -> action connection peer

nextRequest :: Peer -> IO Object
nextRequest = atomically . readTQueue . peerRequests

field :: Key -> Object -> Value
field key = fromMaybe Null . K.lookup key

params :: Object -> Object
params request = case field "params" request of Object value -> value; _ -> error "Missing request parameters"

respondLoad :: Peer -> Object -> IO ()
respondLoad peer request = respond peer request (object ["sessionId" .= field "sessionId" (params request), "session" .= object ["messages" .= [object ["id" .= String "retained", "role" .= String "user", "content" .= ([] :: [Value]), "createdAt" .= (1 :: Int), "updatedAt" .= (1 :: Int)]]], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"]])

respond :: Peer -> Object -> Value -> IO ()
respond peer request value = case field "id" request of
  String identifier -> atomically (writeTQueue (peerIncoming peer) (reply identifier value))
  _ -> assertFailure "Missing request ID"

reject :: Peer -> Object -> IO ()
reject peer request = case field "id" request of
  String identifier -> atomically (writeTQueue (peerIncoming peer) (K.insert "error" (object ["code" .= RpcConflict, "message" .= String "fixture"]) (K.delete "result" (reply identifier Null))))
  _ -> assertFailure "Missing request ID"
