{-# LANGUAGE OverloadedStrings #-}

module MissionRegistrySpec (missionRegistryTests) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (..), DroidEvent (..))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Protocol (RpcChannelError (RpcChannelClosed))
import Factory.Droid.Schema.Control (GetRewindInfoParams (..), GetRewindInfoResult)
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))
import Factory.Droid.Schema.Mission
import Factory.Droid.Schema.Usage (TokenUsage (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.IO.Error (catchIOError, isResourceVanishedError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

missionRegistryTests :: TestTree
missionRegistryTests =
  testGroup
    "Mission association registry"
    [ testCase "lookups do not create stores and pristine reassociation cannot erase data" $ do
        let empty = Mission.emptyMissionRegistry
            parent = Mission.modifyMissionStore "parent" (Mission.setTitle (Just "parent") . Mission.setState MissionRunning) empty
            provisional = Mission.associateSessionWithMission "worker" "worker" parent
            joined = Mission.associateSessionWithMission "worker" "parent" provisional
        Mission.lookupMissionStore "unknown" empty @?= Right Nothing
        Mission.resolveMissionId "unknown" empty @?= Nothing
        Mission.resolveMissionId "worker" joined @?= Just "parent"
        snapshotFor "worker" joined @?= snapshotFor "parent" parent,
      testCase "observed source fields win; reassociation preserves other aliases and separates later updates" $ do
        let initial = Mission.modifyMissionStore "parent" (Mission.setTitle (Just "parent") . Mission.setState MissionRunning) Mission.emptyMissionRegistry
            provisional = Mission.modifyMissionStore "worker" (Mission.setTitle Nothing . Mission.setState MissionPaused) initial
            joined = Mission.associateSessionWithMission "worker" "parent" provisional
            moved = Mission.associateSessionWithMission "worker" "destination" joined
            updated = Mission.modifyMissionStore "worker" (Mission.setTitle (Just "destination")) moved
        missionSnapshotState (snapshotFor "parent" joined) @?= MissionPaused
        missionSnapshotTitle (snapshotFor "parent" joined) @?= Nothing
        Mission.resolveMissionId "parent" updated @?= Just "parent"
        Mission.resolveMissionId "worker" updated @?= Just "destination"
        missionSnapshotTitle (snapshotFor "parent" updated) @?= Nothing
        missionSnapshotTitle (snapshotFor "worker" updated) @?= Just "destination"
        Mission.sharesMission "parent" "worker" updated @?= False,
      testCase "unreferenced provisional stores are removed and parent aliases resolve one shared mission" $ do
        let initial = Mission.modifyMissionStore "solo" (Mission.setState MissionRunning) Mission.emptyMissionRegistry
            moved = Mission.associateSessionWithMission "solo" "mission" initial
            fresh = Mission.associateSessionWithMission "new" "solo" moved
            parent = Mission.associateSessionWithMission "parent" "mission" fresh
            child = Mission.associateWorkerWithParentMission "parent" "worker" parent
        missionSnapshotState (snapshotFor "new" fresh) @?= MissionAwaitingInput
        Mission.resolveMissionId "worker" child @?= Just "mission"
        Mission.sharesMission "solo" "worker" child @?= True
        let orphan = Mission.associateWorkerWithParentMission "unseen-parent" "orphan" Mission.emptyMissionRegistry
        Mission.resolveMissionId "orphan" orphan @?= Just "unseen-parent"
        Mission.lookupMissionStore "unseen-parent" orphan @?= Right Nothing,
      testCase "invalid provisional observations propagate only to their mission and a full snapshot repairs them" $ do
        snapshot <- decodeValue @MissionSnapshot snapshotWire
        let base = Mission.modifyMissionStore "other" (Mission.setState MissionCompleted) Mission.emptyMissionRegistry
            invalid = Mission.invalidateMissionStore "worker" base
            joined = Mission.associateSessionWithMission "worker" "parent" invalid
            registered = Mission.associateSessionWithMission "parent" "parent" joined
        Mission.lookupMissionStore "parent" registered @?= Left Mission.MissionStateInvalid
        missionSnapshotState (snapshotFor "other" registered) @?= MissionCompleted
        let recovered = Mission.restoreRegistrySnapshotAt "observed" "parent" snapshot registered
        missionSnapshotState (snapshotFor "worker" recovered) @?= MissionRunning,
      testCase "associated usage prefers inclusive zero, replaces worker totals and ignores unrelated identities" $ do
        snapshot <- decodeValue @MissionSnapshot snapshotWire
        let counts input = object ["inputTokens" .= input, "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]
            usage :: Text -> Int -> Maybe Int -> IO DroidEvent
            usage identifier input inclusive = UsageEvent <$> decodeValue (object (["type" .= String "session_token_usage_changed", "sessionId" .= identifier, "tokenUsage" .= counts input] <> maybe [] (\total -> ["inclusiveTokenUsage" .= counts total]) inclusive))
            base = Mission.restoreRegistrySnapshotAt "observed" "parent" snapshot Mission.emptyMissionRegistry
        owner <- usage "parent" 10 (Just 0)
        worker <- usage "worker" 2 Nothing
        unrelated <- usage "other" 999 Nothing
        replacement <- usage "worker" 5 Nothing
        let ignored = Mission.applyRegistryEventAt "observed" "unknown" unrelated Mission.emptyMissionRegistry
            accounted = foldl' (flip (Mission.applyRegistryEventAt "observed" "parent")) base [owner, worker, unrelated]
            updated = Mission.applyRegistryEventAt "observed" "worker" replacement accounted
        Mission.lookupMissionStore "unknown" ignored @?= Right Nothing
        Mission.resolveMissionId "unknown" ignored @?= Nothing
        fmap usageInputTokens (missionSnapshotTokenUsage (snapshotFor "parent" accounted)) @?= Just 2
        fmap usageInputTokens (missionSnapshotTokenUsage (snapshotFor "parent" updated)) @?= Just 5
        snapshotFor "worker" updated @?= snapshotFor "parent" updated
        Mission.lookupMissionStore "other" updated @?= Right Nothing,
      testCase "pre-reply worker observations merge into a loaded mission without clobbering pristine fields" $ bounded $ do
        ready <- newEmptyMVar
        release <- newMVar ()
        withRegistryPeer (KeyMap.singleton "mission" snapshotWire) [("worker", stateEvent "paused"), ("other", stateEvent "completed")] [] ready release $ \target ->
          Daemon.withResumedSession (options target) "parent" $ \session -> do
            let connection = Daemon.sessionConnection session
            snapshot <- requiredSnapshot session
            missionSnapshotState snapshot @?= MissionPaused
            missionSnapshotTitle snapshot @?= Just "loaded title"
            Daemon.getMissionIdForSession connection "worker" >>= (@?= Just "parent")
            Daemon.getMissionSnapshotForSession connection "worker" >>= (@?= Just snapshot)
            other <- Daemon.getMissionSnapshotForSession connection "other"
            fmap missionSnapshotState other @?= Just MissionCompleted
            Daemon.getMissionIdForSession connection "unknown" >>= (@?= Nothing)
            Daemon.getMissionSnapshotForSession connection "unknown" >>= (@?= Nothing)
            Daemon.getMissionIdForSession connection "unknown" >>= (@?= Nothing),
      testCase "calling-session receipts bind a worker's provisional state to its parent" $ bounded $ do
        ready <- newEmptyMVar
        release <- newMVar ()
        withRegistryPeer (KeyMap.singleton "callingSessionId" (String "parent")) [("parent", stateEvent "paused"), ("worker", stateEvent "running"), ("worker", titleEvent "early title")] [] ready release $ \target ->
          Daemon.withResumedSession (options target) "worker" $ \session -> do
            let connection = Daemon.sessionConnection session
            snapshot <- requiredSnapshot session
            missionSnapshotState snapshot @?= MissionRunning
            missionSnapshotTitle snapshot @?= Just "early title"
            Daemon.getMissionIdForSession connection "worker" >>= (@?= Just "parent")
            Daemon.getMissionSnapshotForSession connection "parent" >>= (@?= Just snapshot)
            loaded <- maybe (assertFailure "Missing load receipt") pure (Daemon.daemonLoadedState (Daemon.sessionInfo session))
            loadedCallingSessionId loaded @?= Just "parent",
      testCase "calling-session presence preserves empty strings and rejects null or nontext before publication" $ bounded $ do
        forM_ [Nothing, Just ""] $ \parent -> do
          ready <- newEmptyMVar
          release <- newMVar ()
          let extra = maybe mempty (KeyMap.singleton "callingSessionId" . String) parent
          withRegistryPeer extra [] [] ready release $ \target ->
            Daemon.withResumedSession (options target) "worker" $ \session -> do
              loaded <- maybe (assertFailure "Missing load receipt") pure (Daemon.daemonLoadedState (Daemon.sessionInfo session))
              loadedCallingSessionId loaded @?= parent
              Daemon.getMissionSnapshot session >>= (@?= Nothing)
              Daemon.getMissionIdForSession (Daemon.sessionConnection session) "worker" >>= (@?= Nothing)
        forM_ [Null, Bool False, Number 1] $ \parent -> do
          ready <- newEmptyMVar
          release <- newMVar ()
          withRegistryPeer (KeyMap.singleton "callingSessionId" parent) [] [] ready release $ \target ->
            try @DroidError @() (Daemon.withResumedSession (options target) "worker" (\_ -> assertFailure "Invalid parent published a session")) >>= (@?= Left DroidInvalidEvent),
      testCase "mission subscriptions include associated workers but ordinary session subscriptions stay scoped" $ bounded $ do
        ready <- newEmptyMVar
        release <- newMVar ()
        let events = [("worker", stateEvent "paused"), ("other", stateEvent "running"), ("parent", stateEvent "completed")]
        cached <- withRegistryPeer (KeyMap.singleton "mission" snapshotWire) [] events ready release $ \target ->
          Daemon.withResumedSession (options target) "parent" $ \session -> do
            missionStates <- newIORef []
            sessionStates <- newIORef []
            done <- newEmptyMVar
            stopMission <- Daemon.onMissionSnapshot session $ \case
              Left cause -> assertFailure (show cause)
              Right current -> do
                Daemon.getMissionSnapshot session >>= (@?= current)
                forM_ current $ \value -> modifyIORef' missionStates (<> [missionSnapshotState value])
            stopSession <- Daemon.onSessionEvent session $ \case
              Right (MissionStateEvent changed) -> do
                modifyIORef' sessionStates (<> [changedMissionPhase changed])
                putMVar done ()
              Left cause -> assertFailure (show cause)
              _ -> pure ()
            void (query session)
            takeMVar done
            readIORef missionStates >>= (@?= [MissionPaused, MissionCompleted])
            readIORef sessionStates >>= (@?= [MissionCompleted])
            stopMission
            stopMission
            void (query session)
            takeMVar done
            readIORef missionStates >>= (@?= [MissionPaused, MissionCompleted])
            stopSession
            pure (Daemon.getMissionSnapshotForSession (Daemon.sessionConnection session) "worker")
        try @RpcChannelError cached >>= (@?= Left RpcChannelClosed),
      testCase "cancelling an in-flight load with provisional state publishes no handle" $ bounded $ do
        ready <- newEmptyMVar
        release <- newEmptyMVar
        published <- newIORef False
        withRegistryPeer mempty [("worker", stateEvent "running")] [] ready release $ \target -> do
          withAsync (Daemon.withResumedSession (options target) "worker" (\_ -> writeIORef published True)) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled load completed"
            putMVar release ()
        readIORef published >>= (@?= False)
    ]

snapshotFor :: Text -> Mission.MissionRegistry -> MissionSnapshot
snapshotFor identifier registry = case Mission.lookupMissionStore identifier registry of
  Right (Just store) -> Mission.missionSnapshot store
  _ -> error "Expected mission in fixture"

snapshotWire :: Value
snapshotWire = object ["state" .= String "running", "features" .= ([] :: [Value]), "progressLog" .= ([] :: [Value]), "workerSessionIds" .= [String "worker"], "title" .= String "loaded title"]

stateEvent :: Text -> Value
stateEvent state = object ["type" .= String "mission_state_changed", "state" .= state]

titleEvent :: Text -> Value
titleEvent title = object ["type" .= String "mission_progress_entry", "progressLog" .= [object ["type" .= String "mission_accepted", "timestamp" .= String "reported time", "title" .= title]]]

query :: Daemon.DaemonSession -> IO GetRewindInfoResult
query session = Daemon.getRewindInfo (Daemon.sessionConnection session) (GetRewindInfoParams (Daemon.sessionId session) "emit" mempty)

requiredSnapshot :: Daemon.DaemonSession -> IO MissionSnapshot
requiredSnapshot session = Daemon.getMissionSnapshot session >>= maybe (assertFailure "Missing mission snapshot") pure

withRegistryPeer :: Object -> [(Text, Value)] -> [(Text, Value)] -> MVar () -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withRegistryPeer extra before events ready release = withPeer $ \_ connection ->
  (serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
    `catchIOError` \err -> if isResourceVanishedError err then pure () else ioError err
  where
    readFrame connection = WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      load <- readFrame connection
      field "method" load @?= String "daemon.load_session"
      params <- decodeValue @Object (field "params" load)
      identifier <- decodeValue @Text (field "sessionId" params)
      forM_ before (uncurry (notify connection))
      putMVar ready ()
      takeMVar release
      reply connection load (Object (KeyMap.union extra (KeyMap.fromList ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "offline-model", "reasoningEffort" .= String "low"]])))
      forever $ do
        request <- readFrame connection
        body <- decodeValue @Object (field "params" request)
        field "sessionId" body @?= String identifier
        case field "method" request of
          String "daemon.list_terminals" -> reply connection request (object ["terminals" .= ([] :: [Value])])
          _ -> do
            field "method" request @?= String "daemon.get_rewind_info"
            when (field "messageId" body == String "emit") (forM_ events (uncurry (notify connection)))
            reply connection request (object ["availableFiles" .= ([] :: [Value]), "createdFiles" .= ([] :: [Value]), "evictedFiles" .= ([] :: [Value])])
    notify :: WS.Connection -> Text -> Value -> IO ()
    notify connection identifier event = send connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= event]]
    reply connection request result = send connection ["type" .= String "response", "id" .= field "id" request, "result" .= result]
    send connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "/remote") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
