{-# LANGUAGE OverloadedStrings #-}

module MissionEventSpec (missionEventTests, missionWireEvents, missionEventPayload, withMissionPeerState) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (catch, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid (DroidError (..), DroidEvent (..), DroidResult (..), DroidStreamMode (..))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Schema.Control (GetRewindInfoParams (..))
import Factory.Droid.Schema.Mission
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import MissionSpec (dismissalJSON, featureJSON, handoffJSON)
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (enumSchemaTest, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

missionEventTests :: Value -> TestTree
missionEventTests schema =
  testGroup
    "Mission lifecycle events"
    [ enumSchemaTest "mission pause reasons" (schemaAt ["definitions", "MissionPausedEntrySchema", "properties", "pauseReason", "enum"] schema) (Proxy @MissionPauseReason),
      enumSchemaTest "worker failure reasons" (schemaAt ["definitions", "WorkerFailedEntrySchema", "properties", "failureReason", "enum"] schema) (Proxy @WorkerFailureReason),
      testCase "all eleven progress branches preserve required and optional fields" $
        forM_ progressSamples $ \(name, wire) -> do
          entry <- decodeValue @ProgressLogEntry wire
          toJSON entry @?= wire
          required <- either assertFailure (decodeValue @[Key]) (schemaAt ["definitions", name, "required"] schema)
          properties <- either assertFailure (decodeValue @Object) (schemaAt ["definitions", name, "properties"] schema)
          forM_ required $ \key -> rejects (Proxy @ProgressLogEntry) (deleteField key wire)
          forM_ (filter (`notElem` required) (KeyMap.keys properties)) $ \key -> rejects (Proxy @ProgressLogEntry) (insertField key Null wire),
      testCase "blank commit/repo normalization is field-local and preserves nonblank spelling" $ do
        let completed = snd (progressSamples !! 6)
        forM_ ["commitId", "repoPath"] $ \key -> do
          forM_ ["", " \t\n\xfeff"] $ \text -> do
            entry <- decodeValue @ProgressLogEntry (insertField key (String text) completed)
            toJSON entry @?= deleteField key completed
          forM_ ["  value  ", "\x85"] $ \text -> do
            let wire = insertField key (String text) completed
            entry <- decodeValue @ProgressLogEntry wire
            toJSON entry @?= wire
          forM_ [Null, Bool False, Number 0, object []] $ \value -> rejects (Proxy @ProgressLogEntry) (insertField key value completed)
        rejects (Proxy @ProgressLogEntry) (insertField "type" (String "future_entry") completed),
      testCase "optional absence, extensions, exact numbers and snapshot order remain explicit" $ do
        let optionalSamples = [("mission_paused", "pauseReason"), ("mission_resumed", "resumeWorkerSessionId"), ("mission_run_started", "message"), ("handoff_items_dismissed", "dismissals")]
        forM_ optionalSamples $ \(kind, key) -> do
          let wire = progress kind []
          entry <- decodeValue @ProgressLogEntry wire
          toJSON (entry {progressEntryAdditionalFields = KeyMap.fromList ["timestamp" .= String "wrong", "type" .= String "wrong", key .= Null, "future" .= False]}) @?= wire
        entry <- decodeValue @ProgressLogEntry (insertField "exitCode" (Number (-1.5)) (snd (progressSamples !! 6)))
        case progressEntryDetails entry of
          WorkerCompletedProgress details -> do
            progressCompletedExitCode details @?= -1.5
            progressReturnToOrchestrator details @?= False
            progressValidatorsPassed details @?= Just False
          _ -> assertFailure "Wrong progress branch"
        let wire = object ["type" .= String "mission_progress_entry", "progressLog" .= [toJSON entry, toJSON entry], "future" .= False]
        event <- decodeValue @MissionProgressEntry wire
        length (missionProgressLog event) @?= 2
        toJSON event @?= wire
        show event @?= "MissionProgressEntry <redacted>",
      testCase "daemon all-event streams deliver six typed mission events and complete mode filters them" $
        bounded $
          withMissionPeer missionWireEvents $ \target -> Daemon.withSession (options target) $ \session -> do
            observed <- newIORef []
            result <- Daemon.sendEvents session AllEvents "mission-events" (\event -> modifyIORef' observed (<> [event]))
            readIORef observed >>= (@?= missionWireEvents) . mapMaybe missionEventPayload
            resultEvents result @?= []
            resultText result @?= ""
            filtered <- newIORef []
            _ <- Daemon.sendEvents session CompleteMessages "mission-events" (\event -> modifyIORef' filtered (<> [event]))
            readIORef filtered >>= (@?= []) . mapMaybe missionEventPayload,
      testCase "outside-turn mission observers support queries and idempotent unsubscribe" $
        bounded $
          withMissionPeer missionWireEvents $ \target -> Daemon.withSession (options target) $ \session -> do
            observed <- newIORef []
            model <- newIORef Mission.emptyMissionStore
            complete <- newEmptyMVar
            let query message = Daemon.getRewindInfo (Daemon.sessionConnection session) (GetRewindInfoParams (Daemon.sessionId session) message mempty)
            stop <- Daemon.onSessionEvent session $ \case
              Left cause -> assertFailure (show cause)
              Right event -> do
                modifyIORef' model (Mission.applyEventAt "observed-event" event)
                forM_ (missionEventPayload event) $ \value -> modifyIORef' observed (<> [value])
                case event of
                  MissionStateEvent _ -> void (query "query")
                  MissionWorkerCompletedEvent _ -> putMVar complete ()
                  _ -> pure ()
            _ <- query "emit"
            takeMVar complete
            readIORef observed >>= (@?= missionWireEvents)
            snapshot <- Mission.missionSnapshot <$> readIORef model
            missionSnapshotState snapshot @?= MissionRunning
            missionSnapshotTitle snapshot @?= Just "Mission title"
            missionSnapshotWorkers snapshot @?= ["worker"]
            fmap workerCompletedAt (missionSnapshotWorkerStates snapshot >>= Map.lookup "worker") @?= Just (Just "observed-event")
            stop
            stop
            barrier <- newEmptyMVar
            stopBarrier <- Daemon.onSessionEvent session $ \case
              Right (MissionWorkerCompletedEvent _) -> putMVar barrier ()
              Left cause -> assertFailure (show cause)
              _ -> pure ()
            _ <- query "emit"
            takeMVar barrier
            readIORef observed >>= (@?= missionWireEvents)
            readIORef model >>= (@?= snapshot) . Mission.missionSnapshot
            stopBarrier,
      testCase "malformed selected mission events fail instead of becoming raw notifications" $
        bounded $
          forM_ malformedMissionEvents $ \wire ->
            withMissionPeer [wire] $ \target -> Daemon.withSession (options target) $ \session -> do
              result <- try @DroidError (Daemon.sendEvents session AllEvents "mission-events" (\_ -> pure ()))
              result @?= Left DroidInvalidEvent,
      testCase "malformed mission payloads are reported outside turns" $
        bounded $
          forM_ malformedMissionEvents $ \wire ->
            withMissionPeer [wire] $ \target -> Daemon.withSession (options target) $ \session -> do
              delivered <- newEmptyMVar
              stop <- Daemon.onSessionEvent session (putMVar delivered)
              _ <- Daemon.getRewindInfo (Daemon.sessionConnection session) (GetRewindInfoParams (Daemon.sessionId session) "emit" mempty)
              takeMVar delivered >>= (@?= Left DroidInvalidEvent)
              stop
    ]

missionEventPayload :: DroidEvent -> Maybe Value
missionEventPayload = \case
  MissionStateEvent event -> Just (toJSON event)
  MissionFeaturesEvent event -> Just (toJSON event)
  MissionProgressEvent event -> Just (toJSON event)
  MissionHeartbeatEvent event -> Just (toJSON event)
  MissionWorkerStartedEvent event -> Just (toJSON event)
  MissionWorkerCompletedEvent event -> Just (toJSON event)
  _ -> Nothing

missionWireEvents :: [Value]
missionWireEvents =
  [ object ["type" .= String "mission_state_changed", "state" .= String "running", "updatedAt" .= String "opaque update", "future" .= False],
    object ["type" .= String "mission_features_changed", "features" .= [Object featureJSON], "future" .= False],
    object ["type" .= String "mission_progress_entry", "progressLog" .= map snd progressSamples, "future" .= False],
    object ["type" .= String "mission_heartbeat", "timestamp" .= String "opaque heartbeat", "future" .= False],
    object ["type" .= String "mission_worker_started", "workerSessionId" .= String "worker", "future" .= False],
    object ["type" .= String "mission_worker_completed", "workerSessionId" .= String "worker", "exitCode" .= Number 0, "future" .= False]
  ]

malformedMissionEvents :: [Value]
malformedMissionEvents = [object ["type" .= String "mission_state_changed", "state" .= String "future"], object ["type" .= String "mission_features_changed", "features" .= String "invalid"], object ["type" .= String "mission_progress_entry", "progressLog" .= [progress "future_entry" []]], object ["type" .= String "mission_heartbeat", "timestamp" .= Null], object ["type" .= String "mission_worker_started", "workerSessionId" .= Null], object ["type" .= String "mission_worker_completed", "workerSessionId" .= String "worker", "exitCode" .= String "invalid"]]

progressSamples :: [(Key, Value)]
progressSamples =
  [ ("MissionAcceptedEntrySchema", progress "mission_accepted" ["title" .= String "Mission title"]),
    ("MissionPausedEntrySchema", progress "mission_paused" ["pauseReason" .= String "scope_growth_limit_exceeded"]),
    ("MissionResumedEntrySchema", progress "mission_resumed" ["resumeWorkerSessionId" .= String ""]),
    ("MissionRunStartedEntrySchema", progress "mission_run_started" ["message" .= String ""]),
    ("WorkerStartedEntrySchema", progress "worker_started" ["workerSessionId" .= String "worker", "spawnId" .= String "spawn", "featureId" .= String "feature", "modelId" .= String "", "substitutedFromModelId" .= String ""]),
    ("WorkerSelectedFeatureEntrySchema", progress "worker_selected_feature" ["workerSessionId" .= String "worker", "featureId" .= String "feature"]),
    ("WorkerCompletedEntrySchema", progress "worker_completed" ["workerSessionId" .= String "worker", "featureId" .= String "feature", "successState" .= String "partial", "returnToOrchestrator" .= False, "exitCode" .= Number 1, "commitId" .= String " commit ", "repoPath" .= String " /remote ", "validatorsPassed" .= False, "handoff" .= Object handoffJSON]),
    ("WorkerFailedEntrySchema", progress "worker_failed" ["workerSessionId" .= String "worker", "spawnId" .= String "spawn", "exitCode" .= Number 1, "reason" .= String "failure detail", "failureReason" .= String "worker_exited_without_handoff"]),
    ("WorkerPausedEntrySchema", progress "worker_paused" ["workerSessionId" .= String "worker", "featureId" .= String ""]),
    ("HandoffItemsDismissedEntrySchema", progress "handoff_items_dismissed" ["dismissals" .= [Object dismissalJSON]]),
    ("MilestoneValidationTriggeredEntrySchema", progress "milestone_validation_triggered" ["milestone" .= String "milestone", "featureId" .= String "feature"])
  ]

progress :: Text -> [Pair] -> Value
progress kind fields = object (["type" .= kind, "timestamp" .= String "opaque timestamp", "future" .= False] <> fields)

withMissionPeer :: [Value] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withMissionPeer = withMissionPeerState Nothing []

withMissionPeerState :: Maybe Value -> [Value] -> [Value] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withMissionPeerState snapshot afterReply events = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      initialize <- readFrame connection
      case field "method" initialize of
        String "daemon.initialize_session" -> pure ()
        String "daemon.load_session" -> pure ()
        _ -> assertFailure "Unexpected mission attachment"
      params <- decodeValue @Object (field "params" initialize)
      identifier <- decodeValue @Text (field "sessionId" params)
      let identifierField = ["sessionId" .= identifier | field "method" initialize == String "daemon.initialize_session"]
      reply connection initialize (object (["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "offline-model", "reasoningEffort" .= String "low"]] <> identifierField <> maybe [] (\value -> ["mission" .= value]) snapshot))
      forM_ afterReply (notify connection identifier)
      forever $ do
        request <- readFrame connection
        body <- decodeValue @Object (field "params" request)
        field "sessionId" body @?= String identifier
        case field "method" request of
          String "daemon.list_terminals" -> reply connection request (object ["terminals" .= ([] :: [Value])])
          String "daemon.add_user_message" -> do
            turn <- decodeValue @Text (field "messageId" body)
            reply connection request (object [])
            emitEvents connection identifier
            notify connection identifier (object ["type" .= String "agent_turn_completed", "turnId" .= turn, "reason" .= String "completed", "tokenUsage" .= object ["inputTokens" .= Number 0, "outputTokens" .= Number 0, "cacheReadTokens" .= Number 0, "cacheCreationTokens" .= Number 0, "thinkingTokens" .= Number 0]])
          String "daemon.get_rewind_info" -> do
            when (field "messageId" body == String "emit") (emitEvents connection identifier)
            reply connection request (object ["availableFiles" .= ([] :: [Value]), "createdFiles" .= ([] :: [Value]), "evictedFiles" .= ([] :: [Value])])
          String "daemon.interrupt_session" -> reply connection request (object [])
          _ -> assertFailure "Unexpected mission operation"
    emitEvents connection identifier = do
      forM_ malformedMissionEvents (notify connection "foreign-session")
      forM_ events (notify connection identifier)
    notify :: WS.Connection -> Text -> Value -> IO ()
    notify connection identifier event = WS.sendTextData connection (encode (object (envelope ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= event]])))
    reply connection request result = WS.sendTextData connection (encode (object (envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result])))

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "/remote") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

envelope :: [Pair] -> [Pair]
envelope fields = ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

insertField :: Key -> Value -> Value -> Value
insertField key value (Object fields) = Object (KeyMap.insert key value fields)
insertField _ _ _ = error "Expected object fixture"

deleteField :: Key -> Value -> Value
deleteField key (Object fields) = Object (KeyMap.delete key fields)
deleteField _ _ = error "Expected object fixture"

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
