{-# LANGUAGE OverloadedStrings #-}

module DaemonAutomationSpec (daemonAutomationTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Automation
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (enumSchemaTest, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

daemonAutomationTests :: Value -> TestTree
daemonAutomationTests schema =
  testGroup
    "Daemon automation lifecycle"
    [ enumSchemaTest "template variants" (schemaAt ["definitions", "AutomationTemplateIdSchema", "enum"] schema) (Proxy @AutomationTemplate),
      testCase "catalog identity, metadata, invalid entries and pending setups remain distinct" $ do
        entry <- decodeValue @AutomationEntry entryWire
        toJSON entry @?= entryWire
        automationEntryId entry @?= "directory-id"
        automationEntryUuid entry @?= Just "backend-uuid"
        automationEntryComputerId entry @?= Just "registered-computer"
        automationEntryMachineId entry @?= Just "local"
        automationEntryValid entry @?= False
        show entry @?= "AutomationEntry <redacted>"
        forM_ (KeyMap.keys (objectFields entryWire)) $ \key -> rejects (Proxy @AutomationEntry) (insertField key Null entryWire)
        roundTrip (Proxy @ListAutomationsResult) listWire
        minimal <- decodeValue @ListAutomationsResult (object ["automations" .= ([] :: [Value])])
        pendingAutomationSetups minimal @?= Nothing
        empty <- decodeValue @ListAutomationsResult (object ["automations" .= ([] :: [Value]), "pendingSetups" .= ([] :: [Value])])
        pendingAutomationSetups empty @?= Just []
        setup <- decodeValue @AutomationPendingSetup (object ["automationUuid" .= String "uuid", "state" .= String "future-state"])
        pendingAutomationState setup @?= "future-state",
      testCase "address and scaffold codecs preserve omission, aliases and reserved-key precedence" $ do
        toJSON (defaultAutomationAddress "id") @?= object ["automationId" .= String "id", "automationDirName" .= String "id"]
        roundTrip (Proxy @AutomationAddress) (object ["automationId" .= String ""])
        roundTrip (Proxy @AutomationListParams) (object ["basePath" .= String ""])
        roundTrip (Proxy @RunAutomationParams) runParamsWire
        roundTrip (Proxy @AutomationHistoryParams) historyParamsWire
        roundTrip (Proxy @AutomationVisualParams) visualParamsWire
        roundTrip (Proxy @RenameAutomationParams) renameParamsWire
        roundTrip (Proxy @AutomationScaffoldFile) scaffoldFileWire
        roundTrip (Proxy @AutomationScaffoldSkill) scaffoldSkillWire
        let base = defaultAutomationAddress "id"
            injected = base {automationAddressAdditionalFields = KeyMap.fromList ["automationId" .= String "wrong", "computerId" .= String "wrong", "skills" .= Null]}
        toJSON (RunAutomationParams injected Nothing Nothing Nothing) @?= object ["automationId" .= String "id", "automationDirName" .= String "id"]
        forM_ ["automationDirName", "basePath"] $ \key -> rejects (Proxy @AutomationAddress) (object ["automationId" .= String "id", key .= Null])
        rejects (Proxy @RunAutomationParams) (object ["automationId" .= String "id", "skills" .= Null])
        rejects (Proxy @AutomationHistoryParams) (object ["automationId" .= String "id", "limit" .= Null])
        rejects (Proxy @AutomationVisualParams) (object ["automationId" .= String "id", "sessionId" .= Null]),
      testCase "run/history/visual reports preserve preparation data and false/empty values" $ do
        roundTrip (Proxy @RunAutomationResult) runWire
        roundTrip (Proxy @AutomationStatusResult) statusWire
        roundTrip (Proxy @AutomationHistoryResult) historyWire
        roundTrip (Proxy @AutomationVisualResult) visualWire
        run <- decodeValue @RunAutomationResult runWire
        automationRunPrompt run @?= "REMINDER\nPROMPT"
        automationScaffoldReminder run @?= Just "REMINDER"
        history <- decodeValue @AutomationHistoryResult historyWire
        automationHistoryTotal history @?= 0.5
        visual <- decodeValue @AutomationVisualResult visualWire
        automationVisualExists visual @?= False
        automationVisualContent visual @?= Just "<script>NOT EXECUTED</script>"
        forM_ ["model", "reasoningEffort", "templateId", "modelFallback"] $ \key -> rejects (Proxy @RunAutomationResult) (insertField key Null runWire)
        rejects (Proxy @AutomationVisualResult) (object ["automationId" .= String "id", "exists" .= False, "content" .= Null])
        rejects (Proxy @AutomationRunRecord) (insertField "type" (String "future") runRecordWire),
      testCase "all eight global lifecycle calls preserve requests without launching agent sessions" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withAutomationPeer Normal trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ (zip wireCases (highCalls connection)) $ \((_, _, expected), request) -> request >>= (@?= expected)
        readIORef trace >>= (@?= map String ("daemon.authenticate" : map (\(method, _, _) -> method) wireCases)) . map (field "method"),
      testCase "malformed replies and remote errors preserve connection reuse" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withAutomationPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
              forM_ (highCalls connection) $ \request -> do
                result <- try @RpcResultError request
                case result of
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected automation result",
      testCase "cancelled run preparation preserves async identity without a rollback or pause RPC" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withAutomationPeer HeldRun trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.runAutomation connection runParams) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled preparation returned"
          Daemon.listAutomations connection (Just "") >>= (@?= listWire) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.run_automation", "daemon.list_automations"]) . map (field "method"),
      testCase "all low-level lifecycle bindings preserve routing, metadata and caller deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "automation-rpc" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
          forM_ (zip wireCases (lowCalls channel configured)) $ \((method, params, expected), request) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String method
            field "params" frame @?= params
            atomically (writeTQueue incoming (response frame expected))
            wait pending >>= (@?= expected)
          forM_ (lowCalls channel (configured {Client.callTimeoutMicros = Just 0})) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

address :: AutomationAddress
address = AutomationAddress "stable-id" (Just "directory-id") (Just "") (KeyMap.fromList ["automationId" .= String "wrong", "future" .= False])

runParams :: RunAutomationParams
runParams = RunAutomationParams address (Just "computer") (Just [AutomationScaffoldSkill "skill" Nothing "metadata-only" Nothing mempty]) (Just [])

historyParams :: AutomationHistoryParams
historyParams = AutomationHistoryParams address (Just 1.5) (Just (-0.5))

visualParams :: AutomationVisualParams
visualParams = AutomationVisualParams address (Just "")

renameParams :: RenameAutomationParams
renameParams = RenameAutomationParams address ""

highCalls :: Daemon.DaemonConnection -> [IO Value]
highCalls connection = [toJSON <$> Daemon.listAutomations connection (Just ""), toJSON <$> Daemon.runAutomation connection runParams, toJSON <$> Daemon.pauseAutomation connection address, toJSON <$> Daemon.resumeAutomation connection address, toJSON <$> Daemon.getAutomationHistory connection historyParams, toJSON <$> Daemon.getAutomationVisual connection visualParams, toJSON <$> Daemon.renameAutomation connection renameParams, toJSON <$> Daemon.deleteAutomation connection address]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel configured = [toJSON <$> Client.listDaemonAutomations channel configured (AutomationListParams (Just "") mempty), toJSON <$> Client.runDaemonAutomation channel configured runParams, toJSON <$> Client.pauseDaemonAutomation channel configured address, toJSON <$> Client.resumeDaemonAutomation channel configured address, toJSON <$> Client.getDaemonAutomationHistory channel configured historyParams, toJSON <$> Client.getDaemonAutomationVisual channel configured visualParams, toJSON <$> Client.renameDaemonAutomation channel configured renameParams, toJSON <$> Client.deleteDaemonAutomation channel configured address]

wireCases :: [(Text, Value, Value)]
wireCases = [("daemon.list_automations", object ["basePath" .= String ""], listWire), ("daemon.run_automation", runParamsWire, runWire), ("daemon.pause_automation", addressWire, statusWire), ("daemon.resume_automation", addressWire, statusWire), ("daemon.get_automation_history", historyParamsWire, historyWire), ("daemon.get_automation_visual", visualParamsWire, visualWire), ("daemon.rename_automation", renameParamsWire, object ["success" .= False, "error" .= String ""]), ("daemon.delete_automation", addressWire, object ["success" .= False])]

data Mode = Normal | Rejected | Malformed | HeldRun deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withAutomationPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withAutomationPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      forever $ do
        request <- readFrame connection
        case [(method, params, result) | (method, params, result) <- wireCases, field "method" request == String method] of
          [(method, params, result)] -> do
            field "params" request @?= params
            if mode == HeldRun && method == "daemon.run_automation"
              then putMVar ready ()
              else case mode of
                Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Automation rejected"]]
                Malformed -> reply connection request Null
                _ -> reply connection request result
          _ -> assertFailure "Unexpected automation or session request"
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

response :: Object -> Value -> Object
response request result = KeyMap.fromList (envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result])

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . object . envelope

envelope :: [Pair] -> [Pair]
envelope fields = ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

objectFields :: Value -> Object
objectFields (Object fields) = fields
objectFields _ = error "Expected object fixture"

insertField :: Key -> Value -> Value -> Value
insertField key value = Object . KeyMap.insert key value . objectFields

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decodeValue @a value >>= (@?= value) . toJSON

addressWire, runParamsWire, historyParamsWire, visualParamsWire, renameParamsWire, entryWire, listWire, runWire, statusWire, historyWire, runRecordWire, visualWire, scaffoldFileWire, scaffoldSkillWire, fallbackWire :: Value
addressWire = object ["automationId" .= String "stable-id", "automationDirName" .= String "directory-id", "basePath" .= String "", "future" .= False]
runParamsWire = insertField "memoryFiles" (toJSON ([] :: [Value])) (insertField "skills" (toJSON [object ["name" .= String "skill", "fingerprint" .= String "metadata-only"]]) (insertField "computerId" (String "computer") addressWire))
historyParamsWire = insertField "limit" (Number 1.5) (insertField "offset" (Number (-0.5)) addressWire)
visualParamsWire = insertField "sessionId" (String "") addressWire
renameParamsWire = insertField "newName" (String "") addressWire
entryWire = object ["id" .= String "directory-id", "uuid" .= String "backend-uuid", "name" .= String "automation", "description" .= String "", "prompt" .= String "NOT EXECUTED", "status" .= String "future-status", "schedule" .= String "opaque schedule", "model" .= String "model", "reasoningEffort" .= String "future-effort", "tags" .= ([] :: [Value]), "nextRunAt" .= String "opaque", "lastRunAt" .= String "opaque", "lastRunStatus" .= String "future", "isValid" .= False, "path" .= String "/remote/automation", "templateId" .= String "triage", "privacyLevel" .= String "future", "sessionPrivacy" .= String "future", "createdBy" .= object ["name" .= String "", "email" .= String "", "avatarUrl" .= String ""], "forkedFrom" .= String "", "workingDirectory" .= String "/remote/project", "computerId" .= String "registered-computer", "machineId" .= String "local", "hostId" .= String "opaque-host", "workstreamId" .= String "", "setupState" .= String "future-setup"]
listWire = object ["automations" .= [entryWire], "pendingSetups" .= [object ["automationUuid" .= String "pending-id", "sessionId" .= String "session", "state" .= String "needs_input", "startedAt" .= String "opaque time"]]]
fallbackWire = object ["requestedModel" .= String "requested", "reason" .= String "model_not_available_for_org"]
runWire = object ["prompt" .= String "REMINDER\nPROMPT", "automationName" .= String "automation", "automationId" .= String "resolved-uuid", "templateId" .= String "code-review", "cwd" .= String "/remote/project", "model" .= String "resolved", "reasoningEffort" .= String "future-effort", "computerId" .= String "computer", "scaffoldReminder" .= String "REMINDER", "sessionPrivacy" .= String "future", "modelFallback" .= fallbackWire]
statusWire = object ["success" .= False, "automationId" .= String "resolved-uuid", "status" .= String "remote-status", "error" .= String ""]
runRecordWire = object ["runId" .= String "run", "automationId" .= String "resolved-uuid", "type" .= String "run", "status" .= String "future-status", "startedAt" .= String "opaque", "completedAt" .= String "", "durationMs" .= Number 0.25, "errorMessage" .= String "", "isRetry" .= False, "originalRunId" .= String "", "sessionId" .= String "session", "modelFallback" .= fallbackWire]
historyWire = object ["automationId" .= String "resolved-uuid", "runs" .= [runRecordWire], "totalCount" .= Number 0.5]
visualWire = object ["automationId" .= String "resolved-uuid", "exists" .= False, "content" .= String "<script>NOT EXECUTED</script>", "isStale" .= False, "s3Url" .= String "https://visual.invalid/doc"]
scaffoldFileWire = object ["fileId" .= String "sub%2Ffile", "content" .= String "", "fingerprint" .= String "opaque fingerprint"]
scaffoldSkillWire = object ["name" .= String "skill", "fingerprint" .= String "opaque", "supportingFiles" .= [scaffoldFileWire]]
