{-# LANGUAGE OverloadedStrings #-}

module DaemonAutomationConfigSpec (automationConfigTests) where

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
import SchemaTest (enumSchemaTest, rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

automationConfigTests :: Value -> TestTree
automationConfigTests schema =
  testGroup
    "Daemon automation configuration"
    [ enumSchemaTest "automation privacy" (schemaAt ["definitions", "AutomationPrivacyLevelSchema", "enum"] schema) (Proxy @AutomationPrivacy),
      enumSchemaTest "session privacy" (schemaAt ["definitions", "SlackAutomationSessionPrivacySchema", "enum"] schema) (Proxy @AutomationSessionPrivacy),
      enumSchemaTest "creation source" (schemaAt ["definitions", "DaemonCreateAutomationRequestSchema", "allOf"] schema >>= schemaIndex 1 >>= schemaAt ["properties", "params", "properties", "creationSource", "enum"]) (Proxy @AutomationCreationSource),
      enumSchemaTest "apply failure reason" (schemaAt ["definitions", "DaemonApplyAutomationConfigReasonSchema", "enum"] schema) (Proxy @AutomationConfigFailureReason),
      testCase "creation/fork fields preserve raw strings, defaults, flags and scaffold metadata" $ do
        roundTrip (Proxy @CreateAutomationParams) createWire
        roundTrip (Proxy @ForkAutomationParams) forkWire
        toJSON (defaultCreateAutomationParams "" "" "not parsed") @?= object ["id" .= String "", "name" .= String "", "schedule" .= String "not parsed"]
        forM_ ["id", "name", "schedule"] $ \key -> rejects (Proxy @CreateAutomationParams) (deleteField key createWire)
        forM_ ["uuid", "model", "reasoningEffort", "skipFirstRun", "creationSource", "skills", "privacyLevel", "sessionPrivacy", "paused", "tags"] $ \key -> rejects (Proxy @CreateAutomationParams) (insertField key Null createWire)
        forM_ ["automationId", "name", "schedule", "prompt", "forkedFrom", "localDirName"] $ \key -> rejects (Proxy @ForkAutomationParams) (deleteField key forkWire)
        show createParams @?= "CreateAutomationParams <redacted>"
        show forkParams @?= "ForkAutomationParams <redacted>",
      testCase "model clearing is required-nullable while other setters preserve empty values" $ do
        roundTrip (Proxy @UpdateAutomationModelParams) modelWire
        cleared <- decodeValue @UpdateAutomationModelParams modelWire
        updatedAutomationModel cleared @?= Nothing
        rejects (Proxy @UpdateAutomationModelParams) addressWire
        roundTrip (Proxy @UpdateAutomationModelParams) (insertField "model" (String "") addressWire)
        roundTrip (Proxy @UpdateAutomationPrivacyParams) privacyWire
        roundTrip (Proxy @UpdateAutomationPromptParams) promptWire
        roundTrip (Proxy @UpdateAutomationScheduleParams) scheduleWire
        rejects (Proxy @UpdateAutomationPrivacyParams) (insertField "createdBy" Null privacyWire)
        rejects (Proxy @UpdateAutomationPromptParams) (insertField "prompt" Null promptWire)
        rejects (Proxy @UpdateAutomationScheduleParams) (insertField "schedule" Null scheduleWire),
      testCase "apply/update require a full config and retain single-owner extension precedence" $ do
        roundTrip (Proxy @AutomationConfiguration) configWire
        roundTrip (Proxy @ApplyAutomationConfigParams) applyWire
        roundTrip (Proxy @UpdateAutomationParams) updateWire
        toJSON applyParams @?= applyWire
        toJSON updateParams @?= updateWire
        forM_ ["name", "schedule", "prompt"] $ \key -> do
          rejects (Proxy @ApplyAutomationConfigParams) (deleteField key applyWire)
          rejects (Proxy @UpdateAutomationParams) (deleteField key updateWire)
        forM_ ["model", "reasoningEffort", "privacyLevel", "sessionPrivacy", "paused", "workingDirectory", "tags"] $ \key -> rejects (Proxy @UpdateAutomationParams) (insertField key Null updateWire)
        roundTrip (Proxy @AutomationCreationResult) creationResultWire
        roundTrip (Proxy @AutomationCreationResult) (object ["success" .= False, "automationId" .= String "", "error" .= String ""])
        forM_ ["local-file-unavailable", "discovery-failed", "apply-failed"] $ \reason -> roundTrip (Proxy @ApplyAutomationConfigResult) (object ["success" .= False, "reason" .= String reason])
        roundTrip (Proxy @ApplyAutomationConfigResult) (object ["success" .= True, "reason" .= String "apply-failed", "error" .= String ""])
        rejects (Proxy @ApplyAutomationConfigResult) (object ["success" .= False, "reason" .= String "future"]),
      testCase "all eight global config operations preserve exact payloads and privacy-only version omission" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withConfigPeer Normal trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ (zip wireCases (highCalls connection)) $ \((_, _, expected), request) -> request >>= (@?= expected)
        frames <- readIORef trace
        map (field "method") frames @?= map String ("daemon.authenticate" : map (\(method, _, _) -> method) wireCases),
      testCase "config errors remain explicit and preserve connection reuse" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withConfigPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
              forM_ (highCalls connection) $ \request -> do
                result <- try @RpcResultError request
                case result of
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected configuration result",
      testCase "cancelled privacy writes retain async identity and do not alter later request versions" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withConfigPeer HeldPrivacy trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.updateAutomationPrivacy connection privacyParams) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled privacy write returned"
          Daemon.updateAutomationPrompt connection promptParams >>= (@?= resultWire) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", privacyMethod, "daemon.update_automation_prompt"]) . map (field "method"),
      testCase "low-level config calls preserve caller identity/metadata and zero deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let body = KeyMap.fromList ["factoryProtocolVersion" .= String "injected", "marker" .= False, "id" .= String "wrong", "method" .= String "wrong"]
              configured = Client.CallOptions "config-rpc" (WithEnvelope (Just "caller-version") Nothing body) (Just 1000000)
          forM_ (zip wireCases (lowCalls channel configured)) $ \((method, params, expected), request) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "id" frame @?= String "config-rpc"
            field "method" frame @?= String method
            field "params" frame @?= params
            field "marker" frame @?= Bool False
            KeyMap.lookup "factoryProtocolVersion" frame @?= if method == privacyMethod then Nothing else Just (String "caller-version")
            atomically (writeTQueue incoming (response frame expected))
            wait pending >>= (@?= expected)
          forM_ (lowCalls channel (configured {Client.callTimeoutMicros = Just 0})) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

address :: AutomationAddress
address = AutomationAddress "stable-id" (Just "directory-id") (Just "") (KeyMap.fromList ["automationId" .= String "wrong", "future" .= False])

creator :: AutomationCreator
creator = AutomationCreator "creator" (Just "") (Just "") mempty

createParams :: CreateAutomationParams
createParams = (defaultCreateAutomationParams "directory-id" "Automation" "0 * * * *") {createAutomationUuid = Just "backend-id", createAutomationDescription = Just "", createAutomationInstructions = Just "NOT EXECUTED", createAutomationModel = Just "model", createAutomationReasoning = Just "", createAutomationBasePath = Just "", createAutomationVisualDescription = Just "", createAutomationMemoryStrategy = Just "", createAutomationSkipFirstRun = Just False, createAutomationWorkstreamId = Just "", createAutomationSource = Just AutomationFromApi, createAutomationSkills = Just [], createAutomationPrivacy = Just AutomationPrivate, createAutomationSessionPrivacy = Just AutomationSessionTeam, createAutomationPaused = Just False, createAutomationTags = Just [], createAutomationAdditionalFields = KeyMap.fromList ["id" .= String "wrong", "future" .= False]}

forkParams :: ForkAutomationParams
forkParams = ForkAutomationParams "new-id" "Fork" (Just "") "0 * * * *" (Just []) (Just "model") (Just "") "NOT EXECUTED" "source-id" "new-directory" (Just []) (KeyMap.singleton "future" (Bool False))

modelParams :: UpdateAutomationModelParams
modelParams = UpdateAutomationModelParams (address {automationAddressAdditionalFields = KeyMap.insert "model" (String "wrong") (automationAddressAdditionalFields address)}) Nothing

privacyParams :: UpdateAutomationPrivacyParams
privacyParams = UpdateAutomationPrivacyParams address AutomationOrganization (Just creator) (Just AutomationSessionTeam)

promptParams :: UpdateAutomationPromptParams
promptParams = UpdateAutomationPromptParams address ""

scheduleParams :: UpdateAutomationScheduleParams
scheduleParams = UpdateAutomationScheduleParams address "not interpreted"

configuration :: AutomationConfiguration
configuration = (defaultAutomationConfiguration "Automation" "0 * * * *" "NOT EXECUTED") {configuredAutomationDescription = Just "", configuredAutomationModel = Just "model", configuredAutomationReasoning = Just "", configuredAutomationTags = Just [], configuredAutomationPrivacy = Just AutomationPrivate, configuredAutomationSessionPrivacy = Just AutomationSessionTeam, configuredAutomationPaused = Just False, configuredAutomationWorkingDirectory = Just "", configuredAutomationWorkstreamId = Just ""}

applyParams :: ApplyAutomationConfigParams
applyParams = ApplyAutomationConfigParams "stable-id" (Just "") configuration (KeyMap.fromList ["name" .= String "wrong", "future" .= False])

updateParams :: UpdateAutomationParams
updateParams = UpdateAutomationParams (address {automationAddressAdditionalFields = KeyMap.insert "prompt" (String "wrong") (automationAddressAdditionalFields address)}) configuration

highCalls :: Daemon.DaemonConnection -> [IO Value]
highCalls connection = [toJSON <$> Daemon.createAutomation connection createParams, toJSON <$> Daemon.forkAutomation connection forkParams, toJSON <$> Daemon.updateAutomationModel connection modelParams, toJSON <$> Daemon.updateAutomationPrivacy connection privacyParams, toJSON <$> Daemon.updateAutomationPrompt connection promptParams, toJSON <$> Daemon.updateAutomationSchedule connection scheduleParams, toJSON <$> Daemon.applyAutomationConfig connection applyParams, toJSON <$> Daemon.updateAutomation connection updateParams]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel configured = [toJSON <$> Client.createDaemonAutomation channel configured createParams, toJSON <$> Client.forkDaemonAutomation channel configured forkParams, toJSON <$> Client.updateDaemonAutomationModel channel configured modelParams, toJSON <$> Client.updateDaemonAutomationPrivacy channel configured privacyParams, toJSON <$> Client.updateDaemonAutomationPrompt channel configured promptParams, toJSON <$> Client.updateDaemonAutomationSchedule channel configured scheduleParams, toJSON <$> Client.applyDaemonAutomationConfig channel configured applyParams, toJSON <$> Client.updateDaemonAutomation channel configured updateParams]

privacyMethod :: Text
privacyMethod = "daemon.update_automation_privacy"

wireCases :: [(Text, Value, Value)]
wireCases = [("daemon.create_automation", createWire, creationResultWire), ("daemon.fork_automation", forkWire, creationResultWire), ("daemon.update_automation_model", modelWire, resultWire), (privacyMethod, privacyWire, resultWire), ("daemon.update_automation_prompt", promptWire, resultWire), ("daemon.update_automation_schedule", scheduleWire, resultWire), ("daemon.apply_automation_config", applyWire, applyResultWire), ("daemon.update_automation", updateWire, resultWire)]

data Mode = Normal | Rejected | Malformed | HeldPrivacy deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withConfigPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withConfigPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      KeyMap.lookup "factoryProtocolVersion" frame @?= if field "method" frame == String privacyMethod then Nothing else Just (String "1.201.1")
      field "factoryApiVersion" frame @?= String "1.0.0"
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
            if mode == HeldPrivacy && method == privacyMethod
              then putMVar ready ()
              else case mode of
                Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Configuration rejected"]]
                Malformed -> reply connection request Null
                _ -> reply connection request result
          _ -> assertFailure "Unexpected configuration or implicit session request"
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

deleteField :: Key -> Value -> Value
deleteField key = Object . KeyMap.delete key . objectFields

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decodeValue @a value >>= (@?= value) . toJSON

createWire, forkWire, addressWire, modelWire, privacyWire, promptWire, scheduleWire, configWire, applyWire, updateWire, creationResultWire, resultWire, applyResultWire :: Value
createWire = object ["id" .= String "directory-id", "uuid" .= String "backend-id", "name" .= String "Automation", "description" .= String "", "instructions" .= String "NOT EXECUTED", "schedule" .= String "0 * * * *", "model" .= String "model", "reasoningEffort" .= String "", "basePath" .= String "", "visualDescription" .= String "", "memoryStrategy" .= String "", "skipFirstRun" .= False, "workstreamId" .= String "", "creationSource" .= String "api", "skills" .= ([] :: [Value]), "privacyLevel" .= String "private", "sessionPrivacy" .= String "team", "paused" .= False, "tags" .= ([] :: [Value]), "future" .= False]
forkWire = object ["automationId" .= String "new-id", "name" .= String "Fork", "description" .= String "", "schedule" .= String "0 * * * *", "tags" .= ([] :: [Value]), "model" .= String "model", "reasoningEffort" .= String "", "prompt" .= String "NOT EXECUTED", "forkedFrom" .= String "source-id", "localDirName" .= String "new-directory", "skills" .= ([] :: [Value]), "future" .= False]
addressWire = object ["automationId" .= String "stable-id", "automationDirName" .= String "directory-id", "basePath" .= String "", "future" .= False]
modelWire = insertField "model" Null addressWire
privacyWire = insertField "sessionPrivacy" (String "team") (insertField "createdBy" (object ["name" .= String "creator", "email" .= String "", "avatarUrl" .= String ""]) (insertField "privacyLevel" (String "organization") addressWire))
promptWire = insertField "prompt" (String "") addressWire
scheduleWire = insertField "schedule" (String "not interpreted") addressWire
configWire = object ["name" .= String "Automation", "description" .= String "", "schedule" .= String "0 * * * *", "model" .= String "model", "reasoningEffort" .= String "", "prompt" .= String "NOT EXECUTED", "tags" .= ([] :: [Value]), "privacyLevel" .= String "private", "sessionPrivacy" .= String "team", "paused" .= False, "workingDirectory" .= String "", "workstreamId" .= String ""]
applyWire = Object (KeyMap.union (objectFields configWire) (objectFields (object ["automationId" .= String "stable-id", "basePath" .= String "", "future" .= False])))
updateWire = Object (KeyMap.union (objectFields configWire) (objectFields addressWire))
creationResultWire = object ["success" .= True, "automationId" .= String "daemon-reported-id"]
resultWire = object ["success" .= False, "error" .= String ""]
applyResultWire = object ["success" .= False, "error" .= String "", "reason" .= String "local-file-unavailable"]
