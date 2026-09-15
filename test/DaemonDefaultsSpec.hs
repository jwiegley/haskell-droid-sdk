{-# LANGUAGE OverloadedStrings #-}

module DaemonDefaultsSpec (defaultsTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Settings
import Factory.Droid.Schema.Models (ModelAvailability (..), ModelInfo (..))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (enumSchemaTest, rejects, schemaAt, schemaIndex)
import System.Directory (getCurrentDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

defaultsTests :: Value -> TestTree
defaultsTests schema = testGroup "Daemon default settings" (codecTests schema <> operationTests)

codecTests :: Value -> [TestTree]
codecTests schema =
  [ testCase "baseline request fields and report fields match the supplied schemas" $ do
      let updateShape = schemaAt ["definitions", "DaemonUpdateSessionDefaultsRequestSchema", "allOf"] schema >>= schemaIndex 1 >>= schemaAt ["properties", "params", "properties"]
          reportShape = schemaAt ["definitions", "DaemonGetDefaultSettingsResultSchema", "properties"] schema
          newer = ["enableOneHourAnthropicCaching", "worktreeAutoDeleteLimit"]
      fmap (sort . filter (`notElem` newer) . keys) updateShape @?= Right (sort (KeyMap.keys updateWire))
      fmap (sort . filter (`notElem` newer) . keys) reportShape @?= Right (sort (KeyMap.keys defaultsWire)),
    testCase "all baseline update fields retain null, false, empty and exact numeric values" $ do
      update <- decodeValue @UpdateSessionDefaultsParams (Object updateWire)
      toJSON update @?= Object updateWire
      updateDefaultsSpecModel update @?= Just Nothing
      updateDefaultsRunInWorktree update @?= Just (Just False)
      updateDefaultsWorktreeDirectory update @?= Just (Just "")
      updateDefaultsCompactionLimit update @?= Just (-1.25)
      updateDefaultsSubagentInheritTiers update @?= Just [LightSubagent, LightSubagent]
      forM_ (KeyMap.keys updateWire) $ \key -> toJSON (emptySessionDefaultsUpdate {updateDefaultsAdditionalFields = KeyMap.singleton key Null}) @?= object []
      let future = emptySessionDefaultsUpdate {updateDefaultsAdditionalFields = KeyMap.singleton "future" (object ["nested" .= Null])}
      fromJSON (toJSON future) @?= Success future,
    testCase "only the declared update fields accept null resets" $ do
      forM_ ["specModeModelId", "specModeReasoningEffort", "subagentAutonomyLevel", "specSaveDir", "missionOrchestratorModel", "missionOrchestratorReasoningEffort", "runInWorktree", "worktreeDirectory"] $ \key -> do
        value <- decodeValue @UpdateSessionDefaultsParams (object [key .= Null])
        toJSON value @?= object [key .= Null]
      forM_ ["modelId", "reasoningEffort", "interactionMode", "autonomyLevel", "compactionTokenLimit", "compactionTokenLimitPerModel", "compactionModel", "compactionThresholdCheckEnabled", "compactionModelMode", "cloudSessionSync", "subagentModelSettings", "subagentInheritTiers", "missionModelSettings"] $ \key -> rejects (Proxy @UpdateSessionDefaultsParams) (object [key .= Null])
      forM_ ["interactionMode", "autonomyLevel", "reasoningEffort", "subagentAutonomyLevel", "compactionModelMode"] $ \key -> rejects (Proxy @UpdateSessionDefaultsParams) (object [key .= String "future"]),
    testCase "complete report data round trips with reused model and mission contracts" $ do
      value <- decodeValue @DefaultSettings (Object defaultsWire)
      toJSON value @?= Object defaultsWire
      defaultsRunInWorktree value @?= Just False
      defaultsWorktreeDirectory value @?= Just ""
      defaultsCompactionLimit value @?= Just (-1.25)
      fmap (map modelAvailability) (defaultsAvailableModels value) @?= Just [ModelEnabled, ModelDisabled "policy"]
      show value @?= "DefaultSettings <redacted>"
      forM_ (KeyMap.keys defaultsWire) $ \key -> toJSON (value {defaultsAdditionalFields = KeyMap.singleton key Null}) @?= Object defaultsWire,
    testCase "fallbacks are field-local while strict neighboring fields still reject" $ do
      forM_ ["interactionMode", "autonomyLevel", "maxAutonomyLevel", "availableAutonomyLevels", "resolutionChain"] $ \key ->
        forM_ [Null, String "future", Number 1, Bool False] $ \bad -> do
          value <- decodeValue @DefaultSettings (object [key .= bad, "modelId" .= String "kept"])
          toJSON value @?= object ["modelId" .= String "kept"]
      forM_ ["modelId", "reasoningEffort", "autonomyMode", "runInWorktree", "worktreeDirectory", "cloudSessionSync", "specSavePresets", "availableModels", "missionSettings", "subagentModelSettings"] $ \key -> rejects (Proxy @DefaultSettings) (object [key .= Null])
      rejects (Proxy @DefaultSettings) (object ["interactionMode" .= String "future", "reasoningEffort" .= String "future"])
      value <- decodeValue @DefaultSettings (object ["availableAutonomyLevels" .= ([] :: [Value]), "resolutionChain" .= ([] :: [Value])])
      toJSON value @?= object ["availableAutonomyLevels" .= ([] :: [Value]), "resolutionChain" .= ([] :: [Value])],
    testCase "management drops invalid entries, normalizes present maps and preserves nullable sources" $ do
      let valid = object ["disabled" .= False, "source" .= Null, "folderPath" .= String ""]
      report <- decodeValue @DefaultSettings (object ["management" .= object ["modelId" .= valid, "reasoningEffort" .= object ["disabled" .= False], "future" .= valid, "subagent" .= Null, "mission" .= object ["workerModel" .= valid, "skipUserTesting" .= False]]])
      toJSON report @?= object ["management" .= object ["modelId" .= valid, "subagent" .= object [], "mission" .= object ["workerModel" .= valid]]]
      forM_ [Null, Bool True, Number 0, Array mempty] $ \bad -> do
        normalized <- decodeValue @DefaultSettings (object ["management" .= bad])
        toJSON normalized @?= object ["management" .= object []]
      info <- decodeValue @SettingsManagementInfo valid
      managementSource info @?= Nothing
      rejects (Proxy @SettingsManagementInfo) (object ["disabled" .= False])
      rejects (Proxy @SettingsManagementInfo) (object ["disabled" .= False, "source" .= String "future"]),
    testCase "one malformed resolution entry removes the whole report chain, not only that entry" $ do
      value <- decodeValue @DefaultSettings (object ["resolutionChain" .= [resolutionWire, object []], "worktreeDirectory" .= String "kept"])
      toJSON value @?= object ["worktreeDirectory" .= String "kept"]
      event <- decodeValue @SettingsResolutionEvent resolutionWire
      resolutionTimestamp event @?= "opaque time"
      toJSON event @?= resolutionWire
      rejects (Proxy @SettingsResolutionEvent) (object ["timestamp" .= String "time", "keys" .= ([] :: [Value]), "action" .= String "set", "source" .= object ["type" .= String "future"]]),
    testCase "model availability and partial nested settings do not hide invalid values" $ do
      forM_ [KeyMap.insert "disabledReason" Null modelWire, KeyMap.insert "disabled" (Bool True) modelWire] $ \model -> rejects (Proxy @DefaultSettings) (object ["availableModels" .= [Object model]])
      forM_ ["lightModel", "lightReasoningEffort", "mediumModel", "mediumReasoningEffort", "heavyModel", "heavyReasoningEffort"] $ \key -> rejects (Proxy @SubagentModelSettings) (object [key .= Null])
      rejects (Proxy @UpdateSessionDefaultsParams) (object ["subagentInheritTiers" .= [String "future"]])
      rejects (Proxy @UpdateSessionDefaultsParams) (object ["missionModelSettings" .= object ["skipUserTesting" .= Null]]),
    enumSchemaTest "resolution action" (resolutionSchema ["action", "enum"]) (Proxy @ResolutionAction),
    enumSchemaTest "resolution source type" (resolutionSchema ["source", "properties", "type", "enum"]) (Proxy @ResolutionSourceType)
  ]
  where
    keys (Object fields) = KeyMap.keys fields
    keys _ = []
    resolutionSchema path = schemaAt (["definitions", "DaemonGetDefaultSettingsResultSchema", "properties", "resolutionChain", "items", "properties"] <> path) schema

operationTests :: [TestTree]
operationTests =
  [ testCase "authenticated global defaults round trip without loading a session or inventing resolved state" $ bounded $ do
      trace <- newIORef []
      cwd <- getCurrentDirectory
      ready <- newEmptyMVar
      withDefaultsPeer trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
        initial <- Daemon.getDefaultSettings connection
        defaultsModel initial @?= Just "initial"
        update <- decodeValue @UpdateSessionDefaultsParams (Object updateWire)
        result <- Daemon.updateSessionDefaults connection update
        defaultsUpdateSuccess result @?= False
        defaultsModel (updatedDefaults result) @?= Just "resolved-by-daemon"
        next <- Daemon.getDefaultSettings connection
        next @?= updatedDefaults result
      getCurrentDirectory >>= (@?= cwd)
      frames <- readIORef trace
      map (field "method") frames @?= map String ["daemon.authenticate", "daemon.get_default_settings", "daemon.update_session_defaults", "daemon.get_default_settings"]
      map (field "params") (drop 1 frames) @?= [Object mempty, Object updateWire, Object mempty],
    testCase "remote errors and invalid defaults replies leave subsequent queries usable" $ bounded $ do
      trace <- newIORef []
      ready <- newEmptyMVar
      withDefaultsPeer trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
        result <- try @RpcResultError (Daemon.updateSessionDefaults connection (emptySessionDefaultsUpdate {updateDefaultsModel = Just "reject"}))
        case result of Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams; _ -> assertFailure "Missing remote rejection"
        bad <- try @RpcResultError (Daemon.updateSessionDefaults connection (emptySessionDefaultsUpdate {updateDefaultsModel = Just "malformed"}))
        bad @?= Left RpcInvalidResult
        Daemon.getDefaultSettings connection >>= (@?= Just "initial") . defaultsModel,
    testCase "cancelled default writes preserve async identity without claiming remote rollback" $ bounded $ do
      trace <- newIORef []
      ready <- newEmptyMVar
      withDefaultsPeer trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
        withAsync (Daemon.updateSessionDefaults connection (emptySessionDefaultsUpdate {updateDefaultsModel = Just "held"})) $ \pending -> do
          takeMVar ready
          cancel pending
          waitCatch pending >>= \case Left err -> fromException err @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled write returned"
        Daemon.getDefaultSettings connection >>= (@?= Just "initial") . defaultsModel
      readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.update_session_defaults", "daemon.get_default_settings"]) . map (field "method"),
    testCase "low-level default operations retain caller deadlines and do not send after zero budget" $ bounded $ do
      incoming <- newTQueueIO
      outgoing <- newTQueueIO
      withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
        let configured = Client.CallOptions "zero" (WithEnvelope Nothing Nothing mempty) (Just 0)
        result <- try @RpcChannelError (Client.getDaemonDefaultSettings channel configured mempty)
        result @?= Left RpcRequestTimedOut
        result2 <- try @RpcChannelError (Client.updateDaemonSessionDefaults channel configured emptySessionDefaultsUpdate)
        result2 @?= Left RpcRequestTimedOut
        atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
  ]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withDefaultsPeer :: IORef [Object] -> Control.Concurrent.MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withDefaultsPeer trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid client RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "apiKey" (parameters auth) @?= String "OFFLINE_ONLY"
      respond connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      current <- newIORef (object ["modelId" .= String "initial"])
      forever $ do
        frame <- readFrame connection
        case field "method" frame of
          String "daemon.get_default_settings" -> do
            field "params" frame @?= Object mempty
            readIORef current >>= respond connection frame
          String "daemon.update_session_defaults" -> case field "modelId" (parameters frame) of
            String "reject" -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Defaults rejected"]]
            String "malformed" -> respond connection frame (object ["success" .= True, "defaults" .= object ["reasoningEffort" .= String "future"]])
            String "held" -> putMVar ready ()
            _ -> do
              parameters frame @?= updateWire
              let next = object ["modelId" .= String "resolved-by-daemon", "runInWorktree" .= False]
              writeIORef current next
              respond connection frame (object ["success" .= False, "defaults" .= next])
          _ -> assertFailure "Unexpected request or implicit session lifecycle operation"

respond :: WS.Connection -> Object -> Value -> IO ()
respond connection request result = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

parameters :: Object -> Object
parameters value = case field "params" value of Object result -> result; _ -> mempty

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success decoded -> pure decoded; Error err -> assertFailure err

updateWire :: Object
updateWire = KeyMap.fromList ["modelId" .= String "", "reasoningEffort" .= String "low", "interactionMode" .= String "auto", "autonomyLevel" .= String "off", "specModeModelId" .= Null, "specModeReasoningEffort" .= String "low", "compactionTokenLimit" .= (-1.25 :: Double), "compactionTokenLimitPerModel" .= object ["model" .= (0.25 :: Double)], "compactionModel" .= String "current-model", "compactionThresholdCheckEnabled" .= False, "compactionModelMode" .= String "factory-default", "cloudSessionSync" .= False, "subagentModelSettings" .= object ["lightModel" .= String "", "heavyReasoningEffort" .= String "low"], "subagentInheritTiers" .= [String "light", String "light"], "subagentAutonomyLevel" .= String "inherit", "specSaveDir" .= String "", "missionOrchestratorModel" .= Null, "missionOrchestratorReasoningEffort" .= Null, "missionModelSettings" .= object ["workerModel" .= String "", "skipUserTesting" .= False], "runInWorktree" .= False, "worktreeDirectory" .= String ""]

defaultsWire :: Object
defaultsWire = KeyMap.fromList ["autonomyMode" .= String "normal", "interactionMode" .= String "auto", "autonomyLevel" .= String "off", "maxAutonomyLevel" .= String "high", "availableAutonomyLevels" .= [String "off", String "low"], "modelId" .= String "", "reasoningEffort" .= String "low", "specSaveDir" .= String "", "specModeModelId" .= String "", "specModeReasoningEffort" .= String "low", "compactionTokenLimit" .= (-1.25 :: Double), "compactionTokenLimitPerModel" .= object ["model" .= (0.25 :: Double)], "compactionModel" .= String "current-model", "compactionThresholdCheckEnabled" .= False, "compactionModelMode" .= String "factory-default", "cloudSessionSync" .= False, "runInWorktree" .= False, "worktreeDirectory" .= String "", "management" .= object ["modelId" .= object ["disabled" .= False, "source" .= Null]], "subagentAutonomyLevel" .= String "inherit", "missionOrchestratorModel" .= String "", "missionOrchestratorReasoningEffort" .= String "low", "missionSettings" .= object ["workerModel" .= String "", "skipUserTesting" .= False], "subagentModelSettings" .= object ["lightModel" .= String "", "heavyReasoningEffort" .= String "low"], "availableModels" .= [Object modelWire, Object (KeyMap.insert "disabled" (Bool True) (KeyMap.insert "disabledReason" (String "policy") modelWire))], "specSavePresets" .= object ["userFactoryDir" .= String "", "projectFactoryDir" .= String ""], "resolutionChain" .= [resolutionWire]]

modelWire :: Object
modelWire = KeyMap.fromList ["id" .= String "fixture", "displayName" .= String "Fixture", "shortDisplayName" .= String "F", "modelProvider" .= String "factory", "supportedReasoningEfforts" .= [String "low"], "defaultReasoningEffort" .= String "low", "isCustom" .= False, "disabled" .= False]

resolutionWire :: Value
resolutionWire = object ["timestamp" .= String "opaque time", "keys" .= [String "modelId", String "modelId"], "action" .= String "override", "source" .= object ["type" .= String "user", "filePath" .= String "", "flagName" .= String "", "key" .= String "", "orgId" .= String ""], "value" .= object ["secret-like-data" .= String "OFFLINE_ONLY"], "reason" .= String "", "location" .= object ["package" .= String "sdk", "file" .= String "settings", "function" .= String "resolve"]]
