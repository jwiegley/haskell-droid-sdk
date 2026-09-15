{-# LANGUAGE OverloadedStrings #-}

module DaemonCustomModelSpec (customModelTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Settings
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

customModelTests :: TestTree
customModelTests =
  testGroup
    "Daemon custom models"
    [ testCase "upsert fields distinguish omission, reset, empty and false without normalizing indices" $ do
        create <- decodeValue @UpsertCustomModelParams createWire
        toJSON create @?= createWire
        edit <- decodeValue @UpsertCustomModelParams editWire
        toJSON edit @?= editWire
        upsertCustomModelApiKey edit @?= Nothing
        upsertCustomModelMaxOutputTokens edit @?= Just Nothing
        upsertCustomModelNoImageSupport edit @?= Just (Just False)
        upsertCustomModelIndex edit @?= Just 1
        let fractionalWire = object ["model" .= String "m", "provider" .= String "p", "rawIndex" .= Number 1.5]
        fractional <- decodeValue @UpsertCustomModelParams fractionalWire
        toJSON fractional @?= fractionalWire
        let minimal = defaultUpsertCustomModelParams (upsertCustomModelId create) (upsertCustomModelProvider create)
        toJSON minimal @?= object ["model" .= String "created", "provider" .= String "openai"]
        explicitEmpty <- decodeValue @UpsertCustomModelParams (object ["model" .= String " ", "provider" .= String "\t", "baseUrl" .= String "", "apiKey" .= String "", "maxOutputTokens" .= Number (-0.25), "noImageSupport" .= Null])
        upsertCustomModelApiKey explicitEmpty @?= Just ""
        upsertCustomModelMaxOutputTokens explicitEmpty @?= Just (Just (-0.25))
        upsertCustomModelNoImageSupport explicitEmpty @?= Just Nothing
        show create @?= "UpsertCustomModelParams <redacted>"
        forM_ ["rawIndex", "expectedModel", "model", "displayName", "provider", "baseUrl", "apiKey", "maxOutputTokens", "noImageSupport"] $ \key -> toJSON (edit {upsertCustomModelAdditionalFields = KeyMap.insert key (String "INJECTED") (upsertCustomModelAdditionalFields edit)}) @?= editWire,
      testCase "required nonempty inputs and nullable overrides follow their separate contracts" $ do
        forM_ [object ["model" .= String "", "provider" .= String "openai"], object ["model" .= String "model", "provider" .= String ""], object ["model" .= Null, "provider" .= String "openai"]] $ rejects (Proxy @UpsertCustomModelParams)
        forM_ ["rawIndex", "expectedModel", "displayName", "baseUrl", "apiKey"] $ \key -> rejects (Proxy @UpsertCustomModelParams) (object ["model" .= String "model", "provider" .= String "openai", key .= Null])
        rejects (Proxy @UpsertCustomModelParams) (object ["model" .= String "model", "provider" .= String "openai", "maxOutputTokens" .= String "wrong"])
        deletion <- decodeValue @DeleteCustomModelParams (object ["rawIndex" .= Number (-0.5), "expectedModel" .= String ""])
        toJSON deletion @?= object ["rawIndex" .= Number (-0.5), "expectedModel" .= String ""]
        rejects (Proxy @DeleteCustomModelParams) (object ["rawIndex" .= Number 0])
        rejects (Proxy @DeleteCustomModelParams) (object ["rawIndex" .= Number 0, "expectedModel" .= Null])
        show deletion @?= "DeleteCustomModelParams <redacted>",
      testCase "invalid configuration rows, optional summary data and false results remain observable" $ do
        summary <- decodeValue @CustomModelSummary (Object summaryFields)
        customModelIsValid summary @?= False
        customModelId summary @?= ""
        customModelRawIndex summary @?= -0.5
        toJSON summary @?= Object summaryFields
        show summary @?= "CustomModelSummary <redacted>"
        forM_ (KeyMap.keys summaryFields) $ \key -> do
          rejects (Proxy @CustomModelSummary) (Object (KeyMap.insert key Null summaryFields))
          toJSON (summary {customModelAdditionalFields = KeyMap.singleton key Null}) @?= Object summaryFields
        result <- decodeValue @UpdateCustomModelsResult (mutationWire False [Object summaryFields])
        customModelsUpdateSuccess result @?= False
        updatedCustomModels result @?= [summary]
        show result @?= "UpdateCustomModelsResult <redacted>"
        listing <- decodeValue @ListCustomModelsResult (listWire [])
        listedCustomModels listing @?= []
        show listing @?= "ListCustomModelsResult <redacted>"
        rejects (Proxy @ListCustomModelsResult) (listWire [object []])
        rejects (Proxy @UpdateCustomModelsResult) (object ["success" .= True, "models" .= Null]),
      testCase "global list/create/edit/delete preserve guards, key omission and daemon results" $ bounded $ do
        trace <- newIORef []
        create <- decodeValue @UpsertCustomModelParams createWire
        edit <- decodeValue @UpsertCustomModelParams editWire
        let steps = [Step "daemon.list_custom_models" (object []) (Reply (listWire [invalidSummary])), Step "daemon.upsert_custom_model" createWire (Reply (mutationWire True [createdSummary])), Step "daemon.upsert_custom_model" editWire (Reply (mutationWire True [editedSummary])), Step "daemon.delete_custom_model" deleteWire (Reply (mutationWire False [editedSummary])), Step "daemon.list_custom_models" (object []) (Reply (listWire [editedSummary]))]
        withModelPeer trace steps $ \target -> Daemon.withConnection (options target) $ \connection -> do
          Daemon.listCustomModels connection >>= (@?= listWire [invalidSummary]) . toJSON
          Daemon.upsertCustomModel connection create >>= (@?= mutationWire True [createdSummary]) . toJSON
          edited <- Daemon.upsertCustomModel connection edit
          toJSON edited @?= mutationWire True [editedSummary]
          map customModelHasApiKey (updatedCustomModels edited) @?= [True]
          deleted <- Daemon.deleteCustomModel connection (DeleteCustomModelParams 1 "edited" mempty)
          customModelsUpdateSuccess deleted @?= False
          toJSON deleted @?= mutationWire False [editedSummary]
          Daemon.listCustomModels connection >>= (@?= listWire [editedSummary]) . toJSON
        frames <- readIORef trace
        map (field "method") frames @?= map String ["daemon.authenticate", "daemon.list_custom_models", "daemon.upsert_custom_model", "daemon.upsert_custom_model", "daemon.delete_custom_model", "daemon.list_custom_models"],
      testCase "guard rejection and malformed results are explicit without retry or connection retirement" $ bounded $ do
        trace <- newIORef []
        edit <- decodeValue @UpsertCustomModelParams editWire
        let rejected = JsonRpcError RpcInvalidParams "Stale model guard" Nothing mempty
            steps = [Step "daemon.upsert_custom_model" editWire (Reject rejected), Step "daemon.delete_custom_model" deleteWire (Reply (object ["success" .= True, "models" .= Null])), Step "daemon.list_custom_models" (object []) (Reply (listWire [object []])), Step "daemon.list_custom_models" (object []) (Reply (listWire []))]
        withModelPeer trace steps $ \target -> Daemon.withConnection (options target) $ \connection -> do
          try @RpcResultError (Daemon.upsertCustomModel connection edit) >>= (@?= Left (RpcRemoteFailure rejected))
          try @RpcResultError (Daemon.deleteCustomModel connection (DeleteCustomModelParams 1 "edited" mempty)) >>= (@?= Left RpcInvalidResult)
          try @RpcResultError (Daemon.listCustomModels connection) >>= (@?= Left RpcInvalidResult)
          Daemon.listCustomModels connection >>= (@?= []) . listedCustomModels,
      testCase "cancelled model writes preserve async identity and do not claim remote rollback" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        edit <- decodeValue @UpsertCustomModelParams editWire
        withModelPeer trace [Step "daemon.upsert_custom_model" editWire (Hold ready), Step "daemon.list_custom_models" (object []) (Reply (listWire [editedSummary]))] $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.upsertCustomModel connection edit) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled model write returned"
          Daemon.listCustomModels connection >>= (@?= listWire [editedSummary]) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.upsert_custom_model", "daemon.list_custom_models"]) . map (field "method"),
      testCase "low-level model calls retain caller deadlines and send nothing with zero budget" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        create <- decodeValue @UpsertCustomModelParams createWire
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "zero" (WithEnvelope Nothing Nothing mempty) (Just 0)
          try @RpcChannelError (Client.listDaemonCustomModels channel configured mempty) >>= (@?= Left RpcRequestTimedOut)
          try @RpcChannelError (Client.upsertDaemonCustomModel channel configured create) >>= (@?= Left RpcRequestTimedOut)
          try @RpcChannelError (Client.deleteDaemonCustomModel channel configured (DeleteCustomModelParams 1 "edited" mempty)) >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

data Step = Step Text Value Reply

data Reply = Reply Value | Reject JsonRpcError | Hold (MVar ())

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withModelPeer :: IORef [Object] -> [Step] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withModelPeer trace steps = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC frame")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      forM_ steps $ \(Step method params response) -> do
        request <- readFrame connection
        field "method" request @?= String method
        field "params" request @?= params
        case response of
          Reply value -> reply connection request value
          Reject err -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= err]
          Hold ready -> putMVar ready ()
      forever (readFrame connection >> assertFailure "Unexpected request after model script")
    reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

createWire, editWire, deleteWire :: Value
createWire = object ["model" .= String "created", "provider" .= String "openai", "baseUrl" .= String "https://provider.invalid/v1", "apiKey" .= String "OFFLINE_SECRET", "future" .= False]
editWire = object ["rawIndex" .= Number 1, "expectedModel" .= String "created", "model" .= String "edited", "provider" .= String "openai", "displayName" .= String "", "baseUrl" .= String "https://provider.invalid/v1", "maxOutputTokens" .= Null, "noImageSupport" .= False, "future" .= False]
deleteWire = object ["rawIndex" .= Number 1, "expectedModel" .= String "edited"]

summaryFields :: Object
summaryFields = KeyMap.fromList ["rawIndex" .= Number (-0.5), "model" .= String "", "displayName" .= String "", "provider" .= String "", "baseUrl" .= String "opaque", "hasApiKey" .= False, "apiKeyMask" .= String "masked", "maxOutputTokens" .= Number 0.25, "noImageSupport" .= False, "hasBedrockConfig" .= False, "isValid" .= False]

invalidSummary, createdSummary, editedSummary :: Value
invalidSummary = object ["rawIndex" .= Number 0, "model" .= String "", "provider" .= String "", "hasApiKey" .= False, "hasBedrockConfig" .= False, "isValid" .= False]
createdSummary = object ["rawIndex" .= Number 1, "model" .= String "created", "provider" .= String "openai", "hasApiKey" .= True, "apiKeyMask" .= String "unchanged-mask", "hasBedrockConfig" .= False, "isValid" .= True]
editedSummary = object ["rawIndex" .= Number 1, "model" .= String "edited", "provider" .= String "openai", "hasApiKey" .= True, "apiKeyMask" .= String "unchanged-mask", "noImageSupport" .= False, "hasBedrockConfig" .= False, "isValid" .= True]

listWire :: [Value] -> Value
listWire models = object ["models" .= models]

mutationWire :: Bool -> [Value] -> Value
mutationWire success models = object ["success" .= success, "models" .= models]
