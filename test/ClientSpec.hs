{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (clientTests) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (atomically, readTQueue, tryReadTQueue)
import Control.Exception (try)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Factory.Droid.Client
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Control (ChangeWorkingDirectoryParams (..))
import Factory.Droid.Schema.Local
import Factory.Droid.Schema.Metadata (TraceContextMeta (..))
import Factory.Droid.Schema.Models (ListModelsOptions (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Transport.Process (receiveObject, sendObject)
import ProcessSpec (bounded, withPeer)
import ProtocolSpec (feed, reply, withMemory)
import SchemaTest (rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

clientTests :: Value -> Value -> TestTree
clientTests shared local =
  testGroup
    "Typed local operations"
    [ binding "AddUserMessage" (Proxy @AddUserMessageRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" (object ["text" .= String "hello\nسلام"]) accepted addUserMessage,
      binding "AppendMessages" (Proxy @AppendMessagesRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" (object ["messages" .= [message]]) empty appendMessages,
      binding "AuthenticateMcpServer" (Proxy @AuthenticateMcpServerRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" server success authenticateMcpServer,
      binding "CancelMcpAuth" (Proxy @CancelMcpAuthRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" server success cancelMcpAuth,
      binding "ChangeWorkingDirectory" (Proxy @ChangeWorkingDirectoryRequest) (Proxy @ChangeWorkingDirectoryResponse) "ChangeWorkingDirectoryResponseSchema" (object ["workingDirectory" .= String "fixture-path"]) (object ["resolvedPath" .= String "resolved-path"]) changeWorkingDirectory,
      binding "ClearMcpAuth" (Proxy @ClearMcpAuthRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" server success clearMcpAuth,
      binding "CloseSession" (Proxy @CloseSessionRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" empty accepted closeSession,
      binding "CompactSession" (Proxy @CompactSessionRequest) (Proxy @CompactSessionResponse) "CompactSessionResponseSchema" empty (object ["newSessionId" .= String "new", "removedCount" .= Number 2.5]) compactSession,
      binding "ExecuteRewind" (Proxy @ExecuteRewindRequest) (Proxy @ExecuteRewindResponse) "ExecuteRewindResponseSchema" rewindParams rewindResult executeRewind,
      binding "ForkSession" (Proxy @ForkSessionRequest) (Proxy @ForkSessionResponse) "ForkSessionResponseSchema" empty (object ["newSessionId" .= String "fork"]) forkSession,
      binding "GetContextBreakdown" (Proxy @GetContextBreakdownRequest) (Proxy @GetContextBreakdownResponse) "GetContextBreakdownResponseSchema" empty breakdown getContextBreakdown,
      binding "GetContextStats" (Proxy @GetContextStatsRequest) (Proxy @GetContextStatsResponse) "GetContextStatsResponseSchema" empty stats getContextStats,
      binding "GetRewindInfo" (Proxy @GetRewindInfoRequest) (Proxy @GetRewindInfoResponse) "GetRewindInfoResponseSchema" (object ["sessionId" .= String "session", "messageId" .= String "message"]) (object ["availableFiles" .= emptyList, "createdFiles" .= emptyList, "evictedFiles" .= emptyList]) getRewindInfo,
      binding "InterruptSession" (Proxy @InterruptSessionRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" empty accepted interruptSession,
      binding "KillWorkerSession" (Proxy @KillWorkerSessionRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" (object ["workerSessionId" .= String "worker"]) empty killWorkerSession,
      binding "ListCommands" (Proxy @ListCommandsRequest) (Proxy @ListCommandsResponse) "ListCommandsResponseSchema" empty (object ["commands" .= emptyList]) listCommands,
      binding "ListMcpRegistry" (Proxy @ListMcpRegistryRequest) (Proxy @ListMcpRegistryResponse) "ListMcpRegistryResponseSchema" empty (object ["servers" .= emptyList]) listMcpRegistry,
      binding "ListMcpServers" (Proxy @ListMcpServersRequest) (Proxy @ListMcpServersResponse) "ListMcpServersResponseSchema" empty (object ["servers" .= emptyList, "summary" .= object ["total" .= Number 0, "connected" .= Number 0, "connecting" .= Number 0, "failed" .= Number 0]]) listMcpServers,
      binding "ListMcpTools" (Proxy @ListMcpToolsRequest) (Proxy @ListMcpToolsResponse) "ListMcpToolsResponseSchema" empty (object ["tools" .= emptyList]) listMcpTools,
      binding "ListModels" (Proxy @ListModelsRequest) (Proxy @ListModelsResponse) "ListModelsResponseSchema" (object ["includeDisabled" .= False]) (object ["models" .= emptyList]) listModels,
      binding "ListSkills" (Proxy @ListSkillsRequest) (Proxy @ListSkillsResponse) "ListSkillsResponseSchema" empty (object ["skills" .= emptyList]) listSkills,
      binding "ListTools" (Proxy @ListToolsRequest) (Proxy @ListToolsResponse) "ListToolsResponseSchema" (object ["specModeModelId" .= Null, "depth" .= Number 0]) (object ["tools" .= emptyList]) listTools,
      binding "RemoveMcpServer" (Proxy @RemoveMcpServerRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["serverName" .= String "fixture", "settingsLevel" .= String "user"]) success removeMcpServer,
      binding "RenameSession" (Proxy @RenameSessionRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["title" .= String ""]) success renameSession,
      binding "ResolveQueuedUserMessage" (Proxy @ResolveQueuedUserMessageRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" (object ["requestId" .= String "queued", "action" .= String "delete"]) empty resolveQueuedUserMessage,
      binding "SetSkillDisabled" (Proxy @SetSkillDisabledRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["skillName" .= String "fixture", "disabled" .= False]) success setSkillDisabled,
      binding "SubmitBugReport" (Proxy @SubmitBugReportRequest) (Proxy @SubmitBugReportResponse) "SubmitBugReportResponseSchema" (object ["userComment" .= String "fixture comment"]) (object ["bugReportId" .= String "report"]) submitBugReport,
      binding "SubmitMcpAuthCode" (Proxy @SubmitMcpAuthCodeRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["serverName" .= String "fixture", "code" .= String "not-a-real-code", "state" .= String "fixture-state"]) success submitMcpAuthCode,
      binding "SubmitMcpAuthError" (Proxy @SubmitMcpAuthErrorRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["serverName" .= String "fixture", "error" .= String "fixture-error", "state" .= String "fixture-state"]) success submitMcpAuthError,
      binding "ToggleMcpServer" (Proxy @ToggleMcpServerRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["serverName" .= String "fixture", "settingsLevel" .= String "user", "enabled" .= False]) success toggleMcpServer,
      binding "ToggleMcpTool" (Proxy @ToggleMcpToolRequest) (Proxy @SuccessResultResponse) "SuccessResultResponseSchema" (object ["serverName" .= String "fixture", "toolName" .= String "tool", "enabled" .= False]) success toggleMcpTool,
      binding "UpdateSessionSettings" (Proxy @UpdateSessionSettingsRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" (object ["modelId" .= String "model", "specModeModelId" .= Null]) accepted updateSessionSettings,
      binding "WarmupCache" (Proxy @WarmupCacheRequest) (Proxy @EmptyObjectResponse) "EmptyObjectResponseSchema" empty empty warmupCache,
      testCase "native peer verifies model discovery and directory-change bindings" $
        bounded $
          withPeer "client-bindings" 4096 $ \process ->
            withRpcChannel (sendObject process) (receiveObject process) $ \channel -> do
              models <- listModels channel (options {callRequestId = "models"}) (ListModelsOptions (Just False) mempty)
              toJSON models @?= object ["models" .= emptyList]
              changed <- changeWorkingDirectory channel (options {callRequestId = "directory"}) (ChangeWorkingDirectoryParams "/fixture/current" mempty)
              toJSON changed @?= object ["resolvedPath" .= String "/fixture/resolved"],
      testCase "required typed parameters may be explicitly null only when their type permits it" $ do
        let request = MethodRequest @"fixture.nullable" "id" (Nothing :: Maybe Bool) mempty
            expected = object ["type" .= String "request", "id" .= String "id", "method" .= String "fixture.nullable", "params" .= Null]
        toJSON request @?= expected
        fromJSON expected @?= Success request
        baseRequestParams (eraseMethodRequest request) @?= Just Null
        rejects (Proxy @(MethodRequest "fixture.nullable" (Maybe Bool))) (object ["type" .= String "request", "id" .= String "id", "method" .= String "fixture.nullable"]),
      testCase "call envelope cannot inject reserved request fields or replace its metadata" $ bounded $ withMemory $ \channel incoming sent -> do
        let meta = TraceContextMeta (Just "caller-trace") Nothing Nothing mempty
            extras = KeyMap.fromList ["id" .= String "injected", "method" .= String "wrong", "params" .= Null, "type" .= String "response", "_meta" .= Null, "factoryProtocolVersion" .= String "wrong", "future" .= Number 7]
            configured = options {callEnvelope = WithEnvelope (Just "1.205.0") (Just meta) extras}
        withAsync (listModels channel configured (ListModelsOptions Nothing mempty)) $ \worker -> do
          sentRequest <- atomically (readTQueue sent)
          KeyMap.lookup "id" sentRequest @?= Just (String "request-id")
          KeyMap.lookup "method" sentRequest @?= Just (String "droid.list_models")
          KeyMap.lookup "type" sentRequest @?= Just (String "request")
          KeyMap.lookup "params" sentRequest @?= Just empty
          KeyMap.lookup "_meta" sentRequest @?= Just (toJSON meta)
          KeyMap.lookup "factoryProtocolVersion" sentRequest @?= Just (String "1.205.0")
          KeyMap.lookup "future" sentRequest @?= Just (Number 7)
          feed incoming (reply "request-id" (object ["models" .= emptyList]))
          voidResult <- wait worker
          toJSON voidResult @?= object ["models" .= emptyList],
      testCase "metadata and protocol signals are not silently supplied when caller omits them" $ bounded $ withMemory $ \channel incoming sent ->
        withAsync (warmupCache channel (options {callEnvelope = WithEnvelope Nothing Nothing mempty}) mempty) $ \worker -> do
          sentRequest <- atomically (readTQueue sent)
          KeyMap.lookup "factoryProtocolVersion" sentRequest @?= Nothing
          KeyMap.lookup "_meta" sentRequest @?= Nothing
          KeyMap.lookup "params" sentRequest @?= Just empty
          feed incoming (reply "request-id" empty)
          wait worker >>= (@?= mempty),
      testCase "zero deadline is applied before any operation send" $ bounded $ withMemory $ \channel _ sent -> do
        result <- try @RpcChannelError (warmupCache channel (options {callTimeoutMicros = Just 0}) mempty)
        result @?= Left RpcRequestTimedOut
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "call configuration and method request Show redact contents" $ do
        show (options {callRequestId = "private-id", callEnvelope = WithEnvelope Nothing Nothing (KeyMap.singleton "private" (String "value"))}) @?= "CallOptions <redacted>"
        show (MethodRequest @"private-method" "private-id" (String "private-params") mempty) @?= "MethodRequest <redacted>"
    ]
  where
    binding :: forall request response params result. (FromJSON request, ToJSON request, FromJSON response, ToJSON response, FromJSON params, ToJSON result) => String -> Proxy request -> Proxy response -> Key -> Value -> Value -> (RpcChannel -> CallOptions -> params -> IO result) -> TestTree
    binding name requestProxy responseProxy responseName params result invoke =
      testGroup
        name
        [ testCase "typed aliases follow declared method and response links" $ do
            fields <- requestFields
            method <- methodLiteral
            roundTrip requestProxy (Object fields)
            (definition >>= schemaAt ["x-factory-response"]) @?= Right (toJSON responseName)
            responseDefinition <- either assertFailure pure (schemaAt ["definitions", responseName] (if responseName `elem` ["EmptyObjectResponseSchema", "SuccessResultResponseSchema"] then shared else local))
            successBranch <- either assertFailure pure (schemaAt ["anyOf"] responseDefinition >>= schemaIndex 0 >>= schemaAt ["allOf"] >>= schemaIndex 1)
            schemaAt ["required"] successBranch @?= Right (toJSON (["type", "id", "result"] :: [Key]))
            roundTrip responseProxy (Object (reply "request-id" result))
            Just method @?= KeyMap.lookup "method" fields,
          testCase "named request decoder enforces literal and required parameter presence" $ do
            fields <- requestFields
            forM_ ["jsonrpc", "factoryApiVersion", "type", "id", "method", "params"] $ \key -> rejects requestProxy (Object (KeyMap.delete key fields))
            forM_ ["method", "type"] $ \key -> rejects requestProxy (Object (KeyMap.insert key (String "different") fields))
            rejects requestProxy (Object (KeyMap.insert "params" Null fields))
            let extra = KeyMap.insert "future" (Number 4) fields
            roundTrip requestProxy (Object extra),
          testCase "typed call sends exact schema request and returns decoded result" $ bounded $ withMemory $ \channel incoming sent -> do
            parameterValue <- decode params
            expected <- requestFields
            withAsync (invoke channel options parameterValue) $ \worker -> do
              atomically (readTQueue sent) >>= (@?= expected)
              feed incoming (reply "request-id" result)
              actual <- wait worker
              toJSON actual @?= result,
          testCase "errors precede result validation and invalid success results are rejected" $ bounded $ withMemory $ \channel incoming sent -> do
            parameterValue <- decode params
            let failure = KeyMap.insert "error" (toJSON remoteError) (reply "request-id" (Bool False))
            roundTrip responseProxy (Object failure)
            withAsync (try @RpcResultError (invoke channel options parameterValue)) $ \worker -> do
              _ <- atomically (readTQueue sent)
              feed incoming failure
              wait worker >>= \case
                Left err -> err @?= RpcRemoteFailure remoteError
                Right _ -> assertFailure "Remote error was accepted as success"
            withAsync (try @RpcResultError (invoke channel (options {callRequestId = "invalid-result"}) parameterValue)) $ \worker -> do
              _ <- atomically (readTQueue sent)
              feed incoming (reply "invalid-result" (Bool False))
              wait worker >>= \case
                Left err -> err @?= RpcInvalidResult
                Right _ -> assertFailure "Invalid typed result was accepted"
        ]
      where
        definition = schemaAt ["definitions", fromString (name <> "RequestSchema")] local
        methodLiteral = either assertFailure pure (definition >>= schemaAt ["allOf"] >>= schemaIndex 1 >>= schemaAt ["properties", "method", "const"])
        requestFields = do
          method <- methodLiteral
          pure (KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.205.0", "type" .= String "request", "id" .= String "request-id", "method" .= method, "params" .= params])

options :: CallOptions
options = CallOptions "request-id" (WithEnvelope (Just "1.205.0") Nothing mempty) Nothing

remoteError :: JsonRpcError
remoteError = JsonRpcError RpcConflict "fixture conflict" Nothing mempty

decode :: (FromJSON a) => Value -> IO a
decode value = case fromJSON value of
  Error _ -> assertFailure "Invalid typed fixture"
  Success parsed -> pure parsed

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = do
  parsed <- decode @a value
  toJSON parsed @?= value

empty, emptyList, success, accepted, server, message, rewindParams, rewindResult, stats, breakdown :: Value
empty = Object mempty
emptyList = Array mempty
success = object ["success" .= False]
accepted = object ["accepted" .= True]
server = object ["serverName" .= String "fixture"]
message = object ["id" .= String "message", "role" .= String "assistant", "content" .= emptyList, "createdAt" .= Number 0, "updatedAt" .= Number 1, "visibility" .= String "user_only"]
rewindParams = object ["sessionId" .= String "session", "messageId" .= String "message", "filesToRestore" .= emptyList, "filesToDelete" .= emptyList, "forkTitle" .= String ""]
rewindResult = object ["newSessionId" .= String "new", "restoredCount" .= Number 0, "deletedCount" .= Number 0, "failedRestoreCount" .= Number 0, "failedDeleteCount" .= Number 0]
stats = object ["used" .= Number 1.5, "remaining" .= Number 2.5, "limit" .= Number 4, "accuracy" .= String "estimated", "updatedAt" .= String "fixture"]
breakdown = object ["modelId" .= String "model", "modelDisplayName" .= String "Model", "contextBudget" .= Number 10, "usedTokens" .= Number 2, "freeTokens" .= Number 8, "categories" .= emptyList, "skills" .= emptyList, "mcpServers" .= emptyList, "droids" .= emptyList]
