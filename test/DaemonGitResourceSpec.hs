{-# LANGUAGE OverloadedStrings #-}

module DaemonGitResourceSpec (gitResourceTests) where

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
import Factory.Droid.Schema.Daemon.Git
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Schema.Session (SessionIdParams (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (enumSchemaTest, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

gitResourceTests :: Value -> TestTree
gitResourceTests schema =
  testGroup
    "Daemon publishing readiness and semantic diff"
    [ enumSchemaTest "readiness states" (warningSchema "state") (Proxy @MissionReadinessState),
      enumSchemaTest "readiness numeric levels" (warningSchema "level") (Proxy @MissionReadinessLevel),
      testCase "readiness preserves independent flags, nullable remote and optional warning" $ do
        roundTrip (Proxy @MissionReadinessResult) readinessWire
        result <- decodeValue @MissionReadinessResult (object ["isGitRepo" .= False, "hasRemote" .= False, "remoteUrl" .= Null, "isEmpty" .= False])
        readinessWarning result @?= Nothing
        readinessRemoteUrl result @?= Nothing
        rejects (Proxy @MissionReadinessResult) (object ["isGitRepo" .= False, "hasRemote" .= False, "isEmpty" .= False])
        rejects (Proxy @MissionReadinessResult) (insertField "warning" Null readinessWire)
        forM_ [Number 0, Number 6, Number 1.5, Null] $ \level -> rejects (Proxy @MissionReadinessWarning) (object ["state" .= String "low_score", "level" .= level])
        roundTrip (Proxy @MissionReadinessWarning) (object ["state" .= String "ok", "repoUrl" .= String ""]),
      testCase "publication and semantic inputs preserve raw empty/false values and extensions" $ do
        roundTrip (Proxy @GitCommitParams) commitWire
        roundTrip (Proxy @CreatePullRequestParams) createWire
        roundTrip (Proxy @CreatePullRequestResult) createdWire
        roundTrip (Proxy @SemanticDiffTarget) targetWire
        roundTrip (Proxy @SaveSemanticDiffParams) saveWire
        roundTrip (Proxy @GenerateSemanticDiffParams) generateWire
        roundTrip (Proxy @GenerateSemanticDiffResult) generatedWire
        toJSON (defaultCreatePullRequestParams "id" "" "") @?= object ["sessionId" .= String "id", "title" .= String "", "baseBranch" .= String ""]
        forM_ ["body", "draft", "linkedTicketIds", "linkedTicketUrls", "jiraIssueKeys", "linearIssueIds"] $ \key -> rejects (Proxy @CreatePullRequestParams) (insertField key Null createWire)
        forM_ ["commitHash", "modelId", "unstagedDiff"] $ \key -> rejects (Proxy @GenerateSemanticDiffParams) (insertField key Null generateWire)
        show createParams @?= "CreatePullRequestParams <redacted>"
        show generateParams @?= "GenerateSemanticDiffParams <redacted>",
      testCase "semantic cache misses retain required nulls rather than synthesized empty text" $ do
        miss <- decodeValue @SemanticDiffCacheResult cacheWire
        cachedSemanticContent miss @?= Nothing
        cachedSemanticCommitHash miss @?= Nothing
        cachedSemanticTruncated miss @?= False
        toJSON miss @?= cacheWire
        roundTrip (Proxy @SemanticDiffCacheResult) (object ["content" .= String "", "commitHash" .= String "", "truncated" .= False])
        rejects (Proxy @SemanticDiffCacheResult) (object ["commitHash" .= Null, "truncated" .= False])
        rejects (Proxy @SemanticDiffCacheResult) (object ["content" .= Null, "truncated" .= False])
        rejects (Proxy @SemanticDiffCacheResult) (insertField "truncated" Null cacheWire),
      testCase "all eight requests preserve results without automatic acknowledgement/publication/save/session work" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withResourcePeer Normal trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ (zip wireCases (highCalls connection)) $ \((_, _, expected), request) -> request >>= (@?= expected)
        frames <- readIORef trace
        map (field "method") frames @?= map String ("daemon.authenticate" : map (\(method, _, _) -> method) wireCases),
      testCase "remote/malformed outcomes stay explicit and allow subsequent connection calls" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withResourcePeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
              forM_ (highCalls connection) $ \request -> do
                result <- try @RpcResultError request
                case result of
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected Git resource result",
      testCase "cancelled generation preserves async identity without cache save or remote rollback" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withResourcePeer HeldGenerate trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.generateSemanticDiff connection generateParams) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled generation returned"
          Daemon.getSemanticDiffCache connection semanticTarget >>= (@?= cacheWire) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.generate_semantic_diff", "daemon.get_semantic_diff_cache"]) . map (field "method"),
      testCase "all low-level resource calls preserve caller routing and zero deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "resource-rpc" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
          forM_ (zip wireCases (lowCalls channel configured)) $ \((method, params, expected), request) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String method
            field "params" frame @?= params
            atomically (writeTQueue incoming (response frame expected))
            wait pending >>= (@?= expected)
          forM_ (lowCalls channel (configured {Client.callTimeoutMicros = Just 0})) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]
  where
    warningSchema key = schemaAt ["definitions", "DaemonInspectMissionReadinessResultSchema", "properties", "warning", "properties", key, "enum"] schema

commitParams :: GitCommitParams
commitParams = GitCommitParams "source-session" "message\n\"quoted\"" (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])

createParams :: CreatePullRequestParams
createParams = (defaultCreatePullRequestParams "source-session" "Requested title" "main") {createPullRequestBody = Just "", createPullRequestDraft = Just False, createPullRequestTicketIds = Just [], createPullRequestTicketUrls = Just [], createPullRequestJiraKeys = Just [], createPullRequestLinearIds = Just [], createPullRequestAdditionalFields = KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False]}

semanticTarget :: SemanticDiffTarget
semanticTarget = SemanticDiffTarget "topic" "main" (KeyMap.singleton "future" (Bool False))

saveParams :: SaveSemanticDiffParams
saveParams = SaveSemanticDiffParams "topic" "main" "" "" False (KeyMap.singleton "future" (Bool False))

generateParams :: GenerateSemanticDiffParams
generateParams = GenerateSemanticDiffParams "source-session" "DIFF" "main" "topic" (Just "") (Just "") (Just "") (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])

highCalls :: Daemon.DaemonConnection -> [IO Value]
highCalls connection = [toJSON <$> Daemon.inspectMissionReadiness connection "/remote", toJSON <$> Daemon.acknowledgeMissionReadinessWarning connection "/remote", toJSON <$> Daemon.gitPush connection "source-session", toJSON <$> Daemon.gitCommit connection commitParams, toJSON <$> Daemon.createPullRequest connection createParams, toJSON <$> Daemon.getSemanticDiffCache connection semanticTarget, toJSON <$> Daemon.saveSemanticDiffCache connection saveParams, toJSON <$> Daemon.generateSemanticDiff connection generateParams]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel configured = [toJSON <$> Client.inspectDaemonMissionReadiness channel configured (GitDirectoryParams "/remote" mempty), toJSON <$> Client.acknowledgeDaemonMissionReadinessWarning channel configured (GitDirectoryParams "/remote" mempty), toJSON <$> Client.pushDaemonGit channel configured (SessionIdParams "source-session" mempty), toJSON <$> Client.commitDaemonGit channel configured commitParams, toJSON <$> Client.createDaemonPullRequest channel configured createParams, toJSON <$> Client.getDaemonSemanticDiffCache channel configured semanticTarget, toJSON <$> Client.saveDaemonSemanticDiffCache channel configured saveParams, toJSON <$> Client.generateDaemonSemanticDiff channel configured generateParams]

wireCases :: [(Text, Value, Value)]
wireCases = [("daemon.inspect_mission_readiness", object ["cwd" .= String "/remote"], readinessWire), ("daemon.acknowledge_mission_readiness_warning", object ["cwd" .= String "/remote"], object []), ("daemon.git_push", object ["sessionId" .= String "source-session"], successWire), ("daemon.git_commit", commitWire, successWire), ("daemon.create_pr", createWire, createdWire), ("daemon.get_semantic_diff_cache", targetWire, cacheWire), ("daemon.save_semantic_diff_cache", saveWire, successWire), ("daemon.generate_semantic_diff", generateWire, generatedWire)]

data Mode = Normal | Rejected | Malformed | HeldGenerate deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withResourcePeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withResourcePeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
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
            if mode == HeldGenerate && method == "daemon.generate_semantic_diff"
              then putMVar ready ()
              else case mode of
                Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Resource rejected"]]
                Malformed -> reply connection request Null
                _ -> reply connection request result
          _ -> assertFailure "Unexpected publication, acknowledgement, cache or session request"
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

response :: Object -> Value -> Object
response request result = KeyMap.fromList (envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result])

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . object . envelope

envelope :: [Pair] -> [Pair]
envelope fields = ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

insertField :: Key -> Value -> Value -> Value
insertField key value (Object fields) = Object (KeyMap.insert key value fields)
insertField _ _ _ = error "Expected object fixture"

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decodeValue @a value >>= (@?= value) . toJSON

readinessWire, commitWire, createWire, createdWire, targetWire, cacheWire, saveWire, generateWire, generatedWire, successWire :: Value
readinessWire = object ["isGitRepo" .= True, "hasRemote" .= True, "remoteUrl" .= String "https://provider.invalid/repo", "isEmpty" .= False, "warning" .= object ["state" .= String "low_score", "level" .= Number 2, "repoUrl" .= String "https://provider.invalid/repo"]]
commitWire = object ["sessionId" .= String "source-session", "message" .= String "message\n\"quoted\"", "future" .= False]
createWire = object ["sessionId" .= String "source-session", "title" .= String "Requested title", "body" .= String "", "baseBranch" .= String "main", "draft" .= False, "linkedTicketIds" .= ([] :: [Value]), "linkedTicketUrls" .= ([] :: [Value]), "jiraIssueKeys" .= ([] :: [Value]), "linearIssueIds" .= ([] :: [Value]), "future" .= False]
createdWire = object ["number" .= Number 42, "title" .= String "Provider title", "url" .= String "https://provider.invalid/request/42", "state" .= String "opened", "draft" .= False]
targetWire = object ["currentBranch" .= String "topic", "baseBranch" .= String "main", "future" .= False]
cacheWire = object ["content" .= Null, "commitHash" .= Null, "truncated" .= False]
saveWire = object ["currentBranch" .= String "topic", "baseBranch" .= String "main", "commitHash" .= String "", "content" .= String "", "truncated" .= False, "future" .= False]
generateWire = object ["sessionId" .= String "source-session", "diff" .= String "DIFF", "baseBranch" .= String "main", "currentBranch" .= String "topic", "commitHash" .= String "", "modelId" .= String "", "unstagedDiff" .= String "", "future" .= False]
generatedWire = object ["content" .= String "SEMANTIC REPORT", "truncated" .= False, "sessionId" .= String "generation-session"]
successWire = object ["success" .= False]
