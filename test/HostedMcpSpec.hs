{-# LANGUAGE OverloadedStrings #-}

module HostedMcpSpec (hostedMcpTests) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (cancel, mapConcurrently, poll, wait, waitCatch, withAsync)
import Control.Exception (bracket_, try)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, encode, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid qualified as Droid
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.MCP.Server
import Factory.Droid.MCP.Tool
import Factory.Droid.Protocol (RpcResultError)
import Factory.Droid.Schema.Control (ForkSessionParams (..))
import Factory.Droid.Schema.MCP (HttpHeader (..))
import Factory.Droid.Schema.MCP.Config (McpRemoteConfig (..), McpServerConfig (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hAccept, hAuthorization, hContentType, statusCode)
import ProcessSpec (bounded)
import System.Environment (getExecutablePath)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

hostedMcpTests :: TestTree
hostedMcpTests = testGroup "Hosted MCP tools" hostedCases

hostedCases :: [TestTree]
hostedCases =
  [ testCase "session-owned tools execute over HTTP during startup, replacement and rollback" $ bounded $ do
      counter <- newIORef (0 :: Int)
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "echo" "Echo" openObjectSchema (\arguments -> modifyIORef' counter (+ 1) >> pure (structuredResult arguments)))
      server <- newMcpServer (defaultMcpServerOptions "hosted-fixture") [tool]
      executable <- getExecutablePath
      let options = (Droid.defaultDroidOptions ".") {Droid.droidExecutable = executable, Droid.droidHostedMcpServers = [server]}
      Droid.withDroidSession options $ \session -> do
        readIORef counter >>= (@?= 1)
        before <- getMcpServerConfig server
        branch <- Droid.forkDroidSession session (ForkSessionParams Nothing Nothing mempty)
        getMcpServerConfig server >>= assertBool "Replacement changed hosted endpoint" . (== before)
        result <- try @Droid.DroidReplacementError (Droid.forkDroidSession branch (ForkSessionParams Nothing Nothing (KeyMap.singleton "fixtureLoadFailure" (Bool True))))
        case result of Left _ -> pure (); Right _ -> assertFailure "Expected rollback"
        readIORef counter >>= (@?= 4)
        getMcpServerConfig server >>= assertBool "Rollback lost hosted endpoint" . (== before)
      getMcpServerConfig server >>= assertBool "Session-owned server was not stopped" . isNothing
      _ <- startMcpServer server
      rejected <- try @RpcResultError (Droid.withDroidSession (options {Droid.droidModel = Just "fixture-reject"}) (\_ -> pure ()))
      case rejected of Left _ -> pure (); Right _ -> assertFailure "Expected initialization failure"
      getMcpServerConfig server >>= assertBool "Session failure stole caller-owned server" . isJust
      closeMcpServer server,
    testCase "remote daemon hosted options fail before exposing a loopback endpoint" $ do
      server <- newMcpServer (defaultMcpServerOptions "remote-check") []
      let target = WebSocket.WebSocketTarget "remote.invalid" 443 "/"
          options = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "/workspace") {Daemon.daemonHostedMcpServers = [server]}
      rejected <- try @McpServerError (Daemon.withSession options (\_ -> pure ()))
      rejected @?= Left HostedMcpRequiresLocalDaemon
      getMcpServerConfig server >>= assertBool "Rejected remote setup started a server" . isNothing,
    testCase "request deadlines return tool errors and cancel handler work" $ bounded $ do
      finalized <- newEmptyMVar
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "slow" "Slow" openObjectSchema (\_ -> bracket_ (pure ()) (putMVar finalized ()) (threadDelay 1000000 >> pure (textResult "late"))))
      server <- newMcpServer ((defaultMcpServerOptions "deadline") {hostedToolTimeoutMicros = Just 10000}) [tool]
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        response <- post manager config (rpc "tools/call" (object ["name" .= String "slow"]))
        resultField response "isError" @?= Just (Bool True)
        takeMVar finalized,
    testCase "native initialize/list/call works and raw arguments exclude HTTP context" $ bounded $ do
      schema <- either (const (assertFailure "Schema construction failed")) pure (mkMcpSchema (KeyMap.fromList ["type" .= String "object", "properties" .= object ["n" .= object ["type" .= String "integer"]], "required" .= [String "n"], "additionalProperties" .= False]))
      count <- newIORef (0 :: Int)
      typed <- either (const (assertFailure "Tool construction failed")) pure (typedTool "double" "Double an integer" schema (\(Arguments n) -> modifyIORef' count (+ 1) >> pure (structuredResult (KeyMap.singleton "n" (Number (fromIntegral (n * 2)))))))
      raw <- either (const (assertFailure "Raw tool construction failed")) pure (rawTool "echo" "Echo arguments only" openObjectSchema (pure . structuredResult))
      server <- newMcpServer (defaultMcpServerOptions "native") [typed, raw]
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        initialized <- post manager config (rpc "initialize" (object ["protocolVersion" .= String "2025-11-25", "capabilities" .= object [], "clientInfo" .= object ["name" .= String "offline-peer", "version" .= String "1"]]))
        resultField initialized "protocolVersion" @?= Just (String "2025-11-25")
        listed <- post manager config (rpc "tools/list" (object []))
        case resultField listed "tools" of
          Just (Array values) -> length values @?= 2
          _ -> assertFailure "Missing listed tools"
        called <- post manager config (rpc "tools/call" (object ["name" .= String "double", "arguments" .= object ["n" .= (3 :: Int)]]))
        resultField called "structuredContent" @?= Just (object ["n" .= (6 :: Int)])
        invalid <- post manager config (rpc "tools/call" (object ["name" .= String "double", "arguments" .= object ["n" .= String "wrong"]]))
        resultField invalid "isError" @?= Just (Bool True)
        readIORef count >>= (@?= 1)
        echoed <- post manager config (rpc "tools/call" (object ["name" .= String "echo", "arguments" .= object ["hello" .= String "world"]]))
        resultField echoed "structuredContent" @?= Just (object ["hello" .= String "world"])
      getMcpServerConfig server >>= assertBool "Scoped server remained active" . isNothing,
    testCase "authentication, origin, method, path and body limits guard invocation" $ bounded $ do
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "echo" "Echo" openObjectSchema (pure . structuredResult))
      server <- newMcpServer ((defaultMcpServerOptions "boundary") {hostedRequestLimitBytes = 128}) [tool]
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        request <- requestFor config
        let perform value = HTTP.httpLbs value manager
        unauthenticated <- perform (request {HTTP.requestHeaders = filter ((/= hAuthorization) . fst) (HTTP.requestHeaders request)})
        statusCode (HTTP.responseStatus unauthenticated) @?= 401
        origin <- perform (request {HTTP.requestHeaders = ("Origin", "https://untrusted.invalid") : HTTP.requestHeaders request})
        statusCode (HTTP.responseStatus origin) @?= 403
        wrongPath <- perform (request {HTTP.path = "/other"})
        statusCode (HTTP.responseStatus wrongPath) @?= 404
        wrongMethod <- perform (request {HTTP.method = "GET"})
        statusCode (HTTP.responseStatus wrongMethod) @?= 405
        oversized <- perform (request {HTTP.requestBody = HTTP.RequestBodyLBS (BL.replicate 129 32)})
        statusCode (HTTP.responseStatus oversized) @?= 413
        noJson <- perform (request {HTTP.requestHeaders = (hAccept, "application/json;q=0, text/event-stream") : filter ((/= hAccept) . fst) (HTTP.requestHeaders request)})
        statusCode (HTTP.responseStatus noJson) @?= 406
        wrongContent <- perform (request {HTTP.requestHeaders = (hContentType, "application/json-malformed") : filter ((/= hContentType) . fst) (HTTP.requestHeaders request)})
        statusCode (HTTP.responseStatus wrongContent) @?= 415,
    testCase "nested batches and malformed notifications fail before handler dispatch" $ bounded $ do
      count <- newIORef (0 :: Int)
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "constructor" "Valid non-prototype name" openObjectSchema (\_ -> modifyIORef' count (+ 1) >> pure (textResult "called")))
      server <- newMcpServer (defaultMcpServerOptions "envelopes") [tool]
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        request <- requestFor config
        let call = rpc "tools/call" (object ["name" .= String "constructor"])
            invalid = [toJSON (replicate 2 (replicate 32 call)), object ["jsonrpc" .= String "2.0", "method" .= String "notifications/initialized", "params" .= False], object ["jsonrpc" .= String "2.0", "id" .= String "client", "error" .= False], object ["jsonrpc" .= String "2.0", "id" .= String "client", "result" .= False], object ["jsonrpc" .= String "2.0", "id" .= String "client", "result" .= object ["_meta" .= False]]]
        forM_ invalid $ \payload -> do
          response <- HTTP.httpLbs (request {HTTP.requestBody = HTTP.RequestBodyLBS (encode payload)}) manager
          statusCode (HTTP.responseStatus response) @?= 400
        readIORef count >>= (@?= 0),
    testCase "oversized identifiers cannot bypass response bounds" $ bounded $ do
      server <- newMcpServer ((defaultMcpServerOptions "response-bound") {hostedResponseLimitBytes = 128}) []
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        request <- requestFor config
        let payload = object ["jsonrpc" .= String "2.0", "id" .= Text.replicate 129 "x", "method" .= String "ping"]
        response <- HTTP.httpLbs (request {HTTP.requestBody = HTTP.RequestBodyLBS (encode payload)}) manager
        statusCode (HTTP.responseStatus response) @?= 400
        assertBool "Fallback response exceeded configured limit" (BL.length (HTTP.responseBody response) <= 128),
    testCase "repeated manual start, borrowed lease and restart retain correct ownership" $ bounded $ do
      server <- newMcpServer (defaultMcpServerOptions "lifecycle") []
      first <- startMcpServer server
      again <- startMcpServer server
      assertBool "Repeated start did not share runtime" (first == again)
      concurrent <- mapConcurrently (const (startMcpServer server)) ([1 .. 8] :: [Int])
      assertBool "Concurrent starts diverged" (all (== first) concurrent)
      withMcpServer server $ \borrowed -> assertBool "Borrowed scope changed config" (borrowed == first)
      getMcpServerConfig server >>= assertBool "Borrowed scope stopped manual owner" . isJust
      closeMcpServer server
      closeMcpServer server
      getMcpServerConfig server >>= assertBool "Closed config remained present" . isNothing
      second <- startMcpServer server
      assertBool "Restart failed to rotate authentication" (headers first /= headers second)
      closeMcpServer server,
    testCase "concurrent leases keep one endpoint until the last owner leaves" $ bounded $ do
      server <- newMcpServer (defaultMcpServerOptions "leases") []
      firstReady <- newEmptyMVar
      secondReady <- newEmptyMVar
      releaseFirst <- newEmptyMVar
      releaseSecond <- newEmptyMVar
      withAsync (withMcpServer server (\config -> putMVar firstReady config >> takeMVar releaseFirst)) $ \first -> do
        firstConfig <- takeMVar firstReady
        withAsync (withMcpServer server (\config -> putMVar secondReady config >> takeMVar releaseSecond)) $ \second -> do
          secondConfig <- takeMVar secondReady
          assertBool "Concurrent leases used different endpoints" (firstConfig == secondConfig)
          putMVar releaseFirst ()
          wait first
          getMcpServerConfig server >>= assertBool "First lease stopped remaining owner" . isJust
          putMVar releaseSecond ()
          wait second
      getMcpServerConfig server >>= assertBool "Last lease failed to stop server" . isNothing,
    testCase "concurrent close and restart wait for finalizers without holding state lock" $ bounded $ do
      serverRef <- newEmptyMVar
      started <- newEmptyMVar
      held <- newEmptyMVar
      finalizing <- newEmptyMVar
      release <- newEmptyMVar
      let cleanup = do
            server <- readMVar serverRef
            getMcpServerConfig server >>= assertBool "Closing configuration remained visible" . isNothing
            putMVar finalizing ()
            takeMVar release
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "wait" "Wait" openObjectSchema (\_ -> bracket_ (putMVar started ()) cleanup (takeMVar held >> pure (textResult "done"))))
      server <- newMcpServer ((defaultMcpServerOptions "closing") {hostedToolTimeoutMicros = Nothing}) [tool]
      putMVar serverRef server
      config <- startMcpServer server
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      withAsync (post manager config (rpc "tools/call" (object ["name" .= String "wait"]))) $ \request -> do
        takeMVar started
        withAsync (closeMcpServer server) $ \first -> do
          takeMVar finalizing
          withAsync (closeMcpServer server) $ \second ->
            withAsync (startMcpServer server) $ \restart -> do
              bracket_ (pure ()) (void (tryPutMVar release ())) $ do
                timeout 10000 (wait second) >>= assertBool "Second close returned before cleanup" . isNothing
                poll restart >>= assertBool "Restart crossed cleanup" . isNothing
              wait first
              wait second
              restarted <- wait restart
              assertBool "Restart reused authentication" (headers config /= headers restarted)
        _ <- waitCatch request
        pure ()
      closeMcpServer server,
    testCase "self-close from a handler fails rather than deadlocking" $ bounded $ do
      serverRef <- newEmptyMVar
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "close" "Close" openObjectSchema (\_ -> readMVar serverRef >>= closeMcpServer >> pure (textResult "closed")))
      server <- newMcpServer (defaultMcpServerOptions "self-close") [tool]
      putMVar serverRef server
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        result <- post manager config (rpc "tools/call" (object ["name" .= String "close"]))
        resultField result "isError" @?= Just (Bool True)
        getMcpServerConfig server >>= assertBool "Self-close changed ownership" . isJust,
    testCase "cancelled manual starts leave cleanup reachable and can be retried" $ bounded $ do
      server <- newMcpServer (defaultMcpServerOptions "start-cancellation") []
      forM_ ([1 .. 12] :: [Int]) $ \_ -> do
        withAsync (startMcpServer server) $ \starting -> do
          cancel starting
          _ <- waitCatch starting
          pure ()
        closeMcpServer server
        getMcpServerConfig server >>= assertBool "Cancelled start retained configuration" . isNothing
      withMcpServer server (const (pure ())),
    testCase "close joins in-flight handler finalizers" $ bounded $ do
      started <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "wait" "Wait" openObjectSchema (\_ -> bracket_ (putMVar started ()) (putMVar finished ()) (takeMVar hold >> pure (textResult "done"))))
      server <- newMcpServer ((defaultMcpServerOptions "cleanup") {hostedToolTimeoutMicros = Nothing}) [tool]
      config <- startMcpServer server
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      withAsync (post manager config (rpc "tools/call" (object ["name" .= String "wait"]))) $ \request -> do
        takeMVar started
        closeMcpServer server
        takeMVar finished
        _ <- waitCatch request
        pure ()
      getMcpServerConfig server >>= assertBool "Server remained active after cancellation" . isNothing,
    testCase "structured output is advertised and checked independently of decoding" $ bounded $ do
      output <- either (const (assertFailure "Output schema construction failed")) pure (mkMcpSchema (KeyMap.fromList ["type" .= String "object", "properties" .= object ["total" .= object ["type" .= String "integer", "minimum" .= (1 :: Int)]], "required" .= [String "total"]]))
      good <- either (const (assertFailure "Tool construction failed")) pure (structuredTool "good" "Good" openObjectSchema output (\(_ :: Value) -> pure (object ["total" .= (2 :: Int)])))
      bad <- either (const (assertFailure "Tool construction failed")) pure (structuredTool "bad" "Bad" openObjectSchema output (\(_ :: Value) -> pure (object ["total" .= (0 :: Int)])))
      malformed <- either (const (assertFailure "Tool construction failed")) pure (rawTool "malformed" "Malformed" openObjectSchema (\_ -> pure ((textResult "text") {toolResultAdditionalFields = KeyMap.singleton "_meta" (Bool False)})))
      server <- newMcpServer (defaultMcpServerOptions "output") [good, bad, malformed]
      withMcpServer server $ \config -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        tools <- post manager config (rpc "tools/list" (object []))
        case resultField tools "tools" of
          Just (Array entries) -> assertBool "Structured schema not advertised" (any (\case Object fields -> KeyMap.lookup "outputSchema" fields == Just (Object (mcpSchemaObject output)); _ -> False) entries)
          _ -> assertFailure "Missing tools"
        valid <- post manager config (rpc "tools/call" (object ["name" .= String "good"]))
        resultField valid "structuredContent" @?= Just (object ["total" .= (2 :: Int)])
        invalid <- post manager config (rpc "tools/call" (object ["name" .= String "bad"]))
        resultField invalid "isError" @?= Just (Bool True)
        invalidEnvelope <- post manager config (rpc "tools/call" (object ["name" .= String "malformed"]))
        case invalidEnvelope of
          Object fields | Just (Object problem) <- KeyMap.lookup "error" fields -> KeyMap.lookup "code" problem @?= Just (Number (-32602))
          _ -> assertFailure "Malformed result was treated as success",
    testCase "rich content preserves valid variants and rejects malformed media or metadata" $ do
      let values = [object ["type" .= String "text", "text" .= String "hello", "annotations" .= object ["lastModified" .= String "2025-01-02T03:04Z"]], object ["type" .= String "image", "data" .= String " /x==\n", "mimeType" .= String "image/png"], object ["type" .= String "audio", "data" .= String "Zg", "mimeType" .= String "audio/wav"], object ["type" .= String "resource", "resource" .= object ["uri" .= String "fixture:blob", "blob" .= String "Zg=="]], object ["type" .= String "resource_link", "uri" .= String "fixture:item", "name" .= String "item", "size" .= (2 :: Int), "icons" .= [object ["src" .= String "fixture:icon", "theme" .= String "dark"]]]]
      forM_ values $ \value -> case value of
        Object fields -> case jsonContent fields of Right content -> toJSON content @?= value; Left _ -> assertFailure "Valid content rejected"
        _ -> assertFailure "Invalid fixture"
      forM_ [object ["type" .= String "image", "data" .= String "Zg=", "mimeType" .= String "image/png"], object ["type" .= String "audio", "data" .= String "?", "mimeType" .= String "audio/wav"], object ["type" .= String "text", "text" .= String "x", "annotations" .= object ["lastModified" .= String "not-a-date"]], object ["type" .= String "resource_link", "uri" .= String "fixture:x", "name" .= String "x", "size" .= String "invalid"]] $ \case
        Object fields -> case jsonContent fields of Left InvalidMcpToolResult -> pure (); _ -> assertFailure "Malformed content accepted"
        _ -> assertFailure "Invalid fixture",
    testCase "unsupported pointer escapes cannot select a different validation target" $ do
      let fields = KeyMap.fromList ["type" .= String "object", "$defs" .= object ["~1" .= object ["type" .= String "integer"], "/" .= object ["type" .= String "string"]], "properties" .= object ["v" .= object ["$ref" .= String "#/$defs/~01"]]]
      case mkMcpSchema fields of
        Left UnsupportedMcpSchema -> pure ()
        _ -> assertFailure "Ambiguous dependency pointer decoding was admitted",
    testCase "checked schemas enforce ref siblings and reject unsupported keywords" $ do
      schema <- either (const (assertFailure "Schema construction failed")) pure (mkMcpSchema (KeyMap.fromList ["type" .= String "object", "$defs" .= object ["number" .= object ["type" .= String "integer"]], "properties" .= object ["value" .= object ["$ref" .= String "#/$defs/number", "minimum" .= (3 :: Int)]]]))
      tool <- either (const (assertFailure "Tool construction failed")) pure (rawTool "check" "Check" schema (const (pure (textResult "valid"))))
      result <- invokeTool tool (KeyMap.singleton "value" (Number 2))
      case result of Right value -> toolResultIsError value @?= Just True; _ -> assertFailure "Unexpected protocol error"
      case mkMcpSchema (KeyMap.fromList ["type" .= String "object", "unevaluatedProperties" .= False]) of
        Left UnsupportedMcpSchema -> pure ()
        _ -> assertFailure "Unsupported schema was silently admitted"
      forM_ [KeyMap.fromList ["type" .= String "object", "$ref" .= String "#/x-schema", "x-schema" .= object ["unevaluatedProperties" .= False]], KeyMap.fromList ["type" .= String "object", "$ref" .= String "#/default", "default" .= object ["patternProperties" .= object []]]] $ \fields ->
        case mkMcpSchema fields of Left _ -> pure (); Right _ -> assertFailure "Reference escaped checked schema locations"
  ]

newtype Arguments = Arguments Int

instance FromJSON Arguments where
  parseJSON = withObject "Arguments" (fmap Arguments . (.: "n"))

rpc :: Text.Text -> Value -> Value
rpc method params = object ["jsonrpc" .= String "2.0", "id" .= String "request", "method" .= method, "params" .= params]

headers :: McpServerConfig -> [HttpHeader]
headers (McpHttpConfig config) = fromMaybe [] (mcpRemoteHeaders config)
headers _ = []

requestFor :: McpServerConfig -> IO HTTP.Request
requestFor config = case config of
  McpHttpConfig remote -> do
    request <- HTTP.parseRequest (Text.unpack (mcpRemoteUrl remote))
    pure request {HTTP.method = "POST", HTTP.requestHeaders = [(hAccept, "application/json, text/event-stream"), (hContentType, "application/json")] <> [(fromString (Text.unpack (httpHeaderName header)), Text.encodeUtf8 (httpHeaderValue header)) | header <- headers config], HTTP.requestBody = HTTP.RequestBodyLBS (encode (rpc "ping" (object [])))}
  _ -> assertFailure "Hosted server returned non-HTTP config"

post :: HTTP.Manager -> McpServerConfig -> Value -> IO Value
post manager config value = do
  request <- requestFor config
  response <- HTTP.httpLbs (request {HTTP.requestBody = HTTP.RequestBodyLBS (encode value)}) manager
  statusCode (HTTP.responseStatus response) @?= 200
  either (const (assertFailure "Invalid MCP JSON response")) pure (eitherDecode (HTTP.responseBody response))

resultField :: Value -> Key -> Maybe Value
resultField (Object fields) key = case KeyMap.lookup "result" fields of Just (Object result) -> KeyMap.lookup key result; _ -> Nothing
resultField _ _ = Nothing
