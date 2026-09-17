{-# LANGUAGE OverloadedStrings #-}

module RESTSpec (restTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Exception (IOException, fromException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (FromJSON, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Version (showVersion)
import Factory.Droid.REST
import Factory.Droid.Schema.REST
import Factory.Droid.Transport.WebSocket (WebSocketTarget (..))
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (Method, Query, RequestHeaders, hAccept, hAuthorization, hContentType, hLocation, status200, status204, status302, status401, status403, status422, status500)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Paths_droid_sdk (version)
import ProcessSpec (bounded)
import System.IO.Error (isResourceVanishedError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Text.Read (readMaybe)
import WebSocketSpec (withSocketPeer, withTLSCertificate, withTLSPeer)

restTests :: TestTree
restTests = testGroup "Factory REST" [wireTests, responseTests, codecTests, cancellationTests, retryTest, tlsTest, portTests, resetTest]

portTests :: TestTree
portTests = testCase "invalid URI ports cannot wrap to another destination" $
  forM_ ["65979", "65536", "18446744073709617595", "0"] $ \port ->
    expectRest RestInvalidOptions (newRestClient ((defaultRestOptions "OFFLINE_API_KEY") {restBaseUrl = "https://localhost:" <> port}))

resetTest :: TestTree
resetTest = testCase "a connection reset after response headers remains a REST error"
  $ bounded
  $ withSocketPeer
    ( \socket -> do
        _ <- readRequest socket
        Socket.sendAll socket "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n{\"computers\":["
        threadDelay 20000
        Socket.setSockOpt socket Socket.Linger (Socket.StructLinger 1 0)
    )
  $ \target -> do
    client <- newRestClient (optionsFor (webSocketPort target))
    expectRest RestTransportFailure (listComputers client)

data Observed = Observed !Method !ByteString !Query !RequestHeaders !BL.ByteString deriving stock (Eq, Show)

wireTests :: TestTree
wireTests = testCase "all thirteen helpers issue native method/path/query/body requests" $ bounded $ do
  requests <- newIORef []
  let app request respond = do
        body <- Wai.strictRequestBody request
        atomicModifyIORef' requests (\values -> (values <> [Observed (Wai.requestMethod request) (Wai.rawPathInfo request) (Wai.queryString request) (Wai.requestHeaders request) body], ()))
        let segments = drop 3 (Wai.pathInfo request)
            payload = case segments of
              ["machines", "templates"] -> page "templates" [templateValue]
              ["machines", "templates", _] -> templateValue
              ["sessions"] -> page "sessions" [sessionValue]
              ["computers", _, "metrics"] -> toJSON [metricValue]
              ["computers", _, "refresh"] -> object ["configured" .= (2 :: Int)]
              ["computers"] | Wai.requestMethod request == "GET" -> object ["computers" .= [computerValue]]
              _ -> computerValue
        if Wai.requestMethod request == "DELETE" || last segments == "restart"
          then respond (Wai.responseLBS status204 [] "")
          else respond (Wai.responseLBS status200 [(hContentType, "application/json")] (encode payload))
  Warp.testWithApplication (pure app) $ \port -> do
    client <- newRestClient ((optionsFor port) {restBaseUrl = urlFor port <> "/prefix"})
    templates <- listMachineTemplates client (PageOptions (Just 7) (Just "a b&?="))
    map machineTemplateId (pageItems templates) @?= ["template"]
    pagePagination templates @?= Pagination True (Just "next")
    getMachineTemplate client "tpl /?#%é" >>= (@?= "Build") . machineTemplateName
    listComputers client >>= (@?= ["machine"]) . map computerId
    getComputer client "machine" >>= (@?= "Box") . computerName
    getComputerByName client "Box /?#%é" >>= (@?= "machine") . computerId
    createComputer client ((defaultCreateComputerParams "New" "user") {createComputerProvider = Just E2b, createComputerHostId = Just "", createComputerRepos = Just [], createComputerAutoInstallDeps = Just False, createComputerServiceAccountId = Just "account"}) >>= (@?= "machine") . computerId
    updateComputer client "machine" (UpdateComputerParams (Just "") (Just "remote") (Just "host")) >>= (@?= "Box") . computerName
    deleteComputer client "machine"
    restartComputer client "machine"
    refreshComputer client "machine" >>= (@?= 2)
    metrics <- getComputerMetrics client "machine" (Just "")
    map metricCpuUsedPct metrics @?= [12.5]
    retryInstallDeps client "machine" >>= (@?= "machine") . computerId
    sessions <- listRemoteSessions client (Just "machine") defaultPageOptions
    map remoteSessionId (pageItems sessions) @?= ["session"]
  observed <- readIORef requests
  let paths = [(method, path) | Observed method path _ _ _ <- observed]
  paths @?= [("GET", "/prefix/api/v0/machines/templates"), ("GET", "/prefix/api/v0/machines/templates/tpl%20%2F%3F%23%25%C3%A9"), ("GET", "/prefix/api/v0/computers"), ("GET", "/prefix/api/v0/computers/machine"), ("GET", "/prefix/api/v0/computers/name/Box%20%2F%3F%23%25%C3%A9"), ("POST", "/prefix/api/v0/computers"), ("PATCH", "/prefix/api/v0/computers/machine"), ("DELETE", "/prefix/api/v0/computers/machine"), ("POST", "/prefix/api/v0/computers/machine/restart"), ("POST", "/prefix/api/v0/computers/machine/refresh"), ("GET", "/prefix/api/v0/computers/machine/metrics"), ("POST", "/prefix/api/v0/computers/machine/install-deps"), ("GET", "/prefix/api/v0/sessions")]
  forM_ (zip [0 :: Int ..] observed) $ \(index, Observed _ _ query headers body) -> do
    lookup hAuthorization headers @?= Just "Bearer OFFLINE_API_KEY"
    lookup hAccept headers @?= Just "application/json"
    lookup "X-Factory-Client" headers @?= Just "sdk"
    lookup "X-Factory-Sdk" headers @?= Just (BS8.pack ("haskell/" <> showVersion version))
    if index `elem` [5, 6]
      then lookup hContentType headers @?= Just "application/json"
      else lookup hContentType headers @?= Nothing
    query @?= case index of 0 -> [("limit", Just "7"), ("cursor", Just "a b&?=")]; 10 -> [("start", Just "")]; 12 -> [("computerId", Just "machine")]; _ -> []
    case index of
      5 -> eitherDecode body @?= Right (object ["name" .= String "New", "remoteUser" .= String "user", "provider" .= String "e2b", "hostId" .= String "", "repos" .= ([] :: [Value]), "autoInstallDeps" .= False, "serviceAccountId" .= String "account"])
      6 -> eitherDecode body @?= Right (object ["name" .= String "", "remoteUser" .= String "remote", "hostId" .= String "host"])
      _ -> body @?= ""

responseTests :: TestTree
responseTests =
  testGroup
    "response boundaries"
    [ testCase "HTTP errors, invalid JSON and typed shapes remain distinct and redacted" $
        bounded $
          forM_ [(status401, "{\"error\":\"SECRET_FIXTURE\"}", RestAuthenticationError 401 (Just "SECRET_FIXTURE")), (status403, "{}", RestAuthenticationError 403 Nothing), (status422, "{\"error\":\"invalid\"}", RestApiError 422 (Just "invalid")), (status500, "{\"error\":false}", RestApiError 500 Nothing), (status401, "not JSON", RestNonJsonResponse 401), (status200, "{}", RestUnexpectedResponse 200), (status204, "", RestUnexpectedResponse 204)] $ \(status, body, expected) ->
            Warp.testWithApplication (pure (\_ respond -> respond (Wai.responseLBS status [] body))) $ \port -> do
              client <- newRestClient (optionsFor port)
              expectRest expected (listComputers client)
              assertBool "Error display leaked response text" (not ("SECRET_FIXTURE" `isInfixOf` show expected)),
      testCase "sparse query options and empty patches retain their wire meaning" $ bounded $ do
        seen <- newIORef []
        let app request respond = do
              body <- Wai.strictRequestBody request
              atomicModifyIORef' seen (\old -> (old <> [(Wai.requestMethod request, Wai.queryString request, body)], ()))
              let value = case reverse (Wai.pathInfo request) of "sessions" : _ -> page "sessions" []; "templates" : _ -> page "templates" []; "metrics" : _ -> toJSON ([] :: [Value]); _ -> computerValue
              respond (Wai.responseLBS status200 [] (encode value))
        Warp.testWithApplication (pure app) $ \port -> do
          client <- newRestClient (optionsFor port)
          void (listMachineTemplates client (PageOptions (Just 0) (Just "")))
          void (listRemoteSessions client Nothing (PageOptions (Just 1.5) Nothing))
          getComputerMetrics client "machine" Nothing >>= (@?= [])
          void (updateComputer client "machine" defaultUpdateComputerParams)
        readIORef seen >>= (@?= [("GET", [("limit", Just "0"), ("cursor", Just "")], ""), ("GET", [("limit", Just "1.5")], ""), ("GET", [], ""), ("PATCH", [], "{}")]),
      testCase "a handled API error leaves the reusable client usable" $ bounded $ do
        seen <- newIORef False
        let app _ respond = do
              previous <- atomicModifyIORef' seen (True,)
              respond (if previous then Wai.responseLBS status200 [] "{\"computers\":[]}" else Wai.responseLBS status422 [] "{\"error\":\"invalid\"}")
        Warp.testWithApplication (pure app) $ \port -> do
          client <- newRestClient (optionsFor port)
          expectRest (RestApiError 422 (Just "invalid")) (listComputers client)
          listComputers client >>= (@?= []),
      testCase "decoded response bounds include exact boundary and UTF-8 BOM" $ bounded $ do
        let body = "{\"computers\":[]}"
        Warp.testWithApplication (pure (\_ respond -> respond (Wai.responseLBS status200 [] body))) $ \port -> do
          client <- newRestClient ((optionsFor port) {restResponseLimitBytes = fromIntegral (BL.length body)})
          listComputers client >>= (@?= [])
          small <- newRestClient ((optionsFor port) {restResponseLimitBytes = fromIntegral (BL.length body) - 1})
          expectRest RestResponseTooLarge (listComputers small)
        Warp.testWithApplication (pure (\_ respond -> respond (Wai.responseLBS status200 [] ("\xef\xbb\xbf" <> body)))) $ \port -> newRestClient (optionsFor port) >>= listComputers >>= (@?= []),
      testCase "redirects never move credentials to another endpoint" $ bounded $ do
        reached <- newIORef False
        Warp.testWithApplication (pure (\_ respond -> atomicModifyIORef' reached (const (True, ())) >> respond (Wai.responseLBS status200 [] "{\"computers\":[]}"))) $ \other ->
          Warp.testWithApplication (pure (\_ respond -> respond (Wai.responseLBS status302 [(hLocation, BS8.pack (Text.unpack (urlFor other)))] "{}"))) $ \port -> do
            client <- newRestClient (optionsFor port)
            expectRest (RestApiError 302 Nothing) (listComputers client)
        readIORef reached >>= (@?= False),
      testCase "configuration and dot-segment identifiers fail before requests" $ bounded $ do
        reached <- newIORef False
        Warp.testWithApplication (pure (\_ respond -> atomicModifyIORef' reached (const (True, ())) >> respond (Wai.responseLBS status200 [] "{}"))) $ \port -> do
          forM_ [(optionsFor port) {restApiKey = ""}, (optionsFor port) {restApiKey = "key\r\nX-Leak: yes"}, (optionsFor port) {restResponseLimitBytes = 0}, (optionsFor port) {restTimeoutMicros = Just (-1)}, (optionsFor port) {restBaseUrl = "http://user:password@127.0.0.1/"}, (optionsFor port) {restBaseUrl = urlFor port <> "?q=1"}, (optionsFor port) {restBaseUrl = urlFor port <> "#fragment"}, (optionsFor port) {restBaseUrl = "file:///OFFLINE_ONLY"}] $ \options -> expectRest RestInvalidOptions (newRestClient options)
          client <- newRestClient (optionsFor port)
          forM_ ["", ".", ".."] $ \ident -> expectRest RestInvalidIdentifier (deleteComputer client ident)
          zero <- newRestClient ((optionsFor port) {restTimeoutMicros = Just 0})
          expectRest RestTimedOut (listComputers zero)
        readIORef reached >>= (@?= False)
    ]

codecTests :: TestTree
codecTests = testCase "REST codecs preserve computer extensions, strip other extras and distinguish null" $ do
  computer <- decoded @Computer computerValue
  computerAdditionalFields computer @?= KeyMap.singleton "x-server" (Bool False)
  toJSON computer @?= computerValue
  computerStatus computer @?= Just Active
  computerCreatedAt computer @?= 9007199254740993
  template <- decoded @MachineTemplate templateValue
  machineTemplateLastUpdatedAt template @?= Just Nothing
  map environmentValue <$> machineTemplateEnvironment template @?= Just ["SECRET_FIXTURE", "second"]
  machineTemplateBuildStatus template @?= Just (TemplateBuildStatus BuildFailed (Just SetupScriptError) (Just 1) (Just 2) (Just "Build log"))
  machineTemplateCreatedAt template @?= Just 1
  machineTemplateSetupScript template @?= Just "setup"
  machineTemplateUserEnvironment template @?= Just []
  metric <- decoded @ComputerMetric metricValue
  toJSON metric @?= metricValue
  session <- decoded @RemoteSession sessionValue
  toJSON session @?= sessionValue
  remoteSessionStatus session @?= RemoteRunning
  assertBool "Template display leaked fields" (not ("SECRET_FIXTURE" `isInfixOf` show template))
  case templateValue of
    Object fields -> do
      absent <- decoded @MachineTemplate (Object (KeyMap.delete "lastUpdatedAt" fields))
      machineTemplateLastUpdatedAt absent @?= Nothing
      reject (Proxy @MachineTemplate) (Object (KeyMap.insert "createdAt" Null fields))
      case toJSON template of Object actual -> KeyMap.lookup "x-server" actual @?= Nothing; _ -> assertFailure "Invalid template encoding"
    _ -> assertFailure "Invalid fixture"
  forM_ ["byom", "e2b"] $ \provider -> decoded @ComputerProvider (String provider) >>= \value -> toJSON value @?= String provider
  forM_ ["provisioning", "active", "error"] $ \status -> decoded @ComputerStatus (String status) >>= \value -> toJSON value @?= String status
  forM_ ["building", "success", "failed"] $ \status -> decoded @TemplateBuildState (String status) >>= \value -> toJSON value @?= String status
  forM_ ["setup_script_error", "system_error"] $ \reason -> decoded @TemplateFailureReason (String reason) >>= \value -> toJSON value @?= String reason
  forM_ ["idle", "pending", "running"] $ \status -> decoded @RemoteSessionStatus (String status) >>= \value -> toJSON value @?= String status
  forM_ [Null, String "unknown", Number 1] $ \value -> reject (Proxy @ComputerProvider) value >> reject (Proxy @ComputerStatus) value >> reject (Proxy @RemoteSessionStatus) value
  reject (Proxy @Pagination) (object ["hasMore" .= True])
  forM_ [Null, Number 0] $ \value -> case computerValue of Object fields -> reject (Proxy @Computer) (Object (KeyMap.insert "status" value fields)); _ -> assertFailure "Invalid fixture"
  case computerValue of Object fields -> reject (Proxy @Computer) (Object (KeyMap.insert "createdAt" (Number 1.5) fields)); _ -> assertFailure "Invalid fixture"
  toJSON (defaultCreateComputerParams "Box" "user") @?= object ["name" .= String "Box", "remoteUser" .= String "user"]
  toJSON defaultUpdateComputerParams @?= object []

cancellationTests :: TestTree
cancellationTests =
  testGroup
    "owned responses"
    [ testCase "cancelled body reads preserve async identity and close the connection" $ bounded $ do
        started <- newEmptyMVar
        closed <- newEmptyMVar
        withSocketPeer (slowResponse started closed) $ \target -> do
          client <- newRestClient ((optionsFor (webSocketPort target)) {restTimeoutMicros = Nothing})
          withAsync (listComputers client) $ \call -> do
            takeMVar started
            cancel call
            result <- waitCatch call
            case result of Left err -> fromException err @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled request returned"
          takeMVar closed,
      testCase "whole-exchange timeout closes an unfinished response" $ bounded $ do
        started <- newEmptyMVar
        closed <- newEmptyMVar
        withSocketPeer (slowResponse started closed) $ \target -> do
          -- A startup timeout does not exercise unfinished-response cleanup.
          -- Observe response admission and keep a lower bound on the deadline.
          client <- newRestClient ((optionsFor (webSocketPort target)) {restTimeoutMicros = Just 1000000})
          began <- getMonotonicTimeNSec
          withAsync (expectRest RestTimedOut (listComputers client)) $ \call -> do
            takeMVar started
            wait call
          finished <- getMonotonicTimeNSec
          assertBool "REST timeout expired before its configured deadline" (finished - began >= 1000000000)
          assertBool "REST timeout exceeded deadline plus cleanup allowance" (finished - began < 3000000000)
          takeMVar closed
    ]

retryTest :: TestTree
retryTest = testCase "a possibly completed POST is not retried on a reused connection"
  $ bounded
  $ withSocketPeer
    ( \socket -> do
        _ <- readRequest socket
        let body = encode (object ["computers" .= [computerValue]])
        Socket.sendAll socket ("HTTP/1.1 200 OK\r\nContent-Length: " <> BS8.pack (show (BL.length body)) <> "\r\n\r\n" <> BL.toStrict body)
        (headers, bodyBytes) <- readRequest socket
        assertBool "Expected the mutation on the reused connection" ("POST /api/v0/computers HTTP/1.1" `BS.isPrefixOf` headers)
        eitherDecode (BL.fromStrict bodyBytes) @?= Right (object ["name" .= String "Box", "remoteUser" .= String "user"])
    )
  $ \target -> do
    client <- newRestClient ((optionsFor (webSocketPort target)) {restTimeoutMicros = Just 1000000})
    void (listComputers client)
    expectRest RestTransportFailure (createComputer client (defaultCreateComputerParams "Box" "user"))

tlsTest :: TestTree
tlsTest = testCase "HTTPS rejects an untrusted certificate without plaintext fallback" $
  bounded $
    withTLSCertificate $ \credential _ -> withTLSPeer credential (\_ -> assertFailure "Untrusted REST TLS accepted") $ \target -> do
      client <- newRestClient ((optionsFor (webSocketPort target)) {restBaseUrl = "https://localhost:" <> Text.pack (show (webSocketPort target))})
      expectRest RestTransportFailure (listComputers client)

slowResponse :: MVar () -> MVar () -> Socket.Socket -> IO ()
slowResponse started closed socket = do
  _ <- readRequest socket
  Socket.sendAll socket "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n1\r\n{\r\n"
  putMVar started ()
  ended <- try @IOException (Socket.recv socket 1)
  case ended of
    Right bytes -> bytes @?= BS.empty
    Left err -> unless (isResourceVanishedError err) (throwIO err)
  putMVar closed ()

readRequest :: Socket.Socket -> IO (ByteString, ByteString)
readRequest socket = headers BS.empty
  where
    headers bytes = case BS.breakSubstring "\r\n\r\n" bytes of
      (_, rest) | BS.null rest -> receive >>= headers . (bytes <>)
      (headBytes, rest) -> do
        let sizes = [BS8.dropWhile (== ' ') (BS.drop 1 value) | line <- BS8.lines headBytes, let (key, value) = BS8.break (== ':') line, BS8.map toLower key == "content-length"]
        size <- case sizes of [] -> pure 0; [value] -> maybe (assertFailure "Invalid content length") pure (readMaybe (BS8.unpack value)); _ -> assertFailure "Duplicate content length"
        (headBytes,) <$> body size (BS.drop 4 rest)
    body size bytes | BS.length bytes >= size = pure bytes
    body size bytes = receive >>= body size . (bytes <>)
    receive = do
      bytes <- Socket.recv socket 4096
      when (BS.null bytes) (assertFailure "Unexpected peer EOF")
      pure bytes

optionsFor :: Int -> RestOptions
optionsFor port = (defaultRestOptions "OFFLINE_API_KEY") {restBaseUrl = urlFor port}

urlFor :: Int -> Text.Text
urlFor port = "http://127.0.0.1:" <> Text.pack (show port)

expectRest :: RestError -> IO a -> Assertion
expectRest expected action = try @RestError action >>= \case Left err -> err @?= expected; Right _ -> assertFailure "Expected REST failure"

decoded :: (FromJSON a) => Value -> IO a
decoded value = case fromJSON value of Error _ -> assertFailure "Fixture decoding failed"; Success result -> pure result

reject :: forall a. (FromJSON a) => Proxy a -> Value -> Assertion
reject _ value = case fromJSON value :: Result a of Error _ -> pure (); Success _ -> assertFailure "Malformed REST response accepted"

computerValue :: Value
computerValue = object ["id" .= String "machine", "name" .= String "Box", "hostname" .= String "host", "providerType" .= String "byom", "status" .= String "active", "createdAt" .= (9007199254740993 :: Integer), "relayClientUrl" .= String "wss://relay.invalid", "remoteUser" .= String "remote", "x-server" .= False]

templateValue :: Value
templateValue = object ["templateId" .= String "template", "repoUrl" .= String "https://repository.invalid/project", "templateName" .= String "Build", "defaultBranch" .= String "main", "createdBy" .= String "user", "createdAt" .= (1 :: Int), "buildStatus" .= object ["status" .= String "failed", "failureReason" .= String "setup_script_error", "buildStartedAt" .= (1 :: Int), "builtAt" .= (2 :: Int), "logs" .= String "Build log"], "lastUpdatedAt" .= Null, "environmentVariables" .= [object ["key" .= String "TOKEN", "value" .= String "SECRET_FIXTURE"], object ["key" .= String "TOKEN", "value" .= String "second"]], "userEnvironmentVariablesByUser" .= ([] :: [Value]), "setupScript" .= String "setup", "x-server" .= False]

sessionValue :: Value
sessionValue = object ["sessionId" .= String "session", "title" .= String "Work", "status" .= String "running", "messageCount" .= (2 :: Int), "createdAt" .= (1 :: Int), "updatedAt" .= (2 :: Int), "completedAt" .= (3 :: Int), "computerId" .= String "machine"]

metricValue :: Value
metricValue = object ["timestamp" .= String "sample", "cpuUsedPct" .= (12.5 :: Double), "cpuCount" .= (2 :: Int), "memUsed" .= (1 :: Int), "memTotal" .= (8 :: Int), "diskUsed" .= (3 :: Int), "diskTotal" .= (16 :: Int)]

page :: Key -> [Value] -> Value
page key items = object [key .= items, "pagination" .= object ["hasMore" .= True, "nextCursor" .= String "next"]]
