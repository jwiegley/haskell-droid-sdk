{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Authenticated loopback MCP hosting. Explicit starts are caller-owned;
-- scoped leases share a runtime and release only their own ownership.
module Factory.Droid.MCP.Server
  ( McpServer,
    McpServerOptions (..),
    defaultMcpServerOptions,
    McpServerError (..),
    newMcpServer,
    startMcpServer,
    closeMcpServer,
    getMcpServerConfig,
    withMcpServer,
    withMcpServers,
    withMcpServerOptions,
    mcpServerName,
  )
where

import Control.Concurrent (ThreadId, myThreadId, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (Async, asyncThreadId, asyncWithUnmask, mapConcurrently_, race, uninterruptibleCancel, wait)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVarMasked, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (Exception, SomeException, bracket, bracketOnError, evaluate, finally, mask, mask_, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (unless, when)
import Crypto.Random (getRandomBytes)
import Data.Aeson (Object, Value (..), eitherDecodeStrict', encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray qualified as Bytes
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (isInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Factory.Droid.MCP.Tool
import Factory.Droid.Schema.MCP (HttpHeader (..), StdioMcp (..))
import Factory.Droid.Schema.MCP.Config (McpOAuthConfig (..), McpRemoteConfig (..), McpServerConfig (..), McpSessionOptions (..), validateMcpConfiguration)
import Network.HTTP.Media qualified as Media
import Network.HTTP.Types
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Timeout (timeout)

data McpServerOptions = McpServerOptions
  { hostedServerName :: !Text,
    hostedServerVersion :: !Text,
    hostedRequestLimitBytes :: !Int,
    hostedResponseLimitBytes :: !Int,
    hostedToolTimeoutMicros :: !(Maybe Int)
  }
  deriving stock (Eq)

instance Show McpServerOptions where
  show _ = "McpServerOptions <redacted>"

defaultMcpServerOptions :: Text -> McpServerOptions
defaultMcpServerOptions name = McpServerOptions name "1.0.0" (4 * 1024 * 1024) (10 * 1024 * 1024) (Just 30000000)

data McpServerError = InvalidMcpServerOptions | DuplicateMcpToolName | McpServerStartFailure | McpServerCloseFromHandler | McpServerNameConflict | HostedMcpRequiresLocalDaemon
  deriving stock (Eq, Show)

instance Exception McpServerError

data Runtime = Runtime
  { runtimeSocket :: !Socket.Socket,
    runtimeTask :: !(Async ()),
    runtimeWorkers :: !(TVar (Map ThreadId (Async ()))),
    runtimeAlive :: !(IORef Bool),
    runtimeConfig :: !McpServerConfig
  }

data ServerState = Stopped | Running !Runtime !Bool !Int | Closing !Runtime !(MVar (Either SomeException ()))

data CloseAction = NoClose | AwaitClose !(MVar (Either SomeException ())) | FinishClose !Runtime !(MVar (Either SomeException ()))

data McpServer = McpServer !McpServerOptions ![McpTool] !(MVar ServerState)

instance Eq McpServer where
  McpServer _ _ first == McpServer _ _ second = first == second

instance Show McpServer where
  show _ = "McpServer <redacted>"

newMcpServer :: McpServerOptions -> [McpTool] -> IO McpServer
newMcpServer options tools = do
  unless (not (Text.null (hostedServerName options)) && not (Text.null (hostedServerVersion options)) && hostedRequestLimitBytes options > 0 && hostedResponseLimitBytes options >= 128 && maybe True (>= 0) (hostedToolTimeoutMicros options)) (throwIO InvalidMcpServerOptions)
  unless (Map.size (Map.fromList [(toolName tool, ()) | tool <- tools]) == length tools) (throwIO DuplicateMcpToolName)
  McpServer options tools <$> newMVar Stopped

mcpServerName :: McpServer -> Text
mcpServerName (McpServer options _ _) = hostedServerName options

-- | Explicit start pins ownership until explicit close. Scoped users do not
-- stop a manually started server. Repeated starts share the committed runtime.
startMcpServer :: McpServer -> IO McpServerConfig
startMcpServer server@(McpServer options tools state) = do
  decision <- modifyMVarMasked state $ \current -> case current of
    Running runtime _ leases -> do
      alive <- readIORef (runtimeAlive runtime)
      unless alive (throwIO McpServerStartFailure)
      pure (Running runtime True leases, Right (runtimeConfig runtime))
    Closing runtime done -> rejectHandlerClose runtime >> pure (current, Left done)
    Stopped -> do
      runtime <- startRuntime options tools
      pure (Running runtime True 0, Right (runtimeConfig runtime))
  case decision of
    Left done -> readMVar done >>= either throwIO (const (startMcpServer server))
    Right config -> pure config

closeMcpServer :: McpServer -> IO ()
closeMcpServer (McpServer _ _ state) = uninterruptibleMask_ $ do
  closing <- modifyMVar state $ \current -> case current of
    Stopped -> pure (Stopped, NoClose)
    Closing runtime done -> rejectHandlerClose runtime >> pure (current, AwaitClose done)
    Running runtime _ _ -> rejectHandlerClose runtime >> prepareClose runtime
  finishClose state closing

getMcpServerConfig :: McpServer -> IO (Maybe McpServerConfig)
getMcpServerConfig (McpServer _ _ state) = withMVar state $ \case
  Stopped -> pure Nothing
  Closing _ _ -> pure Nothing
  Running runtime _ _ -> do
    alive <- readIORef (runtimeAlive runtime)
    pure (if alive then Just (runtimeConfig runtime) else Nothing)

withMcpServer :: McpServer -> (McpServerConfig -> IO a) -> IO a
withMcpServer (McpServer options tools state) action = bracket acquire release (action . runtimeConfig)
  where
    acquire = do
      decision <- modifyMVar state $ \current -> case current of
        Running runtime manual leases -> do
          alive <- readIORef (runtimeAlive runtime)
          unless alive (throwIO McpServerStartFailure)
          pure (Running runtime manual (leases + 1), Right runtime)
        Closing runtime done -> rejectHandlerClose runtime >> pure (current, Left done)
        Stopped -> do
          runtime <- startRuntime options tools
          pure (Running runtime False 1, Right runtime)
      case decision of
        Left done -> readMVar done >>= either throwIO (const acquire)
        Right runtime -> pure runtime
    release borrowed = uninterruptibleMask_ $ do
      closing <- modifyMVar state $ \current -> case current of
        Running runtime manual leases
          | runtimeAlive runtime == runtimeAlive borrowed ->
              if leases == 1 && not manual
                then prepareClose runtime
                else pure (Running runtime manual (leases - 1), NoClose)
        _ -> pure (current, NoClose)
      finishClose state closing

prepareClose :: Runtime -> IO (ServerState, CloseAction)
prepareClose runtime = do
  done <- newEmptyMVar
  pure (Closing runtime done, FinishClose runtime done)

rejectHandlerClose :: Runtime -> IO ()
rejectHandlerClose runtime = do
  caller <- myThreadId
  workers <- readTVarIO (runtimeWorkers runtime)
  when (Map.member caller workers) (throwIO McpServerCloseFromHandler)

-- Cleanup runs outside the state lock: handler finalizers may inspect config.
-- Every concurrent closer observes the same completion, and starts wait for it.
finishClose :: MVar ServerState -> CloseAction -> IO ()
finishClose _ NoClose = pure ()
finishClose _ (AwaitClose done) = readMVar done >>= either throwIO pure
finishClose state (FinishClose runtime done) = do
  result <- try @SomeException (stopRuntime runtime)
  modifyMVar_ state (const (pure Stopped))
  putMVar done result
  either throwIO pure result

withMcpServers :: [McpServer] -> ([McpServerConfig] -> IO a) -> IO a
withMcpServers [] action = action []
withMcpServers (server : rest) action = withMcpServer server $ \config -> withMcpServers rest (action . (config :))

-- | Add leased hosted endpoints to existing configuration. Caller-started
-- runtimes retain their explicit ownership; failed startup unwinds all leases.
withMcpServerOptions :: [McpServer] -> McpSessionOptions -> (McpSessionOptions -> IO a) -> IO a
withMcpServerOptions servers options action = do
  validated <- either throwIO pure (validateMcpConfiguration options)
  let names = map mcpServerName servers
      external = maybe [] (map nameOf) (sessionMcpServers validated)
  when (Map.size (Map.fromList [(name, ()) | name <- names]) /= length names || any (`elem` external) names) (throwIO McpServerNameConflict)
  withMcpServers servers $ \configs ->
    action (if null configs then validated else validated {sessionMcpServers = Just (fromMaybe [] (sessionMcpServers validated) <> configs)})
  where
    nameOf (McpStdioConfig value) = stdioMcpName value
    nameOf (McpHttpConfig value) = mcpRemoteName value
    nameOf (McpSseConfig value) = mcpRemoteName value

startRuntime :: McpServerOptions -> [McpTool] -> IO Runtime
startRuntime options tools = mask $ \restore -> do
  token <- Base64.encodeUnpadded <$> (getRandomBytes 32 :: IO ByteString)
  workers <- newTVarIO Map.empty
  alive <- newIORef True
  bracketOnError (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
    Socket.setSocketOption listener Socket.ReuseAddr 1
    Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen listener Socket.maxListenQueue
    address <- Socket.getSocketName listener
    port <- case address of Socket.SockAddrInet value _ -> pure (fromIntegral value :: Int); _ -> throwIO McpServerStartFailure
    ready <- newEmptyMVar
    let authority = B8.pack ("127.0.0.1:" <> show port)
        settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar ready ()) $ Warp.setFork (ownedFork workers) $ Warp.setOnException (\_ _ -> pure ()) $ Warp.setOnExceptionResponse (const (Wai.responseLBS status500 [(hContentType, "application/json")] (encode (rpcError Null (-32603) "Internal server error")))) $ Warp.setMaximumBodyFlush (Just 0) $ Warp.setGracefulShutdownTimeout (Just 0) $ Warp.setGracefulCloseTimeout1 0 $ Warp.setTimeout 10 Warp.defaultSettings
        config = McpHttpConfig (McpRemoteConfig (hostedServerName options) (Text.decodeUtf8 ("http://" <> authority <> "/mcp")) (Just [HttpHeader "Authorization" (Text.decodeUtf8 ("Bearer " <> token)) mempty]) (Just McpOAuthDisabled) mempty)
    task <- asyncWithUnmask $ \unmask -> unmask (Warp.runSettingsSocket settings listener (application options tools authority token alive)) `finally` writeIORef alive False
    let runtime = Runtime listener task workers alive config
    ( do
        started <- restore (race (threadDelay 5000000) (race (wait task) (takeMVar ready)))
        case started of
          Right (Right ()) -> pure runtime
          _ -> throwIO McpServerStartFailure
      )
      `onException` stopRuntime runtime

-- Warp's accept thread owns admission. Shutdown joins it before snapshotting
-- workers, so no worker can register after the cleanup snapshot.
ownedFork :: TVar (Map ThreadId (Async ())) -> ((forall a. IO a -> IO a) -> IO ()) -> IO ()
ownedFork workers action = mask_ $ do
  ready <- newEmptyMVar
  worker <- asyncWithUnmask $ \unmask -> do
    identifier <- myThreadId
    (takeMVar ready >> action unmask) `finally` atomically (modifyTVar' workers (Map.delete identifier))
  atomically (modifyTVar' workers (Map.insert (asyncThreadId worker) worker))
  putMVar ready ()

stopRuntime :: Runtime -> IO ()
stopRuntime runtime = do
  writeIORef (runtimeAlive runtime) False
  Socket.close (runtimeSocket runtime)
  uninterruptibleCancel (runtimeTask runtime)
  active <- Map.elems <$> readTVarIO (runtimeWorkers runtime)
  mapConcurrently_ uninterruptibleCancel active

application :: McpServerOptions -> [McpTool] -> ByteString -> ByteString -> IORef Bool -> Wai.Application
application options tools authority token alive request respond = do
  open <- readIORef alive
  let headers = Wai.requestHeaders request
      values key = [value | (name, value) <- headers, name == key]
      hosts = [authority, "localhost" <> B8.dropWhile (/= ':') authority]
      origins = map ("http://" <>) hosts
      json status value extra = respond (Wai.responseLBS status ((hContentType, "application/json") : extra) (encode value))
      rejected status message = json status (rpcError Null (-32603) message)
  if
    | not open -> rejected status503 "Server closed" []
    | Wai.rawPathInfo request /= "/mcp" || not (BS.null (Wai.rawQueryString request)) -> respond (Wai.responseLBS status404 [] "")
    | Wai.requestMethod request /= methodPost -> respond (Wai.responseLBS status405 [(hAllow, "POST")] "")
    | not (case values hHost of [host] -> B8.map toLower host `elem` hosts; _ -> False) -> rejected status403 "Invalid Host" []
    | not (case values "Origin" of [] -> True; [origin] -> origin `elem` origins; _ -> False) -> rejected status403 "Invalid Origin" []
    | not (case values hAuthorization of [authorization] -> authorized token authorization; _ -> False) -> rejected status401 "Unauthorized" [(hWWWAuthenticate, "Bearer")]
    | not (acceptsMcp (BS.intercalate "," (values hAccept))) -> rejected status406 "Unsupported Accept" []
    | not (case values hContentType of [value] -> jsonMedia value; _ -> False) -> rejected status415 "Unsupported Content-Type" []
    | otherwise -> do
        body <- readBody (hostedRequestLimitBytes options) request
        case body of
          Nothing -> rejected status413 "Request too large" []
          Just bytes -> case eitherDecodeStrict' bytes of
            Left _ -> json status400 (rpcError Null (-32700) "Parse error") []
            Right value -> do
              let protocol = case values "MCP-Protocol-Version" of [] -> Right (); [version] | version `elem` map Text.encodeUtf8 supportedVersions -> Right (); _ -> Left ()
                  initialize = case value of Object fields -> KeyMap.lookup "method" fields == Just (String "initialize"); _ -> False
              if not initialize && protocol == Left ()
                then rejected status400 "Unsupported protocol version" []
                else do
                  result <- exchange options tools value
                  case result of
                    Nothing -> respond (Wai.responseLBS status202 [] "")
                    Just (status, message) -> do
                      let encoded = encode message
                      size <- evaluate (BL.length encoded)
                      if size > fromIntegral (hostedResponseLimitBytes options)
                        then json status500 (rpcError Null (-32603) "Response too large") []
                        else respond (Wai.responseLBS status [(hContentType, "application/json")] encoded)

acceptsMcp :: ByteString -> Bool
acceptsMcp header = case Media.parseQuality header :: Maybe [Media.Quality Media.MediaType] of
  Nothing -> False
  Just entries -> all (\wanted -> any (\entry -> Media.isAcceptable entry && sameMedia (Media.qualityData entry) wanted) entries) ["application/json", "text/event-stream"]

jsonMedia :: ByteString -> Bool
jsonMedia header = case Media.parseAccept header :: Maybe Media.MediaType of
  Just value -> sameMedia value "application/json"
  Nothing -> False

sameMedia :: Media.MediaType -> Media.MediaType -> Bool
sameMedia first second = Media.mainType first == Media.mainType second && Media.subType first == Media.subType second

readBody :: Int -> Wai.Request -> IO (Maybe ByteString)
readBody limit request = case Wai.requestBodyLength request of
  Wai.KnownLength size | size > fromIntegral limit -> pure Nothing
  _ -> go limit []
  where
    go remaining chunks = do
      chunk <- Wai.getRequestBodyChunk request
      if BS.null chunk
        then pure (Just (BS.concat (reverse chunks)))
        else
          if BS.length chunk > remaining
            then pure Nothing
            else go (remaining - BS.length chunk) (chunk : chunks)

authorized :: ByteString -> ByteString -> Bool
authorized token header = case B8.break (== ' ') header of
  (scheme, supplied) -> B8.map toLower scheme == "bearer" && not (BS.null supplied) && Bytes.constEq token (BS.tail supplied)

exchange :: McpServerOptions -> [McpTool] -> Value -> IO (Maybe (Status, Value))
exchange options tools input = case input of
  Array values
    | Vector.null values || Vector.length values > 32 || any isBatch values -> pure (Just (status400, rpcError Null (-32600) "Invalid batch"))
    | any isInitialize (Vector.toList values) -> pure (Just (status400, rpcError Null (-32600) "Initialize must be a single request"))
    | otherwise -> do
        replies <- traverse (exchange options tools) (Vector.toList values)
        let messages = map snd (catMaybes replies)
        pure (if null messages then Nothing else Just (status200, toJSON messages))
  Object fields | KeyMap.lookup "jsonrpc" fields == Just (String "2.0") ->
    case (KeyMap.lookup "id" fields, KeyMap.lookup "method" fields) of
      (Nothing, Just (String _)) | validParams (KeyMap.lookup "params" fields) -> pure Nothing
      (Just identifier, Nothing) | validId identifier && validResponse fields -> pure Nothing
      (Just identifier, Just (String _)) | BL.length (encode (rpcError identifier (-32603) "Internal server error")) > fromIntegral (hostedResponseLimitBytes options) -> pure (Just (status400, rpcError Null (-32600) "Request ID too large"))
      (Just identifier, Just (String method)) | validId identifier -> do
        let params = case KeyMap.lookup "params" fields of Nothing -> Just mempty; Just (Object value) | validParams (Just (Object value)) -> Just value; _ -> Nothing
        result <- maybe (pure (Left (-32602, "Invalid parameters"))) (dispatch options tools method) params
        pure (Just (status200, either (uncurry (rpcError identifier)) (\value -> object ["jsonrpc" .= String "2.0", "id" .= identifier, "result" .= value]) result))
      _ -> pure (Just (status400, rpcError Null (-32600) "Invalid request"))
  _ -> pure (Just (status400, rpcError Null (-32600) "Invalid request"))
  where
    isInitialize (Object fields) = KeyMap.lookup "method" fields == Just (String "initialize")
    isInitialize _ = False
    isBatch (Array _) = True
    isBatch _ = False

dispatch :: McpServerOptions -> [McpTool] -> Text -> Object -> IO (Either (Int, Text) Value)
dispatch options tools method params = case method of
  "initialize" -> case (KeyMap.lookup "protocolVersion" params, KeyMap.lookup "capabilities" params, KeyMap.lookup "clientInfo" params) of
    (Just (String version), Just (Object _), Just (Object client))
      | Just (String _) <- KeyMap.lookup "name" client,
        Just (String _) <- KeyMap.lookup "version" client ->
          pure (Right (object ["protocolVersion" .= (if version `elem` supportedVersions then version else "2025-11-25"), "capabilities" .= object ["tools" .= object []], "serverInfo" .= object ["name" .= hostedServerName options, "version" .= hostedServerVersion options]]))
    _ -> pure (Left (-32602, "Invalid initialize parameters"))
  "ping" -> pure (Right (object []))
  "tools/list" -> pure (Right (object ["tools" .= map describe tools]))
  "tools/call" -> case (KeyMap.lookup "name" params, KeyMap.lookup "arguments" params) of
    (Just (String name), arguments) -> case arguments of
      Just value | not (isObject value) -> pure (Left (-32602, "Invalid tool parameters"))
      _ -> case Map.lookup name registry of
        Nothing -> pure (Right (toJSON (errorResult "Unknown tool")))
        Just tool -> do
          let objectArguments = case arguments of Just (Object value) -> value; _ -> mempty
              invoke = invokeTool tool objectArguments
          result <- case hostedToolTimeoutMicros options of
            Nothing -> invoke
            Just micros -> fromMaybe (Right (errorResult "Tool request timed out")) <$> timeout micros invoke
          pure (either (const (Left (-32602, "Invalid tool result"))) (Right . toJSON) result)
    _ -> pure (Left (-32602, "Invalid tool parameters"))
  _ -> pure (Left (-32601, "Method not found"))
  where
    registry = Map.fromList [(toolName tool, tool) | tool <- tools]
    describe tool = object (["name" .= toolName tool, "description" .= toolDescription tool, "inputSchema" .= toolInputSchema tool] <> maybe [] (\value -> ["outputSchema" .= value]) (toolOutputSchema tool))
    isObject (Object _) = True
    isObject _ = False

validId :: Value -> Bool
validId (String _) = True
validId (Number value) = isInteger value
validId _ = False

validParams :: Maybe Value -> Bool
validParams Nothing = True
validParams (Just (Object fields)) = case KeyMap.lookup "_meta" fields of Nothing -> True; Just (Object _) -> True; _ -> False
validParams _ = False

validResponse :: Object -> Bool
validResponse fields = case (KeyMap.lookup "result" fields, KeyMap.lookup "error" fields) of
  (Just result@(Object _), Nothing) -> validParams (Just result)
  (Nothing, Just (Object problem)) -> case (KeyMap.lookup "code" problem, KeyMap.lookup "message" problem) of
    (Just (Number code), Just (String _)) -> isInteger code
    _ -> False
  _ -> False

rpcError :: Value -> Int -> Text -> Value
rpcError identifier code message = object ["jsonrpc" .= String "2.0", "id" .= identifier, "error" .= object ["code" .= code, "message" .= message]]

supportedVersions :: [Text]
supportedVersions = ["2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05", "2024-10-07"]
