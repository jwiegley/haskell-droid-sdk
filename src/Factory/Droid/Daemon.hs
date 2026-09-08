{-# LANGUAGE OverloadedStrings #-}

-- | One scoped session on an existing daemon. The SDK owns its connection,
-- not the daemon or saved session. Ordinary scope exit only disconnects.
module Factory.Droid.Daemon
  ( DaemonCredential (..),
    DaemonOptions (..),
    defaultDaemonOptions,
    DaemonError (..),
    DaemonConnection,
    withConnection,
    connectionUser,
    getMcpConfig,
    updateMcpConfig,
    DaemonSession,
    sessionId,
    sessionStatus,
    authenticatedUser,
    withSession,
    withSessionHandlers,
    withResumedSession,
    withResumedSessionHandlers,
    sendPrompt,
    sendTurn,
    sendEvents,
    sendInput,
    sendInputEvents,
    sendOutput,
    sendOutputEvents,
    sendInputOutput,
    sendInputOutputEvents,
    interruptSession,
    onSessionEvent,
    listMcpServers,
    listMcpTools,
    listMcpRegistry,
    addMcpServer,
    removeMcpServer,
    toggleMcpServer,
    toggleMcpTool,
    authenticateMcpServer,
    cancelMcpAuth,
    clearMcpAuth,
    submitMcpAuthCode,
    submitMcpAuthError,
  )
where

import Control.Concurrent.STM (atomically, newEmptyTMVarIO, orElse, readTMVar, tryPutTMVar, writeTVar)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), withObject, (.:), (.:!), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.Types qualified as UUID
import Data.UUID.V4 (nextRandom)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Input (DroidInput)
import Factory.Droid.Interaction (DroidHandlers (..), defaultDroidHandlers, permissionRpcHandler, questionRpcHandler)
import Factory.Droid.Internal.Output (DroidOutput, DroidOutputResult)
import Factory.Droid.Internal.Session qualified as Core
import Factory.Droid.Internal.Stream (DroidEvent, DroidResult, DroidStreamMode, decodeDaemonNotification)
import Factory.Droid.Protocol
import Factory.Droid.Protocol.Dispatch
import Factory.Droid.Schema.Control (AddUserMessageParams)
import Factory.Droid.Schema.Discovery (GetUserInfoResult)
import Factory.Droid.Schema.MCP (ListMcpRegistryResult, ListMcpServersResult, ListMcpToolsResult, McpServerNameParams (..), RemoveMcpServerParams (..), SubmitMcpAuthCodeParams, SubmitMcpAuthErrorParams, ToggleMcpServerParams (..), ToggleMcpToolParams (..))
import Factory.Droid.Schema.MCP.Config (AddMcpServerParams, GetMcpConfigResult, McpConfigurationError (..), McpSessionOptions (..), UpdateMcpConfigParams, UpdateMcpConfigResult, defaultMcpSessionOptions, mcpInitializeFields, mcpLoadFields, validateMcpConfiguration)
import Factory.Droid.Schema.Notifications (CreateMessage (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import GHC.TypeLits (KnownSymbol)
import System.Timeout (timeout)

-- | Explicit credentials; no login files or environment variables are read.
-- A delegation grant is valid only alongside a token, not an API key.
data DaemonCredential = DaemonApiKey !Text | DaemonToken !Text !(Maybe Text)

instance Show DaemonCredential where
  show _ = "DaemonCredential <redacted>"

-- | Cwd, machine and model apply to new sessions and refer to the daemon host,
-- not this process. A model on resume is rejected rather than silently ignored.
-- Protocol version is explicit; the default follows CLI 0.212.1, not a negotiated
-- compatibility guarantee. TLS is enabled by the default transport options.
data DaemonOptions = DaemonOptions
  { daemonTarget :: !WebSocket.WebSocketTarget,
    daemonTransport :: !WebSocket.WebSocketOptions,
    daemonCredential :: !DaemonCredential,
    daemonWorkingDirectory :: !Text,
    daemonMachineId :: !Text,
    daemonModel :: !(Maybe Text),
    daemonTurnTimeoutMicros :: !(Maybe Int),
    daemonProtocolVersion :: !Text,
    daemonMcpOptions :: !McpSessionOptions
  }

instance Show DaemonOptions where
  show _ = "DaemonOptions <redacted>"

defaultDaemonOptions :: WebSocket.WebSocketTarget -> DaemonCredential -> Text -> DaemonOptions
defaultDaemonOptions target credential cwd = DaemonOptions target WebSocket.defaultWebSocketOptions credential cwd "local" Nothing Nothing "1.201.1" defaultMcpSessionOptions

data DaemonError = InvalidDaemonCredential | DaemonModelRequiresNewSession
  deriving stock (Eq, Show)

instance Exception DaemonError

-- | An authenticated connection without an initialized or loaded session.
data DaemonConnection = DaemonConnection !Core.SessionConnection !GetUserInfoResult

-- | Uses endpoint, transport, credential and protocol options only. No session
-- is created; global configuration mutations remain explicit caller operations.
withConnection :: DaemonOptions -> (DaemonConnection -> IO a) -> IO a
withConnection options = withDaemonConnection options True defaultMcpSessionOptions

connectionUser :: DaemonConnection -> GetUserInfoResult
connectionUser (DaemonConnection _ identity) = identity

getMcpConfig :: DaemonConnection -> IO GetMcpConfigResult
getMcpConfig (DaemonConnection connection _) = Core.connectionRequest connection 30000000 (\channel options -> Client.getDaemonMcpConfig channel options mempty)

updateMcpConfig :: DaemonConnection -> UpdateMcpConfigParams -> IO UpdateMcpConfigResult
updateMcpConfig (DaemonConnection connection _) params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  Core.connectionRequest connection 30000000 (\channel options -> Client.updateDaemonMcpConfig channel options validated)

withDaemonConnection :: DaemonOptions -> Bool -> McpSessionOptions -> (DaemonConnection -> IO a) -> IO a
withDaemonConnection options reject mcpOptions action = do
  when (Text.null (credentialText (daemonCredential options))) (throwIO InvalidDaemonCredential)
  WebSocket.withWebSocket (daemonTransport options) (daemonTarget options) $ \transport ->
    withRpcChannel (WebSocket.sendObject transport) (WebSocket.receiveObject transport) $ \channel ->
      Core.withSessionConnection channel (daemonBackend options) reject mcpOptions $ \connection -> do
        identity <- Core.connectionRequest connection 30000000 $ \rpc callOptions ->
          Client.call (Proxy @(WithEnvelope (MethodRequest "daemon.authenticate" Object))) rpc callOptions (authenticationParams (daemonCredential options))
        action (DaemonConnection connection identity)

-- The distinct handle prevents applying local replacement semantics to daemon
-- sessions, whose fork/compact operations have different ownership contracts.
data DaemonSession = DaemonSession !Core.DroidSession !GetUserInfoResult

sessionId :: DaemonSession -> Text
sessionId (DaemonSession session _) = Core.droidSessionId session

sessionStatus :: DaemonSession -> IO Core.DroidSessionStatus
sessionStatus (DaemonSession session _) = Core.droidSessionStatus session

authenticatedUser :: DaemonSession -> GetUserInfoResult
authenticatedUser (DaemonSession _ identity) = identity

withSession :: DaemonOptions -> (DaemonSession -> IO a) -> IO a
withSession options = withSessionHandlers options defaultDroidHandlers

withSessionHandlers :: DaemonOptions -> DroidHandlers -> (DaemonSession -> IO a) -> IO a
withSessionHandlers options handlers = withDaemonSession options handlers Nothing

withResumedSession :: DaemonOptions -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSession options = withResumedSessionHandlers options defaultDroidHandlers

withResumedSessionHandlers :: DaemonOptions -> DroidHandlers -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSessionHandlers options handlers identifier = withDaemonSession options handlers (Just identifier)

withDaemonSession :: DaemonOptions -> DroidHandlers -> Maybe Text -> (DaemonSession -> IO a) -> IO a
withDaemonSession options handlers saved action = do
  let credential = credentialText (daemonCredential options)
  when (Text.null credential) (throwIO InvalidDaemonCredential)
  when (isJust saved && isJust (daemonModel options)) (throwIO DaemonModelRequiresNewSession)
  forM_ (daemonTurnTimeoutMicros options) $ \micros -> when (micros < 0) (throwIO RpcInvalidTimeout)
  mcpOptions <- either throwIO pure (validateMcpConfiguration (daemonMcpOptions options))
  when (isJust saved && isJust (sessionBlockOnMcpLoad mcpOptions)) (throwIO McpInitOnlyOptionOnResume)
  identifier <- maybe (UUID.toText <$> nextRandom) pure saved
  withDaemonConnection options (isNothing (onDroidPermission handlers)) mcpOptions $ \(DaemonConnection connection identity) -> do
    let dispatcher = Core.connectionDispatcher connection
    void (registerRpcHandler dispatcher "daemon.request_permission" (sessionReply identifier True (permissionRpcHandler handlers) (permissionRpcHandler defaultDroidHandlers)))
    void (registerRpcHandler dispatcher "daemon.ask_user" (sessionReply identifier False (questionRpcHandler handlers) (questionRpcHandler defaultDroidHandlers)))
    void (onRpcNotification dispatcher (observeLifecycle connection identifier))
    Core.installMcpObserver connection "daemon.session_notification" (Just identifier) handlers
    (attached, snapshot) <- Core.sessionBoundary connection $ case saved of
      Nothing ->
        Core.callSettingsResult connection Nothing "daemon.initialize_session" $
          KeyMap.union (mcpInitializeFields mcpOptions) $
            KeyMap.fromList
              ( [ "machineId" .= daemonMachineId options,
                  "cwd" .= daemonWorkingDirectory options,
                  "token" .= credential,
                  "sessionId" .= identifier,
                  "autoRejectPermissionRequests" .= Core.connectionAutoRejectPermissions connection,
                  "sessionOriginHint" .= String "api",
                  "sessionSource" .= KeyMap.fromList ["platform" .= String "api", "delegationSessionId" .= identifier]
                ]
                  <> maybe [] (\model -> ["modelId" .= model]) (daemonModel options)
              )
      Just existing ->
        Core.callSettingsResult connection (Just existing) "daemon.load_session" $
          KeyMap.union (mcpLoadFields mcpOptions) (KeyMap.fromList ["sessionId" .= existing, "token" .= credential, "loadAllMessages" .= True, "autoRejectPermissionRequests" .= Core.connectionAutoRejectPermissions connection])
    unless (attached == identifier) (throwIO Core.DroidInvalidEvent)
    forM_ saved (\_ -> restorePending connection attached snapshot)
    session <- Core.newSessionHandle attached connection (daemonTurnTimeoutMicros options)
    action (DaemonSession session identity)

restorePending :: Core.SessionConnection -> Text -> Object -> IO ()
restorePending connection identifier snapshot =
  forM_ [("pendingPermissions", "daemon.request_permission"), ("pendingAskUserRequests", "daemon.ask_user")] $ \(key, method) -> do
    pending <- either (const (throwIO Core.DroidInvalidEvent)) pure (parseEither (.:! key) snapshot)
    forM_ (fromMaybe [] pending) $ \fields -> do
      requestId <- either (const (throwIO Core.DroidInvalidEvent)) pure (parseEither (.: "requestId") fields)
      let params = KeyMap.insert "sessionId" (String identifier) (KeyMap.delete "requestId" fields)
          context = Core.backendContext (Core.connectionBackend connection)
      dispatchRpcRequest (Core.connectionDispatcher connection) (context {envelopeBody = BaseRequest requestId method (Just (Object params)) mempty})

credentialText :: DaemonCredential -> Text
credentialText (DaemonApiKey key) = key
credentialText (DaemonToken token _) = token

authenticationParams :: DaemonCredential -> Object
authenticationParams credential = KeyMap.fromList ("caller" .= String "haskell-sdk" : fields)
  where
    fields = case credential of
      DaemonApiKey key -> ["apiKey" .= key]
      DaemonToken token grant -> ["token" .= token] <> maybe [] (\value -> ["actAsGrant" .= value]) grant

daemonBackend :: DaemonOptions -> Core.SessionBackend
daemonBackend options =
  Core.SessionBackend
    (WithEnvelope (Just (daemonProtocolVersion options)) Nothing mempty)
    decodeDaemonNotification
    submitMessage
    (\channel callOptions identifier -> Client.call (Proxy @(WithEnvelope (MethodRequest "daemon.interrupt_session" Object))) channel callOptions (KeyMap.singleton "sessionId" (String identifier)))

-- Register both listeners before sending: a create_message notification may
-- precede the immediate ACK. ACK and notification share the caller's deadline.
submitMessage :: RpcDispatcher -> RpcChannel -> Client.CallOptions -> Text -> AddUserMessageParams -> IO Object
submitMessage dispatcher channel options identifier input = do
  completed <- newEmptyTMVarIO
  failed <- newEmptyTMVarIO
  fields <- either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (toJSON input))
  let params = KeyMap.insert "sessionId" (String identifier) (KeyMap.union fields (KeyMap.singleton "userMessageSource" (String "api")))
      observe notification = case notificationPayload identifier notification of
        Right (Just payload)
          | KeyMap.lookup "type" payload == Just (String "create_message"),
            KeyMap.lookup "requestId" payload == Just (String (Client.callRequestId options)) -> do
              let parsed = parseEither parseJSON (Object payload) :: Either String CreateMessage
              atomically (void (tryPutTMVar completed (either (const (Left Core.DroidInvalidEvent)) (const (Right ())) parsed)))
        _ -> pure ()
      exchange = bracket (onRpcNotification dispatcher observe) id $ \_ ->
        bracket (onRpcError dispatcher (atomically . void . tryPutTMVar failed)) id $ \_ -> do
          result <- Client.call (Proxy @(WithEnvelope (MethodRequest "daemon.add_user_message" Object))) channel options params
          when (KeyMap.lookup "accepted" result == Just (Bool True)) $ do
            settlement <- atomically ((Right <$> readTMVar completed) `orElse` (Left <$> readTMVar failed))
            either throwIO (either throwIO pure) settlement
          pure result
  result <- maybe (Just <$> exchange) (`timeout` exchange) (Client.callTimeoutMicros options)
  maybe (throwIO RpcRequestTimedOut) pure result

-- Preserve the request's execution session, not the public handle's session:
-- daemon permissions can originate in associated worker sessions.
sessionReply :: Text -> Bool -> RpcRequestHandler -> RpcRequestHandler -> RpcRequestHandler
sessionReply ownedId allowAssociated handler cancel request = case baseRequestParams (envelopeBody request) of
  Just params -> case parseEither (withObject "daemon interaction" (.: "sessionId")) params of
    Right (identifier :: Text) -> do
      let associated = case parseEither (withObject "permission associations" (.:! "associatedSessionIds")) params of
            Right (Just (identifiers :: [Text])) -> ownedId `elem` identifiers
            _ -> False
          selected = if identifier == ownedId || (allowAssociated && associated) then handler else cancel
      response <- selected request
      pure $ case response of
        Right (Object fields) -> Right (Object (KeyMap.insert "sessionId" (String identifier) fields))
        Left err -> Left err
        Right _ -> Left (JsonRpcError RpcInternalError "Invalid interaction response" Nothing mempty)
    Left _ -> pure invalid
  Nothing -> pure invalid
  where
    invalid = Left (JsonRpcError RpcInvalidParams "Missing execution session" Nothing mempty)

notificationPayload :: Text -> JsonRpcBaseNotification -> Either String (Maybe Object)
notificationPayload identifier notification
  | baseNotificationMethod (envelopeBody notification) /= "daemon.session_notification" = Right Nothing
  | otherwise = case baseNotificationParams (envelopeBody notification) of
      Nothing -> Left "Missing notification parameters"
      Just params ->
        parseEither
          ( withObject "daemon notification" $ \fields -> do
              actual <- fields .: "sessionId"
              if actual /= identifier then pure Nothing else Just <$> fields .: "notification"
          )
          params

observeLifecycle :: Core.SessionConnection -> Text -> JsonRpcBaseNotification -> IO ()
observeLifecycle connection identifier notification = case notificationPayload identifier notification of
  Right (Just payload)
    | Just (String kind) <- KeyMap.lookup "type" payload,
      kind `elem` ["session_closed", "session_unsubscribed", "session_inactivity", "session_process_exited"] -> do
        atomically (writeTVar (Core.connectionOpen connection) False)
        closeRpcChannel (Core.connectionChannel connection)
  _ -> pure ()

sendPrompt :: DaemonSession -> Text -> (Text -> IO ()) -> IO DroidResult
sendPrompt (DaemonSession session _) = Core.sendPrompt session

sendTurn :: DaemonSession -> Text -> (Text -> IO ()) -> IO DroidResult
sendTurn (DaemonSession session _) = Core.sendDroidTurn session

sendEvents :: DaemonSession -> DroidStreamMode -> Text -> (DroidEvent -> IO ()) -> IO DroidResult
sendEvents (DaemonSession session _) = Core.sendDroidEvents session

sendInput :: DaemonSession -> DroidInput -> (Text -> IO ()) -> IO DroidResult
sendInput (DaemonSession session _) = Core.sendDroidInput session

sendInputEvents :: DaemonSession -> DroidStreamMode -> DroidInput -> (DroidEvent -> IO ()) -> IO DroidResult
sendInputEvents (DaemonSession session _) = Core.sendDroidInputEvents session

sendOutput :: DaemonSession -> DroidOutput a -> Text -> (Text -> IO ()) -> IO (DroidOutputResult a)
sendOutput (DaemonSession session _) = Core.sendDroidOutput session

sendOutputEvents :: DaemonSession -> DroidOutput a -> DroidStreamMode -> Text -> (DroidEvent -> IO ()) -> IO (DroidOutputResult a)
sendOutputEvents (DaemonSession session _) = Core.sendDroidOutputEvents session

sendInputOutput :: DaemonSession -> DroidOutput a -> DroidInput -> (Text -> IO ()) -> IO (DroidOutputResult a)
sendInputOutput (DaemonSession session _) = Core.sendDroidInputOutput session

sendInputOutputEvents :: DaemonSession -> DroidOutput a -> DroidStreamMode -> DroidInput -> (DroidEvent -> IO ()) -> IO (DroidOutputResult a)
sendInputOutputEvents (DaemonSession session _) = Core.sendDroidInputOutputEvents session

interruptSession :: DaemonSession -> IO ()
interruptSession (DaemonSession session _) = Core.interruptDroidSession session

-- | Runs on dispatcher intake. Keep callbacks brief; do not start a turn,
-- interrupt an incompletely submitted turn, or wait for further intake here.
-- Turn callbacks passed to sendEvents run on the caller's thread instead.
onSessionEvent :: DaemonSession -> (Either Core.DroidError DroidEvent -> IO ()) -> IO (IO ())
onSessionEvent (DaemonSession session _) = Core.onDroidSessionEvent session

listMcpServers :: DaemonSession -> IO ListMcpServersResult
listMcpServers session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_mcp_servers" Object))) session (mempty :: Object)

listMcpTools :: DaemonSession -> IO ListMcpToolsResult
listMcpTools session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_mcp_tools" Object))) session (mempty :: Object)

listMcpRegistry :: DaemonSession -> IO ListMcpRegistryResult
listMcpRegistry session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_mcp_registry" Object))) session (mempty :: Object)

addMcpServer :: DaemonSession -> AddMcpServerParams -> IO SuccessResult
addMcpServer session params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.add_mcp_server" Object))) session validated

removeMcpServer :: DaemonSession -> Text -> IO SuccessResult
removeMcpServer session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.remove_mcp_server" Object))) session (RemoveMcpServerParams name mempty)

toggleMcpServer :: DaemonSession -> Text -> Bool -> IO SuccessResult
toggleMcpServer session name enabled = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.toggle_mcp_server" Object))) session (ToggleMcpServerParams name enabled mempty)

toggleMcpTool :: DaemonSession -> Text -> Text -> Bool -> IO SuccessResult
toggleMcpTool session name tool enabled = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.toggle_mcp_tool" Object))) session (ToggleMcpToolParams name tool enabled mempty)

-- | Five-minute RPC budget. The acknowledgement is not proof of authentication
-- completion, and timeout does not automatically cancel shared server-side OAuth.
authenticateMcpServer :: DaemonSession -> Text -> IO SuccessResult
authenticateMcpServer session name = daemonSessionRequest Core.MutatingRequest 300000000 (Proxy @(WithEnvelope (MethodRequest "daemon.authenticate_mcp_server" Object))) session (McpServerNameParams name mempty)

cancelMcpAuth :: DaemonSession -> Text -> IO SuccessResult
cancelMcpAuth session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.cancel_mcp_auth" Object))) session (McpServerNameParams name mempty)

clearMcpAuth :: DaemonSession -> Text -> IO SuccessResult
clearMcpAuth session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.clear_mcp_auth" Object))) session (McpServerNameParams name mempty)

submitMcpAuthCode :: DaemonSession -> SubmitMcpAuthCodeParams -> IO SuccessResult
submitMcpAuthCode = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.submit_mcp_auth_code" Object)))

submitMcpAuthError :: DaemonSession -> SubmitMcpAuthErrorParams -> IO SuccessResult
submitMcpAuthError = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.submit_mcp_auth_error" Object)))

daemonSessionRequest :: (KnownSymbol method, ToJSON params, FromJSON result) => Core.RequestEffect -> Int -> Proxy (WithEnvelope (MethodRequest method Object)) -> DaemonSession -> params -> IO result
daemonSessionRequest effect deadline method (DaemonSession session _) params =
  Core.sessionRequestWithTimeout effect deadline session $ \channel options -> do
    fields <- either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (toJSON params))
    Client.call method channel options (KeyMap.insert "sessionId" (String (Core.droidSessionId session)) fields)
