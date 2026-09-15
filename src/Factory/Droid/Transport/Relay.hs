{-# LANGUAGE OverloadedStrings #-}

-- | Relay authentication over the existing verified WebSocket owner. Relay
-- control frames precede, and are distinct from, daemon JSON-RPC authentication.
module Factory.Droid.Transport.Relay
  ( RelayCredential (..),
    RelayOptions (..),
    defaultRelayOptions,
    RelayAuthCode (..),
    RelayAuthFailure (..),
    RelayError (..),
    RelayConnection,
    relayTransport,
    relayDaemonAuthentication,
    relayOrganization,
    withRelayConnection,
    withRelayConnectionObserved,
    RelayTunnel,
    defaultTunnelWebSocketOptions,
    relayTunnelTarget,
    withRelayTunnel,
    sendTunnelData,
    receiveTunnelData,
    isRelayTunnelOpen,
    closeRelayTunnel,
  )
where

import Control.Exception (Exception, catch, finally, mask_, onException, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecode, encode, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Scientific (Scientific, isInteger, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Daemon (DaemonAuthentication (..), DaemonCredential (..))
import Factory.Droid.Internal.Exception (finallyPreserving)
import Factory.Droid.Internal.JSON (additionalFields, requireLiteral)
import Factory.Droid.Transport (ObjectTransport (..), TransportKind (RelayTransport), objectTransport)
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.HTTP.Types.URI (urlEncode)
import System.Timeout (timeout)

-- | API keys carry their own organization. Bearer tokens are fetched at relay
-- connect and daemon auth/init/load; the organization is fetched only at relay
-- connect. Grants belong to daemon auth, never relay.authenticate.
data RelayCredential
  = RelayApiKey !Text
  | RelayTokenProvider !(IO (Maybe Text)) !(IO (Maybe Text)) !(Maybe Text)

instance Show RelayCredential where show _ = "RelayCredential <redacted>"

data RelayOptions = RelayOptions
  { relayAuthenticationTimeoutMicros :: !Int,
    relayPreludeByteLimit :: !Int,
    relayPreludeMessageLimit :: !Int
  }
  deriving stock (Eq, Show)

defaultRelayOptions :: RelayOptions
defaultRelayOptions = RelayOptions 10000000 (10 * 1024 * 1024) 64

defaultTunnelWebSocketOptions :: WebSocket.WebSocketOptions
defaultTunnelWebSocketOptions = WebSocket.defaultWebSocketOptions {WebSocket.webSocketConnectTimeoutMicros = 30000000}

data RelayAuthCode = RelayComputerAlreadyConnected | RelayUnauthorized | RelayServiceUnavailable | RelayRateLimited deriving stock (Eq, Show)

instance FromJSON RelayAuthCode where
  parseJSON = withText "RelayAuthCode" $ \case
    "computer_already_connected" -> pure RelayComputerAlreadyConnected
    "unauthorized" -> pure RelayUnauthorized
    "service_unavailable" -> pure RelayServiceUnavailable
    "rate_limited" -> pure RelayRateLimited
    _ -> fail "Unknown relay authentication code"

data RelayAuthFailure = RelayAuthFailure
  { relayFailureMessage :: !Text,
    relayFailureCode :: !(Maybe RelayAuthCode),
    relayFailureRetryable :: !(Maybe Bool),
    relayFailureRetryAfterMillis :: !(Maybe Scientific),
    relayFailureAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show RelayAuthFailure where show _ = "RelayAuthFailure <redacted>"

instance FromJSON RelayAuthFailure where
  parseJSON = withObject "RelayAuthFailure" $ \fields -> do
    requireLiteral "type" "relay.auth_error" fields
    message <- fields .: "message"
    code <- fields .:! "code"
    retryable <- fields .:! "retryable"
    retryAfter <- fields .:! "retryAfterMs"
    let validDelay value = let number = toRealFloat value :: Double in number > 0 && not (isInfinite number) && isInteger value
    when (maybe False (not . validDelay) retryAfter) (fail "Invalid relay retry delay")
    pure (RelayAuthFailure message code retryable retryAfter (additionalFields ["type", "message", "code", "retryable", "retryAfterMs"] fields))

data RelayError = InvalidRelayOptions | InvalidRelayTunnelTarget | RelayTunnelGrantUnsupported | RelayClosed | RelayMissingToken | RelayAuthenticationTimedOut | RelayAuthenticationRejected !RelayAuthFailure | RelayMalformedAuthentication | RelayPreludeTooLarge
  deriving stock (Eq)

instance Show RelayError where
  show = \case
    InvalidRelayOptions -> "InvalidRelayOptions"
    InvalidRelayTunnelTarget -> "InvalidRelayTunnelTarget"
    RelayTunnelGrantUnsupported -> "RelayTunnelGrantUnsupported"
    RelayClosed -> "RelayClosed"
    RelayMissingToken -> "RelayMissingToken"
    RelayAuthenticationTimedOut -> "RelayAuthenticationTimedOut"
    RelayAuthenticationRejected _ -> "RelayAuthenticationRejected <redacted>"
    RelayMalformedAuthentication -> "RelayMalformedAuthentication"
    RelayPreludeTooLarge -> "RelayPreludeTooLarge"

instance Exception RelayError

data RelayConnection = RelayConnection
  { relayTransport :: !ObjectTransport,
    relayDaemonAuthentication :: !DaemonAuthentication,
    relayOrganization :: !(Maybe Text)
  }

instance Show RelayConnection where show _ = "RelayConnection <redacted>"

-- | Provider fetches occur inside socket ownership, before the handshake budget.
-- A new scope fetches fresh credentials; there is no implicit reconnect/replay.
-- The returned object transport remains nonlocal for hosted-MCP safety.
withRelayConnection :: WebSocket.WebSocketOptions -> WebSocket.WebSocketTarget -> RelayOptions -> RelayCredential -> (RelayConnection -> IO a) -> IO a
withRelayConnection socketOptions target options credential = withRelayConnectionObserved socketOptions target options credential (pure ())

-- | Report transport acquisition before relay authentication, without adding
-- a reader or changing the ownership of the enclosing WebSocket scope.
withRelayConnectionObserved :: WebSocket.WebSocketOptions -> WebSocket.WebSocketTarget -> RelayOptions -> RelayCredential -> IO () -> (RelayConnection -> IO a) -> IO a
withRelayConnectionObserved socketOptions target options credential opened action = do
  validateRelayOptions options
  WebSocket.withWebSocket socketOptions target $ \socket -> do
    opened
    (token, organization, daemonAuthentication) <- resolveCredential credential
    let wire = RelayWire (WebSocket.sendObject socket) (WebSocket.sendObject socket) (WebSocket.receiveObjectWithClose socket) Just (fromIntegral . BL.length . encode)
    prefix <- authenticateRelay options token organization wire `catch` \(WebSocket.WebSocketClose code _) -> throwIO (WebSocket.WebSocketPeerClosed code)
    withRelayStream wire prefix $ \stream ->
      action (RelayConnection ((objectTransport (streamSend stream) (streamReceive stream)) {transportKind = RelayTransport}) daemonAuthentication organization)

data RelayTunnel = RelayTunnel !(RelayStream WebSocket.WebSocketMessage) !(IO ())

instance Show RelayTunnel where show _ = "RelayTunnel <redacted>"

-- | Select the canonical tunnel route using only the relay origin. Computer
-- identity is an encoded path component, never an injected route/query.
relayTunnelTarget :: WebSocket.WebSocketTarget -> Text -> Int -> Either RelayError WebSocket.WebSocketTarget
relayTunnelTarget origin computerId port
  | Text.null computerId || computerId `elem` [".", ".."] || port <= 0 || port > 65535 = Left InvalidRelayTunnelTarget
  | otherwise = Right origin {WebSocket.webSocketPath = "/v0/computer/" <> BS.unpack (urlEncode True (Text.encodeUtf8 computerId)) <> "/tunnel?port=" <> show port}

-- | One connect/authentication attempt. Binary data and full peer-close
-- metadata are independent of daemon JSON-RPC; text messages are ignored.
-- A grant cannot be applied to this endpoint and is rejected before acquisition.
withRelayTunnel :: WebSocket.WebSocketOptions -> WebSocket.WebSocketTarget -> RelayOptions -> RelayCredential -> (RelayTunnel -> IO a) -> IO a
withRelayTunnel socketOptions target options credential action = do
  validateRelayOptions options
  case credential of
    RelayTokenProvider _ _ (Just _) -> throwIO RelayTunnelGrantUnsupported
    _ -> pure ()
  WebSocket.withMessageWebSocket socketOptions target $ \socket ->
    finallyPreserving
      ( do
          (token, organization, _) <- resolveCredential credential
          let wire = RelayWire (WebSocket.sendMessage socket . WebSocket.WebSocketText . encode) (WebSocket.sendMessage socket) (WebSocket.receiveMessage socket) authObject messageBytes
          prefix <- authenticateRelay options token organization wire
          withRelayStream wire prefix $ \stream ->
            action (RelayTunnel stream (WebSocket.closeMessageWebSocket socket "tunnel closed"))
      )
      (WebSocket.closeMessageWebSocket socket "tunnel closed")
  where
    authObject = \case
      WebSocket.WebSocketText bytes -> either (const Nothing) Just (eitherDecode bytes)
      WebSocket.WebSocketBinary _ -> Nothing
    messageBytes = \case
      WebSocket.WebSocketText bytes -> fromIntegral (BL.length bytes)
      WebSocket.WebSocketBinary bytes -> fromIntegral (BL.length bytes)

sendTunnelData :: RelayTunnel -> BL.ByteString -> IO ()
sendTunnelData (RelayTunnel stream _) = streamSend stream . WebSocket.WebSocketBinary

-- | Empty binary payload is data, not EOF. Close throws WebSocketClose with
-- exact code/reason; other I/O errors retain the underlying transport failure.
receiveTunnelData :: RelayTunnel -> IO BL.ByteString
receiveTunnelData (RelayTunnel stream _) = receive `onException` streamRetire stream
  where
    receive =
      streamReceive stream >>= \case
        WebSocket.WebSocketBinary bytes -> pure bytes
        WebSocket.WebSocketText _ -> receive

-- | Last-known local openness, not a ping or liveness probe. Receive to observe
-- remote close/error. No receive worker is created by this scoped adapter.
isRelayTunnelOpen :: RelayTunnel -> IO Bool
isRelayTunnelOpen (RelayTunnel stream _) = streamOpen stream

closeRelayTunnel :: RelayTunnel -> IO ()
closeRelayTunnel (RelayTunnel stream close) = mask_ (streamRetire stream >> close)

data RelayWire frame = RelayWire
  { wireSendAuth :: !(Object -> IO ()),
    wireSendFrame :: !(frame -> IO ()),
    wireReceive :: !(IO frame),
    wireAuthObject :: !(frame -> Maybe Object),
    wireFrameBytes :: !(frame -> Integer)
  }

data RelayStream frame = RelayStream
  { streamSend :: !(frame -> IO ()),
    streamReceive :: !(IO frame),
    streamOpen :: !(IO Bool),
    streamRetire :: !(IO ())
  }

withRelayStream :: RelayWire frame -> [frame] -> (RelayStream frame -> IO a) -> IO a
withRelayStream wire prefix action = do
  pending <- newIORef prefix
  alive <- newIORef True
  let retire = writeIORef alive False >> writeIORef pending []
      perform :: IO b -> IO b
      perform operation = do
        open <- readIORef alive
        unless open (throwIO RelayClosed)
        operation `onException` retire
      receive = perform $ do
        next <- atomicModifyIORef' pending (\case [] -> ([], Nothing); value : rest -> (rest, Just value))
        maybe (wireReceive wire) pure next
  action (RelayStream (perform . wireSendFrame wire) receive (readIORef alive) retire) `finally` retire

validateRelayOptions :: RelayOptions -> IO ()
validateRelayOptions options = when (relayAuthenticationTimeoutMicros options < 0 || relayPreludeByteLimit options < 0 || relayPreludeMessageLimit options < 0) (throwIO InvalidRelayOptions)

resolveCredential :: RelayCredential -> IO (Text, Maybe Text, DaemonAuthentication)
resolveCredential = \case
  RelayApiKey key -> do
    when (Text.null key) (throwIO RelayMissingToken)
    pure (key, Nothing, DaemonAuthenticate (DaemonApiKey key))
  RelayTokenProvider provider organizationProvider grant -> do
    token <- provider >>= maybe (throwIO RelayMissingToken) pure
    when (Text.null token) (throwIO RelayMissingToken)
    organization <- organizationProvider
    let selected = organization >>= \value -> if Text.null value then Nothing else Just value
    pure (token, selected, DaemonTokenProvider provider grant)

authenticateRelay :: RelayOptions -> Text -> Maybe Text -> RelayWire frame -> IO [frame]
authenticateRelay options token organization wire = do
  let authenticate = do
        wireSendAuth wire (KeyMap.fromList (["method" .= String "relay.authenticate", "token" .= token] <> maybe [] (\value -> ["activeOrganizationId" .= value]) organization))
        awaitAuthentication (toInteger (relayPreludeByteLimit options)) (relayPreludeMessageLimit options) []
      awaitAuthentication bytesLeft messagesLeft prefix = do
        frame <- wireReceive wire
        case wireAuthObject wire frame of
          Just fields | KeyMap.lookup "type" fields == Just (String "relay.auth_ok") -> pure (reverse prefix)
          Just fields | KeyMap.lookup "type" fields == Just (String "relay.auth_error") -> case parseEither parseJSON (Object fields) of
            Left _ -> throwIO RelayMalformedAuthentication
            Right failure -> throwIO (RelayAuthenticationRejected failure)
          _ -> do
            let size = wireFrameBytes wire frame
            when (messagesLeft == 0 || size > bytesLeft) (throwIO RelayPreludeTooLarge)
            awaitAuthentication (bytesLeft - size) (messagesLeft - 1) (frame : prefix)
  outcome <- timeout (relayAuthenticationTimeoutMicros options) authenticate
  maybe (throwIO RelayAuthenticationTimedOut) pure outcome
