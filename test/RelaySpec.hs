{-# LANGUAGE OverloadedStrings #-}

module RelaySpec (relayTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Exception (Exception, catch, fromException, throwIO, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Scientific (scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Transport (ObjectTransport (..), TransportKind (RelayTransport), TransportLocality (UnspecifiedHost), objectTransport)
import Factory.Droid.Transport.Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

relayTests :: TestTree
relayTests =
  testGroup
    "Relay authentication"
    [ testCase "bearer organization is forwarded without daemon grants or RPC envelope fields" $ bounded $ do
        requested <- newEmptyMVar
        withRelayPeer (\socket -> receiveFrame socket >>= putMVar requested >> authOk socket >> keepOpen socket) $ \target ->
          withRelayConnection plain target defaultRelayOptions (RelayTokenProvider (pure (Just "bearer")) (pure (Just "sub-org")) (Just "grant")) $ \connection -> do
            takeMVar requested >>= (@?= fields ["method" .= String "relay.authenticate", "token" .= String "bearer", "activeOrganizationId" .= String "sub-org"])
            relayOrganization connection @?= Just "sub-org"
            transportLocality (relayTransport connection) @?= UnspecifiedHost
            transportKind (relayTransport connection) @?= RelayTransport
            case relayDaemonAuthentication connection of
              Daemon.DaemonTokenProvider _ grant -> grant @?= Just "grant"
              _ -> assertFailure "Bearer auth shape changed",
      testCase "API-key authentication omits organization and retains the daemon key variant" $ bounded $ do
        requested <- newEmptyMVar
        withRelayPeer (\socket -> receiveFrame socket >>= putMVar requested >> authOk socket >> keepOpen socket) $ \target ->
          withRelayConnection plain target defaultRelayOptions (RelayApiKey "api-key") $ \connection -> do
            takeMVar requested >>= (@?= fields ["method" .= String "relay.authenticate", "token" .= String "api-key"])
            relayOrganization connection @?= Nothing
            case relayDaemonAuthentication connection of
              Daemon.DaemonAuthenticate (Daemon.DaemonApiKey key) -> key @?= "api-key"
              _ -> assertFailure "API key became bearer auth",
      testCase "empty organizations omit the field but whitespace remains an exact organization" $
        bounded $
          forM_ [(Nothing, Nothing), (Just "", Nothing), (Just " ", Just " ")] $ \(organization, expected) -> do
            requested <- newEmptyMVar
            withRelayPeer (\socket -> receiveFrame socket >>= putMVar requested >> authOk socket >> keepOpen socket) $ \target ->
              withRelayConnection plain target defaultRelayOptions (RelayTokenProvider (pure (Just "token")) (pure organization) Nothing) $ \connection -> do
                relayOrganization connection @?= expected
                takeMVar requested >>= (@?= fmap String expected) . KeyMap.lookup "activeOrganizationId",
      testCase "new connection scopes fetch fresh tokens and organization snapshots" $ bounded $ do
        tokens <- newIORef [Just "first-token", Just "second-token"]
        organizations <- newIORef [Just "first-org", Just "second-org"]
        requested <- newIORef []
        let credentials = RelayTokenProvider (nextValue tokens) (nextValue organizations) Nothing
        forM_ [1 :: Int, 2] $ \_ ->
          withRelayPeer (\socket -> receiveFrame socket >>= \frame -> modifyIORef' requested (<> [frame]) >> authOk socket >> keepOpen socket) $ \target ->
            withRelayConnection plain target defaultRelayOptions credentials (const (pure ()))
        frames <- readIORef requested
        map (KeyMap.lookup "token") frames @?= map (Just . String) ["first-token", "second-token"]
        map (KeyMap.lookup "activeOrganizationId") frames @?= map (Just . String) ["first-org", "second-org"],
      testCase "missing credentials and provider exceptions never publish an authenticated transport" $ bounded $ do
        forM_ [Nothing, Just ""] $ \token ->
          withRelayPeer keepOpen $ \target ->
            try @RelayError (withRelayConnection plain target defaultRelayOptions (RelayTokenProvider (pure token) (assertFailure "Organization fetched without token") Nothing) (const (pure ()))) >>= (@?= Left RelayMissingToken)
        withRelayPeer keepOpen $ \target ->
          try @ProviderFailure (withRelayConnection plain target defaultRelayOptions (RelayTokenProvider (throwIO ProviderFailure) (pure Nothing) Nothing) (const (pure ()))) >>= (@?= Left ProviderFailure),
      testCase "rejection retains optional fields and redacts its display" $ bounded $ do
        let rejection = object ["type" .= String "relay.auth_error", "message" .= String "private reason", "code" .= String "rate_limited", "retryable" .= False, "retryAfterMs" .= Number 9007199254740993, "future" .= False]
        withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode rejection) >> keepOpen socket) $ \target -> do
          result <- try @RelayError (withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") (const (pure ())))
          case result of
            Left (RelayAuthenticationRejected failure) -> do
              relayFailureMessage failure @?= "private reason"
              relayFailureCode failure @?= Just RelayRateLimited
              relayFailureRetryable failure @?= Just False
              relayFailureRetryAfterMillis failure @?= Just 9007199254740993
              relayFailureAdditionalFields failure @?= fields ["future" .= False]
              show failure @?= "RelayAuthFailure <redacted>"
            _ -> assertFailure "Relay rejection lost",
      testCase "malformed recognized rejection is not accepted as success" $
        bounded $
          forM_ [object ["type" .= String "relay.auth_error", "message" .= Null], object ["type" .= String "relay.auth_error", "message" .= String "bad", "code" .= String "future"], object ["type" .= String "relay.auth_error", "message" .= String "bad", "retryAfterMs" .= Number 1.5], object ["type" .= String "relay.auth_error", "message" .= String "bad", "retryable" .= Null]] $ \rejection ->
            withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode rejection) >> keepOpen socket) $ \target ->
              try @RelayError (withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") (const (pure ()))) >>= (@?= Left RelayMalformedAuthentication),
      testCase "handshake timeout and cancellation retain scoped cleanup" $ bounded $ do
        withRelayPeer keepOpen $ \target ->
          try @RelayError (withRelayConnection plain target (defaultRelayOptions {relayAuthenticationTimeoutMicros = 10000}) (RelayApiKey "key") (const (pure ()))) >>= (@?= Left RelayAuthenticationTimedOut)
        waiting <- newEmptyMVar
        withRelayPeer (\socket -> void (receiveFrame socket) >> putMVar waiting () >> keepOpen socket) $ \target ->
          withAsync (withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") (const (pure ()))) $ \connecting -> do
            takeMVar waiting
            cancel connecting
            waitCatch connecting >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Relay cancellation lost",
      testCase "valid pre-authentication objects are preserved and escaped prefixes are revoked" $ bounded $ do
        let prelude = object ["early" .= True]
        withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode prelude) >> authOk socket >> keepOpen socket) $ \target ->
          withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") $ \connection ->
            transportReceiveObject (relayTransport connection) >>= (@?= fields ["early" .= True])
        escaped <- withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode prelude) >> authOk socket >> keepOpen socket) $ \target ->
          withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") pure
        try @RelayError (transportReceiveObject (relayTransport escaped)) >>= (@?= Left RelayClosed),
      testCase "failed sends retire buffered pre-authentication objects as well as socket I/O" $
        bounded $
          withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode (object ["early" .= True])) >> authOk socket >> keepOpen socket) $ \target ->
            withRelayConnection (plain {WebSocket.webSocketMessageLimitBytes = 80}) target defaultRelayOptions (RelayApiKey "key") $ \connection -> do
              let transport = relayTransport connection
              try @WebSocket.WebSocketError (transportSendObject transport (fields ["oversized" .= Text.replicate 128 "x"])) >>= (@?= Left WebSocket.WebSocketMessageTooLarge)
              try @RelayError (transportReceiveObject transport) >>= (@?= Left RelayClosed),
      testCase "prelude limits fail closed instead of buffering indefinitely" $
        bounded $
          forM_ [defaultRelayOptions {relayPreludeMessageLimit = 0}, defaultRelayOptions {relayPreludeByteLimit = 0}] $ \options ->
            withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode (object ["early" .= True])) >> keepOpen socket) $ \target ->
              try @RelayError (withRelayConnection plain target options (RelayApiKey "key") (const (pure ()))) >>= (@?= Left RelayPreludeTooLarge),
      testCase "relay and daemon authentication remain separate and session loads refresh tokens" $ bounded $ do
        tokens <- newIORef [Just "relay-token", Just "auth-token", Just "load-one", Just "load-two"]
        observed <- newIORef []
        let credentials = RelayTokenProvider (nextValue tokens) (pure (Just "organization")) (Just "grant")
        withRelayPeer (tokenPeer observed) $ \target ->
          withRelayConnection plain target defaultRelayOptions credentials $ \relay -> do
            let options = (Daemon.defaultDaemonClientOptions (relayDaemonAuthentication relay) "") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
            Daemon.withConnectionOn options (relayTransport relay) $ \connection -> do
              void (Daemon.loadSessionInfo connection "saved")
              void (Daemon.loadSessionInfo connection "saved")
        frames <- readIORef observed
        map (KeyMap.lookup "method") frames @?= map (Just . String) ["relay.authenticate", "daemon.authenticate", "daemon.load_session", "daemon.load_session"]
        case frames of
          [relay, auth, loadOne, loadTwo] -> do
            KeyMap.lookup "activeOrganizationId" relay @?= Just (String "organization")
            KeyMap.lookup "actAsGrant" (params auth) @?= Just (String "grant")
            map (KeyMap.lookup "token" . params) [auth, loadOne, loadTwo] @?= map (Just . String) ["auth-token", "load-one", "load-two"]
          _ -> assertFailure "Expected relay auth, daemon auth and two loads",
      testCase "unavailable refreshed token never reuses the cached authentication token" $ bounded $ do
        tokens <- newIORef [Just "auth-token", Nothing, Just ""]
        observed <- newIORef []
        withRelayPeer (daemonTokenPeer observed) $ \target ->
          WebSocket.withWebSocket plain target $ \socket -> do
            let transport = objectTransport (WebSocket.sendObject socket) (WebSocket.receiveObject socket)
                options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonTokenProvider (nextValue tokens) Nothing) "") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
            Daemon.withConnectionOn options transport $ \connection -> do
              try @Daemon.DaemonError (Daemon.loadSessionInfo connection "saved") >>= \case Left cause -> cause @?= Daemon.DaemonCredentialUnavailable; Right _ -> assertFailure "Missing token did not reject load"
              void (Daemon.loadSessionInfo connection "saved")
        frames <- readIORef observed
        case frames of
          [_, loaded] -> KeyMap.lookup "token" (params loaded) @?= Just (String "")
          _ -> assertFailure "Unavailable token sent a load or replayed authentication",
      testCase "all rejection codes and absent optional fields remain distinct" $
        bounded $
          forM_ [(Nothing, Nothing), (Just "computer_already_connected", Just RelayComputerAlreadyConnected), (Just "unauthorized", Just RelayUnauthorized), (Just "service_unavailable", Just RelayServiceUnavailable), (Just "rate_limited", Just RelayRateLimited)] $ \(code, expected) -> do
            let rejection = object (["type" .= String "relay.auth_error", "message" .= String ""] <> maybe [] (\value -> ["code" .= String value]) code)
            withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode rejection) >> keepOpen socket) $ \target -> do
              result <- try @RelayError (withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") (const (pure ())))
              case result of
                Left (RelayAuthenticationRejected failure) -> do
                  relayFailureCode failure @?= expected
                  relayFailureMessage failure @?= ""
                  relayFailureRetryable failure @?= Nothing
                  relayFailureRetryAfterMillis failure @?= Nothing
                _ -> assertFailure "Expected typed relay rejection",
      testCase "nonpositive and extreme retry delays reject without expanding an unbounded integer" $
        bounded $
          forM_ [0, -1, scientific 1 10000, scientific 1 (-10000)] $ \delay ->
            withRelayPeer (\socket -> void (receiveFrame socket) >> WS.sendTextData socket (encode (object ["type" .= String "relay.auth_error", "message" .= String "", "retryAfterMs" .= Number delay])) >> keepOpen socket) $ \target ->
              try @RelayError (withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") (const (pure ()))) >>= (@?= Left RelayMalformedAuthentication),
      testCase "organization and callback failures preserve identity and close the owned socket" $
        bounded $
          forM_ [False, True] $ \failInCallback -> do
            closed <- newEmptyMVar
            let peer socket = do
                  when failInCallback (void (receiveFrame socket) >> authOk socket)
                  keepOpen socket `catch` \(_ :: WS.ConnectionException) -> putMVar closed ()
                credentials = if failInCallback then RelayApiKey "key" else RelayTokenProvider (pure (Just "token")) (throwIO ProviderFailure) Nothing
            withRelayPeer peer $ \target -> do
              try @ProviderFailure (withRelayConnection plain target defaultRelayOptions credentials (const (throwIO ProviderFailure :: IO ()))) >>= (@?= Left ProviderFailure)
              timeout 1000000 (takeMVar closed) >>= (@?= Just ()),
      testCase "cancellation during token acquisition closes the socket with the original exception" $ bounded $ do
        fetching <- newEmptyMVar
        blocked <- newEmptyMVar
        closed <- newEmptyMVar
        let credentials = RelayTokenProvider (putMVar fetching () >> takeMVar blocked) (pure Nothing) Nothing
            peer socket = keepOpen socket `catch` \(_ :: WS.ConnectionException) -> putMVar closed ()
        withRelayPeer peer $ \target ->
          withAsync (withRelayConnection plain target defaultRelayOptions credentials (const (pure ()))) $ \worker -> do
            takeMVar fetching
            cancel worker
            waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Provider cancellation lost"
            timeout 1000000 (takeMVar closed) >>= (@?= Just ()),
      testCase "close, binary and invalid JSON during auth remain transport failures" $
        bounded $
          forM_ [(\socket -> WS.sendCloseCode socket 1013 ("private reason" :: Text), WebSocket.WebSocketPeerClosed 1013), (\socket -> WS.sendBinaryData socket (encode (object [])), WebSocket.WebSocketBinaryMessage), (\socket -> WS.sendTextData socket ("not-json" :: Text), WebSocket.WebSocketInvalidObject)] $ \(respond, expected) ->
            withRelayPeer (\socket -> void (receiveFrame socket) >> respond socket >> keepOpen socket) $ \target ->
              try @WebSocket.WebSocketError (withRelayConnection plain target defaultRelayOptions (RelayApiKey "key") (const (pure ()))) >>= (@?= Left expected),
      testCase "normal create and resume constructors fetch a fresh session token" $
        bounded $
          forM_ [Nothing, Just "saved"] $ \saved -> do
            tokens <- newIORef [Just "relay-token", Just "auth-token", Just "session-token"]
            observed <- newIORef []
            let credentials = RelayTokenProvider (nextValue tokens) (pure (Just "organization")) Nothing
            withRelayPeer (tokenPeer observed) $ \target ->
              withRelayConnection plain target defaultRelayOptions credentials $ \relay -> do
                let options = (Daemon.defaultDaemonClientOptions (relayDaemonAuthentication relay) "/fixture") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
                case saved of
                  Nothing -> Daemon.withSessionUsing options (relayTransport relay) (const (pure ()))
                  Just identifier -> Daemon.withResumedSessionUsing options (relayTransport relay) identifier (const (pure ()))
            readIORef observed >>= \case
              [_, _, request] -> do
                KeyMap.lookup "method" request @?= Just (String (maybe "daemon.initialize_session" (const "daemon.load_session") saved))
                KeyMap.lookup "token" (params request) @?= Just (String "session-token")
              _ -> assertFailure "Expected distinct relay/auth/session operations",
      testCase "inherited token providers remain lazy until a session operation" $ bounded $ do
        identity <- case fromJSON (object ["userId" .= String "user", "orgId" .= String "organization"]) of Error _ -> assertFailure "Invalid inherited fixture"; Success value -> pure value
        calls <- newIORef (0 :: Int)
        observed <- newIORef []
        let provider = modifyIORef' calls (+ 1) >> pure (Just "inherited-session-token")
        withRelayPeer (daemonTokenPeer observed) $ \target ->
          WebSocket.withWebSocket plain target $ \socket -> do
            let options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthenticationProvider identity provider) "") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
            Daemon.withConnectionOn options (objectTransport (WebSocket.sendObject socket) (WebSocket.receiveObject socket)) $ \connection -> do
              readIORef calls >>= (@?= 0)
              void (Daemon.loadSessionInfo connection "saved")
        readIORef calls >>= (@?= 1)
        readIORef observed >>= \case [request] -> KeyMap.lookup "token" (params request) @?= Just (String "inherited-session-token"); _ -> assertFailure "Inherited mode performed authentication",
      testCase "invalid options and missing initial dynamic credentials fail before authentication" $ bounded $ do
        forM_ [defaultRelayOptions {relayAuthenticationTimeoutMicros = -1}, defaultRelayOptions {relayPreludeByteLimit = -1}, defaultRelayOptions {relayPreludeMessageLimit = -1}] $ \options ->
          try @RelayError (withRelayConnection plain (WebSocket.WebSocketTarget "127.0.0.1" 1 "/") options (RelayTokenProvider (assertFailure "Provider invoked before preflight") (pure Nothing) Nothing) (const (pure ()))) >>= (@?= Left InvalidRelayOptions)
        forM_ [(Nothing, Daemon.DaemonCredentialUnavailable), (Just "", Daemon.InvalidDaemonCredential)] $ \(token, expected) ->
          withRelayPeer (\socket -> void (receiveFrame socket) >> assertFailure "Missing credentials sent authentication") $ \target ->
            WebSocket.withWebSocket plain target $ \socket -> do
              let options = Daemon.defaultDaemonClientOptions (Daemon.DaemonTokenProvider (pure token) Nothing) ""
              try @Daemon.DaemonError (Daemon.withConnectionOn options (objectTransport (WebSocket.sendObject socket) (WebSocket.receiveObject socket)) (const (pure ()))) >>= (@?= Left expected)
    ]

data ProviderFailure = ProviderFailure deriving stock (Eq, Show)

instance Exception ProviderFailure

plain :: WebSocket.WebSocketOptions
plain = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing, WebSocket.webSocketConnectTimeoutMicros = 1000000, WebSocket.webSocketCloseTimeoutMicros = 10000}

withRelayPeer :: (WS.Connection -> IO ()) -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withRelayPeer peer = withPeer (\_ socket -> peer socket `catch` \(_ :: WS.ConnectionException) -> pure ())

keepOpen :: WS.Connection -> IO ()
keepOpen socket = forever (void (WS.receiveDataMessage socket))

receiveFrame :: WS.Connection -> IO Object
receiveFrame socket = WS.receiveData socket >>= either (const (assertFailure "Invalid relay client JSON")) pure . eitherDecode

authOk :: WS.Connection -> IO ()
authOk socket = WS.sendTextData socket (encode (object ["type" .= String "relay.auth_ok"]))

fields :: [Pair] -> Object
fields values = case object values of Object result -> result; _ -> error "Object fixture"

nextValue :: IORef [a] -> IO a
nextValue values = atomicModifyIORef' values (\case [] -> ([], Nothing); value : rest -> (rest, Just value)) >>= maybe (assertFailure "Credential provider exhausted") pure

params :: Object -> Object
params frame = case KeyMap.lookup "params" frame of Just (Object value) -> value; _ -> fields []

reply :: WS.Connection -> Object -> Value -> IO ()
reply socket request result = WS.sendTextData socket (encode (object ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "response", "id" .= KeyMap.lookup "id" request, "result" .= result]))

tokenPeer :: IORef [Object] -> WS.Connection -> IO ()
tokenPeer observed socket = do
  relay <- receiveFrame socket
  modifyIORef' observed (<> [relay])
  relay @?= fields ["method" .= String "relay.authenticate", "token" .= String "relay-token", "activeOrganizationId" .= String "organization"]
  authOk socket
  daemonTokenPeer observed socket

daemonTokenPeer :: IORef [Object] -> WS.Connection -> IO ()
daemonTokenPeer observed socket = forever $ do
  request <- receiveFrame socket
  modifyIORef' observed (<> [request])
  case KeyMap.lookup "method" request of
    Just (String "daemon.authenticate") -> reply socket request (object ["userId" .= String "user", "orgId" .= String "organization"])
    Just (String "daemon.load_session") -> reply socket request (object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]])
    Just (String "daemon.initialize_session") -> reply socket request (object ["sessionId" .= KeyMap.lookup "sessionId" (params request), "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]])
    _ -> assertFailure "Unexpected daemon operation or implicit replay"
