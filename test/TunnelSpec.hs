{-# LANGUAGE OverloadedStrings #-}

module TunnelSpec (tunnelTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, poll, waitCatch, withAsync)
import Control.Exception (Exception, catch, fromException, mask_, throwIO, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Transport.Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer, withSocketPeer)

tunnelTests :: TestTree
tunnelTests =
  testGroup
    "Binary relay tunnels"
    [ testCase "target construction retains origin and encodes an opaque computer identity" $ do
        let origin = WebSocket.WebSocketTarget "relay.invalid" 8443 "/ignored?old=query"
        target <- either throwIO pure (relayTunnelTarget origin "node /?#é" 8080)
        WebSocket.webSocketHost target @?= "relay.invalid"
        WebSocket.webSocketPort target @?= 8443
        WebSocket.webSocketPath target @?= "/v0/computer/node%20%2F%3F%23%C3%A9/tunnel?port=8080"
        WebSocket.webSocketConnectTimeoutMicros defaultTunnelWebSocketOptions @?= 30000000
        forM_ [0, -1, 65536] $ \port -> relayTunnelTarget origin "node" port @?= Left InvalidRelayTunnelTarget
        forM_ ["", ".", ".."] $ \identifier -> relayTunnelTarget origin identifier 1 @?= Left InvalidRelayTunnelTarget,
      testCase "canonical target and relay authentication precede binary bytes on the wire" $
        bounded $
          withSocketPeer
            ( \socket -> do
                pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
                WS.requestPath (WS.pendingRequest pending) @?= "/v0/computer/node/tunnel?port=8080"
                connection <- WS.acceptRequest pending
                receiveAuth connection >>= (@?= fields ["method" .= String "relay.authenticate", "token" .= String "token", "activeOrganizationId" .= String "sub-org"])
                authOk connection
                WS.receiveDataMessage connection >>= \case WS.Binary bytes -> bytes @?= BL.pack [0, 255, 128, 10]; _ -> assertFailure "Tunnel data was not binary"
                WS.sendBinaryData connection (BL.pack [255, 0, 128])
                acknowledgeClose connection "tunnel closed"
            )
            ( \origin -> do
                target <- either throwIO pure (relayTunnelTarget origin "node" 8080)
                withRelayTunnel plain target defaultRelayOptions (RelayTokenProvider (pure (Just "token")) (pure (Just "sub-org")) Nothing) $ \tunnel -> do
                  isRelayTunnelOpen tunnel >>= (@?= True)
                  sendTunnelData tunnel (BL.pack [0, 255, 128, 10])
                  receiveTunnelData tunnel >>= (@?= BL.pack [255, 0, 128])
            ),
      testCase "binary prefixes, empty payloads and nonzero-offset slices retain exact bytes" $ bounded $ do
        let backing = BS.pack [99, 1, 2, 3, 88]
            payload = BL.fromChunks [BS.take 3 (BS.drop 1 backing), BS.pack [4, 5]]
        withTunnelPeer
          ( \socket -> do
              void (receiveAuth socket)
              WS.sendBinaryData socket (BL.pack [254, 0])
              authOk socket
              WS.sendTextData socket ("not JSON, not tunnel data" :: Text)
              WS.sendTextData socket (encode (object ["method" .= String "daemon.request_permission"]))
              WS.sendBinaryData socket BL.empty
              WS.receiveDataMessage socket >>= \case WS.Binary bytes -> bytes @?= BL.pack [1, 2, 3, 4, 5]; _ -> assertFailure "Slice lost binary framing"
              WS.sendBinaryData socket payload
              acknowledgeClose socket "tunnel closed"
          )
          $ \target ->
            withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") $ \tunnel -> do
              receiveTunnelData tunnel >>= (@?= BL.pack [254, 0])
              receiveTunnelData tunnel >>= (@?= BL.empty)
              sendTunnelData tunnel payload
              receiveTunnelData tunnel >>= (@?= BL.pack [1, 2, 3, 4, 5]),
      testCase "JSON-shaped binary cannot satisfy relay authentication"
        $ bounded
        $ withTunnelPeer
          ( \socket -> do
              void (receiveAuth socket)
              WS.sendBinaryData socket (encode (object ["type" .= String "relay.auth_ok"]))
              WS.sendTextData socket (encode (object ["type" .= String "relay.auth_error", "message" .= String "denied", "code" .= String "unauthorized"]))
              keepOpen socket
          )
        $ \target ->
          try @RelayError (withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") (const (pure ()))) >>= (@?= Left (RelayAuthenticationRejected (RelayAuthFailure "denied" (Just RelayUnauthorized) Nothing Nothing mempty))),
      testCase "full close metadata retires the tunnel and redacts its display" $
        bounded $
          withTunnelPeer (\socket -> void (receiveAuth socket) >> authOk socket >> WS.sendCloseCode socket 4014 ("reason 🌐" :: Text) >> keepOpen socket) $ \target ->
            withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") $ \tunnel -> do
              try @WebSocket.WebSocketClose (receiveTunnelData tunnel) >>= (@?= Left (WebSocket.WebSocketClose 4014 "reason 🌐"))
              isRelayTunnelOpen tunnel >>= (@?= False)
              try @RelayError (receiveTunnelData tunnel) >>= (@?= Left RelayClosed)
              show (WebSocket.WebSocketClose 4014 "private") @?= "WebSocketClose <redacted>",
      testCase "explicit close is idempotent and scope exit does not send a second close" $ bounded $ do
        observed <- newEmptyMVar
        withTunnelPeer (\socket -> void (receiveAuth socket) >> authOk socket >> acknowledgeClose socket "tunnel closed" >> putMVar observed ()) $ \target ->
          withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") $ \tunnel -> do
            closeRelayTunnel tunnel
            closeRelayTunnel tunnel
            timeout 1000000 (takeMVar observed) >>= (@?= Just ())
            isRelayTunnelOpen tunnel >>= (@?= False)
            try @RelayError (sendTunnelData tunnel BL.empty) >>= (@?= Left RelayClosed)
            try @RelayError (receiveTunnelData tunnel) >>= (@?= Left RelayClosed),
      testCase "send failure revokes retained binary data" $
        bounded $
          withTunnelPeer (\socket -> void (receiveAuth socket) >> WS.sendBinaryData socket (BL.pack [1]) >> authOk socket >> keepOpen socket) $ \target ->
            withRelayTunnel (plain {WebSocket.webSocketMessageLimitBytes = 80}) target defaultRelayOptions (RelayApiKey "key") $ \tunnel -> do
              try @WebSocket.WebSocketError (sendTunnelData tunnel (BL.replicate 81 0)) >>= (@?= Left WebSocket.WebSocketMessageTooLarge)
              try @RelayError (receiveTunnelData tunnel) >>= (@?= Left RelayClosed)
              isRelayTunnelOpen tunnel >>= (@?= False),
      testCase "binary authentication buffering is bounded by bytes and frame count" $
        bounded $
          forM_ [defaultRelayOptions {relayPreludeByteLimit = 0}, defaultRelayOptions {relayPreludeMessageLimit = 0}] $ \options ->
            withTunnelPeer (\socket -> void (receiveAuth socket) >> WS.sendBinaryData socket (BL.pack [1]) >> keepOpen socket) $ \target ->
              try @RelayError (withRelayTunnel plain target options (RelayApiKey "key") (const (pure ()))) >>= (@?= Left RelayPreludeTooLarge),
      testCase "a daemon-only grant cannot be silently ignored by a binary endpoint" $
        bounded $
          try @RelayError (withRelayTunnel plain (WebSocket.WebSocketTarget "127.0.0.1" 1 "/") defaultRelayOptions (RelayTokenProvider (assertFailure "Token fetched for unsupported grant") (pure Nothing) (Just "")) (const (pure ()))) >>= (@?= Left RelayTunnelGrantUnsupported),
      testCase "authentication close is immediate and retains full metadata" $
        bounded $
          withTunnelPeer (\socket -> void (receiveAuth socket) >> WS.sendCloseCode socket 4005 ("auth closed" :: Text) >> keepOpen socket) $ \target ->
            try @WebSocket.WebSocketClose (withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") (const (pure ()))) >>= (@?= Left (WebSocket.WebSocketClose 4005 "auth closed")),
      testCase "callback exceptions close the socket and preserve the original exception" $ bounded $ do
        closed <- newEmptyMVar
        withTunnelPeer (\socket -> void (receiveAuth socket) >> authOk socket >> acknowledgeClose socket "tunnel closed" >> putMVar closed ()) $ \target -> do
          try @TunnelFailure (withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") (const (throwIO TunnelFailure :: IO ()))) >>= (@?= Left TunnelFailure)
          timeout 1000000 (takeMVar closed) >>= (@?= Just ()),
      testCase "cancelled reads preserve cancellation and cannot keep an escaped scope alive" $ bounded $ do
        ready <- newEmptyMVar
        escaped <- withTunnelPeer (\socket -> void (receiveAuth socket) >> authOk socket >> keepOpen socket) $ \target ->
          withRelayTunnel plain target defaultRelayOptions (RelayApiKey "key") $ \tunnel -> do
            withAsync (putMVar ready () >> receiveTunnelData tunnel) $ \reader -> do
              takeMVar ready
              cancel reader
              waitCatch reader >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled tunnel read returned"
            pure tunnel
        isRelayTunnelOpen escaped >>= (@?= False)
        try @RelayError (sendTunnelData escaped BL.empty) >>= (@?= Left RelayClosed),
      testCase "raw message close validates UTF-8 byte length before retiring the scope" $
        bounded $
          withTunnelPeer (\socket -> WS.sendBinaryData socket (BL.pack [7]) >> acknowledgeClose socket (Text.replicate 61 "é" <> "a")) $ \target ->
            WebSocket.withMessageWebSocket plain target $ \socket -> do
              try @WebSocket.WebSocketError (WebSocket.closeMessageWebSocket socket (Text.replicate 62 "é")) >>= (@?= Left WebSocket.InvalidWebSocketCloseReason)
              WebSocket.isMessageWebSocketOpen socket >>= (@?= True)
              WebSocket.receiveMessage socket >>= (@?= WebSocket.WebSocketBinary (BL.pack [7]))
              WebSocket.closeMessageWebSocket socket (Text.replicate 61 "é" <> "a"),
      testCase "cancelled concurrent close waits for the existing cleanup then preserves cancellation" $ bounded $ do
        closeSeen <- newEmptyMVar
        allowAck <- newEmptyMVar
        let peer socket = do
              void (receiveAuth socket)
              authOk socket
              WS.receive socket >>= \case
                WS.ControlMessage (WS.Close code reason) -> do
                  code @?= 1000
                  reason @?= "tunnel closed"
                  putMVar closeSeen ()
                  takeMVar allowAck
                  WS.sendCloseCode socket code reason
                _ -> assertFailure "Expected close request"
        withTunnelPeer peer $ \target ->
          withRelayTunnel (plain {WebSocket.webSocketCloseTimeoutMicros = 1000000}) target defaultRelayOptions (RelayApiKey "key") $ \tunnel ->
            withAsync (closeRelayTunnel tunnel) $ \first -> do
              takeMVar closeSeen
              entered <- newEmptyMVar
              withAsync (mask_ (putMVar entered () >> closeRelayTunnel tunnel)) $ \second -> do
                takeMVar entered
                withAsync (cancel second) $ \canceller -> do
                  timeout 10000 (void (waitCatch second)) >>= (@?= Nothing)
                  poll first >>= \case Nothing -> pure (); Just _ -> assertFailure "First close finished before acknowledgement"
                  putMVar allowAck ()
                  waitCatch first >>= either throwIO pure
                  waitCatch second >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Close cancellation lost"
                  waitCatch canceller >>= either throwIO pure
    ]

data TunnelFailure = TunnelFailure deriving stock (Eq, Show)

instance Exception TunnelFailure

plain :: WebSocket.WebSocketOptions
plain = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing, WebSocket.webSocketConnectTimeoutMicros = 1000000, WebSocket.webSocketCloseTimeoutMicros = 100000}

withTunnelPeer :: (WS.Connection -> IO ()) -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withTunnelPeer peer = withPeer (\_ socket -> peer socket `catch` \(_ :: WS.ConnectionException) -> pure ())

receiveAuth :: WS.Connection -> IO Object
receiveAuth socket =
  WS.receiveDataMessage socket >>= \case
    WS.Text bytes _ -> either (const (assertFailure "Invalid auth object")) pure (eitherDecode bytes)
    _ -> assertFailure "Authentication was not a text control frame"

authOk :: WS.Connection -> IO ()
authOk socket = WS.sendTextData socket (encode (object ["type" .= String "relay.auth_ok"]))

fields :: [(Key, Value)] -> Object
fields = KeyMap.fromList

keepOpen :: WS.Connection -> IO ()
keepOpen socket = forever (void (WS.receiveDataMessage socket))

acknowledgeClose :: WS.Connection -> Text -> IO ()
acknowledgeClose socket reason =
  WS.receive socket >>= \case
    WS.ControlMessage (WS.Close code bytes) -> do
      code @?= 1000
      bytes @?= BL.fromStrict (Text.encodeUtf8 reason)
      WS.sendCloseCode socket code bytes
    _ -> assertFailure "Missing normal tunnel close"
