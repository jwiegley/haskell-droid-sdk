{-# LANGUAGE OverloadedStrings #-}

module WebSocketSpec (webSocketTests, withPeer, withSocketPeer, withTLSCertificate, withTLSPeer) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, forConcurrently_, link, wait, waitCatch, withAsync)
import Control.Exception (Exception, Handler (..), IOException, bracket, catch, catches, fromException, throwIO, try)
import Control.Monad (forM_, forever, replicateM, unless, void, (>=>))
import Data.Aeson (Object, Value (..), eitherDecode, encode, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
import Data.List (sort)
import Data.Text qualified as Text
import Data.Word (Word8)
import Data.X509.CertificateStore qualified as Certificates
import Factory.Droid.Client (CallOptions (..), listModels)
import Factory.Droid.Protocol (withRpcChannel)
import Factory.Droid.Schema.Models (ListModelsOptions (..))
import Factory.Droid.Schema.RPC (WithEnvelope (..))
import Factory.Droid.Transport.WebSocket
import Network.Connection qualified as Connection
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Network.TLS qualified as TLS
import Network.TLS.Extra qualified as TLSExtra
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as Stream
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import System.Directory (removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (ioeGetErrorString, isResourceVanishedError)
import System.Posix.Temp (mkdtemp)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

webSocketTests :: TestTree
webSocketTests =
  testGroup
    "WebSocket object transport"
    [ testCase "TLS verifies trusted roots and hostnames without plaintext fallback" $
        bounded $
          withTLSCertificate $ \credential trusted -> do
            withTLSPeer credential (\connection -> WS.sendTextData connection (encode payload)) $ \target ->
              withWebSocket (defaultWebSocketOptions {webSocketTLS = Just trusted}) (target {webSocketHost = "localhost"}) (receiveObject >=> (@?= payload))
            withTLSPeer credential (\_ -> assertFailure "Untrusted TLS reached WebSocket") $ \target ->
              expectError WebSocketConnectFailure (withWebSocket defaultWebSocketOptions (target {webSocketHost = "localhost"}) (\_ -> assertFailure "Untrusted TLS succeeded"))
            withTLSPeer credential (\_ -> assertFailure "Wrong hostname reached WebSocket") $ \target ->
              expectError WebSocketConnectFailure (withWebSocket (defaultWebSocketOptions {webSocketTLS = Just trusted}) target (\_ -> assertFailure "Wrong hostname succeeded")),
      testCase "HTTP response header limit prevents unbounded setup input" $
        bounded $
          withSocketPeer (\socket -> SocketBS.sendAll socket ("HTTP/1.1 101 Switching Protocols\r\nX-Padding: " <> BS.replicate 65536 97) >> drainSocket socket) $ \target ->
            expectError WebSocketConnectFailure (withPlainWebSocket 128 1000000 10000 target (\_ -> assertFailure "Oversized header accepted")),
      testCase "callback IO exceptions are not relabeled as connection failures" $
        bounded $
          withPeer (\_ _ -> pure ()) $ \target -> do
            result <- try @IOException (withPlainWebSocket 128 1000000 10000 target (\_ -> ioError (userError "callback identity")) :: IO ())
            case result of
              Left err -> ioeGetErrorString err @?= "callback identity"
              Right _ -> assertFailure "Missing callback exception",
      testCase "escaped transport handles fail after their scope" $
        bounded $
          withPeer (\_ _ -> pure ()) $ \target -> do
            escaped <- withPlainWebSocket 128 1000000 10000 target pure
            expectError WebSocketWriteFailure (sendObject escaped mempty)
            expectError WebSocketReadFailure (receiveObject escaped),
      testCase "text messages preserve JSON and Unicode without JSONL delimiters"
        $ bounded
        $ withPeer
          ( \_ connection -> do
              message <- WS.receiveDataMessage connection
              case message of
                WS.Text bytes _ -> bytes @?= encode payload
                _ -> assertFailure "Client sent binary JSON"
              WS.sendTextData connection (encode payload)
          )
        $ \target ->
          withPlainWebSocket 4096 1000000 50000 target $ \channel -> do
            sendObject channel payload
            receiveObject channel >>= (@?= payload),
      testCase "fragmented UTF-8 with an interleaved ping is reassembled"
        $ bounded
        $ withPeer
          ( \socket connection -> do
              let (first, second) = BS.splitAt 10 (BL.toStrict (encode payload))
              SocketBS.sendAll socket (frame 1 first <> frame 137 "hi" <> frame 128 second)
              WS.receive connection >>= \case
                WS.ControlMessage (WS.Pong bytes) -> bytes @?= "hi"
                _ -> assertFailure "Client failed to answer interleaved ping"
          )
        $ \target ->
          withPlainWebSocket 4096 1000000 50000 target (receiveObject >=> (@?= payload)),
      testCase "binary messages are not accepted as JSON text" $
        bounded $
          withPeer (\_ connection -> WS.sendBinaryData connection ("{}" :: BS.ByteString)) $ \target ->
            expectError WebSocketBinaryMessage (withPlainWebSocket 64 1000000 50000 target receiveObject),
      testCase "malformed and non-object JSON stay payload-free" $
        bounded $
          forM_ ["{private:broken}", "[]", "null", "true", "{}{}"] $ \bytes ->
            withPeer (\_ connection -> WS.sendTextData connection (bytes :: BS.ByteString)) $ \target ->
              expectError WebSocketInvalidObject (withPlainWebSocket 128 1000000 50000 target receiveObject),
      testCase "invalid UTF-8 inside JSON strings is rejected" $
        bounded $
          withPeer (\_ connection -> WS.sendTextData connection ("{\"text\":\"" <> BS.pack [255] <> "\"}")) $ \target ->
            expectError WebSocketInvalidObject (withPlainWebSocket 64 1000000 50000 target receiveObject),
      testCase "exact message limit accepts an empty object" $
        bounded $
          withPeer (\_ connection -> WS.sendTextData connection ("{}" :: BS.ByteString)) $ \target ->
            withPlainWebSocket 2 1000000 50000 target (receiveObject >=> (@?= mempty)),
      testCase "oversized frame is rejected before its claimed payload arrives" $
        bounded $
          withPeer (\socket _ -> SocketBS.sendAll socket (BS.pack [129, 127, 0, 0, 0, 0, 0, 1, 0, 0])) $ \target ->
            expectError WebSocketReadFailure (withPlainWebSocket 64 1000000 50000 target receiveObject),
      testCase "assembled-message limit also covers small fragmented frames" $
        bounded $
          withPeer (\socket _ -> SocketBS.sendAll socket (frame 1 "{\"value\":\"" <> frame 128 "too long\"}")) $ \target ->
            expectError WebSocketReadFailure (withPlainWebSocket 12 1000000 50000 target receiveObject),
      testCase "outgoing size counts UTF-8 bytes" $ bounded $ do
        let value = KeyMap.singleton "text" (String (Text.replicate 30 "😀"))
            size = fromIntegral (BL.length (encode value))
        withPeer (\_ connection -> (WS.receiveData connection :: IO BL.ByteString) >>= (@?= encode value)) $ \target ->
          withPlainWebSocket size 1000000 50000 target (`sendObject` value)
        withPeer (\_ _ -> pure ()) $ \target ->
          expectError WebSocketMessageTooLarge (withPlainWebSocket (size - 1) 1000000 50000 target (`sendObject` value)),
      testCase "peer close retains only its status code" $
        bounded $
          withPeer (\_ connection -> WS.sendCloseCode connection 4001 ("private reason" :: BS.ByteString)) $ \target ->
            expectError (WebSocketPeerClosed 4001) (withPlainWebSocket 2 1000000 50000 target receiveObject),
      testCase "normal scope closure sends code 1000"
        $ bounded
        $ withPeer
          ( \_ connection -> do
              result <- try @WS.ConnectionException (WS.receiveDataMessage connection)
              case result of
                Left (WS.CloseRequest code reason) -> do
                  code @?= 1000
                  reason @?= "Client disconnect"
                _ -> assertFailure "Missing normal close handshake"
          )
        $ \target ->
          withPlainWebSocket 128 1000000 50000 target (\_ -> pure ()),
      testCase "close grace ends even when peer does not acknowledge"
        $ bounded
        $ withSocketPeer
          ( \socket -> do
              pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
              _ <- WS.acceptRequest pending
              drainSocket socket
          )
        $ \target ->
          withPlainWebSocket 128 1000000 10000 target (\_ -> pure ()),
      testCase "callback exceptions retain identity and release the socket" $
        bounded $
          withPeer (\_ _ -> pure ()) $ \target -> do
            result <- try @CallbackAbort (withPlainWebSocket 128 1000000 50000 target (\_ -> throwIO CallbackAbort) :: IO ())
            result @?= Left CallbackAbort,
      testCase "cancellation during receive releases the socket and preserves identity" $
        bounded $
          withPeer (\_ connection -> WS.sendTextData connection ("{}" :: BS.ByteString)) $ \target -> do
            ready <- newEmptyMVar
            withAsync (withPlainWebSocket 128 1000000 50000 target $ \channel -> receiveObject channel >> putMVar ready () >> receiveObject channel) $ \worker -> do
              takeMVar ready
              cancel worker
              waitCatch worker >>= \case
                Left err -> fromException err @?= Just AsyncCancelled
                Right _ -> assertFailure "Cancelled receive succeeded",
      testCase "cancellation of a blocked write releases the socket" $ bounded $ do
        release <- newEmptyMVar
        ready <- newEmptyMVar
        withSocketPeer
          ( \socket -> do
              Socket.setSocketOption socket Socket.RecvBuffer 4096
              pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
              connection <- WS.acceptRequest pending
              void (SocketBS.recv socket 1)
              WS.sendTextData connection ("{}" :: BS.ByteString)
              takeMVar release
              drainSocket socket
          )
          $ \target -> do
            let value = KeyMap.singleton "text" (String (Text.replicate (4 * 1024 * 1024) "x"))
            withAsync
              ( withPlainWebSocket (8 * 1024 * 1024) 1000000 50000 target $ \channel ->
                  withAsync (sendObject channel value) $ \sender -> receiveObject channel >> putMVar ready () >> wait sender
              )
              $ \worker -> do
                takeMVar ready
                cancel worker
                waitCatch worker >>= \case
                  Left err -> fromException err @?= Just AsyncCancelled
                  Right _ -> assertFailure "Blocked write unexpectedly succeeded"
                putMVar release (),
      testCase "HTTP handshake deadline closes an accepted socket" $ bounded $ do
        accepted <- newEmptyMVar
        withSocketPeer (\socket -> void (SocketBS.recv socket 1) >> putMVar accepted () >> drainSocket socket) $ \target -> do
          result <- try @WebSocketError (withPlainWebSocket 128 1000000 50000 target (\_ -> pure ()))
          result @?= Left WebSocketConnectTimeout
          seen <- timeout 100000 (takeMVar accepted)
          seen @?= Just (),
      testCase "outer cancellation during handshake is not relabeled" $ bounded $ do
        started <- newEmptyMVar
        withSocketPeer (\socket -> void (SocketBS.recv socket 1) >> putMVar started () >> drainSocket socket) $ \target ->
          withAsync (withPlainWebSocket 128 1000000 50000 target (\_ -> pure ())) $ \worker -> do
            takeMVar started
            cancel worker
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled handshake returned",
      testCase "HTTP rejection remains payload-free" $
        bounded $
          withSocketPeer (\socket -> WS.makePendingConnection socket WS.defaultConnectionOptions >>= \pending -> WS.rejectRequest pending "private rejection") $ \target ->
            expectError WebSocketConnectFailure (withPlainWebSocket 128 1000000 50000 target (\_ -> pure ())),
      testCase "invalid configuration is rejected before network activity" $ do
        let target = WebSocketTarget "127.0.0.1" 1 "/"
        expectError InvalidWebSocketLimit (withPlainWebSocket 0 1 0 target (\_ -> pure ()))
        expectError InvalidWebSocketTimeout (withPlainWebSocket 128 (-1) 0 target (\_ -> pure ()))
        expectError WebSocketConnectTimeout (withPlainWebSocket 128 0 0 target (\_ -> pure ()))
        forM_ [target {webSocketHost = ""}, target {webSocketHost = "user@host"}, target {webSocketHost = "host\r\ninjected"}, target {webSocketPort = -1}, target {webSocketPort = 0}, target {webSocketPort = 65536}, target {webSocketPath = ""}, target {webSocketPath = "/bad path"}, target {webSocketPath = "/#fragment"}, target {webSocketPath = "/\\path"}] $ \bad ->
          expectError InvalidWebSocketTarget (withPlainWebSocket 128 1 0 bad (\_ -> pure ()))
        show (target {webSocketPath = "/?private=value"}) @?= "WebSocketTarget <redacted>",
      testCase "concurrent writers emit distinct complete text messages"
        $ bounded
        $ withPeer
          ( \_ connection -> do
              values <- replicateM 20 (WS.receiveData connection :: IO BL.ByteString)
              decoded <- mapM (either (const (assertFailure "Malformed concurrent message")) pure . eitherDecode) values :: IO [Object]
              sort (map (KeyMap.lookup "id") decoded) @?= sort (map (Just . toJSON) ([1 .. 20] :: [Int]))
          )
        $ \target ->
          withPlainWebSocket 128 1000000 50000 target $ \channel ->
            forConcurrently_ ([1 .. 20] :: [Int]) (sendObject channel . KeyMap.singleton "id" . toJSON),
      testCase "typed RPC calls use the same WebSocket text channel"
        $ bounded
        $ withPeer
          ( \_ connection -> do
              bytes <- WS.receiveData connection
              request <- either (const (assertFailure "Invalid RPC fixture")) pure (eitherDecode bytes) :: IO Object
              KeyMap.lookup "method" request @?= Just (String "droid.list_models")
              KeyMap.lookup "params" request @?= Just (Object mempty)
              WS.sendTextData connection (encode (reply "models" (Object (KeyMap.singleton "models" (Array mempty)))))
          )
        $ \target ->
          withPlainWebSocket 4096 1000000 50000 target $ \socket ->
            withRpcChannel (sendObject socket) (receiveObject socket) $ \channel -> do
              models <- listModels channel (CallOptions "models" (WithEnvelope (Just "1.205.0") Nothing mempty) Nothing) (ListModelsOptions Nothing mempty)
              toJSON models @?= Object (KeyMap.singleton "models" (Array mempty))
    ]

withPlainWebSocket :: Int -> Int -> Int -> WebSocketTarget -> (ObjectWebSocket -> IO a) -> IO a
withPlainWebSocket limit connectMicros closeMicros = withWebSocket (WebSocketOptions Nothing limit connectMicros closeMicros)

-- Test-only credentials are generated in an owned private directory, not
-- checked in, logged or trusted outside this test connection.
withTLSCertificate :: (TLS.Credential -> Connection.TLSSettings -> IO a) -> IO a
withTLSCertificate action = bracket (mkdtemp "/tmp/droid-tls-fixture") removePathForcibly $ \directory -> do
  let cert = directory <> "/certificate.pem"
      key = directory <> "/key.pem"
      config = directory <> "/openssl.cnf"
  writeFile config "[req]\ndistinguished_name=dn\nx509_extensions=ext\n[dn]\n[ext]\nsubjectAltName=DNS:localhost\nbasicConstraints=critical,CA:TRUE\nkeyUsage=critical,keyCertSign,digitalSignature,keyEncipherment\n"
  (status, _, _) <- readProcessWithExitCode "openssl" ["req", "-new", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1", "-subj", "/CN=fixture", "-config", config, "-keyout", key, "-out", cert] ""
  status @?= ExitSuccess
  credential <- TLS.credentialLoadX509 cert key >>= either (const (assertFailure "Invalid generated TLS credential")) pure
  store <- Certificates.readCertificateStore cert >>= maybe (assertFailure "Invalid generated CA store") pure
  let params = (TLS.defaultParamsClient "localhost" "") {TLS.clientShared = def {TLS.sharedCAStore = store}, TLS.clientSupported = def {TLS.supportedCiphers = TLSExtra.ciphersuite_default}}
  action credential (Connection.TLSSettings params)

withTLSPeer :: TLS.Credential -> (WS.Connection -> IO ()) -> (WebSocketTarget -> IO a) -> IO a
withTLSPeer credential server = withSocketPeer $ \socket ->
  let params = def {TLS.serverShared = def {TLS.sharedCredentials = TLS.Credentials [credential]}, TLS.serverSupported = def {TLS.supportedCiphers = TLSExtra.ciphersuite_default}}
      serve = bracket (TLS.contextNew socket params) TLS.contextClose $ \context -> do
        TLS.handshake context
        stream <- Stream.makeStream (do bytes <- TLS.recvData context; pure (if BS.null bytes then Nothing else Just bytes)) (mapM_ (TLS.sendData context))
        pending <- WS.makePendingConnectionFromStream stream WS.defaultConnectionOptions
        connection <- WS.acceptRequest pending
        server connection
        forever (void (WS.receiveDataMessage connection)) `catch` \(_ :: WS.ConnectionException) -> pure ()
   in serve `catches` [Handler (\(_ :: IOException) -> pure ()), Handler (\(_ :: TLS.TLSException) -> pure ())]

payload :: Object
payload = KeyMap.singleton "text" (String "😀")

withPeer :: (Socket.Socket -> WS.Connection -> IO ()) -> (WebSocketTarget -> IO a) -> IO a
withPeer server = withSocketPeer $ \socket -> do
  pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
  connection <- WS.acceptRequest pending
  server socket connection
  forever (void (WS.receiveDataMessage connection)) `catch` \(_ :: WS.ConnectionException) -> pure ()

withSocketPeer :: (Socket.Socket -> IO ()) -> (WebSocketTarget -> IO a) -> IO a
withSocketPeer server client = bracket (WS.makeListenSocket "127.0.0.1" 0) Socket.close $ \listener -> do
  port <-
    Socket.getSocketName listener >>= \case
      Socket.SockAddrInet port _ -> pure (fromIntegral port)
      _ -> assertFailure "Expected loopback IPv4 socket"
  withAsync (bracket (Socket.accept listener) (Socket.close . fst) (server . fst)) $ \peer -> do
    link peer
    result <- client (WebSocketTarget "127.0.0.1" port "/fixture?query=value")
    wait peer
    pure result

drainSocket :: Socket.Socket -> IO ()
drainSocket socket =
  (SocketBS.recv socket 4096 >>= \bytes -> unless (BS.null bytes) (drainSocket socket))
    `catch` \err -> unless (isResourceVanishedError err) (throwIO (err :: IOException))

frame :: Word8 -> BS.ByteString -> BS.ByteString
frame opcode bytes = BS.pack [opcode, fromIntegral (BS.length bytes)] <> bytes

expectError :: WebSocketError -> IO a -> IO ()
expectError expected action =
  try @WebSocketError action >>= \case
    Left err -> err @?= expected
    Right _ -> assertFailure "Expected WebSocket failure"

data CallbackAbort = CallbackAbort deriving stock (Eq, Show)

instance Exception CallbackAbort
