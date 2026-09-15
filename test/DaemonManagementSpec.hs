{-# LANGUAGE OverloadedStrings #-}

module DaemonManagementSpec (managementTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Management
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

managementTests :: TestTree
managementTests =
  testGroup
    "Daemon management"
    [ testCase "management codecs preserve false/empty/optional values and redact displays" $ do
        trigger <- decodeValue @TriggerUpdateResult (object ["triggered" .= False, "message" .= String ""])
        updateTriggered trigger @?= False
        updateMessage trigger @?= Just ""
        key <- decodeValue @InstallSshKeyParams (object ["publicKey" .= String " "])
        toJSON key @?= object ["publicKey" .= String " "]
        installed <- decodeValue @InstallSshKeyResult (object ["installed" .= False])
        sshKeyInstalled installed @?= False
        token <- decodeValue @ProxyTokenResult (object ["token" .= String ""])
        proxyToken token @?= ""
        relay <- decodeValue @RelayStartResult (object ["relayUrl" .= String "", "computerId" .= String ""])
        startedRelayUrl relay @?= ""
        state <- decodeValue @RelayStatus (object ["connected" .= False, "url" .= String "", "clientCount" .= Number (-0.5), "computerId" .= String ""])
        relayClientCount state @?= Just (-0.5)
        minimal <- decodeValue @RelayStatus (object ["connected" .= True])
        relayUrl minimal @?= Nothing
        [show trigger, show key, show installed, show token, show relay, show state] @?= ["TriggerUpdateResult <redacted>", "InstallSshKeyParams <redacted>", "InstallSshKeyResult <redacted>", "ProxyTokenResult <redacted>", "RelayStartResult <redacted>", "RelayStatus <redacted>"]
        rejects (Proxy @InstallSshKeyParams) (object ["publicKey" .= String ""])
        rejects (Proxy @InstallSshKeyParams) (object ["publicKey" .= Null])
        rejects (Proxy @ProxyTokenResult) (object ["token" .= Null])
        forM_ ["url", "clientCount", "computerId"] $ \name -> rejects (Proxy @RelayStatus) (object ["connected" .= False, name .= Null])
        fromJSON (object []) @?= Success RelayStopResult
        toJSON RelayStopResult @?= object []
        rejects (Proxy @RelayStopResult) (object ["unexpected" .= False])
        rejects (Proxy @RelayStopResult) Null,
      testCase "all seven global operations and early relay events preserve exact wire contracts" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        seen <- newTQueueIO
        key <- keyParams
        withManagementPeer Normal trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          stop <- Daemon.onRelayStatusChanged connection (atomically . writeTQueue seen)
          token <- Daemon.getProxyToken connection
          proxyToken token @?= "OFFLINE_PROXY_TOKEN"
          show token @?= "ProxyTokenResult <redacted>"
          Daemon.installSshKey connection (installedSshPublicKey key) >>= (@?= False) . sshKeyInstalled
          Daemon.triggerUpdate connection >>= (@?= object ["triggered" .= False, "message" .= String ""]) . toJSON
          Daemon.startRelay connection >>= (@?= object ["relayUrl" .= String relayAddress, "computerId" .= String "computer"]) . toJSON
          first <- atomically (readTQueue seen)
          fmap relayConnected first @?= Right True
          Daemon.getRelayStatus connection >>= (@?= statusWire True) . toJSON
          Daemon.stopRelay connection >>= (@?= RelayStopResult)
          second <- atomically (readTQueue seen)
          fmap relayConnected second @?= Right False
          Daemon.getRelayStatus connection >>= (@?= statusWire False) . toJSON
          stop
          Daemon.logout connection >>= (@?= logoutWire) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.get_proxy_token", "daemon.install_ssh_key", "daemon.trigger_update", "daemon.relay.start", "daemon.relay.get_status", "daemon.relay.stop", "daemon.relay.get_status", "daemon.logout"]) . map (field "method"),
      testCase "relay observation filters methods, reports malformed payloads and permits callback queries" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        bad <- newEmptyMVar
        observed <- newEmptyMVar
        withManagementPeer BadEvents trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          stop <- Daemon.onRelayStatusChanged connection $ \case
            Left err -> putMVar bad err
            Right _ -> Daemon.getRelayStatus connection >>= putMVar observed . toJSON
          _ <- Daemon.startRelay connection
          takeMVar bad >>= (@?= Daemon.InvalidDaemonEvent)
          takeMVar observed >>= (@?= statusWire True)
          stop,
      testCase "remote and malformed management replies remain explicit and leave the connection reusable" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            key <- keyParams
            withManagementPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
              forM_ [void (Daemon.getProxyToken connection), void (Daemon.installSshKey connection (installedSshPublicKey key)), void (Daemon.triggerUpdate connection), void (Daemon.startRelay connection), void (Daemon.stopRelay connection), void (Daemon.getRelayStatus connection), void (Daemon.logout connection)] $ \request -> do
                result <- try @RpcResultError request
                case result of
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected management result",
      testCase "cancelled update and logout waits preserve async identity without sending remote cancellation" $
        bounded $
          forM_ [("daemon.trigger_update", void . Daemon.triggerUpdate), ("daemon.logout", void . Daemon.logout)] $ \(method, request) -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withManagementPeer (HeldRequest method) trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
              withAsync (request connection) $ \pending -> do
                takeMVar ready
                cancel pending
                waitCatch pending >>= \case
                  Left cause -> fromException cause @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled management request returned"
              Daemon.getProxyToken connection >>= (@?= "OFFLINE_PROXY_TOKEN") . proxyToken
            readIORef trace >>= (@?= map String ["daemon.authenticate", method, "daemon.get_proxy_token"]) . map (field "method"),
      testCase "disconnect before logout acknowledgment stays a channel failure" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withManagementPeer Disconnected trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          try @RpcChannelError (void (Daemon.logout connection)) >>= (@?= Left RpcChannelReadFailure)
          try @RpcChannelError (void (Daemon.getProxyToken connection)) >>= (@?= Left RpcChannelReadFailure),
      testCase "proxy omission, logout acknowledgment and envelopes obey caller deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        key <- keyParams
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let body = KeyMap.fromList ["params" .= Null, "method" .= String "wrong", "id" .= String "wrong", "trace" .= False]
              configured = Client.CallOptions "proxy" (WithEnvelope (Just "1.201.1") Nothing body) (Just 1000000)
          withAsync (Client.getDaemonProxyToken channel configured) $ \pending -> do
            request <- atomically (readTQueue outgoing)
            KeyMap.lookup "params" request @?= Nothing
            field "id" request @?= String "proxy"
            field "method" request @?= String "daemon.get_proxy_token"
            field "trace" request @?= Bool False
            atomically (writeTQueue incoming (response request (object ["token" .= String "OFFLINE_PROXY_TOKEN"])))
            wait pending >>= (@?= "OFFLINE_PROXY_TOKEN") . proxyToken
          withAsync (Client.logoutDaemon channel (configured {Client.callRequestId = "logout"})) $ \pending -> do
            request <- atomically (readTQueue outgoing)
            field "params" request @?= object []
            field "id" request @?= String "logout"
            field "method" request @?= String "daemon.logout"
            field "trace" request @?= Bool False
            atomically (writeTQueue incoming (response request logoutWire))
            wait pending >>= (@?= logoutWire) . toJSON
          forM_ [("missing", object []), ("false", object ["accepted" .= False]), ("null", object ["accepted" .= Null]), ("nonobject", Null)] $ \(identifier, malformed) ->
            withAsync (Client.logoutDaemon channel (configured {Client.callRequestId = identifier})) $ \pending -> do
              request <- atomically (readTQueue outgoing)
              atomically (writeTQueue incoming (response request malformed))
              try @RpcResultError (wait pending) >>= (@?= Left RpcInvalidResult)
          let expired = configured {Client.callTimeoutMicros = Just 0}
          forM_ [void (Client.getDaemonProxyToken channel expired), void (Client.installDaemonSshKey channel expired key), void (Client.triggerDaemonUpdate channel expired mempty), void (Client.startDaemonRelay channel expired mempty), void (Client.stopDaemonRelay channel expired mempty), void (Client.getDaemonRelayStatus channel expired mempty), void (Client.logoutDaemon channel expired)] $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

data Mode = Normal | BadEvents | Rejected | Malformed | HeldRequest Text | Disconnected deriving stock (Eq, Show)

keyParams :: IO InstallSshKeyParams
keyParams = decodeValue (object ["publicKey" .= String publicKey])

publicKey, relayAddress :: Text
publicKey = "ssh-ed25519 OFFLINE_TEST_KEY fixture"
relayAddress = "wss://relay.invalid/client"

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withManagementPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withManagementPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      request <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      field "factoryProtocolVersion" request @?= String "1.201.1"
      modifyIORef' trace (<> [request])
      pure request
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      connected <- newIORef False
      forever $ do
        request <- readFrame connection
        method <- case field "method" request of String name -> pure name; _ -> assertFailure "Missing method"
        case method of
          "daemon.get_proxy_token" -> KeyMap.lookup "params" request @?= Nothing
          "daemon.install_ssh_key" -> field "params" request @?= object ["publicKey" .= String publicKey]
          _ -> field "params" request @?= object []
        case mode of
          Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Management rejected"]]
          Malformed -> reply connection request (if method == "daemon.relay.stop" then object ["unexpected" .= False] else Null)
          Disconnected -> WS.sendClose connection ("fixture disconnect" :: Text)
          HeldRequest held | method == held -> putMVar ready ()
          _ -> case method of
            "daemon.logout" -> reply connection request logoutWire
            "daemon.get_proxy_token" -> reply connection request (object ["token" .= String "OFFLINE_PROXY_TOKEN"])
            "daemon.install_ssh_key" -> reply connection request (object ["installed" .= False])
            "daemon.trigger_update" -> reply connection request (object ["triggered" .= False, "message" .= String ""])
            "daemon.relay.start" -> do
              writeIORef connected True
              when (mode == BadEvents) $ do
                notify connection "daemon.unrelated" (Bool False)
                notify connection "daemon.relay.status_changed" (object ["connected" .= String "wrong"])
              notify connection "daemon.relay.status_changed" (statusWire True)
              reply connection request (object ["relayUrl" .= String relayAddress, "computerId" .= String "computer"])
            "daemon.relay.stop" -> do
              writeIORef connected False
              notify connection "daemon.relay.status_changed" (statusWire False)
              reply connection request (object [])
            "daemon.relay.get_status" -> readIORef connected >>= reply connection request . statusWire
            _ -> assertFailure "Unexpected management request"
    reply connection request result = WS.sendTextData connection (encode (Object (response request result)))
    notify connection method params = sendFrame connection ["type" .= String "notification", "method" .= (method :: Text), "params" .= params]

logoutWire :: Value
logoutWire = object ["accepted" .= True, "extension" .= False]

statusWire :: Bool -> Value
statusWire connected = object ["connected" .= connected, "url" .= String relayAddress, "clientCount" .= (if connected then (2 :: Int) else 0), "computerId" .= String "computer"]

response :: Object -> Value -> Object
response request result = envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . Object . envelope

envelope :: [Pair] -> Object
envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
