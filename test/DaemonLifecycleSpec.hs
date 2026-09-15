{-# LANGUAGE OverloadedStrings #-}

module DaemonLifecycleSpec (lifecycleTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Control
import Factory.Droid.Schema.Daemon.Session (DaemonCloseSessionParams (..), defaultDaemonCloseSessionParams)
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

lifecycleTests :: TestTree
lifecycleTests =
  testGroup
    "Daemon session lifecycle controls"
    [ testCase "close preserves absent/false/true draft policy and reserves declared fields" $ do
        toJSON (defaultDaemonCloseSessionParams "worker-session") @?= object ["sessionId" .= String "worker-session"]
        toJSON closeParams @?= closeWire
        forM_ [False, True] $ \preserve -> roundTrip (Proxy @DaemonCloseSessionParams) (object ["sessionId" .= String "", "preserveEmptyDraft" .= preserve])
        rejects (Proxy @DaemonCloseSessionParams) (object ["sessionId" .= String "worker-session", "preserveEmptyDraft" .= Null])
        rejects (Proxy @DaemonCloseSessionParams) (object ["preserveEmptyDraft" .= False])
        show closeParams @?= "DaemonCloseSessionParams <redacted>",
      testCase "shared rewind/fork/compact codecs retain all fields and strict optionals" $ do
        roundTrip (Proxy @GetRewindInfoResult) infoWire
        roundTrip (Proxy @ExecuteRewindParams) rewindWire
        roundTrip (Proxy @ExecuteRewindResult) rewoundWire
        roundTrip (Proxy @CompactSessionResult) compactedWire
        roundTrip (Proxy @ForkSessionParams) (object ["title" .= String "", "tags" .= ([] :: [Value])])
        toJSON (CompactSessionParams Nothing mempty) @?= object []
        toJSON (ForkSessionParams Nothing Nothing mempty) @?= object []
        rejects (Proxy @CompactSessionParams) (object ["customInstructions" .= Null])
        rejects (Proxy @ForkSessionParams) (object ["tags" .= Null])
        roundTrip (Proxy @CompactSessionResult) (object ["newSessionId" .= String "", "removedCount" .= Number (-1.5)]),
      testCase "six explicit controls return reports without implicit load/replacement or scope-exit close" $ bounded $ do
        trace <- newIORef []
        mode <- newIORef Normal
        ready <- newEmptyMVar
        withControlPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          forM_ (zip wireCases (highCalls connection)) $ \((_, _, expected), request) -> request >>= (@?= expected)
          Daemon.getRewindInfo connection infoParams >>= (@?= infoWire) . toJSON
        frames <- readIORef trace
        map (field "method") frames @?= map String ("daemon.authenticate" : map (\(method, _, _) -> method) wireCases <> ["daemon.get_rewind_info"])
        length (nub (map (field "id") frames)) @?= length frames,
      testCase "remote and malformed outcomes remain explicit with successful follow-up calls" $ bounded $ do
        trace <- newIORef []
        mode <- newIORef Normal
        ready <- newEmptyMVar
        withControlPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ [Rejected, Malformed] $ \failure -> forM_ (highCalls connection) $ \request -> do
            writeIORef mode failure
            result <- try @RpcResultError request
            case result of
              Left (RpcRemoteFailure err) | failure == Rejected -> do
                rpcErrorCode err @?= RpcInvalidParams
                rpcErrorData err @?= Just (object ["code" .= String "control_rejected", "retryable" .= False])
              Left RpcInvalidResult | failure == Malformed -> pure ()
              _ -> assertFailure "Unexpected lifecycle result"
            writeIORef mode Normal
            Daemon.getRewindInfo connection infoParams >>= (@?= infoWire) . toJSON,
      testCase "cancelling each admitted mutation preserves async identity and connection reuse" $
        bounded $
          forM_ (zip [1 .. 5] (drop 1 wireCases)) $ \(index, (method, _, _)) -> do
            trace <- newIORef []
            mode <- newIORef (Held method)
            ready <- newEmptyMVar
            withControlPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
              withAsync (highCalls connection !! index) $ \pending -> do
                takeMVar ready
                cancel pending
                waitCatch pending >>= \case
                  Left cause -> fromException cause @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled control returned"
              Daemon.getRewindInfo connection infoParams >>= (@?= infoWire) . toJSON
            readIORef trace >>= (@?= map String ["daemon.authenticate", method, "daemon.get_rewind_info"]) . map (field "method"),
      testCase "channel loss fails every control and later connection calls" $
        bounded $
          forM_ [0 .. 5] $ \index -> do
            trace <- newIORef []
            mode <- newIORef Disconnect
            ready <- newEmptyMVar
            withControlPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
              expectChannelFailure (highCalls connection !! index)
              expectChannelFailure (toJSON <$> Daemon.getRewindInfo connection infoParams),
      testCase "low-level routing and deadlines remain caller-owned, including compaction" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "control-rpc" (WithEnvelope (Just "custom-protocol") Nothing (KeyMap.fromList ["factoryProtocolVersion" .= String "wrong", "params" .= String "wrong", "method" .= String "wrong", "id" .= String "wrong", "callerExtra" .= False])) (Just 1000000)
          forM_ (zip [0 ..] wireCases) $ \(index, (method, params, expected)) -> do
            let identifier = method <> "-rpc"
            withAsync (lowCalls channel (configured {Client.callRequestId = identifier}) !! index) $ \pending -> do
              frame <- atomically (readTQueue outgoing)
              field "method" frame @?= String method
              field "params" frame @?= params
              field "id" frame @?= String identifier
              field "factoryApiVersion" frame @?= String "1.0.0"
              field "factoryProtocolVersion" frame @?= String "custom-protocol"
              field "callerExtra" frame @?= Bool False
              atomically (writeTQueue incoming (response frame expected))
              wait pending >>= (@?= expected)
          forM_ (lowCalls channel (configured {Client.callTimeoutMicros = Just 0})) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
          withAsync (try @RpcChannelError (Client.compactDaemonSession channel (configured {Client.callTimeoutMicros = Just 20000}) "source-session" compactParams)) $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String "daemon.compact_session"
            wait pending >>= (@?= Left RpcRequestTimedOut)
    ]

infoParams :: GetRewindInfoParams
infoParams = GetRewindInfoParams "source-session" "message" (KeyMap.fromList ["sessionId" .= String "wrong", "messageId" .= String "wrong", "future" .= False])

rewindParams :: ExecuteRewindParams
rewindParams = ExecuteRewindParams "source-session" "message" [RewindFileSnapshot "file.txt" "opaque-hash" 4 mempty] [RewindFileCreation "created.txt" mempty] "Rewind title" (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])

compactParams :: CompactSessionParams
compactParams = CompactSessionParams (Just "") (KeyMap.fromList ["sessionId" .= String "wrong", "customInstructions" .= String "wrong", "future" .= False])

forkParams :: ForkSessionParams
forkParams = ForkSessionParams (Just "") (Just [ForkSessionTag "" (Just (KeyMap.singleton "kind" "")) mempty]) (KeyMap.fromList ["sessionId" .= String "wrong", "title" .= String "wrong", "future" .= False])

workerParams :: KillWorkerSessionParams
workerParams = KillWorkerSessionParams "worker-session" (KeyMap.fromList ["sessionId" .= String "wrong", "workerSessionId" .= String "wrong", "future" .= False])

closeParams :: DaemonCloseSessionParams
closeParams = DaemonCloseSessionParams "worker-session" (Just False) (KeyMap.fromList ["sessionId" .= String "wrong", "preserveEmptyDraft" .= True, "future" .= False])

highCalls :: Daemon.DaemonConnection -> [IO Value]
highCalls connection = [toJSON <$> Daemon.getRewindInfo connection infoParams, toJSON <$> Daemon.executeRewind connection rewindParams, toJSON <$> Daemon.compactSession connection "source-session" compactParams, toJSON <$> Daemon.forkSession connection "source-session" forkParams, toJSON <$> Daemon.killWorkerSession connection "source-session" workerParams, toJSON <$> Daemon.closeSession connection closeParams]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel configured = [toJSON <$> Client.getDaemonRewindInfo channel configured infoParams, toJSON <$> Client.executeDaemonRewind channel configured rewindParams, toJSON <$> Client.compactDaemonSession channel configured "source-session" compactParams, toJSON <$> Client.forkDaemonSession channel configured "source-session" forkParams, toJSON <$> Client.killDaemonWorkerSession channel configured "source-session" workerParams, toJSON <$> Client.closeDaemonSession channel configured closeParams]

wireCases :: [(Text, Value, Value)]
wireCases = [("daemon.get_rewind_info", infoParamsWire, infoWire), ("daemon.execute_rewind", rewindWire, rewoundWire), ("daemon.compact_session", compactWire, compactedWire), ("daemon.fork_session", forkWire, object ["newSessionId" .= String "forked-session"]), ("daemon.kill_worker_session", workerWire, object []), ("daemon.close_session", closeWire, object [])]

data Mode = Normal | Rejected | Malformed | Held Text | Disconnect deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withControlPeer :: IORef Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withControlPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
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
            readIORef mode >>= \case
              Rejected -> WS.sendTextData connection (encode (Object (KeyMap.insert "error" (object ["code" .= RpcInvalidParams, "message" .= String "Control rejected", "data" .= object ["code" .= String "control_rejected", "retryable" .= False]]) (KeyMap.delete "result" (response request Null)))))
              Malformed -> reply connection request Null
              Held selected | selected == method -> putMVar ready ()
              Disconnect -> WS.sendClose connection ("control connection closed" :: Text)
              _ -> reply connection request result
          _ -> assertFailure "Unexpected automatic session load, interrupt or close"
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

response :: Object -> Value -> Object
response request result = KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "response", "id" .= field "id" request, "result" .= result]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = case fromJSON @a value of Success result -> toJSON result @?= value; Error err -> assertFailure err

expectChannelFailure :: IO Value -> IO ()
expectChannelFailure action = try @RpcChannelError action >>= \case Left _ -> pure (); Right _ -> assertFailure "Closed connection accepted a control"

infoParamsWire, infoWire, rewindWire, rewoundWire, compactWire, compactedWire, forkWire, workerWire, closeWire :: Value
infoParamsWire = object ["sessionId" .= String "source-session", "messageId" .= String "message", "future" .= False]
infoWire = object ["availableFiles" .= [object ["filePath" .= String "file.txt", "contentHash" .= String "opaque-hash", "size" .= Number 4]], "createdFiles" .= [object ["filePath" .= String "created.txt"]], "evictedFiles" .= [object ["filePath" .= String "evicted.txt", "reason" .= String "future reason"]], "future" .= False]
rewindWire = object ["sessionId" .= String "source-session", "messageId" .= String "message", "filesToRestore" .= [object ["filePath" .= String "file.txt", "contentHash" .= String "opaque-hash", "size" .= Number 4]], "filesToDelete" .= [object ["filePath" .= String "created.txt"]], "forkTitle" .= String "Rewind title", "future" .= False]
rewoundWire = object ["newSessionId" .= String "rewound-session", "restoredCount" .= Number 0, "deletedCount" .= Number 1, "failedRestoreCount" .= Number 1, "failedDeleteCount" .= Number 0, "future" .= False]
compactWire = object ["sessionId" .= String "source-session", "customInstructions" .= String "", "future" .= False]
compactedWire = object ["newSessionId" .= String "compacted-session", "removedCount" .= Number 2, "future" .= False]
forkWire = object ["sessionId" .= String "source-session", "title" .= String "", "tags" .= [object ["name" .= String "", "metadata" .= object ["kind" .= String ""]]], "future" .= False]
workerWire = object ["sessionId" .= String "source-session", "workerSessionId" .= String "worker-session", "future" .= False]
closeWire = object ["sessionId" .= String "worker-session", "preserveEmptyDraft" .= False, "future" .= False]
