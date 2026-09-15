{-# LANGUAGE OverloadedStrings #-}

module DaemonQueueSpec (queueTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (SomeException, catch, fromException, try)
import Control.Monad (forM_, forever, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidInvalidEvent))
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Control (QueuePlacement (..), QueueResolution (..), ResolveQueuedMessageParams (..))
import Factory.Droid.Schema.Daemon.Settings (defaultsModel)
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

queueTests :: TestTree
queueTests =
  testGroup
    "Daemon queue resolution"
    [ testCase "all queue actions preserve scoping and complete across early/late ACK and legacy replies" $
        bounded $
          forM_ [BeforeAck, AfterAck, Legacy] $ \mode -> forM_ [UpdateQueuedMessage QueueEndOfTurn, UpdateQueuedMessage QueueEndOfLoop, DeleteQueuedMessage] $ \resolution -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withQueuePeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
              result <- Daemon.resolveQueuedUserMessage connection "session" (input resolution)
              result @?= if mode == Legacy then legacyResult else mempty
            frames <- readIORef trace
            map (field "method") frames @?= [String "daemon.authenticate", String "daemon.resolve_queued_user_message"]
            case drop 1 frames of
              [request] -> field "params" request @?= Object (expectedParams resolution)
              _ -> assertFailure "Unexpected request count",
      testCase "foreign-session and original-queue-id events cannot complete the new RPC" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withQueuePeer WrongOnly trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          withAsync (try @RpcChannelError (Daemon.resolveQueuedUserMessage connection "session" (input DeleteQueuedMessage))) $ \pending -> do
            takeMVar ready
            Daemon.getDefaultSettings connection >>= (@?= Just "available") . defaultsModel
            wait pending >>= (@?= Left RpcChannelReadFailure),
      testCase "remote errors and malformed replies/events retain their errors and permit reuse" $
        bounded $
          forM_ [Rejected, InvalidReply, InvalidEvent] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withQueuePeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
              result <- try @SomeException (Daemon.resolveQueuedUserMessage connection "session" (input DeleteQueuedMessage))
              case result of
                Right _ -> assertFailure "Invalid resolution succeeded"
                Left cause -> case mode of
                  Rejected -> case fromException cause of
                    Just (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
                    _ -> assertFailure "Remote rejection changed exception type"
                  InvalidReply -> fromException cause @?= Just RpcInvalidResult
                  InvalidEvent -> fromException cause @?= Just DroidInvalidEvent
                  _ -> assertFailure "Unexpected test mode"
              Daemon.getDefaultSettings connection >>= (@?= Just "available") . defaultsModel,
      testCase "cancelling an acknowledged resolution preserves async identity and does not cancel remotely" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withQueuePeer Held trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.resolveQueuedUserMessage connection "session" (input DeleteQueuedMessage)) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled queue resolution returned"
          Daemon.getDefaultSettings connection >>= (@?= Just "available") . defaultsModel
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.resolve_queued_user_message", "daemon.get_default_settings"]) . map (field "method"),
      testCase "raw low-level replies expose ACKs immediately and respect a caller zero deadline" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "rpc-id" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
          withAsync (Client.resolveDaemonQueuedUserMessageRaw channel configured "session" (input (UpdateQueuedMessage QueueEndOfLoop))) $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "params" frame @?= Object (expectedParams (UpdateQueuedMessage QueueEndOfLoop))
            atomically (writeTQueue incoming (response frame (object ["accepted" .= True])))
            wait pending >>= (@?= KeyMap.singleton "accepted" (Bool True))
          result <- try @RpcChannelError (Client.resolveDaemonQueuedUserMessageRaw channel (configured {Client.callTimeoutMicros = Just 0}) "session" (input DeleteQueuedMessage))
          result @?= Left RpcRequestTimedOut
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

data ReplyMode = BeforeAck | AfterAck | Legacy | WrongOnly | Rejected | InvalidReply | InvalidEvent | Held
  deriving stock (Eq, Show)

input :: QueueResolution -> ResolveQueuedMessageParams
input resolution = ResolveQueuedMessageParams "queued-id" resolution (KeyMap.fromList ["sessionId" .= String "wrong", "requestId" .= String "wrong", "action" .= String "wrong", "future" .= False])

expectedParams :: QueueResolution -> Object
expectedParams resolution =
  KeyMap.fromList
    ( ["sessionId" .= String "session", "requestId" .= String "queued-id", "future" .= False] <> case resolution of
        UpdateQueuedMessage QueueEndOfTurn -> ["action" .= String "update_queue", "queuePlacement" .= String "end_of_turn"]
        UpdateQueuedMessage QueueEndOfLoop -> ["action" .= String "update_queue", "queuePlacement" .= String "end_of_loop"]
        DeleteQueuedMessage -> ["action" .= String "delete"]
    )

legacyResult :: Object
legacyResult = KeyMap.fromList ["accepted" .= False, "kept" .= False]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withQueuePeer :: ReplyMode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withQueuePeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid client RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      WS.sendTextData connection (encode (Object (response auth (object ["userId" .= String "user", "orgId" .= String "org"]))))
      forever $ do
        frame <- readFrame connection
        case field "method" frame of
          String "daemon.resolve_queued_user_message" -> case mode of
            Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Queue request rejected"]]
            InvalidReply -> reply connection frame Null
            Legacy -> reply connection frame (Object legacyResult)
            BeforeAck -> created connection "session" (field "id" frame) True >> ack connection frame
            AfterAck -> ack connection frame >> created connection "session" (field "id" frame) True
            InvalidEvent -> ack connection frame >> created connection "session" (field "id" frame) False
            Held -> ack connection frame >> putMVar ready ()
            WrongOnly -> do
              created connection "foreign" (field "id" frame) True
              created connection "session" (String "queued-id") True
              ack connection frame
              putMVar ready ()
          String "daemon.get_default_settings" -> do
            field "params" frame @?= Object mempty
            reply connection frame (object ["modelId" .= String "available"])
            when (mode == WrongOnly) (WS.sendClose connection ("fixture disconnect" :: Text))
          _ -> assertFailure "Unexpected request or implicit lifecycle operation"
    ack connection frame = reply connection frame (object ["accepted" .= True])
    reply connection frame value = WS.sendTextData connection (encode (Object (response frame value)))
    created connection identifier requestId valid = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= (identifier :: Text), "notification" .= object ["type" .= String "create_message", "requestId" .= requestId, "message" .= if valid then object ["id" .= String "created", "role" .= String "user", "content" .= ([] :: [Value]), "createdAt" .= (0 :: Int), "updatedAt" .= (0 :: Int)] else Bool False]]]

response :: Object -> Value -> Object
response request result = envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . Object . envelope

envelope :: [Pair] -> Object
envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key
