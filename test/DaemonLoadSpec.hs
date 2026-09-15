{-# LANGUAGE OverloadedStrings #-}

module DaemonLoadSpec (loadTests) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Exception (catch, finally, fromException, try)
import Control.Monad (forM_, forever)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Factory.Droid (DroidError (DroidInvalidEvent))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Protocol (RpcChannelError (RpcChannelClosed), RpcResultError (RpcRemoteFailure))
import Factory.Droid.Schema.Control (AddUserMessageParams (..), QueueResolution (DeleteQueuedMessage), QueuedUserMessage (..), ResolveQueuedMessageParams (..))
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))
import Factory.Droid.Schema.Mission (MissionSnapshot (..))
import Factory.Droid.Schema.Notifications (DroidWorkingState (WorkingThinking))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams))
import Factory.Droid.Schema.Session (SessionSnapshot (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import MissionStateSpec (missionSnapshotWire)
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

loadTests :: TestTree
loadTests =
  testGroup
    "Daemon load publication"
    [ testCase "loaded messages, settings, state and queues retain wire data and extension precedence" $ do
        state <- decodeValue @LoadedSessionState (Object loadWire)
        toJSON state @?= Object loadWire
        loadedWorkingState state @?= Just WorkingThinking
        loadedAgentLoopInProgress state @?= Just False
        loadedHasOlderMessages state @?= Just False
        sessionTitle (loadedSessionSnapshot state) @?= Just ""
        minimal <- decodeValue @LoadedSessionState (object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= field "settings" loadWire])
        loadedQueuedMessages minimal @?= Nothing
        emptyQueue <- decodeValue @LoadedSessionState (Object (KeyMap.insert "queuedMessages" (toJSON ([] :: [Value])) loadWire))
        loadedQueuedMessages emptyQueue @?= Just []
        queued <- decodeValue @QueuedUserMessage queuedWire
        let base = queuedMessageInput queued
            injected = queued {queuedMessageInput = base {userMessageAdditionalFields = KeyMap.insert "requestId" (String "wrong") (userMessageAdditionalFields base)}}
        toJSON injected @?= queuedWire
        show queued @?= "QueuedUserMessage <redacted>"
        show state @?= "LoadedSessionState <redacted>",
      testCase "validated mission load receipt restores a caller-owned immutable state" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        release <- newMVar ()
        let wire = Object (KeyMap.insert "mission" missionSnapshotWire loadWire)
        withLoadPeer (Right wire) trace ready release (pure ()) $ \target ->
          Daemon.withResumedSession (options target) "saved" $ \session -> do
            loaded <- maybe (assertFailure "Missing loaded state") pure (Daemon.daemonLoadedState (Daemon.sessionInfo session))
            snapshot <- maybe (assertFailure "Missing mission snapshot") pure (loadedMissionSnapshot loaded)
            toJSON snapshot @?= missionSnapshotWire
            toJSON loaded @?= wire
            let restored = Mission.missionSnapshot (Mission.restoreSnapshotAt "native-observation" snapshot Mission.emptyMissionStore)
            missionSnapshotWorkers restored @?= ["worker-b", "worker-a"]
            missionSnapshotTitle restored @?= Just ""
            missionSnapshotWorkingDirectory snapshot @?= Just "/remote/worktree"
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals"]) . map (field "method"),
      testCase "required snapshots and optional queue/state fields reject malformed known values" $ do
        forM_ invalidLoads $ rejects (Proxy @LoadedSessionState) . Object
        forM_ [object ["text" .= String ""], object ["requestId" .= Null, "text" .= String ""], object ["requestId" .= String "id"]] $ rejects (Proxy @QueuedUserMessage),
      testCase "load publication precedes queue access and reuses one authenticated connection" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        release <- newEmptyMVar
        published <- newIORef False
        withLoadPeer (Right (Object loadWire)) trace ready release (pure ()) $ \target -> do
          borrowed <- withAsync
            ( Daemon.withResumedSession (options target) "saved" $ \session -> do
                writeIORef published True
                state <- maybe (assertFailure "No loaded state") pure (Daemon.daemonLoadedState (Daemon.sessionInfo session))
                toJSON state @?= Object loadWire
                let connection = Daemon.sessionConnection session
                Daemon.connectionUser connection @?= Daemon.authenticatedUser session
                case loadedQueuedMessages state of
                  Just [queued] -> Daemon.resolveQueuedUserMessage connection (Daemon.sessionId session) (ResolveQueuedMessageParams (queuedMessageRequestId queued) DeleteQueuedMessage mempty) >>= (@?= mempty)
                  _ -> assertFailure "Queued submission missing"
                pure connection
            )
            $ \loading -> do
              takeMVar ready
              readIORef published >>= (@?= False)
              putMVar release ()
              wait loading
          readIORef published >>= (@?= True)
          late <- try @RpcChannelError (Daemon.getDefaultSettings borrowed)
          late @?= Left RpcChannelClosed
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.resolve_queued_user_message"]) . map (field "method"),
      testCase "malformed load state never reaches the session action" $
        bounded $
          forM_ invalidLoads $ \wire -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            release <- newMVar ()
            published <- newIORef False
            withLoadPeer (Right (Object wire)) trace ready release (pure ()) $ \target -> do
              result <- try @DroidError (Daemon.withResumedSession (options target) "saved" (\_ -> writeIORef published True))
              result @?= Left DroidInvalidEvent
            readIORef published >>= (@?= False)
            readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.load_session"]) . map (field "method"),
      testCase "cancelling a held load publishes no handle and releases owned transport work" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        release <- newEmptyMVar
        stopped <- newEmptyMVar
        published <- newIORef False
        withLoadPeer (Right (Object loadWire)) trace ready release (putMVar stopped ()) $ \target ->
          withAsync (Daemon.withResumedSession (options target) "saved" (\_ -> writeIORef published True)) $ \loading -> do
            takeMVar ready
            cancel loading
            waitCatch loading >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled load returned"
            putMVar release ()
        readIORef published >>= (@?= False)
        tryTakeMVar stopped >>= (@?= Just ())
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.load_session"]) . map (field "method"),
      testCase "load rejection preserves the remote error without creating a replacement" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        release <- newMVar ()
        let rejected = JsonRpcError RpcInvalidParams "Load rejected" Nothing mempty
        withLoadPeer (Left rejected) trace ready release (pure ()) $ \target -> do
          result <- try @RpcResultError (Daemon.withResumedSession (options target) "saved" (const (assertFailure "Rejected load published a session" :: IO ())))
          result @?= Left (RpcRemoteFailure rejected)
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.load_session"]) . map (field "method")
    ]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withLoadPeer :: Either JsonRpcError Value -> IORef [Object] -> MVar () -> MVar () -> IO () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withLoadPeer report trace ready release stopped = withPeer (\_ connection -> (serve connection `catch` \(_ :: WS.ConnectionException) -> pure ()) `finally` stopped)
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
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      request <- readFrame connection
      field "method" request @?= String "daemon.load_session"
      field "params" request @?= object ["sessionId" .= String "saved", "token" .= String "OFFLINE_ONLY", "loadAllMessages" .= True, "autoRejectPermissionRequests" .= True]
      putMVar ready ()
      takeMVar release
      case report of
        Right value -> reply connection request value
        Left err -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= err]
      forever $ do
        resolution <- readFrame connection
        case field "method" resolution of
          String "daemon.list_terminals" -> do
            field "params" resolution @?= object ["sessionId" .= String "saved"]
            reply connection resolution (object ["terminals" .= ([] :: [Value])])
          _ -> do
            field "method" resolution @?= String "daemon.resolve_queued_user_message"
            field "params" resolution @?= object ["sessionId" .= String "saved", "requestId" .= String "stored-queue", "action" .= String "delete"]
            reply connection resolution (object ["accepted" .= True])
            sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= String "saved", "notification" .= object ["type" .= String "create_message", "requestId" .= field "id" resolution, "message" .= savedMessage]]]
    reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

savedMessage :: Value
savedMessage = object ["id" .= String "saved-message", "role" .= String "user", "content" .= [object ["type" .= String "text", "text" .= String "saved text"]], "createdAt" .= (0 :: Int), "updatedAt" .= (0 :: Int)]

queuedWire :: Value
queuedWire = object ["requestId" .= String "stored-queue", "text" .= String "", "queuePlacement" .= String "end_of_turn", "skipAgentLoop" .= False, "future" .= False]

loadWire :: Object
loadWire = KeyMap.fromList ["session" .= object ["messages" .= [savedMessage], "title" .= String ""], "settings" .= object ["modelId" .= String "offline-model", "reasoningEffort" .= String "low"], "hasOlderMessages" .= False, "isAgentLoopInProgress" .= False, "workingState" .= String "thinking", "queuedMessages" .= [queuedWire], "cwd" .= String "/daemon/cwd", "future" .= object ["retained" .= Null]]

invalidLoads :: [Object]
invalidLoads = [KeyMap.delete "session" loadWire, KeyMap.delete "settings" loadWire, KeyMap.insert "session" (object ["messages" .= [Bool False]]) loadWire, KeyMap.insert "session" (object ["messages" .= ([] :: [Value]), "title" .= False]) loadWire, KeyMap.insert "queuedMessages" (toJSON [object ["text" .= String "missing id"]]) loadWire] <> [KeyMap.insert key Null loadWire | key <- ["hasOlderMessages", "isAgentLoopInProgress", "workingState", "queuedMessages", "mission"]] <> [KeyMap.insert "workingState" (String "future") loadWire, KeyMap.insert "mission" (object ["state" .= String "running"]) loadWire, KeyMap.insert "mission" (object ["state" .= String "running", "features" .= ([] :: [Value]), "progressLog" .= [object ["type" .= String "future_entry"]], "workerSessionIds" .= ([] :: [Value])]) loadWire]
