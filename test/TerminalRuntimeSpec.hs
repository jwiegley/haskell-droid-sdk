{-# LANGUAGE OverloadedStrings #-}

module TerminalRuntimeSpec (terminalRuntimeTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (Exception, catch, finally, fromException, throwIO, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Daemon.Terminal (TerminalNotification (..), WriteTerminalDataParams (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcEntityNotFound))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

terminalRuntimeTests :: TestTree
terminalRuntimeTests =
  testGroup
    "Terminal runtime state"
    [ testCase "default resumed-session loading restores terminal metadata and exact serialized state" $ bounded $ do
        (_, trace) <- withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \_ -> do
            terminals <- Daemon.getTerminals connection "owned"
            fmap State.terminalMetadataStatus (Map.lookup "main" terminals) @?= Just State.TerminalConnected
            snapshot <- Daemon.getTerminalSerializedState connection "owned" "main"
            fmap State.terminalSerializedText snapshot @?= Just "screen-1"
            fmap State.terminalSerializedCursorHidden snapshot @?= Just (Just False)
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Nothing)
            Daemon.setActiveTerminalId connection "owned" (Just "main")
            Daemon.getActiveTerminalId connection "owned" >>= (@?= Just "main")
        map (field "method") trace @?= map String ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals"],
      testCase "disabled automatic restoration and read-only listing do not change retained terminals" $ bounded $ do
        void $ withStatePeer Immediate False $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \_ -> do
            Daemon.getTerminals connection "owned" >>= (@?= mempty)
            void (Daemon.listTerminals connection "owned")
            Daemon.getTerminals connection "owned" >>= (@?= mempty)
            void (Daemon.loadTerminals connection "owned")
            Daemon.getTerminals connection "owned" >>= (@?= ["main"]) . Map.keys,
      testCase "writers flush existing data, support ordinary queries, route live output and unsubscribe by token" $ bounded $ do
        void $ withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \session -> do
            barrier <- outputBarrier connection "owned"
            sendOutput connection "owned" "before"
            barrier
            seen <- newIORef []
            oldStop <- Daemon.registerTerminalWriteHandler session "main" $ \value -> do
              void (Daemon.listTerminals connection "owned")
              modifyIORef' seen (<> [value])
            readIORef seen >>= (@?= ["before"])
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Nothing)
            fresh <- newIORef []
            freshStop <- Daemon.registerTerminalWriteHandler session "main" (\value -> modifyIORef' fresh (<> [value]))
            oldStop
            oldStop
            sendOutput connection "owned" "live"
            barrier
            readIORef fresh >>= (@?= ["live"])
            freshStop
            sendOutput connection "owned" "unmounted"
            barrier
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "unmounted"),
      testCase "failed initial flush preserves the original exception and buffers for explicit retry" $ bounded $ do
        void $ withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \session -> do
            barrier <- outputBarrier connection "owned"
            sendOutput connection "owned" "before"
            barrier
            failed <- try @SinkFailure (Daemon.registerTerminalWriteHandler session "main" (const (throwIO SinkFailure)))
            case failed of Left cause -> cause @?= SinkFailure; Right _ -> assertFailure "Writer failure was swallowed"
            Daemon.getSessionState connection "owned" >>= (@?= Just (State.TerminalWriterFailed "main")) . State.sessionTerminalError
            sendOutput connection "owned" "after"
            barrier
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "beforeafter")
            seen <- newIORef []
            stop <- Daemon.registerTerminalWriteHandler session "main" (\value -> modifyIORef' seen (<> [value]))
            readIORef seen >>= (@?= ["beforeafter"])
            Daemon.getSessionState connection "owned" >>= (@?= Nothing) . State.sessionTerminalError
            stop,
      testCase "cancelled registration leaves buffered data and removes its writer without changing async identity" $ bounded $ do
        void $ withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \session -> do
            barrier <- outputBarrier connection "owned"
            sendOutput connection "owned" "before"
            barrier
            started <- newEmptyMVar
            held <- newEmptyMVar
            ended <- newEmptyMVar
            withAsync (Daemon.registerTerminalWriteHandler session "main" (\_ -> (putMVar started () >> takeMVar held) `finally` putMVar ended ())) $ \pending -> do
              takeMVar started
              cancel pending
              waitCatch pending >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled registration returned"
            takeMVar ended
            sendOutput connection "owned" "after"
            barrier
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "beforeafter"),
      testCase "detach cancels admitted writer IO but leaves the external terminal and future output intact" $ bounded $ do
        (_, trace) <- withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \session -> do
            barrier <- outputBarrier connection "owned"
            started <- newEmptyMVar
            held <- newEmptyMVar
            ended <- newEmptyMVar
            _ <- Daemon.registerTerminalWriteHandler session "main" (\_ -> (putMVar started () >> takeMVar held) `finally` putMVar ended ())
            sendOutput connection "owned" "blocked"
            takeMVar started
            Daemon.detachSession session
            takeMVar ended
            barrier
            sendOutput connection "owned" "after"
            barrier
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "blockedafter")
        [method | frame <- trace, let { method = field "method" frame }, method `elem` map String ["daemon.close_session", "daemon.close_terminal", "daemon.logout"]] @?= [],
      testCase "terminal output remains session-scoped even when terminal IDs match" $ bounded $ do
        void $ withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \session -> do
            barrier <- outputBarrier connection "owned"
            seen <- newIORef []
            _ <- Daemon.registerTerminalWriteHandler session "main" (\value -> modifyIORef' seen (<> [value]))
            sendOutput connection "foreign" "foreign"
            sendOutput connection "owned" "owned"
            barrier
            readIORef seen >>= (@?= ["owned"]),
      testCase "restoration retains output received after its request began" $ bounded $ do
        void $ withStatePeer HoldFirst False $ \connection lists -> do
          Daemon.addTerminal connection "owned" (State.defaultTerminalMetadata "main" State.TerminalConnected)
          barrier <- outputBarrier connection "owned"
          sendOutput connection "owned" "old"
          barrier
          withAsync (Daemon.loadTerminals connection "owned") $ \pending -> do
            void (atomically (readTQueue lists))
            sendOutput connection "owned" "new"
            barrier
            void (Daemon.getProxyToken connection)
            void (wait pending)
          Daemon.getTerminalSerializedState connection "owned" "main" >>= (@?= Just "screen-1") . fmap State.terminalSerializedText
          Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "new"),
      testCase "newer terminal restorations supersede earlier responses" $ bounded $ do
        void $ withStatePeer HoldFirst False $ \connection lists -> do
          withAsync (Daemon.loadTerminals connection "owned") $ \old -> do
            void (atomically (readTQueue lists))
            void (Daemon.loadTerminals connection "owned")
            void (Daemon.getProxyToken connection)
            waitCatch old >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Superseded terminal listing returned"
          Daemon.getTerminalSerializedState connection "owned" "main" >>= (@?= Just "screen-2") . fmap State.terminalSerializedText,
      testCase "local snapshot changes and removal win over a pending terminal restoration" $
        bounded $
          forM_ [False, True] $ \remove ->
            void $ withStatePeer HoldFirst False $ \connection lists -> do
              Daemon.addTerminal connection "owned" (State.defaultTerminalMetadata "main" State.TerminalConnected)
              withAsync (Daemon.loadTerminals connection "owned") $ \pending -> do
                void (atomically (readTQueue lists))
                if remove then Daemon.removeTerminalFromStore connection "owned" "main" else Daemon.storeTerminalState connection "owned" "main" localSnapshot
                void (Daemon.getProxyToken connection)
                void (wait pending)
              if remove
                then Daemon.getTerminals connection "owned" >>= (@?= mempty)
                else Daemon.getTerminalSerializedState connection "owned" "main" >>= (@?= Just localSnapshot),
      testCase "malformed and remote-error listings cannot substitute a successful restoration" $ bounded $ do
        void $ withStatePeer Malformed False $ \connection _ -> do
          Daemon.addTerminal connection "owned" (State.defaultTerminalMetadata "main" State.TerminalError)
          before <- Daemon.getTerminals connection "owned"
          try @RpcResultError (Daemon.loadTerminals connection "owned") >>= (@?= Left RpcInvalidResult)
          Daemon.getTerminals connection "owned" >>= (@?= before)
          failed <- try @RpcResultError (Daemon.loadTerminals connection "missing")
          case failed of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Missing remote restoration failure",
      testCase "a listing without screen state cannot erase the local snapshot or buffered output" $ bounded $ do
        void $ withStatePeer WithoutScreen False $ \connection _ -> do
          Daemon.addTerminal connection "owned" (State.defaultTerminalMetadata "main" State.TerminalConnected)
          Daemon.storeTerminalState connection "owned" "main" localSnapshot
          barrier <- outputBarrier connection "owned"
          sendOutput connection "owned" "pending"
          barrier
          void (Daemon.loadTerminals connection "owned")
          Daemon.getTerminalSerializedState connection "owned" "main" >>= (@?= Just localSnapshot)
          Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "pending"),
      testCase "failed live writers are unregistered without losing buffered output or blocking other observers" $ bounded $ do
        void $ withStatePeer Immediate True $ \connection _ ->
          Daemon.withResumedSessionOn connection "owned" $ \session -> do
            attempts <- newIORef (0 :: Int)
            barrier <- outputBarrier connection "owned"
            _ <- Daemon.registerTerminalWriteHandler session "main" (\_ -> modifyIORef' attempts (+ 1) >> throwIO SinkFailure)
            sendOutput connection "owned" "first"
            barrier
            sendOutput connection "owned" "second"
            barrier
            readIORef attempts >>= (@?= 1)
            Daemon.getTerminalBufferedData connection "owned" "main" >>= (@?= Just "firstsecond")
            Daemon.getSessionState connection "owned" >>= (@?= Just (State.TerminalWriterFailed "main")) . State.sessionTerminalError,
      testCase "session-load invalidation revokes a pending automatic terminal restoration" $ bounded $ do
        void $ withStatePeer HoldFirst True $ \connection lists ->
          withAsync (Daemon.loadSessionInfo connection "owned") $ \pending -> do
            void (atomically (readTQueue lists))
            Daemon.markSessionNotLoaded connection "owned"
            void (Daemon.getProxyToken connection)
            waitCatch pending >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Invalidated session load returned"
            Daemon.getTerminals connection "owned" >>= (@?= mempty)
            Daemon.getSessionReadiness connection "owned" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase
    ]

data SinkFailure = SinkFailure deriving stock (Eq, Show)

instance Exception SinkFailure

data ListMode = Immediate | HoldFirst | Malformed | WithoutScreen deriving stock (Eq)

withStatePeer :: ListMode -> Bool -> (Daemon.DaemonConnection -> TQueue Object -> IO a) -> IO (a, [Object])
withStatePeer mode automatic action = do
  trace <- newIORef []
  listed <- newTQueueIO
  count <- newIORef (0 :: Int)
  held <- newIORef Nothing
  result <- withPeer (\_ connection -> serve trace listed count held connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target -> do
    let defaults = Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD"
        options = defaults {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}, Daemon.daemonRestoreTerminalsOnLoad = automatic}
    Daemon.daemonRestoreTerminalsOnLoad defaults @?= True
    Daemon.withConnection options (`action` listed)
  frames <- readIORef trace
  pure (result, frames)
  where
    serve trace listed count held connection = forever $ do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid terminal RPC")) pure . eitherDecode
      modifyIORef' trace (<> [frame])
      let params = asObject (field "params" frame)
      case field "method" frame of
        String "daemon.authenticate" -> reply connection frame (object ["userId" .= String "user", "orgId" .= String "org"])
        String "daemon.load_session" -> reply connection frame (object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]])
        String "daemon.list_terminals" -> do
          index <- atomicModifyIORef' count (\n -> (n + 1, n + 1))
          atomically (writeTQueue listed frame)
          let info = case mode of Malformed -> object []; WithoutScreen -> Object (KeyMap.delete "state" (asObject (infoValue index))); _ -> infoValue index
              result = object ["terminals" .= [info]]
          if field "sessionId" params == String "missing"
            then sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcEntityNotFound, "message" .= String "Missing terminal scope"]]
            else if mode == HoldFirst && index == 1 then modifyIORef' held (const (Just (frame, result))) else reply connection frame result
        String "daemon.get_proxy_token" -> do
          pending <- atomicModifyIORef' held (Nothing,)
          maybe (pure ()) (uncurry (reply connection)) pending
          reply connection frame (object ["token" .= String "OFFLINE_TOKEN"])
        String "daemon.write_terminal_data" -> do
          notify connection (field "sessionId" params) (object ["type" .= String "daemon.terminal_data", "terminalId" .= field "terminalId" params, "data" .= field "data" params])
          reply connection frame (object ["success" .= True])
        _ -> assertFailure "Unexpected terminal-state RPC, including implicit mutation"

outputBarrier :: Daemon.DaemonConnection -> Text -> IO (IO ())
outputBarrier connection identifier = do
  queue <- newTQueueIO
  _ <- Daemon.onTerminalEvent connection identifier $ \case Right (TerminalDataEvent _) -> atomically (writeTQueue queue ()); _ -> pure ()
  pure (atomically (readTQueue queue))

sendOutput :: Daemon.DaemonConnection -> Text -> Text -> IO ()
sendOutput connection identifier value = void (Daemon.writeTerminalData connection identifier (WriteTerminalDataParams "main" value mempty))

localSnapshot :: State.TerminalSerializedState
localSnapshot = State.TerminalSerializedState "local" 1.25 2.5 (State.TerminalEpochMilliseconds 0) (Just False)

infoValue :: Int -> Value
infoValue index = object ["id" .= String "main", "pid" .= Null, "cols" .= Number 80.25, "rows" .= Number 24.125, "createdAt" .= String "2026-09-10T00:00:00Z", "state" .= object ["serialized" .= ("screen-" <> Text.pack (show index)), "plainText" .= String "plain", "cols" .= Number 80.25, "rows" .= Number 24.125, "timestamp" .= String "2026-09-10T00:00:01.123456789Z", "cursorHidden" .= False]]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = mempty

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

notify :: WS.Connection -> Value -> Value -> IO ()
notify connection identifier value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= value]]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))
