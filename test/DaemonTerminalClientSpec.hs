{-# LANGUAGE OverloadedStrings #-}

module DaemonTerminalClientSpec (terminalClientTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Exception (catch, finally, fromException, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Daemon.Terminal
import Factory.Droid.Schema.Primitives (rfc3339TimestampText)
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcInvalidParams), SuccessResult (..))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

terminalClientTests :: TestTree
terminalClientTests = testGroup "Daemon terminal operations" terminalCases

terminalCases :: [TestTree]
terminalCases =
  [ testCase "five operations preserve session routing, false results, input bytes and screen state" $ bounded $ do
      trace <- newIORef []
      withTerminalPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        events <- newEmptyMVar
        _ <- Daemon.onTerminalEvent connection "owned" (putMVar events)
        let create = (defaultCreateTerminalParams "main") {createdTerminalCols = Just 80.5, createdTerminalRows = Just 24.25, createdTerminalCwd = Just "/caller/remote", createdTerminalEnv = Just (KeyMap.singleton "KEY" "OFFLINE_VALUE"), createTerminalAdditionalFields = KeyMap.singleton "sessionId" (String "injected")}
        Daemon.createTerminal connection "owned" create >>= (@?= TerminalCreated mempty)
        takeMVar events >>= (@?= Right (TerminalDataEvent (TerminalData "main" "boot\r\n" mempty)))
        Daemon.createTerminal connection "owned" (defaultCreateTerminalParams "exists") >>= (@?= TerminalAlreadyExists mempty)
        Daemon.writeTerminalData connection "owned" (WriteTerminalDataParams "main" "line\r\n\ESC[31m" (KeyMap.singleton "sessionId" (String "injected"))) >>= (@?= True) . resultSuccess
        takeMVar events >>= (@?= Right (TerminalDataEvent (TerminalData "main" "line\r\n\ESC[31m" mempty)))
        Daemon.resizeTerminal connection "owned" (ResizeTerminalParams "main" 100.5 40.25 (KeyMap.singleton "sessionId" (String "injected"))) >>= (@?= False) . resultSuccess
        listed <- Daemon.listTerminals connection "owned"
        case listedTerminals listed of
          [info] -> do
            terminalInfoId info @?= "main"
            terminalInfoPid info @?= Nothing
            rfc3339TimestampText (terminalInfoCreatedAt info) @?= "2026-09-08T00:00:00Z"
            case terminalInfoState info of
              Just screen -> do
                screenSerialized screen @?= "\ESC[31mrestored"
                screenPlainText screen @?= "restored"
                screenCursorHidden screen @?= Just False
              Nothing -> assertFailure "Missing saved terminal state"
          _ -> assertFailure "Missing terminal listing"
        Daemon.closeTerminal connection "owned" (CloseTerminalParams "main" (KeyMap.singleton "sessionId" (String "injected"))) >>= (@?= False) . resultSuccess
        takeMVar events >>= (@?= Right (TerminalExitEvent (TerminalExit "main" 0 "" mempty)))
      frames <- readIORef trace
      map (field "method") frames @?= map String ["daemon.authenticate", "daemon.create_terminal", "daemon.create_terminal", "daemon.write_terminal_data", "daemon.resize_terminal", "daemon.list_terminals", "daemon.close_terminal"]
      map parameters (drop 1 frames) @?= [KeyMap.fromList ["sessionId" .= String "owned", "terminalId" .= String "main", "cols" .= (80.5 :: Double), "rows" .= (24.25 :: Double), "cwd" .= String "/caller/remote", "env" .= object ["KEY" .= String "OFFLINE_VALUE"]], KeyMap.fromList ["sessionId" .= String "owned", "terminalId" .= String "exists"], KeyMap.fromList ["sessionId" .= String "owned", "terminalId" .= String "main", "data" .= String "line\r\n\ESC[31m"], KeyMap.fromList ["sessionId" .= String "owned", "terminalId" .= String "main", "cols" .= (100.5 :: Double), "rows" .= (40.25 :: Double)], KeyMap.singleton "sessionId" (String "owned"), KeyMap.fromList ["sessionId" .= String "owned", "terminalId" .= String "main"]],
    testCase "terminal timestamp strings preserve offsets, precision and leap-second spelling" $
      bounded $
        forM_ ["2026-09-08t00:00:00.123456789+05:30", "2000-01-01T00:00:00-00:00", "1998-12-31T15:59:60.123-08:00"] $ \spelling -> do
          trace <- newIORef []
          let info = infoValue (String spelling) (String spelling)
          withTerminalPeerInfo trace info $ \target -> Daemon.withConnection (options target) $ \connection -> do
            listed <- Daemon.listTerminals connection "owned"
            toJSON listed @?= object ["terminals" .= [info]],
    testCase "terminal Date-coercion inputs fail without replacing retained state or poisoning the connection" $
      bounded $
        forM_ [Null, Bool False, Bool True, Number 0, Number (-1), Number 1.9, object [], toJSON [0 :: Int], String "", String "2020-02-31T00:00:00Z", String "2000-01-01", String "01/02/2000", String "2000-01-01T00:00:00"] $ \invalid ->
          forM_ [(invalid, String "2000-01-01T00:00:00Z"), (String "2000-01-01T00:00:00Z", invalid)] $ \(created, screen) -> do
            trace <- newIORef []
            withTerminalPeerInfo trace (infoValue created screen) $ \target -> Daemon.withConnection (options target) $ \connection -> do
              Daemon.addTerminal connection "owned" (State.defaultTerminalMetadata "retained" State.TerminalError)
              before <- Daemon.getTerminals connection "owned"
              try @RpcResultError (Daemon.listTerminals connection "owned") >>= (@?= Left RpcInvalidResult)
              try @RpcResultError (Daemon.loadTerminals connection "owned") >>= (@?= Left RpcInvalidResult)
              Daemon.getTerminals connection "owned" >>= (@?= before)
              Daemon.resizeTerminal connection "owned" (ResizeTerminalParams "main" 80 24 mempty) >>= (@?= False) . resultSuccess,
    testCase "terminal observers ignore foreign and unrelated payloads before terminal validation" $ bounded $ do
      trace <- newIORef []
      withTerminalPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        seen <- newIORef []
        finished <- newEmptyMVar
        _ <- Daemon.onTerminalEvent connection "owned" $ \event -> do
          modifyIORef' seen (<> [event])
          case event of Right (TerminalExitEvent _) -> putMVar finished (); _ -> pure ()
        Daemon.writeTerminalData connection "owned" (WriteTerminalDataParams "main" "events" mempty) >>= (@?= False) . resultSuccess
        takeMVar finished
        readIORef seen >>= (@?= [Right (TerminalDataEvent (TerminalData "other-terminal" "other" mempty)), Left Daemon.InvalidDaemonEvent, Right (TerminalExitEvent (TerminalExit "main" (-1.25) "signal" mempty))])
        Daemon.listTerminals connection "owned" >>= (@?= 1) . length . listedTerminals,
    testCase "terminal RPC failures and malformed restoration data remain explicit" $ bounded $ do
      trace <- newIORef []
      withTerminalPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        rejected <- try @RpcResultError (Daemon.writeTerminalData connection "owned" (WriteTerminalDataParams "main" "error" mempty))
        case rejected of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Missing terminal RPC error"
        bad <- try @RpcResultError (Daemon.listTerminals connection "bad-list")
        bad @?= Left RpcInvalidResult
        malformed <- try @RpcResultError (Daemon.createTerminal connection "owned" (defaultCreateTerminalParams "bad-create"))
        malformed @?= Left RpcInvalidResult
        Daemon.listTerminals connection "owned" >>= (@?= 1) . length . listedTerminals,
    testCase "terminal subscription cleanup finalizes an admitted callback without closing the remote terminal" $ bounded $ do
      trace <- newIORef []
      started <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      withTerminalPeer trace $ \target -> do
        Daemon.withConnection (options target) $ \connection -> do
          _ <- Daemon.onTerminalEvent connection "owned" (\_ -> (putMVar started () >> takeMVar hold) `finally` putMVar finished ())
          void (Daemon.createTerminal connection "owned" (defaultCreateTerminalParams "main"))
          takeMVar started
        takeMVar finished
      readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.create_terminal"]) . map (field "method"),
    testCase "cancelled writes keep async identity and do not implicitly close terminals" $ bounded $ do
      trace <- newIORef []
      withTerminalPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        ready <- newEmptyMVar
        _ <- Daemon.onTerminalEvent connection "owned" (const (putMVar ready ()))
        withAsync (Daemon.writeTerminalData connection "owned" (WriteTerminalDataParams "main" "held" mempty)) $ \pending -> do
          takeMVar ready
          cancel pending
          waitCatch pending >>= \case Left err -> fromException err @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled terminal write returned"
        Daemon.listTerminals connection "owned" >>= (@?= 1) . length . listedTerminals
      readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.write_terminal_data", "daemon.list_terminals"]) . map (field "method"),
    testCase "terminal unsubscribe is idempotent and ordinary callback failures are isolated" $ bounded $ do
      trace <- newIORef []
      withTerminalPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        seen <- newIORef (0 :: Int)
        barrier <- newEmptyMVar
        _ <- Daemon.onTerminalEvent connection "owned" (\_ -> ioError (userError "ordinary callback error"))
        stop <- Daemon.onTerminalEvent connection "owned" (\_ -> modifyIORef' seen (+ 1))
        _ <- Daemon.onTerminalEvent connection "owned" (const (putMVar barrier ()))
        void (Daemon.writeTerminalData connection "owned" (WriteTerminalDataParams "main" "first" mempty))
        takeMVar barrier
        readIORef seen >>= (@?= 1)
        stop
        stop
        void (Daemon.writeTerminalData connection "owned" (WriteTerminalDataParams "main" "second" mempty))
        takeMVar barrier
        readIORef seen >>= (@?= 1)
  ]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withTerminalPeer :: IORef [Object] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withTerminalPeer trace = withTerminalPeerInfo trace (infoValue (String "2026-09-08T00:00:00Z") (String "2026-09-08T00:01:00Z"))

withTerminalPeerInfo :: IORef [Object] -> Value -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withTerminalPeerInfo trace info = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid client RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      KeyMap.lookup "_meta" frame @?= Nothing
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "apiKey" (parameters auth) @?= String "OFFLINE_ONLY"
      field "caller" (parameters auth) @?= String "haskell-sdk"
      respond connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      forever $ do
        frame <- readFrame connection
        let params = parameters frame
            ident = field "sessionId" params
            terminal = field "terminalId" params
        case field "method" frame of
          String "daemon.create_terminal" -> case terminal of
            String "exists" -> respond connection frame (object ["success" .= False, "error" .= String "TerminalIdExists"])
            String "bad-create" -> respond connection frame (object ["success" .= False, "error" .= String "other"])
            _ -> do
              notify connection ident (object ["type" .= String "daemon.terminal_data", "terminalId" .= terminal, "data" .= String "boot\r\n"])
              respond connection frame (object ["success" .= True])
          String "daemon.write_terminal_data" -> case field "data" params of
            String "error" -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "rejected"]]
            String "held" -> notify connection ident (object ["type" .= String "daemon.terminal_data", "terminalId" .= terminal, "data" .= String "ready"])
            String "events" -> do
              notify connection (String "foreign") (Bool False)
              notify connection ident (object ["type" .= String "settings_updated"])
              notify connection ident (object ["type" .= String "daemon.terminal_data", "terminalId" .= String "other-terminal", "data" .= String "other"])
              notify connection ident (object ["type" .= String "daemon.terminal_data", "terminalId" .= terminal, "data" .= False])
              notify connection ident (object ["type" .= String "daemon.terminal_exit", "terminalId" .= terminal, "exitCode" .= (-1.25 :: Double), "signal" .= String "signal"])
              respond connection frame (object ["success" .= False])
            _ -> do
              notify connection ident (object ["type" .= String "daemon.terminal_data", "terminalId" .= terminal, "data" .= field "data" params])
              respond connection frame (object ["success" .= True])
          String "daemon.resize_terminal" -> respond connection frame (object ["success" .= False])
          String "daemon.close_terminal" -> do
            notify connection ident (object ["type" .= String "daemon.terminal_exit", "terminalId" .= terminal, "exitCode" .= (0 :: Int), "signal" .= String ""])
            respond connection frame (object ["success" .= False])
          String "daemon.list_terminals" -> respond connection frame (object ["terminals" .= [if ident == String "bad-list" then object [] else info]])
          _ -> assertFailure "Unexpected method or implicit session lifecycle call"

respond :: WS.Connection -> Object -> Value -> IO ()
respond connection request result = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= result]

notify :: WS.Connection -> Value -> Value -> IO ()
notify connection ident payload = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= ident, "notification" .= payload]]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

parameters :: Object -> Object
parameters value = case field "params" value of Object result -> result; _ -> mempty

infoValue :: Value -> Value -> Value
infoValue created timestamp = object ["id" .= String "main", "pid" .= Null, "cols" .= (100.5 :: Double), "rows" .= (40.25 :: Double), "createdAt" .= created, "state" .= object ["serialized" .= String "\ESC[31mrestored", "plainText" .= String "restored", "cols" .= (100.5 :: Double), "rows" .= (40.25 :: Double), "timestamp" .= timestamp, "cursorHidden" .= False]]
