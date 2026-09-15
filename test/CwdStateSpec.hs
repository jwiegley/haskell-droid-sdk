{-# LANGUAGE OverloadedStrings #-}

module CwdStateSpec (cwdStateTests) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidInvalidEvent), DroidEvent (WorkingDirectoryEvent))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Control (ChangeWorkingDirectoryResult (..))
import Factory.Droid.Schema.Daemon.Workspace (ChangeSessionWorkingDirectoryParams (..))
import Factory.Droid.Schema.Notifications (ChildSessionAvailable, SessionWorkingDirectoryChanged (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcInvalidParams))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Directory (getCurrentDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

cwdStateTests :: TestTree
cwdStateTests =
  testGroup
    "Authoritative cwd state"
    [ testCase "inheritance remains provisional and cannot replace reported empty, absent or invalid state" $ do
        let parent = State.WorkingDirectoryReported (Just "/parent")
            inherited = State.inheritWorkingDirectory parent State.emptySessionState
        State.sessionWorkingDirectory inherited @?= State.WorkingDirectoryInherited "/parent"
        forM_ [Nothing, Just ""] $ \path -> do
          let reported = State.observeWorkingDirectory path State.emptySessionState
          State.inheritWorkingDirectory parent reported @?= reported
        let invalid = State.invalidateWorkingDirectory inherited
        State.sessionWorkingDirectory invalid @?= State.WorkingDirectoryInvalid (Just "/parent")
        State.inheritWorkingDirectory parent invalid @?= invalid
        State.sessionWorkingDirectory (State.observeWorkingDirectory (Just "") invalid) @?= State.WorkingDirectoryReported (Just "")
        State.inheritWorkingDirectory (State.WorkingDirectoryReported (Just "")) State.emptySessionState @?= State.emptySessionState,
      testCase "initialization uses requested cwd or reported worktree path without touching the process directory" $ bounded $ do
        before <- getCurrentDirectory
        forM_ [Nothing, Just "/tree", Just ""] $ \tree -> do
          let extra = maybe mempty (\path -> KeyMap.singleton "worktree" (object ["path" .= path, "branch" .= String "branch", "repoRoot" .= String "/root", "isNewlyCreated" .= False])) tree
          void $ withCwdPeer False [extra] [] $ \target _ _ ->
            Daemon.withSession (options target) $ \session -> do
              let expected = Just (fromMaybe "/requested" tree)
              Daemon.getWorkingDirectory (Daemon.sessionConnection session) (Daemon.sessionId session) >>= (@?= expected)
              Daemon.daemonInitialWorkingDirectory (Daemon.sessionInfo session) @?= expected
        getCurrentDirectory >>= (@?= before),
      testCase "loaded cwd, empty cwd, legacy path and absence retain existing receipt precedence" $
        bounded $
          forM_ [(KeyMap.singleton "cwd" (String "/reported"), Just "/reported"), (KeyMap.singleton "cwd" (String ""), Just ""), (KeyMap.singleton "worktree" (object ["path" .= String "/legacy"]), Just "/legacy"), (mempty, Nothing)] $ \(extra, expected) ->
            void $ withCwdPeer False [extra] [] $ \target _ _ ->
              Daemon.withConnection (options target) $ \connection -> do
                void (Daemon.loadSessionInfo connection "owned")
                Daemon.getWorkingDirectory connection "owned" >>= (@?= expected)
                Daemon.getSessionState connection "owned" >>= (@?= State.WorkingDirectoryReported expected) . State.sessionWorkingDirectory,
      testCase "explicit change publishes the resolved path, preserves extension values and leaves immutable receipts alone" $ bounded $ do
        before <- getCurrentDirectory
        (_, trace) <- withCwdPeer False [cwd "/initial"] [] $ \target _ _ ->
          Daemon.withResumedSession (options target) "owned" $ \session -> do
            let connection = Daemon.sessionConnection session
            result <- Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "false-extra" (KeyMap.singleton "sessionId" (String "foreign")))
            changedResolvedPath result @?= "/resolved"
            changedDirectoryAdditionalFields result @?= KeyMap.singleton "success" (Bool False)
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/resolved")
            Daemon.daemonInitialWorkingDirectory (Daemon.sessionInfo session) @?= Just "/initial"
            void (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "empty" mempty))
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "")
        let changes = [asObject (field "params" frame) | frame <- trace, field "method" frame == String "daemon.change_working_directory"]
        changes @?= [KeyMap.fromList ["sessionId" .= String "owned", "workingDirectory" .= String "false-extra"], KeyMap.fromList ["sessionId" .= String "owned", "workingDirectory" .= String "empty"]]
        getCurrentDirectory >>= (@?= before),
      testCase "notifications publish before callbacks, isolate sessions and expose malformed cwd until repaired" $ bounded $ do
        let batches = [[("foreign", cwdEvent (String "/foreign")), ("owned", cwdEvent (String ""))], [("owned", cwdEvent (Bool False))], [("owned", cwdEvent (String "/repaired"))]]
        void $ withCwdPeer False [cwd "/initial"] batches $ \target _ _ ->
          Daemon.withResumedSession (options target) "owned" $ \session -> do
            let connection = Daemon.sessionConnection session
            observed <- newTQueueIO
            _ <- Daemon.onSessionEvent session $ \case
              Right (WorkingDirectoryEvent _) -> try @DroidError (Daemon.getWorkingDirectory connection "owned") >>= atomically . writeTQueue observed
              Left _ -> try @DroidError (Daemon.getWorkingDirectory connection "owned") >>= atomically . writeTQueue observed
              _ -> pure ()
            forM_ [Right (Just ""), Left DroidInvalidEvent, Right (Just "/repaired")] $ \expected -> do
              void (Daemon.getProxyToken connection)
              atomically (readTQueue observed) >>= (@?= expected)
            Daemon.getWorkingDirectory connection "foreign" >>= (@?= Just "/foreign"),
      testCase "event and change-result authority follows wire order rather than IO waiter order" $ bounded $ do
        void $ withCwdPeer False [cwd "/initial"] [] $ \target _ _ ->
          Daemon.withConnection (options target) $ \connection -> do
            void (Daemon.loadSessionInfo connection "owned")
            void (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "pre" mempty))
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/resolved")
            void (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "post" mempty))
            waitCwd connection "owned" (State.WorkingDirectoryReported (Just "/event-after")),
      testCase "malformed and rejected changes preserve prior observed cwd" $ bounded $ do
        void $ withCwdPeer False [cwd "/initial"] [] $ \target _ _ ->
          Daemon.withConnection (options target) $ \connection -> do
            void (Daemon.loadSessionInfo connection "owned")
            try @RpcResultError (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "bad" mempty)) >>= (@?= Left RpcInvalidResult)
            failed <- try @RpcResultError (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "error" mempty))
            case failed of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Missing cwd rejection"
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/initial"),
      testCase "cancelled changes preserve async identity while a later authoritative notification is still accepted" $ bounded $ do
        void $ withCwdPeer False [cwd "/initial"] [[], [("owned", cwdEvent (String "/confirmed"))]] $ \target _ changes ->
          Daemon.withConnection (options target) $ \connection -> do
            void (Daemon.loadSessionInfo connection "owned")
            withAsync (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "owned" "held" mempty)) $ \pending -> do
              void (atomically (readTQueue changes))
              cancel pending
              waitCatch pending >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled cwd change returned"
            void (Daemon.getProxyToken connection)
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/initial")
            void (Daemon.getProxyToken connection)
            waitCwd connection "owned" (State.WorkingDirectoryReported (Just "/confirmed")),
      testCase "superseded load receipts cannot overwrite newer cwd" $ bounded $ do
        void $ withCwdPeer True [cwd "/old", cwd "/new"] [[]] $ \target loads _ ->
          Daemon.withConnection (options target) $ \connection ->
            withAsync (Daemon.loadSessionInfo connection "owned") $ \old -> do
              void (atomically (readTQueue loads))
              void (Daemon.loadSessionInfo connection "owned")
              void (Daemon.getProxyToken connection)
              waitCatch old >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Superseded cwd load returned"
              Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/new"),
      testCase "malformed load cwd never publishes and a valid load can repair a failed observation" $ bounded $ do
        void $ withCwdPeer False [cwd "/initial", KeyMap.singleton "cwd" Null, cwd "/repaired"] [[("owned", cwdEvent Null)]] $ \target _ _ ->
          Daemon.withConnection (options target) $ \connection -> do
            void (Daemon.loadSessionInfo connection "owned")
            try @DroidError (Daemon.loadSessionInfo connection "owned") >>= \case Left DroidInvalidEvent -> pure (); _ -> assertFailure "Malformed load cwd accepted"
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/initial")
            void (Daemon.getProxyToken connection)
            waitCwd connection "owned" (State.WorkingDirectoryInvalid (Just "/initial"))
            void (Daemon.loadSessionInfo connection "owned")
            Daemon.getWorkingDirectory connection "owned" >>= (@?= Just "/repaired"),
      testCase "child discovery seeds only unobserved cwd and accepted child loads replace provisional values" $ bounded $ do
        void $ withCwdPeer False [cwd "/parent", cwd "/child", mempty] [] $ \target _ _ ->
          Daemon.withConnection (options target) $ \connection -> do
            void (Daemon.loadSessionInfo connection "parent")
            available <- decodeValue @ChildSessionAvailable (object ["type" .= String "child_session_available", "childSessionId" .= String "child", "timestamp" .= Number 0])
            void (Daemon.registerChildSession connection "parent" available)
            Daemon.getSessionState connection "child" >>= (@?= State.WorkingDirectoryInherited "/parent") . State.sessionWorkingDirectory
            void (Daemon.ensureChildSessionAttached connection "child")
            Daemon.getWorkingDirectory connection "child" >>= (@?= Just "/child")
            void (Daemon.loadSessionInfo connection "child")
            void (Daemon.registerChildSession connection "parent" available)
            Daemon.getWorkingDirectory connection "child" >>= (@?= Nothing),
      testCase "the pure event reducer also records cwd events and repairs invalid observations" $ do
        let before = State.invalidateWorkingDirectory (State.observeWorkingDirectory (Just "/before") State.emptySessionState)
            after = State.applySessionEventAt 0 0 (WorkingDirectoryEvent (SessionWorkingDirectoryChanged "" mempty)) before
        State.sessionWorkingDirectory after @?= State.WorkingDirectoryReported (Just "")
    ]

cwd :: Text -> Object
cwd path = KeyMap.singleton "cwd" (String path)

cwdEvent :: Value -> Value
cwdEvent value = object ["type" .= String "session_working_directory_changed", "cwd" .= value]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "/requested") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withCwdPeer :: Bool -> [Object] -> [[(Text, Value)]] -> (WebSocket.WebSocketTarget -> TQueue Object -> TQueue Object -> IO a) -> IO (a, [Object])
withCwdPeer holdFirst extras batches action = do
  replies <- newIORef extras
  notifications <- newIORef batches
  pending <- newIORef []
  trace <- newIORef []
  loads <- newTQueueIO
  changes <- newTQueueIO
  index <- newIORef (0 :: Int)
  result <- withPeer (\_ connection -> serve replies notifications pending trace loads changes index connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target -> action target loads changes
  frames <- readIORef trace
  pure (result, frames)
  where
    serve replies notifications pending trace loads changes index connection = forever $ do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid cwd peer RPC")) pure . eitherDecode
      modifyIORef' trace (<> [frame])
      let params = asObject (field "params" frame)
      case field "method" frame of
        String "daemon.authenticate" -> reply connection frame (object ["userId" .= String "user", "orgId" .= String "org"])
        String method | method `elem` ["daemon.load_session", "daemon.initialize_session"] -> do
          extra <- atomicModifyIORef' replies (\case next : rest -> (rest, Just next); [] -> ([], Nothing)) >>= maybe (assertFailure "Cwd load fixture exhausted") pure
          number <- atomicModifyIORef' index (\n -> (n + 1, n + 1))
          atomically (writeTQueue loads frame)
          let base = KeyMap.fromList ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]]
              body = Object (KeyMap.union extra (if method == "daemon.initialize_session" then KeyMap.insert "sessionId" (field "sessionId" params) base else base))
          if holdFirst && number == 1 then modifyIORef' pending (<> [(frame, body)]) else reply connection frame body
        String "daemon.list_terminals" -> reply connection frame (object ["terminals" .= ([] :: [Value])])
        String "daemon.change_working_directory" -> do
          atomically (writeTQueue changes frame)
          let path = field "workingDirectory" params
              identifier = field "sessionId" params
              result = object (["resolvedPath" .= (if path == String "empty" then String "" else String "/resolved")] <> ["success" .= False | path == String "false-extra"])
          case path of
            String "bad" -> reply connection frame (object ["resolvedPath" .= False])
            String "error" -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Rejected cwd"]]
            String "held" -> modifyIORef' pending (<> [(frame, result)])
            _ -> do
              when (path == String "pre") (notify connection identifier (cwdEvent (String "/event-before")))
              reply connection frame result
              when (path == String "post") (notify connection identifier (cwdEvent (String "/event-after")))
        String "daemon.get_proxy_token" -> do
          held <- atomicModifyIORef' pending ([],)
          forM_ held (uncurry (reply connection))
          batch <- atomicModifyIORef' notifications (\case next : rest -> (rest, Just next); [] -> ([], Nothing)) >>= maybe (assertFailure "Cwd event fixture exhausted") pure
          forM_ batch $ \(identifier, value) -> notify connection (String identifier) value
          reply connection frame (object ["token" .= String "OFFLINE_TOKEN"])
        _ -> assertFailure "Unexpected cwd request, including implicit trust, validation or lifecycle mutation"

waitCwd :: Daemon.DaemonConnection -> Text -> State.WorkingDirectoryState -> IO ()
waitCwd connection identifier expected = do
  state <- Daemon.getSessionState connection identifier
  if State.sessionWorkingDirectory state == expected then pure () else Daemon.waitSessionStateChange connection identifier state >> waitCwd connection identifier expected

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

asObject :: Value -> Object
asObject (Object fields) = fields
asObject _ = mempty

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

notify :: WS.Connection -> Value -> Value -> IO ()
notify connection identifier value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= value]]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
