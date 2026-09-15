{-# LANGUAGE OverloadedStrings #-}

module DaemonLoadCoordinationSpec (loadCoordinationTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, yield)
import Control.Concurrent.Async (Async, AsyncCancelled (..), asyncThreadId, cancel, wait, waitCatch, withAsync)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidInvalidEvent), DroidEvent (WorkingStateEvent))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (RpcChannelClosed), RpcResultError (..))
import Factory.Droid.Schema.Daemon.Management (ProxyTokenResult (proxyToken))
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))
import Factory.Droid.Schema.Mission (MissionPhase (MissionRunning), MissionSnapshot (missionSnapshotState))
import Factory.Droid.Schema.Notifications (DroidWorkingState (..), DroidWorkingStateChanged (workingStateNewState))
import Factory.Droid.Schema.RPC (JsonRpcError (rpcErrorCode), JsonRpcErrorCode (RpcEntityNotFound))
import Factory.Droid.Schema.Settings (SessionSettings (settingsModel))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import GHC.Conc (BlockReason (BlockedOnSTM), ThreadStatus (ThreadBlocked, ThreadDied, ThreadFinished), threadStatus)
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

waitForSTM :: Async a -> IO ()
waitForSTM pending =
  threadStatus (asyncThreadId pending) >>= \case
    ThreadBlocked BlockedOnSTM -> pure ()
    ThreadFinished -> assertFailure "Readiness waiter returned before release"
    ThreadDied -> assertFailure "Readiness waiter died before release"
    _ -> yield >> waitForSTM pending

loadCoordinationTests :: TestTree
loadCoordinationTests =
  testGroup
    "Daemon load coordination"
    [ testCase "unknown and pre-init-only state do not cause speculative loads; readiness waits expire with ownership" $ bounded $ do
        (escaped, trace) <- withLoadPeer $ \connection _ -> do
          initial <- Daemon.getSessionReadiness connection "unknown"
          Daemon.readinessKnown initial @?= False
          Daemon.readinessPhase initial @?= Daemon.SessionNotLoaded
          Daemon.sessionReadinessBusy initial @?= Right False
          Daemon.ensureSessionLoaded connection "unknown"
          Daemon.setSessionPreInit connection "unknown" True
          marked <- Daemon.getSessionReadiness connection "unknown"
          Daemon.readinessPreInit marked @?= True
          Daemon.readinessKnown marked @?= False
          Daemon.ensureSessionLoaded connection "unknown"
          Daemon.clearSessionNotFound connection "unknown"
          Daemon.setSessionPreInit connection "unknown" False
          pure (Daemon.waitSessionReadinessChange connection "unknown" initial)
        try @RpcChannelError escaped >>= (@?= Left RpcChannelClosed)
        methods trace @?= ["daemon.authenticate"],
      testCase "readiness waiters join one in-flight load and a cancelled waiter does not cancel its owner" $ bounded $ do
        (_, trace) <- withLoadPeer $ \connection fixture ->
          withAsync (Daemon.loadSessionInfo connection "holding") $ \owner -> do
            takeMVar (loadStarted fixture)
            loading <- Daemon.getSessionReadiness connection "holding"
            Daemon.readinessPhase loading @?= Daemon.SessionLoading
            Daemon.readinessLoading loading @?= True
            withAsync (Daemon.ensureSessionLoaded connection "holding") $ \cancelled ->
              withAsync (Daemon.ensureSessionLoaded connection "holding") $ \follower -> do
                waitForSTM cancelled
                waitForSTM follower
                cancel cancelled
                waitCatch cancelled >>= \case
                  Left cause -> fromException cause @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled readiness wait completed"
                Daemon.getProxyToken connection >>= (@?= "OFFLINE_TOKEN") . proxyToken
                wait owner >>= infoModel >>= (@?= "holding")
                wait follower
            current <- Daemon.getSessionReadiness connection "holding"
            Daemon.readinessPhase current @?= Daemon.SessionLoaded
            Daemon.readinessLoading current @?= False
            Daemon.ensureSessionLoaded connection "holding"
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.get_proxy_token", "daemon.list_terminals"],
      testCase "a cancelled load keeps the owner's async exception but releases joined readiness callers with a typed interruption" $ bounded $ do
        void $ withLoadPeer $ \connection fixture ->
          withAsync (Daemon.loadSessionInfo connection "holding") $ \owner -> do
            takeMVar (loadStarted fixture)
            withAsync (try @Daemon.DaemonError (Daemon.ensureSessionLoaded connection "holding")) $ \follower -> do
              waitForSTM follower
              cancel owner
              waitCatch owner >>= \case
                Left cause -> fromException cause @?= Just AsyncCancelled
                Right _ -> assertFailure "Cancelled load returned"
              wait follower >>= (@?= Left Daemon.DaemonLoadInterrupted)
            state <- Daemon.getSessionReadiness connection "holding"
            Daemon.readinessPhase state @?= Daemon.SessionNotLoaded
            Daemon.readinessLoading state @?= False
            void (Daemon.getProxyToken connection)
            Daemon.ensureSessionLoaded connection "holding",
      testCase "new explicit loads supersede older receipts without overwriting settings, mission state or readiness" $ bounded $ do
        void $ withLoadPeer $ \connection fixture ->
          Daemon.withResumedSessionOn connection "shared" $ \session ->
            withAsync (try @Daemon.DaemonError (Daemon.loadSessionInfo connection "shared")) $ \old -> do
              takeMVar (loadStarted fixture)
              Daemon.loadSessionInfo connection "shared" >>= infoModel >>= (@?= "newest")
              wait old >>= \case
                Left Daemon.DaemonLoadSuperseded -> pure ()
                _ -> assertFailure "Superseded load succeeded"
              Daemon.getSettings session >>= (@?= "newest") . settingsModel
              Daemon.getMissionSnapshot session >>= (@?= Just MissionRunning) . fmap missionSnapshotState
              readiness <- Daemon.getSessionReadiness connection "shared"
              Daemon.readinessPhase readiness @?= Daemon.SessionLoaded
              Daemon.readinessLoading readiness @?= False,
      testCase "explicit invalidation revokes an in-flight receipt and keeps the session eligible for a fresh ensure" $ bounded $ do
        (_, trace) <- withLoadPeer $ \connection fixture ->
          withAsync (try @Daemon.DaemonError (Daemon.loadSessionInfo connection "holding")) $ \old -> do
            takeMVar (loadStarted fixture)
            Daemon.markSessionNotLoaded connection "holding"
            void (Daemon.getProxyToken connection)
            wait old >>= (@?= Left Daemon.DaemonLoadSuperseded)
            invalidated <- Daemon.getSessionReadiness connection "holding"
            Daemon.readinessPhase invalidated @?= Daemon.SessionNotLoaded
            Daemon.readinessKnown invalidated @?= True
            Daemon.ensureSessionLoaded connection "holding"
            Daemon.getSessionReadiness connection "holding" >>= (@?= Daemon.SessionLoaded) . Daemon.readinessPhase
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.get_proxy_token", "daemon.load_session", "daemon.list_terminals"],
      testCase "not-found is distinct from an ordinary failure and explicit retry clears it" $ bounded $ do
        (_, trace) <- withLoadPeer $ \connection _ -> do
          try @RpcResultError (Daemon.loadSessionInfo connection "missing") >>= \case
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcEntityNotFound
            _ -> assertFailure "Lost original not-found error"
          missing <- Daemon.getSessionReadiness connection "missing"
          Daemon.readinessNotFound missing @?= True
          Daemon.readinessKnown missing @?= False
          Daemon.ensureSessionLoaded connection "missing"
          Daemon.clearSessionNotFound connection "missing"
          Daemon.getSessionReadiness connection "missing" >>= (@?= False) . Daemon.readinessNotFound
          Daemon.loadSessionInfo connection "missing" >>= infoModel >>= (@?= "missing")
          recovered <- Daemon.getSessionReadiness connection "missing"
          Daemon.readinessPhase recovered @?= Daemon.SessionLoaded
          Daemon.readinessNotFound recovered @?= False
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.load_session", "daemon.list_terminals"],
      testCase "malformed full receipts never become loaded and can be retried" $ bounded $ do
        void $ withLoadPeer $ \connection _ -> do
          try @DroidError (Daemon.loadSessionInfo connection "malformed") >>= (@?= Left DroidInvalidEvent)
          failed <- Daemon.getSessionReadiness connection "malformed"
          Daemon.readinessPhase failed @?= Daemon.SessionNotLoaded
          Daemon.readinessNotFound failed @?= False
          Daemon.readinessLoading failed @?= False
          Daemon.ensureSessionLoaded connection "malformed"
          Daemon.getSessionReadiness connection "malformed" >>= (@?= Daemon.SessionLoaded) . Daemon.readinessPhase,
      testCase "invalid restored-request containers cannot publish fresh settings to an existing attachment" $ bounded $ do
        void $ withLoadPeer $ \connection _ ->
          Daemon.withResumedSessionOn connection "bad-pending" $ \session -> do
            try @DroidError (Daemon.loadSessionInfo connection "bad-pending") >>= (@?= Left DroidInvalidEvent)
            Daemon.getSettings session >>= (@?= "bad-pending") . settingsModel
            Daemon.getSessionReadiness connection "bad-pending" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase,
      testCase "the daemon loop flag supplies busy state when a load reports idle" $ bounded $ do
        void $ withLoadPeer $ \connection _ -> do
          void (Daemon.loadSessionInfo connection "busy")
          readiness <- Daemon.getSessionReadiness connection "busy"
          Daemon.readinessWorkingState readiness @?= Right (Just WorkingStreamingAssistantMessage)
          Daemon.sessionReadinessBusy readiness @?= Right True,
      testCase "post-reply working-state notifications win over snapshot busy state at the ordered barrier" $ bounded $ do
        void $ withLoadPeer $ \connection _ ->
          Daemon.withResumedSessionOn connection "ordered" $ \session -> do
            entered <- newEmptyMVar
            release <- newEmptyMVar
            stop <- Daemon.onSessionEvent session $ \case
              Right (WorkingStateEvent event) | workingStateNewState event == WorkingThinking -> putMVar entered () >> takeMVar release
              _ -> pure ()
            withAsync (Daemon.loadSessionInfo connection "ordered") $ \loading -> do
              takeMVar entered
              void (Daemon.getProxyToken connection)
              putMVar release ()
              void (wait loading)
            readiness <- Daemon.getSessionReadiness connection "ordered"
            Daemon.readinessWorkingState readiness @?= Right (Just WorkingExecutingTool)
            Daemon.sessionReadinessBusy readiness @?= Right True
            stop,
      testCase "readiness changes preserve pre-init markers and malformed working state is explicit" $ bounded $ do
        void $ withLoadPeer $ \connection _ -> do
          void (Daemon.loadSessionInfo connection "working")
          Daemon.setSessionPreInit connection "working" True
          before <- Daemon.getSessionReadiness connection "working"
          withAsync (Daemon.waitSessionReadinessChange connection "working" before) $ \changed -> do
            void (Daemon.getProxyToken connection)
            after <- wait changed
            Daemon.readinessPreInit after @?= True
            Daemon.sessionReadinessBusy after @?= Left DroidInvalidEvent
          invalid <- Daemon.getSessionReadiness connection "working"
          withAsync (Daemon.waitSessionReadinessChange connection "working" invalid) $ \changed -> do
            void (Daemon.getProxyToken connection)
            after <- wait changed
            Daemon.readinessWorkingState after @?= Right (Just WorkingIdle)
            Daemon.sessionReadinessBusy after @?= Right False
          Daemon.setSessionPreInit connection "working" False
          Daemon.getSessionReadiness connection "working" >>= (@?= False) . Daemon.readinessPreInit
    ]

infoModel :: Daemon.DaemonSessionInfo -> IO Text
infoModel info = maybe (assertFailure "Missing load snapshot") (pure . settingsModel . loadedSessionSettings) (Daemon.daemonLoadedState info)

newtype LoadFixture = LoadFixture {loadStarted :: MVar ()}

withLoadPeer :: (Daemon.DaemonConnection -> LoadFixture -> IO a) -> IO (a, [Object])
withLoadPeer action = do
  trace <- newIORef []
  counts <- newIORef mempty
  held <- newIORef mempty
  proxies <- newIORef (0 :: Int)
  fixture <- LoadFixture <$> newEmptyMVar
  result <- withPeer (\_ connection -> serve trace counts held proxies fixture connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
    Daemon.withConnection (options target) (`action` fixture)
  recorded <- readIORef trace
  pure (result, recorded)

serve :: IORef [Object] -> IORef (Map Text Int) -> IORef (Map Text Object) -> IORef Int -> LoadFixture -> WS.Connection -> IO ()
serve trace counts held proxies fixture connection = forever $ do
  request <- WS.receiveData connection >>= either (const (assertFailure "Malformed request")) pure . eitherDecode
  modifyIORef' trace (<> [request])
  method <- textField "method" request
  case method of
    "daemon.authenticate" -> do
      field "params" request @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection request (object ["userId" .= String "user", "orgId" .= String "org"])
    "daemon.list_terminals" -> reply connection request (object ["terminals" .= ([] :: [Value])])
    "daemon.load_session" -> do
      params <- objectField "params" request
      field "token" params @?= String "OFFLINE_ONLY"
      identifier <- textField "sessionId" params
      number <- atomicModifyIORef' counts (\current -> let next = Map.findWithDefault 0 identifier current + 1 in (Map.insert identifier next current, next))
      case (identifier, number) of
        ("holding", 1) -> modifyIORef' held (Map.insert identifier request) >> putMVar (loadStarted fixture) ()
        ("shared", 2) -> modifyIORef' held (Map.insert identifier request) >> putMVar (loadStarted fixture) ()
        ("shared", 3) -> do
          reply connection request (withMission "running" (loaded "newest"))
          pending <- readIORef held
          old <- maybe (assertFailure "Missing old load") pure (Map.lookup identifier pending)
          writeIORef held (Map.delete identifier pending)
          reply connection old (withMission "paused" (loaded "stale"))
        ("missing", 1) -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcEntityNotFound, "message" .= String "Session missing"]]
        ("malformed", 1) -> reply connection request (set "queuedMessages" (Bool False) (loaded identifier))
        ("bad-pending", 2) -> reply connection request (set "pendingPermissions" (Bool False) (loaded "invalid"))
        ("busy", _) -> reply connection request (set "isAgentLoopInProgress" (Bool True) (set "workingState" (String "idle") (loaded identifier)))
        ("ordered", 2) -> do
          notify connection identifier (object ["type" .= String "droid_working_state_changed", "newState" .= String "thinking"])
          reply connection request (set "isAgentLoopInProgress" (Bool True) (set "workingState" (String "thinking") (loaded identifier)))
          notify connection identifier (object ["type" .= String "droid_working_state_changed", "newState" .= String "executing_tool"])
        _ -> reply connection request (loaded identifier)
    "daemon.get_proxy_token" -> do
      pending <- atomicModifyIORef' held (mempty,)
      forM_ (Map.toList pending) $ \(identifier, old) -> reply connection old (loaded identifier)
      observed <- readIORef counts
      number <- atomicModifyIORef' proxies (\n -> (n + 1, n + 1))
      when (Map.member "working" observed) $
        notify connection "working" (object ["type" .= String "droid_working_state_changed", "newState" .= if number == 1 then Bool False else String "idle"])
      reply connection request (object ["token" .= String "OFFLINE_TOKEN"])
    _ -> assertFailure "Unexpected load-coordination request"

loaded :: Text -> Value
loaded identifier = object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= identifier, "reasoningEffort" .= String "low"]]

withMission :: Text -> Value -> Value
withMission phase = set "mission" (object ["state" .= phase, "features" .= ([] :: [Value]), "progressLog" .= ([] :: [Value]), "workerSessionIds" .= ([] :: [Value])])

set :: Key -> Value -> Value -> Value
set key value (Object fields) = Object (KeyMap.insert key value fields)
set _ _ _ = error "Expected object fixture"

notify :: WS.Connection -> Text -> Value -> IO ()
notify connection identifier value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= value]]

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection values = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> values)))

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

methods :: [Object] -> [Text]
methods trace = [method | frame <- trace, String method <- [field "method" frame]]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

textField :: Key -> Object -> IO Text
textField key fields = case field key fields of String value -> pure value; _ -> assertFailure "Missing text field"

objectField :: Key -> Object -> IO Object
objectField key fields = case field key fields of Object value -> pure value; _ -> assertFailure "Missing object field"
