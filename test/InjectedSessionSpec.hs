{-# LANGUAGE OverloadedStrings #-}

module InjectedSessionSpec (injectedSessionTests, withFixtureTransport) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, link, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, fromException, throwIO, try)
import Control.Monad (forM_, replicateM, when)
import DaemonSpec (AckMode (AfterAck), runDaemonPeer)
import Data.Aeson (Object, Result (..), Value (..), eitherDecodeStrict', fromJSON, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import DroidSpec (assertReaped)
import Factory.Droid
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Interaction (cancelDroidQuestions)
import Factory.Droid.MCP.Server qualified as Hosted
import Factory.Droid.MCP.Tool qualified as Hosted
import Factory.Droid.Protocol (RpcChannelError (RpcInvalidTimeout), RpcResultError (..))
import Factory.Droid.Schema.Discovery (GetUserInfoResult)
import Factory.Droid.Schema.Interaction (cancelPermissionResult)
import Factory.Droid.Transport
import Factory.Droid.Transport.IPC qualified as IPC
import Factory.Droid.Transport.InProcess qualified as InProcess
import Factory.Droid.Transport.Process qualified as Process
import IpcSpec (newRegistry, wire)
import ProcessSpec (bounded)
import System.Environment (getExecutablePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

injectedSessionTests :: TestTree
injectedSessionTests =
  testGroup
    "Injected session runtimes"
    [ testCase "local sessions retain normal turn streaming and expire without owning the transport" $ bounded $ do
        identifier <- withFixtureTransport $ \transport -> do
          transportKind transport @?= ProcessTransport
          chunks <- newIORef []
          escaped <- withDroidSessionOn (defaultDroidSessionOptions ".") transport $ \session -> do
            first <- sendPrompt session "hello" (\text -> modifyIORef' chunks (<> [text]))
            resultText first @?= "Hello سلام\n😀"
            second <- sendPrompt session "turn" (const (pure ()))
            resultText second @?= "2"
            readIORef chunks >>= (@?= "Hello سلام\n😀") . Text.concat
            pure session
          try @DroidError (getDroidSettings escaped) >>= (@?= Left DroidSessionUnusable)
          pure (droidSessionId escaped)
        assertReaped identifier,
      testCase "local resume and model override use the shared loaded-session path" $ bounded $ do
        identifier <- withFixtureTransport $ \transport ->
          withResumedDroidSessionOn ((defaultDroidSessionOptions ".") {droidSessionModel = Just "injected-model"}) transport "saved-session" $ \session -> do
            droidSessionId session @?= "saved-session"
            sendPrompt session "model" (const (pure ())) >>= (@?= "injected-model") . resultText
            resultText <$> sendPrompt session "peer-pid" (const (pure ()))
        assertReaped identifier,
      testCase "session-only option validation precedes supplied transport access" $ bounded $ do
        let unavailable = objectTransport (const (assertFailure "Unexpected send")) (assertFailure "Unexpected receive")
            options = (defaultDroidSessionOptions ".") {droidSessionTimeoutMicros = Just (-1)}
        try @RpcChannelError (withDroidSessionOn options unavailable (const (pure ()))) >>= (@?= Left RpcInvalidTimeout),
      testCase "logical connection request namespaces cannot reuse prior wire identities" $ bounded $ do
        identifiers <- newIORef []
        peers <- replicateM 2 $ withFixtureTransport $ \transport -> do
          let observed =
                transport
                  { transportSendObject = \frame -> do
                      case KeyMap.lookup "method" frame of
                        Just (String "droid.initialize_session") -> case KeyMap.lookup "id" frame of
                          Just (String identifier) -> modifyIORef' identifiers (<> [identifier])
                          _ -> assertFailure "Missing request identity"
                        _ -> pure ()
                      transportSendObject transport frame
                  }
          withDroidSessionOn (defaultDroidSessionOptions ".") observed (pure . droidSessionId)
        mapM_ assertReaped peers
        readIORef identifiers >>= \case
          [first, second] -> assertBool "Request identity reused across connection scopes" (not (Text.null first) && not (Text.null second) && first /= second)
          _ -> assertFailure "Initialization count changed",
      testCase "daemon creation and turns use supplied IPC and in-process channels with explicit auth" $
        bounded $
          forM_ [(kind, trusted) | kind <- [IpcBackend, InProcessBackend], trusted <- [False, True]] $ \(kind, trusted) ->
            withDaemonCallbacks kind trusted True $ \transport peer -> do
              escaped <- Daemon.withSessionUsing (clientOptions trusted) transport $ \session -> do
                chunks <- newIORef []
                result <- Daemon.sendPrompt session "hello" (\text -> modifyIORef' chunks (<> [text]))
                resultText result @?= "Hello سلام"
                readIORef chunks >>= (@?= "Hello سلام") . Text.concat
                Daemon.connectionUser (Daemon.sessionConnection session) @?= trustedIdentity
                Daemon.getTransportKind (Daemon.sessionConnection session) @?= case kind of IpcBackend -> IpcTransport; InProcessBackend -> InProcessTransport
                pure session
              try @DroidError (Daemon.sendPrompt escaped "hello" (const (pure ()))) >>= \case Left cause -> cause @?= DroidSessionUnusable; Right _ -> assertFailure "Expired daemon handle remained usable"
              trace <- readIORef (callbackTrace peer)
              let methods = methodNames trace
              ("daemon.authenticate" `elem` methods) @?= not trusted
              length (filter (== "daemon.initialize_session") methods) @?= 1,
      testCase "borrowed daemon resume restores interactions and readiness precedes terminal queries" $
        bounded $
          forM_ [IpcBackend, InProcessBackend] $ \kind ->
            withDaemonCallbacks kind True True $ \transport peer -> do
              Daemon.withConnectionOn (clientOptions True) transport $ \connection ->
                Daemon.withResumedSessionOn connection "pending-saved" $ \session -> do
                  Daemon.sessionId session @?= "pending-saved"
                  Daemon.sendPrompt session "hello" (const (pure ())) >>= (@?= "Hello سلام") . resultText
                  when (kind == InProcessBackend) $ do
                    ready <- requiredGate peer "pending-saved"
                    atomically ready
              trace <- readIORef (callbackTrace peer)
              let methods = methodNames trace
              "daemon.authenticate" `elem` methods @?= False
              "daemon.load_session" `elem` methods @?= True
              "daemon.list_terminals" `elem` methods @?= True,
      testCase "supplied permission and question handlers retain daemon execution routing" $
        bounded $
          forM_ [IpcBackend, InProcessBackend] $ \kind ->
            withDaemonCallbacks kind False False $ \transport _ -> do
              permissions <- newIORef (0 :: Int)
              questions <- newIORef (0 :: Int)
              let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> modifyIORef' permissions (+ 1) >> pure cancelPermissionResult), onDroidQuestion = Just (\_ -> modifyIORef' questions (+ 1) >> pure cancelDroidQuestions)}
              Daemon.withSessionUsingHandlers (clientOptions False) transport handlers $ \session ->
                Daemon.sendPrompt session "interactions" (const (pure ())) >>= (@?= "Hello سلام") . resultText
              readIORef permissions >>= (@?= 1)
              readIORef questions >>= (@?= 1),
      testCase "locality never implies authentication and rejected auth never initializes" $
        bounded $
          withDaemonCallbacks InProcessBackend False True $ \transport peer -> do
            let options = (clientOptions False) {Daemon.daemonClientAuthentication = Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "REJECT")}
            try @RpcResultError (Daemon.withSessionUsing options transport (const (pure ()))) >>= \case Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Authentication rejection lost"
            readIORef (callbackTrace peer) >>= (@?= ["daemon.authenticate"]) . methodNames,
      testCase "empty credentials fail before touching an injected transport" $ bounded $ do
        let unavailable = objectTransport (const (assertFailure "Unexpected daemon send")) (assertFailure "Unexpected daemon receive")
            options = (clientOptions False) {Daemon.daemonClientAuthentication = Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "")}
        try @Daemon.DaemonError (Daemon.withConnectionOn options unavailable (const (pure ()))) >>= (@?= Left Daemon.InvalidDaemonCredential),
      testCase "readiness registration failure prevents initialization and settles its gate" $
        bounded $
          withDaemonCallbacks InProcessBackend False True $ \transport peer -> do
            captured <- newEmptyMVar
            let failed = transport {transportPendingSessionReady = Just (\_ gate -> putMVar captured gate >> throwIO InjectedAbort)}
            try @InjectedFailure (Daemon.withSessionUsing (clientOptions False) failed (const (pure ()))) >>= (@?= Left InjectedAbort)
            gate <- takeMVar captured
            try @InjectedFailure (atomically gate) >>= (@?= Left InjectedAbort)
            readIORef (callbackTrace peer) >>= (@?= ["daemon.authenticate"]) . methodNames,
      testCase "cancelled injected load preserves the raw readiness failure" $
        bounded $
          withDaemonCallbacks InProcessBackend False True $ \transport peer -> do
            stopped <- newEmptyMVar
            hold <- newEmptyMVar
            received <- newIORef (0 :: Int)
            let delayed =
                  transport
                    { transportReceiveObject = do
                        value <- transportReceiveObject transport
                        modifyIORef' received (+ 1)
                        count <- readIORef received
                        when (count == 2) (putMVar stopped () >> takeMVar hold)
                        pure value
                    }
            withAsync (Daemon.withResumedSessionUsing (clientOptions False) delayed "saved" (const (pure ()))) $ \loading -> do
              takeMVar stopped
              gate <- requiredGate peer "saved"
              cancel loading
              waitCatch loading >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Load cancellation lost"
              try @SomeException (atomically gate) >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled load gate succeeded",
      testCase "hosted MCP requires declared locality and works with an in-process daemon" $ bounded $ do
        calls <- newIORef (0 :: Int)
        tool <- either (const (assertFailure "Hosted tool construction failed")) pure (Hosted.rawTool "echo" "Echo" Hosted.openObjectSchema (\arguments -> modifyIORef' calls (+ 1) >> pure (Hosted.structuredResult arguments)))
        server <- Hosted.newMcpServer (Hosted.defaultMcpServerOptions "hosted-fixture") [tool]
        let options = (clientOptions False) {Daemon.daemonClientHostedMcpServers = [server]}
        withDaemonCallbacks IpcBackend False True $ \transport peer -> do
          try @Hosted.McpServerError (Daemon.withSessionUsing options transport (const (pure ()))) >>= (@?= Left Hosted.HostedMcpRequiresLocalDaemon)
          readIORef (callbackTrace peer) >>= (@?= [])
        withDaemonCallbacks InProcessBackend False True $ \transport _ ->
          Daemon.withSessionUsing options transport $ \_ -> readIORef calls >>= (@?= 1)
        Hosted.getMcpServerConfig server >>= (@?= Nothing)
    ]

withFixtureTransport :: (ObjectTransport -> IO a) -> IO a
withFixtureTransport action = do
  executable <- getExecutablePath
  Process.withJsonLinesProcess (10 * 1024 * 1024) 50000 (Process.droidProcess executable Process.StreamJsonRpc) (action . processTransport)

data InjectedFailure = InjectedAbort deriving stock (Eq, Show)

instance Exception InjectedFailure

data CallbackBackend = IpcBackend | InProcessBackend deriving stock (Eq, Show)

data CallbackPeer = CallbackPeer
  { callbackTrace :: IORef [Object],
    callbackReady :: TVar (Map.Map Text (STM ()))
  }

clientOptions :: Bool -> Daemon.DaemonClientOptions
clientOptions trusted = Daemon.defaultDaemonClientOptions authentication "/daemon-workspace"
  where
    authentication = if trusted then Daemon.DaemonInheritAuthentication trustedIdentity "" else Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "OFFLINE_ONLY")

trustedIdentity :: GetUserInfoResult
trustedIdentity = case fromJSON (object ["userId" .= ("offline-user" :: Text), "orgId" .= ("offline-org" :: Text)]) of
  Success identity -> identity
  Error failure -> error failure

withDaemonCallbacks :: CallbackBackend -> Bool -> Bool -> (ObjectTransport -> CallbackPeer -> IO a) -> IO a
withDaemonCallbacks kind trusted reject action = do
  (subscribe, emitMessage, _, _) <- newRegistry
  (subscribeClose, emitClose, _, _) <- newRegistry
  incoming <- newTQueueIO
  trace <- newIORef []
  ready <- newTVarIO Map.empty
  let peer = CallbackPeer trace ready
      receive = atomically (readTQueue incoming)
      send frame = emitMessage (wire (Object frame))
      submit text = do
        frame <- either (const (assertFailure "Invalid callback JSON")) pure (eitherDecodeStrict' (Text.encodeUtf8 text))
        let method = KeyMap.lookup "method" frame
            params = case KeyMap.lookup "params" frame of Just (Object fields) -> fields; _ -> mempty
        when (kind == InProcessBackend && method `elem` [Just (String "daemon.initialize_session"), Just (String "daemon.load_session"), Just (String "daemon.list_terminals")]) $ do
          identifier <- requiredText "sessionId" params
          gate <- requiredGate peer identifier
          if method == Just (String "daemon.list_terminals")
            then atomically gate
            else atomically ((gate >> pure True) `orElse` pure False) >>= (@?= False)
        atomically (writeTQueue incoming frame)
  withAsync (runDaemonPeer AfterAck reject "1.201.1" trace trusted receive send (emitClose (Just "fixture disconnect"))) $ \server -> do
    link server
    case kind of
      IpcBackend ->
        IPC.withIpcChannel (10 * 1024 * 1024) (IPC.IpcMessageChannel (pure True) submit subscribe (Just subscribeClose)) (\transport -> action (ipcTransport transport) peer)
      InProcessBackend -> do
        let runtime =
              (InProcess.defaultInProcessRuntime submit)
                { InProcess.inProcessOnMessage = Just subscribe,
                  InProcess.inProcessOnClose = Just (\callback -> subscribeClose (callback 1000 . fromMaybe "fixture disconnect")),
                  InProcess.inProcessPendingSessionReady = Just (\identifier gate -> atomically (modifyTVar' ready (Map.insert identifier gate)))
                }
        InProcess.withInProcessChannel (10 * 1024 * 1024) "fixture" runtime (const (pure ())) (\transport -> action (inProcessTransport transport) peer)

requiredText :: Key -> Object -> IO Text
requiredText key fields = case KeyMap.lookup key fields of
  Just (String value) -> pure value
  _ -> assertFailure "Missing callback request identity"

requiredGate :: CallbackPeer -> Text -> IO (STM ())
requiredGate peer identifier = readTVarIO (callbackReady peer) >>= maybe (assertFailure "Readiness gate was not registered before dispatch") pure . Map.lookup identifier

methodNames :: [Object] -> [Text]
methodNames frames = [method | frame <- frames, Just (String method) <- [KeyMap.lookup "method" frame]]
