{-# LANGUAGE OverloadedStrings #-}

module DaemonSpec (daemonTests) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Exception (Exception, catch, finally, fromException, throwIO, try)
import Control.Monad (forM_, replicateM, replicateM_, unless, void, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Factory.Droid (DroidError (..), DroidEvent (..), DroidHandlers (..), DroidOutputResult (..), DroidResult (..), DroidSessionStatus (..), DroidStreamMode (..), defaultDroidHandlers, rawDroidOutput)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Input (DroidInput (..), documentFromText, droidDocumentSource, droidImageSource, droidInput, imageFromBytes)
import Factory.Droid.Interaction (DroidMcpFailure (..), cancelDroidQuestions)
import Factory.Droid.MCP.Server qualified as Hosted
import Factory.Droid.MCP.Tool qualified as Hosted
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Content (ImageMediaType (ImagePNG))
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Interaction (cancelPermissionResult)
import Factory.Droid.Schema.MCP
import Factory.Droid.Schema.MCP.Config
import Factory.Droid.Schema.Notifications (AgentTurnCompleted (..), AgentTurnCompletionReason (..))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (..), SuccessResult (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import McpConfigSpec (fixtureMcpOptions, fixtureMcpWire, fixtureStoredServer)
import McpPeer (earlyMcpEvents, handleMcpRequest, invokeHosted)
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withSocketPeer, withTLSCertificate, withTLSPeer)

data AckMode = BeforeAck | AfterAck | LegacyResponse
  deriving stock (Eq, Show)

daemonTests :: TestTree
daemonTests = testGroup "Existing daemon session path" (wireTests <> lifecycleTests <> [testGroup "External MCP management" mcpSessionTests, testGroup "MCP configuration lifecycle" mcpConfigurationTests])

wireTests :: [TestTree]
wireTests =
  [ testCase ("authentication, session, follow-up turns and disconnect / " <> show mode) $ bounded $ do
      trace <- newIORef []
      withDaemonPeer mode True "1.201.1" trace $ \target -> do
        chunks <- newIORef []
        escaped <- Daemon.withSession (options target) $ \session -> do
          reportedUserId (Daemon.authenticatedUser session) @?= "offline-user"
          first <- Daemon.sendPrompt session "hello" (\text -> modifyIORef' chunks (<> [text]))
          resultText first @?= "Hello سلام"
          resultSessionId first @?= Daemon.sessionId session
          second <- Daemon.sendPrompt session "again" (\_ -> pure ())
          resultText second @?= "Hello سلام"
          Daemon.sessionStatus session >>= (@?= SessionReady)
          pure session
        readIORef chunks >>= (@?= ["Hello سلام"])
        Daemon.sessionStatus escaped >>= (@?= SessionUnavailable)
        expectDroid DroidSessionUnusable (Daemon.sendPrompt escaped "late" (\_ -> pure ()))
      frames <- readIORef trace
      methods frames @?= ["daemon.authenticate", "daemon.initialize_session", "daemon.add_user_message", "daemon.add_user_message"]
  | mode <- [BeforeAck, AfterAck, LegacyResponse]
  ]

lifecycleTests :: [TestTree]
lifecycleTests =
  [ testCase "resume attaches with token without initializing or replaying a turn" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.151.0" trace $ \target -> do
        let configured = (options target) {Daemon.daemonProtocolVersion = "1.151.0", Daemon.daemonCredential = Daemon.DaemonToken "OFFLINE_ONLY" (Just "OFFLINE_GRANT")}
        Daemon.withResumedSession configured "saved-session" $ \session -> do
          Daemon.sessionId session @?= "saved-session"
          result <- Daemon.sendPrompt session "hello" (\_ -> pure ())
          resultText result @?= "Hello سلام"
      frames <- readIORef trace
      take 2 (methods frames) @?= ["daemon.authenticate", "daemon.load_session"]
      case frames of
        first : _ -> KeyMap.lookup "actAsGrant" (parameters first) @?= Just (String "OFFLINE_GRANT")
        [] -> assertFailure "Missing authentication request",
    testCase "resume restores pending interactions through owned dispatcher workers" $ bounded $ do
      trace <- newIORef []
      ready <- newEmptyMVar
      release <- newEmptyMVar
      settled <- newEmptyMVar
      let handlers =
            defaultDroidHandlers
              { onDroidPermission = Just (\_ -> putMVar ready () >> readMVar release >> pure cancelPermissionResult),
                onDroidQuestion = Just (\_ -> putMVar ready () >> readMVar release >> pure cancelDroidQuestions)
              }
      withDaemonPeer AfterAck False "1.201.1" trace $ \target ->
        Daemon.withResumedSessionHandlers (options target) handlers "pending-saved" $ \session -> do
          stop <- Daemon.onSessionEvent session $ \case
            Right (OtherNotificationEvent fields) | KeyMap.lookup "type" fields == Just (String "pending_settled") -> putMVar settled ()
            _ -> pure ()
          replicateM_ 2 (takeMVar ready)
          putMVar release ()
          takeMVar settled
          result <- Daemon.sendPrompt session "hello" (\_ -> pure ())
          resultText result @?= "Hello سلام"
          stop,
    testCase "daemon input plus output retains validated attachments through the backend" $ bounded $ do
      trace <- newIORef []
      image <- either throwIO pure (imageFromBytes "\x89PNG\r\n\x1a\n" ImagePNG)
      document <- either throwIO pure (documentFromText "attached" (Just "note.txt") (Just "text/plain"))
      output <- either throwIO pure (rawDroidOutput (KeyMap.singleton "type" (String "object")))
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          result <- Daemon.sendInputOutput session output ((droidInput "input-output") {inputImages = [image], inputDocuments = [document]}) (\_ -> pure ())
          outputValue result @?= Right (KeyMap.singleton "answer" (Number 7))
      frames <- readIORef trace
      case [parameters frame | frame <- frames, KeyMap.lookup "method" frame == Just (String "daemon.add_user_message")] of
        [params] -> do
          KeyMap.lookup "images" params @?= Just (toJSON [droidImageSource image])
          KeyMap.lookup "files" params @?= Just (toJSON [droidDocumentSource document])
        _ -> assertFailure "Missing daemon input request",
    testCase "TLS daemon path carries authentication and a complete turn" $
      bounded $
        withTLSCertificate $ \credential trusted -> do
          trace <- newIORef []
          withTLSPeer credential (daemonPeer AfterAck True "1.201.1" trace) $ \target -> do
            let configured = (options (target {WebSocket.webSocketHost = "localhost"})) {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Just trusted}}
            Daemon.withSession configured $ \session -> do
              result <- Daemon.sendPrompt session "hello" (\_ -> pure ())
              resultText result @?= "Hello سلام",
    testCase "daemon callbacks retain execution session and only permission callback disables rejection" $ bounded $ do
      forM_ [False, True] $ \permission -> do
        trace <- newIORef []
        seen <- newIORef ([] :: [Text])
        let handlers =
              defaultDroidHandlers
                { onDroidPermission = if permission then Just (\_ -> modifyIORef' seen (<> ["permission"]) >> pure cancelPermissionResult) else Nothing,
                  onDroidQuestion = Just (\_ -> modifyIORef' seen (<> ["question"]) >> pure cancelDroidQuestions)
                }
        withDaemonPeer AfterAck (not permission) "1.201.1" trace $ \target ->
          Daemon.withSessionHandlers (options target) handlers $ \session -> do
            _ <- Daemon.sendPrompt session "interactions" (\_ -> pure ())
            readIORef seen >>= (@?= (["permission" | permission] <> ["question"])),
    testCase "unrelated permissions and questions cancel without invoking scoped callbacks" $ bounded $ do
      trace <- newIORef []
      seen <- newIORef ([] :: [Text])
      let handlers =
            defaultDroidHandlers
              { onDroidPermission = Just (\_ -> modifyIORef' seen (<> ["permission"]) >> pure cancelPermissionResult),
                onDroidQuestion = Just (\_ -> modifyIORef' seen (<> ["question"]) >> pure cancelDroidQuestions)
              }
      withDaemonPeer AfterAck False "1.201.1" trace $ \target ->
        Daemon.withSessionHandlers (options target) handlers $ \session -> do
          _ <- Daemon.sendPrompt session "unrelated-interactions" (\_ -> pure ())
          readIORef seen >>= (@?= []),
    testCase "foreign session and turn events cannot settle or poison this turn" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          result <- Daemon.sendPrompt session "foreign" (\_ -> pure ())
          resultText result @?= "Hello سلام",
    testCase "daemon requires session IDs and correlated terminal IDs" $
      bounded $
        forM_ ["missing-session", "missing-turn", "invalid-creation"] $ \prompt -> do
          trace <- newIORef []
          withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
            Daemon.withSession (options target) $ \session -> do
              expectDroid DroidInvalidEvent (Daemon.sendPrompt session prompt (\_ -> pure ()))
              Daemon.sessionStatus session >>= (@?= SessionUnavailable),
    testCase "session closure invalidates even with a later queued success completion" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          expectDroid DroidSessionUnusable (Daemon.sendPrompt session "closed" (\_ -> pure ()))
          Daemon.sessionStatus session >>= (@?= SessionUnavailable),
    testCase "interruption uses the daemon session ID and preserves settled reuse" $ bounded $ do
      trace <- newIORef []
      started <- newEmptyMVar
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          withAsync (Daemon.sendTurn session "hang" (\_ -> putMVar started ())) $ \turn -> do
            takeMVar started
            Daemon.interruptSession session
            result <- wait turn
            turnCompletionReason (resultCompletion result) @?= TurnCancelled
            resultText result @?= "partial"
          result <- Daemon.sendPrompt session "hello" (\_ -> pure ())
          resultText result @?= "Hello سلام",
    testCase "cancelled turn retains exception identity and invalidates its handle" $ bounded $ do
      trace <- newIORef []
      started <- newEmptyMVar
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          withAsync (Daemon.sendTurn session "hang" (\_ -> putMVar started ())) $ \turn -> do
            takeMVar started
            cancel turn
            waitCatch turn >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled daemon turn returned"
          Daemon.sessionStatus session >>= (@?= SessionUnavailable),
    testCase "turn deadline and missing creation acknowledgement settle with cleanup" $
      bounded $
        forM_ ["hang", "missing-creation"] $ \prompt -> do
          trace <- newIORef []
          withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
            Daemon.withSession ((options target) {Daemon.daemonTurnTimeoutMicros = Just 100000}) $ \session ->
              expectDroid DroidTurnTimedOut (Daemon.sendTurn session prompt (\_ -> pure ())),
    testCase "structured output and rich events reuse the common turn engine" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          output <- either throwIO pure (rawDroidOutput (KeyMap.singleton "type" (String "object")))
          events <- newIORef []
          result <- Daemon.sendOutputEvents session output AllEvents "output" (\event -> modifyIORef' events (<> [event]))
          outputValue result @?= Right (KeyMap.singleton "answer" (Number 7))
          readIORef events >>= \seen -> unless (any isStructured seen) (assertFailure "Missing rich structured event"),
    testCase "scope exit cancels and joins pending daemon interaction workers" $ bounded $ do
      trace <- newIORef []
      started <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> (putMVar started () >> takeMVar hold) `finally` putMVar finished ())}
      withDaemonPeer AfterAck False "1.201.1" trace $ \target -> do
        result <- try @CallbackAbort $ Daemon.withSessionHandlers (options target) handlers $ \session ->
          withAsync (Daemon.sendTurn session "pending-handler" (\_ -> pure ())) $ \_ -> takeMVar started >> throwIO CallbackAbort
        result @?= (Left CallbackAbort :: Either CallbackAbort ())
        takeMVar finished,
    testCase "authentication failure is preserved and no session is initialized" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target -> do
        result <- try @RpcResultError (Daemon.withSession ((options target) {Daemon.daemonCredential = Daemon.DaemonApiKey "REJECT"}) (\_ -> assertFailure "Authentication accepted"))
        case result of
          Left (RpcRemoteFailure _) -> pure ()
          _ -> assertFailure "Expected remote authentication failure"
      readIORef trace >>= (@?= ["daemon.authenticate"]) . methods,
    testCase "disconnect rejects an acknowledged request still awaiting creation" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          result <- try @RpcChannelError (Daemon.sendTurn session "disconnect" (\_ -> pure ()))
          case result of
            Left RpcChannelReadFailure -> pure ()
            _ -> assertFailure "Expected connection loss while awaiting creation"
          Daemon.sessionStatus session >>= (@?= SessionUnavailable),
    testCase "invalid options fail before connection and sensitive configuration is redacted" $ do
      let target = WebSocket.WebSocketTarget "127.0.0.1" 1 "/"
      result <- try @Daemon.DaemonError (Daemon.withSession ((options target) {Daemon.daemonCredential = Daemon.DaemonApiKey ""}) (\_ -> pure ()))
      result @?= Left Daemon.InvalidDaemonCredential
      result2 <- try @Daemon.DaemonError (Daemon.withResumedSession ((options target) {Daemon.daemonModel = Just "override"}) "saved" (\_ -> pure ()))
      result2 @?= Left Daemon.DaemonModelRequiresNewSession
      show (options target) @?= "DaemonOptions <redacted>"
      show (Daemon.DaemonToken "private" (Just "grant")) @?= "DaemonCredential <redacted>"
  ]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target =
  (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "/daemon-workspace")
    { Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing, WebSocket.webSocketConnectTimeoutMicros = 1000000, WebSocket.webSocketCloseTimeoutMicros = 100000}
    }

mcpConfigurationTests :: [TestTree]
mcpConfigurationTests =
  [ testCase "daemon startup and resume invoke session-owned Haskell tools over HTTP" $ bounded $ do
      calls <- newIORef (0 :: Int)
      tool <- either (const (assertFailure "Hosted tool construction failed")) pure (Hosted.rawTool "echo" "Echo" Hosted.openObjectSchema (\arguments -> modifyIORef' calls (+ 1) >> pure (Hosted.structuredResult arguments)))
      server <- Hosted.newMcpServer (Hosted.defaultMcpServerOptions "hosted-fixture") [tool]
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession ((options target) {Daemon.daemonHostedMcpServers = [server]}) $ \_ -> readIORef calls >>= (@?= 1)
      Hosted.getMcpServerConfig server >>= (@?= Nothing)
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withResumedSession ((options target) {Daemon.daemonHostedMcpServers = [server]}) "saved" $ \_ -> readIORef calls >>= (@?= 2)
      Hosted.getMcpServerConfig server >>= (@?= Nothing),
    testCase "connection MCP observer receives pre-publication events with owned routing" $ bounded $ do
      trace <- newIORef []
      observed <- newIORef []
      let handlers = defaultDroidHandlers {onDroidMcpEvent = Just (\source event -> case event of Left (DroidMcpConnectionFailure _) -> pure (); _ -> modifyIORef' observed (<> [(source, event)]))}
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSessionHandlers ((options target) {Daemon.daemonMcpOptions = fixtureMcpOptions}) handlers $ \session -> do
          events <- readIORef observed
          map fst events @?= replicate 3 (Just (Daemon.sessionId session))
          case map snd events of
            [Right (McpAuthRequiredEvent _), Right (McpStatusEvent _), Right (McpAuthCompletedEvent _)] -> pure ()
            _ -> assertFailure "Startup MCP events were lost or foreign events admitted"
      malformed <- newIORef []
      let badHandler = defaultDroidHandlers {onDroidMcpEvent = Just (\source event -> case event of Left (DroidMcpConnectionFailure _) -> pure (); _ -> modifyIORef' malformed (<> [(source, event)]))}
          badPolicy = fixtureMcpOptions {sessionMcpOAuthCallbackUri = Just "fixture:malformed", sessionBlockOnMcpLoad = Nothing}
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withResumedSessionHandlers ((options target) {Daemon.daemonMcpOptions = badPolicy}) badHandler "saved" $ \_ ->
          readIORef malformed >>= (@?= [(Just "saved", Left DroidMcpInvalidEvent)]),
    testCase "startup cancellation joins admitted daemon MCP observation" $ bounded $ do
      trace <- newIORef []
      ready <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      let handlers = defaultDroidHandlers {onDroidMcpEvent = Just (\_ event -> case event of Right (McpAuthRequiredEvent _) -> (putMVar ready () >> takeMVar hold) `finally` putMVar finished (); _ -> pure ())}
      withDaemonPeer AfterAck True "1.201.1" trace $ \target -> do
        withAsync (Daemon.withSessionHandlers ((options target) {Daemon.daemonMcpOptions = fixtureMcpOptions}) handlers (\_ -> pure ())) $ \opening -> do
          takeMVar ready
          cancel opening
          waitCatch opening >>= \case
            Left err -> fromException err @?= Just AsyncCancelled
            Right _ -> assertFailure "Cancelled startup completed"
        takeMVar finished,
    testCase "daemon new/load options preserve omission, empty and normalized policy" $ bounded $ do
      forM_ [defaultMcpSessionOptions, defaultMcpSessionOptions {sessionMcpServers = Just []}, fixtureMcpOptions] $ \policy -> do
        trace <- newIORef []
        withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
          Daemon.withSession ((options target) {Daemon.daemonMcpOptions = policy}) (\_ -> pure ())
        frames <- readIORef trace
        let startup = [parameters value | value <- frames, KeyMap.lookup "method" value == Just (String "daemon.initialize_session")]
            expected = if policy == fixtureMcpOptions then Just (toJSON fixtureMcpWire) else toJSON <$> sessionMcpServers policy
        case startup of
          [params] -> do
            KeyMap.lookup "mcpServers" params @?= expected
            KeyMap.lookup "blockOnMcpLoad" params @?= (Bool <$> sessionBlockOnMcpLoad policy)
          _ -> assertFailure "Missing daemon initialization"
        resumedTrace <- newIORef []
        withDaemonPeer AfterAck True "1.201.1" resumedTrace $ \target ->
          Daemon.withResumedSession ((options target) {Daemon.daemonMcpOptions = policy {sessionBlockOnMcpLoad = Nothing}}) "saved" (\_ -> pure ())
        resumed <- readIORef resumedTrace
        case [parameters value | value <- resumed, KeyMap.lookup "method" value == Just (String "daemon.load_session")] of
          [params] -> do
            KeyMap.lookup "mcpServers" params @?= expected
            KeyMap.lookup "mcpOAuthCallbackUri" params @?= (String <$> sessionMcpOAuthCallbackUri policy)
            KeyMap.lookup "blockOnMcpLoad" params @?= Nothing
          _ -> assertFailure "Missing daemon load",
    testCase "add supports every configured transport and fails local validation before a mutation" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          forM_ (sessionMcpServers fixtureMcpOptions) $ \configs -> forM_ configs $ \config -> do
            result <- Daemon.addMcpServer session (mcpServerParams config)
            resultSuccess result @?= False
          let bad = AddMcpServerParams "invalid" McpHttp Nothing Nothing Nothing Nothing Nothing (Just (McpOAuthEnabled (emptyMcpOAuthOptions {mcpOAuthClientId = Just "secret"}))) mempty
          result <- try @McpConfigurationError (Daemon.addMcpServer session bad)
          result @?= Left InvalidMcpConfiguration
          Daemon.sessionStatus session >>= (@?= SessionReady)
      frames <- readIORef trace
      length [value | value <- frames, KeyMap.lookup "method" value == Just (String "daemon.add_mcp_server")] @?= 3,
    testCase "global config connection authenticates without creating or loading a session" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target -> do
        escaped <- Daemon.withConnection ((options target) {Daemon.daemonModel = Just "not-used", Daemon.daemonTurnTimeoutMicros = Just (-1)}) $ \connection -> do
          reportedUserId (Daemon.connectionUser connection) @?= "offline-user"
          result <- Daemon.getMcpConfig connection
          toJSON result @?= object ["servers" .= [fixtureStoredServer]]
          forM_ [McpConfigAdd, McpConfigRemove, McpConfigEnable, McpConfigDisable] $ \action -> do
            let oauth = emptyMcpOAuthOptions {mcpOAuthClientId = Just " client ", mcpOAuthIssuer = Just "https://issuer.invalid"}
                server = StoredMcpHttp (StoredRemoteMcp "https://mcp.invalid" (Just mempty) (Just oauth) (Just False) mempty)
                params = UpdateMcpConfigParams action ["remote", "remote"] (if action == McpConfigAdd then Just server else Nothing) mempty
            changed <- Daemon.updateMcpConfig connection params
            mcpConfigSuccess changed @?= False
            mcpConfigUpdateError changed @?= Just "declined"
          let invalid = StoredMcpHttp (StoredRemoteMcp "https://mcp.invalid" Nothing (Just (emptyMcpOAuthOptions {mcpOAuthClientSecret = Just "secret"})) Nothing mempty)
          invalidResult <- try @McpConfigurationError (Daemon.updateMcpConfig connection (UpdateMcpConfigParams McpConfigAdd ["remote"] (Just invalid) mempty))
          invalidResult @?= Left InvalidMcpConfiguration
          pure connection
        result <- try @RpcChannelError (Daemon.getMcpConfig escaped)
        case result of
          Left RpcChannelClosed -> pure ()
          _ -> assertFailure "Escaped global connection remained usable"
      frames <- readIORef trace
      methods frames @?= ["daemon.authenticate", "daemon.get_mcp_config", "daemon.update_mcp_config", "daemon.update_mcp_config", "daemon.update_mcp_config", "daemon.update_mcp_config"],
    testCase "global rejection and callback failure preserve ownership and disconnect" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target -> do
        result <- try @CallbackAbort $ Daemon.withConnection (options target) $ \connection -> do
          rejected <- try @RpcResultError (Daemon.updateMcpConfig connection (UpdateMcpConfigParams McpConfigRemove ["rpc-error"] Nothing mempty))
          case rejected of
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
            _ -> assertFailure "Missing global configuration rejection"
          _ <- Daemon.getMcpConfig connection
          throwIO CallbackAbort
        result @?= (Left CallbackAbort :: Either CallbackAbort ())
      frames <- readIORef trace
      methods frames @?= ["daemon.authenticate", "daemon.update_mcp_config", "daemon.get_mcp_config"],
    testCase "invalid startup and init-only resume configuration fail before networking" $ do
      let target = WebSocket.WebSocketTarget "127.0.0.1" 1 "/"
          invalid = fixtureMcpOptions {sessionMcpServers = Just [McpHttpConfig (McpRemoteConfig "remote" "not-a-uri" Nothing Nothing mempty)]}
      result <- try @McpConfigurationError (Daemon.withSession ((options target) {Daemon.daemonMcpOptions = invalid}) (\_ -> pure ()))
      result @?= Left InvalidMcpConfiguration
      resumed <- try @McpConfigurationError (Daemon.withResumedSession ((options target) {Daemon.daemonMcpOptions = fixtureMcpOptions}) "saved" (\_ -> pure ()))
      resumed @?= Left McpInitOnlyOptionOnResume
  ]

mcpSessionTests :: [TestTree]
mcpSessionTests =
  [ testCase "queries share typed status events and ignore malformed foreign status" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          observed <- newEmptyMVar
          _ <- Daemon.onSessionEvent session $ \case
            Left err -> putMVar observed (Left err)
            Right (McpStatusEvent value) -> putMVar observed (Right (changedMcpStatus value))
            _ -> pure ()
          servers <- Daemon.listMcpServers session
          map mcpStatusName (listedMcpServers servers) @?= ["fixture"]
          takeMVar observed >>= (@?= Right servers)
          tools <- Daemon.listMcpTools session
          map mcpToolEnabled (listedMcpTools tools) @?= [False]
          registry <- Daemon.listMcpRegistry session
          map registryServerName (mcpRegistryServers registry) @?= ["fixture"],
    testCase "mutations preserve false flags and enforce owned session routing" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          let code = SubmitMcpAuthCodeParams "fixture" "fixture-code" "fixture-state" (KeyMap.singleton "sessionId" (String "foreign-session"))
              err = SubmitMcpAuthErrorParams "fixture" "denied" "fixture-state" (Just "description") mempty
              calls =
                [ (Daemon.removeMcpServer session "fixture", toJSON (RemoveMcpServerParams "fixture" mempty)),
                  (Daemon.toggleMcpServer session "fixture" False, toJSON (ToggleMcpServerParams "fixture" False mempty)),
                  (Daemon.toggleMcpServer session "fixture" True, toJSON (ToggleMcpServerParams "fixture" True mempty)),
                  (Daemon.toggleMcpTool session "fixture" "lookup" False, toJSON (ToggleMcpToolParams "fixture" "lookup" False mempty)),
                  (Daemon.authenticateMcpServer session "fixture", toJSON (McpServerNameParams "fixture" mempty)),
                  (Daemon.cancelMcpAuth session "fixture", toJSON (McpServerNameParams "fixture" mempty)),
                  (Daemon.clearMcpAuth session "fixture", toJSON (McpServerNameParams "fixture" mempty)),
                  (Daemon.submitMcpAuthCode session code, toJSON code),
                  (Daemon.submitMcpAuthError session err, toJSON err)
                ]
          forM_ calls $ \(action, expected) -> do
            result <- action
            resultSuccess result @?= False
            case expected of
              Object fields -> KeyMap.lookup "observedParams" (resultAdditionalFields result) @?= Just (Object (KeyMap.insert "sessionId" (String (Daemon.sessionId session)) fields))
              _ -> assertFailure "Expected object parameters"
          Daemon.sessionStatus session >>= (@?= SessionReady),
    testCase "auth ACK does not invent completion or state correlation" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          required <- newEmptyMVar
          completed <- newEmptyMVar
          _ <- Daemon.onSessionEvent session $ \case
            Right (McpAuthRequiredEvent value) -> putMVar required value
            Right (McpAuthCompletedEvent value) -> putMVar completed value
            _ -> pure ()
          Daemon.authenticateMcpServer session "oauth" >>= (@?= True) . resultSuccess
          offered <- takeMVar required
          mcpAuthState offered @?= "fixture-state"
          tryTakeMVar completed >>= (@?= Nothing)
          Daemon.submitMcpAuthCode session (SubmitMcpAuthCodeParams "oauth" "fixture-code" (mcpAuthState offered) mempty) >>= (@?= False) . resultSuccess
          takeMVar completed >>= (@?= McpAuthFailed) . mcpAuthOutcome
          Daemon.submitMcpAuthError session (SubmitMcpAuthErrorParams "oauth" "denied" "fixture-state" Nothing mempty) >>= (@?= False) . resultSuccess
          takeMVar completed >>= (@?= McpAuthCancelled) . mcpAuthOutcome
          Daemon.sessionStatus session >>= (@?= SessionReady),
    testCase "pending auth admits cancellation while the first RPC is waiting" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          ready <- newEmptyMVar
          _ <- Daemon.onSessionEvent session $ \case
            Right (McpAuthRequiredEvent _) -> putMVar ready ()
            _ -> pure ()
          withAsync (Daemon.authenticateMcpServer session "held") $ \pending -> do
            takeMVar ready
            Daemon.cancelMcpAuth session "held" >>= (@?= True) . resultSuccess
            wait pending >>= (@?= False) . resultSuccess
          Daemon.sessionStatus session >>= (@?= SessionReady),
    testCase "malformed known auth notification remains an explicit observer error" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          observed <- newEmptyMVar
          _ <- Daemon.onSessionEvent session (putMVar observed)
          Daemon.authenticateMcpServer session "invalid-event" >>= (@?= False) . resultSuccess
          takeMVar observed >>= (@?= Left DroidInvalidEvent)
          Daemon.sessionStatus session >>= (@?= SessionReady),
    testCase "remote auth rejection preserves reuse and escaped handles fail" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target -> do
        escaped <- Daemon.withSession (options target) $ \session -> do
          result <- try @RpcResultError (Daemon.authenticateMcpServer session "rpc-error")
          case result of
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
            _ -> assertFailure "Missing MCP remote rejection"
          Daemon.sessionStatus session >>= (@?= SessionReady)
          pure session
        expectDroid DroidSessionUnusable (Daemon.listMcpServers escaped),
    testCase "cancelled auth invalidates without implicitly cancelling OAuth" $ bounded $ do
      trace <- newIORef []
      withDaemonPeer AfterAck True "1.201.1" trace $ \target ->
        Daemon.withSession (options target) $ \session -> do
          ready <- newEmptyMVar
          _ <- Daemon.onSessionEvent session $ \case
            Right (McpAuthRequiredEvent _) -> putMVar ready ()
            _ -> pure ()
          withAsync (Daemon.authenticateMcpServer session "held") $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled authentication returned"
          Daemon.sessionStatus session >>= (@?= SessionUnavailable)
      frames <- readIORef trace
      when ("daemon.cancel_mcp_auth" `elem` methods frames) (assertFailure "OAuth silently cancelled")
  ]

withDaemonPeer :: AckMode -> Bool -> Text -> IORef [Object] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withDaemonPeer mode reject version trace = withSocketPeer serve
  where
    serve socket = do
      pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
      lookup "Authorization" (WS.requestHeaders (WS.pendingRequest pending)) @?= Nothing
      WS.requestPath (WS.pendingRequest pending) @?= "/fixture?query=value"
      connection <- WS.acceptRequest pending
      daemonPeer mode reject version trace connection

daemonPeer :: AckMode -> Bool -> Text -> IORef [Object] -> WS.Connection -> IO ()
daemonPeer mode reject version trace connection = serve `catch` \(_ :: WS.ConnectionException) -> pure ()
  where
    readFrame = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid client JSON")) pure . eitherDecode
      KeyMap.lookup "jsonrpc" frame @?= Just (String "2.0")
      KeyMap.lookup "factoryApiVersion" frame @?= Just (String "1.0.0")
      KeyMap.lookup "factoryProtocolVersion" frame @?= Just (String version)
      modifyIORef' trace (<> [frame])
      pure frame
    writeFrame fields = WS.sendTextData connection (encode (KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= version] <> fields)))
    respond request fields = writeFrame (["type" .= String "response", "id" .= KeyMap.lookup "id" request] <> fields)
    notify identifier payload = writeFrame ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= payload]]
    serve = do
      auth <- readFrame
      KeyMap.lookup "method" auth @?= Just (String "daemon.authenticate")
      KeyMap.lookup "caller" (parameters auth) @?= Just (String "haskell-sdk")
      if KeyMap.lookup "apiKey" (parameters auth) == Just (String "REJECT")
        then do
          respond auth ["error" .= object ["code" .= (-32001 :: Int), "message" .= String "fixture authentication rejected"]]
          void readFrame
        else do
          unless (KeyMap.lookup "apiKey" (parameters auth) == Just (String "OFFLINE_ONLY") || KeyMap.lookup "token" (parameters auth) == Just (String "OFFLINE_ONLY")) (assertFailure "Incorrect authentication payload")
          respond auth ["result" .= object ["userId" .= String "offline-user", "orgId" .= String "offline-org"]]
          first <- readFrame
          if KeyMap.lookup "method" first `elem` [Just (String "daemon.get_mcp_config"), Just (String "daemon.update_mcp_config")]
            then globalLoop first
            else attach first
    globalLoop request = do
      let params = parameters request
      when (KeyMap.member "sessionId" params) (assertFailure "Global configuration acquired a session ID")
      case KeyMap.lookup "method" request of
        Just (String "daemon.get_mcp_config") -> do
          params @?= mempty
          respond request ["result" .= object ["servers" .= [fixtureStoredServer]]]
        Just (String "daemon.update_mcp_config")
          | KeyMap.lookup "serverNames" params == Just (toJSON (["rpc-error"] :: [Text])) -> respond request ["error" .= object ["code" .= (-32602 :: Int), "message" .= String "Global configuration rejection"]]
          | otherwise -> do
              KeyMap.lookup "serverNames" params @?= Just (toJSON (["remote", "remote"] :: [Text]))
              when (KeyMap.lookup "action" params == Just (String "add")) $
                case KeyMap.lookup "serverConfig" params of
                  Just (Object config) -> case KeyMap.lookup "oauth" config of
                    Just (Object oauth) -> KeyMap.lookup "clientId" oauth @?= Just (String "client")
                    _ -> assertFailure "Missing normalized global OAuth"
                  _ -> assertFailure "Missing global server configuration"
              respond request ["result" .= object ["success" .= False, "servers" .= [fixtureStoredServer], "error" .= String "declined"]]
        _ -> assertFailure "Unexpected global request or implicit session initialization"
      readFrame >>= globalLoop
    attach initialize = do
      let params = parameters initialize
      KeyMap.lookup "token" params @?= Just (String "OFFLINE_ONLY")
      KeyMap.lookup "autoRejectPermissionRequests" params @?= Just (Bool reject)
      identifier <- textField "sessionId" params
      when (KeyMap.member "mcpServers" params) (notify "foreign-session" (object ["type" .= String "mcp_auth_required"]))
      forM_ (earlyMcpEvents params) (notify identifier)
      invokeHosted params
      case KeyMap.lookup "method" initialize of
        Just (String "daemon.initialize_session") -> do
          KeyMap.lookup "machineId" params @?= Just (String "local")
          KeyMap.lookup "cwd" params @?= Just (String "/daemon-workspace")
          respond initialize ["result" .= object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= settings]]
        Just (String "daemon.load_session") -> do
          KeyMap.lookup "loadAllMessages" params @?= Just (Bool True)
          let pending =
                if identifier == "pending-saved"
                  then ["pendingPermissions" .= [object ["requestId" .= String "stored-permission", "toolUses" .= ([] :: [Value]), "options" .= [object ["label" .= String "Cancel", "value" .= String "cancel"]]]], "pendingAskUserRequests" .= [object ["requestId" .= String "stored-question", "toolCallId" .= String "stored-tool", "questions" .= ([] :: [Value])]]]
                  else []
          respond initialize ["result" .= object (["session" .= object ["messages" .= ([] :: [Value])], "settings" .= settings] <> pending)]
          when (identifier == "pending-saved") $ do
            replies <- replicateM 2 readFrame
            forM_ replies $ \response -> do
              KeyMap.lookup "type" response @?= Just (String "response")
              case KeyMap.lookup "id" response of
                Just (String "stored-permission") -> KeyMap.lookup "result" response @?= Just (object ["sessionId" .= identifier, "selectedOption" .= String "cancel"])
                Just (String "stored-question") -> KeyMap.lookup "result" response @?= Just (object ["sessionId" .= identifier, "answers" .= ([] :: [Value]), "cancelled" .= True])
                _ -> assertFailure "Unexpected restored interaction response"
            notify identifier (object ["type" .= String "pending_settled"])
        _ -> assertFailure "Unexpected session initialization"
      loop identifier Nothing
    settings = object ["modelId" .= String "offline-model", "reasoningEffort" .= String "low"]
    loop identifier active = do
      request <- readFrame
      let params = parameters request
      case KeyMap.lookup "method" request of
        Just (String "daemon.add_user_message") -> do
          KeyMap.lookup "sessionId" params @?= Just (String identifier)
          KeyMap.lookup "userMessageSource" params @?= Just (String "api")
          turn <- textField "messageId" params
          prompt <- textField "text" params
          unless (KeyMap.lookup "id" request /= Just (String turn)) (assertFailure "RPC ID reused as turn ID")
          let created = object ["type" .= String "create_message", "requestId" .= KeyMap.lookup "id" request, "message" .= if prompt == "invalid-creation" then Bool False else object ["id" .= turn, "role" .= String "user", "content" .= ([] :: [Value]), "createdAt" .= Number 0, "updatedAt" .= Number 0]]
              creation = unless (prompt `elem` ["missing-creation", "disconnect"]) (notify identifier created)
          when (mode == BeforeAck) creation
          respond request ["result" .= if mode == LegacyResponse then object [] else object ["accepted" .= True]]
          when (mode == AfterAck) creation
          case prompt of
            "invalid-creation" -> loop identifier Nothing
            "missing-creation" -> loop identifier Nothing
            "disconnect" -> WS.sendClose connection ("fixture disconnect" :: Text) >> void readFrame
            "pending-handler" -> do
              writeFrame ["type" .= String "request", "id" .= String "held-permission", "method" .= String "daemon.request_permission", "params" .= permissionParams identifier]
              loop identifier (Just turn)
            "hang" -> notify identifier (delta "partial") >> loop identifier (Just turn)
            _ -> do
              when (prompt == "interactions") $ do
                interaction "permission" "daemon.request_permission" (permissionParams identifier) (object ["sessionId" .= String "execution-session", "selectedOption" .= String "cancel"])
                interaction "question" "daemon.ask_user" (object ["sessionId" .= identifier, "toolCallId" .= String "question-tool", "questions" .= ([] :: [Value])]) (object ["sessionId" .= identifier, "answers" .= ([] :: [Value]), "cancelled" .= True])
              when (prompt == "unrelated-interactions") $ do
                interaction "foreign-permission" "daemon.request_permission" (object ["sessionId" .= String "foreign-session", "toolUses" .= ([] :: [Value]), "options" .= [object ["label" .= String "Cancel", "value" .= String "cancel"]]]) (object ["sessionId" .= String "foreign-session", "selectedOption" .= String "cancel"])
                interaction "foreign-question" "daemon.ask_user" (object ["sessionId" .= String "foreign-session", "associatedSessionIds" .= [identifier], "toolCallId" .= String "foreign-tool", "questions" .= ([] :: [Value])]) (object ["sessionId" .= String "foreign-session", "answers" .= ([] :: [Value]), "cancelled" .= True])
              when (prompt == "foreign") $ do
                notify "foreign-session" (object ["type" .= String "create_message", "message" .= False])
                notify identifier (object ["type" .= String "agent_turn_completed", "turnId" .= String "foreign-turn"])
              when (prompt == "missing-session") (writeFrame ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["notification" .= delta "wrong"]])
              when (prompt == "closed") (notify identifier (object ["type" .= String "session_closed"]))
              notify identifier (delta "Hello سلام")
              when (prompt `elem` ["output", "input-output"]) $ do
                KeyMap.lookup "outputFormat" params @?= Just (object ["type" .= String "json_schema", "schema" .= object ["type" .= String "object"]])
                notify identifier (object ["type" .= String "structured_output", "messageId" .= String "answer", "structuredOutput" .= object ["answer" .= Number 7]])
              notify identifier (completion (if prompt == "missing-turn" then Nothing else Just turn) "completed")
              loop identifier Nothing
        Just (String "daemon.interrupt_session") -> do
          KeyMap.lookup "sessionId" params @?= Just (String identifier)
          respond request ["result" .= object []]
          forM_ active $ \turn -> notify identifier (completion (Just turn) "cancelled")
          loop identifier Nothing
        _ -> do
          handled <- handleMcpRequest "daemon." identifier request respond notify readFrame
          if handled then loop identifier active else assertFailure "Unexpected daemon request, including forbidden implicit close/logout"
    interaction identifier method params expected = do
      writeFrame ["type" .= String "request", "id" .= (identifier :: Text), "method" .= (method :: Text), "params" .= params]
      response <- readFrame
      KeyMap.lookup "type" response @?= Just (String "response")
      KeyMap.lookup "id" response @?= Just (String identifier)
      KeyMap.lookup "result" response @?= Just expected
    permissionParams identifier = object ["sessionId" .= String "execution-session", "associatedSessionIds" .= [identifier], "toolUses" .= ([] :: [Value]), "options" .= [object ["label" .= String "Cancel", "value" .= String "cancel"]]]
    delta text = object ["type" .= String "assistant_text_delta", "messageId" .= String "answer", "blockIndex" .= Number 0, "textDelta" .= (text :: Text)]
    completion turn reason = object (["type" .= String "agent_turn_completed", "reason" .= (reason :: Text), "tokenUsage" .= object ["inputTokens" .= Number 1, "outputTokens" .= Number 1, "cacheCreationTokens" .= Number 0, "cacheReadTokens" .= Number 0, "thinkingTokens" .= Number 0]] <> maybe [] (\identifier -> ["turnId" .= (identifier :: Text)]) turn)

parameters :: Object -> Object
parameters fields = case KeyMap.lookup "params" fields of
  Just (Object value) -> value
  _ -> mempty

textField :: Key -> Object -> IO Text
textField key fields = case KeyMap.lookup key fields of
  Just (String value) -> pure value
  _ -> assertFailure "Missing expected text field"

methods :: [Object] -> [Text]
methods frames = [method | frame <- frames, Just (String method) <- [KeyMap.lookup "method" frame]]

expectDroid :: DroidError -> IO a -> IO ()
expectDroid expected action =
  try @DroidError action >>= \case
    Left err -> err @?= expected
    Right _ -> assertFailure "Expected daemon session error"

isStructured :: DroidEvent -> Bool
isStructured (StructuredOutputEvent _) = True
isStructured _ = False

data CallbackAbort = CallbackAbort deriving stock (Eq, Show)

instance Exception CallbackAbort
