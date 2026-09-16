{-# LANGUAGE OverloadedStrings #-}

module LoadPolicySpec (loadPolicyTests, runLoadPolicyPeer) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, yield)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (fromException, try)
import Control.Monad (forM_, unless, void, when, (>=>))
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecodeStrict', encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import DroidSpec (assertReaped)
import Factory.Droid qualified as Droid
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Interaction (cancelDroidQuestions)
import Factory.Droid.Protocol
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Control (ForkSessionParams (..))
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Schema.Settings (ToolPolicy (..), UpdateSessionSettingsParams (..), emptySettingsUpdate, emptyToolPolicy)
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport
import Factory.Droid.Transport.Process qualified as Process
import ProcessSpec (bounded)
import ProtocolSpec (PeerFailure (CallbackFailed), message, reply, request)
import SchemaTest (rejects)
import System.Environment (getExecutablePath)
import System.IO (hFlush, isEOF, stdout)
import System.Posix.Process (getProcessID)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

loadPolicyTests :: TestTree
loadPolicyTests =
  testGroup
    "Resume and load policy"
    [ testCase "load codecs retain explicit false, empty, null, exact numbers and extensions" $ do
        roundTrip (Proxy @LoadSessionParams) loadWire
        roundTrip (Proxy @DaemonLoadSessionParams) (add ["token" .= String "", "disableInactivityTimeout" .= False, "runtimeSettingsPath" .= String "", "skipPermissionsUnsafe" .= False] loadWire),
      testCase "init-only and restriction fields cannot be disguised as load parameters" $ do
        forM_ ["machineId", "cwd", "modelId", "systemPrompt", "worktree", "workspaceId", "tags", "restrictToolIds", "blockOnMcpLoad", "inactivityTimeoutMs"] $ \key ->
          rejects (Proxy @LoadSessionParams) (add [key .= Null] loadWire)
        forM_ [Number 0, Number (-1), Number 1.5, Number (scientific 1 100000000), Number (scientific 1 (-100000000)), String "1", Null] $ \value ->
          rejects (Proxy @LoadSessionParams) (add ["messageLimit" .= value] loadWire),
      testCase "retained intent merge treats null/false/empty as overrides, not omissions" $ do
        base <- decode @LoadSessionParams loadWire
        let old = (loadConfiguration base) {loadToolPolicy = emptyToolPolicy {policyDisabledTools = Just ["old"], policyRestrictedTools = Just ["old"]}, loadStructuredOutput = Just Nothing}
            new = defaultSessionLoadConfiguration {loadToolPolicy = emptyToolPolicy {policyDisabledTools = Just []}, loadAllMessages = Just False, loadAdditionalFields = KeyMap.singleton "extension" (String "new")}
            merged = mergeSessionLoadConfiguration old new
        policyDisabledTools (loadToolPolicy merged) @?= Just []
        policyRestrictedTools (loadToolPolicy merged) @?= Just ["old"]
        loadStructuredOutput merged @?= Just Nothing
        loadAllMessages merged @?= Just False
        KeyMap.lookup "extension" (loadAdditionalFields merged) @?= Just (String "new"),
      testCase "preparation returns a legal load plus the exact post-load restriction patch" $ do
        let config = policy ""
            (params, patch) = prepareLoadSessionParams "id" (loadMcpOptions (defaultLoadSessionParams "id")) True config
        validateLoadSessionParams params @?= Right ()
        KeyMap.lookup "restrictToolIds" (loadSessionFields params) @?= Nothing
        case patch of
          Just value -> field "restrictToolIds" (asObject (toJSON value)) @?= toJSON (["", ""] :: [Text])
          Nothing -> assertFailure "Restriction patch was lost"
        validateLoadSessionParams ((defaultLoadSessionParams "id") {loadConfiguration = config}) @?= Left InvalidLoadParams,
      testCase "typed low-level load methods retain exact frames and raw results" $ bounded $ do
        active <- newIORef "root"
        withFixture (engineReplies active "") $ \transport sent _ ->
          withRpcChannel (transportSendObject transport) (transportReceiveObject transport) $ \channel -> do
            params <- decode @LoadSessionParams loadWire
            let options = Client.CallOptions "local" (WithEnvelope (Just "1.205.0") Nothing mempty) (Just 1000000)
            result <- Client.loadSession channel options params
            frame <- atomically (readTQueue sent)
            field "method" frame @?= String "droid.load_session"
            field "params" frame @?= loadWire
            field "cwd" result @?= String "/saved/id"
            void (Client.loadDaemonSession channel (options {Client.callRequestId = "daemon"}) (DaemonLoadSessionParams params "" defaultDaemonLoadConfiguration))
            frame2 <- atomically (readTQueue sent)
            field "method" frame2 @?= String "daemon.load_session"
            field "token" (paramsOf frame2) @?= String "",
      testCase "queued stale admission sends nothing and preserves its exception without poisoning" $ bounded $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        allowed <- newTVarIO True
        sent <- newTQueueIO
        incoming <- newTQueueIO
        let hold = message (NotificationBody (BaseNotification "hold" Nothing mempty))
            send frame = do
              when (field "method" frame == String "hold") (putMVar entered () >> takeMVar release)
              atomically (writeTQueue sent frame)
              when (field "method" frame /= String "hold") (atomically (writeTQueue incoming (reply (frameId frame) Null)))
            admit = readTVar allowed >>= \current -> unless current (throwSTM CallbackFailed)
        withRpcChannel send (atomically (readTQueue incoming)) $ \channel ->
          withAsync (sendRpcMessage channel hold) $ \writer -> do
            takeMVar entered
            withAsync (try @PeerFailure (requestReplyWithAdmission channel RunBeforeRequest admit Nothing (request "stale"))) $ \pending -> do
              awaitCondition ((== 1) <$> atomically (getRpcPendingCount channel))
              atomically (writeTVar allowed False)
              putMVar release ()
              wait writer
              wait pending >>= (@?= Left CallbackFailed)
            frames <- atomically (flushTQueue sent)
            map (field "method") frames @?= [String "hold"]
            atomically (getRpcPendingCount channel) >>= (@?= 0)
            void (requestReply channel Nothing (request "healthy")),
      testCase "failed initial admission runs no optional hook and registers no pending request" $ bounded $ do
        withRpcChannel (const (assertFailure "Unexpected send")) (atomically retry) $ \channel -> do
          called <- newIORef False
          void (setRpcBeforeRequest channel (Just (\_ _ -> writeIORef called True)))
          let frame = (request "blocked") {envelopeBody = (envelopeBody (request "blocked")) {baseRequestParams = Just (object ["sessionId" .= String "s"])}}
          try @PeerFailure (requestReplyWithAdmission channel RunBeforeRequest (throwSTM CallbackFailed) Nothing frame) >>= (@?= Left CallbackFailed)
          readIORef called >>= (@?= False)
          atomically (getRpcPendingCount channel) >>= (@?= 0),
      testCase "local resume applies load fields then restrictions before publishing saved cwd" $ bounded $ withEngine $ \transport sent _ -> do
        let options = (Droid.defaultDroidSessionOptions ".") {Droid.droidSessionLoadConfiguration = policy "read"}
        Droid.withResumedDroidSessionOn options transport "saved" $ \session -> do
          Droid.getDroidWorkingDirectory session >>= (@?= Just "/saved/saved")
          Droid.getDroidWorkingDirectoryState session >>= (@?= State.WorkingDirectoryReported (Just "/saved/saved"))
          frames <- atomically (flushTQueue sent)
          map (field "method") frames @?= map String ["droid.load_session", "droid.update_session_settings"]
          case frames of
            [load, update] -> do
              field "disabledToolIds" (paramsOf load) @?= toJSON (["read"] :: [Text])
              field "restrictToolIds" (paramsOf load) @?= Null
              field "restrictToolIds" (paramsOf update) @?= toJSON (["read", "read"] :: [Text])
            _ -> assertFailure "Unexpected load sequence",
      testCase "owned resume projects load policy and reaps its native peer" $ bounded $ do
        executable <- getExecutablePath
        let options = (Droid.defaultDroidOptions ".") {Droid.droidExecutable = executable, Droid.droidLoadConfiguration = policy "owned", Droid.droidLaunchOptions = Process.defaultDroidLaunchOptions {Process.launchArguments = Just ["--load-policy-peer"]}}
        pid <- Droid.withResumedDroidSession options "saved" $ \session -> do
          Droid.getDroidWorkingDirectory session >>= (@?= Just "/saved/saved")
          fields <- asObject . toJSON <$> Droid.getDroidSettings session
          field "disabledToolIds" (asObject (field "loadParams" fields)) @?= toJSON (["owned"] :: [Text])
          pure (textField "fixturePid" fields)
        assertReaped pid,
      testCase "local creation policy survives replacement without putting restrictions on load" $ bounded $ withEngine $ \transport sent _ -> do
        let config = defaultSessionConfiguration {configurationToolPolicy = emptyToolPolicy {policyRestrictedTools = Just ["initial"]}, configurationDisableBuiltinSkills = Just False}
            options = (Droid.defaultDroidSessionOptions ".") {Droid.droidSessionConfiguration = config}
        Droid.withDroidSessionOn options transport $ \parent -> do
          child <- Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty)
          Droid.droidSessionId child @?= "child"
          Droid.getDroidWorkingDirectory child >>= (@?= Just "/saved/child")
          frames <- atomically (flushTQueue sent)
          map (field "method") frames @?= map String ["droid.initialize_session", "droid.fork_session", "droid.load_session", "droid.update_session_settings"]
          let loads = [paramsOf frame | frame <- frames, field "method" frame == String "droid.load_session"]
          map (field "disableBuiltinSkills") loads @?= [Bool False]
          map (KeyMap.lookup "restrictToolIds") loads @?= [Nothing]
          try @Droid.DroidError (Droid.getDroidWorkingDirectory parent) >>= \case Left _ -> pure (); Right _ -> assertFailure "Retired cwd handle remained usable",
      testGroup
        "acknowledged local policy survives replacement"
        [ testCase (show resumed <> if clear then " clears explicit lists" else " retains partial updates") $ bounded $ withEngine $ \transport sent _ ->
            withLocalPolicySession resumed transport $ \parent -> do
              let updated = ToolPolicy (Just ["extra", "extra"]) (Just ["enabled"]) (Just ["blocked"]) (Just ["Read"])
                  expected = if clear then updated {policyDisabledTools = Just [], policyRestrictedTools = Just []} else updated
              void (Droid.updateDroidSettings parent (emptySettingsUpdate {updateSettingsToolPolicy = updated}))
              when clear $ void (Droid.updateDroidSettings parent (emptySettingsUpdate {updateSettingsToolPolicy = emptyToolPolicy {policyDisabledTools = Just [], policyRestrictedTools = Just []}}))
              void (Droid.updateDroidSettings parent (emptySettingsUpdate {updateSettingsModel = Just "changed"}))
              void (atomically (flushTQueue sent))
              void (Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty))
              assertLocalLoadPolicy sent ["child"] expected
        | resumed <- [False, True],
          clear <- [False, True]
        ],
      testGroup
        "failed local policy updates are not retained"
        [ testCase label $ bounded $ do
            active <- newIORef "root"
            let plan frame
                  | field "method" frame == String "droid.update_session_settings",
                    field "disabledToolIds" (paramsOf frame) == toJSON ["rejected" :: Text] =
                      pure [respond frame]
                  | otherwise = engineReplies active "" frame
            withFixture plan $ \transport sent _ -> withLocalPolicySession False transport $ \parent -> do
              outcome <- try @RpcResultError (Droid.updateDroidSettings parent (emptySettingsUpdate {updateSettingsToolPolicy = emptyToolPolicy {policyDisabledTools = Just ["rejected"], policyRestrictedTools = Just []}}))
              case (label, outcome) of
                ("remote rejection", Left (RpcRemoteFailure _)) -> pure ()
                ("malformed reply", Left RpcInvalidResult) -> pure ()
                _ -> assertFailure "Expected the settings failure"
              Droid.droidSessionStatus parent >>= (@?= Droid.SessionReady)
              void (atomically (flushTQueue sent))
              void (Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty))
              assertLocalLoadPolicy sent ["child"] (loadToolPolicy (policy "initial"))
        | (label, respond) <- [("remote rejection", rejected), ("malformed reply", \frame -> reply (frameId frame) (Bool False))]
        ],
      testCase "rollback replays the latest acknowledged local policy" $ bounded $ do
        active <- newIORef "root"
        let plan frame = do
              current <- readIORef active
              if current == "child" && field "method" frame == String "droid.update_session_settings"
                then pure [rejected frame]
                else engineReplies active "" frame
            updated = (loadToolPolicy (policy "initial")) {policyDisabledTools = Just ["latest"], policyRestrictedTools = Just ["Read"]}
        withFixture plan $ \transport sent _ -> withLocalPolicySession False transport $ \parent -> do
          void (Droid.updateDroidSettings parent (emptySettingsUpdate {updateSettingsToolPolicy = updated}))
          void (atomically (flushTQueue sent))
          outcome <- try @Droid.DroidReplacementError (Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty))
          case outcome of
            Left failure -> case Droid.replacementRollbackError failure of Nothing -> pure (); Just _ -> assertFailure "Rollback failed"
            Right _ -> assertFailure "Rejected successor policy was published"
          Droid.droidSessionStatus parent >>= (@?= Droid.SessionReady)
          assertLocalLoadPolicy sent ["child", "root"] updated,
      testCase "concurrent local policy updates retain reply-intake order" $ bounded $ do
        active <- newIORef "root"
        submitted <- newEmptyMVar
        let plan frame
              | field "method" frame == String "droid.update_session_settings",
                KeyMap.member "disabledToolIds" (paramsOf frame) =
                  putMVar submitted frame >> pure []
              | otherwise = engineReplies active "" frame
            patch label = emptySettingsUpdate {updateSettingsToolPolicy = emptyToolPolicy {policyDisabledTools = Just [label]}}
        withFixture plan $ \transport sent feed -> withLocalPolicySession False transport $ \parent -> do
          withAsync (Droid.updateDroidSettings parent (patch "first")) $ \first -> do
            request1 <- takeMVar submitted
            withAsync (Droid.updateDroidSettings parent (patch "second")) $ \second -> do
              request2 <- takeMVar submitted
              feed (reply (frameId request2) (object []))
              feed (reply (frameId request1) (object []))
              void (wait second)
              void (wait first)
          void (atomically (flushTQueue sent))
          void (Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty))
          assertLocalLoadPolicy sent ["child"] ((loadToolPolicy (policy "initial")) {policyDisabledTools = Just ["first"]}),
      testCase "callback settings updates return before observation and replacement waits for that observation" $ bounded $ withEngine $ \transport sent feed ->
        withLocalPolicySession False transport $ \parent -> do
          firstEvent <- newIORef True
          updated <- newEmptyMVar
          release <- newEmptyMVar
          let changed = emptyToolPolicy {policyDisabledTools = Just ["callback"]}
          void $ Droid.onDroidSessionEvent parent $ \_ -> do
            first <- atomicModifyIORef' firstEvent (False,)
            when first $ do
              void (Droid.updateDroidSettings parent (emptySettingsUpdate {updateSettingsToolPolicy = changed}))
              putMVar updated ()
              takeMVar release
          feed (notice False "root" (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "trigger"]]))
          takeMVar updated
          void (atomically (flushTQueue sent))
          withAsync (Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty)) $ \replacing -> do
            fork <- atomically (readTQueue sent)
            premature <- timeout 50000 (atomically (readTQueue sent))
            putMVar release ()
            void (wait replacing)
            field "method" fork @?= String "droid.fork_session"
            premature @?= Nothing
          assertLocalLoadPolicy sent ["child"] ((loadToolPolicy (policy "initial")) {policyDisabledTools = Just ["callback"]}),
      testCase "local load null and omission remain distinct and never invent launch cwd" $ bounded $ do
        forM_ [Nothing, Just Null] $ \cwd -> do
          active <- newIORef "root"
          let plan frame = do
                responses <- engineReplies active "" frame
                pure (map (alterResult (\fields -> maybe (KeyMap.delete "cwd" fields) (\value -> KeyMap.insert "cwd" value fields) cwd)) responses)
          withFixture plan $ \transport _ _ ->
            Droid.withResumedDroidSessionOn (Droid.defaultDroidSessionOptions ".") transport "saved" $ \session -> do
              Droid.getDroidWorkingDirectory session >>= (@?= Nothing)
              Droid.getDroidWorkingDirectoryState session >>= (@?= case cwd of Nothing -> State.WorkingDirectoryUnknown; Just _ -> State.WorkingDirectoryReported Nothing),
      testCase "invalid loaded cwd rejects publication" $ bounded $ do
        active <- newIORef "root"
        withFixture (fmap (map (alterResult (KeyMap.insert "cwd" (Bool False)))) . engineReplies active "") $ \transport _ _ ->
          try @Droid.DroidError (Droid.withResumedDroidSessionOn (Droid.defaultDroidSessionOptions ".") transport "saved" (const (assertFailure "Invalid cwd published" :: IO ()))) >>= (@?= Left Droid.DroidInvalidEvent),
      testCase "local initialization prefers validated worktree cwd over launch cwd" $ bounded $ do
        active <- newIORef "root"
        let worktree = object ["branch" .= String "fixture", "path" .= String "/worktree", "isNewlyCreated" .= False]
        withFixture (fmap (map (alterResult (KeyMap.insert "worktree" worktree))) . engineReplies active "") $ \transport _ _ ->
          Droid.withDroidSessionOn (Droid.defaultDroidSessionOptions ".") transport (Droid.getDroidWorkingDirectory >=> (@?= Just "/worktree")),
      testCase "cwd response and later events keep wire order; malformed events stay invalid until repaired" $ bounded $ do
        active <- newIORef "root"
        let plan frame = do
              responses <- engineReplies active "" frame
              identifier <- readIORef active
              pure (responses <> [notice False identifier (object ["type" .= String "session_working_directory_changed", "cwd" .= String "/later"]) | field "method" frame == String "droid.change_working_directory"])
        withFixture plan $ \transport _ feed ->
          Droid.withResumedDroidSessionOn (Droid.defaultDroidSessionOptions ".") transport "saved" $ \session -> do
            void (Droid.changeDroidWorkingDirectory session "/requested")
            awaitCondition ((== Just "/later") <$> Droid.getDroidWorkingDirectory session)
            feed (notice False "saved" (object ["type" .= String "session_working_directory_changed", "cwd" .= False]))
            awaitCondition $ do value <- Droid.getDroidWorkingDirectoryState session; pure (value == State.WorkingDirectoryInvalid (Just "/later"))
            try @Droid.DroidError (Droid.getDroidWorkingDirectory session) >>= (@?= Left Droid.DroidInvalidEvent)
            feed (notice False "saved" (object ["type" .= String "session_working_directory_changed", "cwd" .= String ""]))
            awaitCondition ((== State.WorkingDirectoryReported (Just "")) <$> Droid.getDroidWorkingDirectoryState session)
            Droid.getDroidWorkingDirectory session >>= (@?= Just ""),
      testCase "daemon resume retains exact base/spawn policy and applies restrictions before readiness" $ bounded $ withEngine $ \transport sent _ -> do
        let options = clientOptions {Daemon.daemonClientLoadConfiguration = policy "daemon", Daemon.daemonClientLoadSpawnConfiguration = spawnPolicy ""}
        Daemon.withResumedSessionUsing options transport "saved" $ \session -> do
          frames <- atomically (flushTQueue sent)
          map (field "method") frames @?= map String ["daemon.load_session", "daemon.update_session_settings"]
          case frames of
            [load, patch] -> do
              field "loadAllMessages" (paramsOf load) @?= Bool False
              field "messageLimit" (paramsOf load) @?= Number 9007199254740993
              field "disableInactivityTimeout" (paramsOf load) @?= Bool False
              field "runtimeSettingsPath" (paramsOf load) @?= String ""
              field "skipPermissionsUnsafe" (paramsOf load) @?= Bool False
              field "restrictToolIds" (paramsOf patch) @?= toJSON (["daemon", "daemon"] :: [Text])
            _ -> assertFailure "Unexpected daemon load sequence"
          Daemon.getSessionReadiness (Daemon.sessionConnection session) "saved" >>= (@?= Daemon.SessionLoaded) . Daemon.readinessPhase,
      testCase "restriction ACK waits for matching completion before readiness and restored work" $ bounded $ withRestrictionAck $ \connection awaitPatch feed -> do
        published <- newTVarIO False
        restored <- newTVarIO False
        let handlers = Droid.defaultDroidHandlers {Droid.onDroidQuestion = Just (\_ -> atomically (writeTVar restored True) >> pure cancelDroidQuestions)}
            ready _ = atomically (writeTVar published True) >> atomically (readTVar restored >>= check)
            stillLoading = do
              void (Daemon.getProxyToken connection)
              timeout 50000 (atomically (readTVar published >>= check)) >>= (@?= Nothing)
              readTVarIO restored >>= (@?= False)
              Daemon.getSessionReadiness connection "s" >>= (@?= Daemon.SessionLoading) . Daemon.readinessPhase
        withAsync (Daemon.withResumedSessionOnConfigured connection handlers "s" (policy "guarded") defaultDaemonLoadConfiguration ready) $ \loading -> do
          patch <- awaitPatch
          stillLoading
          feed (restrictionCompletion "other" (frameId patch) (object []))
          feed (restrictionCompletion "s" "other-request" (object []))
          stillLoading
          feed (restrictionCompletion "s" (frameId patch) (object ["modelId" .= String "restricted"]))
          wait loading
          readTVarIO published >>= (@?= True)
          readTVarIO restored >>= (@?= True),
      testCase "malformed restriction completion prevents readiness and restored work" $ bounded $ withRestrictionAck $ \connection awaitPatch feed -> do
        restored <- newTVarIO False
        let handlers = Droid.defaultDroidHandlers {Droid.onDroidQuestion = Just (\_ -> atomically (writeTVar restored True) >> pure cancelDroidQuestions)}
        withAsync (try @Droid.DroidError (Daemon.withResumedSessionOnConfigured connection handlers "s" (policy "guarded") defaultDaemonLoadConfiguration (const (assertFailure "Malformed completion published a handle" :: IO ())))) $ \loading -> do
          patch <- awaitPatch
          void (Daemon.getProxyToken connection)
          feed (restrictionCompletion "s" (frameId patch) (Bool False))
          wait loading >>= (@?= Left Droid.DroidInvalidEvent)
          readTVarIO restored >>= (@?= False)
          Daemon.getSessionReadiness connection "s" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase,
      testCase "cancelling restriction completion preserves identity without restoring work" $ bounded $ withRestrictionAck $ \connection awaitPatch _ -> do
        restored <- newTVarIO False
        let handlers = Droid.defaultDroidHandlers {Droid.onDroidQuestion = Just (\_ -> atomically (writeTVar restored True) >> pure cancelDroidQuestions)}
        withAsync (Daemon.withResumedSessionOnConfigured connection handlers "s" (policy "guarded") defaultDaemonLoadConfiguration (const (assertFailure "Cancelled completion published a handle"))) $ \loading -> do
          void awaitPatch
          void (Daemon.getProxyToken connection)
          cancel loading
          waitCatch loading >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation lost"
          readTVarIO restored >>= (@?= False)
          Daemon.getSessionReadiness connection "s" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase
          void (Daemon.getProxyToken connection),
      testCase "stored daemon policy is per session; omission keeps it and explicit null clears capability" $ bounded $ withEngine $ \transport sent _ ->
        Daemon.withConnectionOn clientOptions transport $ \connection -> do
          format <- decode (object ["type" .= String "json_schema", "schema" .= object []])
          let config = (policy "a") {loadStructuredOutput = Just (Just format)}
          void (Daemon.loadSessionInfoWithConfiguration connection "a" config defaultDaemonLoadConfiguration)
          void (Daemon.loadSessionInfo connection "b")
          void (Daemon.loadSessionInfo connection "a")
          void (Daemon.loadSessionInfoWithConfiguration connection "a" (defaultSessionLoadConfiguration {loadStructuredOutput = Just Nothing}) defaultDaemonLoadConfiguration)
          void (Daemon.loadSessionInfo connection "a")
          frames <- atomically (flushTQueue sent)
          let loads = [paramsOf frame | frame <- frames, field "method" frame == String "daemon.load_session"]
          map (field "sessionId") loads @?= map String ["a", "b", "a", "a", "a"]
          map (KeyMap.lookup "structuredOutputFormat") loads @?= [Just (toJSON format), Nothing, Just (toJSON format), Just Null, Just Null]
          map (KeyMap.lookup "disabledToolIds") loads @?= [Just (toJSON ["a" :: Text]), Nothing, Just (toJSON ["a" :: Text]), Just (toJSON ["a" :: Text]), Just (toJSON ["a" :: Text])],
      testCase "a superseded token-provider wait cannot send stale load options" $ bounded $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        fetched <- newIORef (0 :: Int)
        let provider = do number <- atomicModifyIORef' fetched (\n -> (n + 1, n + 1)); when (number == 1) (putMVar entered () >> takeMVar release); pure (Just (Text.pack (show number)))
            options = Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthenticationProvider identity provider) "/work"
        withEngine $ \transport sent _ -> Daemon.withConnectionOn options transport $ \connection ->
          withAsync (try @Daemon.DaemonError (Daemon.loadSessionInfoWithConfiguration connection "s" (policy "old") defaultDaemonLoadConfiguration)) $ \old -> do
            takeMVar entered
            void (Daemon.loadSessionInfoWithConfiguration connection "s" (policy "new") defaultDaemonLoadConfiguration)
            putMVar release ()
            wait old >>= \case Left failure -> failure @?= Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Old load succeeded"
            frames <- atomically (flushTQueue sent)
            let loads = [paramsOf frame | frame <- frames, field "method" frame == String "daemon.load_session"]
            map (field "disabledToolIds") loads @?= [toJSON ["new" :: Text]]
            map (field "token") loads @?= [String "2"],
      testCase "a queued stale restriction write is rejected at writer admission" $ bounded $ do
        active <- newIORef "root"
        firstLoad <- newEmptyMVar
        holdEntered <- newEmptyMVar
        releaseHold <- newEmptyMVar
        count <- newIORef (0 :: Int)
        let plan frame = case field "method" frame of
              String "daemon.load_session" -> do
                n <- atomicModifyIORef' count (\x -> (x + 1, x + 1))
                if n == 1 then putMVar firstLoad frame >> pure [] else engineReplies active "" frame
              String "daemon.get_proxy_token" -> putMVar holdEntered () >> takeMVar releaseHold >> engineReplies active "" frame
              _ -> engineReplies active "" frame
        withFixture plan $ \transport sent feed -> Daemon.withConnectionOn clientOptions transport $ \connection -> do
          settled <- newTQueueIO
          void (Daemon.onRequestSettled connection (atomically . writeTQueue settled))
          withAsync (try @Daemon.DaemonError (Daemon.loadSessionInfoWithConfiguration connection "s" (policy "old") defaultDaemonLoadConfiguration)) $ \old -> do
            load <- takeMVar firstLoad
            withAsync (Daemon.getProxyToken connection) $ \holder -> do
              takeMVar holdEntered
              responses <- engineReplies active "" load
              mapM_ feed responses
              atomically (readTQueue settled) >>= (@?= frameId load)
              awaitCondition ((== 2) <$> Daemon.getPendingCount connection)
              withAsync (Daemon.loadSessionInfoWithConfiguration connection "s" (policy "new") defaultDaemonLoadConfiguration) $ \new -> do
                awaitCondition ((== 3) <$> Daemon.getPendingCount connection)
                putMVar releaseHold ()
                void (wait holder)
                void (wait new)
              wait old >>= \case Left failure -> failure @?= Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Stale load succeeded"
          frames <- atomically (flushTQueue sent)
          let updates = [field "restrictToolIds" (paramsOf frame) | frame <- frames, field "method" frame == String "daemon.update_session_settings"]
          updates @?= [toJSON ["new" :: Text, "new"]],
      testCase "definitive session close clears retained per-session load intent" $ bounded $ withEngine $ \transport sent feed ->
        Daemon.withConnectionOn clientOptions transport $ \connection -> do
          void (Daemon.loadSessionInfoWithConfiguration connection "s" (policy "retired") (spawnPolicy "private"))
          before <- Daemon.getSessionReadiness connection "s"
          feed (notice True "s" (object ["type" .= String "session_closed"]))
          void (Daemon.waitSessionReadinessChange connection "s" before)
          Daemon.getSessionReadiness connection "s" >>= (@?= False) . Daemon.readinessKnown
          void (atomically (flushTQueue sent))
          void (Daemon.loadSessionInfo connection "s")
          frames <- atomically (flushTQueue sent)
          let loads = [paramsOf frame | frame <- frames, field "method" frame == String "daemon.load_session"]
          map (KeyMap.lookup "disabledToolIds") loads @?= [Nothing]
          map (KeyMap.lookup "runtimeSettingsPath") loads @?= [Nothing],
      testCase "invalid load configuration precedes transport and token-provider access" $ bounded $ do
        let invalid = defaultSessionLoadConfiguration {loadMessageLimit = Just 0}
            transport = objectTransport (const (assertFailure "Unexpected send")) (assertFailure "Unexpected receive")
        try @LoadConfigurationError (Droid.withResumedDroidSessionOn ((Droid.defaultDroidSessionOptions ".") {Droid.droidSessionLoadConfiguration = invalid}) transport "s" (const (pure ()))) >>= (@?= Left InvalidLoadParams)
        try @LoadConfigurationError (Daemon.withResumedSessionUsing (clientOptions {Daemon.daemonClientAuthentication = Daemon.DaemonInheritAuthenticationProvider identity (assertFailure "Unexpected token provider"), Daemon.daemonClientLoadConfiguration = invalid}) transport "s" (const (pure ()))) >>= (@?= Left InvalidLoadParams),
      testCase "failed local restriction restore rolls back cwd and reapplies the original policy" $ bounded $ do
        active <- newIORef "root"
        let plan frame = do
              current <- readIORef active
              if current == "child" && field "method" frame == String "droid.update_session_settings"
                then pure [rejected frame]
                else engineReplies active "" frame
            config = defaultSessionConfiguration {configurationToolPolicy = emptyToolPolicy {policyRestrictedTools = Just ["original"]}}
        withFixture plan $ \transport sent _ ->
          Droid.withDroidSessionOn ((Droid.defaultDroidSessionOptions ".") {Droid.droidSessionConfiguration = config}) transport $ \parent -> do
            outcome <- try @Droid.DroidReplacementError (Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty))
            case outcome of
              Left failure -> do
                Droid.replacementTarget failure @?= "child"
                case Droid.replacementRollbackError failure of Nothing -> pure (); Just _ -> assertFailure "Rollback failed"
                case fromException (Droid.replacementCause failure) of Just (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Restriction error identity lost"
              Right _ -> assertFailure "Rejected restriction was published"
            Droid.droidSessionStatus parent >>= (@?= Droid.SessionReady)
            Droid.getDroidWorkingDirectory parent >>= (@?= Just "/saved/root")
            frames <- atomically (flushTQueue sent)
            map (field "method") frames @?= map String ["droid.initialize_session", "droid.fork_session", "droid.load_session", "droid.update_session_settings", "droid.load_session", "droid.update_session_settings"]
            [field "restrictToolIds" (paramsOf frame) | frame <- frames, field "method" frame == String "droid.update_session_settings"] @?= replicate 2 (toJSON (["original"] :: [Text])),
      testCase "a rejected daemon restriction patch prevents handle readiness without retry" $ bounded $ do
        active <- newIORef "root"
        let plan frame = if field "method" frame == String "daemon.update_session_settings" then pure [rejected frame] else engineReplies active "" frame
        withFixture plan $ \transport sent _ -> Daemon.withConnectionOn clientOptions transport $ \connection -> do
          outcome <- try @RpcResultError (Daemon.withResumedSessionOnConfigured connection Droid.defaultDroidHandlers "s" (policy "denied") defaultDaemonLoadConfiguration (const (assertFailure "Rejected policy was published" :: IO ())))
          case outcome of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Restriction rejection lost"
          Daemon.getSessionReadiness connection "s" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase
          frames <- atomically (flushTQueue sent)
          map (field "method") frames @?= map String ["daemon.load_session", "daemon.update_session_settings"],
      testCase "daemon creation retains policy and explicit future-load overrides through invalidation" $ bounded $ withEngine $ \transport sent _ -> do
        let initial = defaultSessionConfiguration {configurationSessionId = Just "created", configurationToolPolicy = emptyToolPolicy {policyRestrictedTools = Just ["initial"]}, configurationDisableBuiltinSkills = Just True, configurationSkipPermissionsUnsafe = Just False}
            future = defaultSessionLoadConfiguration {loadDisableBuiltinSkills = Just False, loadToolPolicy = emptyToolPolicy {policyRestrictedTools = Just []}}
            options = clientOptions {Daemon.daemonClientConfiguration = initial, Daemon.daemonClientSpawnOptions = defaultDaemonSpawnOptions {spawnDisableInactivityTimeout = Just False, spawnInactivityTimeoutMillis = Just 123, spawnRuntimeSettingsPath = Just "runtime"}, Daemon.daemonClientLoadConfiguration = future, Daemon.daemonClientLoadSpawnConfiguration = defaultDaemonLoadConfiguration {daemonLoadSkipPermissionsUnsafe = Just False}}
        Daemon.withSessionUsing options transport $ \session -> do
          let connection = Daemon.sessionConnection session
          Daemon.markSessionNotLoaded connection "created"
          Daemon.ensureSessionLoaded connection "created"
          frames <- atomically (flushTQueue sent)
          map (field "method") frames @?= map String ["daemon.initialize_session", "daemon.load_session", "daemon.update_session_settings"]
          case frames of
            [initialFrame, load, patch] -> do
              field "disableBuiltinSkills" (paramsOf initialFrame) @?= Bool True
              field "disableBuiltinSkills" (paramsOf load) @?= Bool False
              field "runtimeSettingsPath" (paramsOf load) @?= String "runtime"
              field "skipPermissionsUnsafe" (paramsOf load) @?= Bool False
              KeyMap.lookup "inactivityTimeoutMs" (paramsOf load) @?= Nothing
              field "restrictToolIds" (paramsOf patch) @?= toJSON ([] :: [Text])
            _ -> assertFailure "Unexpected retained creation policy sequence",
      testCase "legacy loaded worktree cwd is used only when direct cwd is omitted" $ bounded $ do
        forM_ [(Nothing, Just "/legacy"), (Just (String "/direct"), Just "/direct"), (Just Null, Nothing)] $ \(cwd, expected) -> do
          active <- newIORef "root"
          let alter fields = KeyMap.insert "worktree" (object ["path" .= String "/legacy"]) (maybe (KeyMap.delete "cwd" fields) (\value -> KeyMap.insert "cwd" value fields) cwd)
          withFixture (fmap (map (alterResult alter)) . engineReplies active "") $ \transport _ _ ->
            Droid.withResumedDroidSessionOn (Droid.defaultDroidSessionOptions ".") transport "saved" (Droid.getDroidWorkingDirectory >=> (@?= expected)),
      testCase "replacement inherits known cwd only when its own receipt is unknown" $ bounded $ do
        forM_ [False, True] $ \explicitNull -> do
          active <- newIORef "root"
          let alter frame fields = case field "method" frame of
                String "droid.initialize_session" -> KeyMap.insert "worktree" (object ["branch" .= String "fixture", "path" .= String "/parent", "isNewlyCreated" .= False]) fields
                String "droid.load_session" -> if explicitNull then KeyMap.insert "cwd" Null fields else KeyMap.delete "cwd" fields
                _ -> fields
              plan frame = map (alterResult (alter frame)) <$> engineReplies active "" frame
          withFixture plan $ \transport _ _ -> Droid.withDroidSessionOn (Droid.defaultDroidSessionOptions ".") transport $ \parent -> do
            child <- Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty)
            Droid.getDroidWorkingDirectoryState child >>= (@?= if explicitNull then State.WorkingDirectoryReported Nothing else State.WorkingDirectoryInherited "/parent"),
      testCase "initial unsafe permission bypass is not implicitly copied into later load intent" $ bounded $ withEngine $ \transport sent _ -> do
        let options = clientOptions {Daemon.daemonClientConfiguration = defaultSessionConfiguration {configurationSessionId = Just "unsafe", configurationSkipPermissionsUnsafe = Just True}}
        Daemon.withSessionUsing options transport $ \session -> do
          let connection = Daemon.sessionConnection session
          Daemon.markSessionNotLoaded connection "unsafe"
          Daemon.ensureSessionLoaded connection "unsafe"
          void (Daemon.loadSessionInfoWithConfiguration connection "unsafe" defaultSessionLoadConfiguration (defaultDaemonLoadConfiguration {daemonLoadSkipPermissionsUnsafe = Just True}))
          void (Daemon.loadSessionInfo connection "unsafe")
          frames <- atomically (flushTQueue sent)
          [field "skipPermissionsUnsafe" (paramsOf frame) | frame <- frames, field "method" frame == String "daemon.initialize_session"] @?= [Bool True]
          [KeyMap.lookup "skipPermissionsUnsafe" (paramsOf frame) | frame <- frames, field "method" frame == String "daemon.load_session"] @?= [Nothing, Just (Bool True), Just (Bool True)],
      testCase "cancelling a load retains async identity and admits no restriction mutation" $ bounded $ do
        started <- newEmptyMVar
        withFixture (\frame -> putMVar started frame >> pure []) $ \transport sent _ ->
          Daemon.withConnectionOn clientOptions transport $ \connection ->
            withAsync (Daemon.loadSessionInfoWithConfiguration connection "s" (policy "cancel") defaultDaemonLoadConfiguration) $ \worker -> do
              void (takeMVar started)
              cancel worker
              waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation lost"
              frames <- atomically (flushTQueue sent)
              map (field "method") frames @?= [String "daemon.load_session"]
    ]

withLocalPolicySession :: Bool -> ObjectTransport -> (Droid.DroidSession -> IO a) -> IO a
withLocalPolicySession resumed transport =
  let retained = policy "initial"
      options = (Droid.defaultDroidSessionOptions ".") {Droid.droidSessionLoadConfiguration = retained}
   in if resumed
        then Droid.withResumedDroidSessionOn options transport "root"
        else Droid.withDroidSessionOn (options {Droid.droidSessionConfiguration = defaultSessionConfiguration {configurationToolPolicy = loadToolPolicy retained}}) transport

assertLocalLoadPolicy :: TQueue Object -> [Text] -> ToolPolicy -> IO ()
assertLocalLoadPolicy sent identifiers expected = do
  frames <- atomically (flushTQueue sent)
  let loads = [paramsOf frame | frame <- frames, field "method" frame == String "droid.load_session"]
      patches = [paramsOf frame | frame <- frames, field "method" frame == String "droid.update_session_settings"]
      fields = asObject (toJSON (emptySettingsUpdate {updateSettingsToolPolicy = expected}))
  map (field "sessionId") loads @?= map String identifiers
  forM_ loads $ \params -> do
    forM_ ["additionalToolIds", "enabledToolIds", "disabledToolIds"] $ \key ->
      KeyMap.lookup key params @?= KeyMap.lookup key fields
    KeyMap.lookup "restrictToolIds" params @?= Nothing
    field "messageLimit" params @?= Number 9007199254740993
    field "loadAllMessages" params @?= Bool False
  patches @?= maybe [] (replicate (length identifiers) . KeyMap.singleton "restrictToolIds" . toJSON) (policyRestrictedTools expected)

withRestrictionAck :: (Daemon.DaemonConnection -> IO Object -> (Object -> IO ()) -> IO a) -> IO a
withRestrictionAck action = do
  active <- newIORef "root"
  submitted <- newEmptyMVar
  let plan frame = case field "method" frame of
        String "daemon.update_session_settings" -> putMVar submitted frame >> pure [reply (frameId frame) (object ["accepted" .= True])]
        String "daemon.load_session" -> do
          responses <- engineReplies active "" frame
          let pending = toJSON [object ["requestId" .= String "restored-question", "toolCallId" .= String "question-tool", "questions" .= ([] :: [Value])]]
          pure (map (alterResult (KeyMap.insert "pendingAskUserRequests" pending)) responses)
        Null -> (frameId frame @?= "restored-question") >> pure []
        _ -> engineReplies active "" frame
  withFixture plan $ \transport _ feed -> Daemon.withConnectionOn clientOptions transport $ \connection -> action connection (takeMVar submitted) feed

restrictionCompletion :: Text -> Text -> Value -> Object
restrictionCompletion identifier requestId settings = notice True identifier (object ["type" .= String "settings_updated", "requestId" .= requestId, "settings" .= settings])

rejected :: Object -> Object
rejected frame = KeyMap.insert "error" (toJSON (JsonRpcError RpcConflict "fixture rejection" Nothing mempty)) (reply (frameId frame) Null)

policy :: Text -> SessionLoadConfiguration
policy label = defaultSessionLoadConfiguration {loadToolPolicy = emptyToolPolicy {policyAdditionalTools = Just [], policyEnabledTools = Just [], policyDisabledTools = Just [label], policyRestrictedTools = Just [label, label]}, loadAllMessages = Just False, loadMessageLimit = Just 9007199254740993, loadAutoRejectPermissions = Just False, loadDisableBuiltinSkills = Just False}

spawnPolicy :: Text -> DaemonLoadConfiguration
spawnPolicy path = DaemonLoadConfiguration (Just False) (Just path) (Just False)

loadWire :: Value
loadWire = object ["sessionId" .= String "id", "mcpServers" .= ([] :: [Value]), "mcpOAuthCallbackUri" .= String "http://localhost:1234/callback", "additionalToolIds" .= [String "a", String "a"], "enabledToolIds" .= ([] :: [Value]), "disabledToolIds" .= ([] :: [Value]), "loadAllMessages" .= False, "messageLimit" .= (9007199254740993 :: Integer), "autoRejectPermissionRequests" .= False, "disableBuiltinSkills" .= False, "sessionLocation" .= String "custom location", "sessionSource" .= object ["platform" .= String "api", "delegationSessionId" .= String "source"], "sessionOriginHint" .= String "sdk", "structuredOutputFormat" .= Null, "extension" .= False]

identity :: GetUserInfoResult
identity = GetUserInfoResult "user" "org" mempty

clientOptions :: Daemon.DaemonClientOptions
clientOptions = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication identity "") "/work") {Daemon.daemonClientRestoreTerminalsOnLoad = False}

withFixture :: (Object -> IO [Object]) -> (ObjectTransport -> TQueue Object -> (Object -> IO ()) -> IO a) -> IO a
withFixture plan action = do
  incoming <- newTQueueIO
  sent <- newTQueueIO
  let feed = atomically . writeTQueue incoming
      send frame = atomically (writeTQueue sent frame) >> plan frame >>= mapM_ feed
  action (objectTransport send (atomically (readTQueue incoming))) sent feed

withEngine :: (ObjectTransport -> TQueue Object -> (Object -> IO ()) -> IO a) -> IO a
withEngine action = do
  active <- newIORef "root"
  withFixture (engineReplies active "") action

engineReplies :: IORef Text -> Text -> Object -> IO [Object]
engineReplies active pid frame = do
  let method = textField "method" frame
      params = paramsOf frame
      daemon = Text.isPrefixOf "daemon." method
      answer = reply (frameId frame)
  if method `elem` ["droid.initialize_session", "daemon.initialize_session", "droid.load_session", "daemon.load_session"]
    then do
      let identifier = case field "sessionId" params of String value -> value; _ -> "root"
          settings = object ["modelId" .= String "fixture", "reasoningEffort" .= String "low", "loadParams" .= params, "fixturePid" .= pid]
          result = object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= settings, "cwd" .= ("/saved/" <> identifier)]
      writeIORef active identifier
      pure [answer result]
    else case method of
      "droid.fork_session" -> pure [answer (object ["newSessionId" .= String "child"])]
      "droid.change_working_directory" -> pure [answer (object ["resolvedPath" .= String "/resolved"])]
      "daemon.get_proxy_token" -> pure [answer (object ["token" .= String "fixture"])]
      "daemon.list_terminals" -> pure [answer (object ["terminals" .= ([] :: [Value])])]
      "droid.update_session_settings" -> update daemon params answer
      "daemon.update_session_settings" -> update daemon params answer
      _ -> assertFailure ("Unexpected fixture method: " <> Text.unpack method)
  where
    update daemon params answer = do
      identifier <- readIORef active
      pure [notice daemon identifier (object ["type" .= String "settings_updated", "settings" .= Object (KeyMap.delete "sessionId" params)]), answer (object [])]

notice :: Bool -> Text -> Value -> Object
notice daemon identifier payload = KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.205.0", "type" .= String "notification", "method" .= String (if daemon then "daemon.session_notification" else "droid.session_notification"), "params" .= object ["sessionId" .= identifier, "notification" .= payload]]

alterResult :: (Object -> Object) -> Object -> Object
alterResult action frame = case KeyMap.lookup "result" frame of Just (Object value) -> KeyMap.insert "result" (Object (action value)) frame; _ -> frame

awaitCondition :: IO Bool -> IO ()
awaitCondition condition = do ready <- condition; unless ready (yield >> awaitCondition condition)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

textField :: Key -> Object -> Text
textField key fields = case field key fields of String value -> value; _ -> error "Expected fixture text"

frameId :: Object -> Text
frameId = textField "id"

paramsOf :: Object -> Object
paramsOf frame = case field "params" frame of Object value -> value; Null -> mempty; _ -> error "Expected fixture params"

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = error "Expected fixture object"

add :: [(Key, Value)] -> Value -> Value
add fields value = Object (KeyMap.union (KeyMap.fromList fields) (asObject value))

decode :: (FromJSON a) => Value -> IO a
decode value = case fromJSON value of Success result -> pure result; Error messageText -> assertFailure messageText

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decode @a value >>= (@?= value) . toJSON

runLoadPolicyPeer :: IO ()
runLoadPolicyPeer = do
  active <- newIORef "root"
  pid <- ("fixture-" <>) . Text.pack . show <$> getProcessID
  let loop = do
        ended <- isEOF
        unless ended $ do
          line <- BS.getLine
          frame <- either fail pure (eitherDecodeStrict' line)
          responses <- engineReplies active pid frame
          forM_ responses (\response -> BL.putStr (encode response <> "\n"))
          hFlush stdout
          loop
  loop
