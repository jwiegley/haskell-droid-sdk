{-# LANGUAGE OverloadedStrings #-}

module DroidSpec (droidTests, runDroidPeer, assertReaped) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), asyncThreadId, cancel, wait, waitCatch, withAsync)
import Control.Exception (Exception, IOException, bracket, bracket_, fromException, throwIO, throwTo, toException, try)
import Control.Monad (forM_, replicateM_, unless, void, when, (>=>))
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecodeStrict', encode, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid
import Factory.Droid.Input
import Factory.Droid.Interaction
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Content (Base64ImageSource (..), Base64PDFSource (..), DocumentSource (..), ImageMediaType (..), PlainTextSource (..))
import Factory.Droid.Schema.Context (ContextAccuracy (ContextEstimated), ContextStats (..), GetContextBreakdownResult (..))
import Factory.Droid.Schema.Control (ChangeWorkingDirectoryResult (..), CompactSessionParams (..), CompactSessionResult (..), ExecuteRewindResult (..), ForkSessionParams (..), GetRewindInfoResult (..), RewindFileCreation (..), RewindFileSnapshot (..))
import Factory.Droid.Schema.Discovery
import Factory.Droid.Schema.Enums (AutonomyLevel (..), DroidInteractionMode (DroidAuto, DroidSpec), ReasoningEffort (..))
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.MCP
import Factory.Droid.Schema.MCP.Config
import Factory.Droid.Schema.Mission (MissionPhase (MissionCompleted, MissionPaused, MissionRunning), MissionSnapshot (..))
import Factory.Droid.Schema.Models (ListModelsOptions (..), ListModelsResult (..), ModelAvailability (..), ModelInfo (..), ModelMetadata (..))
import Factory.Droid.Schema.Notifications (AgentTurnCompleted (..), AgentTurnCompletionReason (..), ErrorNotification (..), ToolConfirmationOutcome (..))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (..), SuccessResult (..))
import Factory.Droid.Schema.Settings
import Factory.Droid.Schema.Usage (TokenUsage (..))
import Factory.Droid.Transport.Process (DroidLaunchOptions (..), JsonLinesError (InvalidFrameLimit), defaultDroidLaunchOptions)
import McpConfigSpec (fixtureMcpOptions, fixtureMcpWire)
import McpPeer (earlyMcpEvents, handleMcpRequest, invokeHosted)
import MissionEventSpec (missionEventPayload, missionWireEvents)
import ProcessSpec (bounded)
import System.Directory (doesFileExist, removePathForcibly)
import System.Environment (getExecutablePath, lookupEnv)
import System.IO (hFlush, hReady, hSetBinaryMode, stdin, stdout)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Text.Read (readMaybe)

droidTests :: TestTree
droidTests =
  testGroup
    "High-level local Droid path"
    [ testGroup "External MCP management" mcpSessionTests,
      testGroup "MCP configuration lifecycle" mcpConfigurationSessionTests,
      testCase "custom launch arguments and environment are applied before session initialization" $ bounded $ do
        options <- fixtureOptions
        let launch = defaultDroidLaunchOptions {launchPrefixArguments = ["--fixture-launch-prefix"], launchExtraArguments = ["--fixture-launch-extra", ""], launchEnvironment = Map.fromList [("DROID_LAUNCH_VALUE", "ordinary"), ("FACTORY_UPSTREAM_CLIENT_TYPE", "untrusted"), ("FACTORY_UPSTREAM_SDK", "untrusted")], launchTrustedEnvironment = Map.fromList [("DROID_LAUNCH_VALUE", "trusted"), ("DROID_LAUNCH_EMPTY", "")]}
        result <- runDroid (options {droidModel = Just "launch-options", droidLaunchOptions = launch}) "hello"
        resultText result @?= "Hello سلام\n😀"
        assertReaped (resultSessionId result),
      testCase "one-shot prompt returns text and reaps its CLI" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "hello"
        resultText result @?= "Hello سلام\n😀"
        assertReaped (resultSessionId result),
      testCase "streaming and follow-up prompts share one session without duplicate text" $ bounded $ do
        options <- fixtureOptions
        chunks <- newIORef []
        identifier <- withDroidSession options $ \session -> do
          first <- sendPrompt session "hello" (\text -> readIORef chunks >>= writeIORef chunks . (<> [text]))
          resultText first @?= "Hello سلام\n😀"
          second <- sendPrompt session "turn" (\_ -> pure ())
          resultText second @?= "2"
          resultSessionId first @?= resultSessionId second
          pure (droidSessionId session)
        readIORef chunks >>= (@?= "Hello سلام\n😀") . Text.concat
        assertReaped identifier,
      testCase "a prefilled assistant message works without deltas" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "prefilled"
        resultText result @?= "prefilled"
        assertReaped (resultSessionId result),
      testCase "local notifications may omit the session ID" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "local"
        resultText result @?= "local"
        assertReaped (resultSessionId result),
      testCase "unhandled permissions and questions are rejected" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "permissions"
        resultText result @?= "defaults denied"
        assertReaped (resultSessionId result),
      testCase "turn deadline interrupts and scope exit reaps the CLI" $ bounded $ do
        options <- fixtureOptions
        identifier <- newIORef ""
        result <- try @DroidError $ withDroidSession (options {droidTurnTimeoutMicros = Just 50000}) $ \session -> do
          writeIORef identifier (droidSessionId session)
          sendPrompt session "hang" (\_ -> pure ())
        result @?= Left DroidTurnTimedOut
        readIORef identifier >>= assertReaped,
      testCase "callback failure retains its identity and cleans up" $ bounded $ do
        options <- fixtureOptions
        identifier <- newIORef ""
        result <- try @CallbackAbort $ withDroidSession options $ \session -> do
          writeIORef identifier (droidSessionId session)
          sendPrompt session "hello" (\text -> unless (Text.null text) (throwIO CallbackAbort))
        result @?= Left CallbackAbort
        readIORef identifier >>= assertReaped,
      testCase "asynchronous cancellation cleans up an active prompt" $ bounded $ do
        options <- fixtureOptions
        ready <- newEmptyMVar
        identifier <- newIORef ""
        withAsync
          ( withDroidSession options $ \session -> do
              writeIORef identifier (droidSessionId session)
              sendPrompt session "hang" (\_ -> putMVar ready ())
          )
          $ \worker -> do
            takeMVar ready
            cancel worker
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled run succeeded"
        readIORef identifier >>= assertReaped,
      testCase "malformed selected events fail instead of hanging" $ bounded $ do
        options <- fixtureOptions
        result <- try @DroidError (runDroid options "malformed")
        result @?= Left DroidInvalidEvent,
      testCase "a settled failed turn reports a typed reason and permits another prompt" $ bounded $ do
        options <- fixtureOptions
        withDroidSession options $ \session -> do
          failed <- try @DroidError (sendPrompt session "failed" (\_ -> pure ()))
          failed @?= Left (DroidTurnFailed TurnError)
          droidSessionStatus session >>= (@?= SessionReady)
          reused <- sendPrompt session "hello" (\_ -> pure ())
          resultText reused @?= "Hello سلام\n😀",
      testCase "startup errors retain structured RPC details" $ bounded $ do
        options <- fixtureOptions
        result <- try @RpcResultError (runDroid (options {droidModel = Just "fixture-reject"}) "hello")
        result @?= Left (RpcRemoteFailure (JsonRpcError RpcAuthenticationError "fixture authentication failure" Nothing mempty)),
      testCase "resume loads saved state without replaying history into new output" $ bounded $ do
        options <- fixtureOptions
        chunks <- newIORef []
        peer <- withResumedDroidSession options "saved-session" $ \session -> do
          droidSessionId session @?= "saved-session"
          first <- sendPrompt session "turn" (\text -> readIORef chunks >>= writeIORef chunks . (<> [text]))
          resultText first @?= "2"
          readIORef chunks >>= (@?= ["2"])
          second <- sendPrompt session "turn" (\_ -> pure ())
          resultText second @?= "3"
          resultSessionId second @?= "saved-session"
          model <- sendPrompt session "model" (\_ -> pure ())
          resultText model @?= "saved-model"
          permissions <- sendPrompt session "permissions" (\_ -> pure ())
          resultText permissions @?= "defaults denied"
          resultText <$> sendPrompt session "peer-pid" (\_ -> pure ())
        assertReaped peer,
      testCase "resume applies an explicit model override after loading" $ bounded $ do
        options <- fixtureOptions
        peer <- withResumedDroidSession (options {droidModel = Just "chosen-model"}) "saved-session" $ \session -> do
          model <- sendPrompt session "model" (\_ -> pure ())
          resultText model @?= "chosen-model"
          resultText <$> sendPrompt session "peer-pid" (\_ -> pure ())
        assertReaped peer,
      testCase "missing saved sessions fail without entering the callback or creating a session" $ bounded $ do
        options <- fixtureOptions
        entered <- newIORef False
        result <- try @RpcResultError (withResumedDroidSession options "missing" (\_ -> writeIORef entered True))
        readIORef entered >>= (@?= False)
        assertRemoteStartupError RpcEntityNotFound result,
      testCase "rejected resume model overrides fail before exposing the session" $ bounded $ do
        options <- fixtureOptions
        entered <- newIORef False
        result <- try @RpcResultError (withResumedDroidSession (options {droidModel = Just "fixture-reject"}) "saved-session" (\_ -> writeIORef entered True))
        readIORef entered >>= (@?= False)
        assertRemoteStartupError RpcInvalidParams result,
      testGroup
        "malformed load results"
        [ testCase (Text.unpack identifier) $ bounded $ do
            options <- fixtureOptions
            entered <- newIORef False
            result <- try @DroidError (withResumedDroidSession options identifier (\_ -> writeIORef entered True))
            result @?= Left DroidInvalidEvent
            readIORef entered >>= (@?= False)
        | identifier <- ["invalid-load", "invalid-history", "invalid-settings"]
        ],
      testCase "cancelling a resumed prompt interrupts and reaps its new child" $ bounded $ do
        options <- fixtureOptions
        ready <- newEmptyMVar
        identifier <- newIORef ""
        withAsync
          ( withResumedDroidSession options "saved-session" $ \session -> do
              peer <- sendPrompt session "peer-pid" (\_ -> pure ())
              writeIORef identifier (resultText peer)
              sendPrompt session "hang" (\_ -> putMVar ready ())
          )
          $ \worker -> do
            takeMVar ready
            cancel worker
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled resumed prompt succeeded"
        readIORef identifier >>= assertReaped,
      testCase "scoped model discovery preserves options, extensions and typed availability" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          forM_ [Nothing, Just False, Just True] $ \include -> do
            let params = ListModelsOptions include (KeyMap.singleton "probe" (String "value"))
            catalog <- listDroidModels session params
            map (metadataId . modelMetadata) (catalogModels catalog) @?= if include == Just True then ["available", "blocked"] else ["available"]
            map modelAvailability (catalogModels catalog) @?= if include == Just True then [ModelEnabled, ModelDisabled "fixture policy"] else [ModelEnabled]
            KeyMap.lookup "observedParams" (catalogAdditionalFields catalog) @?= Just (toJSON params)
          turn <- sendPrompt session "turn" (\_ -> pure ())
          resultText turn @?= "1"
          resultText <$> sendPrompt session "peer-pid" (\_ -> pure ())
        assertReaped peer,
      testCase "scoped context queries return typed snapshots on resumed sessions" $ bounded $ do
        options <- fixtureOptions
        peer <- withResumedDroidSession options "saved-session" $ \session -> do
          stats <- getDroidContextStats session
          contextUsed stats @?= 12.5
          contextRemaining stats @?= 87.5
          contextAccuracy stats @?= ContextEstimated
          KeyMap.lookup "fixture" (contextStatsAdditionalFields stats) @?= Just (Bool True)
          breakdown <- getDroidContextBreakdown session
          breakdownModelId breakdown @?= "saved-model"
          breakdownUsedTokens breakdown @?= 12.5
          breakdownFreeTokens breakdown @?= 87.5
          length (breakdownCategories breakdown) @?= 1
          resultText <$> sendPrompt session "peer-pid" (\_ -> pure ())
        assertReaped peer,
      testCase "context queries are safe inside a text callback" $ bounded $ do
        options <- fixtureOptions
        called <- newIORef False
        identifier <- withDroidSession options $ \session -> do
          result <- sendPrompt session "hello" $ \text -> when (text == "Hello ") $ do
            writeIORef called True
            stats <- getDroidContextStats session
            contextUsed stats @?= 12.5
          resultText result @?= "Hello سلام\n😀"
          pure (droidSessionId session)
        readIORef called >>= (@?= True)
        assertReaped identifier,
      testCase "query result and remote errors do not invalidate an otherwise usable session" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          invalid <- try @RpcResultError (listDroidModels session (ListModelsOptions Nothing (KeyMap.singleton "fixture" (String "invalid"))))
          invalid @?= Left RpcInvalidResult
          remote <- try @RpcResultError (listDroidModels session (ListModelsOptions Nothing (KeyMap.singleton "fixture" (String "error"))))
          case remote of
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
            _ -> assertFailure "Expected structured query error"
          turn <- sendPrompt session "turn" (\_ -> pure ())
          resultText turn @?= "1"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "queries reject sessions invalidated by malformed turn events" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          failed <- try @DroidError (sendPrompt session "malformed" (\_ -> pure ()))
          failed @?= Left DroidInvalidEvent
          stats <- try @DroidError (getDroidContextStats session)
          stats @?= Left DroidSessionUnusable
          breakdown <- try @DroidError (getDroidContextBreakdown session)
          breakdown @?= Left DroidSessionUnusable
          models <- try @DroidError (listDroidModels session (ListModelsOptions Nothing mempty))
          models @?= Left DroidSessionUnusable
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "rename preserves real success flags and empty titles" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          renamed <- renameDroidSession session "new title"
          resultSuccess renamed @?= True
          KeyMap.lookup "title" (resultAdditionalFields renamed) @?= Just (String "new title")
          denied <- renameDroidSession session "denied"
          resultSuccess denied @?= False
          empty <- renameDroidSession session ""
          resultSuccess empty @?= True
          KeyMap.lookup "title" (resultAdditionalFields empty) @?= Just (String "")
          turn <- sendPrompt session "turn" (\_ -> pure ())
          resultText turn @?= "1"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "directory control returns the peer path without changing the caller directory" $ bounded $ do
        options <- fixtureOptions
        before <- getWorkingDirectory
        identifier <- withDroidSession options $ \session -> do
          result <- changeDroidWorkingDirectory session "nested/../target"
          changedResolvedPath result @?= "/fixture/target"
          KeyMap.lookup "requested" (changedDirectoryAdditionalFields result) @?= Just (String "nested/../target")
          getWorkingDirectory >>= (@?= before)
          pure (droidSessionId session)
        getWorkingDirectory >>= (@?= before)
        assertReaped identifier,
      testCase "ordinary controls and rewind information work from resumed text callbacks" $ bounded $ do
        options <- fixtureOptions
        called <- newIORef False
        peer <- withResumedDroidSession options "saved-session" $ \session -> do
          result <- sendPrompt session "hello" $ \text -> when (text == "Hello ") $ do
            writeIORef called True
            renamed <- renameDroidSession session "callback title"
            resultSuccess renamed @?= True
            info <- getDroidRewindInfo session "message-42"
            length (rewindAvailableFiles info) @?= 1
            length (rewindCreatedFiles info) @?= 1
            length (rewindEvictedFiles info) @?= 1
            KeyMap.lookup "requestedMessage" (rewindInfoAdditionalFields info) @?= Just (String "message-42")
          resultText result @?= "Hello سلام\n😀"
          resultText <$> sendPrompt session "peer-pid" (\_ -> pure ())
        readIORef called >>= (@?= True)
        assertReaped peer,
      testCase "ordinary control failures preserve the session and structured errors" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          denied <- try @RpcResultError (changeDroidWorkingDirectory session "denied")
          case denied of
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
            _ -> assertFailure "Expected structured directory error"
          invalid <- try @RpcResultError (changeDroidWorkingDirectory session "invalid-result")
          invalid @?= Left RpcInvalidResult
          invalidRename <- try @RpcResultError (renameDroidSession session "invalid-result")
          invalidRename @?= Left RpcInvalidResult
          renamed <- renameDroidSession session "still usable"
          resultSuccess renamed @?= True
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "ordinary controls reject invalidated handles" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          failed <- try @DroidError (sendPrompt session "malformed" (\_ -> pure ()))
          failed @?= Left DroidInvalidEvent
          renamed <- try @DroidError (renameDroidSession session "ignored")
          renamed @?= Left DroidSessionUnusable
          changed <- try @DroidError (changeDroidWorkingDirectory session "ignored")
          changed @?= Left DroidSessionUnusable
          rewind <- try @DroidError (getDroidRewindInfo session "ignored")
          rewind @?= Left DroidSessionUnusable
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "fork retires the source and retains one scoped CLI connection" $ bounded $ do
        options <- fixtureOptions
        (peer, source, successor) <- withDroidSession options $ \session -> do
          droidSessionStatus session >>= (@?= SessionReady)
          peer <- peerIdentifier session
          void (sendDroidEvents session AllEvents "mission-events" (\_ -> pure ()))
          getDroidMissionSnapshot session >>= (@?= Just MissionRunning) . fmap missionSnapshotState
          successor <- forkDroidSession session defaultFork
          droidSessionStatus session >>= (@?= SessionReplaced (droidSessionId successor))
          droidSessionStatus successor >>= (@?= SessionReady)
          getDroidMissionSnapshot successor >>= (@?= Nothing)
          expectDroidError (DroidSessionReplaced (droidSessionId successor)) (getDroidMissionSnapshot session)
          expectDroidError (DroidSessionReplaced (droidSessionId successor)) (getDroidContextStats session)
          peerIdentifier successor >>= (@?= peer)
          pure (peer, session, successor)
        assertReaped peer
        droidSessionStatus source >>= (@?= SessionReplaced (droidSessionId successor))
        droidSessionStatus successor >>= (@?= SessionUnavailable)
        expectDroidError DroidSessionUnusable (getDroidMissionSnapshot successor)
        expectDroidError DroidSessionUnusable (getDroidContextStats successor),
      testCase "compaction and rewind share replacement ownership and preserve metadata" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \source -> do
          peer <- peerIdentifier source
          (compacted, compactedResult) <- compactDroidSession source (CompactSessionParams (Just "retain decisions") mempty)
          compactionNewSessionId compactedResult @?= droidSessionId compacted
          compactionRemovedCount compactedResult @?= 2.5
          let choices = DroidRewindOptions "message-target" [RewindFileSnapshot "restore-me" "hash" 12 mempty] [RewindFileCreation "delete-me" mempty] "rewound"
          (rewound, rewindResult) <- rewindDroidSession compacted choices
          rewindNewSessionId rewindResult @?= droidSessionId rewound
          rewindRestoredCount rewindResult @?= 1
          rewindDeletedCount rewindResult @?= 0
          rewindFailedDeleteCount rewindResult @?= 1
          droidSessionStatus source >>= (@?= SessionReplaced (droidSessionId compacted))
          droidSessionStatus compacted >>= (@?= SessionReplaced (droidSessionId rewound))
          peerIdentifier rewound >>= (@?= peer)
          pure peer
        assertReaped peer,
      testCase "an explicit replacement rejection preserves the original handle" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          result <- try @RpcResultError (forkDroidSession session (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureFailure" (String "remote")}))
          case result of
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
            _ -> assertFailure "Expected replacement rejection"
          droidSessionStatus session >>= (@?= SessionReady)
          _ <- getDroidContextStats session
          pure peer
        assertReaped peer,
      testCase "an uncertain replacement result invalidates the source" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          result <- try @RpcResultError (forkDroidSession session (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureFailure" (String "invalid")}))
          case result of
            Left err -> err @?= RpcInvalidResult
            Right _ -> assertFailure "Malformed replacement succeeded"
          droidSessionStatus session >>= (@?= SessionUnavailable)
          expectDroidError DroidSessionUnusable (getDroidContextStats session)
          pure peer
        assertReaped peer,
      testCase "failed successor loading reloads the original before restoring usability" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          result <- try @DroidReplacementError (forkDroidSession session (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureLoadFailure" (Bool True)}))
          case result of
            Left failure -> do
              replacementSource failure @?= droidSessionId session
              case replacementRollbackError failure of
                Nothing -> pure ()
                Just _ -> assertFailure "Rollback unexpectedly failed"
              case fromException (replacementCause failure) of
                Just (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcEntityNotFound
                _ -> assertFailure "Lost original load failure"
              history <- sendPrompt session "loads" (\_ -> pure ())
              resultText history @?= Text.intercalate "," [replacementTarget failure, droidSessionId session]
            Right _ -> assertFailure "Rejected successor was exposed"
          droidSessionStatus session >>= (@?= SessionReady)
          pure peer
        assertReaped peer,
      testCase "failed rollback preserves both failures and invalidates the source" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          let flags = KeyMap.fromList ["fixtureLoadFailure" .= True, "fixtureRollbackFailure" .= True]
          result <- try @DroidReplacementError (forkDroidSession session (defaultFork {forkSessionAdditionalFields = flags}))
          case result of
            Left failure -> do
              case fromException (replacementCause failure) of
                Just (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcEntityNotFound
                _ -> assertFailure "Missing original failure"
              case replacementRollbackError failure >>= fromException of
                Just (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcEntityNotFound
                _ -> assertFailure "Missing rollback failure"
            Right _ -> assertFailure "Failed rollback returned a successor"
          droidSessionStatus session >>= (@?= SessionUnavailable)
          expectDroidError DroidSessionUnusable (renameDroidSession session "unusable")
          pure peer
        assertReaped peer,
      testCase "replacement from an active text callback fails without deadlocking" $ bounded $ do
        options <- fixtureOptions
        called <- newIORef False
        identifier <- withDroidSession options $ \session -> do
          result <- sendPrompt session "hello" $ \text -> when (text == "Hello ") $ do
            writeIORef called True
            droidSessionStatus session >>= (@?= SessionRunning)
            expectDroidError DroidSessionBusy (forkDroidSession session defaultFork)
          resultText result @?= "Hello سلام\n😀"
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        readIORef called >>= (@?= True)
        assertReaped identifier,
      testCase "replacement drains requests and rejects queued work on the retired handle" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          let query = listDroidModels session (ListModelsOptions Nothing (KeyMap.singleton "fixtureGate" (String (Text.pack gate))))
          withAsync query $ \queryWorker -> do
            waitGate gate
            withAsync (forkDroidSession session defaultFork) $ \replacementWorker -> do
              waitStatus session SessionReplacing
              expectDroidError DroidSessionBusy (getDroidContextStats session)
              timeout 20000 (wait replacementWorker) >>= \case
                Nothing -> pure ()
                Just _ -> assertFailure "Replacement completed before the held query"
              withAsync (sendPrompt session "turn" (\_ -> pure ())) $ \queuedPrompt -> do
                releaseGate gate
                _ <- wait queryWorker
                successor <- wait replacementWorker
                expectDroidError (DroidSessionReplaced (droidSessionId successor)) (wait queuedPrompt)
                peerIdentifier successor >>= (@?= peer)
          pure peer
        assertReaped peer,
      testCase "cancelling before replacement mutation leaves the original usable" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          let query = listDroidModels session (ListModelsOptions Nothing (KeyMap.singleton "fixtureGate" (String (Text.pack gate))))
          withAsync query $ \queryWorker -> do
            waitGate gate
            withAsync (forkDroidSession session defaultFork) $ \replacementWorker -> do
              waitStatus session SessionReplacing
              throwTo (asyncThreadId replacementWorker) AsyncCancelled
              waitCatch replacementWorker >>= \case
                Left err -> fromException err @?= Just AsyncCancelled
                Right _ -> assertFailure "Cancelled replacement succeeded"
              droidSessionStatus session >>= (@?= SessionReady)
            releaseGate gate
            _ <- wait queryWorker
            pure ()
          history <- sendPrompt session "loads" (\_ -> pure ())
          resultText history @?= ""
          pure peer
        assertReaped peer,
      testCase "cancelling a sent replacement with unknown outcome invalidates the source" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          let flags = KeyMap.singleton "fixtureGate" (String (Text.pack gate))
          withAsync (forkDroidSession session (defaultFork {forkSessionAdditionalFields = flags})) $ \worker -> do
            waitGate gate
            throwTo (asyncThreadId worker) AsyncCancelled
            releaseGate gate
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled replacement succeeded"
          droidSessionStatus session >>= (@?= SessionUnavailable)
          expectDroidError DroidSessionUnusable (getDroidContextStats session)
          pure peer
        assertReaped peer,
      testCase "cancelled successor load preserves AsyncCancelled and safe ownership" $ bounded $ cancelReplacementLoad AsyncCancelled,
      testCase "cancelled successor load preserves custom asynchronous exceptions" $ bounded $ cancelReplacementLoad CallbackAbort,
      testCase "a cancelled ordinary mutation prevents the waiting replacement from crossing it" $ bounded $ cancelMutation False,
      testCase "repeated cancellation cannot bypass mutation invalidation" $ bounded $ cancelMutation True,
      testCase "cancellation during rollback invalidates without swallowing cancellation" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          let flags = KeyMap.fromList ["fixtureLoadFailure" .= True, "fixtureRollbackGate" .= Text.pack gate]
          withAsync (forkDroidSession session (defaultFork {forkSessionAdditionalFields = flags})) $ \worker -> do
            waitGate gate
            throwTo (asyncThreadId worker) AsyncCancelled
            releaseGate gate
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled rollback succeeded"
          trace <- readRequestTrace gate
          traceLoadTargets trace @?= [droidSessionId session]
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure peer
        assertReaped peer,
      testCase "cancellation after retirement leaves the committed successor owned by the scope" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          peer <- peerIdentifier session
          committed <- newEmptyMVar
          hold <- newEmptyMVar @()
          withAsync (forkDroidSession session defaultFork >>= \successor -> putMVar committed successor >> takeMVar hold) $ \worker -> do
            successor <- takeMVar committed
            throwTo (asyncThreadId worker) AsyncCancelled
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled caller action succeeded"
            droidSessionStatus session >>= (@?= SessionReplaced (droidSessionId successor))
            droidSessionStatus successor >>= (@?= SessionReady)
            peerIdentifier successor >>= (@?= peer)
          pure peer
        assertReaped peer,
      testCase "independent session replacements do not share locks or ownership" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        firstPeer <- withDroidSession options $ \first -> do
          firstPeer <- peerIdentifier first
          withAsync (forkDroidSession first (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureGate" (String (Text.pack gate))})) $ \worker -> do
            waitGate gate
            secondPeer <- withDroidSession options $ \second -> do
              successor <- forkDroidSession second defaultFork
              peerIdentifier successor
            when (firstPeer == secondPeer) (assertFailure "Independent sessions shared a child")
            assertReaped secondPeer
            releaseGate gate
            successor <- wait worker
            peerIdentifier successor >>= (@?= firstPeer)
          pure firstPeer
        assertReaped firstPeer,
      testCase "load-history notifications do not leak into successor or rollback prompts" $ bounded $ do
        options <- fixtureOptions
        forM_ [False, True] $ \failLoad -> do
          peer <- withDroidSession options $ \session -> do
            peer <- peerIdentifier session
            let flags = KeyMap.fromList ["fixtureReplay" .= True, "fixtureLoadFailure" .= failLoad]
            result <- try @DroidReplacementError (forkDroidSession session (defaultFork {forkSessionAdditionalFields = flags}))
            active <- case result of
              Right successor -> do
                when failLoad (assertFailure "Rejected successor was exposed")
                pure successor
              Left failure -> do
                unless failLoad (assertFailure "Successor unexpectedly failed")
                case replacementRollbackError failure of
                  Nothing -> pure ()
                  Just _ -> assertFailure "Rollback unexpectedly failed"
                pure session
            turn <- sendPrompt active "hello" (\_ -> pure ())
            resultText turn @?= "Hello سلام\n😀"
            pure peer
          assertReaped peer,
      testCase "terminal results preserve typed completion usage and errors" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidTurn session "hello" (\_ -> pure ())
          turnCompletionReason (resultCompletion result) @?= TurnCompleted
          usageInputTokens (turnTokenUsage (resultCompletion result)) @?= 10
          usageOutputTokens (turnTokenUsage (resultCompletion result)) @?= 2
          usageFactoryCredits (turnTokenUsage (resultCompletion result)) @?= Just 0.25
          resultErrors result @?= []
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "reported errors remain data until a failed completion settles the turn" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidTurn session "reported-error" (\_ -> pure ())
          resultText result @?= "partial"
          turnCompletionReason (resultCompletion result) @?= TurnError
          map errorNotificationMessage (resultErrors result) @?= ["fixture failure"]
          droidSessionStatus session >>= (@?= SessionReady)
          next <- sendPrompt session "hello" (\_ -> pure ())
          resultText next @?= "Hello سلام\n😀"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "spec handoff succeeds and permission rejection remains a typed settled result" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          spec <- sendPrompt session "spec" (\_ -> pure ())
          turnCompletionReason (resultCompletion spec) @?= TurnSpecHandoff
          denied <- sendDroidTurn session "permission-denied" (\_ -> pure ())
          turnCompletionReason (resultCompletion denied) @?= TurnPermissionRejected
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "turn identifiers stay distinct across new and resumed processes" $ bounded $ do
        options <- fixtureOptions
        first <- runDroid options "hello"
        assertReaped (resultSessionId first)
        (second, peer) <- withResumedDroidSession options (resultSessionId first) $ \session -> do
          second <- sendDroidTurn session "hello" (\_ -> pure ())
          peer <- peerIdentifier session
          pure (second, peer)
        case (completedTurnId (resultCompletion first), completedTurnId (resultCompletion second)) of
          (Just one, Just two) -> do
            Text.length one @?= 36
            Text.length two @?= 36
            when (one == two) (assertFailure "A durable turn identifier was reused")
          _ -> assertFailure "Missing correlated turn identifier"
        assertReaped peer,
      testCase "foreign completion cannot terminate the current turn" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "foreign-turn"
        resultText result @?= "current turn"
        turnCompletionReason (resultCompletion result) @?= TurnCompleted
        assertReaped (resultSessionId result),
      testCase "completion without the requested turn identifier invalidates the turn" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          expectDroidError DroidInvalidEvent (sendDroidTurn session "missing-turn-id" (\_ -> pure ()))
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "explicit interruption from a callback returns partial text and permits reuse" $ bounded $ do
        options <- fixtureOptions
        called <- newIORef False
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidTurn session "hang" $ \text -> when (text == "started") $ do
            writeIORef called True
            interruptDroidSession session
          resultText result @?= "started"
          turnCompletionReason (resultCompletion result) @?= TurnCancelled
          droidSessionStatus session >>= (@?= SessionReady)
          next <- sendPrompt session "turn" (\_ -> pure ())
          resultText next @?= "2"
          pure (droidSessionId session)
        readIORef called >>= (@?= True)
        assertReaped identifier,
      testCase "idle interrupts send nothing and retired handles cannot interrupt successors" $ bounded $ do
        options <- fixtureOptions
        peer <- withDroidSession options $ \session -> do
          interruptDroidSession session
          count <- sendPrompt session "interrupt-count" (\_ -> pure ())
          resultText count @?= "0"
          successor <- forkDroidSession session defaultFork
          expectDroidError (DroidSessionReplaced (droidSessionId successor)) (interruptDroidSession session)
          interruptDroidSession successor
          peerIdentifier successor
        assertReaped peer,
      testCase "a delayed interrupt acknowledgement fences the next prompt" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        started <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          withAsync (sendDroidTurn session ("stop-gate:" <> Text.pack gate) (\_ -> putMVar started ())) $ \first -> do
            takeMVar started
            withAsync (interruptDroidSession session) $ \stop -> do
              waitGate gate
              result <- wait first
              turnCompletionReason (resultCompletion result) @?= TurnCancelled
              withAsync (sendDroidTurn session "turn" (\_ -> pure ())) $ \next -> do
                timeout 20000 (wait next) >>= \case
                  Nothing -> pure ()
                  Just _ -> assertFailure "A new prompt crossed a pending interrupt"
                releaseGate gate
                wait stop
                resultNext <- wait next
                resultText resultNext @?= "2"
                turnCompletionReason (resultCompletion resultNext) @?= TurnCompleted
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "interruption waits for the current prompt submission acknowledgement" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          withAsync (sendDroidTurn session ("submit-gate:" <> Text.pack gate) (\_ -> pure ())) $ \turn -> do
            waitGate gate
            withAsync (interruptDroidSession session) $ \stop -> do
              timeout 20000 (wait stop) >>= (@?= Nothing)
              releaseGate gate
              wait stop
              result <- wait turn
              turnCompletionReason (resultCompletion result) @?= TurnCancelled
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "a rejected interrupt leaves the active turn available for retry" $ bounded $ do
        options <- fixtureOptions
        started <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          withAsync (sendDroidTurn session "interrupt-denied" (\_ -> putMVar started ())) $ \turn -> do
            takeMVar started
            rejected <- try @RpcResultError (interruptDroidSession session)
            case rejected of
              Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcConflict
              _ -> assertFailure "Expected an interrupt rejection"
            droidSessionStatus session >>= (@?= SessionRunning)
            interruptDroidSession session
            result <- wait turn
            turnCompletionReason (resultCompletion result) @?= TurnCancelled
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "cancelling an unacknowledged interrupt prevents a following turn" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        started <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          withAsync (sendDroidTurn session ("stop-gate:" <> Text.pack gate) (\_ -> putMVar started ())) $ \first -> do
            takeMVar started
            withAsync (interruptDroidSession session) $ \stop -> do
              waitGate gate
              _ <- wait first
              withAsync (sendDroidTurn session "turn" (\_ -> pure ())) $ \next -> do
                throwTo (asyncThreadId stop) AsyncCancelled
                releaseGate gate
                waitCatch stop >>= \case
                  Left err -> fromException err @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled interrupt succeeded"
                expectDroidError DroidSessionUnusable (wait next)
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "cancelling an interrupt before submission does not invalidate the unsent interrupt" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          withAsync (sendDroidTurn session ("submit-gate:" <> Text.pack gate) (\_ -> pure ())) $ \turn -> do
            waitGate gate
            withAsync (interruptDroidSession session) $ \stop -> do
              timeout 20000 (wait stop) >>= (@?= Nothing)
              cancel stop
              waitCatch stop >>= \case
                Left err -> fromException err @?= Just AsyncCancelled
                Right _ -> assertFailure "Cancelled pre-submission interrupt succeeded"
            droidSessionStatus session >>= (@?= SessionRunning)
            releaseGate gate
            interruptDroidSession session
            result <- wait turn
            turnCompletionReason (resultCompletion result) @?= TurnCancelled
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "success-checking prompt reports interruption without invalidating settled state" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          result <- try @DroidError (sendPrompt session "hang" (\_ -> interruptDroidSession session))
          result @?= Left (DroidTurnFailed TurnCancelled)
          droidSessionStatus session >>= (@?= SessionReady)
          next <- sendPrompt session "hello" (\_ -> pure ())
          resultText next @?= "Hello سلام\n😀"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "sessionless provisional mission state binds once and follows associated children across load boundaries" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession (options {droidModel = Just "mission-provisional"}) $ \session -> do
          initial <- getDroidMissionSnapshot session >>= maybe (assertFailure "Missing provisional mission") pure
          missionSnapshotState initial @?= MissionRunning
          missionSnapshotTitle initial @?= Just "before reply"
          missionSnapshotWorkers initial @?= ["mission-child"]
          fmap usageInputTokens (missionSnapshotTokenUsage initial) @?= Just 7
          delivered <- newEmptyMVar
          observed <- newIORef (0 :: Int)
          stop <- onDroidMissionSnapshot session $ \case
            Left cause -> assertFailure (show cause)
            Right snapshot -> do
              modifyIORef' observed (+ 1)
              fmap missionSnapshotState snapshot @?= Just MissionPaused
              putMVar delivered ()
          ordinary <- newIORef []
          void (sendDroidEvents session AllEvents "mission-child-events" (\event -> modifyIORef' ordinary (<> [event])))
          takeMVar delivered
          readIORef ordinary >>= (@?= []) . mapMaybe missionEventPayload
          successor <- forkDroidSession session defaultFork
          getDroidMissionSnapshot successor >>= (@?= Just MissionCompleted) . fmap missionSnapshotState
          getDroidMissionSnapshot successor >>= (@?= Just (Just ("loaded " <> droidSessionId successor))) . fmap missionSnapshotTitle
          expectDroidError (DroidSessionReplaced (droidSessionId successor)) (getDroidMissionSnapshot session)
          expectDroidError (DroidSessionReplaced (droidSessionId successor)) (onDroidMissionSnapshot session (\_ -> pure ()))
          successorStates <- newIORef []
          stopSuccessor <- onDroidMissionSnapshot successor $ \case
            Left cause -> assertFailure (show cause)
            Right snapshot -> modifyIORef' successorStates (<> [fmap missionSnapshotState snapshot])
          void (sendDroidEvents successor AllEvents "mission-events" (\_ -> pure ()))
          readIORef successorStates >>= (@?= replicate 5 (Just MissionRunning))
          readIORef observed >>= (@?= 1)
          stopSuccessor
          stop
          peerIdentifier successor
        assertReaped identifier,
      testCase "mission events use the local stream and observer without a parallel runtime" $ bounded $ do
        options <- fixtureOptions
        streamed <- newIORef []
        observed <- newIORef []
        delivered <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          getDroidMissionSnapshot session >>= (@?= Nothing)
          stop <- onDroidSessionEvent session $ \case
            Left cause -> assertFailure (show cause)
            Right event -> do
              forM_ (missionEventPayload event) $ \value -> modifyIORef' observed (<> [value])
              case event of
                MissionStateEvent _ -> getDroidMissionSnapshot session >>= (@?= Just MissionRunning) . fmap missionSnapshotState
                MissionProgressEvent _ -> getDroidMissionSnapshot session >>= (@?= Just (Just "Mission title")) . fmap missionSnapshotTitle
                MissionWorkerCompletedEvent _ -> putMVar delivered ()
                _ -> pure ()
          result <- sendDroidEvents session AllEvents "mission-events" (\event -> modifyIORef' streamed (<> [event]))
          takeMVar delivered
          readIORef streamed >>= (@?= missionWireEvents) . mapMaybe missionEventPayload
          readIORef observed >>= (@?= missionWireEvents)
          resultEvents result @?= []
          resultText result @?= ""
          stop
          filtered <- newIORef []
          _ <- sendDroidEvents session CompleteMessages "mission-events" (\event -> modifyIORef' filtered (<> [event]))
          readIORef filtered >>= (@?= []) . mapMaybe missionEventPayload
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "malformed local mission payload is not delivered as a raw extension" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          result <- try @DroidError (sendDroidEvents session AllEvents "bad-mission-event" (\_ -> pure ()))
          result @?= Left DroidInvalidEvent
          expectDroidError DroidSessionUnusable (getDroidMissionSnapshot session)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "all-event streams expose typed messages, tools, hooks and partial events in order" $ bounded $ do
        options <- fixtureOptions
        observed <- newIORef []
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidEvents session AllEvents "rich-events" (\event -> modifyIORef' observed (<> [event]))
          events <- readIORef observed
          map eventTag events @?= ["message", "thinking_delta", "thinking_complete", "tool_call_delta", "tool_call", "message", "tool_progress", "tool_result", "hook_started", "hook_completed", "usage", "working", "permission", "settings", "title", "directory", "other", "error", "text_delta", "text_complete", "message", "structured", "completed"]
          case [value | SettingsUpdatedEvent value <- events] of
            [event] -> do
              settingsUpdateRequestId event @?= Just "settings-request"
              changedSettingsMode (settingsUpdateValues event) @?= Just DroidSpec
              changedSettingsAutonomy (settingsUpdateValues event) @?= Nothing
              changedSettingsAvailableAutonomy (settingsUpdateValues event) @?= Just [AutonomyOff, AutonomyLow]
              changedSettingsSpecModel (settingsUpdateValues event) @?= Just "reported-spec"
            _ -> assertFailure "Missing typed settings event"
          map eventTag (resultEvents result) @?= completeRichTags
          resultText result @?= "final answer"
          resultStructuredOutput result @?= Just (KeyMap.singleton "answer" (Number 42))
          map errorNotificationMessage (resultErrors result) @?= ["recoverable observation"]
          [fields | OtherNotificationEvent fields <- events] @?= [KeyMap.fromList ["type" .= String "future_notice", "private" .= String "fixture secret"]]
          case [toJSON tool | ToolCallEvent tool <- events] of
            [Object fields] -> KeyMap.lookup "name" fields @?= Just (String "Read")
            _ -> assertFailure "Missing complete tool call"
          case [toJSON tool | ToolResultEvent tool <- events] of
            [Object fields] -> KeyMap.lookup "content" fields @?= Just (String "file data")
            _ -> assertFailure "Missing complete tool result"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "complete-message mode filters callbacks without losing terminal data" $ bounded $ do
        options <- fixtureOptions
        observed <- newIORef []
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidEvents session CompleteMessages "rich-events" (\event -> modifyIORef' observed (<> [event]))
          events <- readIORef observed
          map eventTag events @?= completeRichTags <> ["completed"]
          map eventTag (resultEvents result) @?= completeRichTags
          resultText result @?= "final answer"
          resultStructuredOutput result @?= Just (KeyMap.singleton "answer" (Number 42))
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "complete message snapshots determine final text without duplicating text callbacks" $ bounded $ do
        options <- fixtureOptions
        text <- newIORef ""
        result <- streamDroid options "snapshot-correction" (\piece -> modifyIORef' text (<> piece))
        readIORef text >>= (@?= "draft")
        resultText result @?= "final"
        map eventTag (resultEvents result) @?= ["message"]
        assertReaped (resultSessionId result),
      testGroup
        "terminal text retains partial updates after a snapshot"
        [ testCase (name <> "/" <> Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            result <- withDroidSession options $ \session -> do
              result <- run session prompt
              turnCompletionReason (resultCompletion result) @?= TurnCancelled
              droidSessionStatus session >>= (@?= SessionReady)
              pure result
            resultText result @?= expected
            assertReaped (resultSessionId result)
        | (name, run) <-
            [ ("text", \session prompt -> sendDroidTurn session prompt (\_ -> pure ())),
              ("all", \session prompt -> sendDroidEvents session AllEvents prompt (\_ -> pure ())),
              ("complete", \session prompt -> sendDroidEvents session CompleteMessages prompt (\_ -> pure ()))
            ],
          (prompt, expected) <- [("later-partial", "Answer so far"), ("snapshot-continued", "Hello world")]
        ],
      testCase "snapshot-only prefix extensions reach text callbacks once" $ bounded $ do
        options <- fixtureOptions
        text <- newIORef ""
        result <- streamDroid options "snapshot-prefixes" (\piece -> modifyIORef' text (<> piece))
        readIORef text >>= (@?= "Hello")
        resultText result @?= "Hello"
        assertReaped (resultSessionId result),
      testGroup
        "text delivery survives corrections without replaying prefixes"
        [ testCase (Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            text <- newIORef ""
            result <- streamDroid options prompt (\piece -> modifyIORef' text (<> piece))
            readIORef text >>= (@?= delivered)
            resultText result @?= final
            assertReaped (resultSessionId result)
        | (prompt, delivered, final) <-
            [ ("snapshot-regrowth", "Hello", "Hello"),
              ("snapshot-regrowth-extra", "Hello!", "Hello!"),
              ("delta-regrowth", "Hello", "Hello"),
              ("divergent-update", "draft", "final answer"),
              ("retraction-replay", "Hello", "Hello")
            ]
        ],
      testCase "retraction removes final text and associated structured output while retaining the event" $ bounded $ do
        options <- fixtureOptions
        observed <- newIORef []
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidEvents session CompleteMessages "retraction" (\event -> modifyIORef' observed (<> [eventTag event]))
          resultText result @?= "surviving"
          resultStructuredOutput result @?= Nothing
          readIORef observed >>= (@?= ["message", "retracted", "completed"])
          map eventTag (resultEvents result) @?= ["message", "retracted"]
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "explicit null clears previously reported structured output" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "structured-null"
        resultStructuredOutput result @?= Nothing
        assertReaped (resultSessionId result),
      testGroup
        "known malformed stream payloads never become raw fallbacks"
        [ testCase (show mode <> "/" <> Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            identifier <- withDroidSession options $ \session -> do
              expectDroidError DroidInvalidEvent (sendDroidEvents session mode prompt (\_ -> pure ()))
              droidSessionStatus session >>= (@?= SessionUnavailable)
              pure (droidSessionId session)
            assertReaped identifier
        | mode <- [CompleteMessages, AllEvents],
          prompt <- ["malformed-partial", "malformed-message"]
        ],
      testGroup
        "event callback failures retain exception identity and invalidate"
        [ testCase (Text.unpack target) $ bounded $ do
            options <- fixtureOptions
            called <- newIORef False
            identifier <- withDroidSession options $ \session -> do
              failure <- try @CallbackAbort (sendDroidEvents session AllEvents "rich-events" (\event -> when (eventTag event == target) (writeIORef called True >> throwIO CallbackAbort)))
              failure @?= Left CallbackAbort
              readIORef called >>= (@?= True)
              droidSessionStatus session >>= (@?= SessionUnavailable)
              pure (droidSessionId session)
            assertReaped identifier
        | target <- ["tool_call", "completed"]
        ],
      testCase "typed event callbacks retain explicit interruption and session reuse" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidEvents session AllEvents "hang" $ \case
            TextDeltaEvent _ -> interruptDroidSession session
            _ -> pure ()
          turnCompletionReason (resultCompletion result) @?= TurnCancelled
          droidSessionStatus session >>= (@?= SessionReady)
          next <- sendPrompt session "hello" (\_ -> pure ())
          resultText next @?= "Hello سلام\n😀"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "raw event payloads are redacted by Show" $
        show (OtherNotificationEvent (KeyMap.singleton "private" (String "fixture secret"))) @?= "DroidEvent <redacted>",
      testGroup
        "output specifications require explicit object schemas"
        [ testCase name $ do
            void (rawDroidOutput schema) @?= Left DroidOutputSchemaNotObject
            void (jsonDroidOutput @OutputAnswer schema) @?= Left DroidOutputSchemaNotObject
        | (name, schema) <-
            [ ("absent", mempty),
              ("array", KeyMap.singleton "type" (String "array")),
              ("string", KeyMap.singleton "type" (String "string")),
              ("null", KeyMap.singleton "type" Null),
              ("union", KeyMap.singleton "type" (toJSON [String "object", String "null"]))
            ]
        ],
      testCase "typed output requests preserve the schema and prefer notification data" $ bounded $ do
        options <- fixtureOptions
        output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
        text <- newIORef ""
        outcome <- withDroidSession options $ \session -> sendDroidOutput session output "output-notification" (\piece -> modifyIORef' text (<> piece))
        outputValue outcome @?= Right (OutputAnswer 42)
        resultStructuredOutput (outputTurnResult outcome) @?= Just (KeyMap.singleton "answer" (Number 42))
        readIORef text >>= (@?= "{\"answer\":99}")
        assertReaped (resultSessionId (outputTurnResult outcome)),
      testGroup
        "output adaptation does not replace rich events"
        [ testCase (show mode) $ bounded $ do
            options <- fixtureOptions
            output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
            observed <- newIORef []
            outcome <- withDroidSession options $ \session -> sendDroidOutputEvents session output mode "output-notification" (\event -> modifyIORef' observed (<> [eventTag event]))
            outputValue outcome @?= Right (OutputAnswer 42)
            readIORef observed >>= (@?= if mode == AllEvents then ["structured", "text_delta", "completed"] else ["completed"])
            assertReaped (resultSessionId (outputTurnResult outcome))
        | mode <- [CompleteMessages, AllEvents]
        ],
      testGroup
        "requested output falls back to plain JSON text"
        [ testCase (Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
            outcome <- withDroidSession options $ \session -> sendDroidOutput session output prompt (\_ -> pure ())
            outputValue outcome @?= Right (OutputAnswer 7)
            resultStructuredOutput (outputTurnResult outcome) @?= Just (KeyMap.singleton "answer" (Number 7))
            assertReaped (resultSessionId (outputTurnResult outcome))
        | prompt <- ["output-text", "output-null-text"]
        ],
      testGroup
        "unusable output remains a local outcome on a settled session"
        [ testCase (Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            output <- either throwIO pure (rawDroidOutput outputTestSchema)
            identifier <- withDroidSession options $ \session -> do
              outcome <- sendDroidOutput session output prompt (\_ -> pure ())
              outputValue outcome @?= Left DroidOutputMissing
              turnCompletionReason (resultCompletion (outputTurnResult outcome)) @?= TurnCompleted
              droidSessionStatus session >>= (@?= SessionReady)
              next <- sendPrompt session "hello" (\_ -> pure ())
              resultText next @?= "Hello سلام\n😀"
              pure (droidSessionId session)
            assertReaped identifier
        | prompt <- ["output-missing", "output-fenced", "output-array", "output-scalar", "output-retracted"]
        ],
      testGroup
        "typed output honors structural and custom FromJSON validation"
        [ testCase (Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
            identifier <- withDroidSession options $ \session -> do
              outcome <- sendDroidOutput session output prompt (\_ -> pure ())
              case outputValue outcome of
                Left (DroidOutputInvalid _) -> pure ()
                _ -> assertFailure "Expected local output validation failure"
              resultStructuredOutput (outputTurnResult outcome) @?= Just raw
              droidSessionStatus session >>= (@?= SessionReady)
              _ <- sendPrompt session "hello" (\_ -> pure ())
              pure (droidSessionId session)
            assertReaped identifier
        | (prompt, raw) <- [("output-invalid-type", KeyMap.singleton "answer" (String "private invalid answer")), ("output-invalid-value", KeyMap.singleton "answer" (Number 0))]
        ],
      testCase "raw output checks objects but does not claim JSON Schema validation" $ bounded $ do
        options <- fixtureOptions
        output <- either throwIO pure (rawDroidOutput outputTestSchema)
        outcome <- withDroidSession options $ \session -> sendDroidOutput session output "output-invalid-type" (\_ -> pure ())
        outputValue outcome @?= Right (KeyMap.singleton "answer" (String "private invalid answer"))
        assertReaped (resultSessionId (outputTurnResult outcome)),
      testCase "successful output decoding does not hide a failed turn" $ bounded $ do
        options <- fixtureOptions
        output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
        identifier <- withDroidSession options $ \session -> do
          outcome <- sendDroidOutput session output "output-failure" (\_ -> pure ())
          outputValue outcome @?= Right (OutputAnswer 42)
          turnCompletionReason (resultCompletion (outputTurnResult outcome)) @?= TurnError
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "plain turns do not infer structured output from JSON text" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "plain-json"
        resultStructuredOutput result @?= Nothing
        resultText result @?= "{\"answer\":7}"
        assertReaped (resultSessionId result),
      testCase "structured output callbacks preserve exception identity and invalidate" $ bounded $ do
        options <- fixtureOptions
        output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
        identifier <- withDroidSession options $ \session -> do
          result <- try @CallbackAbort (sendDroidOutput session output "output-notification" (\_ -> throwIO CallbackAbort))
          result @?= Left CallbackAbort
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "output event callbacks retain interruption and report missing partial output" $ bounded $ do
        options <- fixtureOptions
        output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
        identifier <- withDroidSession options $ \session -> do
          outcome <- sendDroidOutputEvents session output AllEvents "hang" $ \case
            TextDeltaEvent _ -> interruptDroidSession session
            _ -> pure ()
          turnCompletionReason (resultCompletion (outputTurnResult outcome)) @?= TurnCancelled
          outputValue outcome @?= Left DroidOutputMissing
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "malformed output notifications remain protocol failures rather than validation results" $ bounded $ do
        options <- fixtureOptions
        output <- either throwIO pure (rawDroidOutput outputTestSchema)
        identifier <- withDroidSession options $ \session -> do
          expectDroidError DroidInvalidEvent (sendDroidOutput session output "output-malformed" (\_ -> pure ()))
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "output specifications, errors and results redact sensitive fields" $ bounded $ do
        output <- either throwIO pure (rawDroidOutput outputTestSchema)
        show output @?= "DroidOutput <redacted>"
        show (DroidOutputInvalid "private diagnostic") @?= "DroidOutputError <redacted>"
        options <- fixtureOptions
        outcome <- withDroidSession options $ \session -> sendDroidOutput session output "output-invalid-type" (\_ -> pure ())
        show outcome @?= "DroidOutputResult <redacted>"
        assertReaped (resultSessionId (outputTurnResult outcome)),
      testCase "attachment inputs preserve arrays, metadata and the text callback path" $ bounded $ do
        options <- fixtureOptions
        input <- attachmentInput "attachments"
        text <- newIORef ""
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidInput session input (\piece -> modifyIORef' text (<> piece))
          resultText result @?= "attachments received"
          readIORef text >>= (@?= "attachments received")
          _ <- sendPrompt session "hello" (\_ -> pure ())
          pure (droidSessionId session)
        assertReaped identifier,
      testGroup
        "attachment inputs retain complete and all-event modes"
        [ testCase (show mode) $ bounded $ do
            options <- fixtureOptions
            input <- attachmentInput "attachments"
            observed <- newIORef []
            result <- withDroidSession options $ \session -> sendDroidInputEvents session mode input (\event -> modifyIORef' observed (<> [eventTag event]))
            resultText result @?= "attachments received"
            readIORef observed >>= (@?= ["message", "completed"])
            assertReaped (resultSessionId result)
        | mode <- [CompleteMessages, AllEvents]
        ],
      testCase "attachment-only prompts retain empty text" $ bounded $ do
        options <- fixtureOptions
        input <- attachmentInput ""
        result <- withDroidSession options $ \session -> sendDroidInput session input (\_ -> pure ())
        resultText result @?= "attachments received"
        assertReaped (resultSessionId result),
      testCase "attachments combine with typed output and text callbacks" $ bounded $ do
        options <- fixtureOptions
        input <- attachmentInput "attachments"
        output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
        observed <- newIORef ""
        result <- withDroidSession options $ \session -> sendDroidInputOutput session output input (\piece -> modifyIORef' observed (<> piece))
        outputValue result @?= Right (OutputAnswer 42)
        readIORef observed >>= (@?= "{\"answer\":42}")
        assertReaped (resultSessionId (outputTurnResult result)),
      testCase "attachments combine with structured output and rich callbacks" $ bounded $ do
        options <- fixtureOptions
        input <- attachmentInput "attachments"
        output <- either throwIO pure (jsonDroidOutput @OutputAnswer outputTestSchema)
        observed <- newIORef []
        result <- withDroidSession options $ \session -> sendDroidInputOutputEvents session output AllEvents input (\event -> modifyIORef' observed (<> [eventTag event]))
        outputValue result @?= Right (OutputAnswer 42)
        readIORef observed >>= (@?= ["structured", "text_delta", "completed"])
        assertReaped (resultSessionId (outputTurnResult result)),
      testCase "attachment callback failures retain exception identity and invalidate" $ bounded $ do
        options <- fixtureOptions
        input <- attachmentInput "attachments"
        identifier <- withDroidSession options $ \session -> do
          result <- try @CallbackAbort (sendDroidInput session input (\_ -> throwIO CallbackAbort))
          result @?= Left CallbackAbort
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "attachment event callbacks can interrupt without losing session ownership" $ bounded $ do
        options <- fixtureOptions
        input <- attachmentInput "attachments-hang"
        identifier <- withDroidSession options $ \session -> do
          result <- sendDroidInputEvents session AllEvents input $ \case
            TextDeltaEvent _ -> interruptDroidSession session
            _ -> pure ()
          turnCompletionReason (resultCompletion result) @?= TurnCancelled
          droidSessionStatus session >>= (@?= SessionReady)
          _ <- sendPrompt session "hello" (\_ -> pure ())
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "configured frame limits reject oversized inputs without truncating" $ bounded $ do
        options <- fixtureOptions
        image <- either throwIO pure (imageFromBytes ("\x89PNG\r\n\x1a\n" <> BS.replicate 2048 '\0') ImagePNG)
        let input = (droidInput "not-sent") {inputImages = [image]}
        identifier <- withDroidSession (options {droidFrameLimitBytes = 1024}) $ \session -> do
          result <- try @RpcChannelError (sendDroidInput session input (\_ -> pure ()))
          result @?= Left RpcChannelWriteFailure
          droidSessionStatus session >>= (@?= SessionUnavailable)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "aggregate image payloads can exceed the default frame ceiling explicitly" $ bounded $ do
        options <- fixtureOptions
        image <- either throwIO pure (imageFromBytes ("\x89PNG\r\n\x1a\n" <> BS.replicate (5 * 1024 * 1024 - 8) '\0') ImagePNG)
        let input = (droidInput "attachments-large") {inputImages = [image, image]}
        rejected <- withDroidSession options $ \session -> do
          failure <- try @RpcChannelError (sendDroidInput session input (\_ -> pure ()))
          failure @?= Left RpcChannelWriteFailure
          pure (droidSessionId session)
        assertReaped rejected
        result <- withDroidSession (options {droidFrameLimitBytes = 20 * 1024 * 1024}) $ \session -> sendDroidInput session input (\_ -> pure ())
        resultText result @?= "large attachments received"
        assertReaped (resultSessionId result),
      testGroup
        "frame limits must be positive before launching a process"
        [ testCase (show limit) $ bounded $ do
            options <- fixtureOptions
            result <- try @JsonLinesError (withDroidSession (options {droidFrameLimitBytes = limit, droidExecutable = "/does/not/exist"}) (\_ -> pure ()))
            result @?= Left InvalidFrameLimit
        | limit <- [0, -1]
        ],
      testCase "settings patches preserve spec resets, policy lists and peer acknowledgements" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          let patch = emptySettingsUpdate {updateSettingsModel = Just "changed-model", updateSettingsMode = Just DroidSpec, updateSettingsAutonomy = Just AutonomyOff, updateSettingsSpecModel = Just Nothing, updateSettingsSpecReasoning = Just (Just ReasoningHigh), updateSettingsCompactionTokenLimit = Just 123.5, updateSettingsCompactionThresholdEnabled = Just False, updateSettingsToolPolicy = emptyToolPolicy {policyRestrictedTools = Just ["Read", "Read"], policyDisabledTools = Just []}}
          response <- updateDroidSettings session patch
          KeyMap.lookup "observedParams" response @?= Just (toJSON patch)
          current <- sendPrompt session "model" (\_ -> pure ())
          resultText current @?= "changed-model"
          empty <- updateDroidSettings session emptySettingsUpdate
          KeyMap.lookup "observedParams" empty @?= Just (object [])
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "tool discovery keeps identifiers and independent allow flags without applying hypothetical settings" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          before <- getDroidContextBreakdown session
          let query = defaultListToolsOptions {toolQueryModel = Just "hypothetical", toolQueryMode = Just DroidSpec, toolQuerySpecModel = Just Nothing, toolQueryPolicy = emptyToolPolicy {policyEnabledTools = Just ["Execute"]}, toolQuerySkipPermissionsUnsafe = Just True, toolQueryDepth = Just 0}
          result <- listDroidTools session query
          KeyMap.lookup "observedParams" (listedToolsAdditionalFields result) @?= Just (toJSON query)
          case listedTools result of
            [tool] -> do
              execToolId tool @?= "sdk-tool"
              execToolLlmId tool @?= "model-tool"
              execToolDefaultAllowed tool @?= False
              execToolCurrentlyAllowed tool @?= True
            _ -> assertFailure "Expected a tool entry"
          after <- getDroidContextBreakdown session
          breakdownModelId after @?= breakdownModelId before
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "command and skill discovery returns metadata without executing or opening it" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          commands <- listDroidCommands session
          map customCommandName (listedCommands commands) @?= ["fixture-command"]
          map customCommandIsExecutable (listedCommands commands) @?= [Just True]
          skills <- listDroidSkills session
          map skillName (listedSkills skills) @?= ["fixture-skill"]
          map skillFilePath (listedSkills skills) @?= ["/not/opened/SKILL.md"]
          listedSkillsProjectAvailable skills @?= Just False
          result <- setDroidSkillDisabled session (SetSkillDisabledParams "deny" True (Just EditableSkillProject) mempty)
          resultSuccess result @?= False
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "ordinary settings and discovery controls are callback-safe" $ bounded $ do
        options <- fixtureOptions
        invoked <- newIORef False
        identifier <- withDroidSession options $ \session -> do
          _ <- sendPrompt session "prefilled" $ \_ -> do
            writeIORef invoked True
            _ <- updateDroidSettings session (emptySettingsUpdate {updateSettingsModel = Just "callback-model", updateSettingsMode = Just DroidAuto})
            _ <- listDroidTools session defaultListToolsOptions
            _ <- listDroidCommands session
            _ <- listDroidSkills session
            _ <- setDroidSkillDisabled session (SetSkillDisabledParams "fixture-skill" False Nothing mempty)
            pure ()
          readIORef invoked >>= (@?= True)
          current <- getDroidContextBreakdown session
          breakdownModelId current @?= "callback-model"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "handled settings and tool-query errors preserve the session" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          rejected <- try @RpcResultError (updateDroidSettings session (emptySettingsUpdate {updateSettingsModel = Just "fixture-reject"}))
          case rejected of
            Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
            _ -> assertFailure "Expected the peer settings rejection"
          malformed <- try @RpcResultError (updateDroidSettings session (emptySettingsUpdate {updateSettingsAdditionalFields = KeyMap.singleton "fixture" (String "invalid")}))
          malformed @?= Left RpcInvalidResult
          forM_ ["error", "invalid"] $ \failure -> do
            result <- try @RpcResultError (listDroidTools session (defaultListToolsOptions {toolQueryAdditionalFields = KeyMap.singleton "fixture" (String failure)}))
            case result of
              Left _ -> pure ()
              Right _ -> assertFailure "Expected the tool query failure"
          droidSessionStatus session >>= (@?= SessionReady)
          _ <- listDroidTools session defaultListToolsOptions
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "new controls reject retired handles" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          next <- forkDroidSession session defaultFork
          forM_ [void (updateDroidSettings session emptySettingsUpdate), void (listDroidTools session defaultListToolsOptions), void (listDroidCommands session), void (listDroidSkills session), void (setDroidSkillDisabled session (SetSkillDisabledParams "skill" False Nothing mempty))] $ \action ->
            expectDroidError (DroidSessionReplaced (droidSessionId next)) action
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "an uncertain settings mutation invalidates before replacement admission" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        identifier <- withDroidSession options $ \session -> do
          let patch = emptySettingsUpdate {updateSettingsModel = Just "pending-model", updateSettingsAdditionalFields = KeyMap.singleton "fixtureGate" (String (Text.pack gate))}
          withAsync (updateDroidSettings session patch) $ \worker -> do
            waitGate gate
            cancel worker
            waitCatch worker >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled settings mutation succeeded"
          droidSessionStatus session >>= (@?= SessionUnavailable)
          expectDroidError DroidSessionUnusable (forkDroidSession session defaultFork)
          pure (droidSessionId session)
        assertReaped identifier,
      testGroup
        "new-session system prompts use the canonical initialization field"
        [ testCase name $ bounded $ do
            options <- fixtureOptions
            prompt <- maybe (assertFailure "Expected valid system prompt") pure configured
            result <- withDroidSession (options {droidSystemPrompt = Just prompt}) $ \session -> sendPrompt session "system-prompt" (\_ -> pure ())
            eitherDecodeStrict' (Text.encodeUtf8 (resultText result)) @?= Right expected
            assertReaped (resultSessionId result)
        | (name, configured, expected) <-
            [ ("custom", customSystemPrompt "  custom سلام instructions\n", String "  custom سلام instructions\n"),
              ("append", appendedSystemPrompt "Additional instructions", object ["type" .= String "preset", "preset" .= String "droid", "append" .= String "Additional instructions"])
            ]
        ],
      testCase "system prompt omission remains distinct from an explicit override" $ bounded $ do
        options <- fixtureOptions
        result <- runDroid options "system-prompt"
        resultText result @?= "null"
        assertReaped (resultSessionId result),
      testCase "unsupported resume prompt overrides fail before process launch" $ bounded $ do
        options <- fixtureOptions
        prompt <- maybe (assertFailure "Expected valid system prompt") pure (customSystemPrompt "instructions")
        result <- try @DroidError (withResumedDroidSession (options {droidSystemPrompt = Just prompt, droidExecutable = "/does/not/exist"}) "saved-session" (\_ -> pure ()))
        result @?= Left DroidSystemPromptRequiresNewSession,
      testCase "replacement loads do not invent a system-prompt load parameter" $ bounded $ do
        options <- fixtureOptions
        prompt <- maybe (assertFailure "Expected valid system prompt") pure (appendedSystemPrompt "instructions")
        identifier <- withDroidSession (options {droidSystemPrompt = Just prompt}) $ \session -> do
          next <- forkDroidSession session defaultFork
          result <- sendPrompt next "system-prompt" (\_ -> pure ())
          eitherDecodeStrict' (Text.encodeUtf8 (resultText result)) @?= Right (toJSON prompt)
          pure (droidSessionId session)
        assertReaped identifier,
      testGroup
        "malformed settings notifications fail in both stream modes"
        [ testCase (show mode <> "/" <> Text.unpack prompt) $ bounded $ do
            options <- fixtureOptions
            identifier <- withDroidSession options $ \session -> do
              expectDroidError DroidInvalidEvent (sendDroidEvents session mode prompt (\_ -> pure ()))
              droidSessionStatus session >>= (@?= SessionUnavailable)
              pure (droidSessionId session)
            assertReaped identifier
        | mode <- [CompleteMessages, AllEvents],
          prompt <- ["settings-missing", "settings-null-spec"]
        ],
      testCase "configured permission handlers receive typed actions and associated session IDs" $ bounded $ do
        options <- handlerOptions
        seen <- newIORef []
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\request -> modifyIORef' seen (<> [request]) >> allowFixturePermission request)}
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          result <- sendDroidTurn session "handlers-permission" (\_ -> pure ())
          resultText result @?= "permission handled"
          requests <- readIORef seen
          case requests of
            [request] -> do
              permissionAssociatedSessionIds request @?= Just [droidSessionId session]
              map confirmationInfoType (permissionToolUses request) @?= [ConfirmationTypeExec]
            _ -> assertFailure "Expected one permission request"
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "edited spec content may be explicitly empty when that choice was offered" $ bounded $ do
        options <- handlerOptions
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\request -> maybe (assertFailure "Expected offered edit") pure (respondPermission request ConfirmProceedEdit Nothing (Just "")))}
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          result <- sendDroidTurn session "handlers-edit" (\_ -> pure ())
          turnCompletionReason (resultCompletion result) @?= TurnCompleted
          pure (droidSessionId session)
        assertReaped identifier,
      testGroup
        "permission failures cancel without invalidating settled sessions"
        [ testCase name $ bounded $ do
            options <- handlerOptions
            failures <- newIORef []
            let handlers = defaultDroidHandlers {onDroidPermission = Just callback, onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
            identifier <- withDroidSessionHandlers options handlers $ \session -> do
              result <- sendDroidTurn session prompt (\_ -> pure ())
              turnCompletionReason (resultCompletion result) @?= TurnPermissionRejected
              readIORef failures >>= (@?= [expected])
              droidSessionStatus session >>= (@?= SessionReady)
              _ <- sendPrompt session "hello" (\_ -> pure ())
              pure (droidSessionId session)
            assertReaped identifier
        | (name, prompt, callback, expected) <-
            [ ("exception", "handlers-cancel", \_ -> throwIO (userError "private command"), InteractionHandlerFailed PermissionInteraction),
              ("unoffered choice", "handlers-cancel", \_ -> maybe (assertFailure "Expected wire response") pure (mkRequestPermissionResult ConfirmProceedAlways Nothing Nothing mempty), InvalidInteractionResponse PermissionInteraction),
              ("malformed request", "handlers-malformed", \_ -> assertFailure "Malformed request reached user code", InvalidInteractionRequest PermissionInteraction)
            ]
        ],
      testCase "a question-only handler leaves automatic permission rejection enabled" $ bounded $ do
        options <- fixtureOptions
        let handlers = defaultDroidHandlers {onDroidQuestion = Just answerFixtureQuestions}
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          result <- sendDroidTurn session "handlers-question" (\_ -> pure ())
          turnCompletionReason (resultCompletion result) @?= TurnCompleted
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "invalid question answers are cancelled and reported safely" $ bounded $ do
        options <- fixtureOptions
        failures <- newIORef []
        let answer = AskUserResult [AskUserCollectedAnswer 9 "not requested" "private answer" mempty] (Just False) mempty
            handlers = defaultDroidHandlers {onDroidQuestion = Just (\_ -> pure answer), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          _ <- sendDroidTurn session "handlers-question-cancel" (\_ -> pure ())
          readIORef failures >>= (@?= [InvalidInteractionResponse QuestionInteraction])
          droidSessionStatus session >>= (@?= SessionReady)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "permission callbacks are installed before initialization can request them" $ bounded $ do
        options <- handlerOptions
        called <- newIORef False
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\request -> writeIORef called True >> allowFixturePermission request)}
        identifier <- withDroidSessionHandlers (options {droidModel = Just "handlers-startup"}) handlers $ \session -> do
          readIORef called >>= (@?= True)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "resumed and replacement sessions retain the connection handler policy" $ bounded $ do
        options <- fixtureOptions
        called <- newIORef (0 :: Int)
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\request -> modifyIORef' called (+ 1) >> allowFixturePermission request)}
        identifier <- withResumedDroidSessionHandlers options handlers "handlers-saved" $ \session -> do
          readIORef called >>= (@?= 1)
          peer <- peerIdentifier session
          next <- forkDroidSession session defaultFork
          _ <- sendDroidTurn next "handlers-permission" (\_ -> pure ())
          readIORef called >>= (@?= 2)
          pure peer
        assertReaped identifier,
      testCase "permission workers can make ordinary scoped queries" $ bounded $ do
        options <- handlerOptions
        owned <- newIORef Nothing
        let handlers =
              defaultDroidHandlers
                { onDroidPermission =
                    Just
                      ( \request -> do
                          session <- readIORef owned >>= maybe (assertFailure "Missing owned session") pure
                          models <- listDroidModels session (ListModelsOptions Nothing mempty)
                          catalogModels models @?= []
                          allowFixturePermission request
                      )
                }
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          writeIORef owned (Just session)
          _ <- sendDroidTurn session "handlers-permission" (\_ -> pure ())
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "independent permission requests use concurrent owned workers" $ bounded $ do
        options <- handlerOptions
        first <- newEmptyMVar
        second <- newEmptyMVar
        release <- newEmptyMVar
        let handlers =
              defaultDroidHandlers
                { onDroidPermission =
                    Just
                      ( \request -> do
                          case permissionAssociatedSessionIds request of
                            Just ["one"] -> putMVar first ()
                            Just ["two"] -> putMVar second ()
                            _ -> assertFailure "Unexpected request identity"
                          readMVar release
                          allowFixturePermission request
                      )
                }
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          withAsync (sendDroidTurn session "handlers-parallel" (\_ -> pure ())) $ \turn -> do
            takeMVar first
            takeMVar second
            putMVar release ()
            result <- wait turn
            turnCompletionReason (resultCompletion result) @?= TurnCompleted
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "scope exit cancels and joins permission and question workers before reaping" $ bounded $ do
        options <- handlerOptions
        permissionStarted <- newEmptyMVar
        questionStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        finalized <- newIORef (0 :: Int)
        let finish = atomicModifyIORef' finalized (\count -> (count + 1, ()))
            handlers =
              defaultDroidHandlers
                { onDroidPermission = Just (\_ -> bracket_ (putMVar permissionStarted ()) finish (takeMVar blocked >> pure cancelPermissionResult)),
                  onDroidQuestion = Just (\_ -> bracket_ (putMVar questionStarted ()) finish (takeMVar blocked >> pure cancelDroidQuestions))
                }
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          withAsync (sendDroidTurn session "handlers-blocked" (\_ -> pure ())) $ \turn -> do
            takeMVar permissionStarted
            takeMVar questionStarted
            cancel turn
            waitCatch turn >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled turn completed"
          readIORef finalized >>= (@?= 0)
          pure (droidSessionId session)
        readIORef finalized >>= (@?= 2)
        assertReaped identifier,
      testCase "pending permission workers survive turn completion and handle retirement" $ bounded $ do
        options <- handlerOptions
        started <- newEmptyMVar
        release <- newEmptyMVar
        finalized <- newIORef False
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\request -> bracket_ (putMVar started ()) (writeIORef finalized True) (takeMVar release >> allowFixturePermission request))}
        identifier <- withDroidSessionHandlers options handlers $ \session -> do
          _ <- sendDroidTurn session "handlers-late" (\_ -> pure ())
          takeMVar started
          readIORef finalized >>= (@?= False)
          next <- forkDroidSession session defaultFork
          readIORef finalized >>= (@?= False)
          droidSessionStatus session >>= (@?= SessionReplaced (droidSessionId next))
          putMVar release ()
          result <- sendDroidTurn next "handlers-flush" (\_ -> pure ())
          resultText result @?= "late response received"
          readIORef finalized >>= (@?= True)
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "session observers receive settings notifications outside turns and can query" $ bounded $ do
        options <- fixtureOptions
        observed <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session $ \event -> do
            _ <- listDroidModels session (ListModelsOptions Nothing mempty)
            putMVar observed event
          _ <- notifyFixture session "valid"
          event <- takeMVar observed
          case event of
            Right (SettingsUpdatedEvent update) -> changedSettingsModel (settingsUpdateValues update) @?= Just "observed-model"
            _ -> assertFailure "Expected a settings notification"
          stop
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "session observers coexist with correlated turn streams" $ bounded $ do
        options <- fixtureOptions
        completed <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session $ \case
            Right (TurnCompletedEvent event) -> putMVar completed event
            _ -> pure ()
          result <- sendPrompt session "hello" (\_ -> pure ())
          event <- takeMVar completed
          event @?= resultCompletion result
          resultText result @?= "Hello سلام\n😀"
          stop
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "session observers accept uncorrelated wire completion without weakening turns" $ bounded $ do
        options <- fixtureOptions
        observed <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session (putMVar observed)
          _ <- notifyFixture session "completion"
          takeMVar observed >>= \case
            Right (TurnCompletedEvent event) -> completedTurnId event @?= Nothing
            _ -> assertFailure "Expected uncorrelated completion metadata"
          stop
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "session observer routing ignores foreign malformed payloads and accepts untagged local data" $ bounded $ do
        options <- fixtureOptions
        observed <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session (putMVar observed)
          _ <- notifyFixture session "routing"
          takeMVar observed >>= \case
            Right (SettingsUpdatedEvent _) -> getDroidSettings session >>= (@?= "observed-model") . settingsModel
            _ -> assertFailure "Expected only local settings data"
          stop
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "malformed observed payloads are explicit errors and ordinary callback failures are isolated" $ bounded $ do
        options <- fixtureOptions
        observed <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stopFailing <- onDroidSessionEvent session (\_ -> throwIO CallbackAbort)
          stop <- onDroidSessionEvent session (putMVar observed)
          _ <- notifyFixture session "malformed"
          takeMVar observed >>= (@?= Left DroidInvalidEvent)
          droidSessionStatus session >>= (@?= SessionReady)
          stopFailing
          stop
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "session observer unsubscribe is idempotent and blocks later admission" $ bounded $ do
        options <- fixtureOptions
        count <- newIORef (0 :: Int)
        delivered <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session (\_ -> modifyIORef' count (+ 1) >> putMVar delivered ())
          _ <- notifyFixture session "valid"
          takeMVar delivered
          stop
          stop
          marker <- onDroidSessionEvent session (\_ -> putMVar delivered ())
          _ <- notifyFixture session "valid"
          takeMVar delivered
          readIORef count >>= (@?= 1)
          marker
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "already admitted observer callbacks can finish after unsubscribe" $ bounded $ do
        options <- fixtureOptions
        started <- newEmptyMVar
        release <- newEmptyMVar
        finished <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session (\_ -> putMVar started () >> takeMVar release >> putMVar finished ())
          _ <- notifyFixture session "valid"
          takeMVar started
          stop
          putMVar release ()
          takeMVar finished
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "retired session observers do not follow successor notifications" $ bounded $ do
        options <- fixtureOptions
        oldCount <- newIORef (0 :: Int)
        delivered <- newEmptyMVar
        identifier <- withDroidSession options $ \session -> do
          stopOld <- onDroidSessionEvent session (\_ -> modifyIORef' oldCount (+ 1))
          next <- forkDroidSession session defaultFork
          stopNext <- onDroidSessionEvent next (\_ -> putMVar delivered ())
          _ <- notifyFixture next "valid"
          takeMVar delivered
          readIORef oldCount >>= (@?= 0)
          expectDroidError (DroidSessionReplaced (droidSessionId next)) (onDroidSessionEvent session (\_ -> pure ()))
          stopOld
          stopNext
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "session observers receive channel failure and retain safe cleanup" $ bounded $ do
        options <- fixtureOptions
        observed <- newEmptyMVar
        (identifier, stop) <- withDroidSession options $ \session -> do
          stop <- onDroidSessionEvent session (putMVar observed)
          _ <- notifyFixture session "close"
          takeMVar observed >>= (@?= Left DroidSessionUnusable)
          droidSessionStatus session >>= (@?= SessionUnavailable)
          expectDroidError DroidSessionUnusable (onDroidSessionEvent session (\_ -> pure ()))
          expectDroidError DroidSessionUnusable (getDroidSettings session)
          pure (droidSessionId session, stop)
        stop
        assertReaped identifier,
      testCase "observed settings are seeded before new-session publication" $ bounded $ do
        options <- fixtureOptions
        identifier <- withDroidSession (options {droidModel = Just "settings-seeded", droidSystemPrompt = customSystemPrompt "Private prompt"}) $ \session -> do
          settings <- getDroidSettings session
          settingsModel settings @?= "settings-seeded"
          settingsReasoning settings @?= ReasoningLow
          settingsSpecModel settings @?= Just "initial-spec"
          settingsSystemPrompt settings @?= customSystemPrompt "Private prompt"
          pure (droidSessionId session)
        assertReaped identifier,
      testCase "resumed settings preserve saved values and observe model overrides" $ bounded $ do
        options <- fixtureOptions
        forM_ [Nothing, Just "override-model"] $ \model ->
          withResumedDroidSession (options {droidModel = model}) "saved" $ \session -> do
            settings <- getDroidSettings session
            settingsModel settings @?= fromMaybe "saved-model" model
            settingsReasoning settings @?= ReasoningLow,
      testCase "malformed full settings fail startup instead of publishing a partial baseline" $ bounded $ do
        options <- fixtureOptions
        forM_ ["settings-missing-reason", "settings-invalid-reason"] $ \model ->
          expectDroidError DroidInvalidEvent (withDroidSession (options {droidModel = Just model}) (\_ -> assertFailure "Published invalid settings"))
        expectDroidError DroidInvalidEvent (withResumedDroidSession options "invalid-settings-fields" (\_ -> assertFailure "Published invalid loaded settings")),
      testCase "settings callbacks read their own update while partial fields retain prior observations" $ bounded $ do
        options <- fixtureOptions
        withDroidSession (options {droidModel = Just "settings-seeded"}) $ \session -> do
          settings <- observeFixtureSettings session (object ["reasoningEffort" .= String "high", "disabledToolIds" .= ([] :: [Value]), "availableAutonomyLevels" .= ([] :: [Value]), "compactionThresholdCheckEnabled" .= False]) >>= either throwIO pure
          settingsModel settings @?= "settings-seeded"
          settingsReasoning settings @?= ReasoningHigh
          settingsSpecModel settings @?= Nothing
          settingsSpecReasoning settings @?= Nothing
          settingsMission settings @?= Nothing
          settingsToolPolicy settings @?= ToolPolicy Nothing (Just ["Read"]) (Just []) Nothing
          settingsAvailableAutonomy settings @?= Just []
          settingsCompactionThresholdEnabled settings @?= Just False
          KeyMap.lookup "future" (settingsAdditionalFields settings) @?= Just (String "snapshot"),
      testCase "reported mission settings replace the whole object and empty spec values remain values" $ bounded $ do
        options <- fixtureOptions
        withDroidSession (options {droidModel = Just "settings-seeded"}) $ \session -> do
          settings <- observeFixtureSettings session (object ["specModeModelId" .= String "", "specModeReasoningEffort" .= String "low", "missionSettings" .= object ["workerModel" .= String "replacement"]]) >>= either throwIO pure
          settingsSpecModel settings @?= Just ""
          settingsSpecReasoning settings @?= Just ReasoningLow
          toJSON (settingsMission settings) @?= object ["workerModel" .= String "replacement"],
      testCase "observed extensions remain data and snapshot-only fields are validated" $ bounded $ do
        options <- fixtureOptions
        withDroidSession options $ \session -> do
          settings <- observeFixtureSettings session (object ["sandbox" .= object ["enabled" .= False], "future" .= object ["value" .= Null]]) >>= either throwIO pure
          toJSON (settingsSandbox settings) @?= object ["enabled" .= False]
          KeyMap.lookup "future" (settingsAdditionalFields settings) @?= Just (object ["value" .= Null])
          observeFixtureSettings session (object ["sandbox" .= object []]) >>= (@?= Left DroidInvalidEvent)
          droidSessionStatus session >>= (@?= SessionReady),
      testCase "enum fallbacks do not invent observations or invalidate settings" $ bounded $ do
        options <- fixtureOptions
        withDroidSession options $ \session -> do
          _ <- observeFixtureSettings session (object ["interactionMode" .= String "spec", "availableAutonomyLevels" .= [String "off"]]) >>= either throwIO pure
          settings <- observeFixtureSettings session (object ["interactionMode" .= Null, "availableAutonomyLevels" .= [String "invalid"]]) >>= either throwIO pure
          settingsMode settings @?= Just DroidSpec
          settingsAvailableAutonomy settings @?= Just [AutonomyOff],
      testCase "settings acknowledgements and rejections do not synthesize observations" $ bounded $ do
        options <- fixtureOptions
        withDroidSession options $ \session -> do
          before <- getDroidSettings session
          _ <- updateDroidSettings session (emptySettingsUpdate {updateSettingsModel = Just "not-observed", updateSettingsSpecModel = Just Nothing, updateSettingsAdditionalFields = KeyMap.singleton "fixtureNotify" (String "ack-only")})
          getDroidSettings session >>= (@?= before)
          _ <- try @RpcResultError (updateDroidSettings session (emptySettingsUpdate {updateSettingsModel = Just "fixture-reject"}))
          getDroidSettings session >>= (@?= before),
      testCase "malformed observation stays invalid through partial updates and rollback restores full baseline" $ bounded $ do
        options <- fixtureOptions
        withDroidSession options $ \session -> do
          observeFixtureSettings session Null >>= (@?= Left DroidInvalidEvent)
          observeFixtureSettings session (object ["modelId" .= String "insufficient"]) >>= (@?= Left DroidInvalidEvent)
          result <- try @DroidReplacementError (forkDroidSession session (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureLoadFailure" (Bool True)}))
          case result of
            Left err -> case replacementRollbackError err of
              Nothing -> pure ()
              Just _ -> assertFailure "Rollback failed"
            Right _ -> assertFailure "Expected rejected target load"
          getDroidSettings session >>= (@?= "new-model") . settingsModel
          droidSessionStatus session >>= (@?= SessionReady),
      testCase "successor settings replace baseline and retired handles cannot read them" $ bounded $ do
        options <- fixtureOptions
        withDroidSession options $ \session -> do
          _ <- observeFixtureSettings session (object ["specModeModelId" .= String "observed-override", "future" .= True]) >>= either throwIO pure
          next <- forkDroidSession session defaultFork
          settings <- getDroidSettings next
          settingsModel settings @?= "new-model"
          settingsSpecModel settings @?= Nothing
          settingsAdditionalFields settings @?= mempty
          expectDroidError (DroidSessionReplaced (droidSessionId next)) (getDroidSettings session),
      testCase "loaded snapshot orders earlier and later settings notifications" $ bounded $ do
        options <- fixtureOptions
        withDroidSession (options {droidModel = Just "settings-on-load"}) $ \session -> do
          next <- forkDroidSession session defaultFork
          _ <- sendPrompt next "hello" (\_ -> pure ())
          settings <- getDroidSettings next
          settingsModel settings @?= "after-load"
          settingsReasoning settings @?= ReasoningLow
          settingsAdditionalFields settings @?= mempty,
      testCase "settings reads reject replacing and escaped handles" $ bounded $ withGate $ \gate -> do
        options <- fixtureOptions
        escaped <- withDroidSession options $ \session -> do
          withAsync (forkDroidSession session (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureLoadGate" (String (Text.pack gate))})) $ \worker -> do
            waitForFile (gate <> "/ready")
            expectDroidError DroidSessionBusy (getDroidSettings session)
            releaseGate gate
            wait worker
        expectDroidError DroidSessionUnusable (getDroidSettings escaped),
      testCase "replacement diagnostics and choices redact sensitive fields" $ do
        show (DroidReplacementError "private-source" "private-target" (toException (userError "private-cause")) (Just (toException (userError "private-rollback")))) @?= "DroidReplacementError <redacted>"
        show (DroidRewindOptions "private-message" [] [] "private-title") @?= "DroidRewindOptions <redacted>"
        show (SessionReplaced "private-target") @?= "SessionReplaced <redacted>"
    ]

observeFixtureSettings :: DroidSession -> Value -> IO (Either DroidError SessionSettings)
observeFixtureSettings session fields = do
  observed <- newEmptyMVar
  bracket
    (onDroidSessionEvent session (\_ -> try @DroidError (getDroidSettings session) >>= putMVar observed))
    id
    (\_ -> updateDroidSettings session (emptySettingsUpdate {updateSettingsAdditionalFields = KeyMap.singleton "fixtureSettings" fields}) >> takeMVar observed)

fixtureSettingsSnapshot :: Text -> Maybe Value -> Value
fixtureSettingsSnapshot model prompt
  | model == "settings-missing-reason" = object ["modelId" .= model]
  | model == "settings-invalid-reason" = object ["modelId" .= model, "reasoningEffort" .= False]
  | otherwise =
      object
        ( ["modelId" .= model, "reasoningEffort" .= String "low"]
            <> maybe [] (\value -> ["systemPrompt" .= value]) prompt
            <> if model == "settings-seeded"
              then ["specModeModelId" .= String "initial-spec", "specModeReasoningEffort" .= String "high", "missionSettings" .= object ["workerModel" .= String "original", "workerReasoningEffort" .= String "high"], "enabledToolIds" .= [String "Read"], "disabledToolIds" .= [String "Execute"], "compactionThresholdCheckEnabled" .= True, "future" .= String "snapshot"]
              else []
        )

notifyFixture :: DroidSession -> Text -> IO Object
notifyFixture session behavior = updateDroidSettings session (emptySettingsUpdate {updateSettingsAdditionalFields = KeyMap.singleton "fixtureNotify" (String behavior)})

handlerOptions :: IO DroidOptions
handlerOptions = do
  options <- fixtureOptions
  pure (options {droidModel = Just "handlers-mode"})

allowFixturePermission :: RequestPermissionParams -> IO RequestPermissionResult
allowFixturePermission request = maybe (assertFailure "Expected proceed_once offer") pure (respondPermission request ConfirmProceedOnce (Just "ok") Nothing)

answerFixtureQuestions :: AskUserParams -> IO AskUserResult
answerFixtureQuestions request = case askUserQuestions request of
  [first, second] -> maybe (assertFailure "Expected valid question answers") pure (submitDroidAnswers request [answerDroidQuestion first "free form", answerDroidQuestionMultiple second ["red", "blue"]])
  _ -> assertFailure "Expected two questions"

permissionAccepted :: Value
permissionAccepted = object ["selectedOption" .= String "proceed_once", "comment" .= String "ok"]

fixturePermission :: Text -> [ToolConfirmationOutcome] -> Value
fixturePermission session choices =
  let editing = ConfirmProceedEdit `elem` choices
      kind = if editing then "exit_spec_mode" else "exec" :: Text
      details = if editing then object ["type" .= kind, "plan" .= String "original plan"] else object ["type" .= kind, "fullCommand" .= String "not executed", "command" .= String "fixture"]
   in object ["toolUses" .= [object ["toolUse" .= object ["type" .= String "tool_use", "id" .= String "call", "name" .= String "Fixture", "input" .= object []], "confirmationType" .= kind, "details" .= details]], "options" .= [object ["label" .= String "Choice", "value" .= choice] | choice <- choices], "associatedSessionIds" .= [session]]

fixtureQuestions, fixtureAnswers :: Value
fixtureQuestions = object ["toolCallId" .= String "question-tool", "questions" .= [object ["index" .= Number 1.5, "topic" .= String "First", "question" .= String "Which?", "options" .= [String "red"]], object ["index" .= Number 2, "topic" .= String "Second", "question" .= String "Multiple?", "options" .= [String "red", String "blue"], "multiSelect" .= True]]]
fixtureAnswers = object ["cancelled" .= False, "answers" .= [object ["index" .= Number 1.5, "question" .= String "Which?", "answer" .= String "free form"], object ["index" .= Number 2, "question" .= String "Multiple?", "answer" .= String "red, blue"]]]

attachmentInput :: Text -> IO DroidInput
attachmentInput text = do
  images <- traverse (either throwIO pure . imageFromSource) attachmentImages
  documents <- traverse (either throwIO pure . documentFromSource) attachmentDocuments
  pure (DroidInput text images documents)

attachmentImages :: [Base64ImageSource]
attachmentImages =
  [ Base64ImageSource "iVBORw0KGgo=" ImagePNG (KeyMap.singleton "extra" (String "image")),
    Base64ImageSource "R0lGODlh" ImageGIF mempty
  ]

attachmentDocuments :: [DocumentSource]
attachmentDocuments =
  [ PlainTextDocument (PlainTextSource "text\0سلام" (Just "notes.txt") (Just "text/x-note") (KeyMap.singleton "extra" (Bool True))),
    PDFDocument (Base64PDFSource "JVBERi0=" (Just "parsed") (Just "file.pdf") (Just "/metadata/not-opened.pdf") (KeyMap.singleton "extra" Null))
  ]

mcpConfigurationSessionTests :: [TestTree]
mcpConfigurationSessionTests =
  [ testCase "connection MCP observer receives startup, replacement and rollback prefixes" $ bounded $ do
      options <- fixtureOptions
      seen <- newIORef []
      let handlers = defaultDroidHandlers {onDroidMcpEvent = Just (\source event -> case event of Left (DroidMcpConnectionFailure _) -> pure (); _ -> modifyIORef' seen (<> [(source, fmap eventTag event)]))}
      withDroidSessionHandlers (options {droidMcpOptions = fixtureMcpOptions}) handlers $ \session -> do
        readIORef seen >>= (@?= [(Nothing, Right "mcp_auth_required"), (Nothing, Right "mcp_status"), (Nothing, Right "mcp_auth_completed")])
        branch <- forkDroidSession session defaultFork
        result <- try @DroidReplacementError (forkDroidSession branch (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureLoadFailure" (Bool True)}))
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected rollback"
        events <- readIORef seen
        length events @?= 12
        map snd events @?= concat (replicate 4 [Right "mcp_auth_required", Right "mcp_status", Right "mcp_auth_completed"])
        map fst (take 3 (drop 3 events)) @?= replicate 3 (Just (droidSessionId branch))
        map fst (drop 9 events) @?= replicate 3 (Just (droidSessionId branch)),
    testCase "startup MCP observer reports malformed events and isolates ordinary callback errors" $ bounded $ do
      options <- fixtureOptions
      malformed <- newIORef []
      let handlers = defaultDroidHandlers {onDroidMcpEvent = Just (\_ event -> case event of Left (DroidMcpConnectionFailure _) -> pure (); _ -> modifyIORef' malformed (<> [fmap eventTag event]))}
          badPolicy = fixtureMcpOptions {sessionMcpOAuthCallbackUri = Just "fixture:malformed"}
      withDroidSessionHandlers (options {droidMcpOptions = badPolicy}) handlers $ \_ -> readIORef malformed >>= (@?= [Left DroidMcpInvalidEvent])
      seen <- newIORef []
      let throwing = defaultDroidHandlers {onDroidMcpEvent = Just (\_ event -> case event of Right (McpAuthRequiredEvent _) -> ioError (userError "fixture observer failure"); Right value -> modifyIORef' seen (<> [eventTag value]); Left _ -> pure ())}
      withDroidSessionHandlers (options {droidMcpOptions = fixtureMcpOptions}) throwing $ \_ -> readIORef seen >>= (@?= ["mcp_status", "mcp_auth_completed"]),
    testCase "cancelling startup joins an admitted MCP observer before scope exit" $ bounded $ do
      options <- fixtureOptions
      ready <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      let handlers = defaultDroidHandlers {onDroidMcpEvent = Just (\_ event -> case event of Right (McpAuthRequiredEvent _) -> bracket_ (putMVar ready ()) (putMVar finished ()) (takeMVar hold); _ -> pure ())}
      withAsync (withDroidSessionHandlers (options {droidMcpOptions = fixtureMcpOptions}) handlers (\_ -> pure ())) $ \opening -> do
        takeMVar ready
        cancel opening
        waitCatch opening >>= \case
          Left err -> fromException err @?= Just AsyncCancelled
          Right _ -> assertFailure "Cancelled startup completed"
      takeMVar finished,
    testCase "new/resumed startup preserves omitted, empty and normalized configurations" $ bounded $ do
      options <- fixtureOptions
      forM_ [defaultMcpSessionOptions, defaultMcpSessionOptions {sessionMcpServers = Just []}, fixtureMcpOptions] $ \policy -> do
        let expected
              | policy == fixtureMcpOptions = expectedMcpInitialization
              | sessionMcpServers policy == Just [] = KeyMap.singleton "mcpServers" (toJSON ([] :: [Value]))
              | otherwise = mempty
        withDroidSession (options {droidMcpOptions = policy}) (mcpHistory >=> (@?= [expected]))
        withResumedDroidSession (options {droidMcpOptions = policy {sessionBlockOnMcpLoad = Nothing}}) "saved" (mcpHistory >=> (@?= [KeyMap.delete "blockOnMcpLoad" expected])),
    testCase "fork, compaction, rewind and rollback replay immutable MCP load policy" $ bounded $ do
      options <- fixtureOptions
      identifier <- withDroidSession (options {droidMcpOptions = fixtureMcpOptions}) $ \session -> do
        branch <- forkDroidSession session defaultFork
        (compacted, _) <- compactDroidSession branch (CompactSessionParams (Just "retain decisions") mempty)
        (rewound, _) <- rewindDroidSession compacted (DroidRewindOptions "message-target" [RewindFileSnapshot "restore-me" "hash" 12 mempty] [RewindFileCreation "delete-me" mempty] "rewound")
        result <- try @DroidReplacementError (forkDroidSession rewound (defaultFork {forkSessionAdditionalFields = KeyMap.singleton "fixtureLoadFailure" (Bool True)}))
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected failed attachment and rollback"
        droidSessionStatus rewound >>= (@?= SessionReady)
        mcpHistory rewound >>= (@?= (expectedMcpInitialization : replicate 5 (KeyMap.delete "blockOnMcpLoad" expectedMcpInitialization)))
        expectDroidError (DroidSessionReplaced (droidSessionId branch)) (addDroidMcpServer session (AddMcpServerParams "old" McpHttp Nothing Nothing Nothing Nothing Nothing Nothing mempty))
        pure (droidSessionId session)
      assertReaped identifier,
    testCase "add server normalizes OAuth and rejects invalid configuration without retiring session" $ bounded $ do
      options <- fixtureOptions
      withDroidSession options $ \session -> do
        forM_ (fromMaybe [] (sessionMcpServers fixtureMcpOptions)) $ \config -> do
          result <- addDroidMcpServer session (mcpServerParams config)
          resultSuccess result @?= False
          observed <- either (const (assertFailure "Missing add observation")) pure (parseEither (.: "observedParams") (resultAdditionalFields result))
          case config of
            McpStdioConfig _ -> KeyMap.lookup "type" observed @?= Just (String "stdio")
            McpHttpConfig _ -> do
              KeyMap.lookup "headers" observed @?= Just (object ["X-Fixture" .= String "last"])
              case KeyMap.lookup "oauth" observed of
                Just (Object oauth) -> KeyMap.lookup "clientId" oauth @?= Just (String "fixture-client")
                _ -> assertFailure "Missing OAuth options"
            McpSseConfig _ -> KeyMap.lookup "oauth" observed @?= Just (Bool False)
        let bad = AddMcpServerParams "invalid" McpHttp Nothing Nothing Nothing Nothing Nothing (Just (McpOAuthEnabled (emptyMcpOAuthOptions {mcpOAuthClientId = Just "secret"}))) mempty
        result <- try @McpConfigurationError (addDroidMcpServer session bad)
        result @?= Left InvalidMcpConfiguration
        droidSessionStatus session >>= (@?= SessionReady),
    testCase "invalid startup and init-only resume options fail before process launch" $ do
      let invalid = fixtureMcpOptions {sessionMcpServers = Just [McpHttpConfig (McpRemoteConfig "remote" "not-a-uri" Nothing Nothing mempty)]}
          options = (defaultDroidOptions ".") {droidExecutable = "/missing-mcp-fixture", droidMcpOptions = invalid}
      result <- try @McpConfigurationError (withDroidSession options (\_ -> pure ()))
      result @?= Left InvalidMcpConfiguration
      resumed <- try @McpConfigurationError (withResumedDroidSession (options {droidMcpOptions = fixtureMcpOptions}) "saved" (\_ -> pure ()))
      resumed @?= Left McpInitOnlyOptionOnResume
  ]

expectedMcpInitialization :: Object
expectedMcpInitialization = KeyMap.fromList ["mcpServers" .= fixtureMcpWire, "mcpOAuthCallbackUri" .= String "fixture:callback", "blockOnMcpLoad" .= True]

mcpHistory :: DroidSession -> IO [Object]
mcpHistory session = do
  registry <- listDroidMcpRegistry session
  either (const (assertFailure "Missing MCP load history")) pure (parseEither (.: "mcpHistory") (mcpRegistryAdditionalFields registry))

mcpSessionTests :: [TestTree]
mcpSessionTests =
  [ testCase "queries preserve server/tool/registry reports and scoped status events" $ bounded $ do
      options <- fixtureOptions
      identifier <- withDroidSession options $ \session -> do
        observed <- newEmptyMVar
        _ <- onDroidSessionEvent session $ \case
          Left err -> putMVar observed (Left err)
          Right (McpStatusEvent value) -> putMVar observed (Right (changedMcpStatus value))
          _ -> pure ()
        servers <- listDroidMcpServers session
        map mcpStatusName (listedMcpServers servers) @?= ["fixture"]
        map mcpStatusHasAuthTokens (listedMcpServers servers) @?= [Just False]
        takeMVar observed >>= (@?= Right servers)
        tools <- listDroidMcpTools session
        map mcpToolEnabled (listedMcpTools tools) @?= [False]
        registry <- listDroidMcpRegistry session
        map registryServerName (mcpRegistryServers registry) @?= ["fixture"]
        pure (droidSessionId session)
      assertReaped identifier,
    testCase "mutations preserve false acknowledgements and wire parameters" $ bounded $ do
      options <- fixtureOptions
      withDroidSession options $ \session -> do
        let code = SubmitMcpAuthCodeParams "fixture" "fixture-code" "fixture-state" mempty
            err = SubmitMcpAuthErrorParams "fixture" "denied" "fixture-state" (Just "description") mempty
            calls =
              [ (removeDroidMcpServer session "fixture", toJSON (RemoveMcpServerParams "fixture" mempty)),
                (toggleDroidMcpServer session "fixture" False, toJSON (ToggleMcpServerParams "fixture" False mempty)),
                (toggleDroidMcpServer session "fixture" True, toJSON (ToggleMcpServerParams "fixture" True mempty)),
                (toggleDroidMcpTool session "fixture" "lookup" False, toJSON (ToggleMcpToolParams "fixture" "lookup" False mempty)),
                (authenticateDroidMcpServer session "fixture", toJSON (McpServerNameParams "fixture" mempty)),
                (cancelDroidMcpAuth session "fixture", toJSON (McpServerNameParams "fixture" mempty)),
                (clearDroidMcpAuth session "fixture", toJSON (McpServerNameParams "fixture" mempty)),
                (submitDroidMcpAuthCode session code, toJSON code),
                (submitDroidMcpAuthError session err, toJSON err)
              ]
        forM_ calls $ \(action, expected) -> do
          result <- action
          resultSuccess result @?= False
          KeyMap.lookup "observedParams" (resultAdditionalFields result) @?= Just expected
        droidSessionStatus session >>= (@?= SessionReady),
    testCase "auth acknowledgement, callback submission and terminal outcome are separate" $ bounded $ do
      options <- fixtureOptions
      withDroidSession options $ \session -> do
        required <- newEmptyMVar
        completed <- newEmptyMVar
        _ <- onDroidSessionEvent session $ \case
          Right (McpAuthRequiredEvent value) -> putMVar required value
          Right (McpAuthCompletedEvent value) -> putMVar completed value
          _ -> pure ()
        authenticateDroidMcpServer session "oauth" >>= (@?= True) . resultSuccess
        offered <- takeMVar required
        mcpAuthState offered @?= "fixture-state"
        tryTakeMVar completed >>= (@?= Nothing)
        submitDroidMcpAuthCode session (SubmitMcpAuthCodeParams "oauth" "fixture-code" (mcpAuthState offered) mempty) >>= (@?= False) . resultSuccess
        takeMVar completed >>= (@?= McpAuthFailed) . mcpAuthOutcome
        submitDroidMcpAuthError session (SubmitMcpAuthErrorParams "oauth" "denied" "fixture-state" Nothing mempty) >>= (@?= False) . resultSuccess
        takeMVar completed >>= (@?= McpAuthCancelled) . mcpAuthOutcome
        droidSessionStatus session >>= (@?= SessionReady),
    testCase "pending authentication permits concurrent explicit cancellation" $ bounded $ do
      options <- fixtureOptions
      withDroidSession options $ \session -> do
        ready <- newEmptyMVar
        _ <- onDroidSessionEvent session $ \case
          Right (McpAuthRequiredEvent _) -> putMVar ready ()
          _ -> pure ()
        withAsync (authenticateDroidMcpServer session "held") $ \pending -> do
          takeMVar ready
          cancelDroidMcpAuth session "held" >>= (@?= True) . resultSuccess
          wait pending >>= (@?= False) . resultSuccess
        droidSessionStatus session >>= (@?= SessionReady),
    testCase "malformed adapted MCP notifications reach observers as errors" $ bounded $ do
      options <- fixtureOptions
      withDroidSession options $ \session -> do
        observed <- newEmptyMVar
        _ <- onDroidSessionEvent session (putMVar observed)
        authenticateDroidMcpServer session "invalid-event" >>= (@?= False) . resultSuccess
        takeMVar observed >>= (@?= Left DroidInvalidEvent)
        droidSessionStatus session >>= (@?= SessionReady),
    testCase "remote rejection preserves reuse and escaped handles reject queries" $ bounded $ do
      options <- fixtureOptions
      escaped <- withDroidSession options $ \session -> do
        result <- try @RpcResultError (authenticateDroidMcpServer session "rpc-error")
        case result of
          Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
          _ -> assertFailure "Missing MCP remote rejection"
        droidSessionStatus session >>= (@?= SessionReady)
        pure session
      expectDroidError DroidSessionUnusable (listDroidMcpServers escaped)
      assertReaped (droidSessionId escaped),
    testCase "cancelled authentication preserves exception identity and invalidates its lease" $ bounded $ do
      options <- fixtureOptions
      identifier <- withDroidSession options $ \session -> do
        ready <- newEmptyMVar
        _ <- onDroidSessionEvent session $ \case
          Right (McpAuthRequiredEvent _) -> putMVar ready ()
          _ -> pure ()
        withAsync (authenticateDroidMcpServer session "held") $ \pending -> do
          takeMVar ready
          cancel pending
          waitCatch pending >>= \case
            Left err -> fromException err @?= Just AsyncCancelled
            Right _ -> assertFailure "Cancelled authentication returned"
        droidSessionStatus session >>= (@?= SessionUnavailable)
        pure (droidSessionId session)
      assertReaped identifier,
    testCase "mutation result display hides server extensions" $
      show (SuccessResult False (KeyMap.singleton "private" (String "fixture-code-and-state"))) @?= "SuccessResult <redacted>"
  ]

newtype OutputAnswer = OutputAnswer Int deriving stock (Eq, Show)

instance FromJSON OutputAnswer where
  parseJSON = withObject "OutputAnswer" $ \fields -> do
    answer <- fields .: "answer"
    if answer > 0 then pure (OutputAnswer answer) else fail "answer must be positive"

outputTestSchema :: Object
outputTestSchema =
  KeyMap.fromList
    [ "type" .= String "object",
      "properties" .= object ["answer" .= object ["type" .= String "integer", "minimum" .= Number 1]],
      "required" .= [String "answer"],
      "additionalProperties" .= False,
      "x-fixture" .= object ["preserve" .= True]
    ]

completeRichTags :: [Text]
completeRichTags = ["message", "tool_call", "message", "tool_result", "hook_started", "hook_completed", "error", "message"]

eventTag :: DroidEvent -> Text
eventTag = \case
  MessageEvent _ -> "message"
  TextDeltaEvent _ -> "text_delta"
  TextCompleteEvent _ -> "text_complete"
  ThinkingDeltaEvent _ -> "thinking_delta"
  ThinkingCompleteEvent _ -> "thinking_complete"
  ToolCallDeltaEvent _ -> "tool_call_delta"
  ToolCallEvent _ -> "tool_call"
  ToolResultEvent _ -> "tool_result"
  ToolProgressEvent _ -> "tool_progress"
  ToolHeartbeatEvent _ -> "heartbeat"
  ToolPhaseEvent _ -> "phase"
  RetryEvent _ -> "retry"
  UsageEvent _ -> "usage"
  WorkingStateEvent _ -> "working"
  TitleEvent _ -> "title"
  WorkingDirectoryEvent _ -> "directory"
  SettingsUpdatedEvent _ -> "settings"
  McpStatusEvent _ -> "mcp_status"
  McpAuthRequiredEvent _ -> "mcp_auth_required"
  McpAuthCompletedEvent _ -> "mcp_auth_completed"
  MissionStateEvent _ -> "mission_state"
  MissionFeaturesEvent _ -> "mission_features"
  MissionProgressEvent _ -> "mission_progress"
  MissionHeartbeatEvent _ -> "mission_heartbeat"
  MissionWorkerStartedEvent _ -> "mission_worker_started"
  MissionWorkerCompletedEvent _ -> "mission_worker_completed"
  PermissionEvent _ -> "permission"
  HookStartedEvent _ -> "hook_started"
  HookCompletedEvent _ -> "hook_completed"
  StructuredOutputEvent _ -> "structured"
  MessageRetractedEvent _ -> "retracted"
  SessionCompactedEvent _ -> "compacted"
  QueuedMessagesDiscardedEvent _ -> "queue_discarded"
  ChildSessionAvailableEvent _ -> "child_available"
  TurnCompletedEvent _ -> "completed"
  ErrorEvent _ -> "error"
  OtherNotificationEvent _ -> "other"

defaultFork :: ForkSessionParams
defaultFork = ForkSessionParams Nothing Nothing mempty

peerIdentifier :: DroidSession -> IO Text
peerIdentifier session = resultText <$> sendPrompt session "peer-pid" (\_ -> pure ())

cancelMutation :: Bool -> IO ()
cancelMutation repeatCancellation = withGate $ \gate -> do
  options <- fixtureOptions
  peer <- withDroidSession options $ \session -> do
    peer <- peerIdentifier session
    withAsync (renameDroidSession session ("gate:" <> Text.pack gate)) $ \mutation -> do
      waitGate gate
      withAsync (forkDroidSession session defaultFork) $ \replacement -> do
        waitStatus session SessionReplacing
        throwTo (asyncThreadId mutation) AsyncCancelled
        when repeatCancellation (throwTo (asyncThreadId mutation) AsyncCancelled)
        releaseGate gate
        waitCatch mutation >>= \case
          Left err -> fromException err @?= Just AsyncCancelled
          Right _ -> assertFailure "Cancelled mutation succeeded"
        outcome <- waitCatch replacement
        trace <- readRequestTrace gate
        case outcome of
          Left err -> fromException err @?= Just DroidSessionUnusable
          Right _ -> assertFailure ("Replacement crossed an uncertain mutation: " <> show trace)
        let methods = [method | Object fields <- trace, Just (String method) <- [KeyMap.lookup "method" fields]]
        when ("droid.fork_session" `elem` methods) (assertFailure "Premature replacement reached the peer")
        unless (repeatCancellation || "droid.interrupt_session" `elem` methods) (assertFailure "Uncertain mutation did not request interruption")
    droidSessionStatus session >>= (@?= SessionUnavailable)
    pure peer
  assertReaped peer

cancelReplacementLoad :: (Exception e, Eq e, Show e) => e -> IO ()
cancelReplacementLoad exception = withGate $ \gate -> do
  options <- fixtureOptions
  peer <- withDroidSession (options {droidModel = Just "mission-provisional"}) $ \session -> do
    peer <- peerIdentifier session
    let flags = KeyMap.singleton "fixtureLoadGate" (String (Text.pack gate))
    withAsync (forkDroidSession session (defaultFork {forkSessionAdditionalFields = flags})) $ \worker -> do
      waitGate gate
      throwTo (asyncThreadId worker) exception
      releaseGate gate
      waitCatch worker >>= \case
        Left err -> fromException err @?= Just exception
        Right _ -> assertFailure "Cancelled successor load succeeded"
    droidSessionStatus session >>= (@?= SessionReady)
    snapshot <- getDroidMissionSnapshot session >>= maybe (assertFailure "Rollback lost mission state") pure
    missionSnapshotState snapshot @?= MissionCompleted
    missionSnapshotTitle snapshot @?= Just ("loaded " <> droidSessionId session)
    fmap usageInputTokens (missionSnapshotTokenUsage snapshot) @?= Just 7
    trace <- readRequestTrace gate
    case traceLoadTargets trace of
      [_, restored] -> restored @?= droidSessionId session
      _ -> assertFailure "Cancelled load did not attempt a healthy rollback"
    pure peer
  assertReaped peer

fixtureOptions :: IO DroidOptions
fixtureOptions = do
  executable <- getExecutablePath
  pure ((defaultDroidOptions ".") {droidExecutable = executable})

assertReaped :: Text -> IO ()
assertReaped identifier = case Text.stripPrefix "fixture-" identifier >>= readMaybe . Text.unpack of
  Nothing -> assertFailure "Missing fixture PID"
  Just pid ->
    try @IOException (signalProcess nullSignal pid) >>= \case
      Left err | isDoesNotExistError err -> pure ()
      _ -> assertFailure "CLI child was not reaped"

assertRemoteStartupError :: JsonRpcErrorCode -> Either RpcResultError a -> IO ()
assertRemoteStartupError expected (Left (RpcRemoteFailure err)) = do
  rpcErrorCode err @?= expected
  case rpcErrorData err of
    Just (Object fields) | Just (String peer) <- KeyMap.lookup "peerId" fields -> assertReaped peer
    _ -> assertFailure "Missing fixture PID in startup error"
assertRemoteStartupError _ _ = assertFailure "Expected a structured startup failure"

readRequestTrace :: FilePath -> IO [Value]
readRequestTrace directory = BS.readFile (directory <> "/requests.jsonl") >>= mapM (either (const (throwIO CallbackAbort)) pure . eitherDecodeStrict') . BS.lines

traceLoadTargets :: [Value] -> [Text]
traceLoadTargets frames = [target | Object fields <- frames, KeyMap.lookup "method" fields == Just (String "droid.load_session"), Just (String target) <- [KeyMap.lookup "sessionId" fields]]

withGate :: (FilePath -> IO a) -> IO a
withGate = bracket (mkdtemp "/tmp/droid-replacement-") removePathForcibly

waitForFile :: FilePath -> IO ()
waitForFile path = doesFileExist path >>= \exists -> unless exists (threadDelay 1000 >> waitForFile path)

waitGate :: FilePath -> IO ()
waitGate directory = waitForFile (directory <> "/ready")

releaseGate :: FilePath -> IO ()
releaseGate directory = writeFile (directory <> "/release") "release"

peerGate :: FilePath -> IO ()
peerGate directory = writeFile (directory <> "/ready") "ready" >> waitForFile (directory <> "/release")

waitStatus :: DroidSession -> DroidSessionStatus -> IO ()
waitStatus session expected = do
  status <- droidSessionStatus session
  unless (status == expected) (threadDelay 1000 >> waitStatus session expected)

expectDroidError :: DroidError -> IO a -> IO ()
expectDroidError expected action =
  try @DroidError action >>= \case
    Left err -> err @?= expected
    Right _ -> assertFailure "Expected a session-state error"

data PeerState = PeerState
  { peerSessions :: [(Text, (Int, Text))],
    peerFailedLoads :: [Text],
    peerGatedLoads :: [(Text, FilePath)],
    peerLoadHistory :: [Text],
    peerReplayLoads :: [Text],
    peerTraceDirectory :: Maybe FilePath,
    peerActiveTurn :: Maybe Text,
    peerInterruptMode :: Text,
    peerInterruptCount :: Int,
    peerSystemPrompt :: Maybe Value,
    peerRejectPermissions :: Bool,
    peerLatePermission :: Bool,
    peerMcpHistory :: [Object]
  }

data CallbackAbort = CallbackAbort deriving stock (Eq, Show)

instance Exception CallbackAbort

-- Test executable accepts the real CLI argv and speaks the selected runtime's
-- minimal protocol. No Factory executable, credentials or service is involved.
runDroidPeer :: IO ()
runDroidPeer = newIORef (PeerState [] [] [] [] [] Nothing Nothing "" 0 Nothing True False []) >>= runDroidPeerWithState

runDroidPeerWithState :: IORef PeerState -> IO ()
runDroidPeerWithState state = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  pid <- getProcessID
  let session = "fixture-" <> Text.pack (show pid)
  startup <- readFrame
  expect "factoryProtocolVersion" (String "1.201.1") startup
  case KeyMap.lookup "params" startup of
    Just (Object params) -> do
      let customPermissions = any (\key -> case KeyMap.lookup key params of Just (String value) -> "handlers-" `Text.isPrefixOf` value; _ -> False) ["modelId", "sessionId"]
      expect "autoRejectPermissionRequests" (Bool (not customPermissions)) params
      modifyIORef' state (\current -> current {peerRejectPermissions = not customPermissions})
      recordMcpPolicy params
      when (KeyMap.member "_meta" startup || KeyMap.member "tags" params) (throwIO CallbackAbort)
      case KeyMap.lookup "method" startup of
        Just (String "droid.initialize_session") -> do
          expect "machineId" (String "default") params
          when (KeyMap.member "systemPromptOverride" params) (throwIO CallbackAbort)
          forM_ (KeyMap.lookup "systemPrompt" params) $ \value -> do
            prompt <- either (const (throwIO CallbackAbort)) pure (parseEither parseJSON value :: Either String SystemPromptConfig)
            unless (toJSON prompt == value) (throwIO CallbackAbort)
          modifyIORef' state (\current -> current {peerSystemPrompt = KeyMap.lookup "systemPrompt" params})
          model <- case KeyMap.lookup "modelId" params of
            Nothing -> pure "new-model"
            Just (String value) -> pure value
            _ -> throwIO CallbackAbort
          when (model == "launch-options") $ do
            lookupEnv "DROID_LAUNCH_VALUE" >>= (@?= Just "trusted")
            lookupEnv "DROID_LAUNCH_EMPTY" >>= (@?= Just "")
            lookupEnv "FACTORY_UPSTREAM_CLIENT_TYPE" >>= (@?= Nothing)
            lookupEnv "FACTORY_UPSTREAM_SDK" >>= (@?= Nothing)
          if model == "fixture-reject"
            then writeFrame (response startup ["error" .= object ["code" .= (-32001 :: Int), "message" .= String "fixture authentication failure"]])
            else do
              when (model == "handlers-startup") (ask "startup-permission" "droid.request_permission" (fixturePermission session [ConfirmProceedOnce, ConfirmCancel]) permissionAccepted)
              when (model == "mission-provisional") $ do
                emitFor Nothing (object ["type" .= String "mission_state_changed", "state" .= String "running"])
                emitFor Nothing (object ["type" .= String "mission_progress_entry", "progressLog" .= [object ["type" .= String "mission_accepted", "timestamp" .= String "reported time", "title" .= String "before reply"]]])
                emitFor Nothing (object ["type" .= String "mission_worker_started", "workerSessionId" .= String "mission-child"])
                forM_ [(session, 7 :: Int), ("unrelated", 999)] $ \(identifier, input) -> emitFor Nothing (object ["type" .= String "session_token_usage_changed", "sessionId" .= identifier, "tokenUsage" .= object ["inputTokens" .= input, "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]])
              writeFrame (response startup ["result" .= object ["sessionId" .= session, "settings" .= fixtureSettingsSnapshot model (KeyMap.lookup "systemPrompt" params)]])
              loop session (1 :: Int) model
        Just (String "droid.load_session") -> do
          case KeyMap.lookup "sessionId" params of
            Just (String identifier) -> modifyIORef' state (\current -> current {peerLoadHistory = peerLoadHistory current <> [identifier]})
            _ -> throwIO CallbackAbort
          expectMcpLoadKeys params
          when (KeyMap.lookup "sessionId" params == Just (String "handlers-saved")) (ask "load-permission" "droid.request_permission" (fixturePermission "handlers-saved" [ConfirmProceedOnce, ConfirmCancel]) permissionAccepted)
          identifier <- case KeyMap.lookup "sessionId" params of
            Just (String value) -> pure value
            _ -> throwIO CallbackAbort
          if identifier == "missing"
            then reject startup RpcEntityNotFound "fixture session not found"
            else do
              let history = object ["messages" .= [object ["id" .= String "old", "role" .= String "assistant", "content" .= [object ["type" .= String "text", "text" .= String "old output"]]]]]
                  settings = object ["modelId" .= String "saved-model", "reasoningEffort" .= String "low"]
                  loaded = case identifier of
                    "invalid-load" -> object []
                    "invalid-history" -> object ["session" .= object ["messages" .= String "invalid"], "settings" .= settings]
                    "invalid-settings" -> object ["session" .= history, "settings" .= Null]
                    "invalid-settings-fields" -> object ["session" .= history, "settings" .= object ["modelId" .= String "saved-model"]]
                    _ -> object ["session" .= history, "settings" .= settings, "cwd" .= String "/saved-working-directory"]
              writeFrame (response startup ["result" .= loaded])
              loop identifier 2 "saved-model"
        _ -> throwIO CallbackAbort
    _ -> throwIO CallbackAbort
  where
    loop session turn model = do
      remember session turn model
      request <- readFrame
      case KeyMap.lookup "method" request of
        Just (String "droid.interrupt_session") -> do
          modifyIORef' state (\current -> current {peerInterruptCount = peerInterruptCount current + 1})
          current <- readIORef state
          case peerActiveTurn current of
            Nothing -> writeFrame (response request ["result" .= object []])
            Just _ -> case Text.stripPrefix "stop-gate:" (peerInterruptMode current) of
              Just gate -> do
                emit session (completed "cancelled")
                gateQuery request (Text.unpack gate)
                writeFrame (response request ["result" .= object []])
              Nothing | peerInterruptMode current == "interrupt-denied" -> do
                modifyIORef' state (\value -> value {peerInterruptMode = "hang"})
                reject request RpcConflict "fixture interrupt rejected"
              Nothing -> do
                writeFrame (response request ["result" .= object []])
                emit session (completed "cancelled")
          loop session turn model
        Just (String "droid.fork_session") -> do
          params <- requestParams request
          replacePeer request "fork" session turn model params []
          loop session turn model
        Just (String "droid.compact_session") -> do
          params <- requestParams request
          replacePeer request "compact" session turn model params ["removedCount" .= Number 2.5]
          loop session turn model
        Just (String "droid.execute_rewind") -> do
          params <- requestParams request
          expect "sessionId" (String session) params
          expect "messageId" (String "message-target") params
          expect "forkTitle" (String "rewound") params
          expect "filesToRestore" (toJSON [RewindFileSnapshot "restore-me" "hash" 12 mempty]) params
          expect "filesToDelete" (toJSON [RewindFileCreation "delete-me" mempty]) params
          replacePeer request "rewind" session turn model params ["restoredCount" .= Number 1, "deletedCount" .= Number 0, "failedRestoreCount" .= Number 0, "failedDeleteCount" .= Number 1]
          loop session turn model
        Just (String "droid.load_session") -> do
          params <- requestParams request
          expectMcpLoadKeys params
          recordMcpPolicy params
          rejectedByDefault <- peerRejectPermissions <$> readIORef state
          expect "autoRejectPermissionRequests" (Bool rejectedByDefault) params
          target <- case KeyMap.lookup "sessionId" params of
            Just (String identifier) -> pure identifier
            _ -> throwIO CallbackAbort
          modifyIORef' state (\current -> current {peerLoadHistory = peerLoadHistory current <> [target]})
          current <- readIORef state
          forM_ (lookup target (peerGatedLoads current)) $ \gate -> do
            modifyIORef' state (\value -> value {peerGatedLoads = filter ((/= target) . fst) (peerGatedLoads value)})
            enableTrace gate request
            gateHandshake
            peerGate gate
          when (target `elem` peerReplayLoads current) (replicateM_ 128 (emitFor Nothing (created "old load history")))
          if target `elem` peerFailedLoads current
            then reject request RpcEntityNotFound "fixture load rejected" >> loop session turn model
            else case lookup target (peerSessions current) of
              Nothing -> reject request RpcEntityNotFound "fixture unknown successor" >> loop session turn model
              Just (savedTurn, savedModel) -> do
                when (savedModel == "settings-on-load") (emitFor Nothing (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "before-load", "reasoningEffort" .= String "high", "beforeSnapshot" .= True]]))
                when (savedModel == "mission-provisional") $ do
                  emitFor Nothing (object ["type" .= String "mission_state_changed", "state" .= String "completed"])
                  emitFor Nothing (object ["type" .= String "mission_progress_entry", "progressLog" .= [object ["type" .= String "mission_accepted", "timestamp" .= String "reported time", "title" .= ("loaded " <> target)]]])
                writeFrame (response request ["result" .= object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= fixtureSettingsSnapshot savedModel Nothing, "cwd" .= String "/saved-working-directory"]])
                when (savedModel == "settings-on-load") (emitFor Nothing (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "after-load"]]))
                loop target savedTurn savedModel
        Just (String "droid.list_tools") -> do
          params <- requestParams request
          case KeyMap.lookup "fixture" params of
            Just (String "error") -> reject request RpcInvalidParams "fixture tool-query rejection"
            Just (String "invalid") -> writeFrame (response request ["result" .= object ["tools" .= False]])
            _ -> writeFrame (response request ["result" .= object ["tools" .= [object ["id" .= String "sdk-tool", "llmId" .= String "model-tool", "displayName" .= String "Tool", "description" .= String "Description", "category" .= String "execute", "defaultAllowed" .= False, "currentlyAllowed" .= True]], "observedParams" .= params]])
          loop session turn model
        Just (String "droid.list_commands") -> do
          expect "params" (object []) request
          writeFrame (response request ["result" .= object ["commands" .= [object ["name" .= String "fixture-command", "description" .= String "Metadata only", "isExecutable" .= True]]]])
          loop session turn model
        Just (String "droid.list_skills") -> do
          expect "params" (object []) request
          writeFrame (response request ["result" .= object ["skills" .= [object ["name" .= String "fixture-skill", "location" .= String "project", "filePath" .= String "/not/opened/SKILL.md"]], "projectAvailable" .= False]])
          loop session turn model
        Just (String "droid.set_skill_disabled") -> do
          params <- requestParams request
          _ <- either (const (throwIO CallbackAbort)) pure (parseEither parseJSON (Object params) :: Either String SetSkillDisabledParams)
          writeFrame (response request ["result" .= object ["success" .= (KeyMap.lookup "skillName" params /= Just (String "deny")), "observedParams" .= params]])
          loop session turn model
        Just (String "droid.list_models") -> do
          params <- case KeyMap.lookup "params" request of
            Just (Object fields) -> pure fields
            _ -> throwIO CallbackAbort
          case KeyMap.lookup "fixtureGate" params of
            Just (String directory) -> gateQuery request (Text.unpack directory)
            _ -> pure ()
          case KeyMap.lookup "fixture" params of
            Just (String "error") -> reject request RpcInvalidParams "fixture query error"
            Just (String "invalid") -> writeFrame (response request ["result" .= object ["models" .= String "invalid"]])
            _ -> do
              let metadata identifier = ["id" .= (identifier :: Text), "displayName" .= String "Fixture model", "shortDisplayName" .= String "Fixture", "modelProvider" .= String "anthropic", "supportedReasoningEfforts" .= [String "low"], "defaultReasoningEffort" .= String "low"]
                  enabled = object (metadata "available" <> ["disabled" .= False])
                  disabled = object (metadata "blocked" <> ["disabled" .= True, "disabledReason" .= String "fixture policy"])
                  models = [enabled] <> [disabled | KeyMap.lookup "includeDisabled" params == Just (Bool True)]
              writeFrame (response request ["result" .= object ["models" .= models, "observedParams" .= params]])
          loop session turn model
        Just (String "droid.get_context_stats") -> do
          expect "params" (object []) request
          writeFrame (response request ["result" .= object ["used" .= Number 12.5, "remaining" .= Number 87.5, "limit" .= Number 100, "accuracy" .= String "estimated", "updatedAt" .= String "fixture-time", "fixture" .= True]])
          loop session turn model
        Just (String "droid.get_context_breakdown") -> do
          expect "params" (object []) request
          writeFrame (response request ["result" .= object ["modelId" .= model, "modelDisplayName" .= String "Fixture model", "contextBudget" .= Number 100, "usedTokens" .= Number 12.5, "freeTokens" .= Number 87.5, "categories" .= [object ["name" .= String "Messages", "tokens" .= Number 12.5, "colorKey" .= String "messages"]], "skills" .= ([] :: [Value]), "mcpServers" .= ([] :: [Value]), "droids" .= ([] :: [Value])]])
          loop session turn model
        Just (String "droid.rename_session") -> do
          title <- case KeyMap.lookup "params" request of
            Just (Object params) -> do
              expectKeys ["title"] params
              case KeyMap.lookup "title" params of
                Just (String value) -> pure value
                _ -> throwIO CallbackAbort
            _ -> throwIO CallbackAbort
          forM_ (Text.stripPrefix "gate:" title) $ \directory -> do
            enableTrace (Text.unpack directory) request
            gateHandshake
            peerGate (Text.unpack directory)
          let result = if title == "invalid-result" then object ["success" .= String "invalid"] else object ["success" .= (title /= "denied"), "title" .= title]
          writeFrame (response request ["result" .= result])
          loop session turn model
        Just (String "droid.change_working_directory") -> do
          directory <- case KeyMap.lookup "params" request of
            Just (Object params) -> do
              expectKeys ["workingDirectory"] params
              case KeyMap.lookup "workingDirectory" params of
                Just (String value) -> pure value
                _ -> throwIO CallbackAbort
            _ -> throwIO CallbackAbort
          case directory of
            "denied" -> reject request RpcInvalidParams "fixture directory denied"
            "invalid-result" -> writeFrame (response request ["result" .= object ["resolvedPath" .= False]])
            _ -> writeFrame (response request ["result" .= object ["resolvedPath" .= String "/fixture/target", "requested" .= directory]])
          loop session turn model
        Just (String "droid.get_rewind_info") -> do
          message <- case KeyMap.lookup "params" request of
            Just (Object params) -> do
              expectKeys ["messageId", "sessionId"] params
              expect "sessionId" (String session) params
              case KeyMap.lookup "messageId" params of
                Just (String value) -> pure value
                _ -> throwIO CallbackAbort
            _ -> throwIO CallbackAbort
          writeFrame (response request ["result" .= object ["availableFiles" .= [object ["filePath" .= String "not-opened", "contentHash" .= String "opaque", "size" .= Number 12]], "createdFiles" .= [object ["filePath" .= String "not-deleted"]], "evictedFiles" .= [object ["filePath" .= String "expired", "reason" .= String "fixture"]], "requestedMessage" .= message]])
          loop session turn model
        Just (String "droid.update_session_settings") -> do
          params <- requestParams request
          patch <- either (const (throwIO CallbackAbort)) pure (parseEither parseJSON (Object params) :: Either String UpdateSessionSettingsParams)
          forM_ (KeyMap.lookup "fixtureGate" params) $ \case
            String directory -> gateQuery request (Text.unpack directory)
            _ -> throwIO CallbackAbort
          let selected = fromMaybe model (updateSettingsModel patch)
          if selected == "fixture-reject"
            then reject request RpcInvalidParams "fixture invalid model" >> loop session turn model
            else
              if KeyMap.lookup "fixture" params == Just (String "invalid")
                then writeFrame (response request ["result" .= Number 0]) >> loop session turn model
                else do
                  let settings = object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "observed-model"]]
                      notify = KeyMap.lookup "fixtureNotify" params
                  case notify of
                    Just (String "valid") -> emit session settings
                    Just (String "malformed") -> emit session (object ["type" .= String "settings_updated", "settings" .= Null])
                    Just (String "completion") -> emitRawFor (Just session) (completed "completed")
                    Just (String "routing") -> do
                      emit "foreign-session" (object ["type" .= String "settings_updated", "settings" .= Null])
                      emitFor Nothing settings
                    _ -> forM_ (KeyMap.lookup "fixtureSettings" params) $ \fields -> emit session (object ["type" .= String "settings_updated", "settings" .= fields])
                  when (isNothing notify && isNothing (KeyMap.lookup "fixtureSettings" params)) $
                    forM_ (updateSettingsModel patch) $
                      \value -> emit session (object ["type" .= String "settings_updated", "requestId" .= KeyMap.lookup "id" request, "settings" .= object ["modelId" .= value]])
                  writeFrame (response request ["result" .= object ["observedParams" .= params]])
                  unless (notify == Just (String "close")) (loop session turn selected)
        Just (String "droid.add_user_message") -> do
          params <- requestParams request
          prompt <- case KeyMap.lookup "text" params of
            Just (String text) -> pure text
            _ -> throwIO CallbackAbort
          case KeyMap.lookup "outputFormat" params of
            Nothing -> when ("output-" `Text.isPrefixOf` prompt) (throwIO CallbackAbort)
            Just format -> unless (format == object ["type" .= String "json_schema", "schema" .= Object outputTestSchema]) (throwIO CallbackAbort)
          forM_ ["imagePaths", "content"] $ \key -> when (KeyMap.member key params) (throwIO CallbackAbort)
          if prompt == "attachments-large"
            then do
              images <- either (const (throwIO CallbackAbort)) pure (parseEither (.: "images") params :: Either String [Base64ImageSource])
              unless (length images == 2 && all (\image -> imageSourceMediaType image == ImagePNG && Text.length (imageSourceData image) == 4 * ((5 * 1024 * 1024 + 2) `div` 3) && "iVBORw0KGgo" `Text.isPrefixOf` imageSourceData image) images) (throwIO CallbackAbort)
              when (KeyMap.member "files" params) (throwIO CallbackAbort)
            else
              if "attachments" `Text.isPrefixOf` prompt || Text.null prompt
                then expect "images" (toJSON attachmentImages) params >> expect "files" (toJSON attachmentDocuments) params
                else forM_ ["images", "files"] $ \key -> when (KeyMap.member key params) (throwIO CallbackAbort)
          turnId <- case KeyMap.lookup "messageId" params of
            Just (String identifier) -> pure identifier
            _ -> throwIO CallbackAbort
          modifyIORef' state (\current -> current {peerActiveTurn = Just turnId, peerInterruptMode = prompt})
          forM_ (Text.stripPrefix "submit-gate:" prompt) (gateQuery request . Text.unpack)
          writeFrame (response request ["result" .= object ["accepted" .= True]])
          case prompt of
            "hang" -> emit session (delta "started")
            "attachments-hang" -> emit session (delta "started")
            "attachments-large" -> emit session (created "large attachments received") >> emit session (completed "completed")
            _
              | prompt == "attachments" || Text.null prompt ->
                  if KeyMap.member "outputFormat" params
                    then outputTurn session (Just (object ["answer" .= Number 42])) "{\"answer\":42}" "completed"
                    else emit session (created "attachments received") >> emit session (completed "completed")
            "output-notification" -> outputTurn session (Just (object ["answer" .= Number 42])) "{\"answer\":99}" "completed"
            "output-invalid-type" -> outputTurn session (Just (object ["answer" .= String "private invalid answer"])) "{\"answer\":42}" "completed"
            "output-invalid-value" -> outputTurn session (Just (object ["answer" .= Number 0])) "" "completed"
            "output-failure" -> outputTurn session Nothing "{\"answer\":42}" "error"
            "output-text" -> outputTurn session Nothing " \n{\"answer\":7}\n " "completed"
            "output-null-text" -> outputTurn session (Just Null) "{\"answer\":7}" "completed"
            "output-missing" -> outputTurn session Nothing "not JSON" "completed"
            "output-fenced" -> outputTurn session Nothing "```json\n{\"answer\":7}\n```" "completed"
            "output-array" -> outputTurn session Nothing "[]" "completed"
            "output-scalar" -> outputTurn session Nothing "7" "completed"
            "output-malformed" -> outputTurn session (Just (toJSON ([] :: [Value]))) "" "completed"
            "output-retracted" -> do
              emit session (delta "{\"answer\":42}")
              emit session (object ["type" .= String "structured_output", "messageId" .= String "message", "structuredOutput" .= object ["answer" .= Number 42]])
              emit session (object ["type" .= String "assistant_message_retracted", "messageId" .= String "message"])
              emit session (completed "completed")
            "plain-json" -> outputTurn session Nothing "{\"answer\":7}" "completed"
            "mission-child-events" -> do
              emit "mission-child" (object ["type" .= String "mission_state_changed", "state" .= String "paused"])
              emit session (completed "completed")
            "mission-events" -> do
              emit "foreign-session" (object ["type" .= String "mission_state_changed", "state" .= String "future"])
              forM_ missionWireEvents (emit session)
              emit session (completed "completed")
            "bad-mission-event" -> emit session (object ["type" .= String "mission_progress_entry", "progressLog" .= [object ["type" .= String "future_entry"]]])
            "rich-events" -> richEvents session
            "snapshot-correction" -> do
              emit session (delta "draft")
              emit session (created "final")
              emit session (completed "completed")
            "later-partial" -> do
              emit session (created "Checking…")
              emit session (deltaFor "survivor" "Answer so far")
              emit session (completed "cancelled")
            "snapshot-continued" -> do
              emit session (created "Hello")
              emit session (delta " world")
              emit session (completed "cancelled")
            "snapshot-prefixes" -> do
              emit session (created "")
              emit session (created "Hel")
              emit session (created "Hello")
              emit session (completed "completed")
            "snapshot-regrowth" -> do
              forM_ ["Hello", "He", "Hello"] (emit session . created)
              emit session (completed "completed")
            "snapshot-regrowth-extra" -> do
              forM_ ["Hello", "He", "Hello!"] (emit session . created)
              emit session (completed "completed")
            "delta-regrowth" -> do
              forM_ ["Hello", "He"] (emit session . created)
              emit session (delta "llo")
              emit session (completed "completed")
            "divergent-update" -> do
              forM_ ["draft", "final"] (emit session . created)
              emit session (delta " answer")
              emit session (completed "completed")
            "retraction-replay" -> do
              emit session (created "Hello")
              emit session (object ["type" .= String "assistant_message_retracted", "messageId" .= String "message"])
              emit session (created "Hello")
              emit session (completed "completed")
            "retraction" -> do
              emit session (delta "discarded")
              emit session (created "discarded")
              emit session (object ["type" .= String "structured_output", "messageId" .= String "message", "structuredOutput" .= object ["discarded" .= True]])
              emit session (object ["type" .= String "assistant_message_retracted", "messageId" .= String "message"])
              emit session (deltaFor "survivor" "surviving")
              emit session (completed "completed")
            "structured-null" -> do
              emit session (object ["type" .= String "structured_output", "messageId" .= String "message", "structuredOutput" .= object ["value" .= Number 1]])
              emit session (object ["type" .= String "structured_output", "messageId" .= String "message", "structuredOutput" .= Null])
              emit session (completed "completed")
            "malformed-partial" -> emit session (object ["type" .= String "thinking_text_delta", "messageId" .= String "message", "blockIndex" .= Number 0, "textDelta" .= False])
            "malformed-message" -> emit session (object ["type" .= String "create_message", "message" .= object ["id" .= String "message", "role" .= String "assistant", "content" .= ([] :: [Value])]])
            "interrupt-denied" -> emit session (delta "started")
            "reported-error" -> do
              emit session (delta "partial")
              emit session (object ["type" .= String "error", "message" .= String "fixture failure", "errorType" .= String "Error", "timestamp" .= String "fixture-time"])
              emit session (completed "error")
            "spec" -> emit session (delta "spec plan") >> emit session (completed "spec_handoff")
            "permission-denied" -> emit session (completed "permission_rejected")
            "foreign-turn" -> do
              emit session (object ["type" .= String "agent_turn_completed", "turnId" .= String "foreign-turn", "reason" .= False])
              emit session (delta "current turn")
              emit session (completed "completed")
            "missing-turn-id" -> emitRawFor (Just session) (completed "completed")
            "interrupt-count" -> do
              count <- peerInterruptCount <$> readIORef state
              emit session (delta (Text.pack (show count)))
              emit session (completed "completed")
            "malformed" -> emit session (object ["type" .= String "assistant_text_delta", "messageId" .= String "message", "blockIndex" .= (0 :: Int)])
            "failed" -> emit session (completed "error")
            "prefilled" -> emit session (created "prefilled") >> emit session (completed "completed")
            "local" -> emitFor Nothing (delta "local") >> emitFor Nothing (completed "completed")
            "permissions" -> do
              ask "permission" "droid.request_permission" (object ["toolUses" .= ([] :: [Value]), "options" .= ([] :: [Value])]) (object ["selectedOption" .= String "cancel"])
              ask "question" "droid.ask_user" (object ["toolCallId" .= String "tool", "questions" .= ([] :: [Value])]) (object ["cancelled" .= True, "answers" .= ([] :: [Value])])
              emit session (delta "defaults denied")
              emit session (completed "completed")
            "turn" -> emit session (delta (Text.pack (show turn))) >> emit session (completed "completed")
            "loads" -> do
              history <- peerLoadHistory <$> readIORef state
              emit session (delta (Text.intercalate "," history))
              emit session (completed "completed")
            "handlers-permission" -> do
              ask "permission" "droid.request_permission" (fixturePermission session [ConfirmProceedOnce, ConfirmCancel]) permissionAccepted
              emit session (delta "permission handled")
              emit session (completed "completed")
            "handlers-edit" -> do
              ask "permission-edit" "droid.request_permission" (fixturePermission session [ConfirmProceedEdit, ConfirmCancel]) (object ["selectedOption" .= String "proceed_edit", "editedSpecContent" .= String ""])
              emit session (completed "completed")
            "handlers-cancel" -> do
              ask "permission-cancel" "droid.request_permission" (fixturePermission session [ConfirmProceedOnce, ConfirmCancel]) (object ["selectedOption" .= String "cancel"])
              emit session (completed "permission_rejected")
            "handlers-malformed" -> do
              ask "permission-malformed" "droid.request_permission" (object ["toolUses" .= False, "options" .= ([] :: [Value])]) (object ["selectedOption" .= String "cancel"])
              emit session (completed "permission_rejected")
            "handlers-question" -> do
              ask "question" "droid.ask_user" fixtureQuestions fixtureAnswers
              emit session (completed "completed")
            "handlers-question-cancel" -> do
              ask "question" "droid.ask_user" fixtureQuestions (object ["cancelled" .= True, "answers" .= ([] :: [Value])])
              emit session (completed "completed")
            "handlers-parallel" -> do
              forM_ ["one", "two"] $ \identifier -> writeFrame (envelope ["type" .= String "request", "id" .= identifier, "method" .= String "droid.request_permission", "params" .= fixturePermission identifier [ConfirmProceedOnce, ConfirmCancel]])
              first <- serverAnswer
              second <- serverAnswer
              unless (fst first /= fst second && all (\(identifier, value) -> identifier `elem` ["one", "two"] && value == permissionAccepted) [first, second]) (throwIO CallbackAbort)
              emit session (completed "completed")
            "handlers-late" -> do
              writeFrame (envelope ["type" .= String "request", "id" .= String "late-permission", "method" .= String "droid.request_permission", "params" .= fixturePermission session [ConfirmProceedOnce, ConfirmCancel]])
              emit session (completed "completed")
            "handlers-flush" -> do
              received <- peerLatePermission <$> readIORef state
              unless received $ do
                answer <- serverAnswer
                unless (answer == ("late-permission", permissionAccepted)) (throwIO CallbackAbort)
              emit session (delta "late response received")
              emit session (completed "completed")
            "handlers-blocked" -> do
              writeFrame (envelope ["type" .= String "request", "id" .= String "blocked-permission", "method" .= String "droid.request_permission", "params" .= fixturePermission session [ConfirmProceedOnce, ConfirmCancel]])
              writeFrame (envelope ["type" .= String "request", "id" .= String "blocked-question", "method" .= String "droid.ask_user", "params" .= fixtureQuestions])
              _ <- serverAnswer
              throwIO CallbackAbort
            "model" -> emit session (delta model) >> emit session (completed "completed")
            "system-prompt" -> do
              storedPrompt <- peerSystemPrompt <$> readIORef state
              emit session (delta (Text.decodeUtf8 (BL.toStrict (encode (fromMaybe Null storedPrompt)))))
              emit session (completed "completed")
            "settings-missing" -> emit session (object ["type" .= String "settings_updated"])
            "settings-null-spec" -> emit session (object ["type" .= String "settings_updated", "settings" .= object ["specModeModelId" .= Null]])
            "peer-pid" -> do
              pid <- getProcessID
              emit session (delta ("fixture-" <> Text.pack (show pid)))
              emit session (completed "completed")
            _ | Text.isPrefixOf "stop-gate:" prompt || Text.isPrefixOf "submit-gate:" prompt -> emit session (delta "started")
            _ -> do
              emit "other-session" (delta "ignored")
              emit session (object ["type" .= String "droid_working_state_changed", "newState" .= String "idle"])
              emit session (created "")
              emit session (delta "Hello ")
              emit session (delta "سلام\n😀")
              emit session (created "Hello سلام\n😀")
              emit session (completed "completed")
          loop session (turn + 1) model
        Nothing | KeyMap.lookup "id" request == Just (String "late-permission") -> do
          expect "result" permissionAccepted request
          modifyIORef' state (\current -> current {peerLatePermission = True})
          loop session turn model
        _ -> do
          handled <- handleMcpRequest "droid." session request respondMcp emit readFrame
          if handled then loop session turn model else throwIO CallbackAbort
    recordMcpPolicy params = do
      modifyIORef' state (\current -> current {peerMcpHistory = peerMcpHistory current <> [KeyMap.filterWithKey (\key _ -> key `elem` ["mcpServers", "mcpOAuthCallbackUri", "blockOnMcpLoad"]) params]})
      let source = case KeyMap.lookup "sessionId" params of Just (String value) -> Just value; _ -> Nothing
      forM_ (earlyMcpEvents params) (emitRawFor source)
      invokeHosted params
    expectMcpLoadKeys params = expectKeys (["autoRejectPermissionRequests", "sessionId"] <> filter (`KeyMap.member` params) ["mcpServers", "mcpOAuthCallbackUri"]) params
    respondMcp original fields = do
      history <- peerMcpHistory <$> readIORef state
      let decorate (key, Object value) | key == "result", KeyMap.lookup "method" original == Just (String "droid.list_mcp_registry") = (key, Object (KeyMap.insert "mcpHistory" (toJSON history) value))
          decorate pair = pair
      writeFrame (response original (map decorate fields))
    richEvents session = do
      active <- peerActiveTurn <$> readIORef state
      identifier <- maybe (throwIO CallbackAbort) pure active
      let tool = object ["type" .= String "tool_use", "id" .= String "call-1", "name" .= String "Read", "input" .= object ["path" .= String "not-opened"]]
      forM_
        [ messageCreated identifier "user" [object ["type" .= String "text", "text" .= String "prompt"]],
          object ["type" .= String "thinking_text_delta", "messageId" .= String "message", "blockIndex" .= Number 0, "textDelta" .= String "thinking"],
          object ["type" .= String "thinking_text_complete", "messageId" .= String "message", "blockIndex" .= Number 0],
          object ["type" .= String "tool_call", "toolUse" .= tool],
          messageCreated "tool-message" "assistant" [tool],
          object ["type" .= String "tool_progress_update", "toolUseId" .= String "call-1", "toolName" .= String "Read", "update" .= object ["type" .= String "status", "text" .= String "working"]],
          object ["type" .= String "tool_result", "messageId" .= String "tool-result", "toolUseId" .= String "call-1", "content" .= String "file data", "isError" .= False],
          object ["type" .= String "hook_execution_started", "hookId" .= String "hook", "hookEventName" .= String "PostToolUse", "hookCommands" .= [object ["command" .= String "not executed"]]],
          object ["type" .= String "hook_execution_completed", "hookId" .= String "hook", "hookStatus" .= String "completed"],
          object ["type" .= String "session_token_usage_changed", "sessionId" .= session, "tokenUsage" .= TokenUsage 10 2 0 5 1 (Just 0.25) mempty],
          object ["type" .= String "droid_working_state_changed", "newState" .= String "streaming_assistant_message"],
          object ["type" .= String "permission_resolved", "requestId" .= String "permission", "toolUseIds" .= [String "call-1"], "selectedOption" .= String "cancel"],
          object ["type" .= String "settings_updated", "requestId" .= String "settings-request", "settings" .= object ["interactionMode" .= String "spec", "autonomyLevel" .= String "unknown", "availableAutonomyLevels" .= [String "off", String "low"], "specModeModelId" .= String "reported-spec"]],
          object ["type" .= String "session_title_updated", "title" .= String "Title"],
          object ["type" .= String "session_working_directory_changed", "cwd" .= String "/reported-path"],
          object ["type" .= String "future_notice", "private" .= String "fixture secret"],
          object ["type" .= String "error", "message" .= String "recoverable observation", "errorType" .= String "Error", "timestamp" .= String "fixture-time"],
          delta "draft text",
          object ["type" .= String "assistant_text_complete", "messageId" .= String "message", "blockIndex" .= Number 0],
          created "final answer",
          object ["type" .= String "structured_output", "messageId" .= String "message", "structuredOutput" .= object ["answer" .= Number 42]],
          completed "completed"
        ]
        (emit session)
    reject request code message = do
      pid <- getProcessID
      writeFrame (response request ["error" .= object ["code" .= (code :: JsonRpcErrorCode), "message" .= (message :: Text), "data" .= object ["peerId" .= ("fixture-" <> Text.pack (show pid))]]])
    ask identifier method params expected = do
      writeFrame (envelope ["type" .= String "request", "id" .= identifier, "method" .= (method :: Text), "params" .= params])
      (responseId, value) <- serverAnswer
      unless (String responseId == identifier && value == expected) (throwIO CallbackAbort)
    serverAnswer = do
      answer <- readFrame
      case KeyMap.lookup "method" answer of
        Just (String "droid.list_models") -> do
          writeFrame (response answer ["result" .= object ["models" .= ([] :: [Value])]])
          serverAnswer
        Just (String "droid.interrupt_session") -> do
          writeFrame (response answer ["result" .= object []])
          serverAnswer
        Nothing -> case (KeyMap.lookup "id" answer, KeyMap.lookup "result" answer) of
          (Just (String identifier), Just value) -> pure (identifier, value)
          _ -> throwIO CallbackAbort
        _ -> throwIO CallbackAbort
    outputTurn :: Text -> Maybe Value -> Text -> Text -> IO ()
    outputTurn session output text reason = do
      forM_ output (\value -> emit session (object ["type" .= String "structured_output", "messageId" .= String "message", "structuredOutput" .= value]))
      unless (Text.null text) (emit session (delta text))
      emit session (completed reason)
    emit :: Text -> Value -> IO ()
    emit session = emitFor (Just session)
    emitFor :: Maybe Text -> Value -> IO ()
    emitFor session notification = do
      active <- peerActiveTurn <$> readIORef state
      case notification of
        Object fields | KeyMap.lookup "type" fields == Just (String "agent_turn_completed") -> do
          let tagged = if KeyMap.member "turnId" fields then fields else KeyMap.insert "turnId" (maybe Null String active) fields
          emitRawFor session (Object tagged)
          when (KeyMap.lookup "turnId" tagged == (String <$> active)) (modifyIORef' state (\current -> current {peerActiveTurn = Nothing}))
        _ -> emitRawFor session notification
    emitRawFor session notification = writeFrame (envelope ["type" .= String "notification", "method" .= String "droid.session_notification", "params" .= object (["notification" .= notification] <> maybe [] (\identifier -> ["sessionId" .= (identifier :: Text)]) session)])
    delta = deltaFor "message"
    deltaFor identifier text = object ["type" .= String "assistant_text_delta", "messageId" .= (identifier :: Text), "blockIndex" .= (0 :: Int), "textDelta" .= (text :: Text)]
    created text = messageCreated "message" "assistant" [object ["type" .= String "text", "text" .= (text :: Text)]]
    messageCreated identifier role blocks = object ["type" .= String "create_message", "message" .= object ["id" .= (identifier :: Text), "role" .= (role :: Text), "createdAt" .= Number 0, "updatedAt" .= Number 0, "content" .= (blocks :: [Value])]]
    completed reason = object ["type" .= String "agent_turn_completed", "reason" .= (reason :: Text), "tokenUsage" .= TokenUsage 10 2 0 5 1 (Just 0.25) mempty, "durationMs" .= Number 12.5]
    remember session turn model = modifyIORef' state (\current -> current {peerSessions = (session, (turn, model)) : filter ((/= session) . fst) (peerSessions current)})
    requestParams request = case KeyMap.lookup "params" request of
      Just (Object params) -> pure params
      _ -> throwIO CallbackAbort
    replacePeer request kind session turn model params metadata =
      case KeyMap.lookup "fixtureFailure" params of
        Just (String "remote") -> reject request RpcInvalidParams "fixture replacement rejected"
        Just (String "invalid") -> writeFrame (response request ["result" .= object ["newSessionId" .= False]])
        _ -> do
          pid <- getProcessID
          identifier <- case KeyMap.lookup "id" request of
            Just (String value) -> pure value
            _ -> throwIO CallbackAbort
          let target = kind <> "-" <> Text.pack (show pid) <> "-" <> identifier
          remember target turn model
          modifyIORef' state $ \current ->
            current
              { peerFailedLoads = [target | KeyMap.lookup "fixtureLoadFailure" params == Just (Bool True)] <> [session | KeyMap.lookup "fixtureRollbackFailure" params == Just (Bool True)] <> peerFailedLoads current,
                peerGatedLoads = [(target, Text.unpack path) | Just (String path) <- [KeyMap.lookup "fixtureLoadGate" params]] <> [(session, Text.unpack path) | Just (String path) <- [KeyMap.lookup "fixtureRollbackGate" params]] <> peerGatedLoads current,
                peerReplayLoads = [replayed | KeyMap.lookup "fixtureReplay" params == Just (Bool True), replayed <- [target, session]] <> peerReplayLoads current
              }
          case KeyMap.lookup "fixtureGate" params of
            Just (String path) -> peerGate (Text.unpack path)
            _ -> pure ()
          writeFrame (response request ["result" .= object (["newSessionId" .= target] <> metadata)])
    expect key value fields = unless (KeyMap.lookup key fields == Just value) (throwIO CallbackAbort)
    expectKeys keys fields = unless (KeyMap.size fields == length keys && all (`KeyMap.member` fields) keys) (throwIO CallbackAbort)
    response request fields = envelope (("type", String "response") : ("id", fromMaybe Null (KeyMap.lookup "id" request)) : fields)
    envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)
    gateHandshake = ask "gate-handshake" "droid.request_permission" (object ["toolUses" .= ([] :: [Value]), "options" .= ([] :: [Value])]) (object ["selectedOption" .= String "cancel"])
    enableTrace directory request = do
      writeFile (directory <> "/requests.jsonl") ""
      modifyIORef' state (\current -> current {peerTraceDirectory = Just directory})
      recordFrame request
    recordFrame frame = do
      directory <- peerTraceDirectory <$> readIORef state
      forM_ directory $ \path -> case KeyMap.lookup "method" frame of
        Just (String method) -> do
          let session = case KeyMap.lookup "params" frame of
                Just (Object params) -> fromMaybe Null (KeyMap.lookup "sessionId" params)
                _ -> Null
          BL.appendFile (path <> "/requests.jsonl") (encode (object ["method" .= method, "sessionId" .= session]) <> "\n")
        _ -> pure ()
    gateQuery request directory = do
      enableTrace directory request
      gateHandshake
      writeFile (directory <> "/ready") "ready"
      let waitRelease = do
            incoming <- hReady stdin
            when incoming $ do
              early <- readFrame
              reject early RpcConflict "request arrived before held query settled"
            released <- doesFileExist (directory <> "/release")
            unless released (threadDelay 1000 >> waitRelease)
      waitRelease
    readFrame = do
      frame <- BS.hGetLine stdin >>= either (const (throwIO CallbackAbort)) pure . eitherDecodeStrict'
      recordFrame frame
      pure frame
    writeFrame frame = BL.hPutStr stdout (encode frame <> "\n") >> hFlush stdout
