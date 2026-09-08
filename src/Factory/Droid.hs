{-# LANGUAGE OverloadedStrings #-}

-- | A small high-level SDK for the local Droid CLI. Ordinary callers provide
-- prompts and text/event callbacks; subprocesses, RPC IDs and subscriptions are owned
-- by the session scope. This path targets CLI 0.212.1 / protocol 1.201.1.
module Factory.Droid
  ( DroidOptions (..),
    defaultDroidOptions,
    DroidSession,
    DroidSessionStatus (..),
    droidSessionStatus,
    onDroidSessionEvent,
    DroidReplacementError (..),
    DroidRewindOptions (..),
    droidSessionId,
    DroidResult (..),
    DroidEvent (..),
    DroidStreamMode (..),
    DroidInput (..),
    droidInput,
    DroidOutput,
    DroidOutputError (..),
    DroidOutputResult (..),
    rawDroidOutput,
    jsonDroidOutput,
    SystemPromptConfig,
    customSystemPrompt,
    appendedSystemPrompt,
    DroidError (..),
    DroidHandlers (..),
    defaultDroidHandlers,
    DroidInteraction (..),
    DroidInteractionFailure (..),
    withDroidSession,
    withResumedDroidSession,
    withDroidSessionHandlers,
    withResumedDroidSessionHandlers,
    sendPrompt,
    sendDroidTurn,
    sendDroidEvents,
    sendDroidOutput,
    sendDroidOutputEvents,
    sendDroidInput,
    sendDroidInputEvents,
    sendDroidInputOutput,
    sendDroidInputOutputEvents,
    interruptDroidSession,
    runDroid,
    streamDroid,
    listDroidModels,
    listDroidTools,
    listDroidCommands,
    listDroidSkills,
    getDroidSettings,
    updateDroidSettings,
    setDroidSkillDisabled,
    getDroidContextStats,
    getDroidContextBreakdown,
    renameDroidSession,
    changeDroidWorkingDirectory,
    getDroidRewindInfo,
    forkDroidSession,
    compactDroidSession,
    rewindDroidSession,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTQueueIO, newTVarIO, readTQueue, readTVar, throwSTM, writeTQueue, writeTVar)
import Control.Exception (Exception, Handler (..), SomeException, bracket, bracket_, catches, evaluate, finally, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.Types qualified as UUID
import Data.UUID.V4 (nextRandom)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Input (DroidInput (..), droidDocumentSource, droidImageSource, droidInput)
import Factory.Droid.Interaction
import Factory.Droid.Internal.Output
import Factory.Droid.Internal.Stream
import Factory.Droid.Protocol
import Factory.Droid.Protocol.Dispatch
import Factory.Droid.Schema.Context (ContextStats, GetContextBreakdownResult)
import Factory.Droid.Schema.Control (AddUserMessageParams (..), ChangeWorkingDirectoryParams (..), ChangeWorkingDirectoryResult, CompactSessionParams, CompactSessionResult (..), ExecuteRewindParams (..), ExecuteRewindResult (..), ForkSessionParams, ForkSessionResult (..), GetRewindInfoParams (..), GetRewindInfoResult, OutputFormat, RenameSessionParams (..), RewindFileCreation, RewindFileSnapshot)
import Factory.Droid.Schema.Discovery (ListCommandsResult, ListSkillsResult, ListToolsResult, SetSkillDisabledParams)
import Factory.Droid.Schema.Models (ListModelsOptions, ListModelsResult)
import Factory.Droid.Schema.Notifications (AgentTurnCompleted (..), AgentTurnCompletionReason (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Schema.Settings (ListToolsOptions, SessionSettings, SettingsChange, SettingsUpdated (..), UpdateSessionSettingsParams (..), emptySettingsUpdate)
import Factory.Droid.Schema.SystemPrompt (SystemPromptConfig, appendedSystemPrompt, customSystemPrompt)
import Factory.Droid.Transport.Process qualified as Process
import System.Directory (makeAbsolute)
import System.Environment (getEnvironment)
import System.Process (CreateProcess (cwd, env))
import System.Timeout (timeout)

-- | Minimal local configuration. Authentication is inherited from the caller's
-- environment or the CLI's existing login; the SDK does not discover credentials.
-- A supplied model initializes new sessions or overrides a saved model after load.
-- A turn timeout covers submission and consumption, in microseconds.
-- The positive frame limit includes encoded attachments, excluding the newline.
-- System prompts apply only to new-session initialization.
data DroidOptions = DroidOptions
  { droidExecutable :: !FilePath,
    droidWorkingDirectory :: !FilePath,
    droidModel :: !(Maybe Text),
    droidTurnTimeoutMicros :: !(Maybe Int),
    droidFrameLimitBytes :: !Int,
    droidSystemPrompt :: !(Maybe SystemPromptConfig)
  }
  deriving stock (Eq)

instance Show DroidOptions where
  show _ = "DroidOptions <redacted>"

-- | Use the installed droid, the given directory and its default model. Turns
-- have no implicit deadline; asynchronous cancellation remains available.
defaultDroidOptions :: FilePath -> DroidOptions
defaultDroidOptions directory = DroidOptions "droid" directory Nothing Nothing (10 * 1024 * 1024) Nothing

-- | Local stream failures. Reported messages/reasons remain explicit data but
-- are not included in Show. RPC and process failures retain their existing types.
data DroidError = DroidInvalidEvent | DroidTurnTimedOut | DroidSessionUnusable | DroidSessionBusy | DroidSessionReplaced !Text | DroidReportedError !Text | DroidTurnFailed !AgentTurnCompletionReason | DroidSystemPromptRequiresNewSession
  deriving stock (Eq)

instance Show DroidError where
  show DroidInvalidEvent = "DroidInvalidEvent"
  show DroidTurnTimedOut = "DroidTurnTimedOut"
  show DroidSessionUnusable = "DroidSessionUnusable"
  show DroidSessionBusy = "DroidSessionBusy"
  show (DroidSessionReplaced _) = "DroidSessionReplaced <redacted>"
  show (DroidReportedError _) = "DroidReportedError <redacted>"
  show (DroidTurnFailed _) = "DroidTurnFailed <redacted>"
  show DroidSystemPromptRequiresNewSession = "DroidSystemPromptRequiresNewSession"

instance Exception DroidError

-- | SDK-known handle state. A retired handle retains its successor identifier.
data DroidSessionStatus = SessionReady | SessionRunning | SessionReplacing | SessionReplaced !Text | SessionUnavailable
  deriving stock (Eq)

instance Show DroidSessionStatus where
  show SessionReady = "SessionReady"
  show SessionRunning = "SessionRunning"
  show SessionReplacing = "SessionReplacing"
  show (SessionReplaced _) = "SessionReplaced <redacted>"
  show SessionUnavailable = "SessionUnavailable"

-- | A known successor could not be attached. Nothing means rollback succeeded.
-- Explicit causes may contain sensitive data; Show does not expose them.
data DroidReplacementError = DroidReplacementError
  { replacementSource :: !Text,
    replacementTarget :: !Text,
    replacementCause :: !SomeException,
    replacementRollbackError :: !(Maybe SomeException)
  }

instance Show DroidReplacementError where
  show _ = "DroidReplacementError <redacted>"

instance Exception DroidReplacementError

-- | Rewind choices for the owned session. The CLI, not the SDK, changes files.
data DroidRewindOptions = DroidRewindOptions
  { rewindMessageId :: !Text,
    rewindRestoreFiles :: ![RewindFileSnapshot],
    rewindDeleteFiles :: ![RewindFileCreation],
    rewindTitle :: !Text
  }
  deriving stock (Eq)

instance Show DroidRewindOptions where
  show _ = "DroidRewindOptions <redacted>"

-- | All successors remain owned by the original new/resumed session scope.
data DroidSession = DroidSession
  { droidSessionId :: !Text,
    sessionConnection :: !SessionConnection,
    sessionLifecycle :: !(TVar (DroidSessionStatus, Int)),
    sessionTurnTimeout :: !(Maybe Int),
    sessionInterrupts :: !(TVar Int),
    sessionSubmitted :: !(TVar Bool)
  }

data SessionConnection = SessionConnection
  { connectionChannel :: !RpcChannel,
    connectionDispatcher :: !RpcDispatcher,
    connectionCounter :: !(IORef Integer),
    connectionTurnLock :: !(MVar ()),
    connectionOpen :: !(TVar Bool),
    connectionAutoRejectPermissions :: !Bool,
    connectionSettings :: !(TVar (Maybe (Text, Either DroidError SessionSettings)))
  }

-- | Observe local lifecycle state; subsequent operations can still race closure.
droidSessionStatus :: DroidSession -> IO DroidSessionStatus
droidSessionStatus = atomically . sessionStatus

-- | Observe typed session notifications, including outside turns, without
-- consuming the turn stream. Left reports malformed events or a failed channel.
-- Callbacks run on the dispatcher intake: keep them brief. Ordinary queries are
-- permitted, but do not start a turn, replace/load a session, or wait for later
-- events inside a callback. Ordinary callback exceptions are isolated.
--
-- The returned unsubscribe is idempotent and prevents later admission; an
-- already admitted callback may finish. Replacing/retired/unavailable handles
-- admit no new notifications, and all subscriptions end with the connection.
onDroidSessionEvent :: DroidSession -> (Either DroidError DroidEvent -> IO ()) -> IO (IO ())
onDroidSessionEvent session callback = mask_ $ do
  atomically (ensureSession session)
  let dispatcher = connectionDispatcher (sessionConnection session)
      observe notification = do
        active <-
          atomically $
            sessionStatus session >>= \case
              SessionReady -> pure True
              SessionRunning -> pure True
              _ -> pure False
        when active $ case decodeSessionNotification (droidSessionId session) notification of
          Left _ -> callback (Left DroidInvalidEvent)
          Right events -> forM_ events (callback . Right)
      failed _ = do
        retired <-
          atomically $
            readTVar (sessionLifecycle session) >>= \case
              (SessionReplaced _, _) -> pure True
              _ -> pure False
        unless retired (callback (Left DroidSessionUnusable))
  unsubscribe <- onRpcNotification dispatcher observe
  unsubscribeError <- onRpcError dispatcher failed `onException` unsubscribe
  pure (unsubscribeError >> unsubscribe)

-- | Own a local CLI process and one session, including follow-up prompts.
-- Unhandled permission requests and questions are rejected. Unsupported SDK
-- language tags are left unset rather than impersonating Python/TypeScript.
-- Scope exit releases handlers, RPC reader and child process, including errors.
withDroidSession :: DroidOptions -> (DroidSession -> IO a) -> IO a
withDroidSession options = withDroidSessionHandlers options defaultDroidHandlers

-- | Load a saved session in a new owned CLI process. The CLI restores its saved
-- working directory and conversation; the options directory controls only launch.
-- A missing model override retains the saved model. Load failures never create a
-- replacement session. Permission rejection and scoped cleanup match new sessions.
withResumedDroidSession :: DroidOptions -> Text -> (DroidSession -> IO a) -> IO a
withResumedDroidSession options = withResumedDroidSessionHandlers options defaultDroidHandlers

-- | Explicit handlers live for the original CLI connection, including startup
-- and successor sessions. Ordinary callback failures cancel their interaction;
-- dispatcher-owned workers are cancelled/joined before the process scope closes.
withDroidSessionHandlers :: DroidOptions -> DroidHandlers -> (DroidSession -> IO a) -> IO a
withDroidSessionHandlers options handlers = withLocalSession options handlers Nothing

-- | Resume with the same connection-scoped handler and cleanup policy.
withResumedDroidSessionHandlers :: DroidOptions -> DroidHandlers -> Text -> (DroidSession -> IO a) -> IO a
withResumedDroidSessionHandlers options handlers identifier = withLocalSession options handlers (Just identifier)

withLocalSession :: DroidOptions -> DroidHandlers -> Maybe Text -> (DroidSession -> IO a) -> IO a
withLocalSession options handlers saved action = do
  case (saved, droidSystemPrompt options) of
    (Just _, Just _) -> throwIO DroidSystemPromptRequiresNewSession
    _ -> pure ()
  forM_ (droidTurnTimeoutMicros options) $ \micros -> unless (micros >= 0) (throwIO RpcInvalidTimeout)
  directory <- makeAbsolute (droidWorkingDirectory options)
  environment <- filter (\(key, _) -> key `notElem` ["FACTORY_UPSTREAM_CLIENT_TYPE", "FACTORY_UPSTREAM_SDK"]) <$> getEnvironment
  let process = (Process.proc (droidExecutable options) ["exec", "--input-format", "stream-jsonrpc", "--output-format", "stream-jsonrpc"]) {cwd = Just directory, env = Just environment}
  Process.withJsonLinesProcess (droidFrameLimitBytes options) 5000000 process $ \transport ->
    withRpcChannel (Process.sendObject transport) (Process.receiveObject transport) $ \channel ->
      withRpcDispatcher channel context $ \dispatcher -> do
        counter <- newIORef 0
        lock <- newMVar ()
        open <- newTVarIO True
        settings <- newTVarIO Nothing
        let rejectPermissions = isNothing (onDroidPermission handlers)
            connection = SessionConnection channel dispatcher counter lock open rejectPermissions settings
        void (onRpcError dispatcher (\_ -> atomically (writeTVar open False)))
        void (onRpcNotification dispatcher (atomically . observeSettingsNotification connection))
        void (registerRpcHandler dispatcher "droid.request_permission" (permissionRpcHandler handlers))
        void (registerRpcHandler dispatcher "droid.ask_user" (questionRpcHandler handlers))
        identifier <- case saved of
          Nothing -> sessionBoundary connection $ do
            let params = KeyMap.fromList (["machineId" .= String "default", "cwd" .= directory, "autoRejectPermissionRequests" .= rejectPermissions] <> maybe [] (\model -> ["modelId" .= model]) (droidModel options) <> maybe [] (\prompt -> ["systemPrompt" .= prompt]) (droidSystemPrompt options))
            callSettings connection Nothing "droid.initialize_session" params
          Just identifier -> do
            loadSession connection identifier
            forM_ (droidModel options) $ \model ->
              sessionBoundary connection $
                void (connectionRequest connection 30000000 (\rpc callOptions -> Client.updateSessionSettings rpc callOptions (emptySettingsUpdate {updateSettingsModel = Just model})))
            pure identifier
        session <- newSessionHandle identifier connection (droidTurnTimeoutMicros options)
        action session
          `finally` atomically (writeTVar open False)

-- | Run a prompt without a streaming callback and close the session afterward.
runDroid :: DroidOptions -> Text -> IO DroidResult
runDroid options prompt = streamDroid options prompt (\_ -> pure ())

-- | Run one prompt, deliver text chunks and close the session afterward.
streamDroid :: DroidOptions -> Text -> (Text -> IO ()) -> IO DroidResult
streamDroid options prompt callback = withDroidSession options (\session -> sendPrompt session prompt callback)

-- | Read the model catalog with explicit discovery options. Read-only queries
-- have a thirty-second exchange deadline and may run from a text callback.
listDroidModels :: DroidSession -> ListModelsOptions -> IO ListModelsResult
listDroidModels session params = sessionRequest ReadOnlyRequest session (\channel options -> Client.listModels channel options params)

-- | Query hypothetical native-tool availability without changing settings.
listDroidTools :: DroidSession -> ListToolsOptions -> IO ListToolsResult
listDroidTools session params = sessionRequest ReadOnlyRequest session (\channel options -> Client.listTools channel options params)

-- | Read command metadata; executable flags do not execute anything locally.
listDroidCommands :: DroidSession -> IO ListCommandsResult
listDroidCommands session = sessionRequest ReadOnlyRequest session (\channel options -> Client.listCommands channel options mempty)

-- | Read skill metadata without opening resource paths or invoking skills.
listDroidSkills :: DroidSession -> IO ListSkillsResult
listDroidSkills session = sessionRequest ReadOnlyRequest session (\channel options -> Client.listSkills channel options mempty)

-- | Read last dispatcher-observed settings without sending or waiting for an
-- RPC. Inside a session observer this includes the current notification; queued
-- frames may remain. A malformed settings event makes this getter throw
-- DroidInvalidEvent until a valid full load establishes a new baseline.
-- Lifecycle and session-ID checks are atomic with the read.
getDroidSettings :: DroidSession -> IO SessionSettings
getDroidSettings session = atomically $ do
  ensureSession session
  observed <- readTVar (connectionSettings (sessionConnection session))
  case observed of
    Just (identifier, settings) | identifier == droidSessionId session -> either throwSTM pure settings
    _ -> throwSTM DroidSessionUnusable

-- | Apply a partial settings update and retain the peer's acknowledgement.
-- No optimistic settings cache is maintained. Unknown mutation outcomes invalidate
-- under the same lease rules as other session controls.
updateDroidSettings :: DroidSession -> UpdateSessionSettingsParams -> IO EmptyObject
updateDroidSettings session params = sessionRequest MutatingRequest session (\channel options -> Client.updateSessionSettings channel options params)

-- | Ask the CLI to change a skill's disabled state; False remains peer data.
setDroidSkillDisabled :: DroidSession -> SetSkillDisabledParams -> IO SuccessResult
setDroidSkillDisabled session params = sessionRequest MutatingRequest session (\channel options -> Client.setSkillDisabled channel options params)

-- | Read peer-reported aggregate context usage without interrupting a turn.
getDroidContextStats :: DroidSession -> IO ContextStats
getDroidContextStats session = sessionRequest ReadOnlyRequest session (\channel options -> Client.getContextStats channel options mempty)

-- | Read peer-reported context categories and resource usage.
getDroidContextBreakdown :: DroidSession -> IO GetContextBreakdownResult
getDroidContextBreakdown session = sessionRequest ReadOnlyRequest session (\channel options -> Client.getContextBreakdown channel options mempty)

-- | Ask Droid to rename this session, retaining its actual success flag.
renameDroidSession :: DroidSession -> Text -> IO SuccessResult
renameDroidSession session title = sessionRequest MutatingRequest session (\channel options -> Client.renameSession channel options (RenameSessionParams title mempty))

-- | Change the CLI's working directory and return its resolved path. This does
-- not change the caller's process directory; the CLI enforces runtime restrictions.
changeDroidWorkingDirectory :: DroidSession -> Text -> IO ChangeWorkingDirectoryResult
changeDroidWorkingDirectory session directory = sessionRequest MutatingRequest session (\channel options -> Client.changeWorkingDirectory channel options (ChangeWorkingDirectoryParams directory mempty))

-- | Read rewind metadata for this session without restoring or deleting files.
getDroidRewindInfo :: DroidSession -> Text -> IO GetRewindInfoResult
getDroidRewindInfo session message = sessionRequest ReadOnlyRequest session (\channel options -> Client.getRewindInfo channel options (GetRewindInfoParams (droidSessionId session) message mempty))

-- | Fork, attach the successor on this connection, then retire this handle.
-- Replacement is rejected during a turn. The original scope owns the successor.
-- Retirement is the atomic commit point. If it wins a cancellation race, the
-- source stays replaced even if the caller does not receive the returned handle;
-- its status retains the committed target ID. No scope lifetime is extended.
forkDroidSession :: DroidSession -> ForkSessionParams -> IO DroidSession
forkDroidSession session params = fst <$> replaceSession session 30000000 (\channel options -> Client.forkSession channel options params) forkedSessionId

-- | Compact with the reference four-minute exchange deadline. Compaction may
-- invoke the CLI's model; returned counts are peer reports, not SDK measurements.
compactDroidSession :: DroidSession -> CompactSessionParams -> IO (DroidSession, CompactSessionResult)
compactDroidSession session params = replaceSession session 240000000 (\channel options -> Client.compactSession channel options params) compactionNewSessionId

-- | Execute the caller's restoration/deletion choices and attach the successor.
rewindDroidSession :: DroidSession -> DroidRewindOptions -> IO (DroidSession, ExecuteRewindResult)
rewindDroidSession session options =
  let params = ExecuteRewindParams (droidSessionId session) (rewindMessageId options) (rewindRestoreFiles options) (rewindDeleteFiles options) (rewindTitle options) mempty
   in replaceSession session 60000000 (\channel callOptions -> Client.executeRewind channel callOptions params) rewindNewSessionId

-- | Convenience prompt API: successful completion/spec handoff returns a result;
-- other settled outcomes throw DroidTurnFailed without invalidating the session.
sendPrompt :: DroidSession -> Text -> (Text -> IO ()) -> IO DroidResult
sendPrompt session prompt callback = do
  result <- sendDroidTurn session prompt callback
  case turnCompletionReason (resultCompletion result) of
    TurnCompleted -> pure result
    TurnSpecHandoff -> pure result
    reason -> throwIO (DroidTurnFailed reason)

-- | Return every terminal outcome with an append-only text callback. Snapshots
-- and retractions cannot undo chunks. A divergent block resumes delivery only
-- when its text extends the previously delivered prefix; 'sendDroidEvents'
-- exposes raw deltas and snapshots for a replaceable display.
sendDroidTurn :: DroidSession -> Text -> (Text -> IO ()) -> IO DroidResult
sendDroidTurn session prompt = sendDroidInput session (droidInput prompt)

-- | Observe complete message-level artifacts or all partial/metadata events.
-- Mode changes callback delivery, not accumulation or the terminal result.
sendDroidEvents :: DroidSession -> DroidStreamMode -> Text -> (DroidEvent -> IO ()) -> IO DroidResult
sendDroidEvents session mode prompt = sendDroidInputEvents session mode (droidInput prompt)

-- | Request structured output and retain both the wire terminal outcome and
-- local decoding outcome. Adaptation follows the settled turn; missing/invalid
-- output does not invalidate the session. The turn deadline excludes adaptation.
sendDroidOutput :: DroidSession -> DroidOutput a -> Text -> (Text -> IO ()) -> IO (DroidOutputResult a)
sendDroidOutput session output prompt = sendDroidInputOutput session output (droidInput prompt)

-- | Structured output with complete/all-event callbacks. Raw events are not
-- replaced by locally adapted values; the latter are returned after completion.
sendDroidOutputEvents :: DroidSession -> DroidOutput a -> DroidStreamMode -> Text -> (DroidEvent -> IO ()) -> IO (DroidOutputResult a)
sendDroidOutputEvents session output mode prompt = sendDroidInputOutputEvents session output mode (droidInput prompt)

-- | A text turn with validated attachments. Callback and terminal semantics
-- match 'sendDroidTurn'; file constructors read before turn submission.
sendDroidInput :: DroidSession -> DroidInput -> (Text -> IO ()) -> IO DroidResult
sendDroidInput session input callback = runEventTurn session (promptInput input Nothing) (\_ pieces -> forM_ pieces callback)

-- | A complete/all-event turn with validated attachments.
sendDroidInputEvents :: DroidSession -> DroidStreamMode -> DroidInput -> (DroidEvent -> IO ()) -> IO DroidResult
sendDroidInputEvents session mode input callback = runEventTurn session (promptInput input Nothing) (\event _ -> when (mode == AllEvents || completeEvent event) (callback event))

-- | Combine validated attachments and a structured-output specification.
sendDroidInputOutput :: DroidSession -> DroidOutput a -> DroidInput -> (Text -> IO ()) -> IO (DroidOutputResult a)
sendDroidInputOutput session output input callback = do
  result <- runEventTurn session (promptInput input (Just (outputWireFormat output))) (\_ pieces -> forM_ pieces callback)
  evaluate (adaptOutput output result)

-- | Structured output and attachments with complete/all-event callbacks.
sendDroidInputOutputEvents :: DroidSession -> DroidOutput a -> DroidStreamMode -> DroidInput -> (DroidEvent -> IO ()) -> IO (DroidOutputResult a)
sendDroidInputOutputEvents session output mode input callback = do
  result <- runEventTurn session (promptInput input (Just (outputWireFormat output))) (\event _ -> when (mode == AllEvents || completeEvent event) (callback event))
  evaluate (adaptOutput output result)

promptInput :: DroidInput -> Maybe OutputFormat -> AddUserMessageParams
promptInput input output =
  AddUserMessageParams
    { userMessageText = inputText input,
      userMessageId = Nothing,
      userMessageContent = Nothing,
      userMessageImages = nonempty (map droidImageSource (inputImages input)),
      userMessageImagePaths = Nothing,
      userMessageFiles = nonempty (map droidDocumentSource (inputDocuments input)),
      userMessageOutputFormat = output,
      userMessageSkipAgentLoop = Nothing,
      userMessageQueuePlacement = Nothing,
      userMessageRole = Nothing,
      userMessageVisibility = Nothing,
      userMessageSource = Nothing,
      userMessageAdditionalFields = mempty
    }
  where
    nonempty [] = Nothing
    nonempty values = Just values

runEventTurn :: DroidSession -> AddUserMessageParams -> (DroidEvent -> [Text] -> IO ()) -> IO DroidResult
runEventTurn session input callback = withPrompt session $ do
  turnIdentifier <- UUID.toText <$> nextRandom
  let connection = sessionConnection session
      identifier = droidSessionId session
      channel = connectionChannel connection
      dispatcher = connectionDispatcher connection
      deadline = sessionTurnTimeout session
  events <- newTQueueIO
  let publish notification = case decodeNotification identifier turnIdentifier notification of
        Right decoded -> atomically (forM_ decoded (writeTQueue events . Right))
        Left _ -> atomically (writeTQueue events (Left DroidInvalidEvent))
      disconnected _ = atomically (writeTQueue events (Left DroidSessionUnusable))
      exchange = do
        synchronizeRpcEvents channel
        bracket (onRpcNotification dispatcher publish) id $ \_ ->
          bracket (onRpcError dispatcher disconnected) id $ \_ -> do
            _ <- connectionRequest connection 30000000 (\rpc options -> Client.addUserMessage rpc options (input {userMessageId = Just turnIdentifier}))
            atomically (writeTVar (sessionSubmitted session) True)
            collect initialStreamState
      collect state = do
        event <- atomically (readTQueue events) >>= either throwIO pure
        let (next, pieces) = stepStream state event
        callback event pieces
        case event of
          TurnCompletedEvent completion -> pure (finishStream identifier completion next)
          _ -> collect next
      invalidate = invalidateSession session
      timed = maybe (Just <$> exchange) (`timeout` exchange) deadline
  (timed >>= maybe (throwIO DroidTurnTimedOut) pure) `onException` invalidate

-- | Request interruption of the current turn; idle sessions are a no-op. The
-- acknowledgement is not completion: await sendDroidTurn for the terminal result.
-- Pending interrupts fence the next prompt so a delayed request cannot cancel it.
interruptDroidSession :: DroidSession -> IO ()
interruptDroidSession session = bracket acquire release $ \active -> when active $ do
  result <- timeout 30000000 $ do
    submitted <- atomically $ do
      status <- sessionStatus session
      case status of
        SessionUnavailable -> throwSTM DroidSessionUnusable
        SessionReplaced target -> throwSTM (DroidSessionReplaced target)
        _ -> pure ()
      submitted <- readTVar (sessionSubmitted session)
      if submitted then pure True else check (status /= SessionRunning) >> pure False
    when submitted $
      performSessionRequest MutatingRequest session $
        connectionRequest (sessionConnection session) 30000000 (\channel options -> void (Client.interruptSession channel options mempty))
  maybe (throwIO RpcRequestTimedOut) pure result
  where
    acquire = atomically $ do
      ensureSession session
      status <- sessionStatus session
      if status == SessionRunning
        then do
          beginUse session
          modifyTVar' (sessionInterrupts session) (+ 1)
          pure True
        else pure False
    release active = when active $ atomically $ do
      modifyTVar' (sessionInterrupts session) (subtract 1)
      endUse session

context :: JsonRpcEnvelope
context = WithEnvelope (Just "1.201.1") Nothing mempty

data RequestEffect = ReadOnlyRequest | MutatingRequest

-- A cancelled mutating RPC can outlive its caller on the peer. Invalidate
-- before releasing its lease so a waiting replacement cannot cross it.
sessionRequest :: RequestEffect -> DroidSession -> (RpcChannel -> Client.CallOptions -> IO a) -> IO a
sessionRequest effect session request =
  bracket_ (atomically (beginUse session)) (atomically (endUse session)) $
    performSessionRequest effect session (connectionRequest (sessionConnection session) 30000000 request)

performSessionRequest :: RequestEffect -> DroidSession -> IO a -> IO a
performSessionRequest effect session action = mask $ \restore -> do
  outcome <- try @SomeException (restore action)
  case outcome of
    Right result -> pure result
    Left cause -> do
      case (effect, fromException cause :: Maybe RpcResultError) of
        (MutatingRequest, Nothing) -> invalidateSession session
        _ -> pure ()
      throwIO cause

connectionRequest :: SessionConnection -> Int -> (RpcChannel -> Client.CallOptions -> IO a) -> IO a
connectionRequest connection deadline request = do
  identifier <- nextRequestId (connectionCounter connection)
  request (connectionChannel connection) (Client.CallOptions identifier context (Just deadline))

sessionStatus :: DroidSession -> STM DroidSessionStatus
sessionStatus session = do
  (status, _) <- readTVar (sessionLifecycle session)
  open <- readTVar (connectionOpen (sessionConnection session))
  pure $ case status of
    SessionReplaced _ -> status
    _ | not open -> SessionUnavailable
    _ -> status

ensureSession :: DroidSession -> STM ()
ensureSession session =
  sessionStatus session >>= \case
    SessionReady -> pure ()
    SessionRunning -> pure ()
    SessionReplacing -> throwSTM DroidSessionBusy
    SessionReplaced target -> throwSTM (DroidSessionReplaced target)
    SessionUnavailable -> throwSTM DroidSessionUnusable

setStatus :: DroidSession -> DroidSessionStatus -> STM ()
setStatus session status = modifyTVar' (sessionLifecycle session) (\(_, active) -> (status, active))

beginUse :: DroidSession -> STM ()
beginUse session = do
  ensureSession session
  modifyTVar' (sessionLifecycle session) (\(status, active) -> (status, active + 1))

endUse :: DroidSession -> STM ()
endUse session = modifyTVar' (sessionLifecycle session) (\(status, active) -> (status, active - 1))

withPrompt :: DroidSession -> IO a -> IO a
withPrompt session action = withMVar (connectionTurnLock (sessionConnection session)) $ \() ->
  bracket_
    ( atomically $ do
        beginUse session
        pending <- readTVar (sessionInterrupts session)
        check (pending == 0)
        writeTVar (sessionSubmitted session) False
        setStatus session SessionRunning
    )
    ( atomically $ do
        endUse session
        (status, _) <- readTVar (sessionLifecycle session)
        case status of
          SessionRunning -> setStatus session SessionReady
          _ -> pure ()
    )
    action

invalidateSession :: DroidSession -> IO ()
invalidateSession session = do
  atomically (setStatus session SessionUnavailable)
  let connection = sessionConnection session
  void (call (connectionChannel connection) (connectionCounter connection) (Just 1000000) "droid.interrupt_session" mempty)
    `catches` [Handler (\(_ :: RpcChannelError) -> pure ()), Handler (\(_ :: RpcResultError) -> pure ())]

loadSession :: SessionConnection -> Text -> IO ()
loadSession connection identifier =
  sessionBoundary connection $
    void (callSettings connection (Just identifier) "droid.load_session" (KeyMap.fromList ["sessionId" .= identifier, "autoRejectPermissionRequests" .= connectionAutoRejectPermissions connection]))

-- Full replies and notification updates write only on the same ordered intake.
-- Source lifecycle guards hide provisional successor settings until publication.
callSettings :: SessionConnection -> Maybe Text -> Text -> Object -> IO Text
callSettings connection saved method params = do
  identifier <- nextRequestId (connectionCounter connection)
  let parse fields = do
        sessionId <- maybe (fields .: "sessionId") pure saved
        forM_ saved $ \_ -> do
          snapshot <- fields .: "session"
          void (snapshot .: "messages" :: Parser [Value])
        settings <- fields .: "settings"
        pure (sessionId, settings)
      observe response = forM_ (decodeRpcResult response) $ \fields ->
        forM_ (parseEither parse fields) $ \(sessionId, settings) ->
          writeTVar (connectionSettings connection) (Just (sessionId, Right settings))
  response <- requestReplyObserved (connectionChannel connection) Nothing (context {envelopeBody = BaseRequest identifier method (Just (Object params)) mempty}) observe
  fields <- either throwIO pure (decodeRpcResult response)
  fst <$> parseSessionResult parse fields

observeSettingsNotification :: SessionConnection -> JsonRpcBaseNotification -> STM ()
observeSettingsNotification connection notification =
  case baseNotificationParams (envelopeBody notification) of
    Just (Object params)
      | Just (Object payload) <- KeyMap.lookup "notification" params,
        KeyMap.lookup "type" payload == Just (String "settings_updated") -> do
          observed <- readTVar (connectionSettings connection)
          forM_ observed $ \(identifier, current) ->
            case decodeSessionNotification identifier notification of
              Left _ -> writeTVar (connectionSettings connection) (Just (identifier, Left DroidInvalidEvent))
              Right events -> forM_ events $ \case
                SettingsUpdatedEvent update ->
                  let !next = current >>= (`mergeObservedSettings` settingsUpdateValues update)
                   in writeTVar (connectionSettings connection) (Just (identifier, next))
                _ -> pure ()
    _ -> pure ()

-- The selected CLI always reports current spec/mission overrides by presence;
-- omission clears those fields. Other fields are partial. Reparse the overlay
-- to validate snapshot-only extensions (notably sandbox) before exposing state.
mergeObservedSettings :: SessionSettings -> SettingsChange -> Either DroidError SessionSettings
mergeObservedSettings previous change =
  either (const (Left DroidInvalidEvent)) Right $
    parseEither
      ( withObject "SessionSettings" $ \old ->
          withObject
            "SettingsChange"
            (\new -> parseJSON (Object (KeyMap.union new (foldr KeyMap.delete old ["specModeModelId", "specModeReasoningEffort", "missionSettings"]))))
            (toJSON change)
      )
      (toJSON previous)

-- Replies still overtake dispatch, but snapshot observations have wire positions.
-- Drain that prefix before publishing a handle, under the same startup deadline.
sessionBoundary :: SessionConnection -> IO a -> IO a
sessionBoundary connection action = do
  result <- timeout 60000000 (action <* synchronizeRpcEvents (connectionChannel connection))
  maybe (throwIO RpcRequestTimedOut) pure result

replaceSession :: DroidSession -> Int -> (RpcChannel -> Client.CallOptions -> IO a) -> (a -> Text) -> IO (DroidSession, a)
replaceSession session deadline operation successorId = mask $ \restore -> do
  atomically (ensureSession session)
  let connection = sessionConnection session
      lock = connectionTurnLock connection
  tryTakeMVar lock >>= \case
    Nothing -> throwIO DroidSessionBusy
    Just () -> (`finally` putMVar lock ()) $ do
      atomically (ensureSession session >> setStatus session SessionReplacing)
      restore
        ( atomically $ do
            open <- readTVar (connectionOpen connection)
            unless open (throwSTM DroidSessionUnusable)
            (status, active) <- readTVar (sessionLifecycle session)
            unless (status == SessionReplacing) (throwSTM DroidSessionUnusable)
            check (active == 0)
        )
        `onException` atomically
          ( do
              (status, _) <- readTVar (sessionLifecycle session)
              when (status == SessionReplacing) (setStatus session SessionReady)
          )
      result <- try @SomeException (restore (connectionRequest connection deadline operation))
      case result of
        Left cause -> do
          case fromException cause of
            Just (RpcRemoteFailure _) -> atomically (setStatus session SessionReady)
            _ -> invalidateSession session
          throwIO cause
        Right value -> do
          let target = successorId value
          attached <- try @SomeException $ do
            restore (loadSession connection target)
            successor <- newSessionHandle target connection (sessionTurnTimeout session)
            atomically $ do
              open <- readTVar (connectionOpen connection)
              unless open (throwSTM DroidSessionUnusable)
              setStatus session (SessionReplaced target)
            pure successor
          case attached of
            Left cause -> rollbackReplacement session target cause
            Right successor -> pure (successor, value)

rollbackReplacement :: DroidSession -> Text -> SomeException -> IO a
rollbackReplacement session target cause = do
  restored <- try @SomeException $ do
    loadSession (sessionConnection session) (droidSessionId session)
    atomically $ do
      open <- readTVar (connectionOpen (sessionConnection session))
      unless open (throwSTM DroidSessionUnusable)
      setStatus session SessionReady
  rollback <- case restored of
    Right () -> pure Nothing
    Left failure -> do
      invalidateSession session
      pure (Just failure)
  case rollback of
    Just failure | not (replacementProtocolFailure failure) -> throwIO failure
    _ | replacementProtocolFailure cause -> throwIO (DroidReplacementError (droidSessionId session) target cause rollback)
    _ -> throwIO cause

replacementProtocolFailure :: SomeException -> Bool
replacementProtocolFailure cause =
  case (fromException cause :: Maybe RpcResultError, fromException cause :: Maybe RpcChannelError, fromException cause :: Maybe DroidError) of
    (Nothing, Nothing, Nothing) -> False
    _ -> True

nextRequestId :: IORef Integer -> IO Text
nextRequestId counter = Text.pack . show <$> atomicModifyIORef' counter (\n -> (n + 1, n))

call :: RpcChannel -> IORef Integer -> Maybe Int -> Text -> Object -> IO Object
call channel counter deadline method params = do
  identifier <- nextRequestId counter
  requestResult channel deadline (context {envelopeBody = BaseRequest identifier method (Just (Object params)) mempty})

newSessionHandle :: Text -> SessionConnection -> Maybe Int -> IO DroidSession
newSessionHandle identifier connection deadline =
  DroidSession identifier connection <$> newTVarIO (SessionReady, 0) <*> pure deadline <*> newTVarIO 0 <*> newTVarIO False

parseSessionResult :: (Object -> Parser a) -> Object -> IO a
parseSessionResult parser fields = either (const (throwIO DroidInvalidEvent)) pure (parseEither parser fields)
