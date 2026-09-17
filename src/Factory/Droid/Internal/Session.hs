{-# LANGUAGE OverloadedStrings #-}

-- | Shared scoped session/turn ownership, with explicit backend operations.
module Factory.Droid.Internal.Session
  ( DroidOptions (..),
    defaultDroidOptions,
    DroidSessionOptions (..),
    defaultDroidSessionOptions,
    droidSessionOptions,
    withDroidSessionOn,
    withObservedDroidSession,
    withObservedDroidSessionOn,
    withDroidSessionOnHandlers,
    withResumedDroidSessionOn,
    withResumedDroidSessionOnHandlers,
    DroidSession,
    DroidSessionStatus (..),
    droidSessionStatus,
    getDroidWorkingDirectory,
    getDroidWorkingDirectoryState,
    onDroidSessionEvent,
    onDroidMissionSnapshot,
    DroidReplacementError (..),
    DroidRewindOptions (..),
    droidSessionId,
    sessionConnection,
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
    DroidMcpFailure (..),
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
    listDroidMcpServers,
    listDroidMcpTools,
    listDroidMcpRegistry,
    addDroidMcpServer,
    removeDroidMcpServer,
    toggleDroidMcpServer,
    toggleDroidMcpTool,
    authenticateDroidMcpServer,
    cancelDroidMcpAuth,
    clearDroidMcpAuth,
    submitDroidMcpAuthCode,
    submitDroidMcpAuthError,
    getDroidSettings,
    getDroidMissionSnapshot,
    lookupMissionSnapshot,
    lookupMissionId,
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
    RequestEffect (..),
    sessionRequest,
    sessionRequestWithTimeout,
    SessionConnection (..),
    SessionBackend (..),
    LocalSessionState,
    SessionStore,
    newSessionStore,
    readSessionStore,
    withSessionConnection,
    withSessionConnectionSetup,
    installMcpObserver,
    newSessionHandle,
    withSessionUse,
    withSessionLease,
    closeDroidSession,
    waitDroidSessionIdle,
    isDroidSessionIdle,
    ownSessionCleanup,
    sessionBoundary,
    connectionBoundary,
    connectionRequest,
    callSettings,
    callSettingsResult,
    callSettingsResultObserved,
    callSettingsResultObservedWithin,
    callSettingsResultObservedWithAdmission,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVarIO, readTVar, throwSTM, writeTVar)
import Control.Exception (Exception, Handler (..), SomeException, bracket, bracket_, catches, evaluate, finally, fromException, mask, mask_, onException, throwIO, toException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), withObject, (.:), (.:!))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID.Types qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Unique (Unique, newUnique)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Input (DroidInput (..), droidDocumentSource, droidImageSource, droidInput)
import Factory.Droid.Interaction
import Factory.Droid.Internal.Attribution qualified as Attribution
import Factory.Droid.Internal.Output
import Factory.Droid.Internal.Stream
import Factory.Droid.MCP.Server qualified as Hosted
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Observability qualified as Obs
import Factory.Droid.Protocol
import Factory.Droid.Protocol.Dispatch
import Factory.Droid.Schema.Configuration qualified as Configuration
import Factory.Droid.Schema.Context (ContextStats, GetContextBreakdownResult)
import Factory.Droid.Schema.Control (AddUserMessageParams (..), ChangeWorkingDirectoryParams (..), ChangeWorkingDirectoryResult (..), CompactSessionParams, CompactSessionResult (..), ExecuteRewindParams (..), ExecuteRewindResult (..), ForkSessionParams, ForkSessionResult (..), GetRewindInfoParams (..), GetRewindInfoResult, OutputFormat, RenameSessionParams (..), RewindFileCreation, RewindFileSnapshot)
import Factory.Droid.Schema.Discovery (ListCommandsResult, ListSkillsResult, ListToolsResult, SetSkillDisabledParams)
import Factory.Droid.Schema.Enums (SessionOrigin (OriginSDK))
import Factory.Droid.Schema.MCP (ListMcpRegistryResult, ListMcpServersResult, ListMcpToolsResult, McpServerNameParams (..), RemoveMcpServerParams (..), SubmitMcpAuthCodeParams, SubmitMcpAuthErrorParams, ToggleMcpServerParams (..), ToggleMcpToolParams (..))
import Factory.Droid.Schema.MCP.Config (AddMcpServerParams, McpConfigurationError (..), McpSessionOptions (..), defaultMcpSessionOptions, validateMcpConfiguration)
import Factory.Droid.Schema.Mission (MissionSnapshot (..))
import Factory.Droid.Schema.Models (ListModelsOptions, ListModelsResult)
import Factory.Droid.Schema.Notifications (AgentTurnCompleted (..), AgentTurnCompletionReason (..), SessionTokenUsageChanged (..), SessionWorkingDirectoryChanged (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Schema.Session (SessionWorktreeInfo (initialWorktreePath))
import Factory.Droid.Schema.Settings (ListToolsOptions, SessionSettings, SettingsChange, SettingsUpdated (..), UpdateSessionSettingsParams (..), emptySettingsUpdate, settingsSystemPrompt)
import Factory.Droid.Schema.SystemPrompt (SystemPromptConfig, appendedSystemPrompt, customSystemPrompt)
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport (ObjectTransport (..), TransportLocality (..))
import Factory.Droid.Transport qualified as Transport
import Factory.Droid.Transport.Process qualified as Process
import System.Directory (makeAbsolute)
import System.Environment (getEnvironment)
import System.Process (CreateProcess (cwd))
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
    droidSystemPrompt :: !(Maybe SystemPromptConfig),
    droidMcpOptions :: !McpSessionOptions,
    droidHostedMcpServers :: ![Hosted.McpServer],
    droidLaunchOptions :: !Process.DroidLaunchOptions,
    droidMachineId :: !(Maybe Text),
    droidConfiguration :: !Configuration.SessionConfiguration,
    droidLoadConfiguration :: !Configuration.SessionLoadConfiguration
  }
  deriving stock (Eq)

instance Show DroidOptions where
  show _ = "DroidOptions <redacted>"

-- | Use the installed droid, the given directory and its default model. Turns
-- have no implicit deadline; asynchronous cancellation remains available.
defaultDroidOptions :: FilePath -> DroidOptions
defaultDroidOptions directory = DroidOptions "droid" directory Nothing Nothing (10 * 1024 * 1024) Nothing defaultMcpSessionOptions [] Process.defaultDroidLaunchOptions Nothing Configuration.defaultSessionConfiguration Configuration.defaultSessionLoadConfiguration

-- | Session settings independent of executable, arguments and transport limits.
data DroidSessionOptions = DroidSessionOptions
  { droidSessionWorkingDirectory :: !FilePath,
    droidSessionModel :: !(Maybe Text),
    droidSessionTimeoutMicros :: !(Maybe Int),
    droidSessionSystemPrompt :: !(Maybe SystemPromptConfig),
    droidSessionMcpOptions :: !McpSessionOptions,
    droidSessionHostedMcpServers :: ![Hosted.McpServer],
    droidSessionMachineId :: !(Maybe Text),
    droidSessionConfiguration :: !Configuration.SessionConfiguration,
    droidSessionLoadConfiguration :: !Configuration.SessionLoadConfiguration
  }
  deriving stock (Eq)

instance Show DroidSessionOptions where show _ = "DroidSessionOptions <redacted>"

defaultDroidSessionOptions :: FilePath -> DroidSessionOptions
defaultDroidSessionOptions directory = DroidSessionOptions directory Nothing Nothing Nothing defaultMcpSessionOptions [] Nothing Configuration.defaultSessionConfiguration Configuration.defaultSessionLoadConfiguration

droidSessionOptions :: DroidOptions -> DroidSessionOptions
droidSessionOptions options = DroidSessionOptions (droidWorkingDirectory options) (droidModel options) (droidTurnTimeoutMicros options) (droidSystemPrompt options) (droidMcpOptions options) (droidHostedMcpServers options) (droidMachineId options) (droidConfiguration options) (droidLoadConfiguration options)

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
    sessionSubmitted :: !(TVar Bool),
    sessionTurnLock :: !(MVar ()),
    sessionClosed :: !(TVar Bool),
    sessionCleanups :: !(TVar (Map Unique (IO ())))
  }

data SessionConnection = SessionConnection
  { connectionChannel :: !RpcChannel,
    connectionDispatcher :: !RpcDispatcher,
    connectionCounter :: !(IORef Integer),
    connectionRequestNamespace :: !Text,
    connectionOpen :: !(TVar Bool),
    connectionAutoRejectPermissions :: !Bool,
    connectionSettings :: !(TVar (Maybe Text, Map Text (Either DroidError SessionSettings))),
    connectionMission :: !(TVar Mission.MissionRegistry),
    connectionMissionTarget :: !(TVar (Maybe (Text, Maybe Text))),
    connectionUnscopedMission :: !(TVar (Map Text (Either DroidError (Maybe Mission.MissionStore)))),
    connectionBackend :: !SessionBackend,
    connectionMcpOptions :: !McpSessionOptions
  }

-- | Daemon cwd/load state already belongs to its controller. Only the local
-- backend carries this scoped state; there is no second daemon cwd cache.
data LocalSessionState = LocalSessionState
  { localLoadConfiguration :: !(TVar Configuration.SessionLoadConfiguration),
    localWorkingDirectories :: !(TVar (Map Text State.WorkingDirectoryState))
  }

-- Backend variation stays in wire operations and its existing state owner.
data SessionBackend = SessionBackend
  { backendContext :: !JsonRpcEnvelope,
    backendDecode :: !(Text -> Maybe Text -> JsonRpcBaseNotification -> Either String [DroidEvent]),
    backendSubmit :: !(RpcDispatcher -> RpcChannel -> Client.CallOptions -> Text -> AddUserMessageParams -> IO Object),
    backendInterrupt :: !(RpcChannel -> Client.CallOptions -> Text -> IO Object),
    backendLocalState :: !(Maybe LocalSessionState)
  }

localBackend :: LocalSessionState -> SessionBackend
localBackend state =
  SessionBackend
    context
    (\identifier expected -> maybe (decodeSessionNotification identifier) (decodeNotification identifier) expected)
    (\_ channel options _ -> Client.addUserMessage channel options)
    (\channel options _ -> Client.interruptSession channel options mempty)
    (Just state)

-- Durable observations are independent of one channel's request counter,
-- dispatcher, lifetime and unscoped mission routing.
data SessionStore = SessionStore
  { storeSettings :: !(TVar (Maybe Text, Map Text (Either DroidError SessionSettings))),
    storeMissions :: !(TVar Mission.MissionRegistry)
  }

newSessionStore :: IO SessionStore
newSessionStore = SessionStore <$> newTVarIO (Nothing, mempty) <*> newTVarIO Mission.emptyMissionRegistry

readSessionStore :: SessionStore -> STM (Map Text (Either DroidError SessionSettings), Mission.MissionRegistry)
readSessionStore store = (,) . snd <$> readTVar (storeSettings store) <*> readTVar (storeMissions store)

withSessionConnection :: RpcChannel -> SessionBackend -> Bool -> McpSessionOptions -> (SessionConnection -> IO a) -> IO a
withSessionConnection channel backend rejectPermissions mcpOptions action = do
  store <- newSessionStore
  withSessionConnectionSetup store channel backend rejectPermissions mcpOptions pure action

-- Setup may authenticate and install the rest of the owner's handlers before
-- queued events reach retained state. A reused store has no live predecessor.
withSessionConnectionSetup :: SessionStore -> RpcChannel -> SessionBackend -> Bool -> McpSessionOptions -> (SessionConnection -> IO b) -> (b -> IO a) -> IO a
withSessionConnectionSetup store channel backend rejectPermissions mcpOptions setup action =
  withRpcDispatcherSetup channel (backendContext backend) prepare $ \(connection, prepared) ->
    action prepared `finally` atomically (writeTVar (connectionOpen connection) False)
  where
    prepare dispatcher = do
      counter <- newIORef 0
      namespace <- UUID.toText <$> nextRandom
      open <- newTVarIO True
      missionTarget <- newTVarIO Nothing
      unscopedMission <- newTVarIO mempty
      let connection = SessionConnection channel dispatcher counter namespace open rejectPermissions (storeSettings store) (storeMissions store) missionTarget unscopedMission backend mcpOptions
      prepared <-
        ( do
            void (onRpcNotification dispatcher (atomically . observeSettingsNotification connection))
            void (onRpcNotification dispatcher (observeMissionNotification connection))
            void (onRpcError dispatcher (\_ -> atomically (writeTVar open False)))
            setup connection
        )
          `onException` atomically (writeTVar open False)
      atomically (modifyTVar' (storeSettings store) (\(_, settings) -> (Nothing, settings)))
      pure (connection, prepared)

-- Local replacement keeps its connection-wide observer; daemon attachments
-- register an owned observer and retain the returned cleanup.
installMcpObserver :: SessionConnection -> Text -> Maybe Text -> DroidHandlers -> IO (IO ())
installMcpObserver connection method owned handlers = case onDroidMcpEvent handlers of
  Nothing -> pure (pure ())
  Just callback -> mask_ $ do
    let dispatcher = connectionDispatcher connection
        observe notification = when (baseNotificationMethod (envelopeBody notification) == method) $
          case baseNotificationParams (envelopeBody notification) of
            Just (Object params)
              | Just (Object payload) <- KeyMap.lookup "notification" params,
                Just (String kind) <- KeyMap.lookup "type" payload,
                kind `elem` ["mcp_status_changed", "mcp_auth_required", "mcp_auth_completed"] ->
                  case parseEither (.:! "sessionId") params of
                    Left _ -> callback Nothing (Left DroidMcpInvalidEvent)
                    Right actual -> case backendDecode (connectionBackend connection) (fromMaybe (fromMaybe "" actual) owned) Nothing notification of
                      Left _ -> callback actual (Left DroidMcpInvalidEvent)
                      Right events -> forM_ events (callback actual . Right)
            _ -> pure ()
    stop <- onRpcNotification dispatcher observe
    stopError <- onRpcError dispatcher (callback owned . Left . DroidMcpConnectionFailure) `onException` stop
    pure (stopError >> stop)

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
onDroidSessionEvent session callback = subscribeSessionNotifications session observe (callback . Left)
  where
    connection = sessionConnection session
    observe notification = case backendDecode (connectionBackend connection) (droidSessionId session) Nothing notification of
      Left _ -> callback (Left DroidInvalidEvent)
      Right events -> forM_ events (callback . Right)

-- | Observe relevant notifications for the owned mission, including explicitly
-- associated sessions. This is not a turn subscription or permission grant.
onDroidMissionSnapshot :: DroidSession -> (Either DroidError (Maybe MissionSnapshot) -> IO ()) -> IO (IO ())
onDroidMissionSnapshot session callback = subscribeSessionNotifications session observe (callback . Left)
  where
    connection = sessionConnection session
    observe notification = do
      admitted <- atomically $ do
        notice <- missionNotificationContext connection notification
        registry <- readTVar (connectionMission connection)
        pure $ case notice of
          Just (Just origin, decoded)
            | origin == droidSessionId session || Mission.sharesMission origin (droidSessionId session) registry ->
                case decoded of
                  Left _ -> True
                  Right events -> any (missionEventAdmitted origin registry) events
          _ -> False
      when admitted (try @DroidError (getDroidMissionSnapshot session) >>= callback)

subscribeSessionNotifications :: DroidSession -> (JsonRpcBaseNotification -> IO ()) -> (DroidError -> IO ()) -> IO (IO ())
subscribeSessionNotifications session notify failedCallback = mask_ $ do
  atomically (ensureSession session)
  let connection = sessionConnection session
      dispatcher = connectionDispatcher connection
      observe notification = do
        active <-
          atomically $
            sessionStatus session >>= \case
              SessionReady -> pure True
              SessionRunning -> pure True
              _ -> pure False
        when active (notify notification)
      failed _ = do
        retired <- atomically $ do
          closed <- readTVar (sessionClosed session)
          (status, _) <- readTVar (sessionLifecycle session)
          pure $ closed || case status of SessionReplaced _ -> True; _ -> False
        unless retired (failedCallback DroidSessionUnusable)
  unsubscribe <- onRpcNotification dispatcher observe
  unsubscribeError <- onRpcError dispatcher failed `onException` unsubscribe
  ownSessionCleanup session (unsubscribeError >> unsubscribe)

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
withDroidSessionHandlers options = withObservedDroidSession Obs.defaultDroidObservability options Nothing

-- | Resume with the same connection-scoped handler and cleanup policy.
withResumedDroidSessionHandlers :: DroidOptions -> DroidHandlers -> Text -> (DroidSession -> IO a) -> IO a
withResumedDroidSessionHandlers options handlers identifier = withObservedDroidSession Obs.defaultDroidObservability options (Just identifier) handlers

-- | Own the RPC/session scope over borrowed local-protocol object I/O.
withDroidSessionOn :: DroidSessionOptions -> ObjectTransport -> (DroidSession -> IO a) -> IO a
withDroidSessionOn options transport = withDroidSessionOnHandlers options transport defaultDroidHandlers

withDroidSessionOnHandlers :: DroidSessionOptions -> ObjectTransport -> DroidHandlers -> (DroidSession -> IO a) -> IO a
withDroidSessionOnHandlers options transport = withObservedDroidSessionOn Obs.defaultDroidObservability options transport Nothing

withResumedDroidSessionOn :: DroidSessionOptions -> ObjectTransport -> Text -> (DroidSession -> IO a) -> IO a
withResumedDroidSessionOn options transport = withResumedDroidSessionOnHandlers options transport defaultDroidHandlers

withResumedDroidSessionOnHandlers :: DroidSessionOptions -> ObjectTransport -> DroidHandlers -> Text -> (DroidSession -> IO a) -> IO a
withResumedDroidSessionOnHandlers options transport handlers identifier = withObservedDroidSessionOn Obs.defaultDroidObservability options transport (Just identifier) handlers

-- | Explicit telemetry for an owned new/resumed session, without changing
-- comparable session settings or installing global callbacks.
withObservedDroidSession :: Obs.DroidObservability -> DroidOptions -> Maybe Text -> DroidHandlers -> (DroidSession -> IO a) -> IO a
withObservedDroidSession observability options saved handlers = withLocalSession observability (droidSessionOptions options) handlers saved LocalHost (withLocalTransport observability options)

-- | The equivalent borrowed-transport scope. Observability belongs to this
-- physical connection; session-handler changes do not rebind its sinks.
withObservedDroidSessionOn :: Obs.DroidObservability -> DroidSessionOptions -> ObjectTransport -> Maybe Text -> DroidHandlers -> (DroidSession -> IO a) -> IO a
withObservedDroidSessionOn observability options transport saved handlers = withLocalSession observability options handlers saved (transportLocality transport) ($ transport)

withLocalSession :: Obs.DroidObservability -> DroidSessionOptions -> DroidHandlers -> Maybe Text -> TransportLocality -> ((ObjectTransport -> IO a) -> IO a) -> (DroidSession -> IO a) -> IO a
withLocalSession observability options handlers saved locality acquire action = do
  case (saved, droidSessionSystemPrompt options) of
    (Just _, Just _) -> throwIO DroidSystemPromptRequiresNewSession
    _ -> pure ()
  when (isJust saved && (isJust (droidSessionMachineId options) || droidSessionConfiguration options /= Configuration.defaultSessionConfiguration)) (throwIO Configuration.InitializationOptionsOnResume)
  forM_ (droidSessionTimeoutMicros options) $ \micros -> unless (micros >= 0) (throwIO RpcInvalidTimeout)
  mcpOptions <- either throwIO pure (validateMcpConfiguration (droidSessionMcpOptions options))
  unless (isNothing saved || isNothing (sessionBlockOnMcpLoad mcpOptions)) (throwIO McpInitOnlyOptionOnResume)
  when (isNothing saved) $ either throwIO pure (Configuration.validateInitializationParams (localInitializationParams options handlers (Text.pack (droidSessionWorkingDirectory options)) mcpOptions))
  let loadConfig = localRetainedLoadConfiguration options saved
      (loadParams, _) = Configuration.prepareLoadSessionParams (fromMaybe "" saved) mcpOptions (localAutoReject options handlers) loadConfig
  either throwIO pure (Configuration.validateLoadSessionParams loadParams)
  when (not (null (droidSessionHostedMcpServers options)) && locality /= LocalHost) (throwIO Hosted.HostedMcpRequiresLocalDaemon)
  Hosted.withMcpServerOptions (droidSessionHostedMcpServers options) mcpOptions $ \activeOptions ->
    acquire (\transport -> openLocalSession observability options handlers saved activeOptions transport action)

localInitializationParams :: DroidSessionOptions -> DroidHandlers -> Text -> McpSessionOptions -> Configuration.InitializeSessionParams
localInitializationParams options handlers directory mcp =
  let config = droidSessionConfiguration options
   in (Configuration.defaultInitializeSessionParams (fromMaybe "default" (droidSessionMachineId options)) directory)
        { Configuration.initializeModel = droidSessionModel options,
          Configuration.initializeSystemPrompt = droidSessionSystemPrompt options,
          Configuration.initializeMcpOptions = mcp,
          Configuration.initializeConfiguration =
            config
              { Configuration.configurationAutoRejectPermissions = Just (localAutoReject options handlers),
                Configuration.configurationOrigin = Just (fromMaybe OriginSDK (Configuration.configurationOrigin config)),
                Configuration.configurationTags = Just (Attribution.withSdkTag (fromMaybe [] (Configuration.configurationTags config)))
              }
        }

localAutoReject :: DroidSessionOptions -> DroidHandlers -> Bool
localAutoReject options handlers = fromMaybe (isNothing (onDroidPermission handlers)) (Configuration.configurationAutoRejectPermissions (droidSessionConfiguration options))

withLocalTransport :: Obs.DroidObservability -> DroidOptions -> (ObjectTransport -> IO a) -> IO a
withLocalTransport observability options action = do
  directory <- makeAbsolute (droidWorkingDirectory options)
  environment <- getEnvironment
  let sanitize = Map.delete "FACTORY_UPSTREAM_CLIENT_TYPE" . Map.delete "FACTORY_UPSTREAM_SDK"
      process = (Process.prepareDroidProcess (droidExecutable options) Process.StreamJsonRpc (droidLaunchOptions options) environment sanitize) {cwd = Just directory}
  Process.withObservedJsonLinesProcess observability (droidFrameLimitBytes options) 5000000 process (action . Transport.processTransport)

openLocalSession :: Obs.DroidObservability -> DroidSessionOptions -> DroidHandlers -> Maybe Text -> McpSessionOptions -> ObjectTransport -> (DroidSession -> IO a) -> IO a
openLocalSession observability options handlers saved mcpOptions rawTransport action = do
  let transport = if Obs.observabilityLogTransport observability then Transport.loggedObjectTransport (Obs.observabilityLogger observability) Transport.defaultTransportLogOptions rawTransport else rawTransport
  directory <- makeAbsolute (droidSessionWorkingDirectory options)
  let config = localRetainedLoadConfiguration options saved
  local <- LocalSessionState <$> newTVarIO config <*> newTVarIO mempty
  withObservedRpcChannel observability (transportSendObject transport) (transportReceiveObject transport) $ \channel ->
    withSessionConnection channel (localBackend local) (localAutoReject options handlers) mcpOptions $ \connection -> do
      let dispatcher = connectionDispatcher connection
      void (registerRpcHandler dispatcher "droid.request_permission" (permissionRpcHandler handlers))
      void (registerRpcHandler dispatcher "droid.ask_user" (questionRpcHandler handlers))
      void (installMcpObserver connection "droid.session_notification" Nothing handlers)
      forM_ (onDroidRequestSettled handlers) (onRpcRequestSettled channel)
      identifier <- case saved of
        Nothing -> sessionBoundary connection $ do
          let params = Configuration.initializationFields (localInitializationParams options handlers (Text.pack directory) mcpOptions)
          callSettings connection Nothing "droid.initialize_session" params
        Just identifier -> do
          loadSession connection identifier
          forM_ (droidSessionModel options) $ \model ->
            sessionBoundary connection $
              void (connectionRequest connection 30000000 (\rpc callOptions -> Client.updateSessionSettings rpc callOptions (emptySettingsUpdate {updateSettingsModel = Just model})))
          pure identifier
      session <- newSessionHandle identifier connection (droidSessionTimeoutMicros options)
      action session

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

listDroidMcpServers :: DroidSession -> IO ListMcpServersResult
listDroidMcpServers session = sessionRequest ReadOnlyRequest session (\channel options -> Client.listMcpServers channel options mempty)

listDroidMcpTools :: DroidSession -> IO ListMcpToolsResult
listDroidMcpTools session = sessionRequest ReadOnlyRequest session (\channel options -> Client.listMcpTools channel options mempty)

listDroidMcpRegistry :: DroidSession -> IO ListMcpRegistryResult
listDroidMcpRegistry session = sessionRequest ReadOnlyRequest session (\channel options -> Client.listMcpRegistry channel options mempty)

addDroidMcpServer :: DroidSession -> AddMcpServerParams -> IO SuccessResult
addDroidMcpServer session params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  sessionRequest MutatingRequest session (\channel options -> Client.addMcpServer channel options validated)

removeDroidMcpServer :: DroidSession -> Text -> IO SuccessResult
removeDroidMcpServer session name = sessionRequest MutatingRequest session (\channel options -> Client.removeMcpServer channel options (RemoveMcpServerParams name mempty))

toggleDroidMcpServer :: DroidSession -> Text -> Bool -> IO SuccessResult
toggleDroidMcpServer session name enabled = sessionRequest MutatingRequest session (\channel options -> Client.toggleMcpServer channel options (ToggleMcpServerParams name enabled mempty))

toggleDroidMcpTool :: DroidSession -> Text -> Text -> Bool -> IO SuccessResult
toggleDroidMcpTool session name tool enabled = sessionRequest MutatingRequest session (\channel options -> Client.toggleMcpTool channel options (ToggleMcpToolParams name tool enabled mempty))

-- | Five-minute RPC budget, not an OAuth-completion wait. Auth notifications
-- remain separate, and submit/cancel calls may run while this request awaits a
-- response. No browser, token exchange or automatic auth cancellation is owned.
authenticateDroidMcpServer :: DroidSession -> Text -> IO SuccessResult
authenticateDroidMcpServer session name = sessionRequestWithTimeout MutatingRequest 300000000 session (\channel options -> Client.authenticateMcpServer channel options (McpServerNameParams name mempty))

cancelDroidMcpAuth :: DroidSession -> Text -> IO SuccessResult
cancelDroidMcpAuth session name = sessionRequest MutatingRequest session (\channel options -> Client.cancelMcpAuth channel options (McpServerNameParams name mempty))

clearDroidMcpAuth :: DroidSession -> Text -> IO SuccessResult
clearDroidMcpAuth session name = sessionRequest MutatingRequest session (\channel options -> Client.clearMcpAuth channel options (McpServerNameParams name mempty))

submitDroidMcpAuthCode :: DroidSession -> SubmitMcpAuthCodeParams -> IO SuccessResult
submitDroidMcpAuthCode session params = sessionRequest MutatingRequest session (\channel options -> Client.submitMcpAuthCode channel options params)

submitDroidMcpAuthError :: DroidSession -> SubmitMcpAuthErrorParams -> IO SuccessResult
submitDroidMcpAuthError session params = sessionRequest MutatingRequest session (\channel options -> Client.submitMcpAuthError channel options params)

-- | Read last dispatcher-observed settings without sending or waiting for an
-- RPC. Inside a session observer this includes the current notification; queued
-- frames may remain. A malformed settings event makes this getter throw
-- DroidInvalidEvent until a valid full load establishes a new baseline.
-- Lifecycle and session-ID checks are atomic with the read.
getDroidSettings :: DroidSession -> IO SessionSettings
getDroidSettings session = atomically $ do
  ensureSession session
  (_, observed) <- readTVar (connectionSettings (sessionConnection session))
  case Map.lookup (droidSessionId session) observed of
    Just settings -> either throwSTM pure settings
    Nothing -> throwSTM DroidSessionUnusable

-- | Read the owned mission view without RPC or waiting. Nothing means that no
-- mission baseline or mutation has been observed. A malformed mission update
-- makes the view invalid until a full successful load/initialization replaces
-- it. Like the settings getter, this is safe inside ordinary event callbacks.
getDroidMissionSnapshot :: DroidSession -> IO (Maybe MissionSnapshot)
getDroidMissionSnapshot session = atomically $ do
  ensureSession session
  missionSnapshotAt (sessionConnection session) (droidSessionId session)

-- Cached, connection-scoped lookups never create stores or load sessions.
lookupMissionSnapshot :: SessionConnection -> Text -> IO (Maybe MissionSnapshot)
lookupMissionSnapshot connection identifier = atomically $ do
  readTVar (connectionOpen connection) >>= flip unless (throwSTM RpcChannelClosed)
  missionSnapshotAt connection identifier

lookupMissionId :: SessionConnection -> Text -> IO (Maybe Text)
lookupMissionId connection identifier = atomically $ do
  readTVar (connectionOpen connection) >>= flip unless (throwSTM RpcChannelClosed)
  Mission.resolveMissionId identifier <$> readTVar (connectionMission connection)

missionSnapshotAt :: SessionConnection -> Text -> STM (Maybe MissionSnapshot)
missionSnapshotAt connection identifier = do
  registry <- readTVar (connectionMission connection)
  either (const (throwSTM DroidInvalidEvent)) (pure . fmap Mission.missionSnapshot) (Mission.lookupMissionStore identifier registry)

-- | Apply a partial settings update and retain the peer's acknowledgement.
-- Accepted tool-policy overrides are retained in reply-intake order for later
-- loads. Omission preserves earlier intent; explicit empty lists are retained.
-- No optimistic settings cache is maintained. Unknown mutation outcomes invalidate
-- under the same lease rules as other session controls.
updateDroidSettings :: DroidSession -> UpdateSessionSettingsParams -> IO EmptyObject
updateDroidSettings session params = sessionRequest MutatingRequest session $ \channel options ->
  Client.callObserved (Proxy @(WithEnvelope (MethodRequest "droid.update_session_settings" UpdateSessionSettingsParams))) channel options params $ \_ ->
    forM_ (backendLocalState (connectionBackend (sessionConnection session))) $ \local ->
      modifyTVar' (localLoadConfiguration local) (`Configuration.mergeSessionLoadConfiguration` retained)
  where
    retained = Configuration.defaultSessionLoadConfiguration {Configuration.loadToolPolicy = updateSettingsToolPolicy params}

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
changeDroidWorkingDirectory session directory = sessionRequest MutatingRequest session $ \channel options ->
  Client.callObserved (Proxy @(WithEnvelope (MethodRequest "droid.change_working_directory" ChangeWorkingDirectoryParams))) channel options (ChangeWorkingDirectoryParams directory mempty) $ \result ->
    recordLocalDirectory (sessionConnection session) (droidSessionId session) (State.WorkingDirectoryReported (Just (changedResolvedPath result)))

-- | Last intake-observed directory, not the caller's process directory.
-- Receipt return alone does not synchronize later notifications.
getDroidWorkingDirectoryState :: DroidSession -> IO State.WorkingDirectoryState
getDroidWorkingDirectoryState session = atomically $ do
  ensureSession session
  local <- requireLocalState (sessionConnection session)
  Map.findWithDefault State.WorkingDirectoryUnknown (droidSessionId session) <$> readTVar (localWorkingDirectories local)

getDroidWorkingDirectory :: DroidSession -> IO (Maybe Text)
getDroidWorkingDirectory session =
  getDroidWorkingDirectoryState session >>= \case
    State.WorkingDirectoryUnknown -> pure Nothing
    State.WorkingDirectoryInherited path -> pure (Just path)
    State.WorkingDirectoryReported path -> pure path
    State.WorkingDirectoryInvalid _ -> throwIO DroidInvalidEvent

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
      userMessageSource = Just OriginSDK,
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
      options = (defaultDroidStreamOptions identifier turnIdentifier) {streamMode = AllEvents}
      admission = do
        closed <- readTVar (sessionClosed session)
        when closed (throwSTM DroidSessionUnusable)
      exchange = withDroidStreamChecked admission options $ \stream -> do
        let publish notification = void $ feedDroidDecoded stream $ do
              open <- readTVar (connectionOpen connection)
              pure $
                if not open
                  then Left (toException DroidSessionUnusable)
                  else either (const (Left (toException DroidInvalidEvent))) Right (backendDecode (connectionBackend connection) identifier (Just turnIdentifier) notification)
            disconnected _ = void (feedDroidError stream (toException DroidSessionUnusable))
        withinSessionLifetime session (synchronizeRpcEvents channel)
        bracket (onRpcNotification dispatcher publish) id $ \_ ->
          bracket (onRpcError dispatcher disconnected) id $ \_ -> do
            _ <- withinSessionLifetime session (connectionRequest connection 30000000 (\rpc callOptions -> backendSubmit (connectionBackend connection) dispatcher rpc callOptions identifier (input {userMessageId = Just turnIdentifier})))
            atomically (writeTVar (sessionSubmitted session) True)
            streamTurnResult <$> consumeDroidStream stream (\frame -> callback (streamFrameEvent frame) (streamFrameText frame))
      invalidate = invalidateSession session
      timed = maybe (Just <$> exchange) (`timeout` exchange) deadline
  (timed >>= maybe (throwIO DroidTurnTimedOut) pure) `onException` invalidate

-- | Request interruption of the current turn; idle sessions are a no-op. The
-- acknowledgement is not completion: await sendDroidTurn for the terminal result.
-- Pending interrupts fence the next prompt so a delayed request cannot cancel it.
interruptDroidSession :: DroidSession -> IO ()
interruptDroidSession session = bracket acquire release $ \active -> when active $ withinSessionLifetime session $ do
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
        connectionRequest (sessionConnection session) 30000000 (\channel options -> void (backendInterrupt (connectionBackend (sessionConnection session)) channel options (droidSessionId session)))
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
sessionRequest effect = sessionRequestWithTimeout effect 30000000

sessionRequestWithTimeout :: RequestEffect -> Int -> DroidSession -> (RpcChannel -> Client.CallOptions -> IO a) -> IO a
sessionRequestWithTimeout effect deadline session request =
  withSessionUse session $
    performSessionRequest effect session (connectionRequest (sessionConnection session) deadline request)

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
  identifier <- nextRequestId connection
  request (connectionChannel connection) (Client.CallOptions identifier (backendContext (connectionBackend connection)) (Just deadline))

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
withPrompt session action = withMVar (sessionTurnLock session) $ \() ->
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
  closed <- atomically $ do
    setStatus session SessionUnavailable
    readTVar (sessionClosed session)
  unless closed $ do
    let connection = sessionConnection session
    void (connectionRequest connection 1000000 (\channel options -> backendInterrupt (connectionBackend connection) channel options (droidSessionId session)))
      `catches` [Handler (\(_ :: RpcChannelError) -> pure ()), Handler (\(_ :: RpcResultError) -> pure ())]

loadSession :: SessionConnection -> Text -> IO ()
loadSession connection identifier = sessionBoundary connection $ do
  -- A callback-safe update can return before its ordered policy observation.
  -- Drain accepted replies before taking the replacement/rollback snapshot.
  synchronizeRpcEvents (connectionChannel connection)
  config <- atomically (requireLocalState connection >>= readTVar . localLoadConfiguration)
  let (params, patch) = Configuration.prepareLoadSessionParams identifier (connectionMcpOptions connection) (connectionAutoRejectPermissions connection) config
  either throwIO pure (Configuration.validateLoadSessionParams params)
  void (callSettings connection (Just identifier) "droid.load_session" (Configuration.loadSessionFields params))
  forM_ patch $ \update -> void (connectionRequest connection 30000000 (\channel options -> Client.updateSessionSettings channel options update))

localRetainedLoadConfiguration :: DroidSessionOptions -> Maybe Text -> Configuration.SessionLoadConfiguration
localRetainedLoadConfiguration options saved =
  let inherited = if isNothing saved then Configuration.loadConfigurationFromInitialization (droidSessionConfiguration options) else Configuration.defaultSessionLoadConfiguration
      retained = Configuration.mergeSessionLoadConfiguration inherited (droidSessionLoadConfiguration options)
   in retained {Configuration.loadOrigin = Just (fromMaybe OriginSDK (Configuration.loadOrigin retained))}

requireLocalState :: SessionConnection -> STM LocalSessionState
requireLocalState connection = maybe (throwSTM DroidSessionUnusable) pure (backendLocalState (connectionBackend connection))

recordLocalDirectory :: SessionConnection -> Text -> State.WorkingDirectoryState -> STM ()
recordLocalDirectory connection identifier directory = forM_ (backendLocalState (connectionBackend connection)) $ \local ->
  modifyTVar' (localWorkingDirectories local) $ case directory of
    State.WorkingDirectoryUnknown -> Map.insertWith (\_ old -> old) identifier directory
    _ -> Map.insert identifier directory

inheritLocalDirectory :: SessionConnection -> Text -> Text -> STM ()
inheritLocalDirectory connection parent identifier = forM_ (backendLocalState (connectionBackend connection)) $ \local ->
  modifyTVar' (localWorkingDirectories local) $ \directories ->
    let inherited = Map.findWithDefault State.WorkingDirectoryUnknown parent directories
        previous = Map.findWithDefault State.WorkingDirectoryUnknown identifier directories
     in Map.insert identifier (State.inheritWorkingDirectoryState inherited previous) directories

parseLocalDirectory :: SessionConnection -> Maybe Text -> Object -> Object -> Parser State.WorkingDirectoryState
parseLocalDirectory connection saved params fields = case backendLocalState (connectionBackend connection) of
  Nothing -> pure State.WorkingDirectoryUnknown
  Just _ | isNothing saved -> do
    requested <- params .: "cwd"
    worktree <- fields .:! "worktree"
    pure (State.WorkingDirectoryReported (Just (maybe requested initialWorktreePath worktree)))
  Just _ -> case KeyMap.lookup "cwd" fields of
    Nothing -> pure $ case KeyMap.lookup "worktree" fields of
      Just (Object legacy) -> case KeyMap.lookup "path" legacy of
        Just (String path) -> State.WorkingDirectoryReported (Just path)
        _ -> State.WorkingDirectoryUnknown
      _ -> State.WorkingDirectoryUnknown
    Just value -> State.WorkingDirectoryReported <$> parseJSON value

-- Full replies and notification updates write only on the same ordered intake.
-- Source lifecycle guards hide provisional successor settings until publication.
callSettings :: SessionConnection -> Maybe Text -> Text -> Object -> IO Text
callSettings connection saved method params = fst <$> callSettingsResult connection saved method params

callSettingsResult :: SessionConnection -> Maybe Text -> Text -> Object -> IO (Text, Object)
callSettingsResult connection saved method params = callSettingsResultObserved connection saved method params pure (\_ _ _ -> pure True)

-- Validate the consumer's complete receipt and gate publication at the same
-- ordered wire position as settings/mission updates, not after a later IO wait.
callSettingsResultObserved :: SessionConnection -> Maybe Text -> Text -> Object -> (Object -> Parser a) -> (UTCTime -> Text -> a -> STM Bool) -> IO (Text, a)
callSettingsResultObserved = callSettingsResultObservedWithin Nothing

-- A wire-only deadline lets a caller retry a specifically idempotent request
-- without replaying receipt restoration or later user work.
callSettingsResultObservedWithin :: Maybe Int -> SessionConnection -> Maybe Text -> Text -> Object -> (Object -> Parser a) -> (UTCTime -> Text -> a -> STM Bool) -> IO (Text, a)
callSettingsResultObservedWithin = callSettingsResultObservedWithAdmission (pure ())

callSettingsResultObservedWithAdmission :: STM () -> Maybe Int -> SessionConnection -> Maybe Text -> Text -> Object -> (Object -> Parser a) -> (UTCTime -> Text -> a -> STM Bool) -> IO (Text, a)
callSettingsResultObservedWithAdmission admit deadline connection saved method params decode observeReceipt = do
  identifier <- nextRequestId connection
  let expected = saved <|> case KeyMap.lookup "sessionId" params of Just (String value) -> Just value; _ -> Nothing
  atomically (admit >> writeTVar (connectionMissionTarget connection) (Just (identifier, expected)))
  let parse fields = do
        sessionId <- maybe (fields .: "sessionId") pure saved
        forM_ expected $ \wanted -> unless (sessionId == wanted) (fail "Unexpected attached session identifier")
        forM_ saved $ \_ -> do
          snapshot <- fields .: "session"
          void (snapshot .: "messages" :: Parser [Value])
        settings <- fields .: "settings"
        when (isNothing saved && KeyMap.member "systemPrompt" params && isNothing (settingsSystemPrompt settings)) (fail "Requested system prompt was not acknowledged")
        mission <- fields .:! "mission"
        parent <- fields .:! "callingSessionId"
        receipt <- decode fields
        directory <- parseLocalDirectory connection saved params fields
        pure (sessionId, settings, mission, parent, receipt, directory)
      observe receivedAt response = forM_ (decodeRpcResult response) $ \fields ->
        forM_ (parseEither parse fields) $ \(sessionId, settings, mission, parent, receipt, directory) -> do
          accepted <- observeReceipt receivedAt sessionId receipt
          when accepted $ do
            modifyTVar' (connectionSettings connection) (\(_, known) -> (Just sessionId, Map.insert sessionId (Right settings) known))
            recordLocalDirectory connection sessionId directory
            pending <- readTVar (connectionUnscopedMission connection)
            registry <- readTVar (connectionMission connection)
            let provisioned = case Map.lookup identifier pending of
                  Just (Left _) -> Mission.invalidateMissionStore sessionId registry
                  Just (Right (Just store)) -> Mission.modifyMissionStore sessionId (Mission.mergeFrom (scopePendingUsage sessionId store)) registry
                  _ -> registry
                restored = case mission of
                  Just snapshot -> Mission.restoreRegistrySnapshotAt (observationTimestamp receivedAt) sessionId snapshot provisioned
                  Nothing -> case parent of
                    Just parentId | not (Text.null parentId) -> Mission.associateWorkerWithParentMission parentId sessionId provisioned
                    _ -> provisioned
                associated = case Mission.lookupMissionStore sessionId restored of
                  Right (Just store) -> foldl' (flip (Mission.associateWorkerWithParentMission sessionId)) restored (missionSnapshotWorkers (Mission.missionSnapshot store))
                  _ -> restored
            writeTVar (connectionMission connection) associated
          modifyTVar' (connectionUnscopedMission connection) (Map.delete identifier)
          current <- readTVar (connectionMissionTarget connection)
          when (fmap fst current == Just identifier) (writeTVar (connectionMissionTarget connection) (if accepted then Just (identifier, Just sessionId) else Nothing))
  let envelope = backendContext (connectionBackend connection)
  response <- requestReplyObservedAtWithAdmission (connectionChannel connection) RunBeforeRequest admit deadline (envelope {envelopeBody = BaseRequest identifier method (Just (Object params)) mempty}) observe
  fields <- either throwIO pure (decodeRpcResult response)
  (sessionId, _, _, _, receipt, _) <- parseSessionResult parse fields
  pure (sessionId, receipt)

observationTimestamp :: UTCTime -> Text
observationTimestamp = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"

observeMissionNotification :: SessionConnection -> JsonRpcBaseNotification -> IO ()
observeMissionNotification connection notification = do
  observedAt <- observationTimestamp <$> getCurrentTime
  atomically $ do
    notice <- missionNotificationContext connection notification
    forM_ notice $ \(origin, decoded) -> case origin of
      Just identifier -> modifyTVar' (connectionMission connection) $ \registry -> case decoded of
        Left _ -> Mission.invalidateMissionStore identifier registry
        Right events -> foldl' (flip (Mission.applyRegistryEventAt observedAt identifier)) registry events
      Nothing -> do
        target <- readTVar (connectionMissionTarget connection)
        forM_ target $ \(requestId, _) -> modifyTVar' (connectionUnscopedMission connection) $ \pending ->
          let previous = Map.findWithDefault (Right Nothing) requestId pending
              next = case decoded of
                Left _ -> Left DroidInvalidEvent
                Right events -> foldl' (\current event -> fmap (applyUnscopedMissionEvent observedAt event) current) previous events
           in Map.insert requestId next pending

missionNotificationContext :: SessionConnection -> JsonRpcBaseNotification -> STM (Maybe (Maybe Text, Either String [DroidEvent]))
missionNotificationContext connection notification = do
  target <- readTVar (connectionMissionTarget connection)
  pure $ case baseNotificationParams (envelopeBody notification) of
    Just (Object params)
      | Just (Object payload) <- KeyMap.lookup "notification" params,
        Just (String kind) <- KeyMap.lookup "type" payload,
        kind `elem` ["mission_state_changed", "mission_features_changed", "mission_progress_entry", "mission_worker_started", "mission_worker_completed", "session_token_usage_changed"] ->
          let reported = case KeyMap.lookup "sessionId" params of Just (String value) -> Just value; _ -> Nothing
              origin = reported <|> (snd =<< target)
           in if isJust origin || baseNotificationMethod (envelopeBody notification) == "droid.session_notification"
                then Just (origin, backendDecode (connectionBackend connection) (fromMaybe "" origin) Nothing notification)
                else Nothing
    _ -> Nothing

applyUnscopedMissionEvent :: Text -> DroidEvent -> Maybe Mission.MissionStore -> Maybe Mission.MissionStore
applyUnscopedMissionEvent observedAt event current = case event of
  UsageEvent _ -> Mission.applyEventAt observedAt event <$> current
  MissionStateEvent _ -> update
  MissionFeaturesEvent _ -> update
  MissionProgressEvent _ -> update
  MissionWorkerStartedEvent _ -> update
  MissionWorkerCompletedEvent _ -> update
  _ -> current
  where
    update = Just (Mission.applyEventAt observedAt event (fromMaybe Mission.emptyMissionStore current))

scopePendingUsage :: Text -> Mission.MissionStore -> Mission.MissionStore
scopePendingUsage identifier store = Mission.setTokenUsageBySessionId kept store
  where
    usage = fromMaybe mempty (missionSnapshotSessionUsage (Mission.missionSnapshot store))
    kept = Map.filterWithKey (\sessionId _ -> sessionId == identifier || Mission.hasWorkerSession sessionId store) usage

missionEventAdmitted :: Text -> Mission.MissionRegistry -> DroidEvent -> Bool
missionEventAdmitted origin registry event = case event of
  UsageEvent usage -> case Mission.lookupMissionStore origin registry of
    Left _ -> True
    Right (Just store) -> sessionUsageId usage == origin || Mission.hasWorkerSession (sessionUsageId usage) store
    Right Nothing -> False
  MissionStateEvent _ -> True
  MissionFeaturesEvent _ -> True
  MissionProgressEvent _ -> True
  MissionWorkerStartedEvent _ -> True
  MissionWorkerCompletedEvent _ -> True
  _ -> False

observeSettingsNotification :: SessionConnection -> JsonRpcBaseNotification -> STM ()
observeSettingsNotification connection notification =
  case baseNotificationParams (envelopeBody notification) of
    Just (Object params)
      | Just (Object payload) <- KeyMap.lookup "notification" params,
        let kind = KeyMap.lookup "type" payload,
        kind == Just (String "settings_updated") || (isJust (backendLocalState (connectionBackend connection)) && kind == Just (String "session_working_directory_changed")) -> do
          (owner, observed) <- readTVar (connectionSettings connection)
          let origin = case KeyMap.lookup "sessionId" params of
                Just (String identifier) -> Just identifier
                _ | baseNotificationMethod (envelopeBody notification) == "droid.session_notification" -> owner
                _ -> Nothing
          forM_ origin $ \identifier -> forM_ (Map.lookup identifier observed) $ \current ->
            case backendDecode (connectionBackend connection) identifier Nothing notification of
              Left _ | kind == Just (String "settings_updated") -> writeTVar (connectionSettings connection) (owner, Map.insert identifier (Left DroidInvalidEvent) observed)
              Left _ -> forM_ (backendLocalState (connectionBackend connection)) $ \local ->
                modifyTVar' (localWorkingDirectories local) (Map.alter (Just . State.invalidateWorkingDirectoryState . fromMaybe State.WorkingDirectoryUnknown) identifier)
              Right events -> forM_ events $ \case
                SettingsUpdatedEvent update ->
                  let !next = current >>= (`mergeObservedSettings` settingsUpdateValues update)
                   in writeTVar (connectionSettings connection) (owner, Map.insert identifier next observed)
                WorkingDirectoryEvent change -> recordLocalDirectory connection identifier (State.WorkingDirectoryReported (Just (updatedWorkingDirectory change)))
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
sessionBoundary connection = connectionBoundary connection 60000000

-- The exchange and its ordered publication share one deadline.
connectionBoundary :: SessionConnection -> Int -> IO a -> IO a
connectionBoundary connection deadline action = do
  result <- timeout deadline (action <* synchronizeRpcEvents (connectionChannel connection))
  maybe (throwIO RpcRequestTimedOut) pure result

replaceSession :: DroidSession -> Int -> (RpcChannel -> Client.CallOptions -> IO a) -> (a -> Text) -> IO (DroidSession, a)
replaceSession session deadline operation successorId = mask $ \restore -> do
  atomically (ensureSession session)
  let connection = sessionConnection session
      lock = sessionTurnLock session
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
            atomically (inheritLocalDirectory connection (droidSessionId session) target)
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

nextRequestId :: SessionConnection -> IO Text
nextRequestId connection = do
  index <- atomicModifyIORef' (connectionCounter connection) (\n -> (n + 1, n))
  pure (connectionRequestNamespace connection <> ":" <> Text.pack (show index))

newSessionHandle :: Text -> SessionConnection -> Maybe Int -> IO DroidSession
newSessionHandle identifier connection deadline =
  DroidSession identifier connection <$> newTVarIO (SessionReady, 0) <*> pure deadline <*> newTVarIO 0 <*> newTVarIO False <*> newMVar () <*> newTVarIO False <*> newTVarIO mempty

-- Close only this handle's admission and subscriptions. Waiting is separate
-- so the dispatcher can report closure without waiting on its own intake.
closeDroidSession :: DroidSession -> IO ()
closeDroidSession session = mask_ $ do
  cleanups <- atomically $ do
    writeTVar (sessionClosed session) True
    (status, active) <- readTVar (sessionLifecycle session)
    case status of
      SessionReplaced _ -> pure ()
      _ -> writeTVar (sessionLifecycle session) (SessionUnavailable, active)
    owned <- readTVar (sessionCleanups session)
    writeTVar (sessionCleanups session) mempty
    pure (Map.elems owned)
  sequence_ cleanups

waitDroidSessionIdle :: DroidSession -> IO ()
waitDroidSessionIdle session = atomically $ do
  (_, active) <- readTVar (sessionLifecycle session)
  check (active == 0)

-- | Nonblocking lease observation for the owner retiring a closed attachment.
isDroidSessionIdle :: DroidSession -> STM Bool
isDroidSessionIdle session = (== 0) . snd <$> readTVar (sessionLifecycle session)

ownSessionCleanup :: DroidSession -> IO () -> IO (IO ())
ownSessionCleanup session cleanup = mask_ $ do
  token <- newUnique
  registered <- atomically $ do
    closed <- readTVar (sessionClosed session)
    unless closed (modifyTVar' (sessionCleanups session) (Map.insert token cleanup))
    pure (not closed)
  unless registered cleanup
  pure (mask_ (cleanup `finally` atomically (modifyTVar' (sessionCleanups session) (Map.delete token))))

-- Closing requests retain admission until their own reply settles, even when
-- a lifecycle notification has already ended ordinary session operations.
withSessionLease :: DroidSession -> IO a -> IO a
withSessionLease session = bracket_ (atomically (beginUse session)) (atomically (endUse session))

withSessionUse :: DroidSession -> IO a -> IO a
withSessionUse session action = withSessionLease session (withinSessionLifetime session action)

withinSessionLifetime :: DroidSession -> IO a -> IO a
withinSessionLifetime session action =
  either throwIO pure =<< race (atomically (readTVar (sessionClosed session) >>= check) >> pure DroidSessionUnusable) action

parseSessionResult :: (Object -> Parser a) -> Object -> IO a
parseSessionResult parser fields = either (const (throwIO DroidInvalidEvent)) pure (parseEither parser fields)
