{-# LANGUAGE OverloadedStrings #-}

-- | Scoped access to an existing daemon. Connections can query saved resources
-- without loading a session. The SDK owns connections, not the daemon or saved
-- sessions; ordinary scope exit only disconnects.
module Factory.Droid.Daemon
  ( DaemonCredential (..),
    DaemonOptions (..),
    defaultDaemonOptions,
    DaemonAuthentication (..),
    DaemonClientOptions (..),
    defaultDaemonClientOptions,
    daemonClientOptions,
    DaemonSessionOptions (..),
    defaultDaemonSessionOptions,
    withSessionOn,
    withSessionOnHandlers,
    withConnectionOn,
    withSessionUsing,
    withSessionUsingHandlers,
    withResumedSessionUsing,
    withResumedSessionUsingHandlers,
    DaemonError (..),
    DaemonConnection,
    DaemonState,
    DaemonStateSnapshot (..),
    withDaemonState,
    connectionState,
    readDaemonStateSnapshot,
    getDaemonStateSnapshot,
    daemonStatePendingInteractions,
    withConnectionState,
    withConnectionStateObserved,
    withConnectionStateOn,
    withConnection,
    withConnectionObserved,
    connectionUser,
    isAuthenticated,
    ConnectionHealth (..),
    getConnectionHealth,
    readConnectionHealth,
    readConnectionFailureCause,
    connectionUsable,
    ensureConnectionAuthenticated,
    getConnectionId,
    getTransportKind,
    getPendingCount,
    getSessionCacheCapacity,
    setSessionCacheCapacity,
    getCachedSessionIds,
    touchSession,
    getActiveSessionId,
    setActiveSessionId,
    removeCachedSession,
    pruneSessionCache,
    SessionDirectoryEntry (..),
    registerSessionState,
    getSessionDirectory,
    readSessionDirectory,
    getSessionMachineId,
    setSessionMachineId,
    hasActiveSessionsForMachine,
    countActiveSessionsForCwd,
    markSessionsNotLoadedForMachine,
    setBeforeRequest,
    onRequestSettled,
    onConnectionError,
    onConnectionClose,
    pendingInteractions,
    withPendingInteractions,
    setSessionHandlers,
    deferPermissionResponse,
    deferQuestionResponse,
    storeDeferredPermission,
    storeDeferredQuestion,
    takeDeferredPermission,
    takeDeferredQuestion,
    clearDeferredUserActions,
    getMcpConfig,
    updateMcpConfig,
    getDefaultSettings,
    updateSessionDefaults,
    listCustomModels,
    upsertCustomModel,
    deleteCustomModel,
    logout,
    listCrons,
    createCron,
    updateCron,
    deleteCron,
    holdSessionCrons,
    resumeSessionCrons,
    onCronStateChanged,
    sfListWorkstreams,
    sfGetWorkstream,
    sfCreateWorkstream,
    sfUpdateWorkstream,
    sfDeleteWorkstream,
    sfPublishWorkstreamContent,
    sfHydrateWorkstreamContent,
    sfListSignals,
    sfListChanges,
    sfListActivities,
    sfResolveActivityReview,
    sfListEvents,
    sfMarkEventsRead,
    sfMarkEventsUnread,
    triggerUpdate,
    installSshKey,
    getProxyToken,
    startRelay,
    stopRelay,
    getRelayStatus,
    onRelayStatusChanged,
    listGitBranches,
    checkoutGitBranch,
    listWorktreeSetupProfiles,
    saveWorktreeSetupProfile,
    deleteWorktreeSetupProfile,
    listManagedWorktrees,
    cleanupWorktree,
    inspectWorktreeDeletion,
    getGitBranchDivergence,
    getGitDiff,
    resolvePullRequestStatuses,
    inspectMissionReadiness,
    acknowledgeMissionReadinessWarning,
    gitPush,
    gitCommit,
    createPullRequest,
    getSemanticDiffCache,
    saveSemanticDiffCache,
    generateSemanticDiff,
    getRewindInfo,
    executeRewind,
    compactSession,
    forkSession,
    killWorkerSession,
    closeSession,
    submitBugReport,
    listAutomations,
    runAutomation,
    pauseAutomation,
    resumeAutomation,
    getAutomationHistory,
    getAutomationVisual,
    renameAutomation,
    deleteAutomation,
    createAutomation,
    forkAutomation,
    updateAutomationModel,
    updateAutomationPrivacy,
    updateAutomationPrompt,
    updateAutomationSchedule,
    applyAutomationConfig,
    updateAutomation,
    resolveQueuedUserMessage,
    queueUserMessage,
    queueUserMessages,
    getQueuedMessages,
    replaceDaemonQueuedMessages,
    dequeueQueuedMessage,
    dequeueQueuedMessages,
    restoreQueuedMessages,
    clearQueuedMessages,
    markQueuedMessageProcessed,
    pauseQueuedMessages,
    sendQueuedUserMessage,
    listOpenedSessions,
    listAvailableSessions,
    listModels,
    getSessionMessages,
    searchSessions,
    archiveSession,
    unarchiveSession,
    DaemonEventError (..),
    onArchiveStateChanged,
    checkFolderTrust,
    trustFolder,
    validateWorkingDirectory,
    changeWorkingDirectory,
    getWorkingDirectory,
    listFiles,
    searchFiles,
    getWorkspaceFileContent,
    writeWorkspaceFileContent,
    pushCwdFileToUrl,
    pullUrlToCwdFile,
    onSetupStepProgress,
    createTerminal,
    writeTerminalData,
    resizeTerminal,
    closeTerminal,
    listTerminals,
    loadTerminals,
    addTerminal,
    getTerminals,
    removeTerminalFromStore,
    updateTerminalStatus,
    getActiveTerminalId,
    setActiveTerminalId,
    storeTerminalState,
    getTerminalSerializedState,
    getTerminalBufferedData,
    clearTerminalBufferedData,
    clearTerminalRestorationState,
    registerTerminalWriteHandler,
    onTerminalEvent,
    DaemonSession,
    DaemonSessionInfo (..),
    sessionInfo,
    sessionConnection,
    sessionId,
    sessionStatus,
    authenticatedUser,
    getSettings,
    getMissionSnapshot,
    getMissionSnapshotForSession,
    getMissionIdForSession,
    onMissionSnapshot,
    updateSettings,
    renameSession,
    listSkills,
    listCommands,
    getContextBreakdown,
    setSkillDisabled,
    listAvailablePlugins,
    listInstalledPlugins,
    installPlugin,
    uninstallPlugin,
    setPluginEnabled,
    updatePlugin,
    listMarketplaces,
    addMarketplace,
    removeMarketplace,
    updateMarketplace,
    withSession,
    withSessionHandlers,
    withResumedSession,
    withResumedSessionHandlers,
    withResumedSessionOn,
    withResumedSessionOnHandlers,
    detachSession,
    closeAttachedSession,
    SessionLoadPhase (..),
    SessionReadiness (..),
    getSessionReadiness,
    waitSessionReadinessChange,
    sessionReadinessBusy,
    loadSessionInfo,
    loadSessionInfoWithConfiguration,
    withResumedSessionOnConfigured,
    ensureSessionLoaded,
    registerChildSession,
    ensureChildSessionAttached,
    findSubagentSessionId,
    getSubagentSessionIdsForParent,
    getSubagentSessionIdsByParent,
    getSubagentInvocationSummary,
    setSubagentInvocationSummary,
    hydrateSubagentInvocationSummaries,
    markSessionNotLoaded,
    clearSessionNotFound,
    setSessionPreInit,
    getSessionState,
    waitSessionStateChange,
    setSessionProgressiveDisplay,
    expandSessionDisplay,
    setSessionDisplayCutoff,
    registerOptimisticSubmission,
    confirmOptimisticSubmission,
    cancelOptimisticSubmission,
    cancelSessionOptimisticSubmissions,
    submitUserMessage,
    sendPrompt,
    sendTurn,
    sendEvents,
    sendInput,
    sendInputEvents,
    sendOutput,
    sendOutputEvents,
    sendInputOutput,
    sendInputOutputEvents,
    interruptSession,
    onSessionEvent,
    listMcpServers,
    listMcpTools,
    listMcpRegistry,
    addMcpServer,
    removeMcpServer,
    toggleMcpServer,
    toggleMcpTool,
    authenticateMcpServer,
    cancelMcpAuth,
    clearMcpAuth,
    submitMcpAuthCode,
    submitMcpAuthError,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, mapConcurrently_, race)
import Control.Concurrent.STM (STM, TMVar, TVar, atomically, check, modifyTVar', newEmptyTMVar, newEmptyTMVarIO, newTVarIO, orElse, putTMVar, readTMVar, readTVar, readTVarIO, throwSTM, tryPutTMVar, writeTVar)
import Control.DeepSeq (force)
import Control.Exception (Exception, SomeAsyncException, SomeException, bracket, catch, evaluate, finally, fromException, mask, mask_, onException, throwIO, toException, try, uninterruptibleMask_)
import Control.Monad (filterM, forM_, join, unless, void, when, (>=>))
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), withObject, (.:), (.:!), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.UUID.Types qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Unique (Unique, newUnique)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Input (DroidInput)
import Factory.Droid.Interaction (DroidHandlers (..), defaultDroidHandlers, permissionRpcHandler, questionRpcHandler)
import Factory.Droid.Interaction qualified as Interaction
import Factory.Droid.Internal.Exception (finallyPreserving)
import Factory.Droid.Internal.JSON (isEcmaWhitespace)
import Factory.Droid.Internal.Output (DroidOutput, DroidOutputResult)
import Factory.Droid.Internal.Session qualified as Core
import Factory.Droid.Internal.Stream (DroidEvent (..), DroidResult, DroidStreamMode, decodeDaemonNotification)
import Factory.Droid.MCP.Server qualified as Hosted
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Observability qualified as Obs
import Factory.Droid.Protocol
import Factory.Droid.Protocol.Dispatch
import Factory.Droid.Retry qualified as Retry
import Factory.Droid.Schema.Configuration qualified as Configuration
import Factory.Droid.Schema.Content (toolUseId)
import Factory.Droid.Schema.Context (GetContextBreakdownResult)
import Factory.Droid.Schema.Control (AddUserMessageParams (..), ChangeWorkingDirectoryParams (..), ChangeWorkingDirectoryResult (..), CompactSessionParams, CompactSessionResult, ExecuteRewindParams, ExecuteRewindResult, ForkSessionParams, ForkSessionResult, GetRewindInfoParams, GetRewindInfoResult, KillWorkerSessionParams, QueuePlacement (..), QueuedUserMessage (..), RenameSessionParams (..), ResolveQueuedMessageParams, SubmitBugReportParams, SubmitBugReportResult, ValidateWorkingDirectoryResult)
import Factory.Droid.Schema.Daemon.Automation (ApplyAutomationConfigParams, ApplyAutomationConfigResult, AutomationAddress, AutomationCreationResult, AutomationHistoryParams, AutomationHistoryResult, AutomationListParams (..), AutomationStatusResult, AutomationVisualParams, AutomationVisualResult, CreateAutomationParams, ForkAutomationParams, ListAutomationsResult, RenameAutomationParams, RunAutomationParams, RunAutomationResult, UpdateAutomationModelParams, UpdateAutomationParams, UpdateAutomationPrivacyParams, UpdateAutomationPromptParams, UpdateAutomationScheduleParams)
import Factory.Droid.Schema.Daemon.Cron (CreateCronParams, CreateCronResult, CronStateChanged, DeleteCronParams, DeleteCronResult, HoldSessionCronsParams, HoldSessionCronsResult, ListCronsParams, ListCronsResult, ResumeSessionCronsResult, UpdateCronParams, UpdateCronResult)
import Factory.Droid.Schema.Daemon.Git (CheckoutBranchParams, CheckoutBranchResult, CreatePullRequestParams, CreatePullRequestResult, GenerateSemanticDiffParams, GenerateSemanticDiffResult, GitBranchDivergence, GitBranchParams, GitCommitParams, GitDiffParams, GitDiffResult, GitDirectoryParams (..), ListGitBranchesResult, MissionReadinessResult, ResolvePullRequestStatusesParams, ResolvePullRequestStatusesResult, SaveSemanticDiffParams, SemanticDiffCacheResult, SemanticDiffTarget)
import Factory.Droid.Schema.Daemon.Management (InstallSshKeyParams (..), InstallSshKeyResult, ProxyTokenResult, RelayStartResult, RelayStatus, RelayStopResult, TriggerUpdateResult)
import Factory.Droid.Schema.Daemon.Plugin (AddMarketplaceParams (..), AddMarketplaceResult, InstallPluginParams, InstallPluginResult, ListAvailablePluginsResult, ListInstalledPluginsResult, ListMarketplacesResult, MarketplaceNameParams (..), MarketplaceSource, PluginScopeParams (..), PluginTargetParams, SetPluginEnabledParams, UpdateMarketplaceParams (..), UpdateMarketplaceResult, UpdatePluginParams, UpdatePluginResult)
import Factory.Droid.Schema.Daemon.Session (ArchiveSessionParams, ArchiveSessionResult, DaemonCloseSessionParams, GetSessionMessagesParams, GetSessionMessagesResult, ListAvailableSessionsParams, ListAvailableSessionsResult, ListOpenedSessionsParams, ListOpenedSessionsResult, LoadedSessionState (..), SearchSessionsParams, SearchSessionsResult, SessionArchiveStateChanged, defaultDaemonCloseSessionParams)
import Factory.Droid.Schema.Daemon.Settings (DefaultSettings, DeleteCustomModelParams, ListCustomModelsResult, UpdateCustomModelsResult, UpdateSessionDefaultsParams, UpdateSessionDefaultsResult, UpsertCustomModelParams)
import Factory.Droid.Schema.Daemon.SoftwareFactory (SfCreateWorkstreamParams, SfDeleteWorkstreamResult, SfGetWorkstreamResult, SfHydrateWorkstreamContentParams, SfListActivitiesParams, SfListActivitiesResult, SfListChangesParams, SfListChangesResult, SfListEventsParams, SfListEventsResult, SfListSignalsParams, SfListSignalsResult, SfListWorkstreamsParams, SfListWorkstreamsResult, SfMarkEventsReadParams, SfMarkEventsUnreadParams, SfMarkedEventsResult, SfPublishWorkstreamContentParams, SfResolveActivityReviewParams, SfResolveActivityReviewResult, SfUpdateWorkstreamParams, SfWorkstreamContentResult, SfWorkstreamResult, SfWorkstreamTarget)
import Factory.Droid.Schema.Daemon.Terminal (CloseTerminalParams, CreateTerminalParams, CreateTerminalResult, DaemonCloseTerminalParams (..), DaemonCreateTerminalParams (..), DaemonResizeTerminalParams (..), DaemonWriteTerminalDataParams (..), ListTerminalsResult, ResizeTerminalParams, TerminalNotification, WriteTerminalDataParams)
import Factory.Droid.Schema.Daemon.Terminal qualified as Terminal
import Factory.Droid.Schema.Daemon.Workspace (ChangeSessionWorkingDirectoryParams (..), CheckFolderTrustResult, FolderPathParams (..), GetWorkspaceFileContentParams, GetWorkspaceFileContentResult, ListFilesParams, ListFilesResult, PullUrlToCwdFileParams, PullUrlToCwdFileResult, PushCwdFileToUrlParams, PushCwdFileToUrlResult, SearchFilesParams (..), SearchFilesResult, SetupStepProgress, TrustFolderResult, WriteWorkspaceFileContentParams, WriteWorkspaceFileContentResult)
import Factory.Droid.Schema.Daemon.Worktree (CleanupWorktreeParams, CleanupWorktreeResult, DeleteWorktreeProfileParams, InspectWorktreeDeletionParams, InspectWorktreeDeletionResult, ListManagedWorktreesParams, ListManagedWorktreesResult, ListWorktreeProfilesParams, ListWorktreeProfilesResult, SaveWorktreeProfileParams, SaveWorktreeProfileResult)
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..), ListCommandsResult, ListSkillsResult, SetSkillDisabledParams)
import Factory.Droid.Schema.Enums qualified as Enums
import Factory.Droid.Schema.Interaction (AskUserResult, RequestPermissionResult, askUserToolCallId, confirmationInfoToolUse, permissionToolUses)
import Factory.Droid.Schema.MCP (ListMcpRegistryResult, ListMcpServersResult, ListMcpToolsResult, McpServerNameParams (..), RemoveMcpServerParams (..), SubmitMcpAuthCodeParams, SubmitMcpAuthErrorParams, ToggleMcpServerParams (..), ToggleMcpToolParams (..))
import Factory.Droid.Schema.MCP.Config (AddMcpServerParams, GetMcpConfigResult, McpConfigurationError (..), McpSessionOptions (..), UpdateMcpConfigParams, UpdateMcpConfigResult, defaultMcpSessionOptions, validateMcpConfiguration)
import Factory.Droid.Schema.Mission (MissionSnapshot, SubagentInvocationSummary (..))
import Factory.Droid.Schema.Models (ListModelsOptions, ListModelsResult)
import Factory.Droid.Schema.Notifications (ChildSessionAvailable (..), CreateMessage (..), DroidWorkingState (..), DroidWorkingStateChanged (..), PermissionResolved (..), SessionTitleUpdated, SessionWorkingDirectoryChanged (..))
import Factory.Droid.Schema.Primitives (NonEmptyText)
import Factory.Droid.Schema.RPC
import Factory.Droid.Schema.Session (SessionIdParams (..), SessionSnapshot (sessionMessages), SessionWorktreeInfo (..), findSubagentSessionTag)
import Factory.Droid.Schema.Settings (SessionSettings, SettingsUpdated, UpdateSessionSettingsParams, settingsTags)
import Factory.Droid.Schema.Sources (SessionSource (..), SessionSourceDetails (SourceApi))
import Factory.Droid.Schema.SystemPrompt (SystemPromptConfig)
import Factory.Droid.SessionState qualified as SessionState
import Factory.Droid.Transport (ObjectTransport (..), TransportLocality (..))
import Factory.Droid.Transport qualified as Transport
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import GHC.Clock (getMonotonicTimeNSec)
import GHC.TypeLits (KnownSymbol)
import Numeric.Natural (Natural)
import System.Timeout (timeout)

-- | Explicit credentials; no login files or environment variables are read.
-- A delegation grant is valid only alongside a token, not an API key.
data DaemonCredential = DaemonApiKey !Text | DaemonToken !Text !(Maybe Text)

instance Show DaemonCredential where
  show _ = "DaemonCredential <redacted>"

-- | Cwd, machine, model and worktree options apply to new sessions on the daemon
-- host, not this process. Supplied model/worktree options on resume are rejected.
-- Protocol version is explicit; the default follows CLI 0.212.1, not a negotiated
-- compatibility guarantee. TLS is enabled by the default transport options.
data DaemonOptions = DaemonOptions
  { daemonTarget :: !WebSocket.WebSocketTarget,
    daemonTransport :: !WebSocket.WebSocketOptions,
    daemonCredential :: !DaemonCredential,
    daemonWorkingDirectory :: !Text,
    daemonWorktree :: !(Maybe Bool),
    daemonWorktreeDirectory :: !(Maybe Text),
    daemonMachineId :: !Text,
    daemonModel :: !(Maybe Text),
    daemonTurnTimeoutMicros :: !(Maybe Int),
    daemonProtocolVersion :: !Text,
    daemonMcpOptions :: !McpSessionOptions,
    daemonHostedMcpServers :: ![Hosted.McpServer],
    daemonHydrateChildSessions :: !Bool,
    daemonRestoreTerminalsOnLoad :: !Bool,
    daemonConfiguration :: !Configuration.SessionConfiguration,
    daemonSystemPrompt :: !(Maybe SystemPromptConfig),
    daemonSpawnOptions :: !Configuration.DaemonSpawnOptions,
    daemonInitializationTimeoutMicros :: !(Maybe Int),
    daemonLoadConfiguration :: !Configuration.SessionLoadConfiguration,
    daemonLoadSpawnConfiguration :: !Configuration.DaemonLoadConfiguration,
    daemonObservability :: !Obs.DroidObservability
  }

instance Show DaemonOptions where
  show _ = "DaemonOptions <redacted>"

defaultDaemonOptions :: WebSocket.WebSocketTarget -> DaemonCredential -> Text -> DaemonOptions
defaultDaemonOptions target credential cwd = DaemonOptions target WebSocket.defaultWebSocketOptions credential cwd Nothing Nothing "local" Nothing Nothing "1.201.1" defaultMcpSessionOptions [] True True Configuration.defaultSessionConfiguration Nothing Configuration.defaultDaemonSpawnOptions Nothing Configuration.defaultSessionLoadConfiguration Configuration.defaultDaemonLoadConfiguration Obs.defaultDroidObservability

-- | Inherited authentication is an explicit caller attestation, never inferred
-- from availability/locality. Providers may run concurrently; refresh the same
-- principal, retaining the grant/identity for this scope. Initial bearer auth
-- requires a nonempty token. Init/load reject Nothing without cached fallback
-- but preserve an explicit empty string. Provider I/O precedes raw RPC budgets
-- and readiness gates; inherited providers are not called at connection setup.
data DaemonAuthentication
  = DaemonAuthenticate !DaemonCredential
  | DaemonTokenProvider !(IO (Maybe Text)) !(Maybe Text)
  | DaemonInheritAuthentication !GetUserInfoResult !Text
  | DaemonInheritAuthenticationProvider !GetUserInfoResult !(IO (Maybe Text))

instance Show DaemonAuthentication where show _ = "DaemonAuthentication <redacted>"

-- | Protocol/session configuration without a fabricated network endpoint.
data DaemonClientOptions = DaemonClientOptions
  { daemonClientAuthentication :: !DaemonAuthentication,
    daemonClientWorkingDirectory :: !Text,
    daemonClientWorktree :: !(Maybe Bool),
    daemonClientWorktreeDirectory :: !(Maybe Text),
    daemonClientMachineId :: !Text,
    daemonClientModel :: !(Maybe Text),
    daemonClientTurnTimeoutMicros :: !(Maybe Int),
    daemonClientProtocolVersion :: !Text,
    daemonClientMcpOptions :: !McpSessionOptions,
    daemonClientHostedMcpServers :: ![Hosted.McpServer],
    daemonClientHydrateChildSessions :: !Bool,
    daemonClientRestoreTerminalsOnLoad :: !Bool,
    daemonClientConfiguration :: !Configuration.SessionConfiguration,
    daemonClientSystemPrompt :: !(Maybe SystemPromptConfig),
    daemonClientSpawnOptions :: !Configuration.DaemonSpawnOptions,
    daemonClientInitializationTimeoutMicros :: !(Maybe Int),
    daemonClientLoadConfiguration :: !Configuration.SessionLoadConfiguration,
    daemonClientLoadSpawnConfiguration :: !Configuration.DaemonLoadConfiguration,
    daemonClientObservability :: !Obs.DroidObservability
  }

instance Show DaemonClientOptions where show _ = "DaemonClientOptions <redacted>"

defaultDaemonClientOptions :: DaemonAuthentication -> Text -> DaemonClientOptions
defaultDaemonClientOptions authentication cwd = DaemonClientOptions authentication cwd Nothing Nothing "local" Nothing Nothing "1.201.1" defaultMcpSessionOptions [] True True Configuration.defaultSessionConfiguration Nothing Configuration.defaultDaemonSpawnOptions Nothing Configuration.defaultSessionLoadConfiguration Configuration.defaultDaemonLoadConfiguration Obs.defaultDroidObservability

-- | Creation and future-load intent for one scoped attachment. Authentication,
-- protocol, transport and connection observers remain owned by the connection.
-- MCP configurations here apply to initialization; later loads retain the
-- connection's existing MCP policy. Scope any caller-owned hosted servers
-- around the operations that need their endpoints.
data DaemonSessionOptions = DaemonSessionOptions
  { daemonSessionParameters :: !Configuration.InitializeSessionParams,
    daemonSessionSpawnOptions :: !Configuration.DaemonSpawnOptions,
    daemonSessionTurnTimeoutMicros :: !(Maybe Int),
    daemonSessionInitializationTimeoutMicros :: !(Maybe Int),
    daemonSessionLoadConfiguration :: !Configuration.SessionLoadConfiguration,
    daemonSessionLoadSpawnConfiguration :: !Configuration.DaemonLoadConfiguration
  }
  deriving stock (Eq)

instance Show DaemonSessionOptions where show _ = "DaemonSessionOptions <redacted>"

defaultDaemonSessionOptions :: Text -> DaemonSessionOptions
defaultDaemonSessionOptions directory = DaemonSessionOptions (Configuration.defaultInitializeSessionParams "local" directory) Configuration.defaultDaemonSpawnOptions Nothing Nothing Configuration.defaultSessionLoadConfiguration Configuration.defaultDaemonLoadConfiguration

sessionCreationOptions :: DaemonClientOptions -> McpSessionOptions -> DaemonSessionOptions
sessionCreationOptions options mcp =
  DaemonSessionOptions
    ( (Configuration.defaultInitializeSessionParams (daemonClientMachineId options) (daemonClientWorkingDirectory options))
        { Configuration.initializeModel = daemonClientModel options,
          Configuration.initializeSystemPrompt = daemonClientSystemPrompt options,
          Configuration.initializeMcpOptions = mcp,
          Configuration.initializeWorktree = daemonClientWorktree options,
          Configuration.initializeWorktreeDirectory = daemonClientWorktreeDirectory options,
          Configuration.initializeConfiguration = daemonClientConfiguration options
        }
    )
    (daemonClientSpawnOptions options)
    (daemonClientTurnTimeoutMicros options)
    (daemonClientInitializationTimeoutMicros options)
    (daemonClientLoadConfiguration options)
    (daemonClientLoadSpawnConfiguration options)

daemonClientOptions :: DaemonOptions -> DaemonClientOptions
daemonClientOptions options = DaemonClientOptions (DaemonAuthenticate (daemonCredential options)) (daemonWorkingDirectory options) (daemonWorktree options) (daemonWorktreeDirectory options) (daemonMachineId options) (daemonModel options) (daemonTurnTimeoutMicros options) (daemonProtocolVersion options) (daemonMcpOptions options) (daemonHostedMcpServers options) (daemonHydrateChildSessions options) (daemonRestoreTerminalsOnLoad options) (daemonConfiguration options) (daemonSystemPrompt options) (daemonSpawnOptions options) (daemonInitializationTimeoutMicros options) (daemonLoadConfiguration options) (daemonLoadSpawnConfiguration options) (daemonObservability options)

data DaemonError = InvalidDaemonCredential | DaemonCredentialUnavailable | DaemonUnauthenticated | DaemonIdentityMismatch | DaemonAuthenticationSuperseded | DaemonModelRequiresNewSession | DaemonWorktreeRequiresNewSession | DaemonSessionAlreadyAttached | DaemonSessionNotRegistered | DaemonStateClosed | DaemonStateInUse | DaemonLoadSuperseded | DaemonLoadInterrupted | InvalidChildSessionIdentity | InvalidSessionCacheIdentity
  deriving stock (Eq, Show)

instance Exception DaemonError

-- | An authenticated connection that can own multiple independent attachments.
data DaemonConnection = DaemonConnection !Core.SessionConnection !DaemonContext

-- | One logical client's observations and pending controller. Physical
-- generations are exclusive within this scope; no reader or reconnect loop
-- is created by the state owner itself.
data DaemonState = DaemonState
  { sharedOpen :: !(TVar Bool),
    sharedLease :: !(TVar (Maybe DaemonStateLease)),
    sharedPrincipal :: !(TVar (Maybe (Text, Text))),
    sharedGeneration :: !(TVar Integer),
    sharedCoreState :: !Core.SessionStore,
    sharedLoads :: !(TVar (Map Text SessionLoadEntry)),
    sharedSessionStates :: !(TVar (Map Text SessionState.SessionState)),
    sharedCache :: !(TVar DaemonSessionCache),
    sharedChildOrder :: !(TVar [Text]),
    sharedInteractions :: !Interaction.PendingInteractions
  }

instance Show DaemonState where show _ = "DaemonState <redacted>"

data DaemonStateLease = PreparingState !Unique | ActiveState !Unique !(TVar Bool) !(TVar Bool) | RetiringState !Unique ![Text]

-- | Cached observations, not remote truth or a persistence format. Intermediate
-- snapshots can coalesce. Settings, missions and other retained metadata are
-- independent of the registered-view cache limit.
data DaemonStateSnapshot = DaemonStateSnapshot
  { daemonStateGeneration :: !Integer,
    daemonStateHealth :: !(Maybe ConnectionHealth),
    daemonStateDirectory :: ![SessionDirectoryEntry],
    daemonStateSessions :: !(Map Text SessionState.SessionState),
    daemonStateSettings :: !(Map Text (Either Core.DroidError SessionSettings)),
    daemonStateMissions :: !Mission.MissionRegistry,
    daemonStatePending :: !Interaction.PendingSnapshot,
    daemonStateCacheCapacity :: !(Maybe Natural),
    daemonStateActiveSession :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show DaemonStateSnapshot where show _ = "DaemonStateSnapshot <redacted>"

-- | Retain one logical client's state across sequential physical generations.
-- Exit revokes authority and joins an admitted generation before closing the
-- pending controller. Callbacks and acquired resources must be cooperative.
withDaemonState :: (DaemonState -> IO a) -> IO a
withDaemonState action = mask $ \restore -> do
  open <- newTVarIO True
  lease <- newTVarIO Nothing
  principal <- newTVarIO Nothing
  generation <- newTVarIO 0
  core <- Core.newSessionStore
  loads <- newTVarIO mempty
  sessions <- newTVarIO mempty
  cache <- newTVarIO (DaemonSessionCache (Just 20) Nothing [] [] mempty)
  order <- newTVarIO []
  pending <- Interaction.newPendingInteractions
  let shared = DaemonState open lease principal generation core loads sessions cache order pending
  finallyPreserving (restore (action shared)) (closeDaemonState shared)

closeDaemonState :: DaemonState -> IO ()
closeDaemonState shared = mask_ $ do
  atomically $ do
    writeTVar (sharedOpen shared) False
    lease <- readTVar (sharedLease shared)
    forM_ lease (suspendDaemonState shared . stateLeaseToken)
    Interaction.markAllPendingInactive (sharedInteractions shared)
  uninterruptibleMask_ (atomically (readTVar (sharedLease shared) >>= check . isNothing))
  atomically (Interaction.closePendingInteractions (sharedInteractions shared))

stateLeaseToken :: DaemonStateLease -> Unique
stateLeaseToken = \case PreparingState token -> token; ActiveState token _ _ -> token; RetiringState token _ -> token

stateLeaseUsable :: Unique -> Maybe DaemonStateLease -> Bool
stateLeaseUsable token = \case Just (PreparingState current) -> current == token; Just (ActiveState current _ _) -> current == token; _ -> False

stateGenerationOpen :: DaemonState -> Unique -> STM Bool
stateGenerationOpen shared token = (&&) <$> readTVar (sharedOpen shared) <*> (stateLeaseUsable token <$> readTVar (sharedLease shared))

checkDaemonState :: DaemonState -> STM ()
checkDaemonState shared = readTVar (sharedOpen shared) >>= \open -> unless open (throwSTM DaemonStateClosed)

withDaemonStateLease :: DaemonState -> (Unique -> IO a) -> IO a
withDaemonStateLease shared action = mask $ \restore -> do
  token <- newUnique
  atomically $ do
    checkDaemonState shared
    current <- readTVar (sharedLease shared)
    when (isJust current) (throwSTM DaemonStateInUse)
    writeTVar (sharedLease shared) (Just (PreparingState token))
  finallyPreserving
    ( restore $ do
        outcome <- race (atomically (readTVar (sharedOpen shared) >>= check . not)) (action token)
        either (const (throwIO DaemonStateClosed)) pure outcome
    )
    ( atomically $ do
        suspendDaemonState shared token
        current <- readTVar (sharedLease shared)
        case current of
          Just (RetiringState owner identifiers) | owner == token -> do
            forM_ identifiers (\identifier -> invalidateStoredSessionLoad (sharedLoads shared) identifier False)
            clearGenerationRequests shared
            Interaction.markAllPendingInactive (sharedInteractions shared)
            writeTVar (sharedLease shared) Nothing
          _ -> pure ()
    )

bindDaemonStatePrincipal :: DaemonState -> Unique -> GetUserInfoResult -> STM ()
bindDaemonStatePrincipal shared token identity = do
  checkDaemonState shared
  lease <- readTVar (sharedLease shared)
  unless (stateLeaseUsable token lease) (throwSTM DaemonStateClosed)
  previous <- readTVar (sharedPrincipal shared)
  let principal = (reportedUserId identity, reportedOrgId identity)
  when (maybe False (/= principal) previous) (throwSTM DaemonIdentityMismatch)
  writeTVar (sharedPrincipal shared) (Just principal)

activateDaemonState :: DaemonConnection -> STM ()
activateDaemonState owned@(DaemonConnection core state) = do
  checkConnection owned
  let shared = connectionLogicalState state
  modifyTVar' (sharedGeneration shared) (+ 1)
  writeTVar (sharedLease shared) (Just (ActiveState (connectionGenerationToken state) (Core.connectionOpen core) (connectionAuthenticated state)))

suspendDaemonState :: DaemonState -> Unique -> STM ()
suspendDaemonState shared token = do
  current <- readTVar (sharedLease shared)
  when (stateLeaseUsable token current) $ do
    case current of
      Just (ActiveState _ open authenticated) -> writeTVar open False >> writeTVar authenticated False
      _ -> pure ()
    entries <- readTVar (sharedLoads shared)
    let identifiers = [identifier | (identifier, entry) <- Map.toList entries, let readiness = entryReadiness entry, readinessPhase readiness /= SessionNotLoaded || readinessLoading readiness || isJust (entryFlight entry)]
    forM_ identifiers (\identifier -> invalidateStoredSessionLoad (sharedLoads shared) identifier False)
    clearGenerationRequests shared
    modifyTVar' (sharedSessionStates shared) (Map.map SessionState.retireSessionSubmissions)
    Interaction.markAllPendingInactive (sharedInteractions shared)
    writeTVar (sharedLease shared) (Just (RetiringState token identifiers))

-- Old local completion/retirement markers do not authorize a new peer's
-- restored requests. Explicit deferred decisions are revalidated separately.
clearGenerationRequests :: DaemonState -> STM ()
clearGenerationRequests shared = modifyTVar' (sharedLoads shared) (Map.map (\entry -> entry {entryRestoredRequests = mempty, entryCompletedRequests = mempty, entryRetiringRequests = mempty}))

connectionState :: DaemonConnection -> DaemonState
connectionState (DaemonConnection _ state) = connectionLogicalState state

daemonStatePendingInteractions :: DaemonState -> Interaction.PendingInteractions
daemonStatePendingInteractions = sharedInteractions

getDaemonStateSnapshot :: DaemonState -> IO DaemonStateSnapshot
getDaemonStateSnapshot = atomically . readDaemonStateSnapshot

readDaemonStateSnapshot :: DaemonState -> STM DaemonStateSnapshot
readDaemonStateSnapshot shared = do
  checkDaemonState shared
  generation <- readTVar (sharedGeneration shared)
  lease <- readTVar (sharedLease shared)
  health <- case lease of
    Just (ActiveState _ open authenticated) -> do
      connected <- readTVar open
      accepted <- readTVar authenticated
      pure (Just (ConnectionHealth connected (connected && accepted)))
    _ -> pure Nothing
  directory <- readSharedDirectory shared
  sessions <- readTVar (sharedSessionStates shared)
  (settings, missions) <- Core.readSessionStore (sharedCoreState shared)
  pending <- Interaction.readPendingSnapshot (sharedInteractions shared)
  cache <- readTVar (sharedCache shared)
  pure (DaemonStateSnapshot generation health directory sessions settings missions pending (cacheMaximum cache) (cacheActiveSession cache))

data DaemonContext = DaemonContext
  { connectionIdentity :: !GetUserInfoResult,
    connectionDefaultMachineId :: !Text,
    connectionLogicalState :: !DaemonState,
    connectionGenerationToken :: !Unique,
    connectionCredential :: !(TVar (IO Text)),
    connectionAuthenticated :: !(TVar Bool),
    connectionAuthLock :: !(MVar (Maybe SomeException)),
    connectionAuthEpoch :: !(TVar Integer),
    connectionInteractions :: !Interaction.PendingInteractions,
    connectionBindings :: !(TVar (Map Text DaemonBinding)),
    connectionLoads :: !(TVar (Map Text SessionLoadEntry)),
    connectionSessionStates :: !(TVar (Map Text SessionState.SessionState)),
    connectionSessionCache :: !(TVar DaemonSessionCache),
    connectionChildOrder :: !(TVar [Text]),
    connectionChildHydrations :: !(TVar (Maybe (Map Text (Unique, Async ())))),
    connectionHydrateChildren :: !Bool,
    connectionRestoreTerminals :: !Bool,
    connectionPendingSessionReady :: !(Maybe (Text -> STM () -> IO ())),
    connectionKind :: !Transport.TransportKind,
    connectionLoadDefaults :: !DaemonLoadPolicy
  }

data DaemonBinding = DaemonBinding
  { bindingSession :: !Core.DroidSession,
    bindingHandlers :: !(TVar DroidHandlers),
    bindingToken :: !Unique,
    bindingTerminalWriters :: !(TVar (Map Text (Unique, Text -> IO ()))),
    bindingTerminalWriteLock :: !(MVar ())
  }

-- Payloads stay in connectionSessionStates. These are only residency/access
-- indexes and references to closed attachments whose admitted work is retiring.
data DaemonSessionCache = DaemonSessionCache
  { cacheMaximum :: !(Maybe Natural),
    cacheActiveSession :: !(Maybe Text),
    cacheSessions :: ![(Text, Text)],
    cacheAccessOrder :: ![Text],
    cacheRetiringBindings :: !(Map Unique Core.DroidSession)
  }

-- | An atomic local directory view, not a remote daemon listing. Machine
-- association is local metadata; changing it does not reroute requests.
data SessionDirectoryEntry = SessionDirectoryEntry
  { directorySessionId :: !Text,
    directoryMachineId :: !Text,
    directoryReadiness :: !SessionReadiness,
    directoryWorkingDirectory :: !SessionState.WorkingDirectoryState
  }
  deriving stock (Eq)

instance Show SessionDirectoryEntry where show _ = "SessionDirectoryEntry <redacted>"

-- | Register an empty, not-yet-loaded local session without an RPC. False
-- means it already existed: neither association nor recency is then changed.
-- A new entry remains subject to the ordinary cache policy.
registerSessionState :: DaemonConnection -> Text -> Text -> IO Bool
registerSessionState connection@(DaemonConnection _ state) identifier machine = atomically $ do
  checkConnection connection
  validateCacheIdentity identifier
  cache <- readTVar (connectionSessionCache state)
  if identifier `elem` cachedSessionIds cache
    then pure False
    else do
      modifySessionState (connectionSessionStates state) identifier id
      modifyTVar' (connectionLoads state) (Map.alter (Just . (\entry -> entry {entryReadiness = (entryReadiness entry) {readinessKnown = True}}) . fromMaybe emptyLoadEntry) identifier)
      rememberChildOrderForMachine state identifier machine
      void (pruneRegisteredSessions state)
      pure True

-- | Current registered entries in insertion order, after eligible maintenance.
getSessionDirectory :: DaemonConnection -> IO [SessionDirectoryEntry]
getSessionDirectory connection@(DaemonConnection _ state) = atomically (void (pruneRegisteredSessions state) >> readSessionDirectory connection)

-- | Read the existing owners atomically. This read performs no maintenance;
-- compose it with STM 'check' to wait for association/readiness/cwd changes.
readSessionDirectory :: DaemonConnection -> STM [SessionDirectoryEntry]
readSessionDirectory connection@(DaemonConnection _ state) = do
  checkConnection connection
  readSharedDirectory (connectionLogicalState state)

readSharedDirectory :: DaemonState -> STM [SessionDirectoryEntry]
readSharedDirectory shared = do
  cache <- readTVar (sharedCache shared)
  loads <- readTVar (sharedLoads shared)
  states <- readTVar (sharedSessionStates shared)
  pure [SessionDirectoryEntry identifier machine (entryReadiness (Map.findWithDefault emptyLoadEntry identifier loads)) (maybe SessionState.WorkingDirectoryUnknown SessionState.sessionWorkingDirectory (Map.lookup identifier states)) | (identifier, machine) <- cacheSessions cache]

getSessionMachineId :: DaemonConnection -> Text -> IO (Maybe Text)
getSessionMachineId connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  validateCacheIdentity identifier
  lookup identifier . cacheSessions <$> readTVar (connectionSessionCache state)

-- | Reassociate an existing local entry. Empty machine IDs remain explicit;
-- absent sessions fail instead of fabricating an entry or querying a daemon.
setSessionMachineId :: DaemonConnection -> Text -> Text -> IO ()
setSessionMachineId connection@(DaemonConnection _ state) identifier machine = atomically $ do
  checkConnection connection
  validateCacheIdentity identifier
  cache <- readTVar (connectionSessionCache state)
  case lookup identifier (cacheSessions cache) of
    Nothing -> throwSTM DaemonSessionNotRegistered
    Just previous ->
      when (previous /= machine) $
        writeTVar (connectionSessionCache state) (cache {cacheSessions = map (\(key, value) -> (key, if key == identifier then machine else value)) (cacheSessions cache)})

-- | Remembered loaded/loading use, excluding throwaway pre-init entries.
-- This does not probe connectivity or imply that a model turn is running.
hasActiveSessionsForMachine :: DaemonConnection -> Text -> IO Bool
hasActiveSessionsForMachine connection machine = atomically $ do
  entries <- readSessionDirectory connection
  pure (any (\entry -> let readiness = directoryReadiness entry in directoryMachineId entry == machine && not (readinessPreInit readiness) && (readinessPhase readiness /= SessionNotLoaded || readinessLoading readiness)) entries)

-- | Count non-idle entries for an exact machine/cwd pair, excluding pre-init.
-- An unreported working state is idle for this projection. Relevant malformed
-- observations fail explicitly rather than being counted as idle or current.
countActiveSessionsForCwd :: DaemonConnection -> Text -> Text -> IO Natural
countActiveSessionsForCwd connection machine directory = atomically $ do
  entries <- readSessionDirectory connection
  active <- filterM matches [entry | entry <- entries, directoryMachineId entry == machine, not (readinessPreInit (directoryReadiness entry))]
  pure (fromIntegral (length active))
  where
    busy = either throwSTM (pure . maybe False (/= WorkingIdle)) . readinessWorkingState . directoryReadiness
    matches entry = case directoryWorkingDirectory entry of
      SessionState.WorkingDirectoryReported (Just actual) | actual == directory -> busy entry
      SessionState.WorkingDirectoryInherited actual | actual == directory -> busy entry
      SessionState.WorkingDirectoryInvalid _ -> do
        active <- busy entry
        if active then throwSTM Core.DroidInvalidEvent else pure False
      _ -> pure False

-- | Invalidate the registered loaded/loading members of one machine as one
-- transaction, returning their IDs. Existing epochs reject superseded receipts;
-- other machines, association, history and retained load intent are preserved.
-- No transport, attachment or remote session is closed by this operation.
markSessionsNotLoadedForMachine :: DaemonConnection -> Text -> IO [Text]
markSessionsNotLoadedForMachine connection@(DaemonConnection _ state) machine = atomically $ do
  entries <- readSessionDirectory connection
  let selected = [directorySessionId entry | entry <- entries, directoryMachineId entry == machine, let readiness = directoryReadiness entry, readinessPhase readiness /= SessionNotLoaded || readinessLoading readiness]
  forM_ selected (\identifier -> invalidateSessionLoad state identifier False)
  pure selected

cachedSessionIds :: DaemonSessionCache -> [Text]
cachedSessionIds = map fst . cacheSessions

-- | Eligible registered-session capacity; the default is @Just 20@.
-- 'Nothing' disables automatic eviction, not explicit removal.
getSessionCacheCapacity :: DaemonConnection -> IO (Maybe Natural)
getSessionCacheCapacity connection@(DaemonConnection _ state) = atomically $ do
  checkConnection connection
  cacheMaximum <$> readTVar (connectionSessionCache state)

-- | Set the soft capacity and return the IDs evicted, least-recent first.
-- Zero retains protected entries only. No remote close or logout is sent.
setSessionCacheCapacity :: DaemonConnection -> Maybe Natural -> IO [Text]
setSessionCacheCapacity connection@(DaemonConnection _ state) capacity = atomically $ do
  checkConnection connection
  modifyTVar' (connectionSessionCache state) (\cache -> cache {cacheMaximum = capacity})
  pruneRegisteredSessions state

-- | Registered cached IDs in insertion order, after eligible eviction.
-- Durable metadata and not-yet-registered observations are not this cache.
getCachedSessionIds :: DaemonConnection -> IO [Text]
getCachedSessionIds connection@(DaemonConnection _ state) = atomically $ do
  checkConnection connection
  void (pruneRegisteredSessions state)
  cachedSessionIds <$> readTVar (connectionSessionCache state)

-- | Mark a cached session recently used. Unknown IDs are not registered.
touchSession :: DaemonConnection -> Text -> IO Bool
touchSession connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  validateCacheIdentity identifier
  cache <- readTVar (connectionSessionCache state)
  let present = identifier `elem` cachedSessionIds cache
  when present (writeTVar (connectionSessionCache state) (touchCachedEntry identifier cache))
  pure present

getActiveSessionId :: DaemonConnection -> IO (Maybe Text)
getActiveSessionId connection@(DaemonConnection _ state) = atomically $ do
  checkConnection connection
  cacheActiveSession <$> readTVar (connectionSessionCache state)

-- | Pin the viewed cache entry, not a daemon-side active session. A valid
-- not-yet-registered ID can be selected without loading or fabricating it.
setActiveSessionId :: DaemonConnection -> Maybe Text -> IO [Text]
setActiveSessionId connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  forM_ identifier validateCacheIdentity
  modifyTVar' (connectionSessionCache state) (\cache -> maybe id touchCachedEntry identifier (cache {cacheActiveSession = identifier}))
  pruneRegisteredSessions state

-- | Remove an eligible cached entry. False means absent or protected; this
-- does not revoke live attachments, pending decisions or remote resources.
removeCachedSession :: DaemonConnection -> Text -> IO Bool
removeCachedSession connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  validateCacheIdentity identifier
  cache <- settleCacheLeases state
  removable <- cacheEntryEligible state cache identifier
  when removable (forgetCachedEntry state identifier)
  pure removable

-- | Reapply the capacity after local activity settles. Attached, loading,
-- non-idle, selected and locally retiring sessions remain protected; retained
-- summaries, policy, counters and other owners are not a hard memory bound.
pruneSessionCache :: DaemonConnection -> IO [Text]
pruneSessionCache connection@(DaemonConnection _ state) = atomically (checkConnection connection >> pruneRegisteredSessions state)

validateCacheIdentity :: Text -> STM ()
validateCacheIdentity identifier = unless (validChildIdentity identifier) (throwSTM InvalidSessionCacheIdentity)

touchCachedEntry :: Text -> DaemonSessionCache -> DaemonSessionCache
touchCachedEntry identifier cache
  | identifier `elem` cachedSessionIds cache = cache {cacheAccessOrder = filter (/= identifier) (cacheAccessOrder cache) <> [identifier]}
  | otherwise = cache

registerCachedEntry :: DaemonContext -> Text -> Text -> STM ()
registerCachedEntry state identifier machine = do
  validateCacheIdentity identifier
  modifyTVar' (connectionSessionCache state) $ \cache ->
    touchCachedEntry identifier (cache {cacheSessions = if identifier `elem` cachedSessionIds cache then cacheSessions cache else cacheSessions cache <> [(identifier, machine)]})

settleCacheLeases :: DaemonContext -> STM DaemonSessionCache
settleCacheLeases state = do
  cache <- readTVar (connectionSessionCache state)
  retiring <- Map.traverseMaybeWithKey (\_ session -> do idle <- Core.isDroidSessionIdle session; pure (if idle then Nothing else Just session)) (cacheRetiringBindings cache)
  let current = cache {cacheRetiringBindings = retiring}
  when (Map.size retiring /= Map.size (cacheRetiringBindings cache)) (writeTVar (connectionSessionCache state) current)
  pure current

cacheEntryEligible :: DaemonContext -> DaemonSessionCache -> Text -> STM Bool
cacheEntryEligible state cache identifier = do
  bindings <- readTVar (connectionBindings state)
  loads <- readTVar (connectionLoads state)
  states <- readTVar (connectionSessionStates state)
  let loadingOrWorking entry =
        let readiness = entryReadiness entry
            pending = any (\(requestId, (method, _)) -> Set.notMember (method, requestId) (entryCompletedRequests entry)) (Map.toList (entryRestoredRequests entry))
         in isJust (entryFlight entry) || readinessLoading readiness || readinessWorkingState readiness `notElem` [Right Nothing, Right (Just WorkingIdle)] || pending
      deferred snapshot = not (Map.null (SessionState.sessionDeferredPermissions snapshot) && Map.null (SessionState.sessionDeferredQuestions snapshot))
      retiring = any ((== identifier) . Core.droidSessionId) (Map.elems (cacheRetiringBindings cache))
  pure (identifier `elem` cachedSessionIds cache && cacheActiveSession cache /= Just identifier && Map.notMember identifier bindings && not retiring && not (maybe False loadingOrWorking (Map.lookup identifier loads)) && not (maybe False deferred (Map.lookup identifier states)))

pruneRegisteredSessions :: DaemonContext -> STM [Text]
pruneRegisteredSessions state = do
  cache <- settleCacheLeases state
  let count = length (cacheSessions cache)
      excess = case cacheMaximum cache of Just capacity | capacity < fromIntegral count -> count - fromIntegral capacity; _ -> 0
  selected <- if excess == 0 then pure [] else take excess <$> filterM (cacheEntryEligible state cache) (cacheAccessOrder cache)
  forM_ selected (forgetCachedEntry state)
  pure selected

forgetCachedEntry :: DaemonContext -> Text -> STM ()
forgetCachedEntry state identifier = do
  -- Keep the generation tombstone and independently retained load policy.
  invalidateSessionLoad state identifier False
  modifyTVar' (connectionLoads state) (Map.adjust (\entry -> entry {entryCreationOwner = Nothing, entryReadiness = (entryReadiness entry) {readinessKnown = False, readinessPreInit = False}}) identifier)
  modifyTVar' (connectionSessionStates state) (Map.update retain identifier)
  modifyTVar' (connectionChildOrder state) (filter (/= identifier))
  modifyTVar' (connectionSessionCache state) (\cache -> cache {cacheSessions = filter ((/= identifier) . fst) (cacheSessions cache), cacheAccessOrder = filter (/= identifier) (cacheAccessOrder cache)})
  where
    retain previous = let kept = SessionState.clearCachedSessionState previous in if kept == SessionState.emptySessionState then Nothing else Just kept

-- | Uses endpoint, transport, credentials, protocol and restoration policies.
-- No root session is created; other configuration mutations remain explicit.
withConnection :: DaemonOptions -> (DaemonConnection -> IO a) -> IO a
withConnection options = withDaemonConnection options True defaultMcpSessionOptions

-- | Report physical transport acquisition before daemon authentication. The
-- observer must not wait for authentication through this same constructor.
withConnectionObserved :: DaemonOptions -> IO () -> (DaemonConnection -> IO a) -> IO a
withConnectionObserved options opened =
  withDaemonConnectionUsing (daemonClientOptions options) True defaultMcpSessionOptions $ \action ->
    withDaemonTransport options $ \transport -> opened >> action transport

-- | Use retained logical state with a fresh physical endpoint scope. A state
-- owner accepts one generation at a time and binds to its first user/org.
withConnectionState :: DaemonState -> DaemonOptions -> (DaemonConnection -> IO a) -> IO a
withConnectionState shared options = withConnectionStateObserved shared options (pure ())

withConnectionStateObserved :: DaemonState -> DaemonOptions -> IO () -> (DaemonConnection -> IO a) -> IO a
withConnectionStateObserved shared options opened =
  withDaemonConnectionStateUsing shared (daemonClientOptions options) True defaultMcpSessionOptions $ \action ->
    withDaemonTransport options $ \transport -> opened >> action transport

connectionUser :: DaemonConnection -> GetUserInfoResult
connectionUser (DaemonConnection _ state) = connectionIdentity state

-- | Remembered authentication plus transport lifetime, not a heartbeat or a
-- replacement for the immutable 'connectionUser' authentication receipt.
isAuthenticated :: DaemonConnection -> IO Bool
isAuthenticated connection = healthAuthenticated <$> getConnectionHealth connection

data ConnectionHealth = ConnectionHealth
  { healthTransportConnected :: !Bool,
    healthAuthenticated :: !Bool
  }
  deriving stock (Eq, Show)

-- | Atomic remembered transport/auth state. An expired or failed transport
-- cannot report usable authentication; this does not probe a half-open socket.
getConnectionHealth :: DaemonConnection -> IO ConnectionHealth
getConnectionHealth = atomically . readConnectionHealth

readConnectionHealth :: DaemonConnection -> STM ConnectionHealth
readConnectionHealth owned@(DaemonConnection _ state) = do
  connected <- connectionScopeOpen owned
  authenticated <- readTVar (connectionAuthenticated state)
  pure (ConnectionHealth connected (connected && authenticated))

-- | Explicit original transport failure data; not a sanitized diagnostic.
readConnectionFailureCause :: DaemonConnection -> STM (Maybe SomeException)
readConnectionFailureCause (DaemonConnection core _) = rpcChannelFailureCause (Core.connectionChannel core)

-- | SDK logical generation, not a peer identity, credential or physical handle.
getConnectionId :: DaemonConnection -> IO (Maybe Text)
getConnectionId owned@(DaemonConnection core _) = atomically $ do
  open <- connectionScopeOpen owned
  pure (if open then Just (Core.connectionRequestNamespace core) else Nothing)

getTransportKind :: DaemonConnection -> Transport.TransportKind
getTransportKind (DaemonConnection _ state) = connectionKind state

getPendingCount :: DaemonConnection -> IO Int
getPendingCount (DaemonConnection core _) = atomically (getRpcPendingCount (Core.connectionChannel core))

-- | Replace the optional load guard. Mandatory authentication gating remains.
setBeforeRequest :: DaemonConnection -> Maybe (Text -> Text -> IO ()) -> IO (IO ())
setBeforeRequest (DaemonConnection core _) = setRpcBeforeRequest (Core.connectionChannel core)

onRequestSettled :: DaemonConnection -> (Text -> IO ()) -> IO (IO ())
onRequestSettled (DaemonConnection core _) = onRpcRequestSettled (Core.connectionChannel core)

-- | Original failures are explicit sensitive data. Ordinary observer failures
-- are isolated; callbacks must not await later intake or load/lifecycle work.
onConnectionError :: DaemonConnection -> (SomeException -> IO ()) -> IO (IO ())
onConnectionError connection@(DaemonConnection core _) callback =
  onRpcError (Core.connectionDispatcher core) (connectionFailure connection >=> callback)

-- | Nothing is logical scope closure, not proof a borrowed transport closed.
-- A failed connection supplies its original cause when available (including
-- full WebSocket close metadata). Delivery follows error observers, once.
onConnectionClose :: DaemonConnection -> (Maybe SomeException -> IO ()) -> IO (IO ())
onConnectionClose connection@(DaemonConnection core _) callback =
  onRpcClose (Core.connectionDispatcher core) (traverse (connectionFailure connection) >=> callback)

connectionFailure :: DaemonConnection -> RpcChannelError -> IO SomeException
connectionFailure connection fallback = fromMaybe (toException fallback) <$> atomically (readConnectionFailureCause connection)

connectionUsable :: ConnectionHealth -> Bool
connectionUsable health = healthTransportConnected health && healthAuthenticated health

-- | Explicitly repair an open, unauthenticated connection in place. Concurrent
-- callers coalesce at the authentication lock. The original principal is fixed;
-- a new principal needs a new connection scope. A failed/uncertain attempt
-- retires the logical connection, leaving physical cleanup with its owner.
ensureConnectionAuthenticated :: DaemonConnection -> DaemonAuthentication -> IO ()
ensureConnectionAuthenticated owned@(DaemonConnection core state) authentication =
  withAuthenticationLock state $ do
    (ready, epoch) <- atomically $ do
      checkConnection owned
      (,) <$> readTVar (connectionAuthenticated state) <*> readTVar (connectionAuthEpoch state)
    unless ready $ do
      let retire = atomically $ do
            writeTVar (Core.connectionOpen core) False
            revokeAuthentication state
      ( do
          observed <- newEmptyTMVarIO
          let publish identity = do
                open <- readTVar (Core.connectionOpen core)
                current <- readTVar (connectionAuthEpoch state)
                let expected = connectionIdentity state
                    samePrincipal = reportedUserId identity == reportedUserId expected && reportedOrgId identity == reportedOrgId expected
                result <-
                  if not open || current /= epoch
                    then pure (Left DaemonAuthenticationSuperseded)
                    else
                      if not samePrincipal
                        then do
                          writeTVar (Core.connectionOpen core) False
                          revokeAuthentication state
                          pure (Left DaemonIdentityMismatch)
                        else do
                          writeTVar (connectionAuthenticated state) True
                          writeTVar (connectionCredential state) (authenticationToken authentication)
                          pure (Right ())
                void (tryPutTMVar observed result)
              authenticate credential = do
                validateDaemonAuthentication (DaemonAuthenticate credential)
                Core.connectionBoundary core 30000000 $
                  Core.connectionRequest core 30000000 $ \rpc options ->
                    void (Client.callObserved (Proxy @(WithEnvelope (MethodRequest "daemon.authenticate" Object))) rpc options (authenticationParams credential) publish)
          case authentication of
            DaemonAuthenticate credential -> authenticate credential
            DaemonTokenProvider provider grant -> fetchSessionToken provider >>= \token -> authenticate (DaemonToken token grant)
            DaemonInheritAuthentication identity _ -> atomically (publish identity)
            DaemonInheritAuthenticationProvider identity _ -> atomically (publish identity)
          outcome <- atomically $ do
            result <- readTMVar observed
            current <- readTVar (connectionAuthEpoch state)
            pure $ case result of
              Left failure -> Left failure
              Right () | current /= epoch -> Left DaemonAuthenticationSuperseded
              Right () -> Right ()
          either throwIO pure outcome
        )
        `onException` retire

-- Authentication failure retires this logical scope, so the lock retains its
-- first outcome for callers already waiting without letting them adopt a retry.
withAuthenticationLock :: DaemonContext -> IO a -> IO a
withAuthenticationLock state action = mask $ \restore -> do
  result <- modifyMVar (connectionAuthLock state) $ \failure -> do
    outcome <- try @SomeException (restore (maybe action throwIO failure))
    pure (failure <|> either Just (const Nothing) outcome, outcome)
  either throwIO pure result

revokeAuthentication :: DaemonContext -> STM ()
revokeAuthentication state = do
  writeTVar (connectionAuthenticated state) False
  modifyTVar' (connectionAuthEpoch state) (+ 1)
  Interaction.clearPendingInteractions (connectionInteractions state)
  modifyTVar' (connectionSessionStates state) (Map.map (snd . SessionState.clearDeferredUserActions))

pendingInteractions :: DaemonConnection -> Interaction.PendingInteractions
pendingInteractions (DaemonConnection _ state) = connectionInteractions state

checkAuthenticated :: DaemonConnection -> STM ()
checkAuthenticated connection@(DaemonConnection _ state) = do
  checkConnection connection
  authenticated <- readTVar (connectionAuthenticated state)
  unless authenticated (throwSTM DaemonUnauthenticated)

-- | Explicit manual-response scope. Otherwise-unconfigured requests defer only
-- here; configured callbacks remain automatic. Scope exit cancels requests it
-- admitted, not older requests. The controller borrows the physical connection.
withPendingInteractions :: DaemonConnection -> (Interaction.PendingInteractions -> IO a) -> IO a
withPendingInteractions connection action = do
  atomically (checkAuthenticated connection)
  let controller = pendingInteractions connection
  Interaction.withDeferredResponses controller (action controller)

-- | Replace handlers for future requests/events. An admitted callback retains
-- its snapshot; no permission, reauthentication or remote settings update occurs.
setSessionHandlers :: DaemonSession -> DroidHandlers -> IO ()
setSessionHandlers session handlers = Core.withSessionUse (daemonCoreSession session) (atomically (writeTVar (bindingHandlers (daemonSessionBinding session)) handlers))

-- | Retain a decision for an inactive request without sending it or resuming
-- the session. A later active request requires explicit revalidated submission.
deferPermissionResponse :: DaemonConnection -> Text -> Interaction.PendingInteractionId -> RequestPermissionResult -> IO ()
deferPermissionResponse connection@(DaemonConnection _ state) surface token response = do
  _ <- evaluate (force (toJSON response))
  timestamp <- fromInteger . floor . (* 1000) <$> getPOSIXTime
  atomically $ do
    checkAuthenticated connection
    pending <- Interaction.deferPendingPermission (connectionInteractions state) surface token response
    identifier <- case permissionToolUses (Interaction.pendingRequestParams pending) of
      first : _ | not (Text.null (toolUseId (confirmationInfoToolUse first))) -> pure (toolUseId (confirmationInfoToolUse first))
      _ -> throwSTM Interaction.PendingCannotDefer
    let action = SessionState.DeferredUserAction (Interaction.pendingRequestId pending) identifier response timestamp (Interaction.pendingAssociatedSessionIds pending)
    modifySessionState (connectionSessionStates state) (Interaction.pendingSessionId pending) (SessionState.storeDeferredPermission action)

deferQuestionResponse :: DaemonConnection -> Text -> Interaction.PendingInteractionId -> AskUserResult -> IO ()
deferQuestionResponse connection@(DaemonConnection _ state) surface token response = do
  _ <- evaluate (force (toJSON response))
  timestamp <- fromInteger . floor . (* 1000) <$> getPOSIXTime
  atomically $ do
    checkAuthenticated connection
    pending <- Interaction.deferPendingQuestion (connectionInteractions state) surface token response
    let identifier = askUserToolCallId (Interaction.pendingRequestParams pending)
    when (Text.null identifier) (throwSTM Interaction.PendingCannotDefer)
    let action = SessionState.DeferredUserAction (Interaction.pendingRequestId pending) identifier response timestamp (Interaction.pendingAssociatedSessionIds pending)
    modifySessionState (connectionSessionStates state) (Interaction.pendingSessionId pending) (SessionState.storeDeferredQuestion action)

storeDeferredPermission :: DaemonConnection -> Text -> SessionState.DeferredUserAction RequestPermissionResult -> IO ()
storeDeferredPermission connection@(DaemonConnection _ state) identifier action = atomically (checkAuthenticated connection >> modifySessionState (connectionSessionStates state) identifier (SessionState.storeDeferredPermission (action {SessionState.deferredActionAssociatedSessionIds = identifier : SessionState.deferredActionAssociatedSessionIds action})))

storeDeferredQuestion :: DaemonConnection -> Text -> SessionState.DeferredUserAction AskUserResult -> IO ()
storeDeferredQuestion connection@(DaemonConnection _ state) identifier action = atomically (checkAuthenticated connection >> modifySessionState (connectionSessionStates state) identifier (SessionState.storeDeferredQuestion action))

takeDeferredPermission :: DaemonConnection -> Text -> Text -> IO (Maybe (SessionState.DeferredUserAction RequestPermissionResult))
takeDeferredPermission connection identifier = transitionDeferredState connection identifier . SessionState.takeDeferredPermission

takeDeferredQuestion :: DaemonConnection -> Text -> Text -> IO (Maybe (SessionState.DeferredUserAction AskUserResult))
takeDeferredQuestion connection identifier = transitionDeferredState connection identifier . SessionState.takeDeferredQuestion

clearDeferredUserActions :: DaemonConnection -> Text -> IO Int
clearDeferredUserActions connection identifier = transitionDeferredState connection identifier SessionState.clearDeferredUserActions

transitionDeferredState :: DaemonConnection -> Text -> (SessionState.SessionState -> (a, SessionState.SessionState)) -> IO a
transitionDeferredState connection@(DaemonConnection _ state) identifier change = atomically $ do
  checkConnection connection
  states <- readTVar (connectionSessionStates state)
  let (result, next) = change (Map.findWithDefault SessionState.emptySessionState identifier states)
  writeTVar (connectionSessionStates state) (Map.insert identifier next states)
  pure result

validChildIdentity :: Text -> Bool
validChildIdentity = not . Text.null . Text.dropAround isEcmaWhitespace

-- | Record discovery/linkage only; this does not load a session or grant its
-- permission handler. A self-link or a conflicting provisional parent is ignored.
registerChildSession :: DaemonConnection -> Text -> ChildSessionAvailable -> IO Bool
registerChildSession connection@(DaemonConnection _ state) parent available = do
  unless (validChildIdentity parent && validChildIdentity (availableChildSessionId available)) (throwIO InvalidChildSessionIdentity)
  atomically (checkConnection connection >> registerChildMetadata state parent available)

registerChildMetadata :: DaemonContext -> Text -> ChildSessionAvailable -> STM Bool
registerChildMetadata state parent available = do
  states <- readTVar (connectionSessionStates state)
  let identifier = availableChildSessionId available
      previous = Map.findWithDefault SessionState.emptySessionState identifier states
      conflicting = maybe False (\existing -> not (Text.null existing) && existing /= parent) (SessionState.sessionCallingSessionId previous)
  if identifier == parent || conflicting
    then pure False
    else do
      let parentCwd = maybe SessionState.WorkingDirectoryUnknown SessionState.sessionWorkingDirectory (Map.lookup parent states)
          discovered = SessionState.observeChildAvailable parent available previous
      writeTVar (connectionSessionStates state) (Map.insert identifier (SessionState.inheritWorkingDirectory parentCwd discovered) states)
      rememberChildOrder state identifier
      modifyTVar' (connectionLoads state) $ Map.alter (Just . seed . fromMaybe emptyLoadEntry) identifier
      void (pruneRegisteredSessions state)
      pure True
  where
    seed entry =
      let readiness = entryReadiness entry
          working = readinessWorkingState readiness
          seeded = if readinessPhase readiness /= SessionLoaded && working `elem` [Right Nothing, Right (Just WorkingIdle)] then Right (Just WorkingStreamingAssistantMessage) else working
       in entry {entryReadiness = readiness {readinessKnown = True, readinessNotFound = False, readinessWorkingState = seeded}}

rememberChildOrder :: DaemonContext -> Text -> STM ()
rememberChildOrder state identifier = rememberChildOrderForMachine state identifier (connectionDefaultMachineId state)

rememberChildOrderForMachine :: DaemonContext -> Text -> Text -> STM ()
rememberChildOrderForMachine state identifier machine = do
  modifyTVar' (connectionChildOrder state) (\known -> if identifier `elem` known then known else known <> [identifier])
  registerCachedEntry state identifier machine

-- | Ensure that this physical connection has loaded a registered child. It
-- joins the existing load coordinator; an unknown child does not cause an RPC.
ensureChildSessionAttached :: DaemonConnection -> Text -> IO Bool
ensureChildSessionAttached connection@(DaemonConnection _ state) identifier = do
  unless (validChildIdentity identifier) (throwIO InvalidChildSessionIdentity)
  known <- atomically $ do
    checkConnection connection
    states <- readTVar (connectionSessionStates state)
    let registered = isJust (Map.lookup identifier states >>= SessionState.sessionCallingSessionId)
    when registered $ modifyTVar' (connectionLoads state) (Map.alter (Just . (\entry -> entry {entryReadiness = (entryReadiness entry) {readinessKnown = True, readinessNotFound = False}}) . fromMaybe emptyLoadEntry) identifier)
    pure registered
  when known (ensureSessionLoaded connection identifier)
  pure known

findSubagentSessionId :: DaemonConnection -> Text -> Text -> IO (Maybe Text)
findSubagentSessionId connection parent tool = Map.lookup tool <$> getSubagentSessionIdsForParent connection parent

getSubagentSessionIdsForParent :: DaemonConnection -> Text -> IO (Map Text Text)
getSubagentSessionIdsForParent connection parent = Map.findWithDefault mempty parent <$> getSubagentSessionIdsByParent connection

getSubagentSessionIdsByParent :: DaemonConnection -> IO (Map Text (Map Text Text))
getSubagentSessionIdsByParent connection@(DaemonConnection _ state) = atomically $ do
  checkConnection connection
  order <- readTVar (connectionChildOrder state)
  states <- readTVar (connectionSessionStates state)
  let links = [(parent, tool, identifier) | identifier <- order, Just snapshot <- [Map.lookup identifier states], Just parent <- [SessionState.sessionCallingSessionId snapshot], not (Text.null parent), Just tool <- [SessionState.sessionCallingToolUseId snapshot], not (Text.null tool), parent /= identifier]
  pure (foldl' (\known (parent, tool, identifier) -> Map.alter (Just . Map.insertWith (\_ previous -> previous) tool identifier . fromMaybe mempty) parent known) mempty links)

getSubagentInvocationSummary :: DaemonConnection -> Text -> IO (Maybe SubagentInvocationSummary)
getSubagentInvocationSummary connection identifier = SessionState.sessionInvocationSummary <$> getSessionState connection identifier

setSubagentInvocationSummary :: DaemonConnection -> SubagentInvocationSummary -> IO ()
setSubagentInvocationSummary connection summary = do
  unless (validChildIdentity (invocationChildSessionId summary)) (throwIO InvalidChildSessionIdentity)
  hydrateSubagentInvocationSummaries connection [summary]

-- | Hydrate reported summaries only, without registering links or starting
-- children. Invalid blank identities are skipped, as in the source bulk API.
hydrateSubagentInvocationSummaries :: DaemonConnection -> [SubagentInvocationSummary] -> IO ()
hydrateSubagentInvocationSummaries connection@(DaemonConnection _ state) summaries = atomically (checkConnection connection >> hydrateInvocationSummaries state summaries)

hydrateInvocationSummaries :: DaemonContext -> [SubagentInvocationSummary] -> STM ()
hydrateInvocationSummaries state summaries = forM_ summaries $ \summary ->
  when (validChildIdentity (invocationChildSessionId summary)) $
    modifySessionState (connectionSessionStates state) (invocationChildSessionId summary) (SessionState.setInvocationSummary summary)

-- Seal the owned job map before cancellation. Every worker waits at a local
-- gate until it has been registered, so closing intake cannot orphan a launch.
startChildHydration :: DaemonConnection -> Text -> IO ()
startChildHydration connection@(DaemonConnection _ state) identifier = mask_ $ do
  needed <- atomically $ do
    jobs <- readTVar (connectionChildHydrations state)
    loads <- readTVar (connectionLoads state)
    let readiness = entryReadiness (Map.findWithDefault emptyLoadEntry identifier loads)
    pure (maybe False (Map.notMember identifier) jobs && not (readinessLoading readiness) && readinessPhase readiness /= SessionLoaded)
  when needed $ do
    token <- newUnique
    start <- newEmptyMVar
    worker <- asyncWithUnmask $ \restore ->
      (takeMVar start >> restore (void (ensureChildSessionAttached connection identifier)))
        `finally` atomically (modifyTVar' (connectionChildHydrations state) (fmap (Map.update (\(current, job) -> if current == token then Nothing else Just (current, job)) identifier)))
    let cancelUnstarted = uninterruptibleMask_ (cancel worker)
    ( do
        admitted <- atomically $ do
          current <- readTVar (connectionChildHydrations state)
          case current of
            Just jobs | Map.notMember identifier jobs -> writeTVar (connectionChildHydrations state) (Just (Map.insert identifier (token, worker) jobs)) >> pure True
            _ -> pure False
        if admitted then putMVar start () else cancelUnstarted
      )
      `onException` cancelUnstarted

stopChildHydrations :: TVar (Maybe (Map Text (Unique, Async ()))) -> IO ()
stopChildHydrations jobs = do
  pending <- atomically $ do
    current <- readTVar jobs
    writeTVar jobs Nothing
    pure (maybe [] (map snd . Map.elems) current)
  uninterruptibleMask_ (mapConcurrently_ cancel pending)

data SessionLoadPhase = SessionNotLoaded | SessionLoading | SessionLoaded deriving stock (Eq, Show)

-- | Load readiness and peer-reported working state are separate from local
-- handle admission. A pre-init flag never creates or pins a connection.
data SessionReadiness = SessionReadiness
  { readinessPhase :: !SessionLoadPhase,
    readinessKnown :: !Bool,
    readinessLoading :: !Bool,
    readinessNotFound :: !Bool,
    readinessPreInit :: !Bool,
    readinessWorkingState :: !(Either Core.DroidError (Maybe DroidWorkingState))
  }
  deriving stock (Eq, Show)

type SessionLoadTicket = TMVar (Either SomeException DaemonSessionInfo)

type RestoredSessionRequest = (Text, Text, Object)

type DaemonLoadPolicy = (Configuration.SessionLoadConfiguration, Configuration.DaemonLoadConfiguration)

mergeLoadPolicy :: DaemonLoadPolicy -> DaemonLoadPolicy -> DaemonLoadPolicy
mergeLoadPolicy (old, oldDaemon) (new, newDaemon) = (Configuration.mergeSessionLoadConfiguration old new, Configuration.mergeDaemonLoadConfiguration oldDaemon newDaemon)

data SessionLoadEntry = SessionLoadEntry
  { entryReadiness :: !SessionReadiness,
    entryEpoch :: !Integer,
    entryFlight :: !(Maybe SessionLoadTicket),
    entryRestoredRequests :: !(Map Text (Text, Object)),
    entryCompletedRequests :: !(Set (Text, Text)),
    entryRetiringRequests :: !(Set Text),
    entryLoadPolicy :: !(Maybe DaemonLoadPolicy),
    entryCreationOwner :: !(Maybe Unique)
  }

emptyLoadEntry :: SessionLoadEntry
emptyLoadEntry = SessionLoadEntry (SessionReadiness SessionNotLoaded False False False False (Right Nothing)) 0 Nothing mempty mempty mempty Nothing Nothing

sessionReadinessBusy :: SessionReadiness -> Either Core.DroidError Bool
sessionReadinessBusy = fmap (maybe False (/= WorkingIdle)) . readinessWorkingState

getSessionReadiness :: DaemonConnection -> Text -> IO SessionReadiness
getSessionReadiness connection identifier = atomically (readSessionReadiness connection identifier)

-- | Observe the next differing snapshot without another callback registry.
-- Intermediate states can coalesce; cancellation only stops this caller's wait.
waitSessionReadinessChange :: DaemonConnection -> Text -> SessionReadiness -> IO SessionReadiness
waitSessionReadinessChange connection identifier previous = atomically $ do
  current <- readSessionReadiness connection identifier
  check (current /= previous)
  pure current

checkConnection :: DaemonConnection -> STM ()
checkConnection connection = connectionScopeOpen connection >>= \open -> unless open (throwSTM RpcChannelClosed)

connectionScopeOpen :: DaemonConnection -> STM Bool
connectionScopeOpen (DaemonConnection core state) = do
  physical <- readTVar (Core.connectionOpen core)
  logical <- stateGenerationOpen (connectionLogicalState state) (connectionGenerationToken state)
  pure (physical && logical)

connectionAuthorityOpen :: DaemonConnection -> STM Bool
connectionAuthorityOpen connection@(DaemonConnection _ state) = (&&) <$> connectionScopeOpen connection <*> readTVar (connectionAuthenticated state)

readSessionReadiness :: DaemonConnection -> Text -> STM SessionReadiness
readSessionReadiness connection@(DaemonConnection _ state) identifier = do
  checkConnection connection
  entries <- readTVar (connectionLoads state)
  pending <- Interaction.hasActivePendingForSession (connectionInteractions state) identifier
  let readiness = entryReadiness (Map.findWithDefault emptyLoadEntry identifier entries)
  pure $ case readinessWorkingState readiness of
    Right _ | pending -> readiness {readinessWorkingState = Right (Just WorkingWaitingForToolConfirmation)}
    _ -> readiness

setSessionPreInit :: DaemonConnection -> Text -> Bool -> IO ()
setSessionPreInit connection@(DaemonConnection _ state) identifier preInit = atomically $ do
  checkConnection connection
  modifyTVar' (connectionLoads state) $ Map.alter (Just . update . fromMaybe emptyLoadEntry) identifier
  where
    update entry = entry {entryReadiness = (entryReadiness entry) {readinessPreInit = preInit}}

clearSessionNotFound :: DaemonConnection -> Text -> IO ()
clearSessionNotFound connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  modifyTVar' (connectionLoads state) (Map.adjust (\entry -> entry {entryReadiness = (entryReadiness entry) {readinessNotFound = False}}) identifier)

-- | Invalidate known readiness without loading, detaching or remote mutation.
-- Any older in-flight receipt loses its authority to publish state.
markSessionNotLoaded :: DaemonConnection -> Text -> IO ()
markSessionNotLoaded connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  invalidateSessionLoad state identifier False

invalidateSessionLoad :: DaemonContext -> Text -> Bool -> STM ()
invalidateSessionLoad state = invalidateStoredSessionLoad (connectionLoads state)

invalidateStoredSessionLoad :: TVar (Map Text SessionLoadEntry) -> Text -> Bool -> STM ()
invalidateStoredSessionLoad stored identifier removeKnown = do
  entries <- readTVar stored
  forM_ (Map.lookup identifier entries) $ \entry -> do
    forM_ (entryFlight entry) (\ticket -> void (tryPutTMVar ticket (Left (toException DaemonLoadSuperseded))))
    let previous = entryReadiness entry
        next = entry {entryEpoch = entryEpoch entry + 1, entryFlight = Nothing, entryRestoredRequests = mempty, entryLoadPolicy = if removeKnown then Nothing else entryLoadPolicy entry, entryReadiness = previous {readinessPhase = SessionNotLoaded, readinessLoading = False, readinessKnown = not removeKnown && readinessKnown previous, readinessPreInit = not removeKnown && readinessPreInit previous, readinessWorkingState = Right Nothing}}
    writeTVar stored (Map.insert identifier next entries)

startSessionLoad :: DaemonContext -> Text -> STM (Integer, SessionLoadTicket, DaemonLoadPolicy)
startSessionLoad state identifier = startSessionLoadForMachine state identifier (connectionDefaultMachineId state)

startSessionLoadForMachine :: DaemonContext -> Text -> Text -> STM (Integer, SessionLoadTicket, DaemonLoadPolicy)
startSessionLoadForMachine state identifier machine = do
  entries <- readTVar (connectionLoads state)
  let previous = Map.findWithDefault emptyLoadEntry identifier entries
      epoch = entryEpoch previous + 1
      readiness = entryReadiness previous
  forM_ (entryFlight previous) (\ticket -> void (tryPutTMVar ticket (Left (toException DaemonLoadSuperseded))))
  ticket <- newEmptyTMVar
  let next = previous {entryEpoch = epoch, entryFlight = Just ticket, entryCreationOwner = Nothing, entryRestoredRequests = mempty, entryReadiness = readiness {readinessPhase = if readinessPhase readiness == SessionLoaded then SessionLoaded else SessionLoading, readinessKnown = True, readinessLoading = True, readinessNotFound = False}}
  writeTVar (connectionLoads state) (Map.insert identifier next entries)
  modifySessionState (connectionSessionStates state) identifier (SessionState.setChildLoadError Nothing)
  rememberChildOrderForMachine state identifier machine
  void (pruneRegisteredSessions state)
  pure (epoch, ticket, fromMaybe (connectionLoadDefaults state) (entryLoadPolicy previous))

-- Explicit reloads supersede previous generations. Readiness checks instead
-- join an existing flight and never speculate about an unknown session.
loadSessionInfo :: DaemonConnection -> Text -> IO DaemonSessionInfo
loadSessionInfo connection identifier = trackSessionLoad connection identifier (performSessionReload connection identifier)

-- | Explicit fields replace retained intent; omissions preserve it. Admission
-- and policy selection share the existing load generation transaction.
loadSessionInfoWithConfiguration :: DaemonConnection -> Text -> Configuration.SessionLoadConfiguration -> Configuration.DaemonLoadConfiguration -> IO DaemonSessionInfo
loadSessionInfoWithConfiguration connection@(DaemonConnection core _) identifier config spawn = do
  let (params, _) = daemonLoadParameters identifier "" (Core.connectionMcpOptions core) True (config, spawn)
  either throwIO pure (Configuration.validateDaemonLoadSessionParams params)
  trackSessionLoadWithConfiguration (Just (config, spawn)) connection identifier (performSessionReload connection identifier)

ensureSessionLoaded :: DaemonConnection -> Text -> IO ()
ensureSessionLoaded connection@(DaemonConnection _ state) identifier = mask $ \restore -> do
  selected <- atomically $ do
    checkConnection connection
    entries <- readTVar (connectionLoads state)
    let entry = Map.findWithDefault emptyLoadEntry identifier entries
    if not (readinessKnown (entryReadiness entry))
      then pure Nothing
      else case entryFlight entry of
        Just ticket -> pure (Just (Right ticket))
        Nothing | readinessPhase (entryReadiness entry) == SessionLoaded -> pure Nothing
        Nothing -> Just . Left <$> startSessionLoad state identifier
  forM_ selected $ \case
    Right ticket -> void (restore (atomically (checkConnection connection >> readTMVar ticket)) >>= either throwIO pure)
    Left (epoch, ticket, policy) -> void (finishSessionLoad state identifier epoch ticket (restore (performSessionReload connection identifier (sessionLoadGuard state identifier epoch policy))))

trackSessionLoad :: DaemonConnection -> Text -> (SessionLoadGuard -> IO DaemonSessionInfo) -> IO DaemonSessionInfo
trackSessionLoad = trackSessionLoadWithConfiguration Nothing

trackSessionLoadWithConfiguration :: Maybe DaemonLoadPolicy -> DaemonConnection -> Text -> (SessionLoadGuard -> IO DaemonSessionInfo) -> IO DaemonSessionInfo
trackSessionLoadWithConfiguration = trackSessionLoadWithCreation Nothing

trackSessionLoadWithCreation :: Maybe (Unique, Text) -> Maybe DaemonLoadPolicy -> DaemonConnection -> Text -> (SessionLoadGuard -> IO DaemonSessionInfo) -> IO DaemonSessionInfo
trackSessionLoadWithCreation creation configured connection@(DaemonConnection _ state) identifier action = mask $ \restore -> do
  (existed, previousPolicy, epoch, ticket, policy) <- atomically $ do
    checkConnection connection
    existed <- (identifier `elem`) . cachedSessionIds <$> readTVar (connectionSessionCache state)
    previousPolicy <- (Map.lookup identifier >=> entryLoadPolicy) <$> readTVar (connectionLoads state)
    forM_ configured $ \new ->
      modifyTVar' (connectionLoads state) $
        Map.alter
          ( \previous ->
              let entry = fromMaybe emptyLoadEntry previous
                  old = fromMaybe (connectionLoadDefaults state) (entryLoadPolicy entry)
               in Just (entry {entryLoadPolicy = Just (mergeLoadPolicy old new)})
          )
          identifier
    (epoch, ticket, policy) <- startSessionLoadForMachine state identifier (maybe (connectionDefaultMachineId state) snd creation)
    unless existed $ forM_ creation $ \(token, _) ->
      modifyTVar' (connectionLoads state) (Map.adjust (\entry -> entry {entryCreationOwner = Just token}) identifier)
    pure (existed, previousPolicy, epoch, ticket, policy)
  finishSessionLoad state identifier epoch ticket (restore (action (sessionLoadGuard state identifier epoch policy)))
    `onException` forM_
      creation
      ( \(token, _) -> unless existed $ atomically $ do
          entries <- readTVar (connectionLoads state)
          let current = maybe False ((== Just token) . entryCreationOwner) (Map.lookup identifier entries)
          when current $ do
            forgetCachedEntry state identifier
            modifyTVar' (connectionLoads state) (Map.adjust (\entry -> entry {entryLoadPolicy = previousPolicy}) identifier)
      )

finishSessionLoad :: DaemonContext -> Text -> Integer -> SessionLoadTicket -> IO DaemonSessionInfo -> IO DaemonSessionInfo
finishSessionLoad state identifier epoch ticket action = do
  outcome <- try @SomeException action
  current <- atomically $ do
    entries <- readTVar (connectionLoads state)
    case Map.lookup identifier entries of
      Just entry | entryEpoch entry == epoch -> do
        let readiness = entryReadiness entry
            missing = case outcome of Left cause -> isMissingSession cause; Right _ -> False
            nextReadiness = case outcome of
              Right _ -> readiness {readinessPhase = SessionLoaded, readinessLoading = False, readinessNotFound = False}
              Left _ -> readiness {readinessPhase = SessionNotLoaded, readinessLoading = False, readinessKnown = not missing, readinessNotFound = missing, readinessWorkingState = Right Nothing}
            shared = case outcome of
              Left cause | Just (_ :: SomeAsyncException) <- fromException cause -> Left (toException DaemonLoadInterrupted)
              _ -> outcome
        let creationOwner = case outcome of Right _ -> Nothing; Left _ -> entryCreationOwner entry
        writeTVar (connectionLoads state) (Map.insert identifier (entry {entryFlight = Nothing, entryReadiness = nextReadiness, entryCreationOwner = creationOwner}) entries)
        let failure = case outcome of
              Right _ -> Nothing
              Left cause
                | missing -> Just SessionState.ChildLoadNotFound
                | Just (_ :: SomeAsyncException) <- fromException cause -> Just SessionState.ChildLoadInterrupted
                | otherwise -> Just SessionState.ChildLoadFailed
        modifySessionState (connectionSessionStates state) identifier (SessionState.setChildLoadError failure)
        void (tryPutTMVar ticket shared)
        void (pruneRegisteredSessions state)
        pure True
      _ -> void (tryPutTMVar ticket (Left (toException DaemonLoadSuperseded))) >> pure False
  case outcome of
    Left cause -> throwIO cause
    Right info -> if current then pure info else throwIO DaemonLoadSuperseded

isMissingSession :: SomeException -> Bool
isMissingSession cause = case fromException cause of
  Just (RpcRemoteFailure failure) -> rpcErrorCode failure == RpcEntityNotFound
  _ -> False

data SessionLoadGuard = SessionLoadGuard
  { observeLoadReceipt :: UTCTime -> Text -> (DaemonSessionInfo, [RestoredSessionRequest]) -> STM Bool,
    loadStillCurrent :: STM Bool,
    restoredRequestCurrent :: Text -> STM Bool,
    awaitRestoredRequest :: RpcDispatcher -> Text -> STM Bool,
    loadPolicySnapshot :: DaemonLoadPolicy
  }

sessionLoadGuard :: DaemonContext -> Text -> Integer -> DaemonLoadPolicy -> SessionLoadGuard
sessionLoadGuard state identifier epoch = SessionLoadGuard (observeSessionLoad state identifier epoch) (current Nothing) (current . Just) ready
  where
    eligible request entry = entryEpoch entry == epoch && maybe True (\key -> case Map.lookup key (entryRestoredRequests entry) of Just (method, _) -> Set.notMember (method, key) (entryCompletedRequests entry); Nothing -> False) request
    current request = maybe False (eligible request) . Map.lookup identifier <$> readTVar (connectionLoads state)
    ready dispatcher request = do
      entries <- readTVar (connectionLoads state)
      case Map.lookup identifier entries of
        Just entry | eligible (Just request) entry -> do
          when (Set.member request (entryRetiringRequests entry)) $ do
            busy <- rpcRequestActive dispatcher request
            check (not busy)
            writeTVar (connectionLoads state) (Map.insert identifier (entry {entryRetiringRequests = Set.delete request (entryRetiringRequests entry)}) entries)
          pure True
        _ -> pure False

observeSessionLoad :: DaemonContext -> Text -> Integer -> UTCTime -> Text -> (DaemonSessionInfo, [RestoredSessionRequest]) -> STM Bool
observeSessionLoad state identifier epoch receivedAt _ (info, restored) = do
  entries <- readTVar (connectionLoads state)
  case Map.lookup identifier entries of
    Just entry | entryEpoch entry == epoch -> do
      retiring <- Interaction.inactivePendingRequestIds (connectionInteractions state) identifier
      let readiness = (entryReadiness entry) {readinessWorkingState = Right (Just (snapshotWorkingState info))}
          requests = Map.fromListWith (\_ earlier -> earlier) [(requestId, (method, fields)) | (method, requestId, fields) <- restored]
      writeTVar (connectionLoads state) (Map.insert identifier (entry {entryReadiness = readiness, entryRestoredRequests = requests, entryRetiringRequests = Set.union (Set.fromList retiring) (entryRetiringRequests entry)}) entries)
      Interaction.clearInactivePendingForSession (connectionInteractions state) identifier
      modifySessionState (connectionSessionStates state) identifier (SessionState.observeWorkingDirectory (daemonInitialWorkingDirectory info))
      forM_ (daemonLoadedState info) $ \snapshot -> do
        let history = sessionMessages (loadedSessionSnapshot snapshot)
            observedAt = realToFrac (utcTimeToPOSIXSeconds receivedAt) * 1000
            reconcile = SessionState.reconcileDaemonQueueAt observedAt history (fromMaybe [] (loadedQueuedMessages snapshot)) (loadedAgentLoopInProgress snapshot == Just True)
            parent = loadedCallingSessionId snapshot >>= \value -> if value == identifier then Nothing else Just value
        modifySessionState (connectionSessionStates state) identifier (SessionState.setCallingMetadata parent (loadedCallingToolUseId snapshot) . SessionState.mergeLoadedMessages history . reconcile)
        states <- readTVar (connectionSessionStates state)
        forM_ (Map.lookup identifier states >>= SessionState.sessionCallingSessionId) $ \_ -> rememberChildOrder state identifier
        forM_ (loadedSubagentInvocations snapshot) (hydrateInvocationSummaries state)
      pure True
    _ -> pure False

snapshotWorkingState :: DaemonSessionInfo -> DroidWorkingState
snapshotWorkingState info = case daemonLoadedState info of
  Nothing -> WorkingIdle
  Just snapshot
    | any pending ["pendingPermissions", "pendingAskUserRequests"] -> WorkingWaitingForToolConfirmation
    | Just working <- loadedWorkingState snapshot, working /= WorkingIdle -> working
    | loadedAgentLoopInProgress snapshot == Just True -> WorkingStreamingAssistantMessage
    | otherwise -> WorkingIdle
    where
      pending key = case KeyMap.lookup key (loadedSessionAdditionalFields snapshot) of Just (Array values) -> not (null values); _ -> False

daemonLoadParameters :: Text -> Text -> McpSessionOptions -> Bool -> DaemonLoadPolicy -> (Configuration.DaemonLoadSessionParams, Maybe UpdateSessionSettingsParams)
daemonLoadParameters identifier token mcp reject (config, spawn) =
  let (params, patch) = Configuration.prepareLoadSessionParams identifier mcp reject (config {Configuration.loadAllMessages = Configuration.loadAllMessages config <|> Just True})
   in (Configuration.DaemonLoadSessionParams params token spawn, patch)

performSessionReload :: DaemonConnection -> Text -> SessionLoadGuard -> IO DaemonSessionInfo
performSessionReload owned@(DaemonConnection connection state) identifier guard = Core.sessionBoundary connection $ do
  reject <- atomically $ do
    bindings <- readTVar (connectionBindings state)
    handlers <- maybe (pure defaultDroidHandlers) (readTVar . bindingHandlers) (Map.lookup identifier bindings)
    authenticated <- readTVar (connectionAuthenticated state)
    enabled <- Interaction.permissionRequestsEnabled (connectionInteractions state) handlers
    pure (not (authenticated && enabled))
  let admit = do
        checkConnection owned
        current <- loadStillCurrent guard
        unless current (throwSTM DaemonLoadSuperseded)
  atomically admit
  credential <- join (readTVarIO (connectionCredential state))
  let (params, patch) = daemonLoadParameters identifier credential (Core.connectionMcpOptions connection) reject (loadPolicySnapshot guard)
  either throwIO pure (Configuration.validateDaemonLoadSessionParams params)
  (_, (info, snapshot)) <-
    withSessionReadyGate state identifier $
      Core.callSettingsResultObservedWithAdmission
        admit
        Nothing
        connection
        (Just identifier)
        "daemon.load_session"
        (Configuration.daemonLoadSessionFields params)
        (attachmentReceipt (connectionIdentity state) Nothing)
        (observeLoadReceipt guard)
  forM_ patch $ \update -> do
    fields <- either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (toJSON update))
    Core.connectionRequest connection 30000000 $ \rpc options ->
      void $
        awaitDaemonCommand
          (Proxy @SettingsUpdated)
          "settings_updated"
          (Core.connectionDispatcher connection)
          options
          identifier
          (Client.callWithAdmission admit SkipBeforeRequest (Proxy @(WithEnvelope (MethodRequest "daemon.update_session_settings" Object))) rpc options (KeyMap.insert "sessionId" (String identifier) fields) :: IO EmptyObject)
  restorePending guard owned identifier snapshot
  when (connectionRestoreTerminals state) (void (loadTerminalsWithGuard (loadStillCurrent guard) owned identifier))
  pure info

getMcpConfig :: DaemonConnection -> IO GetMcpConfigResult
getMcpConfig connection = connectionOperation connection Client.getDaemonMcpConfig mempty

updateMcpConfig :: DaemonConnection -> UpdateMcpConfigParams -> IO UpdateMcpConfigResult
updateMcpConfig connection params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  connectionOperation connection Client.updateDaemonMcpConfig validated

-- | Read defaults from the daemon, without loading a session or a local file.
getDefaultSettings :: DaemonConnection -> IO DefaultSettings
getDefaultSettings connection = connectionOperation connection Client.getDaemonDefaultSettings mempty

-- | Preserve the success flag and returned defaults. No optimistic update or
-- local inheritance calculation is performed; cancellation does not undo writes.
updateSessionDefaults :: DaemonConnection -> UpdateSessionDefaultsParams -> IO UpdateSessionDefaultsResult
updateSessionDefaults connection = connectionOperation connection Client.updateDaemonSessionDefaults

-- | Global daemon configuration, not a local model catalog or settings owner.
listCustomModels :: DaemonConnection -> IO ListCustomModelsResult
listCustomModels connection = connectionOperation connection Client.listDaemonCustomModels mempty

upsertCustomModel :: DaemonConnection -> UpsertCustomModelParams -> IO UpdateCustomModelsResult
upsertCustomModel connection = connectionOperation connection Client.upsertDaemonCustomModel

deleteCustomModel :: DaemonConnection -> DeleteCustomModelParams -> IO UpdateCustomModelsResult
deleteCustomModel connection = connectionOperation connection Client.deleteDaemonCustomModel

-- | Existing-automation lifecycle calls. Returned prompts, HTML, paths and
-- scaffold data are not executed, rendered or written by the SDK.
listAutomations :: DaemonConnection -> Maybe Text -> IO ListAutomationsResult
listAutomations connection basePath = connectionOperation connection Client.listDaemonAutomations (AutomationListParams basePath mempty)

runAutomation :: DaemonConnection -> RunAutomationParams -> IO RunAutomationResult
runAutomation connection = connectionOperation connection Client.runDaemonAutomation

pauseAutomation :: DaemonConnection -> AutomationAddress -> IO AutomationStatusResult
pauseAutomation connection = connectionOperation connection Client.pauseDaemonAutomation

resumeAutomation :: DaemonConnection -> AutomationAddress -> IO AutomationStatusResult
resumeAutomation connection = connectionOperation connection Client.resumeDaemonAutomation

getAutomationHistory :: DaemonConnection -> AutomationHistoryParams -> IO AutomationHistoryResult
getAutomationHistory connection = connectionOperation connection Client.getDaemonAutomationHistory

getAutomationVisual :: DaemonConnection -> AutomationVisualParams -> IO AutomationVisualResult
getAutomationVisual connection = connectionOperation connection Client.getDaemonAutomationVisual

renameAutomation :: DaemonConnection -> RenameAutomationParams -> IO SuccessOrErrorResult
renameAutomation connection = connectionOperation connection Client.renameDaemonAutomation

deleteAutomation :: DaemonConnection -> AutomationAddress -> IO SuccessOrErrorResult
deleteAutomation connection = connectionOperation connection Client.deleteDaemonAutomation

-- | Explicit remote creation/configuration. No local scaffolding, scheduler,
-- privacy-policy inference or automatic execution is performed by the SDK.
createAutomation :: DaemonConnection -> CreateAutomationParams -> IO AutomationCreationResult
createAutomation connection = connectionOperation connection Client.createDaemonAutomation

forkAutomation :: DaemonConnection -> ForkAutomationParams -> IO AutomationCreationResult
forkAutomation connection = connectionOperation connection Client.forkDaemonAutomation

updateAutomationModel :: DaemonConnection -> UpdateAutomationModelParams -> IO SuccessOrErrorResult
updateAutomationModel connection = connectionOperation connection Client.updateDaemonAutomationModel

updateAutomationPrivacy :: DaemonConnection -> UpdateAutomationPrivacyParams -> IO SuccessOrErrorResult
updateAutomationPrivacy connection = connectionOperation connection Client.updateDaemonAutomationPrivacy

updateAutomationPrompt :: DaemonConnection -> UpdateAutomationPromptParams -> IO SuccessOrErrorResult
updateAutomationPrompt connection = connectionOperation connection Client.updateDaemonAutomationPrompt

updateAutomationSchedule :: DaemonConnection -> UpdateAutomationScheduleParams -> IO SuccessOrErrorResult
updateAutomationSchedule connection = connectionOperation connection Client.updateDaemonAutomationSchedule

applyAutomationConfig :: DaemonConnection -> ApplyAutomationConfigParams -> IO ApplyAutomationConfigResult
applyAutomationConfig connection = connectionOperation connection Client.applyDaemonAutomationConfig

updateAutomation :: DaemonConnection -> UpdateAutomationParams -> IO SuccessOrErrorResult
updateAutomation connection = connectionOperation connection Client.updateDaemonAutomation

-- | Git/provider work is requested from the daemon, never run in the SDK's cwd.
-- Checkout resolution and lookup invalidation are explicit caller choices.
listGitBranches :: DaemonConnection -> Text -> IO ListGitBranchesResult
listGitBranches connection cwd = connectionOperation connection Client.listDaemonGitBranches (GitDirectoryParams cwd mempty)

checkoutGitBranch :: DaemonConnection -> CheckoutBranchParams -> IO CheckoutBranchResult
checkoutGitBranch connection = connectionOperation connection Client.checkoutDaemonGitBranch

-- | Query daemon-managed profiles without reading local files. Returned names
-- and content receive SDK normalization; raw wire codecs remain available.
listWorktreeSetupProfiles :: DaemonConnection -> ListWorktreeProfilesParams -> IO ListWorktreeProfilesResult
listWorktreeSetupProfiles connection = connectionOperation connection Client.listDaemonWorktreeSetupProfiles

-- | Explicit remote profile save. Validation precedes request admission; this
-- does not run its scripts, grant repository trust or create a worktree.
saveWorktreeSetupProfile :: DaemonConnection -> SaveWorktreeProfileParams -> IO SaveWorktreeProfileResult
saveWorktreeSetupProfile connection = connectionOperation connection Client.saveDaemonWorktreeSetupProfile

deleteWorktreeSetupProfile :: DaemonConnection -> DeleteWorktreeProfileParams -> IO SuccessResult
deleteWorktreeSetupProfile connection = connectionOperation connection Client.deleteDaemonWorktreeSetupProfile

-- | Nothing in ListManagedWorktreesParams leaves includeSizes absent.
listManagedWorktrees :: DaemonConnection -> ListManagedWorktreesParams -> IO ListManagedWorktreesResult
listManagedWorktrees connection = connectionOperation connection Client.listDaemonManagedWorktrees

-- | Explicit remote cleanup with a three-minute request budget. Flags are
-- never escalated and partial-success warnings remain in the returned report.
cleanupWorktree :: DaemonConnection -> CleanupWorktreeParams -> IO CleanupWorktreeResult
cleanupWorktree connection = connectionOperationWithTimeout 180000000 connection Client.cleanupDaemonWorktree

-- | Inspect a prospective deletion; no cleanup or branch deletion is sent.
inspectWorktreeDeletion :: DaemonConnection -> InspectWorktreeDeletionParams -> IO InspectWorktreeDeletionResult
inspectWorktreeDeletion connection = connectionOperation connection Client.inspectDaemonWorktreeDeletion

getGitBranchDivergence :: DaemonConnection -> GitBranchParams -> IO GitBranchDivergence
getGitBranchDivergence connection = connectionOperation connection Client.getDaemonGitBranchDivergence

getGitDiff :: DaemonConnection -> GitDiffParams -> IO GitDiffResult
getGitDiff connection = connectionOperation connection Client.getDaemonGitDiff

resolvePullRequestStatuses :: DaemonConnection -> ResolvePullRequestStatusesParams -> IO ResolvePullRequestStatusesResult
resolvePullRequestStatuses connection = connectionOperation connection Client.resolveDaemonPullRequestStatuses

-- | Readiness inspection does not acknowledge warnings or grant folder trust.
inspectMissionReadiness :: DaemonConnection -> Text -> IO MissionReadinessResult
inspectMissionReadiness connection cwd = connectionOperation connection Client.inspectDaemonMissionReadiness (GitDirectoryParams cwd mempty)

acknowledgeMissionReadinessWarning :: DaemonConnection -> Text -> IO Object
acknowledgeMissionReadinessWarning connection cwd = connectionOperation connection Client.acknowledgeDaemonMissionReadinessWarning (GitDirectoryParams cwd mempty)

-- | Explicit daemon-side publication/mutation. No local Git or provider CLI is
-- invoked, and cancellation cannot undo remote work.
gitPush :: DaemonConnection -> Text -> IO SuccessResult
gitPush connection identifier = connectionOperation connection Client.pushDaemonGit (SessionIdParams identifier mempty)

gitCommit :: DaemonConnection -> GitCommitParams -> IO SuccessResult
gitCommit connection = connectionOperation connection Client.commitDaemonGit

createPullRequest :: DaemonConnection -> CreatePullRequestParams -> IO CreatePullRequestResult
createPullRequest connection = connectionOperation connection Client.createDaemonPullRequest

getSemanticDiffCache :: DaemonConnection -> SemanticDiffTarget -> IO SemanticDiffCacheResult
getSemanticDiffCache connection = connectionOperation connection Client.getDaemonSemanticDiffCache

saveSemanticDiffCache :: DaemonConnection -> SaveSemanticDiffParams -> IO SuccessResult
saveSemanticDiffCache connection = connectionOperation connection Client.saveDaemonSemanticDiffCache

-- | This may request model-backed work from the daemon. The returned content
-- and session ID are data, not a newly owned SDK session or an automatic save.
generateSemanticDiff :: DaemonConnection -> GenerateSemanticDiffParams -> IO GenerateSemanticDiffResult
generateSemanticDiff connection = connectionOperationWithTimeout 180000000 connection Client.generateDaemonSemanticDiff

-- | Connection-level control of explicit remote targets. Successor IDs are
-- data: no automatic load, local replacement transition or file operation.
getRewindInfo :: DaemonConnection -> GetRewindInfoParams -> IO GetRewindInfoResult
getRewindInfo connection = connectionOperation connection Client.getDaemonRewindInfo

executeRewind :: DaemonConnection -> ExecuteRewindParams -> IO ExecuteRewindResult
executeRewind connection = connectionOperation connection Client.executeDaemonRewind

-- | The baseline compaction budget is 240 seconds; cancellation only stops
-- waiting and cannot roll back work already admitted by the daemon.
compactSession :: DaemonConnection -> Text -> CompactSessionParams -> IO CompactSessionResult
compactSession connection identifier = connectionOperationWithTimeout 240000000 connection (\channel configured -> Client.compactDaemonSession channel configured identifier)

forkSession :: DaemonConnection -> Text -> ForkSessionParams -> IO ForkSessionResult
forkSession connection identifier = connectionOperation connection (\channel configured -> Client.forkDaemonSession channel configured identifier)

killWorkerSession :: DaemonConnection -> Text -> KillWorkerSessionParams -> IO Object
killWorkerSession connection identifier = connectionOperation connection (\channel configured -> Client.killDaemonWorkerSession channel configured identifier)

-- | Explicit remote close, not connection cleanup. Existing owned-session
-- lifecycle notifications still apply; scope exit does not send this request.
closeSession :: DaemonConnection -> DaemonCloseSessionParams -> IO Object
closeSession connection = connectionOperation connection Client.closeDaemonSession

-- | Explicit remote report submission, using only caller-supplied comment,
-- logs and source metadata. The SDK does not collect or sanitize diagnostics.
submitBugReport :: DaemonConnection -> Text -> SubmitBugReportParams -> IO SubmitBugReportResult
submitBugReport connection identifier = connectionOperation connection (\channel configured -> Client.submitDaemonBugReport channel configured identifier)

-- | Explicit remote logout acknowledgment, not connection or scope cleanup.
-- Revocation and any resulting disconnection belong to the daemon.
logout :: DaemonConnection -> IO CommandAck
logout (DaemonConnection core state) =
  Core.connectionBoundary core 30000000 $
    Core.connectionRequest core 30000000 $ \rpc options ->
      Client.callObserved (Proxy @(WithEnvelope (MethodRequest "daemon.logout" EmptyObject))) rpc options mempty $ \(_ :: CommandAck) -> revokeAuthentication state

-- | Explicit cron operations. The daemon owns scheduling and remote effects;
-- replies do not create locally owned sessions, timers or retry/rollback work.
listCrons :: DaemonConnection -> ListCronsParams -> IO ListCronsResult
listCrons connection = connectionOperation connection Client.listDaemonCrons

createCron :: DaemonConnection -> CreateCronParams -> IO CreateCronResult
createCron connection = connectionOperation connection Client.createDaemonCron

updateCron :: DaemonConnection -> UpdateCronParams -> IO UpdateCronResult
updateCron connection = connectionOperation connection Client.updateDaemonCron

deleteCron :: DaemonConnection -> DeleteCronParams -> IO DeleteCronResult
deleteCron connection = connectionOperation connection Client.deleteDaemonCron

holdSessionCrons :: DaemonConnection -> HoldSessionCronsParams -> IO HoldSessionCronsResult
holdSessionCrons connection = connectionOperation connection Client.holdDaemonSessionCrons

resumeSessionCrons :: DaemonConnection -> SessionIdParams -> IO ResumeSessionCronsResult
resumeSessionCrons connection = connectionOperation connection Client.resumeDaemonSessionCrons

onCronStateChanged :: DaemonConnection -> (Either DaemonEventError CronStateChanged -> IO ()) -> IO (IO ())
onCronStateChanged = connectionNotification "daemon.cron.state_changed"

-- | Org-scoped Software Factory requests through the existing daemon.
-- Returned rows/counts do not imply local ownership, execution or cached state.
sfListWorkstreams :: DaemonConnection -> SfListWorkstreamsParams -> IO SfListWorkstreamsResult
sfListWorkstreams connection = connectionOperation connection Client.listDaemonSfWorkstreams

sfGetWorkstream :: DaemonConnection -> SfWorkstreamTarget -> IO SfGetWorkstreamResult
sfGetWorkstream connection = connectionOperation connection Client.getDaemonSfWorkstream

sfCreateWorkstream :: DaemonConnection -> SfCreateWorkstreamParams -> IO SfWorkstreamResult
sfCreateWorkstream connection = connectionOperation connection Client.createDaemonSfWorkstream

sfUpdateWorkstream :: DaemonConnection -> SfUpdateWorkstreamParams -> IO SfWorkstreamResult
sfUpdateWorkstream connection = connectionOperation connection Client.updateDaemonSfWorkstream

sfDeleteWorkstream :: DaemonConnection -> SfWorkstreamTarget -> IO SfDeleteWorkstreamResult
sfDeleteWorkstream connection = connectionOperation connection Client.deleteDaemonSfWorkstream

-- | Explicit publish with caller-supplied optimistic generation. A conflict
-- remains the original RPC error; the SDK does not fetch, retry or force it.
sfPublishWorkstreamContent :: DaemonConnection -> SfPublishWorkstreamContentParams -> IO SfWorkstreamContentResult
sfPublishWorkstreamContent connection = connectionOperation connection Client.publishDaemonSfWorkstreamContent

sfHydrateWorkstreamContent :: DaemonConnection -> SfHydrateWorkstreamContentParams -> IO SfWorkstreamContentResult
sfHydrateWorkstreamContent connection = connectionOperation connection Client.hydrateDaemonSfWorkstreamContent

sfListSignals :: DaemonConnection -> SfListSignalsParams -> IO SfListSignalsResult
sfListSignals connection = connectionOperation connection Client.listDaemonSfSignals

sfListChanges :: DaemonConnection -> SfListChangesParams -> IO SfListChangesResult
sfListChanges connection = connectionOperation connection Client.listDaemonSfChanges

sfListActivities :: DaemonConnection -> SfListActivitiesParams -> IO SfListActivitiesResult
sfListActivities connection = connectionOperation connection Client.listDaemonSfActivities

sfResolveActivityReview :: DaemonConnection -> SfResolveActivityReviewParams -> IO SfResolveActivityReviewResult
sfResolveActivityReview connection = connectionOperation connection Client.resolveDaemonSfActivityReview

sfListEvents :: DaemonConnection -> SfListEventsParams -> IO SfListEventsResult
sfListEvents connection = connectionOperation connection Client.listDaemonSfEvents

sfMarkEventsRead :: DaemonConnection -> SfMarkEventsReadParams -> IO SfMarkedEventsResult
sfMarkEventsRead connection = connectionOperation connection Client.markDaemonSfEventsRead

sfMarkEventsUnread :: DaemonConnection -> SfMarkEventsUnreadParams -> IO SfMarkedEventsResult
sfMarkEventsUnread connection = connectionOperation connection Client.markDaemonSfEventsUnread

-- | Explicit daemon-management requests. These do not update local software,
-- install local keys, connect to the returned relay URL or cache proxy tokens.
triggerUpdate :: DaemonConnection -> IO TriggerUpdateResult
triggerUpdate connection = connectionOperation connection Client.triggerDaemonUpdate mempty

installSshKey :: DaemonConnection -> NonEmptyText -> IO InstallSshKeyResult
installSshKey connection key = connectionOperation connection Client.installDaemonSshKey (InstallSshKeyParams key mempty)

getProxyToken :: DaemonConnection -> IO ProxyTokenResult
getProxyToken (DaemonConnection connection _) = Core.connectionRequest connection 30000000 Client.getDaemonProxyToken

startRelay :: DaemonConnection -> IO RelayStartResult
startRelay connection = connectionOperation connection Client.startDaemonRelay mempty

stopRelay :: DaemonConnection -> IO RelayStopResult
stopRelay connection = connectionOperation connection Client.stopDaemonRelay mempty

getRelayStatus :: DaemonConnection -> IO RelayStatus
getRelayStatus connection = connectionOperation connection Client.getDaemonRelayStatus mempty

onRelayStatusChanged :: DaemonConnection -> (Either DaemonEventError RelayStatus -> IO ()) -> IO (IO ())
onRelayStatusChanged = connectionNotification "daemon.relay.status_changed"

-- | Resolve a daemon queue entry through the existing ACK-aware command path.
-- Publish into the current physical generation only. A retired receipt fails
-- closed; neither that failure nor another error undoes remote effects.
resolveQueuedUserMessage :: DaemonConnection -> Text -> ResolveQueuedMessageParams -> IO Object
resolveQueuedUserMessage owned@(DaemonConnection connection _) identifier params = mask $ \restore -> do
  atomically (checkConnection owned)
  result <- restore $ Core.connectionRequest connection 30000000 $ \channel options ->
    awaitDaemonCommand (Proxy @CreateMessage) "create_message" (Core.connectionDispatcher connection) options identifier (Client.resolveDaemonQueuedUserMessageRaw channel options identifier params)
  modifyConnectionState owned identifier (SessionState.applyQueueResolution params)
  pure (if KeyMap.lookup "accepted" result == Just (Bool True) then mempty else result)

-- | Local queue metadata; enqueueing does not submit a remote message.
queueUserMessage :: DaemonConnection -> Text -> SessionState.QueuedMessageKind -> QueuedUserMessage -> IO ()
queueUserMessage connection identifier kind message = do
  observedAt <- (* 1000) . realToFrac <$> getPOSIXTime
  queueUserMessages connection identifier [SessionState.QueueEntry message kind observedAt]

queueUserMessages :: DaemonConnection -> Text -> [SessionState.QueueEntry] -> IO ()
queueUserMessages connection identifier entries = transitionQueue connection identifier (\state -> (SessionState.enqueueMessages entries state, ()))

getQueuedMessages :: DaemonConnection -> Text -> IO [SessionState.QueueEntry]
getQueuedMessages connection identifier = SessionState.sessionQueue <$> getSessionState connection identifier

replaceDaemonQueuedMessages :: DaemonConnection -> Text -> [SessionState.QueueEntry] -> IO ()
replaceDaemonQueuedMessages connection identifier entries = transitionQueue connection identifier (\state -> (SessionState.replaceDaemonQueue entries state, ()))

dequeueQueuedMessage :: DaemonConnection -> Text -> Maybe SessionState.QueuedMessageKind -> IO (Maybe SessionState.QueueEntry)
dequeueQueuedMessage connection identifier kind = transitionQueue connection identifier (SessionState.dequeueQueuedMessage kind)

dequeueQueuedMessages :: DaemonConnection -> Text -> Maybe [SessionState.QueuedMessageKind] -> IO [SessionState.QueueEntry]
dequeueQueuedMessages connection identifier kinds = transitionQueue connection identifier (SessionState.dequeueQueuedMessages kinds)

restoreQueuedMessages :: DaemonConnection -> Text -> [SessionState.QueueEntry] -> IO ()
restoreQueuedMessages connection identifier entries = transitionQueue connection identifier (\state -> (SessionState.restoreQueueFront entries state, ()))

clearQueuedMessages :: DaemonConnection -> Text -> Maybe [SessionState.QueuedMessageKind] -> IO ()
clearQueuedMessages connection identifier kinds = transitionQueue connection identifier (\state -> (SessionState.clearQueuedMessages kinds state, ()))

markQueuedMessageProcessed :: DaemonConnection -> Text -> Text -> IO ()
markQueuedMessageProcessed connection identifier requestId = transitionQueue connection identifier (\state -> (SessionState.markQueuedMessageProcessed requestId state, ()))

-- | Pause/discard local queue observations only; no interrupt is sent.
pauseQueuedMessages :: DaemonConnection -> Text -> Maybe Text -> IO ()
pauseQueuedMessages connection identifier restoredId = transitionQueue connection identifier (\state -> (SessionState.pauseDaemonQueue restoredId state, ()))

transitionQueue :: DaemonConnection -> Text -> (SessionState.SessionState -> (SessionState.SessionState, a)) -> IO a
transitionQueue connection@(DaemonConnection _ state) identifier transition = atomically $ do
  checkConnection connection
  states <- readTVar (connectionSessionStates state)
  let (next, result) = transition (Map.findWithDefault SessionState.emptySessionState identifier states)
  writeTVar (connectionSessionStates state) (Map.insert identifier next states)
  pure result

-- | Explicitly send one locally deferred/paused entry through normal submission.
-- While its generation remains current, failure restores a paused entry unless
-- confirmation or a newer queue observation already won.
-- Daemon-backed entries must use resolveQueuedUserMessage, never resubmission.
sendQueuedUserMessage :: DaemonConnection -> Text -> Text -> IO Object
sendQueuedUserMessage owned@(DaemonConnection _ state) identifier requestId = mask $ \restore -> do
  generated <- UUID.toText <$> nextRandom
  entry <- atomically $ do
    checkConnection owned
    states <- readTVar (connectionSessionStates state)
    let current = Map.findWithDefault SessionState.emptySessionState identifier states
    queued <- maybe (throwSTM SessionState.QueuedEntryMissing) pure (SessionState.lookupQueuedMessage requestId current)
    when (SessionState.isDaemonQueuedMessage (SessionState.queueEntryKind queued)) (throwSTM SessionState.QueuedEntryAlreadyRemote)
    let message = SessionState.queueEntryMessage queued
        original = queuedMessageInput message
        input = original {userMessageId = Just (fromMaybe generated (userMessageId original))}
    writeTVar (connectionSessionStates state) (Map.insert identifier (SessionState.removeQueuedMessage requestId current) states)
    pure (queued {SessionState.queueEntryMessage = message {queuedMessageInput = input}})
  outcome <- try @SomeException (restore (submitUserMessage owned identifier requestId (queuedMessageInput (SessionState.queueEntryMessage entry))))
  case outcome of
    Right result -> pure result
    Left cause -> do
      atomically $ do
        currentGeneration <- connectionScopeOpen owned
        when currentGeneration $ modifySessionState (connectionSessionStates state) identifier $ \current ->
          if isJust (SessionState.lookupQueuedMessage requestId current)
            then current
            else SessionState.restoreQueueFront [entry {SessionState.queueEntryKind = SessionState.QueuePaused}] current
      throwIO cause

connectionOperation :: DaemonConnection -> (RpcChannel -> Client.CallOptions -> params -> IO a) -> params -> IO a
connectionOperation = connectionOperationWithTimeout 30000000

connectionOperationWithTimeout :: Int -> DaemonConnection -> (RpcChannel -> Client.CallOptions -> params -> IO a) -> params -> IO a
connectionOperationWithTimeout deadline (DaemonConnection connection _) operation params = Core.connectionRequest connection deadline (\channel options -> operation channel options params)

-- | Catalog rows and cursor values are wire data, not loaded session handles.
listOpenedSessions :: DaemonConnection -> ListOpenedSessionsParams -> IO ListOpenedSessionsResult
listOpenedSessions connection = connectionOperation connection Client.listDaemonOpenedSessions

listAvailableSessions :: DaemonConnection -> ListAvailableSessionsParams -> IO ListAvailableSessionsResult
listAvailableSessions connection = connectionOperation connection Client.listDaemonAvailableSessions

-- | Sessionless model discovery. ListModelsOptions Nothing mempty selects
-- the daemon's default without inventing an explicit includeDisabled value.
listModels :: DaemonConnection -> ListModelsOptions -> IO ListModelsResult
listModels connection = connectionOperation connection Client.listDaemonModels

getSessionMessages :: DaemonConnection -> GetSessionMessagesParams -> IO GetSessionMessagesResult
getSessionMessages connection = connectionOperation connection Client.getDaemonSessionMessages

searchSessions :: DaemonConnection -> SearchSessionsParams -> IO SearchSessionsResult
searchSessions connection = connectionOperation connection Client.searchDaemonSessions

-- | Explicitly request archival. False success and the returned timestamp are
-- preserved; no retry, local file mutation or session-handle retirement is added.
archiveSession :: DaemonConnection -> ArchiveSessionParams -> IO ArchiveSessionResult
archiveSession connection = connectionOperation connection Client.archiveDaemonSession

unarchiveSession :: DaemonConnection -> Text -> IO SuccessResult
unarchiveSession connection ident = connectionOperation connection Client.unarchiveDaemonSession (SessionIdParams ident mempty)

-- | Checking trust never grants it. Paths belong to the daemon host.
checkFolderTrust :: DaemonConnection -> Text -> IO CheckFolderTrustResult
checkFolderTrust connection path = connectionOperation connection Client.checkDaemonFolderTrust (FolderPathParams path mempty)

-- | An explicit remote trust mutation, never an automatic prerequisite.
trustFolder :: DaemonConnection -> Text -> IO TrustFolderResult
trustFolder connection path = connectionOperation connection Client.trustDaemonFolder (FolderPathParams path mempty)

validateWorkingDirectory :: DaemonConnection -> Text -> IO ValidateWorkingDirectoryResult
validateWorkingDirectory connection path = connectionOperation connection Client.validateDaemonWorkingDirectory (ChangeWorkingDirectoryParams path mempty)

changeWorkingDirectory :: DaemonConnection -> ChangeSessionWorkingDirectoryParams -> IO ChangeWorkingDirectoryResult
changeWorkingDirectory (DaemonConnection core state) params =
  Core.connectionBoundary core 30000000 $
    Core.connectionRequest core 30000000 $ \rpc options ->
      Client.callObserved (Proxy @(WithEnvelope (MethodRequest "daemon.change_working_directory" ChangeSessionWorkingDirectoryParams))) rpc options params $ \result ->
        modifySessionState (connectionSessionStates state) (changeDirectorySessionId params) (SessionState.observeWorkingDirectory (Just (changedResolvedPath result)))

-- | Last observed (or explicitly provisional) remote cwd, not the SDK process
-- directory or a trust decision. A malformed cwd event makes this read fail
-- until a validated load, change response or event repairs the observation.
getWorkingDirectory :: DaemonConnection -> Text -> IO (Maybe Text)
getWorkingDirectory connection identifier = do
  state <- getSessionState connection identifier
  case SessionState.sessionWorkingDirectory state of
    SessionState.WorkingDirectoryUnknown -> pure Nothing
    SessionState.WorkingDirectoryInherited path -> pure (Just path)
    SessionState.WorkingDirectoryReported path -> pure path
    SessionState.WorkingDirectoryInvalid _ -> throwIO Core.DroidInvalidEvent

listFiles :: DaemonConnection -> ListFilesParams -> IO ListFilesResult
listFiles connection = connectionOperation connection Client.listDaemonFiles

-- | Preserve the baselined controller's default of 50; explicit values win.
searchFiles :: DaemonConnection -> SearchFilesParams -> IO SearchFilesResult
searchFiles connection params = connectionOperation connection Client.searchDaemonFiles (params {searchFilesMaxResults = Just (fromMaybe 50 (searchFilesMaxResults params))})

getWorkspaceFileContent :: DaemonConnection -> GetWorkspaceFileContentParams -> IO GetWorkspaceFileContentResult
getWorkspaceFileContent connection = connectionOperation connection Client.getDaemonWorkspaceFileContent

-- | Explicit daemon-side write. The optional base fingerprint is opaque and
-- conflicts propagate without replay. Session identity resolves cwd only: no
-- live session worker is loaded and the SDK never writes its local filesystem.
writeWorkspaceFileContent :: DaemonConnection -> WriteWorkspaceFileContentParams -> IO WriteWorkspaceFileContentResult
writeWorkspaceFileContent connection = connectionOperation connection Client.writeDaemonWorkspaceFileContent

-- | Fifteen-minute RPC budget, matching the reference presigned-URL lifetime.
-- Cancelling the wait does not promise rollback of the daemon's transfer.
pushCwdFileToUrl :: DaemonConnection -> PushCwdFileToUrlParams -> IO PushCwdFileToUrlResult
pushCwdFileToUrl connection = connectionOperationWithTimeout 900000000 connection Client.pushDaemonCwdFileToUrl

pullUrlToCwdFile :: DaemonConnection -> PullUrlToCwdFileParams -> IO PullUrlToCwdFileResult
pullUrlToCwdFile connection = connectionOperationWithTimeout 900000000 connection Client.pullDaemonUrlToCwdFile

-- | Explicit session identity wins over base-record extensions. No local PTY or
-- terminal-store entry is created, and caller cwd/env are not overridden.
createTerminal :: DaemonConnection -> Text -> CreateTerminalParams -> IO CreateTerminalResult
createTerminal connection ident params = connectionOperation connection Client.createDaemonTerminal (DaemonCreateTerminalParams ident params)

writeTerminalData :: DaemonConnection -> Text -> WriteTerminalDataParams -> IO SuccessResult
writeTerminalData connection ident params = connectionOperation connection Client.writeDaemonTerminalData (DaemonWriteTerminalDataParams ident params)

resizeTerminal :: DaemonConnection -> Text -> ResizeTerminalParams -> IO SuccessResult
resizeTerminal connection ident params = connectionOperation connection Client.resizeDaemonTerminal (DaemonResizeTerminalParams ident params)

-- | Request remote closure explicitly. Connection cleanup does not call this,
-- and its acknowledgement is distinct from a terminal-exit notification.
closeTerminal :: DaemonConnection -> Text -> CloseTerminalParams -> IO SuccessResult
closeTerminal connection ident params = connectionOperation connection Client.closeDaemonTerminal (DaemonCloseTerminalParams ident params)

listTerminals :: DaemonConnection -> Text -> IO ListTerminalsResult
listTerminals connection ident = connectionOperation connection Client.listDaemonTerminals (SessionIdParams ident mempty)

-- | Restore listed terminals at the ordered reply boundary. This is distinct
-- from the read-only 'listTerminals' receipt and never creates a remote terminal.
loadTerminals :: DaemonConnection -> Text -> IO ListTerminalsResult
loadTerminals = loadTerminalsWithGuard (pure True)

loadTerminalsWithGuard :: STM Bool -> DaemonConnection -> Text -> IO ListTerminalsResult
loadTerminalsWithGuard current connection@(DaemonConnection core state) identifier = mask $ \restore -> do
  ticket <- atomically $ do
    checkConnection connection
    admitted <- current
    unless admitted (throwSTM DaemonLoadSuperseded)
    states <- readTVar (connectionSessionStates state)
    let (pending, next) = SessionState.beginTerminalRestoration (Map.findWithDefault SessionState.emptySessionState identifier states)
    writeTVar (connectionSessionStates state) (Map.insert identifier next states)
    pure pending
  result <- restore $
    Core.connectionBoundary core 30000000 $
      Core.connectionRequest core 30000000 $ \rpc options ->
        Client.callObserved (Proxy @(WithEnvelope (MethodRequest "daemon.list_terminals" SessionIdParams))) rpc options (SessionIdParams identifier mempty) $ \receipt -> do
          accepted <- current
          when accepted (modifySessionState (connectionSessionStates state) identifier (SessionState.restoreSessionTerminals ticket (Terminal.listedTerminals receipt)))
  accepted <- atomically $ do
    allowed <- current
    states <- readTVar (connectionSessionStates state)
    pure (allowed && maybe False (SessionState.terminalRestorationCurrent ticket) (Map.lookup identifier states))
  unless accepted (throwIO DaemonLoadSuperseded)
  pure result

modifyConnectionState :: DaemonConnection -> Text -> (SessionState.SessionState -> SessionState.SessionState) -> IO ()
modifyConnectionState connection@(DaemonConnection _ state) identifier change = atomically (checkConnection connection >> modifySessionState (connectionSessionStates state) identifier change)

addTerminal :: DaemonConnection -> Text -> SessionState.TerminalMetadata -> IO ()
addTerminal connection identifier = modifyConnectionState connection identifier . SessionState.addSessionTerminal

getTerminals :: DaemonConnection -> Text -> IO (Map Text SessionState.TerminalMetadata)
getTerminals connection identifier = SessionState.sessionTerminalsById <$> getSessionState connection identifier

removeTerminalFromStore :: DaemonConnection -> Text -> Text -> IO ()
removeTerminalFromStore connection identifier = modifyConnectionState connection identifier . SessionState.removeSessionTerminal

updateTerminalStatus :: DaemonConnection -> Text -> Text -> SessionState.TerminalStatus -> IO ()
updateTerminalStatus connection identifier terminal = modifyConnectionState connection identifier . SessionState.setSessionTerminalStatus terminal

getActiveTerminalId :: DaemonConnection -> Text -> IO (Maybe Text)
getActiveTerminalId connection identifier = SessionState.sessionActiveTerminalId <$> getSessionState connection identifier

setActiveTerminalId :: DaemonConnection -> Text -> Maybe Text -> IO ()
setActiveTerminalId connection identifier = modifyConnectionState connection identifier . SessionState.setActiveTerminalId

-- | Store an explicit snapshot, including its timestamp, without interpreting
-- escapes or changing remote terminal state.
storeTerminalState :: DaemonConnection -> Text -> Text -> SessionState.TerminalSerializedState -> IO ()
storeTerminalState connection identifier terminal = modifyConnectionState connection identifier . SessionState.storeTerminalSerializedState terminal

getTerminalSerializedState :: DaemonConnection -> Text -> Text -> IO (Maybe SessionState.TerminalSerializedState)
getTerminalSerializedState connection identifier terminal = SessionState.getTerminalSerializedState terminal <$> getSessionState connection identifier

getTerminalBufferedData :: DaemonConnection -> Text -> Text -> IO (Maybe Text)
getTerminalBufferedData connection identifier terminal = SessionState.getTerminalBufferedData terminal <$> getSessionState connection identifier

clearTerminalBufferedData :: DaemonConnection -> Text -> Text -> IO ()
clearTerminalBufferedData connection identifier = modifyConnectionState connection identifier . SessionState.clearTerminalBufferedData

clearTerminalRestorationState :: DaemonConnection -> Text -> Text -> IO ()
clearTerminalRestorationState connection identifier = modifyConnectionState connection identifier . SessionState.clearTerminalRestorationState

-- | An attachment-owned output sink, not a remote terminal-input writer.
-- Registration flushes buffered output on this caller's session lifetime; later
-- delivery is serial. Do not register or load from a notification callback.
-- Unsubscribe is token-specific; an already admitted write may finish.
registerTerminalWriteHandler :: DaemonSession -> Text -> (Text -> IO ()) -> IO (IO ())
registerTerminalWriteHandler session terminal handler =
  Core.withSessionUse (daemonCoreSession session) $ withMVar (bindingTerminalWriteLock binding) $ \_ -> mask $ \restore -> do
    token <- newUnique
    let stop = atomically (removeTerminalWriter binding terminal token)
    atomically (modifyTVar' (bindingTerminalWriters binding) (Map.insert terminal (token, handler)))
    restore (flushTerminalWriter (sessionConnection session) binding terminal Nothing) `onException` stop
    pure stop
  where
    binding = daemonSessionBinding session

removeTerminalWriter :: DaemonBinding -> Text -> Unique -> STM ()
removeTerminalWriter binding terminal token = modifyTVar' (bindingTerminalWriters binding) (Map.update (\(current, handler) -> if current == token then Nothing else Just (current, handler)) terminal)

flushTerminalWriter :: DaemonConnection -> DaemonBinding -> Text -> Maybe Text -> IO ()
flushTerminalWriter (DaemonConnection _ state) binding terminal immediate = mask $ \restore -> do
  let identifier = Core.droidSessionId (bindingSession binding)
  selected <- atomically $ do
    writer <- Map.lookup terminal <$> readTVar (bindingTerminalWriters binding)
    states <- readTVar (connectionSessionStates state)
    let snapshot = Map.lookup identifier states >>= SessionState.terminalBufferSnapshot terminal
        buffered = snapshot >>= SessionState.terminalBufferSnapshotText
        payload = if isJust immediate then buffered <|> immediate else buffered >>= \value -> if Text.null value then Nothing else Just value
    pure ((,,) <$> writer <*> pure snapshot <*> payload)
  forM_ selected $ \((token, handler), snapshot, payload) -> do
    outcome <- try @SomeException (restore (handler payload))
    atomically $ case outcome of
      Right () -> do
        forM_ snapshot (modifySessionState (connectionSessionStates state) identifier . SessionState.acknowledgeTerminalBuffer terminal)
        modifySessionState (connectionSessionStates state) identifier (\previous -> if SessionState.sessionTerminalError previous == Just (SessionState.TerminalWriterFailed terminal) then SessionState.clearTerminalError previous else previous)
      Left cause -> do
        removeTerminalWriter binding terminal token
        case fromException cause :: Maybe SomeAsyncException of
          Just _ -> pure ()
          Nothing -> modifySessionState (connectionSessionStates state) identifier (SessionState.invalidateTerminalState (SessionState.TerminalWriterFailed terminal))
    either throwIO pure outcome

data DaemonEventError = InvalidDaemonEvent | DaemonEventConnectionError !RpcChannelError
  deriving stock (Eq, Show)

-- | Observe archive changes for this connection, across session IDs. Callbacks
-- run serially on dispatcher intake; ordinary RPC queries are safe, but do not
-- wait for later notifications here. Unsubscribe is idempotent and stops later
-- admission, not an already-running callback. Connection cleanup joins delivery.
onArchiveStateChanged :: DaemonConnection -> (Either DaemonEventError SessionArchiveStateChanged -> IO ()) -> IO (IO ())
onArchiveStateChanged = connectionNotification "daemon.session.archive_state_changed"

-- | Connection-wide setup text; identical delivery/error/ownership rules to
-- 'onArchiveStateChanged'. It is progress data, not a command or completion.
onSetupStepProgress :: DaemonConnection -> (Either DaemonEventError SetupStepProgress -> IO ()) -> IO (IO ())
onSetupStepProgress = connectionNotification "daemon.session.setup_step_progress"

-- | Data and exits for all terminals in one session. Foreign-session payloads
-- are ignored before terminal decoding. Unrelated event kinds are not adapted.
-- Delivery, failure and unsubscribe semantics match 'onArchiveStateChanged'.
onTerminalEvent :: DaemonConnection -> Text -> (Either DaemonEventError TerminalNotification -> IO ()) -> IO (IO ())
onTerminalEvent connection ident = connectionNotificationWith "daemon.session_notification" decode connection
  where
    decode = withObject "terminal session notification" $ \fields -> do
      actual <- fields .: "sessionId"
      if actual /= ident
        then pure Nothing
        else do
          payload <- fields .: "notification" :: Parser Object
          kind <- payload .: "type" :: Parser Text
          if kind `elem` ["daemon.terminal_data", "daemon.terminal_exit"]
            then Just <$> parseJSON (Object payload)
            else pure Nothing

connectionNotification :: (FromJSON event) => Text -> DaemonConnection -> (Either DaemonEventError event -> IO ()) -> IO (IO ())
connectionNotification method = connectionNotificationWith method (fmap Just . parseJSON)

connectionNotificationWith :: Text -> (Value -> Parser (Maybe event)) -> DaemonConnection -> (Either DaemonEventError event -> IO ()) -> IO (IO ())
connectionNotificationWith method decode (DaemonConnection connection _) callback = mask_ $ do
  let dispatcher = Core.connectionDispatcher connection
      observe notification = when (baseNotificationMethod (envelopeBody notification) == method) $
        case baseNotificationParams (envelopeBody notification) of
          Just value -> case parseEither decode value of
            Left _ -> callback (Left InvalidDaemonEvent)
            Right event -> forM_ event (callback . Right)
          Nothing -> callback (Left InvalidDaemonEvent)
  stop <- onRpcNotification dispatcher observe
  stopError <- onRpcError dispatcher (callback . Left . DaemonEventConnectionError) `onException` stop
  pure (stopError >> stop)

withDaemonConnection :: DaemonOptions -> Bool -> McpSessionOptions -> (DaemonConnection -> IO a) -> IO a
withDaemonConnection options reject mcpOptions = withDaemonConnectionUsing (daemonClientOptions options) reject mcpOptions (withDaemonTransport options)

-- | Own RPC/dispatcher/session state over borrowed object I/O. No root session
-- or implicit MCP configuration mutation is created by this connection scope.
withConnectionOn :: DaemonClientOptions -> ObjectTransport -> (DaemonConnection -> IO a) -> IO a
withConnectionOn options transport = withDaemonConnectionUsing options True defaultMcpSessionOptions ($ transport)

-- | Retained logical observations over borrowed I/O. The caller still owns the
-- transport; each generation has its own channel, dispatcher and handle leases.
withConnectionStateOn :: DaemonState -> DaemonClientOptions -> ObjectTransport -> (DaemonConnection -> IO a) -> IO a
withConnectionStateOn shared options transport = withDaemonConnectionStateUsing shared options True defaultMcpSessionOptions ($ transport)

withDaemonTransport :: DaemonOptions -> (ObjectTransport -> IO a) -> IO a
withDaemonTransport options action =
  WebSocket.withWebSocket (daemonTransport options) (daemonTarget options) $ \transport ->
    action ((Transport.objectTransport (WebSocket.sendObject transport) (WebSocket.receiveObjectWithClose transport)) {transportLocality = daemonTransportLocality options, transportKind = Transport.WebSocketTransport})

daemonTransportLocality :: DaemonOptions -> TransportLocality
daemonTransportLocality options = if WebSocket.webSocketHost (daemonTarget options) `elem` ["127.0.0.1", "localhost", "::1"] then LocalHost else UnspecifiedHost

validateDaemonAuthentication :: DaemonAuthentication -> IO ()
validateDaemonAuthentication = \case
  DaemonAuthenticate credential -> when (Text.null (credentialText credential)) (throwIO InvalidDaemonCredential)
  DaemonTokenProvider _ _ -> pure ()
  DaemonInheritAuthentication _ _ -> pure ()
  DaemonInheritAuthenticationProvider _ _ -> pure ()

authenticationToken :: DaemonAuthentication -> IO Text
authenticationToken = \case
  DaemonAuthenticate credential -> pure (credentialText credential)
  DaemonTokenProvider provider _ -> fetchSessionToken provider
  DaemonInheritAuthentication _ token -> pure token
  DaemonInheritAuthenticationProvider _ provider -> fetchSessionToken provider

fetchSessionToken :: IO (Maybe Text) -> IO Text
fetchSessionToken provider = provider >>= maybe (throwIO DaemonCredentialUnavailable) pure

withDaemonConnectionUsing :: DaemonClientOptions -> Bool -> McpSessionOptions -> ((ObjectTransport -> IO a) -> IO a) -> (DaemonConnection -> IO a) -> IO a
withDaemonConnectionUsing options reject mcpOptions acquire action =
  withDaemonState $ \shared -> withDaemonConnectionStateUsing shared options reject mcpOptions acquire action

withDaemonConnectionStateUsing :: DaemonState -> DaemonClientOptions -> Bool -> McpSessionOptions -> ((ObjectTransport -> IO a) -> IO a) -> (DaemonConnection -> IO a) -> IO a
withDaemonConnectionStateUsing shared options reject mcpOptions acquire action = do
  validateDaemonAuthentication (daemonClientAuthentication options)
  let defaults = (daemonClientLoadConfiguration options, daemonClientLoadSpawnConfiguration options)
      (loadParams, _) = daemonLoadParameters "" "" mcpOptions reject defaults
  either throwIO pure (Configuration.validateDaemonLoadSessionParams loadParams)
  withDaemonStateLease shared $ \generation -> do
    let observability = daemonClientObservability options
    Obs.observeDroidOperation observability "droid.daemon.connection" Nothing $ acquire $ \rawTransport -> do
      let transport = if Obs.observabilityLogTransport observability then Transport.loggedObjectTransport (Obs.observabilityLogger observability) Transport.defaultTransportLogOptions rawTransport else rawTransport
      withObservedRpcChannel observability (transportSendObject transport) (transportReceiveObject transport) $ \channel -> do
        childHydrations <- newTVarIO (Just mempty)
        let prepare connection = do
              let authenticate credential = do
                    validateDaemonAuthentication (DaemonAuthenticate credential)
                    Core.connectionRequest connection 30000000 $ \rpc callOptions ->
                      Client.call (Proxy @(WithEnvelope (MethodRequest "daemon.authenticate" Object))) rpc callOptions (authenticationParams credential)
              identity <- case daemonClientAuthentication options of
                DaemonAuthenticate credential -> authenticate credential
                DaemonTokenProvider provider grant -> fetchSessionToken provider >>= \token -> authenticate (DaemonToken token grant)
                DaemonInheritAuthentication inherited _ -> pure inherited
                DaemonInheritAuthenticationProvider inherited _ -> pure inherited
              atomically (bindDaemonStatePrincipal shared generation identity)
              bindings <- newTVarIO mempty
              authenticated <- newTVarIO True
              authLock <- newMVar Nothing
              authEpoch <- newTVarIO 0
              credential <- newTVarIO (authenticationToken (daemonClientAuthentication options))
              let state = DaemonContext identity (daemonClientMachineId options) shared generation credential authenticated authLock authEpoch (sharedInteractions shared) bindings (sharedLoads shared) (sharedSessionStates shared) (sharedCache shared) (sharedChildOrder shared) childHydrations (daemonClientHydrateChildSessions options) (daemonClientRestoreTerminalsOnLoad options) (transportPendingSessionReady transport) (transportKind transport) defaults
                  owned = DaemonConnection connection state
                  dispatcher = Core.connectionDispatcher connection
              void (setBeforeRequest owned (Just (\identifier method -> unless (Set.member method skipEnsureLoaded) (ensureSessionLoaded owned identifier))))
              void $ registerRpcRequestBarrier channel $ \request -> do
                let method = baseRequestMethod (envelopeBody request)
                -- Explicit revocation must not queue behind the grant it supersedes.
                unless (method == "daemon.authenticate" || method == "daemon.logout") $ do
                  ready <- readTVarIO authenticated
                  if ready
                    then atomically (checkConnection owned)
                    else withMVar authLock (\failure -> mapM_ throwIO failure >> atomically (checkConnection owned))
              void (registerPreparedRpcHandler dispatcher "daemon.request_permission" (sessionReply owned True permissionRpcHandler Interaction.preparePermissionRpcHandler))
              void (registerPreparedRpcHandler dispatcher "daemon.ask_user" (sessionReply owned False questionRpcHandler Interaction.prepareQuestionRpcHandler))
              void (onRpcNotification dispatcher (\notification -> observeLifecycle owned notification `finally` atomically (void (pruneRegisteredSessions state))))
              void (onRpcEvent dispatcher (observePermissionReplay owned))
              void (onRpcClose dispatcher (const (atomically (suspendDaemonState shared generation))))
              pure owned
            run owned =
              finallyPreserving
                ( do
                    synchronizeRpcEvents channel
                    atomically (activateDaemonState owned)
                    action owned
                )
                (retireDaemonConnection owned)
        finallyPreserving
          (Core.withSessionConnectionSetup (sharedCoreState shared) channel (daemonBackend (daemonClientProtocolVersion options) (sharedSessionStates shared) (stateGenerationOpen shared generation)) reject mcpOptions prepare run)
          (stopChildHydrations childHydrations)

retireDaemonConnection :: DaemonConnection -> IO ()
retireDaemonConnection (DaemonConnection _ state) = mask_ $ do
  active <- atomically $ do
    writeTVar (connectionAuthenticated state) False
    modifyTVar' (connectionAuthEpoch state) (+ 1)
    suspendDaemonState (connectionLogicalState state) (connectionGenerationToken state)
    registered <- readTVar (connectionBindings state)
    writeTVar (connectionBindings state) mempty
    cache <- readTVar (connectionSessionCache state)
    pure (map bindingSession (Map.elems registered) <> Map.elems (cacheRetiringBindings cache))
  forM_ active Core.closeDroidSession
  stopChildHydrations (connectionChildHydrations state)
  forM_ active Core.waitDroidSessionIdle
  atomically (void (pruneRegisteredSessions state))

skipEnsureLoaded :: Set Text
skipEnsureLoaded =
  Set.fromList
    [ "daemon.load_session",
      "daemon.initialize_session",
      "daemon.close_session",
      "daemon.interrupt_session",
      "daemon.archive_session",
      "daemon.unarchive_session",
      "daemon.rename_session",
      "daemon.create_terminal",
      "daemon.write_terminal_data",
      "daemon.resize_terminal",
      "daemon.close_terminal",
      "daemon.list_terminals",
      "daemon.list_available_plugins",
      "daemon.list_installed_plugins",
      "daemon.install_plugin",
      "daemon.uninstall_plugin",
      "daemon.set_plugin_enabled",
      "daemon.update_plugin",
      "daemon.list_marketplaces",
      "daemon.add_marketplace",
      "daemon.remove_marketplace",
      "daemon.update_marketplace",
      "daemon.get_automation_visual",
      "daemon.get_workspace_file_content",
      "daemon.write_workspace_file_content",
      "daemon.list_crons",
      "daemon.create_cron",
      "daemon.update_cron",
      "daemon.delete_cron",
      "daemon.hold_session_crons",
      "daemon.resume_session_crons"
    ]

-- The in-process hook follows the RPC receipt, not later terminal restoration
-- or controller publication; waiting for those here would create a load cycle.
withSessionReadyGate :: DaemonContext -> Text -> IO a -> IO a
withSessionReadyGate state identifier action = case connectionPendingSessionReady state of
  Nothing -> action
  Just register -> mask $ \restore -> do
    ready <- newEmptyTMVarIO
    outcome <- try @SomeException $ restore $ do
      register identifier (readTMVar ready >>= either throwSTM pure)
      action
    atomically (putTMVar ready (void outcome))
    either throwIO pure outcome

-- The distinct handle prevents applying local replacement semantics to daemon
-- sessions, whose fork/compact operations have different ownership contracts.
data DaemonSession = DaemonSession
  { daemonSessionBinding :: !DaemonBinding,
    sessionInfo :: !DaemonSessionInfo,
    sessionConnection :: !DaemonConnection
  }

daemonCoreSession :: DaemonSession -> Core.DroidSession
daemonCoreSession = bindingSession . daemonSessionBinding

-- | Immutable attachment receipt, not a live cwd or worktree store. On creation
-- the effective cwd is the reported worktree path or the requested cwd. On load
-- it is the reported cwd (legacy worktree.path fallback), or remains unknown.
data DaemonSessionInfo = DaemonSessionInfo
  { daemonSessionUser :: !GetUserInfoResult,
    daemonInitialWorkingDirectory :: !(Maybe Text),
    daemonInitialWorktree :: !(Maybe SessionWorktreeInfo),
    daemonLoadedState :: !(Maybe LoadedSessionState)
  }
  deriving stock (Eq)

instance Show DaemonSessionInfo where
  show _ = "DaemonSessionInfo <redacted>"

-- sessionConnection borrows the original physical connection's scope. Detach
-- does not shorten that owner's lifetime, and borrowing never extends it.

sessionId :: DaemonSession -> Text
sessionId = Core.droidSessionId . daemonCoreSession

sessionStatus :: DaemonSession -> IO Core.DroidSessionStatus
sessionStatus = Core.droidSessionStatus . daemonCoreSession

authenticatedUser :: DaemonSession -> GetUserInfoResult
authenticatedUser = daemonSessionUser . sessionInfo

-- | Last observed settings, with the same atomic lifecycle/prefix semantics as
-- the local getter. This does not send an RPC or predict the next notification.
getSettings :: DaemonSession -> IO SessionSettings
getSettings = Core.getDroidSettings . daemonCoreSession

-- | Read the owned mission view at the observed event prefix. This sends no
-- RPC and does not associate other sessions or expose a mutable store.
getMissionSnapshot :: DaemonSession -> IO (Maybe MissionSnapshot)
getMissionSnapshot = Core.getDroidMissionSnapshot . daemonCoreSession

-- | Read cached mission observations for an explicit session without loading
-- it or creating a store. The original connection scope still owns this data.
getMissionSnapshotForSession :: DaemonConnection -> Text -> IO (Maybe MissionSnapshot)
getMissionSnapshotForSession (DaemonConnection connection _) = Core.lookupMissionSnapshot connection

getMissionIdForSession :: DaemonConnection -> Text -> IO (Maybe Text)
getMissionIdForSession (DaemonConnection connection _) = Core.lookupMissionId connection

-- | Follow notifications for the owned mission and its explicitly associated
-- members. Session/turn and permission subscriptions retain their own routing.
onMissionSnapshot :: DaemonSession -> (Either Core.DroidError (Maybe MissionSnapshot) -> IO ()) -> IO (IO ())
onMissionSnapshot = Core.onDroidMissionSnapshot . daemonCoreSession

-- | Apply a patch under the session mutation lease, waiting through ACK when
-- necessary. Only observed settings notifications update the shared view.
updateSettings :: DaemonSession -> UpdateSessionSettingsParams -> IO Object
updateSettings attached params =
  let session = daemonCoreSession attached
   in Core.sessionRequest Core.MutatingRequest session $ \channel options -> do
        let identifier = Core.droidSessionId session
            dispatcher = Core.connectionDispatcher (Core.sessionConnection session)
        result <- awaitDaemonCommand (Proxy @SettingsUpdated) "settings_updated" dispatcher options identifier (Client.updateDaemonSessionSettingsRaw channel options identifier params)
        pure (if KeyMap.lookup "accepted" result == Just (Bool True) then mempty else result)

-- | Retain actual legacy success flags; an ACK requires a valid correlated
-- title notification. No local title/store update is synthesized.
renameSession :: DaemonSession -> Text -> IO SuccessResult
renameSession attached title =
  let session = daemonCoreSession attached
   in Core.sessionRequest Core.MutatingRequest session $ \channel options -> do
        let identifier = Core.droidSessionId session
            dispatcher = Core.connectionDispatcher (Core.sessionConnection session)
        result <- awaitDaemonCommand (Proxy @SessionTitleUpdated) "session_title_updated" dispatcher options identifier (Client.renameDaemonSessionRaw channel options identifier (RenameSessionParams title mempty))
        if KeyMap.lookup "accepted" result == Just (Bool True)
          then pure (SuccessResult True mempty)
          else either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (Object result))

-- | Query remote metadata under a read-only session lease. Reported paths,
-- command flags and skill content are data, not local execution instructions.
listSkills :: DaemonSession -> IO ListSkillsResult
listSkills session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_skills" Object))) session (mempty :: Object)

listCommands :: DaemonSession -> IO ListCommandsResult
listCommands session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_commands" Object))) session (mempty :: Object)

getContextBreakdown :: DaemonSession -> IO GetContextBreakdownResult
getContextBreakdown session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.get_context_breakdown" Object))) session (mempty :: Object)

-- | Mutate daemon-owned skill settings, retaining its actual success flag and
-- the existing session invalidation/interruption policy on uncertain failures.
setSkillDisabled :: DaemonSession -> SetSkillDisabledParams -> IO SuccessResult
setSkillDisabled = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.set_skill_disabled" Object)))

-- | Plugin/marketplace data and mutations belong to the daemon. No local
-- installer, Git checkout, policy inference or metadata cache is provided.
listAvailablePlugins :: DaemonSession -> IO ListAvailablePluginsResult
listAvailablePlugins session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_available_plugins" Object))) session (mempty :: Object)

listInstalledPlugins :: DaemonSession -> Maybe Text -> IO ListInstalledPluginsResult
listInstalledPlugins session scope = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_installed_plugins" Object))) session (PluginScopeParams scope mempty)

installPlugin :: DaemonSession -> InstallPluginParams -> IO InstallPluginResult
installPlugin = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.install_plugin" Object)))

uninstallPlugin :: DaemonSession -> PluginTargetParams -> IO SuccessOrErrorResult
uninstallPlugin = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.uninstall_plugin" Object)))

setPluginEnabled :: DaemonSession -> SetPluginEnabledParams -> IO SuccessOrErrorResult
setPluginEnabled = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.set_plugin_enabled" Object)))

updatePlugin :: DaemonSession -> UpdatePluginParams -> IO UpdatePluginResult
updatePlugin = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.update_plugin" Object)))

listMarketplaces :: DaemonSession -> IO ListMarketplacesResult
listMarketplaces session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_marketplaces" Object))) session (mempty :: Object)

addMarketplace :: DaemonSession -> MarketplaceSource -> IO AddMarketplaceResult
addMarketplace session source = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.add_marketplace" Object))) session (AddMarketplaceParams source mempty)

removeMarketplace :: DaemonSession -> Text -> IO SuccessOrErrorResult
removeMarketplace session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.remove_marketplace" Object))) session (MarketplaceNameParams name mempty)

updateMarketplace :: DaemonSession -> Maybe Text -> IO UpdateMarketplaceResult
updateMarketplace session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.update_marketplace" Object))) session (UpdateMarketplaceParams name mempty)

withSession :: DaemonOptions -> (DaemonSession -> IO a) -> IO a
withSession options = withSessionHandlers options defaultDroidHandlers

withSessionHandlers :: DaemonOptions -> DroidHandlers -> (DaemonSession -> IO a) -> IO a
withSessionHandlers options handlers = withDaemonSession options handlers Nothing

withResumedSession :: DaemonOptions -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSession options = withResumedSessionHandlers options defaultDroidHandlers

withResumedSessionHandlers :: DaemonOptions -> DroidHandlers -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSessionHandlers options handlers identifier = withDaemonSession options handlers (Just identifier)

withDaemonSession :: DaemonOptions -> DroidHandlers -> Maybe Text -> (DaemonSession -> IO a) -> IO a
withDaemonSession options handlers saved = withDaemonSessionUsing (daemonClientOptions options) handlers saved (daemonTransportLocality options) (withDaemonTransport options)

withSessionUsing :: DaemonClientOptions -> ObjectTransport -> (DaemonSession -> IO a) -> IO a
withSessionUsing options transport = withSessionUsingHandlers options transport defaultDroidHandlers

withSessionUsingHandlers :: DaemonClientOptions -> ObjectTransport -> DroidHandlers -> (DaemonSession -> IO a) -> IO a
withSessionUsingHandlers options transport handlers = withDaemonSessionUsing options handlers Nothing (transportLocality transport) ($ transport)

withResumedSessionUsing :: DaemonClientOptions -> ObjectTransport -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSessionUsing options transport = withResumedSessionUsingHandlers options transport defaultDroidHandlers

withResumedSessionUsingHandlers :: DaemonClientOptions -> ObjectTransport -> DroidHandlers -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSessionUsingHandlers options transport handlers identifier = withDaemonSessionUsing options handlers (Just identifier) (transportLocality transport) ($ transport)

withDaemonSessionUsing :: DaemonClientOptions -> DroidHandlers -> Maybe Text -> TransportLocality -> ((ObjectTransport -> IO a) -> IO a) -> (DaemonSession -> IO a) -> IO a
withDaemonSessionUsing options handlers saved locality acquire action = do
  validateDaemonAuthentication (daemonClientAuthentication options)
  when (isJust saved && isJust (daemonClientModel options)) (throwIO DaemonModelRequiresNewSession)
  when (isJust saved && (isJust (daemonClientWorktree options) || isJust (daemonClientWorktreeDirectory options))) (throwIO DaemonWorktreeRequiresNewSession)
  when (isJust saved && (daemonClientConfiguration options /= Configuration.defaultSessionConfiguration || daemonClientSpawnOptions options /= Configuration.defaultDaemonSpawnOptions || isJust (daemonClientSystemPrompt options) || isJust (daemonClientInitializationTimeoutMicros options))) (throwIO Configuration.InitializationOptionsOnResume)
  forM_ (daemonClientInitializationTimeoutMicros options) $ \micros -> when (micros < 0 || micros > maxBound `div` 2) (throwIO RpcInvalidTimeout)
  forM_ (daemonClientTurnTimeoutMicros options) $ \micros -> when (micros < 0) (throwIO RpcInvalidTimeout)
  mcpOptions <- either throwIO pure (validateMcpConfiguration (daemonClientMcpOptions options))
  when (isJust saved && isJust (sessionBlockOnMcpLoad mcpOptions)) (throwIO McpInitOnlyOptionOnResume)
  when (isNothing saved) (validateCreatedSessionOptions (sessionCreationOptions options mcpOptions) handlers)
  let (loadParams, _) = daemonLoadParameters (fromMaybe "" saved) "" mcpOptions (daemonAutoReject (daemonClientConfiguration options) handlers) (daemonClientLoadConfiguration options, daemonClientLoadSpawnConfiguration options)
  either throwIO pure (Configuration.validateDaemonLoadSessionParams loadParams)
  when (not (null (daemonClientHostedMcpServers options)) && locality /= LocalHost) (throwIO Hosted.HostedMcpRequiresLocalDaemon)
  Hosted.withMcpServerOptions (daemonClientHostedMcpServers options) mcpOptions $ \activeOptions ->
    openDaemonSession options handlers saved activeOptions acquire action

daemonAutoReject :: Configuration.SessionConfiguration -> DroidHandlers -> Bool
daemonAutoReject config handlers = fromMaybe (isNothing (onDroidPermission handlers)) (Configuration.configurationAutoRejectPermissions config)

validateCreatedSessionOptions :: DaemonSessionOptions -> DroidHandlers -> IO ()
validateCreatedSessionOptions options handlers = do
  forM_ (daemonSessionInitializationTimeoutMicros options) $ \micros -> when (micros < 0 || micros > maxBound `div` 2) (throwIO RpcInvalidTimeout)
  forM_ (daemonSessionTurnTimeoutMicros options) $ \micros -> when (micros < 0) (throwIO RpcInvalidTimeout)
  let base = daemonSessionParameters options
      config = Configuration.initializeConfiguration base
      identifier = fromMaybe "" (Configuration.configurationSessionId config)
      mcp = Configuration.initializeMcpOptions base
  void (either throwIO pure (validateMcpConfiguration mcp))
  either throwIO pure (Configuration.validateDaemonInitializationParams (daemonInitializationParams options handlers identifier ""))
  let (loadParams, _) = daemonLoadParameters identifier "" mcp (daemonAutoReject config handlers) (daemonSessionLoadConfiguration options, daemonSessionLoadSpawnConfiguration options)
  either throwIO pure (Configuration.validateDaemonLoadSessionParams loadParams)

daemonInitializationParams :: DaemonSessionOptions -> DroidHandlers -> Text -> Text -> Configuration.DaemonInitializeSessionParams
daemonInitializationParams options handlers identifier token =
  let base = daemonSessionParameters options
      config = Configuration.initializeConfiguration base
      configured =
        base
          { Configuration.initializeConfiguration =
              config
                { Configuration.configurationSessionId = Just identifier,
                  Configuration.configurationSource = Just (fromMaybe (SessionSource (SourceApi identifier) mempty) (Configuration.configurationSource config)),
                  Configuration.configurationOrigin = Just (fromMaybe Enums.OriginAPI (Configuration.configurationOrigin config)),
                  Configuration.configurationAutoRejectPermissions = Just (daemonAutoReject config handlers)
                }
          }
   in Configuration.DaemonInitializeSessionParams configured token (daemonSessionSpawnOptions options)

initializationTimeout :: DaemonSessionOptions -> Int
initializationTimeout = fromMaybe 60000000 . daemonSessionInitializationTimeoutMicros

-- Only the timeout of a stable-session initialization is retryable. Receipt
-- restoration, auth/token acquisition and user actions remain outside this loop.
retryInitialization :: IO a -> IO a
retryInitialization =
  Retry.retry
    ( Retry.defaultRetryOptions
        { Retry.retryPolicy = Retry.RetryPolicy 2 (Retry.FixedDelay 0) Retry.NoJitter False,
          Retry.retryPredicate = pure . (== Just RpcRequestTimedOut) . fromException
        }
    )

openDaemonSession :: DaemonClientOptions -> DroidHandlers -> Maybe Text -> McpSessionOptions -> ((ObjectTransport -> IO a) -> IO a) -> (DaemonSession -> IO a) -> IO a
openDaemonSession options handlers saved mcpOptions acquire action =
  withDaemonConnectionUsing options (daemonAutoReject (daemonClientConfiguration options) handlers) mcpOptions acquire $ \owned ->
    case saved of
      Nothing -> withCreatedSessionOn owned handlers (sessionCreationOptions options mcpOptions) action
      Just identifier -> withDaemonBinding owned handlers identifier (daemonClientTurnTimeoutMicros options) $ \binding -> do
        info <- Core.withSessionUse (bindingSession binding) (loadSessionInfoWithConfiguration owned identifier (daemonClientLoadConfiguration options) (daemonClientLoadSpawnConfiguration options))
        action (DaemonSession binding info owned)

-- | Create a new scoped session through an already owned connection. No new
-- reader, authentication exchange or physical connection is acquired. Scope exit
-- detaches only this attachment; it does not close the remote session or owner.
withSessionOn :: DaemonConnection -> DaemonSessionOptions -> (DaemonSession -> IO a) -> IO a
withSessionOn owned = withSessionOnHandlers owned defaultDroidHandlers

-- | Handlers are registered before initialization and retain the existing
-- per-attachment routing and cleanup rules. Invalid configuration fails before
-- token-provider or initialization I/O; callers still own remote side effects.
withSessionOnHandlers :: DaemonConnection -> DroidHandlers -> DaemonSessionOptions -> (DaemonSession -> IO a) -> IO a
withSessionOnHandlers owned handlers options action = do
  validateCreatedSessionOptions options handlers
  withCreatedSessionOn owned handlers options action

-- Both connection-owning constructors and the borrowed-owner API initialize
-- through the same binding, generation, readiness gate and retry boundary.
withCreatedSessionOn :: DaemonConnection -> DroidHandlers -> DaemonSessionOptions -> (DaemonSession -> IO a) -> IO a
withCreatedSessionOn owned@(DaemonConnection connection state) handlers options action = do
  let base = daemonSessionParameters options
      config = Configuration.initializeConfiguration base
  identifier <- maybe (UUID.toText <$> nextRandom) pure (Configuration.configurationSessionId config)
  withDaemonBinding owned handlers identifier (daemonSessionTurnTimeoutMicros options) $ \binding -> do
    info <- Core.withSessionUse (bindingSession binding) $ do
      let initial = Configuration.initializeConfiguration (Configuration.daemonInitializeSession (daemonInitializationParams options handlers identifier ""))
          inherited = (Configuration.loadConfigurationFromInitialization initial) {Configuration.loadAutoRejectPermissions = Configuration.configurationAutoRejectPermissions config}
          spawn = Configuration.daemonLoadConfigurationFromSpawn (daemonSessionSpawnOptions options)
          policy = mergeLoadPolicy (inherited, spawn) (daemonSessionLoadConfiguration options, daemonSessionLoadSpawnConfiguration options)
      trackSessionLoadWithCreation (Just (bindingToken binding, Configuration.initializeMachineId base)) (Just policy) owned identifier $ \guard -> Core.connectionBoundary connection (2 * initializationTimeout options) $ do
        let admit = do
              checkConnection owned
              current <- loadStillCurrent guard
              unless current (throwSTM DaemonLoadSuperseded)
        atomically admit
        credential <- join (readTVarIO (connectionCredential state))
        let decode = attachmentReceipt (connectionIdentity state) (Just (Configuration.initializeWorkingDirectory base))
            params = Configuration.daemonInitializationFields (daemonInitializationParams options handlers identifier credential)
        (attached, (receipt, _)) <-
          withSessionReadyGate state identifier $
            retryInitialization $
              Core.callSettingsResultObservedWithAdmission admit (Just (initializationTimeout options)) connection Nothing "daemon.initialize_session" params decode (observeLoadReceipt guard)
        unless (attached == identifier) (throwIO Core.DroidInvalidEvent)
        pure receipt
    action (DaemonSession binding info owned)

-- | Attach a saved session without reauthenticating or taking connection ownership.
-- Scope exit detaches this handle only; no remote close or logout is sent.
withResumedSessionOn :: DaemonConnection -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSessionOn connection = withResumedSessionOnHandlers connection defaultDroidHandlers

withResumedSessionOnHandlers :: DaemonConnection -> DroidHandlers -> Text -> (DaemonSession -> IO a) -> IO a
withResumedSessionOnHandlers owned handlers identifier = withResumedSessionOnConfigured owned handlers identifier Configuration.defaultSessionLoadConfiguration Configuration.defaultDaemonLoadConfiguration

withResumedSessionOnConfigured :: DaemonConnection -> DroidHandlers -> Text -> Configuration.SessionLoadConfiguration -> Configuration.DaemonLoadConfiguration -> (DaemonSession -> IO a) -> IO a
withResumedSessionOnConfigured owned handlers identifier config spawn action =
  withDaemonBinding owned handlers identifier Nothing $ \binding -> do
    info <- Core.withSessionUse (bindingSession binding) (loadSessionInfoWithConfiguration owned identifier config spawn)
    action (DaemonSession binding info owned)

withDaemonBinding :: DaemonConnection -> DroidHandlers -> Text -> Maybe Int -> (DaemonBinding -> IO a) -> IO a
withDaemonBinding owned@(DaemonConnection connection state) handlers identifier deadline action = mask $ \restore -> do
  session <- Core.newSessionHandle identifier connection deadline
  token <- newUnique
  writers <- newTVarIO mempty
  writeLock <- newMVar ()
  currentHandlers <- newTVarIO handlers
  let binding = DaemonBinding session currentHandlers token writers writeLock
  atomically $ do
    open <- readTVar (Core.connectionOpen connection)
    unless open (throwSTM RpcChannelClosed)
    bindings <- readTVar (connectionBindings state)
    when (Map.member identifier bindings) (throwSTM DaemonSessionAlreadyAttached)
    writeTVar (connectionBindings state) (Map.insert identifier binding bindings)
  ( do
      void (Core.ownSessionCleanup session (atomically (writeTVar writers mempty)))
      let dynamicHandlers = defaultDroidHandlers {onDroidMcpEvent = Just (\scopeId event -> do current <- readTVarIO currentHandlers; forM_ (onDroidMcpEvent current) (\callback -> callback scopeId event))}
      cleanup <- Core.installMcpObserver connection "daemon.session_notification" (Just identifier) dynamicHandlers
      void (Core.ownSessionCleanup session cleanup)
      settled <- onRpcSessionRequestSettled (Core.connectionChannel connection) identifier $ \requestId -> do
        current <- readTVarIO currentHandlers
        forM_ (onDroidRequestSettled current) ($ requestId)
      void (Core.ownSessionCleanup session settled)
      restore (action binding)
    )
    `finally` detachBinding True owned binding

-- | Retire one attachment and join its admitted operations. In-flight remote
-- work is not rolled back. Do not call lifecycle operations from callbacks.
detachSession :: DaemonSession -> IO ()
detachSession session = detachBinding True (sessionConnection session) (daemonSessionBinding session)

-- | Explicitly request remote close, then detach this handle on success.
-- Unlike the connection-level close operation, no lifecycle notice is needed
-- to retire this particular handle. Other attachments retain their ownership.
closeAttachedSession :: DaemonSession -> IO Object
closeAttachedSession session = mask $ \restore -> do
  result <- Core.withSessionLease (daemonCoreSession session) (restore (closeSession (sessionConnection session) (defaultDaemonCloseSessionParams (sessionId session))))
  let DaemonConnection _ state = sessionConnection session
      identifier = sessionId session
  atomically $ do
    bindings <- readTVar (connectionBindings state)
    case Map.lookup identifier bindings of
      Just current | bindingToken current == bindingToken (daemonSessionBinding session) -> do
        invalidateSessionLoad state identifier True
        retireChildState state identifier
      _ -> pure ()
  detachSession session
  pure result

detachBinding :: Bool -> DaemonConnection -> DaemonBinding -> IO ()
detachBinding wait (DaemonConnection _ state) binding = mask_ $ do
  Core.closeDroidSession (bindingSession binding)
  atomically $ do
    bindings <- readTVar (connectionBindings state)
    case Map.lookup (Core.droidSessionId (bindingSession binding)) bindings of
      Just current | bindingToken current == bindingToken binding -> do
        let identifier = Core.droidSessionId (bindingSession binding)
        writeTVar (connectionBindings state) (Map.delete identifier bindings)
        modifyTVar' (connectionSessionStates state) (Map.adjust SessionState.cancelSessionSubmissions identifier)
        modifyTVar' (connectionSessionCache state) (\cache -> cache {cacheRetiringBindings = Map.insert (bindingToken binding) (bindingSession binding) (cacheRetiringBindings cache)})
      _ -> pure ()
  when wait (Core.waitDroidSessionIdle (bindingSession binding))
  atomically (void (pruneRegisteredSessions state))

attachmentReceipt :: GetUserInfoResult -> Maybe Text -> Object -> Parser (DaemonSessionInfo, [RestoredSessionRequest])
attachmentReceipt identity initialDirectory fields = do
  worktree <- if isJust initialDirectory then fields .:! "worktree" else pure Nothing
  cwd <- case initialDirectory of
    Just directory -> pure (Just (maybe directory initialWorktreePath worktree))
    Nothing -> do
      reported <- fields .:! "cwd"
      pure $ case reported of
        Just directory -> Just directory
        Nothing -> case KeyMap.lookup "worktree" fields of
          Just (Object legacy) -> case KeyMap.lookup "path" legacy of
            Just (String path) -> Just path
            _ -> Nothing
          _ -> Nothing
  loaded <- if isNothing initialDirectory then Just <$> parseJSON (Object fields) else pure Nothing
  pending <- if isNothing initialDirectory then pendingSessionRequests fields else pure []
  pure (DaemonSessionInfo identity cwd worktree loaded, pending)

restorePending :: SessionLoadGuard -> DaemonConnection -> Text -> [RestoredSessionRequest] -> IO ()
restorePending guard (DaemonConnection connection state) identifier pending = do
  synchronizeRpcEvents (Core.connectionChannel connection)
  forM_ pending $ \(method, requestId, fields) -> do
    let params = KeyMap.insert "sessionId" (String identifier) fields
        context = Core.backendContext (Core.connectionBackend connection)
        dispatcher = Core.connectionDispatcher connection
    ready <- atomically (awaitRestoredRequest guard dispatcher requestId)
    when ready $ do
      when (method == "daemon.request_permission") $ atomically $ do
        current <- restoredRequestCurrent guard requestId
        when current (refreshPermissionRequest state identifier requestId params)
      dispatchRpcRequestWhen dispatcher (restoredRequestCurrent guard requestId) (context {envelopeBody = BaseRequest requestId method (Just (Object params)) mempty})

completeInteractionRequest :: DaemonContext -> Text -> Text -> Text -> STM ()
completeInteractionRequest state execution method requestId = modifyTVar' (connectionLoads state) (Map.alter (Just . completedRequest method requestId . fromMaybe emptyLoadEntry) execution)

completedRequest :: Text -> Text -> SessionLoadEntry -> SessionLoadEntry
completedRequest method requestId entry = entry {entryCompletedRequests = Set.insert (method, requestId) (entryCompletedRequests entry), entryRestoredRequests = Map.update (\value@(pendingMethod, _) -> if pendingMethod == method then Nothing else Just value) requestId (entryRestoredRequests entry)}

rememberRetiringRequests :: DaemonContext -> Text -> [Text] -> STM ()
rememberRetiringRequests state execution requests = modifyTVar' (connectionLoads state) (Map.alter (Just . remember . fromMaybe emptyLoadEntry) execution)
  where
    remember entry = entry {entryRetiringRequests = Set.union (Set.fromList requests) (entryRetiringRequests entry)}

-- A resolution can arrive after a load receipt but before its restored worker
-- is admitted. Revoke that request at the same ordered observation boundary.
retireRestoredPermission :: DaemonContext -> Text -> Text -> STM ()
retireRestoredPermission state surface requestId = modifyTVar' (connectionLoads state) (Map.mapWithKey retire)
  where
    retire execution entry = case Map.lookup requestId (entryRestoredRequests entry) of
      Just ("daemon.request_permission", fields) | associated execution fields -> completedRequest "daemon.request_permission" requestId entry
      _ -> entry
    associated execution fields =
      execution == surface || case parseEither (.:! "associatedSessionIds") fields :: Either String (Maybe [Text]) of
        Right (Just identifiers) -> surface `elem` identifiers
        _ -> False

pendingSessionRequests :: Object -> Parser [(Text, Text, Object)]
pendingSessionRequests snapshot = concat <$> traverse parseGroup [("pendingPermissions", "daemon.request_permission"), ("pendingAskUserRequests", "daemon.ask_user")]
  where
    parseGroup (key, method) = do
      pending <- snapshot .:! key
      traverse
        (\fields -> do requestId <- fields .: "requestId"; pure (method, requestId, KeyMap.delete "requestId" fields))
        (fromMaybe [] pending)

credentialText :: DaemonCredential -> Text
credentialText (DaemonApiKey key) = key
credentialText (DaemonToken token _) = token

authenticationParams :: DaemonCredential -> Object
authenticationParams credential = KeyMap.fromList ("caller" .= String "haskell-sdk" : fields)
  where
    fields = case credential of
      DaemonApiKey key -> ["apiKey" .= key]
      DaemonToken token grant -> ["token" .= token] <> maybe [] (\value -> ["actAsGrant" .= value]) grant

daemonBackend :: Text -> TVar (Map Text SessionState.SessionState) -> STM Bool -> Core.SessionBackend
daemonBackend protocolVersion sessionStates currentGeneration =
  Core.SessionBackend
    (WithEnvelope (Just protocolVersion) Nothing mempty)
    decodeDaemonNotification
    (submitMessage sessionStates currentGeneration)
    ( \channel callOptions identifier -> mask $ \restore -> do
        result <- restore (Client.call (Proxy @(WithEnvelope (MethodRequest "daemon.interrupt_session" Object))) channel callOptions (KeyMap.singleton "sessionId" (String identifier)))
        atomically $ do
          current <- currentGeneration
          unless current (throwSTM RpcChannelClosed)
          modifySessionState sessionStates identifier (SessionState.pauseDaemonQueue Nothing)
        pure result
    )
    Nothing

submitMessage :: TVar (Map Text SessionState.SessionState) -> STM Bool -> RpcDispatcher -> RpcChannel -> Client.CallOptions -> Text -> AddUserMessageParams -> IO Object
submitMessage sessionStates currentGeneration dispatcher channel options identifier input = mask $ \restore -> do
  wireInput <- case userMessageId input of
    Just _ -> pure input
    Nothing -> do messageId <- UUID.toText <$> nextRandom; pure (input {userMessageId = Just messageId})
  fields <- either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (toJSON wireInput))
  now <- monotonicMicros
  placeholder <- UUID.toText <$> nextRandom
  let requestId = Client.callRequestId options
  atomically $ do
    allowed <- currentGeneration
    unless allowed (throwSTM RpcChannelClosed)
    states <- readTVar sessionStates
    let current = Map.findWithDefault SessionState.emptySessionState identifier states
        bubble = maybe placeholder SessionState.submissionPlaceholderId (SessionState.lookupSubmission requestId current)
    next <- either throwSTM pure (SessionState.beginSubmissionAt now (Just 20000000) requestId bubble wireInput current)
    writeTVar sessionStates (Map.insert identifier next states)
  let params = KeyMap.insert "sessionId" (String identifier) (KeyMap.union fields (KeyMap.singleton "userMessageSource" (String "api")))
  outcome <- try @SomeException $ restore (awaitDaemonCommand (Proxy @CreateMessage) "create_message" dispatcher options identifier (Client.call (Proxy @(WithEnvelope (MethodRequest "daemon.add_user_message" Object))) channel options params))
  -- A caller can remain in its own callback after the physical scope retires.
  -- Do not publish its late outcome into a successor generation's observations.
  published <- atomically $ do
    allowed <- currentGeneration
    when allowed $ modifySessionState sessionStates identifier $ case outcome of
      Right _ -> SessionState.finishSubmission requestId
      Left cause -> case submissionFailure cause of
        Just failure -> SessionState.rejectSubmission requestId failure
        Nothing -> SessionState.finishSubmission requestId . SessionState.cancelSubmission requestId
    pure allowed
  case outcome of
    Left cause -> throwIO cause
    Right result -> if published then pure result else throwIO RpcChannelClosed

-- Caller cancellation can carry any exception, not only SomeAsyncException.
-- Only known protocol failures retain an error overlay; every cause is rethrown.
submissionFailure :: SomeException -> Maybe SessionState.SubmissionFailure
submissionFailure cause
  | Just RpcRequestTimedOut <- fromException cause = Just SessionState.SubmissionTimedOut
  | Just RpcDuplicateRequestId <- fromException cause = Just SessionState.SubmissionRejected
  | Just (_ :: RpcChannelError) <- fromException cause = Just SessionState.SubmissionConnectionFailed
  | Just Core.DroidInvalidEvent <- fromException cause = Just SessionState.SubmissionInvalidConfirmation
  | Just (_ :: RpcResultError) <- fromException cause = Just SessionState.SubmissionRejected
  | otherwise = Nothing

-- | A connection-level command with explicit, connection-unique request ID.
-- An ACK waits for its matching create-message notification; legacy results
-- return unchanged. Neither is completion of the agent turn. Cache publication
-- after physical retirement fails closed without undoing remote effects.
submitUserMessage :: DaemonConnection -> Text -> Text -> AddUserMessageParams -> IO Object
submitUserMessage owned@(DaemonConnection connection state) identifier requestId input = mask $ \restore -> do
  prepared <- atomically $ do
    checkConnection owned
    states <- readTVar (connectionSessionStates state)
    pure (isJust (Map.lookup identifier states >>= SessionState.lookupSubmission requestId))
  result <- restore $ Core.connectionRequest connection 30000000 $ \channel options ->
    submitMessage (connectionSessionStates state) (connectionScopeOpen owned) (Core.connectionDispatcher connection) channel (options {Client.callRequestId = requestId}) identifier input
  observedAt <- (* 1000) . realToFrac <$> getPOSIXTime
  atomically $ do
    checkConnection owned
    states <- readTVar (connectionSessionStates state)
    loads <- readTVar (connectionLoads state)
    let busy = case Map.lookup identifier loads of
          Just entry -> case readinessWorkingState (entryReadiness entry) of Right (Just working) -> working /= WorkingIdle; _ -> False
          Nothing -> False
        shouldQueue = userMessageSkipAgentLoop input /= Just True && (userMessageQueuePlacement input == Just QueueEndOfLoop || (busy && not prepared))
    case Map.lookup identifier states >>= SessionState.lookupSubmission requestId of
      Just submission
        | shouldQueue ->
            let actualInput = SessionState.submissionInput submission
                queued = SessionState.QueueEntry (QueuedUserMessage requestId actualInput) (SessionState.queueKindForPlacement (userMessageQueuePlacement actualInput)) observedAt
             in modifySessionState (connectionSessionStates state) identifier (SessionState.enqueueMessages [queued])
      _ -> pure ()
  pure result

monotonicMicros :: IO Integer
monotonicMicros = (`div` 1000) . toInteger <$> getMonotonicTimeNSec

modifySessionState :: TVar (Map Text SessionState.SessionState) -> Text -> (SessionState.SessionState -> SessionState.SessionState) -> STM ()
modifySessionState states identifier update = modifyTVar' states (Map.alter (Just . update . fromMaybe SessionState.emptySessionState) identifier)

-- | Local selection metadata only; these operations do not fetch, compact or
-- delete remote history, and do not require an attached turn handle.
setSessionProgressiveDisplay :: DaemonConnection -> Text -> Bool -> IO ()
setSessionProgressiveDisplay connection@(DaemonConnection _ state) identifier enabled = atomically $ do
  checkConnection connection
  modifySessionState (connectionSessionStates state) identifier (SessionState.setProgressiveDisplay enabled)

expandSessionDisplay :: DaemonConnection -> Text -> IO Bool
expandSessionDisplay connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  states <- readTVar (connectionSessionStates state)
  let previous = Map.findWithDefault SessionState.emptySessionState identifier states
      (current, limited) = SessionState.expandSessionDisplay previous
  writeTVar (connectionSessionStates state) (Map.insert identifier current states)
  pure limited

setSessionDisplayCutoff :: DaemonConnection -> Text -> Maybe Text -> IO ()
setSessionDisplayCutoff connection@(DaemonConnection _ state) identifier boundary = atomically $ do
  checkConnection connection
  modifySessionState (connectionSessionStates state) identifier (SessionState.setSessionDisplayCutoff boundary)

getSessionState :: DaemonConnection -> Text -> IO SessionState.SessionState
getSessionState connection@(DaemonConnection _ state) identifier = do
  now <- monotonicMicros
  atomically $ do
    checkConnection connection
    states <- readTVar (connectionSessionStates state)
    case Map.lookup identifier states of
      Nothing -> pure SessionState.emptySessionState
      Just previous -> do
        let current = SessionState.expireSessionStateAt now previous
        when (current /= previous) (writeTVar (connectionSessionStates state) (Map.insert identifier current states))
        pure current

-- | Caller-owned observation; expiry is evaluated with a monotonic clock.
-- No timer thread is retained when no caller is observing the state.
waitSessionStateChange :: DaemonConnection -> Text -> SessionState.SessionState -> IO SessionState.SessionState
waitSessionStateChange connection@(DaemonConnection _ state) identifier previous = do
  current <- getSessionState connection identifier
  if current /= previous
    then pure current
    else do
      let changed = atomically $ do
            checkConnection connection
            states <- readTVar (connectionSessionStates state)
            check (Map.findWithDefault SessionState.emptySessionState identifier states /= current)
      case SessionState.nextSessionDeadline current of
        Nothing -> changed
        Just deadline -> do
          now <- monotonicMicros
          let remaining = fromInteger (max 0 (min (toInteger (maxBound :: Int)) (deadline - now)))
          void (timeout remaining changed)
      waitSessionStateChange connection identifier previous

-- | Seed an optimistic overlay before sending. Nothing selects the reference
-- twenty-second display-error deadline; it does not change an RPC deadline.
registerOptimisticSubmission :: DaemonConnection -> Text -> Text -> Text -> AddUserMessageParams -> Maybe Int -> IO ()
registerOptimisticSubmission connection@(DaemonConnection _ state) identifier requestId placeholder input deadline = do
  now <- monotonicMicros
  atomically $ do
    checkConnection connection
    states <- readTVar (connectionSessionStates state)
    let previous = Map.findWithDefault SessionState.emptySessionState identifier states
    next <- either throwSTM pure (SessionState.registerSubmissionAt now (Just (fromMaybe 20000000 deadline)) requestId placeholder input previous)
    writeTVar (connectionSessionStates state) (Map.insert identifier next states)

-- | Record an externally established confirmation in local state only. This
-- does not synthesize a message, settle a waiting RPC or acknowledge remote work.
confirmOptimisticSubmission :: DaemonConnection -> Text -> Text -> IO Bool
confirmOptimisticSubmission = updateOptimisticSubmission SessionState.confirmSubmission

-- | Remove only local optimistic state. This does not undo a sent request.
cancelOptimisticSubmission :: DaemonConnection -> Text -> Text -> IO Bool
cancelOptimisticSubmission = updateOptimisticSubmission SessionState.cancelSubmission

updateOptimisticSubmission :: (Text -> SessionState.SessionState -> SessionState.SessionState) -> DaemonConnection -> Text -> Text -> IO Bool
updateOptimisticSubmission update connection@(DaemonConnection _ state) identifier requestId = atomically $ do
  checkConnection connection
  states <- readTVar (connectionSessionStates state)
  let previous = Map.findWithDefault SessionState.emptySessionState identifier states
      existed = isJust (SessionState.lookupSubmission requestId previous)
  when existed (writeTVar (connectionSessionStates state) (Map.insert identifier (update requestId previous) states))
  pure existed

-- | Atomically remove this session's overlays and return their count. RPCs
-- remain caller-owned, and other sessions' overlays are unaffected.
cancelSessionOptimisticSubmissions :: DaemonConnection -> Text -> IO Int
cancelSessionOptimisticSubmissions connection@(DaemonConnection _ state) identifier = atomically $ do
  checkConnection connection
  states <- readTVar (connectionSessionStates state)
  let previous = Map.findWithDefault SessionState.emptySessionState identifier states
      count = length (SessionState.optimisticSubmissions previous)
  when (count > 0) (writeTVar (connectionSessionStates state) (Map.insert identifier (SessionState.cancelSessionSubmissions previous) states))
  pure count

-- Register both listeners before sending: the selected notification may precede
-- the immediate ACK. ACK and completion share the caller's deadline.
awaitDaemonCommand :: forall event. (FromJSON event) => Proxy event -> Text -> RpcDispatcher -> Client.CallOptions -> Text -> IO Object -> IO Object
awaitDaemonCommand _ kind dispatcher options identifier request = do
  completed <- newEmptyTMVarIO
  failed <- newEmptyTMVarIO
  let observe notification = case notificationPayload identifier notification of
        Right (Just payload)
          | KeyMap.lookup "type" payload == Just (String kind),
            KeyMap.lookup "requestId" payload == Just (String (Client.callRequestId options)) -> do
              let parsed = parseEither parseJSON (Object payload) :: Either String event
              atomically (void (tryPutTMVar completed (either (const (Left Core.DroidInvalidEvent)) (const (Right ())) parsed)))
        _ -> pure ()
      exchange = bracket (onRpcNotification dispatcher observe) id $ \_ ->
        bracket (onRpcError dispatcher (atomically . void . tryPutTMVar failed)) id $ \_ -> do
          result <- request
          when (KeyMap.lookup "accepted" result == Just (Bool True)) $ do
            settlement <- atomically ((Right <$> readTMVar completed) `orElse` (Left <$> readTMVar failed))
            either throwIO (either throwIO pure) settlement
          pure result
  result <- maybe (Just <$> exchange) (`timeout` exchange) (Client.callTimeoutMicros options)
  maybe (throwIO RpcRequestTimedOut) pure result

-- Prefer the execution session, then permission associations in reported order.
-- Questions never use association routing; replies retain the execution ID.
sessionReply :: DaemonConnection -> Bool -> (DroidHandlers -> RpcRequestHandler) -> (Interaction.PendingInteractions -> STM Bool -> Text -> DroidHandlers -> JsonRpcBaseRequest -> IO Interaction.PreparedInteraction) -> RpcRequestPreparer
sessionReply owned@(DaemonConnection _ state) allowAssociated render managed origin request = case baseRequestParams (envelopeBody request) of
  Just params -> case parseEither (withObject "daemon interaction" (.: "sessionId")) params of
    Right (identifier :: Text) -> do
      when (origin == RpcLiveRequest) $ atomically $ do
        open <- connectionAuthorityOpen owned
        when open $ modifyTVar' (connectionLoads state) (Map.adjust (\entry -> entry {entryCompletedRequests = Set.delete (baseRequestMethod (envelopeBody request), baseRequestId (envelopeBody request)) (entryCompletedRequests entry), entryRetiringRequests = Set.delete (baseRequestId (envelopeBody request)) (entryRetiringRequests entry)}) identifier)
      let associated = case parseEither (withObject "permission associations" (.:! "associatedSessionIds")) params of
            Right (Just identifiers) | allowAssociated -> identifiers
            _ -> []
          allowed = connectionAuthorityOpen owned
          fallback = Just <$> render defaultDroidHandlers request
      (selected, handlers, authenticated) <- atomically $ do
        bindings <- readTVar (connectionBindings state)
        let selected = listToMaybe [binding | candidate <- identifier : associated, Just binding <- [Map.lookup candidate bindings]]
        handlers <- maybe (pure defaultDroidHandlers) (readTVar . bindingHandlers) selected
        authenticated <- allowed
        pure (selected, handlers, authenticated)
      interaction <- if authenticated then managed (connectionInteractions state) allowed identifier handlers request else pure (Interaction.PreparedInteraction (RpcPreparedRequest fallback (pure ())) fallback)
      let prepared = Interaction.preparedInteractionRequest interaction
          action = case selected of
            Nothing -> runPreparedRequest prepared
            Just binding ->
              Core.withSessionUse (bindingSession binding) (runPreparedRequest prepared)
                `catch` \cause -> case cause of
                  Core.DroidSessionUnusable -> Interaction.cancelPreparedInteraction interaction
                  _ -> throwIO cause
          decorate =
            fmap
              ( \case
                  Right (Object fields) -> Right (Object (KeyMap.insert "sessionId" (String identifier) fields))
                  Left err -> Left err
                  Right _ -> Left (JsonRpcError RpcInternalError "Invalid interaction response" Nothing mempty)
              )
          settle = do
            response <- action
            forM_ response (\_ -> atomically (completeInteractionRequest state identifier (baseRequestMethod (envelopeBody request)) (baseRequestId (envelopeBody request))))
            pure response
      pure (prepared {runPreparedRequest = decorate <$> settle})
    Left _ -> pure invalid
  Nothing -> pure invalid
  where
    invalid = RpcPreparedRequest (pure (Just (Left (JsonRpcError RpcInvalidParams "Missing execution session" Nothing mempty)))) (pure ())

-- Association-only duplicates refresh the pending view without rerunning an
-- admitted callback. Metadata watchers observe this update through the snapshot.
observePermissionReplay :: DaemonConnection -> JsonRpcMessage -> IO ()
observePermissionReplay owned@(DaemonConnection _ state) message = case envelopeBody message of
  RequestBody request
    | baseRequestMethod request == "daemon.request_permission",
      Just (Object params) <- baseRequestParams request,
      Just (String execution) <- KeyMap.lookup "sessionId" params -> atomically $ do
        allowed <- connectionAuthorityOpen owned
        when allowed (refreshPermissionRequest state execution (baseRequestId request) params)
  _ -> pure ()

refreshPermissionRequest :: DaemonContext -> Text -> Text -> Object -> STM ()
refreshPermissionRequest state execution identifier fields = case parseEither parseJSON (Object fields) of
  Right params -> Interaction.refreshPendingPermission (connectionInteractions state) execution identifier params
  Left _ -> pure ()

notificationPayload :: Text -> JsonRpcBaseNotification -> Either String (Maybe Object)
notificationPayload identifier notification
  | baseNotificationMethod (envelopeBody notification) /= "daemon.session_notification" = Right Nothing
  | otherwise = case baseNotificationParams (envelopeBody notification) of
      Nothing -> Left "Missing notification parameters"
      Just params ->
        parseEither
          ( withObject "daemon notification" $ \fields -> do
              actual <- fields .: "sessionId"
              if actual /= identifier then pure Nothing else Just <$> fields .: "notification"
          )
          params

observeLifecycle :: DaemonConnection -> JsonRpcBaseNotification -> IO ()
observeLifecycle owned@(DaemonConnection connection state) notification =
  forM_ (baseNotificationParams (envelopeBody notification)) $ \params ->
    forM_ (parseEither (withObject "lifecycle session" (.: "sessionId")) params) $ \identifier ->
      case notificationPayload identifier notification of
        Right (Just payload) -> do
          let kind = KeyMap.lookup "type" payload
              updateWorking change = modifyTVar' (connectionLoads state) $ Map.adjust (\entry -> if readinessKnown (entryReadiness entry) then entry {entryReadiness = change (entryReadiness entry)} else entry) identifier
          case kind of
            Just (String value) | value `elem` ["session_closed", "session_unsubscribed", "session_inactivity", "session_process_exited"] -> do
              atomically $ do
                if value `elem` ["session_inactivity", "session_process_exited"]
                  then do
                    Interaction.markPendingInactive (connectionInteractions state) identifier
                    retiring <- Interaction.inactivePendingRequestIds (connectionInteractions state) identifier
                    rememberRetiringRequests state identifier retiring
                  else Interaction.clearSessionPendingInteractions (connectionInteractions state) identifier
                invalidateSessionLoad state identifier (value == "session_closed")
                modifySessionState (connectionSessionStates state) identifier SessionState.cancelSessionSubmissions
                when (value == "session_closed") (retireChildState state identifier)
              binding <- atomically (Map.lookup identifier <$> readTVar (connectionBindings state))
              forM_ binding (detachBinding False owned)
            Just (String "session_working_directory_changed") ->
              atomically $ case parseEither parseJSON (Object payload) of
                Right event -> modifySessionState (connectionSessionStates state) identifier (SessionState.observeWorkingDirectory (Just (updatedWorkingDirectory event)))
                Left _ -> modifySessionState (connectionSessionStates state) identifier SessionState.invalidateWorkingDirectory
            Just (String "child_session_available") ->
              case parseEither parseJSON (Object payload) of
                Right available | validChildIdentity identifier && validChildIdentity (availableChildSessionId available) -> do
                  registered <- atomically (registerChildMetadata state identifier available)
                  when (registered && connectionHydrateChildren state) (startChildHydration owned (availableChildSessionId available))
                _ -> atomically (modifySessionState (connectionSessionStates state) identifier (SessionState.invalidateMessageState SessionState.MalformedMessageEvent))
            Just (String value) | value `elem` ["daemon.terminal_data", "daemon.terminal_exit"] ->
              case parseEither parseJSON (Object payload) of
                Right (Terminal.TerminalExitEvent event) -> atomically (modifySessionState (connectionSessionStates state) identifier (SessionState.observeTerminalExit event))
                Right (Terminal.TerminalDataEvent event) -> do
                  atomically (modifySessionState (connectionSessionStates state) identifier (SessionState.appendTerminalBufferedData (Terminal.terminalDataId event) (Terminal.terminalDataText event)))
                  binding <- atomically (Map.lookup identifier <$> readTVar (connectionBindings state))
                  forM_ binding $ \current ->
                    Core.withSessionUse (bindingSession current) $
                      withMVar (bindingTerminalWriteLock current) (\_ -> flushTerminalWriter owned current (Terminal.terminalDataId event) (Just (Terminal.terminalDataText event)))
                Left _ -> atomically (modifySessionState (connectionSessionStates state) identifier (SessionState.invalidateTerminalState SessionState.MalformedTerminalEvent))
            Just (String value) | value `elem` ["create_message", "assistant_text_delta", "assistant_text_complete", "thinking_text_delta", "thinking_text_complete", "tool_call", "tool_result", "assistant_message_retracted", "tool_progress_update", "tool_execution_phase_changed", "llm_retry", "droid_working_state_changed", "agent_turn_completed", "error", "permission_resolved", "hook_execution_started", "hook_execution_completed", "session_compacted", "queued_messages_discarded"] -> do
              wall <- (* 1000) . realToFrac <$> getPOSIXTime
              monotonic <- monotonicMicros
              atomically $ case decodeDaemonNotification identifier Nothing notification of
                Right events -> do
                  modifySessionState (connectionSessionStates state) identifier (\previous -> foldl' (flip (SessionState.applySessionEventAt wall monotonic)) previous events)
                  updateWorking (\readiness -> foldl' applyWorkingEvent readiness events)
                  forM_ events $ \case
                    PermissionEvent resolved -> do
                      retireRestoredPermission state identifier (resolvedRequestId resolved)
                      resolvedPending <- Interaction.recordPermissionResolved (connectionInteractions state) identifier (resolvedRequestId resolved)
                      completeInteractionRequest state identifier "daemon.request_permission" (resolvedRequestId resolved)
                      forM_ resolvedPending (\pending -> completeInteractionRequest state (Interaction.pendingSessionId pending) "daemon.request_permission" (resolvedRequestId resolved))
                      modifyTVar' (connectionSessionStates state) (Map.map (SessionState.retireDeferredPermissions identifier (resolvedRequestId resolved) (resolvedToolUseIds resolved)))
                    _ -> pure ()
                  when (any isTurnCompletion events) $ do
                    (_, settings) <- readTVar (Core.connectionSettings connection)
                    case Map.lookup identifier settings of
                      Just (Right current)
                        | isJust (settingsTags current >>= findSubagentSessionTag) ->
                            modifySessionState (connectionSessionStates state) identifier SessionState.refreshInvocationSummary
                      _ -> pure ()
                Left _ -> do
                  modifySessionState (connectionSessionStates state) identifier (SessionState.invalidateMessageState SessionState.MalformedMessageEvent)
                  when (value == "droid_working_state_changed") (updateWorking (\readiness -> readiness {readinessWorkingState = Left Core.DroidInvalidEvent}))
                  when (value == "create_message") $ case KeyMap.lookup "requestId" payload of
                    Just (String requestId) -> modifySessionState (connectionSessionStates state) identifier (SessionState.failSubmission requestId SessionState.SubmissionInvalidConfirmation)
                    _ -> pure ()
            _ -> pure ()
        _ -> pure ()

isTurnCompletion :: DroidEvent -> Bool
isTurnCompletion (TurnCompletedEvent _) = True
isTurnCompletion _ = False

retireChildState :: DaemonContext -> Text -> STM ()
retireChildState state identifier = do
  modifySessionState (connectionSessionStates state) identifier SessionState.clearChildLink
  modifyTVar' (connectionChildOrder state) (filter (/= identifier))

applyWorkingEvent :: SessionReadiness -> DroidEvent -> SessionReadiness
applyWorkingEvent readiness event = readiness {readinessWorkingState = observed}
  where
    previous = readinessWorkingState readiness
    observed = case event of
      WorkingStateEvent changed -> Right (Just (workingStateNewState changed))
      TextDeltaEvent _ | previous == Right (Just WorkingThinking) -> Right (Just WorkingStreamingAssistantMessage)
      ThinkingDeltaEvent _ | previous == Right (Just WorkingStreamingAssistantMessage) -> Right (Just WorkingThinking)
      ToolResultEvent _ -> Right (Just WorkingExecutingTool)
      PermissionEvent _ | previous == Right (Just WorkingWaitingForToolConfirmation) -> Right (Just WorkingStreamingAssistantMessage)
      ErrorEvent _ -> Right (Just WorkingIdle)
      _ -> previous

sendPrompt :: DaemonSession -> Text -> (Text -> IO ()) -> IO DroidResult
sendPrompt = Core.sendPrompt . daemonCoreSession

sendTurn :: DaemonSession -> Text -> (Text -> IO ()) -> IO DroidResult
sendTurn = Core.sendDroidTurn . daemonCoreSession

sendEvents :: DaemonSession -> DroidStreamMode -> Text -> (DroidEvent -> IO ()) -> IO DroidResult
sendEvents = Core.sendDroidEvents . daemonCoreSession

sendInput :: DaemonSession -> DroidInput -> (Text -> IO ()) -> IO DroidResult
sendInput = Core.sendDroidInput . daemonCoreSession

sendInputEvents :: DaemonSession -> DroidStreamMode -> DroidInput -> (DroidEvent -> IO ()) -> IO DroidResult
sendInputEvents = Core.sendDroidInputEvents . daemonCoreSession

sendOutput :: DaemonSession -> DroidOutput a -> Text -> (Text -> IO ()) -> IO (DroidOutputResult a)
sendOutput = Core.sendDroidOutput . daemonCoreSession

sendOutputEvents :: DaemonSession -> DroidOutput a -> DroidStreamMode -> Text -> (DroidEvent -> IO ()) -> IO (DroidOutputResult a)
sendOutputEvents = Core.sendDroidOutputEvents . daemonCoreSession

sendInputOutput :: DaemonSession -> DroidOutput a -> DroidInput -> (Text -> IO ()) -> IO (DroidOutputResult a)
sendInputOutput = Core.sendDroidInputOutput . daemonCoreSession

sendInputOutputEvents :: DaemonSession -> DroidOutput a -> DroidStreamMode -> DroidInput -> (DroidEvent -> IO ()) -> IO (DroidOutputResult a)
sendInputOutputEvents = Core.sendDroidInputOutputEvents . daemonCoreSession

interruptSession :: DaemonSession -> IO ()
interruptSession = Core.interruptDroidSession . daemonCoreSession

-- | Runs on dispatcher intake. Keep callbacks brief; do not start a turn,
-- interrupt an incompletely submitted turn, or wait for further intake here.
-- Turn callbacks passed to sendEvents run on the caller's thread instead.
onSessionEvent :: DaemonSession -> (Either Core.DroidError DroidEvent -> IO ()) -> IO (IO ())
onSessionEvent = Core.onDroidSessionEvent . daemonCoreSession

listMcpServers :: DaemonSession -> IO ListMcpServersResult
listMcpServers session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_mcp_servers" Object))) session (mempty :: Object)

listMcpTools :: DaemonSession -> IO ListMcpToolsResult
listMcpTools session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_mcp_tools" Object))) session (mempty :: Object)

listMcpRegistry :: DaemonSession -> IO ListMcpRegistryResult
listMcpRegistry session = daemonSessionRequest Core.ReadOnlyRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.list_mcp_registry" Object))) session (mempty :: Object)

addMcpServer :: DaemonSession -> AddMcpServerParams -> IO SuccessResult
addMcpServer session params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.add_mcp_server" Object))) session validated

removeMcpServer :: DaemonSession -> Text -> IO SuccessResult
removeMcpServer session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.remove_mcp_server" Object))) session (RemoveMcpServerParams name mempty)

toggleMcpServer :: DaemonSession -> Text -> Bool -> IO SuccessResult
toggleMcpServer session name enabled = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.toggle_mcp_server" Object))) session (ToggleMcpServerParams name enabled mempty)

toggleMcpTool :: DaemonSession -> Text -> Text -> Bool -> IO SuccessResult
toggleMcpTool session name tool enabled = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.toggle_mcp_tool" Object))) session (ToggleMcpToolParams name tool enabled mempty)

-- | Five-minute RPC budget. The acknowledgement is not proof of authentication
-- completion, and timeout does not automatically cancel shared server-side OAuth.
authenticateMcpServer :: DaemonSession -> Text -> IO SuccessResult
authenticateMcpServer session name = daemonSessionRequest Core.MutatingRequest 300000000 (Proxy @(WithEnvelope (MethodRequest "daemon.authenticate_mcp_server" Object))) session (McpServerNameParams name mempty)

cancelMcpAuth :: DaemonSession -> Text -> IO SuccessResult
cancelMcpAuth session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.cancel_mcp_auth" Object))) session (McpServerNameParams name mempty)

clearMcpAuth :: DaemonSession -> Text -> IO SuccessResult
clearMcpAuth session name = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.clear_mcp_auth" Object))) session (McpServerNameParams name mempty)

submitMcpAuthCode :: DaemonSession -> SubmitMcpAuthCodeParams -> IO SuccessResult
submitMcpAuthCode = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.submit_mcp_auth_code" Object)))

submitMcpAuthError :: DaemonSession -> SubmitMcpAuthErrorParams -> IO SuccessResult
submitMcpAuthError = daemonSessionRequest Core.MutatingRequest 30000000 (Proxy @(WithEnvelope (MethodRequest "daemon.submit_mcp_auth_error" Object)))

daemonSessionRequest :: (KnownSymbol method, ToJSON params, FromJSON result) => Core.RequestEffect -> Int -> Proxy (WithEnvelope (MethodRequest method Object)) -> DaemonSession -> params -> IO result
daemonSessionRequest effect deadline method attached params =
  let session = daemonCoreSession attached
   in Core.sessionRequestWithTimeout effect deadline session $ \channel options -> do
        fields <- either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (toJSON params))
        Client.call method channel options (KeyMap.insert "sessionId" (String (Core.droidSessionId session)) fields)
