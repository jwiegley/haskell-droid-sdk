{-# LANGUAGE OverloadedStrings #-}

-- | Typed low-level local and daemon operations with explicit wire contracts.
-- Calls use the existing channel and caller-supplied IDs, envelope context and
-- deadlines. This is not a launcher, default identity policy or session owner.
module Factory.Droid.Client
  ( CallOptions (..),
    call,
    callObserved,
    callWithHookPolicy,
    callObservedWithHookPolicy,
    initializeSession,
    initializeDaemonSession,
    loadSession,
    loadDaemonSession,
    callWithAdmission,
    callObservedWithAdmission,
    addMcpServer,
    getDaemonMcpConfig,
    updateDaemonMcpConfig,
    getDaemonDefaultSettings,
    updateDaemonSessionDefaults,
    listDaemonCustomModels,
    upsertDaemonCustomModel,
    deleteDaemonCustomModel,
    logoutDaemon,
    listDaemonCrons,
    createDaemonCron,
    updateDaemonCron,
    deleteDaemonCron,
    holdDaemonSessionCrons,
    resumeDaemonSessionCrons,
    listDaemonSfWorkstreams,
    getDaemonSfWorkstream,
    createDaemonSfWorkstream,
    updateDaemonSfWorkstream,
    deleteDaemonSfWorkstream,
    publishDaemonSfWorkstreamContent,
    hydrateDaemonSfWorkstreamContent,
    listDaemonSfSignals,
    listDaemonSfChanges,
    listDaemonSfActivities,
    resolveDaemonSfActivityReview,
    listDaemonSfEvents,
    markDaemonSfEventsRead,
    markDaemonSfEventsUnread,
    triggerDaemonUpdate,
    installDaemonSshKey,
    getDaemonProxyToken,
    startDaemonRelay,
    stopDaemonRelay,
    getDaemonRelayStatus,
    listDaemonGitBranches,
    checkoutDaemonGitBranch,
    listDaemonWorktreeSetupProfiles,
    saveDaemonWorktreeSetupProfile,
    deleteDaemonWorktreeSetupProfile,
    listDaemonManagedWorktrees,
    cleanupDaemonWorktree,
    inspectDaemonWorktreeDeletion,
    getDaemonGitBranchDivergence,
    getDaemonGitDiff,
    resolveDaemonPullRequestStatuses,
    inspectDaemonMissionReadiness,
    acknowledgeDaemonMissionReadinessWarning,
    pushDaemonGit,
    commitDaemonGit,
    createDaemonPullRequest,
    getDaemonSemanticDiffCache,
    saveDaemonSemanticDiffCache,
    generateDaemonSemanticDiff,
    getDaemonRewindInfo,
    executeDaemonRewind,
    compactDaemonSession,
    forkDaemonSession,
    killDaemonWorkerSession,
    closeDaemonSession,
    submitDaemonBugReport,
    listDaemonAutomations,
    runDaemonAutomation,
    pauseDaemonAutomation,
    resumeDaemonAutomation,
    getDaemonAutomationHistory,
    getDaemonAutomationVisual,
    renameDaemonAutomation,
    deleteDaemonAutomation,
    createDaemonAutomation,
    forkDaemonAutomation,
    updateDaemonAutomationModel,
    updateDaemonAutomationPrivacy,
    updateDaemonAutomationPrompt,
    updateDaemonAutomationSchedule,
    applyDaemonAutomationConfig,
    updateDaemonAutomation,
    listDaemonAvailablePlugins,
    listDaemonInstalledPlugins,
    installDaemonPlugin,
    uninstallDaemonPlugin,
    setDaemonPluginEnabled,
    updateDaemonPlugin,
    listDaemonMarketplaces,
    addDaemonMarketplace,
    removeDaemonMarketplace,
    updateDaemonMarketplace,
    resolveDaemonQueuedUserMessageRaw,
    updateDaemonSessionSettingsRaw,
    renameDaemonSessionRaw,
    listDaemonSkills,
    listDaemonCommands,
    getDaemonContextBreakdown,
    setDaemonSkillDisabled,
    listDaemonOpenedSessions,
    listDaemonAvailableSessions,
    listDaemonModels,
    getDaemonSessionMessages,
    searchDaemonSessions,
    archiveDaemonSession,
    unarchiveDaemonSession,
    checkDaemonFolderTrust,
    trustDaemonFolder,
    validateDaemonWorkingDirectory,
    changeDaemonWorkingDirectory,
    listDaemonFiles,
    searchDaemonFiles,
    getDaemonWorkspaceFileContent,
    writeDaemonWorkspaceFileContent,
    pushDaemonCwdFileToUrl,
    pullDaemonUrlToCwdFile,
    createDaemonTerminal,
    writeDaemonTerminalData,
    resizeDaemonTerminal,
    closeDaemonTerminal,
    listDaemonTerminals,
    addUserMessage,
    appendMessages,
    authenticateMcpServer,
    cancelMcpAuth,
    changeWorkingDirectory,
    clearMcpAuth,
    closeSession,
    compactSession,
    executeRewind,
    forkSession,
    getContextBreakdown,
    getContextStats,
    getRewindInfo,
    interruptSession,
    killWorkerSession,
    listCommands,
    listMcpRegistry,
    listMcpServers,
    listMcpTools,
    listModels,
    listSkills,
    listTools,
    removeMcpServer,
    renameSession,
    resolveQueuedUserMessage,
    setSkillDisabled,
    submitBugReport,
    submitMcpAuthCode,
    submitMcpAuthError,
    toggleMcpServer,
    toggleMcpTool,
    updateSessionSettings,
    warmupCache,
  )
where

import Control.Concurrent.STM (STM)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Protocol (RpcChannel, RpcHookPolicy (..), RpcResultError (RpcInvalidResult), decodeRpcResult, requestReplyObservedAtWithAdmission, requestReplyWithAdmission, requestResult)
import Factory.Droid.Schema.Configuration (DaemonInitializeSessionParams, DaemonLoadSessionParams, InitializeSessionParams, LoadSessionParams, validateDaemonInitializationParams, validateDaemonLoadSessionParams, validateInitializationParams, validateLoadSessionParams)
import Factory.Droid.Schema.Context (ContextStats, GetContextBreakdownResult)
import Factory.Droid.Schema.Control
  ( AddUserMessageParams,
    AppendMessagesParams,
    ChangeWorkingDirectoryParams,
    ChangeWorkingDirectoryResult,
    CloseSessionParams,
    CompactSessionParams,
    CompactSessionResult,
    ExecuteRewindParams,
    ExecuteRewindResult,
    ForkSessionParams,
    ForkSessionResult,
    GetRewindInfoParams,
    GetRewindInfoResult,
    KillWorkerSessionParams,
    RenameSessionParams,
    ResolveQueuedMessageParams,
    SubmitBugReportParams (..),
    SubmitBugReportResult,
    ValidateWorkingDirectoryResult,
  )
import Factory.Droid.Schema.Daemon.Automation (ApplyAutomationConfigParams, ApplyAutomationConfigResult, AutomationAddress, AutomationCreationResult, AutomationHistoryParams, AutomationHistoryResult, AutomationListParams, AutomationStatusResult, AutomationVisualParams, AutomationVisualResult, CreateAutomationParams, ForkAutomationParams, ListAutomationsResult, RenameAutomationParams, RunAutomationParams, RunAutomationResult, UpdateAutomationModelParams, UpdateAutomationParams, UpdateAutomationPrivacyParams, UpdateAutomationPromptParams, UpdateAutomationScheduleParams)
import Factory.Droid.Schema.Daemon.Cron (CreateCronParams, CreateCronResult, DeleteCronParams, DeleteCronResult, HoldSessionCronsParams, HoldSessionCronsResult, ListCronsParams, ListCronsResult, ResumeSessionCronsResult, UpdateCronParams, UpdateCronResult)
import Factory.Droid.Schema.Daemon.Git (CheckoutBranchParams, CheckoutBranchResult, CreatePullRequestParams, CreatePullRequestResult, GenerateSemanticDiffParams, GenerateSemanticDiffResult, GitBranchDivergence, GitBranchParams, GitCommitParams, GitDiffParams, GitDiffResult, GitDirectoryParams, ListGitBranchesResult, MissionReadinessResult, ResolvePullRequestStatusesParams, ResolvePullRequestStatusesResult, SaveSemanticDiffParams, SemanticDiffCacheResult, SemanticDiffTarget)
import Factory.Droid.Schema.Daemon.Management (InstallSshKeyParams, InstallSshKeyResult, ProxyTokenResult, RelayStartResult, RelayStatus, RelayStopResult, TriggerUpdateResult)
import Factory.Droid.Schema.Daemon.Plugin (AddMarketplaceParams, AddMarketplaceResult, InstallPluginParams, InstallPluginResult, ListAvailablePluginsResult, ListInstalledPluginsResult, ListMarketplacesResult, MarketplaceNameParams, PluginScopeParams, PluginTargetParams, SetPluginEnabledParams, UpdateMarketplaceParams, UpdateMarketplaceResult, UpdatePluginParams, UpdatePluginResult)
import Factory.Droid.Schema.Daemon.Session (ArchiveSessionParams, ArchiveSessionResult, DaemonCloseSessionParams, GetSessionMessagesParams, GetSessionMessagesResult, ListAvailableSessionsParams, ListAvailableSessionsResult, ListOpenedSessionsParams, ListOpenedSessionsResult, SearchSessionsParams, SearchSessionsResult)
import Factory.Droid.Schema.Daemon.Settings (DefaultSettings, DeleteCustomModelParams, ListCustomModelsResult, UpdateCustomModelsResult, UpdateSessionDefaultsParams, UpdateSessionDefaultsResult, UpsertCustomModelParams)
import Factory.Droid.Schema.Daemon.SoftwareFactory (SfCreateWorkstreamParams, SfDeleteWorkstreamResult, SfGetWorkstreamResult, SfHydrateWorkstreamContentParams, SfListActivitiesParams, SfListActivitiesResult, SfListChangesParams, SfListChangesResult, SfListEventsParams, SfListEventsResult, SfListSignalsParams, SfListSignalsResult, SfListWorkstreamsParams, SfListWorkstreamsResult, SfMarkEventsReadParams, SfMarkEventsUnreadParams, SfMarkedEventsResult, SfPublishWorkstreamContentParams, SfResolveActivityReviewParams, SfResolveActivityReviewResult, SfUpdateWorkstreamParams, SfWorkstreamContentResult, SfWorkstreamResult, SfWorkstreamTarget, parseSfPublishedWorkstreamContentResult)
import Factory.Droid.Schema.Daemon.Terminal (CreateTerminalResult, DaemonCloseTerminalParams, DaemonCreateTerminalParams, DaemonListTerminalsParams, DaemonResizeTerminalParams, DaemonWriteTerminalDataParams, ListTerminalsResult)
import Factory.Droid.Schema.Daemon.Workspace (ChangeSessionWorkingDirectoryParams, CheckFolderTrustParams, CheckFolderTrustResult, GetWorkspaceFileContentParams, GetWorkspaceFileContentResult, ListFilesParams (..), ListFilesResult, PullUrlToCwdFileParams, PullUrlToCwdFileResult, PushCwdFileToUrlParams, PushCwdFileToUrlResult, SearchFilesParams (..), SearchFilesResult, TrustFolderParams, TrustFolderResult, WriteWorkspaceFileContentParams, WriteWorkspaceFileContentResult)
import Factory.Droid.Schema.Daemon.Worktree (CleanupWorktreeParams, CleanupWorktreeResult, DeleteWorktreeProfileParams, InspectWorktreeDeletionParams, InspectWorktreeDeletionResult, ListManagedWorktreesParams, ListManagedWorktreesResult, ListWorktreeProfilesParams, ListWorktreeProfilesResult, SaveWorktreeProfileParams, SaveWorktreeProfileResult, parseSavedWorktreeSetupProfileResult, parseWorktreeSetupProfilesResult, validateSaveWorktreeProfileParams)
import Factory.Droid.Schema.Discovery (ListCommandsResult, ListSkillsResult, ListToolsResult, SetSkillDisabledParams)
import Factory.Droid.Schema.Local
import Factory.Droid.Schema.MCP
  ( ListMcpRegistryResult,
    ListMcpServersResult,
    ListMcpToolsResult,
    McpServerNameParams,
    RemoveMcpServerParams,
    SubmitMcpAuthCodeParams,
    SubmitMcpAuthErrorParams,
    ToggleMcpServerParams,
    ToggleMcpToolParams,
  )
import Factory.Droid.Schema.MCP.Config (AddMcpServerParams, GetMcpConfigResult, UpdateMcpConfigParams, UpdateMcpConfigResult, validateMcpConfiguration)
import Factory.Droid.Schema.Models (ListModelsOptions, ListModelsResult)
import Factory.Droid.Schema.RPC (BaseRequest (..), CommandAck, EmptyObject, JsonRpcEnvelope, MethodRequest (..), SuccessOrErrorResult, SuccessResult, WithEnvelope (..), eraseMethodRequest)
import Factory.Droid.Schema.Session (SessionIdParams)
import Factory.Droid.Schema.Settings (ListToolsOptions, UpdateSessionSettingsParams)
import Factory.Droid.Schema.Sources (validateBugReportSource)
import GHC.TypeLits (KnownSymbol)

-- | Per-call identity, caller-owned metadata/extensions and optional exchange
-- deadline in microseconds. IDs must remain unique within the connection.
-- Nothing disables the deadline; zero expires without sending. Show redacts
-- all fields. No SDK attribution or default timeout is inferred.
data CallOptions = CallOptions
  { callRequestId :: !Text,
    callEnvelope :: !JsonRpcEnvelope,
    callTimeoutMicros :: !(Maybe Int)
  }
  deriving stock (Eq)

instance Show CallOptions where
  show _ = "CallOptions <redacted>"

-- | Validate creation parameters before dispatch. Results retain their full
-- object for callers; normal session constructors perform owned publication.
initializeSession :: RpcChannel -> CallOptions -> InitializeSessionParams -> IO Object
initializeSession channel options params = do
  either throwIO pure (validateInitializationParams params)
  call (Proxy @(WithEnvelope (MethodRequest "droid.initialize_session" InitializeSessionParams))) channel options params

initializeDaemonSession :: RpcChannel -> CallOptions -> DaemonInitializeSessionParams -> IO Object
initializeDaemonSession channel options params = do
  either throwIO pure (validateDaemonInitializationParams params)
  call (Proxy @(WithEnvelope (MethodRequest "daemon.initialize_session" DaemonInitializeSessionParams))) channel options params

-- | Load wire parameters cannot carry init-only or post-load restriction fields.
loadSession :: RpcChannel -> CallOptions -> LoadSessionParams -> IO Object
loadSession channel options params = do
  either throwIO pure (validateLoadSessionParams params)
  call (Proxy @(WithEnvelope (MethodRequest "droid.load_session" LoadSessionParams))) channel options params

loadDaemonSession :: RpcChannel -> CallOptions -> DaemonLoadSessionParams -> IO Object
loadDaemonSession channel options params = do
  either throwIO pure (validateDaemonLoadSessionParams params)
  call (Proxy @(WithEnvelope (MethodRequest "daemon.load_session" DaemonLoadSessionParams))) channel options params

addMcpServer :: RpcChannel -> CallOptions -> AddMcpServerParams -> IO SuccessResult
addMcpServer channel options params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  call (Proxy @AddMcpServerRequest) channel options validated

-- | Daemon-global defaults; updates do not acquire or modify a local session.
getDaemonDefaultSettings :: RpcChannel -> CallOptions -> EmptyObject -> IO DefaultSettings
getDaemonDefaultSettings = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_default_settings" EmptyObject)))

updateDaemonSessionDefaults :: RpcChannel -> CallOptions -> UpdateSessionDefaultsParams -> IO UpdateSessionDefaultsResult
updateDaemonSessionDefaults = call (Proxy @(WithEnvelope (MethodRequest "daemon.update_session_defaults" UpdateSessionDefaultsParams)))

listDaemonCustomModels :: RpcChannel -> CallOptions -> EmptyObject -> IO ListCustomModelsResult
listDaemonCustomModels = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_custom_models" EmptyObject)))

upsertDaemonCustomModel :: RpcChannel -> CallOptions -> UpsertCustomModelParams -> IO UpdateCustomModelsResult
upsertDaemonCustomModel = call (Proxy @(WithEnvelope (MethodRequest "daemon.upsert_custom_model" UpsertCustomModelParams)))

deleteDaemonCustomModel :: RpcChannel -> CallOptions -> DeleteCustomModelParams -> IO UpdateCustomModelsResult
deleteDaemonCustomModel = call (Proxy @(WithEnvelope (MethodRequest "daemon.delete_custom_model" DeleteCustomModelParams)))

-- | Explicit logout with closed empty parameters. The reply acknowledges
-- acceptance, not completed revocation or transport shutdown.
logoutDaemon :: RpcChannel -> CallOptions -> IO CommandAck
logoutDaemon channel options = call (Proxy @(WithEnvelope (MethodRequest "daemon.logout" EmptyObject))) channel options mempty

listDaemonCrons :: RpcChannel -> CallOptions -> ListCronsParams -> IO ListCronsResult
listDaemonCrons = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_crons" ListCronsParams)))

createDaemonCron :: RpcChannel -> CallOptions -> CreateCronParams -> IO CreateCronResult
createDaemonCron = call (Proxy @(WithEnvelope (MethodRequest "daemon.create_cron" CreateCronParams)))

updateDaemonCron :: RpcChannel -> CallOptions -> UpdateCronParams -> IO UpdateCronResult
updateDaemonCron = call (Proxy @(WithEnvelope (MethodRequest "daemon.update_cron" UpdateCronParams)))

deleteDaemonCron :: RpcChannel -> CallOptions -> DeleteCronParams -> IO DeleteCronResult
deleteDaemonCron = call (Proxy @(WithEnvelope (MethodRequest "daemon.delete_cron" DeleteCronParams)))

holdDaemonSessionCrons :: RpcChannel -> CallOptions -> HoldSessionCronsParams -> IO HoldSessionCronsResult
holdDaemonSessionCrons = call (Proxy @(WithEnvelope (MethodRequest "daemon.hold_session_crons" HoldSessionCronsParams)))

resumeDaemonSessionCrons :: RpcChannel -> CallOptions -> SessionIdParams -> IO ResumeSessionCronsResult
resumeDaemonSessionCrons = call (Proxy @(WithEnvelope (MethodRequest "daemon.resume_session_crons" SessionIdParams)))

listDaemonSfWorkstreams :: RpcChannel -> CallOptions -> SfListWorkstreamsParams -> IO SfListWorkstreamsResult
listDaemonSfWorkstreams = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.list_workstreams" SfListWorkstreamsParams)))

getDaemonSfWorkstream :: RpcChannel -> CallOptions -> SfWorkstreamTarget -> IO SfGetWorkstreamResult
getDaemonSfWorkstream = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.get_workstream" SfWorkstreamTarget)))

createDaemonSfWorkstream :: RpcChannel -> CallOptions -> SfCreateWorkstreamParams -> IO SfWorkstreamResult
createDaemonSfWorkstream = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.create_workstream" SfCreateWorkstreamParams)))

updateDaemonSfWorkstream :: RpcChannel -> CallOptions -> SfUpdateWorkstreamParams -> IO SfWorkstreamResult
updateDaemonSfWorkstream = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.update_workstream" SfUpdateWorkstreamParams)))

deleteDaemonSfWorkstream :: RpcChannel -> CallOptions -> SfWorkstreamTarget -> IO SfDeleteWorkstreamResult
deleteDaemonSfWorkstream = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.delete_workstream" SfWorkstreamTarget)))

publishDaemonSfWorkstreamContent :: RpcChannel -> CallOptions -> SfPublishWorkstreamContentParams -> IO SfWorkstreamContentResult
publishDaemonSfWorkstreamContent = callParsed parseSfPublishedWorkstreamContentResult (Proxy @(WithEnvelope (MethodRequest "daemon.sf.publish_workstream_content" SfPublishWorkstreamContentParams)))

hydrateDaemonSfWorkstreamContent :: RpcChannel -> CallOptions -> SfHydrateWorkstreamContentParams -> IO SfWorkstreamContentResult
hydrateDaemonSfWorkstreamContent = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.hydrate_workstream_content" SfHydrateWorkstreamContentParams)))

listDaemonSfSignals :: RpcChannel -> CallOptions -> SfListSignalsParams -> IO SfListSignalsResult
listDaemonSfSignals = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.list_signals" SfListSignalsParams)))

listDaemonSfChanges :: RpcChannel -> CallOptions -> SfListChangesParams -> IO SfListChangesResult
listDaemonSfChanges = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.list_changes" SfListChangesParams)))

listDaemonSfActivities :: RpcChannel -> CallOptions -> SfListActivitiesParams -> IO SfListActivitiesResult
listDaemonSfActivities = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.list_activities" SfListActivitiesParams)))

resolveDaemonSfActivityReview :: RpcChannel -> CallOptions -> SfResolveActivityReviewParams -> IO SfResolveActivityReviewResult
resolveDaemonSfActivityReview = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.resolve_activity_review" SfResolveActivityReviewParams)))

listDaemonSfEvents :: RpcChannel -> CallOptions -> SfListEventsParams -> IO SfListEventsResult
listDaemonSfEvents = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.list_events" SfListEventsParams)))

markDaemonSfEventsRead :: RpcChannel -> CallOptions -> SfMarkEventsReadParams -> IO SfMarkedEventsResult
markDaemonSfEventsRead = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.mark_events_read" SfMarkEventsReadParams)))

markDaemonSfEventsUnread :: RpcChannel -> CallOptions -> SfMarkEventsUnreadParams -> IO SfMarkedEventsResult
markDaemonSfEventsUnread = call (Proxy @(WithEnvelope (MethodRequest "daemon.sf.mark_events_unread" SfMarkEventsUnreadParams)))

triggerDaemonUpdate :: RpcChannel -> CallOptions -> EmptyObject -> IO TriggerUpdateResult
triggerDaemonUpdate = call (Proxy @(WithEnvelope (MethodRequest "daemon.trigger_update" EmptyObject)))

installDaemonSshKey :: RpcChannel -> CallOptions -> InstallSshKeyParams -> IO InstallSshKeyResult
installDaemonSshKey = call (Proxy @(WithEnvelope (MethodRequest "daemon.install_ssh_key" InstallSshKeyParams)))

listDaemonGitBranches :: RpcChannel -> CallOptions -> GitDirectoryParams -> IO ListGitBranchesResult
listDaemonGitBranches = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_git_branches" GitDirectoryParams)))

checkoutDaemonGitBranch :: RpcChannel -> CallOptions -> CheckoutBranchParams -> IO CheckoutBranchResult
checkoutDaemonGitBranch = call (Proxy @(WithEnvelope (MethodRequest "daemon.checkout_git_branch" CheckoutBranchParams)))

-- | Profile operations apply SDK preparation, retaining the separate raw wire
-- codecs. All requests use the existing correlation, hooks and error path.
listDaemonWorktreeSetupProfiles :: RpcChannel -> CallOptions -> ListWorktreeProfilesParams -> IO ListWorktreeProfilesResult
listDaemonWorktreeSetupProfiles = callParsed parseWorktreeSetupProfilesResult (Proxy @(WithEnvelope (MethodRequest "daemon.list_worktree_setup_profiles" ListWorktreeProfilesParams)))

saveDaemonWorktreeSetupProfile :: RpcChannel -> CallOptions -> SaveWorktreeProfileParams -> IO SaveWorktreeProfileResult
saveDaemonWorktreeSetupProfile channel options params = do
  prepared <- either throwIO pure (validateSaveWorktreeProfileParams params)
  callParsed parseSavedWorktreeSetupProfileResult (Proxy @(WithEnvelope (MethodRequest "daemon.save_worktree_setup_profile" SaveWorktreeProfileParams))) channel options prepared

deleteDaemonWorktreeSetupProfile :: RpcChannel -> CallOptions -> DeleteWorktreeProfileParams -> IO SuccessResult
deleteDaemonWorktreeSetupProfile = call (Proxy @(WithEnvelope (MethodRequest "daemon.delete_worktree_setup_profile" DeleteWorktreeProfileParams)))

listDaemonManagedWorktrees :: RpcChannel -> CallOptions -> ListManagedWorktreesParams -> IO ListManagedWorktreesResult
listDaemonManagedWorktrees = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_managed_worktrees" ListManagedWorktreesParams)))

-- | The low-level caller owns the deadline; the Daemon helper selects 180 s.
cleanupDaemonWorktree :: RpcChannel -> CallOptions -> CleanupWorktreeParams -> IO CleanupWorktreeResult
cleanupDaemonWorktree = call (Proxy @(WithEnvelope (MethodRequest "daemon.cleanup_worktree" CleanupWorktreeParams)))

inspectDaemonWorktreeDeletion :: RpcChannel -> CallOptions -> InspectWorktreeDeletionParams -> IO InspectWorktreeDeletionResult
inspectDaemonWorktreeDeletion = call (Proxy @(WithEnvelope (MethodRequest "daemon.inspect_worktree_deletion" InspectWorktreeDeletionParams)))

getDaemonGitBranchDivergence :: RpcChannel -> CallOptions -> GitBranchParams -> IO GitBranchDivergence
getDaemonGitBranchDivergence = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_git_branch_divergence" GitBranchParams)))

getDaemonGitDiff :: RpcChannel -> CallOptions -> GitDiffParams -> IO GitDiffResult
getDaemonGitDiff = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_git_diff" GitDiffParams)))

resolveDaemonPullRequestStatuses :: RpcChannel -> CallOptions -> ResolvePullRequestStatusesParams -> IO ResolvePullRequestStatusesResult
resolveDaemonPullRequestStatuses = call (Proxy @(WithEnvelope (MethodRequest "daemon.resolve_pull_request_statuses" ResolvePullRequestStatusesParams)))

inspectDaemonMissionReadiness :: RpcChannel -> CallOptions -> GitDirectoryParams -> IO MissionReadinessResult
inspectDaemonMissionReadiness = call (Proxy @(WithEnvelope (MethodRequest "daemon.inspect_mission_readiness" GitDirectoryParams)))

acknowledgeDaemonMissionReadinessWarning :: RpcChannel -> CallOptions -> GitDirectoryParams -> IO EmptyObject
acknowledgeDaemonMissionReadinessWarning = call (Proxy @(WithEnvelope (MethodRequest "daemon.acknowledge_mission_readiness_warning" GitDirectoryParams)))

pushDaemonGit :: RpcChannel -> CallOptions -> SessionIdParams -> IO SuccessResult
pushDaemonGit = call (Proxy @(WithEnvelope (MethodRequest "daemon.git_push" SessionIdParams)))

commitDaemonGit :: RpcChannel -> CallOptions -> GitCommitParams -> IO SuccessResult
commitDaemonGit = call (Proxy @(WithEnvelope (MethodRequest "daemon.git_commit" GitCommitParams)))

createDaemonPullRequest :: RpcChannel -> CallOptions -> CreatePullRequestParams -> IO CreatePullRequestResult
createDaemonPullRequest = call (Proxy @(WithEnvelope (MethodRequest "daemon.create_pr" CreatePullRequestParams)))

getDaemonSemanticDiffCache :: RpcChannel -> CallOptions -> SemanticDiffTarget -> IO SemanticDiffCacheResult
getDaemonSemanticDiffCache = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_semantic_diff_cache" SemanticDiffTarget)))

saveDaemonSemanticDiffCache :: RpcChannel -> CallOptions -> SaveSemanticDiffParams -> IO SuccessResult
saveDaemonSemanticDiffCache = call (Proxy @(WithEnvelope (MethodRequest "daemon.save_semantic_diff_cache" SaveSemanticDiffParams)))

-- | The low-level exchange deadline remains caller-owned; the Daemon wrapper
-- supplies the baselined 180-second generation budget.
generateDaemonSemanticDiff :: RpcChannel -> CallOptions -> GenerateSemanticDiffParams -> IO GenerateSemanticDiffResult
generateDaemonSemanticDiff = call (Proxy @(WithEnvelope (MethodRequest "daemon.generate_semantic_diff" GenerateSemanticDiffParams)))

-- | Remote control results do not load or retire SDK session handles.
getDaemonRewindInfo :: RpcChannel -> CallOptions -> GetRewindInfoParams -> IO GetRewindInfoResult
getDaemonRewindInfo = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_rewind_info" GetRewindInfoParams)))

executeDaemonRewind :: RpcChannel -> CallOptions -> ExecuteRewindParams -> IO ExecuteRewindResult
executeDaemonRewind = call (Proxy @(WithEnvelope (MethodRequest "daemon.execute_rewind" ExecuteRewindParams)))

compactDaemonSession :: RpcChannel -> CallOptions -> Text -> CompactSessionParams -> IO CompactSessionResult
compactDaemonSession = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.compact_session" EmptyObject)))

forkDaemonSession :: RpcChannel -> CallOptions -> Text -> ForkSessionParams -> IO ForkSessionResult
forkDaemonSession = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.fork_session" EmptyObject)))

killDaemonWorkerSession :: RpcChannel -> CallOptions -> Text -> KillWorkerSessionParams -> IO EmptyObject
killDaemonWorkerSession = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.kill_worker_session" EmptyObject)))

closeDaemonSession :: RpcChannel -> CallOptions -> DaemonCloseSessionParams -> IO EmptyObject
closeDaemonSession = call (Proxy @(WithEnvelope (MethodRequest "daemon.close_session" DaemonCloseSessionParams)))

-- | Unlike the empty-object management requests, this wire request omits params.
getDaemonProxyToken :: RpcChannel -> CallOptions -> IO ProxyTokenResult
getDaemonProxyToken channel options =
  let context = callEnvelope options
      body = BaseRequest (callRequestId options) "daemon.get_proxy_token" Nothing (envelopeBody context)
   in requestResult channel (callTimeoutMicros options) (context {envelopeBody = body})

startDaemonRelay :: RpcChannel -> CallOptions -> EmptyObject -> IO RelayStartResult
startDaemonRelay = call (Proxy @(WithEnvelope (MethodRequest "daemon.relay.start" EmptyObject)))

stopDaemonRelay :: RpcChannel -> CallOptions -> EmptyObject -> IO RelayStopResult
stopDaemonRelay = call (Proxy @(WithEnvelope (MethodRequest "daemon.relay.stop" EmptyObject)))

getDaemonRelayStatus :: RpcChannel -> CallOptions -> EmptyObject -> IO RelayStatus
getDaemonRelayStatus = call (Proxy @(WithEnvelope (MethodRequest "daemon.relay.get_status" EmptyObject)))

listDaemonAutomations :: RpcChannel -> CallOptions -> AutomationListParams -> IO ListAutomationsResult
listDaemonAutomations = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_automations" AutomationListParams)))

runDaemonAutomation :: RpcChannel -> CallOptions -> RunAutomationParams -> IO RunAutomationResult
runDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.run_automation" RunAutomationParams)))

pauseDaemonAutomation :: RpcChannel -> CallOptions -> AutomationAddress -> IO AutomationStatusResult
pauseDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.pause_automation" AutomationAddress)))

resumeDaemonAutomation :: RpcChannel -> CallOptions -> AutomationAddress -> IO AutomationStatusResult
resumeDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.resume_automation" AutomationAddress)))

getDaemonAutomationHistory :: RpcChannel -> CallOptions -> AutomationHistoryParams -> IO AutomationHistoryResult
getDaemonAutomationHistory = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_automation_history" AutomationHistoryParams)))

getDaemonAutomationVisual :: RpcChannel -> CallOptions -> AutomationVisualParams -> IO AutomationVisualResult
getDaemonAutomationVisual = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_automation_visual" AutomationVisualParams)))

renameDaemonAutomation :: RpcChannel -> CallOptions -> RenameAutomationParams -> IO SuccessOrErrorResult
renameDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.rename_automation" RenameAutomationParams)))

deleteDaemonAutomation :: RpcChannel -> CallOptions -> AutomationAddress -> IO SuccessOrErrorResult
deleteDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.delete_automation" AutomationAddress)))

createDaemonAutomation :: RpcChannel -> CallOptions -> CreateAutomationParams -> IO AutomationCreationResult
createDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.create_automation" CreateAutomationParams)))

forkDaemonAutomation :: RpcChannel -> CallOptions -> ForkAutomationParams -> IO AutomationCreationResult
forkDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.fork_automation" ForkAutomationParams)))

updateDaemonAutomationModel :: RpcChannel -> CallOptions -> UpdateAutomationModelParams -> IO SuccessOrErrorResult
updateDaemonAutomationModel = call (Proxy @(WithEnvelope (MethodRequest "daemon.update_automation_model" UpdateAutomationModelParams)))

-- | Match this operation's baselined version omission, without changing the
-- connection defaults or other caller metadata. Reserved-body fields cannot
-- reintroduce the version through the shared envelope encoder.
updateDaemonAutomationPrivacy :: RpcChannel -> CallOptions -> UpdateAutomationPrivacyParams -> IO SuccessOrErrorResult
updateDaemonAutomationPrivacy channel options =
  call (Proxy @(WithEnvelope (MethodRequest "daemon.update_automation_privacy" UpdateAutomationPrivacyParams))) channel (options {callEnvelope = (callEnvelope options) {envelopeProtocolVersion = Nothing}})

updateDaemonAutomationPrompt :: RpcChannel -> CallOptions -> UpdateAutomationPromptParams -> IO SuccessOrErrorResult
updateDaemonAutomationPrompt = call (Proxy @(WithEnvelope (MethodRequest "daemon.update_automation_prompt" UpdateAutomationPromptParams)))

updateDaemonAutomationSchedule :: RpcChannel -> CallOptions -> UpdateAutomationScheduleParams -> IO SuccessOrErrorResult
updateDaemonAutomationSchedule = call (Proxy @(WithEnvelope (MethodRequest "daemon.update_automation_schedule" UpdateAutomationScheduleParams)))

applyDaemonAutomationConfig :: RpcChannel -> CallOptions -> ApplyAutomationConfigParams -> IO ApplyAutomationConfigResult
applyDaemonAutomationConfig = call (Proxy @(WithEnvelope (MethodRequest "daemon.apply_automation_config" ApplyAutomationConfigParams)))

updateDaemonAutomation :: RpcChannel -> CallOptions -> UpdateAutomationParams -> IO SuccessOrErrorResult
updateDaemonAutomation = call (Proxy @(WithEnvelope (MethodRequest "daemon.update_automation" UpdateAutomationParams)))

listDaemonAvailablePlugins :: RpcChannel -> CallOptions -> SessionIdParams -> IO ListAvailablePluginsResult
listDaemonAvailablePlugins = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_available_plugins" SessionIdParams)))

listDaemonInstalledPlugins :: RpcChannel -> CallOptions -> Text -> PluginScopeParams -> IO ListInstalledPluginsResult
listDaemonInstalledPlugins = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.list_installed_plugins" EmptyObject)))

installDaemonPlugin :: RpcChannel -> CallOptions -> Text -> InstallPluginParams -> IO InstallPluginResult
installDaemonPlugin = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.install_plugin" EmptyObject)))

uninstallDaemonPlugin :: RpcChannel -> CallOptions -> Text -> PluginTargetParams -> IO SuccessOrErrorResult
uninstallDaemonPlugin = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.uninstall_plugin" EmptyObject)))

setDaemonPluginEnabled :: RpcChannel -> CallOptions -> Text -> SetPluginEnabledParams -> IO SuccessOrErrorResult
setDaemonPluginEnabled = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.set_plugin_enabled" EmptyObject)))

updateDaemonPlugin :: RpcChannel -> CallOptions -> Text -> UpdatePluginParams -> IO UpdatePluginResult
updateDaemonPlugin = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.update_plugin" EmptyObject)))

listDaemonMarketplaces :: RpcChannel -> CallOptions -> SessionIdParams -> IO ListMarketplacesResult
listDaemonMarketplaces = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_marketplaces" SessionIdParams)))

addDaemonMarketplace :: RpcChannel -> CallOptions -> Text -> AddMarketplaceParams -> IO AddMarketplaceResult
addDaemonMarketplace = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.add_marketplace" EmptyObject)))

removeDaemonMarketplace :: RpcChannel -> CallOptions -> Text -> MarketplaceNameParams -> IO SuccessOrErrorResult
removeDaemonMarketplace = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.remove_marketplace" EmptyObject)))

updateDaemonMarketplace :: RpcChannel -> CallOptions -> Text -> UpdateMarketplaceParams -> IO UpdateMarketplaceResult
updateDaemonMarketplace = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.update_marketplace" EmptyObject)))

-- | Raw RPC reply: accepted=true is an ACK, not completion. The Daemon
-- connection operation additionally waits for the correlated create_message.
-- The explicit session overrides extensions in the shared queue parameters.
resolveDaemonQueuedUserMessageRaw :: RpcChannel -> CallOptions -> Text -> ResolveQueuedMessageParams -> IO EmptyObject
resolveDaemonQueuedUserMessageRaw = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.resolve_queued_user_message" EmptyObject)))

-- | Raw settings reply; an ACK does not establish notification completion.
updateDaemonSessionSettingsRaw :: RpcChannel -> CallOptions -> Text -> UpdateSessionSettingsParams -> IO EmptyObject
updateDaemonSessionSettingsRaw = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.update_session_settings" EmptyObject)))

-- | Raw title reply: accepted=true or the legacy success object.
renameDaemonSessionRaw :: RpcChannel -> CallOptions -> Text -> RenameSessionParams -> IO EmptyObject
renameDaemonSessionRaw = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.rename_session" EmptyObject)))

listDaemonSkills :: RpcChannel -> CallOptions -> SessionIdParams -> IO ListSkillsResult
listDaemonSkills = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_skills" SessionIdParams)))

listDaemonCommands :: RpcChannel -> CallOptions -> SessionIdParams -> IO ListCommandsResult
listDaemonCommands = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_commands" SessionIdParams)))

getDaemonContextBreakdown :: RpcChannel -> CallOptions -> SessionIdParams -> IO GetContextBreakdownResult
getDaemonContextBreakdown = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_context_breakdown" SessionIdParams)))

-- | Change remote skill enablement; the explicit session wins over extensions.
setDaemonSkillDisabled :: RpcChannel -> CallOptions -> Text -> SetSkillDisabledParams -> IO SuccessResult
setDaemonSkillDisabled = callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.set_skill_disabled" EmptyObject)))

callDaemonSession :: (KnownSymbol method, ToJSON params, FromJSON result) => Proxy (WithEnvelope (MethodRequest method EmptyObject)) -> RpcChannel -> CallOptions -> Text -> params -> IO result
callDaemonSession method channel options identifier input = do
  fields <- either (const (throwIO RpcInvalidResult)) pure (parseEither parseJSON (toJSON input))
  call method channel options (KeyMap.insert "sessionId" (String identifier) fields)

-- | Global daemon configuration: these operations carry no session routing.
getDaemonMcpConfig :: RpcChannel -> CallOptions -> EmptyObject -> IO GetMcpConfigResult
getDaemonMcpConfig = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_mcp_config" EmptyObject)))

updateDaemonMcpConfig :: RpcChannel -> CallOptions -> UpdateMcpConfigParams -> IO UpdateMcpConfigResult
updateDaemonMcpConfig channel options params = do
  validated <- either throwIO pure (validateMcpConfiguration params)
  call (Proxy @(WithEnvelope (MethodRequest "daemon.update_mcp_config" UpdateMcpConfigParams))) channel options validated

-- | Catalog operations do not initialize/load sessions or own saved state.
listDaemonOpenedSessions :: RpcChannel -> CallOptions -> ListOpenedSessionsParams -> IO ListOpenedSessionsResult
listDaemonOpenedSessions = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_opened_sessions" ListOpenedSessionsParams)))

listDaemonAvailableSessions :: RpcChannel -> CallOptions -> ListAvailableSessionsParams -> IO ListAvailableSessionsResult
listDaemonAvailableSessions = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_available_sessions" ListAvailableSessionsParams)))

-- | Sessionless daemon model discovery; Nothing leaves includeDisabled absent.
listDaemonModels :: RpcChannel -> CallOptions -> ListModelsOptions -> IO ListModelsResult
listDaemonModels = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_models" ListModelsOptions)))

getDaemonSessionMessages :: RpcChannel -> CallOptions -> GetSessionMessagesParams -> IO GetSessionMessagesResult
getDaemonSessionMessages = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_session_messages" GetSessionMessagesParams)))

searchDaemonSessions :: RpcChannel -> CallOptions -> SearchSessionsParams -> IO SearchSessionsResult
searchDaemonSessions = call (Proxy @(WithEnvelope (MethodRequest "daemon.search_sessions" SearchSessionsParams)))

archiveDaemonSession :: RpcChannel -> CallOptions -> ArchiveSessionParams -> IO ArchiveSessionResult
archiveDaemonSession = call (Proxy @(WithEnvelope (MethodRequest "daemon.archive_session" ArchiveSessionParams)))

unarchiveDaemonSession :: RpcChannel -> CallOptions -> SessionIdParams -> IO SuccessResult
unarchiveDaemonSession = call (Proxy @(WithEnvelope (MethodRequest "daemon.unarchive_session" SessionIdParams)))

-- | Workspace operations ask the daemon to act on its filesystem and trust
-- configuration. They do not interpret paths or perform local filesystem I/O.
checkDaemonFolderTrust :: RpcChannel -> CallOptions -> CheckFolderTrustParams -> IO CheckFolderTrustResult
checkDaemonFolderTrust = call (Proxy @(WithEnvelope (MethodRequest "daemon.check_folder_trust" CheckFolderTrustParams)))

trustDaemonFolder :: RpcChannel -> CallOptions -> TrustFolderParams -> IO TrustFolderResult
trustDaemonFolder = call (Proxy @(WithEnvelope (MethodRequest "daemon.trust_folder" TrustFolderParams)))

validateDaemonWorkingDirectory :: RpcChannel -> CallOptions -> ChangeWorkingDirectoryParams -> IO ValidateWorkingDirectoryResult
validateDaemonWorkingDirectory = call (Proxy @(WithEnvelope (MethodRequest "daemon.validate_working_directory" ChangeWorkingDirectoryParams)))

changeDaemonWorkingDirectory :: RpcChannel -> CallOptions -> ChangeSessionWorkingDirectoryParams -> IO ChangeWorkingDirectoryResult
changeDaemonWorkingDirectory = call (Proxy @(WithEnvelope (MethodRequest "daemon.change_working_directory" ChangeSessionWorkingDirectoryParams)))

listDaemonFiles :: RpcChannel -> CallOptions -> ListFilesParams -> IO ListFilesResult
listDaemonFiles channel options params = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_files" ListFilesParams))) channel options (params {listFilesShowHidden = Just (fromMaybe False (listFilesShowHidden params))})

-- | The low-level daemon schema supplies 60, unlike the controller's 50.
searchDaemonFiles :: RpcChannel -> CallOptions -> SearchFilesParams -> IO SearchFilesResult
searchDaemonFiles channel options params = call (Proxy @(WithEnvelope (MethodRequest "daemon.search_files" SearchFilesParams))) channel options (params {searchFilesMaxResults = Just (fromMaybe 60 (searchFilesMaxResults params)), searchFilesShowHidden = Just (fromMaybe False (searchFilesShowHidden params))})

getDaemonWorkspaceFileContent :: RpcChannel -> CallOptions -> GetWorkspaceFileContentParams -> IO GetWorkspaceFileContentResult
getDaemonWorkspaceFileContent = call (Proxy @(WithEnvelope (MethodRequest "daemon.get_workspace_file_content" GetWorkspaceFileContentParams)))

writeDaemonWorkspaceFileContent :: RpcChannel -> CallOptions -> WriteWorkspaceFileContentParams -> IO WriteWorkspaceFileContentResult
writeDaemonWorkspaceFileContent = call (Proxy @(WithEnvelope (MethodRequest "daemon.write_workspace_file_content" WriteWorkspaceFileContentParams)))

-- | Transfer URLs are explicit caller data. The daemon transfers bytes; the
-- low-level caller still owns this request's deadline and cancellation.
pushDaemonCwdFileToUrl :: RpcChannel -> CallOptions -> PushCwdFileToUrlParams -> IO PushCwdFileToUrlResult
pushDaemonCwdFileToUrl = call (Proxy @(WithEnvelope (MethodRequest "daemon.push_cwd_file_to_url" PushCwdFileToUrlParams)))

pullDaemonUrlToCwdFile :: RpcChannel -> CallOptions -> PullUrlToCwdFileParams -> IO PullUrlToCwdFileResult
pullDaemonUrlToCwdFile = call (Proxy @(WithEnvelope (MethodRequest "daemon.pull_url_to_cwd_file" PullUrlToCwdFileParams)))

-- | Terminal requests operate on the daemon's PTYs, not local processes.
createDaemonTerminal :: RpcChannel -> CallOptions -> DaemonCreateTerminalParams -> IO CreateTerminalResult
createDaemonTerminal = call (Proxy @(WithEnvelope (MethodRequest "daemon.create_terminal" DaemonCreateTerminalParams)))

writeDaemonTerminalData :: RpcChannel -> CallOptions -> DaemonWriteTerminalDataParams -> IO SuccessResult
writeDaemonTerminalData = call (Proxy @(WithEnvelope (MethodRequest "daemon.write_terminal_data" DaemonWriteTerminalDataParams)))

resizeDaemonTerminal :: RpcChannel -> CallOptions -> DaemonResizeTerminalParams -> IO SuccessResult
resizeDaemonTerminal = call (Proxy @(WithEnvelope (MethodRequest "daemon.resize_terminal" DaemonResizeTerminalParams)))

closeDaemonTerminal :: RpcChannel -> CallOptions -> DaemonCloseTerminalParams -> IO SuccessResult
closeDaemonTerminal = call (Proxy @(WithEnvelope (MethodRequest "daemon.close_terminal" DaemonCloseTerminalParams)))

listDaemonTerminals :: RpcChannel -> CallOptions -> DaemonListTerminalsParams -> IO ListTerminalsResult
listDaemonTerminals = call (Proxy @(WithEnvelope (MethodRequest "daemon.list_terminals" DaemonListTerminalsParams)))

-- | Submit a message. The immediate object response is not turn completion.
addUserMessage :: RpcChannel -> CallOptions -> AddUserMessageParams -> IO EmptyObject
addUserMessage = call (Proxy @AddUserMessageRequest)

-- | Append complete user-only messages; no local message rewrite is performed.
appendMessages :: RpcChannel -> CallOptions -> AppendMessagesParams -> IO EmptyObject
appendMessages = call (Proxy @AppendMessagesRequest)

-- | Ask Droid to begin MCP authentication; the SDK does not open a browser.
authenticateMcpServer :: RpcChannel -> CallOptions -> McpServerNameParams -> IO SuccessResult
authenticateMcpServer = call (Proxy @AuthenticateMcpServerRequest)

-- | Ask Droid to cancel MCP authentication.
cancelMcpAuth :: RpcChannel -> CallOptions -> McpServerNameParams -> IO SuccessResult
cancelMcpAuth = call (Proxy @CancelMcpAuthRequest)

-- | Ask Droid to change its working directory, not the SDK process directory.
changeWorkingDirectory :: RpcChannel -> CallOptions -> ChangeWorkingDirectoryParams -> IO ChangeWorkingDirectoryResult
changeWorkingDirectory = call (Proxy @ChangeWorkingDirectoryRequest)

-- | Ask Droid to clear authentication for an MCP server.
clearMcpAuth :: RpcChannel -> CallOptions -> McpServerNameParams -> IO SuccessResult
clearMcpAuth = call (Proxy @ClearMcpAuthRequest)

-- | Request session closure; transport ownership remains with the caller.
closeSession :: RpcChannel -> CallOptions -> CloseSessionParams -> IO EmptyObject
closeSession = call (Proxy @CloseSessionRequest)

-- | Request compaction and return its result without transferring handles.
compactSession :: RpcChannel -> CallOptions -> CompactSessionParams -> IO CompactSessionResult
compactSession = call (Proxy @CompactSessionRequest)

-- | Request rewind and return its result; no SDK-side file operation occurs.
executeRewind :: RpcChannel -> CallOptions -> ExecuteRewindParams -> IO ExecuteRewindResult
executeRewind = call (Proxy @ExecuteRewindRequest)

-- | Request a fork; the caller retains responsibility for successor ownership.
forkSession :: RpcChannel -> CallOptions -> ForkSessionParams -> IO ForkSessionResult
forkSession = call (Proxy @ForkSessionRequest)

-- | Read the peer's detailed context breakdown. Pass mempty for ordinary params.
getContextBreakdown :: RpcChannel -> CallOptions -> EmptyObject -> IO GetContextBreakdownResult
getContextBreakdown = call (Proxy @GetContextBreakdownRequest)

-- | Read the peer's aggregate context statistics.
getContextStats :: RpcChannel -> CallOptions -> EmptyObject -> IO ContextStats
getContextStats = call (Proxy @GetContextStatsRequest)

-- | Read available rewind data without fetching snapshot files.
getRewindInfo :: RpcChannel -> CallOptions -> GetRewindInfoParams -> IO GetRewindInfoResult
getRewindInfo = call (Proxy @GetRewindInfoRequest)

-- | Request interruption; do not infer turn completion from the acknowledgement.
interruptSession :: RpcChannel -> CallOptions -> EmptyObject -> IO EmptyObject
interruptSession = call (Proxy @InterruptSessionRequest)

-- | Ask Droid to terminate a worker session.
killWorkerSession :: RpcChannel -> CallOptions -> KillWorkerSessionParams -> IO EmptyObject
killWorkerSession = call (Proxy @KillWorkerSessionRequest)

-- | List command metadata without executing a command.
listCommands :: RpcChannel -> CallOptions -> EmptyObject -> IO ListCommandsResult
listCommands = call (Proxy @ListCommandsRequest)

-- | Read the MCP registry.
listMcpRegistry :: RpcChannel -> CallOptions -> EmptyObject -> IO ListMcpRegistryResult
listMcpRegistry = call (Proxy @ListMcpRegistryRequest)

-- | Read MCP server status.
listMcpServers :: RpcChannel -> CallOptions -> EmptyObject -> IO ListMcpServersResult
listMcpServers = call (Proxy @ListMcpServersRequest)

-- | Read MCP tool metadata without invoking tools.
listMcpTools :: RpcChannel -> CallOptions -> EmptyObject -> IO ListMcpToolsResult
listMcpTools = call (Proxy @ListMcpToolsRequest)

-- | Read the model catalog with explicit discovery options.
listModels :: RpcChannel -> CallOptions -> ListModelsOptions -> IO ListModelsResult
listModels = call (Proxy @ListModelsRequest)

-- | Read skill metadata without executing skills.
listSkills :: RpcChannel -> CallOptions -> EmptyObject -> IO ListSkillsResult
listSkills = call (Proxy @ListSkillsRequest)

-- | Query hypothetical native-tool availability; this does not update policy.
listTools :: RpcChannel -> CallOptions -> ListToolsOptions -> IO ListToolsResult
listTools = call (Proxy @ListToolsRequest)

-- | Ask Droid to remove a configured MCP server.
removeMcpServer :: RpcChannel -> CallOptions -> RemoveMcpServerParams -> IO SuccessResult
removeMcpServer = call (Proxy @RemoveMcpServerRequest)

-- | Ask Droid to rename the current session; a false success flag is retained.
renameSession :: RpcChannel -> CallOptions -> RenameSessionParams -> IO SuccessResult
renameSession = call (Proxy @RenameSessionRequest)

-- | Change placement or delete a queued user message.
resolveQueuedUserMessage :: RpcChannel -> CallOptions -> ResolveQueuedMessageParams -> IO EmptyObject
resolveQueuedUserMessage = call (Proxy @ResolveQueuedUserMessageRequest)

-- | Ask Droid to change a skill's disabled state.
setSkillDisabled :: RpcChannel -> CallOptions -> SetSkillDisabledParams -> IO SuccessResult
setSkillDisabled = call (Proxy @SetSkillDisabledRequest)

-- | Submit only caller-provided content, after the shared source preflight.
submitBugReport :: RpcChannel -> CallOptions -> SubmitBugReportParams -> IO SubmitBugReportResult
submitBugReport channel configured params =
  validateBugReportParams params >>= call (Proxy @SubmitBugReportRequest) channel configured

-- | Explicit remote report submission; no local log collection or attribution
-- is inferred, and the explicit session overrides request-body extensions.
submitDaemonBugReport :: RpcChannel -> CallOptions -> Text -> SubmitBugReportParams -> IO SubmitBugReportResult
submitDaemonBugReport channel configured identifier params =
  validateBugReportParams params >>= callDaemonSession (Proxy @(WithEnvelope (MethodRequest "daemon.submit_bug_report" EmptyObject))) channel configured identifier

validateBugReportParams :: SubmitBugReportParams -> IO SubmitBugReportParams
validateBugReportParams params = either throwIO (const (pure params)) (traverse validateBugReportSource (bugReportSource params))

-- | Submit the caller's MCP authorization code; no exchange is performed locally.
submitMcpAuthCode :: RpcChannel -> CallOptions -> SubmitMcpAuthCodeParams -> IO SuccessResult
submitMcpAuthCode = call (Proxy @SubmitMcpAuthCodeRequest)

-- | Submit an MCP authentication error.
submitMcpAuthError :: RpcChannel -> CallOptions -> SubmitMcpAuthErrorParams -> IO SuccessResult
submitMcpAuthError = call (Proxy @SubmitMcpAuthErrorRequest)

-- | Ask Droid to enable or disable an MCP server.
toggleMcpServer :: RpcChannel -> CallOptions -> ToggleMcpServerParams -> IO SuccessResult
toggleMcpServer = call (Proxy @ToggleMcpServerRequest)

-- | Ask Droid to enable or disable an MCP tool.
toggleMcpTool :: RpcChannel -> CallOptions -> ToggleMcpToolParams -> IO SuccessResult
toggleMcpTool = call (Proxy @ToggleMcpToolRequest)

-- | Apply a partial settings update, retaining the peer's object acknowledgement.
updateSessionSettings :: RpcChannel -> CallOptions -> UpdateSessionSettingsParams -> IO EmptyObject
updateSessionSettings = call (Proxy @UpdateSessionSettingsRequest)

-- | Ask Droid to warm its cache; this is not a local SDK cache implementation.
warmupCache :: RpcChannel -> CallOptions -> EmptyObject -> IO EmptyObject
warmupCache = call (Proxy @WarmupCacheRequest)

-- | Execute an explicitly method-indexed request through the same correlation
-- and error path as the named operations. Useful for backend-specific bindings.
call :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> IO result
call = callWithHookPolicy RunBeforeRequest

-- | Explicitly skip only the optional before-request guard, never mandatory
-- request barriers such as an in-progress authentication gate.
callWithHookPolicy :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => RpcHookPolicy -> Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> IO result
callWithHookPolicy = callWithAdmission (pure ())

-- | Recheck a nonblocking STM admission action at the shared physical writer.
-- A failed admission sends nothing and retains the originating exception.
callWithAdmission :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => STM () -> RpcHookPolicy -> Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> IO result
callWithAdmission admit policy method channel options params =
  requestReplyWithAdmission channel policy admit (callTimeoutMicros options) (requestEnvelope method options params) >>= either throwIO pure . decodeRpcResult

-- Domain preparation composes with the same call: remote/missing-result errors
-- retain precedence, and parser failures use the existing result-error category.
callParsed :: forall method params result. (KnownSymbol method, ToJSON params) => (Value -> Parser result) -> Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> IO result
callParsed parser method channel options params = do
  value <- call method channel options params
  either (const (throwIO RpcInvalidResult)) pure (parseEither parser value)

-- | Queue validated result observation at its ordered intake position. The
-- observer must be total and nonblocking. Return does not imply publication;
-- synchronize intake under the caller's deadline before relying on that state.
callObserved :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> (result -> STM ()) -> IO result
callObserved = callObservedWithHookPolicy RunBeforeRequest

callObservedWithHookPolicy :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => RpcHookPolicy -> Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> (result -> STM ()) -> IO result
callObservedWithHookPolicy = callObservedWithAdmission (pure ())

callObservedWithAdmission :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => STM () -> RpcHookPolicy -> Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> (result -> STM ()) -> IO result
callObservedWithAdmission admit policy method channel options params observe = do
  let observeReply _ response = case decodeRpcResult response of Left _ -> pure (); Right value -> observe value
  response <- requestReplyObservedAtWithAdmission channel policy admit (callTimeoutMicros options) (requestEnvelope method options params) observeReply
  either throwIO pure (decodeRpcResult response)

requestEnvelope :: forall method params. (KnownSymbol method, ToJSON params) => Proxy (WithEnvelope (MethodRequest method params)) -> CallOptions -> params -> WithEnvelope BaseRequest
requestEnvelope _ options params =
  let context = callEnvelope options
      body = MethodRequest @method (callRequestId options) params (envelopeBody context)
   in context {envelopeBody = eraseMethodRequest body}
