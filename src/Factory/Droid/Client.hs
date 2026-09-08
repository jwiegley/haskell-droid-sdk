-- | Typed low-level local Droid operations with available 1.205.0 contracts.
-- Calls use the existing channel and caller-supplied IDs, envelope context and
-- deadlines. This is not a launcher, default identity policy or session owner.
module Factory.Droid.Client
  ( CallOptions (..),
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Protocol (RpcChannel, requestResult)
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
    SubmitBugReportParams,
    SubmitBugReportResult,
  )
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
import Factory.Droid.Schema.Models (ListModelsOptions, ListModelsResult)
import Factory.Droid.Schema.RPC (EmptyObject, JsonRpcEnvelope, MethodRequest (..), SuccessResult, WithEnvelope (..), eraseMethodRequest)
import Factory.Droid.Schema.Settings (ListToolsOptions, UpdateSessionSettingsParams)
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

-- | Submit only the report/log content explicitly supplied by the caller.
submitBugReport :: RpcChannel -> CallOptions -> SubmitBugReportParams -> IO SubmitBugReportResult
submitBugReport = call (Proxy @SubmitBugReportRequest)

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

call :: forall method params result. (KnownSymbol method, ToJSON params, FromJSON result) => Proxy (WithEnvelope (MethodRequest method params)) -> RpcChannel -> CallOptions -> params -> IO result
call _ channel options params =
  let context = callEnvelope options
      body = MethodRequest @method (callRequestId options) params (envelopeBody context)
   in requestResult channel (callTimeoutMicros options) (context {envelopeBody = eraseMethodRequest body})
