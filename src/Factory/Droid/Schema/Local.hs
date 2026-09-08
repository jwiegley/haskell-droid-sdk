{-# LANGUAGE DataKinds #-}

-- | Complete local-operation envelopes with statically known method literals.
-- These aliases reuse existing parameter/result codecs. Field-level runtime
-- semantics are documented by the corresponding body codec.
module Factory.Droid.Schema.Local
  ( AddUserMessageRequest,
    AppendMessagesRequest,
    AuthenticateMcpServerRequest,
    CancelMcpAuthRequest,
    ChangeWorkingDirectoryRequest,
    ClearMcpAuthRequest,
    CloseSessionRequest,
    CompactSessionRequest,
    ExecuteRewindRequest,
    ForkSessionRequest,
    GetContextBreakdownRequest,
    GetContextStatsRequest,
    GetRewindInfoRequest,
    InterruptSessionRequest,
    KillWorkerSessionRequest,
    ListCommandsRequest,
    ListMcpRegistryRequest,
    ListMcpServersRequest,
    ListMcpToolsRequest,
    ListModelsRequest,
    ListSkillsRequest,
    ListToolsRequest,
    RemoveMcpServerRequest,
    RenameSessionRequest,
    ResolveQueuedUserMessageRequest,
    SetSkillDisabledRequest,
    SubmitBugReportRequest,
    SubmitMcpAuthCodeRequest,
    SubmitMcpAuthErrorRequest,
    ToggleMcpServerRequest,
    ToggleMcpToolRequest,
    UpdateSessionSettingsRequest,
    WarmupCacheRequest,
    ChangeWorkingDirectoryResponse,
    CompactSessionResponse,
    ExecuteRewindResponse,
    ForkSessionResponse,
    GetContextBreakdownResponse,
    GetContextStatsResponse,
    GetRewindInfoResponse,
    ListCommandsResponse,
    ListMcpRegistryResponse,
    ListMcpServersResponse,
    ListMcpToolsResponse,
    ListModelsResponse,
    ListSkillsResponse,
    ListToolsResponse,
    SubmitBugReportResponse,
  )
where

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
import Factory.Droid.Schema.RPC (EmptyObject, MethodRequest, RpcResponse, WithEnvelope)
import Factory.Droid.Schema.Settings (ListToolsOptions, UpdateSessionSettingsParams)

-- | Add a user message; the result envelope is the shared EmptyObjectResponse.
type AddUserMessageRequest = WithEnvelope (MethodRequest "droid.add_user_message" AddUserMessageParams)

-- | Append complete user-only messages.
type AppendMessagesRequest = WithEnvelope (MethodRequest "droid.append_messages" AppendMessagesParams)

-- | Begin authentication for a named MCP server.
type AuthenticateMcpServerRequest = WithEnvelope (MethodRequest "droid.authenticate_mcp_server" McpServerNameParams)

-- | Cancel a named MCP authentication flow.
type CancelMcpAuthRequest = WithEnvelope (MethodRequest "droid.cancel_mcp_auth" McpServerNameParams)

-- | Request a working-directory change.
type ChangeWorkingDirectoryRequest = WithEnvelope (MethodRequest "droid.change_working_directory" ChangeWorkingDirectoryParams)

-- | Clear stored authentication for a named MCP server.
type ClearMcpAuthRequest = WithEnvelope (MethodRequest "droid.clear_mcp_auth" McpServerNameParams)

-- | Request session closure with an optional reason inside required params.
type CloseSessionRequest = WithEnvelope (MethodRequest "droid.close_session" CloseSessionParams)

-- | Request session compaction.
type CompactSessionRequest = WithEnvelope (MethodRequest "droid.compact_session" CompactSessionParams)

-- | Execute a rewind using the provided file lists.
type ExecuteRewindRequest = WithEnvelope (MethodRequest "droid.execute_rewind" ExecuteRewindParams)

-- | Request a fork without defining client-side handle ownership.
type ForkSessionRequest = WithEnvelope (MethodRequest "droid.fork_session" ForkSessionParams)

-- | Request the detailed context breakdown.
type GetContextBreakdownRequest = WithEnvelope (MethodRequest "droid.get_context_breakdown" EmptyObject)

-- | Request aggregate context statistics.
type GetContextStatsRequest = WithEnvelope (MethodRequest "droid.get_context_stats" EmptyObject)

-- | Request rewind information for a session/message pair.
type GetRewindInfoRequest = WithEnvelope (MethodRequest "droid.get_rewind_info" GetRewindInfoParams)

-- | Request interruption of the current session.
type InterruptSessionRequest = WithEnvelope (MethodRequest "droid.interrupt_session" EmptyObject)

-- | Request termination of a worker session.
type KillWorkerSessionRequest = WithEnvelope (MethodRequest "droid.kill_worker_session" KillWorkerSessionParams)

-- | List command metadata.
type ListCommandsRequest = WithEnvelope (MethodRequest "droid.list_commands" EmptyObject)

-- | List MCP registry entries.
type ListMcpRegistryRequest = WithEnvelope (MethodRequest "droid.list_mcp_registry" EmptyObject)

-- | List MCP server status entries.
type ListMcpServersRequest = WithEnvelope (MethodRequest "droid.list_mcp_servers" EmptyObject)

-- | List MCP tools.
type ListMcpToolsRequest = WithEnvelope (MethodRequest "droid.list_mcp_tools" EmptyObject)

-- | List models with explicit discovery options.
type ListModelsRequest = WithEnvelope (MethodRequest "droid.list_models" ListModelsOptions)

-- | List skill metadata.
type ListSkillsRequest = WithEnvelope (MethodRequest "droid.list_skills" EmptyObject)

-- | Query hypothetical tool availability without changing session policy.
type ListToolsRequest = WithEnvelope (MethodRequest "droid.list_tools" ListToolsOptions)

-- | Remove a named MCP server configuration.
type RemoveMcpServerRequest = WithEnvelope (MethodRequest "droid.remove_mcp_server" RemoveMcpServerParams)

-- | Rename the current session.
type RenameSessionRequest = WithEnvelope (MethodRequest "droid.rename_session" RenameSessionParams)

-- | Change placement or delete a queued user message.
type ResolveQueuedUserMessageRequest = WithEnvelope (MethodRequest "droid.resolve_queued_user_message" ResolveQueuedMessageParams)

-- | Change a skill's disabled state.
type SetSkillDisabledRequest = WithEnvelope (MethodRequest "droid.set_skill_disabled" SetSkillDisabledParams)

-- | Submit caller-provided bug-report content.
type SubmitBugReportRequest = WithEnvelope (MethodRequest "droid.submit_bug_report" SubmitBugReportParams)

-- | Submit a caller-provided MCP authorization code.
type SubmitMcpAuthCodeRequest = WithEnvelope (MethodRequest "droid.submit_mcp_auth_code" SubmitMcpAuthCodeParams)

-- | Report an MCP authentication error.
type SubmitMcpAuthErrorRequest = WithEnvelope (MethodRequest "droid.submit_mcp_auth_error" SubmitMcpAuthErrorParams)

-- | Change a configured MCP server's enabled state.
type ToggleMcpServerRequest = WithEnvelope (MethodRequest "droid.toggle_mcp_server" ToggleMcpServerParams)

-- | Change an MCP tool's enabled state.
type ToggleMcpToolRequest = WithEnvelope (MethodRequest "droid.toggle_mcp_tool" ToggleMcpToolParams)

-- | Apply a partial session settings update; the result is an open object.
type UpdateSessionSettingsRequest = WithEnvelope (MethodRequest "droid.update_session_settings" UpdateSessionSettingsParams)

-- | Request cache warmup.
type WarmupCacheRequest = WithEnvelope (MethodRequest "droid.warmup_cache" EmptyObject)

-- | Directory-change result or unrestricted failure.
type ChangeWorkingDirectoryResponse = WithEnvelope (RpcResponse ChangeWorkingDirectoryResult)

-- | Compaction result or unrestricted failure.
type CompactSessionResponse = WithEnvelope (RpcResponse CompactSessionResult)

-- | Rewind result or unrestricted failure.
type ExecuteRewindResponse = WithEnvelope (RpcResponse ExecuteRewindResult)

-- | Fork result or unrestricted failure.
type ForkSessionResponse = WithEnvelope (RpcResponse ForkSessionResult)

-- | Detailed context result or unrestricted failure.
type GetContextBreakdownResponse = WithEnvelope (RpcResponse GetContextBreakdownResult)

-- | Aggregate context result or unrestricted failure.
type GetContextStatsResponse = WithEnvelope (RpcResponse ContextStats)

-- | Rewind-information result or unrestricted failure.
type GetRewindInfoResponse = WithEnvelope (RpcResponse GetRewindInfoResult)

-- | Inline command-list result or unrestricted failure.
type ListCommandsResponse = WithEnvelope (RpcResponse ListCommandsResult)

-- | MCP registry result or unrestricted failure.
type ListMcpRegistryResponse = WithEnvelope (RpcResponse ListMcpRegistryResult)

-- | MCP server-status result or unrestricted failure.
type ListMcpServersResponse = WithEnvelope (RpcResponse ListMcpServersResult)

-- | MCP tool-list result or unrestricted failure.
type ListMcpToolsResponse = WithEnvelope (RpcResponse ListMcpToolsResult)

-- | Model catalog result or unrestricted failure.
type ListModelsResponse = WithEnvelope (RpcResponse ListModelsResult)

-- | Skill-list result or unrestricted failure.
type ListSkillsResponse = WithEnvelope (RpcResponse ListSkillsResult)

-- | Tool-catalog result or unrestricted failure.
type ListToolsResponse = WithEnvelope (RpcResponse ListToolsResult)

-- | Bug-report result or unrestricted failure.
type SubmitBugReportResponse = WithEnvelope (RpcResponse SubmitBugReportResult)
