{-# LANGUAGE OverloadedStrings #-}

-- | Core local notification payloads from Factory protocol 1.205.0. These
-- records are not JSON-RPC envelopes or a complete session-notification union.
-- Decoding does not dispatch callbacks, alter session state or infer completion.
module Factory.Droid.Schema.Notifications
  ( AgentTurnCompletionReason (..),
    DroidWorkingState (..),
    TitleUpdateType (..),
    NotificationErrorType (..),
    AssistantTextDelta (..),
    AssistantTextComplete (..),
    ThinkingTextDelta (..),
    ThinkingTextComplete (..),
    CreateMessage (..),
    AgentTurnCompleted (..),
    SessionTokenUsageChanged (..),
    DroidWorkingStateChanged (..),
    AssistantMessageRetracted (..),
    SessionTitleUpdated (..),
    SessionWorkingDirectoryChanged (..),
    QueuedMessagesDiscarded (..),
    StructuredOutput (..),
    SessionCompacted (..),
    NotificationError (..),
    ErrorNotification (..),
    ChildSessionAvailable (..),
    ToolConfirmationOutcome (..),
    ToolExecutionPhase (..),
    ToolProgressKind (..),
    LlmRetryReason (..),
    ToolResultNotification (..),
    ToolCallNotification (..),
    ToolExecutionHeartbeat (..),
    ToolExecutionPhaseChanged (..),
    ToolProgressUpdate (..),
    ToolProgressUpdateNotification (..),
    LlmRetry (..),
    PermissionResolved (..),
    DroidHookEvent (..),
    HookCompletionStatus (..),
    HookCommand,
    HookResult (..),
    HookExecutionStarted (..),
    HookExecutionCompleted (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    Options,
    ToJSON (..),
    Value (Object, String),
    camelTo2,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    withObject,
    withText,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, enumOptions, objectWithAdditionalFields, optionalField, requireLiteral)
import Factory.Droid.Schema.Content (ContentBlock (ContentToolResult), ThinkingDuration, ToolResultBlock, ToolUseBlock, contentBlockObject)
import Factory.Droid.Schema.Messages (FactoryDroidMessage, PersistedHookCommand, PersistedHookResult, persistedHookResultObject)
import Factory.Droid.Schema.Primitives (NonNegativeNumber)
import Factory.Droid.Schema.Usage (LastCallTokenUsage, TokenUsage)
import GHC.Generics (Generic)

-- | Every declared reason for an explicit turn-completed event.
data AgentTurnCompletionReason
  = TurnCompleted
  | TurnCancelled
  | TurnPermissionRejected
  | TurnError
  | TurnProcessExit
  | TurnSpecHandoff
  | TurnStructuredOutputMissing
  | TurnStructuredOutputInvalid
  | TurnStructuredOutputSchemaInvalid
  | TurnModelUsageExhausted
  | TurnModelAuthenticationFailed
  | TurnModelRequestRejected
  | TurnModelProviderUnreachable
  | TurnModelProviderUnavailable
  | TurnModelRateLimited
  | TurnPromptRejected
  | TurnCompletionPersistenceFailed
  | TurnNoApproverAvailable
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON AgentTurnCompletionReason where
  parseJSON = genericParseJSON turnReasonOptions

instance ToJSON AgentTurnCompletionReason where
  toJSON = genericToJSON turnReasonOptions
  toEncoding = genericToEncoding turnReasonOptions

-- | Working-state changes do not themselves establish turn completion.
data DroidWorkingState
  = WorkingIdle
  | WorkingThinking
  | WorkingStreamingAssistantMessage
  | WorkingWaitingForToolConfirmation
  | WorkingExecutingTool
  | WorkingCompactingConversation
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON DroidWorkingState where
  parseJSON = genericParseJSON workingOptions

instance ToJSON DroidWorkingState where
  toJSON = genericToJSON workingOptions
  toEncoding = genericToEncoding workingOptions

-- | How a title update was produced, when supplied by the peer.
data TitleUpdateType = TitleLlmGenerated | TitleFirstUserMessage | TitleManualRename
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON TitleUpdateType where
  parseJSON = genericParseJSON titleOptions

instance ToJSON TitleUpdateType where
  toJSON = genericToJSON titleOptions
  toEncoding = genericToEncoding titleOptions

-- | Error-category literals carried by error notifications, not IO exceptions.
data NotificationErrorType
  = NotifyConnectionError
  | NotifyProtocolError
  | NotifySessionError
  | NotifyTimeoutError
  | NotifyDroidClientError
  | NotifyProcessExitError
  | NotifyError
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON NotificationErrorType where
  parseJSON = genericParseJSON errorTypeOptions

instance ToJSON NotificationErrorType where
  toJSON = genericToJSON errorTypeOptions
  toEncoding = genericToEncoding errorTypeOptions

-- | An assistant-text delta. Block indices retain the schema's number domain.
data AssistantTextDelta = AssistantTextDelta
  { assistantDeltaMessageId :: !Text,
    assistantDeltaBlockIndex :: !Scientific,
    assistantDeltaText :: !Text,
    assistantDeltaAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AssistantTextDelta where
  parseJSON = withObject "AssistantTextDelta" $ \fields -> do
    requireLiteral "type" "assistant_text_delta" fields
    AssistantTextDelta <$> fields .: "messageId" <*> fields .: "blockIndex" <*> fields .: "textDelta" <*> pure (additionalFields deltaKeys fields)

instance ToJSON AssistantTextDelta where
  toJSON event = objectWithAdditionalFields deltaKeys (assistantDeltaAdditionalFields event) ["type" .= String "assistant_text_delta", "messageId" .= assistantDeltaMessageId event, "blockIndex" .= assistantDeltaBlockIndex event, "textDelta" .= assistantDeltaText event]

-- | Completion of one assistant text block, without synthesizing its text.
data AssistantTextComplete = AssistantTextComplete
  { assistantCompleteMessageId :: !Text,
    assistantCompleteBlockIndex :: !Scientific,
    assistantCompleteAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AssistantTextComplete where
  parseJSON = withObject "AssistantTextComplete" $ \fields -> do
    requireLiteral "type" "assistant_text_complete" fields
    AssistantTextComplete <$> fields .: "messageId" <*> fields .: "blockIndex" <*> pure (additionalFields completeKeys fields)

instance ToJSON AssistantTextComplete where
  toJSON event = objectWithAdditionalFields completeKeys (assistantCompleteAdditionalFields event) ["type" .= String "assistant_text_complete", "messageId" .= assistantCompleteMessageId event, "blockIndex" .= assistantCompleteBlockIndex event]

-- | A thinking-text delta, distinct from assistant text on the wire.
data ThinkingTextDelta = ThinkingTextDelta
  { thinkingDeltaMessageId :: !Text,
    thinkingDeltaBlockIndex :: !Scientific,
    thinkingDeltaText :: !Text,
    thinkingDeltaAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ThinkingTextDelta where
  parseJSON = withObject "ThinkingTextDelta" $ \fields -> do
    requireLiteral "type" "thinking_text_delta" fields
    ThinkingTextDelta <$> fields .: "messageId" <*> fields .: "blockIndex" <*> fields .: "textDelta" <*> pure (additionalFields deltaKeys fields)

instance ToJSON ThinkingTextDelta where
  toJSON event = objectWithAdditionalFields deltaKeys (thinkingDeltaAdditionalFields event) ["type" .= String "thinking_text_delta", "messageId" .= thinkingDeltaMessageId event, "blockIndex" .= thinkingDeltaBlockIndex event, "textDelta" .= thinkingDeltaText event]

-- | Thinking-block completion with an optional nonnegative millisecond duration.
data ThinkingTextComplete = ThinkingTextComplete
  { thinkingCompleteMessageId :: !Text,
    thinkingCompleteBlockIndex :: !Scientific,
    thinkingCompleteDuration :: !(Maybe ThinkingDuration),
    thinkingCompleteAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ThinkingTextComplete where
  parseJSON = withObject "ThinkingTextComplete" $ \fields -> do
    requireLiteral "type" "thinking_text_complete" fields
    ThinkingTextComplete <$> fields .: "messageId" <*> fields .: "blockIndex" <*> fields .:! "durationMs" <*> pure (additionalFields thinkingCompleteKeys fields)

instance ToJSON ThinkingTextComplete where
  toJSON event =
    objectWithAdditionalFields thinkingCompleteKeys (thinkingCompleteAdditionalFields event) $
      ["type" .= String "thinking_text_complete", "messageId" .= thinkingCompleteMessageId event, "blockIndex" .= thinkingCompleteBlockIndex event]
        <> optionalField "durationMs" (thinkingCompleteDuration event)

-- | Creation of a complete message, with optional parent/request correlation.
data CreateMessage = CreateMessage
  { createdMessage :: !FactoryDroidMessage,
    createdParentId :: !(Maybe Text),
    createdRequestId :: !(Maybe Text),
    createdMessageAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CreateMessage where
  parseJSON = withObject "CreateMessage" $ \fields -> do
    requireLiteral "type" "create_message" fields
    CreateMessage <$> fields .: "message" <*> fields .:! "parentId" <*> fields .:! "requestId" <*> pure (additionalFields createKeys fields)

instance ToJSON CreateMessage where
  toJSON event =
    objectWithAdditionalFields createKeys (createdMessageAdditionalFields event) $
      ["type" .= String "create_message", "message" .= createdMessage event]
        <> optionalField "parentId" (createdParentId event)
        <> optionalField "requestId" (createdRequestId event)

-- | An explicit turn boundary. Token usage may include children; durationMs
-- describes this session's wall-clock turn and is not summed with child clocks.
data AgentTurnCompleted = AgentTurnCompleted
  { turnCompletionReason :: !AgentTurnCompletionReason,
    turnTokenUsage :: !TokenUsage,
    completedTurnId :: !(Maybe Text),
    turnCumulativeTokenUsage :: !(Maybe TokenUsage),
    turnChildTokenUsage :: !(Maybe TokenUsage),
    turnCumulativeChildTokenUsage :: !(Maybe TokenUsage),
    turnDurationMs :: !(Maybe NonNegativeNumber),
    turnAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AgentTurnCompleted where
  parseJSON = withObject "AgentTurnCompleted" $ \fields -> do
    requireLiteral "type" "agent_turn_completed" fields
    AgentTurnCompleted
      <$> fields .: "reason"
      <*> fields .: "tokenUsage"
      <*> fields .:! "turnId"
      <*> fields .:! "cumulativeTokenUsage"
      <*> fields .:! "childTokenUsage"
      <*> fields .:! "cumulativeChildTokenUsage"
      <*> fields .:! "durationMs"
      <*> pure (additionalFields turnKeys fields)

instance ToJSON AgentTurnCompleted where
  toJSON event =
    objectWithAdditionalFields turnKeys (turnAdditionalFields event) $
      ["type" .= String "agent_turn_completed", "reason" .= turnCompletionReason event, "tokenUsage" .= turnTokenUsage event]
        <> optionalField "turnId" (completedTurnId event)
        <> optionalField "cumulativeTokenUsage" (turnCumulativeTokenUsage event)
        <> optionalField "childTokenUsage" (turnChildTokenUsage event)
        <> optionalField "cumulativeChildTokenUsage" (turnCumulativeChildTokenUsage event)
        <> optionalField "durationMs" (turnDurationMs event)

-- | Session usage and optional inclusive/last-call counters.
data SessionTokenUsageChanged = SessionTokenUsageChanged
  { sessionUsageId :: !Text,
    sessionUsageTokens :: !TokenUsage,
    sessionUsageInclusive :: !(Maybe TokenUsage),
    sessionUsageLastCall :: !(Maybe LastCallTokenUsage),
    sessionUsageAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionTokenUsageChanged where
  parseJSON = withObject "SessionTokenUsageChanged" $ \fields -> do
    requireLiteral "type" "session_token_usage_changed" fields
    SessionTokenUsageChanged <$> fields .: "sessionId" <*> fields .: "tokenUsage" <*> fields .:! "inclusiveTokenUsage" <*> fields .:! "lastCallTokenUsage" <*> pure (additionalFields usageKeys fields)

instance ToJSON SessionTokenUsageChanged where
  toJSON event =
    objectWithAdditionalFields usageKeys (sessionUsageAdditionalFields event) $
      ["type" .= String "session_token_usage_changed", "sessionId" .= sessionUsageId event, "tokenUsage" .= sessionUsageTokens event]
        <> optionalField "inclusiveTokenUsage" (sessionUsageInclusive event)
        <> optionalField "lastCallTokenUsage" (sessionUsageLastCall event)

-- | One working-state transition payload, separate from completion events.
data DroidWorkingStateChanged = DroidWorkingStateChanged
  { workingStateNewState :: !DroidWorkingState,
    workingStateAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON DroidWorkingStateChanged where
  parseJSON = withObject "DroidWorkingStateChanged" $ \fields -> do
    requireLiteral "type" "droid_working_state_changed" fields
    DroidWorkingStateChanged <$> fields .: "newState" <*> pure (additionalFields ["type", "newState"] fields)

instance ToJSON DroidWorkingStateChanged where
  toJSON event = objectWithAdditionalFields ["type", "newState"] (workingStateAdditionalFields event) ["type" .= String "droid_working_state_changed", "newState" .= workingStateNewState event]

-- | Retraction of an assistant message by identifier.
data AssistantMessageRetracted = AssistantMessageRetracted
  { retractedMessageId :: !Text,
    retractedAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AssistantMessageRetracted where
  parseJSON = withObject "AssistantMessageRetracted" $ \fields -> do
    requireLiteral "type" "assistant_message_retracted" fields
    AssistantMessageRetracted <$> fields .: "messageId" <*> pure (additionalFields ["type", "messageId"] fields)

instance ToJSON AssistantMessageRetracted where
  toJSON event = objectWithAdditionalFields ["type", "messageId"] (retractedAdditionalFields event) ["type" .= String "assistant_message_retracted", "messageId" .= retractedMessageId event]

-- | Session title update, optionally correlated with a request and origin.
data SessionTitleUpdated = SessionTitleUpdated
  { updatedSessionTitle :: !Text,
    titleRequestId :: !(Maybe Text),
    titleUpdateType :: !(Maybe TitleUpdateType),
    titleAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionTitleUpdated where
  parseJSON = withObject "SessionTitleUpdated" $ \fields -> do
    requireLiteral "type" "session_title_updated" fields
    SessionTitleUpdated <$> fields .: "title" <*> fields .:! "requestId" <*> fields .:! "updateType" <*> pure (additionalFields titleKeys fields)

instance ToJSON SessionTitleUpdated where
  toJSON event = objectWithAdditionalFields titleKeys (titleAdditionalFields event) (["type" .= String "session_title_updated", "title" .= updatedSessionTitle event] <> optionalField "requestId" (titleRequestId event) <> optionalField "updateType" (titleUpdateType event))

-- | Updated working-directory metadata; decoding does not change directory.
data SessionWorkingDirectoryChanged = SessionWorkingDirectoryChanged
  { updatedWorkingDirectory :: !Text,
    workingDirectoryAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionWorkingDirectoryChanged where
  parseJSON = withObject "SessionWorkingDirectoryChanged" $ \fields -> do
    requireLiteral "type" "session_working_directory_changed" fields
    SessionWorkingDirectoryChanged <$> fields .: "cwd" <*> pure (additionalFields ["type", "cwd"] fields)

instance ToJSON SessionWorkingDirectoryChanged where
  toJSON event = objectWithAdditionalFields ["type", "cwd"] (workingDirectoryAdditionalFields event) ["type" .= String "session_working_directory_changed", "cwd" .= updatedWorkingDirectory event]

-- | Discarded queued-message text and optional request correlation.
data QueuedMessagesDiscarded = QueuedMessagesDiscarded
  { discardedMessageText :: !Text,
    discardedRequestId :: !(Maybe Text),
    discardedAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON QueuedMessagesDiscarded where
  parseJSON = withObject "QueuedMessagesDiscarded" $ \fields -> do
    requireLiteral "type" "queued_messages_discarded" fields
    QueuedMessagesDiscarded <$> fields .: "text" <*> fields .:! "requestId" <*> pure (additionalFields discardedKeys fields)

instance ToJSON QueuedMessagesDiscarded where
  toJSON event = objectWithAdditionalFields discardedKeys (discardedAdditionalFields event) (["type" .= String "queued_messages_discarded", "text" .= discardedMessageText event] <> optionalField "requestId" (discardedRequestId event))

-- | Structured output is required but nullable. Nothing emits null; it does
-- not omit the field. The object is not validated against a caller's schema.
data StructuredOutput = StructuredOutput
  { structuredMessageId :: !Text,
    structuredOutputValue :: !(Maybe Object),
    structuredAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON StructuredOutput where
  parseJSON = withObject "StructuredOutput" $ \fields -> do
    requireLiteral "type" "structured_output" fields
    StructuredOutput <$> fields .: "messageId" <*> fields .: "structuredOutput" <*> pure (additionalFields structuredKeys fields)

instance ToJSON StructuredOutput where
  toJSON event = objectWithAdditionalFields structuredKeys (structuredAdditionalFields event) ["type" .= String "structured_output", "messageId" .= structuredMessageId event, "structuredOutput" .= structuredOutputValue event]

-- | Compaction boundary metadata. The visible boundary identifier is required
-- but nullable; the removed count may be fractional in the wire number domain.
data SessionCompacted = SessionCompacted
  { compactedSummaryId :: !Text,
    compactedRemovedCount :: !NonNegativeNumber,
    compactedVisibleBoundaryId :: !(Maybe Text),
    compactedAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionCompacted where
  parseJSON = withObject "SessionCompacted" $ \fields -> do
    requireLiteral "type" "session_compacted" fields
    SessionCompacted <$> fields .: "summaryId" <*> fields .: "removedCount" <*> fields .: "visibleBoundaryMessageId" <*> pure (additionalFields compactedKeys fields)

instance ToJSON SessionCompacted where
  toJSON event = objectWithAdditionalFields compactedKeys (compactedAdditionalFields event) ["type" .= String "session_compacted", "summaryId" .= compactedSummaryId event, "removedCount" .= compactedRemovedCount event, "visibleBoundaryMessageId" .= compactedVisibleBoundaryId event]

-- | Nested error details, retained as data without logging their contents.
data NotificationError = NotificationError
  { notificationErrorName :: !Text,
    notificationErrorMessage :: !Text,
    notificationErrorAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON NotificationError where
  parseJSON = withObject "NotificationError" $ \fields ->
    NotificationError <$> fields .: "name" <*> fields .: "message" <*> pure (additionalFields ["name", "message"] fields)

instance ToJSON NotificationError where
  toJSON err = objectWithAdditionalFields ["name", "message"] (notificationErrorAdditionalFields err) ["name" .= notificationErrorName err, "message" .= notificationErrorMessage err]

-- | An error event. timestamp remains a string, while exitCode is an
-- unbounded integer as specified by the schema, not an OS-sized status value.
data ErrorNotification = ErrorNotification
  { errorNotificationMessage :: !Text,
    errorNotificationType :: !NotificationErrorType,
    errorNotificationTimestamp :: !Text,
    errorNotificationError :: !(Maybe NotificationError),
    errorNotificationExitCode :: !(Maybe Integer),
    errorNotificationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ErrorNotification where
  parseJSON = withObject "ErrorNotification" $ \fields -> do
    requireLiteral "type" "error" fields
    ErrorNotification <$> fields .: "message" <*> fields .: "errorType" <*> fields .: "timestamp" <*> fields .:! "error" <*> fields .:! "exitCode" <*> pure (additionalFields errorKeys fields)

instance ToJSON ErrorNotification where
  toJSON event =
    objectWithAdditionalFields errorKeys (errorNotificationAdditionalFields event) $
      ["type" .= String "error", "message" .= errorNotificationMessage event, "errorType" .= errorNotificationType event, "timestamp" .= errorNotificationTimestamp event]
        <> optionalField "error" (errorNotificationError event)
        <> optionalField "exitCode" (errorNotificationExitCode event)

-- | Availability of a child session; this does not attach or open the child.
data ChildSessionAvailable = ChildSessionAvailable
  { availableChildSessionId :: !Text,
    availableChildTimestamp :: !Scientific,
    availableChildToolUseId :: !(Maybe Text),
    availableChildSubagentType :: !(Maybe Text),
    availableChildDescription :: !(Maybe Text),
    availableChildAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ChildSessionAvailable where
  parseJSON = withObject "ChildSessionAvailable" $ \fields -> do
    requireLiteral "type" "child_session_available" fields
    ChildSessionAvailable <$> fields .: "childSessionId" <*> fields .: "timestamp" <*> fields .:! "toolUseId" <*> fields .:! "subagentType" <*> fields .:! "description" <*> pure (additionalFields childKeys fields)

instance ToJSON ChildSessionAvailable where
  toJSON event =
    objectWithAdditionalFields childKeys (availableChildAdditionalFields event) $
      ["type" .= String "child_session_available", "childSessionId" .= availableChildSessionId event, "timestamp" .= availableChildTimestamp event]
        <> optionalField "toolUseId" (availableChildToolUseId event)
        <> optionalField "subagentType" (availableChildSubagentType event)
        <> optionalField "description" (availableChildDescription event)

-- | Declared permission choices, including cancellation. These values
-- represent a peer's selection; the codec does not apply a permission policy.
data ToolConfirmationOutcome
  = ConfirmProceedOnce
  | ConfirmProceedAlways
  | ConfirmProceedAlwaysFile
  | ConfirmProceedAutoRun
  | ConfirmProceedAutoRunLow
  | ConfirmProceedAutoRunMedium
  | ConfirmProceedAutoRunHigh
  | ConfirmProceedNewSession
  | ConfirmProceedNewSessionLow
  | ConfirmProceedNewSessionMedium
  | ConfirmProceedNewSessionHigh
  | ConfirmProceedEdit
  | ConfirmProceedAlwaysTools
  | ConfirmProceedAlwaysServer
  | ConfirmProceedReportFalsePositive
  | ConfirmCancel
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON ToolConfirmationOutcome where
  parseJSON = genericParseJSON confirmationOptions

instance ToJSON ToolConfirmationOutcome where
  toJSON = genericToJSON confirmationOptions
  toEncoding = genericToEncoding confirmationOptions

-- | Objective tool-execution phases. The settled variants remain distinct.
data ToolExecutionPhase
  = ExecutionStreamingInput
  | ExecutionQueued
  | ExecutionExecuting
  | ExecutionSettledAfterExecution
  | ExecutionSettledWithoutExecution
  | ExecutionSettledUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON ToolExecutionPhase where
  parseJSON = genericParseJSON executionOptions

instance ToJSON ToolExecutionPhase where
  toJSON = genericToJSON executionOptions
  toEncoding = genericToEncoding executionOptions

-- | The kind of a nested tool-progress update, not its free-form status text.
data ToolProgressKind = ProgressToolCall | ProgressToolResult | ProgressError | ProgressStatus | ProgressMessage
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON ToolProgressKind where
  parseJSON = genericParseJSON progressOptions

instance ToJSON ToolProgressKind where
  toJSON = genericToJSON progressOptions
  toEncoding = genericToEncoding progressOptions

-- | The six declared retry causes. Unknown is an explicit literal rather
-- than a decoder fallback for newly introduced causes.
data LlmRetryReason = RetryOverloaded | RetryRateLimited | RetryTimeout | RetryNetwork | RetryEmptyResponse | RetryUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON LlmRetryReason where
  parseJSON = genericParseJSON retryOptions

instance ToJSON LlmRetryReason where
  toJSON = genericToJSON retryOptions
  toEncoding = genericToEncoding retryOptions

-- | The existing tool-result block with a required message identifier.
-- Block fields and extensions retain their ordinary content-codec semantics.
data ToolResultNotification = ToolResultNotification
  { resultNotificationMessageId :: !Text,
    resultNotificationBlock :: !ToolResultBlock
  }
  deriving stock (Eq, Show)

instance FromJSON ToolResultNotification where
  parseJSON = withObject "ToolResultNotification" $ \fields ->
    ToolResultNotification
      <$> fields .: "messageId"
      <*> parseJSON (Object (additionalFields ["messageId"] fields))

instance ToJSON ToolResultNotification where
  toJSON event =
    objectWithAdditionalFields
      ["messageId"]
      (contentBlockObject (ContentToolResult (resultNotificationBlock event)))
      ["messageId" .= resultNotificationMessageId event]

-- | A complete, typed tool invocation carried by a notification.
data ToolCallNotification = ToolCallNotification
  { calledToolUse :: !ToolUseBlock,
    toolCallAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolCallNotification where
  parseJSON = withObject "ToolCallNotification" $ \fields -> do
    requireLiteral "type" "tool_call" fields
    ToolCallNotification <$> fields .: "toolUse" <*> pure (additionalFields ["type", "toolUse"] fields)

instance ToJSON ToolCallNotification where
  toJSON event = objectWithAdditionalFields ["type", "toolUse"] (toolCallAdditionalFields event) ["type" .= String "tool_call", "toolUse" .= calledToolUse event]

-- | A tool heartbeat, without a timestamp or fabricated progress percentage.
data ToolExecutionHeartbeat = ToolExecutionHeartbeat
  { heartbeatToolUseId :: !Text,
    heartbeatToolName :: !Text,
    heartbeatAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolExecutionHeartbeat where
  parseJSON = withObject "ToolExecutionHeartbeat" $ \fields -> do
    requireLiteral "type" "tool_execution_heartbeat" fields
    ToolExecutionHeartbeat <$> fields .: "toolUseId" <*> fields .: "toolName" <*> pure (additionalFields toolIdentityKeys fields)

instance ToJSON ToolExecutionHeartbeat where
  toJSON event = objectWithAdditionalFields toolIdentityKeys (heartbeatAdditionalFields event) ["type" .= String "tool_execution_heartbeat", "toolUseId" .= heartbeatToolUseId event, "toolName" .= heartbeatToolName event]

-- | A tool's reported lifecycle transition, not an instruction to run it.
data ToolExecutionPhaseChanged = ToolExecutionPhaseChanged
  { phaseToolUseId :: !Text,
    phaseToolName :: !Text,
    changedToolPhase :: !ToolExecutionPhase,
    phaseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolExecutionPhaseChanged where
  parseJSON = withObject "ToolExecutionPhaseChanged" $ \fields -> do
    requireLiteral "type" "tool_execution_phase_changed" fields
    ToolExecutionPhaseChanged <$> fields .: "toolUseId" <*> fields .: "toolName" <*> fields .: "phase" <*> pure (additionalFields phaseKeys fields)

instance ToJSON ToolExecutionPhaseChanged where
  toJSON event = objectWithAdditionalFields phaseKeys (phaseAdditionalFields event) ["type" .= String "tool_execution_phase_changed", "toolUseId" .= phaseToolUseId event, "toolName" .= phaseToolName event, "phase" .= changedToolPhase event]

-- | Nested progress data. Only the kind is required. Parameters are an
-- explicitly arbitrary JSON object; status and output remain opaque text.
data ToolProgressUpdate = ToolProgressUpdate
  { progressKind :: !ToolProgressKind,
    progressToolName :: !(Maybe Text),
    progressStatus :: !(Maybe Text),
    progressDetails :: !(Maybe Text),
    progressText :: !(Maybe Text),
    progressError :: !(Maybe Text),
    progressTimestamp :: !(Maybe Scientific),
    progressParameters :: !(Maybe Object),
    progressValueSnippet :: !(Maybe Text),
    progressTerminalId :: !(Maybe Text),
    progressFullOutput :: !(Maybe Text),
    progressSubagentSessionId :: !(Maybe Text),
    progressAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolProgressUpdate where
  parseJSON = withObject "ToolProgressUpdate" $ \fields ->
    ToolProgressUpdate
      <$> fields .: "type"
      <*> fields .:! "toolName"
      <*> fields .:! "status"
      <*> fields .:! "details"
      <*> fields .:! "text"
      <*> fields .:! "error"
      <*> fields .:! "timestamp"
      <*> fields .:! "parameters"
      <*> fields .:! "valueSnippet"
      <*> fields .:! "terminalId"
      <*> fields .:! "fullOutput"
      <*> fields .:! "subagentSessionId"
      <*> pure (additionalFields progressKeys fields)

instance ToJSON ToolProgressUpdate where
  toJSON update =
    objectWithAdditionalFields progressKeys (progressAdditionalFields update) $
      ["type" .= progressKind update]
        <> optionalField "toolName" (progressToolName update)
        <> optionalField "status" (progressStatus update)
        <> optionalField "details" (progressDetails update)
        <> optionalField "text" (progressText update)
        <> optionalField "error" (progressError update)
        <> optionalField "timestamp" (progressTimestamp update)
        <> optionalField "parameters" (progressParameters update)
        <> optionalField "valueSnippet" (progressValueSnippet update)
        <> optionalField "terminalId" (progressTerminalId update)
        <> optionalField "fullOutput" (progressFullOutput update)
        <> optionalField "subagentSessionId" (progressSubagentSessionId update)

-- | A progress update correlated to a tool invocation.
data ToolProgressUpdateNotification = ToolProgressUpdateNotification
  { progressNotificationToolUseId :: !Text,
    progressNotificationToolName :: !Text,
    progressNotificationUpdate :: !ToolProgressUpdate,
    progressNotificationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolProgressUpdateNotification where
  parseJSON = withObject "ToolProgressUpdateNotification" $ \fields -> do
    requireLiteral "type" "tool_progress_update" fields
    ToolProgressUpdateNotification <$> fields .: "toolUseId" <*> fields .: "toolName" <*> fields .: "update" <*> pure (additionalFields progressNotificationKeys fields)

instance ToJSON ToolProgressUpdateNotification where
  toJSON event = objectWithAdditionalFields progressNotificationKeys (progressNotificationAdditionalFields event) ["type" .= String "tool_progress_update", "toolUseId" .= progressNotificationToolUseId event, "toolName" .= progressNotificationToolName event, "update" .= progressNotificationUpdate event]

-- | Retry metadata. Attempt retains the supplied number domain; decoding
-- does not sleep, retry a request or apply a retry policy.
data LlmRetry = LlmRetry
  { retryAttempt :: !Scientific,
    retryReason :: !LlmRetryReason,
    retryAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON LlmRetry where
  parseJSON = withObject "LlmRetry" $ \fields -> do
    requireLiteral "type" "llm_retry" fields
    LlmRetry <$> fields .: "attempt" <*> fields .: "reason" <*> pure (additionalFields retryKeys fields)

instance ToJSON LlmRetry where
  toJSON event = objectWithAdditionalFields retryKeys (retryAdditionalFields event) ["type" .= String "llm_retry", "attempt" .= retryAttempt event, "reason" .= retryReason event]

-- | A reported permission resolution. Batched IDs retain order and duplicates,
-- and an empty array remains valid. Parsing never grants permissions.
data PermissionResolved = PermissionResolved
  { resolvedRequestId :: !Text,
    resolvedToolUseIds :: ![Text],
    resolvedSelection :: !ToolConfirmationOutcome,
    resolvedAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON PermissionResolved where
  parseJSON = withObject "PermissionResolved" $ \fields -> do
    requireLiteral "type" "permission_resolved" fields
    PermissionResolved <$> fields .: "requestId" <*> fields .: "toolUseIds" <*> fields .: "selectedOption" <*> pure (additionalFields permissionKeys fields)

instance ToJSON PermissionResolved where
  toJSON event = objectWithAdditionalFields permissionKeys (resolvedAdditionalFields event) ["type" .= String "permission_resolved", "requestId" .= resolvedRequestId event, "toolUseIds" .= resolvedToolUseIds event, "selectedOption" .= resolvedSelection event]

-- | The nine declared lifecycle hook event names, preserving their case.
data DroidHookEvent
  = HookPreToolUse
  | HookPostToolUse
  | HookNotification
  | HookUserPromptSubmit
  | HookStop
  | HookSubagentStop
  | HookPreCompact
  | HookSessionStart
  | HookSessionEnd
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON DroidHookEvent where
  parseJSON = genericParseJSON hookEventOptions

instance ToJSON DroidHookEvent where
  toJSON = genericToJSON hookEventOptions
  toEncoding = genericToEncoding hookEventOptions

-- | Completed hook events cannot carry the in-progress executing status.
data HookCompletionStatus = HookSucceeded | HookFailed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON HookCompletionStatus where
  parseJSON = withText "HookCompletionStatus" $ \case
    "completed" -> pure HookSucceeded
    "error" -> pure HookFailed
    _ -> fail "Unknown hook completion status"

instance ToJSON HookCompletionStatus where
  toJSON HookSucceeded = String "completed"
  toJSON HookFailed = String "error"

-- | HookCommandSchema has the same fields and codec as persisted commands.
type HookCommand = PersistedHookCommand

-- | Hook output plus optional command/timeout context. The persisted output
-- codec owns exitCode/stdout/stderr/suppressOutput and extension preservation.
data HookResult = HookResult
  { hookResultOutput :: !PersistedHookResult,
    hookResultCommand :: !(Maybe Text),
    hookResultTimeout :: !(Maybe Scientific)
  }
  deriving stock (Eq, Show)

instance FromJSON HookResult where
  parseJSON = withObject "HookResult" $ \fields ->
    HookResult
      <$> parseJSON (Object (additionalFields ["command", "timeout"] fields))
      <*> fields .:! "command"
      <*> fields .:! "timeout"

instance ToJSON HookResult where
  toJSON result =
    objectWithAdditionalFields ["command", "timeout"] (persistedHookResultObject (hookResultOutput result)) $
      optionalField "command" (hookResultCommand result) <> optionalField "timeout" (hookResultTimeout result)

-- | Hook startup payload. Its event name is deliberately free-form text,
-- unlike the constrained optional event name in a completed payload.
data HookExecutionStarted = HookExecutionStarted
  { startedHookId :: !Text,
    startedHookEventName :: !Text,
    startedHookCommands :: ![HookCommand],
    startedHookMatcher :: !(Maybe Text),
    startedHookToolCallId :: !(Maybe Text),
    startedHookHidden :: !(Maybe Bool),
    startedHookParallel :: !(Maybe Bool),
    startedHookParallelGroupId :: !(Maybe Text),
    startedHookParentId :: !(Maybe Text),
    startedHookOrder :: !(Maybe Scientific),
    startedHookAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON HookExecutionStarted where
  parseJSON = withObject "HookExecutionStarted" $ \fields -> do
    requireLiteral "type" "hook_execution_started" fields
    HookExecutionStarted
      <$> fields .: "hookId"
      <*> fields .: "hookEventName"
      <*> fields .: "hookCommands"
      <*> fields .:! "hookMatcher"
      <*> fields .:! "hookToolCallId"
      <*> fields .:! "hiddenFromUserViews"
      <*> fields .:! "isParallelExecution"
      <*> fields .:! "parallelGroupId"
      <*> fields .:! "hookParentId"
      <*> fields .:! "hookOrder"
      <*> pure (additionalFields hookStartedKeys fields)

instance ToJSON HookExecutionStarted where
  toJSON event =
    objectWithAdditionalFields hookStartedKeys (startedHookAdditionalFields event) $
      ["type" .= String "hook_execution_started", "hookId" .= startedHookId event, "hookEventName" .= startedHookEventName event, "hookCommands" .= startedHookCommands event]
        <> optionalField "hookMatcher" (startedHookMatcher event)
        <> optionalField "hookToolCallId" (startedHookToolCallId event)
        <> optionalField "hiddenFromUserViews" (startedHookHidden event)
        <> optionalField "isParallelExecution" (startedHookParallel event)
        <> optionalField "parallelGroupId" (startedHookParallelGroupId event)
        <> optionalField "hookParentId" (startedHookParentId event)
        <> optionalField "hookOrder" (startedHookOrder event)

-- | A hook's terminal report; receiving it does not execute or retry hooks.
data HookExecutionCompleted = HookExecutionCompleted
  { completedHookId :: !Text,
    completedHookStatus :: !HookCompletionStatus,
    completedHookEventName :: !(Maybe DroidHookEvent),
    completedHookMatcher :: !(Maybe Text),
    completedHookResults :: !(Maybe [HookResult]),
    completedHookToolCallId :: !(Maybe Text),
    completedHookHidden :: !(Maybe Bool),
    completedHookParentId :: !(Maybe Text),
    completedHookOrder :: !(Maybe Scientific),
    completedHookPreventedAction :: !(Maybe Bool),
    completedHookAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON HookExecutionCompleted where
  parseJSON = withObject "HookExecutionCompleted" $ \fields -> do
    requireLiteral "type" "hook_execution_completed" fields
    HookExecutionCompleted
      <$> fields .: "hookId"
      <*> fields .: "hookStatus"
      <*> fields .:! "hookEventName"
      <*> fields .:! "hookMatcher"
      <*> fields .:! "hookResults"
      <*> fields .:! "hookToolCallId"
      <*> fields .:! "hiddenFromUserViews"
      <*> fields .:! "hookParentId"
      <*> fields .:! "hookOrder"
      <*> fields .:! "hookPreventedAction"
      <*> pure (additionalFields hookCompletedKeys fields)

instance ToJSON HookExecutionCompleted where
  toJSON event =
    objectWithAdditionalFields hookCompletedKeys (completedHookAdditionalFields event) $
      ["type" .= String "hook_execution_completed", "hookId" .= completedHookId event, "hookStatus" .= completedHookStatus event]
        <> optionalField "hookEventName" (completedHookEventName event)
        <> optionalField "hookMatcher" (completedHookMatcher event)
        <> optionalField "hookResults" (completedHookResults event)
        <> optionalField "hookToolCallId" (completedHookToolCallId event)
        <> optionalField "hiddenFromUserViews" (completedHookHidden event)
        <> optionalField "hookParentId" (completedHookParentId event)
        <> optionalField "hookOrder" (completedHookOrder event)
        <> optionalField "hookPreventedAction" (completedHookPreventedAction event)

hookEventOptions :: Options
hookEventOptions = enumOptions "Hook" id

hookStartedKeys, hookCompletedKeys :: [Key]
hookStartedKeys = ["type", "hookId", "hookEventName", "hookCommands", "hookMatcher", "hookToolCallId", "hiddenFromUserViews", "isParallelExecution", "parallelGroupId", "hookParentId", "hookOrder"]
hookCompletedKeys = ["type", "hookId", "hookStatus", "hookEventName", "hookMatcher", "hookResults", "hookToolCallId", "hiddenFromUserViews", "hookParentId", "hookOrder", "hookPreventedAction"]

confirmationOptions, executionOptions, progressOptions, retryOptions :: Options
confirmationOptions = enumOptions "Confirm" (camelTo2 '_')
executionOptions = enumOptions "Execution" (camelTo2 '_')
progressOptions = enumOptions "Progress" (camelTo2 '_')
retryOptions = enumOptions "Retry" (camelTo2 '_')

toolIdentityKeys, phaseKeys, progressKeys, progressNotificationKeys, retryKeys, permissionKeys :: [Key]
toolIdentityKeys = ["type", "toolUseId", "toolName"]
phaseKeys = toolIdentityKeys <> ["phase"]
progressKeys = ["type", "toolName", "status", "details", "text", "error", "timestamp", "parameters", "valueSnippet", "terminalId", "fullOutput", "subagentSessionId"]
progressNotificationKeys = toolIdentityKeys <> ["update"]
retryKeys = ["type", "attempt", "reason"]
permissionKeys = ["type", "requestId", "toolUseIds", "selectedOption"]

turnReasonOptions, workingOptions, titleOptions, errorTypeOptions :: Options
turnReasonOptions = enumOptions "Turn" (camelTo2 '_')
workingOptions = enumOptions "Working" (camelTo2 '_')
titleOptions = enumOptions "Title" (camelTo2 '_')
errorTypeOptions = enumOptions "Notify" id

deltaKeys, completeKeys, thinkingCompleteKeys, createKeys, turnKeys, usageKeys, titleKeys, discardedKeys, structuredKeys, compactedKeys, errorKeys, childKeys :: [Key]
deltaKeys = ["type", "messageId", "blockIndex", "textDelta"]
completeKeys = ["type", "messageId", "blockIndex"]
thinkingCompleteKeys = completeKeys <> ["durationMs"]
createKeys = ["type", "message", "parentId", "requestId"]
turnKeys = ["type", "reason", "tokenUsage", "turnId", "cumulativeTokenUsage", "childTokenUsage", "cumulativeChildTokenUsage", "durationMs"]
usageKeys = ["type", "sessionId", "tokenUsage", "inclusiveTokenUsage", "lastCallTokenUsage"]
titleKeys = ["type", "title", "requestId", "updateType"]
discardedKeys = ["type", "text", "requestId"]
structuredKeys = ["type", "messageId", "structuredOutput"]
compactedKeys = ["type", "summaryId", "removedCount", "visibleBoundaryMessageId"]
errorKeys = ["type", "message", "errorType", "timestamp", "error", "exitCode"]
childKeys = ["type", "childSessionId", "timestamp", "toolUseId", "subagentType", "description"]
