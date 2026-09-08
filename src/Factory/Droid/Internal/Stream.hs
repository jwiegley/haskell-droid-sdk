{-# LANGUAGE OverloadedStrings #-}

module Factory.Droid.Internal.Stream
  ( DroidEvent (..),
    DroidStreamMode (..),
    DroidResult (..),
    StreamState,
    initialStreamState,
    decodeNotification,
    decodeSessionNotification,
    completeEvent,
    stepStream,
    finishStream,
  )
where

import Data.Aeson (FromJSON (parseJSON), Object, Value (Object), withObject, (.:), (.:!))
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Schema.Content (ContentBlock (..), TextBlock (..), ToolUseBlock)
import Factory.Droid.Schema.Enums (MessageRole (RoleAssistant))
import Factory.Droid.Schema.Messages (Message (..))
import Factory.Droid.Schema.Notifications
import Factory.Droid.Schema.RPC (BaseNotification (..), JsonRpcBaseNotification, WithEnvelope (..))
import Factory.Droid.Schema.Settings (SettingsUpdated)

-- | Complete message-level artifacts, or all partial/metadata notifications too.
data DroidStreamMode = CompleteMessages | AllEvents
  deriving stock (Eq, Show)

-- | Typed stream payloads. OtherNotificationEvent retains untyped extension or
-- not-yet-adapted notifications; a malformed known payload is never relabeled.
data DroidEvent
  = MessageEvent !CreateMessage
  | TextDeltaEvent !AssistantTextDelta
  | TextCompleteEvent !AssistantTextComplete
  | ThinkingDeltaEvent !ThinkingTextDelta
  | ThinkingCompleteEvent !ThinkingTextComplete
  | ToolCallDeltaEvent !ToolCallNotification
  | ToolCallEvent !ToolUseBlock
  | ToolResultEvent !ToolResultNotification
  | ToolProgressEvent !ToolProgressUpdateNotification
  | ToolHeartbeatEvent !ToolExecutionHeartbeat
  | ToolPhaseEvent !ToolExecutionPhaseChanged
  | RetryEvent !LlmRetry
  | UsageEvent !SessionTokenUsageChanged
  | WorkingStateEvent !DroidWorkingStateChanged
  | TitleEvent !SessionTitleUpdated
  | WorkingDirectoryEvent !SessionWorkingDirectoryChanged
  | SettingsUpdatedEvent !SettingsUpdated
  | PermissionEvent !PermissionResolved
  | HookStartedEvent !HookExecutionStarted
  | HookCompletedEvent !HookExecutionCompleted
  | StructuredOutputEvent !StructuredOutput
  | MessageRetractedEvent !AssistantMessageRetracted
  | TurnCompletedEvent !AgentTurnCompleted
  | ErrorEvent !ErrorNotification
  | OtherNotificationEvent !Object
  deriving stock (Eq)

instance Show DroidEvent where
  show _ = "DroidEvent <redacted>"

-- | Terminal data. Text uses the latest observed assistant message's current
-- buffer, or surviving buffers when empty. Events retain complete artifacts in
-- arrival order, excluding the terminal event. Structured output is captured,
-- not schema-validated.
data DroidResult = DroidResult
  { resultSessionId :: !Text,
    resultText :: !Text,
    resultCompletion :: !AgentTurnCompleted,
    resultErrors :: ![ErrorNotification],
    resultEvents :: ![DroidEvent],
    resultStructuredOutput :: !(Maybe Object)
  }
  deriving stock (Eq)

instance Show DroidResult where
  show _ = "DroidResult <redacted>"

type BlockKey = (Text, Scientific)

data StreamState = StreamState
  { trackedOrder :: ![BlockKey],
    trackedBlocks :: !(Map.Map BlockKey Text),
    trackedDelivered :: !(Map.Map BlockKey Text),
    trackedMessages :: ![Text],
    trackedEvents :: ![DroidEvent],
    trackedErrors :: ![ErrorNotification],
    trackedOutput :: !(Maybe (Text, Object))
  }

initialStreamState :: StreamState
initialStreamState = StreamState [] Map.empty Map.empty [] [] [] Nothing

completeEvent :: DroidEvent -> Bool
completeEvent = \case
  MessageEvent _ -> True
  ToolCallEvent _ -> True
  ToolResultEvent _ -> True
  HookStartedEvent _ -> True
  HookCompletedEvent _ -> True
  MessageRetractedEvent _ -> True
  ErrorEvent _ -> True
  TurnCompletedEvent _ -> True
  _ -> False

-- | Accumulate once for both APIs; returned text is append-only callback output.
-- Complete snapshots may correct it, and retractions cannot undo past callbacks.
stepStream :: StreamState -> DroidEvent -> (StreamState, [Text])
stepStream state event = case event of
  TextDeltaEvent delta ->
    let identifier = assistantDeltaMessageId delta
        key = (identifier, assistantDeltaBlockIndex delta)
        order = if Map.member key (trackedBlocks recorded) then trackedOrder recorded else key : trackedOrder recorded
        text = Map.findWithDefault "" key (trackedBlocks recorded) <> assistantDeltaText delta
     in deliver (recorded {trackedOrder = order, trackedBlocks = Map.insert key text (trackedBlocks recorded), trackedMessages = rememberMessage identifier}) [(key, text)]
  MessageEvent created
    | messageRole message == RoleAssistant ->
        let identifier = messageId message
            pieces = [((identifier, fromIntegral index), textBlockText block) | (index, ContentText block) <- zip [0 :: Int ..] (messageContent message)]
            fresh = [(key, text) | (key, text) <- pieces, Map.notMember key (trackedBlocks recorded)]
            others = Map.filterWithKey (\(owner, _) _ -> owner /= identifier) (trackedBlocks recorded)
            blocks = Map.union (Map.fromList pieces) others
            order = reverse (map fst fresh) <> filter (`Map.member` blocks) (trackedOrder recorded)
         in deliver (recorded {trackedOrder = order, trackedBlocks = blocks, trackedMessages = rememberMessage identifier}) pieces
    where
      message = createdMessage created
  MessageRetractedEvent retracted ->
    let identifier = retractedMessageId retracted
        order = filter ((/= identifier) . fst) (trackedOrder recorded)
        blocks = Map.filterWithKey (\(owner, _) _ -> owner /= identifier) (trackedBlocks recorded)
        messages = filter (/= identifier) (trackedMessages recorded)
        output = case trackedOutput recorded of
          Just (owner, _) | owner == identifier -> Nothing
          other -> other
     in (recorded {trackedOrder = order, trackedBlocks = blocks, trackedMessages = messages, trackedOutput = output}, [])
  ErrorEvent err -> (recorded {trackedErrors = err : trackedErrors recorded}, [])
  StructuredOutputEvent output -> (recorded {trackedOutput = (structuredMessageId output,) <$> structuredOutputValue output}, [])
  _ -> (recorded, [])
  where
    recorded = case event of
      TurnCompletedEvent _ -> state
      _ | completeEvent event -> state {trackedEvents = event : trackedEvents state}
      _ -> state
    rememberMessage identifier = identifier : filter (/= identifier) (trackedMessages recorded)
    -- Delivery history cannot be rolled back with authoritative message buffers.
    deliver updated pieces =
      let additions = [(key, text, suffix) | (key, text) <- pieces, Just suffix <- [Text.stripPrefix (Map.findWithDefault "" key (trackedDelivered recorded)) text], not (Text.null suffix)]
          delivered = Map.union (Map.fromList [(key, text) | (key, text, _) <- additions]) (trackedDelivered recorded)
       in (updated {trackedDelivered = delivered}, [suffix | (_, _, suffix) <- additions])

finishStream :: Text -> AgentTurnCompleted -> StreamState -> DroidResult
finishStream identifier completion state =
  let latest = case trackedMessages state of
        owner : _ -> Text.concat [piece | ((message, _), piece) <- Map.toAscList (trackedBlocks state), message == owner]
        [] -> ""
      text = if Text.null latest then Text.concat [Map.findWithDefault "" key (trackedBlocks state) | key <- reverse (trackedOrder state)] else latest
   in DroidResult identifier text completion (reverse (trackedErrors state)) (reverse (trackedEvents state)) (snd <$> trackedOutput state)

decodeNotification :: Text -> Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeNotification identifier expectedTurn = decodeNotificationWithTurn identifier (Just expectedTurn)

decodeSessionNotification :: Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeSessionNotification identifier = decodeNotificationWithTurn identifier Nothing

decodeNotificationWithTurn :: Text -> Maybe Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeNotificationWithTurn identifier expectedTurn notification
  | baseNotificationMethod (envelopeBody notification) /= "droid.session_notification" = Right []
  | otherwise = case baseNotificationParams (envelopeBody notification) of
      Nothing -> Left "Missing notification parameters"
      Just value -> parseEither (withObject "session notification" parse) value
  where
    parse fields = do
      session <- fields .:! "sessionId"
      if maybe False (/= identifier) session then pure [] else fields .: "notification" >>= withObject "turn event" event
    one :: (FromJSON a) => (a -> DroidEvent) -> Object -> Parser [DroidEvent]
    one constructor fields = (: []) . constructor <$> parseJSON (Object fields)
    event fields = do
      kind <- fields .: "type" :: Parser Text
      case kind of
        "create_message" -> do
          created <- parseJSON (Object fields)
          let tools = [ToolCallEvent tool | ContentToolUse tool <- messageContent (createdMessage created)]
          pure (tools <> [MessageEvent created])
        "assistant_text_delta" -> one TextDeltaEvent fields
        "assistant_text_complete" -> one TextCompleteEvent fields
        "thinking_text_delta" -> one ThinkingDeltaEvent fields
        "thinking_text_complete" -> one ThinkingCompleteEvent fields
        "tool_call" -> one ToolCallDeltaEvent fields
        "tool_result" -> one ToolResultEvent fields
        "tool_progress_update" -> one ToolProgressEvent fields
        "tool_execution_heartbeat" -> one ToolHeartbeatEvent fields
        "tool_execution_phase_changed" -> one ToolPhaseEvent fields
        "llm_retry" -> one RetryEvent fields
        "session_token_usage_changed" -> one UsageEvent fields
        "droid_working_state_changed" -> one WorkingStateEvent fields
        "session_title_updated" -> one TitleEvent fields
        "session_working_directory_changed" -> one WorkingDirectoryEvent fields
        "settings_updated" -> one SettingsUpdatedEvent fields
        "permission_resolved" -> one PermissionEvent fields
        "hook_execution_started" -> one HookStartedEvent fields
        "hook_execution_completed" -> one HookCompletedEvent fields
        "structured_output" -> one StructuredOutputEvent fields
        "assistant_message_retracted" -> one MessageRetractedEvent fields
        "agent_turn_completed" -> case expectedTurn of
          Nothing -> one TurnCompletedEvent fields
          Just expected -> do
            turnId <- fields .: "turnId"
            if turnId /= expected then pure [] else one TurnCompletedEvent fields
        "error" -> one ErrorEvent fields
        _ -> pure [OtherNotificationEvent fields]
