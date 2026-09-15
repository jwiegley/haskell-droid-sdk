{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Factory.Droid.Internal.Stream
  ( DroidEvent (..),
    DroidStreamMode (..),
    DroidResult (..),
    StreamFeed,
    DroidStream,
    DroidLegacyStream,
    DroidIdleCompletion (..),
    withDroidLegacyStream,
    DroidStreamOptions (..),
    defaultDroidStreamOptions,
    DroidStreamFrame (..),
    DroidStreamResult (..),
    DroidStreamError (..),
    withDroidStream,
    withDroidStreamChecked,
    feedDroidEvent,
    feedDroidNotification,
    feedDroidDecoded,
    feedDroidError,
    consumeDroidStream,
    getDroidStreamResult,
    getDroidStreamFailure,
    droidStreamCompleted,
    closeDroidStream,
    StreamState,
    initialStreamState,
    DroidStreamSummary (..),
    summarizeStream,
    eventToolName,
    decodeNotification,
    decodeSessionNotification,
    decodeDaemonNotification,
    completeEvent,
    stepStream,
    finishStream,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, throwSTM, writeTVar)
import Control.Exception (Exception, SomeAsyncException, SomeException, fromException, mask, onException, throwIO, toException)
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object), withObject, (.:), (.:!))
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Scientific (Scientific, scientific)
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.Exception (finallyPreserving)
import Factory.Droid.Schema.Content (ContentBlock (..), TextBlock (..), ToolResultBlock (..), ToolUseBlock (..))
import Factory.Droid.Schema.Enums (MessageRole (RoleAssistant))
import Factory.Droid.Schema.MCP (McpAuthCompleted, McpAuthRequired, McpStatusChanged)
import Factory.Droid.Schema.Messages (Message (..))
import Factory.Droid.Schema.Mission (MissionFeaturesChanged, MissionHeartbeat, MissionProgressEntry, MissionStateChanged, MissionWorkerCompleted, MissionWorkerStarted)
import Factory.Droid.Schema.Notifications
import Factory.Droid.Schema.Primitives (nonNegativeNumberValue)
import Factory.Droid.Schema.RPC (BaseNotification (..), JsonRpcBaseNotification, WithEnvelope (..))
import Factory.Droid.Schema.Settings (SettingsUpdated)
import Factory.Droid.Schema.Usage (TokenUsage)
import GHC.Clock (getMonotonicTimeNSec)
import System.Timeout (timeout)

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
  | McpStatusEvent !McpStatusChanged
  | McpAuthRequiredEvent !McpAuthRequired
  | McpAuthCompletedEvent !McpAuthCompleted
  | MissionStateEvent !MissionStateChanged
  | MissionFeaturesEvent !MissionFeaturesChanged
  | MissionProgressEvent !MissionProgressEntry
  | MissionHeartbeatEvent !MissionHeartbeat
  | MissionWorkerStartedEvent !MissionWorkerStarted
  | MissionWorkerCompletedEvent !MissionWorkerCompleted
  | PermissionEvent !PermissionResolved
  | HookStartedEvent !HookExecutionStarted
  | HookCompletedEvent !HookExecutionCompleted
  | StructuredOutputEvent !StructuredOutput
  | MessageRetractedEvent !AssistantMessageRetracted
  | SessionCompactedEvent !SessionCompacted
  | QueuedMessagesDiscardedEvent !QueuedMessagesDiscarded
  | ChildSessionAvailableEvent !ChildSessionAvailable
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

-- | One turn, supplied by a caller-owned notification source. The optional
-- timeout covers the enclosing action, including its consumer callbacks.
data DroidStreamOptions = DroidStreamOptions
  { streamSessionId :: !Text,
    streamTurnId :: !Text,
    streamMode :: !DroidStreamMode,
    streamTimeoutMicros :: !(Maybe Int)
  }

instance Show DroidStreamOptions where
  show _ = "DroidStreamOptions <redacted>"

defaultDroidStreamOptions :: Text -> Text -> DroidStreamOptions
defaultDroidStreamOptions identifier turn = DroidStreamOptions identifier turn CompleteMessages Nothing

-- | Raw event, append-only text additions and the tool name known at admission.
-- Name enrichment never rewrites the original event or its extension fields.
data DroidStreamFrame = DroidStreamFrame
  { streamFrameEvent :: !DroidEvent,
    streamFrameText :: ![Text],
    streamFrameToolName :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show DroidStreamFrame where
  show _ = "DroidStreamFrame <redacted>"

-- | Preserve the wire outcome; effective duration uses its reported value,
-- including zero, or monotonic elapsed time at terminal admission.
data DroidStreamResult = DroidStreamResult
  { streamTurnResult :: !DroidResult,
    streamDurationMs :: !Scientific
  }
  deriving stock (Eq)

instance Show DroidStreamResult where
  show _ = "DroidStreamResult <redacted>"

data DroidStreamError = DroidStreamInvalidOptions | DroidStreamClosed | DroidStreamAlreadyConsumed | DroidStreamInvalidNotification | DroidStreamTimedOut
  deriving stock (Eq, Show)

instance Exception DroidStreamError

-- | An observed non-idle-to-idle boundary, not a peer completion reason,
-- success claim or acknowledgement. Usage is absent until actually observed.
newtype DroidIdleCompletion = DroidIdleCompletion
  { idleTokenUsage :: Maybe TokenUsage
  }
  deriving stock (Eq)

instance Show DroidIdleCompletion where
  show _ = "DroidIdleCompletion <redacted>"

data CompletionPolicy result where
  CompleteOnTurn :: Text -> CompletionPolicy DroidStreamResult
  CompleteOnIdle :: CompletionPolicy DroidIdleCompletion

type DroidStream = StreamFeed DroidStreamResult

type DroidLegacyStream = StreamFeed DroidIdleCompletion

data StreamFeed result = StreamFeed
  { feedSessionId :: !Text,
    feedMode :: !DroidStreamMode,
    feedTimeout :: !(Maybe Int),
    feedPolicy :: !(CompletionPolicy result),
    feedStarted :: !Integer,
    feedAdmission :: !(STM ()),
    feedState :: !(TVar (FeedState result))
  }

data FeedState result = FeedState
  { feedAccumulator :: !StreamState,
    feedFrames :: !(Seq DroidStreamFrame),
    feedOutcome :: !(Maybe (Either SomeException result)),
    feedClosed :: !Bool,
    feedClaimed :: !Bool,
    feedSeenBusy :: !Bool
  }

-- | No reader, transport, subscription or remote interruption is created here.
-- Compose the source's own bracket inside this scope, before submitting work.
withDroidStream :: DroidStreamOptions -> (DroidStream -> IO a) -> IO a
withDroidStream = withDroidStreamChecked (pure ())

withDroidStreamChecked :: STM () -> DroidStreamOptions -> (DroidStream -> IO a) -> IO a
withDroidStreamChecked admission options =
  withStreamFeed admission (streamSessionId options) (streamMode options) (streamTimeoutMicros options) (CompleteOnTurn (streamTurnId options))

-- | Legacy low-level iteration. Initial Idle and explicit completion events
-- are ignored; the first Idle after any non-idle working state ends the feed.
withDroidLegacyStream :: Text -> Maybe Int -> (DroidLegacyStream -> IO a) -> IO a
withDroidLegacyStream identifier deadline = withStreamFeed (pure ()) identifier AllEvents deadline CompleteOnIdle

withStreamFeed :: STM () -> Text -> DroidStreamMode -> Maybe Int -> CompletionPolicy result -> (StreamFeed result -> IO a) -> IO a
withStreamFeed admission identifier mode deadline policy action
  | maybe False (< 0) deadline = throwIO DroidStreamInvalidOptions
  | otherwise = do
      started <- toInteger <$> getMonotonicTimeNSec
      state <- newTVarIO (FeedState initialStreamState Seq.empty Nothing False False False)
      let stream = StreamFeed identifier mode deadline policy started admission state
          run = case deadline of
            Nothing -> action stream
            Just micros -> timeout micros (action stream) >>= maybe expired pure
          expired = do
            -- The cancelled consumer may already have closed the feed.
            atomically (modifyTVar' state (\current -> current {feedOutcome = feedOutcome current <|> Just (Left (toException DroidStreamTimedOut))}))
            throwIO DroidStreamTimedOut
      run `finallyPreserving` closeDroidStream stream

-- | Feed already-scoped data. Explicit streams check terminal IDs. Legacy
-- streams select the low-level text/tool/usage/state/error event family.
-- False means the event was filtered or the stream is terminal/closed/expired.
feedDroidEvent :: StreamFeed result -> DroidEvent -> IO Bool
feedDroidEvent stream event = feedDroidDecoded stream (pure (Right [event]))

-- | Decode local or daemon envelopes with their existing routing rules.
-- Unknown explicit-turn notifications remain raw; legacy filtering follows
-- decoding. Malformed known payloads fail in either mode.
feedDroidNotification :: StreamFeed result -> JsonRpcBaseNotification -> IO Bool
feedDroidNotification stream notification =
  feedDroidDecoded stream $
    pure $
      either (const (Left (toException DroidStreamInvalidNotification))) Right decoded
  where
    identifier = feedSessionId stream
    expected :: Maybe Text
    expected = case feedPolicy stream of
      CompleteOnTurn turn -> Just turn
      CompleteOnIdle -> Nothing
    decoded = case baseNotificationMethod (envelopeBody notification) of
      "daemon.session_notification" -> decodeDaemonNotification identifier expected notification
      _ -> decodeNotificationWithTurn identifier expected notification

feedDroidDecoded :: forall result. StreamFeed result -> STM (Either SomeException [DroidEvent]) -> IO Bool
feedDroidDecoded stream decoded = do
  now <- toInteger <$> getMonotonicTimeNSec
  atomically $ do
    state <- readTVar (feedState stream)
    if feedClosed state || isJust (feedOutcome state)
      then pure False
      else do
        feedAdmission stream
        let elapsed = now - feedStarted stream
        if maybe False (\micros -> elapsed >= toInteger micros * 1000) (feedTimeout stream)
          then writeTVar (feedState stream) (state {feedOutcome = Just (Left (toException DroidStreamTimedOut))}) >> pure False
          else do
            input <- decoded
            let (next, accepted) = case input of
                  Left cause -> (state {feedOutcome = Just (Left cause)}, True)
                  Right events -> foldl' (accept elapsed) (state, False) events
            writeTVar (feedState stream) next
            pure accepted
  where
    accept :: Integer -> (FeedState result, Bool) -> DroidEvent -> (FeedState result, Bool)
    accept elapsed (state, accepted) event
      | isJust (feedOutcome state) = (state, accepted)
      | otherwise = case feedPolicy stream of
          CompleteOnTurn expected -> case event of
            TurnCompletedEvent completion
              | Nothing <- completedTurnId completion -> (state {feedOutcome = Just (Left (toException DroidStreamInvalidNotification))}, True)
              | completedTurnId completion /= Just expected -> (state, accepted)
              | otherwise -> record state event (Just (DroidStreamResult (finishStream (feedSessionId stream) completion (feedAccumulator state)) (maybe (scientific elapsed (-6)) nonNegativeNumberValue (turnDurationMs completion))))
            _ -> record state event Nothing
          CompleteOnIdle -> case event of
            WorkingStateEvent changed
              | workingStateNewState changed /= WorkingIdle -> record (state {feedSeenBusy = True}) event Nothing
              | feedSeenBusy state -> record state event (Just (DroidIdleCompletion (trackedUsage (feedAccumulator state))))
              | otherwise -> (state, accepted)
            _ | legacyEvent event -> record state event Nothing
            _ -> (state, accepted)
    record :: FeedState result -> DroidEvent -> Maybe result -> (FeedState result, Bool)
    record state event outcome =
      let (next, pieces) = stepStream (feedAccumulator state) event
          frame = DroidStreamFrame event pieces (eventToolName next event)
          frames = if feedMode stream == AllEvents || completeEvent event then feedFrames state |> frame else feedFrames state
       in (state {feedAccumulator = next, feedFrames = frames, feedOutcome = Right <$> outcome}, True)

legacyEvent :: DroidEvent -> Bool
legacyEvent = \case
  TextDeltaEvent _ -> True
  ThinkingDeltaEvent _ -> True
  ToolCallEvent _ -> True
  ToolResultEvent _ -> True
  ToolProgressEvent _ -> True
  UsageEvent _ -> True
  ErrorEvent _ -> True
  _ -> False

-- | Ordinary failures follow the accepted event prefix. Asynchronous exceptions
-- are raised on the feeding thread, unchanged, rather than queued as data.
feedDroidError :: StreamFeed result -> SomeException -> IO Bool
feedDroidError stream cause
  | Just (_ :: SomeAsyncException) <- fromException cause = throwIO cause
  | otherwise = feedDroidDecoded stream (pure (Left cause))

-- | Exactly one consumer, on its calling thread. Callback failure or cancellation
-- closes this logical stream; it never sends a remote interrupt.
consumeDroidStream :: StreamFeed result -> (DroidStreamFrame -> IO ()) -> IO result
consumeDroidStream stream callback = mask $ \restore -> do
  atomically $ do
    feedAdmission stream
    state <- readTVar (feedState stream)
    when (feedClosed state) (throwSTM DroidStreamClosed)
    when (feedClaimed state) (throwSTM DroidStreamAlreadyConsumed)
    writeTVar (feedState stream) (state {feedClaimed = True})
  restore collect `onException` closeDroidStream stream
  where
    collect = do
      next <- atomically $ do
        feedAdmission stream
        state <- readTVar (feedState stream)
        when (feedClosed state) (throwSTM DroidStreamClosed)
        case Seq.viewl (feedFrames state) of
          frame :< rest -> writeTVar (feedState stream) (state {feedFrames = rest}) >> pure (Left frame)
          EmptyL -> case feedOutcome state of
            Just (Right result) -> pure (Right result)
            Just (Left cause) -> throwSTM cause
            Nothing -> retry
      case next of
        Left frame -> callback frame >> collect
        Right result -> pure result

-- | Immutable completion observation, available before callback consumption.
-- Failure is raised unchanged; no policy-matched boundary means Nothing.
getDroidStreamResult :: StreamFeed result -> STM (Maybe result)
getDroidStreamResult stream =
  readTVar (feedState stream) >>= \state -> case feedOutcome state of
    Just (Left cause) -> throwSTM cause
    Just (Right result) -> pure (Just result)
    Nothing -> pure Nothing

-- | Explicit sensitive data: the originating exception can contain payloads.
getDroidStreamFailure :: StreamFeed result -> STM (Maybe SomeException)
getDroidStreamFailure stream =
  readTVar (feedState stream) >>= \state -> pure $ case feedOutcome state of
    Just (Left cause) -> Just cause
    _ -> Nothing

droidStreamCompleted :: StreamFeed result -> STM Bool
droidStreamCompleted stream =
  readTVar (feedState stream) >>= \state -> pure $ case feedOutcome state of
    Just (Right _) -> True
    _ -> False

-- | Retire feeding and waiting, discarding undelivered frames. An already
-- observed completion or failure remains inspectable; close is idempotent.
closeDroidStream :: StreamFeed result -> IO ()
closeDroidStream stream = atomically (modifyTVar' (feedState stream) (\state -> state {feedClosed = True, feedFrames = Seq.empty}))

eventToolName :: StreamState -> DroidEvent -> Maybe Text
eventToolName state = \case
  ToolCallEvent tool -> Just (toolUseName tool)
  ToolCallDeltaEvent call -> Just (toolUseName (calledToolUse call))
  ToolResultEvent result -> Map.lookup (toolResultToolUseId (resultNotificationBlock result)) (trackedToolNames state)
  ToolProgressEvent progress ->
    let name = progressNotificationToolName progress
     in if Text.null name then Map.lookup (progressNotificationToolUseId progress) (trackedToolNames state) <|> Just name else Just name
  _ -> Nothing

type BlockKey = (Text, Scientific)

data StreamState = StreamState
  { trackedOrder :: ![BlockKey],
    trackedBlocks :: !(Map.Map BlockKey Text),
    trackedDelivered :: !(Map.Map BlockKey Text),
    trackedMessages :: ![Text],
    trackedEvents :: ![DroidEvent],
    trackedErrors :: ![ErrorNotification],
    trackedOutput :: !(Maybe (Text, Object)),
    trackedToolNames :: !(Map.Map Text Text),
    trackedUsage :: !(Maybe TokenUsage)
  }

initialStreamState :: StreamState
initialStreamState = StreamState [] Map.empty Map.empty [] [] [] Nothing Map.empty Nothing

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
  UsageEvent usage -> (recorded {trackedUsage = Just (sessionUsageTokens usage)}, [])
  _ -> (recorded, [])
  where
    recorded = retained {trackedToolNames = Map.union (Map.fromList [(toolUseId tool, toolUseName tool) | tool <- tools]) (trackedToolNames retained)}
    tools = case event of
      ToolCallEvent tool -> [tool]
      ToolCallDeltaEvent call -> [calledToolUse call]
      MessageEvent created -> [tool | ContentToolUse tool <- messageContent (createdMessage created)]
      _ -> []
    retained = case event of
      TurnCompletedEvent _ -> state
      _ | completeEvent event -> state {trackedEvents = event : trackedEvents state}
      _ -> state
    rememberMessage identifier = identifier : filter (/= identifier) (trackedMessages recorded)
    -- Delivery history cannot be rolled back with authoritative message buffers.
    deliver updated pieces =
      let additions = [(key, text, suffix) | (key, text) <- pieces, Just suffix <- [Text.stripPrefix (Map.findWithDefault "" key (trackedDelivered recorded)) text], not (Text.null suffix)]
          delivered = Map.union (Map.fromList [(key, text) | (key, text, _) <- additions]) (trackedDelivered recorded)
       in (updated {trackedDelivered = delivered}, [suffix | (_, _, suffix) <- additions])

-- | Caller-supplied completion metadata for the pure accumulator. This is not
-- a peer receipt; absent usage stays absent unless a usage event was observed.
-- Duration is supplied explicitly, keeping the pure API independent of clocks.
data DroidStreamSummary = DroidStreamSummary
  { summarySessionId :: !Text,
    summaryReason :: !AgentTurnCompletionReason,
    summaryTokenUsage :: !(Maybe TokenUsage),
    summaryDurationMs :: !Scientific,
    summaryText :: !Text,
    summaryEvents :: ![DroidEvent],
    summaryErrors :: ![ErrorNotification],
    summaryStructuredOutput :: !(Maybe Object)
  }
  deriving stock (Eq)

instance Show DroidStreamSummary where
  show _ = "DroidStreamSummary <redacted>"

summarizeStream :: Text -> AgentTurnCompletionReason -> Maybe TokenUsage -> Scientific -> StreamState -> DroidStreamSummary
summarizeStream identifier reason usage duration state =
  DroidStreamSummary identifier reason (usage <|> trackedUsage state) duration (streamText state) (reverse (trackedEvents state)) (reverse (trackedErrors state)) (snd <$> trackedOutput state)

finishStream :: Text -> AgentTurnCompleted -> StreamState -> DroidResult
finishStream identifier completion state =
  DroidResult identifier (streamText state) completion (reverse (trackedErrors state)) (reverse (trackedEvents state)) (snd <$> trackedOutput state)

streamText :: StreamState -> Text
streamText state =
  let latest = case trackedMessages state of
        owner : _ -> Text.concat [piece | ((message, _), piece) <- Map.toAscList (trackedBlocks state), message == owner]
        [] -> ""
   in if Text.null latest then Text.concat [Map.findWithDefault "" key (trackedBlocks state) | key <- reverse (trackedOrder state)] else latest

decodeNotification :: Text -> Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeNotification identifier expectedTurn = decodeNotificationWithTurn identifier (Just expectedTurn)

decodeSessionNotification :: Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeSessionNotification identifier = decodeNotificationWithTurn identifier Nothing

decodeNotificationWithTurn :: Text -> Maybe Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeNotificationWithTurn = decodeScopedNotification "droid.session_notification" (.:! "sessionId")

decodeDaemonNotification :: Text -> Maybe Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeDaemonNotification = decodeScopedNotification "daemon.session_notification" (\fields -> Just <$> fields .: "sessionId")

decodeScopedNotification :: Text -> (Object -> Parser (Maybe Text)) -> Text -> Maybe Text -> JsonRpcBaseNotification -> Either String [DroidEvent]
decodeScopedNotification method identify identifier expectedTurn notification
  | baseNotificationMethod (envelopeBody notification) /= method = Right []
  | otherwise = case baseNotificationParams (envelopeBody notification) of
      Nothing -> Left "Missing notification parameters"
      Just value -> parseEither (withObject "session notification" parse) value
  where
    parse fields = do
      session <- identify fields
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
        "mcp_status_changed" -> one McpStatusEvent fields
        "mcp_auth_required" -> one McpAuthRequiredEvent fields
        "mcp_auth_completed" -> one McpAuthCompletedEvent fields
        "mission_state_changed" -> one MissionStateEvent fields
        "mission_features_changed" -> one MissionFeaturesEvent fields
        "mission_progress_entry" -> one MissionProgressEvent fields
        "mission_heartbeat" -> one MissionHeartbeatEvent fields
        "mission_worker_started" -> one MissionWorkerStartedEvent fields
        "mission_worker_completed" -> one MissionWorkerCompletedEvent fields
        "permission_resolved" -> one PermissionEvent fields
        "hook_execution_started" -> one HookStartedEvent fields
        "hook_execution_completed" -> one HookCompletedEvent fields
        "structured_output" -> one StructuredOutputEvent fields
        "assistant_message_retracted" -> one MessageRetractedEvent fields
        "session_compacted" -> one SessionCompactedEvent fields
        "queued_messages_discarded" -> one QueuedMessagesDiscardedEvent fields
        "child_session_available" -> one ChildSessionAvailableEvent fields
        "agent_turn_completed" -> case expectedTurn of
          Nothing -> one TurnCompletedEvent fields
          Just expected -> do
            turnId <- fields .: "turnId"
            if turnId /= expected then pure [] else one TurnCompletedEvent fields
        "error" -> one ErrorEvent fields
        _ -> pure [OtherNotificationEvent fields]
