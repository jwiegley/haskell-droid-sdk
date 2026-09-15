{-# LANGUAGE OverloadedStrings #-}

-- | Immutable session state. Messages, submissions and retained resource views
-- share one model; no renderer, clock, RPC or worker is created here.
module Factory.Droid.SessionState
  ( SessionState,
    emptySessionState,
    sessionMessagesById,
    sessionMessages,
    checkedSessionMessages,
    sessionMessageError,
    MessageStateError (..),
    invalidateMessageState,
    applyMessageEventAt,
    applySessionEventAt,
    sessionCallingSessionId,
    sessionCallingToolUseId,
    sessionInvocationSummary,
    sessionChildLoadError,
    ChildLoadError (..),
    observeChildAvailable,
    setCallingMetadata,
    clearChildLink,
    setInvocationSummary,
    refreshInvocationSummary,
    summarizeSessionToolUsage,
    setChildLoadError,
    DeferredUserAction (..),
    sessionDeferredPermissions,
    sessionDeferredQuestions,
    storeDeferredPermission,
    storeDeferredQuestion,
    takeDeferredPermission,
    takeDeferredQuestion,
    clearDeferredUserActions,
    retireDeferredPermissions,
    WorkingDirectoryState (..),
    sessionWorkingDirectory,
    observeWorkingDirectory,
    inheritWorkingDirectory,
    inheritWorkingDirectoryState,
    invalidateWorkingDirectory,
    invalidateWorkingDirectoryState,
    TerminalStatus (..),
    TerminalStateTimestamp (..),
    TerminalSerializedState (..),
    TerminalMetadata (..),
    defaultTerminalMetadata,
    TerminalObservationError (..),
    sessionTerminalError,
    invalidateTerminalState,
    clearTerminalError,
    sessionTerminalsById,
    sessionTerminals,
    sessionActiveTerminalId,
    setActiveTerminalId,
    addSessionTerminal,
    setSessionTerminalStatus,
    observeTerminalExit,
    removeSessionTerminal,
    clearSessionTerminals,
    storeTerminalSerializedState,
    getTerminalSerializedState,
    appendTerminalBufferedData,
    getTerminalBufferedData,
    clearTerminalBufferedData,
    clearTerminalRestorationState,
    TerminalBufferSnapshot,
    terminalBufferSnapshot,
    terminalBufferSnapshotText,
    acknowledgeTerminalBuffer,
    TerminalRestoration,
    beginTerminalRestoration,
    terminalRestorationCurrent,
    restoreSessionTerminals,
    TodoItem (..),
    TodoStatus (..),
    TodoPriority (..),
    parseTodos,
    sessionTodos,
    sessionToolPhases,
    sessionToolProgress,
    allToolProgress,
    clearToolProgress,
    sessionRetry,
    HookObservation (..),
    HookOutcome (..),
    sessionHooks,
    visibleSessionHooks,
    DisplayEntry (..),
    filterDisplayMessages,
    persistedHook,
    sessionDisplayEntries,
    checkedSessionDisplay,
    sessionProgressiveDisplay,
    sessionDisplayLimit,
    sessionDisplayCutoff,
    sessionHiddenMessageCount,
    sessionHydrationFloor,
    setProgressiveDisplay,
    expandSessionDisplay,
    setSessionDisplayCutoff,
    expireSessionStateAt,
    nextSessionDeadline,
    pendingToolCalls,
    orphanToolMessages,
    sessionRecentMessages,
    sessionMessagesByRole,
    upsertSessionMessage,
    removeSessionMessage,
    truncateSessionMessages,
    repairMessageParents,
    orderMessagesByParentChain,
    QueuedMessageKind (..),
    QueueDisplayGroup (..),
    QueueEntry (..),
    QueueOperationError (..),
    sessionQueue,
    queueEntryRequestId,
    lookupQueuedMessage,
    enqueueMessages,
    replaceDaemonQueue,
    dequeueQueuedMessage,
    dequeueQueuedMessages,
    restoreQueueFront,
    clearQueuedMessages,
    removeQueuedMessage,
    markQueuedMessageProcessed,
    pauseDaemonQueue,
    reconcileDaemonQueueAt,
    applyQueueResolution,
    isDaemonQueuedMessage,
    isReviewableQueuedMessage,
    queueDisplayGroup,
    queueReviewPriority,
    queueKindForPlacement,
    optimisticSubmissions,
    lookupSubmission,
    OptimisticSubmission (..),
    SubmissionStatus (..),
    SubmissionFailure (..),
    SubmissionError (..),
    registerSubmissionAt,
    beginSubmissionAt,
    finishSubmission,
    failSubmission,
    rejectSubmission,
    cancelSubmission,
    cancelSessionSubmissions,
    confirmSubmission,
    observeCreatedMessage,
    mergeLoadedMessages,
    expireSubmissionsAt,
    nextSubmissionDeadline,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Data.Aeson (ToJSON (toJSON), Value (..), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (find, findIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Scientific (Scientific, scientific, toBoundedInteger, toRealFloat)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf16BE, encodeUtf8)
import Factory.Droid.Internal.JSON (isEcmaWhitespace)
import Factory.Droid.Internal.Stream (DroidEvent (..))
import Factory.Droid.Schema.Content (BaseContentBlock (..), ContentBlock (..), TextBlock (..), ThinkingBlock (..), ThinkingDuration, ToolResultBlock (..), ToolUseBlock (..), isPendingToolResult, mkThinkingDuration)
import Factory.Droid.Schema.Control (AddUserMessageParams (..), QueuePlacement (..), QueueResolution (..), QueuedUserMessage (..), ResolveQueuedMessageParams (..), UserMessageContent (..))
import Factory.Droid.Schema.Daemon.Terminal qualified as Terminal
import Factory.Droid.Schema.Enums (MessageRole (..), MessageVisibility (..))
import Factory.Droid.Schema.Interaction (AskUserResult, RequestPermissionResult)
import Factory.Droid.Schema.Messages (FactoryDroidMessage, HookStatus (..), Message (..), PersistedHookCommand (..))
import Factory.Droid.Schema.Mission (SubagentInvocationSummary (..), SubagentStatus (..))
import Factory.Droid.Schema.Notifications (AssistantMessageRetracted (..), AssistantTextComplete (..), AssistantTextDelta (..), ChildSessionAvailable (..), CreateMessage (..), DroidWorkingState (..), DroidWorkingStateChanged (..), HookExecutionCompleted (..), HookExecutionStarted (..), LlmRetry, QueuedMessagesDiscarded (..), SessionCompacted (..), SessionWorkingDirectoryChanged (..), ThinkingTextComplete (..), ThinkingTextDelta (..), ToolCallNotification (..), ToolExecutionPhase (..), ToolExecutionPhaseChanged (..), ToolProgressUpdate (..), ToolProgressUpdateNotification (..), ToolResultNotification (..))
import Factory.Droid.Schema.Primitives (Rfc3339Timestamp)
import Numeric (showEFloat)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

data SubmissionFailure = SubmissionRejected | SubmissionTimedOut | SubmissionInvalidConfirmation | SubmissionConnectionFailed deriving stock (Eq, Show)

data SubmissionStatus = SubmissionPending | SubmissionFailed !SubmissionFailure deriving stock (Eq, Show)

data SubmissionError = SubmissionAlreadyInFlight | SubmissionAlreadyConfirmed | SubmissionInvalidTimeout deriving stock (Eq, Show)

instance Exception SubmissionError

data OptimisticSubmission = OptimisticSubmission
  { submissionRequestId :: !Text,
    submissionPlaceholderId :: !Text,
    submissionInput :: !AddUserMessageParams,
    submissionStatus :: !SubmissionStatus,
    submissionInFlight :: !Bool,
    submissionDeadline :: !(Maybe Integer)
  }
  deriving stock (Eq)

instance Show OptimisticSubmission where show _ = "OptimisticSubmission <redacted>"

data MessageStateError = MalformedMessageEvent | MessageRoleConflict deriving stock (Eq, Show)

instance Exception MessageStateError

data StreamBlockKind = TextPart | ThinkingPart deriving stock (Eq, Ord)

type BlockAddress = (Text, StreamBlockKind, Scientific)

data TodoStatus = TodoPending | TodoInProgress | TodoCompleted deriving stock (Eq, Show)

data TodoPriority = TodoHigh | TodoMedium | TodoLow deriving stock (Eq, Show)

data TodoItem = TodoItem
  { todoId :: !Text,
    todoContent :: !Text,
    todoStatus :: !TodoStatus,
    todoPriority :: !TodoPriority
  }
  deriving stock (Eq)

instance Show TodoItem where show _ = "TodoItem <redacted>"

data HookOutcome = HookRunning | HookReported !HookExecutionCompleted | HookLeaseExpired deriving stock (Eq)

instance Show HookOutcome where show _ = "HookOutcome <redacted>"

data HookObservation = HookObservation
  { observedHookStart :: !HookExecutionStarted,
    observedHookWallTime :: !Scientific,
    observedHookMonotonicStart :: !Integer,
    observedHookOutcome :: !HookOutcome
  }
  deriving stock (Eq)

instance Show HookObservation where show _ = "HookObservation <redacted>"

data DisplayEntry = DisplayMessage !FactoryDroidMessage | DisplaySubmission !OptimisticSubmission | DisplayPendingTools ![ToolUseBlock] deriving stock (Eq)

instance Show DisplayEntry where show _ = "DisplayEntry <redacted>"

data QueuedMessageKind = QueueDeferredAfterInterrupt | QueuePaused | QueueDaemonDiscardable | QueueDaemonEndOfLoop | QueueDeferredDuringCompaction deriving stock (Eq, Ord, Show)

-- | Display classification, not the wire placement or a send policy.
data QueueDisplayGroup = QueueSteeringGroup | QueueQueuedGroup deriving stock (Eq, Ord, Show)

data QueueEntry = QueueEntry
  { queueEntryMessage :: !QueuedUserMessage,
    queueEntryKind :: !QueuedMessageKind,
    queueEntryObservedAt :: !Scientific
  }
  deriving stock (Eq)

instance Show QueueEntry where show _ = "QueueEntry <redacted>"

data QueueOperationError = QueuedEntryMissing | QueuedEntryAlreadyRemote deriving stock (Eq, Show)

instance Exception QueueOperationError

data ChildLoadError = ChildLoadNotFound | ChildLoadInterrupted | ChildLoadFailed deriving stock (Eq, Show)

-- | An explicit caller decision retained for later revalidation. Storage is
-- session-local; it does not resume a session, send a response or replay a grant.
data DeferredUserAction response = DeferredUserAction
  { deferredActionRequestId :: !Text,
    deferredActionToolId :: !Text,
    deferredActionResponse :: !response,
    deferredActionStoredAt :: !Scientific,
    deferredActionAssociatedSessionIds :: ![Text]
  }
  deriving stock (Eq)

instance Show (DeferredUserAction response) where show _ = "DeferredUserAction <redacted>"

storeDeferredPermission :: DeferredUserAction RequestPermissionResult -> SessionState -> SessionState
storeDeferredPermission action state = state {sessionDeferredPermissions = Map.insert (deferredActionToolId action) action (sessionDeferredPermissions state)}

storeDeferredQuestion :: DeferredUserAction AskUserResult -> SessionState -> SessionState
storeDeferredQuestion action state = state {sessionDeferredQuestions = Map.insert (deferredActionToolId action) action (sessionDeferredQuestions state)}

takeDeferredPermission :: Text -> SessionState -> (Maybe (DeferredUserAction RequestPermissionResult), SessionState)
takeDeferredPermission identifier state = (Map.lookup identifier (sessionDeferredPermissions state), state {sessionDeferredPermissions = Map.delete identifier (sessionDeferredPermissions state)})

takeDeferredQuestion :: Text -> SessionState -> (Maybe (DeferredUserAction AskUserResult), SessionState)
takeDeferredQuestion identifier state = (Map.lookup identifier (sessionDeferredQuestions state), state {sessionDeferredQuestions = Map.delete identifier (sessionDeferredQuestions state)})

clearDeferredUserActions :: SessionState -> (Int, SessionState)
clearDeferredUserActions state = (Map.size (sessionDeferredPermissions state) + Map.size (sessionDeferredQuestions state), state {sessionDeferredPermissions = mempty, sessionDeferredQuestions = mempty})

retireDeferredPermissions :: Text -> Text -> [Text] -> SessionState -> SessionState
retireDeferredPermissions surface requestId tools state = state {sessionDeferredPermissions = Map.filter keep (sessionDeferredPermissions state)}
  where
    keep action = not (surface `elem` deferredActionAssociatedSessionIds action && (deferredActionRequestId action == requestId || deferredActionToolId action `elem` tools))

data WorkingDirectoryState
  = WorkingDirectoryUnknown
  | WorkingDirectoryInherited !Text
  | WorkingDirectoryReported !(Maybe Text)
  | WorkingDirectoryInvalid !(Maybe Text)
  deriving stock (Eq)

instance Show WorkingDirectoryState where show _ = "WorkingDirectoryState <redacted>"

observeWorkingDirectory :: Maybe Text -> SessionState -> SessionState
observeWorkingDirectory path state = state {sessionWorkingDirectory = WorkingDirectoryReported path}

-- Provisional inheritance cannot replace an explicit empty/absent observation
-- or conceal an invalid one. It is never a permission or trust decision.
inheritWorkingDirectory :: WorkingDirectoryState -> SessionState -> SessionState
inheritWorkingDirectory parent state = state {sessionWorkingDirectory = inheritWorkingDirectoryState parent (sessionWorkingDirectory state)}

inheritWorkingDirectoryState :: WorkingDirectoryState -> WorkingDirectoryState -> WorkingDirectoryState
inheritWorkingDirectoryState parent state = case (parent, state) of
  (WorkingDirectoryInherited path, WorkingDirectoryUnknown) -> seed path
  (WorkingDirectoryReported (Just path), WorkingDirectoryUnknown) -> seed path
  _ -> state
  where
    seed path = if Text.null path then state else WorkingDirectoryInherited path

invalidateWorkingDirectory :: SessionState -> SessionState
invalidateWorkingDirectory state = state {sessionWorkingDirectory = invalidateWorkingDirectoryState (sessionWorkingDirectory state)}

invalidateWorkingDirectoryState :: WorkingDirectoryState -> WorkingDirectoryState
invalidateWorkingDirectoryState state =
  let previous = case state of WorkingDirectoryUnknown -> Nothing; WorkingDirectoryInherited path -> Just path; WorkingDirectoryReported path -> path; WorkingDirectoryInvalid path -> path
   in WorkingDirectoryInvalid previous

data TerminalStatus = TerminalConnecting | TerminalConnected | TerminalDisconnected | TerminalError deriving stock (Eq, Show)

-- | Retain a reported wire timestamp without date coercion; locally supplied
-- snapshots can instead carry exact epoch milliseconds.
data TerminalStateTimestamp = TerminalWireTimestamp !Rfc3339Timestamp | TerminalEpochMilliseconds !Scientific deriving stock (Eq, Show)

data TerminalSerializedState = TerminalSerializedState
  { terminalSerializedText :: !Text,
    terminalSerializedCols :: !Scientific,
    terminalSerializedRows :: !Scientific,
    terminalSerializedTimestamp :: !TerminalStateTimestamp,
    terminalSerializedCursorHidden :: !(Maybe Bool)
  }
  deriving stock (Eq)

instance Show TerminalSerializedState where show _ = "TerminalSerializedState <redacted>"

data TerminalMetadata = TerminalMetadata
  { terminalMetadataId :: !Text,
    terminalMetadataStatus :: !TerminalStatus,
    terminalMetadataExitCode :: !(Maybe Scientific),
    terminalMetadataSignal :: !(Maybe Text),
    terminalMetadataSerializedState :: !(Maybe TerminalSerializedState),
    terminalMetadataBufferedData :: !(Maybe Text),
    terminalMetadataRowsWhenSerialized :: !(Maybe Scientific),
    terminalMetadataColsWhenSerialized :: !(Maybe Scientific)
  }
  deriving stock (Eq)

instance Show TerminalMetadata where show _ = "TerminalMetadata <redacted>"

defaultTerminalMetadata :: Text -> TerminalStatus -> TerminalMetadata
defaultTerminalMetadata identifier status = TerminalMetadata identifier status Nothing Nothing Nothing Nothing Nothing Nothing

data TerminalObservationError = MalformedTerminalEvent | TerminalWriterFailed !Text deriving stock (Eq)

instance Show TerminalObservationError where show _ = "TerminalObservationError <redacted>"

data TerminalEntry = TerminalEntry
  { retainedTerminal :: !TerminalMetadata,
    terminalEntryVersion :: !Integer,
    terminalBufferVersion :: !Integer
  }
  deriving stock (Eq)

data TerminalBufferSnapshot = TerminalBufferSnapshot !Integer !Integer !(Maybe Text) deriving stock (Eq)

instance Show TerminalBufferSnapshot where show _ = "TerminalBufferSnapshot <redacted>"

data SessionState = SessionState
  { messages :: !(Map Text FactoryDroidMessage),
    messageOrder :: !(Seq Text),
    sessionMessageError :: !(Maybe MessageStateError),
    sessionCallingSessionId :: !(Maybe Text),
    sessionCallingToolUseId :: !(Maybe Text),
    sessionInvocationSummary :: !(Maybe SubagentInvocationSummary),
    sessionChildLoadError :: !(Maybe ChildLoadError),
    sessionDeferredPermissions :: !(Map Text (DeferredUserAction RequestPermissionResult)),
    sessionDeferredQuestions :: !(Map Text (DeferredUserAction AskUserResult)),
    sessionWorkingDirectory :: !WorkingDirectoryState,
    terminals :: !(Map Text TerminalEntry),
    terminalOrder :: !(Seq Text),
    nextTerminalVersion :: !Integer,
    terminalRestorationGeneration :: !Integer,
    sessionActiveTerminalId :: !(Maybe Text),
    sessionTerminalError :: !(Maybe TerminalObservationError),
    blockPositions :: !(Map BlockAddress Int),
    thinkingStarts :: !(Map BlockAddress (Integer, Scientific)),
    pendingToolCalls :: ![ToolUseBlock],
    orphanToolMessages :: !(Map Text FactoryDroidMessage),
    todoToolUses :: !(Map Text (Integer, ToolUseBlock)),
    nextTodoSequence :: !Integer,
    selectedTodos :: !(Maybe (Integer, [TodoItem])),
    sessionToolPhases :: !(Map Text ToolExecutionPhase),
    sessionToolProgress :: !(Map Text [ToolProgressUpdate]),
    toolProgressOrder :: !(Seq Text),
    toolProgressDeadlines :: !(Map Text Integer),
    sessionRetry :: !(Maybe LlmRetry),
    hookObservations :: !(Map Text HookObservation),
    hookOrder :: !(Seq Text),
    sessionProgressiveDisplay :: !Bool,
    sessionDisplayLimit :: !(Maybe Int),
    displayInitialized :: !Bool,
    sessionDisplayCutoff :: !(Maybe Text),
    sessionHiddenMessageCount :: !Natural,
    sessionHydrationFloor :: !(Maybe Text),
    submissions :: !(Map Text OptimisticSubmission),
    submissionOrder :: !(Seq Text),
    processedRequests :: !(Set Text),
    inFlightRequests :: !(Set Text),
    queuedMessages :: !(Map Text QueueEntry),
    queuedOrder :: !(Seq Text)
  }
  deriving stock (Eq)

instance Show SessionState where show _ = "SessionState <redacted>"

emptySessionState :: SessionState
emptySessionState =
  SessionState
    { messages = mempty,
      messageOrder = mempty,
      sessionMessageError = Nothing,
      sessionCallingSessionId = Nothing,
      sessionCallingToolUseId = Nothing,
      sessionInvocationSummary = Nothing,
      sessionChildLoadError = Nothing,
      sessionDeferredPermissions = mempty,
      sessionDeferredQuestions = mempty,
      sessionWorkingDirectory = WorkingDirectoryUnknown,
      terminals = mempty,
      terminalOrder = mempty,
      nextTerminalVersion = 0,
      terminalRestorationGeneration = 0,
      sessionActiveTerminalId = Nothing,
      sessionTerminalError = Nothing,
      blockPositions = mempty,
      thinkingStarts = mempty,
      pendingToolCalls = [],
      todoToolUses = mempty,
      nextTodoSequence = 0,
      selectedTodos = Nothing,
      sessionToolPhases = mempty,
      sessionToolProgress = mempty,
      toolProgressOrder = mempty,
      toolProgressDeadlines = mempty,
      sessionRetry = Nothing,
      hookObservations = mempty,
      hookOrder = mempty,
      sessionProgressiveDisplay = True,
      sessionDisplayLimit = Nothing,
      displayInitialized = False,
      sessionDisplayCutoff = Nothing,
      sessionHiddenMessageCount = 0,
      sessionHydrationFloor = Nothing,
      orphanToolMessages = mempty,
      submissions = mempty,
      submissionOrder = mempty,
      processedRequests = mempty,
      inFlightRequests = mempty,
      queuedMessages = mempty,
      queuedOrder = mempty
    }

sessionTerminalsById :: SessionState -> Map Text TerminalMetadata
sessionTerminalsById = Map.map retainedTerminal . terminals

sessionTerminals :: SessionState -> [TerminalMetadata]
sessionTerminals state = mapMaybe (fmap retainedTerminal . (`Map.lookup` terminals state)) (toList (terminalOrder state))

setActiveTerminalId :: Maybe Text -> SessionState -> SessionState
setActiveTerminalId identifier state = state {sessionActiveTerminalId = identifier}

addSessionTerminal :: TerminalMetadata -> SessionState -> SessionState
addSessionTerminal terminal state =
  let identifier = terminalMetadataId terminal
      version = nextTerminalVersion state + 1
   in state
        { terminals = Map.insert identifier (TerminalEntry terminal version version) (terminals state),
          terminalOrder = if Map.member identifier (terminals state) then terminalOrder state else terminalOrder state |> identifier,
          nextTerminalVersion = version
        }

updateTerminalMetadata :: Text -> (TerminalMetadata -> TerminalMetadata) -> SessionState -> SessionState
updateTerminalMetadata identifier change state = case Map.lookup identifier (terminals state) of
  Nothing -> state
  Just entry ->
    let version = nextTerminalVersion state + 1
        next = entry {retainedTerminal = change (retainedTerminal entry), terminalEntryVersion = version}
     in state {terminals = Map.insert identifier next (terminals state), nextTerminalVersion = version}

setSessionTerminalStatus :: Text -> TerminalStatus -> SessionState -> SessionState
setSessionTerminalStatus identifier status = updateTerminalMetadata identifier (\terminal -> terminal {terminalMetadataStatus = status})

observeTerminalExit :: Terminal.TerminalExit -> SessionState -> SessionState
observeTerminalExit event = updateTerminalMetadata (Terminal.terminalExitId event) (\terminal -> terminal {terminalMetadataStatus = TerminalDisconnected, terminalMetadataExitCode = Just (Terminal.terminalExitCode event), terminalMetadataSignal = Just (Terminal.terminalExitSignal event)})

removeSessionTerminal :: Text -> SessionState -> SessionState
removeSessionTerminal identifier state = state {terminals = Map.delete identifier (terminals state), terminalOrder = Seq.filter (/= identifier) (terminalOrder state)}

clearSessionTerminals :: SessionState -> SessionState
clearSessionTerminals state = state {terminals = mempty, terminalOrder = mempty, sessionActiveTerminalId = Nothing, sessionTerminalError = Nothing, terminalRestorationGeneration = terminalRestorationGeneration state + 1}

resetTerminalBuffer :: Text -> (TerminalMetadata -> TerminalMetadata) -> SessionState -> SessionState
resetTerminalBuffer identifier change state = case Map.lookup identifier (terminals state) of
  Nothing -> state
  Just entry ->
    let version = nextTerminalVersion state + 1
        terminal = (change (retainedTerminal entry)) {terminalMetadataBufferedData = Nothing}
     in state {terminals = Map.insert identifier (TerminalEntry terminal version version) (terminals state), nextTerminalVersion = version}

storeTerminalSerializedState :: Text -> TerminalSerializedState -> SessionState -> SessionState
storeTerminalSerializedState identifier snapshot = resetTerminalBuffer identifier (\terminal -> terminal {terminalMetadataSerializedState = Just snapshot})

getTerminalSerializedState :: Text -> SessionState -> Maybe TerminalSerializedState
getTerminalSerializedState identifier state = Map.lookup identifier (terminals state) >>= terminalMetadataSerializedState . retainedTerminal

appendTerminalBufferedData :: Text -> Text -> SessionState -> SessionState
appendTerminalBufferedData identifier value state = state {terminals = Map.adjust append identifier (terminals state)}
  where
    append entry = let terminal = retainedTerminal entry in entry {retainedTerminal = terminal {terminalMetadataBufferedData = Just (fromMaybe "" (terminalMetadataBufferedData terminal) <> value)}}

getTerminalBufferedData :: Text -> SessionState -> Maybe Text
getTerminalBufferedData identifier state = Map.lookup identifier (terminals state) >>= terminalMetadataBufferedData . retainedTerminal

clearTerminalBufferedData :: Text -> SessionState -> SessionState
clearTerminalBufferedData identifier = resetTerminalBuffer identifier id

clearTerminalRestorationState :: Text -> SessionState -> SessionState
clearTerminalRestorationState identifier = resetTerminalBuffer identifier (\terminal -> terminal {terminalMetadataSerializedState = Nothing})

terminalBufferSnapshot :: Text -> SessionState -> Maybe TerminalBufferSnapshot
terminalBufferSnapshot identifier state = do
  entry <- Map.lookup identifier (terminals state)
  pure (TerminalBufferSnapshot (terminalEntryVersion entry) (terminalBufferVersion entry) (terminalMetadataBufferedData (retainedTerminal entry)))

terminalBufferSnapshotText :: TerminalBufferSnapshot -> Maybe Text
terminalBufferSnapshotText (TerminalBufferSnapshot _ _ value) = value

-- | A writer acknowledges only its captured prefix. Later appended output
-- remains; a reset, replacement or previous acknowledgement revokes the claim.
acknowledgeTerminalBuffer :: Text -> TerminalBufferSnapshot -> SessionState -> SessionState
acknowledgeTerminalBuffer identifier (TerminalBufferSnapshot _ expected buffered) state = case (Map.lookup identifier (terminals state), buffered) of
  (Just entry, Just prefix)
    | terminalBufferVersion entry == expected ->
        let terminal = retainedTerminal entry
            remaining = Text.drop (Text.length prefix) (fromMaybe "" (terminalMetadataBufferedData terminal))
            version = nextTerminalVersion state + 1
            next = entry {retainedTerminal = terminal {terminalMetadataBufferedData = if Text.null remaining then Nothing else Just remaining}, terminalBufferVersion = version}
         in state {terminals = Map.insert identifier next (terminals state), nextTerminalVersion = version}
  _ -> state

invalidateTerminalState :: TerminalObservationError -> SessionState -> SessionState
invalidateTerminalState cause state = state {sessionTerminalError = Just cause}

clearTerminalError :: SessionState -> SessionState
clearTerminalError state = state {sessionTerminalError = Nothing}

data TerminalRestoration = TerminalRestoration !Integer !(Map Text TerminalBufferSnapshot) deriving stock (Eq)

instance Show TerminalRestoration where show _ = "TerminalRestoration <redacted>"

beginTerminalRestoration :: SessionState -> (TerminalRestoration, SessionState)
beginTerminalRestoration state =
  let generation = terminalRestorationGeneration state + 1
      snapshots = Map.map (\entry -> TerminalBufferSnapshot (terminalEntryVersion entry) (terminalBufferVersion entry) (terminalMetadataBufferedData (retainedTerminal entry))) (terminals state)
   in (TerminalRestoration generation snapshots, state {terminalRestorationGeneration = generation})

terminalRestorationCurrent :: TerminalRestoration -> SessionState -> Bool
terminalRestorationCurrent (TerminalRestoration generation _) state = generation == terminalRestorationGeneration state

-- | A newer explicit restoration or local metadata change wins. Output that
-- arrived after the request began is not discarded by an older screen snapshot.
restoreSessionTerminals :: TerminalRestoration -> [Terminal.TerminalInfo] -> SessionState -> SessionState
restoreSessionTerminals ticket@(TerminalRestoration _ captured) reports state
  | not (terminalRestorationCurrent ticket state) = state
  | otherwise =
      let restored = foldl' restore state reports
       in if sessionTerminalError restored == Just MalformedTerminalEvent then clearTerminalError restored else restored
  where
    restore current info =
      let identifier = Terminal.terminalInfoId info
          previous = Map.lookup identifier (terminals state)
          saved = Map.lookup identifier captured
          accepted = case (saved, previous) of
            (Nothing, Nothing) -> True
            (Just (TerminalBufferSnapshot expected _ _), Just entry) -> terminalEntryVersion entry == expected
            _ -> False
          consumed = if isJust (Terminal.terminalInfoState info) then maybe current (\buffer -> acknowledgeTerminalBuffer identifier buffer current) saved else current
          remaining = getTerminalBufferedData identifier consumed
          snapshot screen = TerminalSerializedState (Terminal.screenSerialized screen) (Terminal.screenCols screen) (Terminal.screenRows screen) (TerminalWireTimestamp (Terminal.screenTimestamp screen)) (Terminal.screenCursorHidden screen)
          existing = maybe (defaultTerminalMetadata identifier TerminalConnected) retainedTerminal (Map.lookup identifier (terminals current))
          terminal = existing {terminalMetadataStatus = TerminalConnected, terminalMetadataExitCode = Nothing, terminalMetadataSignal = Nothing, terminalMetadataSerializedState = (snapshot <$> Terminal.terminalInfoState info) <|> terminalMetadataSerializedState existing, terminalMetadataBufferedData = remaining}
       in if accepted then addSessionTerminal terminal consumed else current

-- | Availability is provisional linkage, not a permission or execution grant.
-- An already established parent is not changed by a stale availability notice.
observeChildAvailable :: Text -> ChildSessionAvailable -> SessionState -> SessionState
observeChildAvailable parent event state
  | parent == availableChildSessionId event = state
  | Just existing <- nonemptyLink (sessionCallingSessionId state), existing /= parent = state
  | otherwise = (setCallingMetadata (Just parent) (nonemptyLink (sessionCallingToolUseId state) <|> availableChildToolUseId event) state) {sessionInvocationSummary = summary}
  where
    summary = case sessionInvocationSummary state of
      Nothing -> do
        kind <- availableChildSubagentType event
        description <- availableChildDescription event
        pure (SubagentInvocationSummary (availableChildSessionId event) SubagentRunning kind description Nothing Nothing mempty)
      Just previous | invocationStatus previous `elem` [SubagentCompleted, SubagentFailed, SubagentCancelled] -> Just (previous {invocationStatus = SubagentRunning})
      previous -> previous

-- | Nonempty reported linkage overrides provisional linkage. Omission and empty
-- strings do not erase it; the immutable load receipt retains those raw values.
setCallingMetadata :: Maybe Text -> Maybe Text -> SessionState -> SessionState
setCallingMetadata parent tool state = state {sessionCallingSessionId = nonemptyLink parent <|> sessionCallingSessionId state, sessionCallingToolUseId = nonemptyLink tool <|> sessionCallingToolUseId state}

nonemptyLink :: Maybe Text -> Maybe Text
nonemptyLink = (>>= \value -> if Text.null value then Nothing else Just value)

clearChildLink :: SessionState -> SessionState
clearChildLink state = state {sessionCallingSessionId = Nothing, sessionCallingToolUseId = Nothing, sessionChildLoadError = Nothing}

setInvocationSummary :: SubagentInvocationSummary -> SessionState -> SessionState
setInvocationSummary summary state = state {sessionInvocationSummary = Just summary}

setChildLoadError :: Maybe ChildLoadError -> SessionState -> SessionState
setChildLoadError failure state = state {sessionChildLoadError = if isJust (sessionCallingSessionId state) then failure else sessionChildLoadError state}

-- | Tool-use count and optional positive duration in supplied message order.
-- Updated timestamps win when nonzero; zero timestamps are not observations.
summarizeSessionToolUsage :: [FactoryDroidMessage] -> (Scientific, Maybe Scientific)
summarizeSessionToolUsage history = (toolCount, duration)
  where
    toolCount = fromIntegral (length [() | message <- history, messageRole message == RoleAssistant, ContentToolUse _ <- messageContent message])
    timestamps = filter (/= 0) [if messageUpdatedAt message /= 0 then messageUpdatedAt message else messageCreatedAt message | message <- history]
    duration = case timestamps of first : _ : _ -> let elapsed = last timestamps - first in if elapsed > 0 then Just elapsed else Nothing; _ -> Nothing

-- | Refresh observed tool count/duration without manufacturing a terminal task
-- status from a turn boundary. The caller selects known subagent sessions.
refreshInvocationSummary :: SessionState -> SessionState
refreshInvocationSummary state
  | isJust (sessionMessageError state) || null history = state
  | otherwise = state {sessionInvocationSummary = update <$> sessionInvocationSummary state}
  where
    history = sessionMessages state
    (toolCount, duration) = summarizeSessionToolUsage history
    update summary = summary {invocationToolUseCount = Just toolCount, invocationDurationMs = duration <|> invocationDurationMs summary}

sessionMessagesById :: SessionState -> Map Text FactoryDroidMessage
sessionMessagesById = messages

sessionMessages :: SessionState -> [FactoryDroidMessage]
sessionMessages state = mapMaybe (`Map.lookup` messages state) (toList (messageOrder state))

-- | Raw selectors retain last-known data; this view reports a failed observation
-- until a validated load establishes a fresh baseline.
checkedSessionMessages :: SessionState -> Either MessageStateError [FactoryDroidMessage]
checkedSessionMessages state = maybe (Right (sessionMessages state)) Left (sessionMessageError state)

invalidateMessageState :: MessageStateError -> SessionState -> SessionState
invalidateMessageState cause state = state {sessionMessageError = Just cause}

sessionRecentMessages :: Int -> SessionState -> [FactoryDroidMessage]
sessionRecentMessages count state = drop (max 0 (Seq.length (messageOrder state) - max 0 count)) (sessionMessages state)

sessionMessagesByRole :: MessageRole -> SessionState -> [FactoryDroidMessage]
sessionMessagesByRole role = filter ((== role) . messageRole) . sessionMessages

-- | Authoritative content replaces streamed content, retaining missing embedded
-- tool results. An absent parent inherits the prior parent or current tail.
upsertSessionMessage :: FactoryDroidMessage -> SessionState -> SessionState
upsertSessionMessage incoming state = adoptToolResults message (applyMessageTodos message (clearMessageTracking identifier (state {messages = Map.insert identifier message (messages state), messageOrder = order})))
  where
    identifier = messageId incoming
    previous = Map.lookup identifier (messages state)
    incomingParent = messageParentId incoming >>= \parent -> if parent == identifier then Nothing else Just parent
    inheritedParent = maybe (messageId <$> listToMaybe (reverse (sessionMessages state))) messageParentId previous
    retainedResults = [ContentToolResult result | Just old <- [previous], messageRole old == RoleAssistant, messageRole incoming == RoleAssistant, ContentToolResult result <- messageContent old, not (any (matchingResult (toolResultToolUseId result)) (messageContent incoming))]
    message = incoming {messageParentId = incomingParent <|> inheritedParent, messageContent = messageContent incoming <> retainedResults}
    current = sessionMessages state
    order
      | Map.member identifier (messages state) = messageOrder state
      | Just parent <- messageParentId message,
        (prefix, anchor : following) <- break ((== parent) . messageId) current =
          let (descendants, rest) = spanDescendants (Set.singleton parent) following
           in Seq.fromList (map messageId (prefix <> [anchor] <> descendants <> [message] <> rest))
      | otherwise = Seq.fromList (map messageId before <> [identifier] <> map messageId after)
      where
        (before, after) = span (\existing -> (messageCreatedAt existing, encodeUtf16BE (messageId existing)) <= (messageCreatedAt message, encodeUtf16BE identifier)) current
    spanDescendants known (next : rest)
      | maybe False (`Set.member` known) (messageParentId next) =
          let (children, remaining) = spanDescendants (Set.insert (messageId next) known) rest in (next : children, remaining)
    spanDescendants _ remaining = ([], remaining)

removeSessionMessage :: Text -> SessionState -> SessionState
removeSessionMessage identifier state = clearMessageTracking identifier (state {messages = Map.delete identifier (messages state), messageOrder = Seq.filter (/= identifier) (messageOrder state), orphanToolMessages = Map.filter ((/= identifier) . messageId) (orphanToolMessages state)})

-- | Retain the newest entries in the current message order. Counts are
-- nonnegative by construction; zero clears history, not pending submissions.
truncateSessionMessages :: Natural -> SessionState -> SessionState
truncateSessionMessages count state = retainMessages retained state
  where
    retained = Seq.drop (Seq.length (messageOrder state) - fromIntegral (min count (fromIntegral (Seq.length (messageOrder state))))) (messageOrder state)

retainMessages :: Seq Text -> SessionState -> SessionState
retainMessages retained state = state {messages = Map.restrictKeys (messages state) retainedIds, messageOrder = retained, blockPositions = Map.filterWithKey (\(owner, _, _) _ -> Set.member owner retainedIds) (blockPositions state), thinkingStarts = Map.filterWithKey (\(owner, _, _) _ -> Set.member owner retainedIds) (thinkingStarts state)}
  where
    retainedIds = Set.fromList (toList retained)

optimisticSubmissions :: SessionState -> [OptimisticSubmission]
optimisticSubmissions state = mapMaybe (`Map.lookup` submissions state) (toList (submissionOrder state))

lookupSubmission :: Text -> SessionState -> Maybe OptimisticSubmission
lookupSubmission requestId = Map.lookup requestId . submissions

-- | Times are caller-supplied monotonic microseconds, never peer wall clocks.
registerSubmissionAt :: Integer -> Maybe Int -> Text -> Text -> AddUserMessageParams -> SessionState -> Either SubmissionError SessionState
registerSubmissionAt now timeout requestId placeholder input state
  | maybe False (< 0) timeout = Left SubmissionInvalidTimeout
  | Set.member requestId (processedRequests state) = Left SubmissionAlreadyConfirmed
  | otherwise = case Map.lookup requestId (submissions state) of
      Just previous
        | submissionPlaceholderId previous == placeholder ->
            Right (state {submissions = Map.insert requestId (previous {submissionStatus = SubmissionPending, submissionDeadline = deadline}) (submissions state)})
        | Set.member requestId (inFlightRequests state) -> Left SubmissionAlreadyInFlight
      _ | Set.member requestId (inFlightRequests state) -> Left SubmissionAlreadyInFlight
      _ -> Right (state {submissions = Map.insert requestId pending (submissions state), submissionOrder = Seq.filter (/= requestId) (submissionOrder state) |> requestId})
  where
    deadline = (now +) . toInteger <$> timeout
    pending = OptimisticSubmission requestId placeholder input SubmissionPending False deadline

-- | The actual wire input can add a persisted message ID to a prepared overlay.
-- Once started, another send with this request ID cannot replace its ownership.
beginSubmissionAt :: Integer -> Maybe Int -> Text -> Text -> AddUserMessageParams -> SessionState -> Either SubmissionError SessionState
beginSubmissionAt now timeout requestId placeholder input state
  | maybe False (< 0) timeout = Left SubmissionInvalidTimeout
  | Set.member requestId (inFlightRequests state) = Left SubmissionAlreadyInFlight
  | otherwise = do
      registered <- case Map.lookup requestId (submissions state) of
        Just _ -> Right state
        Nothing -> registerSubmissionAt now timeout requestId placeholder input state
      pure (registered {submissions = Map.adjust (\entry -> entry {submissionInput = input, submissionInFlight = True}) requestId (submissions registered), inFlightRequests = Set.insert requestId (inFlightRequests registered)})

finishSubmission :: Text -> SessionState -> SessionState
finishSubmission requestId state = state {submissions = Map.adjust (\entry -> entry {submissionInFlight = False}) requestId (submissions state), inFlightRequests = Set.delete requestId (inFlightRequests state)}

-- | Mark an overlay failed without ending an RPC that is still on the wire.
failSubmission :: Text -> SubmissionFailure -> SessionState -> SessionState
failSubmission requestId failure state = state {submissions = Map.adjust (\entry -> entry {submissionStatus = SubmissionFailed failure, submissionDeadline = Nothing}) requestId (submissions state)}

rejectSubmission :: Text -> SubmissionFailure -> SessionState -> SessionState
rejectSubmission requestId failure = failSubmission requestId failure . finishSubmission requestId

cancelSubmission :: Text -> SessionState -> SessionState
cancelSubmission requestId state = state {submissions = Map.delete requestId (submissions state), submissionOrder = Seq.filter (/= requestId) (submissionOrder state)}

cancelSessionSubmissions :: SessionState -> SessionState
cancelSessionSubmissions state = state {submissions = mempty, submissionOrder = mempty}

-- | A valid echo is authoritative even if it arrives after timeout or cancel.
-- Unknown confirmations are remembered so late preparation cannot add a duplicate.
confirmSubmission :: Text -> SessionState -> SessionState
confirmSubmission requestId state = (removeQueuedMessage requestId (cancelSubmission requestId state)) {processedRequests = Set.insert requestId (processedRequests state)}

observeCreatedMessage :: CreateMessage -> SessionState -> SessionState
observeCreatedMessage event state =
  let updated = upsertSessionMessage (createdMessage event) state
   in maybe updated (`confirmSubmission` updated) (createdRequestId event)

-- | Recover a lost echo by the explicit persisted message ID, not equal text
-- or client/daemon clocks. Previously observed IDs cannot confirm a resubmit.
mergeLoadedMessages :: [FactoryDroidMessage] -> SessionState -> SessionState
mergeLoadedMessages loaded state =
  let fresh = Set.fromList (map messageId loaded) `Set.difference` Map.keysSet (messages state)
      confirmed = [submissionRequestId entry | entry <- optimisticSubmissions state, Just identifier <- [userMessageId (submissionInput entry)], Set.member identifier fresh]
      merged = foldl' (\known message -> Map.insert (messageId message) message known) (messages state) (repairMessageParents loaded)
      identifiers = nubOrd (toList (messageOrder state) <> map messageId loaded)
      repaired = repairMessageParents (mapMaybe (`Map.lookup` merged) identifiers)
      ordered = if singleRooted repaired then orderMessagesByParentChain repaired else sortOn messageCreatedAt repaired
      updated = state {messages = Map.fromList [(messageId message, message) | message <- ordered], messageOrder = Seq.fromList (map messageId ordered), sessionMessageError = Nothing, blockPositions = mempty, thinkingStarts = mempty, sessionToolPhases = mempty, sessionToolProgress = mempty, toolProgressOrder = mempty, toolProgressDeadlines = mempty, sessionRetry = Nothing}
      adopted = foldl' (flip adoptToolResults) updated (reverse ordered)
      rebuilt = rebuildTodos (foldl' (flip confirmSubmission) adopted confirmed)
      limited = rebuilt {sessionDisplayLimit = if displayInitialized state then sessionDisplayLimit state else initialDisplayLimit rebuilt, displayInitialized = True, sessionHiddenMessageCount = 0, sessionHydrationFloor = hydrationFloor ordered}
   in setSessionDisplayCutoff (sessionDisplayCutoff state) limited

-- | Apply already-decoded message events. Wall time supplies provisional message
-- timestamps; monotonic microseconds supply elapsed thinking time only.
applyMessageEventAt :: Scientific -> Integer -> DroidEvent -> SessionState -> SessionState
applyMessageEventAt wall monotonic event state = case event of
  MessageEvent created -> observeCreatedMessage created state
  TextDeltaEvent delta -> appendBlock wall monotonic TextPart (assistantDeltaMessageId delta) (assistantDeltaBlockIndex delta) (assistantDeltaText delta) state
  ThinkingDeltaEvent delta -> appendBlock wall monotonic ThinkingPart (thinkingDeltaMessageId delta) (thinkingDeltaBlockIndex delta) (thinkingDeltaText delta) state
  TextCompleteEvent complete -> completeBlock wall monotonic TextPart (assistantCompleteMessageId complete) (assistantCompleteBlockIndex complete) Nothing state
  ThinkingCompleteEvent complete -> completeBlock wall monotonic ThinkingPart (thinkingCompleteMessageId complete) (thinkingCompleteBlockIndex complete) (thinkingCompleteDuration complete) state
  ToolCallDeltaEvent call -> observeToolCall wall (calledToolUse call) state
  ToolResultEvent result -> observeToolResult wall result state
  MessageRetractedEvent retracted -> removeSessionMessage (retractedMessageId retracted) state
  _ -> state

clearMessageTracking :: Text -> SessionState -> SessionState
clearMessageTracking identifier state = state {blockPositions = Map.filterWithKey (\(owner, _, _) _ -> owner /= identifier) (blockPositions state), thinkingStarts = Map.filterWithKey (\(owner, _, _) _ -> owner /= identifier) (thinkingStarts state)}

blockIs :: StreamBlockKind -> ContentBlock -> Bool
blockIs TextPart (ContentText _) = True
blockIs ThinkingPart (ContentThinking _) = True
blockIs _ _ = False

findBlockPosition :: BlockAddress -> [ContentBlock] -> SessionState -> Maybe Int
findBlockPosition address@(identifier, kind, index) content state =
  (Map.lookup address (blockPositions state) >>= valid) <|> (toBoundedInteger index >>= candidate)
  where
    valid position = position <$ listToMaybe [() | position >= 0, block <- take 1 (drop position content), blockIs kind block]
    available position = not (any (\(other@(owner, part, _), occupied) -> other /= address && owner == identifier && part == kind && occupied == position) (Map.toList (blockPositions state)))
    candidate position = (valid position >>= unclaimed) <|> (listToMaybe (drop (max 0 position) [i | (i, block) <- zip [0 ..] content, blockIs kind block]) >>= \i -> if position < 0 then Nothing else unclaimed i)
    unclaimed position = if available position then Just position else Nothing

setStreaming :: Bool -> BaseContentBlock -> BaseContentBlock
setStreaming streaming base = base {blockAdditionalFields = KeyMap.insert "isStreaming" (Bool streaming) (blockAdditionalFields base)}

appendBlock :: Scientific -> Integer -> StreamBlockKind -> Text -> Scientific -> Text -> SessionState -> SessionState
appendBlock wall monotonic kind identifier index delta original
  | Just existingMessage <- Map.lookup identifier (messages original), messageRole existingMessage /= RoleAssistant = invalidateMessageState MessageRoleConflict original
  | otherwise = updated {blockPositions = Map.insert address position (blockPositions updated), thinkingStarts = starts}
  where
    address = (identifier, kind, index)
    state = if Map.member identifier (messages original) then original else upsertSessionMessage (emptyObservedMessage wall identifier RoleAssistant) original
    message = messages state Map.! identifier
    content = messageContent message
    existing = findBlockPosition address content state
    position = fromMaybe (length content) existing
    start = Map.findWithDefault (monotonic, wall) address (thinkingStarts state)
    starts = if kind == ThinkingPart then Map.insert address start (thinkingStarts state) else thinkingStarts state
    base = BaseContentBlock Nothing mempty
    replacement = case (kind, existing >>= \i -> listToMaybe (drop i content)) of
      (TextPart, Just (ContentText block)) -> ContentText (block {textBlockText = textBlockText block <> delta, textBlockBase = setStreaming True (textBlockBase block)})
      (TextPart, _) -> ContentText (TextBlock delta (setStreaming True base))
      (ThinkingPart, Just (ContentThinking block)) -> ContentThinking (block {thinkingBlockThinking = thinkingBlockThinking block <> delta, thinkingBlockBase = thinkingBase (thinkingBlockBase block)})
      (ThinkingPart, _) -> ContentThinking (ThinkingBlock "" Nothing delta Nothing (thinkingBase base))
    thinkingBase previous = setStreaming True (previous {blockAdditionalFields = KeyMap.insert "startedAtMs" (Number (snd start)) (KeyMap.union (blockAdditionalFields previous) (KeyMap.singleton "supportsThinkingDuration" (Bool True)))})
    updated = updateMessageContent wall identifier (replaceContent position replacement content) state

completeBlock :: Scientific -> Integer -> StreamBlockKind -> Text -> Scientific -> Maybe ThinkingDuration -> SessionState -> SessionState
completeBlock wall monotonic kind identifier index duration state = case Map.lookup identifier (messages state) of
  Nothing -> state
  Just message | messageRole message /= RoleAssistant -> invalidateMessageState MessageRoleConflict state
  Just message -> case findBlockPosition address (messageContent message) state of
    Nothing -> state
    Just position -> updateMessageContent wall identifier (mapAt position complete (messageContent message)) state
  where
    address = (identifier, kind, index)
    elapsed = Map.lookup address (thinkingStarts state) >>= \(started, _) -> mkThinkingDuration (scientific (max 0 (monotonic - started)) (-3))
    complete (ContentText block) = ContentText (block {textBlockBase = setStreaming False (textBlockBase block)})
    complete (ContentThinking block) = ContentThinking (block {thinkingBlockDuration = duration <|> elapsed <|> thinkingBlockDuration block, thinkingBlockBase = setStreaming False (thinkingBlockBase block)})
    complete block = block

replaceContent :: Int -> ContentBlock -> [ContentBlock] -> [ContentBlock]
replaceContent position block content = case splitAt position content of
  (prefix, []) -> prefix <> [block]
  (prefix, _ : suffix) -> prefix <> [block] <> suffix

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt position update values = case splitAt position values of
  (prefix, value : suffix) -> prefix <> [update value] <> suffix
  _ -> values

updateMessageContent :: Scientific -> Text -> [ContentBlock] -> SessionState -> SessionState
updateMessageContent wall identifier content state = state {messages = Map.adjust (\message -> message {messageContent = content, messageUpdatedAt = wall}) identifier (messages state)}

matchingTool :: Text -> ContentBlock -> Bool
matchingTool identifier (ContentToolUse tool) = toolUseId tool == identifier
matchingTool _ _ = False

matchingResult :: Text -> ContentBlock -> Bool
matchingResult identifier (ContentToolResult result) = toolResultToolUseId result == identifier
matchingResult _ _ = False

toolOwner :: Text -> SessionState -> Maybe FactoryDroidMessage
toolOwner identifier state = find (\message -> messageRole message == RoleAssistant && any (matchingTool identifier) (messageContent message)) (reverse (sessionMessages state))

upsertContent :: (ContentBlock -> Bool) -> ContentBlock -> [ContentBlock] -> [ContentBlock]
upsertContent matches block content = maybe (content <> [block]) (\position -> replaceContent position block content) (findIndex matches content)

observeToolCall :: Scientific -> ToolUseBlock -> SessionState -> SessionState
observeToolCall wall tool original = case toolOwner (toolUseId tool) state <|> lastAssistant of
  Nothing -> state {pendingToolCalls = if any ((== toolUseId tool) . toolUseId) (pendingToolCalls state) then map (\old -> if toolUseId old == toolUseId tool then tool else old) (pendingToolCalls state) else pendingToolCalls state <> [tool]}
  Just owner ->
    let content = upsertContent (matchingTool (toolUseId tool)) (ContentToolUse tool) (messageContent owner)
        updated = updateMessageContent wall (messageId owner) content state
     in adoptToolResults (owner {messageContent = content}) updated
  where
    state = trackTodoUse tool original
    lastAssistant = case listToMaybe (reverse (sessionMessages state)) of
      Just message | messageRole message == RoleAssistant -> Just message
      _ -> Nothing

observeToolResult :: Scientific -> ToolResultNotification -> SessionState -> SessionState
observeToolResult wall result original = case Map.lookup identifier (messages state) of
  Just message | messageRole message /= RoleTool -> invalidateMessageState MessageRoleConflict state
  Just message -> updateMessageContent wall identifier (upsertContent (matchingResult toolId) (ContentToolResult block) (messageContent message)) state
  Nothing -> case toolOwner toolId state of
    Just owner -> upsertSessionMessage (newMessage {messageParentId = Just (messageId owner)}) state
    Nothing -> state {orphanToolMessages = Map.insert toolId newMessage (orphanToolMessages state)}
  where
    state = applyTodoResult block original
    identifier = resultNotificationMessageId result
    block = resultNotificationBlock result
    toolId = toolResultToolUseId block
    newMessage = (emptyObservedMessage wall identifier RoleTool) {messageContent = [ContentToolResult block]}

adoptToolResults :: FactoryDroidMessage -> SessionState -> SessionState
adoptToolResults assistant state
  | messageRole assistant /= RoleAssistant = state
  | otherwise = foldl' adopt (state {pendingToolCalls = filter (\tool -> Set.notMember (toolUseId tool) toolSet) (pendingToolCalls state)}) toolIds
  where
    toolIds = nubOrd [toolUseId tool | ContentToolUse tool <- messageContent assistant]
    toolSet = Set.fromList toolIds
    adopt current toolId = case Map.lookup toolId (orphanToolMessages current) of
      Nothing -> current
      Just orphan ->
        let cleaned = current {orphanToolMessages = Map.delete toolId (orphanToolMessages current)}
         in if Map.member (messageId orphan) (messages current) then cleaned else upsertSessionMessage (orphan {messageParentId = Just (messageId assistant)}) cleaned

emptyObservedMessage :: Scientific -> Text -> MessageRole -> FactoryDroidMessage
emptyObservedMessage wall identifier role =
  Message
    { messageId = identifier,
      messageRole = role,
      messageContent = [],
      messageCreatedAt = wall,
      messageUpdatedAt = wall,
      messageParentId = Nothing,
      messageVisibility = Nothing,
      messageOpenAIMessageId = Nothing,
      messageOpenAIPhase = Nothing,
      messageOpenAIEncryptedContent = Nothing,
      messageOpenAIReasoningId = Nothing,
      messageOpenAIReasoningSummary = Nothing,
      messageGeminiThoughtSignature = Nothing,
      messageChatCompletionReasoningField = Nothing,
      messageChatCompletionReasoningContent = Nothing,
      messageIsUserVisible = Nothing,
      messageIsError = Nothing,
      messageUserSource = Nothing,
      messageInteractionMode = Nothing,
      messageModelId = Nothing,
      messageRouterId = Nothing,
      messageReasoningEffort = Nothing,
      messageApiProvider = Nothing,
      messageHookEventName = Nothing,
      messageHookMatcher = Nothing,
      messageHookCommands = Nothing,
      messageHookStatus = Nothing,
      messageHookResults = Nothing,
      messageHookToolCallId = Nothing,
      messageHookParentId = Nothing,
      messageHookOrder = Nothing,
      messageHookPreventedAction = Nothing,
      messageHiddenFromUserViews = Nothing,
      messageHookStartTime = Nothing,
      messageHookEndTime = Nothing,
      messageIsParallelExecution = Nothing,
      messageParallelGroupId = Nothing,
      messageAdditionalFields = mempty
    }

-- | Preserve the first parent for duplicate IDs and cut self-links/cycles.
-- Missing parents remain explicit: loading older history can resolve them.
repairMessageParents :: [FactoryDroidMessage] -> [FactoryDroidMessage]
repairMessageParents original = map cutParent normalized
  where
    firstParents = Map.fromListWith (\_ first -> first) [(messageId message, if messageParentId message == Just (messageId message) then Nothing else messageParentId message) | message <- original]
    normalized = [message {messageParentId = firstParents Map.! messageId message} | message <- original]
    byId = Map.fromList [(messageId message, message) | message <- normalized]
    (_, cut) = foldl' visit (Set.empty, Set.empty) (nubOrd (map messageId normalized))
    visit (safe, broken) identifier =
      let (seen, closing) = walk safe Set.empty Nothing (Map.lookup identifier byId)
       in (safe <> seen, maybe broken (`Set.insert` broken) closing)
    walk _ seen _ Nothing = (seen, Nothing)
    walk safe seen previous (Just message)
      | Set.member identifier safe = (seen, Nothing)
      | Set.member identifier seen = (seen, previous)
      | otherwise = walk safe (Set.insert identifier seen) (Just identifier) (parentMessage byId message)
      where
        identifier = messageId message
    cutParent message
      | Set.member (messageId message) cut = message {messageParentId = Nothing}
      | otherwise = message

parentMessage :: Map Text FactoryDroidMessage -> FactoryDroidMessage -> Maybe FactoryDroidMessage
parentMessage byId message = do
  parent <- messageParentId message
  if Text.null parent || parent == messageId message then Nothing else Map.lookup parent byId

singleRooted :: [FactoryDroidMessage] -> Bool
singleRooted history = case [message | message <- history, Nothing <- [parentMessage byId message]] of
  [root] -> Set.size (descendants Set.empty [messageId root]) == Map.size byId
  _ -> False
  where
    byId = Map.fromList [(messageId message, message) | message <- history]
    children = Map.fromListWith (<>) [(messageId parent, [messageId message]) | message <- history, Just parent <- [parentMessage byId message]]
    descendants seen [] = seen
    descendants seen (identifier : rest)
      | Set.member identifier seen = descendants seen rest
      | otherwise = descendants (Set.insert identifier seen) (Map.findWithDefault [] identifier children <> rest)

-- | Follow the newest leaf's parent chain, then append disconnected history.
-- Equal-time leaf IDs use portable UTF-16 order, not host-locale collation.
orderMessagesByParentChain :: [FactoryDroidMessage] -> [FactoryDroidMessage]
orderMessagesByParentChain history
  | null links = sortOn messageCreatedAt history
  | otherwise = chain <> sortOn messageCreatedAt [message | message <- history, Set.notMember (messageId message) visited]
  where
    byId = Map.fromList [(messageId message, message) | message <- history]
    links = [messageId parent | message <- history, Just parent <- [parentMessage byId message]]
    parents = Set.fromList links
    leaves = [message | message <- history, Set.notMember (messageId message) parents]
    candidates = if null leaves then history else leaves
    tip = listToMaybe (sortOn (\message -> (Down (messageCreatedAt message), encodeUtf16BE (messageId message))) candidates)
    (visited, chain) = follow Set.empty [] tip
    follow seen ordered Nothing = (seen, ordered)
    follow seen ordered (Just message)
      | Set.member (messageId message) seen = (seen, ordered)
      | otherwise = follow (Set.insert (messageId message) seen) (message : ordered) (parentMessage byId message)

sessionTodos :: SessionState -> Maybe [TodoItem]
sessionTodos = fmap snd . selectedTodos

-- | Runtime TodoWrite normalization, distinct from its string-only wire input
-- codec. Structured rows are filtered and numbered after validation.
parseTodos :: Value -> [TodoItem]
parseTodos (String text)
  | Text.isPrefixOf "[" (trimTodo text), Right array@(Array _) <- eitherDecodeStrict' (encodeUtf8 (trimTodo text)) = parseTodos array
  | otherwise = parseTodoLines text
parseTodos (Array values) = case toList values of
  rows@(String _ : _) -> parseTodoLines (Text.intercalate "\n" (map todoLineValue rows))
  rows -> [TodoItem (fromMaybe (Text.pack (show index)) identifier) content status priority | (index, (identifier, content, status, priority)) <- zip [1 :: Integer ..] (mapMaybe todoRow rows)]
parseTodos _ = []

todoRow :: Value -> Maybe (Maybe Text, Text, TodoStatus, TodoPriority)
todoRow (Object fields) = do
  String content <- KeyMap.lookup "content" fields
  String rawStatus <- KeyMap.lookup "status" fields
  status <- todoStatusFromText rawStatus
  let identifier = case KeyMap.lookup "id" fields of Just (String value) -> Just value; _ -> Nothing
      priority = case KeyMap.lookup "priority" fields of Just (String "medium") -> TodoMedium; Just (String "low") -> TodoLow; _ -> TodoHigh
  pure (identifier, content, status, priority)
todoRow _ = Nothing

todoStatusFromText :: Text -> Maybe TodoStatus
todoStatusFromText "pending" = Just TodoPending
todoStatusFromText "in_progress" = Just TodoInProgress
todoStatusFromText "completed" = Just TodoCompleted
todoStatusFromText _ = Nothing

trimTodo :: Text -> Text
trimTodo = Text.dropAround isEcmaWhitespace

todoLineValue :: Value -> Text
todoLineValue (String text) = text
todoLineValue Null = ""
todoLineValue (Array values) = Text.intercalate "," (map todoLineValue (toList values))
todoLineValue (Object _) = "[object Object]"
todoLineValue (Bool value) = if value then "true" else "false"
todoLineValue (Number value) = todoNumberText (toRealFloat value)

-- Mixed string arrays use JavaScript's numeric display coercion only; the
-- retained tool input and all other numeric fields remain exact Scientific.
todoNumberText :: Double -> Text
todoNumberText value
  | value == 0 = "0"
  | isInfinite value = if value < 0 then "-Infinity" else "Infinity"
  | value < 0 = "-" <> todoNumberText (negate value)
  | decimalPosition > 0 && decimalPosition <= 21 = Text.take decimalPosition padded <> if decimalPosition < Text.length digits then "." <> Text.drop decimalPosition digits else ""
  | decimalPosition <= 0 && decimalPosition > -6 = "0." <> Text.replicate (negate decimalPosition) "0" <> digits
  | otherwise = Text.take 1 digits <> fractional <> "e" <> (if power >= 0 then "+" else "") <> Text.pack (show power)
  where
    -- Binary64 needs at most seventeen significant decimal digits. The first
    -- round-tripping precision avoids GHC's longer default spelling of 1e23.
    shortest precision =
      let rendered = showEFloat (Just (precision - 1)) value ""
       in if precision == 17 || readMaybe rendered == Just value then rendered else shortest (precision + 1)
    (mantissa, exponentPart) = break (== 'e') (shortest 1)
    decimalPosition = 1 + read (drop 1 exponentPart)
    digits = Text.filter (/= '.') (Text.pack mantissa)
    padded = digits <> Text.replicate (max 0 (decimalPosition - Text.length digits)) "0"
    fractional = if Text.length digits > 1 then "." <> Text.drop 1 digits else ""
    power = decimalPosition - 1

parseTodoLines :: Text -> [TodoItem]
parseTodoLines text = zipWith parse [1 :: Integer ..] (filter (not . Text.null) (map trimTodo (Text.splitOn "\n" (trimTodo text))))
  where
    parse index line =
      let candidate = fromMaybe line (todoPrefix False line)
          checked = do
            suffix <- Text.stripPrefix "[" candidate
            let (marker, closing) = Text.breakOn "]" suffix
            remainder <- Text.stripPrefix "]" closing
            parsedStatus <- todoStatusFromText marker <|> (if marker `elem` ["x", "X"] then Just TodoCompleted else if Text.all isEcmaWhitespace marker then Just TodoPending else Nothing)
            let description = trimTodo remainder
            if hasLineTerminator description then Nothing else Just (parsedStatus, if Text.null description then "(no description)" else description)
          plain = fromMaybe line (todoPrefix True line >>= \value -> if Text.null value || hasLineTerminator value then Nothing else Just value)
          (status, content) = fromMaybe (TodoPending, plain) checked
       in TodoItem (Text.pack (show index)) content status TodoHigh
    hasLineTerminator = Text.any (`elem` ['\r', '\x2028', '\x2029'])

todoPrefix :: Bool -> Text -> Maybe Text
todoPrefix requireGap text =
  let (digits, suffix) = Text.span isDigit text
      numbered = do
        (punctuation, rest) <- Text.uncons suffix
        if Text.null digits || punctuation `notElem` ['.', ')'] then Nothing else spaced requireGap rest
      bulleted = do
        (bullet, rest) <- Text.uncons text
        if bullet `elem` ['-', '*'] then spaced True rest else Nothing
   in numbered <|> bulleted
  where
    spaced required rest = if required && maybe True (not . isEcmaWhitespace . fst) (Text.uncons rest) then Nothing else Just (Text.dropWhile isEcmaWhitespace rest)

todoListFromTool :: ToolUseBlock -> Maybe [TodoItem]
todoListFromTool tool
  | toolUseName tool /= "TodoWrite" = Nothing
  | otherwise = do
      value <- KeyMap.lookup "todos" (toolUseInput tool)
      let todos = parseTodos value
      if null todos then Nothing else Just todos

trackTodoUse :: ToolUseBlock -> SessionState -> SessionState
trackTodoUse tool state
  | toolUseName tool /= "TodoWrite" = state
  | otherwise = case Map.lookup (toolUseId tool) (todoToolUses state) of
      Just (sequenceNumber, _) -> state {todoToolUses = Map.insert (toolUseId tool) (sequenceNumber, tool) (todoToolUses state)}
      Nothing -> state {todoToolUses = Map.insert (toolUseId tool) (nextTodoSequence state, tool) (todoToolUses state), nextTodoSequence = nextTodoSequence state + 1}

applyTodoResult :: ToolResultBlock -> SessionState -> SessionState
applyTodoResult result state
  | toolResultIsError result == Just True = state
  | Just (sequenceNumber, tool) <- Map.lookup (toolResultToolUseId result) (todoToolUses state),
    maybe True ((<= sequenceNumber) . fst) (selectedTodos state),
    Just todos <- todoListFromTool tool =
      state {selectedTodos = Just (sequenceNumber, todos)}
  | otherwise = state

applyMessageTodos :: FactoryDroidMessage -> SessionState -> SessionState
applyMessageTodos message state = foldl' (flip applyTodoResult) tracked [result | ContentToolResult result <- messageContent message]
  where
    tracked = foldl' (flip trackTodoUse) state [tool | ContentToolUse tool <- messageContent message]

rebuildTodos :: SessionState -> SessionState
rebuildTodos state = tracked {selectedTodos = do tool <- select Set.empty Set.empty Nothing (reverse blocks); (sequenceNumber, _) <- Map.lookup (toolUseId tool) (todoToolUses tracked); todos <- todoListFromTool tool; pure (sequenceNumber, todos)}
  where
    blocks = concatMap messageContent (sessionMessages state) <> map ContentToolUse (pendingToolCalls state)
    tracked = foldl' (flip trackTodoUse) (state {todoToolUses = mempty, nextTodoSequence = 0, selectedTodos = Nothing}) [tool | ContentToolUse tool <- blocks]
    select _ _ pending [] = pending
    select successful failed pending (ContentToolResult result : rest)
      | toolResultIsError result == Just True = select successful (Set.insert (toolResultToolUseId result) failed) pending rest
      | otherwise = select (Set.insert (toolResultToolUseId result) successful) failed pending rest
    select successful failed pending (ContentToolUse tool : rest)
      | Just _ <- todoListFromTool tool = if Set.member (toolUseId tool) successful then Just tool else select successful failed (pending <|> (if Set.member (toolUseId tool) failed then Nothing else Just tool)) rest
    select successful failed pending (_ : rest) = select successful failed pending rest

-- | Session event observation through the same immutable model.
applySessionEventAt :: Scientific -> Integer -> DroidEvent -> SessionState -> SessionState
applySessionEventAt wall monotonic event original = case event of
  WorkingDirectoryEvent changed -> observeWorkingDirectory (Just (updatedWorkingDirectory changed)) state
  ToolCallDeltaEvent call -> setToolPhase (toolUseId (calledToolUse call)) ExecutionStreamingInput state
  ToolProgressEvent progress -> addToolProgress (progressNotificationToolUseId progress) (progressNotificationUpdate progress) (setToolPhase (progressNotificationToolUseId progress) ExecutionExecuting state)
  ToolPhaseEvent phase -> setToolPhase (phaseToolUseId phase) (changedToolPhase phase) state
  ToolResultEvent result ->
    let identifier = toolResultToolUseId (resultNotificationBlock result)
        settled = setToolPhase identifier (settleToolPhase (Map.lookup identifier (sessionToolPhases state))) state
     in settled {toolProgressDeadlines = Map.insert identifier (monotonic + 60000000) (toolProgressDeadlines settled)}
  HookStartedEvent started -> startHook wall monotonic started state
  HookCompletedEvent completed -> state {hookObservations = Map.adjust (\hook -> hook {observedHookOutcome = HookReported completed}) (completedHookId completed) (hookObservations state)}
  SessionCompactedEvent compacted | Just boundary <- compactedVisibleBoundaryId compacted, not (Text.null boundary) -> setSessionDisplayCutoff (Just boundary) state
  QueuedMessagesDiscardedEvent discarded -> pauseDaemonQueue (discardedRequestId discarded) (maybe state (`cancelSubmission` state) (discardedRequestId discarded))
  RetryEvent retry -> state {sessionRetry = Just retry}
  ErrorEvent _ -> finishStreaming wall monotonic state
  WorkingStateEvent changed | workingStateNewState changed `notElem` [WorkingThinking, WorkingStreamingAssistantMessage] -> finishStreaming wall monotonic state
  _ -> state
  where
    applied = applyMessageEventAt wall monotonic event original
    state = if clearsRetry event then applied {sessionRetry = Nothing} else applied

clearsRetry :: DroidEvent -> Bool
clearsRetry = \case
  TextDeltaEvent _ -> True
  ThinkingDeltaEvent _ -> True
  MessageEvent _ -> True
  ToolCallDeltaEvent _ -> True
  ToolResultEvent _ -> True
  ToolProgressEvent _ -> True
  ToolPhaseEvent _ -> True
  WorkingStateEvent _ -> True
  TurnCompletedEvent _ -> True
  ErrorEvent _ -> True
  _ -> False

finishStreaming :: Scientific -> Integer -> SessionState -> SessionState
finishStreaming wall monotonic state = foldl' (\current (identifier, kind, index) -> completeBlock wall monotonic kind identifier index Nothing current) state (filter active (Map.keys (blockPositions state)))
  where
    active address@(identifier, _, _) = fromMaybe False $ do
      message <- Map.lookup identifier (messages state)
      position <- findBlockPosition address (messageContent message) state
      block <- listToMaybe (drop position (messageContent message))
      let fields = case block of ContentText value -> blockAdditionalFields (textBlockBase value); ContentThinking value -> blockAdditionalFields (thinkingBlockBase value); _ -> mempty
      pure (KeyMap.lookup "isStreaming" fields == Just (Bool True))

toolPhaseRank :: ToolExecutionPhase -> Int
toolPhaseRank ExecutionStreamingInput = 0
toolPhaseRank ExecutionQueued = 1
toolPhaseRank ExecutionExecuting = 2
toolPhaseRank _ = 3

setToolPhase :: Text -> ToolExecutionPhase -> SessionState -> SessionState
setToolPhase identifier phase state = state {sessionToolPhases = Map.insertWith (\new old -> if toolPhaseRank new > toolPhaseRank old then new else old) identifier phase (sessionToolPhases state)}

settleToolPhase :: Maybe ToolExecutionPhase -> ToolExecutionPhase
settleToolPhase (Just ExecutionExecuting) = ExecutionSettledAfterExecution
settleToolPhase (Just phase) | phase `elem` [ExecutionStreamingInput, ExecutionQueued] = ExecutionSettledWithoutExecution
settleToolPhase (Just phase) = phase
settleToolPhase Nothing = ExecutionSettledUnknown

addToolProgress :: Text -> ToolProgressUpdate -> SessionState -> SessionState
addToolProgress identifier update state
  | previous : _ <- reverse existing, toJSON (previous {progressTimestamp = Nothing}) == toJSON (update {progressTimestamp = Nothing}) = state
  | otherwise = state {sessionToolProgress = Map.insert identifier (existing <> [update]) (sessionToolProgress state), toolProgressOrder = if Map.member identifier (sessionToolProgress state) then toolProgressOrder state else toolProgressOrder state |> identifier, toolProgressDeadlines = Map.delete identifier (toolProgressDeadlines state)}
  where
    existing = Map.findWithDefault [] identifier (sessionToolProgress state)

allToolProgress :: SessionState -> [ToolProgressUpdate]
allToolProgress state = sortOn (fromMaybe 0 . progressTimestamp) [update | identifier <- toList (toolProgressOrder state), update <- Map.findWithDefault [] identifier (sessionToolProgress state)]

clearToolProgress :: Text -> SessionState -> SessionState
clearToolProgress identifier state = state {sessionToolProgress = Map.delete identifier (sessionToolProgress state), toolProgressOrder = Seq.filter (/= identifier) (toolProgressOrder state), toolProgressDeadlines = Map.delete identifier (toolProgressDeadlines state)}

queueEntryRequestId :: QueueEntry -> Text
queueEntryRequestId = queuedMessageRequestId . queueEntryMessage

sessionQueue :: SessionState -> [QueueEntry]
sessionQueue state = mapMaybe (`Map.lookup` queuedMessages state) (toList (queuedOrder state))

lookupQueuedMessage :: Text -> SessionState -> Maybe QueueEntry
lookupQueuedMessage identifier = Map.lookup identifier . queuedMessages

isDaemonQueuedMessage :: QueuedMessageKind -> Bool
isDaemonQueuedMessage kind = kind `elem` [QueueDaemonDiscardable, QueueDaemonEndOfLoop]

isReviewableQueuedMessage :: QueuedMessageKind -> Bool
isReviewableQueuedMessage = isJust . queueReviewPriority

queueDisplayGroup :: QueuedMessageKind -> QueueDisplayGroup
queueDisplayGroup = \case
  QueueDeferredAfterInterrupt -> QueueSteeringGroup
  QueuePaused -> QueueQueuedGroup
  QueueDaemonDiscardable -> QueueSteeringGroup
  QueueDaemonEndOfLoop -> QueueQueuedGroup
  QueueDeferredDuringCompaction -> QueueQueuedGroup

-- | Lower values are reviewed first. Nothing is not a zero-priority entry;
-- it denotes a non-reviewable kind. This never dequeues or sends anything.
queueReviewPriority :: QueuedMessageKind -> Maybe Int
queueReviewPriority = \case
  QueueDeferredAfterInterrupt -> Nothing
  QueuePaused -> Just 1
  QueueDaemonDiscardable -> Just 0
  QueueDaemonEndOfLoop -> Just 1
  QueueDeferredDuringCompaction -> Nothing

queueKindForPlacement :: Maybe QueuePlacement -> QueuedMessageKind
queueKindForPlacement (Just QueueEndOfLoop) = QueueDaemonEndOfLoop
queueKindForPlacement _ = QueueDaemonDiscardable

-- | Queue observation supersedes the optimistic overlay, but never releases
-- ownership of a request still on the wire. Confirmed IDs cannot be requeued.
enqueueMessages :: [QueueEntry] -> SessionState -> SessionState
enqueueMessages entries state = foldl' enqueue state entries
  where
    enqueue previous entry
      | Set.member identifier (processedRequests previous) = previous
      | otherwise = (cancelSubmission identifier previous) {queuedMessages = Map.insert identifier entry (queuedMessages previous), queuedOrder = if Map.member identifier (queuedMessages previous) then queuedOrder previous else queuedOrder previous |> identifier}
      where
        identifier = queueEntryRequestId entry

removeQueuedMessage :: Text -> SessionState -> SessionState
removeQueuedMessage identifier state = state {queuedMessages = Map.delete identifier (queuedMessages state), queuedOrder = Seq.filter (/= identifier) (queuedOrder state)}

clearQueuedMessages :: Maybe [QueuedMessageKind] -> SessionState -> SessionState
clearQueuedMessages kinds state = state {queuedMessages = Map.fromList [(queueEntryRequestId entry, entry) | entry <- kept], queuedOrder = Seq.fromList (map queueEntryRequestId kept)}
  where
    kept = [entry | entry <- sessionQueue state, not (matchesQueueKind kinds entry)]

matchesQueueKind :: Maybe [QueuedMessageKind] -> QueueEntry -> Bool
matchesQueueKind kinds entry = maybe True (queueEntryKind entry `elem`) kinds

replaceDaemonQueue :: [QueueEntry] -> SessionState -> SessionState
replaceDaemonQueue entries = enqueueMessages entries . clearQueuedMessages (Just [QueueDaemonDiscardable, QueueDaemonEndOfLoop])

dequeueQueuedMessage :: Maybe QueuedMessageKind -> SessionState -> (SessionState, Maybe QueueEntry)
dequeueQueuedMessage kind state = case find (matchesQueueKind ((: []) <$> kind)) (sessionQueue state) of
  Nothing -> (state, Nothing)
  Just entry -> (removeQueuedMessage (queueEntryRequestId entry) state, Just entry)

dequeueQueuedMessages :: Maybe [QueuedMessageKind] -> SessionState -> (SessionState, [QueueEntry])
dequeueQueuedMessages kinds state = (clearQueuedMessages kinds state, filter (matchesQueueKind kinds) (sessionQueue state))

-- | Restore in input order, with the last value for a duplicate ID. The same
-- processed-ID guard applies here, including to a late failed-send callback.
restoreQueueFront :: [QueueEntry] -> SessionState -> SessionState
restoreQueueFront entries state = restored {queuedMessages = Map.union front (queuedMessages restored), queuedOrder = Seq.fromList identifiers <> queuedOrder restored}
  where
    accepted = [entry | entry <- entries, Set.notMember (queueEntryRequestId entry) (processedRequests state)]
    identifiers = nubOrd (map queueEntryRequestId accepted)
    front = Map.fromList [(queueEntryRequestId entry, entry) | entry <- accepted]
    restored = foldl' (\current identifier -> cancelSubmission identifier (removeQueuedMessage identifier current)) state identifiers

markQueuedMessageProcessed :: Text -> SessionState -> SessionState
markQueuedMessageProcessed = confirmSubmission

pauseDaemonQueue :: Maybe Text -> SessionState -> SessionState
pauseDaemonQueue restoredId state = state {queuedMessages = Map.fromList [(queueEntryRequestId entry, entry) | entry <- paused], queuedOrder = Seq.fromList (map queueEntryRequestId paused)}
  where
    paused = mapMaybe pause (sessionQueue state)
    pause entry
      | Just (queueEntryRequestId entry) == restoredId = if hasQueueAttachment entry then Just (entry {queueEntryKind = QueuePaused}) else Nothing
      | not (isDaemonQueuedMessage (queueEntryKind entry)) = Just entry
      | queueEntryKind entry == QueueDaemonDiscardable && not (hasQueueAttachment entry) = Nothing
      | otherwise = Just (entry {queueEntryKind = QueuePaused})

hasQueueAttachment :: QueueEntry -> Bool
hasQueueAttachment entry = or [maybe False (not . null) (userMessageImages input), maybe False (not . null) (userMessageImagePaths input), maybe False (not . null) (userMessageFiles input), maybe False (any (\case UserMessageImage _ -> True; _ -> False)) (userMessageContent input)]
  where
    input = queuedMessageInput (queueEntryMessage entry)

-- | An accepted load replaces daemon entries, preserving local work. Missing
-- idle-daemon entries become paused: this observer never implicitly resends an
-- uncertain mutation or infers delivery from matching text or wall clocks.
reconcileDaemonQueueAt :: Scientific -> [FactoryDroidMessage] -> [QueuedUserMessage] -> Bool -> SessionState -> SessionState
reconcileDaemonQueueAt observedAt loaded restored busy state = replaceDaemonQueue (reported <> retained) confirmed
  where
    restoredIds = Set.fromList (map queuedMessageRequestId restored)
    previous = filter (isDaemonQueuedMessage . queueEntryKind) (sessionQueue state)
    fresh = Set.fromList (map messageId loaded) `Set.difference` Map.keysSet (messages state)
    delivered = [queueEntryRequestId entry | entry <- sessionQueue state, Set.notMember (queueEntryRequestId entry) restoredIds, Just identifier <- [userMessageId (queuedMessageInput (queueEntryMessage entry))], Set.member identifier fresh]
    confirmed = foldl' (flip confirmSubmission) state delivered
    reported = [QueueEntry message (queueKindForPlacement (userMessageQueuePlacement (queuedMessageInput message))) observedAt | message <- restored]
    retained = [if busy then entry else entry {queueEntryKind = QueuePaused} | entry <- previous, Set.notMember (queueEntryRequestId entry) restoredIds, Set.notMember (queueEntryRequestId entry) (processedRequests confirmed)]

applyQueueResolution :: ResolveQueuedMessageParams -> SessionState -> SessionState
applyQueueResolution params state = case queueResolution params of
  DeleteQueuedMessage -> markQueuedMessageProcessed (queueRequestId params) state
  UpdateQueuedMessage placement -> case lookupQueuedMessage (queueRequestId params) state of
    Nothing -> state
    Just entry ->
      let message = queueEntryMessage entry
          input = (queuedMessageInput message) {userMessageQueuePlacement = Just placement}
       in restoreQueueFront [entry {queueEntryKind = queueKindForPlacement (Just placement), queueEntryMessage = message {queuedMessageInput = input}}] state

sessionHooks :: SessionState -> [HookObservation]
sessionHooks state = mapMaybe (`Map.lookup` hookObservations state) (toList (hookOrder state))

visibleSessionHooks :: SessionState -> [HookObservation]
visibleSessionHooks = filter visible . sessionHooks
  where
    visible hook = startedHookHidden (observedHookStart hook) /= Just True && case observedHookOutcome hook of HookReported completed -> completedHookHidden completed /= Just True; _ -> True

startHook :: Scientific -> Integer -> HookExecutionStarted -> SessionState -> SessionState
startHook wall monotonic started state = state {hookObservations = Map.insert identifier (HookObservation started wall monotonic HookRunning) (hookObservations state), hookOrder = if Map.member identifier (hookObservations state) then hookOrder state else hookOrder state |> identifier}
  where
    identifier = startedHookId started

hookLeaseSeconds :: HookObservation -> Scientific
hookLeaseSeconds = foldl' max 60 . map (fromMaybe 60 . hookCommandTimeout) . startedHookCommands . observedHookStart

expireHook :: Integer -> HookObservation -> HookObservation
expireHook now hook = case observedHookOutcome hook of
  HookRunning | scientific (now - observedHookMonotonicStart hook) (-6) - 30 >= hookLeaseSeconds hook -> hook {observedHookOutcome = HookLeaseExpired}
  _ -> hook

hookDeadline :: HookObservation -> Maybe Integer
hookDeadline hook = case observedHookOutcome hook of
  HookRunning | seconds <= nativeLimit -> Just (observedHookMonotonicStart hook + ceiling ((seconds + 30) * 1000000))
  _ -> Nothing
  where
    seconds = hookLeaseSeconds hook
    -- Keep arbitrary reported timeouts exact without expanding huge exponents.
    -- Leases beyond one native wait horizon are still checked on observation.
    nativeLimit = (fromIntegral (maxBound :: Int) - 30000000) / 1000000

filterDisplayMessages :: [FactoryDroidMessage] -> [FactoryDroidMessage]
filterDisplayMessages = filter nonempty . map clean . filter allowed
  where
    allowed message = messageHiddenFromUserViews message /= Just True && messageVisibility message /= Just VisibilityLlmOnly
    clean message
      | messageRole message == RoleAssistant = message
      | otherwise = message {messageContent = mapMaybe cleanBlock (messageContent message)}
    cleanBlock (ContentText block)
      | any (`Text.isInfixOf` textBlockText block) ["<system-reminder>", "<system-notification>"] =
          let text = Text.dropAround isEcmaWhitespace (stripSystemTags (textBlockText block)) in if Text.null text then Nothing else Just (ContentText (block {textBlockText = text}))
    cleanBlock block = Just block
    nonempty message = not (null (messageContent message)) || persistedHook message

-- | Recognize persisted hook metadata, independently of role or visibility.
-- Empty command lists count as present; event names are nonempty, not trimmed.
persistedHook :: FactoryDroidMessage -> Bool
persistedHook message = maybe False (not . Text.null) (messageHookEventName message) && isJust (messageHookCommands message) && isJust (messageHookStatus message)

stripSystemTags :: Text -> Text
stripSystemTags = stripTagged "<system-notification>" "</system-notification>" . stripTagged "<system-reminder>" "</system-reminder>"
  where
    stripTagged start end text = case Text.breakOn start text of
      (_, suffix) | Text.null suffix -> text
      (prefix, suffix) -> case Text.breakOn end (Text.drop (Text.length start) suffix) of
        (_, remaining) | Text.null remaining -> text
        (_, remaining) -> prefix <> stripTagged start end (Text.drop (Text.length end) remaining)

-- | Typed display rows avoid inventing wire-message IDs for local overlays.
-- Pending rows retain registration order; raw input remains explicit data.
sessionDisplayEntries :: SessionState -> [DisplayEntry]
sessionDisplayEntries state = maybe entries (\limit -> drop (max 0 (length entries - limit)) entries) (sessionDisplayLimit state)
  where
    pending = filter visiblePending (optimisticSubmissions state)
    system = [DisplaySubmission entry | entry <- pending, userMessageRole (submissionInput entry) == Just RoleSystem]
    others = [DisplaySubmission entry | entry <- pending, userMessageRole (submissionInput entry) /= Just RoleSystem]
    tools = [DisplayPendingTools (pendingToolCalls state) | not (null (pendingToolCalls state))]
    entries = system <> map DisplayMessage (filterDisplayMessages (sessionMessages state)) <> tools <> others
    visiblePending entry = userMessageVisibility (submissionInput entry) /= Just VisibilityLlmOnly && KeyMap.lookup "hiddenFromUserViews" (userMessageAdditionalFields (submissionInput entry)) /= Just (Bool True)

checkedSessionDisplay :: SessionState -> Either MessageStateError [DisplayEntry]
checkedSessionDisplay state = maybe (Right (sessionDisplayEntries state)) Left (sessionMessageError state)

initialDisplayLimit :: SessionState -> Maybe Int
initialDisplayLimit state = if sessionProgressiveDisplay state && Map.size (messages state) > 30 then Just 30 else Nothing

setProgressiveDisplay :: Bool -> SessionState -> SessionState
setProgressiveDisplay enabled state
  | enabled == sessionProgressiveDisplay state = state
  | otherwise = let changed = state {sessionProgressiveDisplay = enabled} in setSessionDisplayCutoff (sessionDisplayCutoff state) (changed {sessionDisplayLimit = initialDisplayLimit changed})

-- | Double the display window; the Boolean says whether it remains limited.
-- This reveals cached rows only and does not fetch remote history.
expandSessionDisplay :: SessionState -> (SessionState, Bool)
expandSessionDisplay state = case sessionDisplayLimit state of
  Nothing -> (state, False)
  Just limit ->
    let doubled = toInteger limit * 2
        next = if doubled >= toInteger (length (filterDisplayMessages (sessionMessages state))) then Nothing else Just (fromInteger doubled)
     in (state {sessionDisplayLimit = next}, isJust next)

setSessionDisplayCutoff :: Maybe Text -> SessionState -> SessionState
setSessionDisplayCutoff boundary state
  | Nothing <- boundary = state {sessionDisplayCutoff = Nothing, sessionHiddenMessageCount = 0}
  | not (sessionProgressiveDisplay state) = state {sessionDisplayCutoff = boundary}
  | otherwise = pruneBefore boundary (state {sessionDisplayCutoff = boundary})
  where
    pruneBefore (Just identifier) current = case break ((== identifier) . messageId) (sessionMessages current) of
      (_, []) -> current
      (before, after) ->
        let todo = listToMaybe [message | message <- reverse before, any isTodoUse (messageContent message)]
            todoIds = Set.fromList [toolUseId tool | message <- maybeToList todo, ContentToolUse tool <- messageContent message, toolUseName tool == "TodoWrite"]
            retained message = Just (messageId message) == (messageId <$> todo) || any (\case ContentToolResult result -> Set.member (toolResultToolUseId result) todoIds; _ -> False) (messageContent message)
            keep = filter retained before
            trimmed = retainMessages (Seq.fromList (map messageId (keep <> after))) current
         in trimmed {sessionHiddenMessageCount = sessionHiddenMessageCount current + fromIntegral (length before - length keep)}
    pruneBefore Nothing current = current
    isTodoUse (ContentToolUse tool) = toolUseName tool == "TodoWrite"
    isTodoUse _ = False

hydrationFloor :: [FactoryDroidMessage] -> Maybe Text
hydrationFloor history = messageId <$> listToMaybe (reverse (take floorIndex history))
  where
    results = [result | message <- history, ContentToolResult result <- messageContent message]
    settled = Set.fromList [toolResultToolUseId result | result <- results, not (Text.null (toolResultToolUseId result)), isJust (toolResultContent result), not (isPendingToolResult result)]
    unresolved = Set.fromList [toolUseId tool | message <- history, ContentToolUse tool <- messageContent message, Set.notMember (toolUseId tool) settled]
    (_, boundary) = foldl' step (0 :: Int, Nothing) history
    floorIndex = fromMaybe (length history) boundary
    step (index, previous) message =
      let hasText = messageRole message `elem` [RoleUser, RoleAssistant] && any (\case ContentText block -> not (Text.null (Text.dropAround isEcmaWhitespace (textBlockText block))); _ -> False) (messageContent message)
          reset = if hasText then Nothing else previous
          unsettled = (persistedHook message && messageHookStatus message == Just HookExecuting) || any (\case ContentToolUse tool -> Set.member (toolUseId tool) unresolved; ContentToolResult result -> (isPendingToolResult result && Set.notMember (toolResultToolUseId result) settled) || Set.member (toolResultToolUseId result) unresolved; _ -> False) (messageContent message)
       in (index + 1, reset <|> (if unsettled then Just index else Nothing))

expireSessionStateAt :: Integer -> SessionState -> SessionState
expireSessionStateAt now state = expired {hookObservations = Map.map (expireHook now) (hookObservations expired)}
  where
    expired = foldl' (flip clearToolProgress) (expireSubmissionsAt now state) [identifier | (identifier, deadline) <- Map.toList (toolProgressDeadlines state), now >= deadline]

nextSessionDeadline :: SessionState -> Maybe Integer
nextSessionDeadline state = case Map.elems (toolProgressDeadlines state) <> mapMaybe hookDeadline (sessionHooks state) <> maybeToList (nextSubmissionDeadline state) of
  [] -> Nothing
  deadlines -> Just (minimum deadlines)

expireSubmissionsAt :: Integer -> SessionState -> SessionState
expireSubmissionsAt now state = state {submissions = Map.map expire (submissions state)}
  where
    expire entry = case (submissionStatus entry, submissionDeadline entry) of
      (SubmissionPending, Just deadline) | now >= deadline -> entry {submissionStatus = SubmissionFailed SubmissionTimedOut, submissionDeadline = Nothing}
      _ -> entry

nextSubmissionDeadline :: SessionState -> Maybe Integer
nextSubmissionDeadline state = case [deadline | entry <- optimisticSubmissions state, submissionStatus entry == SubmissionPending, Just deadline <- [submissionDeadline entry]] of
  [] -> Nothing
  deadlines -> Just (minimum deadlines)
