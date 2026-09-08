{-# LANGUAGE OverloadedStrings #-}

-- | Mission features, handoff reports and independent notifications for
-- Factory protocol 1.205.0. These are peer-supplied data, not verified evidence
-- or instructions to execute commands, modify files or change mission state.
module Factory.Droid.Schema.Mission
  ( DecompSessionType (..),
    MissionPhase (..),
    FeatureStatus (..),
    FeatureSuccessState (..),
    IssueSeverity (..),
    DismissalType (..),
    SubagentStatus (..),
    MissionFeature (..),
    DiscoveredIssue (..),
    VerificationCommand (..),
    InteractiveCheck (..),
    Verification (..),
    HandoffTestCase (..),
    HandoffTestFile (..),
    HandoffTests (..),
    SkillDeviation (..),
    SkillFeedback (..),
    Handoff (..),
    DismissalRecord (..),
    WorkerStateInfo (..),
    SubagentInvocationSummary (..),
    MissionStateChanged (..),
    MissionFeaturesChanged (..),
    MissionHeartbeat (..),
    MissionWorkerStarted (..),
    MissionWorkerCompleted (..),
  )
where

import Data.Aeson (FromJSON (..), Object, Options, ToJSON (..), Value (String), camelTo2, genericParseJSON, genericToEncoding, genericToJSON, withObject, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, enumOptions, objectWithAdditionalFields, optionalField, requireLiteral)
import GHC.Generics (Generic)

-- | The session's reported role in mission decomposition.
data DecompSessionType = DecompOrchestrator | DecompWorker
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON DecompSessionType where
  parseJSON = genericParseJSON decompOptions

instance ToJSON DecompSessionType where
  toJSON = genericToJSON decompOptions
  toEncoding = genericToEncoding decompOptions

-- | The seven mission phase labels, without transition rules.
data MissionPhase = MissionPlanning | MissionAwaitingInput | MissionInitializing | MissionRunning | MissionPaused | MissionOrchestratorTurn | MissionCompleted
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON MissionPhase where
  parseJSON = genericParseJSON missionOptions

instance ToJSON MissionPhase where
  toJSON = genericToJSON missionOptions
  toEncoding = genericToEncoding missionOptions

-- | A feature's reported lifecycle status, distinct from a success assessment.
data FeatureStatus = FeaturePending | FeatureInProgress | FeatureCompleted | FeatureCancelled
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON FeatureStatus where
  parseJSON = genericParseJSON featureOptions

instance ToJSON FeatureStatus where
  toJSON = genericToJSON featureOptions
  toEncoding = genericToEncoding featureOptions

-- | A worker's success assessment; it is not independent verification.
data FeatureSuccessState = FeatureSuccess | FeaturePartial | FeatureFailure
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON FeatureSuccessState where
  parseJSON = genericParseJSON featureOptions

instance ToJSON FeatureSuccessState where
  toJSON = genericToJSON featureOptions
  toEncoding = genericToEncoding featureOptions

-- | Severity of a reported issue.
data IssueSeverity = IssueBlocking | IssueNonBlocking | IssueSuggestion
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON IssueSeverity where
  parseJSON = genericParseJSON issueOptions

instance ToJSON IssueSeverity where
  toJSON = genericToJSON issueOptions
  toEncoding = genericToEncoding issueOptions

-- | The class of handoff item reported as dismissed.
data DismissalType = DismissedDiscoveredIssue | DismissedCriticalContext | DismissedIncompleteWork
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON DismissalType where
  parseJSON = genericParseJSON dismissalOptions

instance ToJSON DismissalType where
  toJSON = genericToJSON dismissalOptions
  toEncoding = genericToEncoding dismissalOptions

-- | Subagent invocation status, separate from feature and mission states.
data SubagentStatus = SubagentPending | SubagentRunning | SubagentCompleted | SubagentFailed | SubagentCancelled
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON SubagentStatus where
  parseJSON = genericParseJSON subagentOptions

instance ToJSON SubagentStatus where
  toJSON = genericToJSON subagentOptions
  toEncoding = genericToEncoding subagentOptions

-- | A feature description. Legacy worker fields retain absence, explicit null
-- and a present identifier separately; worker lists are not deduplicated.
data MissionFeature = MissionFeature
  { missionFeatureId :: !Text,
    missionFeatureDescription :: !Text,
    missionFeatureStatus :: !FeatureStatus,
    missionFeatureSkillName :: !Text,
    missionFeaturePreconditions :: ![Text],
    missionFeatureExpectedBehavior :: ![Text],
    missionFeatureFulfills :: !(Maybe [Text]),
    missionFeatureMilestone :: !(Maybe Text),
    missionFeatureWorkerSessionIds :: !(Maybe [Text]),
    missionFeatureCurrentWorkerSessionId :: !(Maybe (Maybe Text)),
    missionFeatureCompletedWorkerSessionId :: !(Maybe (Maybe Text)),
    missionFeatureAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionFeature where
  parseJSON = withObject "MissionFeature" $ \fields ->
    MissionFeature
      <$> fields .: "id"
      <*> fields .: "description"
      <*> fields .: "status"
      <*> fields .: "skillName"
      <*> fields .: "preconditions"
      <*> fields .: "expectedBehavior"
      <*> fields .:! "fulfills"
      <*> fields .:! "milestone"
      <*> fields .:! "workerSessionIds"
      <*> fields .:! "currentWorkerSessionId"
      <*> fields .:! "completedWorkerSessionId"
      <*> pure (additionalFields featureKeys fields)

instance ToJSON MissionFeature where
  toJSON feature =
    objectWithAdditionalFields featureKeys (missionFeatureAdditionalFields feature) $
      [ "id" .= missionFeatureId feature,
        "description" .= missionFeatureDescription feature,
        "status" .= missionFeatureStatus feature,
        "skillName" .= missionFeatureSkillName feature,
        "preconditions" .= missionFeaturePreconditions feature,
        "expectedBehavior" .= missionFeatureExpectedBehavior feature
      ]
        <> optionalField "fulfills" (missionFeatureFulfills feature)
        <> optionalField "milestone" (missionFeatureMilestone feature)
        <> optionalField "workerSessionIds" (missionFeatureWorkerSessionIds feature)
        <> optionalField "currentWorkerSessionId" (missionFeatureCurrentWorkerSessionId feature)
        <> optionalField "completedWorkerSessionId" (missionFeatureCompletedWorkerSessionId feature)

-- | An issue reported by a worker, not an issue-tracker mutation.
data DiscoveredIssue = DiscoveredIssue
  { discoveredIssueSeverity :: !IssueSeverity,
    discoveredIssueDescription :: !Text,
    discoveredIssueSuggestedFix :: !(Maybe Text),
    discoveredIssueAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON DiscoveredIssue where
  parseJSON = withObject "DiscoveredIssue" $ \fields -> DiscoveredIssue <$> fields .: "severity" <*> fields .: "description" <*> fields .:! "suggestedFix" <*> pure (additionalFields ["severity", "description", "suggestedFix"] fields)

instance ToJSON DiscoveredIssue where
  toJSON issue = objectWithAdditionalFields ["severity", "description", "suggestedFix"] (discoveredIssueAdditionalFields issue) (["severity" .= discoveredIssueSeverity issue, "description" .= discoveredIssueDescription issue] <> optionalField "suggestedFix" (discoveredIssueSuggestedFix issue))

-- | A reported command result. Exit code is a JSON number; decoding neither
-- executes the command nor establishes that the reported result is accurate.
data VerificationCommand = VerificationCommand
  { verificationCommand :: !Text,
    verificationExitCode :: !Scientific,
    verificationObservation :: !Text,
    verificationCommandAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON VerificationCommand where
  parseJSON = withObject "VerificationCommand" $ \fields -> VerificationCommand <$> fields .: "command" <*> fields .: "exitCode" <*> fields .: "observation" <*> pure (additionalFields ["command", "exitCode", "observation"] fields)

instance ToJSON VerificationCommand where
  toJSON command = objectWithAdditionalFields ["command", "exitCode", "observation"] (verificationCommandAdditionalFields command) ["command" .= verificationCommand command, "exitCode" .= verificationExitCode command, "observation" .= verificationObservation command]

-- | A reported interactive action and observation, not an action to perform.
data InteractiveCheck = InteractiveCheck
  { interactiveCheckAction :: !Text,
    interactiveCheckObserved :: !Text,
    interactiveCheckAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON InteractiveCheck where
  parseJSON = withObject "InteractiveCheck" $ \fields -> InteractiveCheck <$> fields .: "action" <*> fields .: "observed" <*> pure (additionalFields ["action", "observed"] fields)

instance ToJSON InteractiveCheck where
  toJSON check = objectWithAdditionalFields ["action", "observed"] (interactiveCheckAdditionalFields check) ["action" .= interactiveCheckAction check, "observed" .= interactiveCheckObserved check]

-- | Structured verification reports. Required command lists may be empty.
data Verification = Verification
  { verificationCommandsRun :: ![VerificationCommand],
    verificationInteractiveChecks :: !(Maybe [InteractiveCheck]),
    verificationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON Verification where
  parseJSON = withObject "Verification" $ \fields -> Verification <$> fields .: "commandsRun" <*> fields .:! "interactiveChecks" <*> pure (additionalFields ["commandsRun", "interactiveChecks"] fields)

instance ToJSON Verification where
  toJSON verification = objectWithAdditionalFields ["commandsRun", "interactiveChecks"] (verificationAdditionalFields verification) (["commandsRun" .= verificationCommandsRun verification] <> optionalField "interactiveChecks" (verificationInteractiveChecks verification))

-- | A named test report; no test is defined or run by this value.
data HandoffTestCase = HandoffTestCase
  { handoffTestName :: !Text,
    handoffTestVerifies :: !Text,
    handoffTestCaseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON HandoffTestCase where
  parseJSON = withObject "HandoffTestCase" $ \fields -> HandoffTestCase <$> fields .: "name" <*> fields .: "verifies" <*> pure (additionalFields ["name", "verifies"] fields)

instance ToJSON HandoffTestCase where
  toJSON test = objectWithAdditionalFields ["name", "verifies"] (handoffTestCaseAdditionalFields test) ["name" .= handoffTestName test, "verifies" .= handoffTestVerifies test]

-- | A reported test-file path with its test cases. Paths are not accessed.
data HandoffTestFile = HandoffTestFile
  { handoffTestFile :: !Text,
    handoffTestCases :: ![HandoffTestCase],
    handoffTestFileAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON HandoffTestFile where
  parseJSON = withObject "HandoffTestFile" $ \fields -> HandoffTestFile <$> fields .: "file" <*> fields .: "cases" <*> pure (additionalFields ["file", "cases"] fields)

instance ToJSON HandoffTestFile where
  toJSON file = objectWithAdditionalFields ["file", "cases"] (handoffTestFileAdditionalFields file) ["file" .= handoffTestFile file, "cases" .= handoffTestCases file]

-- | Added test reports, optional updated-file names and a coverage description.
data HandoffTests = HandoffTests
  { handoffTestsAdded :: ![HandoffTestFile],
    handoffTestsCoverage :: !Text,
    handoffTestsUpdated :: !(Maybe [Text]),
    handoffTestsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON HandoffTests where
  parseJSON = withObject "HandoffTests" $ \fields -> HandoffTests <$> fields .: "added" <*> fields .: "coverage" <*> fields .:! "updated" <*> pure (additionalFields ["added", "coverage", "updated"] fields)

instance ToJSON HandoffTests where
  toJSON tests = objectWithAdditionalFields ["added", "coverage", "updated"] (handoffTestsAdditionalFields tests) (["added" .= handoffTestsAdded tests, "coverage" .= handoffTestsCoverage tests] <> optionalField "updated" (handoffTestsUpdated tests))

-- | A reported deviation from a skill procedure.
data SkillDeviation = SkillDeviation
  { skillDeviationStep :: !Text,
    skillDeviationInstead :: !Text,
    skillDeviationWhy :: !Text,
    skillDeviationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SkillDeviation where
  parseJSON = withObject "SkillDeviation" $ \fields -> SkillDeviation <$> fields .: "step" <*> fields .: "whatIDidInstead" <*> fields .: "why" <*> pure (additionalFields ["step", "whatIDidInstead", "why"] fields)

instance ToJSON SkillDeviation where
  toJSON deviation = objectWithAdditionalFields ["step", "whatIDidInstead", "why"] (skillDeviationAdditionalFields deviation) ["step" .= skillDeviationStep deviation, "whatIDidInstead" .= skillDeviationInstead deviation, "why" .= skillDeviationWhy deviation]

-- | Skill feedback. The followed flag and deviation list remain independent.
data SkillFeedback = SkillFeedback
  { skillFollowedProcedure :: !Bool,
    skillDeviations :: ![SkillDeviation],
    skillSuggestedChanges :: !(Maybe [Text]),
    skillFeedbackAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SkillFeedback where
  parseJSON = withObject "SkillFeedback" $ \fields -> SkillFeedback <$> fields .: "followedProcedure" <*> fields .: "deviations" <*> fields .:! "suggestedChanges" <*> pure (additionalFields ["followedProcedure", "deviations", "suggestedChanges"] fields)

instance ToJSON SkillFeedback where
  toJSON feedback = objectWithAdditionalFields ["followedProcedure", "deviations", "suggestedChanges"] (skillFeedbackAdditionalFields feedback) (["followedProcedure" .= skillFollowedProcedure feedback, "deviations" .= skillDeviations feedback] <> optionalField "suggestedChanges" (skillSuggestedChanges feedback))

-- | A worker's handoff report. Its claims are data, not proof of completion.
data Handoff = Handoff
  { handoffImplemented :: !Text,
    handoffLeftUndone :: !Text,
    handoffVerification :: !Verification,
    handoffTests :: !HandoffTests,
    handoffDiscoveredIssues :: ![DiscoveredIssue],
    handoffSalientSummary :: !(Maybe Text),
    handoffSkillFeedback :: !(Maybe SkillFeedback),
    handoffAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON Handoff where
  parseJSON = withObject "Handoff" $ \fields -> Handoff <$> fields .: "whatWasImplemented" <*> fields .: "whatWasLeftUndone" <*> fields .: "verification" <*> fields .: "tests" <*> fields .: "discoveredIssues" <*> fields .:! "salientSummary" <*> fields .:! "skillFeedback" <*> pure (additionalFields handoffKeys fields)

instance ToJSON Handoff where
  toJSON handoff =
    objectWithAdditionalFields handoffKeys (handoffAdditionalFields handoff) $
      ["whatWasImplemented" .= handoffImplemented handoff, "whatWasLeftUndone" .= handoffLeftUndone handoff, "verification" .= handoffVerification handoff, "tests" .= handoffTests handoff, "discoveredIssues" .= handoffDiscoveredIssues handoff]
        <> optionalField "salientSummary" (handoffSalientSummary handoff)
        <> optionalField "skillFeedback" (handoffSkillFeedback handoff)

-- | A recorded dismissal; decoding does not dismiss any issue or requirement.
data DismissalRecord = DismissalRecord
  { dismissalType :: !DismissalType,
    dismissalSourceFeatureId :: !Text,
    dismissalSummary :: !Text,
    dismissalJustification :: !Text,
    dismissalAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON DismissalRecord where
  parseJSON = withObject "DismissalRecord" $ \fields -> DismissalRecord <$> fields .: "type" <*> fields .: "sourceFeatureId" <*> fields .: "summary" <*> fields .: "justification" <*> pure (additionalFields dismissalKeys fields)

instance ToJSON DismissalRecord where
  toJSON dismissal = objectWithAdditionalFields dismissalKeys (dismissalAdditionalFields dismissal) ["type" .= dismissalType dismissal, "sourceFeatureId" .= dismissalSourceFeatureId dismissal, "summary" .= dismissalSummary dismissal, "justification" .= dismissalJustification dismissal]

-- | Reported worker timestamps and exit code. Timestamps are unparsed strings,
-- and the optional number does not establish that a worker has completed.
data WorkerStateInfo = WorkerStateInfo
  { workerStartedAt :: !Text,
    workerCompletedAt :: !(Maybe Text),
    workerExitCode :: !(Maybe Scientific),
    workerStateAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON WorkerStateInfo where
  parseJSON = withObject "WorkerStateInfo" $ \fields -> WorkerStateInfo <$> fields .: "startedAt" <*> fields .:! "completedAt" <*> fields .:! "exitCode" <*> pure (additionalFields ["startedAt", "completedAt", "exitCode"] fields)

instance ToJSON WorkerStateInfo where
  toJSON worker = objectWithAdditionalFields ["startedAt", "completedAt", "exitCode"] (workerStateAdditionalFields worker) (["startedAt" .= workerStartedAt worker] <> optionalField "completedAt" (workerCompletedAt worker) <> optionalField "exitCode" (workerExitCode worker))

-- | A subagent invocation report. Counts and durations retain JSON numbers;
-- decoding neither launches a subagent nor verifies its lifecycle state.
data SubagentInvocationSummary = SubagentInvocationSummary
  { invocationChildSessionId :: !Text,
    invocationStatus :: !SubagentStatus,
    invocationSubagentType :: !Text,
    invocationDescription :: !Text,
    invocationToolUseCount :: !(Maybe Scientific),
    invocationDurationMs :: !(Maybe Scientific),
    invocationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SubagentInvocationSummary where
  parseJSON = withObject "SubagentInvocationSummary" $ \fields -> SubagentInvocationSummary <$> fields .: "childSessionId" <*> fields .: "status" <*> fields .: "subagentType" <*> fields .: "description" <*> fields .:! "toolUseCount" <*> fields .:! "durationMs" <*> pure (additionalFields invocationKeys fields)

instance ToJSON SubagentInvocationSummary where
  toJSON invocation = objectWithAdditionalFields invocationKeys (invocationAdditionalFields invocation) (["childSessionId" .= invocationChildSessionId invocation, "status" .= invocationStatus invocation, "subagentType" .= invocationSubagentType invocation, "description" .= invocationDescription invocation] <> optionalField "toolUseCount" (invocationToolUseCount invocation) <> optionalField "durationMs" (invocationDurationMs invocation))

-- | A mission phase-change payload, not a transition applied to a state store.
data MissionStateChanged = MissionStateChanged
  { changedMissionPhase :: !MissionPhase,
    changedMissionUpdatedAt :: !(Maybe Text),
    changedMissionStateAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionStateChanged where
  parseJSON = withObject "MissionStateChanged" $ \fields -> do
    requireLiteral "type" "mission_state_changed" fields
    MissionStateChanged <$> fields .: "state" <*> fields .:! "updatedAt" <*> pure (additionalFields ["type", "state", "updatedAt"] fields)

instance ToJSON MissionStateChanged where
  toJSON event = objectWithAdditionalFields ["type", "state", "updatedAt"] (changedMissionStateAdditionalFields event) (["type" .= String "mission_state_changed", "state" .= changedMissionPhase event] <> optionalField "updatedAt" (changedMissionUpdatedAt event))

-- | A reported feature list; no feature store is updated by this value.
data MissionFeaturesChanged = MissionFeaturesChanged
  { changedMissionFeatures :: ![MissionFeature],
    changedMissionFeaturesAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionFeaturesChanged where
  parseJSON = withObject "MissionFeaturesChanged" $ \fields -> do
    requireLiteral "type" "mission_features_changed" fields
    MissionFeaturesChanged <$> fields .: "features" <*> pure (additionalFields ["type", "features"] fields)

instance ToJSON MissionFeaturesChanged where
  toJSON event = objectWithAdditionalFields ["type", "features"] (changedMissionFeaturesAdditionalFields event) ["type" .= String "mission_features_changed", "features" .= changedMissionFeatures event]

-- | A heartbeat timestamp report, not proof that a worker remains reachable.
data MissionHeartbeat = MissionHeartbeat
  { missionHeartbeatTimestamp :: !Text,
    missionHeartbeatAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionHeartbeat where
  parseJSON = withObject "MissionHeartbeat" $ \fields -> do
    requireLiteral "type" "mission_heartbeat" fields
    MissionHeartbeat <$> fields .: "timestamp" <*> pure (additionalFields ["type", "timestamp"] fields)

instance ToJSON MissionHeartbeat where
  toJSON event = objectWithAdditionalFields ["type", "timestamp"] (missionHeartbeatAdditionalFields event) ["type" .= String "mission_heartbeat", "timestamp" .= missionHeartbeatTimestamp event]

-- | A worker-start notification body, without spawning a process.
data MissionWorkerStarted = MissionWorkerStarted
  { startedMissionWorkerId :: !Text,
    startedMissionWorkerAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionWorkerStarted where
  parseJSON = withObject "MissionWorkerStarted" $ \fields -> do
    requireLiteral "type" "mission_worker_started" fields
    MissionWorkerStarted <$> fields .: "workerSessionId" <*> pure (additionalFields ["type", "workerSessionId"] fields)

instance ToJSON MissionWorkerStarted where
  toJSON event = objectWithAdditionalFields ["type", "workerSessionId"] (startedMissionWorkerAdditionalFields event) ["type" .= String "mission_worker_started", "workerSessionId" .= startedMissionWorkerId event]

-- | A worker-completed notification, distinct from the richer progress-log entry.
data MissionWorkerCompleted = MissionWorkerCompleted
  { completedMissionWorkerId :: !Text,
    completedMissionWorkerExitCode :: !Scientific,
    completedMissionWorkerAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionWorkerCompleted where
  parseJSON = withObject "MissionWorkerCompleted" $ \fields -> do
    requireLiteral "type" "mission_worker_completed" fields
    MissionWorkerCompleted <$> fields .: "workerSessionId" <*> fields .: "exitCode" <*> pure (additionalFields ["type", "workerSessionId", "exitCode"] fields)

instance ToJSON MissionWorkerCompleted where
  toJSON event = objectWithAdditionalFields ["type", "workerSessionId", "exitCode"] (completedMissionWorkerAdditionalFields event) ["type" .= String "mission_worker_completed", "workerSessionId" .= completedMissionWorkerId event, "exitCode" .= completedMissionWorkerExitCode event]

decompOptions, missionOptions, featureOptions, issueOptions, dismissalOptions, subagentOptions :: Options
decompOptions = enumOptions "Decomp" (camelTo2 '_')
missionOptions = enumOptions "Mission" (camelTo2 '_')
featureOptions = enumOptions "Feature" (camelTo2 '_')
issueOptions = enumOptions "Issue" (camelTo2 '_')
dismissalOptions = enumOptions "Dismissed" (camelTo2 '_')
subagentOptions = enumOptions "Subagent" (camelTo2 '_')

featureKeys, handoffKeys, dismissalKeys, invocationKeys :: [Key]
featureKeys = ["id", "description", "status", "skillName", "preconditions", "expectedBehavior", "fulfills", "milestone", "workerSessionIds", "currentWorkerSessionId", "completedWorkerSessionId"]
handoffKeys = ["whatWasImplemented", "whatWasLeftUndone", "verification", "tests", "discoveredIssues", "salientSummary", "skillFeedback"]
dismissalKeys = ["type", "sourceFeatureId", "summary", "justification"]
invocationKeys = ["childSessionId", "status", "subagentType", "description", "toolUseCount", "durationMs"]
