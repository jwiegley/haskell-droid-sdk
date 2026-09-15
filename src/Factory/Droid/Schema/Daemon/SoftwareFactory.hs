{-# LANGUAGE OverloadedStrings #-}

-- | Software Factory operation contracts. Entry projections exclude internal
-- payload/worker fields; explicit public content and outer extensions are sensitive.
module Factory.Droid.Schema.Daemon.SoftwareFactory
  ( SfWorkstreamState (..),
    SfSignalStatus (..),
    SfChangeStatus (..),
    SfReviewDecision (..),
    SfActivityKind (..),
    SfActivityStatus (..),
    SfReviewStatus (..),
    SfEventSeverity (..),
    SfEventStage (..),
    SfAccessMode (..),
    SfWorkstream (..),
    SfSignal (..),
    SfChange (..),
    SfActivity (..),
    SfEvent (..),
    SfListWorkstreamsParams (..),
    SfWorkstreamTarget (..),
    SfCreateWorkstreamParams (..),
    defaultSfCreateWorkstreamParams,
    SfUpdateWorkstreamParams (..),
    defaultSfUpdateWorkstreamParams,
    SfListParams (..),
    defaultSfListParams,
    SfListSignalsParams,
    SfListChangesParams,
    SfListActivitiesParams (..),
    defaultSfListActivitiesParams,
    SfResolveActivityReviewParams (..),
    SfListEventsParams (..),
    defaultSfListEventsParams,
    SfMarkEventsReadParams (..),
    SfMarkEventsUnreadParams (..),
    SfListWorkstreamsResult (..),
    SfGetWorkstreamResult (..),
    SfWorkstreamResult (..),
    SfDeleteWorkstreamResult (..),
    SfListSignalsResult (..),
    SfListChangesResult (..),
    SfListActivitiesResult (..),
    SfResolveActivityReviewResult (..),
    SfListEventsResult (..),
    SfMarkedEventsResult (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)

data SfWorkstreamState = SfDraft | SfActive | SfPaused | SfCanceled | SfArchived deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfWorkstreamState where
  parseJSON = withText "SfWorkstreamState" $ \case "draft" -> pure SfDraft; "active" -> pure SfActive; "paused" -> pure SfPaused; "canceled" -> pure SfCanceled; "archived" -> pure SfArchived; _ -> fail "Unknown workstream state"

instance ToJSON SfWorkstreamState where
  toJSON = String . \case SfDraft -> "draft"; SfActive -> "active"; SfPaused -> "paused"; SfCanceled -> "canceled"; SfArchived -> "archived"

data SfSignalStatus = SfSignalNew | SfSignalTriaged | SfSignalDuplicate | SfSignalIgnored deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfSignalStatus where
  parseJSON = withText "SfSignalStatus" $ \case "new" -> pure SfSignalNew; "triaged" -> pure SfSignalTriaged; "duplicate" -> pure SfSignalDuplicate; "ignored" -> pure SfSignalIgnored; _ -> fail "Unknown signal status"

instance ToJSON SfSignalStatus where
  toJSON = String . \case SfSignalNew -> "new"; SfSignalTriaged -> "triaged"; SfSignalDuplicate -> "duplicate"; SfSignalIgnored -> "ignored"

data SfChangeStatus = SfChangePending | SfChangeInProgress | SfChangeInReview | SfChangeImplemented | SfChangeCompleted | SfChangeFailed | SfChangeSkipped | SfChangeCanceled | SfChangeDeclined deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfChangeStatus where
  parseJSON = withText "SfChangeStatus" $ \case
    "pending" -> pure SfChangePending
    "in_progress" -> pure SfChangeInProgress
    "in_review" -> pure SfChangeInReview
    "implemented" -> pure SfChangeImplemented
    "completed" -> pure SfChangeCompleted
    "failed" -> pure SfChangeFailed
    "skipped" -> pure SfChangeSkipped
    "canceled" -> pure SfChangeCanceled
    "declined" -> pure SfChangeDeclined
    _ -> fail "Unknown change status"

instance ToJSON SfChangeStatus where
  toJSON =
    String . \case
      SfChangePending -> "pending"
      SfChangeInProgress -> "in_progress"
      SfChangeInReview -> "in_review"
      SfChangeImplemented -> "implemented"
      SfChangeCompleted -> "completed"
      SfChangeFailed -> "failed"
      SfChangeSkipped -> "skipped"
      SfChangeCanceled -> "canceled"
      SfChangeDeclined -> "declined"

data SfReviewDecision = SfApprove | SfDecline | SfComment deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfReviewDecision where
  parseJSON = withText "SfReviewDecision" $ \case "approved" -> pure SfApprove; "declined" -> pure SfDecline; "commented" -> pure SfComment; _ -> fail "Unknown review decision"

instance ToJSON SfReviewDecision where
  toJSON = String . \case SfApprove -> "approved"; SfDecline -> "declined"; SfComment -> "commented"

-- | change_review is supplied by the newer schema, not invented negotiation.
data SfActivityKind = SfChangeReview | SfInvestigation | SfImplementation | SfSteward deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfActivityKind where
  parseJSON = withText "SfActivityKind" $ \case "change_review" -> pure SfChangeReview; "investigation" -> pure SfInvestigation; "implementation" -> pure SfImplementation; "steward" -> pure SfSteward; _ -> fail "Unknown activity kind"

instance ToJSON SfActivityKind where
  toJSON = String . \case SfChangeReview -> "change_review"; SfInvestigation -> "investigation"; SfImplementation -> "implementation"; SfSteward -> "steward"

data SfActivityStatus = SfActivityPending | SfActivityInProgress | SfActivityCompleted | SfActivityFailed | SfActivityCanceled deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfActivityStatus where
  parseJSON = withText "SfActivityStatus" $ \case "pending" -> pure SfActivityPending; "in_progress" -> pure SfActivityInProgress; "completed" -> pure SfActivityCompleted; "failed" -> pure SfActivityFailed; "canceled" -> pure SfActivityCanceled; _ -> fail "Unknown activity status"

instance ToJSON SfActivityStatus where
  toJSON = String . \case SfActivityPending -> "pending"; SfActivityInProgress -> "in_progress"; SfActivityCompleted -> "completed"; SfActivityFailed -> "failed"; SfActivityCanceled -> "canceled"

data SfReviewStatus = SfAwaitingReview | SfApproved | SfDeclined | SfCommented deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfReviewStatus where
  parseJSON = withText "SfReviewStatus" $ \case "awaiting" -> pure SfAwaitingReview; "approved" -> pure SfApproved; "declined" -> pure SfDeclined; "commented" -> pure SfCommented; _ -> fail "Unknown review status"

instance ToJSON SfReviewStatus where
  toJSON = String . \case SfAwaitingReview -> "awaiting"; SfApproved -> "approved"; SfDeclined -> "declined"; SfCommented -> "commented"

data SfEventSeverity = SfInfo | SfWarning | SfError deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfEventSeverity where
  parseJSON = withText "SfEventSeverity" $ \case "info" -> pure SfInfo; "warning" -> pure SfWarning; "error" -> pure SfError; _ -> fail "Unknown event severity"

instance ToJSON SfEventSeverity where
  toJSON = String . \case SfInfo -> "info"; SfWarning -> "warning"; SfError -> "error"

data SfEventStage = SfIntake | SfTriage | SfWorker | SfHealth | SfSystem deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfEventStage where
  parseJSON = withText "SfEventStage" $ \case "intake" -> pure SfIntake; "triage" -> pure SfTriage; "worker" -> pure SfWorker; "health" -> pure SfHealth; "system" -> pure SfSystem; _ -> fail "Unknown event stage"

instance ToJSON SfEventStage where
  toJSON = String . \case SfIntake -> "intake"; SfTriage -> "triage"; SfWorker -> "worker"; SfHealth -> "health"; SfSystem -> "system"

data SfAccessMode = SfTeam | SfPrivate deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SfAccessMode where
  parseJSON = withText "SfAccessMode" $ \case "team" -> pure SfTeam; "private" -> pure SfPrivate; _ -> fail "Unknown workstream access mode"

instance ToJSON SfAccessMode where
  toJSON SfTeam = String "team"
  toJSON SfPrivate = String "private"

-- Public entries project declared fields; internal storage rows are not exported.
-- Reported concurrency/ports are integers, while mutation inputs below are numbers.
data SfWorkstream = SfWorkstream
  { sfWorkstreamId :: !Text,
    sfWorkstreamSlug :: !Text,
    sfWorkstreamTitle :: !Text,
    sfWorkstreamGoal :: !Text,
    sfWorkstreamState :: !SfWorkstreamState,
    sfWorkstreamUserId :: !(Maybe Text),
    sfWorkstreamCreationSessionId :: !(Maybe Text),
    sfWorkstreamMachineId :: !(Maybe Text),
    sfWorkstreamConnectors :: ![Value],
    sfWorkstreamRepos :: ![Text],
    sfWorkstreamWorkerConcurrency :: !Integer,
    sfWorkstreamStewardConcurrency :: !(Maybe Integer),
    sfWorkstreamDashboardPort :: !(Maybe Integer),
    sfWorkstreamRequireApproval :: !Bool,
    sfWorkstreamReviewers :: ![Text],
    sfWorkstreamCreatedAt :: !Text,
    sfWorkstreamUpdatedAt :: !Text,
    sfWorkstreamDeletedAt :: !(Maybe Text),
    sfWorkstreamAccessMode :: !(Maybe SfAccessMode),
    sfWorkstreamAutoMerge :: !(Maybe Bool),
    sfWorkstreamDashboardHtml :: !(Maybe Text),
    sfWorkstreamDashboardUrl :: !(Maybe Text),
    sfWorkstreamExecutionTemplateId :: !(Maybe Text),
    sfWorkstreamIcon :: !(Maybe Text),
    sfWorkstreamOwningTeamId :: !(Maybe Text),
    sfWorkstreamRequireChangeApproval :: !(Maybe Bool)
  }
  deriving stock (Eq)

instance Show SfWorkstream where show _ = "SfWorkstream <redacted>"

instance FromJSON SfWorkstream where
  parseJSON = withObject "SfWorkstream" $ \fields -> SfWorkstream <$> fields .: "id" <*> fields .: "slug" <*> fields .: "title" <*> fields .: "goal" <*> fields .: "state" <*> fields .:! "userId" <*> fields .:! "creationSessionId" <*> fields .:! "machineId" <*> fields .: "connectors" <*> fields .: "repos" <*> fields .: "workerConcurrency" <*> fields .:! "stewardConcurrency" <*> fields .:! "dashboardPort" <*> fields .: "requireApproval" <*> fields .: "reviewers" <*> fields .: "createdAt" <*> fields .: "updatedAt" <*> fields .:! "deletedAt" <*> fields .:! "accessMode" <*> fields .:! "autoMerge" <*> fields .:! "dashboardHtml" <*> fields .:! "dashboardUrl" <*> fields .:! "executionTemplateId" <*> fields .:! "icon" <*> fields .:! "owningTeamId" <*> fields .:! "requireChangeApproval"

instance ToJSON SfWorkstream where
  toJSON value = object (["id" .= sfWorkstreamId value, "slug" .= sfWorkstreamSlug value, "title" .= sfWorkstreamTitle value, "goal" .= sfWorkstreamGoal value, "state" .= sfWorkstreamState value, "connectors" .= sfWorkstreamConnectors value, "repos" .= sfWorkstreamRepos value, "workerConcurrency" .= sfWorkstreamWorkerConcurrency value, "requireApproval" .= sfWorkstreamRequireApproval value, "reviewers" .= sfWorkstreamReviewers value, "createdAt" .= sfWorkstreamCreatedAt value, "updatedAt" .= sfWorkstreamUpdatedAt value] <> optionalField "userId" (sfWorkstreamUserId value) <> optionalField "creationSessionId" (sfWorkstreamCreationSessionId value) <> optionalField "machineId" (sfWorkstreamMachineId value) <> optionalField "stewardConcurrency" (sfWorkstreamStewardConcurrency value) <> optionalField "dashboardPort" (sfWorkstreamDashboardPort value) <> optionalField "deletedAt" (sfWorkstreamDeletedAt value) <> optionalField "accessMode" (sfWorkstreamAccessMode value) <> optionalField "autoMerge" (sfWorkstreamAutoMerge value) <> optionalField "dashboardHtml" (sfWorkstreamDashboardHtml value) <> optionalField "dashboardUrl" (sfWorkstreamDashboardUrl value) <> optionalField "executionTemplateId" (sfWorkstreamExecutionTemplateId value) <> optionalField "icon" (sfWorkstreamIcon value) <> optionalField "owningTeamId" (sfWorkstreamOwningTeamId value) <> optionalField "requireChangeApproval" (sfWorkstreamRequireChangeApproval value))

data SfSignal = SfSignal
  { sfSignalId :: !Text,
    sfSignalWorkstreamId :: !Text,
    sfSignalSource :: !Text,
    sfSignalExternalId :: !(Maybe Text),
    sfSignalFingerprint :: !Text,
    sfSignalTitle :: !Text,
    sfSignalSummary :: !Text,
    sfSignalStatus :: !SfSignalStatus,
    sfSignalChangeId :: !(Maybe Text),
    sfSignalCreatedAt :: !Text,
    sfSignalUpdatedAt :: !Text
  }
  deriving stock (Eq)

instance Show SfSignal where show _ = "SfSignal <redacted>"

instance FromJSON SfSignal where
  parseJSON = withObject "SfSignal" $ \fields -> SfSignal <$> fields .: "id" <*> fields .: "workstreamId" <*> fields .: "source" <*> fields .:! "externalId" <*> fields .: "fingerprint" <*> fields .: "title" <*> fields .: "summary" <*> fields .: "status" <*> fields .:! "changeId" <*> fields .: "createdAt" <*> fields .: "updatedAt"

instance ToJSON SfSignal where
  toJSON value = object (["id" .= sfSignalId value, "workstreamId" .= sfSignalWorkstreamId value, "source" .= sfSignalSource value, "fingerprint" .= sfSignalFingerprint value, "title" .= sfSignalTitle value, "summary" .= sfSignalSummary value, "status" .= sfSignalStatus value, "createdAt" .= sfSignalCreatedAt value, "updatedAt" .= sfSignalUpdatedAt value] <> optionalField "externalId" (sfSignalExternalId value) <> optionalField "changeId" (sfSignalChangeId value))

data SfChange = SfChange
  { sfChangeId :: !Text,
    sfChangeWorkstreamId :: !Text,
    sfChangeTitle :: !Text,
    sfChangeDescription :: !Text,
    sfChangeStatus :: !SfChangeStatus,
    sfChangeResult :: !(Maybe Text),
    sfChangeError :: !(Maybe Text),
    sfChangeCompletedAt :: !(Maybe Text),
    sfChangeCreatedAt :: !Text,
    sfChangeUpdatedAt :: !Text
  }
  deriving stock (Eq)

instance Show SfChange where show _ = "SfChange <redacted>"

instance FromJSON SfChange where
  parseJSON = withObject "SfChange" $ \fields -> SfChange <$> fields .: "id" <*> fields .: "workstreamId" <*> fields .: "title" <*> fields .: "description" <*> fields .: "status" <*> fields .:! "result" <*> fields .:! "error" <*> fields .:! "completedAt" <*> fields .: "createdAt" <*> fields .: "updatedAt"

instance ToJSON SfChange where
  toJSON value = object (["id" .= sfChangeId value, "workstreamId" .= sfChangeWorkstreamId value, "title" .= sfChangeTitle value, "description" .= sfChangeDescription value, "status" .= sfChangeStatus value, "createdAt" .= sfChangeCreatedAt value, "updatedAt" .= sfChangeUpdatedAt value] <> optionalField "result" (sfChangeResult value) <> optionalField "error" (sfChangeError value) <> optionalField "completedAt" (sfChangeCompletedAt value))

data SfActivity = SfActivity
  { sfActivityId :: !Text,
    sfActivityChangeId :: !Text,
    sfActivityWorkstreamId :: !Text,
    sfActivityKind :: !SfActivityKind,
    sfActivityStatus :: !SfActivityStatus,
    sfActivityReviewStatus :: !(Maybe SfReviewStatus),
    sfActivityReviewComment :: !(Maybe Text),
    sfActivityReviewedAt :: !(Maybe Text),
    sfActivityResult :: !(Maybe Text),
    sfActivityError :: !(Maybe Text),
    sfActivitySessionId :: !(Maybe Text),
    sfActivityParentId :: !(Maybe Text),
    sfActivityCompletedAt :: !(Maybe Text),
    sfActivityCreatedAt :: !Text,
    sfActivityUpdatedAt :: !Text,
    sfActivityChangeTitle :: !Text,
    sfActivityChangeDescription :: !Text,
    sfActivityReviewerId :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show SfActivity where show _ = "SfActivity <redacted>"

instance FromJSON SfActivity where
  parseJSON = withObject "SfActivity" $ \fields -> SfActivity <$> fields .: "id" <*> fields .: "changeId" <*> fields .: "workstreamId" <*> fields .: "kind" <*> fields .: "status" <*> fields .:! "reviewStatus" <*> fields .:! "reviewComment" <*> fields .:! "reviewedAt" <*> fields .:! "result" <*> fields .:! "error" <*> fields .:! "sessionId" <*> fields .:! "parentActivityId" <*> fields .:! "completedAt" <*> fields .: "createdAt" <*> fields .: "updatedAt" <*> fields .: "changeTitle" <*> fields .: "changeDescription" <*> fields .:! "reviewerId"

instance ToJSON SfActivity where
  toJSON value = object (["id" .= sfActivityId value, "changeId" .= sfActivityChangeId value, "workstreamId" .= sfActivityWorkstreamId value, "kind" .= sfActivityKind value, "status" .= sfActivityStatus value, "createdAt" .= sfActivityCreatedAt value, "updatedAt" .= sfActivityUpdatedAt value, "changeTitle" .= sfActivityChangeTitle value, "changeDescription" .= sfActivityChangeDescription value] <> optionalField "reviewStatus" (sfActivityReviewStatus value) <> optionalField "reviewComment" (sfActivityReviewComment value) <> optionalField "reviewedAt" (sfActivityReviewedAt value) <> optionalField "result" (sfActivityResult value) <> optionalField "error" (sfActivityError value) <> optionalField "sessionId" (sfActivitySessionId value) <> optionalField "parentActivityId" (sfActivityParentId value) <> optionalField "completedAt" (sfActivityCompletedAt value) <> optionalField "reviewerId" (sfActivityReviewerId value))

data SfEvent = SfEvent
  { sfEventId :: !Text,
    sfEventWorkstreamId :: !Text,
    sfEventSeverity :: !SfEventSeverity,
    sfEventStage :: !SfEventStage,
    sfEventTitle :: !Text,
    sfEventDetail :: !(Maybe Text),
    sfEventRead :: !Bool,
    sfEventCreatedAt :: !Text
  }
  deriving stock (Eq)

instance Show SfEvent where show _ = "SfEvent <redacted>"

instance FromJSON SfEvent where
  parseJSON = withObject "SfEvent" $ \fields -> SfEvent <$> fields .: "id" <*> fields .: "workstreamId" <*> fields .: "severity" <*> fields .: "stage" <*> fields .: "title" <*> fields .:! "detail" <*> fields .: "read" <*> fields .: "createdAt"

instance ToJSON SfEvent where
  toJSON value = object (["id" .= sfEventId value, "workstreamId" .= sfEventWorkstreamId value, "severity" .= sfEventSeverity value, "stage" .= sfEventStage value, "title" .= sfEventTitle value, "read" .= sfEventRead value, "createdAt" .= sfEventCreatedAt value] <> optionalField "detail" (sfEventDetail value))

data SfListWorkstreamsParams = SfListWorkstreamsParams {sfListWorkstreamsState :: !(Maybe SfWorkstreamState), sfListWorkstreamsAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfListWorkstreamsParams where show _ = "SfListWorkstreamsParams <redacted>"

instance FromJSON SfListWorkstreamsParams where parseJSON = withObject "SfListWorkstreamsParams" $ \fields -> SfListWorkstreamsParams <$> fields .:! "state" <*> pure (additionalFields ["state"] fields)

instance ToJSON SfListWorkstreamsParams where toJSON value = objectWithAdditionalFields ["state"] (sfListWorkstreamsAdditionalFields value) (optionalField "state" (sfListWorkstreamsState value))

-- | Shared get/delete addressing. It is opaque and need not be nonempty.
data SfWorkstreamTarget = SfWorkstreamTarget {sfWorkstreamIdOrSlug :: !Text, sfWorkstreamTargetAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfWorkstreamTarget where show _ = "SfWorkstreamTarget <redacted>"

instance FromJSON SfWorkstreamTarget where parseJSON = withObject "SfWorkstreamTarget" $ \fields -> SfWorkstreamTarget <$> fields .: "idOrSlug" <*> pure (additionalFields ["idOrSlug"] fields)

instance ToJSON SfWorkstreamTarget where toJSON value = objectWithAdditionalFields ["idOrSlug"] (sfWorkstreamTargetAdditionalFields value) ["idOrSlug" .= sfWorkstreamIdOrSlug value]

data SfCreateWorkstreamParams = SfCreateWorkstreamParams
  { sfCreateTitle :: !Text,
    sfCreateGoal :: !Text,
    sfCreateSlug :: !(Maybe Text),
    sfCreateUserId :: !(Maybe Text),
    sfCreateCreationSessionId :: !(Maybe Text),
    sfCreateConnectors :: !(Maybe [Value]),
    sfCreateRepos :: !(Maybe [Text]),
    sfCreateWorkerConcurrency :: !(Maybe Scientific),
    sfCreateStewardConcurrency :: !(Maybe Scientific),
    sfCreateRequireApproval :: !(Maybe Bool),
    sfCreateReviewers :: !(Maybe [Text]),
    sfCreateAccessMode :: !(Maybe SfAccessMode),
    sfCreateAutoMerge :: !(Maybe Bool),
    sfCreateExecutionTemplateId :: !(Maybe Text),
    sfCreateOwningTeamId :: !(Maybe Text),
    sfCreateRequireChangeApproval :: !(Maybe Bool),
    sfCreateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfCreateWorkstreamParams where show _ = "SfCreateWorkstreamParams <redacted>"

defaultSfCreateWorkstreamParams :: Text -> Text -> SfCreateWorkstreamParams
defaultSfCreateWorkstreamParams title goal = SfCreateWorkstreamParams title goal Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON SfCreateWorkstreamParams where
  parseJSON = withObject "SfCreateWorkstreamParams" $ \fields -> SfCreateWorkstreamParams <$> fields .: "title" <*> fields .: "goal" <*> fields .:! "slug" <*> fields .:! "userId" <*> fields .:! "creationSessionId" <*> fields .:! "connectors" <*> fields .:! "repos" <*> fields .:! "workerConcurrency" <*> fields .:! "stewardConcurrency" <*> fields .:! "requireApproval" <*> fields .:! "reviewers" <*> fields .:! "accessMode" <*> fields .:! "autoMerge" <*> fields .:! "executionTemplateId" <*> fields .:! "owningTeamId" <*> fields .:! "requireChangeApproval" <*> pure (additionalFields createKeys fields)

instance ToJSON SfCreateWorkstreamParams where
  toJSON value = objectWithAdditionalFields createKeys (sfCreateAdditionalFields value) (["title" .= sfCreateTitle value, "goal" .= sfCreateGoal value] <> optionalField "slug" (sfCreateSlug value) <> optionalField "userId" (sfCreateUserId value) <> optionalField "creationSessionId" (sfCreateCreationSessionId value) <> optionalField "connectors" (sfCreateConnectors value) <> optionalField "repos" (sfCreateRepos value) <> optionalField "workerConcurrency" (sfCreateWorkerConcurrency value) <> optionalField "stewardConcurrency" (sfCreateStewardConcurrency value) <> optionalField "requireApproval" (sfCreateRequireApproval value) <> optionalField "reviewers" (sfCreateReviewers value) <> optionalField "accessMode" (sfCreateAccessMode value) <> optionalField "autoMerge" (sfCreateAutoMerge value) <> optionalField "executionTemplateId" (sfCreateExecutionTemplateId value) <> optionalField "owningTeamId" (sfCreateOwningTeamId value) <> optionalField "requireChangeApproval" (sfCreateRequireChangeApproval value))

createKeys :: [Key]
createKeys = ["title", "goal", "slug", "userId", "creationSessionId", "connectors", "repos", "workerConcurrency", "stewardConcurrency", "requireApproval", "reviewers", "accessMode", "autoMerge", "executionTemplateId", "owningTeamId", "requireChangeApproval"]

-- | userId is retained on the wire but the reference store ignores ownership
-- updates. Nothing omits icon; Just Nothing explicitly clears it.
data SfUpdateWorkstreamParams = SfUpdateWorkstreamParams
  { sfUpdateIdOrSlug :: !Text,
    sfUpdateState :: !(Maybe SfWorkstreamState),
    sfUpdateTitle :: !(Maybe Text),
    sfUpdateGoal :: !(Maybe Text),
    sfUpdateUserId :: !(Maybe Text),
    sfUpdateCreationSessionId :: !(Maybe Text),
    sfUpdateConnectors :: !(Maybe [Value]),
    sfUpdateRepos :: !(Maybe [Text]),
    sfUpdateWorkerConcurrency :: !(Maybe Scientific),
    sfUpdateStewardConcurrency :: !(Maybe Scientific),
    sfUpdateDashboardPort :: !(Maybe Scientific),
    sfUpdateRequireApproval :: !(Maybe Bool),
    sfUpdateReviewers :: !(Maybe [Text]),
    sfUpdateAutoMerge :: !(Maybe Bool),
    sfUpdateDashboardHtml :: !(Maybe Text),
    sfUpdateDashboardUrl :: !(Maybe Text),
    sfUpdateIcon :: !(Maybe (Maybe Text)),
    sfUpdateRequireChangeApproval :: !(Maybe Bool),
    sfUpdateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfUpdateWorkstreamParams where show _ = "SfUpdateWorkstreamParams <redacted>"

defaultSfUpdateWorkstreamParams :: Text -> SfUpdateWorkstreamParams
defaultSfUpdateWorkstreamParams identifier = SfUpdateWorkstreamParams identifier Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON SfUpdateWorkstreamParams where
  parseJSON = withObject "SfUpdateWorkstreamParams" $ \fields -> SfUpdateWorkstreamParams <$> fields .: "idOrSlug" <*> fields .:! "state" <*> fields .:! "title" <*> fields .:! "goal" <*> fields .:! "userId" <*> fields .:! "creationSessionId" <*> fields .:! "connectors" <*> fields .:! "repos" <*> fields .:! "workerConcurrency" <*> fields .:! "stewardConcurrency" <*> fields .:! "dashboardPort" <*> fields .:! "requireApproval" <*> fields .:! "reviewers" <*> fields .:! "autoMerge" <*> fields .:! "dashboardHtml" <*> fields .:! "dashboardUrl" <*> fields .:! "icon" <*> fields .:! "requireChangeApproval" <*> pure (additionalFields updateKeys fields)

instance ToJSON SfUpdateWorkstreamParams where
  toJSON value = objectWithAdditionalFields updateKeys (sfUpdateAdditionalFields value) (["idOrSlug" .= sfUpdateIdOrSlug value] <> optionalField "state" (sfUpdateState value) <> optionalField "title" (sfUpdateTitle value) <> optionalField "goal" (sfUpdateGoal value) <> optionalField "userId" (sfUpdateUserId value) <> optionalField "creationSessionId" (sfUpdateCreationSessionId value) <> optionalField "connectors" (sfUpdateConnectors value) <> optionalField "repos" (sfUpdateRepos value) <> optionalField "workerConcurrency" (sfUpdateWorkerConcurrency value) <> optionalField "stewardConcurrency" (sfUpdateStewardConcurrency value) <> optionalField "dashboardPort" (sfUpdateDashboardPort value) <> optionalField "requireApproval" (sfUpdateRequireApproval value) <> optionalField "reviewers" (sfUpdateReviewers value) <> optionalField "autoMerge" (sfUpdateAutoMerge value) <> optionalField "dashboardHtml" (sfUpdateDashboardHtml value) <> optionalField "dashboardUrl" (sfUpdateDashboardUrl value) <> optionalField "icon" (sfUpdateIcon value) <> optionalField "requireChangeApproval" (sfUpdateRequireChangeApproval value))

updateKeys :: [Key]
updateKeys = ["idOrSlug", "state", "title", "goal", "userId", "creationSessionId", "connectors", "repos", "workerConcurrency", "stewardConcurrency", "dashboardPort", "requireApproval", "reviewers", "autoMerge", "dashboardHtml", "dashboardUrl", "icon", "requireChangeApproval"]

-- | Signals and changes have the identical list shape, differing only in status.
data SfListParams status = SfListParams
  { sfListWorkstreamIdOrSlug :: !(Maybe Text),
    sfListStatus :: !(Maybe status),
    sfListLimit :: !(Maybe Scientific),
    sfListAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show (SfListParams status) where show _ = "SfListParams <redacted>"

defaultSfListParams :: SfListParams status
defaultSfListParams = SfListParams Nothing Nothing Nothing mempty

type SfListSignalsParams = SfListParams SfSignalStatus

type SfListChangesParams = SfListParams SfChangeStatus

instance (FromJSON status) => FromJSON (SfListParams status) where
  parseJSON = withObject "SfListParams" $ \fields -> SfListParams <$> fields .:! "workstreamIdOrSlug" <*> fields .:! "status" <*> fields .:! "limit" <*> pure (additionalFields ["workstreamIdOrSlug", "status", "limit"] fields)

instance (ToJSON status) => ToJSON (SfListParams status) where
  toJSON value = objectWithAdditionalFields ["workstreamIdOrSlug", "status", "limit"] (sfListAdditionalFields value) (optionalField "workstreamIdOrSlug" (sfListWorkstreamIdOrSlug value) <> optionalField "status" (sfListStatus value) <> optionalField "limit" (sfListLimit value))

data SfListActivitiesParams = SfListActivitiesParams
  { sfActivitiesWorkstreamIdOrSlug :: !(Maybe Text),
    sfActivitiesChangeId :: !(Maybe Text),
    sfActivitiesAwaitingReviewOnly :: !(Maybe Bool),
    sfActivitiesLimit :: !(Maybe Scientific),
    sfActivitiesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfListActivitiesParams where show _ = "SfListActivitiesParams <redacted>"

defaultSfListActivitiesParams :: SfListActivitiesParams
defaultSfListActivitiesParams = SfListActivitiesParams Nothing Nothing Nothing Nothing mempty

instance FromJSON SfListActivitiesParams where
  parseJSON = withObject "SfListActivitiesParams" $ \fields -> SfListActivitiesParams <$> fields .:! "workstreamIdOrSlug" <*> fields .:! "changeId" <*> fields .:! "awaitingReviewOnly" <*> fields .:! "limit" <*> pure (additionalFields activitiesKeys fields)

instance ToJSON SfListActivitiesParams where
  toJSON value = objectWithAdditionalFields activitiesKeys (sfActivitiesAdditionalFields value) (optionalField "workstreamIdOrSlug" (sfActivitiesWorkstreamIdOrSlug value) <> optionalField "changeId" (sfActivitiesChangeId value) <> optionalField "awaitingReviewOnly" (sfActivitiesAwaitingReviewOnly value) <> optionalField "limit" (sfActivitiesLimit value))

activitiesKeys :: [Key]
activitiesKeys = ["workstreamIdOrSlug", "changeId", "awaitingReviewOnly", "limit"]

data SfResolveActivityReviewParams = SfResolveActivityReviewParams
  { sfReviewActivityId :: !Text,
    sfReviewDecision :: !SfReviewDecision,
    sfReviewComment :: !(Maybe Text),
    sfReviewAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfResolveActivityReviewParams where show _ = "SfResolveActivityReviewParams <redacted>"

instance FromJSON SfResolveActivityReviewParams where
  parseJSON = withObject "SfResolveActivityReviewParams" $ \fields -> SfResolveActivityReviewParams <$> fields .: "activityId" <*> fields .: "decision" <*> fields .:! "comment" <*> pure (additionalFields ["activityId", "decision", "comment"] fields)

instance ToJSON SfResolveActivityReviewParams where
  toJSON value = objectWithAdditionalFields ["activityId", "decision", "comment"] (sfReviewAdditionalFields value) (["activityId" .= sfReviewActivityId value, "decision" .= sfReviewDecision value] <> optionalField "comment" (sfReviewComment value))

data SfListEventsParams = SfListEventsParams
  { sfEventsWorkstreamIdOrSlug :: !(Maybe Text),
    sfEventsUnreadOnly :: !(Maybe Bool),
    sfEventsLimit :: !(Maybe Scientific),
    sfEventsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfListEventsParams where show _ = "SfListEventsParams <redacted>"

defaultSfListEventsParams :: SfListEventsParams
defaultSfListEventsParams = SfListEventsParams Nothing Nothing Nothing mempty

instance FromJSON SfListEventsParams where
  parseJSON = withObject "SfListEventsParams" $ \fields -> SfListEventsParams <$> fields .:! "workstreamIdOrSlug" <*> fields .:! "unreadOnly" <*> fields .:! "limit" <*> pure (additionalFields ["workstreamIdOrSlug", "unreadOnly", "limit"] fields)

instance ToJSON SfListEventsParams where
  toJSON value = objectWithAdditionalFields ["workstreamIdOrSlug", "unreadOnly", "limit"] (sfEventsAdditionalFields value) (optionalField "workstreamIdOrSlug" (sfEventsWorkstreamIdOrSlug value) <> optionalField "unreadOnly" (sfEventsUnreadOnly value) <> optionalField "limit" (sfEventsLimit value))

data SfMarkEventsReadParams = SfMarkEventsReadParams
  { sfReadWorkstreamIdOrSlug :: !Text,
    sfReadEventIds :: !(Maybe [Text]),
    sfReadAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfMarkEventsReadParams where show _ = "SfMarkEventsReadParams <redacted>"

instance FromJSON SfMarkEventsReadParams where
  parseJSON = withObject "SfMarkEventsReadParams" $ \fields -> SfMarkEventsReadParams <$> fields .: "workstreamIdOrSlug" <*> fields .:! "eventIds" <*> pure (additionalFields ["workstreamIdOrSlug", "eventIds"] fields)

instance ToJSON SfMarkEventsReadParams where
  toJSON value = objectWithAdditionalFields ["workstreamIdOrSlug", "eventIds"] (sfReadAdditionalFields value) (["workstreamIdOrSlug" .= sfReadWorkstreamIdOrSlug value] <> optionalField "eventIds" (sfReadEventIds value))

data SfMarkEventsUnreadParams = SfMarkEventsUnreadParams
  { sfUnreadWorkstreamIdOrSlug :: !Text,
    sfUnreadEventIds :: ![Text],
    sfUnreadAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfMarkEventsUnreadParams where show _ = "SfMarkEventsUnreadParams <redacted>"

instance FromJSON SfMarkEventsUnreadParams where
  parseJSON = withObject "SfMarkEventsUnreadParams" $ \fields -> SfMarkEventsUnreadParams <$> fields .: "workstreamIdOrSlug" <*> fields .: "eventIds" <*> pure (additionalFields ["workstreamIdOrSlug", "eventIds"] fields)

instance ToJSON SfMarkEventsUnreadParams where
  toJSON value = objectWithAdditionalFields ["workstreamIdOrSlug", "eventIds"] (sfUnreadAdditionalFields value) ["workstreamIdOrSlug" .= sfUnreadWorkstreamIdOrSlug value, "eventIds" .= sfUnreadEventIds value]

data SfListWorkstreamsResult = SfListWorkstreamsResult {sfWorkstreams :: ![SfWorkstream], sfWorkstreamsResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfListWorkstreamsResult where show _ = "SfListWorkstreamsResult <redacted>"

instance FromJSON SfListWorkstreamsResult where parseJSON = withObject "SfListWorkstreamsResult" $ \fields -> SfListWorkstreamsResult <$> fields .: "workstreams" <*> pure (additionalFields ["workstreams"] fields)

instance ToJSON SfListWorkstreamsResult where toJSON value = objectWithAdditionalFields ["workstreams"] (sfWorkstreamsResultAdditionalFields value) ["workstreams" .= sfWorkstreams value]

-- | No match omits the key. Explicit null is invalid, unlike cron update replies.
data SfGetWorkstreamResult = SfGetWorkstreamResult {sfFoundWorkstream :: !(Maybe SfWorkstream), sfGetResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfGetWorkstreamResult where show _ = "SfGetWorkstreamResult <redacted>"

instance FromJSON SfGetWorkstreamResult where parseJSON = withObject "SfGetWorkstreamResult" $ \fields -> SfGetWorkstreamResult <$> fields .:! "workstream" <*> pure (additionalFields ["workstream"] fields)

instance ToJSON SfGetWorkstreamResult where toJSON value = objectWithAdditionalFields ["workstream"] (sfGetResultAdditionalFields value) (optionalField "workstream" (sfFoundWorkstream value))

-- | Create and update return the same required workstream shape.
data SfWorkstreamResult = SfWorkstreamResult {sfReturnedWorkstream :: !SfWorkstream, sfWorkstreamResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfWorkstreamResult where show _ = "SfWorkstreamResult <redacted>"

instance FromJSON SfWorkstreamResult where parseJSON = withObject "SfWorkstreamResult" $ \fields -> SfWorkstreamResult <$> fields .: "workstream" <*> pure (additionalFields ["workstream"] fields)

instance ToJSON SfWorkstreamResult where toJSON value = objectWithAdditionalFields ["workstream"] (sfWorkstreamResultAdditionalFields value) ["workstream" .= sfReturnedWorkstream value]

data SfDeleteWorkstreamResult = SfDeleteWorkstreamResult
  { sfDeletedWorkstream :: !SfWorkstream,
    sfAutomationsDeleted :: !Scientific,
    sfWorkstreamDirDeleted :: !Bool,
    sfRemoteAutomationsDeleted :: !(Maybe Scientific),
    sfDeleteResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfDeleteWorkstreamResult where show _ = "SfDeleteWorkstreamResult <redacted>"

instance FromJSON SfDeleteWorkstreamResult where
  parseJSON = withObject "SfDeleteWorkstreamResult" $ \fields -> SfDeleteWorkstreamResult <$> fields .: "workstream" <*> fields .: "automationsDeleted" <*> fields .: "workstreamDirDeleted" <*> fields .:! "remoteAutomationsDeleted" <*> pure (additionalFields ["workstream", "automationsDeleted", "workstreamDirDeleted", "remoteAutomationsDeleted"] fields)

instance ToJSON SfDeleteWorkstreamResult where
  toJSON value = objectWithAdditionalFields ["workstream", "automationsDeleted", "workstreamDirDeleted", "remoteAutomationsDeleted"] (sfDeleteResultAdditionalFields value) (["workstream" .= sfDeletedWorkstream value, "automationsDeleted" .= sfAutomationsDeleted value, "workstreamDirDeleted" .= sfWorkstreamDirDeleted value] <> optionalField "remoteAutomationsDeleted" (sfRemoteAutomationsDeleted value))

data SfListSignalsResult = SfListSignalsResult {sfSignals :: ![SfSignal], sfSignalsResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfListSignalsResult where show _ = "SfListSignalsResult <redacted>"

instance FromJSON SfListSignalsResult where parseJSON = withObject "SfListSignalsResult" $ \fields -> SfListSignalsResult <$> fields .: "signals" <*> pure (additionalFields ["signals"] fields)

instance ToJSON SfListSignalsResult where toJSON value = objectWithAdditionalFields ["signals"] (sfSignalsResultAdditionalFields value) ["signals" .= sfSignals value]

data SfListChangesResult = SfListChangesResult {sfChanges :: ![SfChange], sfChangesResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfListChangesResult where show _ = "SfListChangesResult <redacted>"

instance FromJSON SfListChangesResult where parseJSON = withObject "SfListChangesResult" $ \fields -> SfListChangesResult <$> fields .: "changes" <*> pure (additionalFields ["changes"] fields)

instance ToJSON SfListChangesResult where toJSON value = objectWithAdditionalFields ["changes"] (sfChangesResultAdditionalFields value) ["changes" .= sfChanges value]

data SfListActivitiesResult = SfListActivitiesResult {sfActivities :: ![SfActivity], sfActivitiesResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfListActivitiesResult where show _ = "SfListActivitiesResult <redacted>"

instance FromJSON SfListActivitiesResult where parseJSON = withObject "SfListActivitiesResult" $ \fields -> SfListActivitiesResult <$> fields .: "activities" <*> pure (additionalFields ["activities"] fields)

instance ToJSON SfListActivitiesResult where toJSON value = objectWithAdditionalFields ["activities"] (sfActivitiesResultAdditionalFields value) ["activities" .= sfActivities value]

data SfResolveActivityReviewResult = SfResolveActivityReviewResult
  { sfReviewedActivity :: !SfActivity,
    sfFollowUpActivity :: !(Maybe SfActivity),
    sfReviewResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SfResolveActivityReviewResult where show _ = "SfResolveActivityReviewResult <redacted>"

instance FromJSON SfResolveActivityReviewResult where
  parseJSON = withObject "SfResolveActivityReviewResult" $ \fields -> SfResolveActivityReviewResult <$> fields .: "activity" <*> fields .:! "followUpActivity" <*> pure (additionalFields ["activity", "followUpActivity"] fields)

instance ToJSON SfResolveActivityReviewResult where
  toJSON value = objectWithAdditionalFields ["activity", "followUpActivity"] (sfReviewResultAdditionalFields value) (["activity" .= sfReviewedActivity value] <> optionalField "followUpActivity" (sfFollowUpActivity value))

data SfListEventsResult = SfListEventsResult {sfEvents :: ![SfEvent], sfEventsResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfListEventsResult where show _ = "SfListEventsResult <redacted>"

instance FromJSON SfListEventsResult where parseJSON = withObject "SfListEventsResult" $ \fields -> SfListEventsResult <$> fields .: "events" <*> pure (additionalFields ["events"] fields)

instance ToJSON SfListEventsResult where toJSON value = objectWithAdditionalFields ["events"] (sfEventsResultAdditionalFields value) ["events" .= sfEvents value]

data SfMarkedEventsResult = SfMarkedEventsResult {sfMarkedEvents :: !Scientific, sfMarkedResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show SfMarkedEventsResult where show _ = "SfMarkedEventsResult <redacted>"

instance FromJSON SfMarkedEventsResult where parseJSON = withObject "SfMarkedEventsResult" $ \fields -> SfMarkedEventsResult <$> fields .: "marked" <*> pure (additionalFields ["marked"] fields)

instance ToJSON SfMarkedEventsResult where toJSON value = objectWithAdditionalFields ["marked"] (sfMarkedResultAdditionalFields value) ["marked" .= sfMarkedEvents value]
