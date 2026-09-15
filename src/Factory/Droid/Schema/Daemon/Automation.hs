{-# LANGUAGE OverloadedStrings #-}

-- | Daemon automation lifecycle/configuration data. No scheduling, writes, rendering
-- or agent execution occurs in these codecs; explicit fields/JSON are sensitive.
module Factory.Droid.Schema.Daemon.Automation
  ( AutomationTemplate (..),
    AutomationRunType (..),
    AutomationCreator (..),
    AutomationEntry (..),
    AutomationPendingSetup (..),
    AutomationScaffoldFile (..),
    AutomationScaffoldSkill (..),
    AutomationListParams (..),
    AutomationAddress (..),
    defaultAutomationAddress,
    RunAutomationParams (..),
    defaultRunAutomationParams,
    AutomationHistoryParams (..),
    AutomationVisualParams (..),
    RenameAutomationParams (..),
    ListAutomationsResult (..),
    RunAutomationResult (..),
    AutomationStatusResult (..),
    AutomationRunRecord (..),
    AutomationHistoryResult (..),
    AutomationVisualResult (..),
    AutomationPrivacy (..),
    AutomationSessionPrivacy (..),
    AutomationCreationSource (..),
    AutomationConfigFailureReason (..),
    CreateAutomationParams (..),
    defaultCreateAutomationParams,
    ForkAutomationParams (..),
    UpdateAutomationModelParams (..),
    UpdateAutomationPrivacyParams (..),
    UpdateAutomationPromptParams (..),
    UpdateAutomationScheduleParams (..),
    AutomationConfiguration (..),
    defaultAutomationConfiguration,
    ApplyAutomationConfigParams (..),
    UpdateAutomationParams (..),
    AutomationCreationResult (..),
    ApplyAutomationConfigResult (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Models (ModelFallback)

data AutomationTemplate = AutomationCodeReview | AutomationQA | AutomationWiki | AutomationSecurityAudit | AutomationTriage | AutomationIncidentResponse | AutomationAutoEvals
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationTemplate where
  parseJSON = withText "AutomationTemplate" $ \case
    "code-review" -> pure AutomationCodeReview
    "qa" -> pure AutomationQA
    "wiki" -> pure AutomationWiki
    "security-audit" -> pure AutomationSecurityAudit
    "triage" -> pure AutomationTriage
    "incident-response" -> pure AutomationIncidentResponse
    "auto-evals" -> pure AutomationAutoEvals
    _ -> fail "Unknown automation template"

instance ToJSON AutomationTemplate where
  toJSON AutomationCodeReview = String "code-review"
  toJSON AutomationQA = String "qa"
  toJSON AutomationWiki = String "wiki"
  toJSON AutomationSecurityAudit = String "security-audit"
  toJSON AutomationTriage = String "triage"
  toJSON AutomationIncidentResponse = String "incident-response"
  toJSON AutomationAutoEvals = String "auto-evals"

data AutomationRunType = AutomationRunCreated | AutomationRunExecuted deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationRunType where
  parseJSON = withText "AutomationRunType" $ \case "create" -> pure AutomationRunCreated; "run" -> pure AutomationRunExecuted; _ -> fail "Unknown automation run type"

instance ToJSON AutomationRunType where
  toJSON AutomationRunCreated = String "create"
  toJSON AutomationRunExecuted = String "run"

data AutomationCreator = AutomationCreator
  { automationCreatorName :: !Text,
    automationCreatorEmail :: !(Maybe Text),
    automationCreatorAvatarUrl :: !(Maybe Text),
    automationCreatorAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationCreator where show _ = "AutomationCreator <redacted>"

instance FromJSON AutomationCreator where
  parseJSON = withObject "AutomationCreator" $ \fields -> AutomationCreator <$> fields .: "name" <*> fields .:! "email" <*> fields .:! "avatarUrl" <*> pure (additionalFields ["name", "email", "avatarUrl"] fields)

instance ToJSON AutomationCreator where
  toJSON value = objectWithAdditionalFields ["name", "email", "avatarUrl"] (automationCreatorAdditionalFields value) (["name" .= automationCreatorName value] <> optionalField "email" (automationCreatorEmail value) <> optionalField "avatarUrl" (automationCreatorAvatarUrl value))

-- | Directory ID, UUID, machine ID and computer ID are distinct reports. In
-- particular, computerId alone does not establish local versus remote ownership.
data AutomationEntry = AutomationEntry
  { automationEntryId :: !Text,
    automationEntryUuid :: !(Maybe Text),
    automationEntryName :: !Text,
    automationEntryDescription :: !(Maybe Text),
    automationEntryPrompt :: !(Maybe Text),
    automationEntryStatus :: !Text,
    automationEntrySchedule :: !(Maybe Text),
    automationEntryModel :: !(Maybe Text),
    automationEntryReasoning :: !(Maybe Text),
    automationEntryTags :: !(Maybe [Text]),
    automationEntryNextRun :: !(Maybe Text),
    automationEntryLastRun :: !(Maybe Text),
    automationEntryLastRunStatus :: !(Maybe Text),
    automationEntryValid :: !Bool,
    automationEntryPath :: !Text,
    automationEntryTemplate :: !(Maybe AutomationTemplate),
    automationEntryPrivacy :: !(Maybe Text),
    automationEntrySessionPrivacy :: !(Maybe Text),
    automationEntryCreator :: !(Maybe AutomationCreator),
    automationEntryForkedFrom :: !(Maybe Text),
    automationEntryWorkingDirectory :: !(Maybe Text),
    automationEntryComputerId :: !(Maybe Text),
    automationEntryMachineId :: !(Maybe Text),
    automationEntryHostId :: !(Maybe Text),
    automationEntryWorkstreamId :: !(Maybe Text),
    automationEntrySetupState :: !(Maybe Text),
    automationEntryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationEntry where show _ = "AutomationEntry <redacted>"

instance FromJSON AutomationEntry where
  parseJSON = withObject "AutomationEntry" $ \fields -> AutomationEntry <$> fields .: "id" <*> fields .:! "uuid" <*> fields .: "name" <*> fields .:! "description" <*> fields .:! "prompt" <*> fields .: "status" <*> fields .:! "schedule" <*> fields .:! "model" <*> fields .:! "reasoningEffort" <*> fields .:! "tags" <*> fields .:! "nextRunAt" <*> fields .:! "lastRunAt" <*> fields .:! "lastRunStatus" <*> fields .: "isValid" <*> fields .: "path" <*> fields .:! "templateId" <*> fields .:! "privacyLevel" <*> fields .:! "sessionPrivacy" <*> fields .:! "createdBy" <*> fields .:! "forkedFrom" <*> fields .:! "workingDirectory" <*> fields .:! "computerId" <*> fields .:! "machineId" <*> fields .:! "hostId" <*> fields .:! "workstreamId" <*> fields .:! "setupState" <*> pure (additionalFields entryKeys fields)

instance ToJSON AutomationEntry where
  toJSON value = objectWithAdditionalFields entryKeys (automationEntryAdditionalFields value) (["id" .= automationEntryId value, "name" .= automationEntryName value, "status" .= automationEntryStatus value, "isValid" .= automationEntryValid value, "path" .= automationEntryPath value] <> optionalField "uuid" (automationEntryUuid value) <> optionalField "description" (automationEntryDescription value) <> optionalField "prompt" (automationEntryPrompt value) <> optionalField "schedule" (automationEntrySchedule value) <> optionalField "model" (automationEntryModel value) <> optionalField "reasoningEffort" (automationEntryReasoning value) <> optionalField "tags" (automationEntryTags value) <> optionalField "nextRunAt" (automationEntryNextRun value) <> optionalField "lastRunAt" (automationEntryLastRun value) <> optionalField "lastRunStatus" (automationEntryLastRunStatus value) <> optionalField "templateId" (automationEntryTemplate value) <> optionalField "privacyLevel" (automationEntryPrivacy value) <> optionalField "sessionPrivacy" (automationEntrySessionPrivacy value) <> optionalField "createdBy" (automationEntryCreator value) <> optionalField "forkedFrom" (automationEntryForkedFrom value) <> optionalField "workingDirectory" (automationEntryWorkingDirectory value) <> optionalField "computerId" (automationEntryComputerId value) <> optionalField "machineId" (automationEntryMachineId value) <> optionalField "hostId" (automationEntryHostId value) <> optionalField "workstreamId" (automationEntryWorkstreamId value) <> optionalField "setupState" (automationEntrySetupState value))

data AutomationPendingSetup = AutomationPendingSetup
  { pendingAutomationUuid :: !Text,
    pendingAutomationSessionId :: !(Maybe Text),
    pendingAutomationState :: !Text,
    pendingAutomationStartedAt :: !(Maybe Text),
    pendingAutomationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationPendingSetup where show _ = "AutomationPendingSetup <redacted>"

instance FromJSON AutomationPendingSetup where
  parseJSON = withObject "AutomationPendingSetup" $ \fields -> AutomationPendingSetup <$> fields .: "automationUuid" <*> fields .:! "sessionId" <*> fields .: "state" <*> fields .:! "startedAt" <*> pure (additionalFields ["automationUuid", "sessionId", "state", "startedAt"] fields)

instance ToJSON AutomationPendingSetup where
  toJSON value = objectWithAdditionalFields ["automationUuid", "sessionId", "state", "startedAt"] (pendingAutomationAdditionalFields value) (["automationUuid" .= pendingAutomationUuid value, "state" .= pendingAutomationState value] <> optionalField "sessionId" (pendingAutomationSessionId value) <> optionalField "startedAt" (pendingAutomationStartedAt value))

-- | File IDs, content and fingerprints remain daemon inputs. The daemon, not
-- this codec, validates fingerprints and decides whether to write a scaffold.
data AutomationScaffoldFile = AutomationScaffoldFile
  { scaffoldFileId :: !Text,
    scaffoldFileContent :: !(Maybe Text),
    scaffoldFileFingerprint :: !Text,
    scaffoldFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationScaffoldFile where show _ = "AutomationScaffoldFile <redacted>"

instance FromJSON AutomationScaffoldFile where
  parseJSON = withObject "AutomationScaffoldFile" $ \fields -> AutomationScaffoldFile <$> fields .: "fileId" <*> fields .:! "content" <*> fields .: "fingerprint" <*> pure (additionalFields ["fileId", "content", "fingerprint"] fields)

instance ToJSON AutomationScaffoldFile where
  toJSON value = objectWithAdditionalFields ["fileId", "content", "fingerprint"] (scaffoldFileAdditionalFields value) (["fileId" .= scaffoldFileId value, "fingerprint" .= scaffoldFileFingerprint value] <> optionalField "content" (scaffoldFileContent value))

data AutomationScaffoldSkill = AutomationScaffoldSkill
  { scaffoldSkillName :: !Text,
    scaffoldSkillContent :: !(Maybe Text),
    scaffoldSkillFingerprint :: !Text,
    scaffoldSupportingFiles :: !(Maybe [AutomationScaffoldFile]),
    scaffoldSkillAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationScaffoldSkill where show _ = "AutomationScaffoldSkill <redacted>"

instance FromJSON AutomationScaffoldSkill where
  parseJSON = withObject "AutomationScaffoldSkill" $ \fields -> AutomationScaffoldSkill <$> fields .: "name" <*> fields .:! "content" <*> fields .: "fingerprint" <*> fields .:! "supportingFiles" <*> pure (additionalFields ["name", "content", "fingerprint", "supportingFiles"] fields)

instance ToJSON AutomationScaffoldSkill where
  toJSON value = objectWithAdditionalFields ["name", "content", "fingerprint", "supportingFiles"] (scaffoldSkillAdditionalFields value) (["name" .= scaffoldSkillName value, "fingerprint" .= scaffoldSkillFingerprint value] <> optionalField "content" (scaffoldSkillContent value) <> optionalField "supportingFiles" (scaffoldSupportingFiles value))

data AutomationListParams = AutomationListParams
  { automationListBasePath :: !(Maybe Text),
    automationListAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationListParams where show _ = "AutomationListParams <redacted>"

instance FromJSON AutomationListParams where
  parseJSON = withObject "AutomationListParams" $ \fields -> AutomationListParams <$> fields .:! "basePath" <*> pure (additionalFields ["basePath"] fields)

instance ToJSON AutomationListParams where
  toJSON value = objectWithAdditionalFields ["basePath"] (automationListAdditionalFields value) (optionalField "basePath" (automationListBasePath value))

data AutomationAddress = AutomationAddress
  { addressedAutomationId :: !Text,
    addressedAutomationDirectory :: !(Maybe Text),
    automationBasePath :: !(Maybe Text),
    automationAddressAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationAddress where show _ = "AutomationAddress <redacted>"

-- | Mirror the baseline controller's legacy directory alias. Direct records and
-- decoded low-level requests can leave it absent or supply a distinct directory.
defaultAutomationAddress :: Text -> AutomationAddress
defaultAutomationAddress identifier = AutomationAddress identifier (Just identifier) Nothing mempty

instance FromJSON AutomationAddress where
  parseJSON = withObject "AutomationAddress" $ \fields -> AutomationAddress <$> fields .: "automationId" <*> fields .:! "automationDirName" <*> fields .:! "basePath" <*> pure (additionalFields addressKeys fields)

instance ToJSON AutomationAddress where toJSON address = automationObject [] address []

automationObject :: [Key] -> AutomationAddress -> [Pair] -> Value
automationObject reserved address fields = objectWithAdditionalFields (addressKeys <> reserved) (automationAddressAdditionalFields address) (["automationId" .= addressedAutomationId address] <> optionalField "automationDirName" (addressedAutomationDirectory address) <> optionalField "basePath" (automationBasePath address) <> fields)

addressWithout :: [Key] -> Object -> Parser AutomationAddress
addressWithout keys fields = parseJSON (Object (foldr KeyMap.delete fields keys))

data RunAutomationParams = RunAutomationParams
  { runAutomationAddress :: !AutomationAddress,
    runAutomationComputerId :: !(Maybe Text),
    runAutomationSkills :: !(Maybe [AutomationScaffoldSkill]),
    runAutomationMemoryFiles :: !(Maybe [AutomationScaffoldFile])
  }
  deriving stock (Eq)

instance Show RunAutomationParams where show _ = "RunAutomationParams <redacted>"

defaultRunAutomationParams :: Text -> RunAutomationParams
defaultRunAutomationParams identifier = RunAutomationParams (defaultAutomationAddress identifier) Nothing Nothing Nothing

instance FromJSON RunAutomationParams where
  parseJSON = withObject "RunAutomationParams" $ \fields -> RunAutomationParams <$> addressWithout ["computerId", "skills", "memoryFiles"] fields <*> fields .:! "computerId" <*> fields .:! "skills" <*> fields .:! "memoryFiles"

instance ToJSON RunAutomationParams where
  toJSON value = automationObject ["computerId", "skills", "memoryFiles"] (runAutomationAddress value) (optionalField "computerId" (runAutomationComputerId value) <> optionalField "skills" (runAutomationSkills value) <> optionalField "memoryFiles" (runAutomationMemoryFiles value))

data AutomationHistoryParams = AutomationHistoryParams
  { historyAutomationAddress :: !AutomationAddress,
    automationHistoryLimit :: !(Maybe Scientific),
    automationHistoryOffset :: !(Maybe Scientific)
  }
  deriving stock (Eq)

instance Show AutomationHistoryParams where show _ = "AutomationHistoryParams <redacted>"

instance FromJSON AutomationHistoryParams where
  parseJSON = withObject "AutomationHistoryParams" $ \fields -> AutomationHistoryParams <$> addressWithout ["limit", "offset"] fields <*> fields .:! "limit" <*> fields .:! "offset"

instance ToJSON AutomationHistoryParams where
  toJSON value = automationObject ["limit", "offset"] (historyAutomationAddress value) (optionalField "limit" (automationHistoryLimit value) <> optionalField "offset" (automationHistoryOffset value))

data AutomationVisualParams = AutomationVisualParams
  { visualAutomationAddress :: !AutomationAddress,
    visualAutomationSessionId :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show AutomationVisualParams where show _ = "AutomationVisualParams <redacted>"

instance FromJSON AutomationVisualParams where
  parseJSON = withObject "AutomationVisualParams" $ \fields -> AutomationVisualParams <$> addressWithout ["sessionId"] fields <*> fields .:! "sessionId"

instance ToJSON AutomationVisualParams where
  toJSON value = automationObject ["sessionId"] (visualAutomationAddress value) (optionalField "sessionId" (visualAutomationSessionId value))

data RenameAutomationParams = RenameAutomationParams
  { renamedAutomationAddress :: !AutomationAddress,
    renamedAutomationName :: !Text
  }
  deriving stock (Eq)

instance Show RenameAutomationParams where show _ = "RenameAutomationParams <redacted>"

instance FromJSON RenameAutomationParams where
  parseJSON = withObject "RenameAutomationParams" $ \fields -> RenameAutomationParams <$> addressWithout ["newName"] fields <*> fields .: "newName"

instance ToJSON RenameAutomationParams where
  toJSON value = automationObject ["newName"] (renamedAutomationAddress value) ["newName" .= renamedAutomationName value]

data ListAutomationsResult = ListAutomationsResult
  { listedAutomations :: ![AutomationEntry],
    pendingAutomationSetups :: !(Maybe [AutomationPendingSetup]),
    automationListingAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListAutomationsResult where show _ = "ListAutomationsResult <redacted>"

instance FromJSON ListAutomationsResult where
  parseJSON = withObject "ListAutomationsResult" $ \fields -> ListAutomationsResult <$> fields .: "automations" <*> fields .:! "pendingSetups" <*> pure (additionalFields ["automations", "pendingSetups"] fields)

instance ToJSON ListAutomationsResult where
  toJSON value = objectWithAdditionalFields ["automations", "pendingSetups"] (automationListingAdditionalFields value) (["automations" .= listedAutomations value] <> optionalField "pendingSetups" (pendingAutomationSetups value))

-- | Returned run preparation data. The SDK does not create a session or prepend
-- scaffoldReminder again; the daemon already includes it in prompt when needed.
data RunAutomationResult = RunAutomationResult
  { automationRunPrompt :: !Text,
    automationRunName :: !Text,
    automationRunId :: !Text,
    automationRunTemplate :: !(Maybe AutomationTemplate),
    automationRunCwd :: !Text,
    automationRunModel :: !(Maybe Text),
    automationRunReasoning :: !(Maybe Text),
    automationRunComputerId :: !(Maybe Text),
    automationScaffoldReminder :: !(Maybe Text),
    automationRunSessionPrivacy :: !(Maybe Text),
    automationRunModelFallback :: !(Maybe ModelFallback),
    automationRunAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show RunAutomationResult where show _ = "RunAutomationResult <redacted>"

instance FromJSON RunAutomationResult where
  parseJSON = withObject "RunAutomationResult" $ \fields -> RunAutomationResult <$> fields .: "prompt" <*> fields .: "automationName" <*> fields .: "automationId" <*> fields .:! "templateId" <*> fields .: "cwd" <*> fields .:! "model" <*> fields .:! "reasoningEffort" <*> fields .:! "computerId" <*> fields .:! "scaffoldReminder" <*> fields .:! "sessionPrivacy" <*> fields .:! "modelFallback" <*> pure (additionalFields runResultKeys fields)

instance ToJSON RunAutomationResult where
  toJSON value = objectWithAdditionalFields runResultKeys (automationRunAdditionalFields value) (["prompt" .= automationRunPrompt value, "automationName" .= automationRunName value, "automationId" .= automationRunId value, "cwd" .= automationRunCwd value] <> optionalField "templateId" (automationRunTemplate value) <> optionalField "model" (automationRunModel value) <> optionalField "reasoningEffort" (automationRunReasoning value) <> optionalField "computerId" (automationRunComputerId value) <> optionalField "scaffoldReminder" (automationScaffoldReminder value) <> optionalField "sessionPrivacy" (automationRunSessionPrivacy value) <> optionalField "modelFallback" (automationRunModelFallback value))

data AutomationStatusResult = AutomationStatusResult
  { automationStatusSuccess :: !Bool,
    automationStatusId :: !Text,
    automationStatus :: !Text,
    automationStatusError :: !(Maybe Text),
    automationStatusAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationStatusResult where show _ = "AutomationStatusResult <redacted>"

instance FromJSON AutomationStatusResult where
  parseJSON = withObject "AutomationStatusResult" $ \fields -> AutomationStatusResult <$> fields .: "success" <*> fields .: "automationId" <*> fields .: "status" <*> fields .:! "error" <*> pure (additionalFields ["success", "automationId", "status", "error"] fields)

instance ToJSON AutomationStatusResult where
  toJSON value = objectWithAdditionalFields ["success", "automationId", "status", "error"] (automationStatusAdditionalFields value) (["success" .= automationStatusSuccess value, "automationId" .= automationStatusId value, "status" .= automationStatus value] <> optionalField "error" (automationStatusError value))

data AutomationRunRecord = AutomationRunRecord
  { recordedRunId :: !Text,
    recordedAutomationId :: !Text,
    recordedRunType :: !(Maybe AutomationRunType),
    recordedRunStatus :: !Text,
    recordedRunStartedAt :: !Text,
    recordedRunCompletedAt :: !(Maybe Text),
    recordedRunDurationMs :: !(Maybe Scientific),
    recordedRunError :: !(Maybe Text),
    recordedRunIsRetry :: !(Maybe Bool),
    recordedOriginalRunId :: !(Maybe Text),
    recordedRunSessionId :: !(Maybe Text),
    recordedRunModelFallback :: !(Maybe ModelFallback),
    recordedRunAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationRunRecord where show _ = "AutomationRunRecord <redacted>"

instance FromJSON AutomationRunRecord where
  parseJSON = withObject "AutomationRunRecord" $ \fields -> AutomationRunRecord <$> fields .: "runId" <*> fields .: "automationId" <*> fields .:! "type" <*> fields .: "status" <*> fields .: "startedAt" <*> fields .:! "completedAt" <*> fields .:! "durationMs" <*> fields .:! "errorMessage" <*> fields .:! "isRetry" <*> fields .:! "originalRunId" <*> fields .:! "sessionId" <*> fields .:! "modelFallback" <*> pure (additionalFields runRecordKeys fields)

instance ToJSON AutomationRunRecord where
  toJSON value = objectWithAdditionalFields runRecordKeys (recordedRunAdditionalFields value) (["runId" .= recordedRunId value, "automationId" .= recordedAutomationId value, "status" .= recordedRunStatus value, "startedAt" .= recordedRunStartedAt value] <> optionalField "type" (recordedRunType value) <> optionalField "completedAt" (recordedRunCompletedAt value) <> optionalField "durationMs" (recordedRunDurationMs value) <> optionalField "errorMessage" (recordedRunError value) <> optionalField "isRetry" (recordedRunIsRetry value) <> optionalField "originalRunId" (recordedOriginalRunId value) <> optionalField "sessionId" (recordedRunSessionId value) <> optionalField "modelFallback" (recordedRunModelFallback value))

data AutomationHistoryResult = AutomationHistoryResult
  { historyAutomationId :: !Text,
    automationHistoryRuns :: ![AutomationRunRecord],
    automationHistoryTotal :: !Scientific,
    automationHistoryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationHistoryResult where show _ = "AutomationHistoryResult <redacted>"

instance FromJSON AutomationHistoryResult where
  parseJSON = withObject "AutomationHistoryResult" $ \fields -> AutomationHistoryResult <$> fields .: "automationId" <*> fields .: "runs" <*> fields .: "totalCount" <*> pure (additionalFields ["automationId", "runs", "totalCount"] fields)

instance ToJSON AutomationHistoryResult where
  toJSON value = objectWithAdditionalFields ["automationId", "runs", "totalCount"] (automationHistoryAdditionalFields value) ["automationId" .= historyAutomationId value, "runs" .= automationHistoryRuns value, "totalCount" .= automationHistoryTotal value]

data AutomationVisualResult = AutomationVisualResult
  { visualAutomationId :: !Text,
    automationVisualExists :: !Bool,
    automationVisualContent :: !(Maybe Text),
    automationVisualStale :: !(Maybe Bool),
    automationVisualUrl :: !(Maybe Text),
    automationVisualAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationVisualResult where show _ = "AutomationVisualResult <redacted>"

instance FromJSON AutomationVisualResult where
  parseJSON = withObject "AutomationVisualResult" $ \fields -> AutomationVisualResult <$> fields .: "automationId" <*> fields .: "exists" <*> fields .:! "content" <*> fields .:! "isStale" <*> fields .:! "s3Url" <*> pure (additionalFields ["automationId", "exists", "content", "isStale", "s3Url"] fields)

instance ToJSON AutomationVisualResult where
  toJSON value = objectWithAdditionalFields ["automationId", "exists", "content", "isStale", "s3Url"] (automationVisualAdditionalFields value) (["automationId" .= visualAutomationId value, "exists" .= automationVisualExists value] <> optionalField "content" (automationVisualContent value) <> optionalField "isStale" (automationVisualStale value) <> optionalField "s3Url" (automationVisualUrl value))

data AutomationPrivacy = AutomationPrivate | AutomationOrganization deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationPrivacy where
  parseJSON = withText "AutomationPrivacy" $ \case "private" -> pure AutomationPrivate; "organization" -> pure AutomationOrganization; _ -> fail "Unknown automation privacy"

instance ToJSON AutomationPrivacy where
  toJSON AutomationPrivate = String "private"
  toJSON AutomationOrganization = String "organization"

data AutomationSessionPrivacy = AutomationSessionPrivate | AutomationSessionTeam deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationSessionPrivacy where
  parseJSON = withText "AutomationSessionPrivacy" $ \case "private" -> pure AutomationSessionPrivate; "team" -> pure AutomationSessionTeam; _ -> fail "Unknown automation session privacy"

instance ToJSON AutomationSessionPrivacy where
  toJSON AutomationSessionPrivate = String "private"
  toJSON AutomationSessionTeam = String "team"

data AutomationCreationSource = AutomationFromWeb | AutomationFromDesktop | AutomationFromCli | AutomationFromCliMenu | AutomationFromApi | AutomationFromSync | AutomationFromFork
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationCreationSource where
  parseJSON = withText "AutomationCreationSource" $ \case
    "web" -> pure AutomationFromWeb
    "desktop" -> pure AutomationFromDesktop
    "cli" -> pure AutomationFromCli
    "cli-menu" -> pure AutomationFromCliMenu
    "api" -> pure AutomationFromApi
    "sync" -> pure AutomationFromSync
    "fork" -> pure AutomationFromFork
    _ -> fail "Unknown automation creation source"

instance ToJSON AutomationCreationSource where
  toJSON AutomationFromWeb = String "web"
  toJSON AutomationFromDesktop = String "desktop"
  toJSON AutomationFromCli = String "cli"
  toJSON AutomationFromCliMenu = String "cli-menu"
  toJSON AutomationFromApi = String "api"
  toJSON AutomationFromSync = String "sync"
  toJSON AutomationFromFork = String "fork"

data AutomationConfigFailureReason = AutomationLocalFileUnavailable | AutomationDiscoveryFailed | AutomationApplyFailed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationConfigFailureReason where
  parseJSON = withText "AutomationConfigFailureReason" $ \case "local-file-unavailable" -> pure AutomationLocalFileUnavailable; "discovery-failed" -> pure AutomationDiscoveryFailed; "apply-failed" -> pure AutomationApplyFailed; _ -> fail "Unknown automation config failure"

instance ToJSON AutomationConfigFailureReason where
  toJSON AutomationLocalFileUnavailable = String "local-file-unavailable"
  toJSON AutomationDiscoveryFailed = String "discovery-failed"
  toJSON AutomationApplyFailed = String "apply-failed"

data CreateAutomationParams = CreateAutomationParams
  { createAutomationId :: !Text,
    createAutomationUuid :: !(Maybe Text),
    createAutomationName :: !Text,
    createAutomationDescription :: !(Maybe Text),
    createAutomationInstructions :: !(Maybe Text),
    createAutomationSchedule :: !Text,
    createAutomationModel :: !(Maybe Text),
    createAutomationReasoning :: !(Maybe Text),
    createAutomationBasePath :: !(Maybe Text),
    createAutomationVisualDescription :: !(Maybe Text),
    createAutomationMemoryStrategy :: !(Maybe Text),
    createAutomationSkipFirstRun :: !(Maybe Bool),
    createAutomationWorkstreamId :: !(Maybe Text),
    createAutomationSource :: !(Maybe AutomationCreationSource),
    createAutomationSkills :: !(Maybe [AutomationScaffoldSkill]),
    createAutomationPrivacy :: !(Maybe AutomationPrivacy),
    createAutomationSessionPrivacy :: !(Maybe AutomationSessionPrivacy),
    createAutomationPaused :: !(Maybe Bool),
    createAutomationTags :: !(Maybe [Text]),
    createAutomationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CreateAutomationParams where show _ = "CreateAutomationParams <redacted>"

defaultCreateAutomationParams :: Text -> Text -> Text -> CreateAutomationParams
defaultCreateAutomationParams identifier name schedule = CreateAutomationParams identifier Nothing name Nothing Nothing schedule Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON CreateAutomationParams where
  parseJSON = withObject "CreateAutomationParams" $ \fields -> CreateAutomationParams <$> fields .: "id" <*> fields .:! "uuid" <*> fields .: "name" <*> fields .:! "description" <*> fields .:! "instructions" <*> fields .: "schedule" <*> fields .:! "model" <*> fields .:! "reasoningEffort" <*> fields .:! "basePath" <*> fields .:! "visualDescription" <*> fields .:! "memoryStrategy" <*> fields .:! "skipFirstRun" <*> fields .:! "workstreamId" <*> fields .:! "creationSource" <*> fields .:! "skills" <*> fields .:! "privacyLevel" <*> fields .:! "sessionPrivacy" <*> fields .:! "paused" <*> fields .:! "tags" <*> pure (additionalFields createKeys fields)

instance ToJSON CreateAutomationParams where
  toJSON value = objectWithAdditionalFields createKeys (createAutomationAdditionalFields value) (["id" .= createAutomationId value, "name" .= createAutomationName value, "schedule" .= createAutomationSchedule value] <> optionalField "uuid" (createAutomationUuid value) <> optionalField "description" (createAutomationDescription value) <> optionalField "instructions" (createAutomationInstructions value) <> optionalField "model" (createAutomationModel value) <> optionalField "reasoningEffort" (createAutomationReasoning value) <> optionalField "basePath" (createAutomationBasePath value) <> optionalField "visualDescription" (createAutomationVisualDescription value) <> optionalField "memoryStrategy" (createAutomationMemoryStrategy value) <> optionalField "skipFirstRun" (createAutomationSkipFirstRun value) <> optionalField "workstreamId" (createAutomationWorkstreamId value) <> optionalField "creationSource" (createAutomationSource value) <> optionalField "skills" (createAutomationSkills value) <> optionalField "privacyLevel" (createAutomationPrivacy value) <> optionalField "sessionPrivacy" (createAutomationSessionPrivacy value) <> optionalField "paused" (createAutomationPaused value) <> optionalField "tags" (createAutomationTags value))

data ForkAutomationParams = ForkAutomationParams
  { forkAutomationId :: !Text,
    forkAutomationName :: !Text,
    forkAutomationDescription :: !(Maybe Text),
    forkAutomationSchedule :: !Text,
    forkAutomationTags :: !(Maybe [Text]),
    forkAutomationModel :: !(Maybe Text),
    forkAutomationReasoning :: !(Maybe Text),
    forkAutomationPrompt :: !Text,
    forkAutomationFrom :: !Text,
    forkAutomationDirectory :: !Text,
    forkAutomationSkills :: !(Maybe [AutomationScaffoldSkill]),
    forkAutomationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ForkAutomationParams where show _ = "ForkAutomationParams <redacted>"

instance FromJSON ForkAutomationParams where
  parseJSON = withObject "ForkAutomationParams" $ \fields -> ForkAutomationParams <$> fields .: "automationId" <*> fields .: "name" <*> fields .:! "description" <*> fields .: "schedule" <*> fields .:! "tags" <*> fields .:! "model" <*> fields .:! "reasoningEffort" <*> fields .: "prompt" <*> fields .: "forkedFrom" <*> fields .: "localDirName" <*> fields .:! "skills" <*> pure (additionalFields forkKeys fields)

instance ToJSON ForkAutomationParams where
  toJSON value = objectWithAdditionalFields forkKeys (forkAutomationAdditionalFields value) (["automationId" .= forkAutomationId value, "name" .= forkAutomationName value, "schedule" .= forkAutomationSchedule value, "prompt" .= forkAutomationPrompt value, "forkedFrom" .= forkAutomationFrom value, "localDirName" .= forkAutomationDirectory value] <> optionalField "description" (forkAutomationDescription value) <> optionalField "tags" (forkAutomationTags value) <> optionalField "model" (forkAutomationModel value) <> optionalField "reasoningEffort" (forkAutomationReasoning value) <> optionalField "skills" (forkAutomationSkills value))

-- | The model member is required: Nothing encodes explicit null, not omission.
data UpdateAutomationModelParams = UpdateAutomationModelParams
  { modelAutomationAddress :: !AutomationAddress,
    updatedAutomationModel :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show UpdateAutomationModelParams where show _ = "UpdateAutomationModelParams <redacted>"

instance FromJSON UpdateAutomationModelParams where
  parseJSON = withObject "UpdateAutomationModelParams" $ \fields -> UpdateAutomationModelParams <$> addressWithout ["model"] fields <*> fields .: "model"

instance ToJSON UpdateAutomationModelParams where
  toJSON value = automationObject ["model"] (modelAutomationAddress value) ["model" .= updatedAutomationModel value]

data UpdateAutomationPrivacyParams = UpdateAutomationPrivacyParams
  { privacyAutomationAddress :: !AutomationAddress,
    updatedAutomationPrivacy :: !AutomationPrivacy,
    updatedAutomationCreator :: !(Maybe AutomationCreator),
    updatedAutomationSessionPrivacy :: !(Maybe AutomationSessionPrivacy)
  }
  deriving stock (Eq)

instance Show UpdateAutomationPrivacyParams where show _ = "UpdateAutomationPrivacyParams <redacted>"

instance FromJSON UpdateAutomationPrivacyParams where
  parseJSON = withObject "UpdateAutomationPrivacyParams" $ \fields -> UpdateAutomationPrivacyParams <$> addressWithout ["privacyLevel", "createdBy", "sessionPrivacy"] fields <*> fields .: "privacyLevel" <*> fields .:! "createdBy" <*> fields .:! "sessionPrivacy"

instance ToJSON UpdateAutomationPrivacyParams where
  toJSON value = automationObject ["privacyLevel", "createdBy", "sessionPrivacy"] (privacyAutomationAddress value) (["privacyLevel" .= updatedAutomationPrivacy value] <> optionalField "createdBy" (updatedAutomationCreator value) <> optionalField "sessionPrivacy" (updatedAutomationSessionPrivacy value))

data UpdateAutomationPromptParams = UpdateAutomationPromptParams
  { promptAutomationAddress :: !AutomationAddress,
    updatedAutomationPrompt :: !Text
  }
  deriving stock (Eq)

instance Show UpdateAutomationPromptParams where show _ = "UpdateAutomationPromptParams <redacted>"

instance FromJSON UpdateAutomationPromptParams where
  parseJSON = withObject "UpdateAutomationPromptParams" $ \fields -> UpdateAutomationPromptParams <$> addressWithout ["prompt"] fields <*> fields .: "prompt"

instance ToJSON UpdateAutomationPromptParams where
  toJSON value = automationObject ["prompt"] (promptAutomationAddress value) ["prompt" .= updatedAutomationPrompt value]

data UpdateAutomationScheduleParams = UpdateAutomationScheduleParams
  { scheduleAutomationAddress :: !AutomationAddress,
    updatedAutomationSchedule :: !Text
  }
  deriving stock (Eq)

instance Show UpdateAutomationScheduleParams where show _ = "UpdateAutomationScheduleParams <redacted>"

instance FromJSON UpdateAutomationScheduleParams where
  parseJSON = withObject "UpdateAutomationScheduleParams" $ \fields -> UpdateAutomationScheduleParams <$> addressWithout ["schedule"] fields <*> fields .: "schedule"

instance ToJSON UpdateAutomationScheduleParams where
  toJSON value = automationObject ["schedule"] (scheduleAutomationAddress value) ["schedule" .= updatedAutomationSchedule value]

-- | Shared full-config fields, not a partial patch. Unknown wire extensions
-- belong to the enclosing apply/update record rather than this field group.
data AutomationConfiguration = AutomationConfiguration
  { configuredAutomationName :: !Text,
    configuredAutomationDescription :: !(Maybe Text),
    configuredAutomationSchedule :: !Text,
    configuredAutomationModel :: !(Maybe Text),
    configuredAutomationReasoning :: !(Maybe Text),
    configuredAutomationPrompt :: !Text,
    configuredAutomationTags :: !(Maybe [Text]),
    configuredAutomationPrivacy :: !(Maybe AutomationPrivacy),
    configuredAutomationSessionPrivacy :: !(Maybe AutomationSessionPrivacy),
    configuredAutomationPaused :: !(Maybe Bool),
    configuredAutomationWorkingDirectory :: !(Maybe Text),
    configuredAutomationWorkstreamId :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show AutomationConfiguration where show _ = "AutomationConfiguration <redacted>"

defaultAutomationConfiguration :: Text -> Text -> Text -> AutomationConfiguration
defaultAutomationConfiguration name schedule prompt = AutomationConfiguration name Nothing schedule Nothing Nothing prompt Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON AutomationConfiguration where
  parseJSON = withObject "AutomationConfiguration" $ \fields -> AutomationConfiguration <$> fields .: "name" <*> fields .:! "description" <*> fields .: "schedule" <*> fields .:! "model" <*> fields .:! "reasoningEffort" <*> fields .: "prompt" <*> fields .:! "tags" <*> fields .:! "privacyLevel" <*> fields .:! "sessionPrivacy" <*> fields .:! "paused" <*> fields .:! "workingDirectory" <*> fields .:! "workstreamId"

instance ToJSON AutomationConfiguration where toJSON = object . automationConfigFields

automationConfigFields :: AutomationConfiguration -> [Pair]
automationConfigFields value = ["name" .= configuredAutomationName value, "schedule" .= configuredAutomationSchedule value, "prompt" .= configuredAutomationPrompt value] <> optionalField "description" (configuredAutomationDescription value) <> optionalField "model" (configuredAutomationModel value) <> optionalField "reasoningEffort" (configuredAutomationReasoning value) <> optionalField "tags" (configuredAutomationTags value) <> optionalField "privacyLevel" (configuredAutomationPrivacy value) <> optionalField "sessionPrivacy" (configuredAutomationSessionPrivacy value) <> optionalField "paused" (configuredAutomationPaused value) <> optionalField "workingDirectory" (configuredAutomationWorkingDirectory value) <> optionalField "workstreamId" (configuredAutomationWorkstreamId value)

data ApplyAutomationConfigParams = ApplyAutomationConfigParams
  { appliedAutomationId :: !Text,
    appliedAutomationBasePath :: !(Maybe Text),
    appliedAutomationConfiguration :: !AutomationConfiguration,
    appliedAutomationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ApplyAutomationConfigParams where show _ = "ApplyAutomationConfigParams <redacted>"

instance FromJSON ApplyAutomationConfigParams where
  parseJSON = withObject "ApplyAutomationConfigParams" $ \fields -> ApplyAutomationConfigParams <$> fields .: "automationId" <*> fields .:! "basePath" <*> parseJSON (Object fields) <*> pure (additionalFields (["automationId", "basePath"] <> configKeys) fields)

instance ToJSON ApplyAutomationConfigParams where
  toJSON value = objectWithAdditionalFields (["automationId", "basePath"] <> configKeys) (appliedAutomationAdditionalFields value) (["automationId" .= appliedAutomationId value] <> optionalField "basePath" (appliedAutomationBasePath value) <> automationConfigFields (appliedAutomationConfiguration value))

data UpdateAutomationParams = UpdateAutomationParams
  { configuredAutomationAddress :: !AutomationAddress,
    updatedAutomationConfiguration :: !AutomationConfiguration
  }
  deriving stock (Eq)

instance Show UpdateAutomationParams where show _ = "UpdateAutomationParams <redacted>"

instance FromJSON UpdateAutomationParams where
  parseJSON = withObject "UpdateAutomationParams" $ \fields -> UpdateAutomationParams <$> addressWithout configKeys fields <*> parseJSON (Object fields)

instance ToJSON UpdateAutomationParams where
  toJSON value = automationObject configKeys (configuredAutomationAddress value) (automationConfigFields (updatedAutomationConfiguration value))

data AutomationCreationResult = AutomationCreationResult
  { automationCreatedSuccess :: !Bool,
    automationCreatedId :: !(Maybe Text),
    automationCreationError :: !(Maybe Text),
    automationCreationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AutomationCreationResult where show _ = "AutomationCreationResult <redacted>"

instance FromJSON AutomationCreationResult where
  parseJSON = withObject "AutomationCreationResult" $ \fields -> AutomationCreationResult <$> fields .: "success" <*> fields .:! "automationId" <*> fields .:! "error" <*> pure (additionalFields ["success", "automationId", "error"] fields)

instance ToJSON AutomationCreationResult where
  toJSON value = objectWithAdditionalFields ["success", "automationId", "error"] (automationCreationAdditionalFields value) (["success" .= automationCreatedSuccess value] <> optionalField "automationId" (automationCreatedId value) <> optionalField "error" (automationCreationError value))

data ApplyAutomationConfigResult = ApplyAutomationConfigResult
  { automationAppliedSuccess :: !Bool,
    automationApplyError :: !(Maybe Text),
    automationApplyFailureReason :: !(Maybe AutomationConfigFailureReason),
    automationApplyAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ApplyAutomationConfigResult where show _ = "ApplyAutomationConfigResult <redacted>"

instance FromJSON ApplyAutomationConfigResult where
  parseJSON = withObject "ApplyAutomationConfigResult" $ \fields -> ApplyAutomationConfigResult <$> fields .: "success" <*> fields .:! "error" <*> fields .:! "reason" <*> pure (additionalFields ["success", "error", "reason"] fields)

instance ToJSON ApplyAutomationConfigResult where
  toJSON value = objectWithAdditionalFields ["success", "error", "reason"] (automationApplyAdditionalFields value) (["success" .= automationAppliedSuccess value] <> optionalField "error" (automationApplyError value) <> optionalField "reason" (automationApplyFailureReason value))

createKeys, forkKeys, configKeys :: [Key]
createKeys = ["id", "uuid", "name", "description", "instructions", "schedule", "model", "reasoningEffort", "basePath", "visualDescription", "memoryStrategy", "skipFirstRun", "workstreamId", "creationSource", "skills", "privacyLevel", "sessionPrivacy", "paused", "tags"]
forkKeys = ["automationId", "name", "description", "schedule", "tags", "model", "reasoningEffort", "prompt", "forkedFrom", "localDirName", "skills"]
configKeys = ["name", "description", "schedule", "model", "reasoningEffort", "prompt", "tags", "privacyLevel", "sessionPrivacy", "paused", "workingDirectory", "workstreamId"]

addressKeys, entryKeys, runResultKeys, runRecordKeys :: [Key]
addressKeys = ["automationId", "automationDirName", "basePath"]
entryKeys = ["id", "uuid", "name", "description", "prompt", "status", "schedule", "model", "reasoningEffort", "tags", "nextRunAt", "lastRunAt", "lastRunStatus", "isValid", "path", "templateId", "privacyLevel", "sessionPrivacy", "createdBy", "forkedFrom", "workingDirectory", "computerId", "machineId", "hostId", "workstreamId", "setupState"]
runResultKeys = ["prompt", "automationName", "automationId", "templateId", "cwd", "model", "reasoningEffort", "computerId", "scaffoldReminder", "sessionPrivacy", "modelFallback"]
runRecordKeys = ["runId", "automationId", "type", "status", "startedAt", "completedAt", "durationMs", "errorMessage", "isRetry", "originalRunId", "sessionId", "modelFallback"]
