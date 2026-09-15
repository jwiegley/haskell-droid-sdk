{-# LANGUAGE OverloadedStrings #-}

-- | Cron wire contracts, not a scheduler or cron-expression interpreter.
-- Reports retain opaque timestamps and paths. Explicit fields/JSON are sensitive.
module Factory.Droid.Schema.Daemon.Cron
  ( CronText,
    mkCronText,
    cronTextValue,
    CronStatus (..),
    CronSource (..),
    CronUpdateStatus (..),
    CronSchedule (..),
    CronRecordedSchedule (..),
    CronStats (..),
    CronSessionScope (..),
    SessionCronPayload (..),
    RootCronPayload (..),
    CronRecordInfo (..),
    CronRecord (..),
    CreateCronOptions (..),
    defaultCreateCronOptions,
    CreateCronParams (..),
    ListCronsParams (..),
    defaultListCronsParams,
    CronPayloadPatch (..),
    UpdateCronParams (..),
    defaultUpdateCronParams,
    DeleteCronParams (..),
    HoldSessionCronsParams (..),
    ListCronsResult (..),
    CreateCronResult (..),
    UpdateCronResult (..),
    DeleteCronResult (..),
    HoldSessionCronsResult (..),
    ResumeSessionCronsResult (..),
    CronChangeReason (..),
    CronStateChanged (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON (additionalFields, isEcmaWhitespace, objectWithAdditionalFields, optionalField, rejectUnknownFields, requireLiteral)
import Factory.Droid.Schema.Enums (ReasoningEffort)
import Factory.Droid.Schema.Primitives (NonEmptyText, mkNonEmptyText, nonEmptyTextValue)
import Numeric.Natural (Natural)

-- | SDK trim/min(1), not cron syntax validation. NEL is not ECMAScript whitespace.
newtype CronText = CronText NonEmptyText deriving stock (Eq, Ord)

instance Show CronText where show _ = "CronText <redacted>"

mkCronText :: Text -> Maybe CronText
mkCronText = fmap CronText . mkNonEmptyText . Text.dropAround isEcmaWhitespace

cronTextValue :: CronText -> Text
cronTextValue (CronText value) = nonEmptyTextValue value

instance FromJSON CronText where
  parseJSON = withText "CronText" $ maybe (fail "Expected nonblank cron text") pure . mkCronText

instance ToJSON CronText where toJSON = String . cronTextValue

data CronStatus = CronActive | CronHeld | CronPaused | CronRunning | CronError | CronExpired | CronCancelled deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON CronStatus where
  parseJSON = withText "CronStatus" $ \case
    "active" -> pure CronActive
    "held" -> pure CronHeld
    "paused" -> pure CronPaused
    "running" -> pure CronRunning
    "error" -> pure CronError
    "expired" -> pure CronExpired
    "cancelled" -> pure CronCancelled
    _ -> fail "Unknown cron status"

instance ToJSON CronStatus where
  toJSON =
    String . \case
      CronActive -> "active"
      CronHeld -> "held"
      CronPaused -> "paused"
      CronRunning -> "running"
      CronError -> "error"
      CronExpired -> "expired"
      CronCancelled -> "cancelled"

data CronSource = CronLoopCommand | CronTool | CronAutomation | CronMigration deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON CronSource where
  parseJSON = withText "CronSource" $ \case
    "loop_command" -> pure CronLoopCommand
    "cron_tool" -> pure CronTool
    "automation" -> pure CronAutomation
    "migration" -> pure CronMigration
    _ -> fail "Unknown cron source"

instance ToJSON CronSource where
  toJSON = String . \case CronLoopCommand -> "loop_command"; CronTool -> "cron_tool"; CronAutomation -> "automation"; CronMigration -> "migration"

data CronUpdateStatus = ActivateCron | PauseCron deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON CronUpdateStatus where
  parseJSON = withText "CronUpdateStatus" $ \case "active" -> pure ActivateCron; "paused" -> pure PauseCron; _ -> fail "Unknown user-updatable cron status"

instance ToJSON CronUpdateStatus where
  toJSON ActivateCron = String "active"
  toJSON PauseCron = String "paused"

data CronSchedule = CronSchedule
  { cronExpression :: !CronText,
    cronRecurring :: !Bool,
    cronScheduleAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CronSchedule where show _ = "CronSchedule <redacted>"

instance FromJSON CronSchedule where
  parseJSON = withObject "CronSchedule" $ \fields -> CronSchedule <$> fields .: "expression" <*> fields .: "recurring" <*> pure (additionalFields ["expression", "recurring"] fields)

instance ToJSON CronSchedule where
  toJSON value = objectWithAdditionalFields ["expression", "recurring"] (cronScheduleAdditionalFields value) ["expression" .= cronExpression value, "recurring" .= cronRecurring value]

data CronRecordedSchedule = CronRecordedSchedule
  { recordedCronExpression :: !CronText,
    recordedCronRecurring :: !Bool,
    cronNextRunAt :: !(Maybe Text),
    cronFirstFireGuardUntil :: !(Maybe Text),
    recordedCronScheduleAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CronRecordedSchedule where show _ = "CronRecordedSchedule <redacted>"

instance FromJSON CronRecordedSchedule where
  parseJSON = withObject "CronRecordedSchedule" $ \fields -> do
    requireLiteral "timezone" (String "UTC") fields
    CronRecordedSchedule <$> fields .: "expression" <*> fields .: "recurring" <*> fields .:! "nextRunAt" <*> fields .:! "firstFireGuardUntil" <*> pure (additionalFields recordedScheduleKeys fields)

instance ToJSON CronRecordedSchedule where
  toJSON value = objectWithAdditionalFields recordedScheduleKeys (recordedCronScheduleAdditionalFields value) (["expression" .= recordedCronExpression value, "recurring" .= recordedCronRecurring value, "timezone" .= String "UTC"] <> optionalField "nextRunAt" (cronNextRunAt value) <> optionalField "firstFireGuardUntil" (cronFirstFireGuardUntil value))

recordedScheduleKeys :: [Key]
recordedScheduleKeys = ["expression", "recurring", "timezone", "nextRunAt", "firstFireGuardUntil"]

data CronStats = CronStats
  { cronFireCount :: !Natural,
    cronLastRunAt :: !(Maybe Text),
    cronLastCompletedAt :: !(Maybe Text),
    cronLastError :: !(Maybe Text),
    cronStatsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CronStats where show _ = "CronStats <redacted>"

instance FromJSON CronStats where
  parseJSON = withObject "CronStats" $ \fields -> CronStats <$> fields .: "fireCount" <*> fields .:! "lastRunAt" <*> fields .:! "lastCompletedAt" <*> fields .:! "lastError" <*> pure (additionalFields statsKeys fields)

instance ToJSON CronStats where
  toJSON value = objectWithAdditionalFields statsKeys (cronStatsAdditionalFields value) (["fireCount" .= cronFireCount value] <> optionalField "lastRunAt" (cronLastRunAt value) <> optionalField "lastCompletedAt" (cronLastCompletedAt value) <> optionalField "lastError" (cronLastError value))

statsKeys :: [Key]
statsKeys = ["fireCount", "lastRunAt", "lastCompletedAt", "lastError"]

-- | Creation scope. A stored session cron additionally reports storageDir.
data CronSessionScope = CronSessionScope
  { cronScopeSessionId :: !Text,
    cronScopeSessionCwd :: !Text,
    cronSessionScopeAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CronSessionScope where show _ = "CronSessionScope <redacted>"

instance FromJSON CronSessionScope where
  parseJSON = withObject "CronSessionScope" $ \fields -> do
    requireLiteral "type" (String "session") fields
    CronSessionScope <$> fields .: "sessionId" <*> fields .: "sessionCwd" <*> pure (additionalFields sessionScopeKeys fields)

instance ToJSON CronSessionScope where
  toJSON value = objectWithAdditionalFields sessionScopeKeys (cronSessionScopeAdditionalFields value) (sessionScopeFields value)

sessionScopeKeys :: [Key]
sessionScopeKeys = ["type", "sessionId", "sessionCwd"]

sessionScopeFields :: CronSessionScope -> [Pair]
sessionScopeFields value = ["type" .= String "session", "sessionId" .= cronScopeSessionId value, "sessionCwd" .= cronScopeSessionCwd value]

data SessionCronPayload = SessionCronPayload
  { sessionCronPrompt :: !CronText,
    sameSessionTargetAdditionalFields :: !Object,
    sessionCronPayloadAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionCronPayload where show _ = "SessionCronPayload <redacted>"

instance FromJSON SessionCronPayload where
  parseJSON = withObject "SessionCronPayload" $ \fields -> do
    requireLiteral "type" (String "prompt") fields
    target <- fields .: "target"
    requireLiteral "type" (String "same_session") target
    SessionCronPayload <$> fields .: "prompt" <*> pure (additionalFields ["type"] target) <*> pure (additionalFields ["type", "prompt", "target"] fields)

instance ToJSON SessionCronPayload where
  toJSON value = objectWithAdditionalFields ["type", "prompt", "target"] (sessionCronPayloadAdditionalFields value) ["type" .= String "prompt", "prompt" .= sessionCronPrompt value, "target" .= objectWithAdditionalFields ["type"] (sameSessionTargetAdditionalFields value) ["type" .= String "same_session"]]

data RootCronPayload = RootCronPayload
  { rootCronPrompt :: !CronText,
    rootCronCwd :: !(Maybe Text),
    rootCronTitle :: !(Maybe Text),
    rootCronModelId :: !(Maybe Text),
    rootCronReasoningEffort :: !(Maybe ReasoningEffort),
    newSessionTargetAdditionalFields :: !Object,
    rootCronPayloadAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show RootCronPayload where show _ = "RootCronPayload <redacted>"

instance FromJSON RootCronPayload where
  parseJSON = withObject "RootCronPayload" $ \fields -> do
    requireLiteral "type" (String "prompt") fields
    target <- fields .: "target"
    requireLiteral "type" (String "new_session") target
    RootCronPayload <$> fields .: "prompt" <*> target .:! "cwd" <*> target .:! "title" <*> fields .:! "modelId" <*> fields .:! "reasoningEffort" <*> pure (additionalFields ["type", "cwd", "title"] target) <*> pure (additionalFields ["type", "prompt", "target", "modelId", "reasoningEffort"] fields)

instance ToJSON RootCronPayload where
  toJSON value = objectWithAdditionalFields ["type", "prompt", "target", "modelId", "reasoningEffort"] (rootCronPayloadAdditionalFields value) (["type" .= String "prompt", "prompt" .= rootCronPrompt value, "target" .= objectWithAdditionalFields ["type", "cwd", "title"] (newSessionTargetAdditionalFields value) (["type" .= String "new_session"] <> optionalField "cwd" (rootCronCwd value) <> optionalField "title" (rootCronTitle value))] <> optionalField "modelId" (rootCronModelId value) <> optionalField "reasoningEffort" (rootCronReasoningEffort value))

data CronRecordInfo = CronRecordInfo
  { cronId :: !Text,
    cronStatus :: !CronStatus,
    cronSource :: !CronSource,
    cronRecordedSchedule :: !CronRecordedSchedule,
    cronStats :: !CronStats,
    cronCreatedAt :: !Text,
    cronUpdatedAt :: !Text,
    cronHeldAt :: !(Maybe Text),
    cronHoldReason :: !(Maybe Text),
    cronRunPolicyAdditionalFields :: !Object,
    cronRecordAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CronRecordInfo where show _ = "CronRecordInfo <redacted>"

data CronRecord
  = SessionCronRecord !CronRecordInfo !CronSessionScope !Text !SessionCronPayload
  | RootCronRecord !CronRecordInfo !Object !RootCronPayload
  deriving stock (Eq)

instance Show CronRecord where show _ = "CronRecord <redacted>"

instance FromJSON CronRecord where
  parseJSON = withObject "CronRecord" $ \fields -> do
    requireLiteral "version" (Number 1) fields
    kind <- fields .: "kind"
    policy <- fields .: "runPolicy" >>= parsePolicy (inactivePolicy kind)
    info <- CronRecordInfo <$> fields .: "id" <*> fields .: "status" <*> fields .: "source" <*> fields .: "schedule" <*> fields .: "stats" <*> fields .: "createdAt" <*> fields .: "updatedAt" <*> fields .:! "heldAt" <*> fields .:! "holdReason" <*> pure policy <*> pure (additionalFields recordKeys fields)
    scope <- fields .: "scope"
    case kind of
      "session_prompt" -> SessionCronRecord info <$> parseJSON (Object (KeyMap.delete "storageDir" scope)) <*> scope .: "storageDir" <*> fields .: "payload"
      "root_prompt" -> RootCronRecord info <$> parseRootScope scope <*> fields .: "payload"
      _ -> fail "Unknown cron kind"

instance ToJSON CronRecord where
  toJSON value = case value of
    SessionCronRecord info scope storage payload -> render "session_prompt" info (objectWithAdditionalFields ("storageDir" : sessionScopeKeys) (cronSessionScopeAdditionalFields scope) (sessionScopeFields scope <> ["storageDir" .= storage])) (toJSON payload)
    RootCronRecord info scope payload -> render "root_prompt" info (rootScopeValue scope) (toJSON payload)
    where
      render kind info scope payload = objectWithAdditionalFields recordKeys (cronRecordAdditionalFields info) (["version" .= (1 :: Int), "id" .= cronId info, "status" .= cronStatus info, "source" .= cronSource info, "schedule" .= cronRecordedSchedule info, "stats" .= cronStats info, "createdAt" .= cronCreatedAt info, "updatedAt" .= cronUpdatedAt info, "kind" .= kind, "scope" .= scope, "payload" .= payload, "runPolicy" .= policyValue (inactivePolicy kind) (cronRunPolicyAdditionalFields info)] <> optionalField "heldAt" (cronHeldAt info) <> optionalField "holdReason" (cronHoldReason info))

recordKeys :: [Key]
recordKeys = ["version", "id", "status", "source", "schedule", "stats", "createdAt", "updatedAt", "heldAt", "holdReason", "kind", "scope", "runPolicy", "payload"]

inactivePolicy :: Text -> Text
inactivePolicy kind = if kind == "session_prompt" then "hold" else "run_in_background"

parsePolicy :: Text -> Object -> Parser Object
parsePolicy expected fields = requireLiteral "whenSessionInactive" (String expected) fields >> pure (additionalFields ["whenSessionInactive"] fields)

policyValue :: Text -> Object -> Value
policyValue policy extras = objectWithAdditionalFields ["whenSessionInactive"] extras ["whenSessionInactive" .= policy]

parseRootScope :: Object -> Parser Object
parseRootScope fields = requireLiteral "type" (String "root") fields >> pure (additionalFields ["type"] fields)

rootScopeValue :: Object -> Value
rootScopeValue extras = objectWithAdditionalFields ["type"] extras ["type" .= String "root"]

-- | A present run-policy extension object requests the kind's fixed policy;
-- Nothing omits it. Constructors cannot pair a kind with the opposite policy.
data CreateCronOptions = CreateCronOptions
  { createCronSource :: !CronSource,
    createCronSchedule :: !CronSchedule,
    createCronRunImmediately :: !(Maybe Bool),
    createCronRunPolicyAdditionalFields :: !(Maybe Object),
    createCronAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CreateCronOptions where show _ = "CreateCronOptions <redacted>"

-- | The SDK resource's source is cron_tool. No eager execution is requested.
defaultCreateCronOptions :: CronSchedule -> CreateCronOptions
defaultCreateCronOptions schedule = CreateCronOptions CronTool schedule Nothing Nothing mempty

data CreateCronParams
  = CreateSessionCron !CreateCronOptions !CronSessionScope !SessionCronPayload
  | CreateRootCron !CreateCronOptions !Object !RootCronPayload
  deriving stock (Eq)

instance Show CreateCronParams where show _ = "CreateCronParams <redacted>"

instance FromJSON CreateCronParams where
  parseJSON = withObject "CreateCronParams" $ \fields -> do
    kind <- fields .: "kind"
    policy <- fields .:! "runPolicy" >>= traverse (parsePolicy (inactivePolicy kind))
    options <- CreateCronOptions <$> fields .: "source" <*> fields .: "schedule" <*> fields .:! "runImmediately" <*> pure policy <*> pure (additionalFields createKeys fields)
    case kind of
      "session_prompt" -> CreateSessionCron options <$> fields .: "scope" <*> fields .: "payload"
      "root_prompt" -> CreateRootCron options <$> (fields .: "scope" >>= parseRootScope) <*> fields .: "payload"
      _ -> fail "Unknown cron kind"

instance ToJSON CreateCronParams where
  toJSON value = case value of
    CreateSessionCron options scope payload -> render "session_prompt" options (toJSON scope) (toJSON payload)
    CreateRootCron options scope payload -> render "root_prompt" options (rootScopeValue scope) (toJSON payload)
    where
      render kind options scope payload = objectWithAdditionalFields createKeys (createCronAdditionalFields options) (["kind" .= kind, "source" .= createCronSource options, "schedule" .= createCronSchedule options, "scope" .= scope, "payload" .= payload] <> optionalField "runImmediately" (createCronRunImmediately options) <> optionalField "runPolicy" (policyValue (inactivePolicy kind) <$> createCronRunPolicyAdditionalFields options))

createKeys :: [Key]
createKeys = ["kind", "source", "schedule", "scope", "payload", "runImmediately", "runPolicy"]

data ListCronsParams = ListCronsParams
  { listCronsSessionId :: !(Maybe Text),
    listCronsIncludeInactive :: !(Maybe Bool),
    listCronsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListCronsParams where show _ = "ListCronsParams <redacted>"

defaultListCronsParams :: ListCronsParams
defaultListCronsParams = ListCronsParams Nothing Nothing mempty

instance FromJSON ListCronsParams where
  parseJSON = withObject "ListCronsParams" $ \fields -> ListCronsParams <$> fields .:! "sessionId" <*> fields .:! "includeInactive" <*> pure (additionalFields ["sessionId", "includeInactive"] fields)

instance ToJSON ListCronsParams where
  toJSON value = objectWithAdditionalFields ["sessionId", "includeInactive"] (listCronsAdditionalFields value) (optionalField "sessionId" (listCronsSessionId value) <> optionalField "includeInactive" (listCronsIncludeInactive value))

-- | The patch is closed; an empty patch differs from an omitted patch.
newtype CronPayloadPatch = CronPayloadPatch {cronPatchedPrompt :: Maybe CronText} deriving stock (Eq)

instance Show CronPayloadPatch where show _ = "CronPayloadPatch <redacted>"

instance FromJSON CronPayloadPatch where
  parseJSON = withObject "CronPayloadPatch" $ \fields -> rejectUnknownFields ["prompt"] fields >> CronPayloadPatch <$> fields .:! "prompt"

instance ToJSON CronPayloadPatch where toJSON value = object (optionalField "prompt" (cronPatchedPrompt value))

data UpdateCronParams = UpdateCronParams
  { updateCronId :: !Text,
    updateCronStatus :: !(Maybe CronUpdateStatus),
    updateCronSchedule :: !(Maybe CronSchedule),
    updateCronPayload :: !(Maybe CronPayloadPatch),
    updateCronAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateCronParams where show _ = "UpdateCronParams <redacted>"

defaultUpdateCronParams :: Text -> UpdateCronParams
defaultUpdateCronParams identifier = UpdateCronParams identifier Nothing Nothing Nothing mempty

instance FromJSON UpdateCronParams where
  parseJSON = withObject "UpdateCronParams" $ \fields -> UpdateCronParams <$> fields .: "cronId" <*> fields .:! "status" <*> fields .:! "schedule" <*> fields .:! "payload" <*> pure (additionalFields updateKeys fields)

instance ToJSON UpdateCronParams where
  toJSON value = objectWithAdditionalFields updateKeys (updateCronAdditionalFields value) (["cronId" .= updateCronId value] <> optionalField "status" (updateCronStatus value) <> optionalField "schedule" (updateCronSchedule value) <> optionalField "payload" (updateCronPayload value))

updateKeys :: [Key]
updateKeys = ["cronId", "status", "schedule", "payload"]

data DeleteCronParams = DeleteCronParams
  { deleteCronId :: !Text,
    deleteCronSessionId :: !(Maybe Text),
    deleteCronAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show DeleteCronParams where show _ = "DeleteCronParams <redacted>"

instance FromJSON DeleteCronParams where
  parseJSON = withObject "DeleteCronParams" $ \fields -> DeleteCronParams <$> fields .: "cronId" <*> fields .:! "sessionId" <*> pure (additionalFields ["cronId", "sessionId"] fields)

instance ToJSON DeleteCronParams where
  toJSON value = objectWithAdditionalFields ["cronId", "sessionId"] (deleteCronAdditionalFields value) (["cronId" .= deleteCronId value] <> optionalField "sessionId" (deleteCronSessionId value))

data HoldSessionCronsParams = HoldSessionCronsParams
  { holdCronsSessionId :: !Text,
    holdCronsReason :: !Text,
    holdCronsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show HoldSessionCronsParams where show _ = "HoldSessionCronsParams <redacted>"

instance FromJSON HoldSessionCronsParams where
  parseJSON = withObject "HoldSessionCronsParams" $ \fields -> HoldSessionCronsParams <$> fields .: "sessionId" <*> fields .: "reason" <*> pure (additionalFields ["sessionId", "reason"] fields)

instance ToJSON HoldSessionCronsParams where
  toJSON value = objectWithAdditionalFields ["sessionId", "reason"] (holdCronsAdditionalFields value) ["sessionId" .= holdCronsSessionId value, "reason" .= holdCronsReason value]

data ListCronsResult = ListCronsResult {listedCrons :: ![CronRecord], listCronsResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show ListCronsResult where show _ = "ListCronsResult <redacted>"

instance FromJSON ListCronsResult where parseJSON = withObject "ListCronsResult" $ \fields -> ListCronsResult <$> fields .: "crons" <*> pure (additionalFields ["crons"] fields)

instance ToJSON ListCronsResult where toJSON value = objectWithAdditionalFields ["crons"] (listCronsResultAdditionalFields value) ["crons" .= listedCrons value]

data CreateCronResult = CreateCronResult {createdCron :: !CronRecord, createCronResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show CreateCronResult where show _ = "CreateCronResult <redacted>"

instance FromJSON CreateCronResult where parseJSON = withObject "CreateCronResult" $ \fields -> CreateCronResult <$> fields .: "cron" <*> pure (additionalFields ["cron"] fields)

instance ToJSON CreateCronResult where toJSON value = objectWithAdditionalFields ["cron"] (createCronResultAdditionalFields value) ["cron" .= createdCron value]

data UpdateCronResult = UpdateCronResult {updatedCron :: !(Maybe CronRecord), updateCronResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show UpdateCronResult where show _ = "UpdateCronResult <redacted>"

instance FromJSON UpdateCronResult where parseJSON = withObject "UpdateCronResult" $ \fields -> UpdateCronResult <$> fields .: "cron" <*> pure (additionalFields ["cron"] fields)

instance ToJSON UpdateCronResult where toJSON value = objectWithAdditionalFields ["cron"] (updateCronResultAdditionalFields value) ["cron" .= updatedCron value]

data DeleteCronResult = DeleteCronResult {cronDeleted :: !Bool, deleteCronResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show DeleteCronResult where show _ = "DeleteCronResult <redacted>"

instance FromJSON DeleteCronResult where parseJSON = withObject "DeleteCronResult" $ \fields -> DeleteCronResult <$> fields .: "deleted" <*> pure (additionalFields ["deleted"] fields)

instance ToJSON DeleteCronResult where toJSON value = objectWithAdditionalFields ["deleted"] (deleteCronResultAdditionalFields value) ["deleted" .= cronDeleted value]

data HoldSessionCronsResult = HoldSessionCronsResult {heldCronCount :: !Natural, holdCronsResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show HoldSessionCronsResult where show _ = "HoldSessionCronsResult <redacted>"

instance FromJSON HoldSessionCronsResult where parseJSON = withObject "HoldSessionCronsResult" $ \fields -> HoldSessionCronsResult <$> fields .: "heldCount" <*> pure (additionalFields ["heldCount"] fields)

instance ToJSON HoldSessionCronsResult where toJSON value = objectWithAdditionalFields ["heldCount"] (holdCronsResultAdditionalFields value) ["heldCount" .= heldCronCount value]

data ResumeSessionCronsResult = ResumeSessionCronsResult {resumedCronCount :: !Natural, resumeCronsResultAdditionalFields :: !Object} deriving stock (Eq)

instance Show ResumeSessionCronsResult where show _ = "ResumeSessionCronsResult <redacted>"

instance FromJSON ResumeSessionCronsResult where parseJSON = withObject "ResumeSessionCronsResult" $ \fields -> ResumeSessionCronsResult <$> fields .: "resumedCount" <*> pure (additionalFields ["resumedCount"] fields)

instance ToJSON ResumeSessionCronsResult where toJSON value = objectWithAdditionalFields ["resumedCount"] (resumeCronsResultAdditionalFields value) ["resumedCount" .= resumedCronCount value]

data CronChangeReason = CronCreated | CronUpdated | CronDeleted deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON CronChangeReason where parseJSON = withText "CronChangeReason" $ \case "created" -> pure CronCreated; "updated" -> pure CronUpdated; "deleted" -> pure CronDeleted; _ -> fail "Unknown cron change reason"

instance ToJSON CronChangeReason where toJSON = String . \case CronCreated -> "created"; CronUpdated -> "updated"; CronDeleted -> "deleted"

data CronStateChanged = CronStateChanged
  { cronChangeReason :: !CronChangeReason,
    changedCronIds :: ![Text],
    changedCrons :: !(Maybe [CronRecord]),
    cronStateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CronStateChanged where show _ = "CronStateChanged <redacted>"

instance FromJSON CronStateChanged where parseJSON = withObject "CronStateChanged" $ \fields -> CronStateChanged <$> fields .: "reason" <*> fields .: "cronIds" <*> fields .:! "crons" <*> pure (additionalFields ["reason", "cronIds", "crons"] fields)

instance ToJSON CronStateChanged where toJSON value = objectWithAdditionalFields ["reason", "cronIds", "crons"] (cronStateAdditionalFields value) (["reason" .= cronChangeReason value, "cronIds" .= changedCronIds value] <> optionalField "crons" (changedCrons value))
