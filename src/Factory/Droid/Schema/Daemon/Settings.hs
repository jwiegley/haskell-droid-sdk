{-# LANGUAGE OverloadedStrings #-}

-- | Daemon settings, custom-model configuration and provenance. Only report
-- fields with explicit reference fallbacks normalize invalid values. No local
-- settings file, model, session or policy is changed by these codecs.
module Factory.Droid.Schema.Daemon.Settings
  ( SubagentTier (..),
    SubagentAutonomy (..),
    LegacyCompactionMode (..),
    SubagentModelSettings (..),
    emptySubagentModelSettings,
    SettingsManagementInfo (..),
    DefaultSettingsManagement (..),
    ResolutionAction (..),
    ResolutionSourceType (..),
    SettingsResolutionSource (..),
    SettingsResolutionLocation (..),
    SettingsResolutionEvent (..),
    SpecSavePresets (..),
    DefaultSettings (..),
    UpdateSessionDefaultsParams (..),
    emptySessionDefaultsUpdate,
    UpdateSessionDefaultsResult (..),
    CustomModelSummary (..),
    UpsertCustomModelParams (..),
    defaultUpsertCustomModelParams,
    DeleteCustomModelParams (..),
    ListCustomModelsResult (..),
    UpdateCustomModelsResult (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Enums (AutonomyLevel, DroidInteractionMode, ReasoningEffort, SettingsLevel)
import Factory.Droid.Schema.Models (CompactionModel, MissionModelSettings, ModelInfo)
import Factory.Droid.Schema.Primitives (NonEmptyText)
import Factory.Droid.Schema.Settings (LegacyAutonomyMode)

-- | Raw configuration entries include invalid rows. Indices and token limits
-- retain the schema's number domain; summaries need not contain valid model IDs.
data CustomModelSummary = CustomModelSummary
  { customModelRawIndex :: !Scientific,
    customModelId :: !Text,
    customModelDisplayName :: !(Maybe Text),
    customModelProvider :: !Text,
    customModelBaseUrl :: !(Maybe Text),
    customModelHasApiKey :: !Bool,
    customModelApiKeyMask :: !(Maybe Text),
    customModelMaxOutputTokens :: !(Maybe Scientific),
    customModelNoImageSupport :: !(Maybe Bool),
    customModelHasBedrockConfig :: !Bool,
    customModelIsValid :: !Bool,
    customModelAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CustomModelSummary where
  show _ = "CustomModelSummary <redacted>"

instance FromJSON CustomModelSummary where
  parseJSON = withObject "CustomModelSummary" $ \fields ->
    CustomModelSummary <$> fields .: "rawIndex" <*> fields .: "model" <*> fields .:! "displayName" <*> fields .: "provider" <*> fields .:! "baseUrl" <*> fields .: "hasApiKey" <*> fields .:! "apiKeyMask" <*> fields .:! "maxOutputTokens" <*> fields .:! "noImageSupport" <*> fields .: "hasBedrockConfig" <*> fields .: "isValid" <*> pure (additionalFields customModelKeys fields)

instance ToJSON CustomModelSummary where
  toJSON value = objectWithAdditionalFields customModelKeys (customModelAdditionalFields value) (["rawIndex" .= customModelRawIndex value, "model" .= customModelId value, "provider" .= customModelProvider value, "hasApiKey" .= customModelHasApiKey value, "hasBedrockConfig" .= customModelHasBedrockConfig value, "isValid" .= customModelIsValid value] <> optionalField "displayName" (customModelDisplayName value) <> optionalField "baseUrl" (customModelBaseUrl value) <> optionalField "apiKeyMask" (customModelApiKeyMask value) <> optionalField "maxOutputTokens" (customModelMaxOutputTokens value) <> optionalField "noImageSupport" (customModelNoImageSupport value))

-- | Index absence creates an entry; presence edits one. The daemon applies the
-- optional expected-model guard. Omitted keys are not replaced locally.
data UpsertCustomModelParams = UpsertCustomModelParams
  { upsertCustomModelIndex :: !(Maybe Scientific),
    upsertCustomModelExpectedModel :: !(Maybe Text),
    upsertCustomModelId :: !NonEmptyText,
    upsertCustomModelDisplayName :: !(Maybe Text),
    upsertCustomModelProvider :: !NonEmptyText,
    upsertCustomModelBaseUrl :: !(Maybe Text),
    upsertCustomModelApiKey :: !(Maybe Text),
    upsertCustomModelMaxOutputTokens :: !(Maybe (Maybe Scientific)),
    upsertCustomModelNoImageSupport :: !(Maybe (Maybe Bool)),
    upsertCustomModelAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpsertCustomModelParams where
  show _ = "UpsertCustomModelParams <redacted>"

defaultUpsertCustomModelParams :: NonEmptyText -> NonEmptyText -> UpsertCustomModelParams
defaultUpsertCustomModelParams model provider = UpsertCustomModelParams Nothing Nothing model Nothing provider Nothing Nothing Nothing Nothing mempty

instance FromJSON UpsertCustomModelParams where
  parseJSON = withObject "UpsertCustomModelParams" $ \fields ->
    UpsertCustomModelParams <$> fields .:! "rawIndex" <*> fields .:! "expectedModel" <*> fields .: "model" <*> fields .:! "displayName" <*> fields .: "provider" <*> fields .:! "baseUrl" <*> fields .:! "apiKey" <*> fields .:! "maxOutputTokens" <*> fields .:! "noImageSupport" <*> pure (additionalFields customModelUpsertKeys fields)

instance ToJSON UpsertCustomModelParams where
  toJSON value = objectWithAdditionalFields customModelUpsertKeys (upsertCustomModelAdditionalFields value) (["model" .= upsertCustomModelId value, "provider" .= upsertCustomModelProvider value] <> optionalField "rawIndex" (upsertCustomModelIndex value) <> optionalField "expectedModel" (upsertCustomModelExpectedModel value) <> optionalField "displayName" (upsertCustomModelDisplayName value) <> optionalField "baseUrl" (upsertCustomModelBaseUrl value) <> optionalField "apiKey" (upsertCustomModelApiKey value) <> optionalField "maxOutputTokens" (upsertCustomModelMaxOutputTokens value) <> optionalField "noImageSupport" (upsertCustomModelNoImageSupport value))

data DeleteCustomModelParams = DeleteCustomModelParams
  { deletedCustomModelIndex :: !Scientific,
    deletedCustomModelExpectedModel :: !Text,
    deletedCustomModelAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show DeleteCustomModelParams where
  show _ = "DeleteCustomModelParams <redacted>"

instance FromJSON DeleteCustomModelParams where
  parseJSON = withObject "DeleteCustomModelParams" $ \fields ->
    DeleteCustomModelParams <$> fields .: "rawIndex" <*> fields .: "expectedModel" <*> pure (additionalFields ["rawIndex", "expectedModel"] fields)

instance ToJSON DeleteCustomModelParams where
  toJSON value = objectWithAdditionalFields ["rawIndex", "expectedModel"] (deletedCustomModelAdditionalFields value) ["rawIndex" .= deletedCustomModelIndex value, "expectedModel" .= deletedCustomModelExpectedModel value]

data ListCustomModelsResult = ListCustomModelsResult
  { listedCustomModels :: ![CustomModelSummary],
    listCustomModelsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListCustomModelsResult where
  show _ = "ListCustomModelsResult <redacted>"

instance FromJSON ListCustomModelsResult where
  parseJSON = withObject "ListCustomModelsResult" $ \fields -> ListCustomModelsResult <$> fields .: "models" <*> pure (additionalFields ["models"] fields)

instance ToJSON ListCustomModelsResult where
  toJSON value = objectWithAdditionalFields ["models"] (listCustomModelsAdditionalFields value) ["models" .= listedCustomModels value]

-- | The shared upsert/delete result. Success and returned rows are peer data;
-- no optimistic configuration, key merging or local filtering is performed.
data UpdateCustomModelsResult = UpdateCustomModelsResult
  { customModelsUpdateSuccess :: !Bool,
    updatedCustomModels :: ![CustomModelSummary],
    customModelsUpdateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateCustomModelsResult where
  show _ = "UpdateCustomModelsResult <redacted>"

instance FromJSON UpdateCustomModelsResult where
  parseJSON = withObject "UpdateCustomModelsResult" $ \fields -> UpdateCustomModelsResult <$> fields .: "success" <*> fields .: "models" <*> pure (additionalFields ["success", "models"] fields)

instance ToJSON UpdateCustomModelsResult where
  toJSON value = objectWithAdditionalFields ["success", "models"] (customModelsUpdateAdditionalFields value) ["success" .= customModelsUpdateSuccess value, "models" .= updatedCustomModels value]

customModelKeys, customModelUpsertKeys :: [Key]
customModelKeys = ["rawIndex", "model", "displayName", "provider", "baseUrl", "hasApiKey", "apiKeyMask", "maxOutputTokens", "noImageSupport", "hasBedrockConfig", "isValid"]
customModelUpsertKeys = ["rawIndex", "expectedModel", "model", "displayName", "provider", "baseUrl", "apiKey", "maxOutputTokens", "noImageSupport"]

data SubagentTier = LightSubagent | MediumSubagent | HeavySubagent
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SubagentTier where
  parseJSON = withText "SubagentTier" $ \case
    "light" -> pure LightSubagent
    "medium" -> pure MediumSubagent
    "heavy" -> pure HeavySubagent
    _ -> fail "Unknown subagent tier"

instance ToJSON SubagentTier where
  toJSON LightSubagent = String "light"
  toJSON MediumSubagent = String "medium"
  toJSON HeavySubagent = String "heavy"

data SubagentAutonomy = InheritSubagentAutonomy | SetSubagentAutonomy !AutonomyLevel
  deriving stock (Eq, Show)

instance FromJSON SubagentAutonomy where
  parseJSON (String "inherit") = pure InheritSubagentAutonomy
  parseJSON value = SetSubagentAutonomy <$> parseJSON value

instance ToJSON SubagentAutonomy where
  toJSON InheritSubagentAutonomy = String "inherit"
  toJSON (SetSubagentAutonomy value) = toJSON value

data LegacyCompactionMode = CurrentModelCompaction | FactoryDefaultCompaction
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON LegacyCompactionMode where
  parseJSON = withText "LegacyCompactionMode" $ \case
    "current-model" -> pure CurrentModelCompaction
    "factory-default" -> pure FactoryDefaultCompaction
    _ -> fail "Unknown legacy compaction mode"

instance ToJSON LegacyCompactionMode where
  toJSON CurrentModelCompaction = String "current-model"
  toJSON FactoryDefaultCompaction = String "factory-default"

data SubagentModelSettings = SubagentModelSettings
  { subagentLightModel :: !(Maybe Text),
    subagentLightReasoning :: !(Maybe ReasoningEffort),
    subagentMediumModel :: !(Maybe Text),
    subagentMediumReasoning :: !(Maybe ReasoningEffort),
    subagentHeavyModel :: !(Maybe Text),
    subagentHeavyReasoning :: !(Maybe ReasoningEffort),
    subagentModelAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SubagentModelSettings where
  show _ = "SubagentModelSettings <redacted>"

emptySubagentModelSettings :: SubagentModelSettings
emptySubagentModelSettings = SubagentModelSettings Nothing Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON SubagentModelSettings where
  parseJSON = withObject "SubagentModelSettings" $ \fields -> SubagentModelSettings <$> fields .:! "lightModel" <*> fields .:! "lightReasoningEffort" <*> fields .:! "mediumModel" <*> fields .:! "mediumReasoningEffort" <*> fields .:! "heavyModel" <*> fields .:! "heavyReasoningEffort" <*> pure (additionalFields subagentKeys fields)

instance ToJSON SubagentModelSettings where
  toJSON value = objectWithAdditionalFields subagentKeys (subagentModelAdditionalFields value) (optionalField "lightModel" (subagentLightModel value) <> optionalField "lightReasoningEffort" (subagentLightReasoning value) <> optionalField "mediumModel" (subagentMediumModel value) <> optionalField "mediumReasoningEffort" (subagentMediumReasoning value) <> optionalField "heavyModel" (subagentHeavyModel value) <> optionalField "heavyReasoningEffort" (subagentHeavyReasoning value))

-- | Source is required but nullable; disabled is data, not local enforcement.
data SettingsManagementInfo = SettingsManagementInfo
  { managementDisabled :: !Bool,
    managementSource :: !(Maybe SettingsLevel),
    managementFolderPath :: !(Maybe Text),
    managementInfoAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SettingsManagementInfo where
  show _ = "SettingsManagementInfo <redacted>"

instance FromJSON SettingsManagementInfo where
  parseJSON = withObject "SettingsManagementInfo" $ \fields -> SettingsManagementInfo <$> fields .: "disabled" <*> fields .: "source" <*> fields .:! "folderPath" <*> pure (additionalFields ["disabled", "source", "folderPath"] fields)

instance ToJSON SettingsManagementInfo where
  toJSON value = objectWithAdditionalFields ["disabled", "source", "folderPath"] (managementInfoAdditionalFields value) (["disabled" .= managementDisabled value, "source" .= managementSource value] <> optionalField "folderPath" (managementFolderPath value))

-- | Only declared setting keys are used in these maps. Invalid known entries
-- disappear; invalid present maps become empty, distinct from missing maps.
data DefaultSettingsManagement = DefaultSettingsManagement
  { managedDefaults :: !(KeyMap SettingsManagementInfo),
    managedSubagentDefaults :: !(Maybe (KeyMap SettingsManagementInfo)),
    managedMissionDefaults :: !(Maybe (KeyMap SettingsManagementInfo))
  }
  deriving stock (Eq)

instance Show DefaultSettingsManagement where
  show _ = "DefaultSettingsManagement <redacted>"

instance FromJSON DefaultSettingsManagement where
  parseJSON (Object fields) = pure (DefaultSettingsManagement (managementFields managementKeys (Object fields)) (managementFields subagentKeys <$> KeyMap.lookup "subagent" fields) (managementFields missionManagementKeys <$> KeyMap.lookup "mission" fields))
  parseJSON _ = pure (DefaultSettingsManagement mempty Nothing Nothing)

instance ToJSON DefaultSettingsManagement where
  toJSON value = Object (KeyMap.union (managementPairs managementKeys (managedDefaults value)) (KeyMap.fromList (optionalField "subagent" (Object . managementPairs subagentKeys <$> managedSubagentDefaults value) <> optionalField "mission" (Object . managementPairs missionManagementKeys <$> managedMissionDefaults value))))

managementFields :: [Key] -> Value -> KeyMap SettingsManagementInfo
managementFields keys (Object fields) = KeyMap.fromList [(key, value) | key <- keys, Just raw <- [KeyMap.lookup key fields], Just value <- [parseMaybe parseJSON raw]]
managementFields _ _ = mempty

managementPairs :: [Key] -> KeyMap SettingsManagementInfo -> Object
managementPairs keys = fmap toJSON . KeyMap.filterWithKey (\key _ -> key `elem` keys)

data ResolutionAction = ResolutionSet | ResolutionOverride | ResolutionSkip | ResolutionFallback
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ResolutionAction where
  parseJSON = withText "ResolutionAction" $ \case
    "set" -> pure ResolutionSet
    "override" -> pure ResolutionOverride
    "skip" -> pure ResolutionSkip
    "fallback" -> pure ResolutionFallback
    _ -> fail "Unknown settings resolution action"

instance ToJSON ResolutionAction where
  toJSON ResolutionSet = String "set"
  toJSON ResolutionOverride = String "override"
  toJSON ResolutionSkip = String "skip"
  toJSON ResolutionFallback = String "fallback"

data ResolutionSourceType = ResolutionBuiltinDefault | ResolutionDynamicConfig | ResolutionOrg | ResolutionUser | ResolutionProject | ResolutionFolder | ResolutionFeatureFlag | ResolutionLocalStorage | ResolutionNavState | ResolutionOrchestratorOverride | ResolutionAutoSelect | ResolutionSessionState
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ResolutionSourceType where
  parseJSON = withText "ResolutionSourceType" $ \case
    "builtin-default" -> pure ResolutionBuiltinDefault
    "dynamic-config" -> pure ResolutionDynamicConfig
    "org" -> pure ResolutionOrg
    "user" -> pure ResolutionUser
    "project" -> pure ResolutionProject
    "folder" -> pure ResolutionFolder
    "feature-flag" -> pure ResolutionFeatureFlag
    "localstorage" -> pure ResolutionLocalStorage
    "nav-state" -> pure ResolutionNavState
    "orchestrator-override" -> pure ResolutionOrchestratorOverride
    "auto-select" -> pure ResolutionAutoSelect
    "session-state" -> pure ResolutionSessionState
    _ -> fail "Unknown settings resolution source"

instance ToJSON ResolutionSourceType where
  toJSON ResolutionBuiltinDefault = String "builtin-default"
  toJSON ResolutionDynamicConfig = String "dynamic-config"
  toJSON ResolutionOrg = String "org"
  toJSON ResolutionUser = String "user"
  toJSON ResolutionProject = String "project"
  toJSON ResolutionFolder = String "folder"
  toJSON ResolutionFeatureFlag = String "feature-flag"
  toJSON ResolutionLocalStorage = String "localstorage"
  toJSON ResolutionNavState = String "nav-state"
  toJSON ResolutionOrchestratorOverride = String "orchestrator-override"
  toJSON ResolutionAutoSelect = String "auto-select"
  toJSON ResolutionSessionState = String "session-state"

data SettingsResolutionSource = SettingsResolutionSource
  { resolutionSourceType :: !ResolutionSourceType,
    resolutionSourceFilePath :: !(Maybe Text),
    resolutionSourceFlagName :: !(Maybe Text),
    resolutionSourceKey :: !(Maybe Text),
    resolutionSourceOrgId :: !(Maybe Text),
    resolutionSourceAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SettingsResolutionSource where
  show _ = "SettingsResolutionSource <redacted>"

instance FromJSON SettingsResolutionSource where
  parseJSON = withObject "SettingsResolutionSource" $ \fields -> SettingsResolutionSource <$> fields .: "type" <*> fields .:! "filePath" <*> fields .:! "flagName" <*> fields .:! "key" <*> fields .:! "orgId" <*> pure (additionalFields resolutionSourceKeys fields)

instance ToJSON SettingsResolutionSource where
  toJSON value = objectWithAdditionalFields resolutionSourceKeys (resolutionSourceAdditionalFields value) (["type" .= resolutionSourceType value] <> optionalField "filePath" (resolutionSourceFilePath value) <> optionalField "flagName" (resolutionSourceFlagName value) <> optionalField "key" (resolutionSourceKey value) <> optionalField "orgId" (resolutionSourceOrgId value))

data SettingsResolutionLocation = SettingsResolutionLocation
  { resolutionPackage :: !Text,
    resolutionFile :: !Text,
    resolutionFunction :: !(Maybe Text),
    resolutionLocationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SettingsResolutionLocation where
  show _ = "SettingsResolutionLocation <redacted>"

instance FromJSON SettingsResolutionLocation where
  parseJSON = withObject "SettingsResolutionLocation" $ \fields -> SettingsResolutionLocation <$> fields .: "package" <*> fields .: "file" <*> fields .:! "function" <*> pure (additionalFields ["package", "file", "function"] fields)

instance ToJSON SettingsResolutionLocation where
  toJSON value = objectWithAdditionalFields ["package", "file", "function"] (resolutionLocationAdditionalFields value) (["package" .= resolutionPackage value, "file" .= resolutionFile value] <> optionalField "function" (resolutionFunction value))

data SettingsResolutionEvent = SettingsResolutionEvent
  { resolutionTimestamp :: !Text,
    resolutionKeys :: ![Text],
    resolutionAction :: !ResolutionAction,
    resolutionSource :: !SettingsResolutionSource,
    resolutionValue :: !(Maybe Object),
    resolutionReason :: !(Maybe Text),
    resolutionLocation :: !(Maybe SettingsResolutionLocation),
    resolutionAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SettingsResolutionEvent where
  show _ = "SettingsResolutionEvent <redacted>"

instance FromJSON SettingsResolutionEvent where
  parseJSON = withObject "SettingsResolutionEvent" $ \fields -> SettingsResolutionEvent <$> fields .: "timestamp" <*> fields .: "keys" <*> fields .: "action" <*> fields .: "source" <*> fields .:! "value" <*> fields .:! "reason" <*> fields .:! "location" <*> pure (additionalFields resolutionEventKeys fields)

instance ToJSON SettingsResolutionEvent where
  toJSON value = objectWithAdditionalFields resolutionEventKeys (resolutionAdditionalFields value) (["timestamp" .= resolutionTimestamp value, "keys" .= resolutionKeys value, "action" .= resolutionAction value, "source" .= resolutionSource value] <> optionalField "value" (resolutionValue value) <> optionalField "reason" (resolutionReason value) <> optionalField "location" (resolutionLocation value))

data SpecSavePresets = SpecSavePresets
  { presetUserFactoryDirectory :: !Text,
    presetProjectFactoryDirectory :: !(Maybe Text),
    specPresetsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SpecSavePresets where
  show _ = "SpecSavePresets <redacted>"

instance FromJSON SpecSavePresets where
  parseJSON = withObject "SpecSavePresets" $ \fields -> SpecSavePresets <$> fields .: "userFactoryDir" <*> fields .:! "projectFactoryDir" <*> pure (additionalFields ["userFactoryDir", "projectFactoryDir"] fields)

instance ToJSON SpecSavePresets where
  toJSON value = objectWithAdditionalFields ["userFactoryDir", "projectFactoryDir"] (specPresetsAdditionalFields value) (["userFactoryDir" .= presetUserFactoryDirectory value] <> optionalField "projectFactoryDir" (presetProjectFactoryDirectory value))

data DefaultSettings = DefaultSettings
  { defaultsLegacyMode :: !(Maybe LegacyAutonomyMode),
    defaultsMode :: !(Maybe DroidInteractionMode),
    defaultsAutonomy :: !(Maybe AutonomyLevel),
    defaultsMaxAutonomy :: !(Maybe AutonomyLevel),
    defaultsAvailableAutonomy :: !(Maybe [AutonomyLevel]),
    defaultsModel :: !(Maybe Text),
    defaultsReasoning :: !(Maybe ReasoningEffort),
    defaultsSpecSaveDirectory :: !(Maybe Text),
    defaultsSpecModel :: !(Maybe Text),
    defaultsSpecReasoning :: !(Maybe ReasoningEffort),
    defaultsCompactionLimit :: !(Maybe Scientific),
    defaultsPerModelCompactionLimits :: !(Maybe (KeyMap Scientific)),
    defaultsCompactionModel :: !(Maybe CompactionModel),
    defaultsCompactionThresholdEnabled :: !(Maybe Bool),
    defaultsLegacyCompactionMode :: !(Maybe LegacyCompactionMode),
    defaultsCloudSync :: !(Maybe Bool),
    defaultsRunInWorktree :: !(Maybe Bool),
    defaultsWorktreeDirectory :: !(Maybe Text),
    defaultsManagement :: !(Maybe DefaultSettingsManagement),
    defaultsSubagentAutonomy :: !(Maybe SubagentAutonomy),
    defaultsMissionOrchestratorModel :: !(Maybe Text),
    defaultsMissionOrchestratorReasoning :: !(Maybe ReasoningEffort),
    defaultsMission :: !(Maybe MissionModelSettings),
    defaultsSubagentModels :: !(Maybe SubagentModelSettings),
    defaultsAvailableModels :: !(Maybe [ModelInfo]),
    defaultsSpecSavePresets :: !(Maybe SpecSavePresets),
    defaultsResolutionChain :: !(Maybe [SettingsResolutionEvent]),
    defaultsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show DefaultSettings where
  show _ = "DefaultSettings <redacted>"

instance FromJSON DefaultSettings where
  parseJSON = withObject "DefaultSettings" $ \fields ->
    DefaultSettings <$> fields .:! "autonomyMode" <*> reportField fields "interactionMode" <*> reportField fields "autonomyLevel" <*> reportField fields "maxAutonomyLevel" <*> reportField fields "availableAutonomyLevels" <*> fields .:! "modelId" <*> fields .:! "reasoningEffort" <*> fields .:! "specSaveDir" <*> fields .:! "specModeModelId" <*> fields .:! "specModeReasoningEffort" <*> fields .:! "compactionTokenLimit" <*> fields .:! "compactionTokenLimitPerModel" <*> fields .:! "compactionModel" <*> fields .:! "compactionThresholdCheckEnabled" <*> fields .:! "compactionModelMode" <*> fields .:! "cloudSessionSync" <*> fields .:! "runInWorktree" <*> fields .:! "worktreeDirectory" <*> fields .:! "management" <*> fields .:! "subagentAutonomyLevel" <*> fields .:! "missionOrchestratorModel" <*> fields .:! "missionOrchestratorReasoningEffort" <*> fields .:! "missionSettings" <*> fields .:! "subagentModelSettings" <*> fields .:! "availableModels" <*> fields .:! "specSavePresets" <*> reportField fields "resolutionChain" <*> pure (additionalFields defaultsKeys fields)

instance ToJSON DefaultSettings where
  toJSON value = objectWithAdditionalFields defaultsKeys (defaultsAdditionalFields value) (optionalField "autonomyMode" (defaultsLegacyMode value) <> optionalField "interactionMode" (defaultsMode value) <> optionalField "autonomyLevel" (defaultsAutonomy value) <> optionalField "maxAutonomyLevel" (defaultsMaxAutonomy value) <> optionalField "availableAutonomyLevels" (defaultsAvailableAutonomy value) <> optionalField "modelId" (defaultsModel value) <> optionalField "reasoningEffort" (defaultsReasoning value) <> optionalField "specSaveDir" (defaultsSpecSaveDirectory value) <> optionalField "specModeModelId" (defaultsSpecModel value) <> optionalField "specModeReasoningEffort" (defaultsSpecReasoning value) <> optionalField "compactionTokenLimit" (defaultsCompactionLimit value) <> optionalField "compactionTokenLimitPerModel" (defaultsPerModelCompactionLimits value) <> optionalField "compactionModel" (defaultsCompactionModel value) <> optionalField "compactionThresholdCheckEnabled" (defaultsCompactionThresholdEnabled value) <> optionalField "compactionModelMode" (defaultsLegacyCompactionMode value) <> optionalField "cloudSessionSync" (defaultsCloudSync value) <> optionalField "runInWorktree" (defaultsRunInWorktree value) <> optionalField "worktreeDirectory" (defaultsWorktreeDirectory value) <> optionalField "management" (defaultsManagement value) <> optionalField "subagentAutonomyLevel" (defaultsSubagentAutonomy value) <> optionalField "missionOrchestratorModel" (defaultsMissionOrchestratorModel value) <> optionalField "missionOrchestratorReasoningEffort" (defaultsMissionOrchestratorReasoning value) <> optionalField "missionSettings" (defaultsMission value) <> optionalField "subagentModelSettings" (defaultsSubagentModels value) <> optionalField "availableModels" (defaultsAvailableModels value) <> optionalField "specSavePresets" (defaultsSpecSavePresets value) <> optionalField "resolutionChain" (defaultsResolutionChain value))

-- | Nothing omits a field; nested Just Nothing sends an explicit reset. Tier
-- reset arrays preserve order/duplicates; no inheritance is computed locally.
data UpdateSessionDefaultsParams = UpdateSessionDefaultsParams
  { updateDefaultsModel :: !(Maybe Text),
    updateDefaultsReasoning :: !(Maybe ReasoningEffort),
    updateDefaultsMode :: !(Maybe DroidInteractionMode),
    updateDefaultsAutonomy :: !(Maybe AutonomyLevel),
    updateDefaultsSpecModel :: !(Maybe (Maybe Text)),
    updateDefaultsSpecReasoning :: !(Maybe (Maybe ReasoningEffort)),
    updateDefaultsCompactionLimit :: !(Maybe Scientific),
    updateDefaultsPerModelCompactionLimits :: !(Maybe (KeyMap Scientific)),
    updateDefaultsCompactionModel :: !(Maybe CompactionModel),
    updateDefaultsCompactionThresholdEnabled :: !(Maybe Bool),
    updateDefaultsLegacyCompactionMode :: !(Maybe LegacyCompactionMode),
    updateDefaultsCloudSync :: !(Maybe Bool),
    updateDefaultsSubagentModels :: !(Maybe SubagentModelSettings),
    updateDefaultsSubagentInheritTiers :: !(Maybe [SubagentTier]),
    updateDefaultsSubagentAutonomy :: !(Maybe (Maybe SubagentAutonomy)),
    updateDefaultsSpecSaveDirectory :: !(Maybe (Maybe Text)),
    updateDefaultsMissionOrchestratorModel :: !(Maybe (Maybe Text)),
    updateDefaultsMissionOrchestratorReasoning :: !(Maybe (Maybe ReasoningEffort)),
    updateDefaultsMission :: !(Maybe MissionModelSettings),
    updateDefaultsRunInWorktree :: !(Maybe (Maybe Bool)),
    updateDefaultsWorktreeDirectory :: !(Maybe (Maybe Text)),
    updateDefaultsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateSessionDefaultsParams where
  show _ = "UpdateSessionDefaultsParams <redacted>"

emptySessionDefaultsUpdate :: UpdateSessionDefaultsParams
emptySessionDefaultsUpdate = UpdateSessionDefaultsParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON UpdateSessionDefaultsParams where
  parseJSON = withObject "UpdateSessionDefaultsParams" $ \fields -> UpdateSessionDefaultsParams <$> fields .:! "modelId" <*> fields .:! "reasoningEffort" <*> fields .:! "interactionMode" <*> fields .:! "autonomyLevel" <*> fields .:! "specModeModelId" <*> fields .:! "specModeReasoningEffort" <*> fields .:! "compactionTokenLimit" <*> fields .:! "compactionTokenLimitPerModel" <*> fields .:! "compactionModel" <*> fields .:! "compactionThresholdCheckEnabled" <*> fields .:! "compactionModelMode" <*> fields .:! "cloudSessionSync" <*> fields .:! "subagentModelSettings" <*> fields .:! "subagentInheritTiers" <*> fields .:! "subagentAutonomyLevel" <*> fields .:! "specSaveDir" <*> fields .:! "missionOrchestratorModel" <*> fields .:! "missionOrchestratorReasoningEffort" <*> fields .:! "missionModelSettings" <*> fields .:! "runInWorktree" <*> fields .:! "worktreeDirectory" <*> pure (additionalFields updateKeys fields)

instance ToJSON UpdateSessionDefaultsParams where
  toJSON value = objectWithAdditionalFields updateKeys (updateDefaultsAdditionalFields value) (optionalField "modelId" (updateDefaultsModel value) <> optionalField "reasoningEffort" (updateDefaultsReasoning value) <> optionalField "interactionMode" (updateDefaultsMode value) <> optionalField "autonomyLevel" (updateDefaultsAutonomy value) <> optionalField "specModeModelId" (updateDefaultsSpecModel value) <> optionalField "specModeReasoningEffort" (updateDefaultsSpecReasoning value) <> optionalField "compactionTokenLimit" (updateDefaultsCompactionLimit value) <> optionalField "compactionTokenLimitPerModel" (updateDefaultsPerModelCompactionLimits value) <> optionalField "compactionModel" (updateDefaultsCompactionModel value) <> optionalField "compactionThresholdCheckEnabled" (updateDefaultsCompactionThresholdEnabled value) <> optionalField "compactionModelMode" (updateDefaultsLegacyCompactionMode value) <> optionalField "cloudSessionSync" (updateDefaultsCloudSync value) <> optionalField "subagentModelSettings" (updateDefaultsSubagentModels value) <> optionalField "subagentInheritTiers" (updateDefaultsSubagentInheritTiers value) <> optionalField "subagentAutonomyLevel" (updateDefaultsSubagentAutonomy value) <> optionalField "specSaveDir" (updateDefaultsSpecSaveDirectory value) <> optionalField "missionOrchestratorModel" (updateDefaultsMissionOrchestratorModel value) <> optionalField "missionOrchestratorReasoningEffort" (updateDefaultsMissionOrchestratorReasoning value) <> optionalField "missionModelSettings" (updateDefaultsMission value) <> optionalField "runInWorktree" (updateDefaultsRunInWorktree value) <> optionalField "worktreeDirectory" (updateDefaultsWorktreeDirectory value))

data UpdateSessionDefaultsResult = UpdateSessionDefaultsResult
  { defaultsUpdateSuccess :: !Bool,
    updatedDefaults :: !DefaultSettings,
    defaultsUpdateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateSessionDefaultsResult where
  show _ = "UpdateSessionDefaultsResult <redacted>"

instance FromJSON UpdateSessionDefaultsResult where
  parseJSON = withObject "UpdateSessionDefaultsResult" $ \fields -> UpdateSessionDefaultsResult <$> fields .: "success" <*> fields .: "defaults" <*> pure (additionalFields ["success", "defaults"] fields)

instance ToJSON UpdateSessionDefaultsResult where
  toJSON value = objectWithAdditionalFields ["success", "defaults"] (defaultsUpdateAdditionalFields value) ["success" .= defaultsUpdateSuccess value, "defaults" .= updatedDefaults value]

reportField :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
reportField fields key = pure (KeyMap.lookup key fields >>= parseMaybe parseJSON)

subagentKeys, managementKeys, missionManagementKeys, resolutionSourceKeys, resolutionEventKeys, defaultsKeys, updateKeys :: [Key]
subagentKeys = ["lightModel", "lightReasoningEffort", "mediumModel", "mediumReasoningEffort", "heavyModel", "heavyReasoningEffort"]
managementKeys = ["modelId", "reasoningEffort", "interactionMode", "autonomyLevel", "autonomyMode", "specModeModelId", "specModeReasoningEffort", "specSaveDir", "compactionTokenLimit", "compactionTokenLimitPerModel", "compactionModel", "compactionThresholdCheckEnabled", "enableOneHourAnthropicCaching", "compactionModelMode", "cloudSessionSync", "runInWorktree", "worktreeDirectory", "worktreeAutoDeleteLimit", "subagentAutonomyLevel"]
missionManagementKeys = ["orchestratorModel", "orchestratorReasoningEffort", "workerModel", "workerReasoningEffort", "validationWorkerModel", "validationWorkerReasoningEffort", "skipScrutiny", "skipUserTesting"]
resolutionSourceKeys = ["type", "filePath", "flagName", "key", "orgId"]
resolutionEventKeys = ["timestamp", "keys", "action", "source", "value", "reason", "location"]
defaultsKeys = ["autonomyMode", "interactionMode", "autonomyLevel", "maxAutonomyLevel", "availableAutonomyLevels", "modelId", "reasoningEffort", "specSaveDir", "specModeModelId", "specModeReasoningEffort", "compactionTokenLimit", "compactionTokenLimitPerModel", "compactionModel", "compactionThresholdCheckEnabled", "compactionModelMode", "cloudSessionSync", "runInWorktree", "worktreeDirectory", "management", "subagentAutonomyLevel", "missionOrchestratorModel", "missionOrchestratorReasoningEffort", "missionSettings", "subagentModelSettings", "availableModels", "specSavePresets", "resolutionChain"]
updateKeys = ["modelId", "reasoningEffort", "interactionMode", "autonomyLevel", "specModeModelId", "specModeReasoningEffort", "compactionTokenLimit", "compactionTokenLimitPerModel", "compactionModel", "compactionThresholdCheckEnabled", "compactionModelMode", "cloudSessionSync", "subagentModelSettings", "subagentInheritTiers", "subagentAutonomyLevel", "specSaveDir", "missionOrchestratorModel", "missionOrchestratorReasoningEffort", "missionModelSettings", "runInWorktree", "worktreeDirectory"]
