{-# LANGUAGE OverloadedStrings #-}

-- | Settings-update and hypothetical tool-query bodies. Shapes follow the
-- supplied 1.205.0 schema; invalid enum-field fallbacks use the selected CLI
-- 1.201.1 contract. Parsing does not change settings or grant tool permissions.
module Factory.Droid.Schema.Settings
  ( LegacyAutonomyMode (..),
    ToolPolicy (..),
    emptyToolPolicy,
    parseToolPolicy,
    toolPolicyFields,
    toolPolicyKeys,
    UpdateSessionSettingsParams (..),
    emptySettingsUpdate,
    ListToolsOptions (..),
    defaultListToolsOptions,
    SettingsChange (..),
    emptySettingsChange,
    SettingsUpdated (..),
    SessionSettings (..),
    hasDecoupledInteractionSettings,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField, requireLiteral)
import Factory.Droid.Schema.Enums (AutonomyLevel, DroidInteractionMode, ReasoningEffort, ToolExecutionMode)
import Factory.Droid.Schema.Models (MissionModelSettings)
import Factory.Droid.Schema.Session (SandboxStatus, SessionTag)
import Factory.Droid.Schema.SystemPrompt (SystemPromptConfig)
import Numeric.Natural (Natural)

-- | Raw presence inspection, including explicit null or otherwise invalid values.
-- Parsed settings and update/reset validation are separate contracts.
hasDecoupledInteractionSettings :: Object -> Bool
hasDecoupledInteractionSettings fields = KeyMap.member "interactionMode" fields || KeyMap.member "autonomyLevel" fields

-- | Deprecated wire mode; prefer interaction mode and autonomy level.
data LegacyAutonomyMode = LegacyNormal | LegacySpec | LegacyAutoLow | LegacyAutoMedium | LegacyAutoHigh
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON LegacyAutonomyMode where
  parseJSON = withText "LegacyAutonomyMode" $ \case
    "normal" -> pure LegacyNormal
    "spec" -> pure LegacySpec
    "auto-low" -> pure LegacyAutoLow
    "auto-medium" -> pure LegacyAutoMedium
    "auto-high" -> pure LegacyAutoHigh
    _ -> fail "Unknown legacy autonomy mode"

instance ToJSON LegacyAutonomyMode where
  toJSON LegacyNormal = String "normal"
  toJSON LegacySpec = String "spec"
  toJSON LegacyAutoLow = String "auto-low"
  toJSON LegacyAutoMedium = String "auto-medium"
  toJSON LegacyAutoHigh = String "auto-high"

-- | Independent tool-ID overrides. Nothing omits a field; Just [] sends an
-- explicit empty list. Spelling, order and duplicates are not normalized.
data ToolPolicy = ToolPolicy
  { policyAdditionalTools :: !(Maybe [Text]),
    policyEnabledTools :: !(Maybe [Text]),
    policyDisabledTools :: !(Maybe [Text]),
    policyRestrictedTools :: !(Maybe [Text])
  }
  deriving stock (Eq)

instance Show ToolPolicy where
  show _ = "ToolPolicy <redacted>"

emptyToolPolicy :: ToolPolicy
emptyToolPolicy = ToolPolicy Nothing Nothing Nothing Nothing

-- | Partial updates. Spec-model/reasoning fields distinguish Nothing (omit),
-- Just Nothing (clear) and Just (Just value) (set). Numeric limits retain the
-- peer's number domain; no local model registry or policy success is inferred.
data UpdateSessionSettingsParams = UpdateSessionSettingsParams
  { updateSettingsModel :: !(Maybe Text),
    updateSettingsReasoning :: !(Maybe ReasoningEffort),
    updateSettingsLegacyMode :: !(Maybe LegacyAutonomyMode),
    updateSettingsMode :: !(Maybe DroidInteractionMode),
    updateSettingsAutonomy :: !(Maybe AutonomyLevel),
    updateSettingsSpecModel :: !(Maybe (Maybe Text)),
    updateSettingsSpecReasoning :: !(Maybe (Maybe ReasoningEffort)),
    updateSettingsMission :: !(Maybe MissionModelSettings),
    updateSettingsTags :: !(Maybe [SessionTag]),
    updateSettingsCompactionTokenLimit :: !(Maybe Scientific),
    updateSettingsCompactionThresholdEnabled :: !(Maybe Bool),
    updateSettingsToolPolicy :: !ToolPolicy,
    updateSettingsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateSessionSettingsParams where
  show _ = "UpdateSessionSettingsParams <redacted>"

emptySettingsUpdate :: UpdateSessionSettingsParams
emptySettingsUpdate = UpdateSessionSettingsParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing emptyToolPolicy mempty

instance FromJSON UpdateSessionSettingsParams where
  parseJSON = withObject "UpdateSessionSettingsParams" $ \fields ->
    UpdateSessionSettingsParams
      <$> fields .:! "modelId"
      <*> fields .:! "reasoningEffort"
      <*> fields .:! "autonomyMode"
      <*> fallbackField fields "interactionMode"
      <*> fallbackField fields "autonomyLevel"
      <*> fields .:! "specModeModelId"
      <*> fields .:! "specModeReasoningEffort"
      <*> fields .:! "missionSettings"
      <*> fields .:! "tags"
      <*> fields .:! "compactionTokenLimit"
      <*> fields .:! "compactionThresholdCheckEnabled"
      <*> parseToolPolicy fields
      <*> pure (additionalFields settingsKeys fields)

instance ToJSON UpdateSessionSettingsParams where
  toJSON settings =
    objectWithAdditionalFields settingsKeys (updateSettingsAdditionalFields settings) $
      optionalField "modelId" (updateSettingsModel settings)
        <> optionalField "reasoningEffort" (updateSettingsReasoning settings)
        <> optionalField "autonomyMode" (updateSettingsLegacyMode settings)
        <> optionalField "interactionMode" (updateSettingsMode settings)
        <> optionalField "autonomyLevel" (updateSettingsAutonomy settings)
        <> optionalField "specModeModelId" (updateSettingsSpecModel settings)
        <> optionalField "specModeReasoningEffort" (updateSettingsSpecReasoning settings)
        <> optionalField "missionSettings" (updateSettingsMission settings)
        <> optionalField "tags" (updateSettingsTags settings)
        <> optionalField "compactionTokenLimit" (updateSettingsCompactionTokenLimit settings)
        <> optionalField "compactionThresholdCheckEnabled" (updateSettingsCompactionThresholdEnabled settings)
        <> toolPolicyFields (updateSettingsToolPolicy settings)

-- | Hypothetical catalog controls, not session mutations. The unsafe-permission
-- flag changes the query's allow-state calculation; it grants nothing locally.
data ListToolsOptions = ListToolsOptions
  { toolQueryModel :: !(Maybe Text),
    toolQueryLegacyMode :: !(Maybe LegacyAutonomyMode),
    toolQueryMode :: !(Maybe DroidInteractionMode),
    toolQueryAutonomy :: !(Maybe AutonomyLevel),
    toolQuerySpecModel :: !(Maybe (Maybe Text)),
    toolQueryPolicy :: !ToolPolicy,
    toolQuerySkipPermissionsUnsafe :: !(Maybe Bool),
    toolQueryDepth :: !(Maybe Natural),
    toolQueryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListToolsOptions where
  show _ = "ListToolsOptions <redacted>"

defaultListToolsOptions :: ListToolsOptions
defaultListToolsOptions = ListToolsOptions Nothing Nothing Nothing Nothing Nothing emptyToolPolicy Nothing Nothing mempty

instance FromJSON ListToolsOptions where
  parseJSON = withObject "ListToolsOptions" $ \fields ->
    ListToolsOptions
      <$> fields .:! "modelId"
      <*> fields .:! "autonomyMode"
      <*> fallbackField fields "interactionMode"
      <*> fallbackField fields "autonomyLevel"
      <*> fields .:! "specModeModelId"
      <*> parseToolPolicy fields
      <*> fields .:! "skipPermissionsUnsafe"
      <*> fields .:! "depth"
      <*> pure (additionalFields toolQueryKeys fields)

instance ToJSON ListToolsOptions where
  toJSON options =
    objectWithAdditionalFields toolQueryKeys (toolQueryAdditionalFields options) $
      optionalField "modelId" (toolQueryModel options)
        <> optionalField "autonomyMode" (toolQueryLegacyMode options)
        <> optionalField "interactionMode" (toolQueryMode options)
        <> optionalField "autonomyLevel" (toolQueryAutonomy options)
        <> optionalField "specModeModelId" (toolQuerySpecModel options)
        <> optionalField "skipPermissionsUnsafe" (toolQuerySkipPermissionsUnsafe options)
        <> optionalField "depth" (toolQueryDepth options)
        <> toolPolicyFields (toolQueryPolicy options)

-- | A reported partial settings update, not a complete settings snapshot.
-- Unlike request patches, reported spec overrides are optional but non-null.
data SettingsChange = SettingsChange
  { changedSettingsLegacyMode :: !(Maybe LegacyAutonomyMode),
    changedSettingsMode :: !(Maybe DroidInteractionMode),
    changedSettingsAutonomy :: !(Maybe AutonomyLevel),
    changedSettingsAvailableAutonomy :: !(Maybe [AutonomyLevel]),
    changedSettingsModel :: !(Maybe Text),
    changedSettingsReasoning :: !(Maybe ReasoningEffort),
    changedSettingsSpecModel :: !(Maybe Text),
    changedSettingsSpecReasoning :: !(Maybe ReasoningEffort),
    changedSettingsMission :: !(Maybe MissionModelSettings),
    changedSettingsTags :: !(Maybe [SessionTag]),
    changedSettingsCompactionThresholdEnabled :: !(Maybe Bool),
    changedSettingsToolPolicy :: !ToolPolicy,
    changedSettingsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SettingsChange where
  show _ = "SettingsChange <redacted>"

emptySettingsChange :: SettingsChange
emptySettingsChange = SettingsChange Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing emptyToolPolicy mempty

instance FromJSON SettingsChange where
  parseJSON = withObject "SettingsChange" $ \fields ->
    SettingsChange
      <$> fields .:! "autonomyMode"
      <*> fallbackField fields "interactionMode"
      <*> fallbackField fields "autonomyLevel"
      <*> fallbackField fields "availableAutonomyLevels"
      <*> fields .:! "modelId"
      <*> fields .:! "reasoningEffort"
      <*> fields .:! "specModeModelId"
      <*> fields .:! "specModeReasoningEffort"
      <*> fields .:! "missionSettings"
      <*> fields .:! "tags"
      <*> fields .:! "compactionThresholdCheckEnabled"
      <*> parseToolPolicy fields
      <*> pure (additionalFields changeKeys fields)

instance ToJSON SettingsChange where
  toJSON change =
    objectWithAdditionalFields changeKeys (changedSettingsAdditionalFields change) $
      optionalField "autonomyMode" (changedSettingsLegacyMode change)
        <> optionalField "interactionMode" (changedSettingsMode change)
        <> optionalField "autonomyLevel" (changedSettingsAutonomy change)
        <> optionalField "availableAutonomyLevels" (changedSettingsAvailableAutonomy change)
        <> optionalField "modelId" (changedSettingsModel change)
        <> optionalField "reasoningEffort" (changedSettingsReasoning change)
        <> optionalField "specModeModelId" (changedSettingsSpecModel change)
        <> optionalField "specModeReasoningEffort" (changedSettingsSpecReasoning change)
        <> optionalField "missionSettings" (changedSettingsMission change)
        <> optionalField "tags" (changedSettingsTags change)
        <> optionalField "compactionThresholdCheckEnabled" (changedSettingsCompactionThresholdEnabled change)
        <> toolPolicyFields (changedSettingsToolPolicy change)

data SettingsUpdated = SettingsUpdated
  { settingsUpdateRequestId :: !(Maybe Text),
    settingsUpdateValues :: !SettingsChange,
    settingsUpdateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SettingsUpdated where
  show _ = "SettingsUpdated <redacted>"

instance FromJSON SettingsUpdated where
  parseJSON = withObject "SettingsUpdated" $ \fields -> do
    requireLiteral "type" (String "settings_updated") fields
    SettingsUpdated <$> fields .:! "requestId" <*> fields .: "settings" <*> pure (additionalFields ["type", "requestId", "settings"] fields)

instance ToJSON SettingsUpdated where
  toJSON event = objectWithAdditionalFields ["type", "requestId", "settings"] (settingsUpdateAdditionalFields event) (["type" .= String "settings_updated", "settings" .= settingsUpdateValues event] <> optionalField "requestId" (settingsUpdateRequestId event))

-- | Full initialize/load settings. Optional fields are peer observations, not
-- locally inferred defaults. Show redacts payloads; fields and JSON do not.
data SessionSettings = SessionSettings
  { settingsModel :: !Text,
    settingsReasoning :: !ReasoningEffort,
    settingsLegacyMode :: !(Maybe LegacyAutonomyMode),
    settingsMode :: !(Maybe DroidInteractionMode),
    settingsAutonomy :: !(Maybe AutonomyLevel),
    settingsAvailableAutonomy :: !(Maybe [AutonomyLevel]),
    settingsSpecModel :: !(Maybe Text),
    settingsSpecReasoning :: !(Maybe ReasoningEffort),
    settingsMission :: !(Maybe MissionModelSettings),
    settingsTags :: !(Maybe [SessionTag]),
    settingsSandbox :: !(Maybe SandboxStatus),
    settingsCompactionThresholdEnabled :: !(Maybe Bool),
    settingsToolExecutionMode :: !(Maybe ToolExecutionMode),
    settingsAllowSubagentsInScripts :: !(Maybe Bool),
    settingsSystemPrompt :: !(Maybe SystemPromptConfig),
    settingsToolPolicy :: !ToolPolicy,
    settingsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionSettings where
  show _ = "SessionSettings <redacted>"

instance FromJSON SessionSettings where
  parseJSON = withObject "SessionSettings" $ \fields ->
    SessionSettings
      <$> fields .: "modelId"
      <*> fields .: "reasoningEffort"
      <*> fields .:! "autonomyMode"
      <*> fallbackField fields "interactionMode"
      <*> fallbackField fields "autonomyLevel"
      <*> fallbackField fields "availableAutonomyLevels"
      <*> fields .:! "specModeModelId"
      <*> fields .:! "specModeReasoningEffort"
      <*> fields .:! "missionSettings"
      <*> fields .:! "tags"
      <*> fields .:! "sandbox"
      <*> fields .:! "compactionThresholdCheckEnabled"
      <*> fields .:! "toolExecutionMode"
      <*> fields .:! "allowSubagentsInScripts"
      <*> fields .:! "systemPrompt"
      <*> parseToolPolicy fields
      <*> pure (additionalFields snapshotKeys fields)

instance ToJSON SessionSettings where
  toJSON settings =
    objectWithAdditionalFields snapshotKeys (settingsAdditionalFields settings) $
      ["modelId" .= settingsModel settings, "reasoningEffort" .= settingsReasoning settings]
        <> optionalField "autonomyMode" (settingsLegacyMode settings)
        <> optionalField "interactionMode" (settingsMode settings)
        <> optionalField "autonomyLevel" (settingsAutonomy settings)
        <> optionalField "availableAutonomyLevels" (settingsAvailableAutonomy settings)
        <> optionalField "specModeModelId" (settingsSpecModel settings)
        <> optionalField "specModeReasoningEffort" (settingsSpecReasoning settings)
        <> optionalField "missionSettings" (settingsMission settings)
        <> optionalField "tags" (settingsTags settings)
        <> optionalField "sandbox" (settingsSandbox settings)
        <> optionalField "compactionThresholdCheckEnabled" (settingsCompactionThresholdEnabled settings)
        <> optionalField "toolExecutionMode" (settingsToolExecutionMode settings)
        <> optionalField "allowSubagentsInScripts" (settingsAllowSubagentsInScripts settings)
        <> optionalField "systemPrompt" (settingsSystemPrompt settings)
        <> toolPolicyFields (settingsToolPolicy settings)

snapshotKeys :: [Key]
snapshotKeys = changeKeys <> ["sandbox", "toolExecutionMode", "allowSubagentsInScripts", "systemPrompt"]

changeKeys :: [Key]
changeKeys = ["autonomyMode", "interactionMode", "autonomyLevel", "availableAutonomyLevels", "modelId", "reasoningEffort", "specModeModelId", "specModeReasoningEffort", "missionSettings", "tags", "compactionThresholdCheckEnabled"] <> toolPolicyKeys

fallbackField :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
fallbackField fields key = fields .:! key <|> pure Nothing

parseToolPolicy :: Object -> Parser ToolPolicy
parseToolPolicy fields = ToolPolicy <$> fields .:! "additionalToolIds" <*> fields .:! "enabledToolIds" <*> fields .:! "disabledToolIds" <*> fields .:! "restrictToolIds"

toolPolicyFields :: ToolPolicy -> [Pair]
toolPolicyFields policy = optionalField "additionalToolIds" (policyAdditionalTools policy) <> optionalField "enabledToolIds" (policyEnabledTools policy) <> optionalField "disabledToolIds" (policyDisabledTools policy) <> optionalField "restrictToolIds" (policyRestrictedTools policy)

toolPolicyKeys, settingsKeys, toolQueryKeys :: [Key]
toolPolicyKeys = ["additionalToolIds", "enabledToolIds", "disabledToolIds", "restrictToolIds"]
settingsKeys = ["modelId", "reasoningEffort", "autonomyMode", "interactionMode", "autonomyLevel", "specModeModelId", "specModeReasoningEffort", "missionSettings", "tags", "compactionTokenLimit", "compactionThresholdCheckEnabled"] <> toolPolicyKeys
toolQueryKeys = ["modelId", "autonomyMode", "interactionMode", "autonomyLevel", "specModeModelId", "skipPermissionsUnsafe", "depth"] <> toolPolicyKeys
