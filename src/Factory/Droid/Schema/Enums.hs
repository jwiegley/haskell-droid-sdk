{-# LANGUAGE OverloadedStrings #-}

-- | Shared wire enumerations from the Factory protocol 1.205.0 schema.
--
-- JSON instances use the protocol's literal spellings and reject unknown
-- values. Invalid-value fallbacks on particular request fields belong to
-- those fields' codecs, not to these enumeration instances.
module Factory.Droid.Schema.Enums
  ( AutonomyLevel (..),
    CustomModelAuthMode (..),
    DroidInteractionMode (..),
    FileEditToolProfile (..),
    MessageRole (..),
    MessageVisibility (..),
    ModelFallbackReason (..),
    ModelProvider (..),
    ReasoningEffort (..),
    SandboxMode (..),
    SessionOrigin (..),
    SettingsLevel (..),
    SkillLocation (..),
    ToolExecutionMode (..),
    WorktreeLifecycle (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options,
    ToJSON (..),
    Value (String),
    camelTo2,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    withText,
  )
import Data.Char (toLower)
import Factory.Droid.Internal.JSON (enumOptions)
import GHC.Generics (Generic)

-- | The requested autonomy level; tool availability is a separate policy.
data AutonomyLevel = AutonomyOff | AutonomyLow | AutonomyMedium | AutonomyHigh
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON AutonomyLevel where
  toJSON = genericToJSON autonomyOptions
  toEncoding = genericToEncoding autonomyOptions

instance FromJSON AutonomyLevel where
  parseJSON = genericParseJSON autonomyOptions

autonomyOptions :: Options
autonomyOptions = enumOptions "Autonomy" (map toLower)

-- | Authentication scheme for a custom model.
data CustomModelAuthMode = CustomModelProviderDefault | CustomModelBearer
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON CustomModelAuthMode where
  toJSON = genericToJSON authModeOptions
  toEncoding = genericToEncoding authModeOptions

instance FromJSON CustomModelAuthMode where
  parseJSON = genericParseJSON authModeOptions

authModeOptions :: Options
authModeOptions = enumOptions "CustomModel" (camelTo2 '-')

-- | The protocol interaction mode, including advanced AGI and mission modes.
data DroidInteractionMode = DroidAuto | DroidSpec | DroidAgi | DroidMission
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON DroidInteractionMode where
  toJSON = genericToJSON interactionOptions
  toEncoding = genericToEncoding interactionOptions

instance FromJSON DroidInteractionMode where
  parseJSON = genericParseJSON interactionOptions

interactionOptions :: Options
interactionOptions = enumOptions "Droid" (map toLower)

-- | The file-editing tool interface selected for a model.
data FileEditToolProfile = FileEditApplyPatch | FileEditCreateEdit
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON FileEditToolProfile where
  toJSON = genericToJSON fileEditOptions
  toEncoding = genericToEncoding fileEditOptions

instance FromJSON FileEditToolProfile where
  parseJSON = genericParseJSON fileEditOptions

fileEditOptions :: Options
fileEditOptions = enumOptions "FileEdit" (camelTo2 '-')

-- | The author role of a protocol message.
data MessageRole = RoleUser | RoleAssistant | RoleTool | RoleSystem
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON MessageRole where
  toJSON = genericToJSON roleOptions
  toEncoding = genericToEncoding roleOptions

instance FromJSON MessageRole where
  parseJSON = genericParseJSON roleOptions

roleOptions :: Options
roleOptions = enumOptions "Role" (map toLower)

-- | Whether a message is visible to the model, the user, or both.
data MessageVisibility = VisibilityBoth | VisibilityLlmOnly | VisibilityUserOnly
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON MessageVisibility where
  toJSON = genericToJSON visibilityOptions
  toEncoding = genericToEncoding visibilityOptions

instance FromJSON MessageVisibility where
  parseJSON = genericParseJSON visibilityOptions

visibilityOptions :: Options
visibilityOptions = enumOptions "Visibility" (camelTo2 '_')

-- | The reported reason that the requested model was replaced.
data ModelFallbackReason
  = FallbackCustomModelNotConfigured
  | FallbackModelNotAvailableForOrg
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON ModelFallbackReason where
  toJSON = genericToJSON fallbackOptions
  toEncoding = genericToEncoding fallbackOptions

instance FromJSON ModelFallbackReason where
  parseJSON = genericParseJSON fallbackOptions

fallbackOptions :: Options
fallbackOptions = enumOptions "Fallback" (camelTo2 '_')

-- | The provider identifier reported by the model catalog.
data ModelProvider
  = ProviderAnthropic
  | ProviderOpenAI
  | ProviderGenericChatCompletionAPI
  | ProviderFactory
  | ProviderGoogle
  | ProviderXAI
  | ProviderVoyage
  | ProviderBedrockConverse
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToJSON ModelProvider where
  toJSON provider = String $ case provider of
    ProviderAnthropic -> "anthropic"
    ProviderOpenAI -> "openai"
    ProviderGenericChatCompletionAPI -> "generic-chat-completion-api"
    ProviderFactory -> "factory"
    ProviderGoogle -> "google"
    ProviderXAI -> "xai"
    ProviderVoyage -> "voyage"
    ProviderBedrockConverse -> "bedrock-converse"

instance FromJSON ModelProvider where
  parseJSON = withText "ModelProvider" $ \case
    "anthropic" -> pure ProviderAnthropic
    "openai" -> pure ProviderOpenAI
    "generic-chat-completion-api" -> pure ProviderGenericChatCompletionAPI
    "factory" -> pure ProviderFactory
    "google" -> pure ProviderGoogle
    "xai" -> pure ProviderXAI
    "voyage" -> pure ProviderVoyage
    "bedrock-converse" -> pure ProviderBedrockConverse
    _ -> fail "Unknown model provider"

-- | A reasoning-effort setting. The wire distinguishes none, off and dynamic.
data ReasoningEffort
  = ReasoningNone
  | ReasoningDynamic
  | ReasoningOff
  | ReasoningMinimal
  | ReasoningLow
  | ReasoningMedium
  | ReasoningHigh
  | ReasoningXHigh
  | ReasoningMax
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON ReasoningEffort where
  toJSON = genericToJSON reasoningOptions
  toEncoding = genericToEncoding reasoningOptions

instance FromJSON ReasoningEffort where
  parseJSON = genericParseJSON reasoningOptions

reasoningOptions :: Options
reasoningOptions = enumOptions "Reasoning" (map toLower)

-- | Whether the sandbox covers each command or the whole process.
data SandboxMode = SandboxPerCommand | SandboxWholeProcess
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON SandboxMode where
  toJSON = genericToJSON sandboxOptions
  toEncoding = genericToEncoding sandboxOptions

instance FromJSON SandboxMode where
  parseJSON = genericParseJSON sandboxOptions

sandboxOptions :: Options
sandboxOptions = enumOptions "Sandbox" (camelTo2 '-')

-- | The recorded origin of a session. This is distinct from request attribution.
data SessionOrigin
  = OriginWeb
  | OriginDesktop
  | OriginCliTui
  | OriginCliExec
  | OriginCliAcp
  | OriginSlack
  | OriginJira
  | OriginLinear
  | OriginMicrosoftTeams
  | OriginSessionsAPI
  | OriginAPI
  | OriginSDK
  | OriginAutomation
  | OriginReadinessRemediation
  | OriginReadinessEvaluation
  | OriginWikiGeneration
  | OriginWikiCISetup
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToJSON SessionOrigin where
  toJSON origin = String $ case origin of
    OriginWeb -> "web"
    OriginDesktop -> "desktop"
    OriginCliTui -> "cli_tui"
    OriginCliExec -> "cli_exec"
    OriginCliAcp -> "cli_acp"
    OriginSlack -> "slack"
    OriginJira -> "jira"
    OriginLinear -> "linear"
    OriginMicrosoftTeams -> "microsoft-teams"
    OriginSessionsAPI -> "sessions_api"
    OriginAPI -> "api"
    OriginSDK -> "sdk"
    OriginAutomation -> "automation"
    OriginReadinessRemediation -> "readiness-remediation"
    OriginReadinessEvaluation -> "readiness-evaluation"
    OriginWikiGeneration -> "wiki-generation"
    OriginWikiCISetup -> "wiki-ci-setup"

instance FromJSON SessionOrigin where
  parseJSON = withText "SessionOrigin" $ \case
    "web" -> pure OriginWeb
    "desktop" -> pure OriginDesktop
    "cli_tui" -> pure OriginCliTui
    "cli_exec" -> pure OriginCliExec
    "cli_acp" -> pure OriginCliAcp
    "slack" -> pure OriginSlack
    "jira" -> pure OriginJira
    "linear" -> pure OriginLinear
    "microsoft-teams" -> pure OriginMicrosoftTeams
    "sessions_api" -> pure OriginSessionsAPI
    "api" -> pure OriginAPI
    "sdk" -> pure OriginSDK
    "automation" -> pure OriginAutomation
    "readiness-remediation" -> pure OriginReadinessRemediation
    "readiness-evaluation" -> pure OriginReadinessEvaluation
    "wiki-generation" -> pure OriginWikiGeneration
    "wiki-ci-setup" -> pure OriginWikiCISetup
    _ -> fail "Unknown session origin"

-- | The settings layer addressed by an operation.
data SettingsLevel
  = SettingsOrg
  | SettingsRuntime
  | SettingsUser
  | SettingsProject
  | SettingsFolder
  | SettingsDynamic
  | SettingsBuiltin
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON SettingsLevel where
  toJSON = genericToJSON settingsOptions
  toEncoding = genericToEncoding settingsOptions

instance FromJSON SettingsLevel where
  parseJSON = genericParseJSON settingsOptions

settingsOptions :: Options
settingsOptions = enumOptions "Settings" (map toLower)

-- | The location from which a skill was discovered.
data SkillLocation = SkillProject | SkillPersonal | SkillBuiltin | SkillAutomation
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON SkillLocation where
  toJSON = genericToJSON skillOptions
  toEncoding = genericToEncoding skillOptions

instance FromJSON SkillLocation where
  parseJSON = genericParseJSON skillOptions

skillOptions :: Options
skillOptions = enumOptions "Skill" (map toLower)

-- | Whether tools are exposed directly, through scripts, or through both.
data ToolExecutionMode = ToolDirectOnly | ToolDirectAndScript | ToolScriptOnly
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON ToolExecutionMode where
  toJSON = genericToJSON toolOptions
  toEncoding = genericToEncoding toolOptions

instance FromJSON ToolExecutionMode where
  parseJSON = genericParseJSON toolOptions

toolOptions :: Options
toolOptions = enumOptions "Tool" (camelTo2 '_')

-- | The requested lifetime of a managed worktree.
data WorktreeLifecycle = WorktreeEphemeral | WorktreePersistent
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON WorktreeLifecycle where
  toJSON = genericToJSON worktreeOptions
  toEncoding = genericToEncoding worktreeOptions

instance FromJSON WorktreeLifecycle where
  parseJSON = genericParseJSON worktreeOptions

worktreeOptions :: Options
worktreeOptions = enumOptions "Worktree" (map toLower)
