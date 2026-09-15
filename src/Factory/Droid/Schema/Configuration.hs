{-# LANGUAGE OverloadedStrings #-}

-- | Creation and load configuration with complete operation request bodies.
-- Authentication, retries and session publication stay with their runtime
-- owners. Explicit tokens belong to the daemon wrapper. Omission, false/empty
-- values and nullable output remain distinct.
module Factory.Droid.Schema.Configuration
  ( SessionPrivacy (..),
    SessionConfiguration (..),
    defaultSessionConfiguration,
    InitializeSessionParams (..),
    defaultInitializeSessionParams,
    DaemonSpawnOptions (..),
    defaultDaemonSpawnOptions,
    DaemonInitializeSessionParams (..),
    InitializationError (..),
    initializationFields,
    daemonInitializationFields,
    validateInitializationParams,
    validateDaemonInitializationParams,
    SessionLoadConfiguration (..),
    defaultSessionLoadConfiguration,
    loadConfigurationFromInitialization,
    mergeSessionLoadConfiguration,
    LoadSessionParams (..),
    defaultLoadSessionParams,
    loadSessionFields,
    DaemonLoadConfiguration (..),
    defaultDaemonLoadConfiguration,
    daemonLoadConfigurationFromSpawn,
    mergeDaemonLoadConfiguration,
    DaemonLoadSessionParams (..),
    daemonLoadSessionFields,
    LoadConfigurationError (..),
    validateLoadSessionParams,
    validateDaemonLoadSessionParams,
    prepareLoadSessionParams,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad (unless, void)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object, String), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.List ((\\))
import Data.Maybe (isJust)
import Data.Scientific (Scientific, isInteger, toRealFloat)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, fieldsWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Control (OutputFormat)
import Factory.Droid.Schema.Enums (AutonomyLevel, DroidInteractionMode, ReasoningEffort, SessionOrigin)
import Factory.Droid.Schema.MCP.Config (McpSessionOptions (..), defaultMcpSessionOptions, mcpInitializeFields, mcpLoadFields)
import Factory.Droid.Schema.Mission (DecompSessionType)
import Factory.Droid.Schema.Models (MissionModelSettings)
import Factory.Droid.Schema.Session (SessionTag)
import Factory.Droid.Schema.Settings (LegacyAutonomyMode, ToolPolicy (..), UpdateSessionSettingsParams (..), emptySettingsUpdate, emptyToolPolicy, parseToolPolicy, toolPolicyFields, toolPolicyKeys)
import Factory.Droid.Schema.Sources (SessionSource)
import Factory.Droid.Schema.SystemPrompt (SystemPromptConfig)

data SessionPrivacy = SessionPrivate | SessionOrganization deriving stock (Eq, Show)

instance FromJSON SessionPrivacy where
  parseJSON = withText "SessionPrivacy" $ \case
    "private" -> pure SessionPrivate
    "organization" -> pure SessionOrganization
    _ -> fail "Unknown session privacy"

instance ToJSON SessionPrivacy where
  toJSON SessionPrivate = String "private"
  toJSON SessionOrganization = String "organization"

-- | Supplemental creation settings. Existing model, prompt, cwd, machine, MCP
-- and worktree options remain in their current owners, avoiding two competing
-- settings for the same field. Unsafe permission behavior is never inferred.
data SessionConfiguration = SessionConfiguration
  { configurationSessionId :: !(Maybe Text),
    configurationWorkspaceId :: !(Maybe Text),
    configurationLegacyMode :: !(Maybe LegacyAutonomyMode),
    configurationMode :: !(Maybe DroidInteractionMode),
    configurationAutonomy :: !(Maybe AutonomyLevel),
    configurationReasoning :: !(Maybe ReasoningEffort),
    configurationSpecModel :: !(Maybe Text),
    configurationSpecReasoning :: !(Maybe ReasoningEffort),
    configurationMission :: !(Maybe MissionModelSettings),
    configurationCompactionThresholdEnabled :: !(Maybe Bool),
    configurationDecompType :: !(Maybe DecompSessionType),
    configurationDecompMissionId :: !(Maybe Text),
    configurationSkipPermissionsUnsafe :: !(Maybe Bool),
    configurationLocation :: !(Maybe Text),
    configurationSource :: !(Maybe SessionSource),
    configurationOrigin :: !(Maybe SessionOrigin),
    configurationTags :: !(Maybe [SessionTag]),
    configurationPrivacy :: !(Maybe SessionPrivacy),
    configurationTitle :: !(Maybe Text),
    configurationAutoRejectPermissions :: !(Maybe Bool),
    configurationDisableBuiltinSkills :: !(Maybe Bool),
    configurationSystemPromptOverride :: !(Maybe Text),
    configurationStructuredOutput :: !(Maybe (Maybe OutputFormat)),
    configurationToolPolicy :: !ToolPolicy,
    configurationAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionConfiguration where show _ = "SessionConfiguration <redacted>"

defaultSessionConfiguration :: SessionConfiguration
defaultSessionConfiguration =
  SessionConfiguration
    { configurationSessionId = Nothing,
      configurationWorkspaceId = Nothing,
      configurationLegacyMode = Nothing,
      configurationMode = Nothing,
      configurationAutonomy = Nothing,
      configurationReasoning = Nothing,
      configurationSpecModel = Nothing,
      configurationSpecReasoning = Nothing,
      configurationMission = Nothing,
      configurationCompactionThresholdEnabled = Nothing,
      configurationDecompType = Nothing,
      configurationDecompMissionId = Nothing,
      configurationSkipPermissionsUnsafe = Nothing,
      configurationLocation = Nothing,
      configurationSource = Nothing,
      configurationOrigin = Nothing,
      configurationTags = Nothing,
      configurationPrivacy = Nothing,
      configurationTitle = Nothing,
      configurationAutoRejectPermissions = Nothing,
      configurationDisableBuiltinSkills = Nothing,
      configurationSystemPromptOverride = Nothing,
      configurationStructuredOutput = Nothing,
      configurationToolPolicy = emptyToolPolicy,
      configurationAdditionalFields = mempty
    }

data InitializeSessionParams = InitializeSessionParams
  { initializeMachineId :: !Text,
    initializeWorkingDirectory :: !Text,
    initializeModel :: !(Maybe Text),
    initializeSystemPrompt :: !(Maybe SystemPromptConfig),
    initializeMcpOptions :: !McpSessionOptions,
    initializeWorktree :: !(Maybe Bool),
    initializeWorktreeDirectory :: !(Maybe Text),
    initializeConfiguration :: !SessionConfiguration
  }
  deriving stock (Eq)

instance Show InitializeSessionParams where show _ = "InitializeSessionParams <redacted>"

defaultInitializeSessionParams :: Text -> Text -> InitializeSessionParams
defaultInitializeSessionParams machine directory = InitializeSessionParams machine directory Nothing Nothing defaultMcpSessionOptions Nothing Nothing defaultSessionConfiguration

instance FromJSON InitializeSessionParams where
  parseJSON = withObject "InitializeSessionParams" $ \fields ->
    InitializeSessionParams
      <$> fields .: "machineId"
      <*> fields .: "cwd"
      <*> fields .:! "modelId"
      <*> fields .:! "systemPrompt"
      <*> parseJSON (Object fields)
      <*> fields .:! "worktree"
      <*> fields .:! "worktreeDir"
      <*> parseConfiguration fields

instance ToJSON InitializeSessionParams where toJSON = Object . initializationFields

initializationFields :: InitializeSessionParams -> Object
initializationFields params =
  let config = initializeConfiguration params
   in fieldsWithAdditionalFields allInitializationKeys (configurationAdditionalFields config) $
        ["machineId" .= initializeMachineId params, "cwd" .= initializeWorkingDirectory params]
          <> optionalField "modelId" (initializeModel params)
          <> optionalField "systemPrompt" (initializeSystemPrompt params)
          <> optionalField "worktree" (initializeWorktree params)
          <> optionalField "worktreeDir" (initializeWorktreeDirectory params)
          <> KeyMap.toList (mcpInitializeFields (initializeMcpOptions params))
          <> configurationFields config

parseConfiguration :: Object -> Parser SessionConfiguration
parseConfiguration fields =
  SessionConfiguration
    <$> fields .:! "sessionId"
    <*> fields .:! "workspaceId"
    <*> fields .:! "autonomyMode"
    <*> (fields .:! "interactionMode" <|> pure Nothing)
    <*> (fields .:! "autonomyLevel" <|> pure Nothing)
    <*> fields .:! "reasoningEffort"
    <*> fields .:! "specModeModelId"
    <*> fields .:! "specModeReasoningEffort"
    <*> fields .:! "missionSettings"
    <*> fields .:! "compactionThresholdCheckEnabled"
    <*> fields .:! "decompSessionType"
    <*> fields .:! "decompMissionId"
    <*> fields .:! "skipPermissionsUnsafe"
    <*> fields .:! "sessionLocation"
    <*> fields .:! "sessionSource"
    <*> fields .:! "sessionOriginHint"
    <*> fields .:! "tags"
    <*> fields .:! "privacyLevel"
    <*> fields .:! "title"
    <*> fields .:! "autoRejectPermissionRequests"
    <*> fields .:! "disableBuiltinSkills"
    <*> fields .:! "systemPromptOverride"
    <*> fields .:! "structuredOutputFormat"
    <*> parseToolPolicy fields
    <*> pure (additionalFields allInitializationKeys fields)

configurationFields :: SessionConfiguration -> [Pair]
configurationFields config =
  optionalField "sessionId" (configurationSessionId config)
    <> optionalField "workspaceId" (configurationWorkspaceId config)
    <> optionalField "autonomyMode" (configurationLegacyMode config)
    <> optionalField "interactionMode" (configurationMode config)
    <> optionalField "autonomyLevel" (configurationAutonomy config)
    <> optionalField "reasoningEffort" (configurationReasoning config)
    <> optionalField "specModeModelId" (configurationSpecModel config)
    <> optionalField "specModeReasoningEffort" (configurationSpecReasoning config)
    <> optionalField "missionSettings" (configurationMission config)
    <> optionalField "compactionThresholdCheckEnabled" (configurationCompactionThresholdEnabled config)
    <> optionalField "decompSessionType" (configurationDecompType config)
    <> optionalField "decompMissionId" (configurationDecompMissionId config)
    <> optionalField "skipPermissionsUnsafe" (configurationSkipPermissionsUnsafe config)
    <> optionalField "sessionLocation" (configurationLocation config)
    <> optionalField "sessionSource" (configurationSource config)
    <> optionalField "sessionOriginHint" (configurationOrigin config)
    <> optionalField "tags" (configurationTags config)
    <> optionalField "privacyLevel" (configurationPrivacy config)
    <> optionalField "title" (configurationTitle config)
    <> optionalField "autoRejectPermissionRequests" (configurationAutoRejectPermissions config)
    <> optionalField "disableBuiltinSkills" (configurationDisableBuiltinSkills config)
    <> optionalField "systemPromptOverride" (configurationSystemPromptOverride config)
    <> optionalField "structuredOutputFormat" (configurationStructuredOutput config)
    <> toolPolicyFields (configurationToolPolicy config)

data DaemonSpawnOptions = DaemonSpawnOptions
  { spawnInactivityTimeoutMillis :: !(Maybe Scientific),
    spawnDisableInactivityTimeout :: !(Maybe Bool),
    spawnRuntimeSettingsPath :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show DaemonSpawnOptions where show _ = "DaemonSpawnOptions <redacted>"

defaultDaemonSpawnOptions :: DaemonSpawnOptions
defaultDaemonSpawnOptions = DaemonSpawnOptions Nothing Nothing Nothing

instance FromJSON DaemonSpawnOptions where
  parseJSON = withObject "DaemonSpawnOptions" $ \fields -> do
    millis <- positiveIntegerField fields "inactivityTimeoutMs"
    DaemonSpawnOptions millis <$> fields .:! "disableInactivityTimeout" <*> fields .:! "runtimeSettingsPath"

instance ToJSON DaemonSpawnOptions where toJSON = Object . KeyMap.fromList . spawnFields

spawnFields :: DaemonSpawnOptions -> [Pair]
spawnFields options = optionalField "inactivityTimeoutMs" (spawnInactivityTimeoutMillis options) <> optionalField "disableInactivityTimeout" (spawnDisableInactivityTimeout options) <> optionalField "runtimeSettingsPath" (spawnRuntimeSettingsPath options)

data DaemonInitializeSessionParams = DaemonInitializeSessionParams
  { daemonInitializeSession :: !InitializeSessionParams,
    daemonInitializeToken :: !Text,
    daemonInitializeSpawn :: !DaemonSpawnOptions
  }
  deriving stock (Eq)

instance Show DaemonInitializeSessionParams where show _ = "DaemonInitializeSessionParams <redacted>"

instance FromJSON DaemonInitializeSessionParams where
  parseJSON value = withObject "DaemonInitializeSessionParams" (\fields -> DaemonInitializeSessionParams <$> parseJSON value <*> fields .: "token" <*> parseJSON value) value

instance ToJSON DaemonInitializeSessionParams where toJSON = Object . daemonInitializationFields

daemonInitializationFields :: DaemonInitializeSessionParams -> Object
daemonInitializationFields params = KeyMap.union (KeyMap.fromList (["token" .= daemonInitializeToken params] <> spawnFields (daemonInitializeSpawn params))) (initializationFields (daemonInitializeSession params))

data InitializationError = InvalidInitializationParams | InitializationOptionsOnResume deriving stock (Eq, Show)

instance Exception InitializationError

validateInitializationParams :: InitializeSessionParams -> Either InitializationError ()
validateInitializationParams params = either (const (Left InvalidInitializationParams)) (const (Right ())) (parseEither (void . parseJSON @InitializeSessionParams) (toJSON params))

validateDaemonInitializationParams :: DaemonInitializeSessionParams -> Either InitializationError ()
validateDaemonInitializationParams params = either (const (Left InvalidInitializationParams)) (const (Right ())) (parseEither (void . parseJSON @DaemonInitializeSessionParams) (toJSON params))

allInitializationKeys :: [Key]
allInitializationKeys =
  [ "machineId",
    "cwd",
    "modelId",
    "systemPrompt",
    "worktree",
    "worktreeDir",
    "mcpServers",
    "mcpOAuthCallbackUri",
    "blockOnMcpLoad",
    "sessionId",
    "workspaceId",
    "autonomyMode",
    "interactionMode",
    "autonomyLevel",
    "reasoningEffort",
    "specModeModelId",
    "specModeReasoningEffort",
    "missionSettings",
    "compactionThresholdCheckEnabled",
    "decompSessionType",
    "decompMissionId",
    "skipPermissionsUnsafe",
    "sessionLocation",
    "sessionSource",
    "sessionOriginHint",
    "tags",
    "privacyLevel",
    "title",
    "autoRejectPermissionRequests",
    "disableBuiltinSkills",
    "systemPromptOverride",
    "structuredOutputFormat",
    "token",
    "inactivityTimeoutMs",
    "disableInactivityTimeout",
    "runtimeSettingsPath"
  ]
    <> toolPolicyKeys

-- | Retained load intent. Restrictive tools are a post-load settings step,
-- not a load wire field. The runtime owns that split; low-level validation
-- rejects a LoadSessionParams value that still contains restrictions.
data SessionLoadConfiguration = SessionLoadConfiguration
  { loadToolPolicy :: !ToolPolicy,
    loadAllMessages :: !(Maybe Bool),
    loadMessageLimit :: !(Maybe Scientific),
    loadAutoRejectPermissions :: !(Maybe Bool),
    loadDisableBuiltinSkills :: !(Maybe Bool),
    loadLocation :: !(Maybe Text),
    loadSource :: !(Maybe SessionSource),
    loadOrigin :: !(Maybe SessionOrigin),
    loadStructuredOutput :: !(Maybe (Maybe OutputFormat)),
    loadAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionLoadConfiguration where show _ = "SessionLoadConfiguration <redacted>"

defaultSessionLoadConfiguration :: SessionLoadConfiguration
defaultSessionLoadConfiguration = SessionLoadConfiguration emptyToolPolicy Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

loadConfigurationFromInitialization :: SessionConfiguration -> SessionLoadConfiguration
loadConfigurationFromInitialization config =
  defaultSessionLoadConfiguration
    { loadToolPolicy = configurationToolPolicy config,
      loadAutoRejectPermissions = configurationAutoRejectPermissions config,
      loadDisableBuiltinSkills = configurationDisableBuiltinSkills config,
      loadLocation = configurationLocation config,
      loadSource = configurationSource config,
      loadOrigin = configurationOrigin config,
      loadStructuredOutput = configurationStructuredOutput config
    }

-- | A new explicit value wins; omission preserves retained intent. In
-- particular Just Nothing clears output and Just [] clears a tool list.
mergeSessionLoadConfiguration :: SessionLoadConfiguration -> SessionLoadConfiguration -> SessionLoadConfiguration
mergeSessionLoadConfiguration old new =
  let before = loadToolPolicy old
      after = loadToolPolicy new
   in SessionLoadConfiguration
        ( ToolPolicy
            (policyAdditionalTools after <|> policyAdditionalTools before)
            (policyEnabledTools after <|> policyEnabledTools before)
            (policyDisabledTools after <|> policyDisabledTools before)
            (policyRestrictedTools after <|> policyRestrictedTools before)
        )
        (loadAllMessages new <|> loadAllMessages old)
        (loadMessageLimit new <|> loadMessageLimit old)
        (loadAutoRejectPermissions new <|> loadAutoRejectPermissions old)
        (loadDisableBuiltinSkills new <|> loadDisableBuiltinSkills old)
        (loadLocation new <|> loadLocation old)
        (loadSource new <|> loadSource old)
        (loadOrigin new <|> loadOrigin old)
        (loadStructuredOutput new <|> loadStructuredOutput old)
        (KeyMap.union (loadAdditionalFields new) (loadAdditionalFields old))

data LoadSessionParams = LoadSessionParams
  { loadSessionId :: !Text,
    loadMcpOptions :: !McpSessionOptions,
    loadConfiguration :: !SessionLoadConfiguration
  }
  deriving stock (Eq)

instance Show LoadSessionParams where show _ = "LoadSessionParams <redacted>"

defaultLoadSessionParams :: Text -> LoadSessionParams
defaultLoadSessionParams identifier = LoadSessionParams identifier defaultMcpSessionOptions defaultSessionLoadConfiguration

-- | Lower retained intent into a legal load and an optional post-load patch.
-- Owners must apply the patch only after a successful, still-current load.
prepareLoadSessionParams :: Text -> McpSessionOptions -> Bool -> SessionLoadConfiguration -> (LoadSessionParams, Maybe UpdateSessionSettingsParams)
prepareLoadSessionParams identifier mcp reject config =
  let policy = loadToolPolicy config
      params = LoadSessionParams identifier (mcp {sessionBlockOnMcpLoad = Nothing}) (config {loadToolPolicy = policy {policyRestrictedTools = Nothing}, loadAutoRejectPermissions = loadAutoRejectPermissions config <|> Just reject})
      patch = (\tools -> emptySettingsUpdate {updateSettingsToolPolicy = emptyToolPolicy {policyRestrictedTools = Just tools}}) <$> policyRestrictedTools policy
   in (params, patch)

instance FromJSON LoadSessionParams where
  parseJSON = withObject "LoadSessionParams" $ \fields -> do
    let prohibited = ["restrictToolIds", "blockOnMcpLoad", "inactivityTimeoutMs"] <> (allInitializationKeys \\ loadKeys)
    unless (all (\key -> not (KeyMap.member key fields)) prohibited) (fail "Creation-only or post-load option in load parameters")
    config <-
      SessionLoadConfiguration
        <$> parseToolPolicy fields
        <*> fields .:! "loadAllMessages"
        <*> positiveIntegerField fields "messageLimit"
        <*> fields .:! "autoRejectPermissionRequests"
        <*> fields .:! "disableBuiltinSkills"
        <*> fields .:! "sessionLocation"
        <*> fields .:! "sessionSource"
        <*> fields .:! "sessionOriginHint"
        <*> fields .:! "structuredOutputFormat"
        <*> pure (additionalFields loadKeys fields)
    LoadSessionParams <$> fields .: "sessionId" <*> parseJSON (Object fields) <*> pure config

instance ToJSON LoadSessionParams where toJSON = Object . loadSessionFields

loadSessionFields :: LoadSessionParams -> Object
loadSessionFields params =
  let config = loadConfiguration params
   in fieldsWithAdditionalFields loadKeys (loadAdditionalFields config) $
        ["sessionId" .= loadSessionId params]
          <> KeyMap.toList (mcpLoadFields (loadMcpOptions params))
          <> toolPolicyFields (loadToolPolicy config)
          <> optionalField "loadAllMessages" (loadAllMessages config)
          <> optionalField "messageLimit" (loadMessageLimit config)
          <> optionalField "autoRejectPermissionRequests" (loadAutoRejectPermissions config)
          <> optionalField "disableBuiltinSkills" (loadDisableBuiltinSkills config)
          <> optionalField "sessionLocation" (loadLocation config)
          <> optionalField "sessionSource" (loadSource config)
          <> optionalField "sessionOriginHint" (loadOrigin config)
          <> optionalField "structuredOutputFormat" (loadStructuredOutput config)

data DaemonLoadConfiguration = DaemonLoadConfiguration
  { daemonLoadDisableInactivity :: !(Maybe Bool),
    daemonLoadRuntimeSettingsPath :: !(Maybe Text),
    daemonLoadSkipPermissionsUnsafe :: !(Maybe Bool)
  }
  deriving stock (Eq)

instance Show DaemonLoadConfiguration where show _ = "DaemonLoadConfiguration <redacted>"

defaultDaemonLoadConfiguration :: DaemonLoadConfiguration
defaultDaemonLoadConfiguration = DaemonLoadConfiguration Nothing Nothing Nothing

-- Initial unsafe-permission bypass is not a saved load default in the source
-- controller. Re-establishing it requires an explicit load configuration.
daemonLoadConfigurationFromSpawn :: DaemonSpawnOptions -> DaemonLoadConfiguration
daemonLoadConfigurationFromSpawn spawn = DaemonLoadConfiguration (spawnDisableInactivityTimeout spawn) (spawnRuntimeSettingsPath spawn) Nothing

mergeDaemonLoadConfiguration :: DaemonLoadConfiguration -> DaemonLoadConfiguration -> DaemonLoadConfiguration
mergeDaemonLoadConfiguration old new = DaemonLoadConfiguration (daemonLoadDisableInactivity new <|> daemonLoadDisableInactivity old) (daemonLoadRuntimeSettingsPath new <|> daemonLoadRuntimeSettingsPath old) (daemonLoadSkipPermissionsUnsafe new <|> daemonLoadSkipPermissionsUnsafe old)

data DaemonLoadSessionParams = DaemonLoadSessionParams
  { daemonLoadSession :: !LoadSessionParams,
    daemonLoadToken :: !Text,
    daemonLoadConfiguration :: !DaemonLoadConfiguration
  }
  deriving stock (Eq)

instance Show DaemonLoadSessionParams where show _ = "DaemonLoadSessionParams <redacted>"

instance FromJSON DaemonLoadSessionParams where
  parseJSON value = withObject "DaemonLoadSessionParams" (\fields -> DaemonLoadSessionParams <$> parseJSON value <*> fields .: "token" <*> (DaemonLoadConfiguration <$> fields .:! "disableInactivityTimeout" <*> fields .:! "runtimeSettingsPath" <*> fields .:! "skipPermissionsUnsafe")) value

instance ToJSON DaemonLoadSessionParams where toJSON = Object . daemonLoadSessionFields

daemonLoadSessionFields :: DaemonLoadSessionParams -> Object
daemonLoadSessionFields params =
  let config = daemonLoadConfiguration params
   in KeyMap.union
        (KeyMap.fromList (["token" .= daemonLoadToken params] <> optionalField "disableInactivityTimeout" (daemonLoadDisableInactivity config) <> optionalField "runtimeSettingsPath" (daemonLoadRuntimeSettingsPath config) <> optionalField "skipPermissionsUnsafe" (daemonLoadSkipPermissionsUnsafe config)))
        (loadSessionFields (daemonLoadSession params))

data LoadConfigurationError = InvalidLoadParams deriving stock (Eq, Show)

instance Exception LoadConfigurationError

validateLoadSessionParams :: LoadSessionParams -> Either LoadConfigurationError ()
validateLoadSessionParams params
  | isJust (sessionBlockOnMcpLoad (loadMcpOptions params)) = Left InvalidLoadParams
  | otherwise = either (const (Left InvalidLoadParams)) (const (Right ())) (parseEither (void . parseJSON @LoadSessionParams) (toJSON params))

validateDaemonLoadSessionParams :: DaemonLoadSessionParams -> Either LoadConfigurationError ()
validateDaemonLoadSessionParams params = do
  validateLoadSessionParams (daemonLoadSession params)
  either (const (Left InvalidLoadParams)) (const (Right ())) (parseEither (void . parseJSON @DaemonLoadSessionParams) (toJSON params))

positiveIntegerField :: Object -> Key -> Parser (Maybe Scientific)
positiveIntegerField fields key = do
  value <- fields .:! key
  let valid number = let floating = toRealFloat number :: Double in floating > 0 && not (isInfinite floating) && isInteger number
  mapM_ (\number -> unless (valid number) (fail "Expected positive finite integer")) value
  pure value

loadKeys :: [Key]
loadKeys = ["sessionId", "mcpServers", "mcpOAuthCallbackUri", "loadAllMessages", "messageLimit", "autoRejectPermissionRequests", "disableBuiltinSkills", "sessionLocation", "sessionSource", "sessionOriginHint", "structuredOutputFormat", "token", "disableInactivityTimeout", "runtimeSettingsPath", "skipPermissionsUnsafe"] <> toolPolicyKeys
