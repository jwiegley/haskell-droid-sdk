{-# LANGUAGE OverloadedStrings #-}

-- | MCP wire metadata and operation bodies for Factory protocol 1.205.0.
-- No process, network, authentication or settings operation occurs here.
-- Record 'Show' instances redact all fields, including arbitrary extensions;
-- JSON encoding deliberately retains the full payload and is not safe logging.
module Factory.Droid.Schema.MCP
  ( McpServerName,
    McpServerType (..),
    McpServerStatus (..),
    McpSettingsLevel (..),
    McpAuthOutcome (..),
    McpHttpServerConfigFields (..),
    McpStdioServerConfigFields (..),
    StdioMcp (..),
    HttpHeader (..),
    McpRegistryServer (..),
    ListMcpRegistryResult (..),
    McpToolInputSchema (..),
    McpToolInfo (..),
    ListMcpToolsResult (..),
    McpConfigError (..),
    McpStatusSummary (..),
    McpServerStatusInfo (..),
    ListMcpServersResult (..),
    McpStatusChanged (..),
    McpAuthRequired (..),
    McpAuthCompleted (..),
    McpServerNameParams (..),
    RemoveMcpServerParams (..),
    ToggleMcpServerParams (..),
    ToggleMcpToolParams (..),
    SubmitMcpAuthCodeParams (..),
    SubmitMcpAuthErrorParams (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object, String), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, fieldsWithAdditionalFields, objectWithAdditionalFields, optionalField, requireLiteral)
import Factory.Droid.Schema.Enums (SettingsLevel)

-- | Server names are unconstrained strings in the supplied wire schema.
type McpServerName = Text

-- | The three supported transport labels; decoding does not open a transport.
data McpServerType = McpStdio | McpHttp | McpSse
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON McpServerType where
  parseJSON = withText "McpServerType" $ \case
    "stdio" -> pure McpStdio
    "http" -> pure McpHttp
    "sse" -> pure McpSse
    _ -> fail "Unknown MCP server type"

instance ToJSON McpServerType where
  toJSON McpStdio = String "stdio"
  toJSON McpHttp = String "http"
  toJSON McpSse = String "sse"

-- | A reported connection state, not a state machine transition.
data McpServerStatus = McpConnecting | McpConnected | McpDisconnected | McpFailed | McpDisabled
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON McpServerStatus where
  parseJSON = withText "McpServerStatus" $ \case
    "connecting" -> pure McpConnecting
    "connected" -> pure McpConnected
    "disconnected" -> pure McpDisconnected
    "failed" -> pure McpFailed
    "disabled" -> pure McpDisabled
    _ -> fail "Unknown MCP server status"

instance ToJSON McpServerStatus where
  toJSON McpConnecting = String "connecting"
  toJSON McpConnected = String "connected"
  toJSON McpDisconnected = String "disconnected"
  toJSON McpFailed = String "failed"
  toJSON McpDisabled = String "disabled"

-- | The sole settings level accepted by remove/toggle-server bodies.
data McpSettingsLevel = McpUserSettings
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON McpSettingsLevel where
  parseJSON = withText "McpSettingsLevel" $ \case
    "user" -> pure McpUserSettings
    _ -> fail "MCP settings level must be user"

instance ToJSON McpSettingsLevel where
  toJSON McpUserSettings = String "user"

-- | A reported authentication outcome; success does not itself provide tokens.
data McpAuthOutcome = McpAuthSuccess | McpAuthCancelled | McpAuthFailed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON McpAuthOutcome where
  parseJSON = withText "McpAuthOutcome" $ \case
    "success" -> pure McpAuthSuccess
    "cancelled" -> pure McpAuthCancelled
    "failed" -> pure McpAuthFailed
    _ -> fail "Unknown MCP authentication outcome"

instance ToJSON McpAuthOutcome where
  toJSON McpAuthSuccess = String "success"
  toJSON McpAuthCancelled = String "cancelled"
  toJSON McpAuthFailed = String "failed"

-- | Partial registry HTTP fields. URL is a wire string, not a validated URI.
data McpHttpServerConfigFields = McpHttpServerConfigFields
  { mcpConfigUrl :: !(Maybe Text),
    mcpHttpFieldsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpHttpServerConfigFields where
  show _ = "McpHttpServerConfigFields <redacted>"

instance FromJSON McpHttpServerConfigFields where
  parseJSON = withObject "McpHttpServerConfigFields" $ \fields -> McpHttpServerConfigFields <$> fields .:! "url" <*> pure (additionalFields ["url"] fields)

instance ToJSON McpHttpServerConfigFields where
  toJSON fields = objectWithAdditionalFields ["url"] (mcpHttpFieldsAdditionalFields fields) (optionalField "url" (mcpConfigUrl fields))

-- | Partial registry stdio fields; command and arguments are both optional.
data McpStdioServerConfigFields = McpStdioServerConfigFields
  { mcpConfigCommand :: !(Maybe Text),
    mcpConfigArgs :: !(Maybe [Text]),
    mcpStdioFieldsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpStdioServerConfigFields where
  show _ = "McpStdioServerConfigFields <redacted>"

instance FromJSON McpStdioServerConfigFields where
  parseJSON = withObject "McpStdioServerConfigFields" $ \fields -> McpStdioServerConfigFields <$> fields .:! "command" <*> fields .:! "args" <*> pure (additionalFields ["command", "args"] fields)

instance ToJSON McpStdioServerConfigFields where
  toJSON fields = objectWithAdditionalFields ["command", "args"] (mcpStdioFieldsAdditionalFields fields) (optionalField "command" (mcpConfigCommand fields) <> optionalField "args" (mcpConfigArgs fields))

-- | The standalone stdio configuration body. No type discriminator is declared.
-- Schema defaults are annotations here: missing args/env remain absent rather
-- than being materialized as empty collections. No command is launched.
data StdioMcp = StdioMcp
  { stdioMcpName :: !Text,
    stdioMcpCommand :: !Text,
    stdioMcpArgs :: !(Maybe [Text]),
    stdioMcpEnv :: !(Maybe (KeyMap Text)),
    stdioMcpAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show StdioMcp where
  show _ = "StdioMcp <redacted>"

instance FromJSON StdioMcp where
  parseJSON = withObject "StdioMcp" $ \fields -> StdioMcp <$> fields .: "name" <*> fields .: "command" <*> fields .:! "args" <*> fields .:! "env" <*> pure (additionalFields stdioKeys fields)

instance ToJSON StdioMcp where
  toJSON config = objectWithAdditionalFields stdioKeys (stdioMcpAdditionalFields config) (["name" .= stdioMcpName config, "command" .= stdioMcpCommand config] <> optionalField "args" (stdioMcpArgs config) <> optionalField "env" (stdioMcpEnv config))

-- | Header wire data, not validation for an HTTP request. A transport must
-- validate header syntax before use; this codec never emits an HTTP header.
data HttpHeader = HttpHeader
  { httpHeaderName :: !Text,
    httpHeaderValue :: !Text,
    httpHeaderAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show HttpHeader where
  show _ = "HttpHeader <redacted>"

instance FromJSON HttpHeader where
  parseJSON = withObject "HttpHeader" $ \fields -> HttpHeader <$> fields .: "name" <*> fields .: "value" <*> pure (additionalFields ["name", "value"] fields)

instance ToJSON HttpHeader where
  toJSON header = objectWithAdditionalFields ["name", "value"] (httpHeaderAdditionalFields header) ["name" .= httpHeaderName header, "value" .= httpHeaderValue header]

-- | Registry metadata. Transport type does not make URL/command fields required
-- or mutually exclusive in this schema; no launch configuration is inferred.
data McpRegistryServer = McpRegistryServer
  { registryServerName :: !McpServerName,
    registryServerDescription :: !Text,
    registryServerType :: !McpServerType,
    registryServerUrl :: !(Maybe Text),
    registryServerCommand :: !(Maybe Text),
    registryServerArgs :: !(Maybe [Text]),
    registryServerNote :: !(Maybe Text),
    registryServerLogoUrl :: !(Maybe Text),
    registryServerAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpRegistryServer where
  show _ = "McpRegistryServer <redacted>"

instance FromJSON McpRegistryServer where
  parseJSON = withObject "McpRegistryServer" $ \fields ->
    McpRegistryServer <$> fields .: "name" <*> fields .: "description" <*> fields .: "type" <*> fields .:! "url" <*> fields .:! "command" <*> fields .:! "args" <*> fields .:! "note" <*> fields .:! "logoUrl" <*> pure (additionalFields registryKeys fields)

instance ToJSON McpRegistryServer where
  toJSON server =
    objectWithAdditionalFields registryKeys (registryServerAdditionalFields server) $
      ["name" .= registryServerName server, "description" .= registryServerDescription server, "type" .= registryServerType server]
        <> optionalField "url" (registryServerUrl server)
        <> optionalField "command" (registryServerCommand server)
        <> optionalField "args" (registryServerArgs server)
        <> optionalField "note" (registryServerNote server)
        <> optionalField "logoUrl" (registryServerLogoUrl server)

-- | An ordered registry snapshot, possibly empty.
data ListMcpRegistryResult = ListMcpRegistryResult
  { mcpRegistryServers :: ![McpRegistryServer],
    mcpRegistryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListMcpRegistryResult where
  show _ = "ListMcpRegistryResult <redacted>"

instance FromJSON ListMcpRegistryResult where
  parseJSON = withObject "ListMcpRegistryResult" $ \fields -> ListMcpRegistryResult <$> fields .: "servers" <*> pure (additionalFields ["servers"] fields)

instance ToJSON ListMcpRegistryResult where
  toJSON result = objectWithAdditionalFields ["servers"] (mcpRegistryAdditionalFields result) ["servers" .= mcpRegistryServers result]

-- | The declared MCP input-schema subset. Property values and extensions are
-- arbitrary JSON; type is any string and required names need not be property
-- keys. Parsing this object is not full JSON Schema validation.
data McpToolInputSchema = McpToolInputSchema
  { mcpInputType :: !(Maybe Text),
    mcpInputProperties :: !(Maybe Object),
    mcpInputRequired :: !(Maybe [Text]),
    mcpInputAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpToolInputSchema where
  show _ = "McpToolInputSchema <redacted>"

instance FromJSON McpToolInputSchema where
  parseJSON = withObject "McpToolInputSchema" $ \fields -> McpToolInputSchema <$> fields .:! "type" <*> fields .:! "properties" <*> fields .:! "required" <*> pure (additionalFields inputKeys fields)

instance ToJSON McpToolInputSchema where
  toJSON schema = objectWithAdditionalFields inputKeys (mcpInputAdditionalFields schema) (optionalField "type" (mcpInputType schema) <> optionalField "properties" (mcpInputProperties schema) <> optionalField "required" (mcpInputRequired schema))

-- | A tool report. Enabled/read-only flags are data, not enforced permissions.
data McpToolInfo = McpToolInfo
  { mcpToolServerName :: !McpServerName,
    mcpToolName :: !Text,
    mcpToolEnabled :: !Bool,
    mcpToolDescription :: !(Maybe Text),
    mcpToolReadOnly :: !(Maybe Bool),
    mcpToolInputSchema :: !(Maybe McpToolInputSchema),
    mcpToolAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpToolInfo where
  show _ = "McpToolInfo <redacted>"

instance FromJSON McpToolInfo where
  parseJSON = withObject "McpToolInfo" $ \fields -> McpToolInfo <$> fields .: "serverName" <*> fields .: "name" <*> fields .: "isEnabled" <*> fields .:! "description" <*> fields .:! "isReadOnly" <*> fields .:! "inputSchema" <*> pure (additionalFields toolKeys fields)

instance ToJSON McpToolInfo where
  toJSON tool = objectWithAdditionalFields toolKeys (mcpToolAdditionalFields tool) (["serverName" .= mcpToolServerName tool, "name" .= mcpToolName tool, "isEnabled" .= mcpToolEnabled tool] <> optionalField "description" (mcpToolDescription tool) <> optionalField "isReadOnly" (mcpToolReadOnly tool) <> optionalField "inputSchema" (mcpToolInputSchema tool))

-- | An ordered tool report, preserving empty lists and duplicates.
data ListMcpToolsResult = ListMcpToolsResult
  { listedMcpTools :: ![McpToolInfo],
    listedMcpToolsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListMcpToolsResult where
  show _ = "ListMcpToolsResult <redacted>"

instance FromJSON ListMcpToolsResult where
  parseJSON = withObject "ListMcpToolsResult" $ \fields -> ListMcpToolsResult <$> fields .: "tools" <*> pure (additionalFields ["tools"] fields)

instance ToJSON ListMcpToolsResult where
  toJSON result = objectWithAdditionalFields ["tools"] (listedMcpToolsAdditionalFields result) ["tools" .= listedMcpTools result]

-- | A reported configuration error; its text is not safe to log verbatim.
data McpConfigError = McpConfigError
  { mcpConfigErrorPath :: !Text,
    mcpConfigErrorMessage :: !Text,
    mcpConfigErrorAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpConfigError where
  show _ = "McpConfigError <redacted>"

instance FromJSON McpConfigError where
  parseJSON = withObject "McpConfigError" $ \fields -> McpConfigError <$> fields .: "path" <*> fields .: "message" <*> pure (additionalFields ["path", "message"] fields)

instance ToJSON McpConfigError where
  toJSON err = objectWithAdditionalFields ["path", "message"] (mcpConfigErrorAdditionalFields err) ["path" .= mcpConfigErrorPath err, "message" .= mcpConfigErrorMessage err]

-- | Aggregate counts in the schema's number domain, without sum constraints.
data McpStatusSummary = McpStatusSummary
  { mcpTotal :: !Scientific,
    mcpConnected :: !Scientific,
    mcpConnecting :: !Scientific,
    mcpFailed :: !Scientific,
    mcpDisabled :: !(Maybe Scientific),
    mcpConfigError :: !(Maybe McpConfigError),
    mcpSummaryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpStatusSummary where
  show _ = "McpStatusSummary <redacted>"

instance FromJSON McpStatusSummary where
  parseJSON = withObject "McpStatusSummary" $ \fields -> McpStatusSummary <$> fields .: "total" <*> fields .: "connected" <*> fields .: "connecting" <*> fields .: "failed" <*> fields .:! "disabled" <*> fields .:! "configError" <*> pure (additionalFields summaryKeys fields)

instance ToJSON McpStatusSummary where
  toJSON summary = objectWithAdditionalFields summaryKeys (mcpSummaryAdditionalFields summary) (["total" .= mcpTotal summary, "connected" .= mcpConnected summary, "connecting" .= mcpConnecting summary, "failed" .= mcpFailed summary] <> optionalField "disabled" (mcpDisabled summary) <> optionalField "configError" (mcpConfigError summary))

-- | Per-server status and authentication reports. Optional flags/state fields
-- are independent; the codec does not infer or perform authentication.
data McpServerStatusInfo = McpServerStatusInfo
  { mcpStatusName :: !Text,
    mcpStatus :: !McpServerStatus,
    mcpStatusSource :: !SettingsLevel,
    mcpStatusManaged :: !Bool,
    mcpStatusServerType :: !McpServerType,
    mcpStatusError :: !(Maybe Text),
    mcpStatusToolCount :: !(Maybe Scientific),
    mcpStatusHasAuthTokens :: !(Maybe Bool),
    mcpStatusRequiresAuth :: !(Maybe Bool),
    mcpStatusPendingAuthUrl :: !(Maybe Text),
    mcpStatusPendingAuthMessage :: !(Maybe Text),
    mcpStatusPendingAuthState :: !(Maybe Text),
    mcpStatusBlockedByPolicy :: !(Maybe Bool),
    mcpStatusAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpServerStatusInfo where
  show _ = "McpServerStatusInfo <redacted>"

instance FromJSON McpServerStatusInfo where
  parseJSON = withObject "McpServerStatusInfo" $ \fields ->
    McpServerStatusInfo
      <$> fields .: "name"
      <*> fields .: "status"
      <*> fields .: "source"
      <*> fields .: "isManaged"
      <*> fields .: "serverType"
      <*> fields .:! "error"
      <*> fields .:! "toolCount"
      <*> fields .:! "hasAuthTokens"
      <*> fields .:! "requiresAuth"
      <*> fields .:! "pendingAuthUrl"
      <*> fields .:! "pendingAuthMessage"
      <*> fields .:! "pendingAuthState"
      <*> fields .:! "blockedByPolicy"
      <*> pure (additionalFields statusKeys fields)

instance ToJSON McpServerStatusInfo where
  toJSON status =
    objectWithAdditionalFields statusKeys (mcpStatusAdditionalFields status) $
      ["name" .= mcpStatusName status, "status" .= mcpStatus status, "source" .= mcpStatusSource status, "isManaged" .= mcpStatusManaged status, "serverType" .= mcpStatusServerType status]
        <> optionalField "error" (mcpStatusError status)
        <> optionalField "toolCount" (mcpStatusToolCount status)
        <> optionalField "hasAuthTokens" (mcpStatusHasAuthTokens status)
        <> optionalField "requiresAuth" (mcpStatusRequiresAuth status)
        <> optionalField "pendingAuthUrl" (mcpStatusPendingAuthUrl status)
        <> optionalField "pendingAuthMessage" (mcpStatusPendingAuthMessage status)
        <> optionalField "pendingAuthState" (mcpStatusPendingAuthState status)
        <> optionalField "blockedByPolicy" (mcpStatusBlockedByPolicy status)

-- | Servers and their reported summary, without consistency normalization.
data ListMcpServersResult = ListMcpServersResult
  { listedMcpServers :: ![McpServerStatusInfo],
    listedMcpSummary :: !McpStatusSummary,
    listedMcpServersAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListMcpServersResult where
  show _ = "ListMcpServersResult <redacted>"

instance FromJSON ListMcpServersResult where
  parseJSON = withObject "ListMcpServersResult" $ \fields -> ListMcpServersResult <$> fields .: "servers" <*> fields .: "summary" <*> pure (additionalFields ["servers", "summary"] fields)

instance ToJSON ListMcpServersResult where
  toJSON = Object . serversObject

serversObject :: ListMcpServersResult -> Object
serversObject result = fieldsWithAdditionalFields ["servers", "summary"] (listedMcpServersAdditionalFields result) ["servers" .= listedMcpServers result, "summary" .= listedMcpSummary result]

-- | A status-change payload reusing the complete listing codec. Its required
-- type literal is not retained as an extension of the embedded listing.
newtype McpStatusChanged = McpStatusChanged {changedMcpStatus :: ListMcpServersResult}
  deriving stock (Eq)

instance Show McpStatusChanged where
  show _ = "McpStatusChanged <redacted>"

instance FromJSON McpStatusChanged where
  parseJSON = withObject "McpStatusChanged" $ \fields -> do
    requireLiteral "type" "mcp_status_changed" fields
    McpStatusChanged <$> parseJSON (Object (KeyMap.delete "type" fields))

instance ToJSON McpStatusChanged where
  toJSON = Object . KeyMap.insert "type" (String "mcp_status_changed") . serversObject . changedMcpStatus

-- | An authentication prompt from the peer. URLs/state remain opaque strings;
-- decoding neither opens a browser nor authorizes the callback destination.
data McpAuthRequired = McpAuthRequired
  { mcpAuthRequiredServer :: !Text,
    mcpAuthUrl :: !Text,
    mcpAuthRequiredMessage :: !Text,
    mcpAuthState :: !Text,
    mcpAuthRequiredAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpAuthRequired where
  show _ = "McpAuthRequired <redacted>"

instance FromJSON McpAuthRequired where
  parseJSON = withObject "McpAuthRequired" $ \fields -> do
    requireLiteral "type" "mcp_auth_required" fields
    McpAuthRequired <$> fields .: "serverName" <*> fields .: "authUrl" <*> fields .: "message" <*> fields .: "state" <*> pure (additionalFields authRequiredKeys fields)

instance ToJSON McpAuthRequired where
  toJSON event = objectWithAdditionalFields authRequiredKeys (mcpAuthRequiredAdditionalFields event) ["type" .= String "mcp_auth_required", "serverName" .= mcpAuthRequiredServer event, "authUrl" .= mcpAuthUrl event, "message" .= mcpAuthRequiredMessage event, "state" .= mcpAuthState event]

-- | An authentication completion report, without token retrieval or storage.
data McpAuthCompleted = McpAuthCompleted
  { mcpAuthCompletedServer :: !Text,
    mcpAuthOutcome :: !McpAuthOutcome,
    mcpAuthCompletedMessage :: !Text,
    mcpAuthCompletedAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpAuthCompleted where
  show _ = "McpAuthCompleted <redacted>"

instance FromJSON McpAuthCompleted where
  parseJSON = withObject "McpAuthCompleted" $ \fields -> do
    requireLiteral "type" "mcp_auth_completed" fields
    McpAuthCompleted <$> fields .: "serverName" <*> fields .: "outcome" <*> fields .: "message" <*> pure (additionalFields authCompletedKeys fields)

instance ToJSON McpAuthCompleted where
  toJSON event = objectWithAdditionalFields authCompletedKeys (mcpAuthCompletedAdditionalFields event) ["type" .= String "mcp_auth_completed", "serverName" .= mcpAuthCompletedServer event, "outcome" .= mcpAuthOutcome event, "message" .= mcpAuthCompletedMessage event]

-- | A server-name operation body; an empty name is allowed on the wire.
data McpServerNameParams = McpServerNameParams
  { requestedMcpServerName :: !McpServerName,
    mcpNameParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpServerNameParams where
  show _ = "McpServerNameParams <redacted>"

instance FromJSON McpServerNameParams where
  parseJSON = withObject "McpServerNameParams" $ \fields -> McpServerNameParams <$> fields .: "serverName" <*> pure (additionalFields ["serverName"] fields)

instance ToJSON McpServerNameParams where
  toJSON params = objectWithAdditionalFields ["serverName"] (mcpNameParamsAdditionalFields params) ["serverName" .= requestedMcpServerName params]

-- | Removal parameters with intrinsic user settings level. No server is removed.
data RemoveMcpServerParams = RemoveMcpServerParams
  { removedMcpServerName :: !McpServerName,
    removeMcpAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show RemoveMcpServerParams where
  show _ = "RemoveMcpServerParams <redacted>"

instance FromJSON RemoveMcpServerParams where
  parseJSON = withObject "RemoveMcpServerParams" $ \fields -> do
    requireLiteral "settingsLevel" "user" fields
    RemoveMcpServerParams <$> fields .: "serverName" <*> pure (additionalFields ["serverName", "settingsLevel"] fields)

instance ToJSON RemoveMcpServerParams where
  toJSON params = objectWithAdditionalFields ["serverName", "settingsLevel"] (removeMcpAdditionalFields params) ["serverName" .= removedMcpServerName params, "settingsLevel" .= McpUserSettings]

-- | Enablement parameters with intrinsic user settings level, not a mutation.
data ToggleMcpServerParams = ToggleMcpServerParams
  { toggledMcpServerName :: !McpServerName,
    toggledMcpServerEnabled :: !Bool,
    toggleMcpServerAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ToggleMcpServerParams where
  show _ = "ToggleMcpServerParams <redacted>"

instance FromJSON ToggleMcpServerParams where
  parseJSON = withObject "ToggleMcpServerParams" $ \fields -> do
    requireLiteral "settingsLevel" "user" fields
    ToggleMcpServerParams <$> fields .: "serverName" <*> fields .: "enabled" <*> pure (additionalFields ["serverName", "enabled", "settingsLevel"] fields)

instance ToJSON ToggleMcpServerParams where
  toJSON params = objectWithAdditionalFields ["serverName", "enabled", "settingsLevel"] (toggleMcpServerAdditionalFields params) ["serverName" .= toggledMcpServerName params, "enabled" .= toggledMcpServerEnabled params, "settingsLevel" .= McpUserSettings]

-- | Tool enablement parameters, without applying permissions or changing state.
data ToggleMcpToolParams = ToggleMcpToolParams
  { toggledMcpToolServer :: !McpServerName,
    toggledMcpToolName :: !Text,
    toggledMcpToolEnabled :: !Bool,
    toggleMcpToolAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ToggleMcpToolParams where
  show _ = "ToggleMcpToolParams <redacted>"

instance FromJSON ToggleMcpToolParams where
  parseJSON = withObject "ToggleMcpToolParams" $ \fields -> ToggleMcpToolParams <$> fields .: "serverName" <*> fields .: "toolName" <*> fields .: "enabled" <*> pure (additionalFields ["serverName", "toolName", "enabled"] fields)

instance ToJSON ToggleMcpToolParams where
  toJSON params = objectWithAdditionalFields ["serverName", "toolName", "enabled"] (toggleMcpToolAdditionalFields params) ["serverName" .= toggledMcpToolServer params, "toolName" .= toggledMcpToolName params, "enabled" .= toggledMcpToolEnabled params]

-- | An opaque authorization-code submission body. It performs no OAuth exchange.
data SubmitMcpAuthCodeParams = SubmitMcpAuthCodeParams
  { submittedMcpCodeServer :: !McpServerName,
    submittedMcpCode :: !Text,
    submittedMcpCodeState :: !Text,
    submittedMcpCodeAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SubmitMcpAuthCodeParams where
  show _ = "SubmitMcpAuthCodeParams <redacted>"

instance FromJSON SubmitMcpAuthCodeParams where
  parseJSON = withObject "SubmitMcpAuthCodeParams" $ \fields -> SubmitMcpAuthCodeParams <$> fields .: "serverName" <*> fields .: "code" <*> fields .: "state" <*> pure (additionalFields ["serverName", "code", "state"] fields)

instance ToJSON SubmitMcpAuthCodeParams where
  toJSON params = objectWithAdditionalFields ["serverName", "code", "state"] (submittedMcpCodeAdditionalFields params) ["serverName" .= submittedMcpCodeServer params, "code" .= submittedMcpCode params, "state" .= submittedMcpCodeState params]

-- | An OAuth callback error body. State and error descriptions remain opaque.
data SubmitMcpAuthErrorParams = SubmitMcpAuthErrorParams
  { submittedMcpErrorServer :: !McpServerName,
    submittedMcpError :: !Text,
    submittedMcpErrorState :: !Text,
    submittedMcpErrorDescription :: !(Maybe Text),
    submittedMcpErrorAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SubmitMcpAuthErrorParams where
  show _ = "SubmitMcpAuthErrorParams <redacted>"

instance FromJSON SubmitMcpAuthErrorParams where
  parseJSON = withObject "SubmitMcpAuthErrorParams" $ \fields -> SubmitMcpAuthErrorParams <$> fields .: "serverName" <*> fields .: "error" <*> fields .: "state" <*> fields .:! "errorDescription" <*> pure (additionalFields ["serverName", "error", "state", "errorDescription"] fields)

instance ToJSON SubmitMcpAuthErrorParams where
  toJSON params = objectWithAdditionalFields ["serverName", "error", "state", "errorDescription"] (submittedMcpErrorAdditionalFields params) (["serverName" .= submittedMcpErrorServer params, "error" .= submittedMcpError params, "state" .= submittedMcpErrorState params] <> optionalField "errorDescription" (submittedMcpErrorDescription params))

stdioKeys, registryKeys, inputKeys, toolKeys, summaryKeys, statusKeys, authRequiredKeys, authCompletedKeys :: [Key]
stdioKeys = ["name", "command", "args", "env"]
registryKeys = ["name", "description", "type", "url", "command", "args", "note", "logoUrl"]
inputKeys = ["type", "properties", "required"]
toolKeys = ["serverName", "name", "isEnabled", "description", "isReadOnly", "inputSchema"]
summaryKeys = ["total", "connected", "connecting", "failed", "disabled", "configError"]
statusKeys = ["name", "status", "source", "isManaged", "serverType", "error", "toolCount", "hasAuthTokens", "requiresAuth", "pendingAuthUrl", "pendingAuthMessage", "pendingAuthState", "blockedByPolicy"]
authRequiredKeys = ["type", "serverName", "authUrl", "message", "state"]
authCompletedKeys = ["type", "serverName", "outcome", "message"]
