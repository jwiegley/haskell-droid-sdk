{-# LANGUAGE OverloadedStrings #-}

-- | Operational MCP configuration. API boundaries validate and normalize these
-- records before I/O. URI fields require RFC 3986 syntax, not browser aliases.
module Factory.Droid.Schema.MCP.Config
  ( McpConfigurationError (..),
    validateMcpConfiguration,
    McpOAuthMethod (..),
    McpOAuthResource (..),
    McpOAuthOptions (..),
    emptyMcpOAuthOptions,
    McpOAuthConfig (..),
    McpRemoteConfig (..),
    McpServerConfig (..),
    McpSessionOptions (..),
    defaultMcpSessionOptions,
    mcpInitializeFields,
    mcpLoadFields,
    AddMcpServerParams (..),
    mcpServerParams,
    StoredStdioMcp (..),
    StoredRemoteMcp (..),
    StoredMcpConfig (..),
    McpConfigSource (..),
    McpServerInfo (..),
    GetMcpConfigResult (..),
    McpConfigAction (..),
    UpdateMcpConfigParams (..),
    UpdateMcpConfigResult (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16)
import Factory.Droid.Internal.JSON (additionalFields, isEcmaWhitespace, objectWithAdditionalFields, optionalField, requireLiteral)
import Factory.Droid.Schema.MCP (HttpHeader (..), McpServerStatus, McpServerType (..), StdioMcp (..))
import Network.URI (URI (..), URIAuth (..), isURI, parseURI)

data McpConfigurationError = InvalidMcpConfiguration | McpInitOnlyOptionOnResume
  deriving stock (Eq, Show)

instance Exception McpConfigurationError

-- | Normalize with the same parser used for wire input; errors never echo data.
validateMcpConfiguration :: (ToJSON a, FromJSON a) => a -> Either McpConfigurationError a
validateMcpConfiguration = either (const (Left InvalidMcpConfiguration)) Right . parseEither parseJSON . toJSON

data McpOAuthMethod = McpOAuthNone | McpClientSecretBasic | McpClientSecretPost
  deriving stock (Eq, Show)

instance FromJSON McpOAuthMethod where
  parseJSON = withText "McpOAuthMethod" $ \case
    "none" -> pure McpOAuthNone
    "client_secret_basic" -> pure McpClientSecretBasic
    "client_secret_post" -> pure McpClientSecretPost
    _ -> fail "Invalid MCP OAuth method"

instance ToJSON McpOAuthMethod where
  toJSON McpOAuthNone = String "none"
  toJSON McpClientSecretBasic = String "client_secret_basic"
  toJSON McpClientSecretPost = String "client_secret_post"

data McpOAuthResource = McpResourceURI !Text | McpResourceDisabled
  deriving stock (Eq)

instance Show McpOAuthResource where
  show _ = "McpOAuthResource <redacted>"

instance FromJSON McpOAuthResource where
  parseJSON (Bool False) = pure McpResourceDisabled
  parseJSON value = McpResourceURI <$> parseURIText value

instance ToJSON McpOAuthResource where
  toJSON (McpResourceURI value) = String value
  toJSON McpResourceDisabled = Bool False

data McpOAuthOptions = McpOAuthOptions
  { mcpOAuthScopes :: !(Maybe [Text]),
    mcpOAuthResource :: !(Maybe McpOAuthResource),
    mcpOAuthIssuer :: !(Maybe Text),
    mcpOAuthMetadataUrl :: !(Maybe Text),
    mcpOAuthClientId :: !(Maybe Text),
    mcpOAuthClientSecret :: !(Maybe Text),
    mcpOAuthCallbackPort :: !(Maybe Word16),
    mcpOAuthMethod :: !(Maybe McpOAuthMethod),
    mcpOAuthAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpOAuthOptions where
  show _ = "McpOAuthOptions <redacted>"

emptyMcpOAuthOptions :: McpOAuthOptions
emptyMcpOAuthOptions = McpOAuthOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON McpOAuthOptions where
  parseJSON = withObject "McpOAuthOptions" $ \fields -> do
    scopes <- fields .:! "scopes" >>= traverse (traverse nonblank)
    resource <- fields .:! "resource"
    issuer <- fields .:! "authorizationServerIssuer" >>= traverse parseURIText
    metadata <- fields .:! "clientMetadataUrl" >>= traverse parseURIText
    client <- fields .:! "clientId" >>= traverse nonblank
    secret <- fields .:! "clientSecret"
    forM_ secret $ \value -> unless (Text.any (not . isEcmaWhitespace) value) (fail "Blank MCP client secret")
    port <- fields .:! "callbackPort"
    when (port == Just 0) (fail "Invalid MCP callback port")
    method <- fields .:! "tokenEndpointAuthMethod"
    forM_ metadata $ \value -> unless (validMetadataURI value) (fail "Invalid MCP client metadata URI")
    let credentials = isJust client || isJust secret
    when (isJust metadata && credentials) (fail "MCP metadata URI conflicts with client credentials")
    when (isJust metadata && maybe False (/= McpOAuthNone) method) (fail "MCP metadata URI requires public authentication")
    when (credentials && isNothing issuer) (fail "MCP client credentials require an issuer")
    pure (McpOAuthOptions scopes resource issuer metadata client secret port method (additionalFields oauthKeys fields))
    where
      nonblank value = let trimmed = Text.dropAround isEcmaWhitespace value in if Text.null trimmed then fail "Blank MCP OAuth value" else pure trimmed

instance ToJSON McpOAuthOptions where
  toJSON value =
    objectWithAdditionalFields oauthKeys (mcpOAuthAdditionalFields value) $
      optionalField "scopes" (mcpOAuthScopes value)
        <> optionalField "resource" (mcpOAuthResource value)
        <> optionalField "authorizationServerIssuer" (mcpOAuthIssuer value)
        <> optionalField "clientMetadataUrl" (mcpOAuthMetadataUrl value)
        <> optionalField "clientId" (mcpOAuthClientId value)
        <> optionalField "clientSecret" (mcpOAuthClientSecret value)
        <> optionalField "callbackPort" (mcpOAuthCallbackPort value)
        <> optionalField "tokenEndpointAuthMethod" (mcpOAuthMethod value)

data McpOAuthConfig = McpOAuthDisabled | McpOAuthEnabled !McpOAuthOptions
  deriving stock (Eq)

instance Show McpOAuthConfig where
  show _ = "McpOAuthConfig <redacted>"

instance FromJSON McpOAuthConfig where
  parseJSON (Bool False) = pure McpOAuthDisabled
  parseJSON value = McpOAuthEnabled <$> parseJSON value

instance ToJSON McpOAuthConfig where
  toJSON McpOAuthDisabled = Bool False
  toJSON (McpOAuthEnabled options) = toJSON options

-- | Startup headers are an ordered array, not the map used by add/config RPCs.
data McpRemoteConfig = McpRemoteConfig
  { mcpRemoteName :: !Text,
    mcpRemoteUrl :: !Text,
    mcpRemoteHeaders :: !(Maybe [HttpHeader]),
    mcpRemoteOAuth :: !(Maybe McpOAuthConfig),
    mcpRemoteAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpRemoteConfig where
  show _ = "McpRemoteConfig <redacted>"

data McpServerConfig = McpStdioConfig !StdioMcp | McpHttpConfig !McpRemoteConfig | McpSseConfig !McpRemoteConfig
  deriving stock (Eq)

instance Show McpServerConfig where
  show _ = "McpServerConfig <redacted>"

instance FromJSON McpServerConfig where
  parseJSON value = stdio <|> remote "http" McpHttpConfig <|> remote "sse" McpSseConfig
    where
      stdio = do
        parsed <- parseJSON value
        pure (McpStdioConfig (parsed {stdioMcpAdditionalFields = additionalFields startupKeys (stdioMcpAdditionalFields parsed)}))
      remote kind constructor =
        withObject
          "McpServerConfig"
          ( \fields -> do
              requireLiteral "type" (String kind) fields
              name <- fields .: "name"
              url <- fields .: "url" >>= parseURIText
              constructor <$> (McpRemoteConfig name url <$> fields .:! "headers" <*> fields .:! "oauth" <*> pure (additionalFields startupKeys fields))
          )
          value

instance ToJSON McpServerConfig where
  toJSON (McpStdioConfig value) = toJSON (value {stdioMcpAdditionalFields = additionalFields startupKeys (stdioMcpAdditionalFields value)})
  toJSON (McpHttpConfig value) = remoteConfigValue "http" value
  toJSON (McpSseConfig value) = remoteConfigValue "sse" value

remoteConfigValue :: Text -> McpRemoteConfig -> Value
remoteConfigValue kind value =
  objectWithAdditionalFields startupKeys (mcpRemoteAdditionalFields value) $
    ["type" .= kind, "name" .= mcpRemoteName value, "url" .= mcpRemoteUrl value]
      <> optionalField "headers" (mcpRemoteHeaders value)
      <> optionalField "oauth" (mcpRemoteOAuth value)

-- | Omission and an explicit empty server list remain distinct. This is a
-- forwarding policy, not a promise about filesystem precedence or persistence.
data McpSessionOptions = McpSessionOptions
  { sessionMcpServers :: !(Maybe [McpServerConfig]),
    sessionMcpOAuthCallbackUri :: !(Maybe Text),
    sessionBlockOnMcpLoad :: !(Maybe Bool)
  }
  deriving stock (Eq)

instance Show McpSessionOptions where
  show _ = "McpSessionOptions <redacted>"

defaultMcpSessionOptions :: McpSessionOptions
defaultMcpSessionOptions = McpSessionOptions Nothing Nothing Nothing

instance FromJSON McpSessionOptions where
  parseJSON = withObject "McpSessionOptions" $ \fields -> McpSessionOptions <$> fields .:! "mcpServers" <*> fields .:! "mcpOAuthCallbackUri" <*> fields .:! "blockOnMcpLoad"

instance ToJSON McpSessionOptions where
  toJSON = Object . mcpInitializeFields

mcpInitializeFields :: McpSessionOptions -> Object
mcpInitializeFields options = KeyMap.union (mcpLoadFields options) (KeyMap.fromList (optionalField "blockOnMcpLoad" (sessionBlockOnMcpLoad options)))

mcpLoadFields :: McpSessionOptions -> Object
mcpLoadFields options = KeyMap.fromList (optionalField "mcpServers" (sessionMcpServers options) <> optionalField "mcpOAuthCallbackUri" (sessionMcpOAuthCallbackUri options))

-- | The add operation permits optional transport fields rather than requiring
-- one complete startup shape. The peer's result remains authoritative.
data AddMcpServerParams = AddMcpServerParams
  { addedMcpName :: !Text,
    addedMcpType :: !McpServerType,
    addedMcpCommand :: !(Maybe Text),
    addedMcpArgs :: !(Maybe [Text]),
    addedMcpEnv :: !(Maybe (KeyMap Text)),
    addedMcpUrl :: !(Maybe Text),
    addedMcpHeaders :: !(Maybe (KeyMap Text)),
    addedMcpOAuth :: !(Maybe McpOAuthConfig),
    addMcpAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AddMcpServerParams where
  show _ = "AddMcpServerParams <redacted>"

instance FromJSON AddMcpServerParams where
  parseJSON = withObject "AddMcpServerParams" $ \fields -> AddMcpServerParams <$> fields .: "name" <*> fields .: "type" <*> fields .:! "command" <*> fields .:! "args" <*> fields .:! "env" <*> fields .:! "url" <*> fields .:! "headers" <*> fields .:! "oauth" <*> pure (additionalFields startupKeys fields)

instance ToJSON AddMcpServerParams where
  toJSON value =
    objectWithAdditionalFields startupKeys (addMcpAdditionalFields value) $
      ["name" .= addedMcpName value, "type" .= addedMcpType value]
        <> optionalField "command" (addedMcpCommand value)
        <> optionalField "args" (addedMcpArgs value)
        <> optionalField "env" (addedMcpEnv value)
        <> optionalField "url" (addedMcpUrl value)
        <> optionalField "headers" (addedMcpHeaders value)
        <> optionalField "oauth" (addedMcpOAuth value)

-- | Header conversion follows the reference SDK: last exact-name value wins.
mcpServerParams :: McpServerConfig -> AddMcpServerParams
mcpServerParams (McpStdioConfig value) = AddMcpServerParams (stdioMcpName value) McpStdio (Just (stdioMcpCommand value)) (stdioMcpArgs value) (stdioMcpEnv value) Nothing Nothing Nothing (stdioMcpAdditionalFields value)
mcpServerParams (McpHttpConfig value) = remoteParams McpHttp value
mcpServerParams (McpSseConfig value) = remoteParams McpSse value

remoteParams :: McpServerType -> McpRemoteConfig -> AddMcpServerParams
remoteParams kind value = AddMcpServerParams (mcpRemoteName value) kind Nothing Nothing Nothing (Just (mcpRemoteUrl value)) (KeyMap.fromList . map (\header -> (Key.fromText (httpHeaderName header), httpHeaderValue header)) <$> mcpRemoteHeaders value) (mcpRemoteOAuth value) (mcpRemoteAdditionalFields value)

-- | Global daemon configuration has required stdio args and an optional type.
data StoredStdioMcp = StoredStdioMcp
  { storedMcpCommand :: !Text,
    storedMcpArgs :: ![Text],
    storedMcpEnv :: !(Maybe (KeyMap Text)),
    storedMcpDisabled :: !(Maybe Bool),
    storedMcpExplicitType :: !Bool,
    storedStdioAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show StoredStdioMcp where
  show _ = "StoredStdioMcp <redacted>"

instance FromJSON StoredStdioMcp where
  parseJSON = withObject "StoredStdioMcp" $ \fields -> do
    kind <- fields .:! "type"
    unless (maybe True (== ("stdio" :: Text)) kind) (fail "Invalid stored stdio type")
    StoredStdioMcp <$> fields .: "command" <*> fields .: "args" <*> fields .:! "env" <*> fields .:! "disabled" <*> pure (isJust kind) <*> pure (additionalFields storedKeys fields)

instance ToJSON StoredStdioMcp where
  toJSON value =
    objectWithAdditionalFields storedKeys (storedStdioAdditionalFields value) $
      ["command" .= storedMcpCommand value, "args" .= storedMcpArgs value]
        <> ["type" .= String "stdio" | storedMcpExplicitType value]
        <> optionalField "env" (storedMcpEnv value)
        <> optionalField "disabled" (storedMcpDisabled value)

data StoredRemoteMcp oauth = StoredRemoteMcp
  { storedMcpUrl :: !Text,
    storedMcpHeaders :: !(Maybe (KeyMap Text)),
    storedMcpOAuth :: !(Maybe oauth),
    storedRemoteDisabled :: !(Maybe Bool),
    storedRemoteAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show (StoredRemoteMcp oauth) where
  show _ = "StoredRemoteMcp <redacted>"

-- | Reads/add accept OAuth false; global updates deliberately require options.
data StoredMcpConfig oauth = StoredMcpStdio !StoredStdioMcp | StoredMcpHttp !(StoredRemoteMcp oauth) | StoredMcpSse !(StoredRemoteMcp oauth)
  deriving stock (Eq)

instance Show (StoredMcpConfig oauth) where
  show _ = "StoredMcpConfig <redacted>"

instance (FromJSON oauth) => FromJSON (StoredMcpConfig oauth) where
  parseJSON value = (StoredMcpStdio <$> parseJSON value) <|> remote "http" StoredMcpHttp <|> remote "sse" StoredMcpSse
    where
      remote kind constructor =
        withObject
          "StoredMcpConfig"
          ( \fields -> do
              requireLiteral "type" (String kind) fields
              constructor <$> (StoredRemoteMcp <$> fields .: "url" <*> fields .:! "headers" <*> fields .:! "oauth" <*> fields .:! "disabled" <*> pure (additionalFields storedKeys fields))
          )
          value

instance (ToJSON oauth) => ToJSON (StoredMcpConfig oauth) where
  toJSON (StoredMcpStdio value) = toJSON value
  toJSON (StoredMcpHttp value) = storedRemoteValue "http" value
  toJSON (StoredMcpSse value) = storedRemoteValue "sse" value

storedRemoteValue :: (ToJSON oauth) => Text -> StoredRemoteMcp oauth -> Value
storedRemoteValue kind value =
  objectWithAdditionalFields storedKeys (storedRemoteAdditionalFields value) $
    ["type" .= kind, "url" .= storedMcpUrl value]
      <> optionalField "headers" (storedMcpHeaders value)
      <> optionalField "oauth" (storedMcpOAuth value)
      <> optionalField "disabled" (storedRemoteDisabled value)

data McpConfigSource = McpUserConfig | McpProjectConfig
  deriving stock (Eq, Show)

instance FromJSON McpConfigSource where
  parseJSON = withText "McpConfigSource" $ \case
    "user" -> pure McpUserConfig
    "project" -> pure McpProjectConfig
    _ -> fail "Invalid MCP configuration source"

instance ToJSON McpConfigSource where
  toJSON McpUserConfig = String "user"
  toJSON McpProjectConfig = String "project"

data McpServerInfo = McpServerInfo
  { configuredMcpName :: !Text,
    configuredMcpServer :: !(StoredMcpConfig McpOAuthConfig),
    configuredMcpSource :: !McpConfigSource,
    configuredMcpStatus :: !(Maybe McpServerStatus),
    configuredMcpError :: !(Maybe Text),
    configuredMcpAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpServerInfo where
  show _ = "McpServerInfo <redacted>"

instance FromJSON McpServerInfo where
  parseJSON = withObject "McpServerInfo" $ \fields -> McpServerInfo <$> fields .: "name" <*> fields .: "config" <*> fields .: "source" <*> fields .:! "status" <*> fields .:! "error" <*> pure (additionalFields infoKeys fields)

instance ToJSON McpServerInfo where
  toJSON value = objectWithAdditionalFields infoKeys (configuredMcpAdditionalFields value) (["name" .= configuredMcpName value, "config" .= configuredMcpServer value, "source" .= configuredMcpSource value] <> optionalField "status" (configuredMcpStatus value) <> optionalField "error" (configuredMcpError value))

data GetMcpConfigResult = GetMcpConfigResult
  { configuredMcpServers :: ![McpServerInfo],
    getMcpConfigAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GetMcpConfigResult where
  show _ = "GetMcpConfigResult <redacted>"

instance FromJSON GetMcpConfigResult where
  parseJSON = withObject "GetMcpConfigResult" $ \fields -> GetMcpConfigResult <$> fields .: "servers" <*> pure (additionalFields ["servers"] fields)

instance ToJSON GetMcpConfigResult where
  toJSON value = objectWithAdditionalFields ["servers"] (getMcpConfigAdditionalFields value) ["servers" .= configuredMcpServers value]

data McpConfigAction = McpConfigAdd | McpConfigRemove | McpConfigEnable | McpConfigDisable
  deriving stock (Eq, Show)

instance FromJSON McpConfigAction where
  parseJSON = withText "McpConfigAction" $ \case
    "add" -> pure McpConfigAdd
    "remove" -> pure McpConfigRemove
    "enable" -> pure McpConfigEnable
    "disable" -> pure McpConfigDisable
    _ -> fail "Invalid MCP configuration action"

instance ToJSON McpConfigAction where
  toJSON McpConfigAdd = String "add"
  toJSON McpConfigRemove = String "remove"
  toJSON McpConfigEnable = String "enable"
  toJSON McpConfigDisable = String "disable"

data UpdateMcpConfigParams = UpdateMcpConfigParams
  { mcpConfigAction :: !McpConfigAction,
    updatedMcpNames :: ![Text],
    updatedMcpConfig :: !(Maybe (StoredMcpConfig McpOAuthOptions)),
    updateMcpConfigAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateMcpConfigParams where
  show _ = "UpdateMcpConfigParams <redacted>"

instance FromJSON UpdateMcpConfigParams where
  parseJSON = withObject "UpdateMcpConfigParams" $ \fields -> UpdateMcpConfigParams <$> fields .: "action" <*> fields .: "serverNames" <*> fields .:! "serverConfig" <*> pure (additionalFields ["action", "serverNames", "serverConfig"] fields)

instance ToJSON UpdateMcpConfigParams where
  toJSON value = objectWithAdditionalFields ["action", "serverNames", "serverConfig"] (updateMcpConfigAdditionalFields value) (["action" .= mcpConfigAction value, "serverNames" .= updatedMcpNames value] <> optionalField "serverConfig" (updatedMcpConfig value))

data UpdateMcpConfigResult = UpdateMcpConfigResult
  { mcpConfigSuccess :: !Bool,
    mcpConfigServers :: ![McpServerInfo],
    mcpConfigUpdateError :: !(Maybe Text),
    mcpConfigResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateMcpConfigResult where
  show _ = "UpdateMcpConfigResult <redacted>"

instance FromJSON UpdateMcpConfigResult where
  parseJSON = withObject "UpdateMcpConfigResult" $ \fields -> UpdateMcpConfigResult <$> fields .: "success" <*> fields .: "servers" <*> fields .:! "error" <*> pure (additionalFields ["success", "servers", "error"] fields)

instance ToJSON UpdateMcpConfigResult where
  toJSON value = objectWithAdditionalFields ["success", "servers", "error"] (mcpConfigResultAdditionalFields value) (["success" .= mcpConfigSuccess value, "servers" .= mcpConfigServers value] <> optionalField "error" (mcpConfigUpdateError value))

parseURIText :: Value -> Parser Text
parseURIText = withText "MCP URI" $ \value -> if isURI (Text.unpack value) then pure value else fail "Invalid MCP URI"

validMetadataURI :: Text -> Bool
validMetadataURI value =
  not dotSegments && case parseURI (Text.unpack value) of
    Just uri ->
      Text.toLower (Text.pack (uriScheme uri)) == "https:"
        && maybe False (\authority -> not (null (uriRegName authority)) && uriUserInfo authority `elem` ["", "@", ":@"]) (uriAuthority uri)
        && uriPath uri `notElem` ["", "/"]
        && uriQuery uri `elem` ["", "?"]
        && uriFragment uri `elem` ["", "#"]
    Nothing -> False
  where
    dotSegments = any ((`elem` [".", ".."]) . Text.takeWhile (`notElem` ("?#" :: String))) (Text.splitOn "/" (Text.replace "%2e" "." (Text.toLower value)))

oauthKeys, startupKeys, storedKeys, infoKeys :: [Key]
oauthKeys = ["scopes", "resource", "authorizationServerIssuer", "clientMetadataUrl", "clientId", "clientSecret", "callbackPort", "tokenEndpointAuthMethod"]
startupKeys = ["name", "type", "command", "args", "env", "url", "headers", "oauth"]
storedKeys = ["type", "command", "args", "env", "url", "headers", "oauth", "disabled"]
infoKeys = ["name", "config", "source", "status", "error"]
