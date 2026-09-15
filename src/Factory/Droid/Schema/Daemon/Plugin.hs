{-# LANGUAGE OverloadedStrings #-}

-- | Plugin/marketplace wire data. Paths, sources and scopes are daemon data;
-- these codecs do not inspect Git repositories, install files or execute plugins.
module Factory.Droid.Schema.Daemon.Plugin
  ( MarketplaceGitRef,
    mkMarketplaceGitRef,
    marketplaceGitRefText,
    MarketplaceGitSha,
    mkMarketplaceGitSha,
    marketplaceGitShaText,
    MarketplaceSource (..),
    ReportedMarketplaceSource (..),
    PluginActivationReason (..),
    MarketplaceProvisionedBy (..),
    AvailablePlugin (..),
    InstalledPlugin (..),
    MarketplaceInfo (..),
    PluginList (..),
    ListAvailablePluginsResult,
    ListInstalledPluginsResult,
    ListMarketplacesResult (..),
    PluginScopeParams (..),
    InstallPluginParams (..),
    PluginTargetParams (..),
    SetPluginEnabledParams (..),
    UpdatePluginParams (..),
    defaultUpdatePluginParams,
    AddMarketplaceParams (..),
    MarketplaceNameParams (..),
    UpdateMarketplaceParams (..),
    InstallPluginResult (..),
    AddMarketplaceResult (..),
    PluginUpdateEntry (..),
    MarketplaceUpdateEntry (..),
    UpdateBatch (..),
    UpdatePluginResult,
    UpdateMarketplaceResult,
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON (additionalFields, fieldsWithAdditionalFields, isEcmaWhitespace, objectWithAdditionalFields, optionalField)

newtype MarketplaceGitRef = MarketplaceGitRef Text deriving stock (Eq, Ord)

instance Show MarketplaceGitRef where show _ = "MarketplaceGitRef <redacted>"

-- | Match the SDK's trim/min(1), not Git's stricter ref-name grammar.
mkMarketplaceGitRef :: Text -> Maybe MarketplaceGitRef
mkMarketplaceGitRef value = let trimmed = Text.dropAround isEcmaWhitespace value in if Text.null trimmed then Nothing else Just (MarketplaceGitRef trimmed)

marketplaceGitRefText :: MarketplaceGitRef -> Text
marketplaceGitRefText (MarketplaceGitRef value) = value

instance FromJSON MarketplaceGitRef where
  parseJSON = withText "MarketplaceGitRef" $ maybe (fail "Empty Git reference") pure . mkMarketplaceGitRef

instance ToJSON MarketplaceGitRef where toJSON = String . marketplaceGitRefText

newtype MarketplaceGitSha = MarketplaceGitSha Text deriving stock (Eq, Ord)

instance Show MarketplaceGitSha where show _ = "MarketplaceGitSha <redacted>"

-- | The baselined runtime regex is case-insensitive; retain the original case.
mkMarketplaceGitSha :: Text -> Maybe MarketplaceGitSha
mkMarketplaceGitSha value
  | Text.length value == 40 && Text.all (`elem` ("0123456789abcdefABCDEF" :: String)) value = Just (MarketplaceGitSha value)
  | otherwise = Nothing

marketplaceGitShaText :: MarketplaceGitSha -> Text
marketplaceGitShaText (MarketplaceGitSha value) = value

instance FromJSON MarketplaceGitSha where
  parseJSON = withText "MarketplaceGitSha" $ maybe (fail "Expected a full Git SHA") pure . mkMarketplaceGitSha

instance ToJSON MarketplaceGitSha where toJSON = String . marketplaceGitShaText

-- | Source objects project declared fields like the baselined SDK. In
-- particular, reported local sources below cannot retain a private path.
data MarketplaceSource
  = GithubMarketplace !Text !(Maybe MarketplaceGitRef) !(Maybe MarketplaceGitSha)
  | UrlMarketplace !Text !(Maybe MarketplaceGitRef) !(Maybe MarketplaceGitSha)
  | LocalMarketplace !Text
  | GitSubdirMarketplace !Text !Text !(Maybe MarketplaceGitRef) !(Maybe MarketplaceGitSha)
  deriving stock (Eq)

instance Show MarketplaceSource where show _ = "MarketplaceSource <redacted>"

instance FromJSON MarketplaceSource where
  parseJSON = withObject "MarketplaceSource" $ \fields -> do
    kind <- fields .: "source" :: Parser Text
    case kind of
      "github" -> GithubMarketplace <$> fields .: "repo" <*> fields .:! "ref" <*> fields .:! "sha"
      "url" -> UrlMarketplace <$> fields .: "url" <*> fields .:! "ref" <*> fields .:! "sha"
      "local" -> LocalMarketplace <$> fields .: "path"
      "git-subdir" -> GitSubdirMarketplace <$> fields .: "url" <*> fields .: "path" <*> fields .:! "ref" <*> fields .:! "sha"
      _ -> fail "Unknown marketplace source"

instance ToJSON MarketplaceSource where
  toJSON (GithubMarketplace repo ref sha) = object (["source" .= String "github", "repo" .= repo] <> optionalField "ref" ref <> optionalField "sha" sha)
  toJSON (UrlMarketplace url ref sha) = object (["source" .= String "url", "url" .= url] <> optionalField "ref" ref <> optionalField "sha" sha)
  toJSON (LocalMarketplace path) = object ["source" .= String "local", "path" .= path]
  toJSON (GitSubdirMarketplace url path ref sha) = object (["source" .= String "git-subdir", "url" .= url, "path" .= path] <> optionalField "ref" ref <> optionalField "sha" sha)

data ReportedMarketplaceSource
  = ReportedGithubMarketplace !Text
  | ReportedUrlMarketplace !Text
  | ReportedLocalMarketplace
  | ReportedGitSubdirMarketplace !Text !Text
  deriving stock (Eq)

instance Show ReportedMarketplaceSource where show _ = "ReportedMarketplaceSource <redacted>"

instance FromJSON ReportedMarketplaceSource where
  parseJSON = withObject "ReportedMarketplaceSource" $ \fields -> do
    kind <- fields .: "source" :: Parser Text
    case kind of
      "github" -> ReportedGithubMarketplace <$> fields .: "repo"
      "url" -> ReportedUrlMarketplace <$> fields .: "url"
      "local" -> pure ReportedLocalMarketplace
      "git-subdir" -> ReportedGitSubdirMarketplace <$> fields .: "url" <*> fields .: "path"
      _ -> fail "Unknown reported marketplace source"

instance ToJSON ReportedMarketplaceSource where
  toJSON (ReportedGithubMarketplace repo) = object ["source" .= String "github", "repo" .= repo]
  toJSON (ReportedUrlMarketplace url) = object ["source" .= String "url", "url" .= url]
  toJSON ReportedLocalMarketplace = object ["source" .= String "local"]
  toJSON (ReportedGitSubdirMarketplace url path) = object ["source" .= String "git-subdir", "url" .= url, "path" .= path]

data PluginActivationReason = PluginEnabled | PluginNotEnabled deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON PluginActivationReason where
  parseJSON = withText "PluginActivationReason" $ \case "enabled" -> pure PluginEnabled; "not enabled" -> pure PluginNotEnabled; _ -> fail "Unknown plugin activation reason"

instance ToJSON PluginActivationReason where
  toJSON PluginEnabled = String "enabled"
  toJSON PluginNotEnabled = String "not enabled"

data MarketplaceProvisionedBy = MarketplaceOrg | MarketplaceProject deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON MarketplaceProvisionedBy where
  parseJSON = withText "MarketplaceProvisionedBy" $ \case "org" -> pure MarketplaceOrg; "project" -> pure MarketplaceProject; _ -> fail "Unknown marketplace provisioning source"

instance ToJSON MarketplaceProvisionedBy where
  toJSON MarketplaceOrg = String "org"
  toJSON MarketplaceProject = String "project"

data AvailablePlugin = AvailablePlugin
  { availablePluginName :: !Text,
    availablePluginMarketplace :: !Text,
    availablePluginDescription :: !(Maybe Text),
    availablePluginAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AvailablePlugin where show _ = "AvailablePlugin <redacted>"

instance FromJSON AvailablePlugin where
  parseJSON = withObject "AvailablePlugin" $ \fields -> AvailablePlugin <$> fields .: "name" <*> fields .: "marketplace" <*> fields .:! "description" <*> pure (additionalFields ["name", "marketplace", "description"] fields)

instance ToJSON AvailablePlugin where
  toJSON value = objectWithAdditionalFields ["name", "marketplace", "description"] (availablePluginAdditionalFields value) (["name" .= availablePluginName value, "marketplace" .= availablePluginMarketplace value] <> optionalField "description" (availablePluginDescription value))

data InstalledPlugin = InstalledPlugin
  { installedPluginId :: !Text,
    installedPluginScope :: !Text,
    installedPluginVersion :: !Text,
    installedPluginPath :: !Text,
    pluginInstalledAt :: !Text,
    pluginLastUpdated :: !Text,
    installedPluginSource :: !Text,
    installedPluginActive :: !(Maybe Bool),
    installedPluginManaged :: !(Maybe Bool),
    installedPluginReason :: !(Maybe PluginActivationReason),
    installedPluginAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show InstalledPlugin where show _ = "InstalledPlugin <redacted>"

instance FromJSON InstalledPlugin where
  parseJSON = withObject "InstalledPlugin" $ \fields -> InstalledPlugin <$> fields .: "id" <*> fields .: "scope" <*> fields .: "version" <*> fields .: "installPath" <*> fields .: "installedAt" <*> fields .: "lastUpdated" <*> fields .: "source" <*> fields .:! "active" <*> fields .:! "managed" <*> fields .:! "reason" <*> pure (additionalFields installedPluginKeys fields)

instance ToJSON InstalledPlugin where
  toJSON value = objectWithAdditionalFields installedPluginKeys (installedPluginAdditionalFields value) (["id" .= installedPluginId value, "scope" .= installedPluginScope value, "version" .= installedPluginVersion value, "installPath" .= installedPluginPath value, "installedAt" .= pluginInstalledAt value, "lastUpdated" .= pluginLastUpdated value, "source" .= installedPluginSource value] <> optionalField "active" (installedPluginActive value) <> optionalField "managed" (installedPluginManaged value) <> optionalField "reason" (installedPluginReason value))

-- | Missing removable is unknown, not permission inferred from provenance.
data MarketplaceInfo = MarketplaceInfo
  { marketplaceInfoName :: !Text,
    marketplaceDisplayName :: !(Maybe Text),
    marketplaceInfoSource :: !ReportedMarketplaceSource,
    marketplacePluginCount :: !Scientific,
    marketplaceAutoUpdate :: !Bool,
    marketplaceProvisionedBy :: !(Maybe MarketplaceProvisionedBy),
    marketplaceRemovable :: !(Maybe Bool),
    marketplaceAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show MarketplaceInfo where show _ = "MarketplaceInfo <redacted>"

instance FromJSON MarketplaceInfo where
  parseJSON = withObject "MarketplaceInfo" $ \fields -> MarketplaceInfo <$> fields .: "name" <*> fields .:! "displayName" <*> fields .: "source" <*> fields .: "pluginCount" <*> fields .: "autoUpdate" <*> fields .:! "provisionedBy" <*> fields .:! "removable" <*> pure (additionalFields marketplaceKeys fields)

instance ToJSON MarketplaceInfo where
  toJSON value = objectWithAdditionalFields marketplaceKeys (marketplaceAdditionalFields value) (["name" .= marketplaceInfoName value, "source" .= marketplaceInfoSource value, "pluginCount" .= marketplacePluginCount value, "autoUpdate" .= marketplaceAutoUpdate value] <> optionalField "displayName" (marketplaceDisplayName value) <> optionalField "provisionedBy" (marketplaceProvisionedBy value) <> optionalField "removable" (marketplaceRemovable value))

data PluginList a = PluginList
  { listedPlugins :: ![a],
    pluginListAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show (PluginList a) where show _ = "PluginList <redacted>"

instance (FromJSON a) => FromJSON (PluginList a) where
  parseJSON = withObject "PluginList" $ \fields -> PluginList <$> fields .: "plugins" <*> pure (additionalFields ["plugins"] fields)

instance (ToJSON a) => ToJSON (PluginList a) where
  toJSON value = objectWithAdditionalFields ["plugins"] (pluginListAdditionalFields value) ["plugins" .= listedPlugins value]

type ListAvailablePluginsResult = PluginList AvailablePlugin

type ListInstalledPluginsResult = PluginList InstalledPlugin

data ListMarketplacesResult = ListMarketplacesResult
  { listedMarketplaces :: ![MarketplaceInfo],
    marketplaceListAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListMarketplacesResult where show _ = "ListMarketplacesResult <redacted>"

instance FromJSON ListMarketplacesResult where
  parseJSON = withObject "ListMarketplacesResult" $ \fields -> ListMarketplacesResult <$> fields .: "marketplaces" <*> pure (additionalFields ["marketplaces"] fields)

instance ToJSON ListMarketplacesResult where
  toJSON value = objectWithAdditionalFields ["marketplaces"] (marketplaceListAdditionalFields value) ["marketplaces" .= listedMarketplaces value]

data PluginScopeParams = PluginScopeParams
  { pluginScopeFilter :: !(Maybe Text),
    pluginScopeAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PluginScopeParams where show _ = "PluginScopeParams <redacted>"

instance FromJSON PluginScopeParams where
  parseJSON = withObject "PluginScopeParams" $ \fields -> PluginScopeParams <$> fields .:! "scope" <*> pure (additionalFields ["scope"] fields)

instance ToJSON PluginScopeParams where
  toJSON value = objectWithAdditionalFields ["scope"] (pluginScopeAdditionalFields value) (optionalField "scope" (pluginScopeFilter value))

data InstallPluginParams = InstallPluginParams
  { installMarketplace :: !Text,
    installPluginName :: !Text,
    installPluginScope :: !Text,
    installPluginAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show InstallPluginParams where show _ = "InstallPluginParams <redacted>"

instance FromJSON InstallPluginParams where
  parseJSON = withObject "InstallPluginParams" $ \fields -> InstallPluginParams <$> fields .: "marketplace" <*> fields .: "pluginName" <*> fields .: "scope" <*> pure (additionalFields ["marketplace", "pluginName", "scope"] fields)

instance ToJSON InstallPluginParams where
  toJSON value = objectWithAdditionalFields ["marketplace", "pluginName", "scope"] (installPluginAdditionalFields value) ["marketplace" .= installMarketplace value, "pluginName" .= installPluginName value, "scope" .= installPluginScope value]

data PluginTargetParams = PluginTargetParams
  { targetPluginId :: !Text,
    targetPluginScope :: !Text,
    targetPluginAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PluginTargetParams where show _ = "PluginTargetParams <redacted>"

instance FromJSON PluginTargetParams where
  parseJSON = withObject "PluginTargetParams" $ \fields -> PluginTargetParams <$> fields .: "pluginId" <*> fields .: "scope" <*> pure (additionalFields ["pluginId", "scope"] fields)

instance ToJSON PluginTargetParams where toJSON = Object . pluginTargetObject

pluginTargetObject :: PluginTargetParams -> Object
pluginTargetObject value = fieldsWithAdditionalFields ["pluginId", "scope"] (targetPluginAdditionalFields value) ["pluginId" .= targetPluginId value, "scope" .= targetPluginScope value]

data SetPluginEnabledParams = SetPluginEnabledParams
  { enabledPluginTarget :: !PluginTargetParams,
    pluginEnabled :: !Bool
  }
  deriving stock (Eq)

instance Show SetPluginEnabledParams where show _ = "SetPluginEnabledParams <redacted>"

instance FromJSON SetPluginEnabledParams where
  parseJSON = withObject "SetPluginEnabledParams" $ \fields -> SetPluginEnabledParams <$> parseJSON (Object (KeyMap.delete "enabled" fields)) <*> fields .: "enabled"

instance ToJSON SetPluginEnabledParams where
  toJSON value = Object (KeyMap.insert "enabled" (Bool (pluginEnabled value)) (pluginTargetObject (enabledPluginTarget value)))

data UpdatePluginParams = UpdatePluginParams
  { updatePluginId :: !(Maybe Text),
    updatePluginScope :: !(Maybe Text),
    updatePluginAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdatePluginParams where show _ = "UpdatePluginParams <redacted>"

instance FromJSON UpdatePluginParams where
  parseJSON = withObject "UpdatePluginParams" $ \fields -> UpdatePluginParams <$> fields .:! "pluginId" <*> fields .:! "scope" <*> pure (additionalFields ["pluginId", "scope"] fields)

instance ToJSON UpdatePluginParams where
  toJSON value = objectWithAdditionalFields ["pluginId", "scope"] (updatePluginAdditionalFields value) (optionalField "pluginId" (updatePluginId value) <> optionalField "scope" (updatePluginScope value))

defaultUpdatePluginParams :: UpdatePluginParams
defaultUpdatePluginParams = UpdatePluginParams Nothing Nothing mempty

data AddMarketplaceParams = AddMarketplaceParams
  { addedMarketplaceSource :: !MarketplaceSource,
    addMarketplaceAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AddMarketplaceParams where show _ = "AddMarketplaceParams <redacted>"

instance FromJSON AddMarketplaceParams where
  parseJSON = withObject "AddMarketplaceParams" $ \fields -> AddMarketplaceParams <$> fields .: "source" <*> pure (additionalFields ["source"] fields)

instance ToJSON AddMarketplaceParams where
  toJSON value = objectWithAdditionalFields ["source"] (addMarketplaceAdditionalFields value) ["source" .= addedMarketplaceSource value]

data MarketplaceNameParams = MarketplaceNameParams
  { marketplaceName :: !Text,
    marketplaceNameAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show MarketplaceNameParams where show _ = "MarketplaceNameParams <redacted>"

instance FromJSON MarketplaceNameParams where
  parseJSON = withObject "MarketplaceNameParams" $ \fields -> MarketplaceNameParams <$> fields .: "name" <*> pure (additionalFields ["name"] fields)

instance ToJSON MarketplaceNameParams where
  toJSON value = objectWithAdditionalFields ["name"] (marketplaceNameAdditionalFields value) ["name" .= marketplaceName value]

data UpdateMarketplaceParams = UpdateMarketplaceParams
  { marketplaceUpdateName :: !(Maybe Text),
    marketplaceUpdateAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show UpdateMarketplaceParams where show _ = "UpdateMarketplaceParams <redacted>"

instance FromJSON UpdateMarketplaceParams where
  parseJSON = withObject "UpdateMarketplaceParams" $ \fields -> UpdateMarketplaceParams <$> fields .:! "name" <*> pure (additionalFields ["name"] fields)

instance ToJSON UpdateMarketplaceParams where
  toJSON value = objectWithAdditionalFields ["name"] (marketplaceUpdateAdditionalFields value) (optionalField "name" (marketplaceUpdateName value))

data InstallPluginResult = InstallPluginResult
  { pluginInstallSuccess :: !Bool,
    pluginInstalledId :: !(Maybe Text),
    pluginInstallError :: !(Maybe Text),
    pluginInstallResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show InstallPluginResult where show _ = "InstallPluginResult <redacted>"

instance FromJSON InstallPluginResult where
  parseJSON = withObject "InstallPluginResult" $ \fields -> InstallPluginResult <$> fields .: "success" <*> fields .:! "pluginId" <*> fields .:! "error" <*> pure (additionalFields ["success", "pluginId", "error"] fields)

instance ToJSON InstallPluginResult where
  toJSON value = objectWithAdditionalFields ["success", "pluginId", "error"] (pluginInstallResultAdditionalFields value) (["success" .= pluginInstallSuccess value] <> optionalField "pluginId" (pluginInstalledId value) <> optionalField "error" (pluginInstallError value))

data AddMarketplaceResult = AddMarketplaceResult
  { marketplaceAddSuccess :: !Bool,
    marketplaceAddedName :: !(Maybe Text),
    marketplaceAddError :: !(Maybe Text),
    marketplaceAddResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AddMarketplaceResult where show _ = "AddMarketplaceResult <redacted>"

instance FromJSON AddMarketplaceResult where
  parseJSON = withObject "AddMarketplaceResult" $ \fields -> AddMarketplaceResult <$> fields .: "success" <*> fields .:! "name" <*> fields .:! "error" <*> pure (additionalFields ["success", "name", "error"] fields)

instance ToJSON AddMarketplaceResult where
  toJSON value = objectWithAdditionalFields ["success", "name", "error"] (marketplaceAddResultAdditionalFields value) (["success" .= marketplaceAddSuccess value] <> optionalField "name" (marketplaceAddedName value) <> optionalField "error" (marketplaceAddError value))

data PluginUpdateEntry = PluginUpdateEntry
  { updatedPluginId :: !Text,
    pluginUpdateSuccess :: !Bool,
    pluginUpdateError :: !(Maybe Text),
    pluginUpdateEntryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PluginUpdateEntry where show _ = "PluginUpdateEntry <redacted>"

instance FromJSON PluginUpdateEntry where
  parseJSON = withObject "PluginUpdateEntry" $ \fields -> PluginUpdateEntry <$> fields .: "pluginId" <*> fields .: "success" <*> fields .:! "error" <*> pure (additionalFields ["pluginId", "success", "error"] fields)

instance ToJSON PluginUpdateEntry where
  toJSON value = objectWithAdditionalFields ["pluginId", "success", "error"] (pluginUpdateEntryAdditionalFields value) (["pluginId" .= updatedPluginId value, "success" .= pluginUpdateSuccess value] <> optionalField "error" (pluginUpdateError value))

data MarketplaceUpdateEntry = MarketplaceUpdateEntry
  { updatedMarketplaceName :: !Text,
    marketplaceUpdateSuccess :: !Bool,
    marketplaceUpdateError :: !(Maybe Text),
    marketplaceUpdateEntryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show MarketplaceUpdateEntry where show _ = "MarketplaceUpdateEntry <redacted>"

instance FromJSON MarketplaceUpdateEntry where
  parseJSON = withObject "MarketplaceUpdateEntry" $ \fields -> MarketplaceUpdateEntry <$> fields .: "name" <*> fields .: "success" <*> fields .:! "error" <*> pure (additionalFields ["name", "success", "error"] fields)

instance ToJSON MarketplaceUpdateEntry where
  toJSON value = objectWithAdditionalFields ["name", "success", "error"] (marketplaceUpdateEntryAdditionalFields value) (["name" .= updatedMarketplaceName value, "success" .= marketplaceUpdateSuccess value] <> optionalField "error" (marketplaceUpdateError value))

data UpdateBatch a = UpdateBatch
  { updateEntries :: ![a],
    updateBatchAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show (UpdateBatch a) where show _ = "UpdateBatch <redacted>"

instance (FromJSON a) => FromJSON (UpdateBatch a) where
  parseJSON = withObject "UpdateBatch" $ \fields -> UpdateBatch <$> fields .: "results" <*> pure (additionalFields ["results"] fields)

instance (ToJSON a) => ToJSON (UpdateBatch a) where
  toJSON value = objectWithAdditionalFields ["results"] (updateBatchAdditionalFields value) ["results" .= updateEntries value]

type UpdatePluginResult = UpdateBatch PluginUpdateEntry

type UpdateMarketplaceResult = UpdateBatch MarketplaceUpdateEntry

installedPluginKeys, marketplaceKeys :: [Key]
installedPluginKeys = ["id", "scope", "version", "installPath", "installedAt", "lastUpdated", "source", "active", "managed", "reason"]
marketplaceKeys = ["name", "displayName", "source", "pluginCount", "autoUpdate", "provisionedBy", "removable"]
