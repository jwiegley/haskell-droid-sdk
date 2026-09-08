{-# LANGUAGE OverloadedStrings #-}

-- | Tool, command, skill and identity metadata for Factory protocol 1.205.0.
-- These codecs neither discover files nor execute commands or change settings.
module Factory.Droid.Schema.Discovery
  ( ToolCatalogCategory (..),
    ExecToolInfo (..),
    ListToolsResult (..),
    CustomCommandInfo (..),
    ListCommandsResult (..),
    SkillResourceType (..),
    SkillResource (..),
    SkillDisabledSource (..),
    SkillDisabledBy (..),
    SkillInfo (..),
    ListSkillsResult (..),
    EditableSkillSettingsLevel (..),
    SetSkillDisabledParams (..),
    GetUserInfoResult (..),
    GitRepoInfo (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Enums (SettingsLevel, SkillLocation)

-- | Native tool catalog categories, not permission grants.
data ToolCatalogCategory = CatalogRead | CatalogEdit | CatalogExecute | CatalogOther
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ToolCatalogCategory where
  parseJSON = withText "ToolCatalogCategory" $ \case
    "read" -> pure CatalogRead
    "edit" -> pure CatalogEdit
    "execute" -> pure CatalogExecute
    "other" -> pure CatalogOther
    _ -> fail "Unknown tool catalog category"

instance ToJSON ToolCatalogCategory where
  toJSON CatalogRead = String "read"
  toJSON CatalogEdit = String "edit"
  toJSON CatalogExecute = String "execute"
  toJSON CatalogOther = String "other"

-- | A native tool's distinct SDK/model identifiers and reported allow states.
data ExecToolInfo = ExecToolInfo
  { execToolId :: !Text,
    execToolLlmId :: !Text,
    execToolDisplayName :: !Text,
    execToolDescription :: !Text,
    execToolCategory :: !ToolCatalogCategory,
    execToolDefaultAllowed :: !Bool,
    execToolCurrentlyAllowed :: !Bool,
    execToolAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ExecToolInfo where
  parseJSON = withObject "ExecToolInfo" $ \fields ->
    ExecToolInfo <$> fields .: "id" <*> fields .: "llmId" <*> fields .: "displayName" <*> fields .: "description" <*> fields .: "category" <*> fields .: "defaultAllowed" <*> fields .: "currentlyAllowed" <*> pure (additionalFields toolKeys fields)

instance ToJSON ExecToolInfo where
  toJSON tool = objectWithAdditionalFields toolKeys (execToolAdditionalFields tool) ["id" .= execToolId tool, "llmId" .= execToolLlmId tool, "displayName" .= execToolDisplayName tool, "description" .= execToolDescription tool, "category" .= execToolCategory tool, "defaultAllowed" .= execToolDefaultAllowed tool, "currentlyAllowed" .= execToolCurrentlyAllowed tool]

-- | The ordered native-tool catalog, including an empty catalog.
data ListToolsResult = ListToolsResult
  { listedTools :: ![ExecToolInfo],
    listedToolsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ListToolsResult where
  parseJSON = withObject "ListToolsResult" $ \fields -> ListToolsResult <$> fields .: "tools" <*> pure (additionalFields ["tools"] fields)

instance ToJSON ListToolsResult where
  toJSON result = objectWithAdditionalFields ["tools"] (listedToolsAdditionalFields result) ["tools" .= listedTools result]

-- | Slash-command metadata. The executable flag is not an instruction to run it.
data CustomCommandInfo = CustomCommandInfo
  { customCommandName :: !Text,
    customCommandDescription :: !Text,
    customCommandArgumentHint :: !(Maybe Text),
    customCommandIsExecutable :: !(Maybe Bool),
    customCommandAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CustomCommandInfo where
  parseJSON = withObject "CustomCommandInfo" $ \fields ->
    CustomCommandInfo <$> fields .: "name" <*> fields .: "description" <*> fields .:! "argumentHint" <*> fields .:! "isExecutable" <*> pure (additionalFields commandKeys fields)

instance ToJSON CustomCommandInfo where
  toJSON command =
    objectWithAdditionalFields commandKeys (customCommandAdditionalFields command) $
      ["name" .= customCommandName command, "description" .= customCommandDescription command]
        <> optionalField "argumentHint" (customCommandArgumentHint command)
        <> optionalField "isExecutable" (customCommandIsExecutable command)

-- | The inline result body of ListCommandsResponseSchema, without its envelope.
data ListCommandsResult = ListCommandsResult
  { listedCommands :: ![CustomCommandInfo],
    listedCommandsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ListCommandsResult where
  parseJSON = withObject "ListCommandsResult" $ \fields -> ListCommandsResult <$> fields .: "commands" <*> pure (additionalFields ["commands"] fields)

instance ToJSON ListCommandsResult where
  toJSON result = objectWithAdditionalFields ["commands"] (listedCommandsAdditionalFields result) ["commands" .= listedCommands result]

-- | The reported resource classification; filename extensions are not inspected.
data SkillResourceType = ResourceReference | ResourceAsset
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SkillResourceType where
  parseJSON = withText "SkillResourceType" $ \case
    "reference" -> pure ResourceReference
    "asset" -> pure ResourceAsset
    _ -> fail "Unknown skill resource type"

instance ToJSON SkillResourceType where
  toJSON ResourceReference = String "reference"
  toJSON ResourceAsset = String "asset"

-- | A named resource path. Decoding does not establish that the path exists.
data SkillResource = SkillResource
  { skillResourceName :: !Text,
    skillResourcePath :: !Text,
    skillResourceType :: !SkillResourceType,
    skillResourceAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SkillResource where
  parseJSON = withObject "SkillResource" $ \fields ->
    SkillResource <$> fields .: "name" <*> fields .: "path" <*> fields .: "type" <*> pure (additionalFields resourceKeys fields)

instance ToJSON SkillResource where
  toJSON resource = objectWithAdditionalFields resourceKeys (skillResourceAdditionalFields resource) ["name" .= skillResourceName resource, "path" .= skillResourcePath resource, "type" .= skillResourceType resource]

-- | A reported disabling settings layer. All shared settings levels are valid;
-- folderPath is optional even for the folder level in this wire schema.
data SkillDisabledSource = SkillDisabledSource
  { disabledSourceLevel :: !SettingsLevel,
    disabledSourceFolderPath :: !(Maybe Text),
    disabledSourceAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SkillDisabledSource where
  parseJSON = withObject "SkillDisabledSource" $ \fields ->
    SkillDisabledSource <$> fields .: "level" <*> fields .:! "folderPath" <*> pure (additionalFields ["level", "folderPath"] fields)

instance ToJSON SkillDisabledSource where
  toJSON source = objectWithAdditionalFields ["level", "folderPath"] (disabledSourceAdditionalFields source) (["level" .= disabledSourceLevel source] <> optionalField "folderPath" (disabledSourceFolderPath source))

-- | Ledger-based or frontmatter-based disabling. Only ledger values declare
-- sources; a sources member on the other branch remains an extension.
data SkillDisabledBy
  = SkillDisabledByLedger
      { disabledSkillSources :: ![SkillDisabledSource],
        skillDisabledAdditionalFields :: !Object
      }
  | SkillDisabledByFrontmatter
      { skillDisabledAdditionalFields :: !Object
      }
  deriving stock (Eq, Show)

instance FromJSON SkillDisabledBy where
  parseJSON = withObject "SkillDisabledBy" $ \fields -> do
    kind <- fields .: "kind" :: Parser Text
    case kind of
      "ledger" -> SkillDisabledByLedger <$> fields .: "sources" <*> pure (additionalFields ["kind", "sources"] fields)
      "frontmatter" -> pure (SkillDisabledByFrontmatter (additionalFields ["kind"] fields))
      _ -> fail "Unknown skill disabling kind"

instance ToJSON SkillDisabledBy where
  toJSON (SkillDisabledByLedger sources extras) = objectWithAdditionalFields ["kind", "sources"] extras ["kind" .= String "ledger", "sources" .= sources]
  toJSON (SkillDisabledByFrontmatter extras) = objectWithAdditionalFields ["kind"] extras ["kind" .= String "frontmatter"]

-- | Skill metadata. Optional enabled/disabledBy fields are independent reports,
-- not a locally reconciled policy. Content and resources are not evaluated.
data SkillInfo = SkillInfo
  { skillName :: !Text,
    skillLocation :: !SkillLocation,
    skillFilePath :: !Text,
    skillDescription :: !(Maybe Text),
    skillEnabled :: !(Maybe Bool),
    skillUserInvocable :: !(Maybe Bool),
    skillVersion :: !(Maybe Text),
    skillContent :: !(Maybe Text),
    skillResources :: !(Maybe [SkillResource]),
    skillDisabledBy :: !(Maybe SkillDisabledBy),
    skillAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SkillInfo where
  parseJSON = withObject "SkillInfo" $ \fields ->
    SkillInfo
      <$> fields .: "name"
      <*> fields .: "location"
      <*> fields .: "filePath"
      <*> fields .:! "description"
      <*> fields .:! "enabled"
      <*> fields .:! "userInvocable"
      <*> fields .:! "version"
      <*> fields .:! "content"
      <*> fields .:! "resources"
      <*> fields .:! "disabledBy"
      <*> pure (additionalFields skillKeys fields)

instance ToJSON SkillInfo where
  toJSON skill =
    objectWithAdditionalFields skillKeys (skillAdditionalFields skill) $
      ["name" .= skillName skill, "location" .= skillLocation skill, "filePath" .= skillFilePath skill]
        <> optionalField "description" (skillDescription skill)
        <> optionalField "enabled" (skillEnabled skill)
        <> optionalField "userInvocable" (skillUserInvocable skill)
        <> optionalField "version" (skillVersion skill)
        <> optionalField "content" (skillContent skill)
        <> optionalField "resources" (skillResources skill)
        <> optionalField "disabledBy" (skillDisabledBy skill)

-- | Listed skills and an optional project-availability report.
data ListSkillsResult = ListSkillsResult
  { listedSkills :: ![SkillInfo],
    listedSkillsProjectAvailable :: !(Maybe Bool),
    listedSkillsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ListSkillsResult where
  parseJSON = withObject "ListSkillsResult" $ \fields ->
    ListSkillsResult <$> fields .: "skills" <*> fields .:! "projectAvailable" <*> pure (additionalFields ["skills", "projectAvailable"] fields)

instance ToJSON ListSkillsResult where
  toJSON result = objectWithAdditionalFields ["skills", "projectAvailable"] (listedSkillsAdditionalFields result) (["skills" .= listedSkills result] <> optionalField "projectAvailable" (listedSkillsProjectAvailable result))

-- | Only user/project layers can be addressed by the skill-disable operation.
data EditableSkillSettingsLevel = EditableSkillUser | EditableSkillProject
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON EditableSkillSettingsLevel where
  parseJSON = withText "EditableSkillSettingsLevel" $ \case
    "user" -> pure EditableSkillUser
    "project" -> pure EditableSkillProject
    _ -> fail "Unknown editable skill settings level"

instance ToJSON EditableSkillSettingsLevel where
  toJSON EditableSkillUser = String "user"
  toJSON EditableSkillProject = String "project"

-- | Parameters for changing skill enablement, not a settings mutation.
data SetSkillDisabledParams = SetSkillDisabledParams
  { changedSkillName :: !Text,
    changedSkillDisabled :: !Bool,
    changedSkillSettingsLevel :: !(Maybe EditableSkillSettingsLevel),
    changedSkillAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SetSkillDisabledParams where
  parseJSON = withObject "SetSkillDisabledParams" $ \fields ->
    SetSkillDisabledParams <$> fields .: "skillName" <*> fields .: "disabled" <*> fields .:! "settingsLevel" <*> pure (additionalFields changedSkillKeys fields)

instance ToJSON SetSkillDisabledParams where
  toJSON params = objectWithAdditionalFields changedSkillKeys (changedSkillAdditionalFields params) (["skillName" .= changedSkillName params, "disabled" .= changedSkillDisabled params] <> optionalField "settingsLevel" (changedSkillSettingsLevel params))

-- | Reported user/organization identifiers, without authentication side effects.
data GetUserInfoResult = GetUserInfoResult
  { reportedUserId :: !Text,
    reportedOrgId :: !Text,
    userInfoAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GetUserInfoResult where
  parseJSON = withObject "GetUserInfoResult" $ \fields ->
    GetUserInfoResult <$> fields .: "userId" <*> fields .: "orgId" <*> pure (additionalFields ["userId", "orgId"] fields)

instance ToJSON GetUserInfoResult where
  toJSON result = objectWithAdditionalFields ["userId", "orgId"] (userInfoAdditionalFields result) ["userId" .= reportedUserId result, "orgId" .= reportedOrgId result]

-- | Repository name and optional owner, without inspecting a Git repository.
data GitRepoInfo = GitRepoInfo
  { gitRepoName :: !Text,
    gitRepoOwner :: !(Maybe Text),
    gitRepoAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GitRepoInfo where
  parseJSON = withObject "GitRepoInfo" $ \fields ->
    GitRepoInfo <$> fields .: "repoName" <*> fields .:! "owner" <*> pure (additionalFields ["repoName", "owner"] fields)

instance ToJSON GitRepoInfo where
  toJSON repo = objectWithAdditionalFields ["repoName", "owner"] (gitRepoAdditionalFields repo) (["repoName" .= gitRepoName repo] <> optionalField "owner" (gitRepoOwner repo))

toolKeys, commandKeys, resourceKeys, skillKeys, changedSkillKeys :: [Key]
toolKeys = ["id", "llmId", "displayName", "description", "category", "defaultAllowed", "currentlyAllowed"]
commandKeys = ["name", "description", "argumentHint", "isExecutable"]
resourceKeys = ["name", "path", "type"]
skillKeys = ["name", "location", "filePath", "description", "enabled", "userInvocable", "version", "content", "resources", "disabledBy"]
changedSkillKeys = ["skillName", "disabled", "settingsLevel"]
