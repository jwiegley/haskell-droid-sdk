{-# LANGUAGE OverloadedStrings #-}

module DiscoverySpec (discoveryTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Discovery
import Factory.Droid.Schema.Enums (SettingsLevel (..), SkillLocation (..))
import SchemaTest (enumSchemaTest, nonNullableRecordTests, rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

discoveryTests :: Value -> TestTree
discoveryTests schema =
  testGroup
    "Discovery metadata"
    [ records "ExecToolInfoSchema" tool tool toolJSON toolJSON (\extras value -> value {execToolAdditionalFields = extras}),
      records "ListToolsResultSchema" tools tools toolsJSON toolsJSON (\extras value -> value {listedToolsAdditionalFields = extras}),
      records "CustomCommandInfoSchema" command (command {customCommandArgumentHint = Nothing, customCommandIsExecutable = Nothing}) commandJSON (KeyMap.fromList ["name" .= String "command", "description" .= String "description"]) (\extras value -> value {customCommandAdditionalFields = extras}),
      nonNullableRecordTests "ListCommandsResult" commandResultSchema commands commands commandsJSON commandsJSON (\extras value -> value {listedCommandsAdditionalFields = extras}),
      records "SkillResourceSchema" resource resource resourceJSON resourceJSON (\extras value -> value {skillResourceAdditionalFields = extras}),
      nonNullableRecordTests "SkillDisabledSource" (ledgerSchema >>= schemaAt ["properties", "sources", "items"]) source (source {disabledSourceFolderPath = Nothing}) sourceJSON (KeyMap.singleton "level" (String "folder")) (\extras value -> value {disabledSourceAdditionalFields = extras}),
      nonNullableRecordTests "ledger disabling" ledgerSchema ledger ledger ledgerJSON ledgerJSON (\extras value -> value {skillDisabledAdditionalFields = extras}),
      nonNullableRecordTests "frontmatter disabling" frontmatterSchema frontmatter frontmatter frontmatterJSON frontmatterJSON (\extras value -> value {skillDisabledAdditionalFields = extras}),
      records "SkillInfoSchema" skill minimalSkill skillJSON minimalSkillJSON (\extras value -> value {skillAdditionalFields = extras}),
      records "ListSkillsResultSchema" skills (skills {listedSkillsProjectAvailable = Nothing}) skillsJSON (KeyMap.delete "projectAvailable" skillsJSON) (\extras value -> value {listedSkillsAdditionalFields = extras}),
      records "SetSkillDisabledRequestParamsSchema" changed (changed {changedSkillSettingsLevel = Nothing}) changedJSON (KeyMap.delete "settingsLevel" changedJSON) (\extras value -> value {changedSkillAdditionalFields = extras}),
      records "GetUserInfoResultSchema" user user userJSON userJSON (\extras value -> value {userInfoAdditionalFields = extras}),
      records "GitRepoInfoSchema" repo (repo {gitRepoOwner = Nothing}) repoJSON (KeyMap.delete "owner" repoJSON) (\extras value -> value {gitRepoAdditionalFields = extras}),
      enumSchemaTest "tool catalog categories" (schemaAt ["definitions", "ExecToolInfoSchema", "properties", "category", "enum"] schema) (Proxy @ToolCatalogCategory),
      enumSchemaTest "skill resource types" (schemaAt ["definitions", "SkillResourceSchema", "properties", "type", "enum"] schema) (Proxy @SkillResourceType),
      enumSchemaTest "editable skill levels" (schemaAt ["definitions", "EditableSkillSettingsLevelSchema", "enum"] schema) (Proxy @EditableSkillSettingsLevel),
      testCase "disabling source levels are broader than editable levels" $ do
        forM_ [minBound .. maxBound] $ \level -> do
          let value = SkillDisabledSource level Nothing mempty
          fromJSON (object ["level" .= level]) @?= Success value
        forM_ [SettingsOrg, SettingsRuntime, SettingsFolder, SettingsDynamic, SettingsBuiltin] $ \level ->
          rejects (Proxy @SetSkillDisabledParams) (Object (KeyMap.insert "settingsLevel" (toJSON level) changedJSON)),
      testCase "frontmatter extensions do not adopt ledger constraints" $ do
        let value = SkillDisabledByFrontmatter (KeyMap.singleton "sources" Null)
            wire = object ["kind" .= String "frontmatter", "sources" .= Null]
        fromJSON wire @?= Success value
        toJSON value @?= wire
        rejects (Proxy @SkillDisabledBy) (object ["kind" .= String "ledger", "sources" .= Null])
        rejects (Proxy @SkillDisabledBy) (object ["kind" .= String "future"]),
      testCase "reported flags are independent without policy reconciliation" $ do
        let value = skill {skillEnabled = Just True}
            wire = Object (KeyMap.insert "enabled" (Bool True) skillJSON)
        fromJSON wire @?= Success value
        toJSON value @?= wire
        execToolDefaultAllowed tool @?= True
        execToolCurrentlyAllowed tool @?= False,
      testCase "catalog and source arrays may be empty" $ do
        fromJSON (object ["tools" .= ([] :: [Value])]) @?= Success (ListToolsResult [] mempty)
        fromJSON (object ["commands" .= ([] :: [Value])]) @?= Success (ListCommandsResult [] mempty)
        fromJSON (object ["skills" .= ([] :: [Value])]) @?= Success (ListSkillsResult [] Nothing mempty)
        fromJSON (object ["kind" .= String "ledger", "sources" .= ([] :: [Value])]) @?= Success (SkillDisabledByLedger [] mempty)
        fromJSON (Object (KeyMap.insert "resources" (Array mempty) skillJSON)) @?= Success (skill {skillResources = Just []}),
      testCase "catalog nesting validates objects and discriminators" $ do
        forM_ [Null, object [], String "entry"] $ \bad -> do
          rejects (Proxy @ListToolsResult) (object ["tools" .= [bad]])
          rejects (Proxy @ListCommandsResult) (object ["commands" .= [bad]])
          rejects (Proxy @ListSkillsResult) (object ["skills" .= [bad]])
          rejects (Proxy @SkillInfo) (Object (KeyMap.insert "resources" (toJSON [bad]) skillJSON))
          rejects (Proxy @SkillDisabledBy) (object ["kind" .= String "ledger", "sources" .= [bad]])
        rejects (Proxy @ListToolsResult) (object ["tools" .= [Object (KeyMap.insert "category" (String "future") toolJSON)]])
        rejects (Proxy @SkillInfo) (Object (KeyMap.insert "disabledBy" (object ["kind" .= String "future"]) skillJSON)),
      testCase "nested source extensions and duplicate entries survive" $ do
        let value = SkillDisabledByLedger [source {disabledSourceAdditionalFields = KeyMap.singleton "future" Null}, source] mempty
            wire = object ["kind" .= String "ledger", "sources" .= [Object (KeyMap.insert "future" Null sourceJSON), Object sourceJSON]]
        fromJSON wire @?= Success value
        toJSON value @?= wire,
      testCase "names, paths and identities are unnormalized strings" $ do
        fromJSON (object ["userId" .= String "", "orgId" .= String "not-a-uuid"]) @?= Success (GetUserInfoResult "" "not-a-uuid" mempty)
        fromJSON (object ["repoName" .= String "", "owner" .= String ""]) @?= Success (GitRepoInfo "" (Just "") mempty)
        let value = SkillResource "not-markdown.bin" "../not-read" ResourceReference mempty
        fromJSON (object ["name" .= String "not-markdown.bin", "path" .= String "../not-read", "type" .= String "reference"]) @?= Success value
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)
    disabling = schemaAt ["definitions", "SkillInfoSchema", "properties", "disabledBy", "anyOf"] schema
    ledgerSchema = disabling >>= schemaIndex 0
    frontmatterSchema = disabling >>= schemaIndex 1
    commandResultSchema = schemaAt ["definitions", "ListCommandsResponseSchema", "anyOf"] schema >>= schemaIndex 0 >>= schemaAt ["allOf"] >>= schemaIndex 1 >>= schemaAt ["properties", "result"]

tool :: ExecToolInfo
tool = ExecToolInfo "sdk-id" "model-id" "display" "description" CatalogExecute True False mempty

tools :: ListToolsResult
tools = ListToolsResult [tool] mempty

command :: CustomCommandInfo
command = CustomCommandInfo "command" "description" (Just "arguments") (Just False) mempty

commands :: ListCommandsResult
commands = ListCommandsResult [command] mempty

resource :: SkillResource
resource = SkillResource "resource" "not-read" ResourceAsset mempty

source :: SkillDisabledSource
source = SkillDisabledSource SettingsFolder (Just "folder") mempty

ledger, frontmatter :: SkillDisabledBy
ledger = SkillDisabledByLedger [source] mempty
frontmatter = SkillDisabledByFrontmatter mempty

minimalSkill, skill :: SkillInfo
minimalSkill = SkillInfo "skill" SkillAutomation "not-read" Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty
skill = minimalSkill {skillDescription = Just "description", skillEnabled = Just False, skillUserInvocable = Just False, skillVersion = Just "version", skillContent = Just "raw markdown", skillResources = Just [resource], skillDisabledBy = Just ledger}

skills :: ListSkillsResult
skills = ListSkillsResult [skill] (Just False) mempty

changed :: SetSkillDisabledParams
changed = SetSkillDisabledParams "skill" False (Just EditableSkillProject) mempty

user :: GetUserInfoResult
user = GetUserInfoResult "user-id" "org-id" mempty

repo :: GitRepoInfo
repo = GitRepoInfo "repo" (Just "owner") mempty

toolJSON, toolsJSON, commandJSON, commandsJSON, resourceJSON, sourceJSON, ledgerJSON, frontmatterJSON, minimalSkillJSON, skillJSON, skillsJSON, changedJSON, userJSON, repoJSON :: Object
toolJSON = KeyMap.fromList ["id" .= String "sdk-id", "llmId" .= String "model-id", "displayName" .= String "display", "description" .= String "description", "category" .= String "execute", "defaultAllowed" .= True, "currentlyAllowed" .= False]
toolsJSON = KeyMap.singleton "tools" (toJSON [Object toolJSON])
commandJSON = KeyMap.fromList ["name" .= String "command", "description" .= String "description", "argumentHint" .= String "arguments", "isExecutable" .= False]
commandsJSON = KeyMap.singleton "commands" (toJSON [Object commandJSON])
resourceJSON = KeyMap.fromList ["name" .= String "resource", "path" .= String "not-read", "type" .= String "asset"]
sourceJSON = KeyMap.fromList ["level" .= String "folder", "folderPath" .= String "folder"]
ledgerJSON = KeyMap.fromList ["kind" .= String "ledger", "sources" .= [Object sourceJSON]]
frontmatterJSON = KeyMap.singleton "kind" (String "frontmatter")
minimalSkillJSON = KeyMap.fromList ["name" .= String "skill", "location" .= String "automation", "filePath" .= String "not-read"]
skillJSON = KeyMap.fromList ["name" .= String "skill", "location" .= String "automation", "filePath" .= String "not-read", "description" .= String "description", "enabled" .= False, "userInvocable" .= False, "version" .= String "version", "content" .= String "raw markdown", "resources" .= [Object resourceJSON], "disabledBy" .= Object ledgerJSON]
skillsJSON = KeyMap.fromList ["skills" .= [Object skillJSON], "projectAvailable" .= False]
changedJSON = KeyMap.fromList ["skillName" .= String "skill", "disabled" .= False, "settingsLevel" .= String "project"]
userJSON = KeyMap.fromList ["userId" .= String "user-id", "orgId" .= String "org-id"]
repoJSON = KeyMap.fromList ["repoName" .= String "repo", "owner" .= String "owner"]
