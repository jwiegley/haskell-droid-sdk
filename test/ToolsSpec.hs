{-# LANGUAGE OverloadedStrings #-}

module ToolsSpec (toolTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON,
    Object,
    Result (..),
    ToJSON,
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific)
import Factory.Droid.Schema.Tools
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, chooseInt, forAll, testProperty, (===))

toolTests :: Value -> TestTree
toolTests schema =
  testGroup
    "Tool schemas"
    [ records "ApplyPatchToolInputSchema" patchInput patchInput patchInputJSON patchInputJSON (\extras input -> input {patchInputAdditionalFields = extras}),
      records "CreateToolInputSchema" createInput createInput createJSON createJSON (\extras input -> input {createAdditionalFields = extras}),
      records "EditToolInputSchema" editInput (editInput {editChangeAll = Nothing}) editJSON (remove ["change_all"] editJSON) (\extras input -> input {editAdditionalFields = extras}),
      records "ExecuteToolInputSchema" executeInput minimalExecute executeJSON (KeyMap.singleton "command" (String "echo fixture")) (\extras input -> input {executeAdditionalFields = extras}),
      records "ReadToolInputSchema" readInput (ReadToolInput "read-path" Nothing Nothing Nothing mempty) readJSON (KeyMap.singleton "file_path" (String "read-path")) (\extras input -> input {readAdditionalFields = extras}),
      records "GlobToolInputSchema" globInput (GlobToolInput Nothing Nothing Nothing mempty) globJSON mempty (\extras input -> input {globAdditionalFields = extras}),
      records "GrepToolInputSchema" grepInput minimalGrep grepJSON (KeyMap.singleton "pattern" (String "pattern")) (\extras input -> input {grepAdditionalFields = extras}),
      records "LSToolInputSchema" listInput (LSToolInput Nothing Nothing mempty) listJSON mempty (\extras input -> input {listAdditionalFields = extras}),
      records "WebSearchToolInputSchema" searchInput (WebSearchToolInput "query" Nothing Nothing Nothing Nothing mempty) searchJSON (KeyMap.singleton "query" (String "query")) (\extras input -> input {searchAdditionalFields = extras}),
      records "FetchUrlToolInputSchema" fetchInput fetchInput fetchJSON fetchJSON (\extras input -> input {fetchAdditionalFields = extras}),
      records "TaskToolInputSchema" taskInput taskInput taskJSON taskJSON (\extras input -> input {taskAdditionalFields = extras}),
      records "TodoWriteToolInputSchema" todoInput todoInput todoJSON todoJSON (\extras input -> input {todosAdditionalFields = extras}),
      records "ExitSpecModeToolInputSchema" exitInput (exitInput {exitSpecTitle = Nothing}) exitJSON (remove ["title"] exitJSON) (\extras input -> input {exitSpecAdditionalFields = extras}),
      records "SkillToolInputSchema" skillInput skillInput skillJSON skillJSON (\extras input -> input {skillAdditionalFields = extras}),
      records "ProposeMissionToolInputSchema" missionInput (missionInput {missionTitle = Nothing}) missionJSON (remove ["title"] missionJSON) (\extras input -> input {missionAdditionalFields = extras}),
      nonNullableRecordTests "DiffLineNumbers" (schemaAt ["definitions", "DiffLineSchema", "properties", "lineNumber"] schema) lineNumbers (DiffLineNumbers Nothing Nothing mempty) lineNumbersJSON mempty (\extras numbers -> numbers {diffLineNumberAdditionalFields = extras}),
      records "DiffLineSchema" diffLine (diffLine {diffLineNumbers = Nothing}) diffJSON (remove ["lineNumber"] diffJSON) (\extras line -> line {diffLineAdditionalFields = extras}),
      records "FileOperationResultSchema" fileResult emptyFileResult fileResultJSON mempty (\extras result -> result {fileResultAdditionalFields = extras}),
      records "ApplyPatchFileChangeSchema" patchChange minimalPatchChange patchChangeJSON (KeyMap.fromList ["file_path" .= String "patch-path", "display_operation" .= String "update"]) (\extras file -> file {patchFileAdditionalFields = extras}),
      records "ApplyPatchToolResultSchema" patchResult patchResult patchResultJSON patchResultJSON (\extras result -> result {patchResultAdditionalFields = extras}),
      enumTests "ExecuteToolInputSchema" "riskLevel" (Proxy @RiskLevel),
      enumTests "ReadToolInputSchema" "image_quality" (Proxy @ImageQuality),
      enumTests "GrepToolInputSchema" "output_mode" (Proxy @GrepOutputMode),
      enumTests "ApplyPatchFileChangeSchema" "display_operation" (Proxy @PatchOperation),
      enumTests "DiffLineSchema" "type" (Proxy @DiffLineType),
      testCase "patch success is the Boolean true, not a truthy value" $
        forM_ [Bool False, Number 1, String "true", Null] $ \value ->
          rejects (Proxy @ApplyPatchToolResult) (Object (KeyMap.insert "success" value patchResultJSON)),
      testCase "patch results require a nonempty typed file array" $ do
        rejects (Proxy @ApplyPatchToolResult) (object ["success" .= True, "files" .= ([] :: [Value])])
        rejects (Proxy @ApplyPatchToolResult) (object ["success" .= True, "files" .= [object []]])
        rejects (Proxy @ApplyPatchToolResult) (object ["success" .= True, "files" .= [Null]])
        let value = ApplyPatchToolResult (minimalPatchChange :| [minimalPatchChange {patchOperation = PatchDelete}]) mempty
        eitherDecode (encode value) @?= Right value,
      testCase "patch operations do not invent conditional content requirements" $
        forM_ [PatchCreate, PatchUpdate, PatchDelete] $ \operation -> do
          let value = minimalPatchChange {patchOperation = operation}
          fromJSON (toJSON value) @?= Success value,
      testCase "file-path spellings remain independent" $ do
        let fields = KeyMap.fromList ["file_path" .= String "snake", "filePath" .= String "camel"]
            value = emptyFileResult {fileResultSnakePath = Just "snake", fileResultCamelPath = Just "camel"}
        fromJSON (Object fields) @?= Success value
        toJSON value @?= Object fields,
      testCase "all-optional tool options and results accept empty objects" $ do
        fromJSON (object []) @?= Success (GlobToolInput Nothing Nothing Nothing mempty)
        fromJSON (object []) @?= Success (LSToolInput Nothing Nothing mempty)
        fromJSON (object []) @?= Success emptyFileResult
        fromJSON (object []) @?= Success (DiffLineNumbers Nothing Nothing mempty),
      testCase "empty arrays and explicit false are not omitted" $ do
        let glob = GlobToolInput (Just []) Nothing (Just []) mempty
            grep = minimalGrep {grepCaseInsensitive = Just False, grepLineNumbers = Just False}
            execution = minimalExecute {executeFireAndForget = Just False}
        toJSON glob @?= object ["patterns" .= ([] :: [Value]), "excludePatterns" .= ([] :: [Value])]
        toJSON grep @?= object ["pattern" .= String "pattern", "case_insensitive" .= False, "line_numbers" .= False]
        toJSON execution @?= object ["command" .= String "echo fixture", "fireAndForget" .= False]
        fromJSON (toJSON glob) @?= Success glob
        fromJSON (toJSON grep) @?= Success grep
        fromJSON (toJSON execution) @?= Success execution,
      testCase "string arrays reject non-string elements" $ do
        forM_ [Null, Bool True, Number 0, object []] $ \item -> do
          rejects (Proxy @GlobToolInput) (Object (KeyMap.insert "patterns" (toJSON [item]) globJSON))
          rejects (Proxy @GlobToolInput) (Object (KeyMap.insert "excludePatterns" (toJSON [item]) globJSON))
          rejects (Proxy @LSToolInput) (Object (KeyMap.insert "ignorePatterns" (toJSON [item]) listJSON))
          rejects (Proxy @WebSearchToolInput) (Object (KeyMap.insert "includeDomains" (toJSON [item]) searchJSON))
          rejects (Proxy @WebSearchToolInput) (Object (KeyMap.insert "excludeDomains" (toJSON [item]) searchJSON)),
      testCase "nested diff coordinates and lines are validated" $ do
        forM_ [Null, String "1", Bool True] $ \value ->
          rejects (Proxy @DiffLine) (Object (KeyMap.insert "lineNumber" (object ["old" .= value]) diffJSON))
        rejects (Proxy @FileOperationResult) (Object (KeyMap.insert "diffLines" (toJSON [object []]) fileResultJSON))
        rejects (Proxy @ApplyPatchFileChange) (Object (KeyMap.insert "display_operation" (String "future") patchChangeJSON)),
      testCase "string-only wire constraints do not add runtime policy" $ do
        fromJSON (object ["url" .= String "not a URI"]) @?= Success (FetchUrlToolInput "not a URI" mempty)
        fromJSON (object ["file_path" .= String "", "content" .= String ""]) @?= Success (CreateToolInput "" "" mempty)
        fromJSON (object ["pattern" .= String "("]) @?= Success (minimalGrep {grepPattern = "("})
        fromJSON (object ["subagent_type" .= String "fixture", "description" .= String "short", "prompt" .= String ""]) @?= Success (TaskToolInput "fixture" "short" "" mempty)
        rejects (Proxy @TodoWriteToolInput) (object ["todos" .= ([] :: [Value])]),
      testProperty "numeric options preserve arbitrary finite JSON numbers" $
        forAll arbitrary $ \coefficient ->
          forAll (chooseInt (-12, 12)) $ \decimalExponent ->
            let value = minimalExecute {executeTimeout = Just (scientific coefficient decimalExponent)}
             in eitherDecode (encode value) === Right value
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)
    enumTests :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => Key -> Key -> Proxy a -> TestTree
    enumTests name field _ = testCase (show name <> "/" <> show field) $ do
      literals <- either assertFailure pure (schemaAt ["definitions", name, "properties", field, "enum"] schema)
      let values = [minBound .. maxBound] :: [a]
      toJSON values @?= literals
      forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
      forM_ [Null, Bool True, Number 1, String "future", Object mempty, Array mempty] $ rejects (Proxy @a)

patchInput :: ApplyPatchToolInput
patchInput = ApplyPatchToolInput "patch-path" "patch text" mempty

createInput :: CreateToolInput
createInput = CreateToolInput "create-path" "new content" mempty

editInput :: EditToolInput
editInput = EditToolInput "edit-path" "before" "after" (Just True) mempty

executeInput, minimalExecute :: ExecuteToolInput
executeInput = ExecuteToolInput "echo fixture" (Just "summary") (Just 1.125) (Just RiskHigh) (Just "reason") (Just True) mempty
minimalExecute = ExecuteToolInput "echo fixture" Nothing Nothing Nothing Nothing Nothing mempty

readInput :: ReadToolInput
readInput = ReadToolInput "read-path" (Just (-0.25)) (Just 2.5) (Just HighImageQuality) mempty

globInput :: GlobToolInput
globInput = GlobToolInput (Just ["*.hs"]) (Just "folder") (Just ["dist/**"]) mempty

grepInput, minimalGrep :: GrepToolInput
grepInput = GrepToolInput "pattern" (Just "grep-path") (Just "*.txt") (Just True) (Just GrepContent) (Just 1.25) (Just 2.5) (Just 3.75) (Just False) (Just 4.125) mempty
minimalGrep = GrepToolInput "pattern" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

listInput :: LSToolInput
listInput = LSToolInput (Just "directory") (Just ["ignored"]) mempty

searchInput :: WebSearchToolInput
searchInput = WebSearchToolInput "query" (Just 3.75) (Just ["include.example"]) (Just ["exclude.example"]) (Just "category") mempty

fetchInput :: FetchUrlToolInput
fetchInput = FetchUrlToolInput "https://example.invalid/fixture" mempty

taskInput :: TaskToolInput
taskInput = TaskToolInput "fixture" "task description" "prompt text" mempty

todoInput :: TodoWriteToolInput
todoInput = TodoWriteToolInput "todo text" mempty

exitInput :: ExitSpecModeToolInput
exitInput = ExitSpecModeToolInput "plan text" (Just "plan title") mempty

skillInput :: SkillToolInput
skillInput = SkillToolInput "fixture skill" mempty

missionInput :: ProposeMissionToolInput
missionInput = ProposeMissionToolInput "proposal text" (Just "mission title") mempty

lineNumbers :: DiffLineNumbers
lineNumbers = DiffLineNumbers (Just 12.125) (Just 13.5) mempty

diffLine :: DiffLine
diffLine = DiffLine DiffAdded "line content" (Just lineNumbers) mempty

fileResult, emptyFileResult :: FileOperationResult
fileResult = FileOperationResult (Just False) (Just "diff text") (Just [diffLine]) (Just "file content") (Just "result message") (Just "snake-path") (Just "camel-path") mempty
emptyFileResult = FileOperationResult Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

patchChange, minimalPatchChange :: ApplyPatchFileChange
patchChange = ApplyPatchFileChange "patch-path" PatchUpdate (Just "patched content") (Just "patch diff") (Just "file error") (Just "destination") (Just "reminder") mempty
minimalPatchChange = ApplyPatchFileChange "patch-path" PatchUpdate Nothing Nothing Nothing Nothing Nothing mempty

patchResult :: ApplyPatchToolResult
patchResult = ApplyPatchToolResult (patchChange :| []) mempty

patchInputJSON, createJSON, editJSON, executeJSON, readJSON, globJSON, grepJSON, listJSON, searchJSON, fetchJSON, taskJSON, todoJSON, exitJSON, skillJSON, missionJSON, lineNumbersJSON, diffJSON, fileResultJSON, patchChangeJSON, patchResultJSON :: Object
patchInputJSON = KeyMap.fromList ["file_path" .= String "patch-path", "patch" .= String "patch text"]
createJSON = KeyMap.fromList ["file_path" .= String "create-path", "content" .= String "new content"]
editJSON = KeyMap.fromList ["file_path" .= String "edit-path", "old_str" .= String "before", "new_str" .= String "after", "change_all" .= True]
executeJSON = KeyMap.fromList ["command" .= String "echo fixture", "summary" .= String "summary", "timeout" .= Number 1.125, "riskLevel" .= String "high", "riskLevelReason" .= String "reason", "fireAndForget" .= True]
readJSON = KeyMap.fromList ["file_path" .= String "read-path", "offset" .= Number (-0.25), "limit" .= Number 2.5, "image_quality" .= String "high"]
globJSON = KeyMap.fromList ["patterns" .= [String "*.hs"], "folder" .= String "folder", "excludePatterns" .= [String "dist/**"]]
grepJSON = KeyMap.fromList ["pattern" .= String "pattern", "path" .= String "grep-path", "glob_pattern" .= String "*.txt", "case_insensitive" .= True, "output_mode" .= String "content", "context" .= Number 1.25, "context_before" .= Number 2.5, "context_after" .= Number 3.75, "line_numbers" .= False, "head_limit" .= Number 4.125]
listJSON = KeyMap.fromList ["directory_path" .= String "directory", "ignorePatterns" .= [String "ignored"]]
searchJSON = KeyMap.fromList ["query" .= String "query", "numResults" .= Number 3.75, "includeDomains" .= [String "include.example"], "excludeDomains" .= [String "exclude.example"], "category" .= String "category"]
fetchJSON = KeyMap.singleton "url" (String "https://example.invalid/fixture")
taskJSON = KeyMap.fromList ["subagent_type" .= String "fixture", "description" .= String "task description", "prompt" .= String "prompt text"]
todoJSON = KeyMap.singleton "todos" (String "todo text")
exitJSON = KeyMap.fromList ["plan" .= String "plan text", "title" .= String "plan title"]
skillJSON = KeyMap.singleton "skill" (String "fixture skill")
missionJSON = KeyMap.fromList ["proposal" .= String "proposal text", "title" .= String "mission title"]
lineNumbersJSON = KeyMap.fromList ["old" .= Number 12.125, "new" .= Number 13.5]
diffJSON = KeyMap.fromList ["type" .= String "added", "content" .= String "line content", "lineNumber" .= lineNumbersJSON]
fileResultJSON = KeyMap.fromList ["success" .= False, "diff" .= String "diff text", "diffLines" .= [Object diffJSON], "content" .= String "file content", "message" .= String "result message", "file_path" .= String "snake-path", "filePath" .= String "camel-path"]
patchChangeJSON = KeyMap.fromList ["file_path" .= String "patch-path", "display_operation" .= String "update", "content" .= String "patched content", "diff" .= String "patch diff", "error" .= String "file error", "moved_to" .= String "destination", "systemReminder" .= String "reminder"]
patchResultJSON = KeyMap.fromList ["success" .= True, "files" .= [Object patchChangeJSON]]

remove :: [Key] -> Object -> Object
remove keys fields = foldr KeyMap.delete fields keys
