{-# LANGUAGE OverloadedStrings #-}

module InteractionSpec (interactionTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.List (find, sort)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Schema.Content (ToolUseBlock (..))
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome (..))
import Factory.Droid.Schema.Tools (PatchOperation (..))
import SchemaTest (enumSchemaTest, nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

interactionTests :: Value -> TestTree
interactionTests schema =
  testGroup
    "Interaction bodies"
    [ records "AskUserQuestionSchema" question (question {questionMultiSelect = Nothing}) questionJSON (KeyMap.delete "multiSelect" questionJSON) (\extras value -> value {questionAdditionalFields = extras}),
      records "AskUserCollectedAnswerSchema" answer answer answerJSON answerJSON (\extras value -> value {answerAdditionalFields = extras}),
      records "AskUserRequestParamsSchema" askParams askParams askParamsJSON askParamsJSON (\extras value -> value {askUserParamsAdditionalFields = extras}),
      records "AskUserResultSchema" askResult (askResult {askUserCancelled = Nothing}) askResultJSON (KeyMap.delete "cancelled" askResultJSON) (\extras value -> value {askUserResultAdditionalFields = extras}),
      records "ToolConfirmationListItemSchema" option option optionJSON optionJSON (\extras value -> value {confirmationOptionAdditionalFields = extras}),
      records "ToolConfirmationInfoSchema" info info infoJSON infoJSON (\extras value -> value {confirmationInfoAdditionalFields = extras}),
      records "RequestPermissionRequestParamsSchema" permissionParams (permissionParams {permissionAssociatedSessionIds = Nothing}) permissionParamsJSON (KeyMap.delete "associatedSessionIds" permissionParamsJSON) (\extras value -> value {permissionParamsAdditionalFields = extras}),
      nonNullableRecordTests "ParsedQuestionnaire" (detailSchema schema "ask_user" >>= schemaAt ["properties", "parsed"]) parsed parsed parsedJSON parsedJSON (\extras value -> value {parsedQuestionnaireAdditionalFields = extras}),
      nonNullableRecordTests "QuestionnaireParseError" (detailSchema schema "ask_user" >>= schemaAt ["properties", "parseError"]) parseError (parseError {questionnaireErrorLine = Nothing}) parseErrorJSON (KeyMap.delete "line" parseErrorJSON) (\extras value -> value {questionnaireErrorAdditionalFields = extras}),
      nonNullableRecordTests "ConfirmationPatchFile" (detailSchema schema "apply_patch" >>= schemaAt ["properties", "files", "items"]) patchFile (ConfirmationPatchFile "path" "name" PatchUpdate Nothing Nothing Nothing mempty) patchFileJSON (KeyMap.fromList ["filePath" .= String "path", "fileName" .= String "name", "operation" .= String "update"]) (\extras value -> value {confirmationPatchAdditionalFields = extras}),
      testGroup "Detail branches" (map checkDetails detailFixtures),
      enumSchemaTest "confirmation types" (schemaAt ["definitions", "ToolConfirmationInfoSchema", "properties", "confirmationType", "enum"] schema) (Proxy @ToolConfirmationType),
      enumSchemaTest "sandbox operations" (sandboxEnum "operationType") (Proxy @SandboxOperationType),
      enumSchemaTest "sandbox violations" (sandboxEnum "violationType") (Proxy @SandboxViolationType),
      enumSchemaTest "sandbox reasons" (sandboxEnum "violationReason") (Proxy @SandboxViolationReason),
      enumSchemaTest "patch operations" (detailSchema schema "apply_patch" >>= schemaAt ["properties", "files", "items", "properties", "operation", "enum"]) (Proxy @PatchOperation),
      testCase "fixtures cover every declared detail alternative" $ do
        definition <- either assertFailure pure (schemaAt ["definitions", "ToolConfirmationDetailsSchema", "anyOf"] schema)
        case definition of
          Array alternatives -> do
            tags <- either assertFailure pure (traverse (schemaAt ["properties", "type", "const"]) (toList alternatives))
            sort tags @?= sort [String tag | (tag, _, _, _, _) <- detailFixtures]
          _ -> assertFailure "Expected a detail union",
      testCase "inline questions reuse the named schema exactly" $ do
        inline <- either assertFailure pure (detailSchema schema "ask_user" >>= schemaAt ["properties", "parsed", "properties", "questions", "items"])
        named <- either assertFailure pure (schemaAt ["definitions", "AskUserQuestionSchema"] schema)
        inline @?= named,
      testCase "detail extensions reserve only the selected branch fields" $ do
        let body = ToolConfirmationDetails (ConfirmationCreate "path" "name" "") (KeyMap.singleton "newContent" Null)
            wire = object ["type" .= String "create", "filePath" .= String "path", "fileName" .= String "name", "content" .= String "", "newContent" .= Null]
        toJSON body @?= wire
        fromJSON wire @?= Success body
        rejects (Proxy @ToolConfirmationDetails) (object ["type" .= String "edit", "filePath" .= String "path", "fileName" .= String "name", "newContent" .= Null]),
      testCase "unknown discriminators and malformed tool-use bodies are rejected" $ do
        rejects (Proxy @ToolConfirmationDetails) (object ["type" .= String "future"])
        rejects (Proxy @ToolConfirmationInfo) (Object (KeyMap.insert "confirmationType" (String "future") infoJSON))
        forM_ [object [], object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "name", "input" .= Bool False]] $ \bad ->
          rejects (Proxy @ToolConfirmationInfo) (Object (KeyMap.insert "toolUse" bad infoJSON)),
      testCase "outer and inner confirmation kinds are not silently reconciled" $ do
        let mismatched = info {confirmationInfoType = ConfirmationTypeCreate}
        fromJSON (Object (KeyMap.insert "confirmationType" (String "create") infoJSON)) @?= Success mismatched
        toJSON mismatched @?= Object (KeyMap.insert "confirmationType" (String "create") infoJSON),
      testCase "question and permission lists admit empty arrays without defaults" $ do
        let emptyQuestion = AskUserQuestion 0 "" "" [] Nothing mempty
            emptyAsk = AskUserParams "" [] mempty
            emptyResult = AskUserResult [] (Just False) mempty
            emptyPermissions = RequestPermissionParams [] [] (Just []) mempty
        fromJSON (object ["index" .= Number 0, "topic" .= String "", "question" .= String "", "options" .= ([] :: [Value])]) @?= Success emptyQuestion
        toJSON emptyAsk @?= object ["toolCallId" .= String "", "questions" .= ([] :: [Value])]
        toJSON emptyResult @?= object ["answers" .= ([] :: [Value]), "cancelled" .= False]
        fromJSON (object ["toolUses" .= ([] :: [Value]), "options" .= ([] :: [Value]), "associatedSessionIds" .= ([] :: [Value])]) @?= Success emptyPermissions,
      testCase "indices are exact numbers, not coerced integer indices" $ do
        forM_ [-2.5, 0, 1.25, 123456789012345678901234567890] $ \index -> do
          fromJSON (Object (KeyMap.insert "index" (Number index) questionJSON)) @?= Success (question {questionIndex = index})
          fromJSON (Object (KeyMap.insert "index" (Number index) answerJSON)) @?= Success (answer {answerIndex = index}),
      testCase "cancellation retains collected text without interpreting it" $ do
        let cancelled = askResult {askUserCancelled = Just True}
        fromJSON (Object (KeyMap.insert "cancelled" (Bool True) askResultJSON)) @?= Success cancelled
        toJSON cancelled @?= Object (KeyMap.insert "cancelled" (Bool True) askResultJSON),
      testCase "all offered outcome literals reuse the notification enum" $ do
        forM_ [minBound .. maxBound] $ \value -> do
          let item = ToolConfirmationListItem "" value mempty
          eitherDecode (encode item) @?= Right item
        rejects (Proxy @ToolConfirmationListItem) (object ["label" .= String "", "value" .= String "proceed_unknown"]),
      testCase "nested questionnaire, answer and permission types are enforced" $ do
        forM_ [Null, object [], String "question"] $ \bad -> do
          rejects (Proxy @AskUserParams) (Object (KeyMap.insert "questions" (toJSON [bad]) askParamsJSON))
          rejects (Proxy @AskUserResult) (Object (KeyMap.insert "answers" (toJSON [bad]) askResultJSON))
          rejects (Proxy @ParsedQuestionnaire) (object ["questions" .= [bad]])
          forM_ ["toolUses", "options"] $ \key ->
            rejects (Proxy @RequestPermissionParams) (Object (KeyMap.insert key (toJSON [bad]) permissionParamsJSON))
        forM_ [Null, object [], Bool False] $ \bad ->
          rejects (Proxy @RequestPermissionParams) (Object (KeyMap.insert "associatedSessionIds" (toJSON [bad]) permissionParamsJSON))
        rejects (Proxy @AskUserQuestion) (Object (KeyMap.insert "options" (toJSON [Bool False]) questionJSON)),
      testCase "patch details validate nested file operations and contents" $ do
        let patch = KeyMap.fromList ["type" .= String "apply_patch", "filePath" .= String "path", "fileName" .= String "name", "patchContent" .= String "patch"]
        forM_ [Null, object [], Object (KeyMap.insert "operation" (String "move") patchFileJSON), Object (KeyMap.insert "oldContent" (Bool False) patchFileJSON)] $ \bad ->
          rejects (Proxy @ToolConfirmationDetails) (Object (KeyMap.insert "files" (toJSON [bad]) patch)),
      testCase "sandbox fields retain explicit organization denial and strict enums" $ do
        let body = ToolConfirmationDetails (ConfirmationSandboxViolation "Read" "target" SandboxRead ViolationFilesystemRead "policy" (Just ViolationDenyList) True) mempty
        eitherDecode (encode body) @?= Right body
        case toJSON body of
          Object fields -> forM_ ["operationType", "violationType", "violationReason"] $ \key ->
            rejects (Proxy @ToolConfirmationDetails) (Object (KeyMap.insert key (String "future") fields))
          _ -> assertFailure "Expected an object"
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)
    checkDetails (tag, full, minimal, fullJSON, minimalJSON) =
      nonNullableRecordTests
        (Key.fromText tag)
        (detailSchema schema tag)
        (ToolConfirmationDetails full mempty)
        (ToolConfirmationDetails minimal mempty)
        fullJSON
        minimalJSON
        (\extras value -> value {confirmationAdditionalFields = extras})
    sandboxEnum key = detailSchema schema "sandbox_violation" >>= schemaAt ["properties", key, "enum"]

detailSchema :: Value -> Text -> Either String Value
detailSchema schema tag = do
  alternatives <- schemaAt ["definitions", "ToolConfirmationDetailsSchema", "anyOf"] schema
  case alternatives of
    Array items -> maybe (Left "Detail branch missing from schema") Right (find (\item -> schemaAt ["properties", "type", "const"] item == Right (String tag)) items)
    _ -> Left "Expected detail union alternatives"

question :: AskUserQuestion
question = AskUserQuestion (-1.25) "topic" "question?" ["one", "two"] (Just False) mempty

answer :: AskUserCollectedAnswer
answer = AskUserCollectedAnswer (-1.25) "question?" "one, two; free text" mempty

askParams :: AskUserParams
askParams = AskUserParams "tool-id" [question] mempty

askResult :: AskUserResult
askResult = AskUserResult [answer] (Just False) mempty

parsed :: ParsedQuestionnaire
parsed = ParsedQuestionnaire [question] mempty

parseError :: QuestionnaireParseError
parseError = QuestionnaireParseError "fixture error" (Just (-0.5)) mempty

patchFile :: ConfirmationPatchFile
patchFile = ConfirmationPatchFile "path" "name" PatchUpdate (Just "destination") (Just "old") (Just "new") mempty

option :: ToolConfirmationListItem
option = ToolConfirmationListItem "Cancel" ConfirmCancel mempty

info :: ToolConfirmationInfo
info = ToolConfirmationInfo (ToolUseBlock "tool-id" mempty "Execute" Nothing Nothing Nothing mempty) ConfirmationTypeExec (ToolConfirmationDetails (ConfirmationExec "echo example" "echo" Nothing Nothing Nothing) mempty) mempty

permissionParams :: RequestPermissionParams
permissionParams = RequestPermissionParams [info] [option] (Just ["session-id", "session-id"]) mempty

questionJSON, answerJSON, askParamsJSON, askResultJSON, parsedJSON, parseErrorJSON, patchFileJSON, optionJSON, infoJSON, permissionParamsJSON :: Object
questionJSON = KeyMap.fromList ["index" .= Number (-1.25), "topic" .= String "topic", "question" .= String "question?", "options" .= [String "one", String "two"], "multiSelect" .= False]
answerJSON = KeyMap.fromList ["index" .= Number (-1.25), "question" .= String "question?", "answer" .= String "one, two; free text"]
askParamsJSON = KeyMap.fromList ["toolCallId" .= String "tool-id", "questions" .= [Object questionJSON]]
askResultJSON = KeyMap.fromList ["answers" .= [Object answerJSON], "cancelled" .= False]
parsedJSON = KeyMap.singleton "questions" (toJSON [Object questionJSON])
parseErrorJSON = KeyMap.fromList ["message" .= String "fixture error", "line" .= Number (-0.5)]
patchFileJSON = KeyMap.fromList ["filePath" .= String "path", "fileName" .= String "name", "operation" .= String "update", "moveTo" .= String "destination", "oldContent" .= String "old", "newContent" .= String "new"]
optionJSON = KeyMap.fromList ["label" .= String "Cancel", "value" .= String "cancel"]
infoJSON = KeyMap.fromList ["toolUse" .= object ["type" .= String "tool_use", "id" .= String "tool-id", "input" .= object [], "name" .= String "Execute"], "confirmationType" .= String "exec", "details" .= object ["type" .= String "exec", "fullCommand" .= String "echo example", "command" .= String "echo"]]
permissionParamsJSON = KeyMap.fromList ["toolUses" .= [Object infoJSON], "options" .= [Object optionJSON], "associatedSessionIds" .= [String "session-id", String "session-id"]]

detailFixtures :: [(Text, ConfirmationDetails, ConfirmationDetails, Object, Object)]
detailFixtures =
  [ ( "edit",
      ConfirmationEdit "path" "name" (Just "old") (Just "new"),
      ConfirmationEdit "path" "name" Nothing Nothing,
      KeyMap.fromList ["type" .= String "edit", "filePath" .= String "path", "fileName" .= String "name", "oldContent" .= String "old", "newContent" .= String "new"],
      KeyMap.fromList ["type" .= String "edit", "filePath" .= String "path", "fileName" .= String "name"]
    ),
    ( "exec",
      ConfirmationExec "echo example" "echo" (Just ["echo"]) (Just "free-form") (Just "reason"),
      ConfirmationExec "echo example" "echo" Nothing Nothing Nothing,
      KeyMap.fromList ["type" .= String "exec", "fullCommand" .= String "echo example", "command" .= String "echo", "extractedCommands" .= [String "echo"], "impactLevel" .= String "free-form", "riskLevelReason" .= String "reason"],
      KeyMap.fromList ["type" .= String "exec", "fullCommand" .= String "echo example", "command" .= String "echo"]
    ),
    ( "create",
      ConfirmationCreate "path" "name" "content",
      ConfirmationCreate "path" "name" "content",
      KeyMap.fromList ["type" .= String "create", "filePath" .= String "path", "fileName" .= String "name", "content" .= String "content"],
      KeyMap.fromList ["type" .= String "create", "filePath" .= String "path", "fileName" .= String "name", "content" .= String "content"]
    ),
    ( "ask_user",
      ConfirmationAskUser "raw" (Just parsed) (Just parseError),
      ConfirmationAskUser "raw" Nothing Nothing,
      KeyMap.fromList ["type" .= String "ask_user", "questionnaire" .= String "raw", "parsed" .= Object parsedJSON, "parseError" .= Object parseErrorJSON],
      KeyMap.fromList ["type" .= String "ask_user", "questionnaire" .= String "raw"]
    ),
    ( "exit_spec_mode",
      ConfirmationExitSpecMode "plan" (Just "title"),
      ConfirmationExitSpecMode "plan" Nothing,
      KeyMap.fromList ["type" .= String "exit_spec_mode", "plan" .= String "plan", "title" .= String "title"],
      KeyMap.fromList ["type" .= String "exit_spec_mode", "plan" .= String "plan"]
    ),
    ( "propose_mission",
      ConfirmationProposeMission "proposal" (Just "title"),
      ConfirmationProposeMission "proposal" Nothing,
      KeyMap.fromList ["type" .= String "propose_mission", "proposal" .= String "proposal", "title" .= String "title"],
      KeyMap.fromList ["type" .= String "propose_mission", "proposal" .= String "proposal"]
    ),
    ( "start_mission_run",
      ConfirmationStartMissionRun (-0.25) ["running", "running"],
      ConfirmationStartMissionRun (-0.25) ["running", "running"],
      KeyMap.fromList ["type" .= String "start_mission_run", "runningMissionCount" .= Number (-0.25), "runningMissionSessionIds" .= [String "running", String "running"]],
      KeyMap.fromList ["type" .= String "start_mission_run", "runningMissionCount" .= Number (-0.25), "runningMissionSessionIds" .= [String "running", String "running"]]
    ),
    ( "apply_patch",
      ConfirmationApplyPatch "path" "name" "patch" (Just "old") (Just "new") (Just [patchFile]),
      ConfirmationApplyPatch "path" "name" "patch" Nothing Nothing Nothing,
      KeyMap.fromList ["type" .= String "apply_patch", "filePath" .= String "path", "fileName" .= String "name", "patchContent" .= String "patch", "oldContent" .= String "old", "newContent" .= String "new", "files" .= [Object patchFileJSON]],
      KeyMap.fromList ["type" .= String "apply_patch", "filePath" .= String "path", "fileName" .= String "name", "patchContent" .= String "patch"]
    ),
    ( "mcp_tool",
      ConfirmationMcpTool "name" "free-form" (Just "server") (Just "actual"),
      ConfirmationMcpTool "name" "free-form" Nothing Nothing,
      KeyMap.fromList ["type" .= String "mcp_tool", "toolName" .= String "name", "impactLevel" .= String "free-form", "serverName" .= String "server", "actualToolName" .= String "actual"],
      KeyMap.fromList ["type" .= String "mcp_tool", "toolName" .= String "name", "impactLevel" .= String "free-form"]
    ),
    ( "sandbox_violation",
      ConfirmationSandboxViolation "Read" "target" SandboxRead ViolationFilesystemRead "reason" (Just ViolationNotAllowed) False,
      ConfirmationSandboxViolation "Read" "target" SandboxRead ViolationFilesystemRead "reason" Nothing False,
      KeyMap.fromList ["type" .= String "sandbox_violation", "violatingToolName" .= String "Read", "target" .= String "target", "operationType" .= String "read", "violationType" .= String "filesystem-read", "reason" .= String "reason", "violationReason" .= String "not-allowed", "isOrgDeny" .= False],
      KeyMap.fromList ["type" .= String "sandbox_violation", "violatingToolName" .= String "Read", "target" .= String "target", "operationType" .= String "read", "violationType" .= String "filesystem-read", "reason" .= String "reason", "isOrgDeny" .= False]
    ),
    ( "droid_shield_violation",
      ConfirmationDroidShieldViolation "command" "reason",
      ConfirmationDroidShieldViolation "command" "reason",
      KeyMap.fromList ["type" .= String "droid_shield_violation", "command" .= String "command", "reason" .= String "reason"],
      KeyMap.fromList ["type" .= String "droid_shield_violation", "command" .= String "command", "reason" .= String "reason"]
    )
  ]
