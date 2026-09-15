{-# LANGUAGE OverloadedStrings #-}

-- | Permission requests and questionnaires for Factory protocol 1.205.0.
-- Values are passive; the permission-result refinement follows CLI 1.201.1.
-- Request-bound authorization belongs to the interaction adapter.
module Factory.Droid.Schema.Interaction
  ( ToolConfirmationType (..),
    SandboxOperationType (..),
    SandboxViolationType (..),
    SandboxViolationReason (..),
    AskUserQuestion (..),
    AskUserCollectedAnswer (..),
    AskUserParams (..),
    AskUserResult (..),
    ParsedQuestionnaire (..),
    QuestionnaireParseError (..),
    ConfirmationPatchFile (..),
    ConfirmationDetails (..),
    ToolConfirmationDetails (..),
    ToolConfirmationInfo (..),
    permissionToolInputForDisplay,
    ToolConfirmationListItem (..),
    RequestPermissionParams (..),
    RequestPermissionResult,
    mkRequestPermissionResult,
    cancelPermissionResult,
    permissionSelectedOption,
    permissionComment,
    permissionEditedSpecContent,
    permissionResultAdditionalFields,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    Options,
    ToJSON (..),
    Value (String),
    camelTo2,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    withObject,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON (additionalFields, enumOptions, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Content (ToolUseBlock (toolUseInput))
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome (..))
import Factory.Droid.Schema.Tools (PatchOperation)
import GHC.Generics (Generic)

-- | The eleven declared confirmation kinds; unknown kinds are rejected.
data ToolConfirmationType
  = ConfirmationTypeEdit
  | ConfirmationTypeExec
  | ConfirmationTypeCreate
  | ConfirmationTypeAskUser
  | ConfirmationTypeExitSpecMode
  | ConfirmationTypeProposeMission
  | ConfirmationTypeStartMissionRun
  | ConfirmationTypeApplyPatch
  | ConfirmationTypeMcpTool
  | ConfirmationTypeSandboxViolation
  | ConfirmationTypeDroidShieldViolation
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON ToolConfirmationType where
  parseJSON = genericParseJSON confirmationTypeOptions

instance ToJSON ToolConfirmationType where
  toJSON = genericToJSON confirmationTypeOptions
  toEncoding = genericToEncoding confirmationTypeOptions

-- | The operation reported by a sandbox violation.
data SandboxOperationType = SandboxRead | SandboxWrite | SandboxNetwork | SandboxTool
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON SandboxOperationType where
  parseJSON = genericParseJSON sandboxOperationOptions

instance ToJSON SandboxOperationType where
  toJSON = genericToJSON sandboxOperationOptions
  toEncoding = genericToEncoding sandboxOperationOptions

-- | The reported sandbox boundary; these labels do not implement a sandbox.
data SandboxViolationType = ViolationFilesystemRead | ViolationFilesystemWrite | ViolationNetwork | ViolationTool
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON SandboxViolationType where
  parseJSON = genericParseJSON violationOptions

instance ToJSON SandboxViolationType where
  toJSON = genericToJSON violationOptions
  toEncoding = genericToEncoding violationOptions

-- | The explicit policy-rejection categories.
data SandboxViolationReason = ViolationDenyList | ViolationNotAllowed
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON SandboxViolationReason where
  parseJSON = genericParseJSON violationOptions

instance ToJSON SandboxViolationReason where
  toJSON = genericToJSON violationOptions
  toEncoding = genericToEncoding violationOptions

-- | A question with its offered options. The wire index is an unconstrained
-- JSON number, not a proof of a positive integer or a unique question number.
data AskUserQuestion = AskUserQuestion
  { questionIndex :: !Scientific,
    questionTopic :: !Text,
    questionText :: !Text,
    questionOptions :: ![Text],
    questionMultiSelect :: !(Maybe Bool),
    questionAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AskUserQuestion where
  parseJSON = withObject "AskUserQuestion" $ \fields ->
    AskUserQuestion <$> fields .: "index" <*> fields .: "topic" <*> fields .: "question" <*> fields .: "options" <*> fields .:! "multiSelect" <*> pure (additionalFields questionKeys fields)

instance ToJSON AskUserQuestion where
  toJSON question =
    objectWithAdditionalFields questionKeys (questionAdditionalFields question) $
      ["index" .= questionIndex question, "topic" .= questionTopic question, "question" .= questionText question, "options" .= questionOptions question]
        <> optionalField "multiSelect" (questionMultiSelect question)

-- | A collected textual answer. Multi-select responses remain wire text;
-- the codec does not invent a list encoding or enforce membership in options.
data AskUserCollectedAnswer = AskUserCollectedAnswer
  { answerIndex :: !Scientific,
    answerQuestion :: !Text,
    answerText :: !Text,
    answerAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AskUserCollectedAnswer where
  parseJSON = withObject "AskUserCollectedAnswer" $ \fields ->
    AskUserCollectedAnswer <$> fields .: "index" <*> fields .: "question" <*> fields .: "answer" <*> pure (additionalFields answerKeys fields)

instance ToJSON AskUserCollectedAnswer where
  toJSON answer = objectWithAdditionalFields answerKeys (answerAdditionalFields answer) ["index" .= answerIndex answer, "question" .= answerQuestion answer, "answer" .= answerText answer]

-- | Server-to-client questionnaire parameters, without the RPC envelope.
data AskUserParams = AskUserParams
  { askUserToolCallId :: !Text,
    askUserQuestions :: ![AskUserQuestion],
    askUserParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AskUserParams where
  parseJSON = withObject "AskUserParams" $ \fields ->
    AskUserParams <$> fields .: "toolCallId" <*> fields .: "questions" <*> pure (additionalFields ["toolCallId", "questions"] fields)

instance ToJSON AskUserParams where
  toJSON params = objectWithAdditionalFields ["toolCallId", "questions"] (askUserParamsAdditionalFields params) ["toolCallId" .= askUserToolCallId params, "questions" .= askUserQuestions params]

-- | Answers and an optional cancellation flag. Cancellation does not imply
-- an empty answer list in this schema; absent and false remain distinct.
data AskUserResult = AskUserResult
  { askUserAnswers :: ![AskUserCollectedAnswer],
    askUserCancelled :: !(Maybe Bool),
    askUserResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AskUserResult where
  parseJSON = withObject "AskUserResult" $ \fields ->
    AskUserResult <$> fields .: "answers" <*> fields .:! "cancelled" <*> pure (additionalFields ["answers", "cancelled"] fields)

instance ToJSON AskUserResult where
  toJSON result = objectWithAdditionalFields ["answers", "cancelled"] (askUserResultAdditionalFields result) (["answers" .= askUserAnswers result] <> optionalField "cancelled" (askUserCancelled result))

-- | The parsed questionnaire supplied by the peer; no text parsing occurs here.
data ParsedQuestionnaire = ParsedQuestionnaire
  { parsedQuestions :: ![AskUserQuestion],
    parsedQuestionnaireAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ParsedQuestionnaire where
  parseJSON = withObject "ParsedQuestionnaire" $ \fields ->
    ParsedQuestionnaire <$> fields .: "questions" <*> pure (additionalFields ["questions"] fields)

instance ToJSON ParsedQuestionnaire where
  toJSON parsed = objectWithAdditionalFields ["questions"] (parsedQuestionnaireAdditionalFields parsed) ["questions" .= parsedQuestions parsed]

-- | A reported parse error. The optional line has the schema's number domain.
data QuestionnaireParseError = QuestionnaireParseError
  { questionnaireErrorMessage :: !Text,
    questionnaireErrorLine :: !(Maybe Scientific),
    questionnaireErrorAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON QuestionnaireParseError where
  parseJSON = withObject "QuestionnaireParseError" $ \fields ->
    QuestionnaireParseError <$> fields .: "message" <*> fields .:! "line" <*> pure (additionalFields ["message", "line"] fields)

instance ToJSON QuestionnaireParseError where
  toJSON err = objectWithAdditionalFields ["message", "line"] (questionnaireErrorAdditionalFields err) (["message" .= questionnaireErrorMessage err] <> optionalField "line" (questionnaireErrorLine err))

-- | An individual file shown for patch confirmation, not an applied change.
data ConfirmationPatchFile = ConfirmationPatchFile
  { confirmationPatchPath :: !Text,
    confirmationPatchName :: !Text,
    confirmationPatchOperation :: !PatchOperation,
    confirmationPatchMoveTo :: !(Maybe Text),
    confirmationPatchOldContent :: !(Maybe Text),
    confirmationPatchNewContent :: !(Maybe Text),
    confirmationPatchAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ConfirmationPatchFile where
  parseJSON = withObject "ConfirmationPatchFile" $ \fields ->
    ConfirmationPatchFile <$> fields .: "filePath" <*> fields .: "fileName" <*> fields .: "operation" <*> fields .:! "moveTo" <*> fields .:! "oldContent" <*> fields .:! "newContent" <*> pure (additionalFields patchFileKeys fields)

instance ToJSON ConfirmationPatchFile where
  toJSON file =
    objectWithAdditionalFields patchFileKeys (confirmationPatchAdditionalFields file) $
      ["filePath" .= confirmationPatchPath file, "fileName" .= confirmationPatchName file, "operation" .= confirmationPatchOperation file]
        <> optionalField "moveTo" (confirmationPatchMoveTo file)
        <> optionalField "oldContent" (confirmationPatchOldContent file)
        <> optionalField "newContent" (confirmationPatchNewContent file)

-- | The complete detail union. Pattern-match on a constructor before using
-- its fields. Impact labels are free-form text; policy-violation enums are not.
data ConfirmationDetails
  = ConfirmationEdit
      { editConfirmationPath :: !Text,
        editConfirmationName :: !Text,
        editConfirmationOldContent :: !(Maybe Text),
        editConfirmationNewContent :: !(Maybe Text)
      }
  | ConfirmationExec
      { execConfirmationFullCommand :: !Text,
        execConfirmationCommand :: !Text,
        execConfirmationExtractedCommands :: !(Maybe [Text]),
        execConfirmationImpact :: !(Maybe Text),
        execConfirmationRiskReason :: !(Maybe Text)
      }
  | ConfirmationCreate
      { createConfirmationPath :: !Text,
        createConfirmationName :: !Text,
        createConfirmationContent :: !Text
      }
  | ConfirmationAskUser
      { askConfirmationQuestionnaire :: !Text,
        askConfirmationParsed :: !(Maybe ParsedQuestionnaire),
        askConfirmationParseError :: !(Maybe QuestionnaireParseError)
      }
  | ConfirmationExitSpecMode
      { specConfirmationPlan :: !Text,
        specConfirmationTitle :: !(Maybe Text)
      }
  | ConfirmationProposeMission
      { missionConfirmationProposal :: !Text,
        missionConfirmationTitle :: !(Maybe Text)
      }
  | ConfirmationStartMissionRun
      { missionConfirmationRunningCount :: !Scientific,
        missionConfirmationRunningSessionIds :: ![Text]
      }
  | ConfirmationApplyPatch
      { patchConfirmationPath :: !Text,
        patchConfirmationName :: !Text,
        patchConfirmationContent :: !Text,
        patchConfirmationOldContent :: !(Maybe Text),
        patchConfirmationNewContent :: !(Maybe Text),
        patchConfirmationFiles :: !(Maybe [ConfirmationPatchFile])
      }
  | ConfirmationMcpTool
      { mcpConfirmationToolName :: !Text,
        mcpConfirmationImpact :: !Text,
        mcpConfirmationServerName :: !(Maybe Text),
        mcpConfirmationActualToolName :: !(Maybe Text)
      }
  | ConfirmationSandboxViolation
      { sandboxConfirmationToolName :: !Text,
        sandboxConfirmationTarget :: !Text,
        sandboxConfirmationOperation :: !SandboxOperationType,
        sandboxConfirmationViolation :: !SandboxViolationType,
        sandboxConfirmationReason :: !Text,
        sandboxConfirmationViolationReason :: !(Maybe SandboxViolationReason),
        sandboxConfirmationOrgDeny :: !Bool
      }
  | ConfirmationDroidShieldViolation
      { shieldConfirmationCommand :: !Text,
        shieldConfirmationReason :: !Text
      }
  deriving stock (Eq, Show)

-- | A discriminated detail object retaining extensions for its selected branch.
data ToolConfirmationDetails = ToolConfirmationDetails
  { confirmationDetails :: !ConfirmationDetails,
    confirmationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolConfirmationDetails where
  parseJSON = withObject "ToolConfirmationDetails" $ \fields -> do
    kind <- fields .: "type"
    details <- case kind of
      ConfirmationTypeEdit -> ConfirmationEdit <$> fields .: "filePath" <*> fields .: "fileName" <*> fields .:! "oldContent" <*> fields .:! "newContent"
      ConfirmationTypeExec -> ConfirmationExec <$> fields .: "fullCommand" <*> fields .: "command" <*> fields .:! "extractedCommands" <*> fields .:! "impactLevel" <*> fields .:! "riskLevelReason"
      ConfirmationTypeCreate -> ConfirmationCreate <$> fields .: "filePath" <*> fields .: "fileName" <*> fields .: "content"
      ConfirmationTypeAskUser -> ConfirmationAskUser <$> fields .: "questionnaire" <*> fields .:! "parsed" <*> fields .:! "parseError"
      ConfirmationTypeExitSpecMode -> ConfirmationExitSpecMode <$> fields .: "plan" <*> fields .:! "title"
      ConfirmationTypeProposeMission -> ConfirmationProposeMission <$> fields .: "proposal" <*> fields .:! "title"
      ConfirmationTypeStartMissionRun -> ConfirmationStartMissionRun <$> fields .: "runningMissionCount" <*> fields .: "runningMissionSessionIds"
      ConfirmationTypeApplyPatch -> ConfirmationApplyPatch <$> fields .: "filePath" <*> fields .: "fileName" <*> fields .: "patchContent" <*> fields .:! "oldContent" <*> fields .:! "newContent" <*> fields .:! "files"
      ConfirmationTypeMcpTool -> ConfirmationMcpTool <$> fields .: "toolName" <*> fields .: "impactLevel" <*> fields .:! "serverName" <*> fields .:! "actualToolName"
      ConfirmationTypeSandboxViolation -> ConfirmationSandboxViolation <$> fields .: "violatingToolName" <*> fields .: "target" <*> fields .: "operationType" <*> fields .: "violationType" <*> fields .: "reason" <*> fields .:! "violationReason" <*> fields .: "isOrgDeny"
      ConfirmationTypeDroidShieldViolation -> ConfirmationDroidShieldViolation <$> fields .: "command" <*> fields .: "reason"
    let (keys, _) = confirmationEncoding details
    pure (ToolConfirmationDetails details (additionalFields keys fields))

instance ToJSON ToolConfirmationDetails where
  toJSON details =
    let (keys, fields) = confirmationEncoding (confirmationDetails details)
     in objectWithAdditionalFields keys (confirmationAdditionalFields details) fields

confirmationEncoding :: ConfirmationDetails -> ([Key], [Pair])
confirmationEncoding = \case
  ConfirmationEdit path name old new ->
    tagged ConfirmationTypeEdit ["filePath", "fileName", "oldContent", "newContent"] $
      ["filePath" .= path, "fileName" .= name] <> optionalField "oldContent" old <> optionalField "newContent" new
  ConfirmationExec full command extracted impact reason ->
    tagged ConfirmationTypeExec ["fullCommand", "command", "extractedCommands", "impactLevel", "riskLevelReason"] $
      ["fullCommand" .= full, "command" .= command] <> optionalField "extractedCommands" extracted <> optionalField "impactLevel" impact <> optionalField "riskLevelReason" reason
  ConfirmationCreate path name content -> tagged ConfirmationTypeCreate ["filePath", "fileName", "content"] ["filePath" .= path, "fileName" .= name, "content" .= content]
  ConfirmationAskUser questionnaire parsed err ->
    tagged ConfirmationTypeAskUser ["questionnaire", "parsed", "parseError"] $
      ["questionnaire" .= questionnaire] <> optionalField "parsed" parsed <> optionalField "parseError" err
  ConfirmationExitSpecMode plan title -> tagged ConfirmationTypeExitSpecMode ["plan", "title"] (["plan" .= plan] <> optionalField "title" title)
  ConfirmationProposeMission proposal title -> tagged ConfirmationTypeProposeMission ["proposal", "title"] (["proposal" .= proposal] <> optionalField "title" title)
  ConfirmationStartMissionRun count sessions -> tagged ConfirmationTypeStartMissionRun ["runningMissionCount", "runningMissionSessionIds"] ["runningMissionCount" .= count, "runningMissionSessionIds" .= sessions]
  ConfirmationApplyPatch path name patch old new files ->
    tagged ConfirmationTypeApplyPatch ["filePath", "fileName", "patchContent", "oldContent", "newContent", "files"] $
      ["filePath" .= path, "fileName" .= name, "patchContent" .= patch] <> optionalField "oldContent" old <> optionalField "newContent" new <> optionalField "files" files
  ConfirmationMcpTool name impact server actual ->
    tagged ConfirmationTypeMcpTool ["toolName", "impactLevel", "serverName", "actualToolName"] $
      ["toolName" .= name, "impactLevel" .= impact] <> optionalField "serverName" server <> optionalField "actualToolName" actual
  ConfirmationSandboxViolation tool target operation violation reason violationReason orgDeny ->
    tagged ConfirmationTypeSandboxViolation ["violatingToolName", "target", "operationType", "violationType", "reason", "violationReason", "isOrgDeny"] $
      ["violatingToolName" .= tool, "target" .= target, "operationType" .= operation, "violationType" .= violation, "reason" .= reason, "isOrgDeny" .= orgDeny] <> optionalField "violationReason" violationReason
  ConfirmationDroidShieldViolation command reason -> tagged ConfirmationTypeDroidShieldViolation ["command", "reason"] ["command" .= command, "reason" .= reason]
  where
    tagged :: ToolConfirmationType -> [Key] -> [Pair] -> ([Key], [Pair])
    tagged kind keys fields = ("type" : keys, ("type" .= kind) : fields)

-- | The outer confirmation kind and nested discriminator are independent in
-- the supplied schema. Decoding either does not establish policy consistency.
data ToolConfirmationInfo = ToolConfirmationInfo
  { confirmationInfoToolUse :: !ToolUseBlock,
    confirmationInfoType :: !ToolConfirmationType,
    confirmationInfoDetails :: !ToolConfirmationDetails,
    confirmationInfoAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolConfirmationInfo where
  parseJSON = withObject "ToolConfirmationInfo" $ \fields ->
    ToolConfirmationInfo <$> fields .: "toolUse" <*> fields .: "confirmationType" <*> fields .: "details" <*> pure (additionalFields confirmationInfoKeys fields)

instance ToJSON ToolConfirmationInfo where
  toJSON info = objectWithAdditionalFields confirmationInfoKeys (confirmationInfoAdditionalFields info) ["toolUse" .= confirmationInfoToolUse info, "confirmationType" .= confirmationInfoType info, "details" .= confirmationInfoDetails info]

-- | A display-only copy. Matching spec/mission details fill missing, empty or
-- non-string fields; nonempty input strings, including whitespace, win.
-- No permission decision or stored input is changed.
permissionToolInputForDisplay :: ToolConfirmationInfo -> Object
permissionToolInputForDisplay info = foldl' fill original fallbacks
  where
    original = toolUseInput (confirmationInfoToolUse info)
    fallbacks = case (confirmationInfoType info, confirmationDetails (confirmationInfoDetails info)) of
      (ConfirmationTypeExitSpecMode, ConfirmationExitSpecMode plan title) -> [("plan", Just plan), ("title", title)]
      (ConfirmationTypeProposeMission, ConfirmationProposeMission proposal title) -> [("proposal", Just proposal), ("title", title)]
      _ -> []
    fill fields (_, Nothing) = fields
    fill fields (key, Just value)
      | Text.null value = fields
      | Just (String current) <- KeyMap.lookup key fields, not (Text.null current) = fields
      | otherwise = KeyMap.insert key (String value) fields

-- | An offered permission choice. Labels are not interpreted as decisions.
data ToolConfirmationListItem = ToolConfirmationListItem
  { confirmationOptionLabel :: !Text,
    confirmationOptionValue :: !ToolConfirmationOutcome,
    confirmationOptionAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolConfirmationListItem where
  parseJSON = withObject "ToolConfirmationListItem" $ \fields ->
    ToolConfirmationListItem <$> fields .: "label" <*> fields .: "value" <*> pure (additionalFields ["label", "value"] fields)

instance ToJSON ToolConfirmationListItem where
  toJSON item = objectWithAdditionalFields ["label", "value"] (confirmationOptionAdditionalFields item) ["label" .= confirmationOptionLabel item, "value" .= confirmationOptionValue item]

-- | A batch of permission requests and offered choices. Empty lists and
-- duplicate session IDs are permitted by the wire schema, not normalized.
data RequestPermissionParams = RequestPermissionParams
  { permissionToolUses :: ![ToolConfirmationInfo],
    permissionOptions :: ![ToolConfirmationListItem],
    permissionAssociatedSessionIds :: !(Maybe [Text]),
    permissionParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON RequestPermissionParams where
  parseJSON = withObject "RequestPermissionParams" $ \fields ->
    RequestPermissionParams <$> fields .: "toolUses" <*> fields .: "options" <*> fields .:! "associatedSessionIds" <*> pure (additionalFields permissionParamsKeys fields)

instance ToJSON RequestPermissionParams where
  toJSON params =
    objectWithAdditionalFields permissionParamsKeys (permissionParamsAdditionalFields params) $
      ["toolUses" .= permissionToolUses params, "options" .= permissionOptions params] <> optionalField "associatedSessionIds" (permissionAssociatedSessionIds params)

-- | A refined wire result. Choosing proceed_edit requires edited content,
-- including the empty string. This does not establish that a choice was offered.
data RequestPermissionResult = PermissionResult !ToolConfirmationOutcome !(Maybe Text) !(Maybe Text) !Object
  deriving stock (Eq)

instance Show RequestPermissionResult where
  show _ = "RequestPermissionResult <redacted>"

mkRequestPermissionResult :: ToolConfirmationOutcome -> Maybe Text -> Maybe Text -> Object -> Maybe RequestPermissionResult
mkRequestPermissionResult choice comment edited extras = case (choice, edited) of
  (ConfirmProceedEdit, Nothing) -> Nothing
  _ -> Just (PermissionResult choice comment edited extras)

cancelPermissionResult :: RequestPermissionResult
cancelPermissionResult = PermissionResult ConfirmCancel Nothing Nothing mempty

permissionSelectedOption :: RequestPermissionResult -> ToolConfirmationOutcome
permissionSelectedOption (PermissionResult choice _ _ _) = choice

permissionComment :: RequestPermissionResult -> Maybe Text
permissionComment (PermissionResult _ comment _ _) = comment

permissionEditedSpecContent :: RequestPermissionResult -> Maybe Text
permissionEditedSpecContent (PermissionResult _ _ edited _) = edited

permissionResultAdditionalFields :: RequestPermissionResult -> Object
permissionResultAdditionalFields (PermissionResult _ _ _ extras) = extras

instance FromJSON RequestPermissionResult where
  parseJSON = withObject "RequestPermissionResult" $ \fields -> do
    choice <- fields .: "selectedOption"
    comment <- fields .:! "comment"
    edited <- fields .:! "editedSpecContent"
    maybe (fail "Edited spec content is required for proceed_edit") pure (mkRequestPermissionResult choice comment edited (additionalFields permissionResultKeys fields))

instance ToJSON RequestPermissionResult where
  toJSON (PermissionResult choice comment edited extras) = objectWithAdditionalFields permissionResultKeys extras (["selectedOption" .= choice] <> optionalField "comment" comment <> optionalField "editedSpecContent" edited)

permissionResultKeys :: [Key]
permissionResultKeys = ["selectedOption", "comment", "editedSpecContent"]

confirmationTypeOptions, sandboxOperationOptions, violationOptions :: Options
confirmationTypeOptions = enumOptions "ConfirmationType" (camelTo2 '_')
sandboxOperationOptions = enumOptions "Sandbox" (camelTo2 '_')
violationOptions = enumOptions "Violation" (camelTo2 '-')

questionKeys, answerKeys, patchFileKeys, confirmationInfoKeys, permissionParamsKeys :: [Key]
questionKeys = ["index", "topic", "question", "options", "multiSelect"]
answerKeys = ["index", "question", "answer"]
patchFileKeys = ["filePath", "fileName", "operation", "moveTo", "oldContent", "newContent"]
confirmationInfoKeys = ["toolUse", "confirmationType", "details"]
permissionParamsKeys = ["toolUses", "options", "associatedSessionIds"]
