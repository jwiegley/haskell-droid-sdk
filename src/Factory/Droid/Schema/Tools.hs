{-# LANGUAGE OverloadedStrings #-}

-- | Built-in tool input and result records for Factory protocol 1.205.0.
-- Decoding neither executes a tool nor grants permission. Paths, URLs, command
-- text and patterns retain the wire schema's string domain; numeric options
-- are not coerced to integers or assigned constraints absent from the schema.
module Factory.Droid.Schema.Tools
  ( ToolOverrideParams (..),
    RiskLevel (..),
    ImageQuality (..),
    GrepOutputMode (..),
    PatchOperation (..),
    DiffLineType (..),
    ApplyPatchToolInput (..),
    CreateToolInput (..),
    EditToolInput (..),
    ExecuteToolInput (..),
    ReadToolInput (..),
    GlobToolInput (..),
    GrepToolInput (..),
    LSToolInput (..),
    WebSearchToolInput (..),
    FetchUrlToolInput (..),
    TaskToolInput (..),
    TodoWriteToolInput (..),
    ExitSpecModeToolInput (..),
    SkillToolInput (..),
    ProposeMissionToolInput (..),
    DiffLineNumbers (..),
    DiffLine (..),
    FileOperationResult (..),
    ApplyPatchFileChange (..),
    ApplyPatchToolResult (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    withObject,
    withText,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON
  ( additionalFields,
    objectWithAdditionalFields,
    optionalField,
    requireLiteral,
  )

-- | Requested tool-list overrides. Restriction is a separate allowlist, not
-- a permission elevation. This codec preserves lists without calculating
-- effective availability, resolving conflicts or applying any override.
data ToolOverrideParams = ToolOverrideParams
  { overrideAdditionalToolIds :: !(Maybe [Text]),
    overrideEnabledToolIds :: !(Maybe [Text]),
    overrideDisabledToolIds :: !(Maybe [Text]),
    overrideRestrictToolIds :: !(Maybe [Text]),
    overrideAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolOverrideParams where
  parseJSON = withObject "ToolOverrideParams" $ \fields ->
    ToolOverrideParams <$> fields .:! "additionalToolIds" <*> fields .:! "enabledToolIds" <*> fields .:! "disabledToolIds" <*> fields .:! "restrictToolIds" <*> pure (additionalFields overrideKeys fields)

instance ToJSON ToolOverrideParams where
  toJSON overrides =
    objectWithAdditionalFields overrideKeys (overrideAdditionalFields overrides) $
      optionalField "additionalToolIds" (overrideAdditionalToolIds overrides)
        <> optionalField "enabledToolIds" (overrideEnabledToolIds overrides)
        <> optionalField "disabledToolIds" (overrideDisabledToolIds overrides)
        <> optionalField "restrictToolIds" (overrideRestrictToolIds overrides)

overrideKeys :: [Key]
overrideKeys = ["additionalToolIds", "enabledToolIds", "disabledToolIds", "restrictToolIds"]

-- | Declared execution risk; parsing it does not authorize execution.
data RiskLevel = RiskLow | RiskMedium | RiskHigh
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON RiskLevel where
  parseJSON = withText "RiskLevel" $ \case
    "low" -> pure RiskLow
    "medium" -> pure RiskMedium
    "high" -> pure RiskHigh
    _ -> fail "Unknown risk level"

instance ToJSON RiskLevel where
  toJSON RiskLow = String "low"
  toJSON RiskMedium = String "medium"
  toJSON RiskHigh = String "high"

-- | Image quality requested by the read tool.
data ImageQuality = DefaultImageQuality | HighImageQuality
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ImageQuality where
  parseJSON = withText "ImageQuality" $ \case
    "default" -> pure DefaultImageQuality
    "high" -> pure HighImageQuality
    _ -> fail "Unknown image quality"

instance ToJSON ImageQuality where
  toJSON DefaultImageQuality = String "default"
  toJSON HighImageQuality = String "high"

-- | Whether grep returns paths or matching content.
data GrepOutputMode = GrepFilePaths | GrepContent
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON GrepOutputMode where
  parseJSON = withText "GrepOutputMode" $ \case
    "file_paths" -> pure GrepFilePaths
    "content" -> pure GrepContent
    _ -> fail "Unknown grep output mode"

instance ToJSON GrepOutputMode where
  toJSON GrepFilePaths = String "file_paths"
  toJSON GrepContent = String "content"

-- | The displayed operation for a changed file.
data PatchOperation = PatchCreate | PatchUpdate | PatchDelete
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON PatchOperation where
  parseJSON = withText "PatchOperation" $ \case
    "create" -> pure PatchCreate
    "update" -> pure PatchUpdate
    "delete" -> pure PatchDelete
    _ -> fail "Unknown patch operation"

instance ToJSON PatchOperation where
  toJSON PatchCreate = String "create"
  toJSON PatchUpdate = String "update"
  toJSON PatchDelete = String "delete"

-- | Diff line categories; unchanged and context are distinct wire values.
data DiffLineType = DiffAdded | DiffRemoved | DiffUnchanged | DiffContext
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON DiffLineType where
  parseJSON = withText "DiffLineType" $ \case
    "added" -> pure DiffAdded
    "removed" -> pure DiffRemoved
    "unchanged" -> pure DiffUnchanged
    "context" -> pure DiffContext
    _ -> fail "Unknown diff line type"

instance ToJSON DiffLineType where
  toJSON DiffAdded = String "added"
  toJSON DiffRemoved = String "removed"
  toJSON DiffUnchanged = String "unchanged"
  toJSON DiffContext = String "context"

-- | A patch and its target path, without patch parsing or application.
data ApplyPatchToolInput = ApplyPatchToolInput
  { patchInputFilePath :: !Text,
    patchInputPatch :: !Text,
    patchInputAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ApplyPatchToolInput where
  parseJSON = withObject "ApplyPatchToolInput" $ \fields ->
    ApplyPatchToolInput <$> fields .: "file_path" <*> fields .: "patch" <*> pure (additionalFields patchInputKeys fields)

instance ToJSON ApplyPatchToolInput where
  toJSON input = objectWithAdditionalFields patchInputKeys (patchInputAdditionalFields input) ["file_path" .= patchInputFilePath input, "patch" .= patchInputPatch input]

-- | File-creation input. Empty content is a present value.
data CreateToolInput = CreateToolInput
  { createFilePath :: !Text,
    createContent :: !Text,
    createAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CreateToolInput where
  parseJSON = withObject "CreateToolInput" $ \fields ->
    CreateToolInput <$> fields .: "file_path" <*> fields .: "content" <*> pure (additionalFields createKeys fields)

instance ToJSON CreateToolInput where
  toJSON input = objectWithAdditionalFields createKeys (createAdditionalFields input) ["file_path" .= createFilePath input, "content" .= createContent input]

-- | String-replacement input; the optional change-all flag is not defaulted.
data EditToolInput = EditToolInput
  { editFilePath :: !Text,
    editOldString :: !Text,
    editNewString :: !Text,
    editChangeAll :: !(Maybe Bool),
    editAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON EditToolInput where
  parseJSON = withObject "EditToolInput" $ \fields ->
    EditToolInput <$> fields .: "file_path" <*> fields .: "old_str" <*> fields .: "new_str" <*> fields .:! "change_all" <*> pure (additionalFields editKeys fields)

instance ToJSON EditToolInput where
  toJSON input =
    objectWithAdditionalFields editKeys (editAdditionalFields input) $
      ["file_path" .= editFilePath input, "old_str" .= editOldString input, "new_str" .= editNewString input]
        <> optionalField "change_all" (editChangeAll input)

-- | Execution input. Risk and fire-and-forget are data, not SDK policy.
data ExecuteToolInput = ExecuteToolInput
  { executeCommand :: !Text,
    executeSummary :: !(Maybe Text),
    executeTimeout :: !(Maybe Scientific),
    executeRiskLevel :: !(Maybe RiskLevel),
    executeRiskReason :: !(Maybe Text),
    executeFireAndForget :: !(Maybe Bool),
    executeAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ExecuteToolInput where
  parseJSON = withObject "ExecuteToolInput" $ \fields ->
    ExecuteToolInput
      <$> fields .: "command"
      <*> fields .:! "summary"
      <*> fields .:! "timeout"
      <*> fields .:! "riskLevel"
      <*> fields .:! "riskLevelReason"
      <*> fields .:! "fireAndForget"
      <*> pure (additionalFields executeKeys fields)

instance ToJSON ExecuteToolInput where
  toJSON input =
    objectWithAdditionalFields executeKeys (executeAdditionalFields input) $
      ["command" .= executeCommand input]
        <> optionalField "summary" (executeSummary input)
        <> optionalField "timeout" (executeTimeout input)
        <> optionalField "riskLevel" (executeRiskLevel input)
        <> optionalField "riskLevelReason" (executeRiskReason input)
        <> optionalField "fireAndForget" (executeFireAndForget input)

-- | Read-tool options. The numeric wire fields have no integer/range constraint.
data ReadToolInput = ReadToolInput
  { readFilePath :: !Text,
    readOffset :: !(Maybe Scientific),
    readLimit :: !(Maybe Scientific),
    readImageQuality :: !(Maybe ImageQuality),
    readAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ReadToolInput where
  parseJSON = withObject "ReadToolInput" $ \fields ->
    ReadToolInput <$> fields .: "file_path" <*> fields .:! "offset" <*> fields .:! "limit" <*> fields .:! "image_quality" <*> pure (additionalFields readKeys fields)

instance ToJSON ReadToolInput where
  toJSON input =
    objectWithAdditionalFields readKeys (readAdditionalFields input) $
      ["file_path" .= readFilePath input]
        <> optionalField "offset" (readOffset input)
        <> optionalField "limit" (readLimit input)
        <> optionalField "image_quality" (readImageQuality input)

-- | Glob options. Missing arrays differ from explicitly empty arrays.
data GlobToolInput = GlobToolInput
  { globPatterns :: !(Maybe [Text]),
    globFolder :: !(Maybe Text),
    globExcludePatterns :: !(Maybe [Text]),
    globAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GlobToolInput where
  parseJSON = withObject "GlobToolInput" $ \fields ->
    GlobToolInput <$> fields .:! "patterns" <*> fields .:! "folder" <*> fields .:! "excludePatterns" <*> pure (additionalFields globKeys fields)

instance ToJSON GlobToolInput where
  toJSON input =
    objectWithAdditionalFields globKeys (globAdditionalFields input) $
      optionalField "patterns" (globPatterns input)
        <> optionalField "folder" (globFolder input)
        <> optionalField "excludePatterns" (globExcludePatterns input)

-- | Grep options with their mixed wire naming conventions preserved.
data GrepToolInput = GrepToolInput
  { grepPattern :: !Text,
    grepPath :: !(Maybe Text),
    grepGlobPattern :: !(Maybe Text),
    grepCaseInsensitive :: !(Maybe Bool),
    grepOutputMode :: !(Maybe GrepOutputMode),
    grepContext :: !(Maybe Scientific),
    grepContextBefore :: !(Maybe Scientific),
    grepContextAfter :: !(Maybe Scientific),
    grepLineNumbers :: !(Maybe Bool),
    grepHeadLimit :: !(Maybe Scientific),
    grepAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GrepToolInput where
  parseJSON = withObject "GrepToolInput" $ \fields ->
    GrepToolInput
      <$> fields .: "pattern"
      <*> fields .:! "path"
      <*> fields .:! "glob_pattern"
      <*> fields .:! "case_insensitive"
      <*> fields .:! "output_mode"
      <*> fields .:! "context"
      <*> fields .:! "context_before"
      <*> fields .:! "context_after"
      <*> fields .:! "line_numbers"
      <*> fields .:! "head_limit"
      <*> pure (additionalFields grepKeys fields)

instance ToJSON GrepToolInput where
  toJSON input =
    objectWithAdditionalFields grepKeys (grepAdditionalFields input) $
      ["pattern" .= grepPattern input]
        <> optionalField "path" (grepPath input)
        <> optionalField "glob_pattern" (grepGlobPattern input)
        <> optionalField "case_insensitive" (grepCaseInsensitive input)
        <> optionalField "output_mode" (grepOutputMode input)
        <> optionalField "context" (grepContext input)
        <> optionalField "context_before" (grepContextBefore input)
        <> optionalField "context_after" (grepContextAfter input)
        <> optionalField "line_numbers" (grepLineNumbers input)
        <> optionalField "head_limit" (grepHeadLimit input)

-- | Directory-listing options, all optional in the supplied schema.
data LSToolInput = LSToolInput
  { listDirectoryPath :: !(Maybe Text),
    listIgnorePatterns :: !(Maybe [Text]),
    listAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON LSToolInput where
  parseJSON = withObject "LSToolInput" $ \fields ->
    LSToolInput <$> fields .:! "directory_path" <*> fields .:! "ignorePatterns" <*> pure (additionalFields listKeys fields)

instance ToJSON LSToolInput where
  toJSON input = objectWithAdditionalFields listKeys (listAdditionalFields input) (optionalField "directory_path" (listDirectoryPath input) <> optionalField "ignorePatterns" (listIgnorePatterns input))

-- | Search query and optional domain/category restrictions.
data WebSearchToolInput = WebSearchToolInput
  { searchQuery :: !Text,
    searchNumResults :: !(Maybe Scientific),
    searchIncludeDomains :: !(Maybe [Text]),
    searchExcludeDomains :: !(Maybe [Text]),
    searchCategory :: !(Maybe Text),
    searchAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON WebSearchToolInput where
  parseJSON = withObject "WebSearchToolInput" $ \fields ->
    WebSearchToolInput <$> fields .: "query" <*> fields .:! "numResults" <*> fields .:! "includeDomains" <*> fields .:! "excludeDomains" <*> fields .:! "category" <*> pure (additionalFields searchKeys fields)

instance ToJSON WebSearchToolInput where
  toJSON input =
    objectWithAdditionalFields searchKeys (searchAdditionalFields input) $
      ["query" .= searchQuery input]
        <> optionalField "numResults" (searchNumResults input)
        <> optionalField "includeDomains" (searchIncludeDomains input)
        <> optionalField "excludeDomains" (searchExcludeDomains input)
        <> optionalField "category" (searchCategory input)

-- | URL input. No URI grammar or network access is imposed by the wire codec.
data FetchUrlToolInput = FetchUrlToolInput
  { fetchUrl :: !Text,
    fetchAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON FetchUrlToolInput where
  parseJSON = withObject "FetchUrlToolInput" $ \fields ->
    FetchUrlToolInput <$> fields .: "url" <*> pure (additionalFields ["url"] fields)

instance ToJSON FetchUrlToolInput where
  toJSON input = objectWithAdditionalFields ["url"] (fetchAdditionalFields input) ["url" .= fetchUrl input]

-- | Delegation input. Availability of a named droid is a runtime constraint,
-- not something this string-valued wire record can establish.
data TaskToolInput = TaskToolInput
  { taskSubagentType :: !Text,
    taskDescription :: !Text,
    taskPrompt :: !Text,
    taskAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON TaskToolInput where
  parseJSON = withObject "TaskToolInput" $ \fields ->
    TaskToolInput <$> fields .: "subagent_type" <*> fields .: "description" <*> fields .: "prompt" <*> pure (additionalFields taskKeys fields)

instance ToJSON TaskToolInput where
  toJSON input = objectWithAdditionalFields taskKeys (taskAdditionalFields input) ["subagent_type" .= taskSubagentType input, "description" .= taskDescription input, "prompt" .= taskPrompt input]

-- | The todo tool accepts a string rather than a structured task array.
data TodoWriteToolInput = TodoWriteToolInput
  { todosText :: !Text,
    todosAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON TodoWriteToolInput where
  parseJSON = withObject "TodoWriteToolInput" $ \fields ->
    TodoWriteToolInput <$> fields .: "todos" <*> pure (additionalFields ["todos"] fields)

instance ToJSON TodoWriteToolInput where
  toJSON input = objectWithAdditionalFields ["todos"] (todosAdditionalFields input) ["todos" .= todosText input]

-- | Spec-mode exit proposal; encoding does not change a session's mode.
data ExitSpecModeToolInput = ExitSpecModeToolInput
  { exitSpecPlan :: !Text,
    exitSpecTitle :: !(Maybe Text),
    exitSpecAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ExitSpecModeToolInput where
  parseJSON = withObject "ExitSpecModeToolInput" $ \fields ->
    ExitSpecModeToolInput <$> fields .: "plan" <*> fields .:! "title" <*> pure (additionalFields exitSpecKeys fields)

instance ToJSON ExitSpecModeToolInput where
  toJSON input = objectWithAdditionalFields exitSpecKeys (exitSpecAdditionalFields input) (["plan" .= exitSpecPlan input] <> optionalField "title" (exitSpecTitle input))

-- | The requested skill name, without availability resolution.
data SkillToolInput = SkillToolInput
  { skillName :: !Text,
    skillAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SkillToolInput where
  parseJSON = withObject "SkillToolInput" $ \fields ->
    SkillToolInput <$> fields .: "skill" <*> pure (additionalFields ["skill"] fields)

instance ToJSON SkillToolInput where
  toJSON input = objectWithAdditionalFields ["skill"] (skillAdditionalFields input) ["skill" .= skillName input]

-- | Mission proposal text and optional title.
data ProposeMissionToolInput = ProposeMissionToolInput
  { missionProposal :: !Text,
    missionTitle :: !(Maybe Text),
    missionAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ProposeMissionToolInput where
  parseJSON = withObject "ProposeMissionToolInput" $ \fields ->
    ProposeMissionToolInput <$> fields .: "proposal" <*> fields .:! "title" <*> pure (additionalFields missionKeys fields)

instance ToJSON ProposeMissionToolInput where
  toJSON input = objectWithAdditionalFields missionKeys (missionAdditionalFields input) (["proposal" .= missionProposal input] <> optionalField "title" (missionTitle input))

-- | Old/new line coordinates. Either or both may be absent.
data DiffLineNumbers = DiffLineNumbers
  { diffOldLine :: !(Maybe Scientific),
    diffNewLine :: !(Maybe Scientific),
    diffLineNumberAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON DiffLineNumbers where
  parseJSON = withObject "DiffLineNumbers" $ \fields ->
    DiffLineNumbers <$> fields .:! "old" <*> fields .:! "new" <*> pure (additionalFields ["old", "new"] fields)

instance ToJSON DiffLineNumbers where
  toJSON numbers = objectWithAdditionalFields ["old", "new"] (diffLineNumberAdditionalFields numbers) (optionalField "old" (diffOldLine numbers) <> optionalField "new" (diffNewLine numbers))

-- | One diff line and optional coordinates.
data DiffLine = DiffLine
  { diffLineType :: !DiffLineType,
    diffLineContent :: !Text,
    diffLineNumbers :: !(Maybe DiffLineNumbers),
    diffLineAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON DiffLine where
  parseJSON = withObject "DiffLine" $ \fields ->
    DiffLine <$> fields .: "type" <*> fields .: "content" <*> fields .:! "lineNumber" <*> pure (additionalFields diffKeys fields)

instance ToJSON DiffLine where
  toJSON line = objectWithAdditionalFields diffKeys (diffLineAdditionalFields line) (["type" .= diffLineType line, "content" .= diffLineContent line] <> optionalField "lineNumber" (diffLineNumbers line))

-- | Generic file-operation output. The two path spellings are independent
-- fields; neither overwrites the other. Even success is optional here.
data FileOperationResult = FileOperationResult
  { fileResultSuccess :: !(Maybe Bool),
    fileResultDiff :: !(Maybe Text),
    fileResultDiffLines :: !(Maybe [DiffLine]),
    fileResultContent :: !(Maybe Text),
    fileResultMessage :: !(Maybe Text),
    fileResultSnakePath :: !(Maybe Text),
    fileResultCamelPath :: !(Maybe Text),
    fileResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON FileOperationResult where
  parseJSON = withObject "FileOperationResult" $ \fields ->
    FileOperationResult
      <$> fields .:! "success"
      <*> fields .:! "diff"
      <*> fields .:! "diffLines"
      <*> fields .:! "content"
      <*> fields .:! "message"
      <*> fields .:! "file_path"
      <*> fields .:! "filePath"
      <*> pure (additionalFields fileResultKeys fields)

instance ToJSON FileOperationResult where
  toJSON result =
    objectWithAdditionalFields fileResultKeys (fileResultAdditionalFields result) $
      optionalField "success" (fileResultSuccess result)
        <> optionalField "diff" (fileResultDiff result)
        <> optionalField "diffLines" (fileResultDiffLines result)
        <> optionalField "content" (fileResultContent result)
        <> optionalField "message" (fileResultMessage result)
        <> optionalField "file_path" (fileResultSnakePath result)
        <> optionalField "filePath" (fileResultCamelPath result)

-- | A changed file reported by a patch. The operation does not introduce
-- extra content/diff requirements that are absent from the supplied schema.
data ApplyPatchFileChange = ApplyPatchFileChange
  { patchFilePath :: !Text,
    patchOperation :: !PatchOperation,
    patchContent :: !(Maybe Text),
    patchDiff :: !(Maybe Text),
    patchError :: !(Maybe Text),
    patchMovedTo :: !(Maybe Text),
    patchSystemReminder :: !(Maybe Text),
    patchFileAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ApplyPatchFileChange where
  parseJSON = withObject "ApplyPatchFileChange" $ \fields ->
    ApplyPatchFileChange
      <$> fields .: "file_path"
      <*> fields .: "display_operation"
      <*> fields .:! "content"
      <*> fields .:! "diff"
      <*> fields .:! "error"
      <*> fields .:! "moved_to"
      <*> fields .:! "systemReminder"
      <*> pure (additionalFields patchFileKeys fields)

instance ToJSON ApplyPatchFileChange where
  toJSON file =
    objectWithAdditionalFields patchFileKeys (patchFileAdditionalFields file) $
      ["file_path" .= patchFilePath file, "display_operation" .= patchOperation file]
        <> optionalField "content" (patchContent file)
        <> optionalField "diff" (patchDiff file)
        <> optionalField "error" (patchError file)
        <> optionalField "moved_to" (patchMovedTo file)
        <> optionalField "systemReminder" (patchSystemReminder file)

-- | A successful patch response has at least one changed file. The Boolean
-- literal and nonempty array are represented by construction, not flags.
data ApplyPatchToolResult = ApplyPatchToolResult
  { patchResultFiles :: !(NonEmpty ApplyPatchFileChange),
    patchResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ApplyPatchToolResult where
  parseJSON = withObject "ApplyPatchToolResult" $ \fields -> do
    requireLiteral "success" (Bool True) fields
    ApplyPatchToolResult <$> fields .: "files" <*> pure (additionalFields ["success", "files"] fields)

instance ToJSON ApplyPatchToolResult where
  toJSON result = objectWithAdditionalFields ["success", "files"] (patchResultAdditionalFields result) ["success" .= True, "files" .= patchResultFiles result]

patchInputKeys, createKeys, editKeys, executeKeys, readKeys, globKeys, grepKeys, listKeys, searchKeys, taskKeys, exitSpecKeys, missionKeys, diffKeys, fileResultKeys, patchFileKeys :: [Key]
patchInputKeys = ["file_path", "patch"]
createKeys = ["file_path", "content"]
editKeys = ["file_path", "old_str", "new_str", "change_all"]
executeKeys = ["command", "summary", "timeout", "riskLevel", "riskLevelReason", "fireAndForget"]
readKeys = ["file_path", "offset", "limit", "image_quality"]
globKeys = ["patterns", "folder", "excludePatterns"]
grepKeys = ["pattern", "path", "glob_pattern", "case_insensitive", "output_mode", "context", "context_before", "context_after", "line_numbers", "head_limit"]
listKeys = ["directory_path", "ignorePatterns"]
searchKeys = ["query", "numResults", "includeDomains", "excludeDomains", "category"]
taskKeys = ["subagent_type", "description", "prompt"]
exitSpecKeys = ["plan", "title"]
missionKeys = ["proposal", "title"]
diffKeys = ["type", "content", "lineNumber"]
fileResultKeys = ["success", "diff", "diffLines", "content", "message", "file_path", "filePath"]
patchFileKeys = ["file_path", "display_operation", "content", "diff", "error", "moved_to", "systemReminder"]
