{-# LANGUAGE OverloadedStrings #-}

-- | Daemon workspace wire bodies for protocol 1.205.0. No filesystem, network,
-- working-directory or trust mutation occurs here. Record 'Show' instances
-- redact values; explicit field access and JSON encoding remain sensitive.
module Factory.Droid.Schema.Daemon.Workspace
  ( WorkspaceEncoding (..),
    SetupStepKind (..),
    FolderPathParams (..),
    CheckFolderTrustParams,
    TrustFolderParams,
    CheckFolderTrustResult (..),
    TrustFolderResult (..),
    GetWorkspaceFileContentParams (..),
    GetWorkspaceFileContentResult (..),
    WriteWorkspaceFileContentParams (..),
    WriteWorkspaceFileContentResult (..),
    ListFilesParams (..),
    ListFilesResult (..),
    SearchFilesParams (..),
    SearchFilesResult (..),
    PushCwdFileToUrlParams (..),
    PushCwdFileToUrlResult (..),
    PullUrlToCwdFileParams (..),
    PullUrlToCwdFileResult (..),
    ChangeSessionWorkingDirectoryParams (..),
    SetupStepProgress (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Numeric.Natural (Natural)

-- | The peer's declared content encoding, not proof of valid UTF-8 or base64.
data WorkspaceEncoding = WorkspaceUtf8 | WorkspaceBase64
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON WorkspaceEncoding where
  parseJSON = withText "WorkspaceEncoding" $ \case
    "utf8" -> pure WorkspaceUtf8
    "base64" -> pure WorkspaceBase64
    _ -> fail "Unknown workspace content encoding"

instance ToJSON WorkspaceEncoding where
  toJSON WorkspaceUtf8 = String "utf8"
  toJSON WorkspaceBase64 = String "base64"

-- | Reported setup work, without running a worktree command or setup script.
data SetupStepKind = SetupWorktreeCreation | SetupWorktreeScript | SetupOriginPull
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SetupStepKind where
  parseJSON = withText "SetupStepKind" $ \case
    "worktree-creation" -> pure SetupWorktreeCreation
    "worktree-setup-script" -> pure SetupWorktreeScript
    "origin-pull" -> pure SetupOriginPull
    _ -> fail "Unknown setup step kind"

instance ToJSON SetupStepKind where
  toJSON SetupWorktreeCreation = String "worktree-creation"
  toJSON SetupWorktreeScript = String "worktree-setup-script"
  toJSON SetupOriginPull = String "origin-pull"

-- | The identical path-only bodies for checking or requesting folder trust.
-- A path is an unparsed string and does not prove containment or authorization.
data FolderPathParams = FolderPathParams
  { folderPath :: !Text,
    folderPathAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show FolderPathParams where
  show _ = "FolderPathParams <redacted>"

instance FromJSON FolderPathParams where
  parseJSON = withObject "FolderPathParams" $ \fields -> FolderPathParams <$> fields .: "path" <*> pure (additionalFields ["path"] fields)

instance ToJSON FolderPathParams where
  toJSON params = objectWithAdditionalFields ["path"] (folderPathAdditionalFields params) ["path" .= folderPath params]

-- | Check-trust parameters, not a trust decision.
type CheckFolderTrustParams = FolderPathParams

-- | Trust-operation parameters, not a grant or an executed mutation.
type TrustFolderParams = FolderPathParams

-- | Independent trust and prompt flags reported by the daemon. In particular,
-- absence of a required prompt is not inferred to mean that a folder is trusted.
data CheckFolderTrustResult = CheckFolderTrustResult
  { folderIsTrusted :: !Bool,
    folderTrustRootPath :: !Text,
    folderPromptRequired :: !Bool,
    folderTrustAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CheckFolderTrustResult where
  show _ = "CheckFolderTrustResult <redacted>"

instance FromJSON CheckFolderTrustResult where
  parseJSON = withObject "CheckFolderTrustResult" $ \fields -> CheckFolderTrustResult <$> fields .: "isTrusted" <*> fields .: "trustRootPath" <*> fields .: "promptRequired" <*> pure (additionalFields trustKeys fields)

instance ToJSON CheckFolderTrustResult where
  toJSON result = objectWithAdditionalFields trustKeys (folderTrustAdditionalFields result) ["isTrusted" .= folderIsTrusted result, "trustRootPath" .= folderTrustRootPath result, "promptRequired" .= folderPromptRequired result]

-- | The trust root reported after a trust operation; parsing does not grant trust.
data TrustFolderResult = TrustFolderResult
  { trustedRootPath :: !Text,
    trustedRootAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TrustFolderResult where
  show _ = "TrustFolderResult <redacted>"

instance FromJSON TrustFolderResult where
  parseJSON = withObject "TrustFolderResult" $ \fields -> TrustFolderResult <$> fields .: "trustRootPath" <*> pure (additionalFields ["trustRootPath"] fields)

instance ToJSON TrustFolderResult where
  toJSON result = objectWithAdditionalFields ["trustRootPath"] (trustedRootAdditionalFields result) ["trustRootPath" .= trustedRootPath result]

-- | A session-scoped file-read body. Missing encoding/metadataOnly fields stay
-- absent; this codec does not read data or infer a response from the request.
data GetWorkspaceFileContentParams = GetWorkspaceFileContentParams
  { readFileSessionId :: !Text,
    readFilePath :: !Text,
    readFileMetadataOnly :: !(Maybe Bool),
    readFileEncoding :: !(Maybe WorkspaceEncoding),
    readFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GetWorkspaceFileContentParams where
  show _ = "GetWorkspaceFileContentParams <redacted>"

instance FromJSON GetWorkspaceFileContentParams where
  parseJSON = withObject "GetWorkspaceFileContentParams" $ \fields -> GetWorkspaceFileContentParams <$> fields .: "sessionId" <*> fields .: "filePath" <*> fields .:! "metadataOnly" <*> fields .:! "encoding" <*> pure (additionalFields readKeys fields)

instance ToJSON GetWorkspaceFileContentParams where
  toJSON params = objectWithAdditionalFields readKeys (readFileAdditionalFields params) (["sessionId" .= readFileSessionId params, "filePath" .= readFilePath params] <> optionalField "metadataOnly" (readFileMetadataOnly params) <> optionalField "encoding" (readFileEncoding params))

-- | File content and reported metadata. The read-result byte length is an
-- unconstrained JSON number, unlike write/transfer-result lengths. Encoding,
-- binary flags, MIME labels and fingerprints are not verified against content.
data GetWorkspaceFileContentResult = GetWorkspaceFileContentResult
  { workspaceFileContent :: !Text,
    workspaceFileByteLength :: !Scientific,
    workspaceFileEncoding :: !(Maybe WorkspaceEncoding),
    workspaceFileMimeType :: !(Maybe Text),
    workspaceFileIsBinary :: !(Maybe Bool),
    workspaceFileFingerprint :: !(Maybe Text),
    workspaceFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GetWorkspaceFileContentResult where
  show _ = "GetWorkspaceFileContentResult <redacted>"

instance FromJSON GetWorkspaceFileContentResult where
  parseJSON = withObject "GetWorkspaceFileContentResult" $ \fields -> GetWorkspaceFileContentResult <$> fields .: "content" <*> fields .: "byteLength" <*> fields .:! "encoding" <*> fields .:! "mimeType" <*> fields .:! "isBinary" <*> fields .:! "fingerprint" <*> pure (additionalFields contentKeys fields)

instance ToJSON GetWorkspaceFileContentResult where
  toJSON result =
    objectWithAdditionalFields contentKeys (workspaceFileAdditionalFields result) $
      ["content" .= workspaceFileContent result, "byteLength" .= workspaceFileByteLength result]
        <> optionalField "encoding" (workspaceFileEncoding result)
        <> optionalField "mimeType" (workspaceFileMimeType result)
        <> optionalField "isBinary" (workspaceFileIsBinary result)
        <> optionalField "fingerprint" (workspaceFileFingerprint result)

-- | A write body with an optional concurrency fingerprint. No path containment,
-- fingerprint comparison, content write or conflict handling happens here.
data WriteWorkspaceFileContentParams = WriteWorkspaceFileContentParams
  { writeFileSessionId :: !Text,
    writeFilePath :: !Text,
    writeFileContent :: !Text,
    writeFileBaseFingerprint :: !(Maybe Text),
    writeFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WriteWorkspaceFileContentParams where
  show _ = "WriteWorkspaceFileContentParams <redacted>"

instance FromJSON WriteWorkspaceFileContentParams where
  parseJSON = withObject "WriteWorkspaceFileContentParams" $ \fields -> WriteWorkspaceFileContentParams <$> fields .: "sessionId" <*> fields .: "filePath" <*> fields .: "content" <*> fields .:! "baseFingerprint" <*> pure (additionalFields writeKeys fields)

instance ToJSON WriteWorkspaceFileContentParams where
  toJSON params = objectWithAdditionalFields writeKeys (writeFileAdditionalFields params) (["sessionId" .= writeFileSessionId params, "filePath" .= writeFilePath params, "content" .= writeFileContent params] <> optionalField "baseFingerprint" (writeFileBaseFingerprint params))

-- | Reported nonnegative integral byte length and opaque resulting fingerprint.
data WriteWorkspaceFileContentResult = WriteWorkspaceFileContentResult
  { writtenFileByteLength :: !Natural,
    writtenFileFingerprint :: !Text,
    writtenFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WriteWorkspaceFileContentResult where
  show _ = "WriteWorkspaceFileContentResult <redacted>"

instance FromJSON WriteWorkspaceFileContentResult where
  parseJSON = withObject "WriteWorkspaceFileContentResult" $ \fields -> WriteWorkspaceFileContentResult <$> fields .: "byteLength" <*> fields .: "fingerprint" <*> pure (additionalFields ["byteLength", "fingerprint"] fields)

instance ToJSON WriteWorkspaceFileContentResult where
  toJSON result = objectWithAdditionalFields ["byteLength", "fingerprint"] (writtenFileAdditionalFields result) ["byteLength" .= writtenFileByteLength result, "fingerprint" .= writtenFileFingerprint result]

-- | File-list parameters. showHidden's default annotation is not materialized.
data ListFilesParams = ListFilesParams
  { listFilesSessionId :: !Text,
    listFilesPath :: !(Maybe Text),
    listFilesShowHidden :: !(Maybe Bool),
    listFilesParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListFilesParams where
  show _ = "ListFilesParams <redacted>"

instance FromJSON ListFilesParams where
  parseJSON = withObject "ListFilesParams" $ \fields -> ListFilesParams <$> fields .: "sessionId" <*> fields .:! "path" <*> fields .:! "showHidden" <*> pure (additionalFields listParamsKeys fields)

instance ToJSON ListFilesParams where
  toJSON params = objectWithAdditionalFields listParamsKeys (listFilesParamsAdditionalFields params) (["sessionId" .= listFilesSessionId params] <> optionalField "path" (listFilesPath params) <> optionalField "showHidden" (listFilesShowHidden params))

-- | File-list data. Optional counts/depth are nonnegative integers, without
-- assumptions about directory presence, truncation or correspondence to the list.
data ListFilesResult = ListFilesResult
  { listedFilePaths :: ![Text],
    listedDirectoryPaths :: !(Maybe [Text]),
    listedFilesTotal :: !(Maybe Natural),
    listedFilesCompleteDepth :: !(Maybe Natural),
    listedFilesTruncated :: !(Maybe Bool),
    listedFilesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListFilesResult where
  show _ = "ListFilesResult <redacted>"

instance FromJSON ListFilesResult where
  parseJSON = withObject "ListFilesResult" $ \fields -> ListFilesResult <$> fields .: "files" <*> fields .:! "directories" <*> fields .:! "totalFiles" <*> fields .:! "completeDepth" <*> fields .:! "truncated" <*> pure (additionalFields listResultKeys fields)

instance ToJSON ListFilesResult where
  toJSON result = objectWithAdditionalFields listResultKeys (listedFilesAdditionalFields result) (["files" .= listedFilePaths result] <> optionalField "directories" (listedDirectoryPaths result) <> optionalField "totalFiles" (listedFilesTotal result) <> optionalField "completeDepth" (listedFilesCompleteDepth result) <> optionalField "truncated" (listedFilesTruncated result))

-- | Search parameters with an unconstrained numeric result limit. Missing
-- limits/hidden flags stay absent; no searching or default resolution occurs.
data SearchFilesParams = SearchFilesParams
  { searchFilesSessionId :: !Text,
    searchFilesQuery :: !Text,
    searchFilesMaxResults :: !(Maybe Scientific),
    searchFilesShowHidden :: !(Maybe Bool),
    searchFilesParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SearchFilesParams where
  show _ = "SearchFilesParams <redacted>"

instance FromJSON SearchFilesParams where
  parseJSON = withObject "SearchFilesParams" $ \fields -> SearchFilesParams <$> fields .: "sessionId" <*> fields .: "query" <*> fields .:! "maxResults" <*> fields .:! "showHidden" <*> pure (additionalFields searchParamsKeys fields)

instance ToJSON SearchFilesParams where
  toJSON params = objectWithAdditionalFields searchParamsKeys (searchFilesParamsAdditionalFields params) (["sessionId" .= searchFilesSessionId params, "query" .= searchFilesQuery params] <> optionalField "maxResults" (searchFilesMaxResults params) <> optionalField "showHidden" (searchFilesShowHidden params))

-- | Search results use a required JSON number for totalFiles, not the optional
-- nonnegative integer in ListFilesResult. Names remain ordered wire strings.
data SearchFilesResult = SearchFilesResult
  { searchedFilePaths :: ![Text],
    searchedFilesTotal :: !Scientific,
    searchedFilesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SearchFilesResult where
  show _ = "SearchFilesResult <redacted>"

instance FromJSON SearchFilesResult where
  parseJSON = withObject "SearchFilesResult" $ \fields -> SearchFilesResult <$> fields .: "files" <*> fields .: "totalFiles" <*> pure (additionalFields ["files", "totalFiles"] fields)

instance ToJSON SearchFilesResult where
  toJSON result = objectWithAdditionalFields ["files", "totalFiles"] (searchedFilesAdditionalFields result) ["files" .= searchedFilePaths result, "totalFiles" .= searchedFilesTotal result]

-- | A session-scoped upload body. The presigned URL is sensitive opaque text;
-- this codec does not authorize a destination, read a file or upload bytes.
data PushCwdFileToUrlParams = PushCwdFileToUrlParams
  { pushFileSessionId :: !Text,
    pushFilePath :: !Text,
    pushFilePresignedUrl :: !Text,
    pushFileContentType :: !(Maybe Text),
    pushFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PushCwdFileToUrlParams where
  show _ = "PushCwdFileToUrlParams <redacted>"

instance FromJSON PushCwdFileToUrlParams where
  parseJSON = withObject "PushCwdFileToUrlParams" $ \fields -> PushCwdFileToUrlParams <$> fields .: "sessionId" <*> fields .: "filePath" <*> fields .: "presignedPutUrl" <*> fields .:! "contentType" <*> pure (additionalFields pushKeys fields)

instance ToJSON PushCwdFileToUrlParams where
  toJSON params = objectWithAdditionalFields pushKeys (pushFileAdditionalFields params) (["sessionId" .= pushFileSessionId params, "filePath" .= pushFilePath params, "presignedPutUrl" .= pushFilePresignedUrl params] <> optionalField "contentType" (pushFileContentType params))

-- | An upload's reported byte count, not proof of a completed transfer.
data PushCwdFileToUrlResult = PushCwdFileToUrlResult
  { pushedFileByteLength :: !Natural,
    pushedFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PushCwdFileToUrlResult where
  show _ = "PushCwdFileToUrlResult <redacted>"

instance FromJSON PushCwdFileToUrlResult where
  parseJSON = withObject "PushCwdFileToUrlResult" $ \fields -> PushCwdFileToUrlResult <$> fields .: "byteLength" <*> pure (additionalFields ["byteLength"] fields)

instance ToJSON PushCwdFileToUrlResult where
  toJSON result = objectWithAdditionalFields ["byteLength"] (pushedFileAdditionalFields result) ["byteLength" .= pushedFileByteLength result]

-- | A session-scoped download body with an optional nonnegative integral
-- expected length. The destination is not resolved or written by this codec.
data PullUrlToCwdFileParams = PullUrlToCwdFileParams
  { pullFileSessionId :: !Text,
    pullFilePresignedUrl :: !Text,
    pullFileDestination :: !Text,
    pullFileExpectedLength :: !(Maybe Natural),
    pullFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PullUrlToCwdFileParams where
  show _ = "PullUrlToCwdFileParams <redacted>"

instance FromJSON PullUrlToCwdFileParams where
  parseJSON = withObject "PullUrlToCwdFileParams" $ \fields -> PullUrlToCwdFileParams <$> fields .: "sessionId" <*> fields .: "presignedGetUrl" <*> fields .: "destPath" <*> fields .:! "expectedContentLength" <*> pure (additionalFields pullKeys fields)

instance ToJSON PullUrlToCwdFileParams where
  toJSON params = objectWithAdditionalFields pullKeys (pullFileAdditionalFields params) (["sessionId" .= pullFileSessionId params, "presignedGetUrl" .= pullFilePresignedUrl params, "destPath" .= pullFileDestination params] <> optionalField "expectedContentLength" (pullFileExpectedLength params))

-- | Reported written path and nonnegative integral byte length.
data PullUrlToCwdFileResult = PullUrlToCwdFileResult
  { pulledFileByteLength :: !Natural,
    pulledFileWrittenPath :: !Text,
    pulledFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PullUrlToCwdFileResult where
  show _ = "PullUrlToCwdFileResult <redacted>"

instance FromJSON PullUrlToCwdFileResult where
  parseJSON = withObject "PullUrlToCwdFileResult" $ \fields -> PullUrlToCwdFileResult <$> fields .: "byteLength" <*> fields .: "writtenPath" <*> pure (additionalFields ["byteLength", "writtenPath"] fields)

instance ToJSON PullUrlToCwdFileResult where
  toJSON result = objectWithAdditionalFields ["byteLength", "writtenPath"] (pulledFileAdditionalFields result) ["byteLength" .= pulledFileByteLength result, "writtenPath" .= pulledFileWrittenPath result]

-- | Session-targeted directory-change parameters. No SDK process cwd changes.
data ChangeSessionWorkingDirectoryParams = ChangeSessionWorkingDirectoryParams
  { changeDirectorySessionId :: !Text,
    changeDirectoryPath :: !Text,
    changeDirectoryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ChangeSessionWorkingDirectoryParams where
  show _ = "ChangeSessionWorkingDirectoryParams <redacted>"

instance FromJSON ChangeSessionWorkingDirectoryParams where
  parseJSON = withObject "ChangeSessionWorkingDirectoryParams" $ \fields -> ChangeSessionWorkingDirectoryParams <$> fields .: "sessionId" <*> fields .: "workingDirectory" <*> pure (additionalFields ["sessionId", "workingDirectory"] fields)

instance ToJSON ChangeSessionWorkingDirectoryParams where
  toJSON params = objectWithAdditionalFields ["sessionId", "workingDirectory"] (changeDirectoryAdditionalFields params) ["sessionId" .= changeDirectorySessionId params, "workingDirectory" .= changeDirectoryPath params]

-- | Session-scoped setup progress text, not a command or a completed setup.
data SetupStepProgress = SetupStepProgress
  { setupProgressSessionId :: !Text,
    setupProgressKind :: !SetupStepKind,
    setupProgressText :: !Text,
    setupProgressAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SetupStepProgress where
  show _ = "SetupStepProgress <redacted>"

instance FromJSON SetupStepProgress where
  parseJSON = withObject "SetupStepProgress" $ \fields -> SetupStepProgress <$> fields .: "sessionId" <*> fields .: "kind" <*> fields .: "text" <*> pure (additionalFields ["sessionId", "kind", "text"] fields)

instance ToJSON SetupStepProgress where
  toJSON event = objectWithAdditionalFields ["sessionId", "kind", "text"] (setupProgressAdditionalFields event) ["sessionId" .= setupProgressSessionId event, "kind" .= setupProgressKind event, "text" .= setupProgressText event]

trustKeys, readKeys, contentKeys, writeKeys, listParamsKeys, listResultKeys, searchParamsKeys, pushKeys, pullKeys :: [Key]
trustKeys = ["isTrusted", "trustRootPath", "promptRequired"]
readKeys = ["sessionId", "filePath", "metadataOnly", "encoding"]
contentKeys = ["content", "byteLength", "encoding", "mimeType", "isBinary", "fingerprint"]
writeKeys = ["sessionId", "filePath", "content", "baseFingerprint"]
listParamsKeys = ["sessionId", "path", "showHidden"]
listResultKeys = ["files", "directories", "totalFiles", "completeDepth", "truncated"]
searchParamsKeys = ["sessionId", "query", "maxResults", "showHidden"]
pushKeys = ["sessionId", "filePath", "presignedPutUrl", "contentType"]
pullKeys = ["sessionId", "presignedGetUrl", "destPath", "expectedContentLength"]
