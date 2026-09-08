{-# LANGUAGE OverloadedStrings #-}

-- | Local session-control parameter/result bodies for Factory protocol 1.205.0.
-- These codecs do not send RPCs, change files, replace sessions or upload logs.
module Factory.Droid.Schema.Control
  ( OutputFormat (..),
    UserMessageContent (..),
    AddUserMessageParams (..),
    UserOnlyMessage,
    mkUserOnlyMessage,
    userOnlyMessageValue,
    AppendMessagesParams (..),
    QueuePlacement (..),
    QueueResolution (..),
    ResolveQueuedMessageParams (..),
    RewindFileSnapshot (..),
    RewindFileCreation (..),
    RewindEvictedFile (..),
    GetRewindInfoParams (..),
    GetRewindInfoResult (..),
    ExecuteRewindParams (..),
    ExecuteRewindResult (..),
    CompactSessionParams (..),
    CompactSessionResult (..),
    ForkSessionTag (..),
    ForkSessionParams (..),
    ForkSessionResult (..),
    RenameSessionParams (..),
    ChangeWorkingDirectoryParams (..),
    ChangeWorkingDirectoryResult (..),
    ValidateWorkingDirectoryResult (..),
    CloseSessionReason (..),
    CloseSessionParams (..),
    KillWorkerSessionParams (..),
    SubmitBugReportParams (..),
    SubmitBugReportResult (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    Options,
    ToJSON (..),
    Value (Object, String),
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
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, enumOptions, objectWithAdditionalFields, optionalField, requireLiteral)
import Factory.Droid.Schema.Content (Base64ImageSource, DocumentSource, ImageBlock, TextBlock)
import Factory.Droid.Schema.Enums (MessageRole, MessageVisibility (VisibilityUserOnly), SessionOrigin)
import Factory.Droid.Schema.Messages (FactoryDroidMessage, Message (messageVisibility))
import Factory.Droid.Schema.Sources (BugReportSource)
import GHC.Generics (Generic)

-- | Raw structured-output configuration. The schema member must be an object;
-- decoding does not establish that it is a valid JSON Schema document.
data OutputFormat = OutputFormat
  { outputFormatSchema :: !Object,
    outputFormatAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON OutputFormat where
  parseJSON = withObject "OutputFormat" $ \fields -> do
    requireLiteral "type" "json_schema" fields
    OutputFormat <$> fields .: "schema" <*> pure (additionalFields ["type", "schema"] fields)

instance ToJSON OutputFormat where
  toJSON format = objectWithAdditionalFields ["type", "schema"] (outputFormatAdditionalFields format) ["type" .= String "json_schema", "schema" .= outputFormatSchema format]

-- | The text/image-only union accepted in user-message content. Nested
-- blocks share the ordinary content codecs and preserve their extensions.
data UserMessageContent = UserMessageText !TextBlock | UserMessageImage !ImageBlock
  deriving stock (Eq, Show)

instance FromJSON UserMessageContent where
  parseJSON = withObject "UserMessageContent" $ \fields -> do
    kind <- fields .: "type" :: Parser Text
    case kind of
      "text" -> UserMessageText <$> parseJSON (Object fields)
      "image" -> UserMessageImage <$> parseJSON (Object fields)
      _ -> fail "User message content must be text or image"

instance ToJSON UserMessageContent where
  toJSON (UserMessageText block) = toJSON block
  toJSON (UserMessageImage block) = toJSON block

-- | User-message parameters. Text is required, possibly empty; optional
-- content must be nonempty. Attachment data/paths are preserved without
-- base64 decoding, file access, ordering changes or inferred precedence.
data AddUserMessageParams = AddUserMessageParams
  { userMessageText :: !Text,
    userMessageId :: !(Maybe Text),
    userMessageContent :: !(Maybe (NonEmpty UserMessageContent)),
    userMessageImages :: !(Maybe [Base64ImageSource]),
    userMessageImagePaths :: !(Maybe [Text]),
    userMessageFiles :: !(Maybe [DocumentSource]),
    userMessageOutputFormat :: !(Maybe OutputFormat),
    userMessageSkipAgentLoop :: !(Maybe Bool),
    userMessageQueuePlacement :: !(Maybe QueuePlacement),
    userMessageRole :: !(Maybe MessageRole),
    userMessageVisibility :: !(Maybe MessageVisibility),
    userMessageSource :: !(Maybe SessionOrigin),
    userMessageAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AddUserMessageParams where
  parseJSON = withObject "AddUserMessageParams" $ \fields ->
    AddUserMessageParams
      <$> fields .: "text"
      <*> fields .:! "messageId"
      <*> fields .:! "content"
      <*> fields .:! "images"
      <*> fields .:! "imagePaths"
      <*> fields .:! "files"
      <*> fields .:! "outputFormat"
      <*> fields .:! "skipAgentLoop"
      <*> fields .:! "queuePlacement"
      <*> fields .:! "role"
      <*> fields .:! "visibility"
      <*> fields .:! "userMessageSource"
      <*> pure (additionalFields userMessageKeys fields)

instance ToJSON AddUserMessageParams where
  toJSON params =
    objectWithAdditionalFields userMessageKeys (userMessageAdditionalFields params) $
      ["text" .= userMessageText params]
        <> optionalField "messageId" (userMessageId params)
        <> optionalField "content" (userMessageContent params)
        <> optionalField "images" (userMessageImages params)
        <> optionalField "imagePaths" (userMessageImagePaths params)
        <> optionalField "files" (userMessageFiles params)
        <> optionalField "outputFormat" (userMessageOutputFormat params)
        <> optionalField "skipAgentLoop" (userMessageSkipAgentLoop params)
        <> optionalField "queuePlacement" (userMessageQueuePlacement params)
        <> optionalField "role" (userMessageRole params)
        <> optionalField "visibility" (userMessageVisibility params)
        <> optionalField "userMessageSource" (userMessageSource params)

-- | A complete message whose explicit visibility is user_only. The private
-- constructor prevents other visibility states from entering append payloads.
newtype UserOnlyMessage = UserOnlyMessage FactoryDroidMessage
  deriving stock (Eq, Show)

-- | Validate visibility without rewriting the message or its content.
mkUserOnlyMessage :: FactoryDroidMessage -> Maybe UserOnlyMessage
mkUserOnlyMessage message
  | messageVisibility message == Just VisibilityUserOnly = Just (UserOnlyMessage message)
  | otherwise = Nothing

-- | Recover the original immutable message value.
userOnlyMessageValue :: UserOnlyMessage -> FactoryDroidMessage
userOnlyMessageValue (UserOnlyMessage message) = message

instance FromJSON UserOnlyMessage where
  parseJSON value = do
    message <- parseJSON value
    maybe (fail "Appended messages require user_only visibility") pure (mkUserOnlyMessage message)

instance ToJSON UserOnlyMessage where
  toJSON = toJSON . userOnlyMessageValue

-- | Append requires at least one complete, explicitly user-only message.
data AppendMessagesParams = AppendMessagesParams
  { appendMessages :: !(NonEmpty UserOnlyMessage),
    appendAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AppendMessagesParams where
  parseJSON = withObject "AppendMessagesParams" $ \fields ->
    AppendMessagesParams <$> fields .: "messages" <*> pure (additionalFields ["messages"] fields)

instance ToJSON AppendMessagesParams where
  toJSON params = objectWithAdditionalFields ["messages"] (appendAdditionalFields params) ["messages" .= appendMessages params]

-- | The declared queue-placement choices.
data QueuePlacement = QueueEndOfTurn | QueueEndOfLoop
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON QueuePlacement where
  parseJSON = genericParseJSON queueOptions

instance ToJSON QueuePlacement where
  toJSON = genericToJSON queueOptions
  toEncoding = genericToEncoding queueOptions

-- | Update placement or delete a queued message. Delete has no typed placement.
data QueueResolution = UpdateQueuedMessage !QueuePlacement | DeleteQueuedMessage
  deriving stock (Eq, Ord, Show)

-- | Queue resolution with variant-specific extension handling.
data ResolveQueuedMessageParams = ResolveQueuedMessageParams
  { queueRequestId :: !Text,
    queueResolution :: !QueueResolution,
    queueAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ResolveQueuedMessageParams where
  parseJSON = withObject "ResolveQueuedMessageParams" $ \fields -> do
    action <- fields .: "action" :: Parser Text
    resolution <- case action of
      "update_queue" -> UpdateQueuedMessage <$> fields .: "queuePlacement"
      "delete" -> pure DeleteQueuedMessage
      _ -> fail "Unknown queued-message action"
    ResolveQueuedMessageParams <$> fields .: "requestId" <*> pure resolution <*> pure (additionalFields (queueKeys resolution) fields)

instance ToJSON ResolveQueuedMessageParams where
  toJSON params =
    objectWithAdditionalFields (queueKeys (queueResolution params)) (queueAdditionalFields params) $
      ["requestId" .= queueRequestId params] <> case queueResolution params of
        UpdateQueuedMessage placement -> ["action" .= String "update_queue", "queuePlacement" .= placement]
        DeleteQueuedMessage -> ["action" .= String "delete"]

-- | Rewind snapshot metadata. Size is the schema's number type, not a new
-- nonnegative-integer constraint; no content is fetched by decoding.
data RewindFileSnapshot = RewindFileSnapshot
  { rewindFilePath :: !Text,
    rewindContentHash :: !Text,
    rewindFileSize :: !Scientific,
    rewindSnapshotAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON RewindFileSnapshot where
  parseJSON = withObject "RewindFileSnapshot" $ \fields ->
    RewindFileSnapshot <$> fields .: "filePath" <*> fields .: "contentHash" <*> fields .: "size" <*> pure (additionalFields snapshotKeys fields)

instance ToJSON RewindFileSnapshot where
  toJSON file = objectWithAdditionalFields snapshotKeys (rewindSnapshotAdditionalFields file) ["filePath" .= rewindFilePath file, "contentHash" .= rewindContentHash file, "size" .= rewindFileSize file]

-- | A file created after a rewind boundary.
data RewindFileCreation = RewindFileCreation
  { rewindCreatedFilePath :: !Text,
    rewindCreationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON RewindFileCreation where
  parseJSON = withObject "RewindFileCreation" $ \fields ->
    RewindFileCreation <$> fields .: "filePath" <*> pure (additionalFields ["filePath"] fields)

instance ToJSON RewindFileCreation where
  toJSON file = objectWithAdditionalFields ["filePath"] (rewindCreationAdditionalFields file) ["filePath" .= rewindCreatedFilePath file]

-- | A snapshot that is unavailable and its reported reason.
data RewindEvictedFile = RewindEvictedFile
  { rewindEvictedFilePath :: !Text,
    rewindEvictionReason :: !Text,
    rewindEvictedAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON RewindEvictedFile where
  parseJSON = withObject "RewindEvictedFile" $ \fields ->
    RewindEvictedFile <$> fields .: "filePath" <*> fields .: "reason" <*> pure (additionalFields ["filePath", "reason"] fields)

instance ToJSON RewindEvictedFile where
  toJSON file = objectWithAdditionalFields ["filePath", "reason"] (rewindEvictedAdditionalFields file) ["filePath" .= rewindEvictedFilePath file, "reason" .= rewindEvictionReason file]

-- | Identify the session and message whose rewind information is requested.
data GetRewindInfoParams = GetRewindInfoParams
  { rewindInfoSessionId :: !Text,
    rewindInfoMessageId :: !Text,
    rewindInfoParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GetRewindInfoParams where
  parseJSON = withObject "GetRewindInfoParams" $ \fields ->
    GetRewindInfoParams <$> fields .: "sessionId" <*> fields .: "messageId" <*> pure (additionalFields ["sessionId", "messageId"] fields)

instance ToJSON GetRewindInfoParams where
  toJSON params = objectWithAdditionalFields ["sessionId", "messageId"] (rewindInfoParamsAdditionalFields params) ["sessionId" .= rewindInfoSessionId params, "messageId" .= rewindInfoMessageId params]

-- | The three required rewind lists may each be empty.
data GetRewindInfoResult = GetRewindInfoResult
  { rewindAvailableFiles :: ![RewindFileSnapshot],
    rewindCreatedFiles :: ![RewindFileCreation],
    rewindEvictedFiles :: ![RewindEvictedFile],
    rewindInfoAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GetRewindInfoResult where
  parseJSON = withObject "GetRewindInfoResult" $ \fields ->
    GetRewindInfoResult <$> fields .: "availableFiles" <*> fields .: "createdFiles" <*> fields .: "evictedFiles" <*> pure (additionalFields rewindInfoKeys fields)

instance ToJSON GetRewindInfoResult where
  toJSON result = objectWithAdditionalFields rewindInfoKeys (rewindInfoAdditionalFields result) ["availableFiles" .= rewindAvailableFiles result, "createdFiles" .= rewindCreatedFiles result, "evictedFiles" .= rewindEvictedFiles result]

-- | A rewind request body. Restoration/deletion lists are required but may
-- be empty. Encoding does not touch files or assume replacement ownership.
data ExecuteRewindParams = ExecuteRewindParams
  { executeRewindSessionId :: !Text,
    executeRewindMessageId :: !Text,
    executeRewindRestore :: ![RewindFileSnapshot],
    executeRewindDelete :: ![RewindFileCreation],
    executeRewindTitle :: !Text,
    executeRewindAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ExecuteRewindParams where
  parseJSON = withObject "ExecuteRewindParams" $ \fields ->
    ExecuteRewindParams <$> fields .: "sessionId" <*> fields .: "messageId" <*> fields .: "filesToRestore" <*> fields .: "filesToDelete" <*> fields .: "forkTitle" <*> pure (additionalFields executeRewindKeys fields)

instance ToJSON ExecuteRewindParams where
  toJSON params = objectWithAdditionalFields executeRewindKeys (executeRewindAdditionalFields params) ["sessionId" .= executeRewindSessionId params, "messageId" .= executeRewindMessageId params, "filesToRestore" .= executeRewindRestore params, "filesToDelete" .= executeRewindDelete params, "forkTitle" .= executeRewindTitle params]

-- | Rewind result data; the codec does not open or retire session handles.
data ExecuteRewindResult = ExecuteRewindResult
  { rewindNewSessionId :: !Text,
    rewindRestoredCount :: !Scientific,
    rewindDeletedCount :: !Scientific,
    rewindFailedRestoreCount :: !Scientific,
    rewindFailedDeleteCount :: !Scientific,
    rewindResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ExecuteRewindResult where
  parseJSON = withObject "ExecuteRewindResult" $ \fields ->
    ExecuteRewindResult <$> fields .: "newSessionId" <*> fields .: "restoredCount" <*> fields .: "deletedCount" <*> fields .: "failedRestoreCount" <*> fields .: "failedDeleteCount" <*> pure (additionalFields rewindResultKeys fields)

instance ToJSON ExecuteRewindResult where
  toJSON result = objectWithAdditionalFields rewindResultKeys (rewindResultAdditionalFields result) ["newSessionId" .= rewindNewSessionId result, "restoredCount" .= rewindRestoredCount result, "deletedCount" .= rewindDeletedCount result, "failedRestoreCount" .= rewindFailedRestoreCount result, "failedDeleteCount" .= rewindFailedDeleteCount result]

-- | Optional custom compaction instructions, without normalization.
data CompactSessionParams = CompactSessionParams
  { compactionInstructions :: !(Maybe Text),
    compactionParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CompactSessionParams where
  parseJSON = withObject "CompactSessionParams" $ \fields ->
    CompactSessionParams <$> fields .:! "customInstructions" <*> pure (additionalFields ["customInstructions"] fields)

instance ToJSON CompactSessionParams where
  toJSON params = objectWithAdditionalFields ["customInstructions"] (compactionParamsAdditionalFields params) (optionalField "customInstructions" (compactionInstructions params))

-- | Compaction result data, with the schema's unconstrained numeric count.
data CompactSessionResult = CompactSessionResult
  { compactionNewSessionId :: !Text,
    compactionRemovedCount :: !Scientific,
    compactionResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CompactSessionResult where
  parseJSON = withObject "CompactSessionResult" $ \fields ->
    CompactSessionResult <$> fields .: "newSessionId" <*> fields .: "removedCount" <*> pure (additionalFields ["newSessionId", "removedCount"] fields)

instance ToJSON CompactSessionResult where
  toJSON result = objectWithAdditionalFields ["newSessionId", "removedCount"] (compactionResultAdditionalFields result) ["newSessionId" .= compactionNewSessionId result, "removedCount" .= compactionRemovedCount result]

-- | Fork tags use a plain string name in their inline schema, unlike the
-- nonempty SessionTagSchema. Metadata is optional and string-valued.
data ForkSessionTag = ForkSessionTag
  { forkTagName :: !Text,
    forkTagMetadata :: !(Maybe (KeyMap Text)),
    forkTagAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ForkSessionTag where
  parseJSON = withObject "ForkSessionTag" $ \fields ->
    ForkSessionTag <$> fields .: "name" <*> fields .:! "metadata" <*> pure (additionalFields ["name", "metadata"] fields)

instance ToJSON ForkSessionTag where
  toJSON tag = objectWithAdditionalFields ["name", "metadata"] (forkTagAdditionalFields tag) (["name" .= forkTagName tag] <> optionalField "metadata" (forkTagMetadata tag))

-- | Optional fork title and tags. Missing and empty tag arrays remain distinct.
data ForkSessionParams = ForkSessionParams
  { forkSessionTitle :: !(Maybe Text),
    forkSessionTags :: !(Maybe [ForkSessionTag]),
    forkSessionAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ForkSessionParams where
  parseJSON = withObject "ForkSessionParams" $ \fields ->
    ForkSessionParams <$> fields .:! "title" <*> fields .:! "tags" <*> pure (additionalFields ["title", "tags"] fields)

instance ToJSON ForkSessionParams where
  toJSON params = objectWithAdditionalFields ["title", "tags"] (forkSessionAdditionalFields params) (optionalField "title" (forkSessionTitle params) <> optionalField "tags" (forkSessionTags params))

-- | A successor identifier, without any client-side ownership transition.
data ForkSessionResult = ForkSessionResult
  { forkedSessionId :: !Text,
    forkResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ForkSessionResult where
  parseJSON = withObject "ForkSessionResult" $ \fields ->
    ForkSessionResult <$> fields .: "newSessionId" <*> pure (additionalFields ["newSessionId"] fields)

instance ToJSON ForkSessionResult where
  toJSON result = objectWithAdditionalFields ["newSessionId"] (forkResultAdditionalFields result) ["newSessionId" .= forkedSessionId result]

-- | A required session title; the schema permits an empty string.
data RenameSessionParams = RenameSessionParams
  { renameTitle :: !Text,
    renameAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON RenameSessionParams where
  parseJSON = withObject "RenameSessionParams" $ \fields ->
    RenameSessionParams <$> fields .: "title" <*> pure (additionalFields ["title"] fields)

instance ToJSON RenameSessionParams where
  toJSON params = objectWithAdditionalFields ["title"] (renameAdditionalFields params) ["title" .= renameTitle params]

-- | A working-directory request body; no filesystem validation occurs here.
data ChangeWorkingDirectoryParams = ChangeWorkingDirectoryParams
  { requestedWorkingDirectory :: !Text,
    workingDirectoryParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ChangeWorkingDirectoryParams where
  parseJSON = withObject "ChangeWorkingDirectoryParams" $ \fields ->
    ChangeWorkingDirectoryParams <$> fields .: "workingDirectory" <*> pure (additionalFields ["workingDirectory"] fields)

instance ToJSON ChangeWorkingDirectoryParams where
  toJSON params = objectWithAdditionalFields ["workingDirectory"] (workingDirectoryParamsAdditionalFields params) ["workingDirectory" .= requestedWorkingDirectory params]

-- | The peer's resolved path, without changing the SDK process directory.
data ChangeWorkingDirectoryResult = ChangeWorkingDirectoryResult
  { changedResolvedPath :: !Text,
    changedDirectoryAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ChangeWorkingDirectoryResult where
  parseJSON = withObject "ChangeWorkingDirectoryResult" $ \fields ->
    ChangeWorkingDirectoryResult <$> fields .: "resolvedPath" <*> pure (additionalFields ["resolvedPath"] fields)

instance ToJSON ChangeWorkingDirectoryResult where
  toJSON result = objectWithAdditionalFields ["resolvedPath"] (changedDirectoryAdditionalFields result) ["resolvedPath" .= changedResolvedPath result]

-- | Reported directory validity; the schema does not condition optional path
-- or error fields on the Boolean flag.
data ValidateWorkingDirectoryResult = ValidateWorkingDirectoryResult
  { directoryIsValid :: !Bool,
    directoryResolvedPath :: !(Maybe Text),
    directoryValidationError :: !(Maybe Text),
    directoryValidationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ValidateWorkingDirectoryResult where
  parseJSON = withObject "ValidateWorkingDirectoryResult" $ \fields ->
    ValidateWorkingDirectoryResult <$> fields .: "isValid" <*> fields .:! "resolvedPath" <*> fields .:! "error" <*> pure (additionalFields ["isValid", "resolvedPath", "error"] fields)

instance ToJSON ValidateWorkingDirectoryResult where
  toJSON result = objectWithAdditionalFields ["isValid", "resolvedPath", "error"] (directoryValidationAdditionalFields result) (["isValid" .= directoryIsValid result] <> optionalField "resolvedPath" (directoryResolvedPath result) <> optionalField "error" (directoryValidationError result))

-- | The declared session-close reason values.
data CloseSessionReason = CloseClear | CloseLogout | ClosePromptInputExit | CloseOther
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON CloseSessionReason where
  parseJSON = genericParseJSON closeOptions

instance ToJSON CloseSessionReason where
  toJSON = genericToJSON closeOptions
  toEncoding = genericToEncoding closeOptions

-- | An optional close reason; omission is not changed to other.
data CloseSessionParams = CloseSessionParams
  { closeSessionReason :: !(Maybe CloseSessionReason),
    closeAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CloseSessionParams where
  parseJSON = withObject "CloseSessionParams" $ \fields ->
    CloseSessionParams <$> fields .:! "reason" <*> pure (additionalFields ["reason"] fields)

instance ToJSON CloseSessionParams where
  toJSON params = objectWithAdditionalFields ["reason"] (closeAdditionalFields params) (optionalField "reason" (closeSessionReason params))

-- | A worker identifier, without terminating any process or session.
data KillWorkerSessionParams = KillWorkerSessionParams
  { killedWorkerSessionId :: !Text,
    killWorkerAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON KillWorkerSessionParams where
  parseJSON = withObject "KillWorkerSessionParams" $ \fields ->
    KillWorkerSessionParams <$> fields .: "workerSessionId" <*> pure (additionalFields ["workerSessionId"] fields)

instance ToJSON KillWorkerSessionParams where
  toJSON params = objectWithAdditionalFields ["workerSessionId"] (killWorkerAdditionalFields params) ["workerSessionId" .= killedWorkerSessionId params]

-- | Caller-provided report content. Serialization does not upload logs or
-- establish that arbitrary log text is safe to disclose.
data SubmitBugReportParams = SubmitBugReportParams
  { bugReportUserComment :: !Text,
    bugReportClientLogs :: !(Maybe Text),
    bugReportSource :: !(Maybe BugReportSource),
    bugReportParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SubmitBugReportParams where
  parseJSON = withObject "SubmitBugReportParams" $ \fields ->
    SubmitBugReportParams <$> fields .: "userComment" <*> fields .:! "clientLogs" <*> fields .:! "source" <*> pure (additionalFields ["userComment", "clientLogs", "source"] fields)

instance ToJSON SubmitBugReportParams where
  toJSON params = objectWithAdditionalFields ["userComment", "clientLogs", "source"] (bugReportParamsAdditionalFields params) (["userComment" .= bugReportUserComment params] <> optionalField "clientLogs" (bugReportClientLogs params) <> optionalField "source" (bugReportSource params))

-- | The reported bug-report identifier.
data SubmitBugReportResult = SubmitBugReportResult
  { submittedBugReportId :: !Text,
    submittedReportAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SubmitBugReportResult where
  parseJSON = withObject "SubmitBugReportResult" $ \fields ->
    SubmitBugReportResult <$> fields .: "bugReportId" <*> pure (additionalFields ["bugReportId"] fields)

instance ToJSON SubmitBugReportResult where
  toJSON result = objectWithAdditionalFields ["bugReportId"] (submittedReportAdditionalFields result) ["bugReportId" .= submittedBugReportId result]

queueOptions, closeOptions :: Options
queueOptions = enumOptions "Queue" (camelTo2 '_')
closeOptions = enumOptions "Close" (camelTo2 '_')

queueKeys :: QueueResolution -> [Key]
queueKeys (UpdateQueuedMessage _) = ["requestId", "action", "queuePlacement"]
queueKeys DeleteQueuedMessage = ["requestId", "action"]

userMessageKeys :: [Key]
userMessageKeys = ["text", "messageId", "content", "images", "imagePaths", "files", "outputFormat", "skipAgentLoop", "queuePlacement", "role", "visibility", "userMessageSource"]

snapshotKeys, rewindInfoKeys, executeRewindKeys, rewindResultKeys :: [Key]
snapshotKeys = ["filePath", "contentHash", "size"]
rewindInfoKeys = ["availableFiles", "createdFiles", "evictedFiles"]
executeRewindKeys = ["sessionId", "messageId", "filesToRestore", "filesToDelete", "forkTitle"]
rewindResultKeys = ["newSessionId", "restoredCount", "deletedCount", "failedRestoreCount", "failedDeleteCount"]
