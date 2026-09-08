{-# LANGUAGE OverloadedStrings #-}

-- | Session snapshots, identity, tags and worktree metadata for protocol 1.205.0.
-- These codecs do not create sessions, inspect worktrees or add SDK attribution.
module Factory.Droid.Schema.Session
  ( SessionIdParams (..),
    SessionTagName,
    mkSessionTagName,
    sessionTagNameText,
    SessionTag (..),
    SessionWorktreeMetadata (..),
    SessionSnapshot (..),
    SandboxStatus (..),
    WorktreeGitRef,
    mkWorktreeGitRef,
    worktreeGitRefText,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    withObject,
    withText,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Char (isControl, isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON
  ( additionalFields,
    objectWithAdditionalFields,
    optionalField,
  )
import Factory.Droid.Schema.Enums (SandboxMode, WorktreeLifecycle)
import Factory.Droid.Schema.Messages (FactoryDroidMessage)

-- | The common session-ID parameter object. The schema imposes no UUID or
-- nonempty-string constraint on the identifier.
data SessionIdParams = SessionIdParams
  { sessionParamsId :: !Text,
    sessionParamsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionIdParams where
  parseJSON = withObject "SessionIdParams" $ \fields ->
    SessionIdParams <$> fields .: "sessionId" <*> pure (additionalFields ["sessionId"] fields)

instance ToJSON SessionIdParams where
  toJSON params = objectWithAdditionalFields ["sessionId"] (sessionParamsAdditionalFields params) ["sessionId" .= sessionParamsId params]

-- | A nonempty tag name. Whitespace is significant and is not trimmed.
newtype SessionTagName = SessionTagName Text
  deriving stock (Eq, Ord, Show)

-- | Reject only the empty string, as required by SessionTagSchema.
mkSessionTagName :: Text -> Maybe SessionTagName
mkSessionTagName name
  | Text.null name = Nothing
  | otherwise = Just (SessionTagName name)

-- | Recover the original tag name without normalization.
sessionTagNameText :: SessionTagName -> Text
sessionTagNameText (SessionTagName name) = name

instance FromJSON SessionTagName where
  parseJSON = withText "SessionTagName" $ \name ->
    maybe (fail "Session tag name must not be empty") pure (mkSessionTagName name)

instance ToJSON SessionTagName where
  toJSON = toJSON . sessionTagNameText

-- | A tag with optional string-valued metadata. Missing metadata differs
-- from an empty metadata object; explicit null is invalid.
data SessionTag = SessionTag
  { sessionTagName :: !SessionTagName,
    sessionTagMetadata :: !(Maybe (KeyMap Text)),
    sessionTagAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionTag where
  parseJSON = withObject "SessionTag" $ \fields ->
    SessionTag <$> fields .: "name" <*> fields .:! "metadata" <*> pure (additionalFields tagKeys fields)

instance ToJSON SessionTag where
  toJSON tag =
    objectWithAdditionalFields tagKeys (sessionTagAdditionalFields tag) $
      ["name" .= sessionTagName tag] <> optionalField "metadata" (sessionTagMetadata tag)

-- | Worktree metadata. Paths and removedAt remain strings as specified by
-- the schema; decoding does not validate filesystem state or parse a date.
data SessionWorktreeMetadata = SessionWorktreeMetadata
  { worktreeRepoRoot :: !Text,
    worktreeBranch :: !(Maybe Text),
    worktreeLifecycle :: !(Maybe WorktreeLifecycle),
    worktreeParentPath :: !(Maybe Text),
    worktreePath :: !(Maybe Text),
    worktreeRemovedAt :: !(Maybe Text),
    worktreeSetupProfileId :: !(Maybe Text),
    worktreeAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionWorktreeMetadata where
  parseJSON = withObject "SessionWorktreeMetadata" $ \fields ->
    SessionWorktreeMetadata
      <$> fields .: "repoRoot"
      <*> fields .:! "branch"
      <*> fields .:! "lifecycle"
      <*> fields .:! "parentWorktreePath"
      <*> fields .:! "path"
      <*> fields .:! "removedAt"
      <*> fields .:! "setupProfileId"
      <*> pure (additionalFields worktreeKeys fields)

instance ToJSON SessionWorktreeMetadata where
  toJSON worktree =
    objectWithAdditionalFields worktreeKeys (worktreeAdditionalFields worktree) $
      ["repoRoot" .= worktreeRepoRoot worktree]
        <> optionalField "branch" (worktreeBranch worktree)
        <> optionalField "lifecycle" (worktreeLifecycle worktree)
        <> optionalField "parentWorktreePath" (worktreeParentPath worktree)
        <> optionalField "path" (worktreePath worktree)
        <> optionalField "removedAt" (worktreeRemovedAt worktree)
        <> optionalField "setupProfileId" (worktreeSetupProfileId worktree)

-- | Saved message/title data, not a live session handle. Messages retain
-- their supplied order and extensions; a title may be absent or empty.
data SessionSnapshot = SessionSnapshot
  { sessionMessages :: ![FactoryDroidMessage],
    sessionTitle :: !(Maybe Text),
    sessionSnapshotAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionSnapshot where
  parseJSON = withObject "SessionSnapshot" $ \fields ->
    SessionSnapshot <$> fields .: "messages" <*> fields .:! "title" <*> pure (additionalFields ["messages", "title"] fields)

instance ToJSON SessionSnapshot where
  toJSON snapshot = objectWithAdditionalFields ["messages", "title"] (sessionSnapshotAdditionalFields snapshot) (["messages" .= sessionMessages snapshot] <> optionalField "title" (sessionTitle snapshot))

-- | Reported sandbox status. The optional mode and enabled flag are
-- independent data; no sandbox or permission policy is applied by this codec.
data SandboxStatus = SandboxStatus
  { sandboxEnabled :: !Bool,
    sandboxMode :: !(Maybe SandboxMode),
    sandboxStatusAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SandboxStatus where
  parseJSON = withObject "SandboxStatus" $ \fields -> SandboxStatus <$> fields .: "enabled" <*> fields .:! "mode" <*> pure (additionalFields ["enabled", "mode"] fields)

instance ToJSON SandboxStatus where
  toJSON status = objectWithAdditionalFields ["enabled", "mode"] (sandboxStatusAdditionalFields status) (["enabled" .= sandboxEnabled status] <> optionalField "mode" (sandboxMode status))

-- | A reference satisfying the supplied WorktreeGitRefSchema, not the
-- stronger rules of git check-ref-format. The constructor is private.
newtype WorktreeGitRef = WorktreeGitRef Text
  deriving stock (Eq, Ord, Show)

-- | Validate 1–255 Unicode code points, no leading dash or double dot, and
-- the schema's excluded characters. ECMAScript whitespace adds the two line
-- separators and BOM to isSpace; controls include the Unicode Cc category.
mkWorktreeGitRef :: Text -> Maybe WorktreeGitRef
mkWorktreeGitRef value
  | Text.null value || Text.length value > 255 = Nothing
  | "-" `Text.isPrefixOf` value || ".." `Text.isInfixOf` value = Nothing
  | Text.any invalid value = Nothing
  | otherwise = Just (WorktreeGitRef value)
  where
    invalid c = isControl c || isSpace c || c `elem` ['\x2028', '\x2029', '\xFEFF'] || c `elem` ("~^:?*[\\" :: String)

-- | Recover the exact reference without trimming or normalization.
worktreeGitRefText :: WorktreeGitRef -> Text
worktreeGitRefText (WorktreeGitRef value) = value

instance FromJSON WorktreeGitRef where
  parseJSON = withText "WorktreeGitRef" $ \value -> maybe (fail "Invalid worktree Git reference") pure (mkWorktreeGitRef value)

instance ToJSON WorktreeGitRef where
  toJSON = toJSON . worktreeGitRefText

tagKeys, worktreeKeys :: [Key]
tagKeys = ["name", "metadata"]
worktreeKeys = ["repoRoot", "branch", "lifecycle", "parentWorktreePath", "path", "removedAt", "setupProfileId"]
