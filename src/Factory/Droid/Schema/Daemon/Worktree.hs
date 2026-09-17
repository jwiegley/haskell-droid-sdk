{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Worktree-management and setup-profile wire bodies for protocol 1.205.0.
-- Closed inputs reject unknown fields; open reports preserve them. No Git,
-- filesystem, script, profile or session mutation occurs during serialization.
module Factory.Droid.Schema.Daemon.Worktree
  ( WorktreeSetupScript,
    WorktreeProfileName,
    mkWorktreeProfileName,
    worktreeProfileNameText,
    WorktreeProfileSource (..),
    WorktreePreservedReason (..),
    WorktreeProfileContents (..),
    WorktreeProfile (..),
    ListWorktreeProfilesParams (..),
    SaveWorktreeProfileParams (..),
    defaultSaveWorktreeProfileParams,
    WorktreeProfileError (..),
    validateSaveWorktreeProfileParams,
    DeleteWorktreeProfileParams (..),
    ListWorktreeProfilesResult (..),
    SaveWorktreeProfileResult (..),
    parseWorktreeSetupProfilesResult,
    parseSavedWorktreeSetupProfileResult,
    ManagedWorktreeSession (..),
    ManagedWorktree (..),
    ListManagedWorktreesParams (..),
    ListManagedWorktreesResult (..),
    CleanupWorktreeParams (..),
    CleanupWorktreeResult (..),
    InspectWorktreeDeletionParams (..),
    WorktreePullRequestState (..),
    WorktreePullRequest (..),
    InspectWorktreeDeletionResult (..),
    SessionArchiveStateChanged (..),
    WorktreeBranchChanged (..),
    WorktreeRemoved (..),
  )
where

import Control.Exception (Exception)
import Control.Monad (forM, forM_, guard, unless, when)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON (additionalFields, isEcmaWhitespace, objectWithAdditionalFields, optionalField, rejectUnknownFields)
import Factory.Droid.Schema.Enums (WorktreeLifecycle)
import Factory.Droid.Schema.Primitives (BoundedText, NonEmptyText, Rfc3339Timestamp, UUIDText, boundedTextValue, mkBoundedText)
import Numeric.Natural (Natural)

-- | Script text with at most 100,000 Unicode code points, including empty text.
-- This is a length constraint, not executable-script validation.
type WorktreeSetupScript = BoundedText 100000

-- | A profile name with 1–100 code points. Whitespace is significant.
newtype WorktreeProfileName = WorktreeProfileName (BoundedText 100)
  deriving stock (Eq, Ord, Show)

-- | Validate the name without trimming or using it as a filesystem component.
mkWorktreeProfileName :: Text -> Maybe WorktreeProfileName
mkWorktreeProfileName value = do
  guard (not (Text.null value))
  WorktreeProfileName <$> mkBoundedText @100 value

-- | Recover the exact profile name.
worktreeProfileNameText :: WorktreeProfileName -> Text
worktreeProfileNameText (WorktreeProfileName value) = boundedTextValue value

instance FromJSON WorktreeProfileName where
  parseJSON = withText "WorktreeProfileName" $ \value -> maybe (fail "Expected a profile name of 1-100 code points") pure (mkWorktreeProfileName value)

instance ToJSON WorktreeProfileName where
  toJSON = toJSON . worktreeProfileNameText

-- | The reported source of a setup profile.
data WorktreeProfileSource = ProfileLocal | ProfileRepository
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON WorktreeProfileSource where
  parseJSON = withText "WorktreeProfileSource" $ \case
    "local" -> pure ProfileLocal
    "repository" -> pure ProfileRepository
    _ -> fail "Unknown worktree profile source"

instance ToJSON WorktreeProfileSource where
  toJSON ProfileLocal = String "local"
  toJSON ProfileRepository = String "repository"

-- | Why a cleanup report says a worktree was preserved.
data WorktreePreservedReason = PreservedUncommittedChanges | PreservedRemovalFailed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON WorktreePreservedReason where
  parseJSON = withText "WorktreePreservedReason" $ \case
    "uncommitted_changes" -> pure PreservedUncommittedChanges
    "removal_failed" -> pure PreservedRemovalFailed
    _ -> fail "Unknown worktree preservation reason"

instance ToJSON WorktreePreservedReason where
  toJSON PreservedUncommittedChanges = String "uncommitted_changes"
  toJSON PreservedRemovalFailed = String "removal_failed"

-- | Closed on-disk profile content. Every declared member is optional; an
-- empty object is valid and does not synthesize a name or script.
data WorktreeProfileContents = WorktreeProfileContents
  { profileContentName :: !(Maybe WorktreeProfileName),
    profileContentScript :: !(Maybe WorktreeSetupScript),
    profileContentCleanupScript :: !(Maybe WorktreeSetupScript),
    profileContentInitialPrompt :: !(Maybe (BoundedText 100000))
  }
  deriving stock (Eq)

instance Show WorktreeProfileContents where
  show _ = "WorktreeProfileContents <redacted>"

instance FromJSON WorktreeProfileContents where
  parseJSON = withObject "WorktreeProfileContents" $ \fields -> do
    rejectUnknownFields contentKeys fields
    WorktreeProfileContents <$> fields .:! "name" <*> fields .:! "script" <*> fields .:! "cleanupScript" <*> fields .:! "initialPrompt"

instance ToJSON WorktreeProfileContents where
  toJSON content = object (optionalField "name" (profileContentName content) <> optionalField "script" (profileContentScript content) <> optionalField "cleanupScript" (profileContentCleanupScript content) <> optionalField "initialPrompt" (profileContentInitialPrompt content))

-- | Open profile metadata. UUID and timestamp spelling are retained; the
-- timestamp validator has the documented grammar/calendar boundary.
data WorktreeProfile = WorktreeProfile
  { profileId :: !UUIDText,
    profileName :: !WorktreeProfileName,
    profileCreatedAt :: !Rfc3339Timestamp,
    profileUpdatedAt :: !Rfc3339Timestamp,
    profileScript :: !(Maybe WorktreeSetupScript),
    profileCleanupScript :: !(Maybe WorktreeSetupScript),
    profileInitialPrompt :: !(Maybe (BoundedText 100000)),
    profileSource :: !(Maybe WorktreeProfileSource),
    profileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WorktreeProfile where
  show _ = "WorktreeProfile <redacted>"

instance FromJSON WorktreeProfile where
  parseJSON = withObject "WorktreeProfile" $ \fields ->
    WorktreeProfile <$> fields .: "id" <*> fields .: "name" <*> fields .: "createdAt" <*> fields .: "updatedAt" <*> fields .:! "script" <*> fields .:! "cleanupScript" <*> fields .:! "initialPrompt" <*> fields .:! "source" <*> pure (additionalFields profileKeys fields)

instance ToJSON WorktreeProfile where
  toJSON profile =
    objectWithAdditionalFields profileKeys (profileAdditionalFields profile) $
      ["id" .= profileId profile, "name" .= profileName profile, "createdAt" .= profileCreatedAt profile, "updatedAt" .= profileUpdatedAt profile]
        <> optionalField "script" (profileScript profile)
        <> optionalField "cleanupScript" (profileCleanupScript profile)
        <> optionalField "initialPrompt" (profileInitialPrompt profile)
        <> optionalField "source" (profileSource profile)

-- | Closed profile-list parameters. The directory is not inspected here.
newtype ListWorktreeProfilesParams = ListWorktreeProfilesParams {profilesCwd :: Text}
  deriving stock (Eq)

instance Show ListWorktreeProfilesParams where
  show _ = "ListWorktreeProfilesParams <redacted>"

instance FromJSON ListWorktreeProfilesParams where
  parseJSON = withObject "ListWorktreeProfilesParams" $ \fields -> do
    rejectUnknownFields ["cwd"] fields
    ListWorktreeProfilesParams <$> fields .: "cwd"

instance ToJSON ListWorktreeProfilesParams where
  toJSON params = object ["cwd" .= profilesCwd params]

-- | A save request accepts raw text; the SDK operation validates and trims
-- name/content before admission. The cwd and optional UUID are not trimmed.
data SaveWorktreeProfileParams = SaveWorktreeProfileParams
  { saveProfileCwd :: !Text,
    saveProfileName :: !Text,
    saveProfileScript :: !(Maybe Text),
    saveProfileCleanupScript :: !(Maybe Text),
    saveProfileInitialPrompt :: !(Maybe Text),
    saveProfileId :: !(Maybe UUIDText)
  }
  deriving stock (Eq)

instance Show SaveWorktreeProfileParams where show _ = "SaveWorktreeProfileParams <redacted>"

-- | Set at least one nonempty content field before submitting this template.
defaultSaveWorktreeProfileParams :: Text -> Text -> SaveWorktreeProfileParams
defaultSaveWorktreeProfileParams cwd name = SaveWorktreeProfileParams cwd name Nothing Nothing Nothing Nothing

data WorktreeProfileError = InvalidWorktreeProfileName | WorktreeProfileContentTooLong | WorktreeProfileContentRequired
  deriving stock (Eq, Show)

instance Exception WorktreeProfileError

-- | Match SDK preparation without changing the raw protocol codecs' Unicode
-- code-point bounds: trim ECMAScript whitespace, then count UTF-16 code units.
validateSaveWorktreeProfileParams :: SaveWorktreeProfileParams -> Either WorktreeProfileError SaveWorktreeProfileParams
validateSaveWorktreeProfileParams params = do
  name <- normalizedProfileName (saveProfileName params)
  script <- traverse normalizedProfileContent (saveProfileScript params)
  cleanup <- traverse normalizedProfileContent (saveProfileCleanupScript params)
  prompt <- traverse normalizedProfileContent (saveProfileInitialPrompt params)
  when (all Text.null (catMaybes [script, cleanup, prompt])) (Left WorktreeProfileContentRequired)
  pure params {saveProfileName = name, saveProfileScript = script, saveProfileCleanupScript = cleanup, saveProfileInitialPrompt = prompt}

instance FromJSON SaveWorktreeProfileParams where
  parseJSON = withObject "SaveWorktreeProfileParams" $ \fields -> do
    rejectUnknownFields saveProfileKeys fields
    params <- SaveWorktreeProfileParams <$> fields .: "cwd" <*> fields .: "name" <*> fields .:! "script" <*> fields .:! "cleanupScript" <*> fields .:! "initialPrompt" <*> fields .:! "profileId"
    either (fail . show) pure (validateSaveWorktreeProfileParams params)

instance ToJSON SaveWorktreeProfileParams where
  toJSON params = object (["cwd" .= saveProfileCwd params, "name" .= saveProfileName params] <> optionalField "script" (saveProfileScript params) <> optionalField "cleanupScript" (saveProfileCleanupScript params) <> optionalField "initialPrompt" (saveProfileInitialPrompt params) <> optionalField "profileId" (saveProfileId params))

-- | Closed profile-delete parameters, without deleting any profile.
data DeleteWorktreeProfileParams = DeleteWorktreeProfileParams
  { deletedProfileCwd :: !Text,
    deletedProfileId :: !UUIDText
  }
  deriving stock (Eq)

instance Show DeleteWorktreeProfileParams where
  show _ = "DeleteWorktreeProfileParams <redacted>"

instance FromJSON DeleteWorktreeProfileParams where
  parseJSON = withObject "DeleteWorktreeProfileParams" $ \fields -> do
    rejectUnknownFields ["cwd", "profileId"] fields
    DeleteWorktreeProfileParams <$> fields .: "cwd" <*> fields .: "profileId"

instance ToJSON DeleteWorktreeProfileParams where
  toJSON params = object ["cwd" .= deletedProfileCwd params, "profileId" .= deletedProfileId params]

-- | Profile listing and optional last-used identity/root metadata.
data ListWorktreeProfilesResult = ListWorktreeProfilesResult
  { listedProfiles :: ![WorktreeProfile],
    profilesLastUsedId :: !(Maybe UUIDText),
    profilesRepoRoot :: !(Maybe Text),
    listedProfilesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListWorktreeProfilesResult where
  show _ = "ListWorktreeProfilesResult <redacted>"

instance FromJSON ListWorktreeProfilesResult where
  parseJSON = withObject "ListWorktreeProfilesResult" $ \fields -> ListWorktreeProfilesResult <$> fields .: "profiles" <*> fields .:! "lastUsedProfileId" <*> fields .:! "repoRoot" <*> pure (additionalFields ["profiles", "lastUsedProfileId", "repoRoot"] fields)

instance ToJSON ListWorktreeProfilesResult where
  toJSON result = objectWithAdditionalFields ["profiles", "lastUsedProfileId", "repoRoot"] (listedProfilesAdditionalFields result) (["profiles" .= listedProfiles result] <> optionalField "lastUsedProfileId" (profilesLastUsedId result) <> optionalField "repoRoot" (profilesRepoRoot result))

-- | The profile returned by a save operation, not a profile-writing function.
data SaveWorktreeProfileResult = SaveWorktreeProfileResult
  { savedWorktreeProfile :: !WorktreeProfile,
    savedProfileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SaveWorktreeProfileResult where
  show _ = "SaveWorktreeProfileResult <redacted>"

instance FromJSON SaveWorktreeProfileResult where
  parseJSON = withObject "SaveWorktreeProfileResult" $ \fields -> SaveWorktreeProfileResult <$> fields .: "profile" <*> pure (additionalFields ["profile"] fields)

instance ToJSON SaveWorktreeProfileResult where
  toJSON result = objectWithAdditionalFields ["profile"] (savedProfileAdditionalFields result) ["profile" .= savedWorktreeProfile result]

-- | SDK result preparation followed by the existing wire decoder. Unlike
-- TypeScript's stripping, native outer and nested extensions remain preserved.
parseWorktreeSetupProfilesResult :: Value -> Parser ListWorktreeProfilesResult
parseWorktreeSetupProfilesResult = withObject "ListWorktreeProfilesResult" $ \fields -> do
  original <- fields .: "profiles" :: Parser [Value]
  profiles <- traverse normalizeSdkProfile original
  parseJSON (Object (KeyMap.insert "profiles" (toJSON profiles) fields))

parseSavedWorktreeSetupProfileResult :: Value -> Parser SaveWorktreeProfileResult
parseSavedWorktreeSetupProfileResult = withObject "SaveWorktreeProfileResult" $ \fields -> do
  profile <- fields .: "profile" >>= normalizeSdkProfile
  parseJSON (Object (KeyMap.insert "profile" profile fields))

-- | Worktree-linked session metadata. Both ID and title are nonempty here;
-- updatedAt is a JSON number rather than a date-time string.
data ManagedWorktreeSession = ManagedWorktreeSession
  { managedSessionId :: !NonEmptyText,
    managedSessionTitle :: !NonEmptyText,
    managedSessionUpdatedAt :: !Scientific,
    managedSessionAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ManagedWorktreeSession where
  show _ = "ManagedWorktreeSession <redacted>"

instance FromJSON ManagedWorktreeSession where
  parseJSON = withObject "ManagedWorktreeSession" $ \fields -> ManagedWorktreeSession <$> fields .: "sessionId" <*> fields .: "title" <*> fields .: "updatedAt" <*> pure (additionalFields ["sessionId", "title", "updatedAt"] fields)

instance ToJSON ManagedWorktreeSession where
  toJSON session = objectWithAdditionalFields ["sessionId", "title", "updatedAt"] (managedSessionAdditionalFields session) ["sessionId" .= managedSessionId session, "title" .= managedSessionTitle session, "updatedAt" .= managedSessionUpdatedAt session]

-- | A managed-worktree report. Path/root strings are nonempty, not checked
-- filesystem locations; clean/size reports do not trigger inspections.
data ManagedWorktree = ManagedWorktree
  { managedWorktreePath :: !NonEmptyText,
    managedWorktreeRepoRoot :: !NonEmptyText,
    managedWorktreeLifecycle :: !WorktreeLifecycle,
    managedWorktreeSessions :: ![ManagedWorktreeSession],
    managedWorktreeBranch :: !(Maybe Text),
    managedWorktreeIsClean :: !(Maybe Bool),
    managedWorktreeSizeBytes :: !(Maybe Natural),
    managedWorktreeAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ManagedWorktree where
  show _ = "ManagedWorktree <redacted>"

instance FromJSON ManagedWorktree where
  parseJSON = withObject "ManagedWorktree" $ \fields -> ManagedWorktree <$> fields .: "path" <*> fields .: "repoRoot" <*> fields .: "lifecycle" <*> fields .: "sessions" <*> fields .:! "branch" <*> fields .:! "isClean" <*> fields .:! "sizeBytes" <*> pure (additionalFields managedKeys fields)

instance ToJSON ManagedWorktree where
  toJSON tree = objectWithAdditionalFields managedKeys (managedWorktreeAdditionalFields tree) (["path" .= managedWorktreePath tree, "repoRoot" .= managedWorktreeRepoRoot tree, "lifecycle" .= managedWorktreeLifecycle tree, "sessions" .= managedWorktreeSessions tree] <> optionalField "branch" (managedWorktreeBranch tree) <> optionalField "isClean" (managedWorktreeIsClean tree) <> optionalField "sizeBytes" (managedWorktreeSizeBytes tree))

-- | Closed listing options. Absence is not replaced by an includeSizes default.
newtype ListManagedWorktreesParams = ListManagedWorktreesParams {includeWorktreeSizes :: Maybe Bool}
  deriving stock (Eq)

instance Show ListManagedWorktreesParams where
  show _ = "ListManagedWorktreesParams <redacted>"

instance FromJSON ListManagedWorktreesParams where
  parseJSON = withObject "ListManagedWorktreesParams" $ \fields -> do
    rejectUnknownFields ["includeSizes"] fields
    ListManagedWorktreesParams <$> fields .:! "includeSizes"

instance ToJSON ListManagedWorktreesParams where
  toJSON params = object (optionalField "includeSizes" (includeWorktreeSizes params))

-- | Ordered worktree listing and independent pending flags.
data ListManagedWorktreesResult = ListManagedWorktreesResult
  { listedWorktrees :: ![ManagedWorktree],
    worktreeCleanlinessPending :: !(Maybe Bool),
    worktreeSizesPending :: !(Maybe Bool),
    listedWorktreesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListManagedWorktreesResult where
  show _ = "ListManagedWorktreesResult <redacted>"

instance FromJSON ListManagedWorktreesResult where
  parseJSON = withObject "ListManagedWorktreesResult" $ \fields -> ListManagedWorktreesResult <$> fields .: "worktrees" <*> fields .:! "cleanlinessPending" <*> fields .:! "sizesPending" <*> pure (additionalFields ["worktrees", "cleanlinessPending", "sizesPending"] fields)

instance ToJSON ListManagedWorktreesResult where
  toJSON result = objectWithAdditionalFields ["worktrees", "cleanlinessPending", "sizesPending"] (listedWorktreesAdditionalFields result) (["worktrees" .= listedWorktrees result] <> optionalField "cleanlinessPending" (worktreeCleanlinessPending result) <> optionalField "sizesPending" (worktreeSizesPending result))

-- | Closed cleanup parameters. Flags are preserved without force escalation,
-- branch deletion or any other filesystem/Git action.
data CleanupWorktreeParams = CleanupWorktreeParams
  { cleanupWorktreePath :: !NonEmptyText,
    cleanupDeleteLocalBranch :: !(Maybe Bool),
    cleanupDeleteRemoteBranch :: !(Maybe Bool),
    cleanupForce :: !(Maybe Bool)
  }
  deriving stock (Eq)

instance Show CleanupWorktreeParams where
  show _ = "CleanupWorktreeParams <redacted>"

instance FromJSON CleanupWorktreeParams where
  parseJSON = withObject "CleanupWorktreeParams" $ \fields -> do
    rejectUnknownFields cleanupKeys fields
    CleanupWorktreeParams <$> fields .: "worktreePath" <*> fields .:! "deleteLocalBranch" <*> fields .:! "deleteRemoteBranch" <*> fields .:! "force"

instance ToJSON CleanupWorktreeParams where
  toJSON params = object (["worktreePath" .= cleanupWorktreePath params] <> optionalField "deleteLocalBranch" (cleanupDeleteLocalBranch params) <> optionalField "deleteRemoteBranch" (cleanupDeleteRemoteBranch params) <> optionalField "force" (cleanupForce params))

-- | A cleanup report. The schema does not correlate flags, preservation reason
-- and warning text, and decoding does not archive sessions or remove branches.
data CleanupWorktreeResult = CleanupWorktreeResult
  { cleanupReportedPath :: !Text,
    cleanupArchivedSessionIds :: ![Text],
    cleanupWorktreeRemoved :: !Bool,
    cleanupLocalBranchDeleted :: !Bool,
    cleanupRemoteBranchDeleted :: !Bool,
    cleanupWarnings :: ![Text],
    cleanupBranch :: !(Maybe Text),
    cleanupPreservedReason :: !(Maybe WorktreePreservedReason),
    cleanupResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CleanupWorktreeResult where
  show _ = "CleanupWorktreeResult <redacted>"

instance FromJSON CleanupWorktreeResult where
  parseJSON = withObject "CleanupWorktreeResult" $ \fields -> CleanupWorktreeResult <$> fields .: "worktreePath" <*> fields .: "archivedSessionIds" <*> fields .: "worktreeRemoved" <*> fields .: "localBranchDeleted" <*> fields .: "remoteBranchDeleted" <*> fields .: "warnings" <*> fields .:! "branch" <*> fields .:! "preservedReason" <*> pure (additionalFields cleanupResultKeys fields)

instance ToJSON CleanupWorktreeResult where
  toJSON result = objectWithAdditionalFields cleanupResultKeys (cleanupResultAdditionalFields result) (["worktreePath" .= cleanupReportedPath result, "archivedSessionIds" .= cleanupArchivedSessionIds result, "worktreeRemoved" .= cleanupWorktreeRemoved result, "localBranchDeleted" .= cleanupLocalBranchDeleted result, "remoteBranchDeleted" .= cleanupRemoteBranchDeleted result, "warnings" .= cleanupWarnings result] <> optionalField "branch" (cleanupBranch result) <> optionalField "preservedReason" (cleanupPreservedReason result))

-- | Closed inspection parameters. Nonempty does not establish an absolute or
-- existing path; this codec performs no filesystem or Git inspection.
newtype InspectWorktreeDeletionParams = InspectWorktreeDeletionParams {inspectWorktreePath :: NonEmptyText}
  deriving stock (Eq)

instance Show InspectWorktreeDeletionParams where show _ = "InspectWorktreeDeletionParams <redacted>"

instance FromJSON InspectWorktreeDeletionParams where
  parseJSON = withObject "InspectWorktreeDeletionParams" $ \fields -> rejectUnknownFields ["worktreePath"] fields >> InspectWorktreeDeletionParams <$> fields .: "worktreePath"

instance ToJSON InspectWorktreeDeletionParams where
  toJSON params = object ["worktreePath" .= inspectWorktreePath params]

data WorktreePullRequestState = WorktreePullRequestOpen | WorktreePullRequestMerged | WorktreePullRequestClosed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON WorktreePullRequestState where
  parseJSON = withText "WorktreePullRequestState" $ \case "open" -> pure WorktreePullRequestOpen; "merged" -> pure WorktreePullRequestMerged; "closed" -> pure WorktreePullRequestClosed; _ -> fail "Unknown worktree pull-request state"

instance ToJSON WorktreePullRequestState where
  toJSON = String . \case WorktreePullRequestOpen -> "open"; WorktreePullRequestMerged -> "merged"; WorktreePullRequestClosed -> "closed"

data WorktreePullRequest = WorktreePullRequest
  { worktreePullRequestState :: !WorktreePullRequestState,
    worktreePullRequestUrl :: !(Maybe Text),
    worktreePullRequestTitle :: !(Maybe Text),
    worktreePullRequestAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WorktreePullRequest where show _ = "WorktreePullRequest <redacted>"

instance FromJSON WorktreePullRequest where
  parseJSON = withObject "WorktreePullRequest" $ \fields -> WorktreePullRequest <$> fields .: "state" <*> fields .:! "url" <*> fields .:! "title" <*> pure (additionalFields ["state", "url", "title"] fields)

instance ToJSON WorktreePullRequest where
  toJSON result = objectWithAdditionalFields ["state", "url", "title"] (worktreePullRequestAdditionalFields result) (["state" .= worktreePullRequestState result] <> optionalField "url" (worktreePullRequestUrl result) <> optionalField "title" (worktreePullRequestTitle result))

-- | Counts are reported JSON numbers, not locally inferred nonnegative sizes.
-- An absent pull request or commit count does not imply an empty/zero report.
data InspectWorktreeDeletionResult = InspectWorktreeDeletionResult
  { inspectedWorktreePath :: !Text,
    inspectedChangedFiles :: !Scientific,
    inspectedAdditions :: !Scientific,
    inspectedDeletions :: !Scientific,
    inspectedUntrackedFiles :: !Scientific,
    inspectedHasRemoteBranch :: !Bool,
    inspectedRemoteRefsStale :: !Bool,
    inspectedBranch :: !(Maybe Text),
    inspectedLocalOnlyCommits :: !(Maybe Scientific),
    inspectedPullRequest :: !(Maybe WorktreePullRequest),
    inspectedWorktreeAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show InspectWorktreeDeletionResult where show _ = "InspectWorktreeDeletionResult <redacted>"

instance FromJSON InspectWorktreeDeletionResult where
  parseJSON = withObject "InspectWorktreeDeletionResult" $ \fields -> InspectWorktreeDeletionResult <$> fields .: "worktreePath" <*> fields .: "changedFiles" <*> fields .: "additions" <*> fields .: "deletions" <*> fields .: "untrackedFiles" <*> fields .: "hasRemoteBranch" <*> fields .: "remoteRefsStale" <*> fields .:! "branch" <*> fields .:! "localOnlyCommits" <*> fields .:! "pullRequest" <*> pure (additionalFields inspectionKeys fields)

instance ToJSON InspectWorktreeDeletionResult where
  toJSON result = objectWithAdditionalFields inspectionKeys (inspectedWorktreeAdditionalFields result) (["worktreePath" .= inspectedWorktreePath result, "changedFiles" .= inspectedChangedFiles result, "additions" .= inspectedAdditions result, "deletions" .= inspectedDeletions result, "untrackedFiles" .= inspectedUntrackedFiles result, "hasRemoteBranch" .= inspectedHasRemoteBranch result, "remoteRefsStale" .= inspectedRemoteRefsStale result] <> optionalField "branch" (inspectedBranch result) <> optionalField "localOnlyCommits" (inspectedLocalOnlyCommits result) <> optionalField "pullRequest" (inspectedPullRequest result))

-- | An archive-state notification body. Optional fields do not imply an action
-- when absent, and archivedAt remains an unparsed string in this schema.
data SessionArchiveStateChanged = SessionArchiveStateChanged
  { archiveSessionId :: !Text,
    archiveTitle :: !(Maybe Text),
    archiveTimestamp :: !(Maybe Text),
    archiveCwd :: !(Maybe Text),
    archiveRepoRoot :: !(Maybe Text),
    archiveWorktreeRemoved :: !(Maybe Bool),
    archiveAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionArchiveStateChanged where
  show _ = "SessionArchiveStateChanged <redacted>"

instance FromJSON SessionArchiveStateChanged where
  parseJSON = withObject "SessionArchiveStateChanged" $ \fields -> SessionArchiveStateChanged <$> fields .: "sessionId" <*> fields .:! "title" <*> fields .:! "archivedAt" <*> fields .:! "cwd" <*> fields .:! "repoRoot" <*> fields .:! "worktreeRemoved" <*> pure (additionalFields archiveKeys fields)

instance ToJSON SessionArchiveStateChanged where
  toJSON event = objectWithAdditionalFields archiveKeys (archiveAdditionalFields event) (["sessionId" .= archiveSessionId event] <> optionalField "title" (archiveTitle event) <> optionalField "archivedAt" (archiveTimestamp event) <> optionalField "cwd" (archiveCwd event) <> optionalField "repoRoot" (archiveRepoRoot event) <> optionalField "worktreeRemoved" (archiveWorktreeRemoved event))

-- | A checkout's reported branch change; no checkout or Git operation occurs.
data WorktreeBranchChanged = WorktreeBranchChanged
  { changedWorktreeCheckoutPath :: !Text,
    changedWorktreeBranch :: !(Maybe Text),
    changedWorktreeBranchAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WorktreeBranchChanged where
  show _ = "WorktreeBranchChanged <redacted>"

instance FromJSON WorktreeBranchChanged where
  parseJSON = withObject "WorktreeBranchChanged" $ \fields -> WorktreeBranchChanged <$> fields .: "checkoutPath" <*> fields .:! "branch" <*> pure (additionalFields ["checkoutPath", "branch"] fields)

instance ToJSON WorktreeBranchChanged where
  toJSON event = objectWithAdditionalFields ["checkoutPath", "branch"] (changedWorktreeBranchAdditionalFields event) (["checkoutPath" .= changedWorktreeCheckoutPath event] <> optionalField "branch" (changedWorktreeBranch event))

-- | A checkout-removal notification body, not a removal operation.
data WorktreeRemoved = WorktreeRemoved
  { removedWorktreeCheckoutPath :: !Text,
    removedWorktreeAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WorktreeRemoved where
  show _ = "WorktreeRemoved <redacted>"

instance FromJSON WorktreeRemoved where
  parseJSON = withObject "WorktreeRemoved" $ \fields -> WorktreeRemoved <$> fields .: "checkoutPath" <*> pure (additionalFields ["checkoutPath"] fields)

instance ToJSON WorktreeRemoved where
  toJSON event = objectWithAdditionalFields ["checkoutPath"] (removedWorktreeAdditionalFields event) ["checkoutPath" .= removedWorktreeCheckoutPath event]

contentKeys, profileKeys, managedKeys, cleanupKeys, cleanupResultKeys, archiveKeys :: [Key]
contentKeys = ["name", "script", "cleanupScript", "initialPrompt"]
profileKeys = ["id", "name", "createdAt", "updatedAt", "script", "cleanupScript", "initialPrompt", "source"]
managedKeys = ["path", "repoRoot", "lifecycle", "sessions", "branch", "isClean", "sizeBytes"]
cleanupKeys = ["worktreePath", "deleteLocalBranch", "deleteRemoteBranch", "force"]
cleanupResultKeys = ["worktreePath", "archivedSessionIds", "worktreeRemoved", "localBranchDeleted", "remoteBranchDeleted", "warnings", "branch", "preservedReason"]
archiveKeys = ["sessionId", "title", "archivedAt", "cwd", "repoRoot", "worktreeRemoved"]

saveProfileKeys, inspectionKeys :: [Key]
saveProfileKeys = ["cwd", "name", "script", "cleanupScript", "initialPrompt", "profileId"]
inspectionKeys = ["worktreePath", "changedFiles", "additions", "deletions", "untrackedFiles", "hasRemoteBranch", "remoteRefsStale", "branch", "localOnlyCommits", "pullRequest"]

normalizedProfileName :: Text -> Either WorktreeProfileError Text
normalizedProfileName raw = case normalizeProfileText 100 raw of
  Just name | not (Text.null name) -> Right name
  _ -> Left InvalidWorktreeProfileName

normalizedProfileContent :: Text -> Either WorktreeProfileError Text
normalizedProfileContent = maybe (Left WorktreeProfileContentTooLong) Right . normalizeProfileText 100000

normalizeProfileText :: Int -> Text -> Maybe Text
normalizeProfileText limit raw = do
  let value = Text.dropAround isEcmaWhitespace raw
      size = Text.foldl' (\count char -> count + if char > '\xffff' then 2 else 1) 0 value
  guard (size <= limit)
  pure value

-- Normalize before the raw bounded decoders so trim-before-length accepts
-- long whitespace padding. Do not alter the supplied 1.205.0 wire codecs.
normalizeSdkProfile :: Value -> Parser Value
normalizeSdkProfile = withObject "WorktreeProfile" $ \fields -> do
  name <- fields .: "name" >>= either (fail . show) pure . normalizedProfileName
  content <- forM ["script", "cleanupScript", "initialPrompt"] $ \key -> do
    original <- fields .:! key
    normalized <- traverse (either (fail . show) pure . normalizedProfileContent) original
    pure (optionalField key normalized)
  forM_ ["createdAt", "updatedAt"] $ \key -> do
    text <- fields .: key
    -- SDK datetime is UTC with seconds; the raw timestamp decoder below also
    -- validates calendar/time syntax. No offset/minute/leap-second rewriting.
    unless (Text.length text >= 20 && Text.index text 10 == 'T' && Text.isSuffixOf "Z" text && Text.take 2 (Text.drop 17 text) /= "60") (fail "Expected an SDK UTC profile timestamp with seconds")
  pure (Object (KeyMap.union (KeyMap.fromList ("name" .= name : concat content)) fields))
