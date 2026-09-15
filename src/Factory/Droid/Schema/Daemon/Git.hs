{-# LANGUAGE OverloadedStrings #-}

-- | Daemon Git reports and requests. No local Git, provider CLI, cache, checkout
-- or conflict resolution is performed by these codecs. Payloads remain sensitive.
module Factory.Droid.Schema.Daemon.Git
  ( GitDirectoryParams (..),
    GitBranchParams (..),
    CheckoutResolution (..),
    CheckoutBranchParams (..),
    ListGitBranchesResult (..),
    CheckoutBranchResult (..),
    GitBranchDivergence (..),
    GitProvider (..),
    GitDiffUnavailableReason (..),
    PullRequestUnavailableReason (..),
    PullRequestLookupReason (..),
    PullRequestStatus (..),
    PullRequestSubject (..),
    PullRequestLookup (..),
    PullRequestLookupBatch,
    mkPullRequestLookupBatch,
    pullRequestLookupItems,
    ResolvePullRequestStatusesParams (..),
    PullRequestStatusResult (..),
    ResolvePullRequestStatusesResult (..),
    GitDiffParams (..),
    defaultGitDiffParams,
    GitDiffFile (..),
    GitDiffCommit (..),
    GitDiffSection (..),
    GitDiffData (..),
    GitDiffResult (..),
    MissionReadinessState (..),
    MissionReadinessLevel (..),
    MissionReadinessWarning (..),
    MissionReadinessResult (..),
    GitCommitParams (..),
    CreatePullRequestParams (..),
    defaultCreatePullRequestParams,
    CreatePullRequestResult (..),
    SemanticDiffTarget (..),
    SemanticDiffCacheResult (..),
    SaveSemanticDiffParams (..),
    GenerateSemanticDiffParams (..),
    GenerateSemanticDiffResult (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), withObject, withScientific, withText, (.!=), (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField, requireLiteral)
import Numeric.Natural (Natural)

data GitDirectoryParams = GitDirectoryParams
  { gitDirectory :: !Text,
    gitDirectoryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitDirectoryParams where show _ = "GitDirectoryParams <redacted>"

instance FromJSON GitDirectoryParams where
  parseJSON = withObject "GitDirectoryParams" $ \fields -> GitDirectoryParams <$> fields .: "cwd" <*> pure (additionalFields ["cwd"] fields)

instance ToJSON GitDirectoryParams where
  toJSON value = objectWithAdditionalFields ["cwd"] (gitDirectoryAdditionalFields value) ["cwd" .= gitDirectory value]

data GitBranchParams = GitBranchParams
  { gitBranchDirectory :: !Text,
    gitBranchName :: !Text,
    gitBranchAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitBranchParams where show _ = "GitBranchParams <redacted>"

instance FromJSON GitBranchParams where
  parseJSON = withObject "GitBranchParams" $ \fields -> GitBranchParams <$> fields .: "cwd" <*> fields .: "branch" <*> pure (additionalFields ["cwd", "branch"] fields)

instance ToJSON GitBranchParams where
  toJSON value = objectWithAdditionalFields ["cwd", "branch"] (gitBranchAdditionalFields value) ["cwd" .= gitBranchDirectory value, "branch" .= gitBranchName value]

data CheckoutResolution = CheckoutStash | CheckoutCommit deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON CheckoutResolution where
  parseJSON = withText "CheckoutResolution" $ \case "stash" -> pure CheckoutStash; "commit" -> pure CheckoutCommit; _ -> fail "Unknown checkout resolution"

instance ToJSON CheckoutResolution where
  toJSON CheckoutStash = String "stash"
  toJSON CheckoutCommit = String "commit"

data CheckoutBranchParams = CheckoutBranchParams
  { checkoutDirectory :: !Text,
    checkoutBranch :: !Text,
    checkoutCreate :: !(Maybe Bool),
    checkoutResolution :: !(Maybe CheckoutResolution),
    checkoutAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CheckoutBranchParams where show _ = "CheckoutBranchParams <redacted>"

instance FromJSON CheckoutBranchParams where
  parseJSON = withObject "CheckoutBranchParams" $ \fields -> CheckoutBranchParams <$> fields .: "cwd" <*> fields .: "branch" <*> fields .:! "create" <*> fields .:! "resolution" <*> pure (additionalFields ["cwd", "branch", "create", "resolution"] fields)

instance ToJSON CheckoutBranchParams where
  toJSON value = objectWithAdditionalFields ["cwd", "branch", "create", "resolution"] (checkoutAdditionalFields value) (["cwd" .= checkoutDirectory value, "branch" .= checkoutBranch value] <> optionalField "create" (checkoutCreate value) <> optionalField "resolution" (checkoutResolution value))

data ListGitBranchesResult = ListGitBranchesResult
  { isGitRepository :: !Bool,
    gitBranches :: ![Text],
    gitCurrentBranch :: !(Maybe Text),
    gitBranchesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListGitBranchesResult where show _ = "ListGitBranchesResult <redacted>"

instance FromJSON ListGitBranchesResult where
  parseJSON = withObject "ListGitBranchesResult" $ \fields -> ListGitBranchesResult <$> fields .: "isGitRepository" <*> fields .: "branches" <*> fields .: "currentBranch" <*> pure (additionalFields ["isGitRepository", "branches", "currentBranch"] fields)

instance ToJSON ListGitBranchesResult where
  toJSON value = objectWithAdditionalFields ["isGitRepository", "branches", "currentBranch"] (gitBranchesAdditionalFields value) ["isGitRepository" .= isGitRepository value, "branches" .= gitBranches value, "currentBranch" .= gitCurrentBranch value]

data CheckoutBranchResult
  = BranchCheckedOut !Text !(Maybe Text) !Object
  | BranchNeedsResolution !Text !Scientific !Scientific !Scientific !Scientific !Object
  deriving stock (Eq)

instance Show CheckoutBranchResult where show _ = "CheckoutBranchResult <redacted>"

instance FromJSON CheckoutBranchResult where
  parseJSON = withObject "CheckoutBranchResult" $ \fields -> do
    status <- fields .: "status" :: Parser Text
    case status of
      "checked_out" -> BranchCheckedOut <$> fields .: "currentBranch" <*> fields .:! "pullFailure" <*> pure (additionalFields ["status", "currentBranch", "pullFailure"] fields)
      "needs_resolution" -> BranchNeedsResolution <$> fields .: "message" <*> fields .: "changedFiles" <*> fields .: "additions" <*> fields .: "deletions" <*> (fields .:! "untrackedFiles" .!= 0) <*> pure (additionalFields ["status", "message", "changedFiles", "additions", "deletions", "untrackedFiles"] fields)
      _ -> fail "Unknown checkout result"

instance ToJSON CheckoutBranchResult where
  toJSON (BranchCheckedOut branch failure extras) = objectWithAdditionalFields ["status", "currentBranch", "pullFailure"] extras (["status" .= String "checked_out", "currentBranch" .= branch] <> optionalField "pullFailure" failure)
  toJSON (BranchNeedsResolution message changed adds dels untracked extras) = objectWithAdditionalFields ["status", "message", "changedFiles", "additions", "deletions", "untrackedFiles"] extras ["status" .= String "needs_resolution", "message" .= message, "changedFiles" .= changed, "additions" .= adds, "deletions" .= dels, "untrackedFiles" .= untracked]

data GitBranchDivergence = GitBranchTracked !Natural !Natural !Object | GitBranchNoRemote !Object | GitBranchUnavailable !Object
  deriving stock (Eq)

instance Show GitBranchDivergence where show _ = "GitBranchDivergence <redacted>"

instance FromJSON GitBranchDivergence where
  parseJSON = withObject "GitBranchDivergence" $ \fields -> do
    status <- fields .: "status" :: Parser Text
    case status of
      "tracked" -> GitBranchTracked <$> fields .: "ahead" <*> fields .: "behind" <*> pure (additionalFields ["status", "ahead", "behind"] fields)
      "no_remote" -> pure (GitBranchNoRemote (additionalFields ["status"] fields))
      "unavailable" -> pure (GitBranchUnavailable (additionalFields ["status"] fields))
      _ -> fail "Unknown divergence status"

instance ToJSON GitBranchDivergence where
  toJSON (GitBranchTracked ahead behind extras) = objectWithAdditionalFields ["status", "ahead", "behind"] extras ["status" .= String "tracked", "ahead" .= ahead, "behind" .= behind]
  toJSON (GitBranchNoRemote extras) = objectWithAdditionalFields ["status"] extras ["status" .= String "no_remote"]
  toJSON (GitBranchUnavailable extras) = objectWithAdditionalFields ["status"] extras ["status" .= String "unavailable"]

data GitProvider = GitHubProvider | GitLabProvider deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON GitProvider where
  parseJSON = withText "GitProvider" $ \case "github" -> pure GitHubProvider; "gitlab" -> pure GitLabProvider; _ -> fail "Unknown Git provider"

instance ToJSON GitProvider where
  toJSON GitHubProvider = String "github"
  toJSON GitLabProvider = String "gitlab"

data GitDiffUnavailableReason = GitDiffMissingSessionCwd | GitDiffNotRepository | GitDiffGitUnavailable | GitDiffUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON GitDiffUnavailableReason where
  parseJSON = withText "GitDiffUnavailableReason" $ \case "missing_session_cwd" -> pure GitDiffMissingSessionCwd; "not_git_repository" -> pure GitDiffNotRepository; "git_not_available" -> pure GitDiffGitUnavailable; "unknown" -> pure GitDiffUnknown; _ -> fail "Unknown diff unavailable reason"

instance ToJSON GitDiffUnavailableReason where
  toJSON GitDiffMissingSessionCwd = String "missing_session_cwd"
  toJSON GitDiffNotRepository = String "not_git_repository"
  toJSON GitDiffGitUnavailable = String "git_not_available"
  toJSON GitDiffUnknown = String "unknown"

data PullRequestUnavailableReason = PullRequestUnsupportedRemote | PullRequestLookupFailed | PullRequestUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON PullRequestUnavailableReason where
  parseJSON = withText "PullRequestUnavailableReason" $ \case "unsupported_remote" -> pure PullRequestUnsupportedRemote; "lookup_failed" -> pure PullRequestLookupFailed; "unknown" -> pure PullRequestUnknown; _ -> fail "Unknown PR unavailable reason"

instance ToJSON PullRequestUnavailableReason where
  toJSON PullRequestUnsupportedRemote = String "unsupported_remote"
  toJSON PullRequestLookupFailed = String "lookup_failed"
  toJSON PullRequestUnknown = String "unknown"

data PullRequestLookupReason = PullRequestBranchResolved | PullRequestGitActionCompleted | PullRequestManualRefresh | PullRequestTranscriptLink
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON PullRequestLookupReason where
  parseJSON = withText "PullRequestLookupReason" $ \case "branch_resolved" -> pure PullRequestBranchResolved; "git_action_completed" -> pure PullRequestGitActionCompleted; "manual_refresh" -> pure PullRequestManualRefresh; "transcript_link" -> pure PullRequestTranscriptLink; _ -> fail "Unknown PR lookup reason"

instance ToJSON PullRequestLookupReason where
  toJSON PullRequestBranchResolved = String "branch_resolved"
  toJSON PullRequestGitActionCompleted = String "git_action_completed"
  toJSON PullRequestManualRefresh = String "manual_refresh"
  toJSON PullRequestTranscriptLink = String "transcript_link"

data PullRequestStatus = PullRequestOpen !Text !(Maybe Text) !Object | PullRequestNone !Object | PullRequestUnavailable !PullRequestUnavailableReason !Object
  deriving stock (Eq)

instance Show PullRequestStatus where show _ = "PullRequestStatus <redacted>"

instance FromJSON PullRequestStatus where
  parseJSON = withObject "PullRequestStatus" $ \fields -> do
    state <- fields .: "state" :: Parser Text
    case state of
      "open" -> PullRequestOpen <$> fields .: "url" <*> fields .:! "title" <*> pure (additionalFields ["state", "url", "title"] fields)
      "none" -> pure (PullRequestNone (additionalFields ["state"] fields))
      "unavailable" -> pure (PullRequestUnavailable (fallbackValue PullRequestUnknown "reason" fields) (additionalFields ["state", "reason"] fields))
      _ -> fail "Unknown PR status"

instance ToJSON PullRequestStatus where
  toJSON (PullRequestOpen url title extras) = objectWithAdditionalFields ["state", "url", "title"] extras (["state" .= String "open", "url" .= url] <> optionalField "title" title)
  toJSON (PullRequestNone extras) = objectWithAdditionalFields ["state"] extras ["state" .= String "none"]
  toJSON (PullRequestUnavailable reason extras) = objectWithAdditionalFields ["state", "reason"] extras ["state" .= String "unavailable", "reason" .= reason]

data PullRequestSubject = PullRequestSubject
  { pullRequestSessionId :: !Text,
    pullRequestSubjectAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PullRequestSubject where show _ = "PullRequestSubject <redacted>"

instance FromJSON PullRequestSubject where
  parseJSON = withObject "PullRequestSubject" $ \fields -> do
    requireLiteral "kind" (String "branch") fields
    PullRequestSubject <$> fields .: "sessionId" <*> pure (additionalFields ["kind", "sessionId"] fields)

instance ToJSON PullRequestSubject where
  toJSON value = objectWithAdditionalFields ["kind", "sessionId"] (pullRequestSubjectAdditionalFields value) ["kind" .= String "branch", "sessionId" .= pullRequestSessionId value]

data PullRequestLookup = PullRequestLookup
  { lookupPullRequestSubject :: !PullRequestSubject,
    invalidatePullRequestStatus :: !(Maybe Bool),
    pullRequestLookupReason :: !(Maybe PullRequestLookupReason),
    pullRequestLookupAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PullRequestLookup where show _ = "PullRequestLookup <redacted>"

instance FromJSON PullRequestLookup where
  parseJSON = withObject "PullRequestLookup" $ \fields -> PullRequestLookup <$> fields .: "subject" <*> fields .:! "invalidate" <*> pure (KeyMap.lookup "reason" fields >>= parseMaybe parseJSON) <*> pure (additionalFields ["subject", "invalidate", "reason"] fields)

instance ToJSON PullRequestLookup where
  toJSON value = objectWithAdditionalFields ["subject", "invalidate", "reason"] (pullRequestLookupAdditionalFields value) (["subject" .= lookupPullRequestSubject value] <> optionalField "invalidate" (invalidatePullRequestStatus value) <> optionalField "reason" (pullRequestLookupReason value))

newtype PullRequestLookupBatch = PullRequestLookupBatch [PullRequestLookup] deriving stock (Eq)

instance Show PullRequestLookupBatch where show _ = "PullRequestLookupBatch <redacted>"

mkPullRequestLookupBatch :: [PullRequestLookup] -> Maybe PullRequestLookupBatch
mkPullRequestLookupBatch values = if length (take 21 values) <= 20 then Just (PullRequestLookupBatch values) else Nothing

pullRequestLookupItems :: PullRequestLookupBatch -> [PullRequestLookup]
pullRequestLookupItems (PullRequestLookupBatch values) = values

instance FromJSON PullRequestLookupBatch where
  parseJSON value = parseJSON value >>= maybe (fail "At most 20 PR lookups are allowed") pure . mkPullRequestLookupBatch

instance ToJSON PullRequestLookupBatch where toJSON = toJSON . pullRequestLookupItems

data ResolvePullRequestStatusesParams = ResolvePullRequestStatusesParams
  { pullRequestLookups :: !PullRequestLookupBatch,
    pullRequestQueryAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ResolvePullRequestStatusesParams where show _ = "ResolvePullRequestStatusesParams <redacted>"

instance FromJSON ResolvePullRequestStatusesParams where
  parseJSON = withObject "ResolvePullRequestStatusesParams" $ \fields -> ResolvePullRequestStatusesParams <$> fields .: "lookups" <*> pure (additionalFields ["lookups"] fields)

instance ToJSON ResolvePullRequestStatusesParams where
  toJSON value = objectWithAdditionalFields ["lookups"] (pullRequestQueryAdditionalFields value) ["lookups" .= pullRequestLookups value]

-- | Nullable status is distinct from a resolved "none". Remote URL omission is
-- distinct from null; no provider/lifetime policy is inferred by the SDK.
data PullRequestStatusResult = PullRequestStatusResult
  { resolvedPullRequestSubject :: !PullRequestSubject,
    resolvedPullRequestBranch :: !(Maybe Text),
    resolvedPullRequestStatus :: !(Maybe PullRequestStatus),
    pullRequestResolvedAt :: !Scientific,
    pullRequestStaleAfterMs :: !Scientific,
    pullRequestProvider :: !(Maybe GitProvider),
    pullRequestRemoteUrl :: !(Maybe (Maybe Text)),
    pullRequestResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show PullRequestStatusResult where show _ = "PullRequestStatusResult <redacted>"

instance FromJSON PullRequestStatusResult where
  parseJSON = withObject "PullRequestStatusResult" $ \fields -> PullRequestStatusResult <$> fields .: "subject" <*> fields .: "branch" <*> fields .: "status" <*> fields .: "resolvedAt" <*> fields .: "staleAfterMs" <*> fields .:! "provider" <*> fields .:! "remoteUrl" <*> pure (additionalFields statusResultKeys fields)

instance ToJSON PullRequestStatusResult where
  toJSON value = objectWithAdditionalFields statusResultKeys (pullRequestResultAdditionalFields value) (["subject" .= resolvedPullRequestSubject value, "branch" .= resolvedPullRequestBranch value, "status" .= resolvedPullRequestStatus value, "resolvedAt" .= pullRequestResolvedAt value, "staleAfterMs" .= pullRequestStaleAfterMs value] <> optionalField "provider" (pullRequestProvider value) <> optionalField "remoteUrl" (pullRequestRemoteUrl value))

data ResolvePullRequestStatusesResult = ResolvePullRequestStatusesResult
  { resolvedPullRequestStatuses :: ![PullRequestStatusResult],
    pullRequestStatusesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ResolvePullRequestStatusesResult where show _ = "ResolvePullRequestStatusesResult <redacted>"

instance FromJSON ResolvePullRequestStatusesResult where
  parseJSON = withObject "ResolvePullRequestStatusesResult" $ \fields -> ResolvePullRequestStatusesResult <$> fields .: "statuses" <*> pure (additionalFields ["statuses"] fields)

instance ToJSON ResolvePullRequestStatusesResult where
  toJSON value = objectWithAdditionalFields ["statuses"] (pullRequestStatusesAdditionalFields value) ["statuses" .= resolvedPullRequestStatuses value]

data GitDiffParams = GitDiffParams
  { diffSessionId :: !Text,
    requestedBaseBranch :: !(Maybe Text),
    diffStatsOnly :: !(Maybe Bool),
    diffParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitDiffParams where show _ = "GitDiffParams <redacted>"

defaultGitDiffParams :: Text -> GitDiffParams
defaultGitDiffParams identifier = GitDiffParams identifier Nothing Nothing mempty

instance FromJSON GitDiffParams where
  parseJSON = withObject "GitDiffParams" $ \fields -> GitDiffParams <$> fields .: "sessionId" <*> fields .:! "baseBranch" <*> fields .:! "statsOnly" <*> pure (additionalFields ["sessionId", "baseBranch", "statsOnly"] fields)

instance ToJSON GitDiffParams where
  toJSON value = objectWithAdditionalFields ["sessionId", "baseBranch", "statsOnly"] (diffParamsAdditionalFields value) (["sessionId" .= diffSessionId value] <> optionalField "baseBranch" (requestedBaseBranch value) <> optionalField "statsOnly" (diffStatsOnly value))

data GitDiffFile = GitDiffFile
  { diffFilePath :: !Text,
    diffFileAdditions :: !Scientific,
    diffFileDeletions :: !Scientific,
    diffFileStatus :: !Text,
    diffFileAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitDiffFile where show _ = "GitDiffFile <redacted>"

instance FromJSON GitDiffFile where
  parseJSON = withObject "GitDiffFile" $ \fields -> GitDiffFile <$> fields .: "path" <*> fields .: "additions" <*> fields .: "deletions" <*> fields .: "status" <*> pure (additionalFields ["path", "additions", "deletions", "status"] fields)

instance ToJSON GitDiffFile where
  toJSON value = objectWithAdditionalFields ["path", "additions", "deletions", "status"] (diffFileAdditionalFields value) ["path" .= diffFilePath value, "additions" .= diffFileAdditions value, "deletions" .= diffFileDeletions value, "status" .= diffFileStatus value]

data GitDiffCommit = GitDiffCommit
  { diffCommitHash :: !Text,
    diffCommitMessage :: !Text,
    diffCommitAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitDiffCommit where show _ = "GitDiffCommit <redacted>"

instance FromJSON GitDiffCommit where
  parseJSON = withObject "GitDiffCommit" $ \fields -> GitDiffCommit <$> fields .: "hash" <*> fields .: "message" <*> pure (additionalFields ["hash", "message"] fields)

instance ToJSON GitDiffCommit where
  toJSON value = objectWithAdditionalFields ["hash", "message"] (diffCommitAdditionalFields value) ["hash" .= diffCommitHash value, "message" .= diffCommitMessage value]

data GitDiffSection = GitDiffSection
  { sectionDiff :: !Text,
    sectionFiles :: ![GitDiffFile],
    sectionAdditions :: !Scientific,
    sectionDeletions :: !Scientific
  }
  deriving stock (Eq)

instance Show GitDiffSection where show _ = "GitDiffSection <redacted>"

-- | Committed/local/unstaged defaults apply only when fields are absent, never
-- to explicit null or malformed data. Encoding materializes these defaults.
data GitDiffData = GitDiffData
  { gitDiffText :: !Text,
    gitDiffBranch :: !Text,
    gitDiffBaseBranch :: !Text,
    gitDiffFiles :: ![GitDiffFile],
    gitDiffAdditions :: !Scientific,
    gitDiffDeletions :: !Scientific,
    gitDiffRemoteUrl :: !(Maybe Text),
    gitDiffCommits :: ![GitDiffCommit],
    committedGitDiff :: !GitDiffSection,
    localGitDiff :: !GitDiffSection,
    unstagedGitDiff :: !GitDiffSection,
    gitDiffPushableCommitCount :: !(Maybe Scientific),
    gitDiffDetachedHead :: !(Maybe Bool),
    gitDiffDefaultBranch :: !(Maybe Text),
    gitDiffBaseExistsOnRemote :: !(Maybe Bool),
    gitDiffPullRequestStatus :: !(Maybe PullRequestStatus),
    gitDiffPullRequestProvider :: !(Maybe GitProvider),
    gitDiffAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitDiffData where show _ = "GitDiffData <redacted>"

instance FromJSON GitDiffData where
  parseJSON = withObject "GitDiffData" $ \fields -> GitDiffData <$> fields .: "diff" <*> fields .: "branch" <*> fields .: "baseBranch" <*> fields .: "files" <*> fields .: "totalAdditions" <*> fields .: "totalDeletions" <*> fields .: "remoteUrl" <*> fields .: "commits" <*> parseSection committedKeys fields <*> parseSection localKeys fields <*> parseSection unstagedKeys fields <*> fields .:! "pushableCommitCount" <*> fields .:! "isDetachedHead" <*> fields .:! "defaultBranch" <*> fields .:! "baseBranchExistsOnRemote" <*> fields .:! "pullRequestStatus" <*> fields .:! "pullRequestProvider" <*> pure (additionalFields diffDataKeys fields)

instance ToJSON GitDiffData where
  toJSON value = objectWithAdditionalFields diffDataKeys (gitDiffAdditionalFields value) (["diff" .= gitDiffText value, "branch" .= gitDiffBranch value, "baseBranch" .= gitDiffBaseBranch value, "files" .= gitDiffFiles value, "totalAdditions" .= gitDiffAdditions value, "totalDeletions" .= gitDiffDeletions value, "remoteUrl" .= gitDiffRemoteUrl value, "commits" .= gitDiffCommits value] <> sectionFields committedKeys (committedGitDiff value) <> sectionFields localKeys (localGitDiff value) <> sectionFields unstagedKeys (unstagedGitDiff value) <> optionalField "pushableCommitCount" (gitDiffPushableCommitCount value) <> optionalField "isDetachedHead" (gitDiffDetachedHead value) <> optionalField "defaultBranch" (gitDiffDefaultBranch value) <> optionalField "baseBranchExistsOnRemote" (gitDiffBaseExistsOnRemote value) <> optionalField "pullRequestStatus" (gitDiffPullRequestStatus value) <> optionalField "pullRequestProvider" (gitDiffPullRequestProvider value))

data GitDiffResult = GitDiffAvailable !GitDiffData !Object | GitDiffUnavailable !GitDiffUnavailableReason !Text !Object
  deriving stock (Eq)

instance Show GitDiffResult where show _ = "GitDiffResult <redacted>"

instance FromJSON GitDiffResult where
  parseJSON = withObject "GitDiffResult" $ \fields -> case KeyMap.lookup "success" fields of
    Nothing -> GitDiffAvailable <$> parseJSON (Object fields) <*> pure mempty
    Just (Bool True) -> GitDiffAvailable <$> fields .: "data" <*> pure (additionalFields ["success", "data"] fields)
    Just (Bool False) -> GitDiffUnavailable (fallbackValue GitDiffUnknown "unavailableReason" fields) <$> fields .: "unavailableMessage" <*> pure (additionalFields ["success", "unavailableReason", "unavailableMessage"] fields)
    _ -> fail "Invalid diff success discriminator"

instance ToJSON GitDiffResult where
  toJSON (GitDiffAvailable value extras) = objectWithAdditionalFields ["success", "data"] extras ["success" .= True, "data" .= value]
  toJSON (GitDiffUnavailable reason message extras) = objectWithAdditionalFields ["success", "unavailableReason", "unavailableMessage"] extras ["success" .= False, "unavailableReason" .= reason, "unavailableMessage" .= message]

data MissionReadinessState = ReadinessOk | ReadinessNoGit | ReadinessNoRemote | ReadinessNoReport | ReadinessLowScore
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON MissionReadinessState where
  parseJSON = withText "MissionReadinessState" $ \case "ok" -> pure ReadinessOk; "no_git" -> pure ReadinessNoGit; "no_remote" -> pure ReadinessNoRemote; "no_report" -> pure ReadinessNoReport; "low_score" -> pure ReadinessLowScore; _ -> fail "Unknown readiness state"

instance ToJSON MissionReadinessState where
  toJSON ReadinessOk = String "ok"
  toJSON ReadinessNoGit = String "no_git"
  toJSON ReadinessNoRemote = String "no_remote"
  toJSON ReadinessNoReport = String "no_report"
  toJSON ReadinessLowScore = String "low_score"

data MissionReadinessLevel = ReadinessLevelOne | ReadinessLevelTwo | ReadinessLevelThree | ReadinessLevelFour | ReadinessLevelFive
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON MissionReadinessLevel where
  parseJSON = withScientific "MissionReadinessLevel" $ \case 1 -> pure ReadinessLevelOne; 2 -> pure ReadinessLevelTwo; 3 -> pure ReadinessLevelThree; 4 -> pure ReadinessLevelFour; 5 -> pure ReadinessLevelFive; _ -> fail "Expected readiness level 1 through 5"

instance ToJSON MissionReadinessLevel where
  toJSON = Number . fromIntegral . (+ 1) . fromEnum

data MissionReadinessWarning = MissionReadinessWarning
  { readinessWarningState :: !MissionReadinessState,
    readinessWarningLevel :: !(Maybe MissionReadinessLevel),
    readinessWarningRepoUrl :: !(Maybe Text),
    readinessWarningAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show MissionReadinessWarning where show _ = "MissionReadinessWarning <redacted>"

instance FromJSON MissionReadinessWarning where
  parseJSON = withObject "MissionReadinessWarning" $ \fields -> MissionReadinessWarning <$> fields .: "state" <*> fields .:! "level" <*> fields .:! "repoUrl" <*> pure (additionalFields ["state", "level", "repoUrl"] fields)

instance ToJSON MissionReadinessWarning where
  toJSON value = objectWithAdditionalFields ["state", "level", "repoUrl"] (readinessWarningAdditionalFields value) (["state" .= readinessWarningState value] <> optionalField "level" (readinessWarningLevel value) <> optionalField "repoUrl" (readinessWarningRepoUrl value))

-- | Readiness information is not folder trust or permission to acknowledge.
data MissionReadinessResult = MissionReadinessResult
  { readinessIsGitRepo :: !Bool,
    readinessHasRemote :: !Bool,
    readinessRemoteUrl :: !(Maybe Text),
    readinessIsEmpty :: !Bool,
    readinessWarning :: !(Maybe MissionReadinessWarning),
    readinessAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show MissionReadinessResult where show _ = "MissionReadinessResult <redacted>"

instance FromJSON MissionReadinessResult where
  parseJSON = withObject "MissionReadinessResult" $ \fields -> MissionReadinessResult <$> fields .: "isGitRepo" <*> fields .: "hasRemote" <*> fields .: "remoteUrl" <*> fields .: "isEmpty" <*> fields .:! "warning" <*> pure (additionalFields ["isGitRepo", "hasRemote", "remoteUrl", "isEmpty", "warning"] fields)

instance ToJSON MissionReadinessResult where
  toJSON value = objectWithAdditionalFields ["isGitRepo", "hasRemote", "remoteUrl", "isEmpty", "warning"] (readinessAdditionalFields value) (["isGitRepo" .= readinessIsGitRepo value, "hasRemote" .= readinessHasRemote value, "remoteUrl" .= readinessRemoteUrl value, "isEmpty" .= readinessIsEmpty value] <> optionalField "warning" (readinessWarning value))

data GitCommitParams = GitCommitParams
  { commitSessionId :: !Text,
    commitMessage :: !Text,
    commitAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GitCommitParams where show _ = "GitCommitParams <redacted>"

instance FromJSON GitCommitParams where
  parseJSON = withObject "GitCommitParams" $ \fields -> GitCommitParams <$> fields .: "sessionId" <*> fields .: "message" <*> pure (additionalFields ["sessionId", "message"] fields)

instance ToJSON GitCommitParams where
  toJSON value = objectWithAdditionalFields ["sessionId", "message"] (commitAdditionalFields value) ["sessionId" .= commitSessionId value, "message" .= commitMessage value]

data CreatePullRequestParams = CreatePullRequestParams
  { createPullRequestSessionId :: !Text,
    createPullRequestTitle :: !Text,
    createPullRequestBody :: !(Maybe Text),
    createPullRequestBaseBranch :: !Text,
    createPullRequestDraft :: !(Maybe Bool),
    createPullRequestTicketIds :: !(Maybe [Text]),
    createPullRequestTicketUrls :: !(Maybe [Text]),
    createPullRequestJiraKeys :: !(Maybe [Text]),
    createPullRequestLinearIds :: !(Maybe [Text]),
    createPullRequestAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CreatePullRequestParams where show _ = "CreatePullRequestParams <redacted>"

defaultCreatePullRequestParams :: Text -> Text -> Text -> CreatePullRequestParams
defaultCreatePullRequestParams identifier title base = CreatePullRequestParams identifier title Nothing base Nothing Nothing Nothing Nothing Nothing mempty

instance FromJSON CreatePullRequestParams where
  parseJSON = withObject "CreatePullRequestParams" $ \fields -> CreatePullRequestParams <$> fields .: "sessionId" <*> fields .: "title" <*> fields .:! "body" <*> fields .: "baseBranch" <*> fields .:! "draft" <*> fields .:! "linkedTicketIds" <*> fields .:! "linkedTicketUrls" <*> fields .:! "jiraIssueKeys" <*> fields .:! "linearIssueIds" <*> pure (additionalFields createPrKeys fields)

instance ToJSON CreatePullRequestParams where
  toJSON value = objectWithAdditionalFields createPrKeys (createPullRequestAdditionalFields value) (["sessionId" .= createPullRequestSessionId value, "title" .= createPullRequestTitle value, "baseBranch" .= createPullRequestBaseBranch value] <> optionalField "body" (createPullRequestBody value) <> optionalField "draft" (createPullRequestDraft value) <> optionalField "linkedTicketIds" (createPullRequestTicketIds value) <> optionalField "linkedTicketUrls" (createPullRequestTicketUrls value) <> optionalField "jiraIssueKeys" (createPullRequestJiraKeys value) <> optionalField "linearIssueIds" (createPullRequestLinearIds value))

data CreatePullRequestResult = CreatePullRequestResult
  { createdPullRequestNumber :: !Scientific,
    createdPullRequestTitle :: !Text,
    createdPullRequestUrl :: !Text,
    createdPullRequestState :: !Text,
    createdPullRequestDraft :: !Bool,
    createdPullRequestAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CreatePullRequestResult where show _ = "CreatePullRequestResult <redacted>"

instance FromJSON CreatePullRequestResult where
  parseJSON = withObject "CreatePullRequestResult" $ \fields -> CreatePullRequestResult <$> fields .: "number" <*> fields .: "title" <*> fields .: "url" <*> fields .: "state" <*> fields .: "draft" <*> pure (additionalFields ["number", "title", "url", "state", "draft"] fields)

instance ToJSON CreatePullRequestResult where
  toJSON value = objectWithAdditionalFields ["number", "title", "url", "state", "draft"] (createdPullRequestAdditionalFields value) ["number" .= createdPullRequestNumber value, "title" .= createdPullRequestTitle value, "url" .= createdPullRequestUrl value, "state" .= createdPullRequestState value, "draft" .= createdPullRequestDraft value]

data SemanticDiffTarget = SemanticDiffTarget
  { semanticCurrentBranch :: !Text,
    semanticBaseBranch :: !Text,
    semanticTargetAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SemanticDiffTarget where show _ = "SemanticDiffTarget <redacted>"

instance FromJSON SemanticDiffTarget where
  parseJSON = withObject "SemanticDiffTarget" $ \fields -> SemanticDiffTarget <$> fields .: "currentBranch" <*> fields .: "baseBranch" <*> pure (additionalFields ["currentBranch", "baseBranch"] fields)

instance ToJSON SemanticDiffTarget where
  toJSON value = objectWithAdditionalFields ["currentBranch", "baseBranch"] (semanticTargetAdditionalFields value) ["currentBranch" .= semanticCurrentBranch value, "baseBranch" .= semanticBaseBranch value]

-- | Cache misses use required null fields, not omitted or synthesized content.
data SemanticDiffCacheResult = SemanticDiffCacheResult
  { cachedSemanticContent :: !(Maybe Text),
    cachedSemanticCommitHash :: !(Maybe Text),
    cachedSemanticTruncated :: !Bool,
    semanticCacheAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SemanticDiffCacheResult where show _ = "SemanticDiffCacheResult <redacted>"

instance FromJSON SemanticDiffCacheResult where
  parseJSON = withObject "SemanticDiffCacheResult" $ \fields -> SemanticDiffCacheResult <$> fields .: "content" <*> fields .: "commitHash" <*> fields .: "truncated" <*> pure (additionalFields ["content", "commitHash", "truncated"] fields)

instance ToJSON SemanticDiffCacheResult where
  toJSON value = objectWithAdditionalFields ["content", "commitHash", "truncated"] (semanticCacheAdditionalFields value) ["content" .= cachedSemanticContent value, "commitHash" .= cachedSemanticCommitHash value, "truncated" .= cachedSemanticTruncated value]

data SaveSemanticDiffParams = SaveSemanticDiffParams
  { saveSemanticCurrentBranch :: !Text,
    saveSemanticBaseBranch :: !Text,
    saveSemanticCommitHash :: !Text,
    saveSemanticContent :: !Text,
    saveSemanticTruncated :: !Bool,
    saveSemanticAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SaveSemanticDiffParams where show _ = "SaveSemanticDiffParams <redacted>"

instance FromJSON SaveSemanticDiffParams where
  parseJSON = withObject "SaveSemanticDiffParams" $ \fields -> SaveSemanticDiffParams <$> fields .: "currentBranch" <*> fields .: "baseBranch" <*> fields .: "commitHash" <*> fields .: "content" <*> fields .: "truncated" <*> pure (additionalFields ["currentBranch", "baseBranch", "commitHash", "content", "truncated"] fields)

instance ToJSON SaveSemanticDiffParams where
  toJSON value = objectWithAdditionalFields ["currentBranch", "baseBranch", "commitHash", "content", "truncated"] (saveSemanticAdditionalFields value) ["currentBranch" .= saveSemanticCurrentBranch value, "baseBranch" .= saveSemanticBaseBranch value, "commitHash" .= saveSemanticCommitHash value, "content" .= saveSemanticContent value, "truncated" .= saveSemanticTruncated value]

data GenerateSemanticDiffParams = GenerateSemanticDiffParams
  { generateSemanticSessionId :: !Text,
    generateSemanticDiffText :: !Text,
    generateSemanticBaseBranch :: !Text,
    generateSemanticCurrentBranch :: !Text,
    generateSemanticCommitHash :: !(Maybe Text),
    generateSemanticModelId :: !(Maybe Text),
    generateSemanticUnstagedDiff :: !(Maybe Text),
    generateSemanticAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GenerateSemanticDiffParams where show _ = "GenerateSemanticDiffParams <redacted>"

instance FromJSON GenerateSemanticDiffParams where
  parseJSON = withObject "GenerateSemanticDiffParams" $ \fields -> GenerateSemanticDiffParams <$> fields .: "sessionId" <*> fields .: "diff" <*> fields .: "baseBranch" <*> fields .: "currentBranch" <*> fields .:! "commitHash" <*> fields .:! "modelId" <*> fields .:! "unstagedDiff" <*> pure (additionalFields generateSemanticKeys fields)

instance ToJSON GenerateSemanticDiffParams where
  toJSON value = objectWithAdditionalFields generateSemanticKeys (generateSemanticAdditionalFields value) (["sessionId" .= generateSemanticSessionId value, "diff" .= generateSemanticDiffText value, "baseBranch" .= generateSemanticBaseBranch value, "currentBranch" .= generateSemanticCurrentBranch value] <> optionalField "commitHash" (generateSemanticCommitHash value) <> optionalField "modelId" (generateSemanticModelId value) <> optionalField "unstagedDiff" (generateSemanticUnstagedDiff value))

data GenerateSemanticDiffResult = GenerateSemanticDiffResult
  { generatedSemanticContent :: !Text,
    generatedSemanticTruncated :: !Bool,
    generatedSemanticSessionId :: !Text,
    generatedSemanticAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GenerateSemanticDiffResult where show _ = "GenerateSemanticDiffResult <redacted>"

instance FromJSON GenerateSemanticDiffResult where
  parseJSON = withObject "GenerateSemanticDiffResult" $ \fields -> GenerateSemanticDiffResult <$> fields .: "content" <*> fields .: "truncated" <*> fields .: "sessionId" <*> pure (additionalFields ["content", "truncated", "sessionId"] fields)

instance ToJSON GenerateSemanticDiffResult where
  toJSON value = objectWithAdditionalFields ["content", "truncated", "sessionId"] (generatedSemanticAdditionalFields value) ["content" .= generatedSemanticContent value, "truncated" .= generatedSemanticTruncated value, "sessionId" .= generatedSemanticSessionId value]

createPrKeys, generateSemanticKeys :: [Key]
createPrKeys = ["sessionId", "title", "body", "baseBranch", "draft", "linkedTicketIds", "linkedTicketUrls", "jiraIssueKeys", "linearIssueIds"]
generateSemanticKeys = ["sessionId", "diff", "baseBranch", "currentBranch", "commitHash", "modelId", "unstagedDiff"]

fallbackValue :: (FromJSON a) => a -> Key -> Object -> a
fallbackValue fallback key fields = fromMaybe fallback (KeyMap.lookup key fields >>= parseMaybe parseJSON)

type SectionKeys = (Key, Key, Key, Key)

committedKeys, localKeys, unstagedKeys :: SectionKeys
committedKeys = ("committedDiff", "committedFiles", "committedTotalAdditions", "committedTotalDeletions")
localKeys = ("localDiff", "localFiles", "localTotalAdditions", "localTotalDeletions")
unstagedKeys = ("unstagedDiff", "unstagedFiles", "unstagedTotalAdditions", "unstagedTotalDeletions")

parseSection :: SectionKeys -> Object -> Parser GitDiffSection
parseSection (diff, files, adds, dels) fields = GitDiffSection <$> (fields .:! diff .!= "") <*> (fields .:! files .!= []) <*> (fields .:! adds .!= 0) <*> (fields .:! dels .!= 0)

sectionFields :: SectionKeys -> GitDiffSection -> [Pair]
sectionFields (diff, files, adds, dels) value = [diff .= sectionDiff value, files .= sectionFiles value, adds .= sectionAdditions value, dels .= sectionDeletions value]

sectionKeyList :: SectionKeys -> [Key]
sectionKeyList (a, b, c, d) = [a, b, c, d]

statusResultKeys, diffDataKeys :: [Key]
statusResultKeys = ["subject", "branch", "status", "resolvedAt", "staleAfterMs", "provider", "remoteUrl"]
diffDataKeys = ["diff", "branch", "baseBranch", "files", "totalAdditions", "totalDeletions", "remoteUrl", "commits", "pushableCommitCount", "isDetachedHead", "defaultBranch", "baseBranchExistsOnRemote", "pullRequestStatus", "pullRequestProvider"] <> concatMap sectionKeyList [committedKeys, localKeys, unstagedKeys]
