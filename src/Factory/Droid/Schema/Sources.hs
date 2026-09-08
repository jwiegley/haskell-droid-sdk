{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Source provenance for Factory protocol 1.205.0. These records
-- describe origins; they do not contact the named services or select SDK
-- attribution. Nullable options preserve missing, null and present values.
module Factory.Droid.Schema.Sources
  ( SessionSource (..),
    SessionSourceDetails (..),
    SlackSourceData (..),
    JiraSourceData (..),
    LinearSourceData (..),
    TeamsSourceData (..),
    TeamsConversationType (..),
    BugReportSurface (..),
    BugReportRuntime (..),
    BugReportSource (..),
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
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Primitives (BoundedText)

-- | Slack delegation and optional routing identifiers. Nothing omits a
-- nullable field; Just Nothing represents its explicit null value.
data SlackSourceData = SlackSourceData
  { slackDelegationSessionId :: !Text,
    slackTeamId :: !(Maybe (Maybe Text)),
    slackChannel :: !(Maybe (Maybe Text)),
    slackThreadTs :: !(Maybe (Maybe Text)),
    slackUserId :: !(Maybe (Maybe Text)),
    slackAutomationId :: !(Maybe (Maybe Text))
  }
  deriving stock (Eq, Show)

-- | Jira origin metadata, with required cloud, issue and delegation IDs.
data JiraSourceData = JiraSourceData
  { jiraCloudId :: !Text,
    jiraIssueId :: !Text,
    jiraDelegationSessionId :: !Text,
    jiraIssueKey :: !(Maybe (Maybe Text)),
    jiraSiteId :: !(Maybe (Maybe Text)),
    jiraProjectId :: !(Maybe (Maybe Text)),
    jiraCommentId :: !(Maybe (Maybe Text)),
    jiraUserId :: !(Maybe (Maybe Text)),
    jiraTaskId :: !(Maybe (Maybe Text))
  }
  deriving stock (Eq, Show)

-- | Linear origin metadata. URLs remain strings as specified by this schema.
data LinearSourceData = LinearSourceData
  { linearAgentSessionId :: !Text,
    linearDelegationSessionId :: !Text,
    linearIssueId :: !(Maybe (Maybe Text)),
    linearIssueUrl :: !(Maybe (Maybe Text)),
    linearIssueIdentifier :: !(Maybe (Maybe Text)),
    linearOrganizationId :: !(Maybe (Maybe Text)),
    linearUserId :: !(Maybe (Maybe Text))
  }
  deriving stock (Eq, Show)

-- | The declared Bot Framework conversation types.
data TeamsConversationType = TeamsPersonal | TeamsGroupChat | TeamsChannel
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON TeamsConversationType where
  parseJSON = withText "TeamsConversationType" $ \case
    "personal" -> pure TeamsPersonal
    "groupChat" -> pure TeamsGroupChat
    "channel" -> pure TeamsChannel
    _ -> fail "Unknown Teams conversation type"

instance ToJSON TeamsConversationType where
  toJSON TeamsPersonal = String "personal"
  toJSON TeamsGroupChat = String "groupChat"
  toJSON TeamsChannel = String "channel"

-- | Teams origin metadata. The wire codec does not derive delegation IDs,
-- strip conversation suffixes or infer a service URL from another field.
data TeamsSourceData = TeamsSourceData
  { teamsTenantId :: !Text,
    teamsConversationId :: !Text,
    teamsServiceUrl :: !Text,
    teamsDelegationSessionId :: !Text,
    teamsConversationType :: !(Maybe (Maybe TeamsConversationType)),
    teamsRootMessageId :: !(Maybe (Maybe Text)),
    teamsTeamId :: !(Maybe (Maybe Text)),
    teamsChannelId :: !(Maybe (Maybe Text)),
    teamsUserId :: !(Maybe (Maybe Text)),
    teamsAadObjectId :: !(Maybe (Maybe Text))
  }
  deriving stock (Eq, Show)

-- | The complete platform union. Web/API constructors carry a delegation
-- session ID; readiness/wiki constructors carry a repository URL. Remediation
-- arguments are report ID, repository URL and criterion ID. Automation
-- arguments are automation ID and computer ID. Unknown is an explicit literal,
-- not a fallback for a platform added by a different protocol version.
data SessionSourceDetails
  = SourceSlack !SlackSourceData
  | SourceWeb !Text
  | SourceApi !Text
  | SourceSessionsApi !Text
  | SourceJira !JiraSourceData
  | SourceLinear !LinearSourceData
  | SourceTeams !TeamsSourceData
  | SourceReadinessRemediation !Text !Text !Text
  | SourceReadinessEvaluation !Text
  | SourceAutomation !Text !Text
  | SourceWikiGeneration !Text
  | SourceWikiCISetup !Text
  | SourceTui
  | SourceDesktop
  | SourceAcp
  | SourceUnknown
  deriving stock (Eq, Show)

-- | Typed provenance plus extensions. Only fields declared by the selected
-- platform are reserved; another platform's field can be a valid extension.
data SessionSource = SessionSource
  { sessionSourceDetails :: !SessionSourceDetails,
    sessionSourceAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SessionSource where
  parseJSON = withObject "SessionSource" $ \fields -> do
    platform <- fields .: "platform" :: Parser Text
    details <- case platform of
      "slack" ->
        SourceSlack
          <$> ( SlackSourceData
                  <$> fields .: "delegationSessionId"
                  <*> fields .:! "teamId"
                  <*> fields .:! "channel"
                  <*> fields .:! "threadTs"
                  <*> fields .:! "userId"
                  <*> fields .:! "automationId"
              )
      "web" -> SourceWeb <$> fields .: "delegationSessionId"
      "api" -> SourceApi <$> fields .: "delegationSessionId"
      "sessions_api" -> SourceSessionsApi <$> fields .: "delegationSessionId"
      "jira" ->
        SourceJira
          <$> ( JiraSourceData
                  <$> fields .: "cloudId"
                  <*> fields .: "issueId"
                  <*> fields .: "delegationSessionId"
                  <*> fields .:! "issueKey"
                  <*> fields .:! "siteId"
                  <*> fields .:! "projectId"
                  <*> fields .:! "commentId"
                  <*> fields .:! "userId"
                  <*> fields .:! "taskId"
              )
      "linear" ->
        SourceLinear
          <$> ( LinearSourceData
                  <$> fields .: "agentSessionId"
                  <*> fields .: "delegationSessionId"
                  <*> fields .:! "issueId"
                  <*> fields .:! "issueUrl"
                  <*> fields .:! "issueIdentifier"
                  <*> fields .:! "organizationId"
                  <*> fields .:! "userId"
              )
      "microsoft-teams" ->
        SourceTeams
          <$> ( TeamsSourceData
                  <$> fields .: "tenantId"
                  <*> fields .: "conversationId"
                  <*> fields .: "serviceUrl"
                  <*> fields .: "delegationSessionId"
                  <*> fields .:! "conversationType"
                  <*> fields .:! "rootMessageId"
                  <*> fields .:! "teamId"
                  <*> fields .:! "channelId"
                  <*> fields .:! "userId"
                  <*> fields .:! "aadObjectId"
              )
      "readiness-remediation" -> SourceReadinessRemediation <$> fields .: "reportId" <*> fields .: "repoUrl" <*> fields .: "criterionId"
      "readiness-evaluation" -> SourceReadinessEvaluation <$> fields .: "repoUrl"
      "automation" -> SourceAutomation <$> fields .: "automationId" <*> fields .: "computerId"
      "wiki-generation" -> SourceWikiGeneration <$> fields .: "repoUrl"
      "wiki-ci-setup" -> SourceWikiCISetup <$> fields .: "repoUrl"
      "tui" -> pure SourceTui
      "desktop" -> pure SourceDesktop
      "acp" -> pure SourceAcp
      "unknown" -> pure SourceUnknown
      _ -> fail "Unknown session-source platform"
    let (keys, _) = sourceEncoding details
    pure (SessionSource details (additionalFields keys fields))

instance ToJSON SessionSource where
  toJSON source =
    let (keys, fields) = sourceEncoding (sessionSourceDetails source)
     in objectWithAdditionalFields keys (sessionSourceAdditionalFields source) fields

sourceEncoding :: SessionSourceDetails -> ([Key], [Pair])
sourceEncoding = \case
  SourceSlack source ->
    tagged "slack" ["delegationSessionId", "teamId", "channel", "threadTs", "userId", "automationId"] $
      ["delegationSessionId" .= slackDelegationSessionId source]
        <> optionalField "teamId" (slackTeamId source)
        <> optionalField "channel" (slackChannel source)
        <> optionalField "threadTs" (slackThreadTs source)
        <> optionalField "userId" (slackUserId source)
        <> optionalField "automationId" (slackAutomationId source)
  SourceWeb identifier -> delegation "web" identifier
  SourceApi identifier -> delegation "api" identifier
  SourceSessionsApi identifier -> delegation "sessions_api" identifier
  SourceJira source ->
    tagged "jira" ["cloudId", "issueId", "delegationSessionId", "issueKey", "siteId", "projectId", "commentId", "userId", "taskId"] $
      ["cloudId" .= jiraCloudId source, "issueId" .= jiraIssueId source, "delegationSessionId" .= jiraDelegationSessionId source]
        <> optionalField "issueKey" (jiraIssueKey source)
        <> optionalField "siteId" (jiraSiteId source)
        <> optionalField "projectId" (jiraProjectId source)
        <> optionalField "commentId" (jiraCommentId source)
        <> optionalField "userId" (jiraUserId source)
        <> optionalField "taskId" (jiraTaskId source)
  SourceLinear source ->
    tagged "linear" ["agentSessionId", "delegationSessionId", "issueId", "issueUrl", "issueIdentifier", "organizationId", "userId"] $
      ["agentSessionId" .= linearAgentSessionId source, "delegationSessionId" .= linearDelegationSessionId source]
        <> optionalField "issueId" (linearIssueId source)
        <> optionalField "issueUrl" (linearIssueUrl source)
        <> optionalField "issueIdentifier" (linearIssueIdentifier source)
        <> optionalField "organizationId" (linearOrganizationId source)
        <> optionalField "userId" (linearUserId source)
  SourceTeams source ->
    tagged "microsoft-teams" ["tenantId", "conversationId", "serviceUrl", "delegationSessionId", "conversationType", "rootMessageId", "teamId", "channelId", "userId", "aadObjectId"] $
      ["tenantId" .= teamsTenantId source, "conversationId" .= teamsConversationId source, "serviceUrl" .= teamsServiceUrl source, "delegationSessionId" .= teamsDelegationSessionId source]
        <> optionalField "conversationType" (teamsConversationType source)
        <> optionalField "rootMessageId" (teamsRootMessageId source)
        <> optionalField "teamId" (teamsTeamId source)
        <> optionalField "channelId" (teamsChannelId source)
        <> optionalField "userId" (teamsUserId source)
        <> optionalField "aadObjectId" (teamsAadObjectId source)
  SourceReadinessRemediation report repo criterion -> tagged "readiness-remediation" ["reportId", "repoUrl", "criterionId"] ["reportId" .= report, "repoUrl" .= repo, "criterionId" .= criterion]
  SourceReadinessEvaluation repo -> repository "readiness-evaluation" repo
  SourceAutomation automation computer -> tagged "automation" ["automationId", "computerId"] ["automationId" .= automation, "computerId" .= computer]
  SourceWikiGeneration repo -> repository "wiki-generation" repo
  SourceWikiCISetup repo -> repository "wiki-ci-setup" repo
  SourceTui -> tagged "tui" [] []
  SourceDesktop -> tagged "desktop" [] []
  SourceAcp -> tagged "acp" [] []
  SourceUnknown -> tagged "unknown" [] []
  where
    delegation platform identifier = tagged platform ["delegationSessionId"] ["delegationSessionId" .= identifier]
    repository platform repo = tagged platform ["repoUrl"] ["repoUrl" .= repo]

tagged :: Text -> [Key] -> [Pair] -> ([Key], [Pair])
tagged platform keys fields = ("platform" : keys, ("platform" .= platform) : fields)

-- | The client surface associated with a bug report.
data BugReportSurface = BugDesktop | BugWeb | BugCli
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON BugReportSurface where
  parseJSON = withText "BugReportSurface" $ \case
    "desktop" -> pure BugDesktop
    "web" -> pure BugWeb
    "cli" -> pure BugCli
    _ -> fail "Unknown bug-report surface"

instance ToJSON BugReportSurface where
  toJSON BugDesktop = String "desktop"
  toJSON BugWeb = String "web"
  toJSON BugCli = String "cli"

-- | The runtime described by a bug report, not an execution selector.
data BugReportRuntime = BugLocal | BugByom | BugDroidComputer | BugCloudWorkspace
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON BugReportRuntime where
  parseJSON = withText "BugReportRuntime" $ \case
    "local" -> pure BugLocal
    "byom" -> pure BugByom
    "droid-computer" -> pure BugDroidComputer
    "cloud-workspace" -> pure BugCloudWorkspace
    _ -> fail "Unknown bug-report runtime"

instance ToJSON BugReportRuntime where
  toJSON BugLocal = String "local"
  toJSON BugByom = String "byom"
  toJSON BugDroidComputer = String "droid-computer"
  toJSON BugCloudWorkspace = String "cloud-workspace"

-- | Bounded diagnostic provenance. Limits use JSON Schema code-point lengths,
-- not JavaScript UTF-16 code-unit lengths. Encoding does not submit a report.
data BugReportSource = BugReportSource
  { bugReportSurface :: !BugReportSurface,
    bugReportRuntime :: !(Maybe BugReportRuntime),
    bugReportVersion :: !(Maybe (BoundedText 100)),
    bugReportCliVersion :: !(Maybe (BoundedText 100)),
    bugReportPlatform :: !(Maybe (BoundedText 32)),
    bugReportArch :: !(Maybe (BoundedText 32)),
    bugReportOsVersion :: !(Maybe (BoundedText 100)),
    bugReportAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BugReportSource where
  parseJSON = withObject "BugReportSource" $ \fields ->
    BugReportSource
      <$> fields .: "surface"
      <*> fields .:! "runtime"
      <*> fields .:! "version"
      <*> fields .:! "cliVersion"
      <*> fields .:! "platform"
      <*> fields .:! "arch"
      <*> fields .:! "osVersion"
      <*> pure (additionalFields bugReportKeys fields)

instance ToJSON BugReportSource where
  toJSON source =
    objectWithAdditionalFields bugReportKeys (bugReportAdditionalFields source) $
      ["surface" .= bugReportSurface source]
        <> optionalField "runtime" (bugReportRuntime source)
        <> optionalField "version" (bugReportVersion source)
        <> optionalField "cliVersion" (bugReportCliVersion source)
        <> optionalField "platform" (bugReportPlatform source)
        <> optionalField "arch" (bugReportArch source)
        <> optionalField "osVersion" (bugReportOsVersion source)

bugReportKeys :: [Key]
bugReportKeys = ["surface", "runtime", "version", "cliVersion", "platform", "arch", "osVersion"]
