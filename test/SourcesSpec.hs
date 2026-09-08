{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SourcesSpec (sourceTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( Object,
    Result (..),
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, parseEither)
import Data.List (sort, (\\))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Schema.Primitives
import Factory.Droid.Schema.Sources
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

sourceTests :: Value -> TestTree
sourceTests schema =
  testGroup
    "Session sources"
    [ testCase "all sixteen schema variants have full and minimal goldens" $ do
        branches <- either assertFailure pure (sourceBranches schema)
        length branches @?= 16
        length fixtures @?= length branches
        forM_ (zip branches fixtures) $ \(branch, fixture) -> do
          properties <- either assertFailure pure (parseEither (.: "properties") branch :: Either String Object)
          required <- either assertFailure pure (parseEither (.: "required") branch :: Either String [Key])
          platform <- either assertFailure pure (schemaAt ["properties", "platform", "const"] (Object branch))
          KeyMap.lookup "platform" (fixtureFullJSON fixture) @?= Just platform
          sort (KeyMap.keys properties) @?= sort (KeyMap.keys (fixtureFullJSON fixture))
          sort required @?= sort (KeyMap.keys (fixtureMinimalJSON fixture))
          forM_ [(fixtureFull fixture, fixtureFullJSON fixture), (fixtureMinimal fixture, fixtureMinimalJSON fixture)] $ \(details, fields) -> do
            let value = SessionSource details mempty
            fromJSON (Object fields) @?= Success value
            toJSON value @?= Object fields
            eitherDecode (encode value) @?= Right value,
      testCase "every branch enforces required fields and their string types" $ do
        branches <- either assertFailure pure (sourceBranches schema)
        forM_ (zip branches fixtures) $ \(branch, fixture) -> do
          required <- either assertFailure pure (parseEither (.: "required") branch :: Either String [Key])
          forM_ required $ \key -> do
            rejects (Proxy @SessionSource) (Object (KeyMap.delete key (fixtureFullJSON fixture)))
            forM_ [Null, Number 1, Bool True, Array mempty, Object mempty] $ \value ->
              rejects (Proxy @SessionSource) (Object (KeyMap.insert key value (fixtureFullJSON fixture))),
      testCase "nullable metadata distinguishes absence from explicit null" $
        forM_ fixtures $ \fixture -> case fixtureNull fixture of
          Nothing -> pure ()
          Just details -> do
            let optional = KeyMap.keys (fixtureFullJSON fixture) \\ KeyMap.keys (fixtureMinimalJSON fixture)
                fields = foldr (`KeyMap.insert` Null) (fixtureMinimalJSON fixture) optional
                value = SessionSource details mempty
            fromJSON (Object fields) @?= Success value
            toJSON value @?= Object fields
            eitherDecode (encode value) @?= Right value,
      testCase "each nullable field survives alone and rejects non-string data" $
        forM_ fixtures $ \fixture -> do
          let optional = KeyMap.keys (fixtureFullJSON fixture) \\ KeyMap.keys (fixtureMinimalJSON fixture)
          forM_ optional $ \key -> do
            fullValue <- maybe (assertFailure "Missing fixture field") pure (KeyMap.lookup key (fixtureFullJSON fixture))
            forM_ [Null, fullValue] $ \value -> do
              let encoded = Object (KeyMap.insert key value (fixtureMinimalJSON fixture))
              case fromJSON encoded :: Result SessionSource of
                Error err -> assertFailure err
                Success decoded -> toJSON decoded @?= encoded
            forM_ [Number 1, Bool False, Array mempty, Object mempty] $ \value ->
              rejects (Proxy @SessionSource) (Object (KeyMap.insert key value (fixtureMinimalJSON fixture))),
      testCase "extensions round-trip without overriding declared or omitted fields" $
        forM_ fixtures $ \fixture -> do
          let extension = object ["nested" .= [Null, Bool True]]
              value = SessionSource (fixtureFull fixture) (KeyMap.singleton "future" extension)
          fromJSON (Object (KeyMap.insert "future" extension (fixtureFullJSON fixture))) @?= Success value
          toJSON value @?= Object (KeyMap.insert "future" extension (fixtureFullJSON fixture))
          forM_ (KeyMap.keys (fixtureFullJSON fixture)) $ \key ->
            toJSON (SessionSource (fixtureMinimal fixture) (KeyMap.singleton key (String "injected"))) @?= Object (fixtureMinimalJSON fixture),
      testCase "fields from another platform remain valid extensions" $ do
        let value = SessionSource SourceTui (KeyMap.fromList ["delegationSessionId" .= Null, "cloudId" .= False, "channel" .= ([] :: [Value])])
            encoded = object ["platform" .= String "tui", "delegationSessionId" .= Null, "cloudId" .= False, "channel" .= ([] :: [Value])]
        fromJSON encoded @?= Success value
        toJSON value @?= encoded
        let slack = SessionSource (SourceSlack minimalSlack) (KeyMap.singleton "cloudId" (Bool True))
        eitherDecode (encode slack) @?= Right slack,
      testCase "Teams conversation kinds match the nullable inline enum" $ do
        branches <- either assertFailure pure (sourceBranches schema)
        case [branch | branch <- branches, Right (String "microsoft-teams") <- [schemaAt ["properties", "platform", "const"] (Object branch)]] of
          [teams] -> do
            alternatives <- either assertFailure pure (schemaAt ["properties", "conversationType", "anyOf"] (Object teams))
            alternatives @?= toJSON [object ["type" .= String "string", "enum" .= [TeamsPersonal, TeamsGroupChat, TeamsChannel]], object ["type" .= String "null"]]
          _ -> assertFailure "Expected one Teams schema branch"
        forM_ [TeamsPersonal, TeamsGroupChat, TeamsChannel] $ \kind -> do
          let value = SessionSource (SourceTeams (minimalTeams {teamsConversationType = Just (Just kind)})) mempty
          fromJSON (toJSON value) @?= Success value
        rejects (Proxy @SessionSource) (object ["platform" .= String "microsoft-teams", "tenantId" .= String "tenant", "conversationId" .= String "conversation", "serviceUrl" .= String "service", "delegationSessionId" .= String "delegation", "conversationType" .= String "future"]),
      testCase "unknown is an explicit platform, not an unknown-value fallback" $ do
        fromJSON (object ["platform" .= String "unknown"]) @?= Success (SessionSource SourceUnknown mempty)
        rejects (Proxy @SessionSource) (object ["platform" .= String "future"])
        forM_ [Null, Bool True, Number 1, Array mempty, Object mempty] $ \value -> rejects (Proxy @SessionSource) (object ["platform" .= value]),
      testCase "wire strings are not normalized into other identifiers or URLs" $ do
        let teams = minimalTeams {teamsConversationId = "conversation;messageid=literal", teamsServiceUrl = "opaque service", teamsDelegationSessionId = "independent delegation"}
            value = SessionSource (SourceTeams teams) mempty
        fromJSON (toJSON value) @?= Success value
        fromJSON (object ["platform" .= String "web", "delegationSessionId" .= String ""]) @?= Success (SessionSource (SourceWeb "") mempty),
      testCase "non-object source values are rejected" $
        forM_ [Null, Bool True, Number 1, String "source", Array mempty] $
          rejects (Proxy @SessionSource),
      nonNullableRecordTests
        "BugReportSourceSchema"
        (schemaAt ["definitions", "BugReportSourceSchema"] schema)
        fullBugReport
        minimalBugReport
        bugReportJSON
        (KeyMap.singleton "surface" (String "cli"))
        (\extras source -> source {bugReportAdditionalFields = extras}),
      testCase "bug report surface and runtime enums match their schemas" $ do
        surfaces <- either assertFailure pure (schemaAt ["definitions", "BugReportSourceSchema", "properties", "surface", "enum"] schema)
        runtimes <- either assertFailure pure (schemaAt ["definitions", "BugReportSourceSchema", "properties", "runtime", "enum"] schema)
        toJSON [BugDesktop, BugWeb, BugCli] @?= surfaces
        toJSON [BugLocal, BugByom, BugDroidComputer, BugCloudWorkspace] @?= runtimes
        forM_ [BugDesktop, BugWeb, BugCli] $ \value -> fromJSON (toJSON value) @?= Success value
        forM_ [BugLocal, BugByom, BugDroidComputer, BugCloudWorkspace] $ \value -> fromJSON (toJSON value) @?= Success value
        rejects (Proxy @BugReportSource) (Object (KeyMap.insert "surface" (String "future") bugReportJSON))
        rejects (Proxy @BugReportSource) (Object (KeyMap.insert "runtime" (String "future") bugReportJSON)),
      testCase "all diagnostic length bounds count Unicode code points" $
        forM_ [("version", 100), ("cliVersion", 100), ("osVersion", 100), ("platform", 32), ("arch", 32)] $ \(key, limit) -> do
          maximumLength <- either assertFailure pure (schemaAt ["definitions", "BugReportSourceSchema", "properties", key, "maxLength"] schema)
          maximumLength @?= Number (fromIntegral limit)
          forM_ ["a", "😀"] $ \character -> do
            forM_ [0, limit] $ \count -> do
              let encoded = object ["surface" .= String "cli", key .= Text.replicate count character]
              case fromJSON encoded :: Result BugReportSource of
                Error err -> assertFailure err
                Success value -> toJSON value @?= encoded
            rejects (Proxy @BugReportSource) (object ["surface" .= String "cli", key .= Text.replicate (limit + 1) character]),
      testCase "bounded text constructors preserve content without truncation" $ do
        fmap boundedTextValue (mkBoundedText @0 "") @?= Just ""
        mkBoundedText @0 "a" @?= Nothing
        let text = Text.replicate 32 "😀"
        fmap boundedTextValue (mkBoundedText @32 text) @?= Just text
        mkBoundedText @32 (text <> "a") @?= Nothing
        forM_ [Null, Bool True, Number 1, Array mempty, Object mempty] $ rejects (Proxy @(BoundedText 32))
    ]

fullBugReport, minimalBugReport :: BugReportSource
fullBugReport = BugReportSource BugCli (Just BugByom) (mkBoundedText "sdk-version") (mkBoundedText "cli-version") (mkBoundedText "platform") (mkBoundedText "architecture") (mkBoundedText "os-version") mempty
minimalBugReport = BugReportSource BugCli Nothing Nothing Nothing Nothing Nothing Nothing mempty

bugReportJSON :: Object
bugReportJSON = KeyMap.fromList ["surface" .= String "cli", "runtime" .= String "byom", "version" .= String "sdk-version", "cliVersion" .= String "cli-version", "platform" .= String "platform", "arch" .= String "architecture", "osVersion" .= String "os-version"]

data SourceFixture = SourceFixture
  { fixtureFull :: SessionSourceDetails,
    fixtureMinimal :: SessionSourceDetails,
    fixtureNull :: Maybe SessionSourceDetails,
    fixtureFullJSON :: Object,
    fixtureMinimalJSON :: Object
  }

fixtures :: [SourceFixture]
fixtures =
  [ SourceFixture
      (SourceSlack fullSlack)
      (SourceSlack minimalSlack)
      (Just (SourceSlack nullSlack))
      (wire "slack" ["delegationSessionId" .= String "slack-delegation", "teamId" .= String "slack-team", "channel" .= String "slack-channel", "threadTs" .= String "slack-thread", "userId" .= String "slack-user", "automationId" .= String "slack-automation"])
      (wire "slack" ["delegationSessionId" .= String "slack-delegation"]),
    plain "web" (SourceWeb "web-delegation") ["delegationSessionId" .= String "web-delegation"],
    plain "api" (SourceApi "api-delegation") ["delegationSessionId" .= String "api-delegation"],
    plain "sessions_api" (SourceSessionsApi "sessions-delegation") ["delegationSessionId" .= String "sessions-delegation"],
    SourceFixture
      (SourceJira fullJira)
      (SourceJira minimalJira)
      (Just (SourceJira nullJira))
      (wire "jira" ["cloudId" .= String "jira-cloud", "issueId" .= String "jira-issue", "delegationSessionId" .= String "jira-delegation", "issueKey" .= String "jira-key", "siteId" .= String "jira-site", "projectId" .= String "jira-project", "commentId" .= String "jira-comment", "userId" .= String "jira-user", "taskId" .= String "jira-task"])
      (wire "jira" ["cloudId" .= String "jira-cloud", "issueId" .= String "jira-issue", "delegationSessionId" .= String "jira-delegation"]),
    SourceFixture
      (SourceLinear fullLinear)
      (SourceLinear minimalLinear)
      (Just (SourceLinear nullLinear))
      (wire "linear" ["agentSessionId" .= String "linear-agent", "delegationSessionId" .= String "linear-delegation", "issueId" .= String "linear-issue", "issueUrl" .= String "linear-url", "issueIdentifier" .= String "linear-identifier", "organizationId" .= String "linear-org", "userId" .= String "linear-user"])
      (wire "linear" ["agentSessionId" .= String "linear-agent", "delegationSessionId" .= String "linear-delegation"]),
    SourceFixture
      (SourceTeams fullTeams)
      (SourceTeams minimalTeams)
      (Just (SourceTeams nullTeams))
      (wire "microsoft-teams" ["tenantId" .= String "teams-tenant", "conversationId" .= String "teams-conversation", "serviceUrl" .= String "teams-service", "delegationSessionId" .= String "teams-delegation", "conversationType" .= String "channel", "rootMessageId" .= String "teams-root", "teamId" .= String "teams-team", "channelId" .= String "teams-channel", "userId" .= String "teams-user", "aadObjectId" .= String "teams-object"])
      (wire "microsoft-teams" ["tenantId" .= String "teams-tenant", "conversationId" .= String "teams-conversation", "serviceUrl" .= String "teams-service", "delegationSessionId" .= String "teams-delegation"]),
    plain "readiness-remediation" (SourceReadinessRemediation "report" "remediation-repo" "criterion") ["reportId" .= String "report", "repoUrl" .= String "remediation-repo", "criterionId" .= String "criterion"],
    plain "readiness-evaluation" (SourceReadinessEvaluation "evaluation-repo") ["repoUrl" .= String "evaluation-repo"],
    plain "automation" (SourceAutomation "automation" "computer") ["automationId" .= String "automation", "computerId" .= String "computer"],
    plain "wiki-generation" (SourceWikiGeneration "wiki-repo") ["repoUrl" .= String "wiki-repo"],
    plain "wiki-ci-setup" (SourceWikiCISetup "wiki-ci-repo") ["repoUrl" .= String "wiki-ci-repo"],
    plain "tui" SourceTui [],
    plain "desktop" SourceDesktop [],
    plain "acp" SourceAcp [],
    plain "unknown" SourceUnknown []
  ]
  where
    plain platform details fields = SourceFixture details details Nothing (wire platform fields) (wire platform fields)

minimalSlack, fullSlack, nullSlack :: SlackSourceData
minimalSlack = SlackSourceData "slack-delegation" Nothing Nothing Nothing Nothing Nothing
fullSlack = SlackSourceData "slack-delegation" (present "slack-team") (present "slack-channel") (present "slack-thread") (present "slack-user") (present "slack-automation")
nullSlack = SlackSourceData "slack-delegation" (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing)

minimalJira, fullJira, nullJira :: JiraSourceData
minimalJira = JiraSourceData "jira-cloud" "jira-issue" "jira-delegation" Nothing Nothing Nothing Nothing Nothing Nothing
fullJira = JiraSourceData "jira-cloud" "jira-issue" "jira-delegation" (present "jira-key") (present "jira-site") (present "jira-project") (present "jira-comment") (present "jira-user") (present "jira-task")
nullJira = JiraSourceData "jira-cloud" "jira-issue" "jira-delegation" (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing)

minimalLinear, fullLinear, nullLinear :: LinearSourceData
minimalLinear = LinearSourceData "linear-agent" "linear-delegation" Nothing Nothing Nothing Nothing Nothing
fullLinear = LinearSourceData "linear-agent" "linear-delegation" (present "linear-issue") (present "linear-url") (present "linear-identifier") (present "linear-org") (present "linear-user")
nullLinear = LinearSourceData "linear-agent" "linear-delegation" (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing)

minimalTeams, fullTeams, nullTeams :: TeamsSourceData
minimalTeams = TeamsSourceData "teams-tenant" "teams-conversation" "teams-service" "teams-delegation" Nothing Nothing Nothing Nothing Nothing Nothing
fullTeams = TeamsSourceData "teams-tenant" "teams-conversation" "teams-service" "teams-delegation" (Just (Just TeamsChannel)) (present "teams-root") (present "teams-team") (present "teams-channel") (present "teams-user") (present "teams-object")
nullTeams = TeamsSourceData "teams-tenant" "teams-conversation" "teams-service" "teams-delegation" (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing) (Just Nothing)

present :: Text -> Maybe (Maybe Text)
present = Just . Just

wire :: Text -> [Pair] -> Object
wire platform fields = KeyMap.fromList (("platform" .= platform) : fields)

sourceBranches :: Value -> Either String [Object]
sourceBranches schema = do
  definition <- schemaAt ["definitions", "SessionSourceSchema"] schema
  parseEither (withObject "SessionSourceSchema" (.: "anyOf")) definition
