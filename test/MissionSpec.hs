{-# LANGUAGE OverloadedStrings #-}

module MissionSpec (missionTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Mission
import SchemaTest (enumSchemaTest, nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

missionTests :: Value -> TestTree
missionTests schema =
  testGroup
    "Mission reports"
    [ records "DiscoveredIssueSchema" issue (issue {discoveredIssueSuggestedFix = Nothing}) issueJSON (KeyMap.delete "suggestedFix" issueJSON) (\extras value -> value {discoveredIssueAdditionalFields = extras}),
      records "VerificationCommandSchema" command command commandJSON commandJSON (\extras value -> value {verificationCommandAdditionalFields = extras}),
      records "InteractiveCheckSchema" check check checkJSON checkJSON (\extras value -> value {interactiveCheckAdditionalFields = extras}),
      records "VerificationSchema" verification (verification {verificationInteractiveChecks = Nothing}) verificationJSON (KeyMap.delete "interactiveChecks" verificationJSON) (\extras value -> value {verificationAdditionalFields = extras}),
      records "TestCaseSchema" test test testJSON testJSON (\extras value -> value {handoffTestCaseAdditionalFields = extras}),
      records "TestFileSchema" file file fileJSON fileJSON (\extras value -> value {handoffTestFileAdditionalFields = extras}),
      records "TestsSchema" tests (tests {handoffTestsUpdated = Nothing}) testsJSON (KeyMap.delete "updated" testsJSON) (\extras value -> value {handoffTestsAdditionalFields = extras}),
      records "SkillDeviationSchema" deviation deviation deviationJSON deviationJSON (\extras value -> value {skillDeviationAdditionalFields = extras}),
      records "SkillFeedbackSchema" feedback (feedback {skillSuggestedChanges = Nothing}) feedbackJSON (KeyMap.delete "suggestedChanges" feedbackJSON) (\extras value -> value {skillFeedbackAdditionalFields = extras}),
      records "HandoffSchema" handoff (handoff {handoffSalientSummary = Nothing, handoffSkillFeedback = Nothing}) handoffJSON (KeyMap.delete "salientSummary" (KeyMap.delete "skillFeedback" handoffJSON)) (\extras value -> value {handoffAdditionalFields = extras}),
      records "DismissalRecordSchema" dismissal dismissal dismissalJSON dismissalJSON (\extras value -> value {dismissalAdditionalFields = extras}),
      records "WorkerStateInfoSchema" worker (WorkerStateInfo "unparsed start" Nothing Nothing mempty) workerJSON (KeyMap.singleton "startedAt" (String "unparsed start")) (\extras value -> value {workerStateAdditionalFields = extras}),
      records "SubagentInvocationSummarySchema" invocation (invocation {invocationToolUseCount = Nothing, invocationDurationMs = Nothing}) invocationJSON (KeyMap.delete "toolUseCount" (KeyMap.delete "durationMs" invocationJSON)) (\extras value -> value {invocationAdditionalFields = extras}),
      records "MissionStateChangedNotificationSchema" stateChange (stateChange {changedMissionUpdatedAt = Nothing}) stateChangeJSON (KeyMap.delete "updatedAt" stateChangeJSON) (\extras value -> value {changedMissionStateAdditionalFields = extras}),
      records "MissionFeaturesChangedNotificationSchema" featuresChange featuresChange featuresChangeJSON featuresChangeJSON (\extras value -> value {changedMissionFeaturesAdditionalFields = extras}),
      records "MissionHeartbeatNotificationSchema" heartbeat heartbeat heartbeatJSON heartbeatJSON (\extras value -> value {missionHeartbeatAdditionalFields = extras}),
      records "MissionWorkerStartedNotificationSchema" started started startedJSON startedJSON (\extras value -> value {startedMissionWorkerAdditionalFields = extras}),
      records "MissionWorkerCompletedNotificationSchema" completed completed completedJSON completedJSON (\extras value -> value {completedMissionWorkerAdditionalFields = extras}),
      enumSchemaTest "decomposition roles" (definition "DecompSessionTypeSchema" >>= schemaAt ["enum"]) (Proxy @DecompSessionType),
      enumSchemaTest "mission phases" (definition "MissionStateEnumSchema" >>= schemaAt ["enum"]) (Proxy @MissionPhase),
      enumSchemaTest "feature lifecycle" (definition "MissionFeatureSchema" >>= schemaAt ["properties", "status", "enum"]) (Proxy @FeatureStatus),
      enumSchemaTest "feature success assessment" (definition "FeatureSuccessStateSchema" >>= schemaAt ["enum"]) (Proxy @FeatureSuccessState),
      enumSchemaTest "issue severities" (definition "DiscoveredIssueSchema" >>= schemaAt ["properties", "severity", "enum"]) (Proxy @IssueSeverity),
      enumSchemaTest "dismissal categories" (definition "DismissalRecordSchema" >>= schemaAt ["properties", "type", "enum"]) (Proxy @DismissalType),
      enumSchemaTest "subagent states" (definition "SubagentInvocationSummarySchema" >>= schemaAt ["properties", "status", "enum"]) (Proxy @SubagentStatus),
      testCase "feature fixture covers every declared field" $ do
        properties <- either assertFailure pure (definition "MissionFeatureSchema" >>= schemaAt ["properties"])
        case properties of
          Object fields -> sort (KeyMap.keys fields) @?= sort (KeyMap.keys featureJSON)
          _ -> assertFailure "Expected feature properties",
      testCase "full and minimal feature goldens preserve every value" $ do
        forM_ [(feature, featureJSON), (minimalFeature, minimalFeatureJSON)] $ \(value, wire) -> do
          fromJSON (Object wire) @?= Success value
          toJSON value @?= Object wire
          eitherDecode (encode value) @?= Right value,
      testCase "feature required fields and root object shape are enforced" $ do
        required <- either assertFailure pure (definition "MissionFeatureSchema" >>= schemaAt ["required"])
        case fromJSON required :: Result [Key] of
          Error err -> assertFailure err
          Success keys -> forM_ keys $ \key -> rejects (Proxy @MissionFeature) (Object (KeyMap.delete key featureJSON))
        forM_ [Null, Bool False, Number 1, String "feature", Array mempty] $ rejects (Proxy @MissionFeature),
      testCase "only the two legacy worker identifiers accept explicit null" $ do
        forM_ (filter (`notElem` ["currentWorkerSessionId", "completedWorkerSessionId"]) (KeyMap.keys featureJSON)) $ \key -> rejects (Proxy @MissionFeature) (Object (KeyMap.insert key Null featureJSON))
        forM_ ["currentWorkerSessionId", "completedWorkerSessionId"] $ \key ->
          forM_ [Bool False, Number 1, Object mempty, Array mempty] $ \bad -> rejects (Proxy @MissionFeature) (Object (KeyMap.insert key bad featureJSON)),
      testCase "both worker identifiers retain all absent/null/present combinations" $ do
        let states = [(Nothing, Nothing), (Just Nothing, Just Null), (Just (Just "worker"), Just (String "worker"))]
        forM_ states $ \(current, currentJSON) ->
          forM_ states $ \(previous, previousJSON) -> do
            let value = minimalFeature {missionFeatureCurrentWorkerSessionId = current, missionFeatureCompletedWorkerSessionId = previous}
                wire = addOptional "currentWorkerSessionId" currentJSON (addOptional "completedWorkerSessionId" previousJSON minimalFeatureJSON)
            fromJSON (Object wire) @?= Success value
            toJSON value @?= Object wire
            eitherDecode (encode value) @?= Right value,
      testCase "feature extensions preserve legacy fields without overriding declared ones" $ do
        let extra = KeyMap.singleton "verificationSteps" (toJSON [String "legacy report"])
            value = feature {missionFeatureAdditionalFields = extra}
            wire = Object (KeyMap.union extra featureJSON)
        fromJSON wire @?= Success value
        toJSON value @?= wire
        forM_ (KeyMap.keys featureJSON) $ \key -> toJSON (minimalFeature {missionFeatureAdditionalFields = KeyMap.singleton key (String "injected")}) @?= Object minimalFeatureJSON,
      testCase "feature arrays and lifecycle statuses are checked without normalization" $ do
        forM_ ["preconditions", "expectedBehavior", "fulfills", "workerSessionIds"] $ \key -> rejects (Proxy @MissionFeature) (Object (KeyMap.insert key (toJSON [Bool False]) featureJSON))
        rejects (Proxy @MissionFeature) (Object (KeyMap.insert "status" (String "future") featureJSON))
        let value = minimalFeature {missionFeatureStatus = FeatureCompleted, missionFeaturePreconditions = [], missionFeatureExpectedBehavior = [], missionFeatureWorkerSessionIds = Just []}
        fromJSON (toJSON value) @?= Success value,
      testCase "nested handoff records reject malformed arrays and objects" $ do
        forM_ [Null, String "record", object []] $ \bad -> do
          rejects (Proxy @Handoff) (Object (KeyMap.insert "verification" bad handoffJSON))
          rejects (Proxy @Handoff) (Object (KeyMap.insert "tests" bad handoffJSON))
          rejects (Proxy @Handoff) (Object (KeyMap.insert "discoveredIssues" (toJSON [bad]) handoffJSON))
          rejects (Proxy @Handoff) (Object (KeyMap.insert "skillFeedback" bad handoffJSON))
          rejects (Proxy @Verification) (Object (KeyMap.insert "commandsRun" (toJSON [bad]) verificationJSON))
          rejects (Proxy @Verification) (Object (KeyMap.insert "interactiveChecks" (toJSON [bad]) verificationJSON))
          rejects (Proxy @HandoffTests) (Object (KeyMap.insert "added" (toJSON [bad]) testsJSON))
          rejects (Proxy @HandoffTestFile) (Object (KeyMap.insert "cases" (toJSON [bad]) fileJSON))
          rejects (Proxy @SkillFeedback) (Object (KeyMap.insert "deviations" (toJSON [bad]) feedbackJSON)),
      testCase "nested string arrays remain typed" $ do
        rejects (Proxy @HandoffTests) (Object (KeyMap.insert "updated" (toJSON [Bool False]) testsJSON))
        rejects (Proxy @SkillFeedback) (Object (KeyMap.insert "suggestedChanges" (toJSON [Number 1]) feedbackJSON)),
      testCase "empty reports and explicit false remain valid data" $ do
        let value = Handoff "" "" (Verification [] (Just []) mempty) (HandoffTests [] "" (Just []) mempty) [] Nothing (Just (SkillFeedback False [] (Just []) mempty)) mempty
        fromJSON (toJSON value) @?= Success value
        toJSON (MissionFeaturesChanged [] mempty) @?= object ["type" .= String "mission_features_changed", "features" .= ([] :: [Value])],
      testCase "counts, durations and exit codes keep the supplied number domain" $ do
        forM_ [-1.25, 0, 123456789012345678901234567890] $ \number -> do
          fromJSON (Object (KeyMap.insert "exitCode" (Number number) commandJSON)) @?= Success (command {verificationExitCode = number})
          fromJSON (Object (KeyMap.insert "exitCode" (Number number) workerJSON)) @?= Success (worker {workerExitCode = Just number})
          fromJSON (Object (KeyMap.insert "durationMs" (Number number) invocationJSON)) @?= Success (invocation {invocationDurationMs = Just number})
          fromJSON (Object (KeyMap.insert "toolUseCount" (Number number) invocationJSON)) @?= Success (invocation {invocationToolUseCount = Just number})
          fromJSON (Object (KeyMap.insert "exitCode" (Number number) completedJSON)) @?= Success (completed {completedMissionWorkerExitCode = number}),
      testCase "notifications validate nested feature structure and independent states" $ do
        rejects (Proxy @MissionFeaturesChanged) (object ["type" .= String "mission_features_changed", "features" .= [object []]])
        rejects (Proxy @MissionStateChanged) (Object (KeyMap.insert "state" (String "future") stateChangeJSON))
        rejects (Proxy @SubagentInvocationSummary) (Object (KeyMap.insert "status" (String "in_progress") invocationJSON))
        let contradictory = feedback {skillFollowedProcedure = True}
        fromJSON (toJSON contradictory) @?= Success contradictory,
      testCase "nested report extensions stay within their declared records" $ do
        let enriched = issue {discoveredIssueAdditionalFields = KeyMap.singleton "future" Null}
            value = handoff {handoffDiscoveredIssues = [enriched, issue]}
            wire = Object (KeyMap.insert "discoveredIssues" (toJSON [Object (KeyMap.insert "future" Null issueJSON), Object issueJSON]) handoffJSON)
        fromJSON wire @?= Success value
        toJSON value @?= wire
    ]
  where
    definition name = schemaAt ["definitions", name] schema
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (definition name)
    addOptional key value fields = maybe fields (\item -> KeyMap.insert key item fields) value

feature, minimalFeature :: MissionFeature
minimalFeature = MissionFeature "feature-id" "description" FeatureInProgress "skill" ["precondition"] ["behavior"] Nothing Nothing Nothing Nothing Nothing mempty
feature = minimalFeature {missionFeatureFulfills = Just ["assertion"], missionFeatureMilestone = Just "milestone", missionFeatureWorkerSessionIds = Just ["worker", "worker"], missionFeatureCurrentWorkerSessionId = Just (Just "current"), missionFeatureCompletedWorkerSessionId = Just Nothing}

issue :: DiscoveredIssue
issue = DiscoveredIssue IssueNonBlocking "reported issue" (Just "suggested fix") mempty

command :: VerificationCommand
command = VerificationCommand "not-executed" (-1.25) "reported observation" mempty

check :: InteractiveCheck
check = InteractiveCheck "reported action" "reported result" mempty

verification :: Verification
verification = Verification [command] (Just [check]) mempty

test :: HandoffTestCase
test = HandoffTestCase "test name" "reported behavior" mempty

file :: HandoffTestFile
file = HandoffTestFile "not-read" [test] mempty

tests :: HandoffTests
tests = HandoffTests [file] "reported coverage" (Just ["updated path"]) mempty

deviation :: SkillDeviation
deviation = SkillDeviation "step" "alternative" "reason" mempty

feedback :: SkillFeedback
feedback = SkillFeedback False [deviation] (Just ["suggestion"]) mempty

handoff :: Handoff
handoff = Handoff "reported implementation" "reported incomplete work" verification tests [issue] (Just "reported summary") (Just feedback) mempty

dismissal :: DismissalRecord
dismissal = DismissalRecord DismissedCriticalContext "feature-id" "summary" "justification" mempty

worker :: WorkerStateInfo
worker = WorkerStateInfo "unparsed start" (Just "unparsed completion") (Just (-1.25)) mempty

invocation :: SubagentInvocationSummary
invocation = SubagentInvocationSummary "child-id" SubagentPending "type" "description" (Just 1.25) (Just (-2.5)) mempty

stateChange :: MissionStateChanged
stateChange = MissionStateChanged MissionOrchestratorTurn (Just "unparsed update") mempty

featuresChange :: MissionFeaturesChanged
featuresChange = MissionFeaturesChanged [feature] mempty

heartbeat :: MissionHeartbeat
heartbeat = MissionHeartbeat "unparsed heartbeat" mempty

started :: MissionWorkerStarted
started = MissionWorkerStarted "worker" mempty

completed :: MissionWorkerCompleted
completed = MissionWorkerCompleted "worker" (-0.5) mempty

minimalFeatureJSON, featureJSON, issueJSON, commandJSON, checkJSON, verificationJSON, testJSON, fileJSON, testsJSON, deviationJSON, feedbackJSON, handoffJSON, dismissalJSON, workerJSON, invocationJSON, stateChangeJSON, featuresChangeJSON, heartbeatJSON, startedJSON, completedJSON :: Object
minimalFeatureJSON = KeyMap.fromList ["id" .= String "feature-id", "description" .= String "description", "status" .= String "in_progress", "skillName" .= String "skill", "preconditions" .= [String "precondition"], "expectedBehavior" .= [String "behavior"]]
featureJSON = KeyMap.union minimalFeatureJSON (KeyMap.fromList ["fulfills" .= [String "assertion"], "milestone" .= String "milestone", "workerSessionIds" .= [String "worker", String "worker"], "currentWorkerSessionId" .= String "current", "completedWorkerSessionId" .= Null])
issueJSON = KeyMap.fromList ["severity" .= String "non_blocking", "description" .= String "reported issue", "suggestedFix" .= String "suggested fix"]
commandJSON = KeyMap.fromList ["command" .= String "not-executed", "exitCode" .= Number (-1.25), "observation" .= String "reported observation"]
checkJSON = KeyMap.fromList ["action" .= String "reported action", "observed" .= String "reported result"]
verificationJSON = KeyMap.fromList ["commandsRun" .= [Object commandJSON], "interactiveChecks" .= [Object checkJSON]]
testJSON = KeyMap.fromList ["name" .= String "test name", "verifies" .= String "reported behavior"]
fileJSON = KeyMap.fromList ["file" .= String "not-read", "cases" .= [Object testJSON]]
testsJSON = KeyMap.fromList ["added" .= [Object fileJSON], "coverage" .= String "reported coverage", "updated" .= [String "updated path"]]
deviationJSON = KeyMap.fromList ["step" .= String "step", "whatIDidInstead" .= String "alternative", "why" .= String "reason"]
feedbackJSON = KeyMap.fromList ["followedProcedure" .= False, "deviations" .= [Object deviationJSON], "suggestedChanges" .= [String "suggestion"]]
handoffJSON = KeyMap.fromList ["whatWasImplemented" .= String "reported implementation", "whatWasLeftUndone" .= String "reported incomplete work", "verification" .= Object verificationJSON, "tests" .= Object testsJSON, "discoveredIssues" .= [Object issueJSON], "salientSummary" .= String "reported summary", "skillFeedback" .= Object feedbackJSON]
dismissalJSON = KeyMap.fromList ["type" .= String "critical_context", "sourceFeatureId" .= String "feature-id", "summary" .= String "summary", "justification" .= String "justification"]
workerJSON = KeyMap.fromList ["startedAt" .= String "unparsed start", "completedAt" .= String "unparsed completion", "exitCode" .= Number (-1.25)]
invocationJSON = KeyMap.fromList ["childSessionId" .= String "child-id", "status" .= String "pending", "subagentType" .= String "type", "description" .= String "description", "toolUseCount" .= Number 1.25, "durationMs" .= Number (-2.5)]
stateChangeJSON = KeyMap.fromList ["type" .= String "mission_state_changed", "state" .= String "orchestrator_turn", "updatedAt" .= String "unparsed update"]
featuresChangeJSON = KeyMap.fromList ["type" .= String "mission_features_changed", "features" .= [Object featureJSON]]
heartbeatJSON = KeyMap.fromList ["type" .= String "mission_heartbeat", "timestamp" .= String "unparsed heartbeat"]
startedJSON = KeyMap.fromList ["type" .= String "mission_worker_started", "workerSessionId" .= String "worker"]
completedJSON = KeyMap.fromList ["type" .= String "mission_worker_completed", "workerSessionId" .= String "worker", "exitCode" .= Number (-0.5)]
