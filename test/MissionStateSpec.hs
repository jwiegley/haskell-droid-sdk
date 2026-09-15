{-# LANGUAGE OverloadedStrings #-}

module MissionStateSpec (missionStateTests, missionSnapshotWire) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid (DroidEvent (..))
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Schema.Mission
import Factory.Droid.Schema.Notifications (SessionTokenUsageChanged (..))
import Factory.Droid.Schema.Usage (TokenUsage (..))
import SchemaTest (rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

missionStateTests :: Value -> TestTree
missionStateTests schema =
  testGroup
    "Mission state transformations"
    [ testCase "snapshot codec preserves reports without inferring aggregate consistency" $ do
        snapshot <- decodeValue @MissionSnapshot missionSnapshotWire
        toJSON snapshot @?= missionSnapshotWire
        missionSnapshotTitle snapshot @?= Just ""
        missionSnapshotWorkers snapshot @?= ["worker-b", "worker-a", "worker-b"]
        missionSnapshotTokenUsage snapshot @?= Just (usage 999 Nothing)
        required <- either assertFailure (decodeValue @[Key]) (schemaAt ["definitions", "MissionStateSchema", "required"] schema)
        properties <- either assertFailure (decodeValue @Object) (schemaAt ["definitions", "MissionStateSchema", "properties"] schema)
        forM_ required $ \key -> rejects (Proxy @MissionSnapshot) (deleteField key missionSnapshotWire)
        forM_ (filter (`notElem` required) (KeyMap.keys properties)) $ \key -> rejects (Proxy @MissionSnapshot) (insertField key Null missionSnapshotWire)
        show snapshot @?= "MissionSnapshot <redacted>",
      testCase "pristine stores render defaults but do not overwrite an observed destination" $ do
        let pristine = Mission.emptyMissionStore
            target = Mission.setTitle (Just "") (Mission.setState MissionRunning pristine)
        missionSnapshotState (Mission.missionSnapshot pristine) @?= MissionAwaitingInput
        missionSnapshotTitle (Mission.missionSnapshot pristine) @?= Nothing
        missionSnapshotWorkerStates (Mission.missionSnapshot pristine) @?= Nothing
        Mission.mergeFrom pristine target @?= target
        Mission.mergeFrom target pristine @?= target,
      testCase "explicit default state, null title and empty lists win merges without erasing workers or usage" $ do
        target <- populatedStore
        let source = Mission.setState MissionAwaitingInput (Mission.setTitle Nothing (Mission.setFeatures [] (Mission.setProgressLog [] Mission.emptyMissionStore)))
            result = Mission.missionSnapshot (Mission.mergeFrom source target)
        missionSnapshotState result @?= MissionAwaitingInput
        missionSnapshotTitle result @?= Nothing
        missionSnapshotFeatures result @?= []
        missionSnapshotProgress result @?= []
        missionSnapshotWorkers result @?= ["worker-b", "worker-a"]
        missionSnapshotTokenUsage result @?= Just (TokenUsage 4 4 0 2 0 (Just 1.25) mempty),
      testCase "features register only roster workers and preserve ordered unique identities" $ do
        feature <- decodeValue @MissionFeature (object ["id" .= String "f", "description" .= String "d", "status" .= String "pending", "skillName" .= String "s", "preconditions" .= ([] :: [Value]), "expectedBehavior" .= ([] :: [Value]), "workerSessionIds" .= [String "worker-b", String "worker-a", String "worker-b", String ""], "currentWorkerSessionId" .= String "legacy-only"])
        let store = Mission.setFeatures [feature] Mission.emptyMissionStore
        missionSnapshotWorkers (Mission.missionSnapshot store) @?= ["worker-b", "worker-a", ""]
        missionSnapshotWorkerStates (Mission.missionSnapshot store) @?= Nothing
        Mission.hasWorkerSession "" store @?= True
        Mission.hasWorkerSession "legacy-only" store @?= False
        missionSnapshotWorkers (Mission.missionSnapshot (Mission.setFeatures [] store)) @?= ["worker-b", "worker-a", ""],
      testCase "progress replacement preserves worker start/history and missing failure exit codes" $ do
        target <- populatedStore
        let result = Mission.missionSnapshot target
        workerState "worker-b" result @?= WorkerStateInfo "first" (Just "failed-later") (Just 3) mempty
        workerState "worker-a" result @?= WorkerStateInfo "selected" Nothing Nothing mempty
        let shortened = Mission.setProgressLog [] target
        missionSnapshotProgress (Mission.missionSnapshot shortened) @?= []
        missionSnapshotWorkerStates (Mission.missionSnapshot shortened) @?= missionSnapshotWorkerStates result
        let failed = ProgressLogEntry "another failure" (WorkerFailedProgress (WorkerFailureDetails "spawn" "failure" (Just "worker-b") (Just (-1.5)) Nothing)) mempty
        workerExitCode (workerState "worker-b" (Mission.missionSnapshot (Mission.setProgressLog [failed] target))) @?= Just (-1.5),
      testCase "new progress workers follow reference object-key ordering" $ do
        let entries = map (started "now") ["10", "2", "z"]
        missionSnapshotWorkers (Mission.missionSnapshot (Mission.setProgressLog entries Mission.emptyMissionStore)) @?= ["2", "10", "z"]
        let prior = Mission.addWorkerAt "prior" "kept" (Mission.addWorkerAt "prior" "5" Mission.emptyMissionStore)
            mixed = map (started "now") ["10", "2", "z", "01", "4294967295", "4294967294", "a", "0"]
        missionSnapshotWorkers (Mission.missionSnapshot (Mission.setProgressLog mixed prior)) @?= ["5", "kept", "0", "2", "10", "4294967294", "z", "01", "4294967295", "a"],
      testCase "explicit worker operations use caller time and completion replaces prior extra fields" $ do
        let state = WorkerStateInfo "" Nothing Nothing (KeyMap.singleton "extra" (Bool False))
            store = Mission.addWorkerWithState "worker" state Mission.emptyMissionStore
        Mission.addWorkerAt "ignored time" "worker" store @?= store
        let completed = Mission.missionSnapshot (Mission.completeWorkerAt "observed" "worker" 0 store)
        workerState "worker" completed @?= WorkerStateInfo "" (Just "observed") (Just 0) mempty
        workerState "new" (Mission.missionSnapshot (Mission.completeWorkerAt "new observation" "new" 1 store)) @?= WorkerStateInfo "new observation" (Just "new observation") (Just 1) mempty
        Mission.hasWorkerSession "__proto__" (Mission.addWorkerAt "observed" "__proto__" store) @?= True,
      testCase "source worker and usage entries override collisions while identities are unioned" $ do
        target <- populatedStore
        let source = Mission.setSessionTokenUsage "worker-b" (usage 7 (Just 0)) (Mission.addWorkerAt observedAt "worker-c" (Mission.addWorkerWithState "worker-b" (WorkerStateInfo "override" Nothing Nothing (KeyMap.singleton "extra" (Bool False))) Mission.emptyMissionStore))
            result = Mission.missionSnapshot (Mission.mergeFrom source target)
        missionSnapshotWorkers result @?= ["worker-b", "worker-a", "worker-c"]
        workerStartedAt (workerState "worker-b" result) @?= "override"
        workerStartedAt (workerState "worker-c" result) @?= observedAt
        missionSnapshotTokenUsage result @?= Just (TokenUsage 10 4 0 2 0 (Just 1.25) mempty),
      testCase "per-session replacement, empty-map clearing and exact numeric totals are distinct" $ do
        let first = (usage 9007199254740993 Nothing) {usageAdditionalFields = KeyMap.singleton "opaque" (Number 99)}
            second = usage (-0.5) (Just 0.125)
            store = Mission.setTokenUsageBySessionId (Map.fromList [("a", first), ("b", second)]) Mission.emptyMissionStore
        missionSnapshotTokenUsage (Mission.missionSnapshot store) @?= Just (TokenUsage 9007199254740992.5 4 0 2 0 (Just 0.125) mempty)
        let replaced = Mission.setSessionTokenUsage "a" (usage 1 Nothing) store
        missionSnapshotTokenUsage (Mission.missionSnapshot replaced) @?= Just (TokenUsage 0.5 4 0 2 0 (Just 0.125) mempty)
        let cleared = Mission.setTokenUsageBySessionId mempty replaced
        missionSnapshotTokenUsage (Mission.missionSnapshot cleared) @?= Nothing
        missionSnapshotSessionUsage (Mission.missionSnapshot cleared) @?= Nothing
        Mission.mergeFrom (Mission.setTokenUsageBySessionId mempty Mission.emptyMissionStore) replaced @?= replaced,
      testCase "snapshot restoration follows load precedence and does not copy aggregate-only usage" $ do
        snapshot <- decodeValue @MissionSnapshot missionSnapshotWire
        let store = Mission.restoreSnapshotAt "restored now" snapshot Mission.emptyMissionStore
            result = Mission.missionSnapshot store
        missionSnapshotWorkers result @?= ["worker-b", "worker-a"]
        workerStartedAt (workerState "worker-b" result) @?= "reported start"
        workerStartedAt (workerState "worker-a" result) @?= "restored now"
        missionSnapshotTokenUsage result @?= Just (TokenUsage 4 4 0 2 0 (Just 1.25) mempty)
        missionSnapshotWorkingDirectory result @?= Nothing
        missionSnapshotWorkingDirectory snapshot @?= Just "/remote/worktree"
        let aggregateOnly = snapshot {missionSnapshotTitle = Nothing, missionSnapshotSessionUsage = Nothing, missionSnapshotWorkerStates = Nothing, missionSnapshotWorkers = []}
            reset = Mission.missionSnapshot (Mission.restoreSnapshotAt "later" aggregateOnly store)
        missionSnapshotTitle reset @?= Nothing
        missionSnapshotTokenUsage reset @?= Nothing
        missionSnapshotWorkers reset @?= ["worker-b", "worker-a"],
      testCase "event application uses first accepted title, explicit observation time and preferred inclusive usage" $ do
        let entries = [ProgressLogEntry "first" (MissionAcceptedProgress "") mempty, ProgressLogEntry "later" (MissionAcceptedProgress "not chosen") mempty]
            progressEvent = MissionProgressEvent (MissionProgressEntry entries mempty)
            store = Mission.applyEventAt "unused" progressEvent Mission.emptyMissionStore
        missionSnapshotTitle (Mission.missionSnapshot store) @?= Just ""
        let startedStore = Mission.applyEventAt "start time" (MissionWorkerStartedEvent (MissionWorkerStarted "worker" mempty)) store
            finishedStore = Mission.applyEventAt "end time" (MissionWorkerCompletedEvent (MissionWorkerCompleted "worker" 0 mempty)) startedStore
            usageEvent = UsageEvent (SessionTokenUsageChanged "worker" (usage 3 Nothing) (Just (usage 999 Nothing)) Nothing mempty)
            result = Mission.applyEventAt "ignored" usageEvent finishedStore
        workerState "worker" (Mission.missionSnapshot result) @?= WorkerStateInfo "start time" (Just "end time") (Just 0) mempty
        missionSnapshotTokenUsage (Mission.missionSnapshot result) @?= Just (TokenUsage 999 2 0 1 0 (Just 0) mempty)
        Mission.applyEventAt "ignored" (MissionHeartbeatEvent (MissionHeartbeat "not a local observation time" mempty)) result @?= result
        Mission.applyEventAt "ignored" (OtherNotificationEvent mempty) result @?= result
    ]

observedAt :: Text
observedAt = "2026-09-09T00:00:00.000Z"

populatedStore :: IO Mission.MissionStore
populatedStore = do
  feature <- decodeValue @MissionFeature (object ["id" .= String "f", "description" .= String "d", "status" .= String "pending", "skillName" .= String "s", "preconditions" .= ([] :: [Value]), "expectedBehavior" .= ([] :: [Value]), "workerSessionIds" .= [String "worker-b", String "worker-a", String "worker-b"]])
  let initial = Mission.setFeatures [feature] (Mission.setTitle (Just "") (Mission.setState MissionRunning Mission.emptyMissionStore))
      completion = WorkerCompletionDetails "worker-b" "f" FeaturePartial False 3 Nothing Nothing Nothing Nothing
      entries = [started "first" "worker-b", ProgressLogEntry "finished" (WorkerCompletedProgress completion) mempty, ProgressLogEntry "failed-later" (WorkerFailedProgress (WorkerFailureDetails "spawn" "failure" (Just "worker-b") Nothing Nothing)) mempty, ProgressLogEntry "selected" (WorkerSelectedFeatureProgress "worker-a" "f") mempty, ProgressLogEntry "unassigned" (WorkerFailedProgress (WorkerFailureDetails "spawn" "failure" Nothing Nothing Nothing)) mempty, started "empty-id" ""]
  pure (Mission.setSessionTokenUsage "worker-a" (usage 3 (Just 1.25)) (Mission.setSessionTokenUsage "worker-b" (usage 1 Nothing) (Mission.setProgressLog entries initial)))

started :: Text -> Text -> ProgressLogEntry
started at identifier = ProgressLogEntry at (WorkerStartedProgress (WorkerStartDetails identifier "spawn" Nothing Nothing Nothing)) mempty

usage :: Scientific -> Maybe Scientific -> TokenUsage
usage input credits = TokenUsage input 2 0 1 0 credits mempty

missionSnapshotWire :: Value
missionSnapshotWire = object ["state" .= String "running", "features" .= ([] :: [Value]), "progressLog" .= [toJSON (started "from progress" "worker-b")], "workerSessionIds" .= [String "worker-b", String "worker-a", String "worker-b"], "title" .= String "", "updatedAt" .= String "opaque update", "workingDirectory" .= String "/remote/worktree", "workerStates" .= object ["worker-b" .= object ["startedAt" .= String "reported start", "future" .= False], "unlisted" .= object ["startedAt" .= String "not imported"]], "tokenUsage" .= usage 999 Nothing, "tokenUsageBySessionId" .= object ["worker-b" .= usage 1 Nothing, "worker-a" .= usage 3 (Just 1.25)], "future" .= object ["retained" .= Null]]

workerState :: Text -> MissionSnapshot -> WorkerStateInfo
workerState identifier snapshot = case missionSnapshotWorkerStates snapshot >>= Map.lookup identifier of
  Just state -> state
  Nothing -> error "Expected worker state in fixture"

insertField :: Key -> Value -> Value -> Value
insertField key value (Object fields) = Object (KeyMap.insert key value fields)
insertField _ _ _ = error "Expected object fixture"

deleteField :: Key -> Value -> Value
deleteField key (Object fields) = Object (KeyMap.delete key fields)
deleteField _ _ = error "Expected object fixture"

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
