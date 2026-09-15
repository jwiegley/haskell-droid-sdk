{-# LANGUAGE OverloadedStrings #-}

module DaemonSoftwareFactorySpec (softwareFactoryTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.SoftwareFactory
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (enumSchemaTest, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

softwareFactoryTests :: Value -> TestTree
softwareFactoryTests schema =
  testGroup
    "Daemon Software Factory"
    [ enumSchemaTest "workstream states" (entryEnum "SfWorkstreamEntrySchema" "state") (Proxy @SfWorkstreamState),
      enumSchemaTest "signal statuses" (entryEnum "SfSignalEntrySchema" "status") (Proxy @SfSignalStatus),
      enumSchemaTest "change statuses" (entryEnum "SfChangeEntrySchema" "status") (Proxy @SfChangeStatus),
      enumSchemaTest "review decisions" (Right (toJSON (["approved", "declined", "commented"] :: [Text]))) (Proxy @SfReviewDecision),
      enumSchemaTest "activity kinds" (entryEnum "SfActivityEntrySchema" "kind") (Proxy @SfActivityKind),
      enumSchemaTest "activity statuses" (entryEnum "SfActivityEntrySchema" "status") (Proxy @SfActivityStatus),
      enumSchemaTest "review statuses" (entryEnum "SfActivityEntrySchema" "reviewStatus") (Proxy @SfReviewStatus),
      enumSchemaTest "event severity" (entryEnum "SfEventEntrySchema" "severity") (Proxy @SfEventSeverity),
      enumSchemaTest "event stages" (entryEnum "SfEventEntrySchema" "stage") (Proxy @SfEventStage),
      enumSchemaTest "access modes" (entryEnum "SfWorkstreamEntrySchema" "accessMode") (Proxy @SfAccessMode),
      testCase "public entry projections enforce required/optional fields and remove private storage data" $ do
        checkEntry (Proxy @SfWorkstream) "SfWorkstreamEntrySchema" "SfWorkstream <redacted>" workstreamWire ["privateStore"]
        checkEntry (Proxy @SfSignal) "SfSignalEntrySchema" "SfSignal <redacted>" signalWire ["payload"]
        checkEntry (Proxy @SfChange) "SfChangeEntrySchema" "SfChange <redacted>" changeWire ["sessionId", "workerId", "retryCount", "claimedAt"]
        checkEntry (Proxy @SfActivity) "SfActivityEntrySchema" "SfActivity <redacted>" activityWire ["workerId", "retryCount", "claimedAt"]
        checkEntry (Proxy @SfEvent) "SfEventEntrySchema" "SfEvent <redacted>" eventWire ["privateStore"]
        workstream <- decodeValue @SfWorkstream workstreamWire
        sfWorkstreamWorkerConcurrency workstream @?= 9007199254740993
        sfWorkstreamDashboardPort workstream @?= Just (-2)
        sfWorkstreamConnectors workstream @?= [Null, object ["unmodeled" .= False]]
        sfWorkstreamCreatedAt workstream @?= "opaque creation"
        sfWorkstreamRequireApproval workstream @?= False
        activity <- decodeValue @SfActivity activityWire
        sfActivitySessionId activity @?= Just ""
        sfActivityReviewerId activity @?= Just "reviewer"
        rejects (Proxy @SfWorkstream) (set "workerConcurrency" (Number 0.5) workstreamWire)
        rejects (Proxy @SfWorkstream) (set "dashboardPort" (Number 0.5) workstreamWire),
      testCase "create and update keep full numeric domains, exact options, and no invented defaults" $ do
        toJSON (defaultSfCreateWorkstreamParams "" " ") @?= object ["title" .= String "", "goal" .= String " "]
        toJSON (defaultSfUpdateWorkstreamParams "") @?= object ["idOrSlug" .= String ""]
        roundTrip (Proxy @SfCreateWorkstreamParams) createWire
        roundTrip (Proxy @SfUpdateWorkstreamParams) updateWire
        create <- decodeValue @SfCreateWorkstreamParams createWire
        sfCreateWorkerConcurrency create @?= Just 0.5
        sfCreateStewardConcurrency create @?= Just (-0.5)
        sfCreateTitle create @?= "  title  "
        sfCreateReviewers create @?= Just []
        forM_ (KeyMap.keys (objectFields createWire)) $ \key -> rejects (Proxy @SfCreateWorkstreamParams) (set key Null createWire)
        forM_ (KeyMap.keys (objectFields updateWire)) $ \key -> if key == "icon" then pure () else rejects (Proxy @SfUpdateWorkstreamParams) (set key Null updateWire)
        forM_ [(Nothing, object ["idOrSlug" .= String ""]), (Just Nothing, object ["idOrSlug" .= String "", "icon" .= Null]), (Just (Just ""), object ["idOrSlug" .= String "", "icon" .= String ""])] $ \(icon, expected) -> do
          let params = (defaultSfUpdateWorkstreamParams "") {sfUpdateIcon = icon}
          toJSON params @?= expected
          decodeValue @SfUpdateWorkstreamParams expected >>= (@?= params)
        rejects (Proxy @SfUpdateWorkstreamParams) (object ["idOrSlug" .= String "", "icon" .= False])
        toJSON ((defaultSfUpdateWorkstreamParams "") {sfUpdateAdditionalFields = KeyMap.fromList ["idOrSlug" .= String "wrong", "state" .= String "active", "icon" .= Null, "extension" .= False]}) @?= object ["idOrSlug" .= String "", "extension" .= False],
      testCase "queries retain absent filters, zero/fractional limits and false flags" $ do
        toJSON (SfListWorkstreamsParams Nothing mempty) @?= object []
        toJSON (defaultSfListParams @SfSignalStatus) @?= object []
        toJSON defaultSfListActivitiesParams @?= object []
        toJSON defaultSfListEventsParams @?= object []
        roundTrip (Proxy @SfListSignalsParams) signalsFilterWire
        roundTrip (Proxy @SfListChangesParams) changesFilterWire
        roundTrip (Proxy @SfListActivitiesParams) activitiesFilterWire
        roundTrip (Proxy @SfListEventsParams) eventsFilterWire
        forM_ ["workstreamIdOrSlug", "status", "limit"] $ \key -> do
          rejects (Proxy @SfListSignalsParams) (object [key .= Null])
          rejects (Proxy @SfListChangesParams) (object [key .= Null])
        forM_ ["workstreamIdOrSlug", "changeId", "awaitingReviewOnly", "limit"] $ \key -> rejects (Proxy @SfListActivitiesParams) (object [key .= Null])
        forM_ ["workstreamIdOrSlug", "unreadOnly", "limit"] $ \key -> rejects (Proxy @SfListEventsParams) (object [key .= Null])
        rejects (Proxy @SfListWorkstreamsParams) (object ["state" .= Null])
        toJSON ((defaultSfListParams @SfSignalStatus) {sfListAdditionalFields = KeyMap.fromList ["status" .= String "new", "limit" .= (100 :: Int), "extension" .= False]}) @?= object ["extension" .= False],
      testCase "absence, old deletion rows, counts and optional review follow-ups remain reports" $ do
        roundTrip (Proxy @SfGetWorkstreamResult) (object [])
        roundTrip (Proxy @SfGetWorkstreamResult) foundWire
        rejects (Proxy @SfGetWorkstreamResult) (object ["workstream" .= Null])
        rejects (Proxy @SfWorkstreamResult) (object [])
        rejects (Proxy @SfWorkstreamResult) (object ["workstream" .= Null])
        removed <- decodeValue @SfDeleteWorkstreamResult deletionWire
        sfAutomationsDeleted removed @?= (-0.5)
        sfWorkstreamDirDeleted removed @?= False
        sfRemoteAutomationsDeleted removed @?= Just 1.25
        toJSON (sfDeletedWorkstream removed) @?= workstreamWire
        roundTrip (Proxy @SfDeleteWorkstreamResult) (Object (KeyMap.delete "remoteAutomationsDeleted" (objectFields deletionWire)))
        rejects (Proxy @SfDeleteWorkstreamResult) (set "remoteAutomationsDeleted" Null deletionWire)
        roundTrip (Proxy @SfResolveActivityReviewResult) reviewedWire
        roundTrip (Proxy @SfResolveActivityReviewResult) (object ["activity" .= activityWire])
        rejects (Proxy @SfResolveActivityReviewResult) (object ["activity" .= activityWire, "followUpActivity" .= Null])
        roundTrip (Proxy @SfMarkedEventsResult) (object ["marked" .= Number (-0.5)])
        rejects (Proxy @SfMarkedEventsResult) (object ["marked" .= Null]),
      testCase "read can omit IDs but unread requires them; neither drops an explicit empty list" $ do
        toJSON (SfMarkEventsReadParams "" Nothing mempty) @?= readAllWire
        toJSON (SfMarkEventsReadParams "" (Just []) mempty) @?= markEmptyWire
        toJSON (SfMarkEventsUnreadParams "" [] mempty) @?= markEmptyWire
        roundTrip (Proxy @SfMarkEventsReadParams) markIdsWire
        roundTrip (Proxy @SfMarkEventsUnreadParams) markIdsWire
        rejects (Proxy @SfMarkEventsUnreadParams) readAllWire
        rejects (Proxy @SfMarkEventsReadParams) (set "eventIds" Null markIdsWire)
        rejects (Proxy @SfMarkEventsUnreadParams) (set "eventIds" Null markIdsWire)
        rejects (Proxy @SfMarkEventsUnreadParams) (set "eventIds" (toJSON [False]) markIdsWire)
        rejects (Proxy @SfResolveActivityReviewParams) (object ["activityId" .= String "", "decision" .= String "awaiting"])
        rejects (Proxy @SfResolveActivityReviewParams) (set "comment" Null reviewParamsWire)
        roundTrip (Proxy @SfResolveActivityReviewParams) reviewParamsWire,
      testCase "all twelve high-level methods preserve exact requests and projected results without session startup" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withSfPeer Normal trace ready $ \endpoint -> Daemon.withConnection (options endpoint) $ \connection ->
          forM_ (zip wireCases (highCalls connection)) $ \((_, _, _, expected), request) -> request >>= (@?= expected)
        readIORef trace >>= (@?= map String ("daemon.authenticate" : map (\(method, _, _, _) -> method) wireCases)) . map (field "method"),
      testCase "remote and malformed replies stay errors on a reusable connection" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withSfPeer mode trace ready $ \endpoint -> Daemon.withConnection (options endpoint) $ \connection ->
              forM_ (highCalls connection) $ \request ->
                try @RpcResultError request >>= \case
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected Software Factory result",
      testCase "cancellation preserves async identity without replay or remote compensation" $
        bounded $
          forM_ (zip [0 :: Int ..] wireCases) $ \(index, (method, _, _, _)) -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withSfPeer (Held method) trace ready $ \endpoint -> Daemon.withConnection (options endpoint) $ \connection -> do
              case drop index (highCalls connection) of
                request : _ -> withAsync request $ \pending -> do
                  takeMVar ready
                  cancel pending
                  waitCatch pending >>= \case
                    Left cause -> fromException cause @?= Just AsyncCancelled
                    Right _ -> assertFailure "Cancelled request returned"
                [] -> assertFailure "Missing high-level request"
              if method == "daemon.sf.list_workstreams"
                then void (Daemon.sfGetWorkstream connection (SfWorkstreamTarget "missing" mempty))
                else void (Daemon.sfListWorkstreams connection (SfListWorkstreamsParams Nothing mempty))
            methods <- map (field "method") <$> readIORef trace
            methods @?= map String ["daemon.authenticate", method, if method == "daemon.sf.list_workstreams" then "daemon.sf.get_workstream" else "daemon.sf.list_workstreams"],
      testCase "disconnection is sticky rather than fabricated resource success" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withSfPeer Dropped trace ready $ \endpoint -> Daemon.withConnection (options endpoint) $ \connection ->
          forM_ (highCalls connection) $ \request -> try @RpcChannelError request >>= (@?= Left RpcChannelReadFailure),
      testCase "low-level methods preserve caller metadata and honor zero deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          forM_ (zip [0 :: Int ..] wireCases) $ \(index, (method, params, raw, expected)) -> do
            let metadata = KeyMap.fromList ["id" .= String "wrong", "method" .= String "wrong", "params" .= Null, "trace" .= False]
                cfg = Client.CallOptions (method <> "-" <> Text.pack (show index)) (WithEnvelope (Just "1.201.1") Nothing metadata) (Just 1000000)
            case drop index (lowCalls channel cfg) of
              request : _ -> withAsync request $ \pending -> do
                sent <- atomically (readTQueue outgoing)
                field "id" sent @?= String (Client.callRequestId cfg)
                field "method" sent @?= String method
                field "params" sent @?= params
                field "trace" sent @?= Bool False
                atomically (writeTQueue incoming (response sent raw))
                wait pending >>= (@?= expected)
              [] -> assertFailure "Missing low-level request"
          forM_ (lowCalls channel (Client.CallOptions "expired" (WithEnvelope (Just "1.201.1") Nothing mempty) (Just 0))) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]
  where
    entryEnum name key = schemaAt ["definitions", name, "properties", key, "enum"] schema
    checkEntry :: (FromJSON a, ToJSON a, Show a) => Proxy a -> Key -> String -> Value -> [Key] -> IO ()
    checkEntry (_ :: Proxy a) name display golden hidden = do
      requiredValue <- either assertFailure pure (schemaAt ["definitions", name, "required"] schema)
      required <- decodeValue @[Key] requiredValue
      let fields = objectFields golden
      roundTrip (Proxy @a) golden
      roundTrip (Proxy @a) (Object (KeyMap.filterWithKey (\key _ -> key `elem` required) fields))
      forM_ required $ \key -> rejects (Proxy @a) (Object (KeyMap.delete key fields))
      forM_ (KeyMap.keys fields) $ \key -> rejects (Proxy @a) (set key Null golden)
      forM_ hidden $ \key -> decodeValue @a (set key (object ["private" .= String "secret"]) golden) >>= (@?= golden) . toJSON
      value <- decodeValue @a golden
      show value @?= display

workstreamWire :: Value
workstreamWire = object ["id" .= String "workstream", "slug" .= String "slug", "title" .= String "  title  ", "goal" .= String "goal", "state" .= String "active", "userId" .= String "original-owner", "creationSessionId" .= String "", "machineId" .= String "local", "connectors" .= [Null, object ["unmodeled" .= False]], "repos" .= [String ""], "workerConcurrency" .= (9007199254740993 :: Integer), "stewardConcurrency" .= (-2 :: Int), "dashboardPort" .= (-2 :: Int), "requireApproval" .= False, "reviewers" .= ([] :: [Text]), "createdAt" .= String "opaque creation", "updatedAt" .= String "", "deletedAt" .= String "", "accessMode" .= String "private", "autoMerge" .= False, "dashboardHtml" .= String "<script>NOT EXECUTED</script>", "dashboardUrl" .= String "not a URL", "executionTemplateId" .= String "", "icon" .= String "", "owningTeamId" .= String "", "requireChangeApproval" .= False]

signalWire :: Value
signalWire = object ["id" .= String "signal", "workstreamId" .= String "workstream", "source" .= String "source", "externalId" .= String "", "fingerprint" .= String "fingerprint", "title" .= String "title", "summary" .= String "summary", "status" .= String "new", "changeId" .= String "", "createdAt" .= String "", "updatedAt" .= String "opaque update"]

changeWire :: Value
changeWire = object ["id" .= String "change", "workstreamId" .= String "workstream", "title" .= String "title", "description" .= String "description", "status" .= String "declined", "result" .= String "", "error" .= String "", "completedAt" .= String "", "createdAt" .= String "created", "updatedAt" .= String "updated"]

activityWire :: Value
activityWire = object ["id" .= String "activity", "changeId" .= String "change", "workstreamId" .= String "workstream", "kind" .= String "investigation", "status" .= String "completed", "reviewStatus" .= String "awaiting", "reviewComment" .= String "", "reviewedAt" .= String "", "result" .= String "", "error" .= String "", "sessionId" .= String "", "parentActivityId" .= String "", "completedAt" .= String "", "createdAt" .= String "created", "updatedAt" .= String "updated", "changeTitle" .= String "change title", "changeDescription" .= String "change description", "reviewerId" .= String "reviewer"]

eventWire :: Value
eventWire = object ["id" .= String "event", "workstreamId" .= String "workstream", "severity" .= String "warning", "stage" .= String "health", "title" .= String "title", "detail" .= String "", "read" .= False, "createdAt" .= String "opaque"]

createWire :: Value
createWire = object ["title" .= String "  title  ", "goal" .= String "", "slug" .= String "", "userId" .= String "", "creationSessionId" .= String "", "connectors" .= [Null, Bool False], "repos" .= ([] :: [Text]), "workerConcurrency" .= Number 0.5, "stewardConcurrency" .= Number (-0.5), "requireApproval" .= False, "reviewers" .= ([] :: [Text]), "accessMode" .= String "private", "autoMerge" .= False, "executionTemplateId" .= String "", "owningTeamId" .= String "", "requireChangeApproval" .= False]

updateWire :: Value
updateWire = object ["idOrSlug" .= String "slug", "state" .= String "paused", "title" .= String "", "goal" .= String "", "userId" .= String "ignored-owner-update", "creationSessionId" .= String "", "connectors" .= ([] :: [Value]), "repos" .= ([] :: [Text]), "workerConcurrency" .= Number (-0.5), "stewardConcurrency" .= Number 0.5, "dashboardPort" .= Number 0.5, "requireApproval" .= False, "reviewers" .= ([] :: [Text]), "autoMerge" .= False, "dashboardHtml" .= String "<html>NOT RENDERED</html>", "dashboardUrl" .= String "", "icon" .= Null, "requireChangeApproval" .= False]

signalsFilterWire, changesFilterWire, activitiesFilterWire, eventsFilterWire, readAllWire, markEmptyWire, markIdsWire, reviewParamsWire, foundWire, deletionWire, reviewedWire :: Value
signalsFilterWire = object ["workstreamIdOrSlug" .= String "", "status" .= String "ignored", "limit" .= Number (-0.5)]
changesFilterWire = object ["workstreamIdOrSlug" .= String "", "status" .= String "in_review", "limit" .= Number 0]
activitiesFilterWire = object ["workstreamIdOrSlug" .= String "", "changeId" .= String "", "awaitingReviewOnly" .= False, "limit" .= Number 0.5]
eventsFilterWire = object ["workstreamIdOrSlug" .= String "", "unreadOnly" .= False, "limit" .= Number (-0.5)]
readAllWire = object ["workstreamIdOrSlug" .= String ""]
markEmptyWire = object ["workstreamIdOrSlug" .= String "", "eventIds" .= ([] :: [Text])]
markIdsWire = object ["workstreamIdOrSlug" .= String "", "eventIds" .= [String "event", String "event"]]
reviewParamsWire = object ["activityId" .= String "activity", "decision" .= String "commented", "comment" .= String ""]
foundWire = object ["workstream" .= workstreamWire]
deletionWire = object ["workstream" .= workstreamWire, "automationsDeleted" .= Number (-0.5), "workstreamDirDeleted" .= False, "remoteAutomationsDeleted" .= Number 1.25]
reviewedWire = object ["activity" .= activityWire, "followUpActivity" .= activityWire]

wireCases :: [(Text, Value, Value, Value)]
wireCases =
  [ same "daemon.sf.list_workstreams" (object []) (object ["workstreams" .= [workstreamWire]]),
    same "daemon.sf.list_workstreams" (object ["state" .= String "draft"]) (object ["workstreams" .= ([] :: [Value])]),
    same "daemon.sf.get_workstream" (object ["idOrSlug" .= String "slug"]) foundWire,
    same "daemon.sf.get_workstream" (object ["idOrSlug" .= String "missing"]) (object []),
    same "daemon.sf.create_workstream" createWire foundWire,
    same "daemon.sf.update_workstream" updateWire foundWire,
    same "daemon.sf.delete_workstream" (object ["idOrSlug" .= String "slug"]) deletionWire,
    ("daemon.sf.list_signals", signalsFilterWire, object ["signals" .= [set "payload" (object ["secret" .= True]) signalWire]], object ["signals" .= [signalWire]]),
    ("daemon.sf.list_changes", changesFilterWire, object ["changes" .= [set "workerId" (String "private") (set "sessionId" (String "private") changeWire)]], object ["changes" .= [changeWire]]),
    ("daemon.sf.list_activities", activitiesFilterWire, object ["activities" .= [set "workerId" (String "private") activityWire]], object ["activities" .= [activityWire]]),
    same "daemon.sf.resolve_activity_review" reviewParamsWire reviewedWire,
    same "daemon.sf.resolve_activity_review" (object ["activityId" .= String "activity", "decision" .= String "approved"]) (object ["activity" .= activityWire]),
    same "daemon.sf.resolve_activity_review" (object ["activityId" .= String "activity", "decision" .= String "declined"]) (object ["activity" .= activityWire]),
    same "daemon.sf.list_events" eventsFilterWire (object ["events" .= [eventWire]]),
    same "daemon.sf.mark_events_read" readAllWire (object ["marked" .= Number 2]),
    same "daemon.sf.mark_events_read" markEmptyWire (object ["marked" .= Number 0]),
    same "daemon.sf.mark_events_unread" markIdsWire (object ["marked" .= Number (-0.5)])
  ]
  where
    same method params result = (method, params, result, result)

highCalls :: Daemon.DaemonConnection -> [IO Value]
highCalls connection =
  [ toJSON <$> Daemon.sfListWorkstreams connection (SfListWorkstreamsParams Nothing mempty),
    toJSON <$> Daemon.sfListWorkstreams connection (SfListWorkstreamsParams (Just SfDraft) mempty),
    toJSON <$> Daemon.sfGetWorkstream connection (SfWorkstreamTarget "slug" mempty),
    toJSON <$> Daemon.sfGetWorkstream connection (SfWorkstreamTarget "missing" mempty),
    decoded (Daemon.sfCreateWorkstream connection) createWire,
    decoded (Daemon.sfUpdateWorkstream connection) updateWire,
    toJSON <$> Daemon.sfDeleteWorkstream connection (SfWorkstreamTarget "slug" mempty),
    decoded (Daemon.sfListSignals connection) signalsFilterWire,
    decoded (Daemon.sfListChanges connection) changesFilterWire,
    decoded (Daemon.sfListActivities connection) activitiesFilterWire,
    decoded (Daemon.sfResolveActivityReview connection) reviewParamsWire,
    toJSON <$> Daemon.sfResolveActivityReview connection (SfResolveActivityReviewParams "activity" SfApprove Nothing mempty),
    toJSON <$> Daemon.sfResolveActivityReview connection (SfResolveActivityReviewParams "activity" SfDecline Nothing mempty),
    decoded (Daemon.sfListEvents connection) eventsFilterWire,
    toJSON <$> Daemon.sfMarkEventsRead connection (SfMarkEventsReadParams "" Nothing mempty),
    toJSON <$> Daemon.sfMarkEventsRead connection (SfMarkEventsReadParams "" (Just []) mempty),
    decoded (Daemon.sfMarkEventsUnread connection) markIdsWire
  ]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel cfg =
  [ toJSON <$> Client.listDaemonSfWorkstreams channel cfg (SfListWorkstreamsParams Nothing mempty),
    toJSON <$> Client.listDaemonSfWorkstreams channel cfg (SfListWorkstreamsParams (Just SfDraft) mempty),
    toJSON <$> Client.getDaemonSfWorkstream channel cfg (SfWorkstreamTarget "slug" mempty),
    toJSON <$> Client.getDaemonSfWorkstream channel cfg (SfWorkstreamTarget "missing" mempty),
    decoded (Client.createDaemonSfWorkstream channel cfg) createWire,
    decoded (Client.updateDaemonSfWorkstream channel cfg) updateWire,
    toJSON <$> Client.deleteDaemonSfWorkstream channel cfg (SfWorkstreamTarget "slug" mempty),
    decoded (Client.listDaemonSfSignals channel cfg) signalsFilterWire,
    decoded (Client.listDaemonSfChanges channel cfg) changesFilterWire,
    decoded (Client.listDaemonSfActivities channel cfg) activitiesFilterWire,
    decoded (Client.resolveDaemonSfActivityReview channel cfg) reviewParamsWire,
    toJSON <$> Client.resolveDaemonSfActivityReview channel cfg (SfResolveActivityReviewParams "activity" SfApprove Nothing mempty),
    toJSON <$> Client.resolveDaemonSfActivityReview channel cfg (SfResolveActivityReviewParams "activity" SfDecline Nothing mempty),
    decoded (Client.listDaemonSfEvents channel cfg) eventsFilterWire,
    toJSON <$> Client.markDaemonSfEventsRead channel cfg (SfMarkEventsReadParams "" Nothing mempty),
    toJSON <$> Client.markDaemonSfEventsRead channel cfg (SfMarkEventsReadParams "" (Just []) mempty),
    decoded (Client.markDaemonSfEventsUnread channel cfg) markIdsWire
  ]

decoded :: (FromJSON a, ToJSON b) => (a -> IO b) -> Value -> IO Value
decoded request value = decodeValue value >>= fmap toJSON . request

data Mode = Normal | Rejected | Malformed | Held Text | Dropped deriving stock (Eq)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options endpoint = (Daemon.defaultDaemonOptions endpoint (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withSfPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withSfPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      request <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      field "factoryProtocolVersion" request @?= String "1.201.1"
      modifyIORef' trace (<> [request])
      pure request
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      sendFrame connection ["type" .= String "response", "id" .= field "id" auth, "result" .= object ["userId" .= String "user", "orgId" .= String "org"]]
      forever $ do
        request <- readFrame connection
        method <- decodeValue @Text (field "method" request)
        (_, _, raw, _) <- maybe (assertFailure "Unexpected Software Factory request/params") pure (find (\(name, params, _, _) -> name == method && params == field "params" request) wireCases)
        case mode of
          Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Software Factory rejected"]]
          Malformed -> WS.sendTextData connection (encode (Object (response request Null)))
          Held held | held == method -> putMVar ready ()
          Dropped -> WS.sendClose connection ("fixture disconnect" :: Text)
          _ -> WS.sendTextData connection (encode (Object (response request raw)))

response :: Object -> Value -> Object
response request result = envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . Object . envelope

envelope :: [Pair] -> Object
envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

set :: Key -> Value -> Value -> Value
set key value = Object . KeyMap.insert key value . objectFields

objectFields :: Value -> Object
objectFields (Object fields) = fields
objectFields _ = error "Expected object in fixture"

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

roundTrip :: (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip (_ :: Proxy a) value = decodeValue @a value >>= (@?= value) . toJSON
