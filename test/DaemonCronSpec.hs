{-# LANGUAGE OverloadedStrings #-}

module DaemonCronSpec (cronTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Cron
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Schema.Session (SessionIdParams (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (enumSchemaTest, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

cronTests :: Value -> TestTree
cronTests schema =
  testGroup
    "Daemon crons"
    [ enumSchemaTest "report statuses" (schemaAt ["definitions", "CronStatusSchema", "enum"] schema) (Proxy @CronStatus),
      enumSchemaTest "sources" (Right (toJSON (["loop_command", "cron_tool", "automation", "migration"] :: [Text]))) (Proxy @CronSource),
      enumSchemaTest "editable statuses" (Right (toJSON (["active", "paused"] :: [Text]))) (Proxy @CronUpdateStatus),
      enumSchemaTest "change reasons" (Right (toJSON (["created", "updated", "deleted"] :: [Text]))) (Proxy @CronChangeReason),
      testCase "only prompt and expression use ECMAScript trim/minimum without interpreting cron syntax" $ do
        forM_ [(" \xfeff\ttext\x3000", "text"), (" \x85 ", "\x85"), (" not cron syntax ", "not cron syntax")] $ \(raw, expected) -> do
          value <- maybe (assertFailure "Expected valid cron text") pure (mkCronText raw)
          cronTextValue value @?= expected
          toJSON value @?= String expected
          show value @?= "CronText <redacted>"
        forM_ ["", " \xfeff\x3000\n"] $ \raw -> mkCronText raw @?= Nothing
        rejects (Proxy @CronText) Null
        normalized <- decodeValue @CronSchedule (object ["expression" .= String " \xfeff 0 12 * * * \x3000", "recurring" .= False])
        toJSON normalized @?= scheduleWire,
      testCase "both stored kinds preserve all data while enforcing kind/scope/payload/policy invariants" $ do
        forM_ [False, True] $ \root -> do
          value <- decodeValue @CronRecord (recordWire root)
          toJSON value @?= recordWire root
          show value @?= "CronRecord <redacted>"
          let fields = objectFields (recordWire root)
          forM_ ["version", "id", "status", "source", "schedule", "stats", "createdAt", "updatedAt", "kind", "scope", "runPolicy", "payload"] $ \key -> rejects (Proxy @CronRecord) (Object (KeyMap.delete key fields))
          forM_ (KeyMap.keys fields) $ \key -> when (key /= "extension") (rejects (Proxy @CronRecord) (set key Null (recordWire root)))
          rejects (Proxy @CronRecord) (set "version" (Number 2) (recordWire root))
          rejects (Proxy @CronRecord) (set "scope" (field "scope" (objectFields (recordWire (not root)))) (recordWire root))
          rejects (Proxy @CronRecord) (set "payload" (field "payload" (objectFields (recordWire (not root)))) (recordWire root))
          rejects (Proxy @CronRecord) (set "runPolicy" (object ["whenSessionInactive" .= String (if root then "hold" else "run_in_background")]) (recordWire root))
          rejects (Proxy @CronRecord) (set "schedule" (set "timezone" (String "local") (field "schedule" fields)) (recordWire root))
          forM_ ["heldAt", "holdReason"] $ \key -> roundTrip (Proxy @CronRecord) (Object (KeyMap.delete key fields))
          forM_ [("schedule", ["nextRunAt", "firstFireGuardUntil"]), ("stats", ["lastRunAt", "lastCompletedAt", "lastError"])] $ \(section, keys) ->
            forM_ keys $ \key -> do
              let nested = objectFields (field section fields)
              rejects (Proxy @CronRecord) (set section (Object (KeyMap.insert key Null nested)) (recordWire root))
              roundTrip (Proxy @CronRecord) (set section (Object (KeyMap.delete key nested)) (recordWire root))
        case fromJSON (recordWire False) of
          Success (SessionCronRecord info scope storage payload) -> do
            cronFireCount (cronStats info) @?= 9007199254740993
            cronCreatedAt info @?= "opaque creation"
            cronScopeSessionCwd scope @?= ""
            storage @?= "opaque storage"
            let changedInfo = info {cronRecordAdditionalFields = KeyMap.insert "version" Null (cronRecordAdditionalFields info), cronRunPolicyAdditionalFields = KeyMap.fromList ["whenSessionInactive" .= String "wrong", "extension" .= False]}
                changedScope = scope {cronSessionScopeAdditionalFields = KeyMap.insert "storageDir" (String "wrong") (cronSessionScopeAdditionalFields scope)}
            toJSON (SessionCronRecord changedInfo changedScope storage payload) @?= recordWire False
          _ -> assertFailure "Session record lost its kind",
      testCase "creation preserves optional policies, source, false and empty values without accepting opposite variants" $ do
        forM_ [False, True] $ \root -> do
          params <- decodeValue @CreateCronParams (createWire root)
          toJSON params @?= createWire root
          forM_ ["source", "schedule", "scope", "payload", "kind"] $ \key -> rejects (Proxy @CreateCronParams) (Object (KeyMap.delete key (objectFields (createWire root))))
          rejects (Proxy @CreateCronParams) (set "runPolicy" Null (createWire root))
          rejects (Proxy @CreateCronParams) (set "runImmediately" Null (createWire root))
          rejects (Proxy @CreateCronParams) (set "payload" (payloadWire (not root)) (createWire root))
          rejects (Proxy @CreateCronParams) (set "runPolicy" (object ["whenSessionInactive" .= String (if root then "hold" else "run_in_background")]) (createWire root))
        forM_ ["modelId", "reasoningEffort"] $ \key -> do
          rejects (Proxy @CreateCronParams) (set "payload" (set key Null (payloadWire True)) (createWire True))
          roundTrip (Proxy @CreateCronParams) (set "payload" (Object (KeyMap.delete key (objectFields (payloadWire True)))) (createWire True))
        forM_ ["cwd", "title"] $ \key -> do
          let target = objectFields (field "target" (objectFields (payloadWire True)))
          rejects (Proxy @CreateCronParams) (set "payload" (set "target" (Object (KeyMap.insert key Null target)) (payloadWire True)) (createWire True))
          roundTrip (Proxy @CreateCronParams) (set "payload" (set "target" (Object (KeyMap.delete key target)) (payloadWire True)) (createWire True))
        expression <- maybe (assertFailure "Invalid fixture") pure (mkCronText " 0 12 * * * ")
        let creationOptions = defaultCreateCronOptions (CronSchedule expression False mempty)
        createCronSource creationOptions @?= CronTool
        createCronRunImmediately creationOptions @?= Nothing
        createCronRunPolicyAdditionalFields creationOptions @?= Nothing,
      testCase "partial updates distinguish omitted and empty patches; null and forbidden patch fields reject" $ do
        toJSON (defaultUpdateCronParams "") @?= object ["cronId" .= String ""]
        roundTrip (Proxy @UpdateCronParams) updateWire
        roundTrip (Proxy @UpdateCronParams) emptyPatchWire
        forM_ ["status", "schedule", "payload"] $ \key -> rejects (Proxy @UpdateCronParams) (object ["cronId" .= String "", key .= Null])
        forM_ [object ["prompt" .= Null], object ["prompt" .= String " "], object ["target" .= object []]] $ \patch -> rejects (Proxy @UpdateCronParams) (object ["cronId" .= String "", "payload" .= patch])
        rejects (Proxy @UpdateCronParams) (object ["cronId" .= String "", "status" .= String "held"])
        rejects (Proxy @UpdateCronParams) (object ["cronId" .= String "", "schedule" .= object ["expression" .= String "*"]])
        toJSON ((defaultUpdateCronParams "") {updateCronAdditionalFields = KeyMap.fromList ["cronId" .= String "wrong", "payload" .= Null, "status" .= String "held", "extension" .= False]}) @?= object ["cronId" .= String "", "extension" .= False],
      testCase "optional filters and result absence/counts stay exact without invented success" $ do
        toJSON defaultListCronsParams @?= object []
        roundTrip (Proxy @ListCronsParams) listFilteredWire
        roundTrip (Proxy @DeleteCronParams) deleteWire
        roundTrip (Proxy @HoldSessionCronsParams) holdWire
        roundTrip (Proxy @UpdateCronResult) (object ["cron" .= Null])
        roundTrip (Proxy @UpdateCronResult) (object ["cron" .= recordWire True])
        roundTrip (Proxy @DeleteCronResult) (object ["deleted" .= False])
        rejects (Proxy @UpdateCronResult) (object [])
        rejects (Proxy @CreateCronResult) (object ["cron" .= Null])
        rejects (Proxy @DeleteCronResult) (object ["deleted" .= Null])
        forM_ [Number (-1), Number 0.5, Null] $ \count -> do
          rejects (Proxy @CronStats) (object ["fireCount" .= count])
          rejects (Proxy @HoldSessionCronsResult) (object ["heldCount" .= count])
          rejects (Proxy @ResumeSessionCronsResult) (object ["resumedCount" .= count])
        forM_ ["sessionId", "includeInactive"] $ \key -> rejects (Proxy @ListCronsParams) (object [key .= Null])
        rejects (Proxy @DeleteCronParams) (object ["cronId" .= String "", "sessionId" .= Null])
        toJSON (defaultListCronsParams {listCronsAdditionalFields = KeyMap.fromList ["sessionId" .= String "injected", "includeInactive" .= True, "extension" .= False]}) @?= object ["extension" .= False],
      testCase "all six methods preserve full requests/results without initializing sessions or scheduling locally" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withCronPeer Normal trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ (zip wireCases (highCalls connection)) $ \((_, _, expected), request) -> request >>= (@?= expected)
        readIORef trace >>= (@?= map String ("daemon.authenticate" : map (\(method, _, _) -> method) wireCases)) . map (field "method"),
      testCase "remote and malformed errors remain explicit across every operation" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withCronPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
              forM_ (highCalls connection) $ \request ->
                try @RpcResultError request >>= \case
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected cron failure",
      testCase "waiting cancellation preserves async identity and sends no rollback or remote cancellation" $
        bounded $
          forM_ (zip [0 :: Int ..] wireCases) $ \(index, (method, _, _)) -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withCronPeer (Held method) trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
              case drop index (highCalls connection) of
                request : _ -> withAsync request $ \pending -> do
                  takeMVar ready
                  cancel pending
                  waitCatch pending >>= \case
                    Left cause -> fromException cause @?= Just AsyncCancelled
                    Right _ -> assertFailure "Cancelled cron operation returned"
                [] -> assertFailure "Missing high-level operation"
              if method == "daemon.list_crons"
                then void (Daemon.deleteCron connection (DeleteCronParams "" (Just "") mempty))
                else void (Daemon.listCrons connection defaultListCronsParams)
            methods <- map (field "method") <$> readIORef trace
            methods @?= map String ["daemon.authenticate", method, if method == "daemon.list_crons" then "daemon.delete_cron" else "daemon.list_crons"],
      testCase "connection loss is sticky rather than an invented cron result" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withCronPeer Dropped trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ (highCalls connection) $ \request -> try @RpcChannelError request >>= (@?= Left RpcChannelReadFailure),
      testCase "low-level calls retain caller envelopes and obey zero deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          forM_ (zip3 (["list", "filter", "session", "root", "update", "missing", "delete", "hold", "resume"] :: [Text]) wireCases [0 :: Int ..]) $ \(identifier, (method, expectedParams, result), index) -> do
            let metadata = KeyMap.fromList ["id" .= String "wrong", "method" .= String "wrong", "params" .= Null, "trace" .= False]
                configured = Client.CallOptions identifier (WithEnvelope (Just "1.201.1") Nothing metadata) (Just 1000000)
            case drop index (lowCalls channel configured) of
              request : _ -> withAsync request $ \pending -> do
                sent <- atomically (readTQueue outgoing)
                field "id" sent @?= String identifier
                field "method" sent @?= String method
                field "params" sent @?= expectedParams
                field "trace" sent @?= Bool False
                atomically (writeTQueue incoming (response sent result))
                wait pending >>= (@?= result)
              [] -> assertFailure "Missing low-level operation"
          forM_ (lowCalls channel (Client.CallOptions "expired" (WithEnvelope (Just "1.201.1") Nothing mempty) (Just 0))) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing),
      testCase "early cron events filter unrelated data, support callback queries and unsubscribe" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        bad <- newEmptyMVar
        observed <- newEmptyMVar
        count <- newIORef (0 :: Int)
        withCronPeer Events trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          stop <- Daemon.onCronStateChanged connection $ \event -> do
            modifyIORef' count (+ 1)
            case event of
              Left cause -> putMVar bad cause
              Right changed -> do
                toJSON changed @?= changeWire
                crons <- Daemon.listCrons connection defaultListCronsParams
                putMVar observed (length (listedCrons crons))
          void (decoded (Daemon.createCron connection) (createWire False))
          takeMVar bad >>= (@?= Daemon.InvalidDaemonEvent)
          takeMVar observed >>= (@?= 2)
          stop
          stop
          barrier <- newEmptyMVar
          stopBarrier <- Daemon.onCronStateChanged connection $ \case Right _ -> putMVar barrier (); Left _ -> pure ()
          void (decoded (Daemon.createCron connection) (createWire False))
          takeMVar barrier
          readIORef count >>= (@?= 2)
          stopBarrier,
      testCase "cron change records distinguish absent and empty snapshots" $ do
        roundTrip (Proxy @CronStateChanged) changeWire
        roundTrip (Proxy @CronStateChanged) (object ["reason" .= String "deleted", "cronIds" .= ([] :: [Text])])
        roundTrip (Proxy @CronStateChanged) (object ["reason" .= String "deleted", "cronIds" .= ([] :: [Text]), "crons" .= ([] :: [Value])])
        rejects (Proxy @CronStateChanged) (object ["reason" .= String "created", "cronIds" .= ([] :: [Text]), "crons" .= Null])
        rejects (Proxy @CronStateChanged) (object ["reason" .= String "other", "cronIds" .= ([] :: [Text])])
    ]

scheduleWire :: Value
scheduleWire = object ["expression" .= String "0 12 * * *", "recurring" .= False]

payloadWire :: Bool -> Value
payloadWire root = object (["type" .= String "prompt", "prompt" .= String "private prompt", "target" .= object (["type" .= String (if root then "new_session" else "same_session"), "extension" .= False] <> if root then ["cwd" .= String "", "title" .= String ""] else []), "extension" .= False] <> if root then ["modelId" .= String "", "reasoningEffort" .= String "low"] else [])

createWire :: Bool -> Value
createWire root = object (["kind" .= String (if root then "root_prompt" else "session_prompt"), "source" .= String "cron_tool", "schedule" .= scheduleWire, "runImmediately" .= False, "scope" .= object (["type" .= String (if root then "root" else "session"), "extension" .= False] <> if root then [] else ["sessionId" .= String "session", "sessionCwd" .= String ""]), "payload" .= payloadWire root, "extension" .= False] <> ["runPolicy" .= object ["whenSessionInactive" .= String "run_in_background", "extension" .= False] | root])

recordWire :: Bool -> Value
recordWire root = object ["version" .= (1 :: Int), "id" .= String "cron", "status" .= String "held", "source" .= String "migration", "schedule" .= set "timezone" (String "UTC") (set "nextRunAt" (String "") (set "firstFireGuardUntil" (String "opaque guard") scheduleWire)), "stats" .= object ["fireCount" .= (9007199254740993 :: Integer), "lastRunAt" .= String "", "lastCompletedAt" .= String "opaque completion", "lastError" .= String "private error"], "createdAt" .= String "opaque creation", "updatedAt" .= String "", "heldAt" .= String "", "holdReason" .= String " ", "kind" .= String (if root then "root_prompt" else "session_prompt"), "scope" .= (if root then field "scope" (objectFields (createWire True)) else set "storageDir" (String "opaque storage") (field "scope" (objectFields (createWire False)))), "runPolicy" .= object ["whenSessionInactive" .= String (if root then "run_in_background" else "hold"), "extension" .= False], "payload" .= payloadWire root, "extension" .= False]

listFilteredWire, updateWire, emptyPatchWire, deleteWire, holdWire, changeWire :: Value
listFilteredWire = object ["sessionId" .= String "", "includeInactive" .= False]
updateWire = object ["cronId" .= String "cron", "status" .= String "paused", "schedule" .= scheduleWire, "payload" .= object ["prompt" .= String "updated prompt"]]
emptyPatchWire = object ["cronId" .= String "", "payload" .= object []]
deleteWire = object ["cronId" .= String "", "sessionId" .= String ""]
holdWire = object ["sessionId" .= String "session", "reason" .= String ""]
changeWire = object ["reason" .= String "created", "cronIds" .= [String "cron"], "crons" .= [recordWire False], "extension" .= False]

wireCases :: [(Text, Value, Value)]
wireCases =
  [ ("daemon.list_crons", object [], object ["crons" .= [recordWire False, recordWire True]]),
    ("daemon.list_crons", listFilteredWire, object ["crons" .= ([] :: [Value])]),
    ("daemon.create_cron", createWire False, object ["cron" .= recordWire False]),
    ("daemon.create_cron", createWire True, object ["cron" .= recordWire True]),
    ("daemon.update_cron", updateWire, object ["cron" .= recordWire True]),
    ("daemon.update_cron", emptyPatchWire, object ["cron" .= Null]),
    ("daemon.delete_cron", deleteWire, object ["deleted" .= False]),
    ("daemon.hold_session_crons", holdWire, object ["heldCount" .= (0 :: Int)]),
    ("daemon.resume_session_crons", object ["sessionId" .= String ""], object ["resumedCount" .= (2 :: Int)])
  ]

highCalls :: Daemon.DaemonConnection -> [IO Value]
highCalls connection =
  [ toJSON <$> Daemon.listCrons connection defaultListCronsParams,
    decoded (Daemon.listCrons connection) listFilteredWire,
    decoded (Daemon.createCron connection) (createWire False),
    decoded (Daemon.createCron connection) (createWire True),
    decoded (Daemon.updateCron connection) updateWire,
    decoded (Daemon.updateCron connection) emptyPatchWire,
    decoded (Daemon.deleteCron connection) deleteWire,
    decoded (Daemon.holdSessionCrons connection) holdWire,
    toJSON <$> Daemon.resumeSessionCrons connection (SessionIdParams "" mempty)
  ]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel configured =
  [ toJSON <$> Client.listDaemonCrons channel configured defaultListCronsParams,
    decoded (Client.listDaemonCrons channel configured) listFilteredWire,
    decoded (Client.createDaemonCron channel configured) (createWire False),
    decoded (Client.createDaemonCron channel configured) (createWire True),
    decoded (Client.updateDaemonCron channel configured) updateWire,
    decoded (Client.updateDaemonCron channel configured) emptyPatchWire,
    decoded (Client.deleteDaemonCron channel configured) deleteWire,
    decoded (Client.holdDaemonSessionCrons channel configured) holdWire,
    toJSON <$> Client.resumeDaemonSessionCrons channel configured (SessionIdParams "" mempty)
  ]

decoded :: (FromJSON a, ToJSON b) => (a -> IO b) -> Value -> IO Value
decoded request value = decodeValue value >>= fmap toJSON . request

data Mode = Normal | Rejected | Malformed | Held Text | Dropped | Events deriving stock (Eq)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withCronPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withCronPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
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
        (_, _, expected) <- maybe (assertFailure "Unexpected cron request/params") pure (find (\(name, params, _) -> name == method && params == field "params" request) wireCases)
        case mode of
          Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Cron rejected"]]
          Malformed -> WS.sendTextData connection (encode (Object (response request Null)))
          Held held | held == method -> putMVar ready ()
          Dropped -> WS.sendClose connection ("fixture disconnect" :: Text)
          _ -> do
            when (mode == Events && method == "daemon.create_cron") $ do
              notify connection "daemon.unrelated" (Bool False)
              notify connection "daemon.cron.state_changed" (object ["reason" .= String "unknown", "cronIds" .= ([] :: [Text])])
              notify connection "daemon.cron.state_changed" changeWire
            WS.sendTextData connection (encode (Object (response request expected)))
    notify connection method params = sendFrame connection ["type" .= String "notification", "method" .= (method :: Text), "params" .= params]

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
