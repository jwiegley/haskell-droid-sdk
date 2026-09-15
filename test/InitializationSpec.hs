{-# LANGUAGE OverloadedStrings #-}

module InitializationSpec (initializationTests, runInitializationPeer) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (fromException, throwIO, try)
import Control.Monad (forM_, unless, void)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecodeStrict', encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import DroidSpec (assertReaped)
import Factory.Droid qualified as Droid
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Transport
import Factory.Droid.Transport.Process qualified as Process
import ProcessSpec (bounded)
import ProtocolSpec (reply, withMemory)
import SchemaTest (rejects)
import System.Environment (getExecutablePath)
import System.IO (hFlush, isEOF, stdout)
import System.Posix.Process (getProcessID)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

initializationTests :: TestTree
initializationTests =
  testGroup
    "Initialization configuration"
    [ testCase "minimal creation is exact and empty required strings are not defaulted" $ do
        roundTrip (Proxy @InitializeSessionParams) (object ["machineId" .= String "", "cwd" .= String ""])
        toJSON (defaultInitializeSessionParams "" "") @?= object ["machineId" .= String "", "cwd" .= String ""],
      testCase "creation configuration preserves every supplied value and extension" $
        roundTrip (Proxy @InitializeSessionParams) richWire,
      testCase "existing model, structured prompt, MCP and worktree fields share the complete codec" $ do
        forM_ [String "required prompt", object ["type" .= String "preset", "preset" .= String "droid", "append" .= String "extra instruction"]] $ \prompt ->
          roundTrip (Proxy @InitializeSessionParams) (addFields ["modelId" .= String "", "systemPrompt" .= prompt, "mcpServers" .= ([] :: [Value]), "mcpOAuthCallbackUri" .= String "http://localhost:1234/callback", "blockOnMcpLoad" .= False, "worktree" .= False, "worktreeDir" .= String ""] richWire),
      testCase "nullable structured capability differs from omission and an object schema" $ do
        forM_ [Null, object ["type" .= String "json_schema", "schema" .= object []]] $ \format ->
          roundTrip (Proxy @InitializeSessionParams) (addFields ["structuredOutputFormat" .= format] minimalWire)
        original <- decode @InitializeSessionParams minimalWire
        configurationStructuredOutput (initializeConfiguration original) @?= Nothing
        cleared <- decode @InitializeSessionParams (addFields ["structuredOutputFormat" .= Null] minimalWire)
        configurationStructuredOutput (initializeConfiguration cleared) @?= Just Nothing,
      testCase "non-nullable options reject null while selected mode fallbacks retain CLI semantics" $ do
        forM_ ["machineId", "cwd", "modelId", "reasoningEffort", "specModeModelId", "specModeReasoningEffort", "autoRejectPermissionRequests", "disableBuiltinSkills", "workspaceId", "sessionId", "tags", "additionalToolIds", "enabledToolIds", "disabledToolIds", "restrictToolIds", "systemPromptOverride", "worktree"] $ \key ->
          rejects (Proxy @InitializeSessionParams) (addFields [key .= Null] minimalWire)
        decoded <- decode @InitializeSessionParams (addFields ["interactionMode" .= String "unknown", "autonomyLevel" .= Null] minimalWire)
        configurationMode (initializeConfiguration decoded) @?= Nothing
        configurationAutonomy (initializeConfiguration decoded) @?= Nothing,
      testCase "invalid declared discriminators and container shapes fail instead of entering extensions" $ do
        forM_ [["privacyLevel" .= String "public"], ["autonomyMode" .= String "invented"], ["decompSessionType" .= String "invented"], ["disabledToolIds" .= String "tool"], ["tags" .= object []], ["structuredOutputFormat" .= object ["type" .= String "text", "schema" .= object []]], ["sessionSource" .= object ["platform" .= String "api"]]] $ \fields ->
          rejects (Proxy @InitializeSessionParams) (addFields fields minimalWire),
      testCase "owned identity, model, MCP and daemon fields cannot be smuggled through extensions" $ do
        let config = defaultSessionConfiguration {configurationSessionId = Just "owned", configurationAutoRejectPermissions = Just False, configurationAdditionalFields = KeyMap.fromList ["machineId" .= String "shadow", "cwd" .= String "shadow", "sessionId" .= String "shadow", "modelId" .= String "shadow", "token" .= String "shadow", "runtimeSettingsPath" .= String "shadow", "mcpServers" .= ([] :: [Value]), "extension" .= False]}
            base = (defaultInitializeSessionParams "machine" "/cwd") {initializeConfiguration = config}
        toJSON (DaemonInitializeSessionParams base "" defaultDaemonSpawnOptions) @?= object ["machineId" .= String "machine", "cwd" .= String "/cwd", "sessionId" .= String "owned", "autoRejectPermissionRequests" .= False, "token" .= String "", "extension" .= False],
      testCase "daemon duration keeps exact integers and rejects zero, fractions and extreme exponents" $ do
        let wire value = addFields ["token" .= String "", "inactivityTimeoutMs" .= value, "disableInactivityTimeout" .= False, "runtimeSettingsPath" .= String ""] richWire
        roundTrip (Proxy @DaemonInitializeSessionParams) (wire (Number 9007199254740993))
        forM_ [Number 0, Number (-1), Number 1.5, Number (scientific 1 100000000), Number (scientific 1 (-100000000)), String "1", Null] $ \value -> rejects (Proxy @DaemonInitializeSessionParams) (wire value)
        let invalid = DaemonInitializeSessionParams (defaultInitializeSessionParams "m" ".") "" (defaultDaemonSpawnOptions {spawnInactivityTimeoutMillis = Just 0})
        validateDaemonInitializationParams invalid @?= Left InvalidInitializationParams,
      testCase "parameter displays redact identity, paths, tags, schemas and credentials" $ do
        params <- decode @InitializeSessionParams richWire
        show params @?= "InitializeSessionParams <redacted>"
        show (initializeConfiguration params) @?= "SessionConfiguration <redacted>"
        show (DaemonInitializeSessionParams params "fixture secret" defaultDaemonSpawnOptions) @?= "DaemonInitializeSessionParams <redacted>",
      testCase "typed local and daemon initialization use the existing exact RPC envelope" $ bounded $ withMemory $ \channel incoming sent -> do
        params <- decode @InitializeSessionParams richWire
        let options = Client.CallOptions "local" (WithEnvelope (Just "1.205.0") Nothing mempty) (Just 1000000)
        withAsync (Client.initializeSession channel options params) $ \worker -> do
          frame <- atomically (readTQueue sent)
          field "method" frame @?= String "droid.initialize_session"
          field "params" frame @?= richWire
          atomically (writeTQueue incoming (Right (reply "local" (object ["receipt" .= False]))))
          wait worker >>= (@?= KeyMap.singleton "receipt" (Bool False))
        let daemonParams = DaemonInitializeSessionParams params "" defaultDaemonSpawnOptions
        withAsync (Client.initializeDaemonSession channel (options {Client.callRequestId = "daemon"}) daemonParams) $ \worker -> do
          frame <- atomically (readTQueue sent)
          field "method" frame @?= String "daemon.initialize_session"
          field "params" frame @?= toJSON daemonParams
          atomically (writeTQueue incoming (Right (reply "daemon" (object []))))
          void (wait worker),
      testCase "typed daemon validation fails before pending or send admission" $ bounded $ withMemory $ \channel _ sent -> do
        let params = DaemonInitializeSessionParams (defaultInitializeSessionParams "m" ".") "" (defaultDaemonSpawnOptions {spawnInactivityTimeoutMillis = Just 0})
        try @InitializationError (Client.initializeDaemonSession channel (Client.CallOptions "invalid" (WithEnvelope Nothing Nothing mempty) Nothing) params) >>= (@?= Left InvalidInitializationParams)
        atomically (getRpcPendingCount channel) >>= (@?= 0)
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "normal injected local creation sends configuration before publishing the handle" $ bounded $ withInitPeer ReplyNormally $ \transport sent _ -> do
        params <- decode @InitializeSessionParams (addFields ["systemPrompt" .= String "required prompt"] richWire)
        let options = (Droid.defaultDroidSessionOptions ".") {Droid.droidSessionMachineId = Just "", Droid.droidSessionConfiguration = initializeConfiguration params, Droid.droidSessionSystemPrompt = initializeSystemPrompt params}
        Droid.withDroidSessionOn options transport $ \session -> do
          Droid.droidSessionId session @?= "requested"
          frame <- atomically (readTQueue sent)
          field "machineId" (paramsOf frame) @?= String ""
          assertFields configurationWire (paramsOf frame)
          field "systemPrompt" (paramsOf frame) @?= String "required prompt"
          settings <- Droid.getDroidSettings session
          field "initializationParams" (asObject (toJSON settings)) @?= Object (paramsOf frame),
      testCase "owned local process projects the same options and is reaped" $ bounded $ do
        executable <- getExecutablePath
        params <- decode @InitializeSessionParams richWire
        let options = (Droid.defaultDroidOptions ".") {Droid.droidExecutable = executable, Droid.droidMachineId = Just "owned-machine", Droid.droidConfiguration = initializeConfiguration params, Droid.droidLaunchOptions = Process.defaultDroidLaunchOptions {Process.launchArguments = Just ["--initialization-peer"]}}
        pid <- Droid.withDroidSession options $ \session -> do
          settings <- asObject . toJSON <$> Droid.getDroidSettings session
          let actual = asObject (field "initializationParams" settings)
          assertFields configurationWire actual
          field "machineId" actual @?= String "owned-machine"
          pure (textField "fixturePid" settings)
        assertReaped pid,
      testCase "normal daemon creation protects caller identity and forwards source and spawn configuration" $ bounded $ withInitPeer ReplyNormally $ \transport sent _ -> do
        params <- decode @InitializeSessionParams (addFields ["systemPrompt" .= String "required prompt"] richWire)
        let options = clientOptions {Daemon.daemonClientConfiguration = initializeConfiguration params, Daemon.daemonClientSystemPrompt = initializeSystemPrompt params, Daemon.daemonClientWorktree = Just False, Daemon.daemonClientWorktreeDirectory = Just "", Daemon.daemonClientSpawnOptions = defaultDaemonSpawnOptions {spawnDisableInactivityTimeout = Just False, spawnRuntimeSettingsPath = Just "", spawnInactivityTimeoutMillis = Just 9007199254740993}}
        Daemon.withSessionUsing options transport $ \session -> do
          Daemon.sessionId session @?= "requested"
          frame <- atomically (readTQueue sent)
          assertFields configurationWire (paramsOf frame)
          field "systemPrompt" (paramsOf frame) @?= String "required prompt"
          field "worktree" (paramsOf frame) @?= Bool False
          field "worktreeDir" (paramsOf frame) @?= String ""
          field "token" (paramsOf frame) @?= String ""
          field "disableInactivityTimeout" (paramsOf frame) @?= Bool False
          field "inactivityTimeoutMs" (paramsOf frame) @?= Number 9007199254740993,
      testCase "missing structured-prompt acknowledgment rejects local and daemon handles" $
        bounded $
          forM_ [False, True] $ \daemon -> withInitPeer IgnorePromptEcho $ \transport _ count -> do
            params <- decode @InitializeSessionParams (addFields ["systemPrompt" .= String "required prompt"] minimalWire)
            let unpublished _ = assertFailure "Unsupported prompt was published" :: IO ()
            result <-
              try @Droid.DroidError $
                if daemon
                  then Daemon.withSessionUsing (clientOptions {Daemon.daemonClientSystemPrompt = initializeSystemPrompt params}) transport unpublished
                  else Droid.withDroidSessionOn ((Droid.defaultDroidSessionOptions ".") {Droid.droidSessionSystemPrompt = initializeSystemPrompt params}) transport unpublished
            result @?= Left Droid.DroidInvalidEvent
            readIORef count >>= (@?= 1),
      testCase "invalid spawn configuration and negative or overflowing budgets precede transport access" $ bounded $ do
        let transport = objectTransport (const (assertFailure "Unexpected send")) (assertFailure "Unexpected receive")
            invalid = clientOptions {Daemon.daemonClientSpawnOptions = defaultDaemonSpawnOptions {spawnInactivityTimeoutMillis = Just 0}}
        try @InitializationError (Daemon.withSessionUsing invalid transport (const (pure ()))) >>= (@?= Left InvalidInitializationParams)
        forM_ [-1, maxBound] $ \value -> try @RpcChannelError (Daemon.withSessionUsing (clientOptions {Daemon.daemonClientInitializationTimeoutMicros = Just value}) transport (const (pure ()))) >>= (@?= Left RpcInvalidTimeout),
      testCase "creation-only configuration is rejected on resume rather than silently ignored" $ bounded $ do
        let transport = objectTransport (const (assertFailure "Unexpected send")) (assertFailure "Unexpected receive")
            config = defaultSessionConfiguration {configurationTitle = Just ""}
        try @InitializationError (Droid.withResumedDroidSessionOn ((Droid.defaultDroidSessionOptions ".") {Droid.droidSessionConfiguration = config}) transport "saved" (const (pure ()))) >>= (@?= Left InitializationOptionsOnResume)
        try @InitializationError (Daemon.withResumedSessionUsing (clientOptions {Daemon.daemonClientConfiguration = config}) transport "saved" (const (pure ()))) >>= (@?= Left InitializationOptionsOnResume),
      testCase "mismatched creation identity is never published" $ bounded $ withInitPeer WrongIdentity $ \transport _ count -> do
        let options = (Droid.defaultDroidSessionOptions ".") {Droid.droidSessionConfiguration = defaultSessionConfiguration {configurationSessionId = Just "requested"}}
        try @Droid.DroidError (Droid.withDroidSessionOn options transport (const (assertFailure "Wrong identity published" :: IO ()))) >>= (@?= Left Droid.DroidInvalidEvent)
        readIORef count >>= (@?= 1),
      testCase "daemon timeout retry keeps the same session and parameters but uses a fresh request ID" $ bounded $ withInitPeer DelayFirst $ \transport sent count -> do
        gates <- newTQueueIO
        let observed = transport {transportPendingSessionReady = Just (\identifier gate -> atomically (writeTQueue gates (identifier, gate)))}
            options = clientOptions {Daemon.daemonClientInitializationTimeoutMicros = Just 50000}
        Daemon.withSessionUsing options observed $ \session -> do
          first <- atomically (readTQueue sent)
          second <- atomically (readTQueue sent)
          paramsOf first @?= paramsOf second
          assertBool "RPC ID reused" (frameId first /= frameId second)
          field "sessionId" (paramsOf first) @?= String (Daemon.sessionId session)
          (identifier, gate) <- atomically (readTQueue gates)
          identifier @?= Daemon.sessionId session
          atomically gate
          extra <- atomically (isEmptyTQueue gates)
          extra @?= True
        readIORef count >>= (@?= 2),
      testCase "remote initialization rejection is not retried" $ bounded $ withInitPeer RejectInit $ \transport _ count -> do
        result <- try @RpcResultError (Daemon.withSessionUsing clientOptions transport (const (assertFailure "Rejected initialization published")))
        case result of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Remote error lost"
        readIORef count >>= (@?= 1),
      testCase "a caller timeout after initialization cannot retry or replay caller work" $ bounded $ withInitPeer ReplyNormally $ \transport _ count -> do
        entered <- newIORef (0 :: Int)
        result <- try @RpcChannelError $ Daemon.withSessionUsing clientOptions transport $ \_ -> do
          atomicModifyIORef' entered (\value -> (value + 1, ()))
          throwIO RpcRequestTimedOut :: IO ()
        result @?= Left RpcRequestTimedOut
        readIORef count >>= (@?= 1)
        readIORef entered >>= (@?= 1),
      testCase "zero initialization budget sends no session request" $ bounded $ withInitPeer ReplyNormally $ \transport _ count -> do
        try @RpcChannelError (Daemon.withSessionUsing (clientOptions {Daemon.daemonClientInitializationTimeoutMicros = Just 0}) transport (const (assertFailure "Zero-budget initialization published" :: IO ()))) >>= (@?= Left RpcRequestTimedOut)
        readIORef count >>= (@?= 0),
      testCase "cancelling initialization preserves identity and does not retry or publish" $ bounded $ withInitPeer HoldInit $ \transport sent count -> do
        withAsync (Daemon.withSessionUsing clientOptions transport (const (assertFailure "Cancelled initialization published"))) $ \worker -> do
          void (atomically (readTQueue sent))
          cancel worker
          waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation lost"
        readIORef count >>= (@?= 1)
    ]

minimalWire :: Value
minimalWire = object ["machineId" .= String "machine", "cwd" .= String "/work"]

configurationWire :: Object
configurationWire =
  KeyMap.fromList
    [ "sessionId" .= String "requested",
      "workspaceId" .= String "",
      "autonomyMode" .= String "auto-low",
      "interactionMode" .= String "spec",
      "autonomyLevel" .= String "high",
      "reasoningEffort" .= String "low",
      "specModeModelId" .= String "",
      "specModeReasoningEffort" .= String "medium",
      "compactionThresholdCheckEnabled" .= False,
      "decompSessionType" .= String "worker",
      "decompMissionId" .= String "",
      "skipPermissionsUnsafe" .= False,
      "sessionLocation" .= String "arbitrary location",
      "sessionSource" .= object ["platform" .= String "api", "delegationSessionId" .= String "source"],
      "sessionOriginHint" .= String "sdk",
      "tags" .= [object ["name" .= String "custom", "metadata" .= object ["label" .= String ""]]],
      "privacyLevel" .= String "private",
      "title" .= String "",
      "autoRejectPermissionRequests" .= False,
      "disableBuiltinSkills" .= False,
      "systemPromptOverride" .= String "",
      "structuredOutputFormat" .= Null,
      "additionalToolIds" .= [String "extra", String "extra"],
      "enabledToolIds" .= ([] :: [Value]),
      "disabledToolIds" .= [String "disabled"],
      "restrictToolIds" .= ([] :: [Value]),
      "extension" .= False
    ]

richWire :: Value
richWire = addFields ["missionSettings" .= object ["workerModel" .= String "", "workerReasoningEffort" .= String "low", "skipScrutiny" .= False, "skipUserTesting" .= False]] (Object (KeyMap.union configurationWire (asObject minimalWire)))

addFields :: [(Key, Value)] -> Value -> Value
addFields fields value = Object (KeyMap.union (KeyMap.fromList fields) (asObject value))

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = error "Expected fixture object"

field :: Key -> Object -> Value
field key fields = fromMaybe Null (KeyMap.lookup key fields)

textField :: Key -> Object -> Text
textField key fields = case field key fields of String value -> value; _ -> error "Expected fixture text"

frameId :: Object -> Text
frameId = textField "id"

paramsOf :: Object -> Object
paramsOf = asObject . field "params"

decode :: (FromJSON a) => Value -> IO a
decode value = case fromJSON value of Error message -> assertFailure message; Success parsed -> pure parsed

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decode @a value >>= (@?= value) . toJSON

assertFields :: Object -> Object -> IO ()
assertFields expected actual = forM_ (KeyMap.toList expected) $ \(key, value) -> KeyMap.lookup key actual @?= Just value

data PeerMode = ReplyNormally | DelayFirst | HoldInit | RejectInit | WrongIdentity | IgnorePromptEcho deriving stock (Eq)

clientOptions :: Daemon.DaemonClientOptions
clientOptions = Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication (GetUserInfoResult "user" "org" mempty) "") "/work"

withInitPeer :: PeerMode -> (ObjectTransport -> TQueue Object -> IORef Int -> IO a) -> IO a
withInitPeer mode action = do
  incoming <- newTQueueIO
  sent <- newTQueueIO
  count <- newIORef 0
  let send frame = do
        atomically (writeTQueue sent frame)
        let method = textField "method" frame
        unless (method `elem` ["droid.initialize_session", "daemon.initialize_session"]) (assertFailure "Unexpected initialization fixture method")
        number <- atomicModifyIORef' count (\old -> (old + 1, old + 1))
        unless (mode == HoldInit || (mode == DelayFirst && number == 1)) $ do
          let response = if mode == RejectInit then KeyMap.insert "error" (toJSON (JsonRpcError RpcConflict "fixture rejection" Nothing mempty)) (reply (frameId frame) Null) else reply (frameId frame) (initializationResult mode "" frame)
          atomically (writeTQueue incoming response)
  action (objectTransport send (atomically (readTQueue incoming))) sent count

initializationResult :: PeerMode -> Text -> Object -> Value
initializationResult mode pid frame =
  let params = paramsOf frame
      identifier = if mode == WrongIdentity then "wrong" else case field "sessionId" params of String value -> value; _ -> "generated"
      prompt = if mode == IgnorePromptEcho then [] else maybe [] (\value -> ["systemPrompt" .= value]) (KeyMap.lookup "systemPrompt" params)
      settings = object (["modelId" .= String "fixture", "reasoningEffort" .= String "low", "initializationParams" .= params, "fixturePid" .= pid] <> prompt)
   in object ["sessionId" .= String identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= settings]

runInitializationPeer :: IO ()
runInitializationPeer = do
  pid <- ("fixture-" <>) . Text.pack . show <$> getProcessID
  let loop = do
        ended <- isEOF
        unless ended $ do
          line <- BS.getLine
          frame <- either fail pure (eitherDecodeStrict' line)
          BL.putStr (encode (reply (frameId frame) (initializationResult ReplyNormally pid frame)) <> "\n")
          hFlush stdout
          loop
  loop
