{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AutomationSpec (automationTests)
import ChildSessionSpec (childSessionTests)
import ClientSpec (clientTests)
import ConnectionHooksSpec (connectionHookTests)
import ConnectionReadinessSpec (connectionReadinessTests)
import ContentSpec (contentTests)
import ContextSpec (contextTests)
import Control.Monad (forM_)
import ControlSpec (controlTests)
import CwdStateSpec (cwdStateTests)
import DaemonAttachmentSpec (attachmentTests)
import DaemonAutomationConfigSpec (automationConfigTests)
import DaemonAutomationSpec (daemonAutomationTests)
import DaemonCacheSpec (cacheTests)
import DaemonCatalogSpec (catalogTests)
import DaemonCronSpec (cronTests)
import DaemonCustomModelSpec (customModelTests)
import DaemonDefaultsSpec (defaultsTests)
import DaemonDiscoveryClientSpec (discoveryClientTests)
import DaemonGitResourceSpec (gitResourceTests)
import DaemonGitSpec (gitTests)
import DaemonLifecycleSpec (lifecycleTests)
import DaemonLoadCoordinationSpec (loadCoordinationTests)
import DaemonLoadSpec (loadTests)
import DaemonManagementSpec (managementTests)
import DaemonPluginSpec (pluginTests)
import DaemonQueueSpec (queueTests)
import DaemonSettingsClientSpec (settingsClientTests)
import DaemonSoftwareFactorySpec (softwareFactoryTests)
import DaemonSpec (daemonTests)
import DaemonTerminalClientSpec (terminalClientTests)
import DaemonWorkspaceClientSpec (workspaceClientTests)
import DaemonWorkspaceSpec (workspaceTests)
import Data.Aeson
  ( FromJSON,
    Result (..),
    ToJSON,
    Value (..),
    eitherDecode,
    eitherDecodeFileStrict,
    encode,
    fromJSON,
    toJSON,
    withObject,
    (.:),
  )
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import DeferredInteractionSpec (deferredInteractionTests)
import DiagnosticSpec (diagnosticTests)
import DiscoverySpec (discoveryTests)
import DispatchSpec (dispatchTests)
import DroidSpec (droidTests, runDroidPeer)
import EnvelopeSpec (envelopeTests)
import Factory.Droid.Schema.Enums
import GHC.IO.Encoding (setFileSystemEncoding, utf8)
import HandlerSpec (handlerTests)
import HostSpec (hostTests)
import HostedMcpSpec (hostedMcpTests)
import InProcessSpec (inProcessTests)
import InitializationSpec (initializationTests, runInitializationPeer)
import InjectedSessionSpec (injectedSessionTests)
import InputSpec (inputTests)
import InteractionSpec (interactionTests)
import IpcSpec (ipcTests)
import LoadPolicySpec (loadPolicyTests, runLoadPolicyPeer)
import LoggingSpec (loggingTests)
import LoopSpec (loopTests)
import MCPSpec (mcpTests)
import McpConfigSpec (mcpConfigTests)
import MessagesSpec (messageTests)
import MetadataSpec (metadataTests)
import MissionEventSpec (missionEventTests)
import MissionObservationSpec (missionObservationTests)
import MissionRegistrySpec (missionRegistryTests)
import MissionSpec (missionTests)
import MissionStateSpec (missionStateTests)
import ModelsSpec (modelTests)
import NotificationsSpec (notificationTests)
import OwnedIpcSpec (ownedIpcTests, runOwnedIpcPeer)
import Paths_droid_sdk (getDataFileName)
import PendingInteractionSpec (pendingInteractionTests)
import ProcessSpec (processTests, runProcessPeer)
import ProtocolSpec (protocolTests)
import QueueStateSpec (queueStateTests)
import RESTSpec (restTests)
import RPCSpec (rpcTests)
import RelaySpec (relayTests)
import RetrySpec (retryTests)
import SavedSessionSelectionSpec (runSavedSelectionPeer, savedSelectionTests)
import SavedSessionSpec (savedSessionTests)
import ScriptSpec (scriptTests)
import SessionSpec (sessionTests)
import SessionStateSpec (sessionStateTests)
import SettingsSpec (settingsTests)
import SourcesSpec (sourceTests)
import StreamSpec (legacyStreamTests, streamTests)
import SubmissionSpec (submissionTests)
import System.Environment (getArgs)
import SystemPromptSpec (systemPromptTests)
import TerminalRuntimeSpec (terminalRuntimeTests)
import TerminalSpec (terminalTests)
import TerminalStateSpec (terminalStateTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (elements, forAll, testProperty, (===))
import TimestampSpec (timestampTests)
import ToolNotificationsSpec (toolNotificationTests)
import ToolsSpec (toolTests)
import TunnelSpec (tunnelTests)
import UsageSpec (usageTests)
import UtilitySpec (utilityTests)
import ValidatorSpec (validatorTests)
import WebSocketSpec (webSocketTests)
import WorktreeSpec (worktreeTests)

main :: IO ()
main =
  getArgs >>= \case
    "--owned-ipc-peer" : _ -> do
      -- The peer's argv contract is UTF-8 even under an empty child environment.
      setFileSystemEncoding utf8
      getArgs >>= runOwnedIpcPeer . drop 1
    "--saved-selection-peer" : arguments -> runSavedSelectionPeer arguments
    ["--jsonl-peer", mode] -> runProcessPeer mode
    ["--initialization-peer"] -> runInitializationPeer
    ["--load-policy-peer"] -> runLoadPolicyPeer
    ["exec", "--output-format", "acp"] -> runProcessPeer "acp"
    ["exec", "--input-format", "stream-jsonrpc", "--output-format", "stream-jsonrpc"] -> runDroidPeer
    ["--fixture-launch-prefix", "exec", "--input-format", "stream-jsonrpc", "--output-format", "stream-jsonrpc", "--fixture-launch-extra", ""] -> runDroidPeer
    _ -> suiteMain

suiteMain :: IO ()
suiteMain = do
  path <- getDataFileName "schema/shared.schema.json"
  schema <- either fail pure =<< eitherDecodeFileStrict path
  droidPath <- getDataFileName "schema/droid.schema.json"
  droidSchema <- either fail pure =<< eitherDecodeFileStrict droidPath
  daemonPath <- getDataFileName "schema/daemon.schema.json"
  daemonSchema <- either fail pure =<< eitherDecodeFileStrict daemonPath
  defaultMain $
    testGroup
      "Droid SDK"
      [ enumTests schema "AutonomyLevelSchema" (Proxy @AutonomyLevel),
        enumTests schema "CustomModelAuthModeSchema" (Proxy @CustomModelAuthMode),
        enumTests schema "DroidInteractionModeSchema" (Proxy @DroidInteractionMode),
        enumTests schema "FileEditToolProfileSchema" (Proxy @FileEditToolProfile),
        enumTests schema "MessageRoleSchema" (Proxy @MessageRole),
        enumTests schema "MessageVisibilitySchema" (Proxy @MessageVisibility),
        enumTests schema "ModelFallbackReasonSchema" (Proxy @ModelFallbackReason),
        enumTests schema "ModelProviderSchema" (Proxy @ModelProvider),
        enumTests schema "ReasoningEffortSchema" (Proxy @ReasoningEffort),
        enumTests schema "SandboxModeSchema" (Proxy @SandboxMode),
        enumTests schema "SessionOriginSchema" (Proxy @SessionOrigin),
        enumTests schema "SettingsLevelSchema" (Proxy @SettingsLevel),
        enumTests schema "SkillLocationSchema" (Proxy @SkillLocation),
        enumTests schema "ToolExecutionModeSchema" (Proxy @ToolExecutionMode),
        enumTests schema "WorktreeLifecycleSchema" (Proxy @WorktreeLifecycle),
        modelTests schema,
        usageTests schema droidSchema,
        utilityTests,
        contentTests schema,
        sessionTests schema droidSchema,
        messageTests schema,
        rpcTests schema,
        metadataTests schema,
        envelopeTests schema,
        toolTests schema,
        scriptTests schema,
        sourceTests schema,
        hostTests schema,
        notificationTests droidSchema,
        toolNotificationTests droidSchema,
        loopTests droidSchema,
        controlTests schema droidSchema,
        settingsTests droidSchema,
        systemPromptTests schema,
        interactionTests droidSchema,
        handlerTests,
        contextTests droidSchema,
        discoveryTests droidSchema,
        mcpTests droidSchema,
        mcpConfigTests,
        hostedMcpTests,
        missionTests droidSchema,
        missionEventTests droidSchema,
        missionObservationTests,
        missionRegistryTests,
        missionStateTests droidSchema,
        automationTests droidSchema,
        workspaceTests droidSchema daemonSchema,
        cwdStateTests,
        terminalTests schema daemonSchema,
        terminalStateTests,
        terminalRuntimeTests,
        timestampTests,
        worktreeTests daemonSchema,
        pendingInteractionTests,
        injectedSessionTests,
        inProcessTests,
        ipcTests,
        relayTests,
        tunnelTests,
        retryTests,
        connectionReadinessTests,
        processTests,
        protocolTests,
        connectionHookTests,
        initializationTests,
        loadPolicyTests,
        dispatchTests,
        clientTests schema droidSchema,
        droidTests,
        streamTests,
        legacyStreamTests,
        webSocketTests,
        daemonTests,
        automationConfigTests daemonSchema,
        daemonAutomationTests daemonSchema,
        catalogTests daemonSchema,
        customModelTests,
        defaultsTests daemonSchema,
        discoveryClientTests,
        gitResourceTests daemonSchema,
        gitTests,
        lifecycleTests,
        loadTests,
        managementTests,
        attachmentTests,
        cacheTests,
        loadCoordinationTests,
        childSessionTests,
        cronTests daemonSchema,
        softwareFactoryTests daemonSchema,
        pluginTests,
        queueTests,
        queueStateTests,
        settingsClientTests,
        workspaceClientTests,
        terminalClientTests,
        deferredInteractionTests,
        diagnosticTests,
        inputTests,
        submissionTests,
        sessionStateTests,
        savedSessionTests,
        savedSelectionTests,
        loggingTests,
        validatorTests,
        ownedIpcTests,
        restTests
      ]

enumTests ::
  forall a.
  (Bounded a, Enum a, Eq a, Show a, ToJSON a, FromJSON a) =>
  Value ->
  String ->
  Proxy a ->
  TestTree
enumTests schema name _ =
  testGroup
    name
    [ testCase "every constructor matches its canonical schema literal" $
        case schemaLiterals name schema of
          Left err -> assertFailure err
          Right literals -> do
            map toJSON values @?= literals
            forM_ (zip values literals) $ \(value, literal) ->
              fromJSON literal @?= Success value,
      testCase "unknown literals and non-strings are rejected" $
        forM_ [String "unknown", Null, Number 0, Bool True, Object mempty, Array mempty] $ \input ->
          case fromJSON input :: Result a of
            Error _ -> pure ()
            Success value -> assertFailure ("Unexpectedly decoded " <> show value),
      testProperty "encoding agrees with toJSON" $
        forAll (elements values) $ \value ->
          eitherDecode (encode value) === Right (toJSON value),
      testProperty "all values round-trip" $
        forAll (elements values) $ \value ->
          fromJSON (toJSON value) === Success value
    ]
  where
    values = [minBound .. maxBound] :: [a]

schemaLiterals :: String -> Value -> Either String [Value]
schemaLiterals name = parseEither $ withObject "schema" $ \root -> do
  definitions <- root .: "definitions"
  definition <- definitions .: fromString name
  definition .: "enum"
