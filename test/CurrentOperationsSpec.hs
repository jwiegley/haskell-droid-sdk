{-# LANGUAGE OverloadedStrings #-}

module CurrentOperationsSpec (currentOperationTests) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (fromException, try)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key, fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Daemon.SoftwareFactory
import Factory.Droid.Schema.Daemon.Workspace
import Factory.Droid.Schema.Daemon.Worktree
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Models
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcConflict), WithEnvelope (..))
import Factory.Droid.Transport (objectTransport)
import ProcessSpec (bounded)
import ProtocolSpec (feed, reply, withMemory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

currentOperationTests :: TestTree
currentOperationTests =
  testGroup
    "Current daemon operations"
    [ operation "models" "daemon.list_models" (pure (ListModelsOptions (Just False) mempty)) (object ["includeDisabled" .= False]) modelResult modelResult (object ["models" .= [object []]]) Client.listDaemonModels Daemon.listModels,
      operation "profile list" "daemon.list_worktree_setup_profiles" (pure (ListWorktreeProfilesParams "")) (object ["cwd" .= String ""]) profileListRaw profileListExpected (object ["profiles" .= [Null]]) Client.listDaemonWorktreeSetupProfiles Daemon.listWorktreeSetupProfiles,
      operation "profile save" "daemon.save_worktree_setup_profile" (pure saveParams) saveExpected (object ["profile" .= profileRaw]) (object ["profile" .= profileExpected]) (object ["profile" .= Null]) Client.saveDaemonWorktreeSetupProfile Daemon.saveWorktreeSetupProfile,
      operation "profile delete" "daemon.delete_worktree_setup_profile" (decoded (object ["cwd" .= String "/repo", "profileId" .= profileIdValue])) (object ["cwd" .= String "/repo", "profileId" .= profileIdValue]) (object ["success" .= False, "future" .= extra]) (object ["success" .= False, "future" .= extra]) (object ["success" .= Null]) Client.deleteDaemonWorktreeSetupProfile Daemon.deleteWorktreeSetupProfile,
      operation "worktree list" "daemon.list_managed_worktrees" (pure (ListManagedWorktreesParams (Just False))) (object ["includeSizes" .= False]) treesResult treesResult (object ["worktrees" .= [object []]]) Client.listDaemonManagedWorktrees Daemon.listManagedWorktrees,
      operation "worktree cleanup" "daemon.cleanup_worktree" (decoded cleanupParams) cleanupParams cleanupResult cleanupResult (alter "worktreeRemoved" Null cleanupResult) Client.cleanupDaemonWorktree Daemon.cleanupWorktree,
      operation "worktree inspection" "daemon.inspect_worktree_deletion" (decoded (object ["worktreePath" .= String "/tree"])) (object ["worktreePath" .= String "/tree"]) inspectionResult inspectionResult (alter "pullRequest" (object ["state" .= String "invalid"]) inspectionResult) Client.inspectDaemonWorktreeDeletion Daemon.inspectWorktreeDeletion,
      operation "workstream publish" "daemon.sf.publish_workstream_content" (pure (SfPublishWorkstreamContentParams "" 0 (KeyMap.singleton "future" extra))) (object ["workstreamId" .= String "", "expectedGeneration" .= (0 :: Int), "future" .= extra]) (contentResult 1) (contentResult 1) (contentResult 0) Client.publishDaemonSfWorkstreamContent Daemon.sfPublishWorkstreamContent,
      operation "workstream hydrate" "daemon.sf.hydrate_workstream_content" (pure (SfHydrateWorkstreamContentParams "" (KeyMap.singleton "future" extra))) (object ["workstreamId" .= String "", "future" .= extra]) (contentResult 0) (contentResult 0) (contentResult (-1)) Client.hydrateDaemonSfWorkstreamContent Daemon.sfHydrateWorkstreamContent,
      operation "workspace write" "daemon.write_workspace_file_content" (pure (WriteWorkspaceFileContentParams "session" "a.txt" "λ\n" (Just "") (KeyMap.singleton "future" extra))) (object ["sessionId" .= String "session", "filePath" .= String "a.txt", "content" .= String "λ\n", "baseFingerprint" .= String "", "future" .= extra]) writeResult writeResult (alter "byteLength" (Number (-1)) writeResult) Client.writeDaemonWorkspaceFileContent Daemon.writeWorkspaceFileContent,
      testCase "default model and worktree listing options remain absent" $ bounded $ do
        withOwner (Just (Right modelResult)) $ \connection sent -> do
          void (Daemon.listModels connection (ListModelsOptions Nothing mempty))
          atomically (readTQueue sent) >>= (@?= object []) . field "params"
        withOwner (Just (Right treesResult)) $ \connection sent -> do
          void (Daemon.listManagedWorktrees connection (ListManagedWorktreesParams Nothing))
          atomically (readTQueue sent) >>= (@?= object []) . field "params",
      testCase "explicit true listing options are forwarded rather than replaced by defaults" $ bounded $ do
        withOwner (Just (Right modelResult)) $ \connection sent -> do
          void (Daemon.listModels connection (ListModelsOptions (Just True) mempty))
          atomically (readTQueue sent) >>= (@?= object ["includeDisabled" .= True]) . field "params"
        withOwner (Just (Right treesResult)) $ \connection sent -> do
          void (Daemon.listManagedWorktrees connection (ListManagedWorktreesParams (Just True)))
          atomically (readTQueue sent) >>= (@?= object ["includeSizes" .= True]) . field "params",
      testCase "saving an existing profile preserves its UUID and untrimmed cwd" $ bounded $ withOwner (Just (Right (object ["profile" .= profileRaw]))) $ \connection sent -> do
        identifier <- decoded profileIdValue
        void (Daemon.saveWorktreeSetupProfile connection (saveParams {saveProfileId = Just identifier}))
        atomically (readTQueue sent) >>= (@?= alter "profileId" profileIdValue saveExpected) . field "params",
      testCase "empty content is still a write and an absent fingerprint stays absent" $ bounded $ withOwner (Just (Right (object ["byteLength" .= (0 :: Int), "fingerprint" .= String ""]))) $ \connection sent -> do
        result <- Daemon.writeWorkspaceFileContent connection (WriteWorkspaceFileContentParams "session" "a.txt" "" Nothing mempty)
        writtenFileByteLength result @?= 0
        writtenFileFingerprint result @?= ""
        atomically (readTQueue sent) >>= (@?= object ["sessionId" .= String "session", "filePath" .= String "a.txt", "content" .= String ""]) . field "params",
      testCase "profile validation trims before UTF-16 bounds and requires nonempty content before sending" $ bounded $ do
        forM_ invalidSaves $ \(name, params, expected) -> withOwner Nothing $ \connection sent -> do
          outcome <- try @WorktreeProfileError (Daemon.saveWorktreeSetupProfile connection params)
          case outcome of Left cause -> cause @?= expected; Right _ -> assertFailure ("Invalid profile accepted: " <> name)
          atomically (tryReadTQueue sent) >>= (@?= Nothing)
        forM_ validSaves $ \(params, expectedName, expectedScript) -> withOwner (Just (Right (object ["profile" .= profileRaw]))) $ \connection sent -> do
          void (Daemon.saveWorktreeSetupProfile connection params)
          request <- atomically (readTQueue sent)
          let fields = field "params" request
          valueField "name" fields @?= String expectedName
          valueField "script" fields @?= maybe Null String expectedScript,
      testCase "new operation bodies reject null missing and invalid fields without inventing count constraints" $ do
        forM_ ["script", "cleanupScript", "initialPrompt", "profileId"] $ \key -> rejects (Proxy @SaveWorktreeProfileParams) (alter key Null saveExpected)
        rejects (Proxy @SaveWorktreeProfileParams) (alter "future" (Bool True) saveExpected)
        rejects (Proxy @SaveWorktreeProfileParams) (alter "profileId" (String "bad") saveExpected)
        forM_ [Number (-1), Number 0.5, Null] $ \generation -> rejects (Proxy @SfPublishWorkstreamContentParams) (object ["workstreamId" .= String "", "expectedGeneration" .= generation])
        rejects (Proxy @SfPublishWorkstreamContentParams) (object ["workstreamId" .= String ""])
        rejects (Proxy @SfHydrateWorkstreamContentParams) (object ["idOrSlug" .= String "not-the-content-key"])
        rejects (Proxy @InspectWorktreeDeletionParams) (object ["worktreePath" .= String ""])
        rejects (Proxy @InspectWorktreeDeletionParams) (object ["worktreePath" .= String "/tree", "force" .= True])
        forM_ ["branch", "localOnlyCommits", "pullRequest"] $ \key -> rejects (Proxy @InspectWorktreeDeletionResult) (alter key Null inspectionResult)
        let fractional = alter "changedFiles" (Number (-0.25)) (alter "localOnlyCommits" (Number 1.5) inspectionResult)
        inspection <- decoded @InspectWorktreeDeletionResult fractional
        toJSON inspection @?= fractional
        let exactCount = alter "totalBytes" (toJSON (9007199254740993 :: Integer)) (contentResult 0)
        content <- decoded @SfWorkstreamContentResult exactCount
        toJSON content @?= exactCount,
      testCase "profile receipts do not inherit the save-request content refinement" $ do
        let minimal = object ["id" .= profileIdValue, "name" .= String "Profile", "createdAt" .= String "2026-09-01T00:00:00Z", "updatedAt" .= String "2026-09-02T00:00:00Z"]
        parsed <- either assertFailure pure (parseEither parseSavedWorktreeSetupProfileResult (object ["profile" .= minimal]))
        toJSON parsed @?= object ["profile" .= minimal],
      testCase "SDK profile parsing validates UTC seconds without weakening the raw wire codecs" $ do
        forM_ ["2026-09-01T00:00:00+00:00", "2026-09-01T00:00Z", "2026-09-01T00:00:60Z", "2026-02-30T00:00:00Z"] $ \timestamp ->
          case parseEither parseSavedWorktreeSetupProfileResult (object ["profile" .= alter "createdAt" (String timestamp) profileRaw]) of
            Left _ -> pure ()
            Right _ -> assertFailure "Invalid SDK profile timestamp accepted"
        let padded = alter "script" (String (Text.replicate 100001 " " <> "echo ready")) (alter "name" (String (Text.replicate 200 " " <> "Profile")) profileRaw)
        parsed <- either assertFailure pure (parseEither parseSavedWorktreeSetupProfileResult (object ["profile" .= padded]))
        toJSON parsed @?= object ["profile" .= profileExpected]
        case fromJSON (String (Text.replicate 100 "😀")) of
          Success name -> worktreeProfileNameText name @?= Text.replicate 100 "😀"
          Error message -> assertFailure message
    ]

operation :: (ToJSON result) => String -> Text -> IO params -> Value -> Value -> Value -> Value -> (RpcChannel -> Client.CallOptions -> params -> IO result) -> (Daemon.DaemonConnection -> params -> IO result) -> TestTree
operation name method makeParams expectedParams receipt expectedResult invalidResult low high =
  testGroup
    name
    [ testCase "named low-level call preserves its method options and full result" $ bounded $ withMemory $ \channel incoming sent -> do
        params <- makeParams
        withAsync (low channel callOptions params) $ \pending -> do
          frame <- atomically (readTQueue sent)
          checkFrame method expectedParams frame
          field "id" frame @?= String "operation-request"
          feed incoming (reply "operation-request" receipt)
          wait pending >>= (@?= expectedResult) . toJSON,
      testCase "existing daemon owner sends once without loading a cached inactive session" $ bounded $ withOwner (Just (Right receipt)) $ \connection sent -> do
        void (Daemon.registerSessionState connection "session" "machine")
        params <- makeParams
        high connection params >>= (@?= expectedResult) . toJSON
        atomically (readTQueue sent) >>= checkFrame method expectedParams
        atomically (tryReadTQueue sent) >>= (@?= Nothing)
        Daemon.getSessionReadiness connection "session" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase,
      testCase "remote conflict preserves its error data without retry or result parsing" $ bounded $ withOwner (Just (Left remoteError)) $ \connection sent -> do
        params <- makeParams
        result <- try @RpcResultError (high connection params)
        case result of Left (RpcRemoteFailure errorValue) -> toJSON errorValue @?= remoteError; _ -> assertFailure "Remote error lost"
        atomically (readTQueue sent) >>= checkFrame method expectedParams
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "a missing wire result retains the protocol error before domain parsing" $ bounded $ withMemory $ \channel incoming sent -> do
        params <- makeParams
        withAsync (try @RpcChannelError (toJSON <$> low channel callOptions params)) $ \pending -> do
          atomically (readTQueue sent) >>= checkFrame method expectedParams
          feed incoming (KeyMap.delete "result" (reply "operation-request" Null))
          wait pending >>= (@?= Left RpcMalformedResponse),
      testCase "invalid result is not accepted as a successful operation" $ bounded $ withOwner (Just (Right invalidResult)) $ \connection sent -> do
        params <- makeParams
        result <- try @RpcResultError (toJSON <$> high connection params)
        result @?= Left RpcInvalidResult
        atomically (readTQueue sent) >>= checkFrame method expectedParams,
      testCase "explicit zero deadline sends nothing" $ bounded $ withMemory $ \channel _ sent -> do
        params <- makeParams
        result <- try @RpcChannelError (toJSON <$> low channel (callOptions {Client.callTimeoutMicros = Just 0}) params)
        result @?= Left RpcRequestTimedOut
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "closed owner sends nothing" $ bounded $ do
        (closed, sent) <- withOwner Nothing (curry pure)
        params <- makeParams
        result <- try @RpcChannelError (toJSON <$> high closed params)
        result @?= Left RpcChannelClosed
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "cancellation preserves the caller exception without a second request" $ bounded $ withOwner Nothing $ \connection sent -> do
        params <- makeParams
        withAsync (high connection params) $ \pending -> do
          atomically (readTQueue sent) >>= checkFrame method expectedParams
          cancel pending
          waitCatch pending >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled operation returned"
        atomically (tryReadTQueue sent) >>= (@?= Nothing)
    ]

callOptions :: Client.CallOptions
callOptions = Client.CallOptions "operation-request" (WithEnvelope (Just "1.201.1") Nothing mempty) (Just 1000000)

checkFrame :: Text -> Value -> Object -> IO ()
checkFrame method params frame = do
  field "method" frame @?= String method
  field "params" frame @?= params
  field "factoryProtocolVersion" frame @?= String "1.201.1"
  field "jsonrpc" frame @?= String "2.0"

withOwner :: Maybe (Either Value Value) -> (Daemon.DaemonConnection -> TQueue Object -> IO a) -> IO a
withOwner response action = do
  incoming <- newTQueueIO
  sent <- newTQueueIO
  let send frame = do
        atomically (writeTQueue sent frame)
        forM_ response $ \answer -> case field "id" frame of
          String identifier -> atomically (writeTQueue incoming (either (\err -> KeyMap.insert "error" err (KeyMap.delete "result" (reply identifier Null))) (reply identifier) answer))
          _ -> assertFailure "Fixture expected a correlated request"
      options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "") "/offline") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
  Daemon.withConnectionOn options (objectTransport send (atomically (readTQueue incoming))) (`action` sent)

field :: Text -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup (fromText key)

valueField :: Text -> Value -> Value
valueField key (Object fields) = field key fields
valueField _ _ = Null

alter :: Key -> Value -> Value -> Value
alter key value (Object fields) = Object (KeyMap.insert key value fields)
alter _ _ _ = error "Expected an object fixture"

decoded :: (FromJSON a) => Value -> IO a
decoded value = case fromJSON value of Success result -> pure result; Error message -> assertFailure message

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of Error _ -> pure (); Success _ -> assertFailure "Invalid operation body accepted"

extra, remoteError, profileIdValue, profileRaw, profileExpected, profileListRaw, profileListExpected, saveExpected, modelResult, treesResult, cleanupParams, cleanupResult, inspectionResult, writeResult :: Value
extra = object ["exact" .= (9007199254740993 :: Integer), "empty" .= String ""]
remoteError = object ["code" .= RpcConflict, "message" .= String "fixture conflict", "data" .= extra]
profileIdValue = String "00000000-0000-4000-8000-000000000001"
profileRaw = object ["id" .= profileIdValue, "name" .= String "  Profile\xa0", "script" .= String "\xfeff\&echo ready \n", "cleanupScript" .= String " \t", "initialPrompt" .= String " hello ", "source" .= String "local", "createdAt" .= String "2026-09-01T00:00:00Z", "updatedAt" .= String "2026-09-02T00:00:00Z", "future" .= extra]
profileExpected = object ["id" .= profileIdValue, "name" .= String "Profile", "script" .= String "echo ready", "cleanupScript" .= String "", "initialPrompt" .= String "hello", "source" .= String "local", "createdAt" .= String "2026-09-01T00:00:00Z", "updatedAt" .= String "2026-09-02T00:00:00Z", "future" .= extra]
profileListRaw = object ["profiles" .= [profileRaw], "lastUsedProfileId" .= profileIdValue, "repoRoot" .= String "", "future" .= extra]
profileListExpected = object ["profiles" .= [profileExpected], "lastUsedProfileId" .= profileIdValue, "repoRoot" .= String "", "future" .= extra]
saveExpected = object ["cwd" .= String " /repo ", "name" .= String "Profile", "script" .= String "echo ready", "cleanupScript" .= String "", "initialPrompt" .= String "hello"]
modelResult = object ["models" .= [object ["id" .= String "model-alpha", "displayName" .= String "Model Alpha", "shortDisplayName" .= String "Alpha", "modelProvider" .= String "factory", "supportedReasoningEfforts" .= [String "low", String "high"], "defaultReasoningEffort" .= String "high", "isCustom" .= False, "disabled" .= False]], "future" .= extra]
treesResult = object ["worktrees" .= [object ["path" .= String "/tree", "repoRoot" .= String "/repo", "lifecycle" .= String "ephemeral", "sessions" .= [object ["sessionId" .= String "s", "title" .= String "title", "updatedAt" .= (12.5 :: Double)]], "branch" .= String "", "isClean" .= False, "sizeBytes" .= (0 :: Int), "future" .= extra]], "cleanlinessPending" .= False, "sizesPending" .= True, "future" .= extra]
cleanupParams = object ["worktreePath" .= String "/tree", "deleteLocalBranch" .= False, "deleteRemoteBranch" .= False, "force" .= False]
cleanupResult = object ["worktreePath" .= String "/tree", "branch" .= String "", "archivedSessionIds" .= [String "s"], "worktreeRemoved" .= False, "preservedReason" .= String "uncommitted_changes", "localBranchDeleted" .= False, "remoteBranchDeleted" .= False, "warnings" .= [String "kept"], "future" .= extra]
inspectionResult = object ["worktreePath" .= String "/tree", "branch" .= String "", "changedFiles" .= (2 :: Int), "additions" .= (3 :: Int), "deletions" .= (4 :: Int), "untrackedFiles" .= (5 :: Int), "localOnlyCommits" .= (0 :: Int), "pullRequest" .= object ["state" .= String "open", "url" .= String "opaque-url", "title" .= String "", "future" .= extra], "hasRemoteBranch" .= False, "remoteRefsStale" .= True, "future" .= extra]
writeResult = object ["byteLength" .= (3 :: Int), "fingerprint" .= String "opaque-fingerprint", "future" .= extra]

contentResult :: Integer -> Value
contentResult generation = object ["generation" .= generation, "fileCount" .= (0 :: Int), "totalBytes" .= (0 :: Int), "workstreamDir" .= String "", "future" .= extra]

saveParams :: SaveWorktreeProfileParams
saveParams = (defaultSaveWorktreeProfileParams " /repo " "  Profile\xa0") {saveProfileScript = Just "\xfeff\&echo ready \n", saveProfileCleanupScript = Just " \t", saveProfileInitialPrompt = Just " hello "}

invalidSaves :: [(String, SaveWorktreeProfileParams, WorktreeProfileError)]
invalidSaves =
  [ ("missing", defaultSaveWorktreeProfileParams "" "Profile", WorktreeProfileContentRequired),
    ("trimmed empty", (defaultSaveWorktreeProfileParams "" "Profile") {saveProfileScript = Just " \xfeff\xa0\n"}, WorktreeProfileContentRequired),
    ("empty name", saveParams {saveProfileName = " \xfeff\xa0"}, InvalidWorktreeProfileName),
    ("long name", saveParams {saveProfileName = Text.replicate 51 "😀"}, InvalidWorktreeProfileName),
    ("long script", saveParams {saveProfileScript = Just (Text.replicate 50001 "😀")}, WorktreeProfileContentTooLong)
  ]

validSaves :: [(SaveWorktreeProfileParams, Text, Maybe Text)]
validSaves =
  [ (saveParams {saveProfileName = Text.replicate 50 "😀"}, Text.replicate 50 "😀", Just "echo ready"),
    (saveParams {saveProfileName = Text.replicate 200 " " <> "Profile"}, "Profile", Just "echo ready"),
    (saveParams {saveProfileScript = Just (Text.replicate 50000 "😀")}, "Profile", Just (Text.replicate 50000 "😀")),
    (saveParams {saveProfileScript = Just (Text.replicate 100001 " " <> "x")}, "Profile", Just "x"),
    ((defaultSaveWorktreeProfileParams "" "Profile") {saveProfileCleanupScript = Just " clean "}, "Profile", Nothing),
    ((defaultSaveWorktreeProfileParams "" "Profile") {saveProfileInitialPrompt = Just " prompt "}, "Profile", Nothing)
  ]
