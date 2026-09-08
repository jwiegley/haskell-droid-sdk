{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WorktreeSpec (worktreeTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Factory.Droid.Schema.Daemon.Worktree
import Factory.Droid.Schema.Enums (WorktreeLifecycle (..))
import Factory.Droid.Schema.Primitives (boundedTextValue, mkBoundedText, mkNonEmptyText, mkRfc3339Timestamp, mkUUIDText)
import SchemaTest (enumSchemaTest, redactedClosedRecordTests, redactedRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

worktreeTests :: Value -> TestTree
worktreeTests schema = case fixtures of
  Nothing -> testCase "validated worktree fixtures" (assertFailure "Valid fixture rejected")
  Just (profile, content, session, tree) ->
    let cleanup = CleanupWorktreeParams (managedWorktreePath tree) (Just False) (Just True) (Just False)
        cleaned = CleanupWorktreeResult "path" ["s", "s"] False False False ["warning"] (Just "branch") (Just PreservedUncommittedChanges) mempty
        archive = SessionArchiveStateChanged "session" (Just "title") (Just "opaque time") (Just "cwd") (Just "root") (Just False) mempty
        branch = WorktreeBranchChanged "path" (Just "branch") mempty
        profiles = ListWorktreeProfilesResult [profile] (Just (profileId profile)) (Just "root") mempty
        trees = ListManagedWorktreesResult [tree] (Just False) (Just True) mempty
     in testGroup
          "Worktree management bodies"
          [ closed "WorktreeSetupProfileFileContentsSchema" content (WorktreeProfileContents Nothing Nothing Nothing Nothing) contentJSON mempty,
            records "WorktreeSetupProfileFileSchema" profile (profile {profileScript = Nothing, profileCleanupScript = Nothing, profileInitialPrompt = Nothing, profileSource = Nothing}) profileJSON minimalProfileJSON (\extras value -> value {profileAdditionalFields = extras}),
            closed "DaemonListWorktreeSetupProfilesRequestParamsSchema" (ListWorktreeProfilesParams "cwd") (ListWorktreeProfilesParams "cwd") (KeyMap.singleton "cwd" (String "cwd")) (KeyMap.singleton "cwd" (String "cwd")),
            closed "DaemonDeleteWorktreeSetupProfileRequestParamsSchema" (DeleteWorktreeProfileParams "cwd" (profileId profile)) (DeleteWorktreeProfileParams "cwd" (profileId profile)) deleteJSON deleteJSON,
            records "DaemonListWorktreeSetupProfilesResultSchema" profiles (ListWorktreeProfilesResult [profile] Nothing Nothing mempty) profilesJSON (KeyMap.singleton "profiles" (toJSON [Object profileJSON])) (\extras value -> value {listedProfilesAdditionalFields = extras}),
            records "DaemonSaveWorktreeSetupProfileResultSchema" (SaveWorktreeProfileResult profile mempty) (SaveWorktreeProfileResult profile mempty) (KeyMap.singleton "profile" (Object profileJSON)) (KeyMap.singleton "profile" (Object profileJSON)) (\extras value -> value {savedProfileAdditionalFields = extras}),
            records "DaemonManagedWorktreeSessionSchema" session session sessionJSON sessionJSON (\extras value -> value {managedSessionAdditionalFields = extras}),
            records "DaemonManagedWorktreeSchema" tree (tree {managedWorktreeBranch = Nothing, managedWorktreeIsClean = Nothing, managedWorktreeSizeBytes = Nothing}) treeJSON minimalTreeJSON (\extras value -> value {managedWorktreeAdditionalFields = extras}),
            closed "DaemonListManagedWorktreesRequestParamsSchema" (ListManagedWorktreesParams (Just False)) (ListManagedWorktreesParams Nothing) (KeyMap.singleton "includeSizes" (Bool False)) mempty,
            records "DaemonListManagedWorktreesResultSchema" trees (ListManagedWorktreesResult [tree] Nothing Nothing mempty) treesJSON (KeyMap.singleton "worktrees" (toJSON [Object treeJSON])) (\extras value -> value {listedWorktreesAdditionalFields = extras}),
            closed "DaemonCleanupWorktreeRequestParamsSchema" cleanup (CleanupWorktreeParams (managedWorktreePath tree) Nothing Nothing Nothing) cleanupJSON (KeyMap.singleton "worktreePath" (String "path")),
            records "DaemonCleanupWorktreeResultSchema" cleaned (cleaned {cleanupBranch = Nothing, cleanupPreservedReason = Nothing}) cleanedJSON (KeyMap.delete "branch" (KeyMap.delete "preservedReason" cleanedJSON)) (\extras value -> value {cleanupResultAdditionalFields = extras}),
            records "DaemonSessionArchiveStateChangedNotificationParamsSchema" archive (SessionArchiveStateChanged "session" Nothing Nothing Nothing Nothing Nothing mempty) archiveJSON (KeyMap.singleton "sessionId" (String "session")) (\extras value -> value {archiveAdditionalFields = extras}),
            records "DaemonWorktreeBranchChangedNotificationParamsSchema" branch (WorktreeBranchChanged "path" Nothing mempty) branchJSON (KeyMap.singleton "checkoutPath" (String "path")) (\extras value -> value {changedWorktreeBranchAdditionalFields = extras}),
            records "DaemonWorktreeRemovedNotificationParamsSchema" (WorktreeRemoved "path" mempty) (WorktreeRemoved "path" mempty) (KeyMap.singleton "checkoutPath" (String "path")) (KeyMap.singleton "checkoutPath" (String "path")) (\extras value -> value {removedWorktreeAdditionalFields = extras}),
            enumSchemaTest "profile sources" (definition "WorktreeSetupProfileSourceSchema" >>= schemaAt ["enum"]) (Proxy @WorktreeProfileSource),
            enumSchemaTest "preservation reasons" (definition "DaemonWorktreePreservedReasonSchema" >>= schemaAt ["enum"]) (Proxy @WorktreePreservedReason),
            testCase "profile-name bounds preserve Unicode and significant whitespace" $ do
              (definition "WorktreeSetupProfileFileSchema" >>= schemaAt ["properties", "name"]) @?= Right (object ["type" .= String "string", "minLength" .= Number 1, "maxLength" .= Number 100])
              (definition "WorktreeSetupProfileFileContentsSchema" >>= schemaAt ["properties", "name"]) @?= (definition "WorktreeSetupProfileFileSchema" >>= schemaAt ["properties", "name"])
              mkWorktreeProfileName "" @?= Nothing
              forM_ [" ", Text.replicate 100 "a", Text.replicate 100 "😀"] $ \text -> case mkWorktreeProfileName text of
                Nothing -> assertFailure "Valid bounded name rejected"
                Just name -> do
                  worktreeProfileNameText name @?= text
                  fromJSON (String text) @?= Success name
                  toJSON name @?= String text
              mkWorktreeProfileName (Text.replicate 101 "😀") @?= Nothing,
            testCase "scripts and initial prompts use exact code-point bounds" $ do
              definition "WorktreeSetupScriptSchema" @?= Right (object ["type" .= String "string", "maxLength" .= Number 100000])
              let maximumText = Text.replicate 100000 "😀"
                  excessive = Text.replicate 100001 "a"
              fmap boundedTextValue (mkBoundedText @100000 maximumText) @?= Just maximumText
              fmap boundedTextValue (mkBoundedText @100000 "") @?= Just ""
              rejects (Proxy @WorktreeSetupScript) (String excessive)
              forM_ ["script", "cleanupScript", "initialPrompt"] $ \key -> do
                rejects (Proxy @WorktreeProfileContents) (Object (KeyMap.insert key (String excessive) contentJSON))
                rejects (Proxy @WorktreeProfile) (Object (KeyMap.insert key (String excessive) profileJSON)),
            testCase "nested profile and session fields retain their validated domains" $ do
              rejects (Proxy @WorktreeProfile) (Object (KeyMap.insert "name" (String "") profileJSON))
              rejects (Proxy @WorktreeProfileContents) (object ["name" .= String ""])
              rejects (Proxy @WorktreeProfile) (Object (KeyMap.insert "id" (String "not-a-uuid") profileJSON))
              rejects (Proxy @DeleteWorktreeProfileParams) (Object (KeyMap.insert "profileId" (String "bad") deleteJSON))
              rejects (Proxy @ListWorktreeProfilesResult) (Object (KeyMap.insert "lastUsedProfileId" (String "bad") profilesJSON))
              forM_ ["createdAt", "updatedAt"] $ \key -> rejects (Proxy @WorktreeProfile) (Object (KeyMap.insert key (String "not-a-timestamp") profileJSON))
              forM_ ["sessionId", "title"] $ \key -> rejects (Proxy @ManagedWorktreeSession) (Object (KeyMap.insert key (String "") sessionJSON)),
            testCase "managed path constraints do not leak to free-form result paths" $ do
              forM_ ["path", "repoRoot"] $ \key -> rejects (Proxy @ManagedWorktree) (Object (KeyMap.insert key (String "") treeJSON))
              rejects (Proxy @CleanupWorktreeParams) (object ["worktreePath" .= String ""])
              fromJSON (Object (KeyMap.insert "worktreePath" (String "") cleanedJSON)) @?= Success (cleaned {cleanupReportedPath = ""})
              fromJSON (object ["cwd" .= String ""]) @?= Success (ListWorktreeProfilesParams ""),
            testCase "size is a nonnegative integer while updatedAt is any number" $ do
              forM_ [Number (-1), Number 0.5] $ \bad -> rejects (Proxy @ManagedWorktree) (Object (KeyMap.insert "sizeBytes" bad treeJSON))
              forM_ [0, 18446744073709551616] $ \size -> fromJSON (Object (KeyMap.insert "sizeBytes" (toJSON size) treeJSON)) @?= Success (tree {managedWorktreeSizeBytes = Just size})
              forM_ [-1.25, 0, 123456789012345678901234567890] $ \time -> fromJSON (Object (KeyMap.insert "updatedAt" (Number time) sessionJSON)) @?= Success (session {managedSessionUpdatedAt = time}),
            testCase "nested record arrays reject malformed entries and permit emptiness" $ do
              forM_ [Null, object [], String "entry"] $ \bad -> do
                rejects (Proxy @ListWorktreeProfilesResult) (object ["profiles" .= [bad]])
                rejects (Proxy @SaveWorktreeProfileResult) (object ["profile" .= bad])
                rejects (Proxy @ListManagedWorktreesResult) (object ["worktrees" .= [bad]])
                rejects (Proxy @ManagedWorktree) (Object (KeyMap.insert "sessions" (toJSON [bad]) treeJSON))
              fromJSON (object ["profiles" .= ([] :: [Value])]) @?= Success (ListWorktreeProfilesResult [] Nothing Nothing mempty)
              fromJSON (object ["worktrees" .= ([] :: [Value])]) @?= Success (ListManagedWorktreesResult [] Nothing Nothing mempty)
              fromJSON (Object (KeyMap.insert "sessions" (Array mempty) treeJSON)) @?= Success (tree {managedWorktreeSessions = []}),
            testCase "closed content cannot adopt open profile metadata" $ do
              rejects (Proxy @WorktreeProfileContents) (Object (KeyMap.insert "source" (String "local") contentJSON))
              let enriched = profile {profileAdditionalFields = KeyMap.singleton "future" (object ["nested" .= Null])}
              fromJSON (toJSON enriched) @?= Success enriched,
            testCase "reports do not reconcile status flags or preservation reason" $ do
              let inconsistent = cleaned {cleanupWorktreeRemoved = True, cleanupLocalBranchDeleted = True, cleanupRemoteBranchDeleted = True}
              fromJSON (toJSON inconsistent) @?= Success inconsistent
              forM_ ["archivedSessionIds", "warnings"] $ \key -> rejects (Proxy @CleanupWorktreeResult) (Object (KeyMap.insert key (toJSON [Number 1]) cleanedJSON))
              rejects (Proxy @CleanupWorktreeResult) (Object (KeyMap.insert "preservedReason" (String "future") cleanedJSON))
              rejects (Proxy @ManagedWorktree) (Object (KeyMap.insert "lifecycle" (String "future") treeJSON))
              rejects (Proxy @WorktreeProfile) (Object (KeyMap.insert "source" (String "future") profileJSON))
          ]
  where
    definition name = schemaAt ["definitions", name] schema
    records :: (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = redactedRecordTests name (definition name)
    closed :: (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> TestTree
    closed name = redactedClosedRecordTests name (definition name)

fixtures :: Maybe (WorktreeProfile, WorktreeProfileContents, ManagedWorktreeSession, ManagedWorktree)
fixtures = do
  identifier <- mkUUIDText uuidText
  timestamp <- mkRfc3339Timestamp timestampText
  name <- mkWorktreeProfileName "profile"
  script <- mkBoundedText @100000 "fixture setup"
  cleanup <- mkBoundedText @100000 "fixture cleanup"
  prompt <- mkBoundedText @100000 "fixture prompt"
  sessionId <- mkNonEmptyText "session"
  title <- mkNonEmptyText "title"
  path <- mkNonEmptyText "path"
  root <- mkNonEmptyText "root"
  let profile = WorktreeProfile identifier name timestamp timestamp (Just script) (Just cleanup) (Just prompt) (Just ProfileRepository) mempty
      content = WorktreeProfileContents (Just name) (Just script) (Just cleanup) (Just prompt)
      session = ManagedWorktreeSession sessionId title (-1.25) mempty
      tree = ManagedWorktree path root WorktreePersistent [session] (Just "branch") (Just False) (Just 0) mempty
  pure (profile, content, session, tree)

uuidText, timestampText :: Text
uuidText = "123E4567-E89B-12D3-A456-426614174000"
timestampText = "2026-09-05t12:30:00-00:00"

contentJSON, minimalProfileJSON, profileJSON, profilesJSON, deleteJSON, sessionJSON, minimalTreeJSON, treeJSON, treesJSON, cleanupJSON, cleanedJSON, archiveJSON, branchJSON :: Object
contentJSON = KeyMap.fromList ["name" .= String "profile", "script" .= String "fixture setup", "cleanupScript" .= String "fixture cleanup", "initialPrompt" .= String "fixture prompt"]
minimalProfileJSON = KeyMap.fromList ["id" .= String uuidText, "name" .= String "profile", "createdAt" .= String timestampText, "updatedAt" .= String timestampText]
profileJSON = KeyMap.union minimalProfileJSON (KeyMap.fromList ["script" .= String "fixture setup", "cleanupScript" .= String "fixture cleanup", "initialPrompt" .= String "fixture prompt", "source" .= String "repository"])
profilesJSON = KeyMap.fromList ["profiles" .= [Object profileJSON], "lastUsedProfileId" .= String uuidText, "repoRoot" .= String "root"]
deleteJSON = KeyMap.fromList ["cwd" .= String "cwd", "profileId" .= String uuidText]
sessionJSON = KeyMap.fromList ["sessionId" .= String "session", "title" .= String "title", "updatedAt" .= Number (-1.25)]
minimalTreeJSON = KeyMap.fromList ["path" .= String "path", "repoRoot" .= String "root", "lifecycle" .= String "persistent", "sessions" .= [Object sessionJSON]]
treeJSON = KeyMap.union minimalTreeJSON (KeyMap.fromList ["branch" .= String "branch", "isClean" .= False, "sizeBytes" .= Number 0])
treesJSON = KeyMap.fromList ["worktrees" .= [Object treeJSON], "cleanlinessPending" .= False, "sizesPending" .= True]
cleanupJSON = KeyMap.fromList ["worktreePath" .= String "path", "deleteLocalBranch" .= False, "deleteRemoteBranch" .= True, "force" .= False]
cleanedJSON = KeyMap.fromList ["worktreePath" .= String "path", "archivedSessionIds" .= [String "s", String "s"], "worktreeRemoved" .= False, "localBranchDeleted" .= False, "remoteBranchDeleted" .= False, "warnings" .= [String "warning"], "branch" .= String "branch", "preservedReason" .= String "uncommitted_changes"]
archiveJSON = KeyMap.fromList ["sessionId" .= String "session", "title" .= String "title", "archivedAt" .= String "opaque time", "cwd" .= String "cwd", "repoRoot" .= String "root", "worktreeRemoved" .= False]
branchJSON = KeyMap.fromList ["checkoutPath" .= String "path", "branch" .= String "branch"]
