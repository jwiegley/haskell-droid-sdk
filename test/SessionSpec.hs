{-# LANGUAGE OverloadedStrings #-}

module SessionSpec (sessionTests) where

import Control.Monad (forM_, unless)
import Data.Aeson
  ( FromJSON,
    Object,
    Result (..),
    ToJSON,
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
import Data.Aeson.Types (parseEither)
import Data.Char (chr)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Factory.Droid.Schema.Enums (SandboxMode (..), WorktreeLifecycle (..))
import Factory.Droid.Schema.Session
import Factory.Droid.Schema.Tools (ToolOverrideParams (..))
import SchemaTest (nonNullableRecordTests, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, (@?=))

sessionTests :: Value -> Value -> TestTree
sessionTests schema local =
  testGroup
    "Session metadata"
    [ testCase "fixtures cover all three schema field sets" $
        forM_ [("SessionIdParamsSchema", ["sessionId"]), ("SessionTagSchema", ["name", "metadata"]), ("SessionWorktreeMetadataSchema", KeyMap.keys worktreeJSON)] $ \(name, keys) -> do
          fields <- either assertFailure pure (schemaFields name schema)
          sort fields @?= sort keys,
      testCase "session identifiers are required strings, not constrained UUIDs" $ do
        forM_ ["session-id", ""] $ \identifier -> do
          let value = SessionIdParams identifier mempty
              wire = object ["sessionId" .= identifier]
          fromJSON wire @?= Success value
          toJSON value @?= wire
        rejects (Proxy @SessionIdParams) (object [])
        forM_ [Null, Number 1, Bool True] $ \value -> rejects (Proxy @SessionIdParams) (object ["sessionId" .= value]),
      testCase "session parameter extensions survive without replacing identity" $ do
        let value = SessionIdParams "session-id" (KeyMap.singleton "future" (Bool True))
        eitherDecode (encode value) @?= Right value
        toJSON (value {sessionParamsAdditionalFields = KeyMap.singleton "sessionId" (String "injected")}) @?= object ["sessionId" .= String "session-id"],
      testCase "tag names reject empty but preserve whitespace and Unicode" $ do
        mkSessionTagName "" @?= Nothing
        rejects (Proxy @SessionTagName) (String "")
        forM_ ["team", " ", "\t", "نام"] $ \text ->
          case mkSessionTagName text of
            Nothing -> assertFailure "A nonempty tag name was rejected"
            Just name -> do
              sessionTagNameText name @?= text
              toJSON name @?= String text
              fromJSON (String text) @?= Success name,
      testCase "tag metadata is optional and string-valued" $ do
        let full = object ["name" .= String "team", "metadata" .= object ["role" .= String "qa"]]
        case fromJSON full :: Result SessionTag of
          Error err -> assertFailure err
          Success tag -> do
            sessionTagNameText (sessionTagName tag) @?= "team"
            sessionTagMetadata tag @?= Just (KeyMap.singleton "role" "qa")
            sessionTagAdditionalFields tag @?= mempty
            toJSON tag @?= full
            let absent = tag {sessionTagMetadata = Nothing}
                empty = tag {sessionTagMetadata = Just mempty}
            toJSON absent @?= object ["name" .= String "team"]
            fromJSON (toJSON absent) @?= Success absent
            toJSON empty @?= object ["name" .= String "team", "metadata" .= object []]
            fromJSON (toJSON empty) @?= Success empty
        forM_ [Null, Number 1, Bool True, String "metadata", object ["role" .= Number 1], object ["role" .= Null]] $ \metadata ->
          rejects (Proxy @SessionTag) (object ["name" .= String "team", "metadata" .= metadata]),
      testCase "tag names are mandatory and typed" $
        forM_ [object [], object ["metadata" .= object []], object ["name" .= String ""], object ["name" .= Null], object ["name" .= Number 1]] $
          rejects (Proxy @SessionTag),
      testCase "tags retain extensions without reserved-key injection" $ do
        let wire = object ["name" .= String "team", "future" .= [Null, Bool True]]
        case fromJSON wire :: Result SessionTag of
          Error err -> assertFailure err
          Success tag -> do
            toJSON tag @?= wire
            eitherDecode (encode tag) @?= Right tag
            forM_ ["name", "metadata"] $ \key ->
              toJSON (tag {sessionTagAdditionalFields = KeyMap.singleton key Null}) @?= object ["name" .= String "team"],
      testCase "worktree metadata golden covers every selector" $ do
        fromJSON (Object worktreeJSON) @?= Success worktree
        toJSON worktree @?= Object worktreeJSON
        eitherDecode (encode worktree) @?= Right worktree,
      testCase "only the worktree repository root is required" $ do
        let value = object ["repoRoot" .= String "/fixture/repo"]
        fromJSON value @?= Success minimalWorktree
        toJSON minimalWorktree @?= value
        rejects (Proxy @SessionWorktreeMetadata) (Object (KeyMap.delete "repoRoot" worktreeJSON)),
      testCase "worktree nulls and unknown lifecycle values are rejected" $ do
        forM_ (KeyMap.keys worktreeJSON) $ \key ->
          rejects (Proxy @SessionWorktreeMetadata) (Object (KeyMap.insert key Null worktreeJSON))
        rejects (Proxy @SessionWorktreeMetadata) (Object (KeyMap.insert "lifecycle" (String "future") worktreeJSON)),
      testCase "worktree extensions cannot reintroduce omitted fields" $ do
        let value = worktree {worktreeAdditionalFields = KeyMap.singleton "future" (object ["nested" .= True])}
        eitherDecode (encode value) @?= Right value
        forM_ (KeyMap.keys worktreeJSON) $ \key ->
          toJSON (minimalWorktree {worktreeAdditionalFields = KeyMap.singleton key (String "injected")}) @?= toJSON minimalWorktree,
      testCase "session metadata objects reject other JSON kinds" $
        forM_ [Null, Number 0, Bool True, String "session", Array mempty] $ \value -> do
          rejects (Proxy @SessionIdParams) value
          rejects (Proxy @SessionTag) value
          rejects (Proxy @SessionWorktreeMetadata) value,
      records "SessionSchema" (SessionSnapshot [] (Just "") mempty) (SessionSnapshot [] Nothing mempty) (KeyMap.fromList ["messages" .= ([] :: [Value]), "title" .= String ""]) (KeyMap.singleton "messages" (Array mempty)) (\extras value -> value {sessionSnapshotAdditionalFields = extras}),
      records "SandboxStatusSchema" (SandboxStatus False (Just SandboxWholeProcess) mempty) (SandboxStatus False Nothing mempty) (KeyMap.fromList ["enabled" .= False, "mode" .= String "whole-process"]) (KeyMap.singleton "enabled" (Bool False)) (\extras value -> value {sandboxStatusAdditionalFields = extras}),
      records "ToolOverrideParamsSchema" overrides (ToolOverrideParams Nothing Nothing Nothing Nothing mempty) overridesJSON mempty (\extras value -> value {overrideAdditionalFields = extras}),
      testCase "session snapshots reuse complete message validation and ordering" $ do
        let message identifier = object ["id" .= String identifier, "role" .= String "user", "content" .= ([] :: [Value]), "createdAt" .= Number 0, "updatedAt" .= Number 0, "future" .= Null]
            wire = object ["messages" .= [message "first", message "second", message "first"], "title" .= String "title"]
        case fromJSON wire :: Result SessionSnapshot of
          Error err -> assertFailure err
          Success value -> do
            length (sessionMessages value) @?= 3
            toJSON value @?= wire
        forM_ [Null, object [], object ["id" .= String "id", "role" .= String "future", "content" .= ([] :: [Value]), "createdAt" .= Number 0, "updatedAt" .= Number 0]] $ \bad -> rejects (Proxy @SessionSnapshot) (object ["messages" .= [bad]]),
      testCase "sandbox and tool overrides remain data, not effective policy" $ do
        forM_ [minBound .. maxBound] $ \mode -> fromJSON (object ["enabled" .= False, "mode" .= mode]) @?= Success (SandboxStatus False (Just mode) mempty)
        rejects (Proxy @SandboxStatus) (object ["enabled" .= True, "mode" .= String "future"])
        let empty = ToolOverrideParams (Just []) (Just []) (Just []) (Just []) mempty
        fromJSON (toJSON empty) @?= Success empty
        forM_ (KeyMap.keys overridesJSON) $ \key -> rejects (Proxy @ToolOverrideParams) (Object (KeyMap.insert key (toJSON [Number 1]) overridesJSON)),
      testCase "worktree reference length constraints match the supplied schema" $ do
        schemaAt ["definitions", "WorktreeGitRefSchema", "minLength"] local @?= Right (Number 1)
        schemaAt ["definitions", "WorktreeGitRefSchema", "maxLength"] local @?= Right (Number 255)
        forM_ ["a", "😀"] $ \character -> do
          isJust (mkWorktreeGitRef (Text.replicate 255 character)) @?= True
          mkWorktreeGitRef (Text.replicate 256 character) @?= Nothing
        mkWorktreeGitRef "" @?= Nothing,
      testCase "worktree references preserve wire-valid spellings without stronger Git rules" $ do
        forM_ ["main", "refs/heads/topic", "refs//topic", "branch.lock", "@", "topic]", "نام", "a-b", "a\x180E\&b", "a\x200B\&b"] $ \text ->
          case mkWorktreeGitRef text of
            Nothing -> assertFailure "Wire-valid reference rejected"
            Just value -> do
              worktreeGitRefText value @?= text
              toJSON value @?= String text
              eitherDecode (encode value) @?= Right value,
      testCase "worktree reference structural exclusions and JSON types are enforced" $ do
        forM_ ["-main", "a..b", "../topic", "main\n", "main\r\n", "main\x2028", "main\x2029", "main\xFEFF", "a b", "a[b", "a\\b"] $ \text -> do
          mkWorktreeGitRef text @?= Nothing
          rejects (Proxy @WorktreeGitRef) (String text)
        forM_ [Null, Number 1, Bool False, Object mempty, Array mempty] $ rejects (Proxy @WorktreeGitRef),
      testCase "worktree character exclusions match the JavaScript Unicode oracle for every scalar" $ do
        -- Enumerated from the supplied regex with the Unicode flag in Node.
        let excluded = [0 .. 0x20] <> [0x2a, 0x3a, 0x3f, 0x5b, 0x5c, 0x5e, 0x7e] <> [0x7f .. 0x9f] <> [0xa0, 0x1680] <> [0x2000 .. 0x200a] <> [0x2028, 0x2029, 0x202f, 0x205f, 0x3000, 0xfeff]
        forM_ [0 .. 0x10ffff] $ \cp -> unless (cp >= 0xd800 && cp <= 0xdfff) $ do
          let actual = isJust (mkWorktreeGitRef ("a" <> Text.singleton (chr cp) <> "b"))
          assertEqual ("Unicode scalar " <> show cp) (cp `notElem` excluded) actual
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] local)

overrides :: ToolOverrideParams
overrides = ToolOverrideParams (Just ["extra", "extra"]) (Just ["shared"]) (Just ["shared"]) (Just []) mempty

overridesJSON :: Object
overridesJSON = KeyMap.fromList ["additionalToolIds" .= [String "extra", String "extra"], "enabledToolIds" .= [String "shared"], "disabledToolIds" .= [String "shared"], "restrictToolIds" .= ([] :: [Value])]

worktree :: SessionWorktreeMetadata
worktree =
  SessionWorktreeMetadata
    { worktreeRepoRoot = "/fixture/repo",
      worktreeBranch = Just "feature",
      worktreeLifecycle = Just WorktreePersistent,
      worktreeParentPath = Just "/fixture/parent",
      worktreePath = Just "/fixture/child",
      worktreeRemovedAt = Just "opaque timestamp",
      worktreeSetupProfileId = Just "profile-id",
      worktreeAdditionalFields = mempty
    }

minimalWorktree :: SessionWorktreeMetadata
minimalWorktree = SessionWorktreeMetadata "/fixture/repo" Nothing Nothing Nothing Nothing Nothing Nothing mempty

worktreeJSON :: Object
worktreeJSON =
  KeyMap.fromList
    [ "repoRoot" .= String "/fixture/repo",
      "branch" .= String "feature",
      "lifecycle" .= String "persistent",
      "parentWorktreePath" .= String "/fixture/parent",
      "path" .= String "/fixture/child",
      "removedAt" .= String "opaque timestamp",
      "setupProfileId" .= String "profile-id"
    ]

schemaFields :: Key -> Value -> Either String [Key]
schemaFields name = parseEither $ withObject "schema" $ \root -> do
  definitions <- root .: "definitions"
  definition <- definitions .: name
  properties <- definition .: "properties"
  pure (KeyMap.keys (properties :: Object))

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of
  Error _ -> pure ()
  Success _ -> assertFailure "Invalid JSON was accepted"
