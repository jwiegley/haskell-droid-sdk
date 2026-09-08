{-# LANGUAGE OverloadedStrings #-}

module ControlSpec (controlTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON,
    Object,
    Result (..),
    ToJSON,
    Value (..),
    fromJSON,
    object,
    toJSON,
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Factory.Droid.Schema.Content
import Factory.Droid.Schema.Control
import Factory.Droid.Schema.Enums (MessageRole (..), MessageVisibility (..), SessionOrigin (..))
import Factory.Droid.Schema.Messages (FactoryDroidMessage, Message (..))
import Factory.Droid.Schema.Session (SessionTag)
import Factory.Droid.Schema.Sources (BugReportSource (..), BugReportSurface (..))
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

controlTests :: Value -> Value -> TestTree
controlTests shared schema = case fromJSON messageJSON :: Result FactoryDroidMessage of
  Error err -> testCase "fixture message" (assertFailure err)
  Success message -> case mkUserOnlyMessage message of
    Nothing -> testCase "user-only fixture" (assertFailure "Visibility validation rejected fixture")
    Just userOnly ->
      let append = AppendMessagesParams (userOnly :| []) mempty
          records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
          records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)
       in testGroup
            "Session control bodies"
            [ records "OutputFormatSchema" outputFormat outputFormat outputFormatJSON outputFormatJSON (\extras value -> value {outputFormatAdditionalFields = extras}),
              records "AddUserMessageRequestParamsSchema" userParams minimalUserParams userParamsJSON (KeyMap.singleton "text" (String "")) (\extras value -> value {userMessageAdditionalFields = extras}),
              records "AppendMessagesRequestParamsSchema" append append appendJSON appendJSON (\extras value -> value {appendAdditionalFields = extras}),
              records "RewindFileSnapshotSchema" snapshot snapshot snapshotJSON snapshotJSON (\extras value -> value {rewindSnapshotAdditionalFields = extras}),
              records "RewindFileCreationSchema" creation creation creationJSON creationJSON (\extras value -> value {rewindCreationAdditionalFields = extras}),
              records "RewindEvictedFileSchema" evicted evicted evictedJSON evictedJSON (\extras value -> value {rewindEvictedAdditionalFields = extras}),
              records "GetRewindInfoRequestParamsSchema" infoParams infoParams infoParamsJSON infoParamsJSON (\extras value -> value {rewindInfoParamsAdditionalFields = extras}),
              records "GetRewindInfoResultSchema" infoResult infoResult infoResultJSON infoResultJSON (\extras value -> value {rewindInfoAdditionalFields = extras}),
              records "ExecuteRewindRequestParamsSchema" rewindParams rewindParams rewindParamsJSON rewindParamsJSON (\extras value -> value {executeRewindAdditionalFields = extras}),
              records "ExecuteRewindResultSchema" rewindResult rewindResult rewindResultJSON rewindResultJSON (\extras value -> value {rewindResultAdditionalFields = extras}),
              records "CompactSessionRequestParamsSchema" compactParams (CompactSessionParams Nothing mempty) compactParamsJSON mempty (\extras value -> value {compactionParamsAdditionalFields = extras}),
              records "CompactSessionResultSchema" compactResult compactResult compactResultJSON compactResultJSON (\extras value -> value {compactionResultAdditionalFields = extras}),
              nonNullableRecordTests "ForkSessionTag" (schemaAt ["definitions", "ForkSessionRequestParamsSchema", "properties", "tags", "items"] schema) forkTag (forkTag {forkTagMetadata = Nothing}) forkTagJSON (KeyMap.delete "metadata" forkTagJSON) (\extras value -> value {forkTagAdditionalFields = extras}),
              records "ForkSessionRequestParamsSchema" forkParams (ForkSessionParams Nothing Nothing mempty) forkParamsJSON mempty (\extras value -> value {forkSessionAdditionalFields = extras}),
              records "ForkSessionResultSchema" forkResult forkResult forkResultJSON forkResultJSON (\extras value -> value {forkResultAdditionalFields = extras}),
              records "RenameSessionRequestParamsSchema" (RenameSessionParams "renamed" mempty) (RenameSessionParams "renamed" mempty) renameJSON renameJSON (\extras value -> value {renameAdditionalFields = extras}),
              records "ChangeWorkingDirectoryRequestParamsSchema" (ChangeWorkingDirectoryParams "requested path" mempty) (ChangeWorkingDirectoryParams "requested path" mempty) changeParamsJSON changeParamsJSON (\extras value -> value {workingDirectoryParamsAdditionalFields = extras}),
              records "ChangeWorkingDirectoryResultSchema" (ChangeWorkingDirectoryResult "resolved path" mempty) (ChangeWorkingDirectoryResult "resolved path" mempty) changeResultJSON changeResultJSON (\extras value -> value {changedDirectoryAdditionalFields = extras}),
              records "ValidateWorkingDirectoryResultSchema" directoryResult (ValidateWorkingDirectoryResult False Nothing Nothing mempty) directoryJSON (KeyMap.singleton "isValid" (Bool False)) (\extras value -> value {directoryValidationAdditionalFields = extras}),
              records "CloseSessionRequestParamsSchema" (CloseSessionParams (Just CloseOther) mempty) (CloseSessionParams Nothing mempty) closeJSON mempty (\extras value -> value {closeAdditionalFields = extras}),
              records "KillWorkerSessionRequestParamsSchema" (KillWorkerSessionParams "worker-id" mempty) (KillWorkerSessionParams "worker-id" mempty) killJSON killJSON (\extras value -> value {killWorkerAdditionalFields = extras}),
              records "SubmitBugReportRequestParamsSchema" bugParams (SubmitBugReportParams "comment" Nothing Nothing mempty) bugParamsJSON (KeyMap.singleton "userComment" (String "comment")) (\extras value -> value {bugReportParamsAdditionalFields = extras}),
              records "SubmitBugReportResultSchema" (SubmitBugReportResult "report-id" mempty) (SubmitBugReportResult "report-id" mempty) bugResultJSON bugResultJSON (\extras value -> value {submittedReportAdditionalFields = extras}),
              testCase "user content must be nonempty and limited to text/image blocks" $ do
                let input content = object ["text" .= String "", "content" .= content]
                forM_ [Array mempty, toJSON [Null], toJSON [object []], toJSON [object ["type" .= String "document", "source" .= Object userFileJSON]], toJSON [object ["type" .= String "tool_use", "id" .= String "id", "name" .= String "name", "input" .= object []]]] $ \bad ->
                  rejects (Proxy @AddUserMessageParams) (input bad)
                forM_ [Null, Bool False, Number 1, String "text", Array mempty] $ rejects (Proxy @UserMessageContent),
              testCase "user content and attachments reuse nested validation" $ do
                let badSource = Object (KeyMap.insert "mediaType" (String "image/bmp") userImageSourceJSON)
                rejects (Proxy @AddUserMessageParams) (object ["text" .= String "", "images" .= [badSource]])
                rejects (Proxy @AddUserMessageParams) (object ["text" .= String "", "content" .= [object ["type" .= String "image", "source" .= badSource]]])
                rejects (Proxy @AddUserMessageParams) (object ["text" .= String "", "files" .= [Object userImageSourceJSON]])
                forM_ ["imagePaths", "images", "files"] $ \key -> rejects (Proxy @AddUserMessageParams) (object ["text" .= String "", key .= [Null]]),
              testCase "optional user attachments may be empty without rewriting text" $ do
                let empty = minimalUserParams {userMessageImages = Just [], userMessageImagePaths = Just [], userMessageFiles = Just []}
                    wire = object ["text" .= String "", "images" .= ([] :: [Value]), "imagePaths" .= ([] :: [Value]), "files" .= ([] :: [Value])]
                fromJSON wire @?= Success empty
                toJSON empty @?= wire,
              testCase "ordered user content retains nested extensions" $ do
                let block = TextBlock "" (BaseContentBlock Nothing (KeyMap.singleton "future" Null))
                    payload = minimalUserParams {userMessageContent = Just (UserMessageText block :| [UserMessageText block])}
                    wire = object ["text" .= String "", "content" .= [object ["type" .= String "text", "text" .= String "", "future" .= Null], object ["type" .= String "text", "text" .= String "", "future" .= Null]]]
                fromJSON wire @?= Success payload
                toJSON payload @?= wire,
              testCase "user input roles, visibility, origin and placement remain strict" $ do
                forM_ ["role", "visibility", "userMessageSource", "queuePlacement"] $ \key ->
                  rejects (Proxy @AddUserMessageParams) (Object (KeyMap.insert key (String "future") userParamsJSON))
                rejects (Proxy @AddUserMessageParams) (object ["text" .= String "", "outputFormat" .= object ["type" .= String "json_schema", "schema" .= Bool True]]),
              enumTests schema ["QueuePlacementSchema", "enum"] (Proxy @QueuePlacement),
              enumTests schema ["CloseSessionRequestParamsSchema", "properties", "reason", "enum"] (Proxy @CloseSessionReason),
              testCase "queue update and delete preserve their distinct requirements" $ do
                let update = ResolveQueuedMessageParams "request-id" (UpdateQueuedMessage QueueEndOfLoop) mempty
                    deletion = ResolveQueuedMessageParams "request-id" DeleteQueuedMessage mempty
                fromJSON (Object updateJSON) @?= Success update
                toJSON update @?= Object updateJSON
                fromJSON (Object deleteJSON) @?= Success deletion
                toJSON deletion @?= Object deleteJSON
                rejects (Proxy @ResolveQueuedMessageParams) (Object (KeyMap.delete "queuePlacement" updateJSON))
                rejects (Proxy @ResolveQueuedMessageParams) (Object (KeyMap.insert "queuePlacement" (String "future") updateJSON))
                rejects (Proxy @ResolveQueuedMessageParams) (Object (KeyMap.insert "action" (String "future") deleteJSON)),
              testCase "queue extensions reserve only the selected branch fields" $ do
                let extra = KeyMap.singleton "queuePlacement" (String "opaque extension")
                    deletion = ResolveQueuedMessageParams "request-id" DeleteQueuedMessage extra
                    update = ResolveQueuedMessageParams "request-id" (UpdateQueuedMessage QueueEndOfLoop) extra
                toJSON deletion @?= Object (KeyMap.insert "queuePlacement" (String "opaque extension") deleteJSON)
                fromJSON (toJSON deletion) @?= Success deletion
                toJSON update @?= Object updateJSON
                forM_ ["requestId", "action"] $ \key -> rejects (Proxy @ResolveQueuedMessageParams) (Object (KeyMap.delete key deleteJSON)),
              testCase "append item schema is the shared message with required user-only visibility" $ do
                base <- either assertFailure pure (schemaAt ["definitions", "FactoryDroidMessageSchema"] shared)
                props <- either assertFailure pure (schemaAt ["properties"] base)
                required <- either assertFailure pure (schemaAt ["required"] base)
                item <- either assertFailure pure (schemaAt ["definitions", "AppendMessagesRequestParamsSchema", "properties", "messages", "items"] schema)
                case (base, props, required) of
                  (Object fields, Object properties, Array keys) -> do
                    let expected =
                          KeyMap.insert
                            "properties"
                            (Object (KeyMap.insert "visibility" (object ["type" .= String "string", "const" .= String "user_only"]) properties))
                            (KeyMap.insert "required" (toJSON (foldr (:) [] keys <> [String "visibility"])) fields)
                    item @?= externalizeSharedRefs (Object expected)
                  _ -> assertFailure "Unexpected shared message schema shape",
              testCase "append requires a nonempty array and explicit user-only visibility" $ do
                userOnlyMessageValue userOnly @?= message
                forM_ [Nothing, Just VisibilityBoth, Just VisibilityLlmOnly] $ \visibility ->
                  mkUserOnlyMessage (message {messageVisibility = visibility}) @?= Nothing
                rejects (Proxy @AppendMessagesParams) (object ["messages" .= ([] :: [Value])])
                rejects (Proxy @AppendMessagesParams) (object ["messages" .= [object []]])
                forM_ [object ["id" .= String "m", "role" .= String "user", "content" .= ([] :: [Value]), "createdAt" .= Number 0, "updatedAt" .= Number 0], object ["id" .= String "m", "role" .= String "user", "content" .= ([] :: [Value]), "createdAt" .= Number 0, "updatedAt" .= Number 0, "visibility" .= String "both"]] $ \invalid ->
                  rejects (Proxy @AppendMessagesParams) (object ["messages" .= [invalid]]),
              testCase "fork tag names follow their inline schema, not SessionTagSchema" $ do
                let value = ForkSessionTag "" Nothing mempty
                fromJSON (object ["name" .= String ""]) @?= Success value
                toJSON value @?= object ["name" .= String ""]
                rejects (Proxy @SessionTag) (object ["name" .= String ""])
                rejects (Proxy @ForkSessionTag) (object ["name" .= String "tag", "metadata" .= object ["x" .= Number 1]])
                let empty = ForkSessionParams Nothing (Just []) mempty
                toJSON empty @?= object ["tags" .= ([] :: [Value])],
              testCase "rewind arrays may be empty but nested records are validated" $ do
                let empty = GetRewindInfoResult [] [] [] mempty
                fromJSON (toJSON empty) @?= Success empty
                let request = rewindParams {executeRewindRestore = [], executeRewindDelete = []}
                fromJSON (toJSON request) @?= Success request
                forM_ ["availableFiles", "createdFiles", "evictedFiles"] $ \key -> rejects (Proxy @GetRewindInfoResult) (Object (KeyMap.insert key (toJSON [object []]) infoResultJSON))
                rejects (Proxy @ExecuteRewindParams) (Object (KeyMap.insert "filesToRestore" (toJSON [object []]) rewindParamsJSON)),
              testCase "output schema is an object without invented meta-schema validation" $ do
                let value = OutputFormat (KeyMap.singleton "arbitrary" Null) mempty
                fromJSON (toJSON value) @?= Success value
                forM_ [Null, Bool True, String "schema", Array mempty] $ \invalid -> rejects (Proxy @OutputFormat) (object ["type" .= String "json_schema", "schema" .= invalid]),
              testCase "bug-report provenance and non-object queue roots are checked" $ do
                rejects (Proxy @SubmitBugReportParams) (Object (KeyMap.insert "source" (object ["surface" .= String "future"]) bugParamsJSON))
                forM_ [Null, Bool True, Number 1, String "queue", Array mempty] $ rejects (Proxy @ResolveQueuedMessageParams)
            ]

externalizeSharedRefs :: Value -> Value
externalizeSharedRefs (Object fields) = Object (KeyMap.mapWithKey rewrite fields)
  where
    rewrite "$ref" (String ref) | "#/" `Text.isPrefixOf` ref = String ("shared.schema.json" <> ref)
    rewrite _ value = externalizeSharedRefs value
externalizeSharedRefs (Array items) = Array (fmap externalizeSharedRefs items)
externalizeSharedRefs value = value

enumTests :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => Value -> [Key] -> Proxy a -> TestTree
enumTests schema path _ = testCase (show path) $ do
  literals <- either assertFailure pure (schemaAt ("definitions" : path) schema)
  let values = [minBound .. maxBound] :: [a]
  toJSON values @?= literals
  forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
  rejects (Proxy @a) (String "future")

minimalUserParams :: AddUserMessageParams
minimalUserParams = AddUserMessageParams "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

userParams :: AddUserMessageParams
userParams =
  minimalUserParams
    { userMessageId = Just "id",
      userMessageContent = Just (UserMessageText (TextBlock "text" (BaseContentBlock Nothing mempty)) :| [UserMessageImage (ImageBlock imageSource (Just False) (BaseContentBlock Nothing mempty))]),
      userMessageImages = Just [imageSource],
      userMessageImagePaths = Just ["not-read"],
      userMessageFiles = Just [PlainTextDocument (PlainTextSource "document" Nothing Nothing mempty)],
      userMessageOutputFormat = Just outputFormat,
      userMessageSkipAgentLoop = Just False,
      userMessageQueuePlacement = Just QueueEndOfLoop,
      userMessageRole = Just RoleAssistant,
      userMessageVisibility = Just VisibilityBoth,
      userMessageSource = Just OriginAPI
    }
  where
    imageSource = Base64ImageSource "opaque" ImagePNG mempty

userParamsJSON, userImageSourceJSON, userFileJSON :: Object
userImageSourceJSON = KeyMap.fromList ["type" .= String "base64", "data" .= String "opaque", "mediaType" .= String "image/png"]
userFileJSON = KeyMap.fromList ["type" .= String "text", "data" .= String "document", "mediaType" .= String "text/plain"]
userParamsJSON = KeyMap.fromList ["text" .= String "", "messageId" .= String "id", "content" .= [object ["type" .= String "text", "text" .= String "text"], object ["type" .= String "image", "source" .= Object userImageSourceJSON, "generated" .= False]], "images" .= [Object userImageSourceJSON], "imagePaths" .= [String "not-read"], "files" .= [Object userFileJSON], "outputFormat" .= Object outputFormatJSON, "skipAgentLoop" .= False, "queuePlacement" .= String "end_of_loop", "role" .= String "assistant", "visibility" .= String "both", "userMessageSource" .= String "api"]

outputFormat :: OutputFormat
outputFormat = OutputFormat (KeyMap.singleton "type" (String "object")) mempty

snapshot :: RewindFileSnapshot
snapshot = RewindFileSnapshot "restored-path" "hash" (-1.25) mempty

creation :: RewindFileCreation
creation = RewindFileCreation "created-path" mempty

evicted :: RewindEvictedFile
evicted = RewindEvictedFile "evicted-path" "reason" mempty

infoParams :: GetRewindInfoParams
infoParams = GetRewindInfoParams "session-id" "message-id" mempty

infoResult :: GetRewindInfoResult
infoResult = GetRewindInfoResult [snapshot] [creation] [evicted] mempty

rewindParams :: ExecuteRewindParams
rewindParams = ExecuteRewindParams "session-id" "message-id" [snapshot] [creation] "rewind-title" mempty

rewindResult :: ExecuteRewindResult
rewindResult = ExecuteRewindResult "successor" 1.25 2.5 3.75 4.125 mempty

compactParams :: CompactSessionParams
compactParams = CompactSessionParams (Just "instructions") mempty

compactResult :: CompactSessionResult
compactResult = CompactSessionResult "compacted-session" (-0.5) mempty

forkTag :: ForkSessionTag
forkTag = ForkSessionTag "fork-tag" (Just (KeyMap.singleton "key" "value")) mempty

forkParams :: ForkSessionParams
forkParams = ForkSessionParams (Just "fork-title") (Just [forkTag]) mempty

forkResult :: ForkSessionResult
forkResult = ForkSessionResult "forked-session" mempty

directoryResult :: ValidateWorkingDirectoryResult
directoryResult = ValidateWorkingDirectoryResult False (Just "resolved path") (Just "error") mempty

bugParams :: SubmitBugReportParams
bugParams = SubmitBugReportParams "comment" (Just "fixture logs") (Just (BugReportSource BugCli Nothing Nothing Nothing Nothing Nothing Nothing mempty)) mempty

messageJSON :: Value
messageJSON = object ["id" .= String "message-id", "role" .= String "assistant", "content" .= ([] :: [Value]), "createdAt" .= Number 0, "updatedAt" .= Number 1, "visibility" .= String "user_only"]

outputFormatJSON, appendJSON, snapshotJSON, creationJSON, evictedJSON, infoParamsJSON, infoResultJSON, rewindParamsJSON, rewindResultJSON, compactParamsJSON, compactResultJSON, forkTagJSON, forkParamsJSON, forkResultJSON, renameJSON, changeParamsJSON, changeResultJSON, directoryJSON, closeJSON, killJSON, bugParamsJSON, bugResultJSON, updateJSON, deleteJSON :: Object
outputFormatJSON = KeyMap.fromList ["type" .= String "json_schema", "schema" .= object ["type" .= String "object"]]
appendJSON = KeyMap.singleton "messages" (toJSON [messageJSON])
snapshotJSON = KeyMap.fromList ["filePath" .= String "restored-path", "contentHash" .= String "hash", "size" .= Number (-1.25)]
creationJSON = KeyMap.singleton "filePath" (String "created-path")
evictedJSON = KeyMap.fromList ["filePath" .= String "evicted-path", "reason" .= String "reason"]
infoParamsJSON = KeyMap.fromList ["sessionId" .= String "session-id", "messageId" .= String "message-id"]
infoResultJSON = KeyMap.fromList ["availableFiles" .= [Object snapshotJSON], "createdFiles" .= [Object creationJSON], "evictedFiles" .= [Object evictedJSON]]
rewindParamsJSON = KeyMap.fromList ["sessionId" .= String "session-id", "messageId" .= String "message-id", "filesToRestore" .= [Object snapshotJSON], "filesToDelete" .= [Object creationJSON], "forkTitle" .= String "rewind-title"]
rewindResultJSON = KeyMap.fromList ["newSessionId" .= String "successor", "restoredCount" .= Number 1.25, "deletedCount" .= Number 2.5, "failedRestoreCount" .= Number 3.75, "failedDeleteCount" .= Number 4.125]
compactParamsJSON = KeyMap.singleton "customInstructions" (String "instructions")
compactResultJSON = KeyMap.fromList ["newSessionId" .= String "compacted-session", "removedCount" .= Number (-0.5)]
forkTagJSON = KeyMap.fromList ["name" .= String "fork-tag", "metadata" .= object ["key" .= String "value"]]
forkParamsJSON = KeyMap.fromList ["title" .= String "fork-title", "tags" .= [Object forkTagJSON]]
forkResultJSON = KeyMap.singleton "newSessionId" (String "forked-session")
renameJSON = KeyMap.singleton "title" (String "renamed")
changeParamsJSON = KeyMap.singleton "workingDirectory" (String "requested path")
changeResultJSON = KeyMap.singleton "resolvedPath" (String "resolved path")
directoryJSON = KeyMap.fromList ["isValid" .= False, "resolvedPath" .= String "resolved path", "error" .= String "error"]
closeJSON = KeyMap.singleton "reason" (String "other")
killJSON = KeyMap.singleton "workerSessionId" (String "worker-id")
bugParamsJSON = KeyMap.fromList ["userComment" .= String "comment", "clientLogs" .= String "fixture logs", "source" .= object ["surface" .= String "cli"]]
bugResultJSON = KeyMap.singleton "bugReportId" (String "report-id")
updateJSON = KeyMap.fromList ["requestId" .= String "request-id", "action" .= String "update_queue", "queuePlacement" .= String "end_of_loop"]
deleteJSON = KeyMap.fromList ["requestId" .= String "request-id", "action" .= String "delete"]
