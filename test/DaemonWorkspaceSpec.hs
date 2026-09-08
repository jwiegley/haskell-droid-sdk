{-# LANGUAGE OverloadedStrings #-}

module DaemonWorkspaceSpec (workspaceTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Factory.Droid.Schema.Control (ChangeWorkingDirectoryParams (..))
import Factory.Droid.Schema.Daemon.Workspace
import SchemaTest (enumSchemaTest, redactedRecordTests, rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

workspaceTests :: Value -> Value -> TestTree
workspaceTests local schema =
  testGroup
    "Daemon workspace bodies"
    [ records "DaemonCheckFolderTrustRequestParamsSchema" folder folder folderJSON folderJSON (\extras value -> value {folderPathAdditionalFields = extras}),
      records "DaemonTrustFolderRequestParamsSchema" folder folder folderJSON folderJSON (\extras value -> value {folderPathAdditionalFields = extras}),
      records "DaemonCheckFolderTrustResultSchema" trust trust trustJSON trustJSON (\extras value -> value {folderTrustAdditionalFields = extras}),
      records "DaemonTrustFolderResultSchema" trusted trusted trustedJSON trustedJSON (\extras value -> value {trustedRootAdditionalFields = extras}),
      redactedRecordTests "read params" (requestParams "DaemonGetWorkspaceFileContentRequestSchema") readParams (GetWorkspaceFileContentParams "session" "file" Nothing Nothing mempty) readParamsJSON (KeyMap.fromList ["sessionId" .= String "session", "filePath" .= String "file"]) (\extras value -> value {readFileAdditionalFields = extras}),
      records "DaemonGetWorkspaceFileContentResultSchema" content (GetWorkspaceFileContentResult "fixture content" (-1.25) Nothing Nothing Nothing Nothing mempty) contentJSON (KeyMap.fromList ["content" .= String "fixture content", "byteLength" .= Number (-1.25)]) (\extras value -> value {workspaceFileAdditionalFields = extras}),
      redactedRecordTests "write params" (requestParams "DaemonWriteWorkspaceFileContentRequestSchema") writeParams (writeParams {writeFileBaseFingerprint = Nothing}) writeParamsJSON (KeyMap.delete "baseFingerprint" writeParamsJSON) (\extras value -> value {writeFileAdditionalFields = extras}),
      records "DaemonWriteWorkspaceFileContentResultSchema" written written writtenJSON writtenJSON (\extras value -> value {writtenFileAdditionalFields = extras}),
      redactedRecordTests "list params" (requestParams "DaemonListFilesRequestSchema") listParams (ListFilesParams "session" Nothing Nothing mempty) listParamsJSON (KeyMap.singleton "sessionId" (String "session")) (\extras value -> value {listFilesParamsAdditionalFields = extras}),
      records "DaemonListFilesResultSchema" listing (ListFilesResult ["second", "first", "second"] Nothing Nothing Nothing Nothing mempty) listingJSON (KeyMap.singleton "files" (toJSON [String "second", String "first", String "second"])) (\extras value -> value {listedFilesAdditionalFields = extras}),
      redactedRecordTests "search params" (requestParams "DaemonSearchFilesRequestSchema") searchParams (SearchFilesParams "session" "query" Nothing Nothing mempty) searchParamsJSON (KeyMap.fromList ["sessionId" .= String "session", "query" .= String "query"]) (\extras value -> value {searchFilesParamsAdditionalFields = extras}),
      records "DaemonSearchFilesResultSchema" searched searched searchedJSON searchedJSON (\extras value -> value {searchedFilesAdditionalFields = extras}),
      records "DaemonPushCwdFileToUrlRequestParamsSchema" pushParams (pushParams {pushFileContentType = Nothing}) pushParamsJSON (KeyMap.delete "contentType" pushParamsJSON) (\extras value -> value {pushFileAdditionalFields = extras}),
      records "DaemonPushCwdFileToUrlResultSchema" pushed pushed pushedJSON pushedJSON (\extras value -> value {pushedFileAdditionalFields = extras}),
      records "DaemonPullUrlToCwdFileRequestParamsSchema" pullParams (pullParams {pullFileExpectedLength = Nothing}) pullParamsJSON (KeyMap.delete "expectedContentLength" pullParamsJSON) (\extras value -> value {pullFileAdditionalFields = extras}),
      records "DaemonPullUrlToCwdFileResultSchema" pulled pulled pulledJSON pulledJSON (\extras value -> value {pulledFileAdditionalFields = extras}),
      records "DaemonChangeWorkingDirectoryRequestParamsSchema" directory directory directoryJSON directoryJSON (\extras value -> value {changeDirectoryAdditionalFields = extras}),
      records "DaemonSetupStepProgressNotificationParamsSchema" progress progress progressJSON progressJSON (\extras value -> value {setupProgressAdditionalFields = extras}),
      enumSchemaTest "content encodings" (definition "DaemonGetWorkspaceFileContentResultSchema" >>= schemaAt ["properties", "encoding", "enum"]) (Proxy @WorkspaceEncoding),
      enumSchemaTest "setup kinds" (definition "DaemonSetupStepProgressNotificationParamsSchema" >>= schemaAt ["properties", "kind", "enum"]) (Proxy @SetupStepKind),
      testCase "check/trust path bodies share identical schemas" $
        definition "DaemonCheckFolderTrustRequestParamsSchema" @?= definition "DaemonTrustFolderRequestParamsSchema",
      testCase "directory validation reuses existing local codecs, not a parallel shape" $ do
        requestParams "DaemonValidateWorkingDirectoryRequestSchema" @?= schemaAt ["definitions", "ChangeWorkingDirectoryRequestParamsSchema"] local
        let response = definition "DaemonValidateWorkingDirectoryResponseSchema" >>= schemaAt ["anyOf"] >>= schemaIndex 0 >>= schemaAt ["allOf"] >>= schemaIndex 1 >>= schemaAt ["properties", "result"]
        response @?= Right (object ["$ref" .= String "droid.schema.json#/definitions/ValidateWorkingDirectoryResultSchema"])
        toJSON (ChangeWorkingDirectoryParams "folder" mempty) @?= object ["workingDirectory" .= String "folder"],
      testCase "request and result encoding enums agree" $
        (requestParams "DaemonGetWorkspaceFileContentRequestSchema" >>= schemaAt ["properties", "encoding"]) @?= (definition "DaemonGetWorkspaceFileContentResultSchema" >>= schemaAt ["properties", "encoding"]),
      testCase "nonnegative integer domains reject signs and fractions" $ do
        forM_ [Number (-1), Number 0.5] $ \bad -> do
          rejects (Proxy @WriteWorkspaceFileContentResult) (Object (KeyMap.insert "byteLength" bad writtenJSON))
          rejects (Proxy @PushCwdFileToUrlResult) (Object (KeyMap.insert "byteLength" bad pushedJSON))
          rejects (Proxy @PullUrlToCwdFileResult) (Object (KeyMap.insert "byteLength" bad pulledJSON))
          rejects (Proxy @PullUrlToCwdFileParams) (Object (KeyMap.insert "expectedContentLength" bad pullParamsJSON))
          forM_ ["totalFiles", "completeDepth"] $ \key -> rejects (Proxy @ListFilesResult) (Object (KeyMap.insert key bad listingJSON)),
      testCase "integer counts retain zero and magnitudes beyond machine integers" $ do
        forM_ [0, 18446744073709551616] $ \count -> do
          fromJSON (object ["byteLength" .= count]) @?= Success (PushCwdFileToUrlResult count mempty)
          fromJSON (Object (KeyMap.insert "byteLength" (toJSON count) writtenJSON)) @?= Success (written {writtenFileByteLength = count})
          fromJSON (Object (KeyMap.insert "byteLength" (toJSON count) pulledJSON)) @?= Success (pulled {pulledFileByteLength = count})
          fromJSON (Object (KeyMap.insert "expectedContentLength" (toJSON count) pullParamsJSON)) @?= Success (pullParams {pullFileExpectedLength = Just count})
          fromJSON (Object (KeyMap.insert "totalFiles" (toJSON count) listingJSON)) @?= Success (listing {listedFilesTotal = Just count}),
      testCase "read/search numbers are not coerced to the integer domains" $ do
        forM_ [-1.25, 0, 123456789012345678901234567890] $ \count -> do
          fromJSON (Object (KeyMap.insert "byteLength" (Number count) contentJSON)) @?= Success (content {workspaceFileByteLength = count})
          fromJSON (Object (KeyMap.insert "totalFiles" (Number count) searchedJSON)) @?= Success (searched {searchedFilesTotal = count})
          fromJSON (Object (KeyMap.insert "maxResults" (Number count) searchParamsJSON)) @?= Success (searchParams {searchFilesMaxResults = Just count}),
      testCase "trust flags remain independent reports" $ do
        forM_ [False, True] $ \trustedFlag -> forM_ [False, True] $ \promptFlag -> do
          let value = CheckFolderTrustResult trustedFlag "root" promptFlag mempty
          fromJSON (object ["isTrusted" .= trustedFlag, "trustRootPath" .= String "root", "promptRequired" .= promptFlag]) @?= Success value,
      testCase "list/search defaults are annotations without coercing missing options" $ do
        (requestParams "DaemonListFilesRequestSchema" >>= schemaAt ["properties", "showHidden", "default"]) @?= Right (Bool False)
        (requestParams "DaemonSearchFilesRequestSchema" >>= schemaAt ["properties", "showHidden", "default"]) @?= Right (Bool False)
        (requestParams "DaemonSearchFilesRequestSchema" >>= schemaAt ["properties", "maxResults", "default"]) @?= Right (Number 60)
        fromJSON (object ["sessionId" .= String "s"]) @?= Success (ListFilesParams "s" Nothing Nothing mempty)
        fromJSON (object ["sessionId" .= String "s", "query" .= String ""]) @?= Success (SearchFilesParams "s" "" Nothing Nothing mempty),
      testCase "file/directory arrays remain typed and may be empty" $ do
        forM_ ["files", "directories"] $ \key -> rejects (Proxy @ListFilesResult) (Object (KeyMap.insert key (toJSON [Null]) listingJSON))
        rejects (Proxy @SearchFilesResult) (object ["files" .= [Bool False], "totalFiles" .= Number 0])
        fromJSON (object ["files" .= ([] :: [Value])]) @?= Success (ListFilesResult [] Nothing Nothing Nothing Nothing mempty)
        fromJSON (object ["files" .= ([] :: [Value]), "totalFiles" .= Number 0]) @?= Success (SearchFilesResult [] 0 mempty),
      testCase "opaque URLs, content and fingerprints are preserved without interpretation" $ do
        let value = GetWorkspaceFileContentResult "not-base64" 99 (Just WorkspaceBase64) (Just "arbitrary MIME") (Just False) (Just "") mempty
        fromJSON (toJSON value) @?= Success value
        fromJSON (object ["sessionId" .= String "", "presignedGetUrl" .= String "not-a-URI", "destPath" .= String "../unresolved"]) @?= Success (PullUrlToCwdFileParams "" "not-a-URI" "../unresolved" Nothing mempty)
        rejects (Proxy @GetWorkspaceFileContentParams) (Object (KeyMap.insert "encoding" (String "future") readParamsJSON))
        rejects (Proxy @GetWorkspaceFileContentResult) (Object (KeyMap.insert "encoding" (String "future") contentJSON)),
      testCase "session identity is required on every scoped file operation" $ do
        rejects (Proxy @GetWorkspaceFileContentParams) (Object (KeyMap.delete "sessionId" readParamsJSON))
        rejects (Proxy @WriteWorkspaceFileContentParams) (Object (KeyMap.delete "sessionId" writeParamsJSON))
        rejects (Proxy @ListFilesParams) (Object (KeyMap.delete "sessionId" listParamsJSON))
        rejects (Proxy @SearchFilesParams) (Object (KeyMap.delete "sessionId" searchParamsJSON))
        rejects (Proxy @PushCwdFileToUrlParams) (Object (KeyMap.delete "sessionId" pushParamsJSON))
        rejects (Proxy @PullUrlToCwdFileParams) (Object (KeyMap.delete "sessionId" pullParamsJSON))
        rejects (Proxy @ChangeSessionWorkingDirectoryParams) (Object (KeyMap.delete "sessionId" directoryJSON))
        rejects (Proxy @SetupStepProgress) (Object (KeyMap.delete "sessionId" progressJSON))
    ]
  where
    definition name = schemaAt ["definitions", name] schema
    requestParams name = definition name >>= schemaAt ["allOf"] >>= schemaIndex 1 >>= schemaAt ["properties", "params"]
    records :: (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = redactedRecordTests name (definition name)

folder :: FolderPathParams
folder = FolderPathParams "folder" mempty

trust :: CheckFolderTrustResult
trust = CheckFolderTrustResult False "root" False mempty

trusted :: TrustFolderResult
trusted = TrustFolderResult "root" mempty

readParams :: GetWorkspaceFileContentParams
readParams = GetWorkspaceFileContentParams "session" "file" (Just False) (Just WorkspaceBase64) mempty

content :: GetWorkspaceFileContentResult
content = GetWorkspaceFileContentResult "fixture content" (-1.25) (Just WorkspaceBase64) (Just "fixture MIME") (Just False) (Just "fingerprint") mempty

writeParams :: WriteWorkspaceFileContentParams
writeParams = WriteWorkspaceFileContentParams "session" "file" "fixture content" (Just "base") mempty

written :: WriteWorkspaceFileContentResult
written = WriteWorkspaceFileContentResult 0 "new fingerprint" mempty

listParams :: ListFilesParams
listParams = ListFilesParams "session" (Just "subdir") (Just False) mempty

listing :: ListFilesResult
listing = ListFilesResult ["second", "first", "second"] (Just ["directory"]) (Just 9) (Just 2) (Just False) mempty

searchParams :: SearchFilesParams
searchParams = SearchFilesParams "session" "query" (Just (-1.25)) (Just False) mempty

searched :: SearchFilesResult
searched = SearchFilesResult ["file"] (-2.5) mempty

pushParams :: PushCwdFileToUrlParams
pushParams = PushCwdFileToUrlParams "session" "file" "fixture put URL" (Just "fixture MIME") mempty

pushed :: PushCwdFileToUrlResult
pushed = PushCwdFileToUrlResult 0 mempty

pullParams :: PullUrlToCwdFileParams
pullParams = PullUrlToCwdFileParams "session" "fixture get URL" "destination" (Just 0) mempty

pulled :: PullUrlToCwdFileResult
pulled = PullUrlToCwdFileResult 0 "written path" mempty

directory :: ChangeSessionWorkingDirectoryParams
directory = ChangeSessionWorkingDirectoryParams "session" "directory" mempty

progress :: SetupStepProgress
progress = SetupStepProgress "session" SetupOriginPull "fixture progress" mempty

folderJSON, trustJSON, trustedJSON, readParamsJSON, contentJSON, writeParamsJSON, writtenJSON, listParamsJSON, listingJSON, searchParamsJSON, searchedJSON, pushParamsJSON, pushedJSON, pullParamsJSON, pulledJSON, directoryJSON, progressJSON :: Object
folderJSON = KeyMap.singleton "path" (String "folder")
trustJSON = KeyMap.fromList ["isTrusted" .= False, "trustRootPath" .= String "root", "promptRequired" .= False]
trustedJSON = KeyMap.singleton "trustRootPath" (String "root")
readParamsJSON = KeyMap.fromList ["sessionId" .= String "session", "filePath" .= String "file", "metadataOnly" .= False, "encoding" .= String "base64"]
contentJSON = KeyMap.fromList ["content" .= String "fixture content", "byteLength" .= Number (-1.25), "encoding" .= String "base64", "mimeType" .= String "fixture MIME", "isBinary" .= False, "fingerprint" .= String "fingerprint"]
writeParamsJSON = KeyMap.fromList ["sessionId" .= String "session", "filePath" .= String "file", "content" .= String "fixture content", "baseFingerprint" .= String "base"]
writtenJSON = KeyMap.fromList ["byteLength" .= Number 0, "fingerprint" .= String "new fingerprint"]
listParamsJSON = KeyMap.fromList ["sessionId" .= String "session", "path" .= String "subdir", "showHidden" .= False]
listingJSON = KeyMap.fromList ["files" .= [String "second", String "first", String "second"], "directories" .= [String "directory"], "totalFiles" .= Number 9, "completeDepth" .= Number 2, "truncated" .= False]
searchParamsJSON = KeyMap.fromList ["sessionId" .= String "session", "query" .= String "query", "maxResults" .= Number (-1.25), "showHidden" .= False]
searchedJSON = KeyMap.fromList ["files" .= [String "file"], "totalFiles" .= Number (-2.5)]
pushParamsJSON = KeyMap.fromList ["sessionId" .= String "session", "filePath" .= String "file", "presignedPutUrl" .= String "fixture put URL", "contentType" .= String "fixture MIME"]
pushedJSON = KeyMap.singleton "byteLength" (Number 0)
pullParamsJSON = KeyMap.fromList ["sessionId" .= String "session", "presignedGetUrl" .= String "fixture get URL", "destPath" .= String "destination", "expectedContentLength" .= Number 0]
pulledJSON = KeyMap.fromList ["byteLength" .= Number 0, "writtenPath" .= String "written path"]
directoryJSON = KeyMap.fromList ["sessionId" .= String "session", "workingDirectory" .= String "directory"]
progressJSON = KeyMap.fromList ["sessionId" .= String "session", "kind" .= String "origin-pull", "text" .= String "fixture progress"]
