{-# LANGUAGE OverloadedStrings #-}

module DaemonWorkspaceClientSpec (workspaceClientTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, readTQueue, tryReadTQueue)
import Control.Exception (bracket, catch, fromException, try)
import Control.Monad (forever, void, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Control (ChangeWorkingDirectoryResult (..), ValidateWorkingDirectoryResult (..))
import Factory.Droid.Schema.Daemon.Workspace
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcConflict, RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import ProtocolSpec (feed, reply, withMemory)
import System.Directory (doesFileExist, getCurrentDirectory, removePathForcibly)
import System.FilePath (takeFileName, (</>))
import System.Posix.Temp (mkdtemp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

workspaceClientTests :: TestTree
workspaceClientTests = testGroup "Daemon workspace operations" workspaceCases

workspaceCases :: [TestTree]
workspaceCases =
  [ testCase "workspace operations preserve remote paths, trust flags, metadata and defaults" $ bounded $ withFixture $ \root -> do
      trace <- newIORef []
      cwd <- getCurrentDirectory
      withWorkspacePeer root trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        checked <- Daemon.checkFolderTrust connection "../REMOTE_ONLY"
        folderIsTrusted checked @?= False
        folderPromptRequired checked @?= False
        folderTrustRootPath checked @?= "/daemon/root"
        validation <- Daemon.validateWorkingDirectory connection "../REMOTE_ONLY"
        directoryIsValid validation @?= False
        Daemon.trustFolder connection "../REMOTE_ONLY" >>= (@?= "/daemon/root") . trustedRootPath
        Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "other-session" "../REMOTE_ONLY" mempty) >>= (@?= "/daemon/resolved") . changedResolvedPath
        listed <- Daemon.listFiles connection (ListFilesParams "other-session" (Just "subdirectory") Nothing mempty)
        listedFilePaths listed @?= ["second", "first", "second"]
        listedDirectoryPaths listed @?= Just ["dir"]
        listedFilesTruncated listed @?= Just False
        searched <- Daemon.searchFiles connection (SearchFilesParams "other-session" "" Nothing Nothing mempty)
        searchedFilesTotal searched @?= 3.5
        void (Daemon.searchFiles connection (SearchFilesParams "other-session" "explicit" (Just 0) (Just True) mempty))
        content <- Daemon.getWorkspaceFileContent connection (GetWorkspaceFileContentParams "other-session" "remote-only.bin" (Just False) (Just WorkspaceBase64) mempty)
        workspaceFileContent content @?= "QQBC"
        workspaceFileByteLength content @?= 3
        workspaceFileEncoding content @?= Just WorkspaceBase64
        workspaceFileFingerprint content @?= Just "fingerprint"
        metadata <- Daemon.getWorkspaceFileContent connection (GetWorkspaceFileContentParams "other-session" "remote-only.bin" (Just True) Nothing mempty)
        workspaceFileContent metadata @?= ""
      getCurrentDirectory >>= (@?= cwd)
      frames <- readIORef trace
      map (field "method") frames @?= map String ["daemon.authenticate", "daemon.check_folder_trust", "daemon.validate_working_directory", "daemon.trust_folder", "daemon.change_working_directory", "daemon.list_files", "daemon.search_files", "daemon.search_files", "daemon.get_workspace_file_content", "daemon.get_workspace_file_content"]
      map parameters (drop 1 frames) @?= [KeyMap.singleton "path" (String "../REMOTE_ONLY"), KeyMap.singleton "workingDirectory" (String "../REMOTE_ONLY"), KeyMap.singleton "path" (String "../REMOTE_ONLY"), KeyMap.fromList ["sessionId" .= String "other-session", "workingDirectory" .= String "../REMOTE_ONLY"], KeyMap.fromList ["sessionId" .= String "other-session", "path" .= String "subdirectory", "showHidden" .= False], KeyMap.fromList ["sessionId" .= String "other-session", "query" .= String "", "maxResults" .= (50 :: Int), "showHidden" .= False], KeyMap.fromList ["sessionId" .= String "other-session", "query" .= String "explicit", "maxResults" .= (0 :: Int), "showHidden" .= True], KeyMap.fromList ["sessionId" .= String "other-session", "filePath" .= String "remote-only.bin", "metadataOnly" .= False, "encoding" .= String "base64"], KeyMap.fromList ["sessionId" .= String "other-session", "filePath" .= String "remote-only.bin", "metadataOnly" .= True]],
    testCase "low-level search uses 60 and low-level transfer deadlines remain caller-owned" $ bounded $ withMemory $ \channel incoming sent -> do
      let configured = Client.CallOptions "request" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
      withAsync (Client.searchDaemonFiles channel configured (SearchFilesParams "saved" "query" Nothing Nothing mempty)) $ \pending -> do
        request <- atomically (readTQueue sent)
        parameters request @?= KeyMap.fromList ["sessionId" .= String "saved", "query" .= String "query", "maxResults" .= (60 :: Int), "showHidden" .= False]
        feed incoming (reply "request" (object ["files" .= ([] :: [Value]), "totalFiles" .= (0 :: Int)]))
        wait pending >>= (@?= []) . searchedFilePaths
      result <- try @RpcChannelError (Client.pushDaemonCwdFileToUrl channel (configured {Client.callRequestId = "zero", Client.callTimeoutMicros = Just 0}) (PushCwdFileToUrlParams "saved" "remote" "https://OFFLINE.invalid" Nothing mempty))
      result @?= Left RpcRequestTimedOut
      atomically (tryReadTQueue sent) >>= (@?= Nothing),
    testCase "daemon-side push and pull transfer actual fixture bytes without touching client files" $ bounded $ withFixture $ \root -> do
      trace <- newIORef []
      uploaded <- newIORef BS.empty
      let bytes = "A\0B\xff"
          destination = takeFileName root <> "-download.bin"
          app request finish = case Wai.requestMethod request of
            "PUT" -> do
              lookup hContentType (Wai.requestHeaders request) @?= Just "application/octet-stream"
              body <- BL.toStrict <$> Wai.strictRequestBody request
              modifyIORef' uploaded (const body)
              finish (Wai.responseLBS status200 [] "")
            "GET" -> finish (Wai.responseLBS status200 [] (BL.fromStrict bytes))
            _ -> assertFailure "Unexpected transfer HTTP method"
      BS.writeFile (root </> "upload.bin") bytes
      doesFileExist destination >>= (@?= False)
      Warp.testWithApplication (pure app) $ \port -> do
        let url = "http://127.0.0.1:" <> Text.pack (show port) <> "/object?signature=OFFLINE_ONLY"
        withWorkspacePeer root trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
          let push = PushCwdFileToUrlParams "remote-session" "upload.bin" url (Just "application/octet-stream") mempty
              pull = PullUrlToCwdFileParams "remote-session" url (Text.pack destination) (Just 4) mempty
          Daemon.pushCwdFileToUrl connection push >>= (@?= 4) . pushedFileByteLength
          readIORef uploaded >>= (@?= bytes)
          result <- Daemon.pullUrlToCwdFile connection pull
          pulledFileByteLength result @?= 4
          pulledFileWrittenPath result @?= Text.pack (root </> destination)
          BS.readFile (root </> destination) >>= (@?= bytes)
          doesFileExist destination >>= (@?= False)
          show push @?= "PushCwdFileToUrlParams <redacted>"
          show pull @?= "PullUrlToCwdFileParams <redacted>"
      frames <- readIORef trace
      map (field "method") frames @?= map String ["daemon.authenticate", "daemon.push_cwd_file_to_url", "daemon.pull_url_to_cwd_file"],
    testCase "setup progress uses existing dispatcher and reports malformed payloads" $ bounded $ withFixture $ \root -> do
      trace <- newIORef []
      withWorkspacePeer root trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        seen <- newEmptyMVar
        stop <- Daemon.onSetupStepProgress connection (putMVar seen)
        void (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "progress" "new" mempty))
        event <- takeMVar seen
        case event of
          Right value -> do
            setupProgressSessionId value @?= "different-session"
            setupProgressKind value @?= SetupOriginPull
            setupProgressText value @?= "remote progress"
          Left _ -> assertFailure "Valid setup event rejected"
        void (Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams "invalid-progress" "new" mempty))
        takeMVar seen >>= (@?= Left Daemon.InvalidDaemonEvent)
        stop
        stop,
    testCase "remote errors and malformed metadata do not become fabricated file contents" $ bounded $ withFixture $ \root -> do
      trace <- newIORef []
      withWorkspacePeer root trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        conflict <- try @RpcResultError (Daemon.getWorkspaceFileContent connection (GetWorkspaceFileContentParams "saved" "conflict" Nothing Nothing mempty))
        case conflict of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Expected remote conflict"
        bad <- try @RpcResultError (Daemon.getWorkspaceFileContent connection (GetWorkspaceFileContentParams "saved" "bad-result" Nothing Nothing mempty))
        bad @?= Left RpcInvalidResult
        Daemon.listFiles connection (ListFilesParams "saved" Nothing Nothing mempty) >>= (@?= ["second", "first", "second"]) . listedFilePaths,
    testCase "cancelling a transfer wait preserves connection reuse without an implicit cancel RPC" $ bounded $ withFixture $ \root -> do
      trace <- newIORef []
      withWorkspacePeer root trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        ready <- newEmptyMVar
        _ <- Daemon.onSetupStepProgress connection (const (putMVar ready ()))
        withAsync (Daemon.pushCwdFileToUrl connection (PushCwdFileToUrlParams "held" "file" "https://OFFLINE.invalid" Nothing mempty)) $ \pending -> do
          takeMVar ready
          cancel pending
          waitCatch pending >>= \case Left err -> fromException err @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled transfer wait returned"
        Daemon.checkFolderTrust connection "remote" >>= (@?= False) . folderIsTrusted
      frames <- readIORef trace
      map (field "method") frames @?= map String ["daemon.authenticate", "daemon.push_cwd_file_to_url", "daemon.check_folder_trust"]
  ]

withFixture :: (FilePath -> IO a) -> IO a
withFixture = bracket (mkdtemp "/tmp/droid-workspace-peer-") removePathForcibly

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withWorkspacePeer :: FilePath -> IORef [Object] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withWorkspacePeer root trace = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid client JSON")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "apiKey" (parameters auth) @?= String "OFFLINE_ONLY"
      respond connection auth (object ["userId" .= String "fixture-user", "orgId" .= String "fixture-org"])
      manager <- HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy HTTP.defaultManagerSettings)
      forever $ do
        frame <- readFrame connection
        let params = parameters frame
        case field "method" frame of
          String "daemon.check_folder_trust" -> respond connection frame (object ["isTrusted" .= False, "trustRootPath" .= String "/daemon/root", "promptRequired" .= False])
          String "daemon.trust_folder" -> respond connection frame (object ["trustRootPath" .= String "/daemon/root"])
          String "daemon.validate_working_directory" -> respond connection frame (object ["isValid" .= False, "error" .= String "remote-only"])
          String "daemon.change_working_directory" -> do
            when (field "sessionId" params == String "progress") (notify connection progressValue)
            when (field "sessionId" params == String "invalid-progress") (notify connection (object ["sessionId" .= String "saved", "kind" .= String "invalid", "text" .= String "data"]))
            respond connection frame (object ["resolvedPath" .= String "/daemon/resolved"])
          String "daemon.list_files" -> respond connection frame (object ["files" .= [String "second", String "first", String "second"], "directories" .= [String "dir"], "totalFiles" .= (3 :: Int), "completeDepth" .= (1 :: Int), "truncated" .= False])
          String "daemon.search_files" -> respond connection frame (object ["files" .= ([] :: [Value]), "totalFiles" .= (3.5 :: Scientific)])
          String "daemon.get_workspace_file_content" -> case field "filePath" params of
            String "conflict" -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcConflict, "message" .= String "fixture conflict"]]
            String "bad-result" -> respond connection frame (object ["content" .= String "data", "byteLength" .= False])
            _ -> respond connection frame (object ["content" .= (if field "metadataOnly" params == Bool True then String "" else String "QQBC"), "byteLength" .= (3 :: Int), "encoding" .= String "base64", "fingerprint" .= String "fingerprint", "isBinary" .= False])
          String "daemon.push_cwd_file_to_url" | field "sessionId" params == String "held" -> notify connection progressValue
          String "daemon.push_cwd_file_to_url" -> do
            file <- textField "filePath" params
            file @?= "upload.bin"
            bytes <- BS.readFile (root </> Text.unpack file)
            url <- textField "presignedPutUrl" params
            request <- HTTP.parseRequest (Text.unpack url)
            contentType <- textField "contentType" params
            result <- HTTP.httpLbs (request {HTTP.method = "PUT", HTTP.requestBody = HTTP.RequestBodyBS bytes, HTTP.requestHeaders = [(hContentType, Text.encodeUtf8 contentType)]}) manager
            HTTP.responseStatus result @?= status200
            respond connection frame (object ["byteLength" .= BS.length bytes])
          String "daemon.pull_url_to_cwd_file" -> do
            url <- textField "presignedGetUrl" params
            destination <- textField "destPath" params
            let output = root </> Text.unpack destination
            Text.pack (takeFileName output) @?= destination
            request <- HTTP.parseRequest (Text.unpack url)
            result <- HTTP.httpLbs request manager
            HTTP.responseStatus result @?= status200
            field "expectedContentLength" params @?= toJSON (BL.length (HTTP.responseBody result))
            BL.writeFile output (HTTP.responseBody result)
            respond connection frame (object ["byteLength" .= BL.length (HTTP.responseBody result), "writtenPath" .= output])
          _ -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Unexpected workspace method"]]

respond :: WS.Connection -> Object -> Value -> IO ()
respond connection request result = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= result]

notify :: WS.Connection -> Value -> IO ()
notify connection value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session.setup_step_progress", "params" .= value]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

parameters :: Object -> Object
parameters value = case field "params" value of Object params -> params; _ -> mempty

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

textField :: Key -> Object -> IO Text
textField key value = case field key value of String text -> pure text; _ -> assertFailure "Missing text field"

progressValue :: Value
progressValue = object ["sessionId" .= String "different-session", "kind" .= String "origin-pull", "text" .= String "remote progress"]
