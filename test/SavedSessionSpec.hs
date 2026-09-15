{-# LANGUAGE OverloadedStrings #-}

module SavedSessionSpec (savedSessionTests, ScanCancelled (..), descriptorsFor, sparseSession, waitForRead, withTemp, writeHeader) where

import Control.Concurrent (threadDelay, throwTo)
import Control.Concurrent.Async (asyncThreadId, waitCatch, withAsync)
import Control.Exception (Exception, IOException, bracket, fromException, try)
import Control.Monad (forM, forM_)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Factory.Droid.Discovery
import Factory.Droid.Schema.Mission (DecompSessionType (..))
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import ProcessSpec (bounded)
import System.Directory (createDirectory, getHomeDirectory, listDirectory, removePathForcibly, renameFile)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode, WriteMode), SeekMode (AbsoluteSeek), hSetFileSize, withBinaryFile)
import System.IO.Error (ioeGetErrorString, ioeGetFileName, isUserError)
import System.Info (os)
import System.Posix.Files (createNamedPipe, createSymbolicLink, deviceID, fileID, getFdStatus, getFileStatus, setFileMode, setFileTimesHiRes, statusChangeTimeHiRes)
import System.Posix.IO (FdOption (CloseOnExec, NonBlockingRead), OpenFileFlags (..), OpenMode (ReadOnly), closeFd, defaultFileFlags, fdSeek, openFd, queryFdOption)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..))
import System.Posix.User (getEffectiveUserID)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Text.Read (readMaybe)

savedSessionTests :: TestTree
savedSessionTests =
  testGroup
    "Saved session file scanning"
    [ testCase "default root is the sessions directory under the current home" $ do
        home <- getHomeDirectory
        defaultDroidSessionsDirectory >>= (@?= home </> ".factory" </> "sessions"),
      testCase "direct scans select jsonl files in filename order, not nested workspaces" $ withTemp $ \root -> do
        writeHeader (root </> "b.jsonl") (header "b") ""
        writeHeader (root </> "a.jsonl") (header "a") ""
        writeHeader (root </> "ignored.JSONL") (header "ignored") ""
        createDirectory (root </> "directory.jsonl")
        createDirectory (root </> "-workspace")
        writeHeader (root </> "-workspace" </> "nested.jsonl") (header "nested") ""
        scanDroidSessionDirectory root >>= (@?= ["a", "b"]) . map sessionFileId
        scanDroidSessionDirectory (root </> "-workspace") >>= (@?= ["nested"]) . map sessionFileId,
      testCase "missing and non-directory roots produce no observations" $ withTemp $ \root -> do
        scanDroidSessionDirectory (root </> "absent") >>= (@?= [])
        BS.writeFile (root </> "file") "plain"
        scanDroidSessionDirectory (root </> "file") >>= (@?= []),
      testCase "file summaries preserve exact metadata and observed modification/change times" $ withTemp $ \root -> do
        let path = root </> "actual.jsonl"
            fields = KeyMap.fromList [("type", String "session_start"), ("title", String "private title"), ("owner", String ""), ("cwd", String "../raw/"), ("sessionId", String "not-the-filename"), ("future", Number 9007199254740993)]
        writeHeader path (Object fields) "not-json\n{}\n"
        setFileTimesHiRes path 0 123.25
        status <- getFileStatus path
        result <- requireFile path
        (sessionFileId result, sessionFileTitle result, sessionFileOwner result, sessionFileCwd result, sessionFileMessageCount result) @?= ("actual", "private title", "", Just "../raw/", 2)
        sessionFileHeader result @?= fields
        sessionFileModifiedAt result @?= posixSecondsToUTCTime 123.25
        sessionFileStatusChangedAt result @?= posixSecondsToUTCTime (statusChangeTimeHiRes status)
        show result @?= "DroidSessionFile <redacted>",
      testCase "empty filename identity and empty text fields remain literal" $ withTemp $ \root -> do
        let path = root </> ".jsonl"
        writeHeader path (object ["type" .= String "session_start", "title" .= String "", "owner" .= String "", "cwd" .= String ""]) ""
        result <- requireFile path
        (sessionFileId result, sessionFileTitle result, sessionFileOwner result, sessionFileCwd result) @?= ("", "", "", Just ""),
      testCase "malformed or absent first-line required fields are rejected" $ withTemp $ \root -> do
        let path = root </> "bad.jsonl"
        forM_ ["", "\n{}", " \t\r\n{}", "{bad", "null", "[]", "{}", "{\"type\":\"message\",\"title\":\"x\"}", "{\"type\":\"session_start\"}", "{\"type\":\"session_start\",\"title\":null}", "{\"type\":\"session_start\",\"title\":\"" <> BS.singleton 255 <> "\"}"] $ \bytes -> do
          BS.writeFile path bytes
          readDroidSessionFile path >>= (@?= Nothing),
      testCase "nonblank legacy titles override modern titles without trimming" $ withTemp $ \root -> do
        let path = root </> "legacy.jsonl"
        forM_ [(String "  legacy  ", Just "modern", Just "  legacy  "), (String "\xfeff \t", Just "modern", Just "modern"), (Bool False, Just "modern", Just "modern"), (String "old", Nothing, Just "old"), (String " ", Nothing, Nothing)] $ \(legacy, modern, expected) -> do
          writeHeader path (object (["type" .= String "session_start", "sessionTitle" .= legacy] <> maybe [] (\title -> ["title" .= (title :: Text)]) modern)) ""
          readDroidSessionFile path >>= (@?= expected) . fmap sessionFileTitle,
      testCase "Python-compatible optional-field tolerance preserves the raw header" $ withTemp $ \root -> do
        let path = root </> "optional.jsonl"
            fields = KeyMap.fromList [("type", String "session_start"), ("title", String "ok"), ("owner", Number 7), ("cwd", Null), ("sessionId", Bool False), ("decompMissionId", Array mempty)]
        writeHeader path (Object fields) ""
        result <- requireFile path
        (sessionFileOwner result, sessionFileCwd result, sessionFileMissionId result) @?= ("", Nothing, Nothing)
        sessionFileHeader result @?= fields,
      testCase "line counts use CR space tab rules and count malformed trailing records" $ withTemp $ \root -> do
        let path = root </> "lines.jsonl"
        writeHeader path (header "lines") " \t\r\n\r\n\v\n\f\nnot-json\r\nlast"
        requireFile path >>= (@?= 4) . sessionFileMessageCount,
      testCase "headers and line counts cross chunk boundaries" $ withTemp $ \root -> do
        let path = root </> "chunks.jsonl"
            title = Text.replicate 30000 "界"
        writeHeader path (header title) (BS.replicate 150000 32 <> "x\n\nlast")
        result <- requireFile path
        (sessionFileTitle result, sessionFileMessageCount result) @?= (title, 2),
      testCase "a first JSON header without a trailing newline has zero messages" $ withTemp $ \root -> do
        let path = root </> "single.jsonl"
        LBS.writeFile path (encode (header "single"))
        requireFile path >>= (@?= 0) . sessionFileMessageCount,
      testCase "valid settings supply first mission tags and retain exact raw extensions" $ withTemp $ \root -> do
        let path = root </> "mission.jsonl"
            fields = KeyMap.fromList [("tags", toJSON [tag "mission:first" (object ["missionId" .= String ""]), tag "mission:later" (object ["missionId" .= String "ignored"]), tag "decompSessionType" (object ["value" .= String "worker"])]), ("future", Number 9007199254740993)]
        writeHeader path (object ["type" .= String "session_start", "title" .= String "mission", "decompMissionId" .= String "header", "decompSessionType" .= String "unknown"]) ""
        LBS.writeFile (root </> "mission.settings.json") (encode fields)
        result <- requireFile path
        (sessionFileDecompType result, sessionFileMissionId result) @?= (Just DecompWorker, Just "")
        sessionFileSettings result @?= Just fields,
      testCase "valid header role wins and first missing mission metadata does not merge later tags" $ withTemp $ \root -> do
        let path = root </> "first.jsonl"
        writeHeader path (object ["type" .= String "session_start", "title" .= String "first", "decompSessionType" .= String "orchestrator", "decompMissionId" .= String "header"]) ""
        LBS.writeFile (root </> "first.settings.json") (encode (object ["tags" .= [object ["name" .= String "mission:first"], tag "mission:second" (object ["missionId" .= String "wrong"]), tag "decompSessionType" (object ["value" .= String "worker"])]]))
        result <- requireFile path
        (sessionFileDecompType result, sessionFileMissionId result) @?= (Just DecompOrchestrator, Just "header"),
      testCase "missing malformed and invalid settings do not remove a valid header" $ withTemp $ \root -> do
        let path = root </> "settings.jsonl"
        writeHeader path (header "settings") ""
        forM_ ["{bad", "null", "[]", "{\"tags\":[{\"name\":\"\"}]}", "{\"archivedAt\":null,\"tags\":[{\"name\":\"mission:x\",\"metadata\":{\"missionId\":\"ignored\"}}]}"] $ \bytes -> do
          BS.writeFile (root </> "settings.settings.json") bytes
          result <- requireFile path
          sessionFileMissionId result @?= Nothing,
      testCase "truthy archive markers win even when unrelated tags are invalid" $ withTemp $ \root -> do
        let path = root </> "archived.jsonl"
        writeHeader path (header "archived") ""
        forM_ [String "date", Bool True, Number (-1), toJSON [Null], object ["x" .= Null]] $ \value -> do
          LBS.writeFile (root </> "archived.settings.json") (encode (object ["archivedAt" .= value, "tags" .= Bool False]))
          readDroidSessionFile path >>= (@?= Nothing),
      testCase "false and empty archive markers remain visible" $ withTemp $ \root -> do
        let path = root </> "visible.jsonl"
        writeHeader path (header "visible") ""
        forM_ [Null, Bool False, Number 0, String "", Array mempty, Object mempty] $ \value -> do
          LBS.writeFile (root </> "visible.settings.json") (encode (object ["archivedAt" .= value]))
          requireFile path >>= (@?= "visible") . sessionFileId,
      testCase "favorites retain only exact string IDs and tolerate missing malformed files" $ withTemp $ \root -> do
        readDroidSessionFavorites root >>= (@?= Set.empty)
        forM_ ["{bad", "{}", "null"] $ \bytes -> do
          BS.writeFile (root </> ".favorites") bytes
          readDroidSessionFavorites root >>= (@?= Set.empty)
        BS.writeFile (root </> ".favorites") "[\"\",\"one\",\"one\",null,false,0,\" ../two \" ]"
        readDroidSessionFavorites root >>= (@?= Set.fromList ["", "one", " ../two "]),
      testCase "missing directories and FIFOs are not opened as session streams" $ bounded $ withTemp $ \root -> do
        createDirectory (root </> "directory.jsonl")
        createNamedPipe (root </> "fifo.jsonl") 0o600
        forM_ ["missing.jsonl", "directory.jsonl", "fifo.jsonl"] $ \name ->
          readDroidSessionFile (root </> name) >>= (@?= Nothing),
      testCase "regular symlink targets work and dangling symlinks are skipped" $ withTemp $ \root -> do
        writeHeader (root </> "target") (header "linked") "message\n"
        createSymbolicLink "target" (root </> "link.jsonl")
        createSymbolicLink "absent" (root </> "dangling.jsonl")
        requireFile (root </> "link.jsonl") >>= (@?= "link") . sessionFileId
        readDroidSessionFile (root </> "dangling.jsonl") >>= (@?= Nothing)
        createDirectory (root </> "within")
        createSymbolicLink "../target" (root </> "within" </> "outside.jsonl")
        requireFile (root </> "within" </> "outside.jsonl") >>= (@?= "linked") . sessionFileTitle,
      testCase "unreadable files follow the current process's actual permissions" $ withTemp $ \root -> do
        let path = root </> "permission.jsonl"
        writeHeader path (header "permission") ""
        setFileMode path 0
        uid <- getEffectiveUserID
        value <- readDroidSessionFile path
        if uid == 0 then fmap sessionFileId value @?= Just "permission" else value @?= Nothing,
      testCase "invalid paths raise a payload-free original I/O error instead of absence" $ do
        result <- try @IOException (readDroidSessionFile "private\0.jsonl")
        case result of
          Left err -> do
            assertBool "Not a user path error" (isUserError err)
            ioeGetErrorString err @?= "Invalid saved-session path"
            ioeGetFileName err @?= Nothing
          Right _ -> assertFailure "Invalid path was silently accepted",
      testCase "descriptor observations preserve exact file positions" $ withTemp $ \root -> do
        let path = root </> "position.jsonl"
        writeHeader path (header "position") ""
        bracket (openFd path ReadOnly defaultFileFlags {cloexec = True}) closeFd $ \fd ->
          forM_ [0, 65537, 4294967297] $ \offset -> do
            _ <- fdSeek fd AbsoluteSeek (fromInteger offset)
            descriptorsFor path >>= (@?= [(fd, offset)])
            descriptorsFor path >>= (@?= [(fd, offset)]),
      testCase "cancelling an active regular-file scan preserves the exception and closes its descriptor" $ bounded $ withTemp $ \root -> do
        let path = root </> "cancel.jsonl"
        sparseSession path (8 * 1024 * 1024 * 1024)
        withAsync (readDroidSessionFile path) $ \worker -> do
          (fd, _) <- waitForRead path
          queryFdOption fd CloseOnExec >>= (@?= True)
          queryFdOption fd NonBlockingRead >>= (@?= True)
          throwTo (asyncThreadId worker) ScanCancelled
          result <- waitCatch worker
          case result of
            Left err -> fromException err @?= Just ScanCancelled
            Right _ -> assertFailure "Scan completed before cancellation"
          descriptorsFor path >>= (@?= []),
      testCase "truncation after reading begins does not publish a partial observation" $ bounded $ withTemp $ \root -> do
        let path = root </> "truncate.jsonl"
        sparseSession path (8 * 1024 * 1024 * 1024)
        withAsync (readDroidSessionFile path) $ \worker -> do
          _ <- waitForRead path
          withBinaryFile path WriteMode (`hSetFileSize` 0)
          waitCatch worker >>= \case
            Right value -> value @?= Nothing
            Left err -> assertFailure (show err),
      testCase "appended records beyond the observed size are not chased" $ bounded $ withTemp $ \root -> do
        let path = root </> "append.jsonl"
            initialSize = 64 * 1024 * 1024
        sparseSession path initialSize
        withAsync (readDroidSessionFile path) $ \worker -> do
          (_, offset) <- waitForRead path
          assertBool "Reader already finished its prefix" (offset < initialSize)
          withBinaryFile path AppendMode (`BS.hPut` "\nextra\n")
          waitCatch worker >>= \case
            Right (Just result) -> sessionFileMessageCount result @?= 1
            _ -> assertFailure "Growing file was not observed",
      testCase "reported birth availability is explicit and macOS supplies its native field" $ withTemp $ \root -> do
        let path = root </> "birth.jsonl"
        writeHeader path (header "birth") ""
        result <- requireFile path
        case sessionFileBirthTime result of
          Just birth -> sessionFileCreatedAt result @?= birth
          Nothing -> do
            assertBool "Native macOS birth field was not read" (os /= "darwin")
            sessionFileCreatedAt result @?= sessionFileStatusChangedAt result,
      testCase "status changes do not replace a reported birth timestamp" $ bounded $ withTemp $ \root -> do
        let path = root </> "changed.jsonl"
        writeHeader path (header "changed") ""
        before <- requireFile path
        let waitForChange = do
              setFileMode path 0o600
              value <- requireFile path
              if sessionFileStatusChangedAt value == sessionFileStatusChangedAt before
                then threadDelay 1000 >> waitForChange
                else pure value
        after <- waitForChange
        sessionFileBirthTime after @?= sessionFileBirthTime before
        case sessionFileBirthTime before of
          Just birth -> sessionFileCreatedAt after @?= birth
          Nothing -> sessionFileCreatedAt after @?= sessionFileStatusChangedAt after,
      testCase "SDK creation projection retains zero, negative and exact fractional birth values" $ withTemp $ \root -> do
        let path = root </> "projection.jsonl"
            changed = posixSecondsToUTCTime 17.25
        writeHeader path (header "projection") ""
        result <- requireFile path
        forM_ [0, -0.000000001, 0.123456789, 9007199254740993] $ \seconds -> do
          let birth = posixSecondsToUTCTime seconds
              observed = result {sessionFileBirthTime = Just birth, sessionFileStatusChangedAt = changed}
          sessionFileCreatedAt observed @?= birth
          sessionFileBirthTime observed @?= Just birth
        let unavailable = result {sessionFileBirthTime = Nothing, sessionFileStatusChangedAt = changed}
        sessionFileCreatedAt unavailable @?= changed
        sessionFileBirthTime unavailable @?= Nothing,
      testCase "birth observations remain bound to the opened inode across path replacement" $ bounded $ withTemp $ \root -> do
        let path = root </> "replace.jsonl"
        sparseSession path (64 * 1024 * 1024)
        original <- requireFile path
        withAsync (readDroidSessionFile path) $ \worker -> do
          _ <- waitForRead path
          renameFile path (root </> "retired.jsonl")
          writeHeader path (header "replacement") ""
          waitCatch worker >>= \case
            Right (Just result) -> do
              sessionFileTitle result @?= "sparse"
              sessionFileBirthTime result @?= sessionFileBirthTime original
              sessionFileCreatedAt result @?= sessionFileCreatedAt original
            _ -> assertFailure "Opened inode was not retained"
    ]

header :: Text -> Value
header title = object ["type" .= String "session_start", "title" .= title]

tag :: Text -> Value -> Value
tag name metadata = object ["name" .= name, "metadata" .= metadata]

writeHeader :: FilePath -> Value -> BS.ByteString -> IO ()
writeHeader path value rest = BS.writeFile path (LBS.toStrict (encode value) <> "\n" <> rest)

requireFile :: FilePath -> IO DroidSessionFile
requireFile path = readDroidSessionFile path >>= maybe (assertFailure "Expected a session file") pure

withTemp :: (FilePath -> IO a) -> IO a
withTemp = bracket (mkdtemp "/tmp/droid-saved-session-") removePathForcibly

sparseSession :: FilePath -> Integer -> IO ()
sparseSession path size = withBinaryFile path WriteMode $ \handle -> do
  BS.hPut handle (LBS.toStrict (encode (header "sparse")))
  BS.hPut handle "\n"
  hSetFileSize handle size

data ScanCancelled = ScanCancelled deriving stock (Eq, Show)

instance Exception ScanCancelled

descriptorsFor :: FilePath -> IO [(Fd, Integer)]
descriptorsFor path = do
  target <- getFileStatus path
  names <- listDirectory "/dev/fd"
  fmap catMaybes $ forM names $ \name -> case readMaybe @Int name of
    Nothing -> pure Nothing
    Just number -> do
      let fd = Fd (fromIntegral number)
      result <- try @IOException $ do
        status <- getFdStatus fd
        if fileID status == fileID target && deviceID status == deviceID target
          then Just . (fd,) <$> observedFilePosition fd
          else pure Nothing
      pure (fromRight Nothing result)

foreign import ccall unsafe "droid_test_fd_position" fdPosition :: CInt -> Ptr Int64 -> IO CInt

observedFilePosition :: Fd -> IO Integer
observedFilePosition (Fd fd) = alloca $ \position -> do
  throwErrnoIfMinus1_ "droid_test_fd_position" (fdPosition fd position)
  toInteger <$> peek position

waitForRead :: FilePath -> IO (Fd, Integer)
waitForRead path = do
  descriptors <- descriptorsFor path
  case [(fd, offset) | (fd, offset) <- descriptors, offset > 0] of
    descriptor : _ -> pure descriptor
    [] -> threadDelay 1000 >> waitForRead path
