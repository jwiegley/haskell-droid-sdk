{-# LANGUAGE OverloadedStrings #-}

module SavedSessionSelectionSpec (savedSelectionTests, runSavedSelectionPeer) where

import Control.Concurrent (throwTo)
import Control.Concurrent.Async (asyncThreadId, waitCatch, withAsync)
import Control.Exception (IOException, fromException, try)
import Control.Monad (forM_, (>=>))
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (POSIXTime)
import Factory.Droid qualified as Droid
import Factory.Droid.Discovery
import ProcessSpec (bounded)
import SavedSessionSpec (ScanCancelled (..), descriptorsFor, sparseSession, waitForRead, withTemp, writeHeader)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)
import System.Posix.Files (createSymbolicLink, setFileTimesHiRes)
import System.Process qualified as Process
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

savedSelectionTests :: TestTree
savedSelectionTests =
  testGroup
    "Saved session selection"
    [ testCase "facade listing combines selected projects and legacy files with root favorites" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
            project = root </> projectKey False cwd
            options = selected root cwd
        createDirectoryIfMissing True cwd
        putSession root "legacy" (Just cwd) 1 "legacy"
        putSession project "project" Nothing 2 "project"
        BS.writeFile (root </> ".favorites") "[\"project\"]"
        BS.writeFile (project </> ".favorites") "[\"legacy\"]"
        results <- Droid.listDroidSessions options
        ids results @?= ["project", "legacy"]
        map savedSessionIsFavorite results @?= [True, False]
        map savedSessionCwd results @?= [Nothing, Just cwd]
        map show results @?= replicate 2 "DroidSavedSession <redacted>"
        show options @?= "ListDroidSessionsOptions <redacted>",
      testCase "legacy cwd is filtered while selected project metadata is descriptive" $ withSelectionTemp $ \root -> do
        let cwd = root </> "chosen"
        putSession root "matched" (Just cwd) 1 "matched"
        putSession root "other" (Just (root </> "other")) 4 "other"
        putSession root "missing" Nothing 3 "missing"
        putSession (root </> projectKey False cwd) "project" (Just (root </> "different")) 2 "project"
        results <- listDroidSessions (selected root cwd)
        ids results @?= ["project", "matched"]
        map savedSessionCwd results @?= [Just (root </> "different"), Just cwd],
      testCase "outside-cwd mode includes only one level of prefixed project directories" $ withSelectionTemp $ \root -> do
        putSession root "legacy" Nothing 1 "legacy"
        putSession (root </> "-project") "project" Nothing 2 "project"
        putSession (root </> "ordinary") "ignored" Nothing 3 "ignored"
        putSession (root </> "-project" </> "-nested") "nested" Nothing 4 "nested"
        listDroidSessions (allStored root) >>= (@?= ["project", "legacy"]) . ids,
      testCase "newest exact modification wins duplicate IDs without rebuilding metadata" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
            project = root </> projectKey False cwd
        putSession root "same" (Just cwd) 1 "old"
        putSession project "same" Nothing 2 "new"
        LBS.writeFile (project </> "same.settings.json") (encode (object ["future" .= Number 9007199254740993]))
        results <- listDroidSessions (selected root cwd)
        map (sessionFileTitle . savedSessionFile) results @?= ["new"]
        map (sessionFileSettings . savedSessionFile) results @?= [Just (KeyMap.singleton "future" (Number 9007199254740993))],
      testCase "same-time duplicates use deterministic path order" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
        putSession root "same" (Just cwd) 2 "legacy"
        putSession (root </> projectKey False cwd) "same" Nothing 2 "project"
        listDroidSessions (selected root cwd) >>= (@?= ["project"]) . map (sessionFileTitle . savedSessionFile),
      testCase "same-time ID order is code-point order rather than locale or UTF-16 order" $ withSelectionTemp $ \root -> do
        forM_ ["z", "a", "\x10000", "\xe000"] $ \identifier -> putSession root identifier Nothing 5 identifier
        listDroidSessions (allStored root) >>= (@?= ["a", "z", "\xe000", "\x10000"]) . ids,
      testCase "sub-millisecond modification differences remain authoritative" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
        putSession root "same" (Just cwd) 10.000000002 "newer"
        putSession (root </> projectKey False cwd) "same" Nothing 10.000000001 "older"
        listDroidSessions (selected root cwd) >>= (@?= ["newer"]) . map (sessionFileTitle . savedSessionFile),
      testCase "workspace filtering happens before duplicate selection" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
        putSession root "same" (Just (root </> "elsewhere")) 100 "foreign"
        putSession (root </> projectKey False cwd) "same" Nothing 1 "selected"
        listDroidSessions (selected root cwd) >>= (@?= ["selected"]) . map (sessionFileTitle . savedSessionFile)
        listDroidSessions (allStored root) >>= (@?= ["foreign"]) . map (sessionFileTitle . savedSessionFile),
      testCase "archived or malformed newer copies do not hide valid older entries" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
            project = root </> projectKey False cwd
        putSession root "same" (Just cwd) 1 "old"
        putSession project "same" Nothing 100 "archived"
        BS.writeFile (project </> "same.settings.json") "{\"archivedAt\":\"date\"}"
        listDroidSessions (selected root cwd) >>= (@?= ["old"]) . map (sessionFileTitle . savedSessionFile)
        BS.writeFile (project </> "same.jsonl") "{bad"
        listDroidSessions (selected root cwd) >>= (@?= ["old"]) . map (sessionFileTitle . savedSessionFile),
      testCase "limit is applied after all duplicate IDs rather than a fixed oversampling cutoff" $ withSelectionTemp $ \root -> do
        forM_ [0 .. 24 :: Int] $ \index -> putSession (root </> ("-copy-" <> show index)) "duplicate" Nothing (fromIntegral index + 100) (Text.pack (show index))
        putSession (root </> "-unique") "unique" Nothing 1 "unique"
        results <- listDroidSessions ((allStored root) {listSessionsLimit = Just 2})
        ids results @?= ["duplicate", "unique"]
        map (sessionFileTitle . savedSessionFile) results @?= ["24", "unique"],
      testCase "omitted zero finite and large limits preserve the final sorted prefix" $ withSelectionTemp $ \root -> do
        putSession root "a" Nothing 1 "a"
        putSession root "b" Nothing 2 "b"
        forM_ [(Nothing, ["b", "a"]), (Just 0, []), (Just 1, ["b"]), (Just 2, ["b", "a"]), (Just maxBound, ["b", "a"])] $ \(limit, expected) ->
          listDroidSessions ((allStored root) {listSessionsLimit = limit}) >>= (@?= expected) . ids,
      testCase "zero limit avoids filesystem access but still validates caller paths" $ withSelectionTemp $ \root -> do
        let loop = root </> "loop"
        createSymbolicLink "loop" loop
        listDroidSessions ((allStored loop) {listSessionsLimit = Just 0}) >>= (@?= [])
        positive <- try @IOException (listDroidSessions ((allStored loop) {listSessionsLimit = Just 1}))
        case positive of
          Left _ -> pure ()
          Right _ -> assertFailure "The positive-limit control did not touch the invalid root"
        invalid <- try @IOException (listDroidSessions ((allStored "private\0") {listSessionsLimit = Just 0}))
        case invalid of
          Left err -> ioeGetErrorString err @?= "Invalid saved-session path"
          Right _ -> assertFailure "Zero limit bypassed path validation",
      testCase "negative limits fail before path validation and NUL options are not normalized" $ do
        result <- try @IOException (listDroidSessions (defaultListDroidSessionsOptions {listSessionsLimit = Just (-1), listSessionsDirectory = Just "private\0"}))
        case result of
          Left err -> ioeGetErrorString err @?= "Saved-session limit must be non-negative"
          Right _ -> assertFailure "Negative limit was accepted"
        forM_ [defaultListDroidSessionsOptions {listSessionsDirectory = Just "private\0"}, defaultListDroidSessionsOptions {listSessionsCwd = Just "private\0", listSessionsOutsideCwd = True}] $ \options -> do
          invalid <- try @IOException (listDroidSessions options)
          case invalid of
            Left err -> ioeGetErrorString err @?= "Invalid saved-session path"
            Right _ -> assertFailure "NUL path was accepted",
      testCase "missing roots and explicit file roots yield no stored sessions" $ withSelectionTemp $ \root -> do
        listDroidSessions (allStored (root </> "missing")) >>= (@?= [])
        BS.writeFile (root </> "file") "not a directory"
        listDroidSessions (allStored (root </> "file")) >>= (@?= [])
        listDroidSessions (selected (root </> "file") root) >>= (@?= []),
      testCase "default home root and current cwd work in an isolated native peer" $ bounded $ withSelectionTemp $ \root -> do
        let home = root </> "home"
            cwd = root </> "work"
        createDirectoryIfMissing True cwd
        putSession (home </> ".factory" </> "sessions") "default" (Just cwd) 1 "default"
        runPeer home cwd [] >>= (@?= [("default", False, Just cwd)]),
      testCase "relative root and empty cwd use one process-cwd snapshot" $ bounded $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
        putSession (cwd </> "sessions") "relative" (Just ".") 1 "relative"
        runPeer (root </> "home") cwd ["sessions", ""] >>= (@?= [("relative", False, Just cwd)]),
      testCase "explicit empty root does not select the default home root" $ bounded $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
            home = root </> "home"
        putSession cwd "empty-root" (Just ".") 1 "empty-root"
        putSession (home </> ".factory" </> "sessions") "wrong" (Just cwd) 2 "wrong"
        runPeer home cwd ["", ""] >>= (@?= [("empty-root", False, Just cwd)]),
      testCase "tilde options are literal relative paths as in the list entry points" $ bounded $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
            literal = cwd </> "~"
        putSession (literal </> "sessions") "literal" (Just "~") 1 "literal"
        runPeer (root </> "home") cwd ["~/sessions", "~"] >>= (@?= [("literal", False, Just literal)]),
      testCase "canonical legacy matching recognizes a symlink alias" $ withSelectionTemp $ \root -> do
        let physical = root </> "physical"
            alias = root </> "alias"
        createDirectoryIfMissing True physical
        createSymbolicLink "physical" alias
        putSession root "physical" (Just physical) 1 "physical"
        putSession root "alias" (Just alias) 2 "alias"
        results <- listDroidSessions (selected root alias)
        ids results @?= ["alias", "physical"]
        map savedSessionCwd results @?= [Just physical, Just physical],
      testCase "missing cwd suffixes retain canonical and lexical stored-directory conventions" $ withSelectionTemp $ \root -> do
        let physical = root </> "physical"
            alias = root </> "alias"
        createDirectoryIfMissing True physical
        createSymbolicLink "physical" alias
        putSession (root </> projectKey False (physical </> "missing")) "canonical" Nothing 1 "canonical"
        putSession (root </> projectKey False (alias </> "missing")) "lexical" Nothing 2 "lexical"
        listDroidSessions (selected root (alias </> "missing")) >>= (@?= ["lexical", "canonical"]) . ids,
      testCase "parent segments across symlinks support both baselined directory keys" $ withSelectionTemp $ \root -> do
        createDirectoryIfMissing True (root </> "physical" </> "child")
        createDirectoryIfMissing True (root </> "physical" </> "choice")
        createDirectoryIfMissing True (root </> "choice")
        createSymbolicLink "physical/child" (root </> "alias")
        putSession (root </> projectKey False (root </> "physical" </> "choice")) "physical" Nothing 1 "physical"
        putSession (root </> projectKey False (root </> "choice")) "lexical" Nothing 2 "lexical"
        listDroidSessions (selected root (root </> "alias" </> ".." </> "choice")) >>= (@?= ["lexical", "physical"]) . ids,
      testCase "both backslash key conventions and root workspace spelling are supported" $ withSelectionTemp $ \root -> do
        let cwd = root </> "with\\slash"
        createDirectoryIfMissing True cwd
        putSession (root </> projectKey False cwd) "literal" Nothing 1 "literal"
        putSession (root </> projectKey True cwd) "escaped" Nothing 2 "escaped"
        listDroidSessions (selected root cwd) >>= (@?= ["escaped", "literal"]) . ids
        putSession (root </> "-") "" Nothing 3 "empty-id"
        listDroidSessions (selected root "/") >>= (@?= [""]) . ids,
      testCase "NUL stored cwd cannot match a legacy workspace or cause filesystem access" $ withSelectionTemp $ \root -> do
        let cwd = root </> "work"
            invalid = cwd <> "\0ignored"
        putSession root "legacy" (Just invalid) 2 "legacy"
        putSession (root </> projectKey False cwd) "project" (Just invalid) 1 "project"
        results <- listDroidSessions (selected root cwd)
        ids results @?= ["project"]
        map savedSessionCwd results @?= [Nothing]
        map (sessionFileCwd . savedSessionFile) results @?= [Just (Text.pack invalid)],
      testCase "cancellation of listing preserves its exception and closes the active file" $ bounded $ withSelectionTemp $ \root -> do
        let path = root </> "slow.jsonl"
        sparseSession path (8 * 1024 * 1024 * 1024)
        withAsync (listDroidSessions (allStored root)) $ \worker -> do
          _ <- waitForRead path
          throwTo (asyncThreadId worker) ScanCancelled
          waitCatch worker >>= \case
            Left err -> fromException err @?= Just ScanCancelled
            Right _ -> assertFailure "Listing completed before cancellation"
          descriptorsFor path >>= (@?= [])
    ]

selected :: FilePath -> FilePath -> ListDroidSessionsOptions
selected root cwd = defaultListDroidSessionsOptions {listSessionsDirectory = Just root, listSessionsCwd = Just cwd}

allStored :: FilePath -> ListDroidSessionsOptions
allStored root = defaultListDroidSessionsOptions {listSessionsDirectory = Just root, listSessionsOutsideCwd = True}

ids :: [DroidSavedSession] -> [Text]
ids = map (sessionFileId . savedSessionFile)

withSelectionTemp :: (FilePath -> IO a) -> IO a
withSelectionTemp action = withTemp (canonicalizePath >=> action)

projectKey :: Bool -> FilePath -> FilePath
projectKey backslashes path = '-' : map replace (dropWhile (== '/') (dropWhileEnd (`elem` ("/\\" :: String)) path))
  where
    replace char = if char == '/' || (backslashes && char == '\\') then '-' else char

putSession :: FilePath -> Text -> Maybe FilePath -> POSIXTime -> Text -> IO ()
putSession directory identifier cwd modified title = do
  createDirectoryIfMissing True directory
  let path = directory </> Text.unpack identifier <> ".jsonl"
  writeHeader path (object (["type" .= String "session_start", "title" .= title] <> maybe [] (\value -> ["cwd" .= value]) cwd)) ""
  setFileTimesHiRes path modified modified

runSavedSelectionPeer :: [String] -> IO ()
runSavedSelectionPeer arguments = do
  options <- case arguments of
    [] -> pure defaultListDroidSessionsOptions
    [root, cwd] -> pure (selected root cwd)
    _ -> fail "Invalid native selection peer arguments"
  results <- Droid.listDroidSessions options
  LBS.putStr (encode [(sessionFileId (savedSessionFile value), savedSessionIsFavorite value, savedSessionCwd value) | value <- results])

runPeer :: FilePath -> FilePath -> [String] -> IO [(Text, Bool, Maybe FilePath)]
runPeer home workingDirectory arguments = do
  executable <- getExecutablePath
  let process = (Process.proc executable ("--saved-selection-peer" : arguments)) {Process.cwd = Just workingDirectory, Process.env = Just [("HOME", home)]}
  (code, output, errors) <- Process.readCreateProcessWithExitCode process ""
  code @?= ExitSuccess
  errors @?= ""
  case eitherDecodeStrict' (Text.encodeUtf8 (Text.pack output)) of
    Left err -> assertFailure err
    Right values -> pure values
