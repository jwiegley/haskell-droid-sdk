{-# LANGUAGE OverloadedStrings #-}

-- | Read-only local session-file discovery, without launching Droid or invoking
-- credential discovery. Caller-selected paths can resolve through symlinks;
-- this is not a filesystem sandbox. Change time is not creation time.
module Factory.Droid.Discovery
  ( DroidSessionFile (..),
    DroidSavedSession (..),
    ListDroidSessionsOptions (..),
    defaultListDroidSessionsOptions,
    listDroidSessions,
    sessionFileCreatedAt,
    defaultDroidSessionsDirectory,
    scanDroidSessionDirectory,
    readDroidSessionFile,
    readDroidSessionFavorites,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (allowInterrupt, bracket, throwIO)
import Control.Monad (void, when)
import Data.Aeson (Object, Value (..), decodeStrict', parseJSON, (.:), (.:!))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.List (dropWhileEnd, find, isPrefixOf, isSuffixOf, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as Vector
import Factory.Droid.Internal.FileTime (fileBirthTime)
import Factory.Droid.Internal.JSON (isEcmaWhitespace, requireLiteral)
import Factory.Droid.Schema.Mission (DecompSessionType)
import Factory.Droid.Schema.Session (SessionTag (..), sessionTagNameText)
import System.Directory (canonicalizePath, doesPathExist, getCurrentDirectory, getHomeDirectory, listDirectory)
import System.FilePath (isAbsolute, isPathSeparator, joinPath, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO.Error (catchIOError, isDoesNotExistError, isPermissionError)
import System.Posix.Files (FileStatus, deviceID, fileID, fileSize, getFdStatus, getFileStatus, isDirectory, isRegularFile, modificationTimeHiRes, statusChangeTimeHiRes)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadOnly), closeFd, defaultFileFlags, fdReadBuf, openFd)
import System.Posix.Types (Fd)

-- | An unarchived file observation, before cross-directory selection. Raw cwd
-- and JSON remain intact. Status-change time is explicitly not creation time.
data DroidSessionFile = DroidSessionFile
  { sessionFilePath :: !FilePath,
    sessionFileId :: !Text,
    sessionFileTitle :: !Text,
    sessionFileOwner :: !Text,
    sessionFileCwd :: !(Maybe Text),
    sessionFileMessageCount :: !Integer,
    sessionFileModifiedAt :: !UTCTime,
    sessionFileStatusChangedAt :: !UTCTime,
    sessionFileBirthTime :: !(Maybe UTCTime),
    sessionFileDecompType :: !(Maybe DecompSessionType),
    sessionFileMissionId :: !(Maybe Text),
    sessionFileHeader :: !Object,
    sessionFileSettings :: !(Maybe Object)
  }
  deriving stock (Eq)

instance Show DroidSessionFile where
  show _ = "DroidSessionFile <redacted>"

-- | SDK-compatible creation-time projection: use reported birth time when
-- available, otherwise the explicitly retained status-change observation.
-- Inspect sessionFileBirthTime to distinguish this fallback from actual birth.
sessionFileCreatedAt :: DroidSessionFile -> UTCTime
sessionFileCreatedAt file = fromMaybe (sessionFileStatusChangedAt file) (sessionFileBirthTime file)

-- | A selected observation with root-level favorite membership and a resolved
-- cwd view. The nested file retains raw cwd, metadata and timestamp provenance.
data DroidSavedSession = DroidSavedSession
  { savedSessionFile :: !DroidSessionFile,
    savedSessionCwd :: !(Maybe FilePath),
    savedSessionIsFavorite :: !Bool
  }
  deriving stock (Eq)

instance Show DroidSavedSession where
  show _ = "DroidSavedSession <redacted>"

data ListDroidSessionsOptions = ListDroidSessionsOptions
  { listSessionsCwd :: !(Maybe FilePath),
    listSessionsDirectory :: !(Maybe FilePath),
    listSessionsOutsideCwd :: !Bool,
    listSessionsLimit :: !(Maybe Int)
  }
  deriving stock (Eq)

instance Show ListDroidSessionsOptions where
  show _ = "ListDroidSessionsOptions <redacted>"

defaultListDroidSessionsOptions :: ListDroidSessionsOptions
defaultListDroidSessionsOptions = ListDroidSessionsOptions Nothing Nothing False Nothing

-- | Read local project and legacy layouts without an engine or authentication.
-- Filter before newest-ID deduplication; limit only the final deterministic
-- order (modification time descending, then ID and path by code-point order).
listDroidSessions :: ListDroidSessionsOptions -> IO [DroidSavedSession]
listDroidSessions options = do
  when (maybe False (< 0) (listSessionsLimit options)) (ioError (userError "Saved-session limit must be non-negative"))
  mapM_ validatePath (listSessionsCwd options)
  mapM_ validatePath (listSessionsDirectory options)
  if listSessionsLimit options == Just 0
    then pure []
    else do
      base <- getCurrentDirectory
      suppliedRoot <- maybe defaultDroidSessionsDirectory pure (listSessionsDirectory options)
      let root = absoluteFrom base suppliedRoot
      rootIsDirectory <- unavailable False (isDirectory <$> getFileStatus root)
      if not rootIsDirectory
        then pure []
        else do
          requested <-
            if listSessionsOutsideCwd options
              then pure Nothing
              else Just <$> resolveWorkingDirectory base (fromMaybe base (listSessionsCwd options))
          projectNames <- case requested of
            Nothing -> filter ("-" `isPrefixOf`) <$> directoryEntries root
            Just paths -> projectDirectoryNames paths
          favorites <- readDroidSessionFavorites root
          groups <- traverse (\(legacy, directory) -> map (legacy,) <$> scanDroidSessionDirectory directory) ((True, root) : [(False, root </> name) | name <- projectNames])
          let prepare (legacy, file) = do
                resolved <- case sessionFileCwd file of
                  Just cwd | not (Text.any (== '\0') cwd) -> Just <$> resolveWorkingDirectory base (Text.unpack cwd)
                  _ -> pure Nothing
                let included = case (legacy, requested) of
                      (True, Just expected) -> maybe False (sameWorkingDirectory expected) resolved
                      _ -> True
                pure $
                  if included
                    then Just (DroidSavedSession file (fst <$> resolved) (Set.member (sessionFileId file) favorites))
                    else Nothing
              rank result = let file = savedSessionFile result in (Down (sessionFileModifiedAt file), sessionFileId file, sessionFilePath file)
              newest left right = if rank left <= rank right then left else right
          eligible <- catMaybes <$> traverse prepare (concat groups)
          let unique = Map.elems (Map.fromListWith newest [(sessionFileId (savedSessionFile result), result) | result <- eligible])
              ordered = sortOn rank unique
          pure (maybe ordered (`take` ordered) (listSessionsLimit options))

absoluteFrom :: FilePath -> FilePath -> FilePath
absoluteFrom base path = if isAbsolute path then path else base </> path

-- Canonical filesystem resolution and lexical resolution are both used by
-- the baselined SDKs. Keep the two meanings explicit rather than guessing an SDK.
resolveWorkingDirectory :: FilePath -> FilePath -> IO (FilePath, FilePath)
resolveWorkingDirectory base path = do
  let absolute = absoluteFrom base path
  canonical <- canonicalizePath absolute
  pure (lexicalAbsolute canonical, lexicalAbsolute absolute)

lexicalAbsolute :: FilePath -> FilePath
lexicalAbsolute path = "/" </> joinPath (reverse (foldl' step [] (splitDirectories path)))
  where
    step parts component
      | all isPathSeparator component || component == "." = parts
      | component == ".." = case parts of [] -> []; _ : rest -> rest
      | otherwise = component : parts

sameWorkingDirectory :: (FilePath, FilePath) -> (FilePath, FilePath) -> Bool
sameWorkingDirectory (canonical, lexical) (otherCanonical, otherLexical) = canonical == otherCanonical || lexical == otherLexical

projectDirectoryNames :: (FilePath, FilePath) -> IO [FilePath]
projectDirectoryNames (canonical, lexical) = do
  exists <- doesPathExist lexical
  strictCanonical <- if exists then canonicalizePath lexical else pure lexical
  let key replaceBackslash path =
        '-' : map (\char -> if char == '/' || (replaceBackslash && char == '\\') then '-' else char) (dropWhile (\char -> char == '/' || (replaceBackslash && char == '\\')) (dropWhileEnd (`elem` ("/\\" :: String)) path))
  pure (Set.toList (Set.fromList [key True canonical, key False (lexicalAbsolute strictCanonical)]))

-- | The current user's default storage root, without reading its contents.
defaultDroidSessionsDirectory :: IO FilePath
defaultDroidSessionsDirectory = do
  home <- getHomeDirectory
  pure (home </> ".factory" </> "sessions")

-- | Scan direct .jsonl entries in one caller-selected directory, in filename
-- order. Missing/permission-denied directories yield no observations. Workspace
-- selection, cross-directory deduplication and limits are separate operations.
scanDroidSessionDirectory :: FilePath -> IO [DroidSessionFile]
scanDroidSessionDirectory directory = do
  names <- directoryEntries directory
  catMaybes <$> traverse (readDroidSessionFile . (directory </>)) (filter (isSuffixOf ".jsonl") names)

directoryEntries :: FilePath -> IO [FilePath]
directoryEntries directory = do
  validatePath directory
  unavailable [] $ do
    status <- getFileStatus directory
    if isDirectory status then sort <$> listDirectory directory else pure []

-- | Read the first JSONL header and count nonempty physical lines after it.
-- Nonempty follows the TypeScript byte scanner: only CR, space and tab are
-- ignored within a line. Later lines are counted, not decoded or retained.
-- Missing, permission-denied, nonregular, malformed and archived files yield Nothing;
-- other I/O failures and asynchronous exceptions propagate unchanged.
readDroidSessionFile :: FilePath -> IO (Maybe DroidSessionFile)
readDroidSessionFile path = do
  validatePath path
  if not (".jsonl" `isSuffixOf` takeFileName path)
    then pure Nothing
    else do
      observed <- foldObservedFile path (\fd status -> (status,) <$> fileBirthTime fd) (LineScan [] False 0 False) scanChunk
      case observed of
        Nothing -> pure Nothing
        Just ((status, birth), LineScan headerParts _ linesSeen pendingLine) ->
          case decodeStrict' (BS.concat (reverse headerParts)) >>= parseMaybe parseHeader of
            Nothing -> pure Nothing
            Just (header, title) -> do
              let identifier = Text.dropEnd 6 (Text.pack (takeFileName path))
              settings <- readJsonObject (takeDirectory path </> Text.unpack identifier <> ".settings.json")
              if maybe False archived settings
                then pure Nothing
                else do
                  let tags = fromMaybe [] (settings >>= parseMaybe parseSettingsTags)
                      metadata predicate key = find (predicate . sessionTagNameText . sessionTagName) tags >>= sessionTagMetadata >>= KeyMap.lookup key
                      role = metadata (== "decompSessionType") "value" >>= parseMaybe parseJSON . String
                      mission = metadata (Text.isPrefixOf "mission:") "missionId"
                      count = max 0 (linesSeen + (if pendingLine then 1 else 0) - 1)
                  pure $
                    Just $
                      DroidSessionFile
                        path
                        identifier
                        title
                        (fromMaybe "" (textField "owner" header))
                        (textField "cwd" header)
                        count
                        (posixSecondsToUTCTime (modificationTimeHiRes status))
                        (posixSecondsToUTCTime (statusChangeTimeHiRes status))
                        birth
                        ((KeyMap.lookup "decompSessionType" header >>= parseMaybe parseJSON) <|> role)
                        (mission <|> textField "decompMissionId" header)
                        header
                        settings

-- | Read .favorites once per root. Only string entries are IDs; empty strings
-- remain literal. Missing/permission-denied or malformed files yield an empty set.
readDroidSessionFavorites :: FilePath -> IO (Set Text)
readDroidSessionFavorites directory = do
  value <- readJsonFile (directory </> ".favorites")
  pure $ case value of
    Just (Array values) -> Set.fromList [text | String text <- Vector.toList values]
    _ -> Set.empty

parseHeader :: Value -> Parser (Object, Text)
parseHeader (Object fields) = do
  requireLiteral "type" "session_start" fields
  title <- case textField "sessionTitle" fields of
    Just legacy | not (Text.null (Text.dropAround isEcmaWhitespace legacy)) -> pure legacy
    _ -> fields .: "title"
  pure (fields, title)
parseHeader _ = fail "Session header must be an object"

textField :: Key -> Object -> Maybe Text
textField key fields = case KeyMap.lookup key fields of
  Just (String value) -> Just value
  _ -> Nothing

parseSettingsTags :: Object -> Parser [SessionTag]
parseSettingsTags fields = do
  void (fields .:! "archivedAt" :: Parser (Maybe Text))
  fromMaybe [] <$> fields .:! "tags"

-- A malformed unrelated setting must not conceal Python's archive marker.
archived :: Object -> Bool
archived fields = case KeyMap.lookup "archivedAt" fields of
  Nothing -> False
  Just Null -> False
  Just (Bool value) -> value
  Just (Number value) -> value /= 0
  Just (String value) -> not (Text.null value)
  Just (Array value) -> not (Vector.null value)
  Just (Object value) -> not (KeyMap.null value)

readJsonObject :: FilePath -> IO (Maybe Object)
readJsonObject path =
  readJsonFile path >>= \case
    Just (Object fields) -> pure (Just fields)
    _ -> pure Nothing

readJsonFile :: FilePath -> IO (Maybe Value)
readJsonFile path = do
  observed <- foldObservedFile path (\_ _ -> pure ()) [] (flip (:))
  pure (observed >>= decodeStrict' . BS.concat . reverse . snd)

data LineScan = LineScan ![BS.ByteString] !Bool !Integer !Bool

scanChunk :: LineScan -> BS.ByteString -> LineScan
scanChunk previous@(LineScan parts headerDone _ _) chunk =
  let LineScan _ _ count nonempty = BS.foldl' countByte previous chunk
      (prefix, suffix) = BS.break (== 10) chunk
   in LineScan (if headerDone then parts else prefix : parts) (headerDone || not (BS.null suffix)) count nonempty
  where
    countByte (LineScan prefixes done count nonempty) byte
      | byte == 10 = LineScan prefixes done (count + if nonempty then 1 else 0) False
      | byte `elem` [13, 32, 9] = LineScan prefixes done count nonempty
      | otherwise = LineScan prefixes done count True

-- Bound each read to the regular file's size at open, never a growing EOF.
-- Descriptor identity is checked after nonblocking/CLOEXEC acquisition, so a
-- pre-open replacement by a FIFO cannot block this reader.
foldObservedFile :: FilePath -> (Fd -> FileStatus -> IO metadata) -> a -> (a -> BS.ByteString -> a) -> IO (Maybe (metadata, a))
foldObservedFile path observe initial step = do
  validatePath path
  unavailable Nothing $ do
    expected <- getFileStatus path
    if not (isRegularFile expected)
      then pure Nothing
      else bracket (openFd path ReadOnly defaultFileFlags {nonBlock = True, cloexec = True}) closeFd $ \fd -> do
        status <- getFdStatus fd
        if not (isRegularFile status) || deviceID expected /= deviceID status || fileID expected /= fileID status
          then pure Nothing
          else do
            metadata <- observe fd status
            let loop remaining value
                  | remaining == 0 = pure (Just (metadata, value))
                  | otherwise = do
                      allowInterrupt
                      let size = fromInteger (min 65536 remaining)
                      bytes <- BS.createUptoN size (\buffer -> fromIntegral <$> fdReadBuf fd buffer (fromIntegral size))
                      if BS.null bytes
                        then pure Nothing
                        else loop (remaining - toInteger (BS.length bytes)) $! step value bytes
            loop (toInteger (fileSize status)) initial

unavailable :: a -> IO a -> IO a
unavailable fallback action =
  action `catchIOError` \err ->
    if isDoesNotExistError err || isPermissionError err then pure fallback else throwIO err

validatePath :: FilePath -> IO ()
validatePath path = when ('\0' `elem` path) (ioError (userError "Invalid saved-session path"))
