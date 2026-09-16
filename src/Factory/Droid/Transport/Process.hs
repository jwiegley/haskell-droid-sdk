{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Scoped JSONL subprocess plumbing and pure Droid launch configuration.
-- Callers own protocol parsing and dispatch; credentials belong in the
-- environment, never command arguments. Standard scopes discard stderr;
-- explicit routing exposes raw data. Errors omit frames and process settings.
module Factory.Droid.Transport.Process
  ( JsonLinesProcess,
    JsonLinesError (..),
    IpcProcessOptions (..),
    defaultIpcProcessOptions,
    DroidProcessMode (..),
    DroidLaunchOptions (..),
    defaultDroidLaunchOptions,
    droidProcess,
    prepareDroidProcess,
    proc,
    withJsonLinesProcess,
    withObservedJsonLinesProcess,
    withJsonLinesProcessStderr,
    withJsonLinesProcessIpc,
    sendObject,
    receiveObject,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (Exception, IOException, bracket, catch, finally, mask, mask_, onException, throwIO, uninterruptibleMask_)
import Control.Monad (unless, void, when)
import Data.Aeson (Object, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable (cast)
import Factory.Droid.Internal.Exception (finallyPreserving)
import Factory.Droid.Observability qualified as Obs
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.FD qualified as FD
import GHC.IO.Handle.FD qualified as Handle
import GHC.IO.Handle.Internals (wantWritableHandle, withAllHandles__)
import GHC.IO.Handle.Types (Handle__ (..))
import System.Directory (findExecutable)
import System.Environment (getEnvironment)
import System.IO (Handle, IOMode (ReadMode, ReadWriteMode, WriteMode), hClose, hFlush, hSetBinaryMode, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO qualified as Posix
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (Fd (..))
import System.Process (CmdSpec (..), CreateProcess (child_group, child_user, close_fds, cmdspec, create_group, cwd, delegate_ctlc, env, new_session, std_err, std_in, std_out), ProcessHandle, StdStream (..), createProcess_, getPid, getProcessExitCode, proc, terminateProcess, waitForProcess)
import System.Process.Internals (ignoreSigPipe, mkProcessHandle, withCEnvironment)
import System.Timeout (timeout)

-- | ACP exposes raw JSONL stdio; ordinary Factory sessions use StreamJsonRpc.
data DroidProcessMode = Acp | StreamJsonRpc deriving stock (Eq, Show)

-- | Launch data is explicit; trusted values are merged after sanitization.
-- An explicit empty argument replacement differs from the default mode flags.
data DroidLaunchOptions = DroidLaunchOptions
  { launchPrefixArguments :: ![String],
    launchArguments :: !(Maybe [String]),
    launchExtraArguments :: ![String],
    launchEnvironment :: !(Map String String),
    launchTrustedEnvironment :: !(Map String String)
  }
  deriving stock (Eq)

instance Show DroidLaunchOptions where show _ = "DroidLaunchOptions <redacted>"

defaultDroidLaunchOptions :: DroidLaunchOptions
defaultDroidLaunchOptions = DroidLaunchOptions [] Nothing [] mempty mempty

-- | Construct, but do not start, a Droid command without a shell. Standard
-- CreateProcess fields supply cwd/environment; no sanitization or discovery is
-- performed here. Unrelated descriptors are closed by default, without changing
-- explicit stdio routing. Use withJsonLinesProcess to own pipes and cleanup.
droidProcess :: FilePath -> DroidProcessMode -> CreateProcess
droidProcess executable mode = (proc executable (droidArguments mode)) {close_fds = True}

-- | Merge inherited values and ordinary overrides, apply the supplied sanitizer,
-- then merge trusted overrides. Environment values never become arguments.
-- Prefix arguments belong to the explicitly selected executable. Descriptor
-- isolation follows 'droidProcess', including when arguments are replaced.
prepareDroidProcess :: FilePath -> DroidProcessMode -> DroidLaunchOptions -> [(String, String)] -> (Map String String -> Map String String) -> CreateProcess
prepareDroidProcess executable mode options inherited sanitize =
  (droidProcess executable mode) {cmdspec = RawCommand executable arguments, env = Just (Map.toList environment)}
  where
    arguments = launchPrefixArguments options <> fromMaybe (droidArguments mode) (launchArguments options) <> launchExtraArguments options
    environment = Map.union (launchTrustedEnvironment options) (sanitize (Map.union (launchEnvironment options) (Map.fromList inherited)))

droidArguments :: DroidProcessMode -> [String]
droidArguments = \case
  Acp -> ["exec", "--output-format", "acp"]
  StreamJsonRpc -> ["exec", "--input-format", "stream-jsonrpc", "--output-format", "stream-jsonrpc"]

-- | The trusted C launcher execs the selected program under the
-- same PID. The budget covers startup, not the user's process scope.
data IpcProcessOptions = IpcProcessOptions
  { ipcProcessLauncher :: !FilePath,
    ipcProcessStartupTimeoutMicros :: !Int
  }
  deriving stock (Eq)

instance Show IpcProcessOptions where show _ = "IpcProcessOptions <redacted>"

defaultIpcProcessOptions :: IpcProcessOptions
defaultIpcProcessOptions = IpcProcessOptions "factory-droid-launcher" 5000000

-- | Payload-free transport failures. EOF is always visible to the caller;
-- an unterminated final frame is not accepted as a complete protocol message.
data JsonLinesError = InvalidFrameLimit | FrameTooLarge | InvalidJsonObject | EndOfStream | TruncatedFrame | ProcessStartFailure | ProcessSetupFailure | ProcessReadFailure | ProcessWriteFailure | ProcessCleanupFailure | InvalidIpcProcessOptions | IpcLauncherUnavailable | IpcStartupTimedOut
  deriving stock (Eq, Show)

instance Exception JsonLinesError

-- | Valid only inside its process scope. Sends and receives are serialized
-- independently; cancellation or a framing/I/O error must end the exchange.
-- Callers must finish their send/receive threads before leaving the callback.
data JsonLinesProcess = JsonLinesProcess !Int !Handle !Handle !(MVar ()) !(MVar BS.ByteString)

-- | Open a channel with an explicit positive byte limit per frame, excluding
-- the newline. Stream settings are replaced by owned pipes and a null stderr
-- sink; the executable, arguments, environment and cwd are otherwise unchanged.
-- The supplied close_fds policy is preserved; Droid command builders enable it.
-- Cleanup requests SIGTERM and allows the specified grace period before SIGKILL
-- and reaping, then closes the pipes. Nonpositive grace skips the wait. Only the
-- owned child is signalled; descendants are not managed. Reaping after SIGKILL
-- defers asynchronous exceptions and still depends on the OS completing exit.
withJsonLinesProcess ::
  -- | Maximum bytes per frame, excluding the newline.
  Int ->
  -- | Shutdown grace in microseconds.
  Int ->
  CreateProcess ->
  (JsonLinesProcess -> IO a) ->
  IO a
withJsonLinesProcess limit grace config action = do
  when (limit <= 0) (throwIO InvalidFrameLimit)
  withBinaryFile "/dev/null" WriteMode $ \sink ->
    withJsonLinesProcessStderr limit grace (config {std_err = UseHandle sink}) (\channel _ -> action channel)

-- | Own ordinary JSONL stdio and an additional Node-compatible JSON IPC
-- channel. The callback receives stdio, IPC and optional captured stderr.
-- Stdin/stdout are owned pipes; stderr follows CreateProcess, as in
-- withJsonLinesProcessStderr. Unrelated descriptors are always closed.
-- Raw commands, cwd/environment and process-group/session flags are supported;
-- user/group changes and delegated terminal control are rejected.
-- Finish all channel/stderr I/O before returning. Cleanup uses the same PID
-- owner as ordinary processes, including timeout and partial startup failure.
-- The deadline covers preparation and handoff, not the callback; delivery can
-- still wait for an OS spawn/filesystem call, and cleanup must finish reaping.
withJsonLinesProcessIpc :: Int -> Int -> IpcProcessOptions -> CreateProcess -> (JsonLinesProcess -> JsonLinesProcess -> Maybe Handle -> IO a) -> IO a
withJsonLinesProcessIpc limit grace options config action = do
  when (limit <= 0) (throwIO InvalidFrameLimit)
  let budget = ipcProcessStartupTimeoutMicros options
  when (budget < 0 || null (ipcProcessLauncher options) || delegate_ctlc config || isJust (child_user config) || isJust (child_group config)) (throwIO InvalidIpcProcessOptions)
  when (budget == 0) (throwIO IpcStartupTimedOut)
  handles <- newIORef []
  owner <- newIORef Nothing
  mask $ \restore -> do
    let release = readIORef owner >>= mapM_ (\(state, process) -> mapM_ (\p -> readIORef handles >>= cleanupHandles grace p) process `finally` cIpcFree state)
        begin = ioBoundary ProcessStartFailure $ do
          (executable, arguments) <- case cmdspec config of
            RawCommand path values -> pure (path, values)
            ShellCommand _ -> throwIO InvalidIpcProcessOptions
          environment <- maybe getEnvironment pure (env config)
          let strings = ipcProcessLauncher options : executable : arguments <> maybe [] pure (cwd config) <> concatMap (\(key, value) -> [key, value]) environment
          when (null executable || any (elem '\0') strings || any (\(key, _) -> null key || '=' `elem` key) environment) (throwIO InvalidIpcProcessOptions)
          launcher <- findExecutable (ipcProcessLauncher options) >>= maybe (throwIO IpcLauncherUnavailable) pure
          encoding <- getFileSystemEncoding
          let stderrMode = case std_err config of CreatePipe -> "pipe"; NoStream -> "closed"; _ -> "inherit"
              argv = [launcher, "--droid-ipc", stderrMode, maybe "inherit-cwd" (const "cwd") (cwd config), fromMaybe "" (cwd config), executable] <> arguments
              flags = fromIntegral (fromEnum (create_group config) + 2 * fromEnum (new_session config))
          withIpcStderr (std_err config) $ \stderrFd ->
            Foreign.withCString encoding launcher $ \program ->
              withMany (Foreign.withCString encoding) argv $ \values ->
                withArray0 nullPtr values $ \argvPtr ->
                  withCEnvironment environment $ \envPtr ->
                    alloca $ \result -> mask_ $ do
                      status <- cIpcBegin program argvPtr envPtr stderrFd flags result
                      when (status /= 0) (throwIO ProcessStartFailure)
                      state <- peek result
                      -- Ownership outlives timeout's result/exception handoff.
                      writeIORef owner (Just (state, Nothing))
                      process <- cIpcPid state >>= \pid -> mkProcessHandle (fromIntegral pid) False
                      cIpcAdopt state
                      writeIORef owner (Just (state, Just process))
                      pure (state, process)
    finallyPreserving
      ( do
          ready <- timeout budget $ do
            (state, process) <- restore begin
            restore $ do
              awaitIpcStartup state process
              input <- ipcHandle state handles 0 WriteMode
              output <- ipcHandle state handles 1 ReadMode
              errors <- case std_err config of CreatePipe -> Just <$> ipcHandle state handles 2 ReadMode; _ -> pure Nothing
              ipc <- ipcHandle state handles 3 ReadWriteMode
              stdio <- JsonLinesProcess limit input output <$> newMVar () <*> newMVar BS.empty
              channel <- JsonLinesProcess limit ipc ipc <$> newMVar () <*> newMVar BS.empty
              pure (stdio, channel, errors)
          case ready of
            Nothing -> throwIO IpcStartupTimedOut
            Just (stdio, channel, errors) -> restore (action stdio channel errors)
      )
      release

withIpcStderr :: StdStream -> (CInt -> IO a) -> IO a
withIpcStderr stream action = case stream of
  NoStream -> action (-1)
  Inherit -> action (-2)
  CreatePipe -> action (-3)
  UseHandle handle -> do
    hFlush handle
    -- External fd users expect blocking I/O; keep both duplex halves in sync.
    withAllHandles__ "Droid IPC stderr" handle $ \Handle__ {haDevice = device, ..} ->
      case cast device of
        Just descriptor -> do
          blocking <- FD.setNonBlockingMode descriptor False
          pure Handle__ {haDevice = blocking, ..}
        Nothing -> throwIO InvalidIpcProcessOptions
    wantWritableHandle "Droid IPC stderr" handle $ \Handle__ {haDevice = device} ->
      case cast device of
        Just descriptor -> action (FD.fdFD descriptor)
        Nothing -> throwIO InvalidIpcProcessOptions

awaitIpcStartup :: Ptr IpcSpawn -> ProcessHandle -> IO ()
awaitIpcStartup state process = do
  status <- cIpcPoll state
  if status == 1
    then pure ()
    else do
      when (status < 0) (throwIO ProcessStartFailure)
      getProcessExitCode process >>= \case
        Just _ -> cIpcPoll state >>= \final -> unless (final == 1) (throwIO ProcessStartFailure)
        Nothing -> threadDelay 1000 >> awaitIpcStartup state process

ipcHandle :: Ptr IpcSpawn -> IORef [Handle] -> CInt -> IOMode -> IO Handle
ipcHandle state handles index mode = mask_ $ do
  fd <- cIpcTakeFd state index
  when (fd < 0) (throwIO ProcessSetupFailure)
  handle <-
    ioBoundary ProcessSetupFailure (Posix.setFdOption (Fd fd) Posix.NonBlockingRead True >> Handle.fdToHandle' fd Nothing True "Droid process channel" mode True)
      `onException` Posix.closeFd (Fd fd)
  modifyIORef' handles (handle :)
  pure handle

data IpcSpawn

foreign import ccall safe "droid_ipc_begin" cIpcBegin :: CString -> Ptr CString -> Ptr CString -> CInt -> CInt -> Ptr (Ptr IpcSpawn) -> IO CInt

foreign import ccall unsafe "droid_ipc_pid" cIpcPid :: Ptr IpcSpawn -> IO CInt

foreign import ccall unsafe "droid_ipc_adopt" cIpcAdopt :: Ptr IpcSpawn -> IO ()

foreign import ccall unsafe "droid_ipc_poll" cIpcPoll :: Ptr IpcSpawn -> IO CInt

foreign import ccall unsafe "droid_ipc_take_fd" cIpcTakeFd :: Ptr IpcSpawn -> CInt -> IO CInt

foreign import ccall safe "droid_ipc_free" cIpcFree :: Ptr IpcSpawn -> IO ()

-- | Observe owned spawn/setup and the complete process scope. No executable,
-- arguments, environment, stderr or message payload is copied into telemetry.
withObservedJsonLinesProcess :: Obs.DroidObservability -> Int -> Int -> CreateProcess -> (JsonLinesProcess -> IO a) -> IO a
withObservedJsonLinesProcess observability limit grace config action
  | Nothing <- Obs.observabilityLogger observability, Nothing <- Obs.observabilityMetrics observability = withJsonLinesProcess limit grace config action
  | otherwise = do
      when (limit <= 0) (throwIO InvalidFrameLimit)
      Obs.observeDroidOperation observability "droid.process" Nothing $ do
        started <- getMonotonicTimeNSec
        withJsonLinesProcess limit grace config $ \channel -> do
          ready <- getMonotonicTimeNSec
          void (Obs.recordDroidMetric (Obs.observabilityMetrics observability) (Obs.DroidMetricEvent "droid.process.startup" Obs.MetricHistogram (fromInteger (toInteger ready - toInteger started) / 1000000) Obs.MetricMilliseconds Nothing))
          void (Obs.emitDroidLog (Obs.observabilityLogger observability) (Obs.DroidLogEvent Obs.LogInfo "droid.process.started" "Owned process started" Nothing Nothing))
          action channel

-- | Opt into the configured stderr routing. CreatePipe yields an owned binary
-- handle; other StdStream choices yield Nothing. UseHandle remains borrowed.
-- The caller drains and joins any stderr reader within this scope. Raw stderr
-- is not bounded, decoded or redacted here. Stdin/stdout remain owned pipes.
withJsonLinesProcessStderr :: Int -> Int -> CreateProcess -> (JsonLinesProcess -> Maybe Handle -> IO a) -> IO a
withJsonLinesProcessStderr limit grace config action = do
  when (limit <= 0) (throwIO InvalidFrameLimit)
  let configured = config {std_in = CreatePipe, std_out = CreatePipe}
  bracket (ioBoundary ProcessStartFailure (createProcess_ "JSONL process" configured)) (cleanup grace) $ \(mInput, mOutput, errors, _) -> do
    input <- maybe (throwIO ProcessSetupFailure) pure mInput
    output <- maybe (throwIO ProcessSetupFailure) pure mOutput
    ioBoundary ProcessSetupFailure $ hSetBinaryMode input True >> hSetBinaryMode output True >> mapM_ (`hSetBinaryMode` True) errors
    writer <- newMVar ()
    reader <- newMVar BS.empty
    action (JsonLinesProcess limit input output writer reader) errors

-- Stop before hClose: cancelled writes can leave a buffered flush blocked on stdin.
cleanup :: Int -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanup grace (input, output, errors, process) = cleanupHandles grace process (foldMap (maybe [] pure) [input, output, errors])

cleanupHandles :: Int -> ProcessHandle -> [Handle] -> IO ()
cleanupHandles grace process handles =
  ioBoundary ProcessCleanupFailure $
    stopOwnedProcess grace process
      `finally` foldr finally (pure ()) [ignoreSigPipe (hClose handle) | handle <- handles]

stopOwnedProcess :: Int -> ProcessHandle -> IO ()
stopOwnedProcess grace process =
  (terminateProcess process >> when (grace > 0) (void (timeout grace awaitExit)))
    `finally` uninterruptibleMask_
      ( do
          getPid process >>= mapM_ kill
          void (waitForProcess process)
      )
  where
    -- Polling may be interrupted; cancelling waitForProcess can lose reaped status.
    awaitExit = getProcessExitCode process >>= maybe (threadDelay 10000 >> awaitExit) (const (pure ()))
    -- No other thread can reap this child between obtaining its PID and signalling.
    kill pid = signalProcess sigKILL pid `catch` \err -> unless (isDoesNotExistError err) (throwIO err)

-- | Write and flush one UTF-8 JSON object atomically with respect to other
-- sends on this channel. No protocol/version or attribution fields are added.
sendObject :: JsonLinesProcess -> Object -> IO ()
sendObject (JsonLinesProcess limit input _ writer _) value = withMVar writer $ \() -> do
  let bytes = encode value
  when (BL.length bytes > fromIntegral limit) (throwIO FrameTooLarge)
  ioBoundary ProcessWriteFailure $ BL.hPutStr input (bytes <> "\n") >> hFlush input

-- | Read one complete JSON object, retaining subsequent bytes for the next
-- call. Partial reads and UTF-8 splits are handled before JSON decoding.
-- Malformed JSON, non-object JSON and blank lines fail rather than being skipped.
receiveObject :: JsonLinesProcess -> IO Object
receiveObject (JsonLinesProcess limit _ output _ reader) = modifyMVar reader $ \buffer -> do
  (line, rest) <- ioBoundary ProcessReadFailure (readFrame limit output buffer)
  value <- either (const (throwIO InvalidJsonObject)) pure (eitherDecodeStrict' line)
  pure (rest, value)

readFrame :: Int -> Handle -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
readFrame limit handle = go [] 0
  where
    go chunks used buffer = do
      let (part, suffix) = BS.break (== 10) buffer
          size = BS.length part
      when (size > limit - used) (throwIO FrameTooLarge)
      if BS.null suffix
        then do
          let total = used + size
          next <- BS.hGetSome handle (min 32768 (limit - total) + 1)
          if BS.null next
            then throwIO (if total == 0 then EndOfStream else TruncatedFrame)
            else go (part : chunks) total next
        else pure (BS.concat (reverse (part : chunks)), BS.drop 1 suffix)

ioBoundary :: JsonLinesError -> IO a -> IO a
ioBoundary failure action = action `catch` \(_ :: IOException) -> throwIO failure
