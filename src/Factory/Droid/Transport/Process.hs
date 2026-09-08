{-# LANGUAGE OverloadedStrings #-}

-- | A scoped JSONL subprocess channel. This is transport plumbing, not a Droid
-- launcher or request dispatcher. Callers supply an executable configuration;
-- credentials belong in the environment, never command arguments. Stderr is
-- discarded, and transport errors do not contain frames or process settings.
module Factory.Droid.Transport.Process
  ( JsonLinesProcess,
    JsonLinesError (..),
    proc,
    withJsonLinesProcess,
    sendObject,
    receiveObject,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (Exception, IOException, bracket, catch, finally, throwIO, uninterruptibleMask_)
import Control.Monad (unless, void, when)
import Data.Aeson (Object, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hSetBinaryMode, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess_, getPid, getProcessExitCode, proc, terminateProcess, waitForProcess)
import System.Process.Internals (ignoreSigPipe)
import System.Timeout (timeout)

-- | Payload-free transport failures. EOF is always visible to the caller;
-- an unterminated final frame is not accepted as a complete protocol message.
data JsonLinesError = InvalidFrameLimit | FrameTooLarge | InvalidJsonObject | EndOfStream | TruncatedFrame | ProcessStartFailure | ProcessSetupFailure | ProcessReadFailure | ProcessWriteFailure | ProcessCleanupFailure
  deriving stock (Eq, Show)

instance Exception JsonLinesError

-- | Valid only inside withJsonLinesProcess. Sends and receives are serialized
-- independently; cancellation or a framing/I/O error must end the exchange.
-- Callers must finish their send/receive threads before leaving the callback.
data JsonLinesProcess = JsonLinesProcess !Int !Handle !Handle !(MVar ()) !(MVar BS.ByteString)

-- | Open a channel with an explicit positive byte limit per frame, excluding
-- the newline. Stream settings are replaced by owned pipes and a null stderr
-- sink; the executable, arguments, environment and cwd are otherwise unchanged.
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
  withBinaryFile "/dev/null" WriteMode $ \sink -> do
    let configured = config {std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle sink}
    bracket (ioBoundary ProcessStartFailure (createProcess_ "JSONL process" configured)) (cleanup grace) $ \(mInput, mOutput, _, _) -> do
      input <- maybe (throwIO ProcessSetupFailure) pure mInput
      output <- maybe (throwIO ProcessSetupFailure) pure mOutput
      ioBoundary ProcessSetupFailure $ hSetBinaryMode input True >> hSetBinaryMode output True
      writer <- newMVar ()
      reader <- newMVar BS.empty
      action (JsonLinesProcess limit input output writer reader)

-- Stop before hClose: cancelled writes can leave a buffered flush blocked on stdin.
cleanup :: Int -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanup grace (input, output, errors, process) =
  ioBoundary ProcessCleanupFailure $
    stopOwnedProcess grace process
      `finally` foldr finally (pure ()) [ignoreSigPipe (mapM_ hClose input), mapM_ hClose output, mapM_ hClose errors]

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
