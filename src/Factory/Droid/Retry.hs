-- | Explicit retries of replay-safe work and scoped connection acquisition.
-- Millisecond schedules follow the SDK policies; no active session, callback,
-- queue or uncertain request is automatically replayed.
module Factory.Droid.Retry
  ( RetryDelay (..),
    RetryJitter (..),
    RetryPolicy (..),
    RetryOptions (..),
    RetryError (..),
    ReconnectOwner (..),
    defaultRetryOptions,
    webSocketRetryOptions,
    connectionRetryOptions,
    reconnectionRetryOptions,
    retryDelayMillis,
    retry,
    withConnectionRetries,
    withReconnection,
  )
where

import Control.Concurrent.STM (STM)
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad (forM_, when)
import Crypto.Random (getRandomBytes)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Factory.Droid.Internal.Exception (trySync)
import Factory.Droid.Internal.Wait qualified as Wait

-- | The exponential value is the /first actual delay/, not an implicit
-- exponent offset. A cap applies before jitter, as in the source strategies.
data RetryDelay = FixedDelay !Double | ExponentialBackoff !Double !Double !(Maybe Double) deriving stock (Eq, Show)

data RetryJitter = NoJitter | HalfToFullJitter | PositiveReconnectJitter deriving stock (Eq, Show)

data RetryPolicy = RetryPolicy
  { retryMaxAttempts :: !Int,
    retryDelay :: !RetryDelay,
    retryJitter :: !RetryJitter,
    retryDelayBeforeFirst :: !Bool
  }
  deriving stock (Eq, Show)

-- | Abort is a wait-only STM action yielding the caller's original reason.
-- It is checked before attempts/publication and interrupts backoff, not an
-- in-flight action. Ordinary Haskell asynchronous cancellation remains available.
-- Success-hook failures and post-publication failures never become retries.
data RetryOptions a = RetryOptions
  { retryPolicy :: !RetryPolicy,
    retryPredicate :: !(SomeException -> IO Bool),
    retryOnRetry :: !(Maybe (SomeException -> Int -> IO ())),
    retryOnSuccess :: !(Maybe (Int -> IO ())),
    retryOnAllError :: !(Maybe (SomeException -> IO a)),
    retryGetDelay :: !(Maybe (SomeException -> Int -> Double -> IO Double)),
    retryAbort :: !(Maybe (STM SomeException))
  }

instance Show (RetryOptions a) where show _ = "RetryOptions <callbacks redacted>"

data RetryError = InvalidRetryPolicy | InvalidRetryDelay | InvalidRetryJitterSample deriving stock (Eq, Show)

instance Exception RetryError

data ReconnectOwner = ReconnectLocally | ReconnectDelegated deriving stock (Eq, Show)

defaultRetryOptions :: RetryOptions a
defaultRetryOptions = RetryOptions (RetryPolicy 3 (FixedDelay 250) NoJitter False) (const (pure True)) Nothing Nothing Nothing Nothing Nothing

webSocketRetryOptions :: RetryOptions a
webSocketRetryOptions = defaultRetryOptions {retryPolicy = RetryPolicy 6 (ExponentialBackoff 500 2 (Just 5000)) NoJitter False}

connectionRetryOptions :: RetryOptions a
connectionRetryOptions = defaultRetryOptions {retryPolicy = RetryPolicy 11 (FixedDelay 2000) NoJitter False}

reconnectionRetryOptions :: RetryOptions a
reconnectionRetryOptions = defaultRetryOptions {retryPolicy = RetryPolicy 3 (ExponentialBackoff 1000 1.5 (Just 10000)) PositiveReconnectJitter True}

-- | Zero-based delay index and a sample in [0,1). Delays are rounded down to
-- whole milliseconds and checked before conversion to the native timer range.
retryDelayMillis :: RetryPolicy -> Int -> Double -> Either RetryError Int
retryDelayMillis policy index sample = do
  validatePolicy policy
  base <- baseDelay policy index
  delayed <- applyJitter (retryJitter policy) sample base
  normalizeDelay delayed

validatePolicy :: RetryPolicy -> Either RetryError ()
validatePolicy policy
  | retryMaxAttempts policy < 1 = Left InvalidRetryPolicy
  | otherwise = case retryDelay policy of
      FixedDelay delay | nonnegative delay -> pure ()
      ExponentialBackoff first factor cap | nonnegative first && nonnegative factor && maybe True nonnegative cap -> pure ()
      _ -> Left InvalidRetryPolicy

nonnegative :: Double -> Bool
nonnegative value = value >= 0 && not (isInfinite value) && not (isNaN value)

baseDelay :: RetryPolicy -> Int -> Either RetryError Double
baseDelay policy index
  | index < 0 = Left InvalidRetryDelay
  | otherwise = pure $ case retryDelay policy of
      FixedDelay delay -> delay
      ExponentialBackoff first factor cap ->
        let value = if first == 0 then 0 else first * factor ** fromIntegral index
         in maybe value (`min` value) cap

applyJitter :: RetryJitter -> Double -> Double -> Either RetryError Double
applyJitter mode sample base
  | mode /= NoJitter && (sample < 0 || sample >= 1 || isNaN sample) = Left InvalidRetryJitterSample
  | otherwise = pure $ case mode of
      HalfToFullJitter -> base * (0.5 + sample * 0.5)
      PositiveReconnectJitter -> base + sample * 0.3 * base
      NoJitter -> base

normalizeDelay :: Double -> Either RetryError Int
normalizeDelay value
  | not (nonnegative value) = Left InvalidRetryDelay
  | milliseconds > toInteger (maxBound :: Int) `div` 1000 = Left InvalidRetryDelay
  | otherwise = pure (fromInteger milliseconds)
  where
    milliseconds = floor value :: Integer

-- | Explicitly select only replay-safe actions. Hook failures propagate;
-- onAllError is an explicit caller recovery, never a hidden default fallback.
retry :: RetryOptions a -> IO a -> IO a
retry options action = runAttempts options $ \publish -> do
  value <- action
  publish
  pure value

-- | The factory must perform only connection setup/authentication before its
-- callback, never initialize a session or submit application work. Existing
-- withConnection, withWebSocket and relay scopes satisfy this boundary.
-- All acquired resources are released before another attempt begins. Once the
-- callback is entered, its errors and subsequent cleanup errors are not retried.
withConnectionRetries :: RetryOptions a -> ((connection -> IO a) -> IO a) -> (connection -> IO a) -> IO a
withConnectionRetries options acquire action = runAttempts options $ \publish -> acquire $ \connection -> publish >> action connection

-- | Delegated recovery invokes only the external factory, once, without a
-- local retry schedule. The external owner supplies its own polling/bounds.
-- Select ownership explicitly, never from locality or a machine-name string.
withReconnection :: RetryOptions a -> ReconnectOwner -> ((connection -> IO a) -> IO a) -> ((connection -> IO a) -> IO a) -> (connection -> IO a) -> IO a
withReconnection options owner local delegated = case owner of
  ReconnectLocally -> withConnectionRetries options local
  ReconnectDelegated -> withConnectionRetries (options {retryPolicy = RetryPolicy 1 (FixedDelay 0) NoJitter False}) delegated

runAttempts :: RetryOptions a -> (IO () -> IO a) -> IO a
runAttempts options action = do
  either throwIO pure (validatePolicy policy)
  checkAbort options
  when (retryDelayBeforeFirst policy) (waitDelay Nothing 0 0)
  attempt 0
  where
    policy = retryPolicy options
    attempt failed = do
      checkAbort options
      published <- newIORef False
      let publish = do
            checkAbort options
            writeIORef published True
            forM_ (retryOnSuccess options) ($ failed)
      result <- trySync (action publish)
      case result of
        Right value -> pure value
        Left cause -> do
          entered <- readIORef published
          if entered
            then throwIO cause
            else do
              checkAbort options
              allowed <- retryPredicate options cause
              let failures = failed + 1
              when allowed $ forM_ (retryOnRetry options) (\notify -> notify cause failures)
              if allowed && failures < retryMaxAttempts policy
                then do
                  let index = if retryDelayBeforeFirst policy then failures else failures - 1
                  waitDelay (Just cause) failures index
                  attempt failures
                else do
                  checkAbort options
                  maybe (throwIO cause) ($ cause) (retryOnAllError options)
    waitDelay cause failures index = do
      checkAbort options
      base <- either throwIO pure (baseDelay policy index)
      delayed <- case (cause, retryGetDelay options) of
        (Just failure, Just override) -> override failure failures base
        _ -> do
          sample <- if retryJitter policy == NoJitter then pure 0 else jitterSample
          either throwIO pure (applyJitter (retryJitter policy) sample base)
      milliseconds <- either throwIO pure (normalizeDelay delayed)
      waitBackoff options milliseconds

checkAbort :: RetryOptions a -> IO ()
checkAbort options = Wait.checkAbort (retryAbort options)

waitBackoff :: RetryOptions a -> Int -> IO ()
waitBackoff options milliseconds = Wait.waitWithAbort (retryAbort options) (milliseconds * 1000)

jitterSample :: IO Double
jitterSample = do
  bytes <- getRandomBytes 8 :: IO BS.ByteString
  let value = BS.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) (0 :: Word64) bytes
  pure (fromIntegral (value `shiftR` 11) / 9007199254740992)
