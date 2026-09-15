module Factory.Droid.Internal.Wait (checkAbort, waitWithAbort) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, orElse)
import Control.Exception (SomeException, throwIO)
import Control.Monad (forM_)

checkAbort :: Maybe (STM SomeException) -> IO ()
checkAbort Nothing = pure ()
checkAbort (Just reason) = do
  pending <- atomically ((Just <$> reason) `orElse` pure Nothing)
  forM_ pending throwIO

-- Callers validate the microsecond range before entering this shared wait.
waitWithAbort :: Maybe (STM SomeException) -> Int -> IO ()
waitWithAbort abort micros = do
  checkAbort abort
  if micros == 0
    then pure ()
    else case abort of
      Nothing -> threadDelay micros
      Just reason -> race (atomically reason) (threadDelay micros) >>= either throwIO pure
