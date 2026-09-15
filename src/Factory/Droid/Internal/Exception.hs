module Factory.Droid.Internal.Exception (trySync, finallyPreserving) where

import Control.Exception (SomeAsyncException, SomeException, fromException, mask, throwIO, try)

-- | Isolate ordinary callback failures without swallowing asynchronous cancellation.
trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try action
  case result of
    Left err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
    _ -> pure result

-- | Attempt cleanup without replacing an exception already raised by the action.
finallyPreserving :: IO a -> IO () -> IO a
finallyPreserving action cleanup = mask $ \restore -> do
  result <- try @SomeException (restore action)
  released <- try @SomeException cleanup
  case result of
    Left cause -> throwIO cause
    Right value -> either throwIO (const (pure value)) released
