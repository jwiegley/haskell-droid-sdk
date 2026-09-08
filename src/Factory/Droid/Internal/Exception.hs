module Factory.Droid.Internal.Exception (trySync) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)

-- | Isolate ordinary callback failures without swallowing asynchronous cancellation.
trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try action
  case result of
    Left err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
    _ -> pure result
