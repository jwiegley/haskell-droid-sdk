{-# LANGUAGE OverloadedStrings #-}

-- | Bounded native JSON Schema validation. Each job owns one worker process;
-- no validator state or uncertain request is reused after timeout/cancellation.
module Factory.Droid.MCP.Validator
  ( SchemaValidatorOptions (..),
    defaultSchemaValidatorOptions,
    SchemaValidatorError (..),
    validateSchemaDefinition,
    validateSchemaValue,
  )
where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (unless, when)
import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Factory.Droid.Transport.Process qualified as Process
import System.Directory (findExecutable)
import System.Process (CreateProcess (close_fds, env, std_err), StdStream (NoStream), proc)
import System.Timeout (timeout)

data SchemaValidatorOptions = SchemaValidatorOptions
  { schemaValidatorExecutable :: !FilePath,
    schemaValidatorTimeoutMicros :: !Int
  }
  deriving stock (Eq)

instance Show SchemaValidatorOptions where show _ = "SchemaValidatorOptions <redacted>"

defaultSchemaValidatorOptions :: SchemaValidatorOptions
defaultSchemaValidatorOptions = SchemaValidatorOptions "factory-droid-validator" 30000000

data SchemaValidatorError
  = InvalidValidatorOptions
  | ValidatorUnavailable
  | ValidatorTimedOut
  | ValidatorRequestTooLarge
  | InvalidSchemaDefinition
  | InvalidValidatorResponse
  | ValidatorProcessFailure
  deriving stock (Eq, Show)

instance Exception SchemaValidatorError

validateSchemaDefinition :: SchemaValidatorOptions -> Value -> IO ()
validateSchemaDefinition options schema = do
  valid <- runValidation options schema Nothing
  unless valid (throwIO InvalidSchemaDefinition)

validateSchemaValue :: SchemaValidatorOptions -> Value -> Value -> IO Bool
validateSchemaValue options schema value = runValidation options schema (Just value)

runValidation :: SchemaValidatorOptions -> Value -> Maybe Value -> IO Bool
runValidation options schema value = do
  let budget = schemaValidatorTimeoutMicros options
      executable = schemaValidatorExecutable options
      request = KeyMap.fromList ([("protocol", Number 1), ("schema", schema)] <> maybe [] (\instanceValue -> [("value", instanceValue)]) value)
      frameLimit = 10 * 1024 * 1024 - 1
  when (budget < 0 || null executable || '\0' `elem` executable) (throwIO InvalidValidatorOptions)
  when (budget == 0) (throwIO ValidatorTimedOut)
  completed <- timeout budget $ do
    when (LBS.length (encode (Object request)) > fromIntegral frameLimit) (throwIO ValidatorRequestTooLarge)
    worker <- findExecutable executable >>= maybe (throwIO ValidatorUnavailable) pure
    let process = (proc worker []) {env = Just [], close_fds = True, std_err = NoStream}
    result <- try @Process.JsonLinesError $
      Process.withJsonLinesProcessStderr frameLimit 0 process $ \channel _ -> do
        Process.sendObject channel request
        Process.receiveObject channel
    fields <- either (const (throwIO ValidatorProcessFailure)) pure result
    when (KeyMap.lookup "protocol" fields /= Just (Number 1)) (throwIO InvalidValidatorResponse)
    case KeyMap.lookup "status" fields of
      Just (String "valid") -> pure True
      Just (String "invalid") -> pure False
      Just (String "invalid_schema") -> throwIO InvalidSchemaDefinition
      Just (String "request_too_large") -> throwIO ValidatorRequestTooLarge
      Just (String "internal_error") -> throwIO ValidatorProcessFailure
      _ -> throwIO InvalidValidatorResponse
  maybe (throwIO ValidatorTimedOut) pure completed
