{-# LANGUAGE OverloadedStrings #-}

module ValidatorSpec (validatorTests, withPidWorker, workerIdentifier) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Exception (bracket, fromException, try)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import DroidSpec (assertReaped)
import Factory.Droid.MCP.Validator
import GHC.Clock (getMonotonicTimeNSec)
import ProcessSpec (bounded)
import System.Directory (doesFileExist, findExecutable, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Posix.Temp (mkdtemp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

validatorTests :: TestTree
validatorTests =
  testGroup
    "Isolated native validator"
    [ testCase "schema compilation and instance invalidity are distinct" $ do
        validateSchemaDefinition options (object ["type" .= String "integer"])
        validateSchemaValue options (object ["type" .= String "integer"]) (Number 1) >>= (@?= True)
        validateSchemaValue options (object ["type" .= String "integer"]) Null >>= (@?= False)
        try @SchemaValidatorError (validateSchemaDefinition options (object ["type" .= String "unknown"])) >>= (@?= Left InvalidSchemaDefinition),
      testCase "exact decimal, large numbers, regex and evaluated-property semantics work through the native worker" $ do
        forM_
          [ ("{\"const\":0.100000000000000000001}", "0.100000000000000000001", "0.1"),
            ("{\"type\":\"integer\",\"minimum\":18446744073709551617}", "18446744073709551617", "18446744073709551616"),
            ("{\"type\":\"string\",\"pattern\":\"(?<=a)b\"}", "\"ab\"", "\"cb\""),
            ("{\"type\":\"string\",\"pattern\":\"b$\"}", "\"a\\u0000b\"", "\"a\\u0000c\""),
            ("{\"allOf\":[{\"properties\":{\"v\":{\"type\":\"integer\"}}}],\"unevaluatedProperties\":false}", "{\"v\":1}", "{\"v\":1,\"other\":false}"),
            ("{\"type\":\"number\"}", "1", "{\"$serde_json::private::Number\":\"1\"}")
          ]
          $ \(rawSchema, rawGood, rawBad) -> do
            schema <- json rawSchema
            good <- json rawGood
            bad <- json rawBad
            validateSchemaValue options schema good >>= (@?= True)
            validateSchemaValue options schema bad >>= (@?= False),
      testCase "escaped and percent-encoded local references remain local and exact" $ do
        schema <- json "{\"$defs\":{\"~1\":{\"type\":\"integer\"},\"/\":{\"type\":\"string\"},\"sp ace\":{\"const\":3}},\"properties\":{\"v\":{\"$ref\":\"#/$defs/~01\"},\"x\":{\"$ref\":\"#/$defs/sp%20ace\"}}}"
        validateSchemaValue options schema (object ["v" .= Number 1, "x" .= Number 3]) >>= (@?= True)
        validateSchemaValue options schema (object ["v" .= String "wrong", "x" .= Number 3]) >>= (@?= False),
      testCase "external references are rejected without retrieval" $
        forM_ ["https://offline.invalid/schema", "file:///OFFLINE_ONLY"] $ \reference ->
          try @SchemaValidatorError (validateSchemaDefinition options (object ["$ref" .= String reference])) >>= (@?= Left InvalidSchemaDefinition),
      testCase "missing worker and invalid budgets fail explicitly without fallback" $ do
        let absent = options {schemaValidatorExecutable = "/nonexistent/factory-validator"}
        try @SchemaValidatorError (validateSchemaValue absent (Bool True) Null) >>= (@?= Left ValidatorUnavailable)
        try @SchemaValidatorError (validateSchemaValue (absent {schemaValidatorTimeoutMicros = 0}) (Bool True) Null) >>= (@?= Left ValidatorTimedOut)
        try @SchemaValidatorError (validateSchemaValue (absent {schemaValidatorTimeoutMicros = -1}) (Bool True) Null) >>= (@?= Left InvalidValidatorOptions),
      testCase "hostile schema compilation times out and the native worker is reaped" $
        bounded $
          withReadyPidWorker $ \configured pidFile -> do
            -- Allow launch under parallel load; still assert the requested
            -- deadline itself, rather than accepting an early timeout.
            started <- getMonotonicTimeNSec
            result <- try @SchemaValidatorError (validateSchemaValue (configured {schemaValidatorTimeoutMicros = 2000000}) hostileSchema (object ["v" .= Number 1]))
            result @?= Left ValidatorTimedOut
            finished <- getMonotonicTimeNSec
            assertBool "Validator expired before its configured deadline" (finished - started >= 2000000000)
            assertBool "Validator exceeded deadline plus cleanup allowance" (finished - started < 4000000000)
            startedWorker <- doesFileExist pidFile
            assertBool "Deadline expired before the timed worker launcher ran" startedWorker
            identifier <- workerIdentifier pidFile
            assertReaped identifier,
      testCase "caller cancellation preserves its identity and reaps the worker" $
        bounded $
          withPidWorker $ \configured pidFile ->
            withAsync (validateSchemaValue configured hostileSchema (object ["v" .= Number 1])) $ \pending -> do
              identifier <- workerIdentifier pidFile
              threadDelay 20000
              cancel pending
              waitCatch pending >>= \case
                Left cause -> fromException cause @?= Just AsyncCancelled
                Right _ -> assertFailure "Hostile validator unexpectedly completed"
              assertReaped identifier,
      testCase "backtracking limits do not change valid matches or their negation" $ do
        let schema = object ["type" .= String "string", "pattern" .= String "^(a|aa)+\\1b|a+$"]
            value = String (Text.replicate 28 "a")
        validateSchemaValue options schema value >>= (@?= True)
        validateSchemaValue options (object ["not" .= schema]) value >>= (@?= False),
      testCase "regex execution uses the owned deadline and reaps its worker" $
        bounded $
          withReadyPidWorker $ \configured pidFile -> do
            let schema = object ["type" .= String "string", "pattern" .= String "^(a|aa)+\\1b|a+$"]
            started <- getMonotonicTimeNSec
            try @SchemaValidatorError (validateSchemaValue (configured {schemaValidatorTimeoutMicros = 2000000}) schema (String (Text.replicate 80 "a"))) >>= (@?= Left ValidatorTimedOut)
            finished <- getMonotonicTimeNSec
            assertBool "Regex execution expired before its configured deadline" (finished - started >= 2000000000)
            assertBool "Regex execution exceeded deadline plus cleanup allowance" (finished - started < 4000000000)
            doesFileExist pidFile >>= assertBool "Deadline expired before the timed worker launcher ran"
            workerIdentifier pidFile >>= assertReaped,
      testCase "regex engine failures cannot become success under negation" $
        bounded $
          forM_ [False, True] $ \negated ->
            withPidWorker $ \configured pidFile -> do
              let patternSchema = object ["type" .= String "string", "pattern" .= String "^(a(?=a))*a$"]
                  schema = if negated then object ["not" .= patternSchema] else patternSchema
              try @SchemaValidatorError (validateSchemaValue configured schema (String (Text.replicate 1100000 "a"))) >>= (@?= Left ValidatorProcessFailure)
              workerIdentifier pidFile >>= assertReaped
    ]

options :: SchemaValidatorOptions
options = defaultSchemaValidatorOptions {schemaValidatorTimeoutMicros = 5000000}

json :: BS.ByteString -> IO Value
json = either assertFailure pure . eitherDecodeStrict'

hostileSchema :: Value
hostileSchema = object ["type" .= String "object", "$defs" .= Object definitions, "properties" .= object ["v" .= object ["$ref" .= String "#/$defs/n35"]]]
  where
    definitions = KeyMap.fromList (("n0", object ["type" .= String "integer"]) : [(Key.fromString ("n" <> show depth), object ["allOf" .= replicate 2 (object ["$ref" .= ("#/$defs/n" <> show (depth - 1))])]) | depth <- [1 .. 35 :: Int]])

-- Warm this exact launcher path, not the subsequent timed process. Its deadline
-- still includes startup. Remove the old marker so only the timed job's PID counts.
withReadyPidWorker :: (SchemaValidatorOptions -> FilePath -> IO a) -> IO a
withReadyPidWorker action = withPidWorker $ \configured pidFile -> do
  validateSchemaValue configured (Bool True) Null >>= (@?= True)
  workerIdentifier pidFile >>= assertReaped
  removeFile pidFile
  action configured pidFile

-- The wrapper only records its PID, then execs the real worker. No schema or
-- value enters argv, environment, or the PID file.
withPidWorker :: (SchemaValidatorOptions -> FilePath -> IO a) -> IO a
withPidWorker action = bracket (getTemporaryDirectory >>= \root -> mkdtemp (root </> "droid-validator-")) removePathForcibly $ \root -> do
  worker <- findExecutable "factory-droid-validator" >>= maybe (assertFailure "Build/install the native validator worker before running tests") pure
  let pidFile = root </> "pid"
      wrapper = root </> "worker"
  writeFile wrapper ("#!/bin/sh\nprintf '%s\\n' \"$$\" > " <> quote pidFile <> "\nexec " <> quote worker <> "\n")
  setFileMode wrapper 0o700
  action (options {schemaValidatorExecutable = wrapper}) pidFile
  where
    quote path = "'" <> concatMap (\char -> if char == '\'' then "'\\''" else [char]) path <> "'"

workerIdentifier :: FilePath -> IO Text.Text
workerIdentifier path = do
  exists <- doesFileExist path
  unless exists (threadDelay 1000)
  if exists
    then do
      value <- readFile path
      if null value then threadDelay 1000 >> workerIdentifier path else pure ("fixture-" <> Text.strip (Text.pack value))
    else workerIdentifier path
