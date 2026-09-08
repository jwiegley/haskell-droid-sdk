{-# LANGUAGE OverloadedStrings #-}

module ScriptSpec (scriptTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( Object,
    Result (..),
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Primitives
import Factory.Droid.Schema.Script
import SchemaTest (rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

scriptTests :: Value -> TestTree
scriptTests schema =
  case traverse mkNonEmptyText ["run-id", "task-id", "description", "tool-id", "tool-name", "result-path"] of
    Just [runId, taskId, description, toolId, toolName, path] ->
      let task = BackgroundTask taskId (Just description)
          tasks = Just (task :| [])
          input = KeyMap.singleton "nested" (object ["items" .= [Null, Bool True, Number 1.125]])
          call = InterruptedCall toolId toolName input (Just taskId)
          taskJSON = object ["taskId" .= String "task-id", "description" .= String "description"]
          callJSON = object ["toolUseId" .= String "tool-id", "name" .= String "tool-name", "input" .= input, "taskId" .= String "task-id"]
          base status = KeyMap.fromList ["runId" .= String "run-id", "status" .= String status]
          cancelled = KeyMap.insert "interruptedCalls" (toJSON ([] :: [Value])) (base "cancelled")
          minimumCases =
            [ (ScriptRunning runId, base "running"),
              (ScriptStalled runId, base "stalled"),
              (ScriptCompleted runId (InlineScriptResult Null) Nothing, KeyMap.insert "result" Null (base "completed")),
              (ScriptCompleted runId (ScriptResultFile path) Nothing, KeyMap.insert "resultPath" (String "result-path") (base "completed")),
              (ScriptFailed runId "" Nothing, KeyMap.insert "error" (String "") (base "failed")),
              (ScriptCancelled runId [] Nothing, cancelled)
            ]
          fullCases =
            [ (ScriptRunning runId, base "running"),
              (ScriptStalled runId, base "stalled"),
              (ScriptCompleted runId (InlineScriptResult Null) tasks, withTasks taskJSON (KeyMap.insert "result" Null (base "completed"))),
              (ScriptCompleted runId (ScriptResultFile path) tasks, withTasks taskJSON (KeyMap.insert "resultPath" (String "result-path") (base "completed"))),
              (ScriptFailed runId "failure" tasks, withTasks taskJSON (KeyMap.insert "error" (String "failure") (base "failed"))),
              (ScriptCancelled runId [call] tasks, withTasks taskJSON (KeyMap.insert "interruptedCalls" (toJSON [callJSON]) (base "cancelled")))
            ]
       in testGroup
            "Script results"
            [ testCase "six minimum variants match required fields and status literals" $ do
                branches <- either assertFailure pure (scriptBranches schema)
                length branches @?= length minimumCases
                forM_ (zip branches minimumCases) $ \(branch, (value, fields)) -> do
                  required <- either assertFailure pure (parseEither (.: "required") branch :: Either String [Key])
                  sort (KeyMap.keys fields) @?= sort required
                  status <- either assertFailure pure (schemaAt ["properties", "status", "const"] (Object branch))
                  KeyMap.lookup "status" fields @?= Just status
                  KeyMap.lookup "additionalProperties" branch @?= Just (Bool False)
                  fromJSON (Object fields) @?= Success value
                  toJSON value @?= Object fields,
              testCase "full variants cover every declared field and round-trip" $ do
                branches <- either assertFailure pure (scriptBranches schema)
                forM_ (zip branches fullCases) $ \(branch, (value, fields)) -> do
                  properties <- either assertFailure pure (parseEither (.: "properties") branch :: Either String Object)
                  sort (KeyMap.keys fields) @?= sort (KeyMap.keys properties)
                  fromJSON (Object fields) @?= Success value
                  toJSON value @?= Object fields
                  eitherDecode (encode value) @?= Right value,
              testCase "all required fields are enforced" $ do
                branches <- either assertFailure pure (scriptBranches schema)
                forM_ (zip branches fullCases) $ \(branch, (_, fields)) -> do
                  required <- either assertFailure pure (parseEither (.: "required") branch :: Either String [Key])
                  forM_ required $ \key -> rejects (Proxy @ScriptRunResult) (Object (KeyMap.delete key fields)),
              testCase "all variants reject unknown fields" $
                forM_ fullCases $ \(_, fields) ->
                  rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "unexpected" Null fields)),
              testCase "completed variants require exactly one result location" $ do
                rejects (Proxy @ScriptRunResult) (Object (base "completed"))
                rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "result" Null (KeyMap.insert "resultPath" (String "result-path") (base "completed"))))
                rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "resultPath" Null (base "completed")))
                rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "resultPath" (String "") (base "completed"))),
              testCase "nonempty identifiers reject empty and wrong JSON kinds" $ do
                forM_ minimumCases $ \(_, fields) ->
                  forM_ [Null, String "", Number 0, Bool True, Array mempty, Object mempty] $ \value ->
                    rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "runId" value fields)),
              testCase "nonempty text preserves whitespace and Unicode" $ do
                mkNonEmptyText "" @?= Nothing
                forM_ [" ", "\n", "نام", "id"] $ \text ->
                  case mkNonEmptyText text of
                    Nothing -> assertFailure "Nonempty text rejected"
                    Just value -> do
                      nonEmptyTextValue value @?= text
                      fromJSON (String text) @?= Success value
                      toJSON value @?= String text,
              testCase "background task lists are optional but nonempty when present" $ do
                branches <- either assertFailure pure (scriptBranches schema)
                forM_ (drop 2 branches) $ \branch -> do
                  minimumItems <- either assertFailure pure (schemaAt ["properties", "backgroundTasks", "minItems"] (Object branch))
                  minimumItems @?= Number 1
                forM_ (drop 2 minimumCases) $ \(_, fields) ->
                  forM_ [Null, toJSON ([] :: [Value]), toJSON [object []], toJSON [Null]] $ \value ->
                    rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "backgroundTasks" value fields)),
              testCase "running and stalled states cannot carry terminal fields" $
                forM_ (take 2 minimumCases) $ \(_, fields) ->
                  forM_ ["backgroundTasks", "error", "result", "resultPath", "interruptedCalls"] $ \key ->
                    rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert key Null fields)),
              testCase "background task records are closed and descriptions nonempty" $ do
                fromJSON taskJSON @?= Success task
                toJSON task @?= taskJSON
                let minimal = BackgroundTask taskId Nothing
                fromJSON (object ["taskId" .= String "task-id"]) @?= Success minimal
                toJSON minimal @?= object ["taskId" .= String "task-id"]
                forM_ [object ["taskId" .= String ""], object ["taskId" .= String "task-id", "description" .= String ""], object ["taskId" .= String "task-id", "description" .= Null], object ["taskId" .= String "task-id", "extra" .= True]] $ rejects (Proxy @BackgroundTask),
              testCase "interrupted calls are closed but input values remain arbitrary JSON" $ do
                fromJSON callJSON @?= Success call
                toJSON call @?= callJSON
                let minimal = InterruptedCall toolId toolName input Nothing
                    fields = KeyMap.fromList ["toolUseId" .= String "tool-id", "name" .= String "tool-name", "input" .= input]
                fromJSON (Object fields) @?= Success minimal
                toJSON minimal @?= Object fields
                forM_ ["toolUseId", "name", "input"] $ \key -> rejects (Proxy @InterruptedCall) (Object (KeyMap.delete key fields))
                forM_ ["toolUseId", "name", "taskId"] $ \key ->
                  forM_ [Null, String "", Number 1] $ \value -> rejects (Proxy @InterruptedCall) (Object (KeyMap.insert key value fields))
                rejects (Proxy @InterruptedCall) (Object (KeyMap.insert "extra" Null fields))
                rejects (Proxy @InterruptedCall) (Object (KeyMap.insert "input" (Array mempty) fields)),
              testCase "cancelled calls array is required, typed and may be empty" $ do
                fromJSON (Object cancelled) @?= Success (ScriptCancelled runId [] Nothing)
                forM_ [Null, Object mempty, toJSON [object []]] $ \value ->
                  rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "interruptedCalls" value cancelled)),
              testCase "inline results preserve every JSON kind including null" $
                forM_ [Null, Bool True, Number 12345678901234567890.125, String "", toJSON [Null, Bool False], Object input] $ \result -> do
                  let value = ScriptCompleted runId (InlineScriptResult result) Nothing
                      fields = KeyMap.insert "result" result (base "completed")
                  fromJSON (Object fields) @?= Success value
                  toJSON value @?= Object fields
                  eitherDecode (encode value) @?= Right value,
              testCase "result-value schema is exactly the recursive JSON domain" $ do
                alternatives <- either assertFailure pure (schemaAt ["definitions", "ScriptRunResultValueSchema", "anyOf"] schema)
                alternatives
                  @?= toJSON
                    [ object ["type" .= String "null"],
                      object ["type" .= String "boolean"],
                      object ["type" .= String "number"],
                      object ["type" .= String "string"],
                      object ["type" .= String "array", "items" .= object ["$ref" .= String "#/definitions/ScriptRunResultValueSchema"]],
                      object ["type" .= String "object", "additionalProperties" .= object ["$ref" .= String "#/definitions/ScriptRunResultValueSchema"]]
                    ],
              testCase "failed error is a required string, including an empty string" $
                forM_ [Null, Number 1, Bool True, Array mempty, Object mempty] $ \value ->
                  rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "error" value (base "failed"))),
              testCase "unknown statuses and non-object records fail safely" $ do
                forM_ [Null, Bool True, Number 0, String "future"] $ \status ->
                  rejects (Proxy @ScriptRunResult) (Object (KeyMap.insert "status" status (base "running")))
                forM_ [Null, Bool True, Number 1, String "script", Array mempty] $ \value -> do
                  rejects (Proxy @ScriptRunResult) value
                  rejects (Proxy @BackgroundTask) value
                  rejects (Proxy @InterruptedCall) value
            ]
    _ -> testCase "valid nonempty fixture text" (assertFailure "Could not construct fixture identifiers")

withTasks :: Value -> Object -> Object
withTasks task = KeyMap.insert "backgroundTasks" (toJSON [task])

scriptBranches :: Value -> Either String [Object]
scriptBranches schema = do
  definition <- schemaAt ["definitions", "ScriptRunResultSchema"] schema
  parseEither (withObject "ScriptRunResultSchema" (.: "anyOf")) definition
