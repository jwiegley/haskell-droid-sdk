{-# LANGUAGE OverloadedStrings #-}

-- | Script-run result records from Factory protocol 1.205.0. The result and
-- nested bookkeeping objects are closed schemas. Decoding reports state only;
-- it does not launch scripts, resume calls or access result-file paths.
module Factory.Droid.Schema.Script
  ( ScriptRunResultValue,
    BackgroundTask (..),
    InterruptedCall (..),
    ScriptCompletion (..),
    ScriptRunResult (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    object,
    withObject,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (optionalField, rejectUnknownFields)
import Factory.Droid.Schema.Primitives (NonEmptyText)

-- | Exactly the recursive JSON value domain: null, Boolean, number, string,
-- array or object. This schema is intentionally dynamic, not an opaque
-- substitute for a more specific record.
type ScriptRunResultValue = Value

-- | Background work retained after a terminal result. A present description
-- must be nonempty; the enclosing task list, when present, is also nonempty.
data BackgroundTask = BackgroundTask
  { backgroundTaskId :: !NonEmptyText,
    backgroundTaskDescription :: !(Maybe NonEmptyText)
  }
  deriving stock (Eq, Show)

instance FromJSON BackgroundTask where
  parseJSON = withObject "BackgroundTask" $ \fields -> do
    rejectUnknownFields ["taskId", "description"] fields
    BackgroundTask <$> fields .: "taskId" <*> fields .:! "description"

instance ToJSON BackgroundTask where
  toJSON task = object (["taskId" .= backgroundTaskId task] <> optionalField "description" (backgroundTaskDescription task))

-- | An interrupted tool invocation. The record is closed, while its input
-- is an arbitrary object of ScriptRunResultValue values.
data InterruptedCall = InterruptedCall
  { interruptedToolUseId :: !NonEmptyText,
    interruptedName :: !NonEmptyText,
    interruptedInput :: !Object,
    interruptedTaskId :: !(Maybe NonEmptyText)
  }
  deriving stock (Eq, Show)

instance FromJSON InterruptedCall where
  parseJSON = withObject "InterruptedCall" $ \fields -> do
    rejectUnknownFields ["toolUseId", "name", "input", "taskId"] fields
    InterruptedCall <$> fields .: "toolUseId" <*> fields .: "name" <*> fields .: "input" <*> fields .:! "taskId"

instance ToJSON InterruptedCall where
  toJSON call =
    object $
      ["toolUseId" .= interruptedToolUseId call, "name" .= interruptedName call, "input" .= interruptedInput call]
        <> optionalField "taskId" (interruptedTaskId call)

-- | Completion has either an inline result (which may be null) or a nonempty
-- result-file path, never both. A path is metadata, not an instruction to read.
data ScriptCompletion = InlineScriptResult !ScriptRunResultValue | ScriptResultFile !NonEmptyText
  deriving stock (Eq, Show)

-- | All six schema variants, with the two completed variants represented by
-- ScriptCompletion. Running/stalled results cannot carry background tasks.
-- Cancellation requires a calls array, but that array may be empty.
data ScriptRunResult
  = ScriptRunning !NonEmptyText
  | ScriptStalled !NonEmptyText
  | ScriptCompleted !NonEmptyText !ScriptCompletion !(Maybe (NonEmpty BackgroundTask))
  | ScriptFailed !NonEmptyText !Text !(Maybe (NonEmpty BackgroundTask))
  | ScriptCancelled !NonEmptyText ![InterruptedCall] !(Maybe (NonEmpty BackgroundTask))
  deriving stock (Eq, Show)

instance FromJSON ScriptRunResult where
  parseJSON = withObject "ScriptRunResult" $ \fields -> do
    status <- fields .: "status" :: Parser Text
    case status of
      "running" -> do
        rejectUnknownFields ["runId", "status"] fields
        ScriptRunning <$> fields .: "runId"
      "stalled" -> do
        rejectUnknownFields ["runId", "status"] fields
        ScriptStalled <$> fields .: "runId"
      "completed" -> do
        completion <- case (KeyMap.member "result" fields, KeyMap.member "resultPath" fields) of
          (True, False) -> do
            rejectUnknownFields ["runId", "status", "result", "backgroundTasks"] fields
            InlineScriptResult <$> fields .: "result"
          (False, True) -> do
            rejectUnknownFields ["runId", "status", "resultPath", "backgroundTasks"] fields
            ScriptResultFile <$> fields .: "resultPath"
          _ -> fail "Completed results require exactly one of result and resultPath"
        ScriptCompleted <$> fields .: "runId" <*> pure completion <*> fields .:! "backgroundTasks"
      "failed" -> do
        rejectUnknownFields ["runId", "status", "error", "backgroundTasks"] fields
        ScriptFailed <$> fields .: "runId" <*> fields .: "error" <*> fields .:! "backgroundTasks"
      "cancelled" -> do
        rejectUnknownFields ["runId", "status", "interruptedCalls", "backgroundTasks"] fields
        ScriptCancelled <$> fields .: "runId" <*> fields .: "interruptedCalls" <*> fields .:! "backgroundTasks"
      _ -> fail "Unknown script-run status"

instance ToJSON ScriptRunResult where
  toJSON = \case
    ScriptRunning identifier -> object (resultFields "running" identifier)
    ScriptStalled identifier -> object (resultFields "stalled" identifier)
    ScriptCompleted identifier completion tasks ->
      object (resultFields "completed" identifier <> completionFields completion <> optionalField "backgroundTasks" tasks)
    ScriptFailed identifier err tasks ->
      object (resultFields "failed" identifier <> ["error" .= err] <> optionalField "backgroundTasks" tasks)
    ScriptCancelled identifier calls tasks ->
      object (resultFields "cancelled" identifier <> ["interruptedCalls" .= calls] <> optionalField "backgroundTasks" tasks)

resultFields :: Text -> NonEmptyText -> [Pair]
resultFields status identifier = ["runId" .= identifier, "status" .= status]

completionFields :: ScriptCompletion -> [Pair]
completionFields (InlineScriptResult result) = ["result" .= result]
completionFields (ScriptResultFile path) = ["resultPath" .= path]
