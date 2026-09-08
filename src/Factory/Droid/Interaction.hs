{-# LANGUAGE OverloadedStrings #-}

-- | Request-bound interaction validation and safe RPC adapters. Handlers belong
-- to the owned CLI connection (including successor sessions), not to one turn.
module Factory.Droid.Interaction
  ( DroidHandlers (..),
    defaultDroidHandlers,
    DroidInteraction (..),
    DroidInteractionFailure (..),
    respondPermission,
    answerDroidQuestion,
    answerDroidQuestionMultiple,
    submitDroidAnswers,
    cancelDroidQuestions,
    permissionRpcHandler,
    questionRpcHandler,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (guard, void)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import Data.Aeson.Types (parseEither)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.Exception (trySync)
import Factory.Droid.Protocol.Dispatch (RpcRequestHandler)
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome)
import Factory.Droid.Schema.RPC (BaseRequest (..), JsonRpcBaseRequest, WithEnvelope (..))

-- | Callbacks may run concurrently on dispatcher-owned workers, including during
-- initialization/loading. Scope exit cancels and joins them. Ordinary failures
-- cancel the interaction; callbacks must cooperate with asynchronous cancellation.
data DroidHandlers = DroidHandlers
  { onDroidPermission :: Maybe (RequestPermissionParams -> IO RequestPermissionResult),
    onDroidQuestion :: Maybe (AskUserParams -> IO AskUserResult),
    onDroidInteractionFailure :: Maybe (DroidInteractionFailure -> IO ())
  }

instance Show DroidHandlers where
  show _ = "DroidHandlers <redacted>"

defaultDroidHandlers :: DroidHandlers
defaultDroidHandlers = DroidHandlers Nothing Nothing Nothing

data DroidInteraction = PermissionInteraction | QuestionInteraction
  deriving stock (Eq, Show)

-- | Payload-free failure classifications. No question, command, answer or
-- exception text is copied into diagnostic output.
data DroidInteractionFailure
  = InvalidInteractionRequest !DroidInteraction
  | InteractionHandlerFailed !DroidInteraction
  | InvalidInteractionResponse !DroidInteraction
  deriving stock (Eq, Show)

respondPermission :: RequestPermissionParams -> ToolConfirmationOutcome -> Maybe Text -> Maybe Text -> Maybe RequestPermissionResult
respondPermission request choice comment edited = do
  guard (choice `elem` map confirmationOptionValue (permissionOptions request))
  mkRequestPermissionResult choice comment edited mempty

answerDroidQuestion :: AskUserQuestion -> Text -> AskUserCollectedAnswer
answerDroidQuestion question answer = AskUserCollectedAnswer (questionIndex question) (questionText question) answer mempty

-- | Multi-select answers use the reference comma-space text convention. Options
-- remain suggestions; free-form answers are not rejected for lacking membership.
answerDroidQuestionMultiple :: AskUserQuestion -> [Text] -> AskUserCollectedAnswer
answerDroidQuestionMultiple question = answerDroidQuestion question . Text.intercalate ", "

submitDroidAnswers :: AskUserParams -> [AskUserCollectedAnswer] -> Maybe AskUserResult
submitDroidAnswers request answers =
  let result = AskUserResult answers (Just False) mempty
   in result <$ guard (validQuestionResponse request result)

cancelDroidQuestions :: AskUserResult
cancelDroidQuestions = AskUserResult [] (Just True) mempty

-- | Safe adapter for the generic dispatcher. Unconfigured handlers cancel
-- quietly; malformed requests, invalid replies and ordinary failures are reported.
permissionRpcHandler :: DroidHandlers -> RpcRequestHandler
permissionRpcHandler handlers request =
  Right <$> handleInteraction handlers PermissionInteraction (onDroidPermission handlers) validPermissionResponse cancelPermissionResult request

questionRpcHandler :: DroidHandlers -> RpcRequestHandler
questionRpcHandler handlers request =
  Right <$> handleInteraction handlers QuestionInteraction (onDroidQuestion handlers) validQuestionResponse cancelDroidQuestions request

validPermissionResponse :: RequestPermissionParams -> RequestPermissionResult -> Bool
validPermissionResponse request response = permissionSelectedOption response `elem` map confirmationOptionValue (permissionOptions request)

validQuestionResponse :: AskUserParams -> AskUserResult -> Bool
validQuestionResponse request response =
  let answers = askUserAnswers response
      expected = Set.fromList [(questionIndex question, questionText question) | question <- askUserQuestions request]
      indices = Set.fromList (map answerIndex answers)
   in (askUserCancelled response /= Just True || null answers)
        && all (\answer -> (answerIndex answer, answerQuestion answer) `Set.member` expected) answers
        && Set.size indices == length answers

handleInteraction :: (FromJSON request, ToJSON response) => DroidHandlers -> DroidInteraction -> Maybe (request -> IO response) -> (request -> response -> Bool) -> response -> JsonRpcBaseRequest -> IO Value
handleInteraction handlers interaction handler valid fallback request = case handler of
  Nothing -> pure (toJSON fallback)
  Just action -> case baseRequestParams (envelopeBody request) of
    Nothing -> reject (InvalidInteractionRequest interaction)
    Just params -> case parseEither parseJSON params of
      Left _ -> reject (InvalidInteractionRequest interaction)
      Right parsed -> do
        outcome <- trySync $ do
          response <- action parsed
          if valid parsed response
            then Just <$> evaluate (force (toJSON response))
            else pure Nothing
        case outcome of
          Left _ -> reject (InteractionHandlerFailed interaction)
          Right Nothing -> reject (InvalidInteractionResponse interaction)
          Right (Just value) -> pure value
  where
    reject failure = do
      case onDroidInteractionFailure handlers of
        Nothing -> pure ()
        Just report -> void (trySync (report failure))
      pure (toJSON fallback)
