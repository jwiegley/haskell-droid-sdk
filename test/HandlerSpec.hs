{-# LANGUAGE OverloadedStrings #-}

module HandlerSpec (handlerTests) where

import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Exception (throwIO, try)
import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (..))
import Factory.Droid.Interaction
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome (..))
import Factory.Droid.Schema.RPC (BaseRequest (..), JsonRpcBaseRequest, WithEnvelope (..))
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

handlerTests :: TestTree
handlerTests =
  testGroup
    "Validated interaction handlers"
    [ testCase "unconfigured handlers cancel quietly even malformed requests" $ do
        failures <- newIORef []
        let handlers = defaultDroidHandlers {onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
        permissionRpcHandler handlers (request Null) >>= (@?= Right (toJSON cancelPermissionResult))
        questionRpcHandler handlers (request Null) >>= (@?= Right (toJSON cancelDroidQuestions))
        readIORef failures >>= (@?= []),
      testGroup
        "every offered permission outcome is supported"
        [ testCase (show choice) $ do
            let params = permissionRequest [choice]
                edited = if choice == ConfirmProceedEdit then Just "" else Nothing
            response <- maybe (assertFailure "Expected valid permission result") pure (respondPermission params choice (Just "comment") edited)
            let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> pure response)}
            permissionRpcHandler handlers (request (toJSON params)) >>= (@?= Right (toJSON response))
        | choice <- [minBound .. maxBound]
        ],
      testCase "proceed_edit requires content but accepts an empty edit" $ do
        mkRequestPermissionResult ConfirmProceedEdit Nothing Nothing mempty @?= Nothing
        response <- maybe (assertFailure "Expected empty edited content") pure (mkRequestPermissionResult ConfirmProceedEdit (Just "") (Just "") mempty)
        toJSON response @?= object ["selectedOption" .= ConfirmProceedEdit, "comment" .= String "", "editedSpecContent" .= String ""]
        fromJSON (toJSON response) @?= Success response
        rejects (Proxy @RequestPermissionResult) (object ["selectedOption" .= ConfirmProceedEdit])
        rejects (Proxy @RequestPermissionResult) (object ["selectedOption" .= ConfirmProceedEdit, "editedSpecContent" .= Null]),
      testCase "permission result fields have strict types and owned extensions" $ do
        rejects (Proxy @RequestPermissionResult) (object ["selectedOption" .= ConfirmCancel, "comment" .= Null])
        rejects (Proxy @RequestPermissionResult) (object ["selectedOption" .= String "unknown"])
        response <- maybe (assertFailure "Expected response") pure (mkRequestPermissionResult ConfirmCancel Nothing Nothing (KeyMap.fromList ["selectedOption" .= ConfirmProceedAlways, "comment" .= String "injected", "editedSpecContent" .= String "injected", "future" .= Number 1]))
        toJSON response @?= object ["selectedOption" .= ConfirmCancel, "future" .= Number 1]
        show response @?= "RequestPermissionResult <redacted>",
      testCase "unoffered choices cancel rather than escalating permission" $ do
        response <- maybe (assertFailure "Expected wire response") pure (mkRequestPermissionResult ConfirmProceedAlways (Just "private") Nothing mempty)
        failures <- newIORef []
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> pure response), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
        respondPermission (permissionRequest [ConfirmProceedOnce]) ConfirmProceedAlways Nothing Nothing @?= Nothing
        permissionRpcHandler handlers (request (toJSON (permissionRequest [ConfirmProceedOnce]))) >>= (@?= Right (toJSON cancelPermissionResult))
        readIORef failures >>= (@?= [InvalidInteractionResponse PermissionInteraction]),
      testCase "malformed permission payloads do not invoke user code" $ do
        called <- newIORef False
        failures <- newIORef []
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> writeIORef called True >> pure cancelPermissionResult), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
        permissionRpcHandler handlers (request (object ["toolUses" .= False, "options" .= ([] :: [Value])])) >>= (@?= Right (toJSON cancelPermissionResult))
        readIORef called >>= (@?= False)
        readIORef failures >>= (@?= [InvalidInteractionRequest PermissionInteraction]),
      testGroup
        "ordinary permission failures are isolated and sanitized"
        [ testCase name $ do
            failures <- newIORef []
            let handlers = defaultDroidHandlers {onDroidPermission = Just callback, onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
            permissionRpcHandler handlers (request (toJSON (permissionRequest [ConfirmProceedOnce]))) >>= (@?= Right (toJSON cancelPermissionResult))
            readIORef failures >>= (@?= [InteractionHandlerFailed PermissionInteraction])
        | (name, callback) <-
            [ ("IO exception", \_ -> throwIO (userError "private failure")),
              ("bottom result", \_ -> pure (error "private bottom")),
              ("nested bottom", \_ -> maybe (assertFailure "Expected response") pure (mkRequestPermissionResult ConfirmProceedOnce (Just (error "private comment")) Nothing mempty)),
              ("extension bottom", \_ -> maybe (assertFailure "Expected response") pure (mkRequestPermissionResult ConfirmProceedOnce Nothing Nothing (KeyMap.singleton "future" (error "private extension"))))
            ]
        ],
      testCase "observer exceptions do not prevent safe fallback" $ do
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> throwIO (userError "private")), onDroidInteractionFailure = Just (\_ -> throwIO (userError "observer private"))}
        permissionRpcHandler handlers (request (toJSON (permissionRequest [ConfirmProceedOnce]))) >>= (@?= Right (toJSON cancelPermissionResult)),
      testCase "asynchronous callback cancellation is not converted to a decision" $ do
        failures <- newIORef []
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> throwIO AsyncCancelled), onDroidQuestion = Just (\_ -> throwIO AsyncCancelled), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
        permission <- try @AsyncCancelled (permissionRpcHandler handlers (request (toJSON (permissionRequest [ConfirmProceedOnce]))))
        permission @?= Left AsyncCancelled
        question <- try @AsyncCancelled (questionRpcHandler handlers (request (toJSON questions)))
        question @?= Left AsyncCancelled
        readIORef failures >>= (@?= []),
      testCase "question helpers retain identities and free-form multi-select text" $ do
        let first = answerDroidQuestion questionOne "not an offered option"
            second = answerDroidQuestionMultiple questionTwo ["red", "blue"]
        answerText second @?= "red, blue"
        response <- maybe (assertFailure "Expected submitted answers") pure (submitDroidAnswers questions [first, second])
        let handlers = defaultDroidHandlers {onDroidQuestion = Just (\_ -> pure response)}
        questionRpcHandler handlers (request (toJSON questions)) >>= (@?= Right (toJSON response)),
      testGroup
        "invalid question answers cancel"
        [ testCase name $ do
            failures <- newIORef []
            let handlers = defaultDroidHandlers {onDroidQuestion = Just (\_ -> pure response), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
            questionRpcHandler handlers (request (toJSON questions)) >>= (@?= Right (toJSON cancelDroidQuestions))
            readIORef failures >>= (@?= [InvalidInteractionResponse QuestionInteraction])
        | (name, response) <-
            [ ("unknown index", AskUserResult [AskUserCollectedAnswer 9 "Which?" "answer" mempty] (Just False) mempty),
              ("wrong question", AskUserResult [AskUserCollectedAnswer 1.5 "different" "answer" mempty] (Just False) mempty),
              ("duplicate index", AskUserResult [answerDroidQuestion questionOne "a", answerDroidQuestion questionOne "b"] Nothing mempty),
              ("cancelled with answers", AskUserResult [answerDroidQuestion questionOne "a"] (Just True) mempty)
            ]
        ],
      testCase "empty noncancelled responses and omitted cancellation remain valid wire values" $ do
        let response = AskUserResult [] Nothing (KeyMap.singleton "future" (Number 1))
            handlers = defaultDroidHandlers {onDroidQuestion = Just (\_ -> pure response)}
        questionRpcHandler handlers (request (toJSON questions)) >>= (@?= Right (toJSON response)),
      testCase "question exceptions and malformed payloads cancel without details" $ do
        failures <- newIORef []
        let handlers = defaultDroidHandlers {onDroidQuestion = Just (\_ -> throwIO (userError "private answer")), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
        questionRpcHandler handlers (request (toJSON questions)) >>= (@?= Right (toJSON cancelDroidQuestions))
        questionRpcHandler handlers (request (object [])) >>= (@?= Right (toJSON cancelDroidQuestions))
        readIORef failures >>= (@?= [InteractionHandlerFailed QuestionInteraction, InvalidInteractionRequest QuestionInteraction])
    ]

request :: Value -> JsonRpcBaseRequest
request params = WithEnvelope (Just "1.201.1") Nothing (BaseRequest "request-id" "fixture" (Just params) mempty)

permissionRequest :: [ToolConfirmationOutcome] -> RequestPermissionParams
permissionRequest choices = RequestPermissionParams [] [ToolConfirmationListItem "label" value mempty | value <- choices] (Just ["source", "source"]) mempty

questionOne, questionTwo :: AskUserQuestion
questionOne = AskUserQuestion 1.5 "Topic" "Which?" ["red", "blue"] Nothing mempty
questionTwo = AskUserQuestion 2 "Second" "Multiple?" ["red", "blue"] (Just True) mempty

questions :: AskUserParams
questions = AskUserParams "tool" [questionOne, questionTwo] mempty
