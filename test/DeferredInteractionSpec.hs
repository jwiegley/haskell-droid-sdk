{-# LANGUAGE OverloadedStrings #-}

module DeferredInteractionSpec (deferredInteractionTests) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Schema.Interaction (AskUserResult (..), RequestPermissionResult, cancelPermissionResult, mkRequestPermissionResult)
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome (ConfirmProceedEdit))
import Factory.Droid.SessionState qualified as State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

deferredInteractionTests :: TestTree
deferredInteractionTests =
  testGroup
    "Deferred interaction state"
    [ testCase "permission upsert is keyed by tool identity and preserves the latest explicit decision" $ do
        let one = permission "request-one" "tool" 0
            two = permission "request-two" "tool" 1.25
            state = State.storeDeferredPermission two (State.storeDeferredPermission one State.emptySessionState)
        Map.size (State.sessionDeferredPermissions state) @?= 1
        Map.lookup "tool" (State.sessionDeferredPermissions state) @?= Just two
        State.sessionDeferredQuestions state @?= mempty,
      testCase "question and permission identities cannot consume each other's stored action" $ do
        let perm = permission "permission" "same-tool-id" 0
            question = State.DeferredUserAction "question" "same-tool-id" (AskUserResult [] (Just False) (KeyMap.singleton "future" (Bool False))) 0 []
            both = State.storeDeferredQuestion question (State.storeDeferredPermission perm State.emptySessionState)
            (taken, after) = State.takeDeferredQuestion "same-tool-id" both
        taken @?= Just question
        State.sessionDeferredQuestions after @?= mempty
        Map.lookup "same-tool-id" (State.sessionDeferredPermissions after) @?= Just perm,
      testCase "consumption is exact and idempotent while an unknown identity leaves state unchanged" $ do
        let value = permission "request" "tool" 0
            state = State.storeDeferredPermission value State.emptySessionState
            (taken, after) = State.takeDeferredPermission "tool" state
        taken @?= Just value
        State.takeDeferredPermission "missing" state @?= (Nothing, state)
        State.takeDeferredPermission "tool" after @?= (Nothing, after),
      testCase "clearing returns the number removed without changing other session observations" $ do
        let original = State.observeWorkingDirectory (Just "/cwd") State.emptySessionState
            stored = State.storeDeferredQuestion (State.DeferredUserAction "question" "tool" (AskUserResult [] Nothing mempty) 0 []) (State.storeDeferredPermission (permission "request" "tool" 0) original)
        State.clearDeferredUserActions stored @?= (2, original)
        State.clearDeferredUserActions original @?= (0, original),
      testCase "identical tool IDs remain independent in different session state values" $ do
        let first = State.storeDeferredPermission (permission "first" "tool" 0) State.emptySessionState
            second = State.storeDeferredPermission (permission "second" "tool" 1) State.emptySessionState
            (_, emptied) = State.takeDeferredPermission "tool" first
        Map.lookup "tool" (State.sessionDeferredPermissions second) @?= Just (permission "second" "tool" 1)
        State.sessionDeferredPermissions emptied @?= mempty,
      testCase "edited permission contents, false extensions and exact timestamps remain data and displays are redacted" $ do
        response <- maybe (assertFailure "Invalid fixture permission") pure (mkRequestPermissionResult ConfirmProceedEdit (Just "") (Just "") (KeyMap.singleton "future" (Bool False)))
        let value = State.DeferredUserAction "request" "tool" response 9007199254740993.125 ["owner"]
            state = State.storeDeferredPermission value State.emptySessionState
        case Map.lookup "tool" (State.sessionDeferredPermissions state) of
          Nothing -> assertFailure "Missing retained action"
          Just stored -> do
            State.deferredActionStoredAt stored @?= 9007199254740993.125
            toJSON (State.deferredActionResponse stored) @?= object ["selectedOption" .= String "proceed_edit", "comment" .= String "", "editedSpecContent" .= String "", "future" .= False]
        show value @?= "DeferredUserAction <redacted>"
        show state @?= "SessionState <redacted>",
      testCase "resolution requires both an associated surface and matching request or tool identity" $ do
        let action = (permission "request" "tool" 0) {State.deferredActionAssociatedSessionIds = ["worker", "owner"]}
            state = State.storeDeferredPermission action State.emptySessionState
        State.retireDeferredPermissions "foreign" "request" ["tool"] state @?= state
        State.retireDeferredPermissions "owner" "other" [] state @?= state
        State.sessionDeferredPermissions (State.retireDeferredPermissions "owner" "other" ["tool"] state) @?= mempty
        State.sessionDeferredPermissions (State.retireDeferredPermissions "worker" "request" [] state) @?= mempty
    ]

permission :: Text -> Text -> Scientific -> State.DeferredUserAction RequestPermissionResult
permission request tool timestamp = State.DeferredUserAction request tool cancelPermissionResult timestamp []
