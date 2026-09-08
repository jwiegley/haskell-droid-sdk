{-# LANGUAGE OverloadedStrings #-}

module AutomationSpec (automationTests) where

import Control.Monad (forM_)
import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Automation
import Factory.Droid.Schema.Primitives (mkNonEmptyText)
import SchemaTest (enumSchemaTest, nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

automationTests :: Value -> TestTree
automationTests schema = case mkNonEmptyText "automation-id" of
  Nothing -> testCase "fixture ID" (assertFailure "Nonempty fixture ID rejected")
  Just identifier ->
    let listing = AutomationListToolInput AutomationRemote mempty
        listingJSON = KeyMap.singleton "executionLocation" (String "remote")
        target = AutomationTarget AutomationLocal identifier mempty
        targetJSON = KeyMap.fromList ["executionLocation" .= String "local", "automationId" .= String "automation-id"]
        definition name = schemaAt ["definitions", name] schema
     in testGroup
          "Automation wire inputs"
          [ nonNullableRecordTests "AutomationListToolInputSchema" (definition "AutomationListToolInputSchema") listing listing listingJSON listingJSON (\extras value -> value {automationListAdditionalFields = extras}),
            nonNullableRecordTests "AutomationReadToolInputSchema" (definition "AutomationReadToolInputSchema") target target targetJSON targetJSON (\extras value -> value {automationTargetAdditionalFields = extras}),
            nonNullableRecordTests "AutomationDeleteToolInputSchema" (definition "AutomationDeleteToolInputSchema") target target targetJSON targetJSON (\extras value -> value {automationTargetAdditionalFields = extras}),
            enumSchemaTest "execution locations" (definition "AutomationListToolInputSchema" >>= schemaAt ["properties", "executionLocation", "enum"]) (Proxy @AutomationExecutionLocation),
            testCase "read and delete share the same constraints apart from descriptions" $ do
              readSchema <- either assertFailure pure (definition "AutomationReadToolInputSchema")
              deleteSchema <- either assertFailure pure (definition "AutomationDeleteToolInputSchema")
              stripDescriptions readSchema @?= stripDescriptions deleteSchema,
            testCase "unknown locations and empty identifiers are rejected" $ do
              rejects (Proxy @AutomationListToolInput) (object ["executionLocation" .= String "future"])
              rejects (Proxy @AutomationReadToolInput) (object ["executionLocation" .= String "local", "automationId" .= String ""])
              rejects (Proxy @AutomationDeleteToolInput) (object ["executionLocation" .= String "future", "automationId" .= String "id"]),
            testCase "both operations preserve padded IDs without older input trimming" $ do
              forM_ [" ", "  id  ", "شناسه"] $ \text -> case mkNonEmptyText text of
                Nothing -> assertFailure "Nonempty ID rejected"
                Just value -> do
                  let input = AutomationTarget AutomationRemote value mempty
                      wire = object ["executionLocation" .= String "remote", "automationId" .= String text]
                  fromJSON wire @?= Success input
                  toJSON input @?= wire
          ]

stripDescriptions :: Value -> Value
stripDescriptions (Object fields) = Object (KeyMap.map stripDescriptions (KeyMap.delete "description" fields))
stripDescriptions (Array values) = Array (fmap stripDescriptions values)
stripDescriptions value = value
