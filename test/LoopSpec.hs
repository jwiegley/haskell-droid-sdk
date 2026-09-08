{-# LANGUAGE OverloadedStrings #-}

module LoopSpec (loopTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON,
    Object,
    Result (..),
    ToJSON,
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Loop
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

loopTests :: Value -> TestTree
loopTests schema = case mkLoopInterval 5000 of
  Nothing -> testCase "minimum loop interval" (assertFailure "Fixture interval rejected")
  Just interval ->
    let full = LoopState "loop-id" LoopWaiting interval 7 18446744073709551617 18446744073709551618 (Just 18446744073709551619) False (Just 11) (Just 12) (Just LoopStopUserStopped) mempty
        minimal = full {loopStateNextRunAt = Nothing, loopStateLastRunStartedAt = Nothing, loopStateLastRunCompletedAt = Nothing, loopStateStopReason = Nothing}
        event = LoopStateChanged full mempty
     in testGroup
          "Loop state"
          [ testCase "full and minimal snapshots preserve all fields" $ do
              forM_ [(full, fullJSON), (minimal, minimalJSON)] $ \(value, fields) -> do
                fromJSON (Object fields) @?= Success value
                toJSON value @?= Object fields
                eitherDecode (encode value) @?= Right value
              props <- either assertFailure pure (schemaAt ["definitions", "LoopStateSchema", "properties"] schema)
              case props of
                Object fields -> sort (KeyMap.keys fields) @?= sort (KeyMap.keys fullJSON)
                _ -> assertFailure "Expected properties",
            testCase "schema-required keys are required even when nullable" $ do
              required <- either assertFailure pure (schemaAt ["definitions", "LoopStateSchema", "required"] schema)
              case fromJSON required :: Result [Key] of
                Error err -> assertFailure err
                Success keys -> forM_ keys $ \key -> rejects (Proxy @LoopState) (Object (KeyMap.delete key fullJSON)),
            testCase "interval bounds are integral and inclusive" $ do
              forM_ [5000, 86400000] $ \value -> case mkLoopInterval value of
                Nothing -> assertFailure "Boundary interval rejected"
                Just decoded -> do
                  loopIntervalMilliseconds decoded @?= value
                  fromJSON (Number (fromInteger value)) @?= Success decoded
                  toJSON decoded @?= Number (fromInteger value)
              forM_ [-1, 0, 4999, 86400001] $ \value -> mkLoopInterval value @?= Nothing
              forM_ [Null, Number 5000.5, String "5000", Bool True] $ rejects (Proxy @LoopInterval)
              bounds <- either assertFailure pure (schemaAt ["definitions", "LoopStateSchema", "properties", "intervalMs"] schema)
              bounds @?= object ["type" .= String "integer", "minimum" .= Number 5000, "maximum" .= Number 86400000],
            testCase "natural fields reject negatives and fractional values" $
              forM_ ["iteration", "startedAt", "updatedAt", "nextRunAt", "lastRunStartedAt", "lastRunCompletedAt"] $ \key ->
                forM_ [Number (-1), Number 0.5, String "1", Bool True] $ \value ->
                  rejects (Proxy @LoopState) (Object (KeyMap.insert key value fullJSON)),
            testCase "only nextRunAt is nullable" $ do
              fromJSON (Object minimalJSON) @?= Success minimal
              forM_ (filter (/= "nextRunAt") (KeyMap.keys fullJSON)) $ \key ->
                rejects (Proxy @LoopState) (Object (KeyMap.insert key Null fullJSON)),
            testCase "extensions cannot override or reintroduce fields" $ do
              let value = full {loopStateAdditionalFields = KeyMap.singleton "future" (object ["value" .= Null])}
              eitherDecode (encode value) @?= Right value
              forM_ (KeyMap.keys fullJSON) $ \key ->
                toJSON (minimal {loopStateAdditionalFields = KeyMap.singleton key (String "injected")}) @?= Object minimalJSON,
            enumTests schema "status" (Proxy @LoopStatus),
            enumTests schema "stopReason" (Proxy @LoopStopReason),
            nonNullableRecordTests
              "LoopStateChangedNotificationSchema"
              (schemaAt ["definitions", "LoopStateChangedNotificationSchema"] schema)
              event
              event
              eventJSON
              eventJSON
              (\extras value -> value {changedLoopAdditionalFields = extras}),
            testCase "nested snapshots and root kinds are checked" $ do
              rejects (Proxy @LoopStateChanged) (object ["type" .= String "loop_state_changed", "loopState" .= object []])
              forM_ [Null, Bool True, Number 1, String "loop", Array mempty] $ rejects (Proxy @LoopState)
          ]

enumTests :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => Value -> Key -> Proxy a -> TestTree
enumTests schema field _ = testCase (show field) $ do
  literals <- either assertFailure pure (schemaAt ["definitions", "LoopStateSchema", "properties", field, "enum"] schema)
  let values = [minBound .. maxBound] :: [a]
  toJSON values @?= literals
  forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
  rejects (Proxy @a) (String "future")

fullJSON, minimalJSON, eventJSON :: Object
fullJSON = KeyMap.fromList ["loopId" .= String "loop-id", "status" .= String "waiting", "intervalMs" .= Number 5000, "iteration" .= Number 7, "startedAt" .= Number 18446744073709551617, "updatedAt" .= Number 18446744073709551618, "nextRunAt" .= Number 18446744073709551619, "isDue" .= False, "lastRunStartedAt" .= Number 11, "lastRunCompletedAt" .= Number 12, "stopReason" .= String "user_stopped"]
minimalJSON = KeyMap.insert "nextRunAt" Null (foldr KeyMap.delete fullJSON ["lastRunStartedAt", "lastRunCompletedAt", "stopReason"])
eventJSON = KeyMap.fromList ["type" .= String "loop_state_changed", "loopState" .= fullJSON]
