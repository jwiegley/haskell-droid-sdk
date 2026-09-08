{-# LANGUAGE OverloadedStrings #-}

-- Shared enum and open/closed record checks for non-nullable declared fields.
-- These structural checks supplement goldens; they are not a Draft-07 validator.
module SchemaTest (enumSchemaTest, nonNullableRecordTests, closedRecordTests, redactedRecordTests, redactedClosedRecordTests, schemaAt, schemaIndex, rejects) where

import Control.Monad (foldM, forM_)
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
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

schemaIndex :: Int -> Value -> Either String Value
schemaIndex index (Array values) = case drop index (toList values) of
  value : _ -> Right value
  [] -> Left "Schema array item missing"
schemaIndex _ _ = Left "Expected schema array"

redactedRecordTests :: forall a. (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> Either String Value -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
redactedRecordTests name schema full minimal fullJSON minimalJSON setExtras =
  testGroup
    (show name)
    [ nonNullableRecordTests name schema full minimal fullJSON minimalJSON setExtras,
      redactedValuesTest [full, minimal, setExtras (KeyMap.singleton "fixture" (String "not-a-real-secret")) full]
    ]

redactedClosedRecordTests :: forall a. (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> Either String Value -> a -> a -> Object -> Object -> TestTree
redactedClosedRecordTests name schema full minimal fullJSON minimalJSON =
  testGroup
    (show name)
    [ closedRecordTests name schema full minimal fullJSON minimalJSON,
      redactedValuesTest [full, minimal]
    ]

redactedValuesTest :: forall a. (Show a, Typeable a) => [a] -> TestTree
redactedValuesTest values = testCase "Show redacts supplied record values" $ do
  let expected = show (typeRep (Proxy @a)) <> " <redacted>"
  forM_ values $ \value -> show value @?= expected

enumSchemaTest :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => String -> Either String Value -> Proxy a -> TestTree
enumSchemaTest name schema _ = testCase name $ do
  expected <- either assertFailure pure schema
  let values = [minBound .. maxBound] :: [a]
  toJSON values @?= expected
  forM_ values $ \value -> eitherDecode (encode value) @?= Right value
  forM_ [String "future", Null, Bool False, Number 0, Object mempty, Array mempty] $ rejects (Proxy @a)

nonNullableRecordTests ::
  forall a.
  (Eq a, Show a, FromJSON a, ToJSON a) =>
  Key ->
  Either String Value ->
  a ->
  a ->
  Object ->
  Object ->
  (Object -> a -> a) ->
  TestTree
nonNullableRecordTests name schema full minimal fullJSON minimalJSON setExtras =
  testGroup (show name) $
    recordShapeTests schema full minimal fullJSON minimalJSON
      <> [ testCase "unknown nested fields survive round trips" $ do
             let extra = object ["nested" .= [Null, Bool True]]
                 value = Object (KeyMap.insert "future" extra fullJSON)
                 expected = setExtras (KeyMap.singleton "future" extra) full
             fromJSON value @?= Success expected
             toJSON expected @?= value,
           testCase "extensions cannot override declared or omitted fields" $
             forM_ (KeyMap.keys fullJSON) $ \key ->
               toJSON (setExtras (KeyMap.singleton key (String "injected")) minimal) @?= Object minimalJSON
         ]

closedRecordTests :: forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Key -> Either String Value -> a -> a -> Object -> Object -> TestTree
closedRecordTests name schema full minimal fullJSON minimalJSON =
  testGroup (show name) $
    recordShapeTests schema full minimal fullJSON minimalJSON
      <> [ testCase "schema explicitly forbids additional properties" $
             (schema >>= schemaAt ["additionalProperties"]) @?= Right (Bool False),
           testCase "unknown nested fields are rejected" $
             rejects (Proxy @a) (Object (KeyMap.insert "future" (object ["nested" .= [Null, Bool True]]) fullJSON))
         ]

recordShapeTests :: forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Either String Value -> a -> a -> Object -> Object -> [TestTree]
recordShapeTests schema full minimal fullJSON minimalJSON =
  [ testCase "fixture covers all declared fields" $ do
      (properties, _) <- either assertFailure pure shape
      sort (KeyMap.keys properties) @?= sort (KeyMap.keys fullJSON),
    testCase "full and minimal goldens encode and decode without loss" $
      forM_ [(full, fullJSON), (minimal, minimalJSON)] $ \(value, fields) -> do
        fromJSON (Object fields) @?= Success value
        toJSON value @?= Object fields
        eitherDecode (encode value) @?= Right value,
    testCase "all required fields are enforced" $ do
      (_, required) <- either assertFailure pure shape
      forM_ required $ \key -> rejects (Proxy @a) (Object (KeyMap.delete key fullJSON)),
    testCase "schema literal fields cannot be substituted" $ do
      (properties, _) <- either assertFailure pure shape
      forM_ (KeyMap.toList properties) $ \(key, definition) ->
        case definition of
          Object rules | KeyMap.member "const" rules -> rejects (Proxy @a) (Object (KeyMap.insert key (String "different") fullJSON))
          _ -> pure (),
    testCase "explicit null is rejected for every declared field" $
      forM_ (KeyMap.keys fullJSON) $
        \key -> rejects (Proxy @a) (Object (KeyMap.insert key Null fullJSON)),
    testCase "non-object input is rejected" $
      forM_ [Null, Bool True, Number 1, String "record", Array mempty] $
        rejects (Proxy @a)
  ]
  where
    shape =
      schema
        >>= parseEither
          ( withObject "schema definition" $ \fields -> do
              properties <- fields .: "properties"
              required <- fields .:? "required" .!= []
              pure (properties :: Object, required)
          )

schemaAt :: [Key] -> Value -> Either String Value
schemaAt keys value = foldM (\node key -> parseEither (withObject "schema node" (.: key)) node) value keys

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of
  Error _ -> pure ()
  Success _ -> assertFailure "Invalid JSON was accepted"
