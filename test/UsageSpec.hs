{-# LANGUAGE OverloadedStrings #-}

module UsageSpec (usageTests) where

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
    (.:),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific)
import Factory.Droid.Schema.Usage
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, chooseInt, forAll, testProperty, (===))

usageTests :: Value -> Value -> TestTree
usageTests shared droid =
  testGroup
    "Token usage"
    [ numericRecordTests "TokenUsage" cumulativeSchema cumulative cumulativeObject,
      numericRecordTests "LastCallTokenUsage" lastCallSchema lastCall lastCallObject,
      testCase "both inline last-call definitions describe the same shape" $
        lastCallSchema @?= schemaAt ["definitions", "SessionTokenUsageChangedNotificationSchema", "properties", "lastCallTokenUsage"] droid,
      testCase "factoryCredits may be absent, but not null" $ do
        let fields = KeyMap.delete "factoryCredits" cumulativeObject
            expected = cumulative {usageFactoryCredits = Nothing}
        fromJSON (Object fields) @?= Success expected
        toJSON expected @?= Object fields
        rejects (Proxy @TokenUsage) (Object (KeyMap.insert "factoryCredits" Null fields)),
      testCase "outputTokens may be absent from last-call usage, but not null" $ do
        let fields = KeyMap.delete "outputTokens" lastCallObject
            expected = lastCall {lastCallOutputTokens = Nothing}
        fromJSON (Object fields) @?= Success expected
        toJSON expected @?= Object fields
        rejects (Proxy @LastCallTokenUsage) (Object (KeyMap.insert "outputTokens" Null fields)),
      testCase "cumulative extension keys cannot override or reintroduce known fields" $
        forM_ (KeyMap.keys cumulativeObject) $ \key -> do
          let base = cumulative {usageFactoryCredits = Nothing}
              injected = base {usageAdditionalFields = KeyMap.singleton key (String "injected")}
          toJSON injected @?= toJSON base,
      testCase "last-call extension keys cannot override or reintroduce known fields" $
        forM_ (KeyMap.keys lastCallObject) $ \key -> do
          let base = lastCall {lastCallOutputTokens = Nothing}
              injected = base {lastCallAdditionalFields = KeyMap.singleton key (String "injected")}
          toJSON injected @?= toJSON base
    ]
  where
    cumulativeSchema = schemaAt ["definitions", "TokenUsageSchema"] shared
    lastCallSchema = schemaAt ["definitions", "LoadSessionResultSchema", "properties", "lastCallTokenUsage"] droid

numericRecordTests ::
  forall a.
  (Eq a, Show a, FromJSON a, ToJSON a) =>
  String ->
  Either String Value ->
  a ->
  Object ->
  TestTree
numericRecordTests name schema expected fixture =
  testGroup
    name
    [ testCase "golden fixture covers every declared field" $ do
        properties <- either assertFailure pure (schema >>= schemaAt ["properties"])
        case properties of
          Object fields -> sort (KeyMap.keys fields) @?= sort (KeyMap.keys fixture)
          _ -> assertFailure "Schema properties are not an object",
      testCase "golden fixture decodes to every typed field" $
        fromJSON (Object fixture) @?= Success expected,
      testCase "every typed field has its canonical wire name" $
        toJSON expected @?= Object fixture,
      testCase "exact decimal and large integer values survive JSON bytes" $
        eitherDecode (encode (Object fixture)) @?= Right expected,
      testCase "all schema-required fields are enforced" $ do
        required <- either assertFailure pure (schema >>= schemaAt ["required"])
        case fromJSON required :: Result [Key] of
          Error err -> assertFailure err
          Success keys -> forM_ keys $ \key -> rejects (Proxy @a) (Object (KeyMap.delete key fixture)),
      testCase "non-number fields, including null, are rejected" $
        forM_ (KeyMap.keys fixture) $ \key ->
          forM_ [Null, Bool True, String "1", Array mempty, Object mempty] $ \value ->
            rejects (Proxy @a) (Object (KeyMap.insert key value fixture)),
      testCase "non-object records are rejected" $
        forM_ [Null, Bool True, Number 1, String "usage", Array mempty] $
          rejects (Proxy @a),
      testCase "open records preserve unknown values" $ do
        let value = Object (KeyMap.insert "future" (object ["nested" .= [Null, Bool True]]) fixture)
        case fromJSON value :: Result a of
          Error err -> assertFailure err
          Success decoded -> toJSON decoded @?= value,
      testProperty "finite JSON numbers round-trip without integer coercion" $
        forAll arbitrary $ \coefficient ->
          forAll (chooseInt (-8, 8)) $ \decimalExponent ->
            let value = Object (KeyMap.map (const (Number (scientific coefficient decimalExponent))) fixture)
             in case fromJSON value :: Result a of
                  Error err -> Left err === Right value
                  Success decoded -> eitherDecode (encode decoded) === Right value
    ]

cumulative :: TokenUsage
cumulative =
  TokenUsage
    { usageInputTokens = 11,
      usageOutputTokens = 9007199254740993,
      usageCacheCreationTokens = 3,
      usageCacheReadTokens = 7,
      usageThinkingTokens = 5,
      usageFactoryCredits = Just 0.1234567890123456789,
      usageAdditionalFields = mempty
    }

cumulativeObject :: Object
cumulativeObject =
  KeyMap.fromList
    [ ("inputTokens", Number 11),
      ("outputTokens", Number 9007199254740993),
      ("cacheCreationTokens", Number 3),
      ("cacheReadTokens", Number 7),
      ("thinkingTokens", Number 5),
      ("factoryCredits", Number 0.1234567890123456789)
    ]

lastCall :: LastCallTokenUsage
lastCall =
  LastCallTokenUsage
    { lastCallInputTokens = 13,
      lastCallCacheReadTokens = 17,
      lastCallOutputTokens = Just 0.1234567890123456789,
      lastCallAdditionalFields = mempty
    }

lastCallObject :: Object
lastCallObject =
  KeyMap.fromList
    [ ("inputTokens", Number 13),
      ("cacheReadTokens", Number 17),
      ("outputTokens", Number 0.1234567890123456789)
    ]

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of
  Error _ -> pure ()
  Success _ -> assertFailure "Invalid JSON was accepted"

schemaAt :: [Key] -> Value -> Either String Value
schemaAt keys value =
  foldM (\node key -> parseEither (withObject "schema node" (.: key)) node) value keys
