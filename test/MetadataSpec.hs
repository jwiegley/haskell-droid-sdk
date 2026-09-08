{-# LANGUAGE OverloadedStrings #-}

module MetadataSpec (metadataTests) where

import Control.Monad (forM_)
import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Factory.Droid.Schema.Metadata
import SchemaTest (enumSchemaTest, redactedRecordTests, rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

metadataTests :: Value -> TestTree
metadataTests schema =
  testGroup
    "RPC trace and attribution metadata"
    [ enumSchemaTest "SDK languages match the schema" (definition "SdkClientMetadataSchema" >>= schemaAt ["properties", "language", "enum"]) (Proxy @SdkLanguage),
      enumSchemaTest "non-SDK clients match the schema" (attributionBranch 1 >>= schemaAt ["properties", "client", "enum"]) (Proxy @NonSdkClient),
      redactedRecordTests "SdkClientMetadataSchema" (definition "SdkClientMetadataSchema") metadata metadata metadataJSON metadataJSON (\extras value -> value {sdkMetadataAdditionalFields = extras}),
      redactedRecordTests "ClientRequestAttributionSchema/sdk" (attributionBranch 0) sdk sdk sdkJSON sdkJSON (\extras _ -> SdkAttribution metadata extras),
      redactedRecordTests "ClientRequestAttributionSchema/non-sdk" (attributionBranch 1) nonSdk nonSdk nonSdkJSON nonSdkJSON (\extras _ -> NonSdkAttribution ClientCli extras),
      redactedRecordTests "TraceContextMetaSchema" (definition "TraceContextMetaSchema") trace emptyTrace traceJSON mempty (\extras value -> value {traceAdditionalFields = extras}),
      testCase "SDK version domain is the supplied pattern and length range" $ do
        (definition "SdkClientMetadataSchema" >>= schemaAt ["properties", "version"]) @?= Right (object ["type" .= String "string", "minLength" .= (1 :: Int), "maxLength" .= (64 :: Int), "pattern" .= String "^[A-Za-z0-9.+-]+$"])
        forM_ ["a", "0.7.0", "+-.", Text.replicate 64 "Z"] $ \text -> do
          value <- maybe (assertFailure "Valid version rejected") pure (mkSdkVersion text)
          sdkVersionText value @?= text
          toJSON value @?= String text
          eitherDecode (encode value) @?= Right value
        forM_ ["", Text.replicate 65 "a", " 1", "1 ", "1_0", "1/0", "1\n", "1\r", "سلام", "１", "😀"] $ \text -> do
          mkSdkVersion text @?= Nothing
          rejects (Proxy @SdkVersion) (String text)
        forM_ [Null, Bool True, Number 1, Array mempty, Object mempty] $ rejects (Proxy @SdkVersion),
      testCase "every ASCII character follows the version alphabet" $
        forM_ ['\0' .. '\127'] $ \char ->
          isJust (mkSdkVersion (Text.singleton char)) @?= (char `elem` (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ".+-")),
      testCase "attribution dispatch requires a declared client and SDK metadata" $ do
        forM_ [Null, Number 1, String "future", String "SDK"] $ \client ->
          rejects (Proxy @ClientRequestAttribution) (object ["client" .= client, "sdk" .= metadata])
        forM_ [Null, Bool False, String "python", object []] $ \value ->
          rejects (Proxy @ClientRequestAttribution) (object ["client" .= String "sdk", "sdk" .= value])
        rejects (Proxy @SdkLanguage) (String "haskell"),
      testCase "non-SDK sdk extension stays untyped and is never promoted to identity" $
        forM_ [minBound .. maxBound] $ \client ->
          forM_ [Null, Bool True, String "extension", object ["language" .= String "future"]] $ \value -> do
            let attribution = NonSdkAttribution client (KeyMap.singleton "sdk" value)
            fromJSON (object ["client" .= client, "sdk" .= value]) @?= Success attribution
            eitherDecode (encode attribution) @?= Right attribution,
      testCase "trace text has no invented W3C or nonempty constraint" $
        forM_ ["", "unvalidated trace", "سلام\n😀"] $ \text -> do
          let value = emptyTrace {traceParent = Just text, traceState = Just text}
          fromJSON (object ["traceparent" .= text, "tracestate" .= text]) @?= Success value
          eitherDecode (encode value) @?= Right value
    ]
  where
    definition name = schemaAt ["definitions", name] schema
    attributionBranch index = definition "ClientRequestAttributionSchema" >>= schemaAt ["anyOf"] >>= schemaIndex index
    version = case mkSdkVersion "0.7.0+fixture" of
      Just value -> value
      Nothing -> error "Invalid SDK version fixture"
    metadata = SdkClientMetadata SdkTypeScript version mempty
    metadataJSON = KeyMap.fromList ["language" .= String "typescript", "version" .= String "0.7.0+fixture"]
    sdk = SdkAttribution metadata mempty
    sdkJSON = KeyMap.fromList ["client" .= String "sdk", "sdk" .= Object metadataJSON]
    nonSdk = NonSdkAttribution ClientCli mempty
    nonSdkJSON = KeyMap.singleton "client" (String "cli")
    trace = TraceContextMeta (Just "fixture-parent") (Just "fixture-state") (Just sdk) mempty
    emptyTrace = TraceContextMeta Nothing Nothing Nothing mempty
    traceJSON = KeyMap.fromList ["traceparent" .= String "fixture-parent", "tracestate" .= String "fixture-state", "requestAttribution" .= Object sdkJSON]
