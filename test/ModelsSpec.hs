{-# LANGUAGE OverloadedStrings #-}

module ModelsSpec (modelTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON,
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
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific)
import Factory.Droid.Schema.Enums
import Factory.Droid.Schema.Models
import SchemaTest (nonNullableRecordTests, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

modelTests :: Value -> TestTree
modelTests schema =
  testGroup
    "Model catalog"
    [ testCase "golden metadata covers every declared field" $
        case metadataSchemaKeys schema of
          Left err -> assertFailure err
          Right keys -> sort keys @?= sort (KeyMap.keys metadataObject),
      testCase "complete metadata encodes to canonical wire names" $
        toJSON metadata @?= Object metadataObject,
      testCase "complete metadata decodes to typed fields" $
        fromJSON (Object metadataObject) @?= Success metadata,
      testCase "precise numeric multipliers survive JSON bytes" $
        eitherDecode (encode (Object metadataObject)) @?= Right metadata,
      testCase "every required field is enforced"
        $ forM_
          [ "id",
            "displayName",
            "shortDisplayName",
            "modelProvider",
            "supportedReasoningEfforts",
            "defaultReasoningEffort"
          ]
        $ \key -> rejects (Proxy @ModelMetadata) (Object (KeyMap.delete key metadataObject)),
      testCase "explicit null is rejected for non-nullable fields" $
        forM_ (KeyMap.keys metadataObject) $ \key ->
          rejects (Proxy @ModelMetadata) (Object (KeyMap.insert key Null metadataObject)),
      testCase "absent optional fields remain absent and isCustom defaults false" $ do
        fromJSON (Object minimalObject) @?= Success minimalMetadata
        toJSON minimalMetadata @?= Object (KeyMap.insert "isCustom" (Bool False) minimalObject),
      testCase "unknown constrained catalog values are rejected"
        $ forM_
          [ ("kind", String "future-kind"),
            ("tier", String "future-tier"),
            ("modelProvider", String "future-provider"),
            ("defaultReasoningEffort", String "future-effort"),
            ("supportedReasoningEfforts", toJSON [String "future-effort"])
          ]
        $ \(key, value) -> rejects (Proxy @ModelMetadata) (Object (KeyMap.insert key value metadataObject)),
      testCase "model kind and tier codecs agree with inline schema enums" $ do
        inlineEnums <- either assertFailure pure (metadataInlineEnums schema)
        inlineEnums @?= (map toJSON [ConcreteModel, RouterModel], map toJSON [StandardTier, PremiumTier, OverageTier])
        forM_ [ConcreteModel, RouterModel] $ \value -> fromJSON (toJSON value) @?= Success value
        forM_ [StandardTier, PremiumTier, OverageTier] $ \value -> fromJSON (toJSON value) @?= Success value,
      testCase "metadata extensions round-trip without loss" $ do
        let fields = KeyMap.insert "future" extension metadataObject
            expected = metadata {metadataAdditionalFields = KeyMap.singleton "future" extension}
        fromJSON (Object fields) @?= Success expected
        toJSON expected @?= Object fields,
      testCase "extensions cannot override any declared metadata field" $
        case metadataSchemaKeys schema of
          Left err -> assertFailure err
          Right keys -> forM_ keys $ \key -> do
            let injected = minimalMetadata {metadataAdditionalFields = KeyMap.singleton key (String "injected")}
            toJSON injected @?= toJSON minimalMetadata,
      testCase "enabled status defaults from absence and encodes explicitly" $ do
        fromJSON (Object metadataObject) @?= Success enabledModel
        fromJSON (Object (KeyMap.insert "disabled" (Bool False) metadataObject)) @?= Success enabledModel
        toJSON enabledModel @?= Object (KeyMap.insert "disabled" (Bool False) metadataObject),
      testCase "disabled models require a reason" $ do
        let fields = KeyMap.insert "disabledReason" (String "not available") (KeyMap.insert "disabled" (Bool True) metadataObject)
        fromJSON (Object fields) @?= Success disabledModel
        toJSON disabledModel @?= Object fields
        rejects (Proxy @ModelInfo) (Object (KeyMap.insert "disabled" (Bool True) metadataObject)),
      testCase "enabled models forbid even a null disabledReason" $
        forM_ [Null, String "not available"] $ \reason ->
          rejects (Proxy @ModelInfo) (Object (KeyMap.insert "disabledReason" reason metadataObject)),
      testCase "disabled status and reason retain their JSON types" $ do
        rejects (Proxy @ModelInfo) (Object (KeyMap.insert "disabled" Null metadataObject))
        rejects (Proxy @ModelInfo) (Object (KeyMap.insert "disabled" (String "true") metadataObject))
        rejects (Proxy @ModelInfo) (Object (KeyMap.insert "disabledReason" Null (KeyMap.insert "disabled" (Bool True) metadataObject))),
      testCase "metadata extensions cannot forge model availability" $ do
        let injected = metadata {metadataAdditionalFields = KeyMap.fromList [("disabled", Bool True), ("disabledReason", String "injected")]}
        toJSON (ModelInfo injected ModelEnabled) @?= toJSON enabledModel
        toJSON (ModelInfo injected (ModelDisabled "not available")) @?= toJSON disabledModel,
      testCase "includeDisabled omission differs from explicit false" $ do
        toJSON (ListModelsOptions Nothing mempty) @?= object []
        toJSON (ListModelsOptions (Just False) mempty) @?= object ["includeDisabled" .= False]
        toJSON (ListModelsOptions (Just True) mempty) @?= object ["includeDisabled" .= True]
        fromJSON (object []) @?= Success (ListModelsOptions Nothing mempty)
        fromJSON (object ["includeDisabled" .= False]) @?= Success (ListModelsOptions (Just False) mempty)
        rejects (Proxy @ListModelsOptions) (object ["includeDisabled" .= Null]),
      testCase "options preserve extensions but reserve includeDisabled" $ do
        let options = ListModelsOptions Nothing (KeyMap.singleton "future" extension)
        fromJSON (object ["future" .= extension]) @?= Success options
        toJSON options @?= object ["future" .= extension]
        toJSON (ListModelsOptions Nothing (KeyMap.singleton "includeDisabled" (Bool True))) @?= object [],
      testCase "catalog results preserve models and extensions" $ do
        let result = ListModelsResult [enabledModel, disabledModel] (KeyMap.singleton "future" extension)
            expected = object ["models" .= [enabledModel, disabledModel], "future" .= extension]
        toJSON result @?= expected
        fromJSON expected @?= Success result
        eitherDecode (encode result) @?= Right result
        rejects (Proxy @ListModelsResult) (object [])
        rejects (Proxy @ListModelsResult) (object ["models" .= Null])
        toJSON (ListModelsResult [] (KeyMap.singleton "models" (String "injected"))) @?= object ["models" .= ([] :: [Value])],
      nonNullableRecordTests
        "ModelFallbackSchema"
        (schemaAt ["definitions", "ModelFallbackSchema"] schema)
        fallback
        fallback
        fallbackJSON
        fallbackJSON
        (\extras value -> value {fallbackAdditionalFields = extras}),
      nonNullableRecordTests
        "MissionModelSettingsSchema"
        (schemaAt ["definitions", "MissionModelSettingsSchema"] schema)
        missionSettings
        emptyMissionSettings
        missionJSON
        mempty
        (\extras value -> value {missionSettingsAdditionalFields = extras}),
      testCase "fallback reasons retain their declared enum" $ do
        forM_ [FallbackCustomModelNotConfigured, FallbackModelNotAvailableForOrg] $ \reason -> do
          let value = fallback {fallbackReason = reason}
          fromJSON (toJSON value) @?= Success value
        rejects (Proxy @ModelFallback) (Object (KeyMap.insert "reason" (String "future") fallbackJSON)),
      testCase "mission settings preserve explicit false without inventing defaults" $ do
        let value = emptyMissionSettings {missionSkipScrutiny = Just False, missionSkipUserTesting = Just False}
        toJSON value @?= object ["skipScrutiny" .= False, "skipUserTesting" .= False]
        fromJSON (toJSON value) @?= Success value
        fromJSON (object []) @?= Success emptyMissionSettings,
      testCase "mission reasoning and flags retain JSON types" $ do
        forM_ ["workerReasoningEffort", "validationWorkerReasoningEffort"] $ \key ->
          rejects (Proxy @MissionModelSettings) (Object (KeyMap.insert key (String "future") missionJSON))
        forM_ ["skipScrutiny", "skipUserTesting"] $ \key ->
          rejects (Proxy @MissionModelSettings) (Object (KeyMap.insert key (String "false") missionJSON)),
      testCase "compaction model branches match the immutable schema" $ do
        alternatives <- either assertFailure pure (schemaAt ["definitions", "CompactionModelSchema", "anyOf"] schema)
        case fromJSON alternatives :: Result [KeyMap.KeyMap Value] of
          Success [current, builtin, custom] -> do
            KeyMap.lookup "const" current @?= Just (toJSON currentCompactionModel)
            KeyMap.lookup "enum" builtin @?= Just (toJSON builtinCompactionModels)
            custom @?= KeyMap.fromList ["type" .= String "string", "pattern" .= String "^custom:.+$"]
          _ -> assertFailure "Unexpected compaction-model schema branches",
      testCase "all declared compaction models decode and preserve their spelling" $
        forM_ (currentCompactionModel : builtinCompactionModels) $ \value -> do
          let text = compactionModelText value
          mkCompactionModel text @?= Just value
          fromJSON (String text) @?= Success value
          toJSON value @?= String text
          eitherDecode (encode value) @?= Right value,
      testCase "custom compaction names follow ECMAScript non-line-terminator matching" $
        forM_ ["custom:model", "custom: ", "custom:😀", "custom:\x85", "custom:\0"] $ \text ->
          case mkCompactionModel text of
            Nothing -> assertFailure "Valid custom selector rejected"
            Just value -> do
              compactionModelText value @?= text
              fromJSON (String text) @?= Success value
              toJSON value @?= String text,
      testCase "invalid custom selectors and unknown model names are rejected" $ do
        forM_ ["", "custom:", "Custom:model", " custom:model", "custom:x\n", "custom:x\r", "custom:x\r\n", "custom:x\x2028", "custom:x\x2029", "custom:x\nmore", "future-model", "auto", "shield-risk"] $ \text -> do
          mkCompactionModel text @?= Nothing
          rejects (Proxy @CompactionModel) (String text)
        forM_ [Null, Number 1, Bool True, Object mempty, Array mempty] $ rejects (Proxy @CompactionModel),
      testCase "Bedrock request metadata matches the string-valued object schema" $ do
        definition <- either assertFailure pure (schemaAt ["definitions", "CustomModelBedrockRequestMetadataSchema"] schema)
        definition @?= object ["type" .= String "object", "additionalProperties" .= object ["type" .= String "string"]]
        let values = [mempty, KeyMap.fromList [("", ""), ("label", "value"), ("نام", "مقدار")]] :: [BedrockRequestMetadata]
        forM_ values $ \value -> do
          fromJSON (toJSON value) @?= Success value
          eitherDecode (encode value) @?= Right value
        forM_ [Null, Bool True, Number 1, Array mempty, Object mempty] $ \value ->
          rejects (Proxy @BedrockRequestMetadata) (object ["field" .= value])
        forM_ [Null, String "object", Array mempty] $ rejects (Proxy @BedrockRequestMetadata)
    ]
  where
    fallback = ModelFallback "requested-model" FallbackCustomModelNotConfigured mempty
    fallbackJSON = KeyMap.fromList ["requestedModel" .= String "requested-model", "reason" .= String "custom_model_not_configured"]
    emptyMissionSettings = MissionModelSettings Nothing Nothing Nothing Nothing Nothing Nothing mempty
    missionSettings = MissionModelSettings (Just "worker") (Just ReasoningLow) (Just "validator") (Just ReasoningHigh) (Just False) (Just True) mempty
    missionJSON = KeyMap.fromList ["workerModel" .= String "worker", "workerReasoningEffort" .= String "low", "validationWorkerModel" .= String "validator", "validationWorkerReasoningEffort" .= String "high", "skipScrutiny" .= False, "skipUserTesting" .= True]

metadata :: ModelMetadata
metadata =
  ModelMetadata
    { metadataId = "model-alpha",
      metadataDisplayName = "Model Alpha",
      metadataShortDisplayName = "Alpha",
      metadataProvider = ProviderFactory,
      metadataSupportedReasoningEfforts = [ReasoningLow, ReasoningHigh],
      metadataDefaultReasoningEffort = ReasoningHigh,
      metadataIsCustom = True,
      metadataNoImageSupport = Just False,
      metadataSupportsImageGeneration = Just True,
      metadataTier = Just PremiumTier,
      metadataTokenMultiplier = Just (scientific 12345678901234567890123456789 (-19)),
      metadataPromoLabel = Just "limited",
      metadataKind = Just RouterModel,
      metadataVariantBadge = Just "beta",
      metadataAdditionalFields = mempty
    }

minimalMetadata :: ModelMetadata
minimalMetadata =
  metadata
    { metadataIsCustom = False,
      metadataNoImageSupport = Nothing,
      metadataSupportsImageGeneration = Nothing,
      metadataTier = Nothing,
      metadataTokenMultiplier = Nothing,
      metadataPromoLabel = Nothing,
      metadataKind = Nothing,
      metadataVariantBadge = Nothing
    }

metadataObject :: KeyMap.KeyMap Value
metadataObject =
  KeyMap.union
    minimalObject
    ( KeyMap.fromList
        [ ("isCustom", Bool True),
          ("noImageSupport", Bool False),
          ("supportsImageGeneration", Bool True),
          ("tier", String "premium"),
          ("tokenMultiplier", Number (scientific 12345678901234567890123456789 (-19))),
          ("promoLabel", String "limited"),
          ("kind", String "router"),
          ("variantBadge", String "beta")
        ]
    )

minimalObject :: KeyMap.KeyMap Value
minimalObject =
  KeyMap.fromList
    [ ("id", String "model-alpha"),
      ("displayName", String "Model Alpha"),
      ("shortDisplayName", String "Alpha"),
      ("modelProvider", String "factory"),
      ("supportedReasoningEfforts", toJSON [String "low", String "high"]),
      ("defaultReasoningEffort", String "high")
    ]

enabledModel :: ModelInfo
enabledModel = ModelInfo metadata ModelEnabled

disabledModel :: ModelInfo
disabledModel = ModelInfo metadata (ModelDisabled "not available")

extension :: Value
extension = object ["nested" .= [Bool True, Null, object ["value" .= String "retained"]]]

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of
  Error _ -> pure ()
  Success _ -> assertFailure "Invalid JSON was accepted"

metadataSchemaKeys :: Value -> Either String [Key]
metadataSchemaKeys = parseEither $ withObject "schema" $ \root -> do
  definitions <- root .: "definitions"
  definition <- definitions .: "ModelMetadataSchema"
  properties <- definition .: "properties"
  pure (KeyMap.keys (properties :: KeyMap.KeyMap Value))

metadataInlineEnums :: Value -> Either String ([Value], [Value])
metadataInlineEnums = parseEither $ withObject "schema" $ \root -> do
  definitions <- root .: "definitions"
  definition <- definitions .: "ModelMetadataSchema"
  properties <- definition .: "properties"
  kind <- properties .: "kind"
  tier <- properties .: "tier"
  (,) <$> kind .: "enum" <*> tier .: "enum"
