{-# LANGUAGE OverloadedStrings #-}

-- | Model-related wire records for Factory protocol 1.205.0.
-- Open objects retain additional fields. Optional non-nullable fields reject
-- explicit null; absent fields are omitted on encoding unless a default applies.
module Factory.Droid.Schema.Models
  ( ModelKind (..),
    ModelTier (..),
    ModelMetadata (..),
    ModelAvailability (..),
    ModelInfo (..),
    ListModelsOptions (..),
    ListModelsResult (..),
    ModelFallback (..),
    MissionModelSettings (..),
    CompactionModel,
    currentCompactionModel,
    builtinCompactionModels,
    mkCompactionModel,
    compactionModelText,
    BedrockRequestMetadata,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    withObject,
    withText,
    (.!=),
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON
  ( additionalFields,
    objectWithAdditionalFields,
    optionalField,
  )
import Factory.Droid.Schema.Enums (ModelFallbackReason, ModelProvider, ReasoningEffort)

-- | CustomModelBedrockRequestMetadataSchema is exactly a string-valued JSON
-- object. Keys and values, including empty strings, are not normalized.
type BedrockRequestMetadata = KeyMap.KeyMap Text

-- | A validated compaction-model selector: current-model, a baselined built-in
-- literal, or the schema's custom-model pattern. This does not establish that
-- a selected model is available to the current user or runtime.
newtype CompactionModel = CompactionModel Text
  deriving stock (Eq, Ord, Show)

-- | Select the model currently used by the session.
currentCompactionModel :: CompactionModel
currentCompactionModel = CompactionModel "current-model"

-- | Every built-in literal in the immutable 1.205.0 schema, in schema order.
builtinCompactionModels :: [CompactionModel]
builtinCompactionModels = map CompactionModel builtinCompactionModelNames

-- | Validate without trimming or case folding. The custom suffix must be
-- nonempty and contain no ECMAScript line terminators, matching @^custom:.+$@.
mkCompactionModel :: Text -> Maybe CompactionModel
mkCompactionModel value
  | value == "current-model" || value `elem` builtinCompactionModelNames = Just (CompactionModel value)
  | otherwise = case Text.stripPrefix "custom:" value of
      Just suffix
        | not (Text.null suffix) && not (Text.any (`elem` ['\n', '\r', '\x2028', '\x2029']) suffix) -> Just (CompactionModel value)
      _ -> Nothing

-- | Recover the original selector text.
compactionModelText :: CompactionModel -> Text
compactionModelText (CompactionModel value) = value

instance FromJSON CompactionModel where
  parseJSON = withText "CompactionModel" $ \value ->
    maybe (fail "Unknown compaction model") pure (mkCompactionModel value)

instance ToJSON CompactionModel where
  toJSON = toJSON . compactionModelText

builtinCompactionModelNames :: [Text]
builtinCompactionModelNames =
  [ "claude-3-5-sonnet-20241022",
    "claude-3-7-sonnet-20250219",
    "claude-sonnet-4-20250514",
    "claude-opus-4-1-20250805",
    "claude-3-5-haiku-20241022",
    "claude-sonnet-4-5-20250929",
    "claude-opus-4-5-20251101",
    "claude-haiku-4-5-20251001",
    "claude-sonnet-4-6",
    "claude-opus-4-6",
    "claude-opus-4-6-fast",
    "claude-opus-4-7",
    "claude-opus-4-7-fast",
    "claude-opus-4-8",
    "claude-opus-4-8-fast",
    "claude-opus-5",
    "claude-opus-5-fast",
    "claude-sonnet-5",
    "claude-fable-5",
    "claude-fable-5.1",
    "atlas-07-21",
    "aster-07-15",
    "gpt-5-2025-08-07",
    "gpt-5-mini-2025-08-07",
    "gpt-5-nano-2025-08-07",
    "gpt-5-codex",
    "gpt-5.1",
    "gpt-5.1-codex",
    "gpt-5.1-codex-max",
    "gpt-5.2",
    "gpt-5.2-codex",
    "gpt-5.3-codex",
    "gpt-5.3-codex-fast",
    "gpt-5.4",
    "gpt-5.4-fast",
    "gpt-5.4-mini",
    "gpt-5.4-mini-fast",
    "gpt-5.5",
    "gpt-5.5-fast",
    "gpt-5.5-pro",
    "gpt-5.6-sol",
    "gpt-5.6-sol-fast",
    "gpt-5.6-terra",
    "gpt-5.6-luna",
    "gpt-6-astra",
    "gpt-image-2",
    "gpt-transcribe",
    "gpt-4o-mini-transcribe",
    "gemini-2.5-flash",
    "gemini-2.5-pro",
    "gemini-3-pro-preview",
    "gemini-3-flash-preview",
    "gemini-3.1-pro-preview",
    "gemini-3.5-flash",
    "gemini-3.6-flash",
    "gemini-3.7-flash",
    "gemini-3.8-flash",
    "gemini-3-pro-image-preview",
    "garnet-07-15",
    "grok-4.5",
    "grok-4.6",
    "glm-4.6",
    "glm-4.7",
    "kimi-k2.5",
    "kimi-k2.6",
    "kimi-k2.7-code",
    "kimi-k3",
    "deepseek-v4-flash-0731",
    "deepseek-v4-pro",
    "minimax-m2.5",
    "minimax-m2.7",
    "minimax-m3",
    "glm-5",
    "glm-5.1",
    "glm-5.2",
    "glm-5.2-fast",
    "glm-5.3",
    "glm-5.3-flash",
    "inkling",
    "nemotron-3-ultra",
    "olive-05-22",
    "oriel-06-01",
    "ocelot-06-01",
    "okappa-alpha",
    "omaffa-alpha",
    "titan-02-12",
    "aspen-05-15",
    "almond-05-27",
    "anise-06-16",
    "olm-03-05",
    "orbit-04-09",
    "oxide-06-01",
    "oxbow-06-01",
    "owl-06-21",
    "gantry-05-07",
    "amber-07-09",
    "agate-07-11"
  ]

-- | A concrete model or a router that selects a model for each task.
data ModelKind = ConcreteModel | RouterModel
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToJSON ModelKind where
  toJSON ConcreteModel = String "concrete"
  toJSON RouterModel = String "router"

instance FromJSON ModelKind where
  parseJSON = withText "ModelKind" $ \case
    "concrete" -> pure ConcreteModel
    "router" -> pure RouterModel
    _ -> fail "Unknown model kind"

-- | The service tier reported by the catalog.
data ModelTier = StandardTier | PremiumTier | OverageTier
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToJSON ModelTier where
  toJSON StandardTier = String "standard"
  toJSON PremiumTier = String "premium"
  toJSON OverageTier = String "overage"

instance FromJSON ModelTier where
  parseJSON = withText "ModelTier" $ \case
    "standard" -> pure StandardTier
    "premium" -> pure PremiumTier
    "overage" -> pure OverageTier
    _ -> fail "Unknown model tier"

-- | Metadata common to enabled and disabled catalog entries.
--
-- 'metadataIsCustom' defaults to false on decoding. Extension fields cannot
-- override any named field, even when that named field is absent.
data ModelMetadata = ModelMetadata
  { metadataId :: !Text,
    metadataDisplayName :: !Text,
    metadataShortDisplayName :: !Text,
    metadataProvider :: !ModelProvider,
    metadataSupportedReasoningEfforts :: ![ReasoningEffort],
    metadataDefaultReasoningEffort :: !ReasoningEffort,
    metadataIsCustom :: !Bool,
    metadataNoImageSupport :: !(Maybe Bool),
    metadataSupportsImageGeneration :: !(Maybe Bool),
    metadataTier :: !(Maybe ModelTier),
    metadataTokenMultiplier :: !(Maybe Scientific),
    metadataPromoLabel :: !(Maybe Text),
    metadataKind :: !(Maybe ModelKind),
    metadataVariantBadge :: !(Maybe Text),
    metadataAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance ToJSON ModelMetadata where
  toJSON metadata =
    objectWithAdditionalFields
      metadataKeys
      (metadataAdditionalFields metadata)
      (metadataFields metadata)

instance FromJSON ModelMetadata where
  parseJSON = withObject "ModelMetadata" $ \fields ->
    ModelMetadata
      <$> fields .: "id"
      <*> fields .: "displayName"
      <*> fields .: "shortDisplayName"
      <*> fields .: "modelProvider"
      <*> fields .: "supportedReasoningEfforts"
      <*> fields .: "defaultReasoningEffort"
      <*> (fields .:! "isCustom" .!= False)
      <*> fields .:! "noImageSupport"
      <*> fields .:! "supportsImageGeneration"
      <*> fields .:! "tier"
      <*> fields .:! "tokenMultiplier"
      <*> fields .:! "promoLabel"
      <*> fields .:! "kind"
      <*> fields .:! "variantBadge"
      <*> pure (additionalFields metadataKeys fields)

-- | Disabled entries require a reason; enabled entries cannot carry one.
data ModelAvailability = ModelEnabled | ModelDisabled !Text
  deriving stock (Eq, Ord, Show)

-- | A catalog entry with its availability invariant represented by a sum type.
-- Additional fields are retained in 'modelMetadata'.
data ModelInfo = ModelInfo
  { modelMetadata :: !ModelMetadata,
    modelAvailability :: !ModelAvailability
  }
  deriving stock (Eq, Show)

instance ToJSON ModelInfo where
  toJSON model =
    objectWithAdditionalFields
      (metadataKeys <> availabilityKeys)
      (metadataAdditionalFields (modelMetadata model))
      (metadataFields (modelMetadata model) <> availabilityFields)
    where
      availabilityFields = case modelAvailability model of
        ModelEnabled -> ["disabled" .= False]
        ModelDisabled reason -> ["disabled" .= True, "disabledReason" .= reason]

instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \fields -> do
    disabled <- fields .:! "disabled" .!= False
    metadata <- parseJSON (Object (additionalFields availabilityKeys fields))
    availability <-
      if disabled
        then ModelDisabled <$> fields .: "disabledReason"
        else
          if KeyMap.member "disabledReason" fields
            then fail "Enabled models must not include disabledReason"
            else pure ModelEnabled
    pure (ModelInfo metadata availability)

-- | Sessionless discovery options. Nothing omits the include-disabled field;
-- Just False is retained as an explicit caller choice.
data ListModelsOptions = ListModelsOptions
  { modelsIncludeDisabled :: !(Maybe Bool),
    modelsOptionsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance ToJSON ListModelsOptions where
  toJSON options =
    objectWithAdditionalFields
      ["includeDisabled"]
      (modelsOptionsAdditionalFields options)
      (optionalField "includeDisabled" (modelsIncludeDisabled options))

instance FromJSON ListModelsOptions where
  parseJSON = withObject "ListModelsOptions" $ \fields ->
    ListModelsOptions
      <$> fields .:! "includeDisabled"
      <*> pure (additionalFields ["includeDisabled"] fields)

-- | The complete model-discovery response.
data ListModelsResult = ListModelsResult
  { catalogModels :: ![ModelInfo],
    catalogAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance ToJSON ListModelsResult where
  toJSON result =
    objectWithAdditionalFields
      ["models"]
      (catalogAdditionalFields result)
      ["models" .= catalogModels result]

instance FromJSON ListModelsResult where
  parseJSON = withObject "ListModelsResult" $ \fields ->
    ListModelsResult
      <$> fields .: "models"
      <*> pure (additionalFields ["models"] fields)

-- | The requested model and the reported reason for using a fallback.
data ModelFallback = ModelFallback
  { fallbackRequestedModel :: !Text,
    fallbackReason :: !ModelFallbackReason,
    fallbackAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ModelFallback where
  parseJSON = withObject "ModelFallback" $ \fields ->
    ModelFallback <$> fields .: "requestedModel" <*> fields .: "reason" <*> pure (additionalFields ["requestedModel", "reason"] fields)

instance ToJSON ModelFallback where
  toJSON fallback = objectWithAdditionalFields ["requestedModel", "reason"] (fallbackAdditionalFields fallback) ["requestedModel" .= fallbackRequestedModel fallback, "reason" .= fallbackReason fallback]

-- | Optional mission model/validation settings. These flags are wire data;
-- parsing does not start a mission or skip any SDK verification step.
data MissionModelSettings = MissionModelSettings
  { missionWorkerModel :: !(Maybe Text),
    missionWorkerReasoningEffort :: !(Maybe ReasoningEffort),
    missionValidationWorkerModel :: !(Maybe Text),
    missionValidationWorkerReasoningEffort :: !(Maybe ReasoningEffort),
    missionSkipScrutiny :: !(Maybe Bool),
    missionSkipUserTesting :: !(Maybe Bool),
    missionSettingsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON MissionModelSettings where
  parseJSON = withObject "MissionModelSettings" $ \fields ->
    MissionModelSettings
      <$> fields .:! "workerModel"
      <*> fields .:! "workerReasoningEffort"
      <*> fields .:! "validationWorkerModel"
      <*> fields .:! "validationWorkerReasoningEffort"
      <*> fields .:! "skipScrutiny"
      <*> fields .:! "skipUserTesting"
      <*> pure (additionalFields missionSettingsKeys fields)

instance ToJSON MissionModelSettings where
  toJSON settings =
    objectWithAdditionalFields missionSettingsKeys (missionSettingsAdditionalFields settings) $
      optionalField "workerModel" (missionWorkerModel settings)
        <> optionalField "workerReasoningEffort" (missionWorkerReasoningEffort settings)
        <> optionalField "validationWorkerModel" (missionValidationWorkerModel settings)
        <> optionalField "validationWorkerReasoningEffort" (missionValidationWorkerReasoningEffort settings)
        <> optionalField "skipScrutiny" (missionSkipScrutiny settings)
        <> optionalField "skipUserTesting" (missionSkipUserTesting settings)

missionSettingsKeys :: [Key]
missionSettingsKeys = ["workerModel", "workerReasoningEffort", "validationWorkerModel", "validationWorkerReasoningEffort", "skipScrutiny", "skipUserTesting"]

metadataKeys :: [Key]
metadataKeys =
  [ "id",
    "displayName",
    "shortDisplayName",
    "modelProvider",
    "supportedReasoningEfforts",
    "defaultReasoningEffort",
    "isCustom",
    "noImageSupport",
    "supportsImageGeneration",
    "tier",
    "tokenMultiplier",
    "promoLabel",
    "kind",
    "variantBadge"
  ]

availabilityKeys :: [Key]
availabilityKeys = ["disabled", "disabledReason"]

metadataFields :: ModelMetadata -> [Pair]
metadataFields metadata =
  [ "id" .= metadataId metadata,
    "displayName" .= metadataDisplayName metadata,
    "shortDisplayName" .= metadataShortDisplayName metadata,
    "modelProvider" .= metadataProvider metadata,
    "supportedReasoningEfforts" .= metadataSupportedReasoningEfforts metadata,
    "defaultReasoningEffort" .= metadataDefaultReasoningEffort metadata,
    "isCustom" .= metadataIsCustom metadata
  ]
    <> optionalField "noImageSupport" (metadataNoImageSupport metadata)
    <> optionalField "supportsImageGeneration" (metadataSupportsImageGeneration metadata)
    <> optionalField "tier" (metadataTier metadata)
    <> optionalField "tokenMultiplier" (metadataTokenMultiplier metadata)
    <> optionalField "promoLabel" (metadataPromoLabel metadata)
    <> optionalField "kind" (metadataKind metadata)
    <> optionalField "variantBadge" (metadataVariantBadge metadata)
