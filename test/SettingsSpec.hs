{-# LANGUAGE OverloadedStrings #-}

module SettingsSpec (settingsTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Enums (AutonomyLevel (..), DroidInteractionMode (..), ReasoningEffort (..))
import Factory.Droid.Schema.Settings
import SchemaTest (rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

settingsTests :: Value -> TestTree
settingsTests schema =
  testGroup
    "Operational settings and tool-query bodies"
    [ testCase "empty patches and queries omit every optional field" $ do
        toJSON emptySettingsUpdate @?= object []
        fromJSON (object []) @?= Success emptySettingsUpdate
        toJSON defaultListToolsOptions @?= object []
        fromJSON (object []) @?= Success defaultListToolsOptions,
      testCase "full session settings cover initialize/load fields without invented defaults" $ do
        let wire = object ["modelId" .= String "model", "reasoningEffort" .= String "high", "autonomyMode" .= String "normal", "interactionMode" .= String "spec", "autonomyLevel" .= String "off", "availableAutonomyLevels" .= [String "off", String "low"], "specModeModelId" .= String "spec", "specModeReasoningEffort" .= String "low", "missionSettings" .= object [], "tags" .= ([] :: [Value]), "sandbox" .= object ["enabled" .= False], "compactionThresholdCheckEnabled" .= False, "toolExecutionMode" .= String "direct_only", "allowSubagentsInScripts" .= False, "systemPrompt" .= String "Private instruction", "additionalToolIds" .= [String "Read", String "Read"], "enabledToolIds" .= ([] :: [Value]), "disabledToolIds" .= [String "Execute"], "restrictToolIds" .= [String " Read "]]
        roundTrip (Proxy @SessionSettings) wire
        ownsSchemaFields "SessionSettingsSchema" wire
        roundTrip (Proxy @SessionSettings) (Object minimalSnapshot),
      testCase "full settings require valid model and reasoning and reject non-null optional violations" $ do
        forM_ ["modelId", "reasoningEffort"] $ \key -> do
          rejects (Proxy @SessionSettings) (Object (KeyMap.delete key minimalSnapshot))
          rejects (Proxy @SessionSettings) (Object (KeyMap.insert key Null minimalSnapshot))
        forM_ ["autonomyMode", "specModeModelId", "specModeReasoningEffort", "missionSettings", "tags", "sandbox", "compactionThresholdCheckEnabled", "toolExecutionMode", "allowSubagentsInScripts", "systemPrompt", "enabledToolIds"] $ \key ->
          rejects (Proxy @SessionSettings) (Object (KeyMap.insert key Null minimalSnapshot)),
      testCase "full settings retain enum fallback and explicit empty autonomy list" $ do
        case fromJSON (Object (minimalSnapshot <> KeyMap.fromList ["interactionMode" .= Null, "autonomyLevel" .= String "invalid", "availableAutonomyLevels" .= [String "off", String "invalid"]])) of
          Error problem -> assertFailure problem
          Success (value :: SessionSettings) -> toJSON value @?= Object minimalSnapshot
        roundTrip (Proxy @SessionSettings) (Object (KeyMap.insert "availableAutonomyLevels" (toJSON ([] :: [Value])) minimalSnapshot)),
      testCase "full settings reuse nested validation and preserve unknown extensions" $ do
        forM_ ["sandbox" .= object [], "systemPrompt" .= String "  ", "missionSettings" .= object ["workerReasoningEffort" .= String "invalid"], "toolExecutionMode" .= String "invalid", "tags" .= [object ["name" .= String ""]]] $ \(key, value) ->
          rejects (Proxy @SessionSettings) (Object (KeyMap.insert key value minimalSnapshot))
        roundTrip (Proxy @SessionSettings) (Object (KeyMap.insert "future" (object ["nested" .= Null]) minimalSnapshot)),
      testCase "full settings own declared keys and redact Show without hiding explicit data" $
        case fromJSON (Object minimalSnapshot) of
          Error problem -> assertFailure problem
          Success value -> do
            toJSON (value {settingsAdditionalFields = KeyMap.fromList ["modelId" .= String "injected", "systemPrompt" .= String "injected", "future" .= True]}) @?= Object (KeyMap.insert "future" (Bool True) minimalSnapshot)
            show value @?= "SessionSettings <redacted>"
            settingsModel value @?= "private-model",
      testCase "all settings fields round trip through their supplied schema surface" $ do
        let wire = object ["modelId" .= String "model", "reasoningEffort" .= String "high", "autonomyMode" .= String "normal", "interactionMode" .= String "spec", "autonomyLevel" .= String "off", "specModeModelId" .= Null, "specModeReasoningEffort" .= String "low", "missionSettings" .= object ["workerModel" .= String "worker"], "tags" .= [object ["name" .= String "tag", "metadata" .= object ["source" .= String "fixture"]]], "compactionTokenLimit" .= Number 42.5, "compactionThresholdCheckEnabled" .= False, "additionalToolIds" .= [String "Read", String "Read"], "enabledToolIds" .= ([] :: [Value]), "disabledToolIds" .= [String "Execute"], "restrictToolIds" .= [String " Read ", String ""]]
        roundTrip (Proxy @UpdateSessionSettingsParams) wire
        ownsSchemaFields "UpdateSessionSettingsRequestParamsSchema" wire,
      testCase "all hypothetical tool-query fields round trip" $ do
        let wire = object ["modelId" .= String "model", "autonomyMode" .= String "auto-low", "interactionMode" .= String "auto", "autonomyLevel" .= String "low", "specModeModelId" .= Null, "additionalToolIds" .= [String "Read"], "enabledToolIds" .= ([] :: [Value]), "disabledToolIds" .= [String "Execute"], "restrictToolIds" .= [String "Read"], "skipPermissionsUnsafe" .= True, "depth" .= Number 0]
        roundTrip (Proxy @ListToolsOptions) wire
        ownsSchemaFields "ListToolsRequestParamsSchema" wire,
      testCase "spec resets distinguish omission, null and a value" $ do
        fromJSON (object ["specModeModelId" .= Null]) @?= Success (emptySettingsUpdate {updateSettingsSpecModel = Just Nothing})
        fromJSON (object ["specModeReasoningEffort" .= Null]) @?= Success (emptySettingsUpdate {updateSettingsSpecReasoning = Just Nothing})
        toJSON (emptySettingsUpdate {updateSettingsSpecModel = Just (Just ""), updateSettingsSpecReasoning = Just (Just ReasoningHigh)}) @?= object ["specModeModelId" .= String "", "specModeReasoningEffort" .= String "high"]
        fromJSON (object ["specModeModelId" .= Null]) @?= Success (defaultListToolsOptions {toolQuerySpecModel = Just Nothing}),
      testGroup
        "invalid-value fallbacks belong only to selected enum fields"
        [ testCase (show key <> "/" <> show bad) $ do
            fromJSON (object [key .= bad]) @?= Success emptySettingsUpdate
            fromJSON (object [key .= bad]) @?= Success defaultListToolsOptions
        | key <- ["interactionMode", "autonomyLevel"],
          bad <- [Null, String "unknown", Bool False, object [], Number 7]
        ],
      testCase "valid enums are retained rather than replaced by fallbacks" $ do
        forM_ [minBound .. maxBound] $ \mode ->
          fromJSON (object ["interactionMode" .= mode]) @?= Success (emptySettingsUpdate {updateSettingsMode = Just (mode :: DroidInteractionMode)})
        forM_ [minBound .. maxBound] $ \level ->
          fromJSON (object ["autonomyLevel" .= level]) @?= Success (defaultListToolsOptions {toolQueryAutonomy = Just (level :: AutonomyLevel)}),
      testCase "invalid fallback fields are removed from extension ownership" $ do
        fromJSON (object ["interactionMode" .= String "unknown", "autonomyLevel" .= Null, "future" .= Number 1]) @?= Success (emptySettingsUpdate {updateSettingsAdditionalFields = KeyMap.singleton "future" (Number 1)})
        toJSON (emptySettingsUpdate {updateSettingsAdditionalFields = KeyMap.fromList ["modelId" .= String "injected", "specModeModelId" .= Null, "restrictToolIds" .= [String "injected"], "future" .= Number 1]}) @?= object ["future" .= Number 1]
        toJSON (defaultListToolsOptions {toolQueryAdditionalFields = KeyMap.fromList ["modelId" .= String "injected", "depth" .= Number 3, "future" .= Number 1]}) @?= object ["future" .= Number 1],
      testCase "ordinary optional fields retain strict null/type checks" $ do
        forM_ ["modelId", "reasoningEffort", "autonomyMode", "missionSettings", "tags", "compactionTokenLimit", "compactionThresholdCheckEnabled", "additionalToolIds", "enabledToolIds", "disabledToolIds", "restrictToolIds"] $ \key ->
          rejects (Proxy @UpdateSessionSettingsParams) (object [key .= Null])
        forM_ ["modelId", "autonomyMode", "depth", "skipPermissionsUnsafe", "additionalToolIds"] $ \key ->
          rejects (Proxy @ListToolsOptions) (object [key .= Null])
        rejects (Proxy @UpdateSessionSettingsParams) (object ["specModeReasoningEffort" .= String "unknown"])
        rejects (Proxy @UpdateSessionSettingsParams) (object ["modelId" .= Number 1])
        rejects (Proxy @ListToolsOptions) (object ["skipPermissionsUnsafe" .= String "true"]),
      testCase "tool lists preserve empty, duplicate and whitespace values" $ do
        let policy = ToolPolicy (Just []) (Just ["Read", "Read"]) (Just [""]) (Just [" Read "])
            wire = object ["additionalToolIds" .= ([] :: [Value]), "enabledToolIds" .= [String "Read", String "Read"], "disabledToolIds" .= [String ""], "restrictToolIds" .= [String " Read "]]
        fromJSON wire @?= Success (emptySettingsUpdate {updateSettingsToolPolicy = policy})
        fromJSON wire @?= Success (defaultListToolsOptions {toolQueryPolicy = policy})
        forM_ [String "Read", toJSON [Bool True]] $ \bad -> do
          rejects (Proxy @UpdateSessionSettingsParams) (object ["enabledToolIds" .= bad])
          rejects (Proxy @ListToolsOptions) (object ["enabledToolIds" .= bad]),
      testCase "compaction keeps the wire number domain, not an invented integer bound" $
        forM_ [-1.25, 0, 999999999999999999999999.5] $ \value ->
          fromJSON (object ["compactionTokenLimit" .= Number value]) @?= Success (emptySettingsUpdate {updateSettingsCompactionTokenLimit = Just value}),
      testCase "tool depth is a nonnegative integer without an Int truncation" $ do
        forM_ [0, 999999999999999999999999] $ \value ->
          fromJSON (object ["depth" .= value]) @?= Success (defaultListToolsOptions {toolQueryDepth = Just value})
        forM_ [Number (-1), Number 0.5, String "1"] $ \bad -> rejects (Proxy @ListToolsOptions) (object ["depth" .= bad]),
      testCase "nested mission settings and tags reuse their validation" $ do
        rejects (Proxy @UpdateSessionSettingsParams) (object ["missionSettings" .= object ["workerReasoningEffort" .= String "unknown"]])
        rejects (Proxy @UpdateSessionSettingsParams) (object ["tags" .= [object ["name" .= String ""]]])
        rejects (Proxy @UpdateSessionSettingsParams) (object ["tags" .= [object ["name" .= String "tag", "metadata" .= object ["x" .= Bool True]]]]),
      testCase "legacy modes retain their exact literals and reject unknowns" $ do
        map toJSON [minBound .. maxBound :: LegacyAutonomyMode] @?= map String ["normal", "spec", "auto-low", "auto-medium", "auto-high"]
        forM_ [minBound .. maxBound :: LegacyAutonomyMode] $ \mode -> fromJSON (toJSON mode) @?= Success mode
        rejects (Proxy @LegacyAutonomyMode) (String "auto")
        rejects (Proxy @LegacyAutonomyMode) Null,
      testCase "settings notifications keep their actual partial payload contract" $ do
        let wire = object ["type" .= String "settings_updated", "requestId" .= String "request", "settings" .= object ["autonomyMode" .= String "normal", "interactionMode" .= String "spec", "autonomyLevel" .= String "off", "availableAutonomyLevels" .= [String "off", String "low", String "off"], "modelId" .= String "model", "reasoningEffort" .= String "high", "specModeModelId" .= String "", "specModeReasoningEffort" .= String "low", "additionalToolIds" .= ([] :: [Value]), "enabledToolIds" .= [String "Read"], "disabledToolIds" .= ([] :: [Value]), "restrictToolIds" .= [String "Read"], "missionSettings" .= object [], "tags" .= ([] :: [Value]), "compactionThresholdCheckEnabled" .= False, "future" .= Number 1], "outer" .= True]
        roundTrip (Proxy @SettingsUpdated) wire
        fromJSON (object ["type" .= String "settings_updated", "settings" .= object []]) @?= Success (SettingsUpdated Nothing emptySettingsChange mempty),
      testCase "available autonomy fallback applies to the whole field without inventing levels" $ do
        fromJSON (object ["availableAutonomyLevels" .= ([] :: [Value])]) @?= Success (emptySettingsChange {changedSettingsAvailableAutonomy = Just []})
        fromJSON (object ["availableAutonomyLevels" .= [String "low", String "invalid"]]) @?= Success emptySettingsChange
        fromJSON (object ["interactionMode" .= Null, "autonomyLevel" .= False, "availableAutonomyLevels" .= Null]) @?= Success emptySettingsChange,
      testCase "notification spec fields are non-null unlike mutation patches" $ do
        rejects (Proxy @SettingsChange) (object ["specModeModelId" .= Null])
        rejects (Proxy @SettingsChange) (object ["specModeReasoningEffort" .= Null])
        rejects (Proxy @SettingsUpdated) (object ["type" .= String "settings_updated", "settings" .= Null])
        rejects (Proxy @SettingsUpdated) (object ["type" .= String "settings_updated"])
        rejects (Proxy @SettingsUpdated) (object ["type" .= String "settings_updated", "settings" .= object [], "requestId" .= Null])
        rejects (Proxy @SettingsUpdated) (object ["type" .= String "other", "settings" .= object []]),
      testCase "settings notification field ownership and redaction are retained" $ do
        let change = emptySettingsChange {changedSettingsAdditionalFields = KeyMap.fromList ["modelId" .= String "injected", "availableAutonomyLevels" .= Null, "compactionTokenLimit" .= Number 8]}
            event = SettingsUpdated Nothing change (KeyMap.fromList ["type" .= String "injected", "requestId" .= Null, "settings" .= Null, "future" .= True])
        toJSON event @?= object ["type" .= String "settings_updated", "settings" .= object ["compactionTokenLimit" .= Number 8], "future" .= True]
        show event @?= "SettingsUpdated <redacted>"
        show change @?= "SettingsChange <redacted>",
      testCase "setting and tool-query records redact payloads in Show" $ do
        show (emptyToolPolicy {policyRestrictedTools = Just ["private"]}) @?= "ToolPolicy <redacted>"
        show (emptySettingsUpdate {updateSettingsModel = Just "private"}) @?= "UpdateSessionSettingsParams <redacted>"
        show (defaultListToolsOptions {toolQueryModel = Just "private"}) @?= "ListToolsOptions <redacted>"
    ]
  where
    minimalSnapshot = KeyMap.fromList ["modelId" .= String "private-model", "reasoningEffort" .= String "low"]
    ownsSchemaFields name (Object fields) = case schemaAt ["definitions", name, "properties"] schema of
      Right (Object expected) -> sort (KeyMap.keys fields) @?= sort (KeyMap.keys expected)
      _ -> assertFailure "Missing settings schema properties"
    ownsSchemaFields _ _ = assertFailure "Expected object fixture"

roundTrip :: forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = case fromJSON value of
  Error problem -> assertFailure problem
  Success (decoded :: a) -> toJSON decoded @?= value
