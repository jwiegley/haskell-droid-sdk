{-# LANGUAGE OverloadedStrings #-}

module ContextSpec (contextTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Context
import Factory.Droid.Schema.Enums (SkillLocation (..))
import SchemaTest (enumSchemaTest, nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

contextTests :: Value -> TestTree
contextTests schema =
  testGroup
    "Context snapshots"
    [ records "ContextStatsSchema" stats stats statsJSON statsJSON (\extras value -> value {contextStatsAdditionalFields = extras}),
      records "ContextBreakdownCategorySchema" category category categoryJSON categoryJSON (\extras value -> value {contextCategoryAdditionalFields = extras}),
      records "GetContextBreakdownResultSchema" breakdown (breakdown {breakdownLastCallCompactionTokens = Nothing}) breakdownJSON (KeyMap.delete "lastCallCompactionTokens" breakdownJSON) (\extras value -> value {breakdownAdditionalFields = extras}),
      nonNullableRecordTests "skill entry" (entrySchema "skills") skill skill skillJSON skillJSON (\extras value -> value {contextEntryAdditionalFields = extras}),
      nonNullableRecordTests "droid entry" (entrySchema "droids") droid droid droidJSON droidJSON (\extras value -> value {contextEntryAdditionalFields = extras}),
      nonNullableRecordTests "MCP entry" (entrySchema "mcpServers") mcp mcp mcpJSON mcpJSON (\extras value -> value {contextMcpAdditionalFields = extras}),
      enumSchemaTest "accuracy" (schemaAt ["definitions", "ContextStatsSchema", "properties", "accuracy", "enum"] schema) (Proxy @ContextAccuracy),
      enumSchemaTest "category colors" (schemaAt ["definitions", "ContextBreakdownCategorySchema", "properties", "colorKey", "enum"] schema) (Proxy @ContextCategoryColorKey),
      enumSchemaTest "droid locations" (entrySchema "droids" >>= schemaAt ["properties", "location", "enum"]) (Proxy @DroidLocation),
      testCase "skill and droid entry schemas differ only in location" $ do
        skillSchema <- either assertFailure pure (entrySchema "skills")
        droidSchema <- either assertFailure pure (entrySchema "droids")
        skillProperties <- either assertFailure pure (schemaAt ["properties"] skillSchema)
        droidLocation <- either assertFailure pure (schemaAt ["properties", "location"] droidSchema)
        case (skillSchema, skillProperties) of
          (Object fields, Object properties) -> Object (KeyMap.insert "properties" (Object (KeyMap.insert "location" droidLocation properties)) fields) @?= droidSchema
          _ -> assertFailure "Expected a record schema",
      testCase "skill-only locations cannot enter droid snapshots" $ do
        forM_ [("builtin", SkillBuiltin), ("automation", SkillAutomation)] $ \(wire, location) -> do
          let fields = KeyMap.insert "location" (String wire) skillJSON
          fromJSON (Object fields) @?= Success (skill {contextEntryLocation = location})
          rejects (Proxy @ContextBreakdownDroidEntry) (Object fields),
      testCase "all breakdown arrays are required but may be empty" $ do
        let value = breakdown {breakdownCategories = [], breakdownSkills = [], breakdownMcpServers = [], breakdownDroids = []}
            wire = foldr (\key -> KeyMap.insert key (Array mempty)) breakdownJSON ["categories", "skills", "mcpServers", "droids"]
        fromJSON (Object wire) @?= Success value
        toJSON value @?= Object wire,
      testCase "nested records reject invalid shapes and enums" $ do
        forM_ ["categories", "skills", "mcpServers", "droids"] $ \key ->
          forM_ [Null, object [], String "entry"] $ \bad -> rejects (Proxy @GetContextBreakdownResult) (Object (KeyMap.insert key (toJSON [bad]) breakdownJSON))
        rejects (Proxy @GetContextBreakdownResult) (Object (KeyMap.insert "categories" (toJSON [Object (KeyMap.insert "colorKey" (String "future") categoryJSON)]) breakdownJSON)),
      testCase "unconstrained numbers retain fractions, signs and large magnitudes" $ do
        forM_ [-1.25, 0, 123456789012345678901234567890] $ \number -> do
          let value = ContextStats number number number ContextEstimated "not parsed as a timestamp" mempty
          fromJSON (object ["used" .= number, "remaining" .= number, "limit" .= number, "accuracy" .= String "estimated", "updatedAt" .= String "not parsed as a timestamp"]) @?= Success value
          fromJSON (Object (KeyMap.insert "toolCount" (Number number) mcpJSON)) @?= Success (mcp {contextMcpToolCount = number}),
      testCase "nested extension fields remain local to their entries" $ do
        let extra = KeyMap.singleton "future" (object ["value" .= Null])
            value = breakdown {breakdownSkills = [skill {contextEntryAdditionalFields = extra}]}
            wire = Object (KeyMap.insert "skills" (toJSON [Object (KeyMap.union extra skillJSON)]) breakdownJSON)
        fromJSON wire @?= Success value
        toJSON value @?= wire
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)
    entrySchema key = schemaAt ["definitions", "GetContextBreakdownResultSchema", "properties", key, "items"] schema

stats :: ContextStats
stats = ContextStats (-1.25) 2.5 100 ContextExact "unparsed timestamp" mempty

category :: ContextBreakdownCategory
category = ContextBreakdownCategory "category" 1.25 ColorSystemPrompt mempty

skill :: ContextBreakdownSkillEntry
skill = LocatedContextEntry "skill" SkillBuiltin 2.5 mempty

droid :: ContextBreakdownDroidEntry
droid = LocatedContextEntry "droid" DroidPersonal 3.75 mempty

mcp :: ContextBreakdownMcpServerEntry
mcp = ContextBreakdownMcpServerEntry "server" (-0.5) 4.125 mempty

breakdown :: GetContextBreakdownResult
breakdown = GetContextBreakdownResult "model-id" "model name" 100.5 200.25 (-10.75) [category] [skill] [mcp] [droid] (Just 0) mempty

statsJSON, categoryJSON, skillJSON, droidJSON, mcpJSON, breakdownJSON :: Object
statsJSON = KeyMap.fromList ["used" .= Number (-1.25), "remaining" .= Number 2.5, "limit" .= Number 100, "accuracy" .= String "exact", "updatedAt" .= String "unparsed timestamp"]
categoryJSON = KeyMap.fromList ["name" .= String "category", "tokens" .= Number 1.25, "colorKey" .= String "systemPrompt"]
skillJSON = KeyMap.fromList ["name" .= String "skill", "location" .= String "builtin", "tokens" .= Number 2.5]
droidJSON = KeyMap.fromList ["name" .= String "droid", "location" .= String "personal", "tokens" .= Number 3.75]
mcpJSON = KeyMap.fromList ["name" .= String "server", "toolCount" .= Number (-0.5), "tokens" .= Number 4.125]
breakdownJSON = KeyMap.fromList ["modelId" .= String "model-id", "modelDisplayName" .= String "model name", "contextBudget" .= Number 100.5, "usedTokens" .= Number 200.25, "freeTokens" .= Number (-10.75), "lastCallCompactionTokens" .= Number 0, "categories" .= [Object categoryJSON], "skills" .= [Object skillJSON], "mcpServers" .= [Object mcpJSON], "droids" .= [Object droidJSON]]
