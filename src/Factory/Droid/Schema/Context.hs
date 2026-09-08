{-# LANGUAGE OverloadedStrings #-}

-- | Context-usage snapshots for Factory protocol 1.205.0. Counts retain the
-- declared JSON-number domain; decoding neither measures nor compacts context.
module Factory.Droid.Schema.Context
  ( ContextAccuracy (..),
    ContextCategoryColorKey (..),
    DroidLocation (..),
    ContextStats (..),
    ContextBreakdownCategory (..),
    LocatedContextEntry (..),
    ContextBreakdownSkillEntry,
    ContextBreakdownDroidEntry,
    ContextBreakdownMcpServerEntry (..),
    GetContextBreakdownResult (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String), withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Enums (SkillLocation)

-- | Whether the peer reports an exact measurement or an estimate.
data ContextAccuracy = ContextExact | ContextEstimated
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ContextAccuracy where
  parseJSON = withText "ContextAccuracy" $ \case
    "exact" -> pure ContextExact
    "estimated" -> pure ContextEstimated
    _ -> fail "Unknown context accuracy"

instance ToJSON ContextAccuracy where
  toJSON ContextExact = String "exact"
  toJSON ContextEstimated = String "estimated"

-- | Semantic display categories, not arbitrary colors or CSS values.
data ContextCategoryColorKey
  = ColorSystemPrompt
  | ColorSystemTools
  | ColorMcpTools
  | ColorUserInfo
  | ColorAgentsMd
  | ColorCustomAgents
  | ColorSkills
  | ColorMessages
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ContextCategoryColorKey where
  parseJSON = withText "ContextCategoryColorKey" $ \case
    "systemPrompt" -> pure ColorSystemPrompt
    "systemTools" -> pure ColorSystemTools
    "mcpTools" -> pure ColorMcpTools
    "userInfo" -> pure ColorUserInfo
    "agentsMd" -> pure ColorAgentsMd
    "customAgents" -> pure ColorCustomAgents
    "skills" -> pure ColorSkills
    "messages" -> pure ColorMessages
    _ -> fail "Unknown context category color key"

instance ToJSON ContextCategoryColorKey where
  toJSON =
    String . \case
      ColorSystemPrompt -> "systemPrompt"
      ColorSystemTools -> "systemTools"
      ColorMcpTools -> "mcpTools"
      ColorUserInfo -> "userInfo"
      ColorAgentsMd -> "agentsMd"
      ColorCustomAgents -> "customAgents"
      ColorSkills -> "skills"
      ColorMessages -> "messages"

-- | Droid locations exclude the builtin and automation skill locations.
data DroidLocation = DroidProject | DroidPersonal
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON DroidLocation where
  parseJSON = withText "DroidLocation" $ \case
    "project" -> pure DroidProject
    "personal" -> pure DroidPersonal
    _ -> fail "Unknown droid location"

instance ToJSON DroidLocation where
  toJSON DroidProject = String "project"
  toJSON DroidPersonal = String "personal"

-- | Aggregate usage. The timestamp is an unparsed wire string; no arithmetic
-- relationship between used, remaining and limit is required by this schema.
data ContextStats = ContextStats
  { contextUsed :: !Scientific,
    contextRemaining :: !Scientific,
    contextLimit :: !Scientific,
    contextAccuracy :: !ContextAccuracy,
    contextUpdatedAt :: !Text,
    contextStatsAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ContextStats where
  parseJSON = withObject "ContextStats" $ \fields ->
    ContextStats <$> fields .: "used" <*> fields .: "remaining" <*> fields .: "limit" <*> fields .: "accuracy" <*> fields .: "updatedAt" <*> pure (additionalFields statsKeys fields)

instance ToJSON ContextStats where
  toJSON stats = objectWithAdditionalFields statsKeys (contextStatsAdditionalFields stats) ["used" .= contextUsed stats, "remaining" .= contextRemaining stats, "limit" .= contextLimit stats, "accuracy" .= contextAccuracy stats, "updatedAt" .= contextUpdatedAt stats]

-- | A named usage category and its declared display color key.
data ContextBreakdownCategory = ContextBreakdownCategory
  { contextCategoryName :: !Text,
    contextCategoryTokens :: !Scientific,
    contextCategoryColor :: !ContextCategoryColorKey,
    contextCategoryAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ContextBreakdownCategory where
  parseJSON = withObject "ContextBreakdownCategory" $ \fields ->
    ContextBreakdownCategory <$> fields .: "name" <*> fields .: "tokens" <*> fields .: "colorKey" <*> pure (additionalFields categoryKeys fields)

instance ToJSON ContextBreakdownCategory where
  toJSON category = objectWithAdditionalFields categoryKeys (contextCategoryAdditionalFields category) ["name" .= contextCategoryName category, "tokens" .= contextCategoryTokens category, "colorKey" .= contextCategoryColor category]

-- | The shared shape of skill and droid usage, parameterized only by their
-- distinct location enums. Every entry retains its own extension fields.
data LocatedContextEntry location = LocatedContextEntry
  { contextEntryName :: !Text,
    contextEntryLocation :: !location,
    contextEntryTokens :: !Scientific,
    contextEntryAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance (FromJSON location) => FromJSON (LocatedContextEntry location) where
  parseJSON = withObject "LocatedContextEntry" $ \fields ->
    LocatedContextEntry <$> fields .: "name" <*> fields .: "location" <*> fields .: "tokens" <*> pure (additionalFields locatedKeys fields)

instance (ToJSON location) => ToJSON (LocatedContextEntry location) where
  toJSON entry = objectWithAdditionalFields locatedKeys (contextEntryAdditionalFields entry) ["name" .= contextEntryName entry, "location" .= contextEntryLocation entry, "tokens" .= contextEntryTokens entry]

-- | Context attributed to a skill at any supported skill location.
type ContextBreakdownSkillEntry = LocatedContextEntry SkillLocation

-- | Context attributed to a project or personal droid.
type ContextBreakdownDroidEntry = LocatedContextEntry DroidLocation

-- | Context attributed to an MCP server. Tool count is a JSON number, not a
-- bounded integer; no connection or tool discovery occurs during decoding.
data ContextBreakdownMcpServerEntry = ContextBreakdownMcpServerEntry
  { contextMcpName :: !Text,
    contextMcpToolCount :: !Scientific,
    contextMcpTokens :: !Scientific,
    contextMcpAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ContextBreakdownMcpServerEntry where
  parseJSON = withObject "ContextBreakdownMcpServerEntry" $ \fields ->
    ContextBreakdownMcpServerEntry <$> fields .: "name" <*> fields .: "toolCount" <*> fields .: "tokens" <*> pure (additionalFields mcpKeys fields)

instance ToJSON ContextBreakdownMcpServerEntry where
  toJSON entry = objectWithAdditionalFields mcpKeys (contextMcpAdditionalFields entry) ["name" .= contextMcpName entry, "toolCount" .= contextMcpToolCount entry, "tokens" .= contextMcpTokens entry]

-- | A detailed context snapshot. Required lists may be empty; absence of
-- last-call compaction tokens is distinct from a reported zero.
data GetContextBreakdownResult = GetContextBreakdownResult
  { breakdownModelId :: !Text,
    breakdownModelDisplayName :: !Text,
    breakdownContextBudget :: !Scientific,
    breakdownUsedTokens :: !Scientific,
    breakdownFreeTokens :: !Scientific,
    breakdownCategories :: ![ContextBreakdownCategory],
    breakdownSkills :: ![ContextBreakdownSkillEntry],
    breakdownMcpServers :: ![ContextBreakdownMcpServerEntry],
    breakdownDroids :: ![ContextBreakdownDroidEntry],
    breakdownLastCallCompactionTokens :: !(Maybe Scientific),
    breakdownAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON GetContextBreakdownResult where
  parseJSON = withObject "GetContextBreakdownResult" $ \fields ->
    GetContextBreakdownResult
      <$> fields .: "modelId"
      <*> fields .: "modelDisplayName"
      <*> fields .: "contextBudget"
      <*> fields .: "usedTokens"
      <*> fields .: "freeTokens"
      <*> fields .: "categories"
      <*> fields .: "skills"
      <*> fields .: "mcpServers"
      <*> fields .: "droids"
      <*> fields .:! "lastCallCompactionTokens"
      <*> pure (additionalFields breakdownKeys fields)

instance ToJSON GetContextBreakdownResult where
  toJSON result =
    objectWithAdditionalFields breakdownKeys (breakdownAdditionalFields result) $
      [ "modelId" .= breakdownModelId result,
        "modelDisplayName" .= breakdownModelDisplayName result,
        "contextBudget" .= breakdownContextBudget result,
        "usedTokens" .= breakdownUsedTokens result,
        "freeTokens" .= breakdownFreeTokens result,
        "categories" .= breakdownCategories result,
        "skills" .= breakdownSkills result,
        "mcpServers" .= breakdownMcpServers result,
        "droids" .= breakdownDroids result
      ]
        <> optionalField "lastCallCompactionTokens" (breakdownLastCallCompactionTokens result)

statsKeys, categoryKeys, locatedKeys, mcpKeys, breakdownKeys :: [Key]
statsKeys = ["used", "remaining", "limit", "accuracy", "updatedAt"]
categoryKeys = ["name", "tokens", "colorKey"]
locatedKeys = ["name", "location", "tokens"]
mcpKeys = ["name", "toolCount", "tokens"]
breakdownKeys = ["modelId", "modelDisplayName", "contextBudget", "usedTokens", "freeTokens", "categories", "skills", "mcpServers", "droids", "lastCallCompactionTokens"]
