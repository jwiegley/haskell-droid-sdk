{-# LANGUAGE OverloadedStrings #-}

-- | Token-usage wire records for Factory protocol 1.205.0.
-- The schema uses JSON numbers, not bounded or nonnegative integers; Scientific
-- retains their numeric values without binary floating-point rounding.
module Factory.Droid.Schema.Usage
  ( TokenUsage (..),
    sumTokenUsage,
    LastCallTokenUsage (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    withObject,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Factory.Droid.Internal.JSON
  ( additionalFields,
    objectWithAdditionalFields,
    optionalField,
  )

-- | Complete cumulative usage. Factory credits may be omitted, but not null.
-- Additional properties are preserved without overriding named fields.
data TokenUsage = TokenUsage
  { usageInputTokens :: !Scientific,
    usageOutputTokens :: !Scientific,
    usageCacheCreationTokens :: !Scientific,
    usageCacheReadTokens :: !Scientific,
    usageThinkingTokens :: !Scientific,
    usageFactoryCredits :: !(Maybe Scientific),
    usageAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON TokenUsage where
  parseJSON = withObject "TokenUsage" $ \fields ->
    TokenUsage
      <$> fields .: "inputTokens"
      <*> fields .: "outputTokens"
      <*> fields .: "cacheCreationTokens"
      <*> fields .: "cacheReadTokens"
      <*> fields .: "thinkingTokens"
      <*> fields .:! "factoryCredits"
      <*> pure (additionalFields usageKeys fields)

instance ToJSON TokenUsage where
  toJSON usage =
    objectWithAdditionalFields
      usageKeys
      (usageAdditionalFields usage)
      ( [ "inputTokens" .= usageInputTokens usage,
          "outputTokens" .= usageOutputTokens usage,
          "cacheCreationTokens" .= usageCacheCreationTokens usage,
          "cacheReadTokens" .= usageCacheReadTokens usage,
          "thinkingTokens" .= usageThinkingTokens usage
        ]
          <> optionalField "factoryCredits" (usageFactoryCredits usage)
      )

-- | Sum known counters exactly. Empty input yields zero, including credits;
-- absent credits contribute zero. Unknown extensions are not summed.
sumTokenUsage :: [TokenUsage] -> TokenUsage
sumTokenUsage = foldl' add (TokenUsage 0 0 0 0 0 (Just 0) mempty)
  where
    add left right = TokenUsage (usageInputTokens left + usageInputTokens right) (usageOutputTokens left + usageOutputTokens right) (usageCacheCreationTokens left + usageCacheCreationTokens right) (usageCacheReadTokens left + usageCacheReadTokens right) (usageThinkingTokens left + usageThinkingTokens right) (Just (fromMaybe 0 (usageFactoryCredits left) + fromMaybe 0 (usageFactoryCredits right))) mempty

-- | Latest provider usage for the context and compaction meter. This shape
-- occurs in load-session results and session-token-usage notifications.
data LastCallTokenUsage = LastCallTokenUsage
  { lastCallInputTokens :: !Scientific,
    lastCallCacheReadTokens :: !Scientific,
    lastCallOutputTokens :: !(Maybe Scientific),
    lastCallAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON LastCallTokenUsage where
  parseJSON = withObject "LastCallTokenUsage" $ \fields ->
    LastCallTokenUsage
      <$> fields .: "inputTokens"
      <*> fields .: "cacheReadTokens"
      <*> fields .:! "outputTokens"
      <*> pure (additionalFields lastCallKeys fields)

instance ToJSON LastCallTokenUsage where
  toJSON usage =
    objectWithAdditionalFields
      lastCallKeys
      (lastCallAdditionalFields usage)
      ( [ "inputTokens" .= lastCallInputTokens usage,
          "cacheReadTokens" .= lastCallCacheReadTokens usage
        ]
          <> optionalField "outputTokens" (lastCallOutputTokens usage)
      )

usageKeys :: [Key]
usageKeys =
  [ "inputTokens",
    "outputTokens",
    "cacheCreationTokens",
    "cacheReadTokens",
    "thinkingTokens",
    "factoryCredits"
  ]

lastCallKeys :: [Key]
lastCallKeys = ["inputTokens", "cacheReadTokens", "outputTokens"]
