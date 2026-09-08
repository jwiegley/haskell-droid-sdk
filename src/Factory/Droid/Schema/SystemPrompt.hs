{-# LANGUAGE OverloadedStrings #-}

-- | System-prompt configuration for initialization. Preset normalization follows
-- CLI protocol 1.201.1; the supplied 1.205.0 schema describes canonical output.
module Factory.Droid.Schema.SystemPrompt
  ( SystemPromptConfig,
    customSystemPrompt,
    appendedSystemPrompt,
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON (isEcmaWhitespace, rejectUnknownFields)

-- | A replacement prompt or the built-in Droid prompt with appended content.
-- Private constructors exclude whitespace-only content; text is not trimmed.
data SystemPromptConfig = CustomPrompt !Text | AppendedPrompt !Text
  deriving stock (Eq)

instance Show SystemPromptConfig where
  show _ = "SystemPromptConfig <redacted>"

customSystemPrompt :: Text -> Maybe SystemPromptConfig
customSystemPrompt text = CustomPrompt text <$ guard (hasContent text)

appendedSystemPrompt :: Text -> Maybe SystemPromptConfig
appendedSystemPrompt text = AppendedPrompt text <$ guard (hasContent text)

instance FromJSON SystemPromptConfig where
  parseJSON (String text) = maybe (fail "System prompt content must not be empty") pure (customSystemPrompt text)
  parseJSON (Object fields) = do
    rejectUnknownFields ["type", "preset", "append"] fields
    text <- fields .: "append"
    maybe (fail "System prompt append content must not be empty") pure (appendedSystemPrompt text)
  parseJSON _ = fail "Expected system prompt text or a preset object"

instance ToJSON SystemPromptConfig where
  toJSON (CustomPrompt text) = String text
  toJSON (AppendedPrompt text) = object ["type" .= String "preset", "preset" .= String "droid", "append" .= text]

hasContent :: Text -> Bool
hasContent = Text.any (not . isEcmaWhitespace)
