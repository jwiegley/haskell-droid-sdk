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
import Factory.Droid.Internal.JSON (rejectUnknownFields)

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

-- ECMAScript /\S/ differs from Haskell/Python whitespace at NEL and BOM.
hasContent :: Text -> Bool
hasContent = Text.any (\char -> char `notElem` ("\t\n\v\f\r \xa0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\xfeff" :: String))
