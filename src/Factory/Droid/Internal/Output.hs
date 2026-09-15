{-# LANGUAGE OverloadedStrings #-}

module Factory.Droid.Internal.Output
  ( DroidOutput,
    DroidOutputError (..),
    DroidOutputResult (..),
    rawDroidOutput,
    jsonDroidOutput,
    outputWireFormat,
    adaptOutput,
    adaptOutputData,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), decodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Internal.Stream (DroidResult (..))
import Factory.Droid.Schema.Control (OutputFormat (..))

-- | An object-rooted wire schema paired with local output decoding.
-- The schema is supplied explicitly; it is not derived from a Haskell type.
data DroidOutput a = DroidOutput !OutputFormat !(Object -> Parser a)

instance Show (DroidOutput a) where
  show _ = "DroidOutput <redacted>"

-- | Local preparation/adaptation errors. Explicit decoder diagnostics can
-- contain private input; Show never includes them.
data DroidOutputError = DroidOutputSchemaNotObject | DroidOutputMissing | DroidOutputInvalid !Text
  deriving stock (Eq)

instance Show DroidOutputError where
  show _ = "DroidOutputError <redacted>"

instance Exception DroidOutputError

-- | Independent wire and local outcomes. Right means successful decoding,
-- not successful turn completion; inspect outputTurnResult as well.
data DroidOutputResult a = DroidOutputResult
  { outputTurnResult :: !DroidResult,
    outputValue :: !(Either DroidOutputError a)
  }
  deriving stock (Eq)

instance Show (DroidOutputResult a) where
  show _ = "DroidOutputResult <redacted>"

-- | Require an object result without local JSON Schema keyword validation.
rawDroidOutput :: Object -> Either DroidOutputError (DroidOutput Object)
rawDroidOutput schema = prepareOutput schema pure

-- | Decode an object result with the caller's FromJSON instance. This does not
-- establish that the supplied schema and decoder describe identical contracts.
jsonDroidOutput :: (FromJSON a) => Object -> Either DroidOutputError (DroidOutput a)
jsonDroidOutput schema = prepareOutput schema (parseJSON . Object)

prepareOutput :: Object -> (Object -> Parser a) -> Either DroidOutputError (DroidOutput a)
prepareOutput schema parser = case KeyMap.lookup "type" schema of
  Just (String "object") -> Right (DroidOutput (OutputFormat schema mempty) parser)
  _ -> Left DroidOutputSchemaNotObject

outputWireFormat :: DroidOutput a -> OutputFormat
outputWireFormat (DroidOutput format _) = format

adaptOutput :: DroidOutput a -> DroidResult -> DroidOutputResult a
adaptOutput output result =
  let (raw, decoded) = adaptOutputData output (resultStructuredOutput result) (resultText result)
   in DroidOutputResult (result {resultStructuredOutput = raw}) decoded

adaptOutputData :: DroidOutput a -> Maybe Object -> Text -> (Maybe Object, Either DroidOutputError a)
adaptOutputData (DroidOutput _ parser) reported text =
  let raw = reported <|> decodeStrict' (Text.encodeUtf8 text)
      decoded = case raw of
        Nothing -> Left DroidOutputMissing
        Just fields -> either (Left . DroidOutputInvalid . Text.pack) Right (parseEither parser fields)
   in (raw, decoded)
