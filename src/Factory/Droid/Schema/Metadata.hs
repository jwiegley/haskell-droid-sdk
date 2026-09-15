{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol 1.205.0 trace and attribution data. The supplied language enum
-- has no Haskell member; native sessions omit optional SDK attribution rather
-- than impersonating another SDK. These codecs retain declared identities
-- for decoding and explicit caller-owned messages.
module Factory.Droid.Schema.Metadata
  ( SdkLanguage (..),
    SdkVersion,
    mkSdkVersion,
    sdkVersionText,
    SdkClientMetadata (..),
    NonSdkClient (..),
    ClientRequestAttribution (..),
    TraceContextMeta (..),
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), withObject, withText, (.:), (.:!), (.=))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Primitives (BoundedText, boundedTextValue, mkBoundedText)

-- | Languages declared by the reference schema, not identities for this port.
data SdkLanguage = SdkTypeScript | SdkPython
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON SdkLanguage where
  parseJSON = withText "SdkLanguage" $ \case
    "typescript" -> pure SdkTypeScript
    "python" -> pure SdkPython
    _ -> fail "Unknown SDK language"

instance ToJSON SdkLanguage where
  toJSON SdkTypeScript = "typescript"
  toJSON SdkPython = "python"

-- | One to 64 ASCII letters, digits, dots, pluses or hyphens. This is not
-- semantic-version parsing; spelling is preserved without normalization.
newtype SdkVersion = SdkVersion (BoundedText 64)
  deriving stock (Eq, Ord, Show)

-- | Validate the exact version domain declared in SdkClientMetadataSchema.
mkSdkVersion :: Text -> Maybe SdkVersion
mkSdkVersion value = do
  guard (not (Text.null value) && Text.all valid value)
  SdkVersion <$> mkBoundedText value
  where
    valid char = isAsciiLower char || isAsciiUpper char || isDigit char || char `elem` (".+-" :: String)

-- | Recover the original version spelling.
sdkVersionText :: SdkVersion -> Text
sdkVersionText (SdkVersion value) = boundedTextValue value

instance FromJSON SdkVersion where
  parseJSON = withText "SdkVersion" $ \value -> maybe (fail "Invalid SDK version") pure (mkSdkVersion value)

instance ToJSON SdkVersion where
  toJSON = toJSON . sdkVersionText

-- | Declared SDK identity data. Show redacts the record; fields and JSON
-- remain explicit, potentially sensitive data rather than verified identity.
data SdkClientMetadata = SdkClientMetadata
  { sdkLanguage :: !SdkLanguage,
    sdkVersion :: !SdkVersion,
    sdkMetadataAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SdkClientMetadata where
  show _ = "SdkClientMetadata <redacted>"

instance FromJSON SdkClientMetadata where
  parseJSON = withObject "SdkClientMetadata" $ \fields ->
    SdkClientMetadata <$> fields .: "language" <*> fields .: "version" <*> pure (additionalFields ["language", "version"] fields)

instance ToJSON SdkClientMetadata where
  toJSON metadata =
    objectWithAdditionalFields
      ["language", "version"]
      (sdkMetadataAdditionalFields metadata)
      ["language" .= sdkLanguage metadata, "version" .= sdkVersion metadata]

-- | The six non-SDK attribution origins declared by the schema.
data NonSdkClient = ClientWebDesktop | ClientWebApp | ClientWebWorkspace | ClientDaemon | ClientCli | ClientBackend
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON NonSdkClient where
  parseJSON = withText "NonSdkClient" $ \case
    "web-desktop" -> pure ClientWebDesktop
    "web-app" -> pure ClientWebApp
    "web-workspace" -> pure ClientWebWorkspace
    "daemon" -> pure ClientDaemon
    "cli" -> pure ClientCli
    "backend" -> pure ClientBackend
    _ -> fail "Unknown non-SDK client"

instance ToJSON NonSdkClient where
  toJSON ClientWebDesktop = "web-desktop"
  toJSON ClientWebApp = "web-app"
  toJSON ClientWebWorkspace = "web-workspace"
  toJSON ClientDaemon = "daemon"
  toJSON ClientCli = "cli"
  toJSON ClientBackend = "backend"

-- | SDK attribution requires metadata; non-SDK attribution does not declare
-- an sdk field, so that key remains an unrestricted extension on that branch.
data ClientRequestAttribution
  = SdkAttribution !SdkClientMetadata !Object
  | NonSdkAttribution !NonSdkClient !Object
  deriving stock (Eq)

instance Show ClientRequestAttribution where
  show _ = "ClientRequestAttribution <redacted>"

instance FromJSON ClientRequestAttribution where
  parseJSON = withObject "ClientRequestAttribution" $ \fields -> do
    client <- fields .: "client"
    if client == ("sdk" :: Text)
      then SdkAttribution <$> fields .: "sdk" <*> pure (additionalFields ["client", "sdk"] fields)
      else NonSdkAttribution <$> parseJSON (toJSON client) <*> pure (additionalFields ["client"] fields)

instance ToJSON ClientRequestAttribution where
  toJSON (SdkAttribution metadata extras) = objectWithAdditionalFields ["client", "sdk"] extras ["client" .= ("sdk" :: Text), "sdk" .= metadata]
  toJSON (NonSdkAttribution client extras) = objectWithAdditionalFields ["client"] extras ["client" .= client]

-- | Optional, non-nullable trace and attribution fields. Trace strings are
-- unrestricted by this schema; decoding neither validates W3C headers nor
-- installs or propagates context. Show redacts the complete record.
data TraceContextMeta = TraceContextMeta
  { traceParent :: !(Maybe Text),
    traceState :: !(Maybe Text),
    traceRequestAttribution :: !(Maybe ClientRequestAttribution),
    traceAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TraceContextMeta where
  show _ = "TraceContextMeta <redacted>"

instance FromJSON TraceContextMeta where
  parseJSON = withObject "TraceContextMeta" $ \fields ->
    TraceContextMeta <$> fields .:! "traceparent" <*> fields .:! "tracestate" <*> fields .:! "requestAttribution" <*> pure (additionalFields ["traceparent", "tracestate", "requestAttribution"] fields)

instance ToJSON TraceContextMeta where
  toJSON metadata =
    objectWithAdditionalFields ["traceparent", "tracestate", "requestAttribution"] (traceAdditionalFields metadata) $
      optionalField "traceparent" (traceParent metadata)
        <> optionalField "tracestate" (traceState metadata)
        <> optionalField "requestAttribution" (traceRequestAttribution metadata)
