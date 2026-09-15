{-# LANGUAGE OverloadedStrings #-}

-- | Message content for Factory protocol 1.205.0. These are wire codecs:
-- source data is a JSON string, not proof of valid base64, a supported file,
-- or compliance with attachment size limits. Open objects retain extensions.
module Factory.Droid.Schema.Content
  ( BaseContentBlock (..),
    TextBlock (..),
    ImageMediaType (..),
    Base64ImageSource (..),
    ImageBlock (..),
    ThinkingDuration,
    mkThinkingDuration,
    thinkingDurationMilliseconds,
    SignatureProvider (..),
    ThinkingBlock (..),
    RedactedThinkingBlock (..),
    ScriptExecution (..),
    ToolUseBlock (..),
    Base64PDFSource (..),
    PlainTextSource (..),
    DocumentSource (..),
    DocumentBlock (..),
    ToolResultItem (..),
    ToolResultContent (..),
    ToolResultBlock (..),
    isPendingToolResult,
    inspectToolResultId,
    UserContentOptions (..),
    defaultUserContentOptions,
    buildUserMessageContent,
    ContentBlock (..),
    contentBlockObject,
    CacheTTL (..),
    CacheControl (..),
    CacheLabel (..),
    CachedContentBlock (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    withObject,
    withScientific,
    withText,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON
  ( additionalFields,
    fieldsWithAdditionalFields,
    isEcmaWhitespace,
    objectWithAdditionalFields,
    optionalField,
    requireLiteral,
  )
import Factory.Droid.Schema.Enums (ModelProvider)

-- | Optional block identity and extension properties. Enclosing block codecs
-- reserve their own fields as well, so extensions cannot override them.
data BaseContentBlock = BaseContentBlock
  { blockId :: !(Maybe Text),
    blockAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BaseContentBlock where
  parseJSON = withObject "BaseContentBlock" (parseBase [])

instance ToJSON BaseContentBlock where
  toJSON base = Object (blockObject [] base [])

-- | Text content, including an empty string.
data TextBlock = TextBlock
  { textBlockText :: !Text,
    textBlockBase :: !BaseContentBlock
  }
  deriving stock (Eq, Show)

instance FromJSON TextBlock where
  parseJSON = withObject "TextBlock" $ \fields -> do
    requireLiteral "type" "text" fields
    TextBlock <$> fields .: "text" <*> parseBase textKeys fields

instance ToJSON TextBlock where
  toJSON = Object . contentBlockObject . ContentText

-- | The four MIME types accepted by an image source.
data ImageMediaType = ImageJPEG | ImagePNG | ImageGIF | ImageWebP
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ImageMediaType where
  parseJSON = withText "ImageMediaType" $ \case
    "image/jpeg" -> pure ImageJPEG
    "image/png" -> pure ImagePNG
    "image/gif" -> pure ImageGIF
    "image/webp" -> pure ImageWebP
    _ -> fail "Unknown image media type"

instance ToJSON ImageMediaType where
  toJSON ImageJPEG = String "image/jpeg"
  toJSON ImagePNG = String "image/png"
  toJSON ImageGIF = String "image/gif"
  toJSON ImageWebP = String "image/webp"

-- | Image source data and its declared MIME type; data is not decoded here.
data Base64ImageSource = Base64ImageSource
  { imageSourceData :: !Text,
    imageSourceMediaType :: !ImageMediaType,
    imageSourceAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON Base64ImageSource where
  parseJSON = withObject "Base64ImageSource" $ \fields -> do
    requireLiteral "type" "base64" fields
    Base64ImageSource
      <$> fields .: "data"
      <*> fields .: "mediaType"
      <*> pure (additionalFields imageSourceKeys fields)

instance ToJSON Base64ImageSource where
  toJSON source =
    objectWithAdditionalFields
      imageSourceKeys
      (imageSourceAdditionalFields source)
      ["type" .= String "base64", "data" .= imageSourceData source, "mediaType" .= imageSourceMediaType source]

-- | Image content. The generated flag remains distinct from its absence.
data ImageBlock = ImageBlock
  { imageBlockSource :: !Base64ImageSource,
    imageBlockGenerated :: !(Maybe Bool),
    imageBlockBase :: !BaseContentBlock
  }
  deriving stock (Eq, Show)

instance FromJSON ImageBlock where
  parseJSON = withObject "ImageBlock" $ \fields -> do
    requireLiteral "type" "image" fields
    ImageBlock <$> fields .: "source" <*> fields .:! "generated" <*> parseBase imageKeys fields

instance ToJSON ImageBlock where
  toJSON = Object . contentBlockObject . ContentImage

-- | A nonnegative, possibly fractional duration in milliseconds. The
-- constructor is private so encoding cannot produce a negative duration.
newtype ThinkingDuration = ThinkingDuration Scientific
  deriving stock (Eq, Ord, Show)

-- | Construct a duration, rejecting negative values.
mkThinkingDuration :: Scientific -> Maybe ThinkingDuration
mkThinkingDuration value
  | value >= 0 = Just (ThinkingDuration value)
  | otherwise = Nothing

-- | Obtain the exact duration in milliseconds.
thinkingDurationMilliseconds :: ThinkingDuration -> Scientific
thinkingDurationMilliseconds (ThinkingDuration value) = value

instance FromJSON ThinkingDuration where
  parseJSON = withScientific "ThinkingDuration" $ \value ->
    maybe (fail "Thinking duration must be nonnegative") pure (mkThinkingDuration value)

instance ToJSON ThinkingDuration where
  toJSON = toJSON . thinkingDurationMilliseconds

-- | A known model provider or the explicit wire literal @unknown@. This is
-- not a fallback for arbitrary unrecognized provider names.
data SignatureProvider = SignedBy !ModelProvider | UnknownSignatureProvider
  deriving stock (Eq, Ord, Show)

instance FromJSON SignatureProvider where
  parseJSON (String "unknown") = pure UnknownSignatureProvider
  parseJSON value = SignedBy <$> parseJSON value

instance ToJSON SignatureProvider where
  toJSON (SignedBy provider) = toJSON provider
  toJSON UnknownSignatureProvider = String "unknown"

-- | Thinking content. A signature is required but may be empty.
data ThinkingBlock = ThinkingBlock
  { thinkingBlockSignature :: !Text,
    thinkingBlockSignatureProvider :: !(Maybe SignatureProvider),
    thinkingBlockThinking :: !Text,
    thinkingBlockDuration :: !(Maybe ThinkingDuration),
    thinkingBlockBase :: !BaseContentBlock
  }
  deriving stock (Eq, Show)

instance FromJSON ThinkingBlock where
  parseJSON = withObject "ThinkingBlock" $ \fields -> do
    requireLiteral "type" "thinking" fields
    ThinkingBlock
      <$> fields .: "signature"
      <*> fields .:! "signatureProvider"
      <*> fields .: "thinking"
      <*> fields .:! "durationMs"
      <*> parseBase thinkingKeys fields

instance ToJSON ThinkingBlock where
  toJSON = Object . contentBlockObject . ContentThinking

-- | Opaque redacted thinking data, retained without interpretation.
data RedactedThinkingBlock = RedactedThinkingBlock
  { redactedThinkingData :: !Text,
    redactedThinkingBase :: !BaseContentBlock
  }
  deriving stock (Eq, Show)

instance FromJSON RedactedThinkingBlock where
  parseJSON = withObject "RedactedThinkingBlock" $ \fields -> do
    requireLiteral "type" "redacted_thinking" fields
    RedactedThinkingBlock <$> fields .: "data" <*> parseBase redactedKeys fields

instance ToJSON RedactedThinkingBlock where
  toJSON = Object . contentBlockObject . ContentRedactedThinking

-- | A tool invocation's script-run identity and enclosing tool-use identity.
data ScriptExecution = ScriptExecution
  { scriptRunId :: !Text,
    scriptOuterToolUseId :: !Text,
    scriptAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ScriptExecution where
  parseJSON = withObject "ScriptExecution" $ \fields ->
    ScriptExecution <$> fields .: "runId" <*> fields .: "outerToolUseId" <*> pure (additionalFields scriptKeys fields)

instance ToJSON ScriptExecution where
  toJSON script = objectWithAdditionalFields scriptKeys (scriptAdditionalFields script) ["runId" .= scriptRunId script, "outerToolUseId" .= scriptOuterToolUseId script]

-- | A tool invocation. Input is deliberately an arbitrary JSON object;
-- a tool's own input schema supplies any further validation.
data ToolUseBlock = ToolUseBlock
  { toolUseId :: !Text,
    toolUseInput :: !Object,
    toolUseName :: !Text,
    toolUseNamespace :: !(Maybe Text),
    toolUseScriptExecution :: !(Maybe ScriptExecution),
    toolUseThoughtSignature :: !(Maybe Text),
    toolUseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ToolUseBlock where
  parseJSON = withObject "ToolUseBlock" $ \fields -> do
    requireLiteral "type" "tool_use" fields
    ToolUseBlock
      <$> fields .: "id"
      <*> fields .: "input"
      <*> fields .: "name"
      <*> fields .:! "namespace"
      <*> fields .:! "scriptExecution"
      <*> fields .:! "thoughtSignature"
      <*> pure (additionalFields toolUseKeys fields)

instance ToJSON ToolUseBlock where
  toJSON = Object . contentBlockObject . ContentToolUse

-- | PDF source data and optional extraction/name/path metadata. A path is
-- metadata only: this codec neither reads nor writes a file.
data Base64PDFSource = Base64PDFSource
  { pdfSourceData :: !Text,
    pdfSourceParsedData :: !(Maybe Text),
    pdfSourceName :: !(Maybe Text),
    pdfSourcePath :: !(Maybe Text),
    pdfSourceAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON Base64PDFSource where
  parseJSON = withObject "Base64PDFSource" $ \fields -> do
    requireLiteral "type" "base64" fields
    requireLiteral "mediaType" "application/pdf" fields
    Base64PDFSource
      <$> fields .: "data"
      <*> fields .:! "parsedData"
      <*> fields .:! "name"
      <*> fields .:! "path"
      <*> pure (additionalFields pdfSourceKeys fields)

instance ToJSON Base64PDFSource where
  toJSON source =
    objectWithAdditionalFields pdfSourceKeys (pdfSourceAdditionalFields source) $
      ["type" .= String "base64", "mediaType" .= String "application/pdf", "data" .= pdfSourceData source]
        <> optionalField "parsedData" (pdfSourceParsedData source)
        <> optionalField "name" (pdfSourceName source)
        <> optionalField "path" (pdfSourcePath source)

-- | Plain-text source data. The optional original MIME hint does not alter
-- the required wire mediaType, which is always @text/plain@.
data PlainTextSource = PlainTextSource
  { plainTextSourceData :: !Text,
    plainTextSourceName :: !(Maybe Text),
    plainTextSourceMime :: !(Maybe Text),
    plainTextSourceAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON PlainTextSource where
  parseJSON = withObject "PlainTextSource" $ \fields -> do
    requireLiteral "type" "text" fields
    requireLiteral "mediaType" "text/plain" fields
    PlainTextSource <$> fields .: "data" <*> fields .:! "name" <*> fields .:! "mime" <*> pure (additionalFields plainTextSourceKeys fields)

instance ToJSON PlainTextSource where
  toJSON source =
    objectWithAdditionalFields plainTextSourceKeys (plainTextSourceAdditionalFields source) $
      ["type" .= String "text", "mediaType" .= String "text/plain", "data" .= plainTextSourceData source]
        <> optionalField "name" (plainTextSourceName source)
        <> optionalField "mime" (plainTextSourceMime source)

-- | The two document source shapes supported by the wire protocol.
data DocumentSource = PDFDocument !Base64PDFSource | PlainTextDocument !PlainTextSource
  deriving stock (Eq, Show)

instance FromJSON DocumentSource where
  parseJSON = withObject "DocumentSource" $ \fields -> do
    tag <- fields .: "type" :: Parser Text
    case tag of
      "base64" -> PDFDocument <$> parseJSON (Object fields)
      "text" -> PlainTextDocument <$> parseJSON (Object fields)
      _ -> fail "Unknown document source type"

instance ToJSON DocumentSource where
  toJSON (PDFDocument source) = toJSON source
  toJSON (PlainTextDocument source) = toJSON source

-- | Document content with an optional block identity.
data DocumentBlock = DocumentBlock
  { documentBlockSource :: !DocumentSource,
    documentBlockBase :: !BaseContentBlock
  }
  deriving stock (Eq, Show)

instance FromJSON DocumentBlock where
  parseJSON = withObject "DocumentBlock" $ \fields -> do
    requireLiteral "type" "document" fields
    DocumentBlock <$> fields .: "source" <*> parseBase documentKeys fields

instance ToJSON DocumentBlock where
  toJSON = Object . contentBlockObject . ContentDocument

-- | Only text, image and document blocks may appear in tool-result arrays.
data ToolResultItem = ResultTextBlock !TextBlock | ResultImageBlock !ImageBlock | ResultDocumentBlock !DocumentBlock
  deriving stock (Eq, Show)

instance FromJSON ToolResultItem where
  parseJSON = withObject "ToolResultItem" $ \fields -> do
    tag <- fields .: "type" :: Parser Text
    case tag of
      "text" -> ResultTextBlock <$> parseJSON (Object fields)
      "image" -> ResultImageBlock <$> parseJSON (Object fields)
      "document" -> ResultDocumentBlock <$> parseJSON (Object fields)
      _ -> fail "Unsupported tool-result block type"

instance ToJSON ToolResultItem where
  toJSON (ResultTextBlock block) = toJSON block
  toJSON (ResultImageBlock block) = toJSON block
  toJSON (ResultDocumentBlock block) = toJSON block

-- | A plain string or a list of permitted result blocks. An empty string
-- and an empty list are distinct from an absent content field.
data ToolResultContent = ResultText !Text | ResultBlocks ![ToolResultItem]
  deriving stock (Eq, Show)

instance FromJSON ToolResultContent where
  parseJSON (String text) = pure (ResultText text)
  parseJSON value@(Array _) = ResultBlocks <$> parseJSON value
  parseJSON _ = fail "Expected tool-result text or an array of blocks"

instance ToJSON ToolResultContent where
  toJSON (ResultText text) = toJSON text
  toJSON (ResultBlocks blocks) = toJSON blocks

-- | A tool result. Optional content and error status reject explicit null.
data ToolResultBlock = ToolResultBlock
  { toolResultToolUseId :: !Text,
    toolResultContent :: !(Maybe ToolResultContent),
    toolResultIsError :: !(Maybe Bool),
    toolResultBase :: !BaseContentBlock
  }
  deriving stock (Eq, Show)

instance FromJSON ToolResultBlock where
  parseJSON = withObject "ToolResultBlock" $ \fields -> do
    requireLiteral "type" "tool_result" fields
    ToolResultBlock <$> fields .: "toolUseId" <*> fields .:! "content" <*> fields .:! "isError" <*> parseBase toolResultKeys fields

instance ToJSON ToolResultBlock where
  toJSON = Object . contentBlockObject . ContentToolResult

-- | The exact scalar marker; a text block inside an array is not this marker.
isPendingToolResult :: ToolResultBlock -> Bool
isPendingToolResult result = toolResultContent result == Just (ResultText "__TOOL_RESULT_PENDING__")

-- | Tolerant inspection only. Preserve empty/non-string values; use the legacy
-- key only when the canonical key is absent or null. Wire decoding is unchanged.
inspectToolResultId :: Object -> Maybe Value
inspectToolResultId fields = case KeyMap.lookup "toolUseId" fields of
  Nothing -> KeyMap.lookup "tool_use_id" fields
  Just Null -> KeyMap.lookup "tool_use_id" fields
  value -> value

-- | The complete seven-variant content-block union. Unknown discriminants
-- fail decoding rather than being mistaken for a known block.
data ContentBlock
  = ContentText !TextBlock
  | ContentImage !ImageBlock
  | ContentThinking !ThinkingBlock
  | ContentRedactedThinking !RedactedThinkingBlock
  | ContentToolUse !ToolUseBlock
  | ContentToolResult !ToolResultBlock
  | ContentDocument !DocumentBlock
  deriving stock (Eq, Show)

data UserContentOptions = UserContentOptions
  { contentTrimText :: !Bool,
    contentIncludeEmptyText :: !Bool
  }
  deriving stock (Eq, Show)

defaultUserContentOptions :: UserContentOptions
defaultUserContentOptions = UserContentOptions False True

-- | Assemble supplied sources in image/document/text order. This performs no
-- file I/O, base64 validation or attachment-limit checks; normal Input does.
buildUserMessageContent :: UserContentOptions -> Maybe Text -> [Base64ImageSource] -> [DocumentSource] -> [ContentBlock]
buildUserMessageContent options text images documents =
  map (\source -> ContentImage (ImageBlock source Nothing base)) images
    <> map (\source -> ContentDocument (DocumentBlock source base)) documents
    <> [ContentText (TextBlock value base) | Just value <- [prepared], contentIncludeEmptyText options || not (Text.null value)]
  where
    base = BaseContentBlock Nothing mempty
    prepared = if contentTrimText options then Text.dropAround isEcmaWhitespace <$> text else text

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \fields -> do
    tag <- fields .: "type" :: Parser Text
    case tag of
      "text" -> ContentText <$> parseJSON (Object fields)
      "image" -> ContentImage <$> parseJSON (Object fields)
      "thinking" -> ContentThinking <$> parseJSON (Object fields)
      "redacted_thinking" -> ContentRedactedThinking <$> parseJSON (Object fields)
      "tool_use" -> ContentToolUse <$> parseJSON (Object fields)
      "tool_result" -> ContentToolResult <$> parseJSON (Object fields)
      "document" -> ContentDocument <$> parseJSON (Object fields)
      _ -> fail "Unknown content block type"

instance ToJSON ContentBlock where
  toJSON = Object . contentBlockObject

-- | Encode a block as an object for typed schema extensions. This is the
-- same field encoding used by the ToJSON instances and cached blocks.
contentBlockObject :: ContentBlock -> Object
contentBlockObject = \case
  ContentText block -> blockObject textKeys (textBlockBase block) ["type" .= String "text", "text" .= textBlockText block]
  ContentImage block ->
    blockObject imageKeys (imageBlockBase block) $
      ["type" .= String "image", "source" .= imageBlockSource block]
        <> optionalField "generated" (imageBlockGenerated block)
  ContentThinking block ->
    blockObject thinkingKeys (thinkingBlockBase block) $
      ["type" .= String "thinking", "signature" .= thinkingBlockSignature block, "thinking" .= thinkingBlockThinking block]
        <> optionalField "signatureProvider" (thinkingBlockSignatureProvider block)
        <> optionalField "durationMs" (thinkingBlockDuration block)
  ContentRedactedThinking block -> blockObject redactedKeys (redactedThinkingBase block) ["type" .= String "redacted_thinking", "data" .= redactedThinkingData block]
  ContentToolUse block ->
    fieldsWithAdditionalFields toolUseKeys (toolUseAdditionalFields block) $
      ["type" .= String "tool_use", "id" .= toolUseId block, "input" .= toolUseInput block, "name" .= toolUseName block]
        <> optionalField "namespace" (toolUseNamespace block)
        <> optionalField "scriptExecution" (toolUseScriptExecution block)
        <> optionalField "thoughtSignature" (toolUseThoughtSignature block)
  ContentToolResult block ->
    blockObject toolResultKeys (toolResultBase block) $
      ["type" .= String "tool_result", "toolUseId" .= toolResultToolUseId block]
        <> optionalField "content" (toolResultContent block)
        <> optionalField "isError" (toolResultIsError block)
  ContentDocument block -> blockObject documentKeys (documentBlockBase block) ["type" .= String "document", "source" .= documentBlockSource block]

-- | The two declared cache lifetimes. Absence of a TTL remains absent.
data CacheTTL = CacheFiveMinutes | CacheOneHour
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON CacheTTL where
  parseJSON = withText "CacheTTL" $ \case
    "5m" -> pure CacheFiveMinutes
    "1h" -> pure CacheOneHour
    _ -> fail "Unknown cache TTL"

instance ToJSON CacheTTL where
  toJSON CacheFiveMinutes = String "5m"
  toJSON CacheOneHour = String "1h"

-- | Ephemeral cache control, with an optional lifetime and open extensions.
data CacheControl = EphemeralCache
  { cacheTTL :: !(Maybe CacheTTL),
    cacheAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CacheControl where
  parseJSON = withObject "CacheControl" $ \fields -> do
    requireLiteral "type" "ephemeral" fields
    EphemeralCache <$> fields .:! "ttl" <*> pure (additionalFields ["type", "ttl"] fields)

instance ToJSON CacheControl where
  toJSON control =
    objectWithAdditionalFields ["type", "ttl"] (cacheAdditionalFields control) $
      ["type" .= String "ephemeral"] <> optionalField "ttl" (cacheTTL control)

-- | The standalone cache-label schema. The wire key is @cache_control@.
data CacheLabel = CacheLabel
  { labelCacheControl :: !(Maybe CacheControl),
    labelAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON CacheLabel where
  parseJSON = withObject "CacheLabel" $ \fields ->
    CacheLabel <$> fields .:! "cache_control" <*> pure (additionalFields ["cache_control"] fields)

instance ToJSON CacheLabel where
  toJSON label = objectWithAdditionalFields ["cache_control"] (labelAdditionalFields label) (optionalField "cache_control" (labelCacheControl label))

-- | A content block intersected with CacheLabelSchema. The explicit cache
-- field takes precedence over any same-named extension on the content block.
-- Nested tool-result items retain their ordinary, non-caching schema.
data CachedContentBlock = CachedContentBlock
  { cachedBlockContent :: !ContentBlock,
    cachedBlockControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show)

instance FromJSON CachedContentBlock where
  parseJSON = withObject "CachedContentBlock" $ \fields ->
    CachedContentBlock
      <$> parseJSON (Object (KeyMap.delete "cache_control" fields))
      <*> fields .:! "cache_control"

instance ToJSON CachedContentBlock where
  toJSON block =
    objectWithAdditionalFields
      ["cache_control"]
      (contentBlockObject (cachedBlockContent block))
      (optionalField "cache_control" (cachedBlockControl block))

parseBase :: [Key] -> Object -> Parser BaseContentBlock
parseBase keys fields = BaseContentBlock <$> fields .:! "id" <*> pure (additionalFields ("id" : keys) fields)

blockObject :: [Key] -> BaseContentBlock -> [Pair] -> Object
blockObject keys base fields = fieldsWithAdditionalFields ("id" : keys) (blockAdditionalFields base) (optionalField "id" (blockId base) <> fields)

textKeys, imageKeys, thinkingKeys, redactedKeys, documentKeys, toolResultKeys :: [Key]
textKeys = ["type", "text"]
imageKeys = ["type", "source", "generated"]
thinkingKeys = ["type", "signature", "signatureProvider", "thinking", "durationMs"]
redactedKeys = ["type", "data"]
documentKeys = ["type", "source"]
toolResultKeys = ["type", "toolUseId", "content", "isError"]

imageSourceKeys, pdfSourceKeys, plainTextSourceKeys, scriptKeys, toolUseKeys :: [Key]
imageSourceKeys = ["type", "data", "mediaType"]
pdfSourceKeys = ["type", "mediaType", "data", "parsedData", "name", "path"]
plainTextSourceKeys = ["type", "mediaType", "data", "name", "mime"]
scriptKeys = ["runId", "outerToolUseId"]
toolUseKeys = ["type", "id", "input", "name", "namespace", "scriptExecution", "thoughtSignature"]
