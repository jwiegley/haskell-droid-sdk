{-# LANGUAGE OverloadedStrings #-}

module ContentSpec (contentTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON,
    Object,
    Result (..),
    ToJSON,
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy (..))
import Data.Scientific (scientific)
import Factory.Droid.Schema.Content
import Factory.Droid.Schema.Enums (ModelProvider (..))
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, chooseInt, forAll, testProperty, (===))

contentTests :: Value -> TestTree
contentTests schema =
  testGroup
    "Content"
    [ records "BaseContentBlockSchema" fullBase emptyBase baseJSON mempty (\extras block -> block {blockAdditionalFields = extras}),
      records "TextBlockSchema" textBlock (TextBlock "text" emptyBase) textJSON (KeyMap.delete "id" textJSON) (\extras block -> block {textBlockBase = (textBlockBase block) {blockAdditionalFields = extras}}),
      records "Base64ImageSourceSchema" imageSource imageSource imageSourceJSON imageSourceJSON (\extras source -> source {imageSourceAdditionalFields = extras}),
      records "ImageBlockSchema" imageBlock (ImageBlock imageSource Nothing emptyBase) imageJSON (remove ["id", "generated"] imageJSON) (\extras block -> block {imageBlockBase = (imageBlockBase block) {blockAdditionalFields = extras}}),
      records "ThinkingBlockSchema" thinkingBlock (ThinkingBlock "signature" Nothing "thought" Nothing emptyBase) thinkingJSON (remove ["id", "signatureProvider", "durationMs"] thinkingJSON) (\extras block -> block {thinkingBlockBase = (thinkingBlockBase block) {blockAdditionalFields = extras}}),
      records "RedactedThinkingBlockSchema" redactedBlock (RedactedThinkingBlock "opaque" emptyBase) redactedJSON (KeyMap.delete "id" redactedJSON) (\extras block -> block {redactedThinkingBase = (redactedThinkingBase block) {blockAdditionalFields = extras}}),
      nonNullableRecordTests "ScriptExecution" (schemaAt ["definitions", "ToolUseSchema", "properties", "scriptExecution"] schema) scriptExecution scriptExecution scriptJSON scriptJSON (\extras script -> script {scriptAdditionalFields = extras}),
      records "ToolUseSchema" toolUseBlock (ToolUseBlock "use-id" toolInput "lookup" Nothing Nothing Nothing mempty) toolUseJSON (remove ["namespace", "scriptExecution", "thoughtSignature"] toolUseJSON) (\extras block -> block {toolUseAdditionalFields = extras}),
      records "Base64PDFSourceSchema" pdfSource (Base64PDFSource "AA==" Nothing Nothing Nothing mempty) pdfJSON (remove ["parsedData", "name", "path"] pdfJSON) (\extras source -> source {pdfSourceAdditionalFields = extras}),
      records "PlainTextSourceSchema" plainSource (PlainTextSource "document text" Nothing Nothing mempty) plainJSON (remove ["name", "mime"] plainJSON) (\extras source -> source {plainTextSourceAdditionalFields = extras}),
      records "DocumentBlockSchema" documentBlock (DocumentBlock (PDFDocument pdfSource) emptyBase) documentJSON (KeyMap.delete "id" documentJSON) (\extras block -> block {documentBlockBase = (documentBlockBase block) {blockAdditionalFields = extras}}),
      records "ToolResultSchema" toolResultBlock (ToolResultBlock "use-id" Nothing Nothing emptyBase) toolResultJSON (remove ["id", "isError", "content"] toolResultJSON) (\extras block -> block {toolResultBase = (toolResultBase block) {blockAdditionalFields = extras}}),
      testCase "content union covers all schema alternatives" $ do
        refs <- either assertFailure pure (unionReferences =<< schemaAt ["definitions", "ContentBlockSchema"] schema)
        refs @?= ["#/definitions/" <> name | (name, _, _) <- contentVariants]
        forM_ contentVariants $ \(_, value, fields) -> do
          fromJSON (Object fields) @?= Success value
          toJSON value @?= Object fields
          eitherDecode (encode value) @?= Right value,
      testCase "document union covers both source shapes" $ do
        refs <- either assertFailure pure (unionReferences =<< schemaAt ["definitions", "DocumentSourceSchema"] schema)
        refs @?= ["#/definitions/Base64PDFSourceSchema", "#/definitions/PlainTextSourceSchema"]
        forM_ [(PDFDocument pdfSource, pdfJSON), (PlainTextDocument plainSource, plainJSON)] $ \(value, fields) -> do
          fromJSON (Object fields) @?= Success value
          toJSON value @?= Object fields
        rejects (Proxy @DocumentSource) (Object imageSourceJSON),
      testCase "tool result items cover exactly text, image and document" $ do
        refs <- either assertFailure pure (schemaAt ["definitions", "ToolResultSchema", "properties", "content"] schema >>= arrayItemSchema >>= unionReferences)
        refs @?= ["#/definitions/TextBlockSchema", "#/definitions/ImageBlockSchema", "#/definitions/DocumentBlockSchema"]
        forM_ [(ResultTextBlock textBlock, textJSON), (ResultImageBlock imageBlock, imageJSON), (ResultDocumentBlock documentBlock, documentJSON)] $ \(value, fields) -> do
          fromJSON (Object fields) @?= Success value
          toJSON value @?= Object fields
        forM_ [thinkingJSON, redactedJSON, toolUseJSON, toolResultJSON] $ \fields -> do
          rejects (Proxy @ToolResultItem) (Object fields)
          rejects (Proxy @ToolResultContent) (toJSON [Object fields]),
      testCase "tool content preserves strings, blocks, emptiness and omission" $ do
        forM_ [(ResultText "", String ""), (ResultText "result", String "result"), (ResultBlocks [], toJSON ([] :: [Value])), (ResultBlocks [ResultTextBlock textBlock], toJSON [Object textJSON])] $ \(value, wire) -> do
          fromJSON wire @?= Success value
          toJSON value @?= wire
          let block = ToolResultBlock "use-id" (Just value) Nothing emptyBase
          toJSON block @?= object ["type" .= String "tool_result", "toolUseId" .= String "use-id", "content" .= wire]
        forM_ [Null, Bool False, Number 0, Object textJSON] $ rejects (Proxy @ToolResultContent),
      testCase "unknown and malformed discriminants fail safely" $
        forM_ [Null, Bool True, Number 1, String "future", Object mempty, Array mempty] $ \tag -> do
          let value = object ["type" .= tag]
          rejects (Proxy @ContentBlock) value
          rejects (Proxy @DocumentSource) value
          rejects (Proxy @ToolResultItem) value,
      testCase "unknown tags fail even on otherwise valid blocks" $
        forM_ contentVariants $ \(_, _, fields) ->
          rejects (Proxy @ContentBlock) (Object (KeyMap.insert "type" (String "future") fields)),
      testCase "every image MIME literal matches the schema" $ do
        literals <- either assertFailure pure (schemaAt ["definitions", "Base64ImageSourceSchema", "properties", "mediaType", "enum"] schema)
        let values = [minBound .. maxBound] :: [ImageMediaType]
        toJSON values @?= literals
        forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
        rejects (Proxy @Base64ImageSource) (Object (KeyMap.insert "mediaType" (String "image/svg+xml") imageSourceJSON)),
      testCase "source MIME constants cannot be substituted" $ do
        rejects (Proxy @Base64PDFSource) (Object (KeyMap.insert "mediaType" (String "text/plain") pdfJSON))
        rejects (Proxy @PlainTextSource) (Object (KeyMap.insert "mediaType" (String "application/pdf") plainJSON)),
      testCase "signature provider supports all known providers and explicit unknown" $ do
        let providers = map SignedBy [minBound .. maxBound] <> [UnknownSignatureProvider]
        forM_ providers $ \value -> fromJSON (toJSON value) @?= Success value
        toJSON UnknownSignatureProvider @?= String "unknown"
        rejects (Proxy @ThinkingBlock) (Object (KeyMap.insert "signatureProvider" (String "future") thinkingJSON)),
      testCase "thinking signature is required but may be empty" $ do
        let value = thinkingBlock {thinkingBlockSignature = ""}
        fromJSON (Object (KeyMap.insert "signature" (String "") thinkingJSON)) @?= Success value,
      testCase "thinking duration rejects negative values but accepts zero" $ do
        mkThinkingDuration (-1) @?= Nothing
        rejects (Proxy @ThinkingBlock) (Object (KeyMap.insert "durationMs" (Number (-0.001)) thinkingJSON))
        case mkThinkingDuration 0 of
          Nothing -> assertFailure "Zero duration was rejected"
          Just duration -> do
            thinkingDurationMilliseconds duration @?= 0
            fromJSON (Number 0) @?= Success duration,
      testProperty "nonnegative fractional durations retain exact numeric values" $
        forAll arbitrary $ \coefficient ->
          forAll (chooseInt (-12, 12)) $ \decimalExponent ->
            let number = scientific (abs coefficient) decimalExponent
             in case mkThinkingDuration number of
                  Nothing -> Nothing === Just number
                  Just duration -> eitherDecode (encode duration) === Right (Number number),
      testCase "script context requires both identities" $ do
        forM_ ["runId", "outerToolUseId"] $ \key ->
          rejects (Proxy @ToolUseBlock) (Object (KeyMap.insert "scriptExecution" (Object (KeyMap.delete key scriptJSON)) toolUseJSON)),
      testCase "tool input must be an object but may contain arbitrary JSON" $ do
        forM_ [Null, Bool True, Number 1, String "input", Array mempty] $ \input ->
          rejects (Proxy @ToolUseBlock) (Object (KeyMap.insert "input" input toolUseJSON))
        fromJSON (Object toolUseJSON) @?= Success toolUseBlock,
      nonNullableRecordTests
        "CacheControl"
        (schemaAt ["definitions", "CacheLabelSchema", "properties", "cache_control"] schema)
        (EphemeralCache (Just CacheOneHour) mempty)
        (EphemeralCache Nothing mempty)
        (KeyMap.fromList ["type" .= String "ephemeral", "ttl" .= String "1h"])
        (KeyMap.singleton "type" (String "ephemeral"))
        (\extras control -> control {cacheAdditionalFields = extras}),
      records
        "CacheLabelSchema"
        (CacheLabel (Just (EphemeralCache Nothing mempty)) mempty)
        (CacheLabel Nothing mempty)
        (KeyMap.singleton "cache_control" (object ["type" .= String "ephemeral"]))
        mempty
        (\extras label -> label {labelAdditionalFields = extras}),
      testCase "cache TTLs match their inline enum" $ do
        literals <- either assertFailure pure (schemaAt ["definitions", "CacheLabelSchema", "properties", "cache_control", "properties", "ttl", "enum"] schema)
        let values = [minBound .. maxBound] :: [CacheTTL]
        toJSON values @?= literals
        forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
        rejects (Proxy @CacheControl) (object ["type" .= String "ephemeral", "ttl" .= String "future"]),
      testCase "all seven block variants support optional cache labels" $
        forM_ contentVariants $ \(_, content, fields) ->
          forM_ [Nothing, Just (EphemeralCache Nothing mempty), Just (EphemeralCache (Just CacheFiveMinutes) mempty)] $ \control -> do
            let value = CachedContentBlock content control
                expected = maybe fields (\item -> KeyMap.insert "cache_control" (toJSON item) fields) control
            toJSON value @?= Object expected
            fromJSON (Object expected) @?= Success value
            eitherDecode (encode value) @?= Right value,
      testCase "typed cache fields replace or remove conflicting block extensions" $
        forM_ contentVariants $ \(_, _, fields) -> do
          let injected = Object (KeyMap.insert "cache_control" Null fields)
          case fromJSON injected :: Result ContentBlock of
            Error err -> assertFailure err
            Success content -> do
              toJSON (CachedContentBlock content Nothing) @?= Object fields
              let control = EphemeralCache (Just CacheOneHour) mempty
              toJSON (CachedContentBlock content (Just control)) @?= Object (KeyMap.insert "cache_control" (toJSON control) fields)
          rejects (Proxy @CachedContentBlock) injected,
      testCase "cache labels preserve independent block and control extensions" $ do
        let wire = Object (KeyMap.insert "future" extension (KeyMap.insert "cache_control" (object ["type" .= String "ephemeral", "futureControl" .= extension]) textJSON))
        case fromJSON wire :: Result CachedContentBlock of
          Error err -> assertFailure err
          Success block -> do
            toJSON block @?= wire
            cachedBlockControl block @?= Just (EphemeralCache Nothing (KeyMap.singleton "futureControl" extension)),
      testCase "cached tool results do not impose caching validation on nested items" $ do
        let nested = Object (KeyMap.insert "cache_control" Null textJSON)
            wire = Object (KeyMap.insert "content" (toJSON [nested]) toolResultJSON)
        case fromJSON wire :: Result CachedContentBlock of
          Error err -> assertFailure err
          Success block -> toJSON block @?= wire
    ]
  where
    extension = object ["nested" .= [Null, Bool True]]
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)

emptyBase, fullBase :: BaseContentBlock
emptyBase = BaseContentBlock Nothing mempty
fullBase = BaseContentBlock (Just "block-id") mempty

baseJSON :: Object
baseJSON = KeyMap.singleton "id" (String "block-id")

textBlock :: TextBlock
textBlock = TextBlock "text" fullBase

textJSON :: Object
textJSON = KeyMap.fromList ["type" .= String "text", "text" .= String "text", "id" .= String "block-id"]

imageSource :: Base64ImageSource
imageSource = Base64ImageSource "AA==" ImagePNG mempty

imageSourceJSON :: Object
imageSourceJSON = KeyMap.fromList ["type" .= String "base64", "data" .= String "AA==", "mediaType" .= String "image/png"]

imageBlock :: ImageBlock
imageBlock = ImageBlock imageSource (Just True) fullBase

imageJSON :: Object
imageJSON = KeyMap.fromList ["type" .= String "image", "source" .= imageSourceJSON, "generated" .= True, "id" .= String "block-id"]

thinkingBlock :: ThinkingBlock
thinkingBlock = ThinkingBlock "signature" (Just (SignedBy ProviderFactory)) "thought" (mkThinkingDuration 0.125) fullBase

thinkingJSON :: Object
thinkingJSON = KeyMap.fromList ["type" .= String "thinking", "signature" .= String "signature", "signatureProvider" .= String "factory", "thinking" .= String "thought", "durationMs" .= Number 0.125, "id" .= String "block-id"]

redactedBlock :: RedactedThinkingBlock
redactedBlock = RedactedThinkingBlock "opaque" fullBase

redactedJSON :: Object
redactedJSON = KeyMap.fromList ["type" .= String "redacted_thinking", "data" .= String "opaque", "id" .= String "block-id"]

scriptExecution :: ScriptExecution
scriptExecution = ScriptExecution "run-id" "outer-use-id" mempty

scriptJSON :: Object
scriptJSON = KeyMap.fromList ["runId" .= String "run-id", "outerToolUseId" .= String "outer-use-id"]

toolInput :: Object
toolInput = KeyMap.fromList ["query" .= String "example", "nested" .= [Null, Number 1, Bool True]]

toolUseBlock :: ToolUseBlock
toolUseBlock = ToolUseBlock "use-id" toolInput "lookup" (Just "fixture") (Just scriptExecution) (Just "thought-signature") mempty

toolUseJSON :: Object
toolUseJSON = KeyMap.fromList ["type" .= String "tool_use", "id" .= String "use-id", "input" .= toolInput, "name" .= String "lookup", "namespace" .= String "fixture", "scriptExecution" .= scriptJSON, "thoughtSignature" .= String "thought-signature"]

pdfSource :: Base64PDFSource
pdfSource = Base64PDFSource "AA==" (Just "parsed") (Just "paper.pdf") (Just "/fixture/paper.pdf") mempty

pdfJSON :: Object
pdfJSON = KeyMap.fromList ["type" .= String "base64", "mediaType" .= String "application/pdf", "data" .= String "AA==", "parsedData" .= String "parsed", "name" .= String "paper.pdf", "path" .= String "/fixture/paper.pdf"]

plainSource :: PlainTextSource
plainSource = PlainTextSource "document text" (Just "notes.md") (Just "text/markdown") mempty

plainJSON :: Object
plainJSON = KeyMap.fromList ["type" .= String "text", "mediaType" .= String "text/plain", "data" .= String "document text", "name" .= String "notes.md", "mime" .= String "text/markdown"]

documentBlock :: DocumentBlock
documentBlock = DocumentBlock (PDFDocument pdfSource) fullBase

documentJSON :: Object
documentJSON = KeyMap.fromList ["type" .= String "document", "source" .= pdfJSON, "id" .= String "block-id"]

toolResultBlock :: ToolResultBlock
toolResultBlock = ToolResultBlock "use-id" (Just (ResultBlocks [ResultTextBlock textBlock, ResultImageBlock imageBlock, ResultDocumentBlock documentBlock])) (Just False) fullBase

toolResultJSON :: Object
toolResultJSON = KeyMap.fromList ["type" .= String "tool_result", "toolUseId" .= String "use-id", "content" .= [Object textJSON, Object imageJSON, Object documentJSON], "isError" .= False, "id" .= String "block-id"]

contentVariants :: [(Key, ContentBlock, Object)]
contentVariants =
  [ ("TextBlockSchema", ContentText textBlock, textJSON),
    ("ImageBlockSchema", ContentImage imageBlock, imageJSON),
    ("ThinkingBlockSchema", ContentThinking thinkingBlock, thinkingJSON),
    ("RedactedThinkingBlockSchema", ContentRedactedThinking redactedBlock, redactedJSON),
    ("ToolUseSchema", ContentToolUse toolUseBlock, toolUseJSON),
    ("ToolResultSchema", ContentToolResult toolResultBlock, toolResultJSON),
    ("DocumentBlockSchema", ContentDocument documentBlock, documentJSON)
  ]

remove :: [Key] -> Object -> Object
remove keys fields = foldr KeyMap.delete fields keys

unionReferences :: Value -> Either String [Key]
unionReferences = parseEither $ withObject "union schema" $ \fields -> do
  alternatives <- fields .: "anyOf"
  traverse (withObject "union member" (.: "$ref")) alternatives

arrayItemSchema :: Value -> Either String Value
arrayItemSchema = parseEither $ withObject "tool result content schema" $ \fields -> do
  alternatives <- fields .: "anyOf"
  case alternatives :: [Value] of
    [_, array] -> withObject "array schema" (.: "items") array
    _ -> fail "Unexpected tool-result content alternatives"
