{-# LANGUAGE OverloadedStrings #-}

-- | Native hosted MCP tools. Argument decoding occurs once and receives only
-- the argument object, never HTTP headers, bearer credentials or request state.
module Factory.Droid.MCP.Tool
  ( McpTool,
    McpToolError (..),
    McpSchema,
    McpSchemaError (..),
    mkMcpSchema,
    openObjectSchema,
    mcpSchemaObject,
    McpContent,
    textContent,
    jsonContent,
    McpToolResult (..),
    textResult,
    errorResult,
    structuredResult,
    rawTool,
    typedTool,
    structuredTool,
    toolName,
    toolDescription,
    toolInputSchema,
    toolOutputSchema,
    invokeTool,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless, void)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), encode, withObject, (.:), (.:!), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Internal.Exception (trySync)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Internal.McpSchema
import Factory.Droid.Schema.Primitives (mkRfc3339Timestamp)

-- | Failures do not contain schemas, arguments or handler exception text.
data McpToolError = InvalidMcpToolName | InvalidMcpToolResult
  deriving stock (Eq, Show)

newtype McpContent = McpContent Object
  deriving stock (Eq)

instance Show McpContent where
  show _ = "McpContent <redacted>"

instance ToJSON McpContent where
  toJSON (McpContent value) = Object value

instance FromJSON McpContent where
  parseJSON = withObject "MCP content" $ \fields -> do
    kind <- fields .: "type" :: Parser Text
    case kind of
      "text" -> void (fields .: "text" :: Parser Text)
      "image" -> media fields
      "audio" -> media fields
      "resource_link" -> do
        void (fields .: "uri" :: Parser Text)
        void (fields .: "name" :: Parser Text)
        void (fields .:! "title" :: Parser (Maybe Text))
        void (fields .:! "description" :: Parser (Maybe Text))
        void (fields .:! "mimeType" :: Parser (Maybe Text))
        void (fields .:! "size" :: Parser (Maybe Scientific))
        icons <- fields .:! "icons" :: Parser (Maybe [Object])
        forM_ icons $ mapM_ $ \icon -> do
          void (icon .: "src" :: Parser Text)
          void (icon .:! "mimeType" :: Parser (Maybe Text))
          void (icon .:! "sizes" :: Parser (Maybe [Text]))
          theme <- icon .:! "theme" :: Parser (Maybe Text)
          forM_ theme $ \value -> unless (value `elem` ["light", "dark"]) (fail "Invalid icon theme")
      "resource" -> do
        resource <- fields .: "resource"
        void (resource .: "uri" :: Parser Text)
        void (resource .:! "mimeType" :: Parser (Maybe Text))
        void (resource .:! "_meta" :: Parser (Maybe Object))
        text <- resource .:! "text" :: Parser (Maybe Text)
        blob <- resource .:! "blob" :: Parser (Maybe Text)
        unless (isJust text || isJust blob) (fail "Missing resource content")
        forM_ blob validateBase64
      _ -> fail "Unknown MCP content type"
    annotation <- fields .:! "annotations" :: Parser (Maybe Object)
    forM_ annotation $ \value -> do
      audience <- value .:! "audience" :: Parser (Maybe [Text])
      forM_ audience $ \roles -> unless (all (`elem` ["user", "assistant"]) roles) (fail "Invalid MCP audience")
      priority <- value .:! "priority" :: Parser (Maybe Scientific)
      forM_ priority $ \number -> unless (number >= 0 && number <= 1) (fail "Invalid MCP priority")
      modified <- value .:! "lastModified" :: Parser (Maybe Text)
      forM_ modified $ \timestamp -> unless (validModified timestamp) (fail "Invalid modification timestamp")
    void (fields .:! "_meta" :: Parser (Maybe Object))
    pure (McpContent fields)
    where
      media fields = do
        fields .: "data" >>= validateBase64
        void (fields .: "mimeType" :: Parser Text)

-- Match the reference's atob acceptance without changing the supplied text.
validateBase64 :: Text -> Parser ()
validateBase64 value = unless valid (fail "Invalid Base64 content")
  where
    compact = Text.filter (`notElem` ("\t\n\f\r " :: String)) value
    body = Text.dropWhileEnd (== '=') compact
    padding = Text.length compact - Text.length body
    remainder = Text.length body `mod` 4
    valid =
      Text.all alphabet body && remainder /= 1 && case padding of
        0 -> True
        1 -> remainder == 3
        2 -> remainder == 2
        _ -> False
    alphabet char = isAsciiUpper char || isAsciiLower char || isDigit char || char `elem` ("+/" :: String)

validModified :: Text -> Bool
validModified value = Text.length value >= 17 && Text.index value 10 == 'T' && not (Text.any (== 'z') value) && Text.take 2 (Text.drop 17 normalized) /= "60" && isJust (mkRfc3339Timestamp normalized)
  where
    normalized = case Text.uncons (Text.drop 16 value) of
      Just (zone, _) | zone `elem` ("Z+-" :: String) -> Text.take 16 value <> ":00" <> Text.drop 16 value
      _ -> value

textContent :: Text -> McpContent
textContent text = McpContent (KeyMap.fromList ["type" .= String "text", "text" .= text])

jsonContent :: Object -> Either McpToolError McpContent
jsonContent = either (const (Left InvalidMcpToolResult)) Right . parseEither parseJSON . Object

data McpToolResult = McpToolResult
  { toolResultContent :: ![McpContent],
    toolResultIsError :: !(Maybe Bool),
    toolResultStructuredContent :: !(Maybe Object),
    toolResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show McpToolResult where
  show _ = "McpToolResult <redacted>"

instance FromJSON McpToolResult where
  parseJSON = withObject "McpToolResult" $ \fields -> do
    void (fields .:! "_meta" :: Parser (Maybe Object))
    McpToolResult <$> fields .: "content" <*> fields .:! "isError" <*> fields .:! "structuredContent" <*> pure (additionalFields ["content", "isError", "structuredContent"] fields)

instance ToJSON McpToolResult where
  toJSON result = objectWithAdditionalFields ["content", "isError", "structuredContent"] (toolResultAdditionalFields result) (["content" .= toolResultContent result] <> optionalField "isError" (toolResultIsError result) <> optionalField "structuredContent" (toolResultStructuredContent result))

textResult :: Text -> McpToolResult
textResult text = McpToolResult [textContent text] Nothing Nothing mempty

errorResult :: Text -> McpToolResult
errorResult text = (textResult text) {toolResultIsError = Just True}

structuredResult :: Object -> McpToolResult
structuredResult value = McpToolResult [textContent (Text.decodeUtf8 (BL.toStrict (encode value)))] (Just False) (Just value) mempty

data McpTool = McpTool !Text !Text !McpSchema !(Maybe McpSchema) !(Object -> IO McpToolResult)

instance Show McpTool where
  show _ = "McpTool <redacted>"

rawTool :: Text -> Text -> McpSchema -> (Object -> IO McpToolResult) -> Either McpToolError McpTool
rawTool name description input action = do
  unless (validName name) (Left InvalidMcpToolName)
  pure (McpTool name description input Nothing action)

typedTool :: (FromJSON a) => Text -> Text -> McpSchema -> (a -> IO McpToolResult) -> Either McpToolError McpTool
typedTool name description input action = rawTool name description input $ \arguments ->
  case parseEither parseJSON (Object arguments) of
    Left _ -> pure (errorResult "Invalid tool arguments")
    Right value -> action value

structuredTool :: (FromJSON a, ToJSON b) => Text -> Text -> McpSchema -> McpSchema -> (a -> IO b) -> Either McpToolError McpTool
structuredTool name description input output action = do
  tool <- typedTool name description input $ \arguments -> do
    result <- action arguments
    case toJSON result of
      Object fields -> pure (structuredResult fields)
      _ -> pure (errorResult "Invalid structured tool output")
  case tool of
    McpTool label summary schema _ invoke -> pure (McpTool label summary schema (Just output) invoke)

toolName :: McpTool -> Text
toolName (McpTool name _ _ _ _) = name

toolDescription :: McpTool -> Text
toolDescription (McpTool _ description _ _ _) = description

toolInputSchema :: McpTool -> Object
toolInputSchema (McpTool _ _ input _ _) = mcpSchemaObject input

toolOutputSchema :: McpTool -> Maybe Object
toolOutputSchema (McpTool _ _ _ output _) = mcpSchemaObject <$> output

-- | Argument/schema/handler failures become tool errors. An invalid result
-- envelope remains a protocol-level failure. Async exceptions are preserved.
invokeTool :: McpTool -> Object -> IO (Either McpToolError McpToolResult)
invokeTool (McpTool _ _ input output action) arguments = do
  attempted <- trySync $ do
    validInput <- evaluate (matchesMcpSchema input (Object arguments))
    if not validInput
      then pure (Right (errorResult "Invalid tool arguments"))
      else do
        response <- action arguments
        encoded <- evaluate (force (toJSON response))
        case parseEither parseJSON encoded of
          Left _ -> pure (Left InvalidMcpToolResult)
          Right result -> case (output, toolResultIsError result, toolResultStructuredContent result) of
            (_, Just True, _) -> pure (Right result)
            (Nothing, _, _) -> pure (Right result)
            (Just schema, _, Just fields) -> do
              validOutput <- evaluate (matchesMcpSchema schema (Object fields))
              pure (Right (if validOutput then result else errorResult "Invalid structured tool output"))
            (Just _, _, Nothing) -> pure (Right (errorResult "Missing structured tool output"))
  pure (fromRight (Right (errorResult "Tool handler failed")) attempted)

validName :: Text -> Bool
validName name = not (Text.null name) && Text.length name <= 128 && Text.all (\char -> isAsciiLower char || isAsciiUpper char || isDigit char || char `elem` ("._-" :: String)) name
