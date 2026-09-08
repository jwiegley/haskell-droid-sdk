{-# LANGUAGE OverloadedStrings #-}

-- | Checked, bounded JSON Schema profile for hosted tool arguments/results.
module Factory.Droid.Internal.McpSchema
  ( McpSchema,
    McpSchemaError (..),
    mkMcpSchema,
    openObjectSchema,
    mcpSchemaObject,
    matchesMcpSchema,
  )
where

import Control.Monad (unless, when)
import Data.Aeson (Object, Value (..), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList, traverse_)
import Data.JSON.JSONSchema qualified as Schema
import Data.List (nub)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Text.Read (readMaybe)

-- The advertised document is retained; the validation document makes $ref
-- siblings conjunctive, correcting the dependency's older short-circuit rule.
data McpSchema = McpSchema !Object !Value

instance Show McpSchema where
  show _ = "McpSchema <redacted>"

data McpSchemaError = InvalidMcpSchema | UnsupportedMcpSchema
  deriving stock (Eq, Show)

mkMcpSchema :: Object -> Either McpSchemaError McpSchema
mkMcpSchema fields = do
  unless (KeyMap.lookup "type" fields == Just (String "object")) (Left InvalidMcpSchema)
  when (BL.length (encode fields) > 65536) (Left InvalidMcpSchema)
  checked <- check (Object fields) 0 (Object fields)
  pure (McpSchema fields checked)

openObjectSchema :: McpSchema
openObjectSchema = McpSchema fields (Object fields)
  where
    fields = KeyMap.singleton "type" (String "object")

mcpSchemaObject :: McpSchema -> Object
mcpSchemaObject (McpSchema fields _) = fields

matchesMcpSchema :: McpSchema -> Value -> Bool
matchesMcpSchema (McpSchema _ checked) = Schema.validateJSONSchema checked

check :: Value -> Int -> Value -> Either McpSchemaError Value
check root depth value
  | depth > 64 = Left InvalidMcpSchema
  | otherwise = case value of
      Bool _ -> Right value
      Object fields -> do
        normalized <- traverseField fields
        case KeyMap.lookup "$ref" normalized of
          Just reference -> do
            let siblings = KeyMap.delete "$ref" normalized
                referenceSchema = Object (KeyMap.singleton "$ref" reference)
            if KeyMap.null siblings
              then pure (Object normalized)
              else case KeyMap.lookup "allOf" siblings of
                Just (Array schemas) -> pure (Object (KeyMap.insert "allOf" (Array (Vector.snoc schemas referenceSchema)) siblings))
                Nothing -> pure (Object (KeyMap.insert "allOf" (Array (Vector.singleton referenceSchema)) siblings))
                _ -> Left InvalidMcpSchema
          Nothing -> pure (Object normalized)
      _ -> Left InvalidMcpSchema
  where
    recurse = check root (depth + 1)
    traverseField fields = KeyMap.fromList <$> traverse inspect (KeyMap.toList fields)
    inspect (key, item) = (key,) <$> keyword (Key.toText key) item
    keyword key item
      | key `elem` ["title", "description", "$comment", "default", "examples", "readOnly", "writeOnly", "deprecated", "format", "contentEncoding", "contentMediaType"] || "x-" `Text.isPrefixOf` key = Right item
      | key == "$schema" = if item == String "https://json-schema.org/draft/2020-12/schema" then Right item else Left UnsupportedMcpSchema
      | key == "$ref" = case item of
          String reference | "~01" `Text.isInfixOf` reference -> Left UnsupportedMcpSchema
          String reference | reference == "#" || ("#/" `Text.isPrefixOf` reference && not (Text.any (== '%') reference)) -> case resolve root reference of
            Just (Object _) -> Right item
            Just (Bool _) -> Right item
            _ -> Left InvalidMcpSchema
          _ -> Left UnsupportedMcpSchema
      | key `elem` ["$defs", "definitions", "properties", "dependentSchemas"] = case item of
          Object entries -> Object <$> traverse recurse entries
          _ -> Left InvalidMcpSchema
      | key `elem` ["additionalProperties", "propertyNames", "items", "contains", "not", "if", "then", "else"] = recurse item
      | key `elem` ["prefixItems", "allOf", "anyOf", "oneOf"] = case item of
          Array values -> do
            when (Vector.null values) (Left InvalidMcpSchema)
            Array <$> traverse recurse values
          _ -> Left InvalidMcpSchema
      | key == "type" = case item of
          String name | name `elem` types -> Right item
          Array names -> do
            when (Vector.null names) (Left InvalidMcpSchema)
            validateNames names
            unless (all (`elem` map String types) names) (Left InvalidMcpSchema)
            Right item
          _ -> Left InvalidMcpSchema
      | key == "required" = case item of
          Array names -> validateNames names >> Right item
          _ -> Left InvalidMcpSchema
      | key == "dependentRequired" = case item of
          Object entries -> traverse_ (keyword "required") entries >> Right item
          _ -> Left InvalidMcpSchema
      | key `elem` ["minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum", "multipleOf"] = case item of
          Number number -> do
            when (key == "multipleOf" && number <= 0) (Left InvalidMcpSchema)
            Right item
          _ -> Left InvalidMcpSchema
      | key `elem` ["minLength", "maxLength", "minItems", "maxItems", "minContains", "maxContains", "minProperties", "maxProperties"] = case item of
          Number number | Just (count :: Int) <- toBoundedInteger number, count >= 0 -> Right item
          _ -> Left InvalidMcpSchema
      | key == "uniqueItems" = case item of
          Bool _ -> Right item
          _ -> Left InvalidMcpSchema
      | key == "const" = Right item
      | key == "enum" = case item of
          Array values | not (Vector.null values), length (nub (toList values)) == Vector.length values -> Right item
          _ -> Left InvalidMcpSchema
      | otherwise = Left UnsupportedMcpSchema
    validateNames values = do
      unless (all isText values) (Left InvalidMcpSchema)
      unless (length (nub (toList values)) == Vector.length values) (Left InvalidMcpSchema)
    isText (String _) = True
    isText _ = False
    types = ["null", "boolean", "string", "number", "integer", "array", "object"]

resolve :: Value -> Text -> Maybe Value
resolve root reference = case Text.stripPrefix "#" reference of
  Just "" -> Just root
  Just pointer -> Text.stripPrefix "/" pointer >>= traverse unescape . Text.splitOn "/" >>= walk root
  Nothing -> Nothing
  where
    walk value [] = Just value
    walk (Object fields) (keyword : name : rest)
      | keyword `elem` ["$defs", "definitions", "properties", "dependentSchemas"] = do
          Object entries <- KeyMap.lookup (Key.fromText keyword) fields
          target <- KeyMap.lookup (Key.fromText name) entries
          walk target rest
      | keyword `elem` ["prefixItems", "allOf", "anyOf", "oneOf"] = do
          Array values <- KeyMap.lookup (Key.fromText keyword) fields
          index <- readMaybe (Text.unpack name)
          unless (Text.pack (show (index :: Int)) == name) Nothing
          target <- values Vector.!? index
          walk target rest
    walk (Object fields) (keyword : rest)
      | keyword `elem` ["additionalProperties", "propertyNames", "items", "contains", "not", "if", "then", "else"] = KeyMap.lookup (Key.fromText keyword) fields >>= (`walk` rest)
    walk _ _ = Nothing
    unescape text = case Text.uncons text of
      Nothing -> Just ""
      Just ('~', rest) -> case Text.uncons rest of
        Just ('0', tailText) -> Text.cons '~' <$> unescape tailText
        Just ('1', tailText) -> Text.cons '/' <$> unescape tailText
        _ -> Nothing
      Just (char, rest) -> Text.cons char <$> unescape rest
