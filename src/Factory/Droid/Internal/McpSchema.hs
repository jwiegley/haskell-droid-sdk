{-# LANGUAGE OverloadedStrings #-}

-- | Retained MCP object-schema documents. Pure construction checks the outer
-- contract; complete compilation and validation use the isolated native worker.
module Factory.Droid.Internal.McpSchema
  ( McpSchema,
    McpSchemaError (..),
    mkMcpSchema,
    openObjectSchema,
    mcpSchemaObject,
    validateMcpSchema,
    matchesMcpSchema,
  )
where

import Control.Monad (unless, when)
import Data.Aeson (Object, Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Factory.Droid.MCP.Validator (SchemaValidatorOptions, validateSchemaDefinition, validateSchemaValue)

newtype McpSchema = McpSchema Object

instance Show McpSchema where show _ = "McpSchema <redacted>"

data McpSchemaError = InvalidMcpSchema | UnsupportedMcpSchema
  deriving stock (Eq, Show)

mkMcpSchema :: Object -> Either McpSchemaError McpSchema
mkMcpSchema fields = do
  unless (KeyMap.lookup "type" fields == Just (String "object")) (Left InvalidMcpSchema)
  when (BL.length (encode fields) > 65536) (Left InvalidMcpSchema)
  pure (McpSchema fields)

openObjectSchema :: McpSchema
openObjectSchema = McpSchema (KeyMap.singleton "type" (String "object"))

mcpSchemaObject :: McpSchema -> Object
mcpSchemaObject (McpSchema fields) = fields

validateMcpSchema :: SchemaValidatorOptions -> McpSchema -> IO ()
validateMcpSchema options = validateSchemaDefinition options . Object . mcpSchemaObject

matchesMcpSchema :: SchemaValidatorOptions -> McpSchema -> Value -> IO Bool
-- The value constructor already proves the only constraint in this schema.
matchesMcpSchema _ (McpSchema fields) (Object _)
  | fields == mcpSchemaObject openObjectSchema = pure True
matchesMcpSchema options schema value = validateSchemaValue options (Object (mcpSchemaObject schema)) value
