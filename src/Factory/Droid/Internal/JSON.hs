-- | JSON operations shared by wire codecs.
module Factory.Droid.Internal.JSON
  ( additionalFields,
    objectWithAdditionalFields,
    fieldsWithAdditionalFields,
    optionalField,
    requireLiteral,
    rejectUnknownFields,
    enumOptions,
    isEcmaWhitespace,
  )
where

import Control.Monad (unless)
import Data.Aeson (Object, Options (constructorTagModifier), ToJSON, Value (Object), defaultOptions, (.:), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser)

-- | Remove every reserved field, including optional fields currently omitted.
additionalFields :: [Key] -> Object -> Object
additionalFields keys fields = foldr KeyMap.delete fields keys

-- | Preserve extensions without allowing them to override typed fields.
objectWithAdditionalFields :: [Key] -> Object -> [Pair] -> Value
objectWithAdditionalFields keys extras fields =
  Object (fieldsWithAdditionalFields keys extras fields)

-- | The object-valued form, for composing schema intersections.
fieldsWithAdditionalFields :: [Key] -> Object -> [Pair] -> Object
fieldsWithAdditionalFields keys extras fields =
  KeyMap.union (KeyMap.fromList fields) (additionalFields keys extras)

-- | Omit an absent field; a present nullable value still encodes as JSON null.
optionalField :: (ToJSON a) => Key -> Maybe a -> [Pair]
optionalField key = maybe [] (\value -> [key .= value])

-- | Require a literal JSON value without echoing its untrusted input.
requireLiteral :: Key -> Value -> Object -> Parser ()
requireLiteral key expected fields = do
  actual <- fields .: key
  unless (actual == expected) (fail ("Unexpected literal in field " <> show key))

-- | Reject extensions on a closed record without disclosing unknown keys.
rejectUnknownFields :: [Key] -> Object -> Parser ()
rejectUnknownFields keys fields =
  unless (KeyMap.null (additionalFields keys fields)) (fail "Unexpected object fields")

-- | Standard Aeson enum options for a fixed constructor prefix and spelling.
enumOptions :: String -> (String -> String) -> Options
enumOptions prefix render =
  defaultOptions {constructorTagModifier = render . drop (length prefix)}

-- ECMAScript whitespace differs from Haskell/Python at NEL and BOM.
isEcmaWhitespace :: Char -> Bool
isEcmaWhitespace char = char `elem` ("\t\n\v\f\r \xa0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\xfeff" :: String)
