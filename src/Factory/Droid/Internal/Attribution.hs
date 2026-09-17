{-# LANGUAGE OverloadedStrings #-}

-- | Producer-owned identity shared by high-level session and REST boundaries.
-- This descriptive identity is not the protocol's closed SDK-language union.
module Factory.Droid.Internal.Attribution (sdkIdentity, withSdkTag) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import Factory.Droid.Schema.Session (SessionTag (..), mkSessionTagName, sessionTagNameText)
import Paths_droid_sdk (version)

sdkVersion :: Text
sdkVersion = Text.pack (showVersion version)

sdkIdentity :: Text
sdkIdentity = "haskell/" <> sdkVersion

-- The reserved tag identifies this producer, not a caller-supplied SDK brand.
-- Other tags (including their order and extensions) remain caller-owned.
withSdkTag :: [SessionTag] -> [SessionTag]
withSdkTag tags = filter ((/= "sdk") . sessionTagNameText . sessionTagName) tags <> [sdkTag]
  where
    sdkTag = case mkSessionTagName "sdk" of
      Just name -> SessionTag name (Just (KeyMap.fromList [("language", "haskell"), ("version", sdkVersion)])) mempty
      Nothing -> error "Invariant: the SDK tag name is nonempty"
