{-# LANGUAGE OverloadedStrings #-}

module SystemPromptSpec (systemPromptTests) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Factory.Droid.Schema.SystemPrompt
import SchemaTest (rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

systemPromptTests :: Value -> TestTree
systemPromptTests schema =
  testGroup
    "System prompt configuration"
    [ testCase "custom and appended prompts preserve content exactly" $ do
        custom <- maybe (assertFailure "Expected custom prompt") pure (customSystemPrompt " \ncustom سلام\xfeff ")
        appended <- maybe (assertFailure "Expected appended prompt") pure (appendedSystemPrompt "  append\n")
        toJSON custom @?= String " \ncustom سلام\xfeff "
        fromJSON (toJSON custom) @?= Success custom
        toJSON appended @?= object ["type" .= String "preset", "preset" .= String "droid", "append" .= String "  append\n"]
        fromJSON (toJSON appended) @?= Success appended,
      testCase "canonical preset fields follow the supplied schema" $ do
        let shape = schemaAt ["definitions", "SystemPromptConfigSchema", "anyOf"] schema >>= schemaIndex 1
        (shape >>= schemaAt ["properties", "type", "const"]) @?= Right (String "preset")
        (shape >>= schemaAt ["properties", "preset", "const"]) @?= Right (String "droid")
        (shape >>= schemaAt ["required"]) @?= Right (toJSON ["type", "preset", "append" :: Text.Text])
        (shape >>= schemaAt ["additionalProperties"]) @?= Right (Bool False),
      testGroup
        "ECMAScript whitespace alone is rejected"
        [ testCase (show code) $ do
            let text = Text.singleton (toEnum code)
            customSystemPrompt text @?= Nothing
            appendedSystemPrompt text @?= Nothing
            rejects (Proxy @SystemPromptConfig) (String text)
            rejects (Proxy @SystemPromptConfig) (object ["append" .= text])
        | code <- [0x9, 0xa, 0xb, 0xc, 0xd, 0x20, 0xa0, 0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200a, 0x2028, 0x2029, 0x202f, 0x205f, 0x3000, 0xfeff]
        ],
      testCase "empty content is rejected" $ do
        customSystemPrompt "" @?= Nothing
        appendedSystemPrompt "" @?= Nothing
        rejects (Proxy @SystemPromptConfig) (String ""),
      testCase "NEL and removed Mongolian whitespace follow JavaScript rather than strip" $ do
        let text = "\x85\x180e"
        prompt <- maybe (assertFailure "Expected JavaScript non-whitespace") pure (customSystemPrompt text)
        toJSON prompt @?= String text
        fromJSON (String text) @?= Success prompt,
      testCase "preset objects normalize tags before validating their canonical shape" $ do
        expected <- maybe (assertFailure "Expected appended prompt") pure (appendedSystemPrompt "instructions")
        fromJSON (object ["append" .= String "instructions"]) @?= Success expected
        fromJSON (object ["type" .= Null, "preset" .= [Bool False], "append" .= String "instructions"]) @?= Success expected,
      testCase "unknown fields and malformed top-level/content values remain invalid" $ do
        rejects (Proxy @SystemPromptConfig) (object ["append" .= String "ok", "extra" .= Null])
        rejects (Proxy @SystemPromptConfig) (object ["type" .= String "preset", "preset" .= String "droid"])
        rejects (Proxy @SystemPromptConfig) (object ["append" .= Null])
        rejects (Proxy @SystemPromptConfig) (object ["append" .= Number 1])
        rejects (Proxy @SystemPromptConfig) Null
        rejects (Proxy @SystemPromptConfig) (Array mempty),
      testCase "Show redacts system prompt contents" $ do
        prompt <- maybe (assertFailure "Expected prompt") pure (customSystemPrompt "private instruction")
        show prompt @?= "SystemPromptConfig <redacted>"
    ]
