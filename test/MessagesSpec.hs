{-# LANGUAGE OverloadedStrings #-}

module MessagesSpec (messageTests) where

import Control.Monad (foldM, forM_)
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
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Content
import Factory.Droid.Schema.Enums
import Factory.Droid.Schema.Messages
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

messageTests :: Value -> TestTree
messageTests schema =
  testGroup
    "Messages"
    [ testCase "message schemas differ only in content representation" $ do
        ordinary <- either assertFailure pure (properties "FactoryDroidMessageSchema" schema)
        cached <- either assertFailure pure (properties "FactoryDroidMessageWithCachingSchema" schema)
        KeyMap.delete "content" ordinary @?= KeyMap.delete "content" cached
        sort (KeyMap.keys ordinary) @?= sort (KeyMap.keys fullJSON)
        items <- either assertFailure pure (schemaAt ["definitions", "FactoryDroidMessageWithCachingSchema", "properties", "content", "items", "allOf"] schema)
        items @?= toJSON [object ["$ref" .= String "#/definitions/ContentBlockSchema"], object ["$ref" .= String "#/definitions/CacheLabelSchema"]],
      testCase "complete message golden maps every field" $ do
        fromJSON (Object fullJSON) @?= Success fullMessage
        toJSON fullMessage @?= Object fullJSON
        eitherDecode (encode fullMessage) @?= Right fullMessage,
      testCase "minimal message omits all optional fields" $ do
        fromJSON (Object minimalJSON) @?= Success minimalMessage
        toJSON minimalMessage @?= Object minimalJSON,
      testCase "every required message field is enforced" $ do
        required <- either assertFailure pure (schemaAt ["definitions", "FactoryDroidMessageSchema", "required"] schema)
        case fromJSON required :: Result [Key] of
          Error err -> assertFailure err
          Success keys -> forM_ keys $ \key -> rejects (Proxy @FactoryDroidMessage) (Object (KeyMap.delete key fullJSON)),
      testCase "only openaiPhase accepts null" $
        forM_ (filter (/= "openaiPhase") (KeyMap.keys fullJSON)) $ \key ->
          rejects (Proxy @FactoryDroidMessage) (Object (KeyMap.insert key Null fullJSON)),
      testCase "OpenAI phase preserves absence, null and both literals" $
        forM_ [(Nothing, Nothing), (Just Nothing, Just Null), (Just (Just CommentaryPhase), Just (String "commentary")), (Just (Just FinalAnswerPhase), Just (String "final_answer"))] $ \(phase, field) -> do
          let value = minimalMessage {messageOpenAIPhase = phase}
              wire = maybe minimalJSON (\item -> KeyMap.insert "openaiPhase" item minimalJSON) field
          fromJSON (Object wire) @?= Success value
          toJSON value @?= Object wire
          eitherDecode (encode value) @?= Right value,
      testCase "unknown constrained message fields are rejected" $
        forM_ ["apiProvider", "openaiPhase", "chatCompletionReasoningField", "hookStatus", "role", "visibility", "interactionMode", "reasoningEffort", "userMessageSource"] $ \key ->
          rejects (Proxy @FactoryDroidMessage) (Object (KeyMap.insert key (String "future") fullJSON)),
      enumTests schema ["apiProvider", "enum"] (Proxy @ApiProvider),
      enumTests schema ["chatCompletionReasoningField", "enum"] (Proxy @ChatCompletionReasoningField),
      enumTests schema ["hookStatus", "enum"] (Proxy @HookStatus),
      testCase "OpenAI phase enum matches its nullable schema branch" $ do
        alternatives <- either assertFailure pure (schemaAt ["definitions", "FactoryDroidMessageSchema", "properties", "openaiPhase", "anyOf"] schema)
        case alternatives of
          Array members -> case foldr (:) [] members of
            [branch, nullBranch] -> do
              literals <- either assertFailure pure (schemaAt ["enum"] branch)
              literals @?= toJSON [CommentaryPhase, FinalAnswerPhase]
              nullBranch @?= object ["type" .= String "null"]
            _ -> assertFailure "Unexpected phase alternatives"
          _ -> assertFailure "Phase alternatives are not an array",
      testCase "message extensions cannot override or reintroduce known fields" $ do
        let value = fullMessage {messageAdditionalFields = KeyMap.singleton "future" extension}
        toJSON value @?= Object (KeyMap.insert "future" extension fullJSON)
        fromJSON (toJSON value) @?= Success value
        forM_ (KeyMap.keys fullJSON) $ \key ->
          toJSON (minimalMessage {messageAdditionalFields = KeyMap.singleton key (String "injected")}) @?= Object minimalJSON,
      testCase "cached message uses the same metadata and typed cache controls" $ do
        let control = EphemeralCache (Just CacheOneHour) mempty
            value = fmap (\block -> CachedContentBlock block (Just control)) fullMessage
            cachedBlock = KeyMap.insert "cache_control" (object ["type" .= String "ephemeral", "ttl" .= String "1h"]) blockJSON
            wire = KeyMap.insert "content" (toJSON [Object cachedBlock]) fullJSON
        fromJSON (Object wire) @?= Success value
        toJSON value @?= Object wire
        eitherDecode (encode value) @?= Right value
        let absent = fmap (`CachedContentBlock` Nothing) minimalMessage
        toJSON absent @?= Object minimalJSON
        fromJSON (Object minimalJSON) @?= Success absent,
      testCase "ordinary blocks allow an opaque cache extension; cached blocks validate it" $ do
        let wire = Object (KeyMap.insert "content" (toJSON [Object (KeyMap.insert "cache_control" Null blockJSON)]) minimalJSON)
        case fromJSON wire :: Result FactoryDroidMessage of
          Error err -> assertFailure err
          Success value -> toJSON value @?= wire
        rejects (Proxy @FactoryDroidMessageWithCaching) wire,
      testCase "hooks have typed full and minimal codecs" $ do
        fromJSON (Object commandJSON) @?= Success hookCommand
        toJSON hookCommand @?= Object commandJSON
        fromJSON (Object resultJSON) @?= Success hookResult
        toJSON hookResult @?= Object resultJSON
        let command = hookCommand {hookCommandTimeout = Nothing}
            result = hookResult {hookResultSuppressOutput = Nothing}
        toJSON command @?= Object (KeyMap.delete "timeout" commandJSON)
        toJSON result @?= Object (KeyMap.delete "suppressOutput" resultJSON)
        fromJSON (toJSON command) @?= Success command
        fromJSON (toJSON result) @?= Success result,
      testCase "hook fixtures cover all inline fields" $ do
        commandFields <- either assertFailure pure (schemaAt ["definitions", "FactoryDroidMessageSchema", "properties", "hookCommands", "items", "properties"] schema)
        resultFields <- either assertFailure pure (schemaAt ["definitions", "FactoryDroidMessageSchema", "properties", "hookResults", "items", "properties"] schema)
        assertKeys commandJSON commandFields
        assertKeys resultJSON resultFields,
      testCase "hooks reject missing, null and mistyped fields" $ do
        rejects (Proxy @PersistedHookCommand) (object [])
        forM_ ["exitCode", "stdout", "stderr"] $ \key -> rejects (Proxy @PersistedHookResult) (Object (KeyMap.delete key resultJSON))
        forM_ (KeyMap.keys commandJSON) $ \key -> rejects (Proxy @PersistedHookCommand) (Object (KeyMap.insert key Null commandJSON))
        forM_ (KeyMap.keys resultJSON) $ \key -> rejects (Proxy @PersistedHookResult) (Object (KeyMap.insert key Null resultJSON))
        rejects (Proxy @FactoryDroidMessage) (Object (KeyMap.insert "hookCommands" (toJSON [object ["command" .= Number 1]]) fullJSON))
        rejects (Proxy @FactoryDroidMessage) (Object (KeyMap.insert "hookResults" (toJSON [object []]) fullJSON)),
      testCase "hook extensions survive without replacing typed fields" $ do
        let command = hookCommand {hookCommandAdditionalFields = KeyMap.singleton "future" extension}
            result = hookResult {hookResultAdditionalFields = KeyMap.singleton "future" extension}
        eitherDecode (encode command) @?= Right command
        eitherDecode (encode result) @?= Right result
        forM_ (KeyMap.keys commandJSON) $ \key ->
          toJSON (hookCommand {hookCommandTimeout = Nothing, hookCommandAdditionalFields = KeyMap.singleton key Null}) @?= Object (KeyMap.delete "timeout" commandJSON)
        forM_ (KeyMap.keys resultJSON) $ \key ->
          toJSON (hookResult {hookResultSuppressOutput = Nothing, hookResultAdditionalFields = KeyMap.singleton key Null}) @?= Object (KeyMap.delete "suppressOutput" resultJSON),
      testCase "message and hook objects reject other JSON kinds" $
        forM_ [Null, Number 0, Bool True, String "message", Array mempty] $ \value -> do
          rejects (Proxy @FactoryDroidMessage) value
          rejects (Proxy @FactoryDroidMessageWithCaching) value
          rejects (Proxy @PersistedHookCommand) value
          rejects (Proxy @PersistedHookResult) value
    ]

enumTests :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => Value -> [Key] -> Proxy a -> TestTree
enumTests schema path _ = testCase (show path) $ do
  literals <- either assertFailure pure (schemaAt (["definitions", "FactoryDroidMessageSchema", "properties"] <> path) schema)
  let values = [minBound .. maxBound] :: [a]
  toJSON values @?= literals
  forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
  forM_ [Null, Number 0, Bool True, String "future", Object mempty, Array mempty] $ rejects (Proxy @a)

fullMessage :: FactoryDroidMessage
fullMessage =
  Message
    { messageId = "message-id",
      messageRole = RoleAssistant,
      messageContent = [ContentText (TextBlock "hello" (BaseContentBlock (Just "block-id") mempty))],
      messageCreatedAt = 12345678901234567890.125,
      messageUpdatedAt = 12345678901234567891.875,
      messageParentId = Just "parent-id",
      messageVisibility = Just VisibilityBoth,
      messageOpenAIMessageId = Just "openai-message-id",
      messageOpenAIPhase = Just (Just CommentaryPhase),
      messageOpenAIEncryptedContent = Just "opaque encrypted content",
      messageOpenAIReasoningId = Just "reasoning-id",
      messageOpenAIReasoningSummary = Just "summary",
      messageGeminiThoughtSignature = Just "legacy opaque signature",
      messageChatCompletionReasoningField = Just ReasoningContentField,
      messageChatCompletionReasoningContent = Just "provider content",
      messageIsUserVisible = Just True,
      messageIsError = Just False,
      messageUserSource = Just OriginCliExec,
      messageInteractionMode = Just DroidSpec,
      messageModelId = Just "model-id",
      messageRouterId = Just "router-id",
      messageReasoningEffort = Just ReasoningHigh,
      messageApiProvider = Just ApiBedrockAnthropic,
      messageHookEventName = Just "event-name",
      messageHookMatcher = Just "matcher",
      messageHookCommands = Just [hookCommand],
      messageHookStatus = Just HookCompleted,
      messageHookResults = Just [hookResult],
      messageHookToolCallId = Just "tool-call-id",
      messageHookParentId = Just "hook-parent-id",
      messageHookOrder = Just 2.5,
      messageHookPreventedAction = Just True,
      messageHiddenFromUserViews = Just False,
      messageHookStartTime = Just 100.25,
      messageHookEndTime = Just 101.75,
      messageIsParallelExecution = Just True,
      messageParallelGroupId = Just "parallel-group-id",
      messageAdditionalFields = mempty
    }

minimalMessage :: FactoryDroidMessage
minimalMessage =
  fullMessage
    { messageParentId = Nothing,
      messageVisibility = Nothing,
      messageOpenAIMessageId = Nothing,
      messageOpenAIPhase = Nothing,
      messageOpenAIEncryptedContent = Nothing,
      messageOpenAIReasoningId = Nothing,
      messageOpenAIReasoningSummary = Nothing,
      messageGeminiThoughtSignature = Nothing,
      messageChatCompletionReasoningField = Nothing,
      messageChatCompletionReasoningContent = Nothing,
      messageIsUserVisible = Nothing,
      messageIsError = Nothing,
      messageUserSource = Nothing,
      messageInteractionMode = Nothing,
      messageModelId = Nothing,
      messageRouterId = Nothing,
      messageReasoningEffort = Nothing,
      messageApiProvider = Nothing,
      messageHookEventName = Nothing,
      messageHookMatcher = Nothing,
      messageHookCommands = Nothing,
      messageHookStatus = Nothing,
      messageHookResults = Nothing,
      messageHookToolCallId = Nothing,
      messageHookParentId = Nothing,
      messageHookOrder = Nothing,
      messageHookPreventedAction = Nothing,
      messageHiddenFromUserViews = Nothing,
      messageHookStartTime = Nothing,
      messageHookEndTime = Nothing,
      messageIsParallelExecution = Nothing,
      messageParallelGroupId = Nothing
    }

minimalJSON, fullJSON, blockJSON, commandJSON, resultJSON :: Object
minimalJSON = KeyMap.fromList ["id" .= String "message-id", "role" .= String "assistant", "content" .= [Object blockJSON], "createdAt" .= Number 12345678901234567890.125, "updatedAt" .= Number 12345678901234567891.875]
fullJSON =
  KeyMap.union minimalJSON $
    KeyMap.fromList
      [ "parentId" .= String "parent-id",
        "visibility" .= String "both",
        "openaiMessageId" .= String "openai-message-id",
        "openaiPhase" .= String "commentary",
        "openaiEncryptedContent" .= String "opaque encrypted content",
        "openaiReasoningId" .= String "reasoning-id",
        "openaiReasoningSummary" .= String "summary",
        "geminiThoughtSignature" .= String "legacy opaque signature",
        "chatCompletionReasoningField" .= String "reasoning_content",
        "chatCompletionReasoningContent" .= String "provider content",
        "isUserVisible" .= True,
        "isError" .= False,
        "userMessageSource" .= String "cli_exec",
        "interactionMode" .= String "spec",
        "modelId" .= String "model-id",
        "routerId" .= String "router-id",
        "reasoningEffort" .= String "high",
        "apiProvider" .= String "bedrock_anthropic",
        "hookEventName" .= String "event-name",
        "hookMatcher" .= String "matcher",
        "hookCommands" .= [Object commandJSON],
        "hookStatus" .= String "completed",
        "hookResults" .= [Object resultJSON],
        "hookToolCallId" .= String "tool-call-id",
        "hookParentId" .= String "hook-parent-id",
        "hookOrder" .= Number 2.5,
        "hookPreventedAction" .= True,
        "hiddenFromUserViews" .= False,
        "hookStartTime" .= Number 100.25,
        "hookEndTime" .= Number 101.75,
        "isParallelExecution" .= True,
        "parallelGroupId" .= String "parallel-group-id"
      ]
blockJSON = KeyMap.fromList ["type" .= String "text", "text" .= String "hello", "id" .= String "block-id"]
commandJSON = KeyMap.fromList ["command" .= String "echo fixture", "timeout" .= Number 1.25]
resultJSON = KeyMap.fromList ["exitCode" .= Number (-0.5), "stdout" .= String "output", "stderr" .= String "error output", "suppressOutput" .= False]

hookCommand :: PersistedHookCommand
hookCommand = PersistedHookCommand "echo fixture" (Just 1.25) mempty

hookResult :: PersistedHookResult
hookResult = PersistedHookResult (-0.5) "output" "error output" (Just False) mempty

extension :: Value
extension = object ["nested" .= [Null, Bool True]]

properties :: Key -> Value -> Either String Object
properties name schema =
  schemaAt ["definitions", name, "properties"] schema >>= \case
    Object fields -> Right fields
    _ -> Left "Schema properties are not an object"

assertKeys :: Object -> Value -> IO ()
assertKeys fixture (Object fields) = sort (KeyMap.keys fixture) @?= sort (KeyMap.keys fields)
assertKeys _ _ = assertFailure "Schema properties are not an object"

schemaAt :: [Key] -> Value -> Either String Value
schemaAt keys value = foldM (\node key -> parseEither (withObject "schema node" (.: key)) node) value keys

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of
  Error _ -> pure ()
  Success _ -> assertFailure "Invalid JSON was accepted"
