{-# LANGUAGE OverloadedStrings #-}

-- | Complete message records for Factory protocol 1.205.0. Numeric wire
-- values retain their decimal precision; provider signatures and hook output
-- are opaque data, not authenticated or executed by these codecs.
module Factory.Droid.Schema.Messages
  ( ApiProvider (..),
    OpenAIPhase (..),
    ChatCompletionReasoningField (..),
    HookStatus (..),
    PersistedHookCommand (..),
    PersistedHookResult (..),
    persistedHookResultObject,
    Message (..),
    FactoryDroidMessage,
    FactoryDroidMessageWithCaching,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (Object, String),
    withObject,
    withText,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON
  ( additionalFields,
    fieldsWithAdditionalFields,
    objectWithAdditionalFields,
    optionalField,
  )
import Factory.Droid.Schema.Content (CachedContentBlock, ContentBlock)
import Factory.Droid.Schema.Enums
  ( DroidInteractionMode,
    MessageRole,
    MessageVisibility,
    ReasoningEffort,
    SessionOrigin,
  )

-- | The API route that served a message. This differs from ModelProvider:
-- several routes may serve the same model provider.
data ApiProvider
  = ApiBedrock
  | ApiAnthropic
  | ApiVertexAnthropic
  | ApiBedrockAnthropic
  | ApiBedrockConverse
  | ApiBedrockOpenAI
  | ApiOpenAI
  | ApiAzureOpenAI
  | ApiGoogle
  | ApiXAI
  | ApiMistral
  | ApiFireworks
  | ApiBaseten
  | ApiSnowflake
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ApiProvider where
  parseJSON = withText "ApiProvider" $ \case
    "bedrock" -> pure ApiBedrock
    "anthropic" -> pure ApiAnthropic
    "vertex_anthropic" -> pure ApiVertexAnthropic
    "bedrock_anthropic" -> pure ApiBedrockAnthropic
    "bedrock_converse" -> pure ApiBedrockConverse
    "bedrock_openai" -> pure ApiBedrockOpenAI
    "openai" -> pure ApiOpenAI
    "azure_openai" -> pure ApiAzureOpenAI
    "google" -> pure ApiGoogle
    "xai" -> pure ApiXAI
    "mistral" -> pure ApiMistral
    "fireworks" -> pure ApiFireworks
    "baseten" -> pure ApiBaseten
    "snowflake" -> pure ApiSnowflake
    _ -> fail "Unknown API provider"

instance ToJSON ApiProvider where
  toJSON ApiBedrock = String "bedrock"
  toJSON ApiAnthropic = String "anthropic"
  toJSON ApiVertexAnthropic = String "vertex_anthropic"
  toJSON ApiBedrockAnthropic = String "bedrock_anthropic"
  toJSON ApiBedrockConverse = String "bedrock_converse"
  toJSON ApiBedrockOpenAI = String "bedrock_openai"
  toJSON ApiOpenAI = String "openai"
  toJSON ApiAzureOpenAI = String "azure_openai"
  toJSON ApiGoogle = String "google"
  toJSON ApiXAI = String "xai"
  toJSON ApiMistral = String "mistral"
  toJSON ApiFireworks = String "fireworks"
  toJSON ApiBaseten = String "baseten"
  toJSON ApiSnowflake = String "snowflake"

-- | The two OpenAI message phases; null is represented by the enclosing field.
data OpenAIPhase = CommentaryPhase | FinalAnswerPhase
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON OpenAIPhase where
  parseJSON = withText "OpenAIPhase" $ \case
    "commentary" -> pure CommentaryPhase
    "final_answer" -> pure FinalAnswerPhase
    _ -> fail "Unknown OpenAI phase"

instance ToJSON OpenAIPhase where
  toJSON CommentaryPhase = String "commentary"
  toJSON FinalAnswerPhase = String "final_answer"

-- | The field used by a chat-completion provider for reasoning content.
data ChatCompletionReasoningField = ReasoningField | ReasoningContentField
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ChatCompletionReasoningField where
  parseJSON = withText "ChatCompletionReasoningField" $ \case
    "reasoning" -> pure ReasoningField
    "reasoning_content" -> pure ReasoningContentField
    _ -> fail "Unknown chat-completion reasoning field"

instance ToJSON ChatCompletionReasoningField where
  toJSON ReasoningField = String "reasoning"
  toJSON ReasoningContentField = String "reasoning_content"

-- | Persisted hook execution status.
data HookStatus = HookExecuting | HookCompleted | HookError
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON HookStatus where
  parseJSON = withText "HookStatus" $ \case
    "executing" -> pure HookExecuting
    "completed" -> pure HookCompleted
    "error" -> pure HookError
    _ -> fail "Unknown hook status"

instance ToJSON HookStatus where
  toJSON HookExecuting = String "executing"
  toJSON HookCompleted = String "completed"
  toJSON HookError = String "error"

-- | Persisted command metadata. Decoding never executes the command.
data PersistedHookCommand = PersistedHookCommand
  { hookCommandText :: !Text,
    hookCommandTimeout :: !(Maybe Scientific),
    hookCommandAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON PersistedHookCommand where
  parseJSON = withObject "PersistedHookCommand" $ \fields ->
    PersistedHookCommand <$> fields .: "command" <*> fields .:! "timeout" <*> pure (additionalFields commandKeys fields)

instance ToJSON PersistedHookCommand where
  toJSON command =
    objectWithAdditionalFields commandKeys (hookCommandAdditionalFields command) $
      ["command" .= hookCommandText command] <> optionalField "timeout" (hookCommandTimeout command)

-- | Persisted hook output. The schema specifies a number for exitCode,
-- without imposing the integer/range restrictions of an OS process status.
data PersistedHookResult = PersistedHookResult
  { hookResultExitCode :: !Scientific,
    hookResultStdout :: !Text,
    hookResultStderr :: !Text,
    hookResultSuppressOutput :: !(Maybe Bool),
    hookResultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON PersistedHookResult where
  parseJSON = withObject "PersistedHookResult" $ \fields ->
    PersistedHookResult
      <$> fields .: "exitCode"
      <*> fields .: "stdout"
      <*> fields .: "stderr"
      <*> fields .:! "suppressOutput"
      <*> pure (additionalFields resultKeys fields)

instance ToJSON PersistedHookResult where
  toJSON = Object . persistedHookResultObject

-- | The same hook-result object encoding used by ToJSON, for typed extensions.
persistedHookResultObject :: PersistedHookResult -> Object
persistedHookResultObject result =
  fieldsWithAdditionalFields resultKeys (hookResultAdditionalFields result) $
    ["exitCode" .= hookResultExitCode result, "stdout" .= hookResultStdout result, "stderr" .= hookResultStderr result]
      <> optionalField "suppressOutput" (hookResultSuppressOutput result)

-- | A message parameterized by its content-block representation. Both wire
-- message schemas share these fields; only the content item schema differs.
--
-- 'messageOpenAIPhase' preserves three states: Nothing omits the field,
-- Just Nothing emits null, and Just (Just phase) emits a phase literal.
-- All other optional fields reject explicit null. Deprecated fields remain
-- available for decoding and round trips, without synthesizing new values.
data Message block = Message
  { messageId :: !Text,
    messageRole :: !MessageRole,
    messageContent :: ![block],
    messageCreatedAt :: !Scientific,
    messageUpdatedAt :: !Scientific,
    messageParentId :: !(Maybe Text),
    messageVisibility :: !(Maybe MessageVisibility),
    messageOpenAIMessageId :: !(Maybe Text),
    messageOpenAIPhase :: !(Maybe (Maybe OpenAIPhase)),
    messageOpenAIEncryptedContent :: !(Maybe Text),
    messageOpenAIReasoningId :: !(Maybe Text),
    messageOpenAIReasoningSummary :: !(Maybe Text),
    messageGeminiThoughtSignature :: !(Maybe Text),
    messageChatCompletionReasoningField :: !(Maybe ChatCompletionReasoningField),
    messageChatCompletionReasoningContent :: !(Maybe Text),
    messageIsUserVisible :: !(Maybe Bool),
    messageIsError :: !(Maybe Bool),
    messageUserSource :: !(Maybe SessionOrigin),
    messageInteractionMode :: !(Maybe DroidInteractionMode),
    messageModelId :: !(Maybe Text),
    messageRouterId :: !(Maybe Text),
    messageReasoningEffort :: !(Maybe ReasoningEffort),
    messageApiProvider :: !(Maybe ApiProvider),
    messageHookEventName :: !(Maybe Text),
    messageHookMatcher :: !(Maybe Text),
    messageHookCommands :: !(Maybe [PersistedHookCommand]),
    messageHookStatus :: !(Maybe HookStatus),
    messageHookResults :: !(Maybe [PersistedHookResult]),
    messageHookToolCallId :: !(Maybe Text),
    messageHookParentId :: !(Maybe Text),
    messageHookOrder :: !(Maybe Scientific),
    messageHookPreventedAction :: !(Maybe Bool),
    messageHiddenFromUserViews :: !(Maybe Bool),
    messageHookStartTime :: !(Maybe Scientific),
    messageHookEndTime :: !(Maybe Scientific),
    messageIsParallelExecution :: !(Maybe Bool),
    messageParallelGroupId :: !(Maybe Text),
    messageAdditionalFields :: !Object
  }
  deriving stock (Eq, Show, Functor)

-- | FactoryDroidMessageSchema, with ordinary content blocks.
type FactoryDroidMessage = Message ContentBlock

-- | FactoryDroidMessageWithCachingSchema, with typed outer-block cache labels.
type FactoryDroidMessageWithCaching = Message CachedContentBlock

instance (FromJSON block) => FromJSON (Message block) where
  parseJSON = withObject "Message" $ \fields ->
    Message
      <$> fields .: "id"
      <*> fields .: "role"
      <*> fields .: "content"
      <*> fields .: "createdAt"
      <*> fields .: "updatedAt"
      <*> fields .:! "parentId"
      <*> fields .:! "visibility"
      <*> fields .:! "openaiMessageId"
      <*> fields .:! "openaiPhase"
      <*> fields .:! "openaiEncryptedContent"
      <*> fields .:! "openaiReasoningId"
      <*> fields .:! "openaiReasoningSummary"
      <*> fields .:! "geminiThoughtSignature"
      <*> fields .:! "chatCompletionReasoningField"
      <*> fields .:! "chatCompletionReasoningContent"
      <*> fields .:! "isUserVisible"
      <*> fields .:! "isError"
      <*> fields .:! "userMessageSource"
      <*> fields .:! "interactionMode"
      <*> fields .:! "modelId"
      <*> fields .:! "routerId"
      <*> fields .:! "reasoningEffort"
      <*> fields .:! "apiProvider"
      <*> fields .:! "hookEventName"
      <*> fields .:! "hookMatcher"
      <*> fields .:! "hookCommands"
      <*> fields .:! "hookStatus"
      <*> fields .:! "hookResults"
      <*> fields .:! "hookToolCallId"
      <*> fields .:! "hookParentId"
      <*> fields .:! "hookOrder"
      <*> fields .:! "hookPreventedAction"
      <*> fields .:! "hiddenFromUserViews"
      <*> fields .:! "hookStartTime"
      <*> fields .:! "hookEndTime"
      <*> fields .:! "isParallelExecution"
      <*> fields .:! "parallelGroupId"
      <*> pure (additionalFields messageKeys fields)

instance (ToJSON block) => ToJSON (Message block) where
  toJSON message =
    objectWithAdditionalFields messageKeys (messageAdditionalFields message) $
      [ "id" .= messageId message,
        "role" .= messageRole message,
        "content" .= messageContent message,
        "createdAt" .= messageCreatedAt message,
        "updatedAt" .= messageUpdatedAt message
      ]
        <> optionalField "parentId" (messageParentId message)
        <> optionalField "visibility" (messageVisibility message)
        <> optionalField "openaiMessageId" (messageOpenAIMessageId message)
        <> optionalField "openaiPhase" (messageOpenAIPhase message)
        <> optionalField "openaiEncryptedContent" (messageOpenAIEncryptedContent message)
        <> optionalField "openaiReasoningId" (messageOpenAIReasoningId message)
        <> optionalField "openaiReasoningSummary" (messageOpenAIReasoningSummary message)
        <> optionalField "geminiThoughtSignature" (messageGeminiThoughtSignature message)
        <> optionalField "chatCompletionReasoningField" (messageChatCompletionReasoningField message)
        <> optionalField "chatCompletionReasoningContent" (messageChatCompletionReasoningContent message)
        <> optionalField "isUserVisible" (messageIsUserVisible message)
        <> optionalField "isError" (messageIsError message)
        <> optionalField "userMessageSource" (messageUserSource message)
        <> optionalField "interactionMode" (messageInteractionMode message)
        <> optionalField "modelId" (messageModelId message)
        <> optionalField "routerId" (messageRouterId message)
        <> optionalField "reasoningEffort" (messageReasoningEffort message)
        <> optionalField "apiProvider" (messageApiProvider message)
        <> optionalField "hookEventName" (messageHookEventName message)
        <> optionalField "hookMatcher" (messageHookMatcher message)
        <> optionalField "hookCommands" (messageHookCommands message)
        <> optionalField "hookStatus" (messageHookStatus message)
        <> optionalField "hookResults" (messageHookResults message)
        <> optionalField "hookToolCallId" (messageHookToolCallId message)
        <> optionalField "hookParentId" (messageHookParentId message)
        <> optionalField "hookOrder" (messageHookOrder message)
        <> optionalField "hookPreventedAction" (messageHookPreventedAction message)
        <> optionalField "hiddenFromUserViews" (messageHiddenFromUserViews message)
        <> optionalField "hookStartTime" (messageHookStartTime message)
        <> optionalField "hookEndTime" (messageHookEndTime message)
        <> optionalField "isParallelExecution" (messageIsParallelExecution message)
        <> optionalField "parallelGroupId" (messageParallelGroupId message)

messageKeys :: [Key]
messageKeys =
  [ "id",
    "role",
    "content",
    "createdAt",
    "updatedAt",
    "parentId",
    "visibility",
    "openaiMessageId",
    "openaiPhase",
    "openaiEncryptedContent",
    "openaiReasoningId",
    "openaiReasoningSummary",
    "geminiThoughtSignature",
    "chatCompletionReasoningField",
    "chatCompletionReasoningContent",
    "isUserVisible",
    "isError",
    "userMessageSource",
    "interactionMode",
    "modelId",
    "routerId",
    "reasoningEffort",
    "apiProvider",
    "hookEventName",
    "hookMatcher",
    "hookCommands",
    "hookStatus",
    "hookResults",
    "hookToolCallId",
    "hookParentId",
    "hookOrder",
    "hookPreventedAction",
    "hiddenFromUserViews",
    "hookStartTime",
    "hookEndTime",
    "isParallelExecution",
    "parallelGroupId"
  ]

commandKeys, resultKeys :: [Key]
commandKeys = ["command", "timeout"]
resultKeys = ["exitCode", "stdout", "stderr", "suppressOutput"]
