{-# LANGUAGE OverloadedStrings #-}

module NotificationsSpec (notificationTests) where

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
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific, scientific)
import Factory.Droid.Schema.Content (mkThinkingDuration)
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.Schema.Notifications
import Factory.Droid.Schema.Primitives
import Factory.Droid.Schema.Usage
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, chooseInt, forAll, testProperty, (===))

notificationTests :: Value -> TestTree
notificationTests schema =
  case (fromJSON messageJSON :: Result FactoryDroidMessage, mkNonNegativeNumber 2.5) of
    (Success message, Just removedCount) ->
      let create = CreateMessage message (Just "parent-id") (Just "request-id") mempty
          compacted = SessionCompacted "summary-id" removedCount Nothing mempty
          records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
          records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)
       in testGroup
            "Core notifications"
            [ records "AssistantTextDeltaNotificationSchema" assistantDelta assistantDelta assistantDeltaJSON assistantDeltaJSON (\extras event -> event {assistantDeltaAdditionalFields = extras}),
              records "AssistantTextCompleteNotificationSchema" assistantComplete assistantComplete assistantCompleteJSON assistantCompleteJSON (\extras event -> event {assistantCompleteAdditionalFields = extras}),
              records "ThinkingTextDeltaNotificationSchema" thinkingDelta thinkingDelta thinkingDeltaJSON thinkingDeltaJSON (\extras event -> event {thinkingDeltaAdditionalFields = extras}),
              records "ThinkingTextCompleteNotificationSchema" thinkingComplete (thinkingComplete {thinkingCompleteDuration = Nothing}) thinkingCompleteJSON (remove ["durationMs"] thinkingCompleteJSON) (\extras event -> event {thinkingCompleteAdditionalFields = extras}),
              records "CreateMessageNotificationSchema" create (create {createdParentId = Nothing, createdRequestId = Nothing}) createJSON (remove ["parentId", "requestId"] createJSON) (\extras event -> event {createdMessageAdditionalFields = extras}),
              records "AgentTurnCompletedNotificationSchema" turnCompleted minimalTurn turnJSON (remove ["turnId", "cumulativeTokenUsage", "childTokenUsage", "cumulativeChildTokenUsage", "durationMs"] turnJSON) (\extras event -> event {turnAdditionalFields = extras}),
              records "SessionTokenUsageChangedNotificationSchema" sessionUsage (sessionUsage {sessionUsageInclusive = Nothing, sessionUsageLastCall = Nothing}) sessionUsageJSON (remove ["inclusiveTokenUsage", "lastCallTokenUsage"] sessionUsageJSON) (\extras event -> event {sessionUsageAdditionalFields = extras}),
              records "DroidWorkingStateChangedNotificationSchema" (DroidWorkingStateChanged WorkingIdle mempty) (DroidWorkingStateChanged WorkingIdle mempty) workingJSON workingJSON (\extras event -> event {workingStateAdditionalFields = extras}),
              records "AssistantMessageRetractedNotificationSchema" (AssistantMessageRetracted "retracted-id" mempty) (AssistantMessageRetracted "retracted-id" mempty) retractedJSON retractedJSON (\extras event -> event {retractedAdditionalFields = extras}),
              records "SessionTitleUpdatedNotificationSchema" title (title {titleRequestId = Nothing, titleUpdateType = Nothing}) titleJSON (remove ["requestId", "updateType"] titleJSON) (\extras event -> event {titleAdditionalFields = extras}),
              records "SessionWorkingDirectoryChangedNotificationSchema" (SessionWorkingDirectoryChanged "/fixture/cwd" mempty) (SessionWorkingDirectoryChanged "/fixture/cwd" mempty) directoryJSON directoryJSON (\extras event -> event {workingDirectoryAdditionalFields = extras}),
              records "QueuedMessagesDiscardedNotificationSchema" discarded (discarded {discardedRequestId = Nothing}) discardedJSON (remove ["requestId"] discardedJSON) (\extras event -> event {discardedAdditionalFields = extras}),
              nonNullableRecordTests "NotificationError" (schemaAt ["definitions", "ErrorNotificationSchema", "properties", "error"] schema) nestedError nestedError nestedErrorJSON nestedErrorJSON (\extras err -> err {notificationErrorAdditionalFields = extras}),
              records "ErrorNotificationSchema" errorEvent (errorEvent {errorNotificationError = Nothing, errorNotificationExitCode = Nothing}) errorJSON (remove ["error", "exitCode"] errorJSON) (\extras event -> event {errorNotificationAdditionalFields = extras}),
              records "ChildSessionAvailableNotificationSchema" child (child {availableChildToolUseId = Nothing, availableChildSubagentType = Nothing, availableChildDescription = Nothing}) childJSON (remove ["toolUseId", "subagentType", "description"] childJSON) (\extras event -> event {availableChildAdditionalFields = extras}),
              enumTests schema ["AgentTurnCompletionReasonSchema", "enum"] (Proxy @AgentTurnCompletionReason),
              enumTests schema ["DroidWorkingStateSchema", "enum"] (Proxy @DroidWorkingState),
              enumTests schema ["SessionTitleUpdatedNotificationSchema", "properties", "updateType", "enum"] (Proxy @TitleUpdateType),
              enumTests schema ["ErrorNotificationSchema", "properties", "errorType", "enum"] (Proxy @NotificationErrorType),
              testCase "structured output is a required nullable object" $ do
                let content = KeyMap.singleton "nested" (toJSON [Null, Number 1, Bool True])
                forM_ [Nothing, Just mempty, Just content] $ \output -> do
                  let value = StructuredOutput "message-id" output mempty
                      encoded = object ["type" .= String "structured_output", "messageId" .= String "message-id", "structuredOutput" .= output]
                  fromJSON encoded @?= Success value
                  toJSON value @?= encoded
                  eitherDecode (encode value) @?= Right value
                rejects (Proxy @StructuredOutput) (Object (KeyMap.delete "structuredOutput" structuredJSON))
                forM_ [String "text", Number 0, Bool False, Array mempty] $ \value -> rejects (Proxy @StructuredOutput) (Object (KeyMap.insert "structuredOutput" value structuredJSON)),
              testCase "compaction preserves required nullable boundary and nonnegative count" $ do
                fromJSON (Object compactedJSON) @?= Success compacted
                toJSON compacted @?= Object compactedJSON
                eitherDecode (encode compacted) @?= Right compacted
                let withBoundary = compacted {compactedVisibleBoundaryId = Just "boundary"}
                fromJSON (Object (KeyMap.insert "visibleBoundaryMessageId" (String "boundary") compactedJSON)) @?= Success withBoundary
                rejects (Proxy @SessionCompacted) (Object (KeyMap.delete "visibleBoundaryMessageId" compactedJSON))
                rejects (Proxy @SessionCompacted) (Object (KeyMap.insert "removedCount" (Number (-1)) compactedJSON)),
              testCase "nullable notification fixtures cover every declared field" $ do
                assertKeys schema "StructuredOutputNotificationSchema" structuredJSON
                assertKeys schema "SessionCompactedNotificationSchema" compactedJSON,
              testCase "nullable notification extensions cannot override named fields" $ do
                let output = StructuredOutput "message-id" Nothing (KeyMap.singleton "future" (Bool True))
                    withExtra = compacted {compactedAdditionalFields = KeyMap.singleton "future" (Bool True)}
                eitherDecode (encode output) @?= Right output
                eitherDecode (encode withExtra) @?= Right withExtra
                forM_ (KeyMap.keys structuredJSON) $ \key -> toJSON (output {structuredAdditionalFields = KeyMap.singleton key (String "injected")}) @?= Object structuredJSON
                forM_ (KeyMap.keys compactedJSON) $ \key -> toJSON (compacted {compactedAdditionalFields = KeyMap.singleton key (String "injected")}) @?= Object compactedJSON,
              testCase "nullable payloads still enforce required fields and tags" $ do
                forM_ (KeyMap.keys structuredJSON) $ \key -> rejects (Proxy @StructuredOutput) (Object (KeyMap.delete key structuredJSON))
                forM_ (KeyMap.keys compactedJSON) $ \key -> rejects (Proxy @SessionCompacted) (Object (KeyMap.delete key compactedJSON))
                rejects (Proxy @StructuredOutput) (Object (KeyMap.insert "type" (String "future") structuredJSON))
                rejects (Proxy @SessionCompacted) (Object (KeyMap.insert "type" (String "future") compactedJSON))
                rejects (Proxy @SessionCompacted) (Object (KeyMap.insert "visibleBoundaryMessageId" (Number 0) compactedJSON)),
              testCase "non-object nullable notifications fail safely" $
                forM_ [Null, Bool True, Number 0, String "event", Array mempty] $ \value -> do
                  rejects (Proxy @StructuredOutput) value
                  rejects (Proxy @SessionCompacted) value,
              testCase "an idle event is not a turn-completed event" $ do
                rejects (Proxy @AgentTurnCompleted) (Object workingJSON)
                rejects (Proxy @DroidWorkingStateChanged) (Object turnJSON),
              testCase "nested message, usage and error records are validated" $ do
                rejects (Proxy @CreateMessage) (Object (KeyMap.insert "message" (object []) createJSON))
                forM_ ["tokenUsage", "cumulativeTokenUsage", "childTokenUsage", "cumulativeChildTokenUsage"] $ \key -> rejects (Proxy @AgentTurnCompleted) (Object (KeyMap.insert key (object []) turnJSON))
                rejects (Proxy @SessionTokenUsageChanged) (Object (KeyMap.insert "lastCallTokenUsage" (object []) sessionUsageJSON))
                rejects (Proxy @ErrorNotification) (Object (KeyMap.insert "error" (object []) errorJSON)),
              testCase "duration bounds and error exit-code integer semantics are enforced" $ do
                rejects (Proxy @ThinkingTextComplete) (Object (KeyMap.insert "durationMs" (Number (-0.1)) thinkingCompleteJSON))
                rejects (Proxy @AgentTurnCompleted) (Object (KeyMap.insert "durationMs" (Number (-0.1)) turnJSON))
                rejects (Proxy @ErrorNotification) (Object (KeyMap.insert "exitCode" (Number 1.5) errorJSON))
                fromJSON (Object errorJSON) @?= Success errorEvent,
              testCase "nonnegative numbers accept zero without truncation" $ do
                mkNonNegativeNumber (-1) @?= Nothing
                fmap nonNegativeNumberValue (mkNonNegativeNumber 0) @?= Just 0
                fmap nonNegativeNumberValue (mkNonNegativeNumber 0.125) @?= Just 0.125,
              testProperty "nonnegative numeric values round-trip exactly" $
                forAll arbitrary $ \coefficient ->
                  forAll (chooseInt (-12, 12)) $ \decimalExponent ->
                    let number = scientific (abs coefficient) decimalExponent
                     in case mkNonNegativeNumber number of
                          Nothing -> Nothing === Just number
                          Just value -> eitherDecode (encode value) === Right (Number number)
            ]
    _ -> testCase "valid notification fixture prerequisites" (assertFailure "Could not construct fixture message or count")

enumTests :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => Value -> [Key] -> Proxy a -> TestTree
enumTests schema path _ = testCase (show path) $ do
  literals <- either assertFailure pure (schemaAt ("definitions" : path) schema)
  let values = [minBound .. maxBound] :: [a]
  toJSON values @?= literals
  forM_ values $ \value -> do
    fromJSON (toJSON value) @?= Success value
    eitherDecode (encode value) @?= Right (toJSON value)
  forM_ [Null, Number 1, Bool True, String "future", Object mempty, Array mempty] $ rejects (Proxy @a)

assertKeys :: Value -> Key -> Object -> IO ()
assertKeys schema name fields = do
  props <- either assertFailure pure (schemaAt ["definitions", name, "properties"] schema)
  case props of
    Object keys -> sort (KeyMap.keys keys) @?= sort (KeyMap.keys fields)
    _ -> assertFailure "Expected schema properties"

assistantDelta :: AssistantTextDelta
assistantDelta = AssistantTextDelta "assistant-message" (-0.5) "delta" mempty

assistantComplete :: AssistantTextComplete
assistantComplete = AssistantTextComplete "assistant-message" 2.5 mempty

thinkingDelta :: ThinkingTextDelta
thinkingDelta = ThinkingTextDelta "thinking-message" 3.5 "thought delta" mempty

thinkingComplete :: ThinkingTextComplete
thinkingComplete = ThinkingTextComplete "thinking-message" 4.5 (mkThinkingDuration 0.125) mempty

turnCompleted, minimalTurn :: AgentTurnCompleted
turnCompleted = AgentTurnCompleted TurnCompleted (usage 1) (Just "turn-id") (Just (usage 11)) (Just (usage 21)) (Just (usage 31)) (mkNonNegativeNumber 10.125) mempty
minimalTurn = AgentTurnCompleted TurnCompleted (usage 1) Nothing Nothing Nothing Nothing Nothing mempty

sessionUsage :: SessionTokenUsageChanged
sessionUsage = SessionTokenUsageChanged "session-id" (usage 41) (Just (usage 51)) (Just (LastCallTokenUsage 61 62 (Just 63) mempty)) mempty

title :: SessionTitleUpdated
title = SessionTitleUpdated "title" (Just "title-request") (Just TitleManualRename) mempty

discarded :: QueuedMessagesDiscarded
discarded = QueuedMessagesDiscarded "discarded text" (Just "discarded-request") mempty

nestedError :: NotificationError
nestedError = NotificationError "ErrorName" "nested error" mempty

errorEvent :: ErrorNotification
errorEvent = ErrorNotification "outer error" NotifyProcessExitError "opaque timestamp" (Just nestedError) (Just 18446744073709551617) mempty

child :: ChildSessionAvailable
child = ChildSessionAvailable "child-id" 9.125 (Just "tool-id") (Just "subagent-type") (Just "description") mempty

usage :: Scientific -> TokenUsage
usage n = TokenUsage n (n + 1) (n + 2) (n + 3) (n + 4) (Just (n + 0.125)) mempty

usageJSON :: Scientific -> Value
usageJSON n = object ["inputTokens" .= Number n, "outputTokens" .= Number (n + 1), "cacheCreationTokens" .= Number (n + 2), "cacheReadTokens" .= Number (n + 3), "thinkingTokens" .= Number (n + 4), "factoryCredits" .= Number (n + 0.125)]

messageJSON :: Value
messageJSON = object ["id" .= String "message-id", "role" .= String "assistant", "content" .= [object ["type" .= String "text", "text" .= String "hello"]], "createdAt" .= Number 1.125, "updatedAt" .= Number 2.25]

assistantDeltaJSON, assistantCompleteJSON, thinkingDeltaJSON, thinkingCompleteJSON, createJSON, turnJSON, sessionUsageJSON, workingJSON, retractedJSON, titleJSON, directoryJSON, discardedJSON, structuredJSON, compactedJSON, nestedErrorJSON, errorJSON, childJSON :: Object
assistantDeltaJSON = KeyMap.fromList ["type" .= String "assistant_text_delta", "messageId" .= String "assistant-message", "blockIndex" .= Number (-0.5), "textDelta" .= String "delta"]
assistantCompleteJSON = KeyMap.fromList ["type" .= String "assistant_text_complete", "messageId" .= String "assistant-message", "blockIndex" .= Number 2.5]
thinkingDeltaJSON = KeyMap.fromList ["type" .= String "thinking_text_delta", "messageId" .= String "thinking-message", "blockIndex" .= Number 3.5, "textDelta" .= String "thought delta"]
thinkingCompleteJSON = KeyMap.fromList ["type" .= String "thinking_text_complete", "messageId" .= String "thinking-message", "blockIndex" .= Number 4.5, "durationMs" .= Number 0.125]
createJSON = KeyMap.fromList ["type" .= String "create_message", "message" .= messageJSON, "parentId" .= String "parent-id", "requestId" .= String "request-id"]
turnJSON = KeyMap.fromList ["type" .= String "agent_turn_completed", "reason" .= String "completed", "tokenUsage" .= usageJSON 1, "turnId" .= String "turn-id", "cumulativeTokenUsage" .= usageJSON 11, "childTokenUsage" .= usageJSON 21, "cumulativeChildTokenUsage" .= usageJSON 31, "durationMs" .= Number 10.125]
sessionUsageJSON = KeyMap.fromList ["type" .= String "session_token_usage_changed", "sessionId" .= String "session-id", "tokenUsage" .= usageJSON 41, "inclusiveTokenUsage" .= usageJSON 51, "lastCallTokenUsage" .= object ["inputTokens" .= Number 61, "cacheReadTokens" .= Number 62, "outputTokens" .= Number 63]]
workingJSON = KeyMap.fromList ["type" .= String "droid_working_state_changed", "newState" .= String "idle"]
retractedJSON = KeyMap.fromList ["type" .= String "assistant_message_retracted", "messageId" .= String "retracted-id"]
titleJSON = KeyMap.fromList ["type" .= String "session_title_updated", "title" .= String "title", "requestId" .= String "title-request", "updateType" .= String "manual_rename"]
directoryJSON = KeyMap.fromList ["type" .= String "session_working_directory_changed", "cwd" .= String "/fixture/cwd"]
discardedJSON = KeyMap.fromList ["type" .= String "queued_messages_discarded", "text" .= String "discarded text", "requestId" .= String "discarded-request"]
structuredJSON = KeyMap.fromList ["type" .= String "structured_output", "messageId" .= String "message-id", "structuredOutput" .= Null]
compactedJSON = KeyMap.fromList ["type" .= String "session_compacted", "summaryId" .= String "summary-id", "removedCount" .= Number 2.5, "visibleBoundaryMessageId" .= Null]
nestedErrorJSON = KeyMap.fromList ["name" .= String "ErrorName", "message" .= String "nested error"]
errorJSON = KeyMap.fromList ["type" .= String "error", "message" .= String "outer error", "errorType" .= String "ProcessExitError", "timestamp" .= String "opaque timestamp", "error" .= nestedErrorJSON, "exitCode" .= Number 18446744073709551617]
childJSON = KeyMap.fromList ["type" .= String "child_session_available", "childSessionId" .= String "child-id", "timestamp" .= Number 9.125, "toolUseId" .= String "tool-id", "subagentType" .= String "subagent-type", "description" .= String "description"]

remove :: [Key] -> Object -> Object
remove keys fields = foldr KeyMap.delete fields keys
