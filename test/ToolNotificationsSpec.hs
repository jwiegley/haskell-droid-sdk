{-# LANGUAGE OverloadedStrings #-}

module ToolNotificationsSpec (toolNotificationTests) where

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
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Content
import Factory.Droid.Schema.Messages (PersistedHookCommand (..), PersistedHookResult (..), persistedHookResultObject)
import Factory.Droid.Schema.Notifications
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

toolNotificationTests :: Value -> TestTree
toolNotificationTests schema =
  testGroup
    "Tool notifications"
    [ records "ToolResultNotificationSchema" resultEvent minimalResultEvent resultJSON minimalResultJSON (\extras event -> event {resultNotificationBlock = (resultNotificationBlock event) {toolResultBase = (toolResultBase (resultNotificationBlock event)) {blockAdditionalFields = extras}}}),
      records "ToolCallNotificationSchema" callEvent callEvent callJSON callJSON (\extras event -> event {toolCallAdditionalFields = extras}),
      records "ToolExecutionHeartbeatNotificationSchema" heartbeat heartbeat heartbeatJSON heartbeatJSON (\extras event -> event {heartbeatAdditionalFields = extras}),
      records "ToolExecutionPhaseChangedNotificationSchema" phase phase phaseJSON phaseJSON (\extras event -> event {phaseAdditionalFields = extras}),
      records "ToolProgressUpdateSchema" fullProgress minimalProgress progressJSON (KeyMap.singleton "type" (String "status")) (\extras value -> value {progressAdditionalFields = extras}),
      records "ToolProgressUpdateNotificationSchema" progressEvent progressEvent progressEventJSON progressEventJSON (\extras event -> event {progressNotificationAdditionalFields = extras}),
      records "LlmRetryNotificationSchema" retry retry retryJSON retryJSON (\extras event -> event {retryAdditionalFields = extras}),
      records "PermissionResolvedNotificationSchema" permission permission permissionJSON permissionJSON (\extras event -> event {resolvedAdditionalFields = extras}),
      enumTests schema ["ToolConfirmationOutcomeSchema", "enum"] (Proxy @ToolConfirmationOutcome),
      enumTests schema ["ToolExecutionPhaseChangedNotificationSchema", "properties", "phase", "enum"] (Proxy @ToolExecutionPhase),
      enumTests schema ["ToolProgressUpdateSchema", "properties", "type", "enum"] (Proxy @ToolProgressKind),
      enumTests schema ["LlmRetryNotificationSchema", "properties", "reason", "enum"] (Proxy @LlmRetryReason),
      testCase "result notifications use the same content object encoding" $ do
        let block = resultNotificationBlock resultEvent
        contentBlockObject (ContentToolResult block) @?= resultBlockJSON
        toJSON (ContentToolResult block) @?= Object resultBlockJSON
        toJSON block @?= Object resultBlockJSON
        toJSON resultEvent @?= Object (KeyMap.insert "messageId" (String "message-id") resultBlockJSON),
      testCase "required notification identity overrides a conflicting block extension" $ do
        let block = resultNotificationBlock resultEvent
            injected = block {toolResultBase = (toolResultBase block) {blockAdditionalFields = KeyMap.singleton "messageId" (String "injected")}}
        toJSON (ToolResultNotification "message-id" injected) @?= Object resultJSON
        rejects (Proxy @ToolResultNotification) (Object resultBlockJSON)
        rejects (Proxy @ToolResultNotification) (Object (KeyMap.insert "messageId" Null resultJSON)),
      testCase "tool-result content retains its restricted array and omission semantics" $ do
        forM_ [Nothing, Just (ResultText ""), Just (ResultText "text"), Just (ResultBlocks [])] $ \content -> do
          let value = minimalResultEvent {resultNotificationBlock = (resultNotificationBlock minimalResultEvent) {toolResultContent = content}}
          fromJSON (toJSON value) @?= Success value
        rejects (Proxy @ToolResultNotification) (Object (KeyMap.insert "content" Null resultJSON))
        rejects (Proxy @ToolResultNotification) (Object (KeyMap.insert "content" (toJSON [Object useJSON]) resultJSON)),
      testCase "tool calls and progress updates validate nested records" $ do
        rejects (Proxy @ToolCallNotification) (object ["type" .= String "tool_call", "toolUse" .= object []])
        rejects (Proxy @ToolCallNotification) (object ["type" .= String "tool_call", "toolUse" .= Null])
        rejects (Proxy @ToolProgressUpdateNotification) (Object (KeyMap.insert "update" (object []) progressEventJSON))
        rejects (Proxy @ToolProgressUpdateNotification) (Object (KeyMap.insert "update" (object ["type" .= String "future"]) progressEventJSON)),
      testCase "progress status stays free-form while kind remains constrained" $ do
        let value = minimalProgress {progressStatus = Just "provider-specific status"}
        fromJSON (object ["type" .= String "status", "status" .= String "provider-specific status"]) @?= Success value
        rejects (Proxy @ToolProgressUpdate) (object ["type" .= String "provider-specific kind"]),
      testCase "progress parameters are arbitrary JSON objects, not arbitrary JSON roots" $ do
        let params = KeyMap.singleton "nested" (object ["values" .= [Null, Bool True, Number 1.25]])
            value = minimalProgress {progressParameters = Just params}
        fromJSON (object ["type" .= String "status", "parameters" .= params]) @?= Success value
        toJSON value @?= object ["type" .= String "status", "parameters" .= params]
        forM_ [Null, Bool True, Number 1, String "params", Array mempty] $ \invalid ->
          rejects (Proxy @ToolProgressUpdate) (object ["type" .= String "status", "parameters" .= invalid]),
      testCase "retry unknown is explicit and attempts retain the number domain" $ do
        let value = LlmRetry (-0.5) RetryUnknown mempty
        fromJSON (object ["type" .= String "llm_retry", "attempt" .= Number (-0.5), "reason" .= String "unknown"]) @?= Success value
        rejects (Proxy @LlmRetry) (Object (KeyMap.insert "reason" (String "future") retryJSON)),
      testCase "permission resolutions preserve all selections and batched IDs" $ do
        forM_ [minBound .. maxBound] $ \selection ->
          forM_ [[], ["one"], ["two", "one", "two"]] $ \identifiers -> do
            let value = PermissionResolved "request-id" identifiers selection mempty
            fromJSON (toJSON value) @?= Success value
            toJSON value @?= object ["type" .= String "permission_resolved", "requestId" .= String "request-id", "toolUseIds" .= identifiers, "selectedOption" .= selection]
        forM_ [Null, Bool True, Number 1, object []] $ \identifier ->
          rejects (Proxy @PermissionResolved) (Object (KeyMap.insert "toolUseIds" (toJSON [identifier]) permissionJSON))
        rejects (Proxy @PermissionResolved) (Object (KeyMap.insert "selectedOption" (String "future") permissionJSON)),
      testCase "tool phases do not collapse settled states" $ do
        forM_ [ExecutionSettledAfterExecution, ExecutionSettledWithoutExecution, ExecutionSettledUnknown] $ \value -> do
          let event = phase {changedToolPhase = value}
          fromJSON (toJSON event) @?= Success event
        rejects (Proxy @ToolExecutionPhaseChanged) (Object (KeyMap.insert "phase" (String "future") phaseJSON)),
      records "HookCommandSchema" hookCommand (hookCommand {hookCommandTimeout = Nothing}) commandJSON (KeyMap.delete "timeout" commandJSON) (\extras command -> command {hookCommandAdditionalFields = extras}),
      records "HookResultSchema" hookResult minimalHookResult hookResultJSON minimalHookResultJSON (\extras result -> result {hookResultOutput = (hookResultOutput result) {hookResultAdditionalFields = extras}}),
      records "HookExecutionStartedNotificationSchema" hookStarted minimalHookStarted hookStartedJSON minimalHookStartedJSON (\extras event -> event {startedHookAdditionalFields = extras}),
      records "HookExecutionCompletedNotificationSchema" hookCompleted minimalHookCompleted hookCompletedJSON minimalHookCompletedJSON (\extras event -> event {completedHookAdditionalFields = extras}),
      enumTests schema ["DroidHookEventSchema", "enum"] (Proxy @DroidHookEvent),
      enumTests schema ["HookExecutionCompletedNotificationSchema", "properties", "hookStatus", "enum"] (Proxy @HookCompletionStatus),
      testCase "hook result context extends the existing output codec" $ do
        let output = hookResultOutput hookResult
        toJSON output @?= Object (persistedHookResultObject output)
        toJSON hookResult @?= Object hookResultJSON
        let injected = output {hookResultAdditionalFields = KeyMap.fromList ["command" .= String "injected", "timeout" .= Number 999]}
        toJSON (hookResult {hookResultOutput = injected}) @?= Object hookResultJSON
        toJSON (HookResult injected Nothing Nothing) @?= Object (persistedHookResultObject output),
      testCase "started hook names are free-form but completed hook names are constrained" $ do
        fromJSON (Object hookStartedJSON) @?= Success hookStarted
        rejects (Proxy @HookExecutionCompleted) (Object (KeyMap.insert "hookEventName" (String "custom hook") hookCompletedJSON))
        rejects (Proxy @HookExecutionCompleted) (Object (KeyMap.insert "hookStatus" (String "executing") hookCompletedJSON)),
      testCase "hook arrays preserve empty and omitted values separately" $ do
        let started = minimalHookStarted {startedHookCommands = []}
            completed = minimalHookCompleted {completedHookResults = Just []}
        toJSON started @?= Object (KeyMap.insert "hookCommands" (toJSON ([] :: [Value])) minimalHookStartedJSON)
        toJSON completed @?= Object (KeyMap.insert "hookResults" (toJSON ([] :: [Value])) minimalHookCompletedJSON)
        fromJSON (toJSON started) @?= Success started
        fromJSON (toJSON completed) @?= Success completed
        rejects (Proxy @HookExecutionStarted) (Object (KeyMap.insert "hookCommands" (toJSON [object []]) hookStartedJSON))
        rejects (Proxy @HookExecutionCompleted) (Object (KeyMap.insert "hookResults" (toJSON [object []]) hookCompletedJSON))
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = nonNullableRecordTests name (schemaAt ["definitions", name] schema)

enumTests :: forall a. (Bounded a, Enum a, Eq a, Show a, FromJSON a, ToJSON a) => Value -> [Key] -> Proxy a -> TestTree
enumTests schema path _ = testCase (show path) $ do
  literals <- either assertFailure pure (schemaAt ("definitions" : path) schema)
  let values = [minBound .. maxBound] :: [a]
  toJSON values @?= literals
  forM_ values $ \value -> do
    fromJSON (toJSON value) @?= Success value
    eitherDecode (encode value) @?= Right (toJSON value)
  forM_ [Null, Bool True, Number 0, String "future", Object mempty, Array mempty] $ rejects (Proxy @a)

resultEvent, minimalResultEvent :: ToolResultNotification
resultEvent = ToolResultNotification "message-id" (ToolResultBlock "tool-use-id" (Just (ResultText "result text")) (Just False) (BaseContentBlock (Just "block-id") mempty))
minimalResultEvent = ToolResultNotification "message-id" (ToolResultBlock "tool-use-id" Nothing Nothing (BaseContentBlock Nothing mempty))

callEvent :: ToolCallNotification
callEvent = ToolCallNotification (ToolUseBlock "called-id" (KeyMap.singleton "argument" (Number 1)) "called-tool" Nothing Nothing Nothing mempty) mempty

heartbeat :: ToolExecutionHeartbeat
heartbeat = ToolExecutionHeartbeat "heartbeat-id" "heartbeat-tool" mempty

phase :: ToolExecutionPhaseChanged
phase = ToolExecutionPhaseChanged "phase-id" "phase-tool" ExecutionSettledWithoutExecution mempty

fullProgress, minimalProgress :: ToolProgressUpdate
fullProgress = ToolProgressUpdate ProgressStatus (Just "progress-tool") (Just "custom-status") (Just "details") (Just "text") (Just "error text") (Just 12345678901234567890.125) (Just (KeyMap.singleton "arbitrary" Null)) (Just "snippet") (Just "terminal-id") (Just "full output") (Just "subagent-id") mempty
minimalProgress = ToolProgressUpdate ProgressStatus Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

progressEvent :: ToolProgressUpdateNotification
progressEvent = ToolProgressUpdateNotification "progress-id" "progress-notification-tool" fullProgress mempty

retry :: LlmRetry
retry = LlmRetry 2.5 RetryRateLimited mempty

permission :: PermissionResolved
permission = PermissionResolved "permission-id" ["first", "second", "first"] ConfirmCancel mempty

hookCommand :: HookCommand
hookCommand = PersistedHookCommand "echo fixture" (Just 1.25) mempty

hookResult, minimalHookResult :: HookResult
hookResult = HookResult (PersistedHookResult (-0.5) "stdout" "stderr" (Just False) mempty) (Just "echo result") (Just 2.75)
minimalHookResult = HookResult (PersistedHookResult (-0.5) "stdout" "stderr" Nothing mempty) Nothing Nothing

hookStarted, minimalHookStarted :: HookExecutionStarted
hookStarted = HookExecutionStarted "started-hook" "custom hook" [hookCommand] (Just "start matcher") (Just "start call") (Just True) (Just False) (Just "parallel group") (Just "start parent") (Just 3.5) mempty
minimalHookStarted = HookExecutionStarted "started-hook" "custom hook" [hookCommand] Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

hookCompleted, minimalHookCompleted :: HookExecutionCompleted
hookCompleted = HookExecutionCompleted "completed-hook" HookFailed (Just HookPreToolUse) (Just "complete matcher") (Just [hookResult]) (Just "complete call") (Just False) (Just "complete parent") (Just 4.5) (Just True) mempty
minimalHookCompleted = HookExecutionCompleted "completed-hook" HookFailed Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

commandJSON, hookResultJSON, minimalHookResultJSON, hookStartedJSON, minimalHookStartedJSON, hookCompletedJSON, minimalHookCompletedJSON :: Object
commandJSON = KeyMap.fromList ["command" .= String "echo fixture", "timeout" .= Number 1.25]
minimalHookResultJSON = KeyMap.fromList ["exitCode" .= Number (-0.5), "stdout" .= String "stdout", "stderr" .= String "stderr"]
hookResultJSON = KeyMap.union minimalHookResultJSON (KeyMap.fromList ["suppressOutput" .= False, "command" .= String "echo result", "timeout" .= Number 2.75])
minimalHookStartedJSON = KeyMap.fromList ["type" .= String "hook_execution_started", "hookId" .= String "started-hook", "hookEventName" .= String "custom hook", "hookCommands" .= [Object commandJSON]]
hookStartedJSON = KeyMap.union minimalHookStartedJSON (KeyMap.fromList ["hookMatcher" .= String "start matcher", "hookToolCallId" .= String "start call", "hiddenFromUserViews" .= True, "isParallelExecution" .= False, "parallelGroupId" .= String "parallel group", "hookParentId" .= String "start parent", "hookOrder" .= Number 3.5])
minimalHookCompletedJSON = KeyMap.fromList ["type" .= String "hook_execution_completed", "hookId" .= String "completed-hook", "hookStatus" .= String "error"]
hookCompletedJSON = KeyMap.union minimalHookCompletedJSON (KeyMap.fromList ["hookEventName" .= String "PreToolUse", "hookMatcher" .= String "complete matcher", "hookResults" .= [Object hookResultJSON], "hookToolCallId" .= String "complete call", "hiddenFromUserViews" .= False, "hookParentId" .= String "complete parent", "hookOrder" .= Number 4.5, "hookPreventedAction" .= True])

resultBlockJSON, resultJSON, minimalResultJSON, useJSON, callJSON, heartbeatJSON, phaseJSON, progressJSON, progressEventJSON, retryJSON, permissionJSON :: Object
resultBlockJSON = KeyMap.fromList ["type" .= String "tool_result", "toolUseId" .= String "tool-use-id", "id" .= String "block-id", "content" .= String "result text", "isError" .= False]
resultJSON = KeyMap.insert "messageId" (String "message-id") resultBlockJSON
minimalResultJSON = KeyMap.fromList ["type" .= String "tool_result", "toolUseId" .= String "tool-use-id", "messageId" .= String "message-id"]
useJSON = KeyMap.fromList ["type" .= String "tool_use", "id" .= String "called-id", "name" .= String "called-tool", "input" .= object ["argument" .= Number 1]]
callJSON = KeyMap.fromList ["type" .= String "tool_call", "toolUse" .= useJSON]
heartbeatJSON = KeyMap.fromList ["type" .= String "tool_execution_heartbeat", "toolUseId" .= String "heartbeat-id", "toolName" .= String "heartbeat-tool"]
phaseJSON = KeyMap.fromList ["type" .= String "tool_execution_phase_changed", "toolUseId" .= String "phase-id", "toolName" .= String "phase-tool", "phase" .= String "settled_without_execution"]
progressJSON = KeyMap.fromList ["type" .= String "status", "toolName" .= String "progress-tool", "status" .= String "custom-status", "details" .= String "details", "text" .= String "text", "error" .= String "error text", "timestamp" .= Number 12345678901234567890.125, "parameters" .= object ["arbitrary" .= Null], "valueSnippet" .= String "snippet", "terminalId" .= String "terminal-id", "fullOutput" .= String "full output", "subagentSessionId" .= String "subagent-id"]
progressEventJSON = KeyMap.fromList ["type" .= String "tool_progress_update", "toolUseId" .= String "progress-id", "toolName" .= String "progress-notification-tool", "update" .= progressJSON]
retryJSON = KeyMap.fromList ["type" .= String "llm_retry", "attempt" .= Number 2.5, "reason" .= String "rate_limited"]
permissionJSON = KeyMap.fromList ["type" .= String "permission_resolved", "requestId" .= String "permission-id", "toolUseIds" .= [String "first", String "second", String "first"], "selectedOption" .= String "cancel"]
