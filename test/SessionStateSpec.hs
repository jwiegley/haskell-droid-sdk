{-# LANGUAGE OverloadedStrings #-}

module SessionStateSpec (sessionStateTests) where

import Control.Concurrent (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, catch)
import Control.Monad (forM_, forever, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid (DroidEvent (..))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Content (BaseContentBlock (..), ContentBlock (..), TextBlock (..), ThinkingBlock (..), ToolResultBlock (..), ToolUseBlock (..), mkThinkingDuration)
import Factory.Droid.Schema.Control (AddUserMessageParams (..), defaultUserMessageParams)
import Factory.Droid.Schema.Enums (MessageRole (..), MessageVisibility (..))
import Factory.Droid.Schema.Messages (FactoryDroidMessage, HookStatus (..), Message (..))
import Factory.Droid.Schema.Notifications (DroidWorkingState (..), HookCompletionStatus (..), HookExecutionCompleted (..), HookExecutionStarted (..), ToolExecutionPhase (..), ToolProgressUpdate (..))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

sessionStateTests :: TestTree
sessionStateTests =
  testGroup
    "Session message state"
    [ testCase "an authoritative load replaces locally newer timestamps and content" $ do
        provisional <- fixture "message" Nothing 200000 "local prefix"
        authoritative <- fixture "message" Nothing 1 "server correction"
        let initial = State.upsertSessionMessage provisional State.emptySessionState
            loaded = State.mergeLoadedMessages [authoritative] initial
        State.sessionMessages loaded @?= [authoritative],
      testCase "updates preserve parent identity and insert after contiguous descendants" $ do
        root <- fixture "root" Nothing 0 "root"
        child <- fixture "child" (Just "root") 1 "child"
        descendant <- fixture "descendant" (Just "child") 2 "descendant"
        other <- fixture "other" Nothing 3 "other root"
        sibling <- fixture "sibling" (Just "root") 0.5 "sibling"
        replacement <- fixture "child" Nothing 20 "replacement"
        let loaded = State.mergeLoadedMessages [root, child, descendant, other] State.emptySessionState
            updated = State.upsertSessionMessage replacement (State.upsertSessionMessage sibling loaded)
        ids updated @?= ["root", "child", "descendant", "sibling", "other"]
        Map.lookup "child" (State.sessionMessagesById updated) @?= Just (replacement {messageParentId = Just "root"}),
      testCase "new unparented messages inherit the current tail without changing their payload" $ do
        root <- fixture "root" Nothing 10 "root"
        next <- fixture "next" Nothing 0 "arrived later"
        let state = State.upsertSessionMessage next (State.upsertSessionMessage root State.emptySessionState)
        State.sessionMessages state @?= [root, next {messageParentId = Just "root"}],
      testCase "standalone parent repair preserves payloads, extensions and unresolved parents" $ do
        first <- fixture "duplicate" (Just "not-loaded") 9007199254740993 "first"
        later <- fixture "duplicate" (Just "different") 0 "later body"
        self <- fixture "self" (Just "self") 1 "self"
        let extended = later {messageAdditionalFields = KeyMap.fromList [("empty", String ""), ("flag", Bool False)]}
            original = [first, extended, self]
            expected = [first, extended {messageParentId = Just "not-loaded"}, self {messageParentId = Nothing}]
        State.repairMessageParents original @?= expected
        State.repairMessageParents expected @?= expected
        State.repairMessageParents [first {messageParentId = Nothing}, extended] @?= [first {messageParentId = Nothing}, extended {messageParentId = Nothing}]
        State.repairMessageParents [] @?= [],
      testCase "standalone ordering preserves unlinked ties and uses UTF-16 leaf ties" $ do
        root <- fixture "root" Nothing 100 "root"
        private <- fixture "\xe000" (Just "root") 1 "private"
        astral <- fixture "\x1f600" (Just "root") 1 "astral"
        State.orderMessagesByParentChain [] @?= []
        State.orderMessagesByParentChain [private, astral] @?= [private, astral]
        State.orderMessagesByParentChain [private, root, astral] @?= [root, astral, private],
      testCase "parent repair retains the first duplicate parent and breaks cycles and self-links" $ do
        first <- fixture "duplicate" (Just "first-parent") 0 "first"
        duplicate <- fixture "duplicate" (Just "wrong-parent") 1 "last body"
        a <- fixture "a" (Just "b") 2 "a"
        b <- fixture "b" (Just "a") 3 "b"
        self <- fixture "self" (Just "self") 4 "self"
        map messageParentId (State.repairMessageParents [first, duplicate, a, b, self]) @?= [Just "first-parent", Just "first-parent", Just "b", Nothing, Nothing]
        let merged = State.mergeLoadedMessages [first, duplicate] State.emptySessionState
        State.sessionMessages merged @?= [duplicate {messageParentId = Just "first-parent"}],
      testCase "a single rooted history follows the newest leaf before disconnected branches" $ do
        root <- fixture "root" Nothing 100 "root"
        a <- fixture "a" (Just "root") 5 "a"
        b <- fixture "b" (Just "a") 10 "b"
        side <- fixture "side" (Just "root") 9 "side"
        ids (State.mergeLoadedMessages [side, b, root, a] State.emptySessionState) @?= ["root", "a", "b", "side"],
      testCase "multiple roots use stable timestamp order and retain missing parents" $ do
        a <- fixture "a" (Just "not-loaded") 10 "a"
        b <- fixture "b" Nothing 5 "b"
        c <- fixture "c" Nothing 5 "c"
        let state = State.mergeLoadedMessages [a, b, c] State.emptySessionState
        State.sessionMessages state @?= [b, c, a]
        State.sessionRecentMessages 2 state @?= [c, a]
        State.sessionRecentMessages 0 state @?= []
        State.sessionRecentMessages (-1) state @?= []
        State.sessionRecentMessages maxBound state @?= [b, c, a],
      testCase "role selection, removal and bounded truncation do not remove pending submissions" $ do
        user <- fixture "user" Nothing 0 "user"
        assistant <- (\message -> message {messageRole = RoleAssistant}) <$> fixture "assistant" (Just "user") 1 "assistant"
        let state = State.mergeLoadedMessages [user, assistant] State.emptySessionState
        prepared <- either (assertFailure . show) pure (State.registerSubmissionAt 0 Nothing "request" "placeholder" (defaultUserMessageParams "pending") state)
        State.sessionMessagesByRole RoleAssistant prepared @?= [assistant]
        State.sessionMessages (State.removeSessionMessage "user" prepared) @?= [assistant]
        State.sessionMessages (State.truncateSessionMessages 1 prepared) @?= [assistant]
        State.sessionMessages (State.truncateSessionMessages (2 ^ (200 :: Int)) prepared) @?= [user, assistant]
        let cleared = State.truncateSessionMessages 0 prepared
        State.sessionMessages cleared @?= []
        Map.null (State.sessionMessagesById cleared) @?= True
        map State.submissionRequestId (State.optimisticSubmissions cleared) @?= ["request"],
      testCase "cyclic parent traversal terminates without dropping messages" $ do
        a <- fixture "a" (Just "b") 1 "a"
        b <- fixture "b" (Just "a") 2 "b"
        map messageId (State.orderMessagesByParentChain [a, b]) @?= ["a", "b"],
      testCase "timestamp-tied insertion uses deterministic UTF-16 ordering for opaque IDs" $ do
        root <- fixture "root" Nothing 2 "root"
        private <- fixture "\xe000" (Just "not-loaded") 1 "private use"
        astral <- fixture "\x1f600" (Just "not-loaded") 1 "supplementary"
        let state = State.upsertSessionMessage astral (State.upsertSessionMessage private (State.upsertSessionMessage root State.emptySessionState))
        ids state @?= ["\x1f600", "\xe000", "root"],
      testCase "text deltas retain independent block identities and completion flags" $ do
        first <- eventAt 10 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 0 "a") State.emptySessionState
        second <- eventAt 11 1 TextDeltaEvent (delta "assistant_text_delta" "stream" 1 "b") first
        third <- eventAt 12 2 TextDeltaEvent (delta "assistant_text_delta" "stream" 0 "c") second
        finished <- eventAt 13 3 TextCompleteEvent (complete "assistant_text_complete" "stream" 0) third
        map textBlockText (textBlocks finished) @?= ["ac", "b"]
        map (KeyMap.lookup "isStreaming" . blockAdditionalFields . textBlockBase) (textBlocks finished) @?= [Just (Bool False), Just (Bool True)]
        map messageCreatedAt (State.sessionMessages finished) @?= [10]
        map messageUpdatedAt (State.sessionMessages finished) @?= [13],
      testCase "thinking uses separate indices and monotonic duration while preserving explicit zero" $ do
        first <- eventAt 1000 100 ThinkingDeltaEvent (delta "thinking_text_delta" "stream" 0 "T") State.emptySessionState
        second <- eventAt 2000 200 TextDeltaEvent (delta "assistant_text_delta" "stream" 0 "X") first
        third <- eventAt 500 1100 ThinkingDeltaEvent (delta "thinking_text_delta" "stream" 0 "h") second
        timed <- eventAt (-100) 1600 ThinkingCompleteEvent (complete "thinking_text_complete" "stream" 0) third
        map thinkingBlockThinking (thinkingBlocks timed) @?= ["Th"]
        map textBlockText (textBlocks timed) @?= ["X"]
        map thinkingBlockDuration (thinkingBlocks timed) @?= [mkThinkingDuration 1.5]
        map (KeyMap.lookup "startedAtMs" . blockAdditionalFields . thinkingBlockBase) (thinkingBlocks timed) @?= [Just (Number 1000)]
        exact <- eventAt (-200) 2000 ThinkingCompleteEvent (object ["type" .= String "thinking_text_complete", "messageId" .= String "stream", "blockIndex" .= (0 :: Int), "durationMs" .= (0 :: Int)]) timed
        map thinkingBlockDuration (thinkingBlocks exact) @?= [mkThinkingDuration 0]
        let preserveFalse (ContentThinking block) = ContentThinking (block {thinkingBlockBase = (thinkingBlockBase block) {blockAdditionalFields = KeyMap.insert "supportsThinkingDuration" (Bool False) (blockAdditionalFields (thinkingBlockBase block))}})
            preserveFalse block = block
            flagged = foldl' (flip State.upsertSessionMessage) exact [message {messageContent = map preserveFalse (messageContent message)} | message <- State.sessionMessages exact]
        continued <- eventAt (-300) 3000 ThinkingDeltaEvent (delta "thinking_text_delta" "stream" 0 "more") flagged
        map (KeyMap.lookup "supportsThinkingDuration" . blockAdditionalFields . thinkingBlockBase) (thinkingBlocks continued) @?= [Just (Bool False)],
      testCase "an authoritative message resets provisional block addresses and content" $ do
        initial <- eventAt 10 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 5 "provisional") State.emptySessionState
        base <- fixture "stream" Nothing 1 "unused"
        let authoritative = base {messageRole = RoleAssistant, messageContent = map (\text -> ContentText (TextBlock text (BaseContentBlock Nothing mempty))) ["zero", "one"]}
        replaced <- eventAt 20 1 MessageEvent (creation authoritative) initial
        continued <- eventAt 21 2 TextDeltaEvent (delta "assistant_text_delta" "stream" 5 "new block") replaced
        map textBlockText (textBlocks continued) @?= ["zero", "one", "new block"]
        map messageCreatedAt (State.sessionMessages continued) @?= [1],
      testCase "negative fractional and large wire indices neither alias blocks nor allocate gaps" $ do
        first <- eventAt 0 0 TextDeltaEvent (delta "assistant_text_delta" "stream" (-1) "negative") State.emptySessionState
        second <- eventAt 0 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 0 "zero") first
        third <- eventAt 0 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 0.5 "fraction") second
        fourth <- eventAt 0 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 1e100 "large") third
        final <- eventAt 0 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 0.5 "al") fourth
        map textBlockText (textBlocks final) @?= ["negative", "zero", "fractional", "large"],
      testCase "invalid role updates preserve raw history and remain checked-invalid until load" $ do
        user <- fixture "user" Nothing 0 "not assistant output"
        let state = State.upsertSessionMessage user State.emptySessionState
        invalid <- eventAt 10 0 TextDeltaEvent (delta "assistant_text_delta" "user" 0 "wrong") state
        State.sessionMessages invalid @?= [user]
        State.checkedSessionMessages invalid @?= Left State.MessageRoleConflict
        State.checkedSessionMessages (State.mergeLoadedMessages [user] invalid) @?= Right [user]
        wrongResult <- eventAt 10 0 ToolResultEvent (toolResultValue "user" "tool" "wrong role") state
        State.checkedSessionMessages wrongResult @?= Left State.MessageRoleConflict
        State.sessionMessages wrongResult @?= [user]
        eventAt 10 0 TextCompleteEvent (complete "assistant_text_complete" "missing" 0) State.emptySessionState >>= (@?= State.emptySessionState),
      testCase "pending calls and orphan results adopt a real assistant without fake message IDs" $ do
        pending <- eventAt 0 0 ToolCallDeltaEvent (object ["type" .= String "tool_call", "toolUse" .= toolValue "tool" "Read"]) State.emptySessionState
        ids pending @?= []
        map toolUseId (State.pendingToolCalls pending) @?= ["tool"]
        orphan <- eventAt 1 1 ToolResultEvent (toolResultValue "result" "tool" "first") pending
        ids orphan @?= []
        Map.keys (State.orphanToolMessages orphan) @?= ["tool"]
        tool <- decodeValue @ToolUseBlock (toolValue "tool" "Read")
        base <- fixture "assistant" Nothing 0 "unused"
        let assistant = base {messageRole = RoleAssistant, messageContent = [ContentToolUse tool]}
            loaded = State.mergeLoadedMessages [assistant] orphan
        ids loaded @?= ["assistant", "result"]
        canonicalResult <- decodeValue @ToolResultBlock (resultBlockValue "tool" "from load")
        resultBase <- fixture "result" (Just "assistant") 0 "unused"
        let reported = resultBase {messageRole = RoleTool, messageContent = [ContentToolResult canonicalResult]}
        State.sessionMessagesByRole RoleTool (State.mergeLoadedMessages [assistant, reported] orphan) @?= [reported]
        adopted <- eventAt 2 2 MessageEvent (creation assistant) orphan
        ids adopted @?= ["assistant", "result"]
        State.pendingToolCalls adopted @?= []
        Map.null (State.orphanToolMessages adopted) @?= True
        map messageParentId (State.sessionMessagesByRole RoleTool adopted) @?= [Just "assistant"]
        updated <- eventAt 3 3 ToolResultEvent (toolResultValue "result" "tool" "replacement") adopted
        length [block | message <- State.sessionMessagesByRole RoleTool updated, ContentToolResult block <- messageContent message] @?= 1
        [toJSON (toolResultContent block) | message <- State.sessionMessagesByRole RoleTool updated, ContentToolResult block <- messageContent message] @?= [String "replacement"],
      testCase "authoritative assistant updates retain only missing embedded tool results" $ do
        first <- decodeValue @ToolResultBlock (resultBlockValue "a" "old")
        second <- decodeValue @ToolResultBlock (resultBlockValue "b" "retained")
        fresh <- decodeValue @ToolResultBlock (resultBlockValue "a" "new")
        base <- fixture "assistant" Nothing 0 "unused"
        let initial = base {messageRole = RoleAssistant, messageContent = [ContentToolResult first, ContentToolResult second]}
            incoming = base {messageRole = RoleAssistant, messageContent = [ContentToolResult fresh]}
            state = State.upsertSessionMessage incoming (State.upsertSessionMessage initial State.emptySessionState)
        map messageContent (State.sessionMessages state) @?= [[ContentToolResult fresh, ContentToolResult second]],
      testCase "retraction removes provisional buffers and allows later authoritative replacement" $ do
        initial <- eventAt 0 0 TextDeltaEvent (delta "assistant_text_delta" "stream" 3 "old") State.emptySessionState
        retracted <- eventAt 1 1 MessageRetractedEvent (object ["type" .= String "assistant_message_retracted", "messageId" .= String "stream"]) initial
        State.sessionMessages retracted @?= []
        resumed <- eventAt 2 2 TextDeltaEvent (delta "assistant_text_delta" "stream" 0 "new") retracted
        map textBlockText (textBlocks resumed) @?= ["new"],
      testCase "native observation precedes callbacks and malformed state is isolated and repaired by load" $ bounded $ do
        base <- fixture "stream" Nothing 1 "canonical"
        let canonical = base {messageRole = RoleAssistant}
            batches = [[("one", delta "assistant_text_delta" "stream" 0 "one"), ("other", delta "assistant_text_delta" "stream" 0 "other"), ("one", complete "assistant_text_complete" "stream" 0)], [("one", object ["type" .= String "assistant_text_delta", "messageId" .= String "stream", "blockIndex" .= True, "textDelta" .= String "invalid"])]]
        withStatePeer [[], [canonical]] batches $ \connection ->
          Daemon.withResumedSessionOn connection "one" $ \session -> do
            observed <- newEmptyMVar
            let callback (Right (TextDeltaEvent _)) = Daemon.getSessionState connection "one" >>= void . tryPutMVar observed . map textBlockText . textBlocks
                callback _ = pure ()
            bracket (Daemon.onSessionEvent session callback) id $ \_ -> do
              void (Daemon.getProxyToken connection)
              takeMVar observed >>= (@?= ["one"])
              other <- waitState connection "other" (not . null . textBlocks)
              map textBlockText (textBlocks other) @?= ["other"]
              void (Daemon.getProxyToken connection)
              invalid <- waitState connection "one" ((== Just State.MalformedMessageEvent) . State.sessionMessageError)
              State.checkedSessionMessages invalid @?= Left State.MalformedMessageEvent
              map textBlockText (textBlocks invalid) @?= ["one"]
              void (Daemon.loadSessionInfo connection "one")
              Daemon.getSessionState connection "one" >>= (@?= Right [canonical]) . State.checkedSessionMessages
              Daemon.getSessionState connection "other" >>= (@?= Nothing) . State.sessionMessageError,
      testCase "native pending tools become parented results and retraction removes the assistant view" $ bounded $ do
        tool <- decodeValue @ToolUseBlock (toolValue "tool" "Read")
        base <- fixture "assistant" Nothing 1 "unused"
        let assistant = base {messageRole = RoleAssistant, messageContent = [ContentToolUse tool]}
            batches = [[("one", object ["type" .= String "tool_call", "toolUse" .= toolValue "tool" "Read"]), ("one", toolResultValue "result" "tool" "output")], [("one", creation assistant)], [("one", object ["type" .= String "assistant_message_retracted", "messageId" .= String "assistant"])]]
        withStatePeer [] batches $ \connection -> do
          void (Daemon.getProxyToken connection)
          orphan <- waitState connection "one" (not . Map.null . State.orphanToolMessages)
          ids orphan @?= []
          void (Daemon.getProxyToken connection)
          adopted <- waitState connection "one" ((== ["assistant", "result"]) . ids)
          map messageParentId (State.sessionMessagesByRole RoleTool adopted) @?= [Just "assistant"]
          void (Daemon.getProxyToken connection)
          void (waitState connection "one" ((== ["result"]) . ids)),
      testCase "todo text normalizes checkboxes status markers whitespace and ASCII numbering" $ do
        let todos = State.parseTodos (String "\xfeff 1. [completed] done\n- [ ] next\n2)[in_progress] now\n* [X]\nplain\n٣. keep\n\x85NEL\n")
        map State.todoId todos @?= map toText [1 .. 7]
        map State.todoStatus todos @?= [State.TodoCompleted, State.TodoPending, State.TodoInProgress, State.TodoCompleted, State.TodoPending, State.TodoPending, State.TodoPending]
        map State.todoContent todos @?= ["done", "next", "now", "(no description)", "plain", "٣. keep", "\x85NEL"]
        State.parseTodos Null @?= [],
      testCase "todo object arrays filter invalid rows and preserve empty IDs and content" $ do
        let rows = [Null, object ["content" .= String "bad", "status" .= String "other"], object ["content" .= String "", "status" .= String "pending", "id" .= String "", "priority" .= String "low"], object ["content" .= String "second", "status" .= String "completed", "id" .= False, "priority" .= String "invalid"]]
            todos = State.parseTodos (toJSON rows)
        todos @?= [State.TodoItem "" "" State.TodoPending State.TodoLow, State.TodoItem "2" "second" State.TodoCompleted State.TodoHigh]
        State.parseTodos (String "[\"one\",\"[x] two\"]") @?= [State.TodoItem "1" "one" State.TodoPending State.TodoHigh, State.TodoItem "2" "two" State.TodoCompleted State.TodoHigh]
        map State.todoContent (State.parseTodos (toJSON [String "task", Number 2, Bool False, Null, object [], toJSON [String "a", String "b"]])) @?= ["task", "2", "false", "[object Object]", "a,b"],
      testCase "mixed todo strings use JavaScript numeric display without rounding the retained input" $ do
        let values = toJSON [String "scalar", Number 0.000001, Number 1e-7, Number 1e20, Number 1e21, Number 100000000000000001, Number 1e23, Number 1.0000000000000001e23, Number 1e1000]
        map State.todoContent (State.parseTodos values) @?= ["scalar", "0.000001", "1e-7", "100000000000000000000", "1e+21", "100000000000000000", "1e+23", "1.0000000000000001e+23", "Infinity"]
        state <- sessionAt 0 0 ToolCallDeltaEvent (todoCall "numbers" values) State.emptySessionState
        map (KeyMap.lookup "todos" . toolUseInput) (State.pendingToolCalls state) @?= [Just values],
      testCase "live todos require success and older late results cannot replace a newer list" $ do
        a <- sessionAt 0 0 ToolCallDeltaEvent (todoCall "a" (String "first")) State.emptySessionState
        State.sessionTodos a @?= Nothing
        first <- sessionAt 1 1 ToolResultEvent (toolResultValue "result-a" "a" "ok") a
        todoTexts first @?= Just ["first"]
        b <- sessionAt 2 2 ToolCallDeltaEvent (todoCall "b" (String "second")) first
        todoTexts b @?= Just ["first"]
        second <- sessionAt 3 3 ToolResultEvent (toolResultValue "result-b" "b" "ok") b
        old <- sessionAt 4 4 ToolResultEvent (toolResultValue "result-a" "a" "late") second
        todoTexts old @?= Just ["second"]
        revised <- sessionAt 5 5 ToolCallDeltaEvent (todoCall "b" (String "revised")) old
        failed <- sessionAt 6 6 ToolResultEvent (object ["type" .= String "tool_result", "messageId" .= String "result-b", "toolUseId" .= String "b", "isError" .= True]) revised
        todoTexts failed @?= Just ["second"]
        succeeded <- sessionAt 7 7 ToolResultEvent (toolResultValue "result-b" "b" "ok") failed
        todoTexts succeeded @?= Just ["revised"],
      testCase "load todo recovery prefers success then an unfailed pending list and preserves pending calls" $ do
        old <- decodeValue @ToolUseBlock (todoTool "old" (String "successful"))
        latest <- decodeValue @ToolUseBlock (todoTool "latest" (String "pending"))
        success <- decodeValue @ToolResultBlock (resultBlockValue "old" "ok")
        failed <- decodeValue @ToolResultBlock (object ["type" .= String "tool_result", "toolUseId" .= String "latest", "isError" .= True])
        base <- fixture "assistant" Nothing 0 "unused"
        let history blocks = State.mergeLoadedMessages [base {messageRole = RoleAssistant, messageContent = blocks}] State.emptySessionState
        todoTexts (history [ContentToolUse old, ContentToolResult success, ContentToolUse latest]) @?= Just ["successful"]
        todoTexts (history [ContentToolUse latest]) @?= Just ["pending"]
        todoTexts (history [ContentToolUse latest, ContentToolResult failed]) @?= Nothing
        pending <- sessionAt 0 0 ToolCallDeltaEvent (todoCall "pending" (String "survives reload")) State.emptySessionState
        let loaded = State.mergeLoadedMessages [] pending
        completed <- sessionAt 1 1 ToolResultEvent (toolResultValue "result" "pending" "ok") loaded
        todoTexts completed @?= Just ["survives reload"],
      testCase "empty or invalid todo updates do not erase the last accepted list" $ do
        a <- sessionAt 0 0 ToolCallDeltaEvent (todoCall "a" (String "keep")) State.emptySessionState
        b <- sessionAt 0 0 ToolResultEvent (toolResultValue "result-a" "a" "ok") a
        empty <- sessionAt 0 0 ToolCallDeltaEvent (todoCall "empty" (toJSON ([] :: [Value]))) b
        result <- sessionAt 0 0 ToolResultEvent (toolResultValue "result-empty" "empty" "ok") empty
        todoTexts result @?= Just ["keep"],
      testCase "tool phase rank is monotone and settlement preserves the first terminal meaning" $ do
        queued <- sessionAt 0 0 ToolPhaseEvent (phaseValue "tool" "queued") State.emptySessionState
        settled <- sessionAt 0 1 ToolResultEvent (toolResultValue "result" "tool" "ok") queued
        Map.lookup "tool" (State.sessionToolPhases settled) @?= Just ExecutionSettledWithoutExecution
        late <- sessionAt 0 2 ToolPhaseEvent (phaseValue "tool" "executing") settled
        changed <- sessionAt 0 3 ToolPhaseEvent (phaseValue "tool" "settled_after_execution") late
        Map.lookup "tool" (State.sessionToolPhases changed) @?= Just ExecutionSettledWithoutExecution
        unknown <- sessionAt 0 0 ToolResultEvent (toolResultValue "result" "unknown" "ok") State.emptySessionState
        Map.lookup "unknown" (State.sessionToolPhases unknown) @?= Just ExecutionSettledUnknown,
      testCase "progress dedup ignores timestamp only and deadline cleanup does not erase terminal phase" $ do
        first <- sessionAt 0 0 ToolProgressEvent (progressValue "tool" "first" (Just 1)) State.emptySessionState
        duplicate <- sessionAt 0 1 ToolProgressEvent (progressValue "tool" "first" (Just 2)) first
        map progressTimestamp (State.allToolProgress duplicate) @?= [Just 1]
        settled <- sessionAt 0 10 ToolResultEvent (toolResultValue "result" "tool" "ok") duplicate
        State.nextSessionDeadline settled @?= Just 60000010
        same <- sessionAt 0 20 ToolProgressEvent (progressValue "tool" "first" (Just 3)) settled
        State.nextSessionDeadline same @?= Just 60000010
        Map.null (State.sessionToolProgress (State.expireSessionStateAt 60000009 same)) @?= False
        let expired = State.expireSessionStateAt 60000010 same
        Map.null (State.sessionToolProgress expired) @?= True
        Map.lookup "tool" (State.sessionToolPhases expired) @?= Just ExecutionSettledAfterExecution
        fresh <- sessionAt 0 30 ToolProgressEvent (progressValue "tool" "changed" (Just 4)) settled
        State.nextSessionDeadline fresh @?= Nothing
        map progressText (State.allToolProgress fresh) @?= [Just "first", Just "changed"],
      testCase "progress ordering is stable and shares the nearest deadline with optimistic state" $ do
        a <- sessionAt 0 0 ToolProgressEvent (progressValue "a" "a" (Just 2)) State.emptySessionState
        b <- sessionAt 0 0 ToolProgressEvent (progressValue "b" "b" Nothing) a
        c <- sessionAt 0 0 ToolProgressEvent (progressValue "a" "c" (Just 0)) b
        map progressText (State.allToolProgress c) @?= [Just "c", Just "b", Just "a"]
        settled <- sessionAt 0 0 ToolResultEvent (toolResultValue "result" "a" "ok") c
        pending <- either (assertFailure . show) pure (State.registerSubmissionAt 0 (Just 100) "request" "placeholder" (defaultUserMessageParams "pending") settled)
        State.nextSessionDeadline pending @?= Just 100
        let expired = State.expireSessionStateAt 100 pending
        State.nextSessionDeadline expired @?= Just 60000000
        length (State.optimisticSubmissions expired) @?= 1,
      testCase "retry clearing follows event kinds and idle does not overwrite completed thinking duration" $ do
        thinking <- sessionAt 0 0 ThinkingDeltaEvent (delta "thinking_text_delta" "stream" 0 "thought") State.emptySessionState
        completed <- sessionAt 1 1000 ThinkingCompleteEvent (object ["type" .= String "thinking_text_complete", "messageId" .= String "stream", "blockIndex" .= (0 :: Int), "durationMs" .= (0 :: Int)]) thinking
        retrying <- sessionAt 2 2000 RetryEvent (object ["type" .= String "llm_retry", "attempt" .= (1 :: Int), "reason" .= String "overloaded"]) completed
        metadata <- sessionAt 3 3000 TextCompleteEvent (complete "assistant_text_complete" "other" 0) retrying
        State.sessionRetry metadata @?= State.sessionRetry retrying
        idle <- sessionAt 4 4000 WorkingStateEvent (object ["type" .= String "droid_working_state_changed", "newState" .= String "idle"]) metadata
        State.sessionRetry idle @?= Nothing
        map thinkingBlockDuration (thinkingBlocks idle) @?= [mkThinkingDuration 0]
        let loaded = State.mergeLoadedMessages [] idle
        State.sessionRetry loaded @?= Nothing
        Map.null (State.sessionToolProgress loaded) @?= True,
      testCase "native todo progress and inferred working state use the existing ordered owners" $ bounded $ do
        let batches = [[("one", todoCall "todo" (String "native todo")), ("one", toolResultValue "result" "todo" "ok"), ("one", progressValue "tool" "native progress" Nothing)], [("one", object ["type" .= String "droid_working_state_changed", "newState" .= String "thinking"]), ("one", delta "assistant_text_delta" "answer" 0 "text")]]
        withStatePeer [[]] batches $ \connection ->
          Daemon.withResumedSessionOn connection "one" $ \_ -> do
            void (Daemon.getProxyToken connection)
            state <- waitState connection "one" (not . null . State.allToolProgress)
            todoTexts state @?= Just ["native todo"]
            map progressText (State.allToolProgress state) @?= [Just "native progress"]
            void (Daemon.getProxyToken connection)
            void (waitState connection "one" (not . null . textBlocks))
            Daemon.getSessionReadiness connection "one" >>= (@?= Right (Just WorkingStreamingAssistantMessage)) . Daemon.readinessWorkingState,
      testCase "hooks preserve start metadata and replacement order and ignore unknown completion" $ do
        ignored <- sessionAt 0 0 HookCompletedEvent (hookCompleteValue "missing" "completed" False) State.emptySessionState
        ignored @?= State.emptySessionState
        first <- sessionAt 10 100 HookStartedEvent (hookStartValue "first" [] False) ignored
        second <- sessionAt 20 200 HookStartedEvent (hookStartValue "second" [100] False) first
        replaced <- sessionAt 30 300 HookStartedEvent (hookStartValue "first" [0] False) second
        map (startedHookId . State.observedHookStart) (State.sessionHooks replaced) @?= ["first", "second"]
        map State.observedHookWallTime (State.sessionHooks replaced) @?= [30, 20]
        map (startedHookParallel . State.observedHookStart) (State.sessionHooks replaced) @?= [Just False, Just False]
        map (startedHookParallelGroupId . State.observedHookStart) (State.sessionHooks replaced) @?= [Just "", Just ""]
        State.sessionHooks (State.mergeLoadedMessages [] replaced) @?= State.sessionHooks replaced,
      testCase "hook lease expiry is distinct from reported completion and late completion remains authoritative" $ do
        started <- sessionAt 1000 0 HookStartedEvent (hookStartValue "hook" [] False) State.emptySessionState
        State.nextSessionDeadline started @?= Just 90000000
        hookTags (State.expireSessionStateAt 89999999 started) @?= ["running"]
        let expired = State.expireSessionStateAt 90000000 started
        hookTags expired @?= ["expired"]
        State.nextSessionDeadline expired @?= Nothing
        reported <- sessionAt (-1000) 90000001 HookCompletedEvent (hookCompleteValue "hook" "error" False) expired
        hookTags reported @?= ["reported"]
        [completedHookStatus report | hook <- State.sessionHooks reported, State.HookReported report <- [State.observedHookOutcome hook]] @?= [HookFailed],
      testCase "hook deadlines preserve fractional and extreme timeout metadata without early expiry" $ bounded $ do
        fractional <- sessionAt 0 0 HookStartedEvent (hookStartValue "fraction" [60.0000001] False) State.emptySessionState
        State.nextSessionDeadline fractional @?= Just 90000001
        hookTags (State.expireSessionStateAt 90000000 fractional) @?= ["running"]
        hookTags (State.expireSessionStateAt 90000001 fractional) @?= ["expired"]
        huge <- sessionAt 0 0 HookStartedEvent (hookStartValue "huge" [scientific 1 1000000] False) State.emptySessionState
        State.nextSessionDeadline huge @?= Nothing
        map (toJSON . State.observedHookStart) (State.sessionHooks huge) @?= [hookStartValue "huge" [scientific 1 1000000] False]
        hookTags (State.expireSessionStateAt (toInteger (maxBound :: Int)) huge) @?= ["running"],
      testCase "hook visibility honors hidden start or completion while retaining the raw reports" $ do
        started <- sessionAt 0 0 HookStartedEvent (hookStartValue "visible" [] False) State.emptySessionState
        hidden <- sessionAt 0 0 HookStartedEvent (hookStartValue "hidden" [] True) started
        map (startedHookId . State.observedHookStart) (State.visibleSessionHooks hidden) @?= ["visible"]
        completed <- sessionAt 1 1 HookCompletedEvent (hookCompleteValue "visible" "completed" True) hidden
        State.visibleSessionHooks completed @?= []
        length (State.sessionHooks completed) @?= 2,
      testCase "display filtering strips only non-assistant system tags and preserves raw messages" $ do
        user <- fixture "user" Nothing 0 "  before <system-reminder>private</system-reminder> after  "
        assistant <- (\message -> message {messageRole = RoleAssistant}) <$> fixture "assistant" Nothing 1 "<system-reminder>quoted</system-reminder>"
        invisible <- (\message -> message {messageVisibility = Just VisibilityLlmOnly}) <$> fixture "llm" Nothing 2 "hidden"
        hidden <- (\message -> message {messageHiddenFromUserViews = Just True}) <$> fixture "hidden" Nothing 3 "hidden"
        onlyTags <- fixture "only-tags" Nothing 4 "<system-notification>private</system-notification>"
        unclosed <- fixture "unclosed" Nothing 5 "prefix <system-reminder>not closed"
        let raw = [user, assistant, invisible, hidden, onlyTags, unclosed]
            visible = State.filterDisplayMessages raw
        map messageId visible @?= ["user", "assistant", "unclosed"]
        [textBlockText block | message <- visible, ContentText block <- messageContent message] @?= ["before  after", "<system-reminder>quoted</system-reminder>", "prefix <system-reminder>not closed"]
        map messageId raw @?= ["user", "assistant", "llm", "hidden", "only-tags", "unclosed"],
      testCase "standalone persisted-hook detection uses metadata presence not display permission" $ do
        base <- fixture "hook" Nothing 0 "ordinary content"
        let hook = base {messageContent = [], messageHookEventName = Just "Stop", messageHookCommands = Just [], messageHookStatus = Just HookExecuting}
            variants =
              [ base,
                hook,
                hook {messageHookEventName = Nothing},
                hook {messageHookEventName = Just ""},
                hook {messageHookCommands = Nothing},
                hook {messageHookStatus = Nothing},
                hook {messageHookEventName = Just " "},
                hook {messageRole = RoleAssistant},
                hook {messageHiddenFromUserViews = Just True, messageVisibility = Just VisibilityLlmOnly}
              ]
        map State.persistedHook variants @?= [False, True, False, False, False, False, True, True, True],
      testCase "standalone display filtering preserves plain whitespace, exact metadata and order" $ do
        plain <- fixture "plain" Nothing 0 " \tplain \n"
        tagged <- fixture "tagged" Nothing 1 "\xfeff<system-reminder>private</system-reminder> shown \x2028"
        let original = tagged {messageHiddenFromUserViews = Just False, messageAdditionalFields = KeyMap.fromList [("empty", String ""), ("flag", Bool False)]}
            expected = original {messageContent = [ContentText (block {textBlockText = "shown"}) | ContentText block <- messageContent original]}
        State.filterDisplayMessages [plain, original] @?= [plain, expected],
      testCase "empty persisted hook messages remain visible but empty hook names do not confer hook status" $ do
        base <- fixture "hook" Nothing 0 "unused"
        let hook = base {messageContent = [], messageHookEventName = Just "Stop", messageHookCommands = Just [], messageHookStatus = Just HookExecuting}
        State.filterDisplayMessages [hook] @?= [hook]
        State.filterDisplayMessages [hook {messageHookEventName = Just ""}] @?= []
        State.filterDisplayMessages [hook {messageHiddenFromUserViews = Just True}] @?= [],
      testCase "progressive display starts at thirty and doubles without truncating history" $ do
        history <- mapM (\n -> fixture (toText n) Nothing (fromIntegral n) "line") [0 .. 99]
        let loaded = State.mergeLoadedMessages history State.emptySessionState
        State.sessionDisplayLimit loaded @?= Just 30
        displayIds loaded @?= map toText [70 .. 99]
        length (State.sessionMessages loaded) @?= 100
        let (expanded, more) = State.expandSessionDisplay loaded
            (allRows, finished) = State.expandSessionDisplay expanded
        more @?= True
        State.sessionDisplayLimit expanded @?= Just 60
        displayIds expanded @?= map toText [40 .. 99]
        finished @?= False
        displayIds allRows @?= map toText [0 .. 99]
        State.sessionDisplayLimit (State.mergeLoadedMessages history allRows) @?= Nothing,
      testCase "progressive disabling and cutoff clearing are local and do not silently restore pruned data" $ do
        history <- mapM (\n -> fixture (toText n) Nothing (fromIntegral n) "line") [0 .. 64]
        let disabled = State.mergeLoadedMessages history (State.setProgressiveDisplay False State.emptySessionState)
            marked = State.setSessionDisplayCutoff (Just "20") disabled
        State.sessionDisplayLimit marked @?= Nothing
        length (State.sessionMessages marked) @?= 65
        let pruned = State.setProgressiveDisplay True marked
            cleared = State.setSessionDisplayCutoff Nothing pruned
        length (State.sessionMessages pruned) @?= 45
        State.sessionHiddenMessageCount pruned @?= 20
        State.sessionHiddenMessageCount cleared @?= 0
        length (State.sessionMessages cleared) @?= 45
        length (State.sessionMessages (State.mergeLoadedMessages history cleared)) @?= 65,
      testCase "compaction retains the latest todo and its result without accumulating hidden counts on reload" $ do
        plain <- fixture "plain" Nothing 0 "old"
        todo <- decodeValue @ToolUseBlock (todoTool "todo" (String "keep todo"))
        todoBase <- fixture "todo" Nothing 1 "unused"
        result <- decodeValue @ToolResultBlock (resultBlockValue "todo" "ok")
        resultBase <- fixture "result" Nothing 2 "unused"
        obsolete <- fixture "obsolete" Nothing 3 "old"
        boundary <- fixture "boundary" Nothing 4 "boundary"
        tailMessage <- fixture "tail" Nothing 5 "tail"
        let history = [plain, todoBase {messageRole = RoleAssistant, messageContent = [ContentToolUse todo]}, resultBase {messageRole = RoleTool, messageContent = [ContentToolResult result]}, obsolete, boundary, tailMessage]
            loaded = State.mergeLoadedMessages history State.emptySessionState
            pruned = State.setSessionDisplayCutoff (Just "boundary") loaded
        ids pruned @?= ["todo", "result", "boundary", "tail"]
        State.sessionHiddenMessageCount pruned @?= 2
        State.sessionHiddenMessageCount (State.setSessionDisplayCutoff (Just "boundary") pruned) @?= 2
        State.sessionHiddenMessageCount (State.mergeLoadedMessages history pruned) @?= 2
        todoTexts pruned @?= Just ["keep todo"]
        unchanged <- sessionAt 0 0 SessionCompactedEvent (object ["type" .= String "session_compacted", "summaryId" .= String "summary", "removedCount" .= Number 0, "visibleBoundaryMessageId" .= Null]) pruned
        State.sessionDisplayCutoff unchanged @?= Just "boundary"
        ids (State.setSessionDisplayCutoff (Just "missing") loaded) @?= ids loaded,
      testCase "hydration floor distinguishes unresolved tools and executing hooks from later settled text" $ do
        user <- fixture "user" Nothing 0 "settled text"
        tool <- decodeValue @ToolUseBlock (toolValue "tool" "Read")
        assistant <- fixture "assistant" Nothing 1 "unused"
        pending <- decodeValue @ToolResultBlock (resultBlockValue "tool" "__TOOL_RESULT_PENDING__")
        result <- fixture "result" Nothing 2 "unused"
        later <- fixture "later" Nothing 3 "new settled turn"
        let history = [user, assistant {messageRole = RoleAssistant, messageContent = [ContentToolUse tool]}, result {messageRole = RoleTool, messageContent = [ContentToolResult pending]}]
        State.sessionHydrationFloor (State.mergeLoadedMessages history State.emptySessionState) @?= Just "user"
        State.sessionHydrationFloor (State.mergeLoadedMessages (history <> [later]) State.emptySessionState) @?= Just "later"
        let hook = later {messageContent = [], messageHookEventName = Just "Stop", messageHookCommands = Just [], messageHookStatus = Just HookExecuting}
        State.sessionHydrationFloor (State.mergeLoadedMessages [user, hook] State.emptySessionState) @?= Just "user",
      testCase "display overlays remain typed and hidden pending input cannot enter the display projection" $ do
        user <- fixture "real" Nothing 0 "real"
        let base = State.upsertSessionMessage user State.emptySessionState
        system <- either (assertFailure . show) pure (State.registerSubmissionAt 0 Nothing "system" "s" ((defaultUserMessageParams "system") {userMessageRole = Just RoleSystem}) base)
        pending <- either (assertFailure . show) pure (State.registerSubmissionAt 1 Nothing "user" "u" (defaultUserMessageParams "pending") system)
        hidden <- either (assertFailure . show) pure (State.registerSubmissionAt 2 Nothing "hidden" "h" ((defaultUserMessageParams "private") {userMessageVisibility = Just VisibilityLlmOnly}) pending)
        calls <- sessionAt 3 3 ToolCallDeltaEvent (object ["type" .= String "tool_call", "toolUse" .= toolValue "tool" "Read"]) hidden
        map displayKey (State.sessionDisplayEntries calls) @?= ["submission:system", "message:real", "pending-tools", "submission:user"]
        State.checkedSessionDisplay (State.invalidateMessageState State.MalformedMessageEvent calls) @?= Left State.MalformedMessageEvent,
      testCase "native selection controls and hook observation do not mutate remote history" $ bounded $ do
        history <- mapM (\n -> fixture (toText n) Nothing (fromIntegral n) "line") [0 .. 64]
        let batches = [[("one", hookStartValue "hook" [] False), ("other", hookStartValue "hook" [] True)], [("one", object ["type" .= String "session_compacted", "summaryId" .= String "summary", "removedCount" .= Number 0.5, "visibleBoundaryMessageId" .= String "20"])], [("one", hookCompleteValue "hook" "completed" False)]]
        withStatePeer [history] batches $ \connection ->
          Daemon.withResumedSessionOn connection "one" $ \session -> do
            Daemon.getSessionState connection "one" >>= (@?= 30) . length . State.sessionDisplayEntries
            Daemon.expandSessionDisplay connection "one" >>= (@?= True)
            Daemon.expandSessionDisplay connection "one" >>= (@?= False)
            observed <- newEmptyMVar
            let callback (Right (HookStartedEvent _)) = Daemon.getSessionState connection "one" >>= void . tryPutMVar observed . length . State.sessionHooks
                callback _ = pure ()
            bracket (Daemon.onSessionEvent session callback) id $ \_ -> do
              void (Daemon.getProxyToken connection)
              takeMVar observed >>= (@?= 1)
              other <- waitState connection "other" (not . null . State.sessionHooks)
              State.visibleSessionHooks other @?= []
              void (Daemon.getProxyToken connection)
              pruned <- waitState connection "one" ((== Just "20") . State.sessionDisplayCutoff)
              State.sessionHiddenMessageCount pruned @?= 20
              length (State.sessionMessages pruned) @?= 45
              void (Daemon.getProxyToken connection)
              void (waitState connection "one" ((== ["reported"]) . hookTags))
              Daemon.setSessionProgressiveDisplay connection "one" False
              Daemon.setSessionDisplayCutoff connection "one" Nothing
              Daemon.getSessionState connection "one" >>= (@?= Nothing) . State.sessionDisplayLimit,
      testCase "malformed native hook events fail the checked display until a validated reload" $ bounded $ do
        let bad = object ["type" .= String "hook_execution_started", "hookId" .= String "bad", "hookEventName" .= String "Stop", "hookCommands" .= False]
        withStatePeer [[]] [[("one", bad)]] $ \connection -> do
          void (Daemon.getProxyToken connection)
          failed <- waitState connection "one" ((== Just State.MalformedMessageEvent) . State.sessionMessageError)
          State.checkedSessionDisplay failed @?= Left State.MalformedMessageEvent
          void (Daemon.loadSessionInfo connection "one")
          Daemon.getSessionState connection "one" >>= (@?= Right []) . State.checkedSessionDisplay
    ]

ids :: State.SessionState -> [Text]
ids = map messageId . State.sessionMessages

fixture :: Text -> Maybe Text -> Scientific -> Text -> IO FactoryDroidMessage
fixture identifier parent timestamp text =
  case fromJSON (object (["id" .= identifier, "role" .= String "user", "content" .= [object ["type" .= String "text", "text" .= text]], "createdAt" .= timestamp, "updatedAt" .= timestamp] <> maybe [] (\value -> ["parentId" .= value]) parent)) of
    Success message -> pure message
    Error err -> assertFailure err

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

eventAt :: (FromJSON a) => Scientific -> Integer -> (a -> DroidEvent) -> Value -> State.SessionState -> IO State.SessionState
eventAt wall monotonic constructor payload state = (\value -> State.applyMessageEventAt wall monotonic (constructor value) state) <$> decodeValue payload

sessionAt :: (FromJSON a) => Scientific -> Integer -> (a -> DroidEvent) -> Value -> State.SessionState -> IO State.SessionState
sessionAt wall monotonic constructor payload state = (\value -> State.applySessionEventAt wall monotonic (constructor value) state) <$> decodeValue payload

todoTexts :: State.SessionState -> Maybe [Text]
todoTexts = fmap (map State.todoContent) . State.sessionTodos

todoTool :: Text -> Value -> Value
todoTool identifier value = object ["type" .= String "tool_use", "id" .= identifier, "name" .= String "TodoWrite", "input" .= object ["todos" .= value]]

todoCall :: Text -> Value -> Value
todoCall identifier value = object ["type" .= String "tool_call", "toolUse" .= todoTool identifier value]

phaseValue :: Text -> Text -> Value
phaseValue identifier phase = object ["type" .= String "tool_execution_phase_changed", "toolUseId" .= identifier, "toolName" .= String "Read", "phase" .= phase]

progressValue :: Text -> Text -> Maybe Scientific -> Value
progressValue identifier text timestamp = object ["type" .= String "tool_progress_update", "toolUseId" .= identifier, "toolName" .= String "Read", "update" .= object (["type" .= String "status", "text" .= text] <> maybe [] (\value -> ["timestamp" .= value]) timestamp)]

toText :: Int -> Text
toText = Text.pack . show

hookStartValue :: Text -> [Scientific] -> Bool -> Value
hookStartValue identifier timeouts hidden = object ["type" .= String "hook_execution_started", "hookId" .= identifier, "hookEventName" .= String "Stop", "hookCommands" .= [object ["command" .= String "offline-command", "timeout" .= seconds] | seconds <- timeouts], "hiddenFromUserViews" .= hidden, "isParallelExecution" .= False, "parallelGroupId" .= String ""]

hookCompleteValue :: Text -> Text -> Bool -> Value
hookCompleteValue identifier status hidden = object ["type" .= String "hook_execution_completed", "hookId" .= identifier, "hookStatus" .= status, "hookResults" .= ([] :: [Value]), "hiddenFromUserViews" .= hidden]

hookTags :: State.SessionState -> [Text]
hookTags state = [case State.observedHookOutcome hook of State.HookRunning -> "running"; State.HookLeaseExpired -> "expired"; State.HookReported _ -> "reported" | hook <- State.sessionHooks state]

displayIds :: State.SessionState -> [Text]
displayIds state = [messageId message | State.DisplayMessage message <- State.sessionDisplayEntries state]

displayKey :: State.DisplayEntry -> Text
displayKey (State.DisplayMessage message) = "message:" <> messageId message
displayKey (State.DisplaySubmission entry) = "submission:" <> State.submissionRequestId entry
displayKey (State.DisplayPendingTools _) = "pending-tools"

textBlocks :: State.SessionState -> [TextBlock]
textBlocks state = [block | message <- State.sessionMessages state, ContentText block <- messageContent message]

thinkingBlocks :: State.SessionState -> [ThinkingBlock]
thinkingBlocks state = [block | message <- State.sessionMessages state, ContentThinking block <- messageContent message]

delta :: Text -> Text -> Scientific -> Text -> Value
delta kind identifier index text = object ["type" .= kind, "messageId" .= identifier, "blockIndex" .= index, "textDelta" .= text]

complete :: Text -> Text -> Scientific -> Value
complete kind identifier index = object ["type" .= kind, "messageId" .= identifier, "blockIndex" .= index]

creation :: FactoryDroidMessage -> Value
creation message = object ["type" .= String "create_message", "message" .= message]

toolValue :: Text -> Text -> Value
toolValue identifier name = object ["type" .= String "tool_use", "id" .= identifier, "name" .= name, "input" .= object []]

resultBlockValue :: Text -> Text -> Value
resultBlockValue identifier text = object ["type" .= String "tool_result", "toolUseId" .= identifier, "content" .= text, "isError" .= False]

toolResultValue :: Text -> Text -> Text -> Value
toolResultValue identifier toolId text = object ["type" .= String "tool_result", "messageId" .= identifier, "toolUseId" .= toolId, "content" .= text, "isError" .= False]

waitState :: Daemon.DaemonConnection -> Text -> (State.SessionState -> Bool) -> IO State.SessionState
waitState connection identifier ready = do
  state <- Daemon.getSessionState connection identifier
  if ready state then pure state else Daemon.waitSessionStateChange connection identifier state >> waitState connection identifier ready

withStatePeer :: [[FactoryDroidMessage]] -> [[(Text, Value)]] -> (Daemon.DaemonConnection -> IO a) -> IO a
withStatePeer histories batches action = do
  loads <- newIORef histories
  notifications <- newIORef batches
  withPeer (\_ connection -> serve loads notifications connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
    Daemon.withConnection ((Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}) action
  where
    serve loads notifications connection = forever $ do
      request <- WS.receiveData connection >>= either (const (assertFailure "Invalid peer RPC")) pure . eitherDecode
      case KeyMap.lookup "method" request of
        Just (String "daemon.authenticate") -> reply connection request (object ["userId" .= String "user", "orgId" .= String "org"])
        Just (String "daemon.list_terminals") -> reply connection request (object ["terminals" .= ([] :: [Value])])
        Just (String "daemon.load_session") -> do
          history <- takeFixture loads
          reply connection request (object ["session" .= object ["messages" .= history], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]])
        Just (String "daemon.get_proxy_token") -> do
          batch <- takeFixture notifications
          forM_ batch $ \(identifier, notification) -> sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= notification]]
          reply connection request (object ["token" .= String "OFFLINE_TOKEN"])
        _ -> assertFailure "Unexpected state peer RPC"

takeFixture :: IORef [a] -> IO a
takeFixture ref = atomicModifyIORef' ref (\case [] -> ([], Nothing); value : rest -> (rest, Just value)) >>= maybe (assertFailure "Peer fixture exhausted") pure

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request result = case KeyMap.lookup "id" request of
  Just identifier -> sendFrame connection ["type" .= String "response", "id" .= identifier, "result" .= result]
  Nothing -> assertFailure "Missing peer request ID"

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection pairs = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> pairs)))
