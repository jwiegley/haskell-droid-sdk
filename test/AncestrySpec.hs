{-# LANGUAGE OverloadedStrings #-}

module AncestrySpec (ancestryTests) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as K
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidEvent (..))
import Factory.Droid.Daemon qualified as D
import Factory.Droid.Schema.Content (ContentBlock (..), ToolUseBlock (..))
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Enums (MessageRole (..), MessageVisibility (..))
import Factory.Droid.Schema.Messages (FactoryDroidMessage, HookStatus (..), Message (..))
import Factory.Droid.SessionState qualified as S
import Factory.Droid.Transport (objectTransport)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

ancestryTests :: TestTree
ancestryTests =
  testGroup "Conversation ancestry" $
    [ testCase name $ do
        let prefix = insert [user, side]
            result = S.upsertSessionMessage (assistant {messageParentId = Just (messageId side)}) prefix
        lastId prefix @?= Just expected
        parentOf "a" result @?= Just expected
        map messageId (S.sessionMessages result) @?= ["u", messageId side, "a"]
        -- Aside from the intended incoming parent, no payload is rewritten.
        S.sessionMessages result @?= [user, side, assistant {messageParentId = Just expected}]
    | (name, side, expected) <-
        [ ("system user-only receipt", receipt, "u"),
          ("ordinary system control", receipt {messageVisibility = Just VisibilityBoth}, "r"),
          ("llm-only system control", receipt {messageVisibility = Just VisibilityLlmOnly}, "r"),
          ("hidden-from-user is not sideband", receipt {messageVisibility = Nothing, messageHiddenFromUserViews = Just True}, "r"),
          ("user visibility control", receipt {messageRole = RoleUser}, "r"),
          ("assistant visibility control", receipt {messageRole = RoleAssistant}, "r"),
          ("persisted hook", hook, "u"),
          ("assistant-role hook", hook {messageRole = RoleAssistant}, "u"),
          ("whitespace hook event is present", hook {messageHookEventName = Just " "}, "u"),
          ("empty hook event is not present", hook {messageHookEventName = Just ""}, "h"),
          ("missing hook commands is not present", hook {messageHookCommands = Nothing}, "h"),
          ("missing hook status is not present", hook {messageHookStatus = Nothing}, "h")
        ]
    ]
      <> [ testCase "implicit sidebands do not acquire or become inferred parents" $ do
             forM_ [receipt, hook] $ \side -> do
               let unlinked = side {messageParentId = Nothing}
                   state = insert [user, unlinked, assistant]
               parentOf (messageId side) state @?= Nothing
               parentOf "a" state @?= Just "u"
               lastId state @?= Just "a",
           testCase "walks through mixed sidebands to the nearest conversation ancestor" $ do
             let state = insert [user, hook, receipt {messageParentId = Just "h"}, assistant {messageParentId = Just "r"}]
             parentOf "a" state @?= Just "u",
           testCase "known root dangling and cyclic sidebands fall back without hanging" $ do
             forM_ [[receipt {messageParentId = Nothing}], [receipt {messageParentId = Just "missing"}], [receipt {messageParentId = Just "r2"}, receipt {messageId = "r2", messageParentId = Just "r"}]] $ \chain -> do
               let state = insert ([user] <> chain <> [assistant {messageParentId = Just "r"}])
               parentOf "a" state @?= Just "u",
           testCase "a sideband-only history supplies no inferred conversation parent" $ do
             let state = insert [receipt {messageParentId = Nothing}, assistant {messageParentId = Just "r"}]
             parentOf "a" state @?= Nothing
             lastId state @?= Just "a",
           testCase "unknown explicit parents survive while self-parents inherit" $ do
             parentOf "a" (insert [user, assistant {messageParentId = Just "unknown"}]) @?= Just "unknown"
             parentOf "a" (insert [user, assistant {messageParentId = Just "a"}]) @?= Just "u",
           testCase "sidebands append despite old timestamps and do not advance the pointer" $ do
             let state = insert [user, assistant, receipt {messageCreatedAt = 0, messageUpdatedAt = 0}]
             map messageId (S.sessionMessages state) @?= ["u", "a", "r"]
             lastId state @?= Just "a",
           testCase "an insertion before the transcript tail remains the conversation pointer" $ do
             map messageId (S.sessionMessages arrival) @?= ["u", "middle", "tail"]
             lastId arrival @?= Just "middle"
             parentOf "next" (S.upsertSessionMessage next arrival) @?= Just "middle",
           testCase "ordinary replacement preserves arrival identity; classification changes recompute" $ do
             let updated = S.upsertSessionMessage (tailMessage {messageUpdatedAt = 12}) arrival
                 reclassified = S.upsertSessionMessage (middle {messageRole = RoleSystem, messageVisibility = Just VisibilityUserOnly}) updated
             lastId updated @?= Just "middle"
             lastId reclassified @?= Just "tail"
             lastId (S.upsertSessionMessage middle reclassified) @?= Just "tail",
           testCase "existing-message parent replacement does not normalize historical links" $ do
             let state = insert [user, receipt, assistant]
             parentOf "a" (S.upsertSessionMessage (assistant {messageParentId = Just "r"}) state) @?= Just "r",
           testCase "removal recomputes only when the current insertion was removed" $ do
             lastId (S.removeSessionMessage "middle" arrival) @?= Just "tail"
             lastId (S.removeSessionMessage "tail" arrival) @?= Just "middle"
             lastId (S.removeSessionMessage "absent" arrival) @?= Just "middle",
           testCase "actual truncation recomputes; a no-op truncation preserves arrival identity" $ do
             lastId (S.truncateSessionMessages 2 arrival) @?= Just "tail"
             lastId (S.truncateSessionMessages 3 arrival) @?= Just "middle"
             lastId (S.truncateSessionMessages 0 arrival) @?= Nothing,
           testCase "loads retain historical graph but recompute the usable conversation tail" $ do
             let loaded = S.mergeLoadedMessages [user, receipt, assistant {messageParentId = Just "r"}] S.emptySessionState
                 sidebandTail = S.mergeLoadedMessages [user, receipt] S.emptySessionState
             parentOf "a" loaded @?= Just "r"
             lastId loaded @?= Just "a"
             lastId sidebandTail @?= Just "u"
             parentOf "a" (S.upsertSessionMessage assistant sidebandTail) @?= Just "u"
             lastId (S.mergeLoadedMessages [] arrival) @?= Just "tail",
           testCase "pruning repairs an evicted pointer but preserves a retained insertion" $ do
             let sidebandTail = insert [user, assistant, receipt {messageParentId = Just "a"}]
                 pruned = S.setSessionDisplayCutoff (Just "r") sidebandTail
             map messageId (S.sessionMessages pruned) @?= ["r"]
             lastId pruned @?= Nothing
             parentOf "next" (S.upsertSessionMessage next pruned) @?= Nothing
             lastId (S.setSessionDisplayCutoff (Just "middle") arrival) @?= Just "middle",
           testCase "text and thinking placeholders inherit the conversation pointer" $ do
             text <- parsed (delta "assistant_text_delta")
             thinking <- parsed (delta "thinking_text_delta")
             parentOf "a" (S.applyMessageEventAt 1000 0 (TextDeltaEvent text) (insert [user, receipt])) @?= Just "u"
             parentOf "a" (S.applyMessageEventAt 1000 0 (ThinkingDeltaEvent thinking) (insert [user, hook])) @?= Just "u",
           testCase "tool calls reuse the conversation assistant behind a sideband tail" $ do
             event <- parsed (object ["type" .= String "tool_call", "toolUse" .= object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "Read", "input" .= object []]])
             let state = S.applyMessageEventAt 1000 0 (ToolCallDeltaEvent event) (insert [user, assistant, receipt {messageParentId = Just "a"}])
             [toolUseId tool | message <- S.sessionMessages state, messageId message == "a", ContentToolUse tool <- messageContent message] @?= ["tool"]
             S.pendingToolCalls state @?= [],
           testCase "empty IDs remain data but not usable conversation parents" $ do
             let empty = user {messageId = ""}
             lastId (insert [empty]) @?= Nothing
             S.sessionMessages (insert [empty]) @?= [empty]
             parentOf "a" (insert [empty, assistant]) @?= Nothing
             parentOf "a" (insert [user, assistant {messageParentId = Just ""}]) @?= Just "",
           testCase "cache retirement clears the pointer with its payload" $ do
             lastId (S.clearCachedSessionState arrival) @?= Nothing,
           testCase "daemon intake applies ancestry before observed state is returned" $ bounded $ do
             incoming <- newTQueueIO
             let send frame = case K.lookup "method" frame of
                   Just (String "daemon.load_session") -> respond incoming frame (object ["sessionId" .= String "s", "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"]])
                   Just (String "daemon.get_proxy_token") -> respond incoming frame (object ["token" .= String "OFFLINE_ONLY"])
                   _ -> assertFailure "Unexpected ancestry fixture request"
                 options = (D.defaultDaemonClientOptions (D.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline") {D.daemonClientRestoreTerminalsOnLoad = False}
             D.withConnectionOn options (objectTransport send (atomically (readTQueue incoming))) $ \connection -> do
               void (D.loadSessionInfo connection "s")
               forM_ [user, receipt, assistant {messageParentId = Just "r"}] $ \message -> atomically $ writeTQueue incoming (K.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= String "s", "notification" .= object ["type" .= String "create_message", "message" .= message]]])
               settled <- newEmptyTMVarIO
               bracket (D.onRequestSettled connection (const (atomically (void (tryPutTMVar settled ()))))) id $ \_ -> do
                 void (D.getProxyToken connection)
                 atomically (readTMVar settled)
               state <- D.getSessionState connection "s"
               parentOf "a" state @?= Just "u"
               lastId state @?= Just "a"
         ]

insert :: [FactoryDroidMessage] -> S.SessionState
insert = foldl' (flip S.upsertSessionMessage) S.emptySessionState

lastId :: S.SessionState -> Maybe Text
lastId = fmap messageId . S.sessionLastConversationMessage

parentOf :: Text -> S.SessionState -> Maybe Text
parentOf identifier state = messageParentId =<< Map.lookup identifier (S.sessionMessagesById state)

user, assistant, receipt, hook, middle, tailMessage, next :: FactoryDroidMessage
user = (base "u" RoleUser 1) {messageAdditionalFields = K.singleton "fixture" (String "u")}
assistant = base "a" RoleAssistant 3
receipt = (base "r" RoleSystem 2) {messageParentId = Just "u", messageVisibility = Just VisibilityUserOnly}
hook = (base "h" RoleSystem 2) {messageParentId = Just "u", messageHookEventName = Just "Stop", messageHookCommands = Just [], messageHookStatus = Just HookCompleted}
middle = (base "middle" RoleAssistant 2) {messageParentId = Just "u"}
tailMessage = (base "tail" RoleAssistant 10) {messageParentId = Just "other"}
next = base "next" RoleUser 13

arrival :: S.SessionState
arrival = insert [user, tailMessage, middle]

base :: Text -> MessageRole -> Int -> FactoryDroidMessage
base identifier role time = case fromJSON (object ["id" .= identifier, "role" .= role, "createdAt" .= time, "updatedAt" .= time, "content" .= ([] :: [Value])]) of
  Success message -> message
  Error problem -> error problem

parsed :: (FromJSON a) => Value -> IO a
parsed value = case fromJSON value of
  Success result -> pure result
  Error problem -> assertFailure problem

delta :: Text -> Value
delta kind = object ["type" .= kind, "messageId" .= String "a", "blockIndex" .= (0 :: Int), "textDelta" .= String "text"]

respond :: TQueue Object -> Object -> Value -> IO ()
respond incoming frame value = case fromMaybe Null (K.lookup "id" frame) of
  String identifier -> atomically (writeTQueue incoming (reply identifier value))
  _ -> assertFailure "Missing ancestry request ID"
