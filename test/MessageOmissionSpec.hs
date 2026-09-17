{-# LANGUAGE OverloadedStrings #-}

module MessageOmissionSpec (messageOmissionTests) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as K
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Factory.Droid.Daemon qualified as D
import Factory.Droid.Schema.Control (defaultUserMessageParams)
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Messages (FactoryDroidMessage, messageId, messageParentId)
import Factory.Droid.Schema.Notifications (CreateMessage (..))
import Factory.Droid.SessionState qualified as S
import Factory.Droid.Transport (objectTransport)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

messageOmissionTests :: TestTree
messageOmissionTests =
  testGroup
    "Observed message metadata"
    [ testCase "omitting hidden metadata does not expose a previously hidden message" $ do
        first <- observe (wire (K.singleton "hiddenFromUserViews" (Bool True))) S.emptySessionState
        second <- observe (wire (K.singleton "content" (toJSON [object ["type" .= String "text", "text" .= String "changed visible-looking text"]]))) first
        map messageId (S.filterDisplayMessages (S.sessionMessages second)) @?= []
        stored second >>= (@?= Bool True) . field "hiddenFromUserViews",
      testCase "every optional metadata field survives while required content and zero timestamps replace" $ do
        let initial = wire rich
            incoming = wire (K.fromList ["createdAt" .= (0 :: Int), "updatedAt" .= (0 :: Int), "content" .= ([] :: [Value])])
        state <- observe initial S.emptySessionState >>= observe incoming
        stored state >>= (@?= overlay incoming initial),
      testCase "explicit false empty null and zero override while other metadata remains" $ do
        let initial = wire rich
            incoming = wire reset
        state <- observe initial S.emptySessionState >>= observe incoming
        stored state >>= (@?= overlay incoming initial)
        map messageId (S.filterDisplayMessages (S.sessionMessages state)) @?= ["m"],
      testCase "opaque extension omission and null remain distinct" $ do
        first <- observe (wire (K.fromList ["future" .= object ["keep" .= (42 :: Int)], "other" .= String "kept"])) S.emptySessionState
        omitted <- observe (wire mempty) first
        stored omitted >>= (@?= object ["keep" .= (42 :: Int)]) . field "future"
        cleared <- observe (wire (K.singleton "future" Null)) omitted
        stored cleared >>= (@?= Null) . field "future"
        stored cleared >>= (@?= String "kept") . field "other",
      testCase "metadata merge stays at observation boundary, not pure full-record editing" $ do
        old <- parsed (wire rich)
        new <- parsed (wire mempty)
        let edited = S.upsertSessionMessage new (S.upsertSessionMessage old S.emptySessionState)
        stored edited >>= (@?= toJSON (new :: FactoryDroidMessage)),
      testCase "an authoritative loaded snapshot clears omitted metadata" $ do
        initial <- observe (wire rich) S.emptySessionState
        new <- parsed (wire mempty)
        stored (S.mergeLoadedMessages [new] initial) >>= (@?= toJSON (new :: FactoryDroidMessage)),
      testCase "retained sideband visibility and hook metadata keep conversation ancestry stable" $ do
        forM_ [K.singleton "visibility" (String "user_only"), K.fromList ["hookEventName" .= String "Stop", "hookCommands" .= ([] :: [Value]), "hookStatus" .= String "completed"]] $ \metadata -> do
          let user = wire (K.fromList ["id" .= String "u"])
              side = wire (K.union metadata (K.fromList ["id" .= String "m", "role" .= String "system", "parentId" .= String "u"]))
              update = wire (K.fromList ["role" .= String "system", "updatedAt" .= (2 :: Int)])
              next = wire (K.fromList ["id" .= String "a", "role" .= String "assistant", "createdAt" .= (3 :: Int), "updatedAt" .= (3 :: Int)])
          prefix <- observe user S.emptySessionState >>= observe side >>= observe update
          fmap messageId (S.sessionLastConversationMessage prefix) @?= Just "u"
          final <- observe next prefix
          (messageParentId =<< Map.lookup "a" (S.sessionMessagesById final)) @?= Just "u",
      testCase "explicit sideband reclassification still updates the conversation pointer" $ do
        initial <- observe (wire (K.fromList ["role" .= String "system", "visibility" .= String "user_only"])) S.emptySessionState
        final <- observe (wire (K.fromList ["role" .= String "system", "visibility" .= String "both"])) initial
        fmap messageId (S.sessionLastConversationMessage initial) @?= Nothing
        fmap messageId (S.sessionLastConversationMessage final) @?= Just "m",
      testCase "new IDs do not inherit another message's metadata" $ do
        initial <- observe (wire rich) S.emptySessionState
        let incoming = wire (K.fromList ["id" .= String "other", "parentId" .= String "external"])
        final <- observe incoming initial
        fmap toJSON (Map.lookup "other" (S.sessionMessagesById final)) @?= Just incoming,
      testCase "request confirmation still follows the observed metadata merge" $ do
        initial <- observe (wire rich) S.emptySessionState
        prepared <- either (const (assertFailure "Could not prepare submission")) pure (S.registerSubmissionAt 0 Nothing "request" "placeholder" (defaultUserMessageParams "fixture") initial)
        event <- parsed (object ["type" .= String "create_message", "requestId" .= String "request", "message" .= wire mempty])
        let final = S.observeCreatedMessage event prepared
        S.isSubmissionConfirmed "request" final @?= True
        S.lookupSubmission "request" final @?= Nothing
        stored final >>= (@?= Bool True) . field "hiddenFromUserViews",
      testCase "nonnullable metadata still rejects null before it can be an observation" $ do
        forM_ ["visibility", "hiddenFromUserViews", "hookEventName", "hookCommands", "modelId"] $ \key ->
          case fromJSON (object ["type" .= String "create_message", "message" .= wire (K.singleton key Null)]) :: Result CreateMessage of
            Error _ -> pure ()
            Success _ -> assertFailure "Invalid null became omission",
      testCase "the default daemon intake preserves hidden metadata without an application mirror" $ bounded $ do
        incoming <- newTQueueIO
        let send frame = case (K.lookup "method" frame, K.lookup "id" frame) of
              (Just (String "daemon.get_proxy_token"), Just (String identifier)) -> atomically (writeTQueue incoming (reply identifier (object ["token" .= String "OFFLINE_ONLY"])))
              _ -> assertFailure "Unexpected omission fixture request"
            options = D.defaultDaemonClientOptions (D.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline"
        D.withConnectionOn options (objectTransport send (atomically (readTQueue incoming))) $ \connection -> do
          void (D.registerSessionState connection "s" "fixture")
          forM_ [wire (K.singleton "hiddenFromUserViews" (Bool True)), wire mempty] $ \message -> atomically $ writeTQueue incoming (K.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= String "s", "notification" .= object ["type" .= String "create_message", "message" .= message]]])
          settled <- newEmptyTMVarIO
          bracket (D.onRequestSettled connection (const (atomically (void (tryPutTMVar settled ()))))) id $ \_ -> do
            void (D.getProxyToken connection)
            atomically (readTMVar settled)
          state <- D.getSessionState connection "s"
          stored state >>= (@?= Bool True) . field "hiddenFromUserViews"
          map messageId (S.filterDisplayMessages (S.sessionMessages state)) @?= []
    ]

parsed :: (FromJSON a) => Value -> IO a
parsed value = case fromJSON value of Success result -> pure result; Error problem -> assertFailure problem

observe :: Value -> S.SessionState -> IO S.SessionState
observe value state = (`S.observeCreatedMessage` state) <$> parsed (object ["type" .= String "create_message", "message" .= value])

stored :: S.SessionState -> IO Value
stored state = maybe (assertFailure "Message missing") (pure . toJSON) (Map.lookup "m" (S.sessionMessagesById state))

field :: Key -> Value -> Value
field key (Object fields) = fromMaybe Null (K.lookup key fields)
field _ _ = Null

wire :: Object -> Value
wire fields = Object (K.union fields (K.fromList ["id" .= String "m", "role" .= String "user", "createdAt" .= (1 :: Int), "updatedAt" .= (1 :: Int), "content" .= [object ["type" .= String "text", "text" .= String "fixture"]]]))

overlay :: Value -> Value -> Value
overlay (Object fresh) (Object old) = Object (K.union fresh old)
overlay _ _ = error "Non-object test message"

rich, reset :: Object
rich = K.fromList ["visibility" .= String "both", "openaiMessageId" .= String "message-fixture", "openaiPhase" .= String "commentary", "openaiEncryptedContent" .= String "not-a-secret", "openaiReasoningId" .= String "reason-fixture", "openaiReasoningSummary" .= String "summary", "geminiThoughtSignature" .= String "fixture-signature", "chatCompletionReasoningField" .= String "reasoning_content", "chatCompletionReasoningContent" .= String "fixture-reasoning", "isUserVisible" .= True, "isError" .= True, "userMessageSource" .= String "api", "interactionMode" .= String "auto", "modelId" .= String "model", "routerId" .= String "router", "reasoningEffort" .= String "low", "apiProvider" .= String "anthropic", "hookEventName" .= String "Stop", "hookMatcher" .= String "match", "hookCommands" .= ([] :: [Value]), "hookStatus" .= String "completed", "hookResults" .= ([] :: [Value]), "hookToolCallId" .= String "tool", "hookParentId" .= String "parent", "hookOrder" .= (4 :: Int), "hookPreventedAction" .= True, "hiddenFromUserViews" .= True, "hookStartTime" .= (12 :: Int), "hookEndTime" .= (14 :: Int), "isParallelExecution" .= True, "parallelGroupId" .= String "group", "future" .= object ["keep" .= (42 :: Int)]]
reset = K.fromList ["createdAt" .= (0 :: Int), "updatedAt" .= (0 :: Int), "openaiMessageId" .= String "", "openaiPhase" .= Null, "openaiEncryptedContent" .= String "", "openaiReasoningId" .= String "", "openaiReasoningSummary" .= String "", "geminiThoughtSignature" .= String "", "chatCompletionReasoningContent" .= String "", "isUserVisible" .= False, "isError" .= False, "hookEventName" .= String "", "hookMatcher" .= String "", "hookCommands" .= ([] :: [Value]), "hookResults" .= ([] :: [Value]), "hookToolCallId" .= String "", "hookParentId" .= String "", "hookOrder" .= (0 :: Int), "hookPreventedAction" .= False, "hiddenFromUserViews" .= False, "hookStartTime" .= (0 :: Int), "hookEndTime" .= (0 :: Int), "isParallelExecution" .= False, "parallelGroupId" .= String "", "future" .= Null]
