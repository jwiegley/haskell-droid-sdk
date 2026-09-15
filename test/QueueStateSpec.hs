{-# LANGUAGE OverloadedStrings #-}

module QueueStateSpec (queueStateTests) where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (cancelWith, wait, waitCatch, withAsync)
import Control.Exception (Exception, catch, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidInvalidEvent), DroidResult (resultCompletion))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError)
import Factory.Droid.Schema.Control (AddUserMessageParams (..), QueuePlacement (..), QueueResolution (..), QueuedUserMessage (..), ResolveQueuedMessageParams (..), defaultUserMessageParams)
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

queueStateTests :: TestTree
queueStateTests =
  testGroup
    "Session queue state"
    [ testCase "all five queue kinds retain exact review, group and daemon classification" $ do
        let variants =
              [ (State.QueueDeferredAfterInterrupt, False, False, State.QueueSteeringGroup, Nothing),
                (State.QueuePaused, False, True, State.QueueQueuedGroup, Just 1),
                (State.QueueDaemonDiscardable, True, True, State.QueueSteeringGroup, Just 0),
                (State.QueueDaemonEndOfLoop, True, True, State.QueueQueuedGroup, Just 1),
                (State.QueueDeferredDuringCompaction, False, False, State.QueueQueuedGroup, Nothing)
              ]
        forM_ variants $ \(kind, daemon, reviewable, group, priority) ->
          (State.isDaemonQueuedMessage kind, State.isReviewableQueuedMessage kind, State.queueDisplayGroup kind, State.queueReviewPriority kind) @?= (daemon, reviewable, group, priority)
        State.queueKindForPlacement Nothing @?= State.QueueDaemonDiscardable
        State.queueKindForPlacement (Just QueueEndOfLoop) @?= State.QueueDaemonEndOfLoop,
      testCase "review selection preserves payloads and stable priority ties without dequeue" $ do
        let manual = entry "manual" State.QueueDeferredDuringCompaction "manual"
            paused = entry "paused" State.QueuePaused "paused"
            end = entry "end" State.QueueDaemonEndOfLoop "end"
            discard = withInput (\input -> input {userMessageText = "", userMessageSkipAgentLoop = Just False, userMessageAdditionalFields = KeyMap.singleton "future" (Number 9007199254740993)}) (entry "discard" State.QueueDaemonDiscardable "discard")
            deferred = entry "deferred" State.QueueDeferredAfterInterrupt "deferred"
            state = State.enqueueMessages [manual, paused, end, discard, deferred] State.emptySessionState
            reviewed = sortOn (State.queueReviewPriority . State.queueEntryKind) (filter (State.isReviewableQueuedMessage . State.queueEntryKind) (State.sessionQueue state))
        reviewed @?= [discard, paused, end]
        map (State.queueDisplayGroup . State.queueEntryKind) reviewed @?= [State.QueueSteeringGroup, State.QueueQueuedGroup, State.QueueQueuedGroup]
        State.sessionQueue state @?= [manual, paused, end, discard, deferred],
      testCase "queue replacement preserves position and exact input and removes optimistic duplicates" $ do
        prepared <- either (assertFailure . show) pure (State.registerSubmissionAt 0 Nothing "a" "placeholder" (defaultUserMessageParams "optimistic") State.emptySessionState)
        let a = entry "a" State.QueueDeferredAfterInterrupt "first"
            b = entry "b" State.QueueDaemonDiscardable "second"
            newer = withInput (\input -> input {userMessageText = "", userMessageSkipAgentLoop = Just False, userMessageAdditionalFields = KeyMap.singleton "future" (Number 100000000000000001)}) a
            queued = State.enqueueMessages [a, b, newer] prepared
        State.sessionQueue queued @?= [newer, b]
        State.optimisticSubmissions queued @?= []
        State.sessionQueue (State.enqueueMessages [a] (State.markQueuedMessageProcessed "a" queued)) @?= [b],
      testCase "daemon replacement preserves local work and applies duplicate and processed-ID guards" $ do
        let local = entry "local" State.QueuePaused "local"
            old = entry "old" State.QueueDaemonDiscardable "old"
            reported = entry "new" State.QueueDaemonEndOfLoop "first"
            newer = withInput (\input -> input {userMessageText = "replacement"}) reported
            state = State.enqueueMessages [old, local] State.emptySessionState
            replaced = State.replaceDaemonQueue [reported, newer] state
        State.sessionQueue replaced @?= [local, newer]
        State.sessionQueue (State.replaceDaemonQueue [] replaced) @?= [local]
        State.sessionQueue (State.replaceDaemonQueue [newer] (State.markQueuedMessageProcessed "new" replaced)) @?= [local],
      testCase "front restoration is ordered and cannot resurrect a confirmed request" $ do
        let a = entry "a" State.QueueDeferredAfterInterrupt "a"
            b = entry "b" State.QueuePaused "b"
            c = entry "c" State.QueueDeferredDuringCompaction "c"
            b2 = withInput (\input -> input {userMessageText = "new b"}) b
            initial = State.enqueueMessages [a, b, c] State.emptySessionState
            restored = State.restoreQueueFront [b, a, b2] initial
        State.sessionQueue restored @?= [b2, a, c]
        State.sessionQueue (State.restoreQueueFront [a] (State.markQueuedMessageProcessed "a" restored)) @?= [b2, c]
        let (removed, selected) = State.dequeueQueuedMessage (Just State.QueuePaused) restored
        selected @?= Just b2
        State.sessionQueue removed @?= [a, c]
        snd (State.dequeueQueuedMessages (Just []) restored) @?= []
        State.sessionQueue (State.clearQueuedMessages (Just []) restored) @?= [b2, a, c]
        State.sessionQueue (State.clearQueuedMessages Nothing restored) @?= [],
      testCase "interrupt pause distinguishes attachments discardable text end-of-loop and local entries" $ do
        document <- decodeValue @AddUserMessageParams (object ["text" .= String "", "files" .= [object ["type" .= String "text", "mediaType" .= String "text/plain", "data" .= String "document"]]])
        image <- decodeValue @AddUserMessageParams (object ["text" .= String "", "content" .= [object ["type" .= String "image", "source" .= object ["type" .= String "base64", "mediaType" .= String "image/png", "data" .= String "AQ=="]]]])
        let discard = entry "discard" State.QueueDaemonDiscardable "text"
            attached = withInput (\input -> input {userMessageImagePaths = Just ["NOT_READ"]}) (entry "attached" State.QueueDaemonDiscardable "")
            emptyAttachment = withInput (\input -> input {userMessageImagePaths = Just []}) (entry "empty" State.QueueDaemonDiscardable "text")
            end = entry "end" State.QueueDaemonEndOfLoop "text"
            local = entry "local" State.QueueDeferredDuringCompaction "local"
            doc = State.QueueEntry (QueuedUserMessage "document" document) State.QueueDaemonDiscardable 0
            picture = State.QueueEntry (QueuedUserMessage "image" image) State.QueueDaemonDiscardable 0
            state = State.enqueueMessages [discard, attached, emptyAttachment, end, local, doc, picture] State.emptySessionState
        map (\queued -> (State.queueEntryRequestId queued, State.queueEntryKind queued)) (State.sessionQueue (State.pauseDaemonQueue Nothing state)) @?= [("attached", State.QueuePaused), ("end", State.QueuePaused), ("local", State.QueueDeferredDuringCompaction), ("document", State.QueuePaused), ("image", State.QueuePaused)]
        map State.queueEntryRequestId (State.sessionQueue (State.pauseDaemonQueue (Just "end") state)) @?= ["attached", "local", "document", "image"],
      testCase "reload keeps busy drained entries but pauses idle uncertainty without text or clock inference" $ do
        let local = entry "local" State.QueueDeferredAfterInterrupt "local"
            remote = entry "remote" State.QueueDaemonDiscardable "same"
            initial = State.enqueueMessages [local, remote] State.emptySessionState
        unrelated <- decodeValue @FactoryDroidMessage (messageValue "unrelated" "same")
        let busy = State.reconcileDaemonQueueAt 100 [unrelated] [] True initial
            paused = State.reconcileDaemonQueueAt 100 [unrelated] [] False initial
        State.sessionQueue busy @?= [local, remote]
        map State.queueEntryKind (State.sessionQueue paused) @?= [State.QueueDeferredAfterInterrupt, State.QueuePaused]
        delivered <- decodeValue @FactoryDroidMessage (messageValue "message-remote" "same")
        State.sessionQueue (State.reconcileDaemonQueueAt 200 [delivered] [] False paused) @?= [local]
        State.sessionQueue (State.reconcileDaemonQueueAt 200 [delivered] [State.queueEntryMessage remote] False initial) @?= [local, remote {State.queueEntryObservedAt = 200}],
      testCase "successful resolution changes placement or retires identity without inventing missing entries" $ do
        let a = entry "a" State.QueueDaemonDiscardable "a"
            b = entry "b" State.QueuePaused "b"
            initial = State.enqueueMessages [b, a] State.emptySessionState
            updated = State.applyQueueResolution (ResolveQueuedMessageParams "a" (UpdateQueuedMessage QueueEndOfLoop) mempty) initial
        map State.queueEntryRequestId (State.sessionQueue updated) @?= ["a", "b"]
        map State.queueEntryKind (State.sessionQueue updated) @?= [State.QueueDaemonEndOfLoop, State.QueuePaused]
        let deleted = State.applyQueueResolution (ResolveQueuedMessageParams "a" DeleteQueuedMessage mempty) updated
        State.sessionQueue (State.restoreQueueFront [a] deleted) @?= [b],
      testCase "native local queue operations do not load or send anything" $ bounded $ do
        (_, trace) <- withQueueStatePeer Normal [] $ \connection _ -> do
          let a = entry "a" State.QueuePaused "a"
              b = entry "b" State.QueueDeferredDuringCompaction "b"
          Daemon.queueUserMessages connection "one" [a, b]
          Daemon.dequeueQueuedMessage connection "one" (Just State.QueuePaused) >>= (@?= Just a)
          Daemon.restoreQueuedMessages connection "one" [a]
          Daemon.getQueuedMessages connection "one" >>= (@?= [a, b])
          Daemon.markQueuedMessageProcessed connection "one" "a"
          Daemon.restoreQueuedMessages connection "one" [a]
          Daemon.getQueuedMessages connection "one" >>= (@?= [b])
          Daemon.clearQueuedMessages connection "one" Nothing
        map (field "method") trace @?= [String "daemon.authenticate"],
      testCase "native reload restores queues at the accepted receipt and never resends missing work" $ bounded $ do
        let remote = entry "remote" State.QueueDaemonDiscardable "same"
            local = entry "local" State.QueuePaused "local"
            reported = entry "reported" State.QueueDaemonEndOfLoop "reported"
            snapshots = [loadValue [] [State.queueEntryMessage reported] False, loadValue [messageValue "message-remote" "same"] [State.queueEntryMessage reported] False, object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= settings, "queuedMessages" .= False]]
        (_, trace) <- withQueueStatePeer Normal snapshots $ \connection _ -> do
          Daemon.queueUserMessages connection "one" [local, remote]
          void (Daemon.loadSessionInfo connection "one")
          first <- Daemon.getQueuedMessages connection "one"
          map State.queueEntryRequestId first @?= ["local", "reported", "remote"]
          map State.queueEntryKind first @?= [State.QueuePaused, State.QueueDaemonDiscardable, State.QueuePaused]
          void (Daemon.loadSessionInfo connection "one")
          current <- Daemon.getQueuedMessages connection "one"
          map State.queueEntryRequestId current @?= ["local", "reported"]
          try @DroidError (Daemon.loadSessionInfo connection "one") >>= \case Left DroidInvalidEvent -> pure (); _ -> assertFailure "Invalid queue receipt accepted"
          Daemon.getQueuedMessages connection "one" >>= (@?= current)
        map (field "method") trace @?= map String ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.load_session", "daemon.list_terminals", "daemon.load_session"],
      testCase "superseded load receipts cannot replace the newer queue" $ bounded $ do
        let newer = entry "new" State.QueueDaemonDiscardable "new"
            older = entry "old" State.QueueDaemonDiscardable "old"
        void $ withQueueStatePeer ReverseLoads [loadValue [] [State.queueEntryMessage newer] False, loadValue [] [State.queueEntryMessage older] False] $ \connection ready ->
          withAsync (try @Daemon.DaemonError (Daemon.loadSessionInfo connection "one")) $ \oldLoad -> do
            void (takeMVar ready)
            void (Daemon.loadSessionInfo connection "one")
            wait oldLoad >>= \case Left cause -> cause @?= Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Stale load was published"
            Daemon.getQueuedMessages connection "one" >>= (@?= ["new"]) . map State.queueEntryRequestId,
      testCase "an original confirmation racing reprioritization cannot be restored by the resolution reply" $ bounded $ do
        void $ withQueueStatePeer ResolveConfirmed [] $ \connection _ -> do
          let queued = entry "remote" State.QueueDaemonDiscardable "text"
          Daemon.queueUserMessages connection "one" [queued]
          void (Daemon.resolveQueuedUserMessage connection "one" (ResolveQueuedMessageParams "remote" (UpdateQueuedMessage QueueEndOfLoop) mempty))
          Daemon.getQueuedMessages connection "one" >>= (@?= [])
          Daemon.restoreQueuedMessages connection "one" [queued]
          Daemon.getQueuedMessages connection "one" >>= (@?= []),
      testCase "explicit local send preserves the wire input and early confirmation prevents resurrection" $ bounded $ do
        (_, trace) <- withQueueStatePeer Normal [] $ \connection _ -> do
          let queued = withInput (\input -> input {userMessageId = Nothing, userMessageText = "", userMessageSkipAgentLoop = Just False, userMessageAdditionalFields = KeyMap.singleton "future" (Number 100000000000000001)}) (entry "send" State.QueuePaused "")
          Daemon.queueUserMessages connection "one" [queued]
          void (Daemon.sendQueuedUserMessage connection "one" "send")
          Daemon.getQueuedMessages connection "one" >>= (@?= [])
          Daemon.restoreQueuedMessages connection "one" [queued]
          Daemon.getQueuedMessages connection "one" >>= (@?= [])
          try @State.QueueOperationError (Daemon.sendQueuedUserMessage connection "one" "missing") >>= (@?= Left State.QueuedEntryMissing)
        case [frame | frame <- trace, field "method" frame == String "daemon.add_user_message"] of
          [frame] -> do
            field "id" frame @?= String "send"
            let params = asObject (field "params" frame)
            field "text" params @?= String ""
            field "skipAgentLoop" params @?= Bool False
            field "future" params @?= Number 100000000000000001
            case field "messageId" params of String ident | ident /= "send" -> pure (); _ -> assertFailure "Missing separate persisted message identity"
          _ -> assertFailure "Unexpected send count",
      testCase "rejection restores paused input but an authoritative echo wins a later rejection" $
        bounded $
          forM_ [Rejected, ConfirmThenReject] $ \mode -> void $ withQueueStatePeer mode [] $ \connection _ -> do
            let queued = entry "send" State.QueueDeferredAfterInterrupt "text"
            Daemon.queueUserMessages connection "one" [queued]
            result <- try @RpcResultError (Daemon.sendQueuedUserMessage connection "one" "send")
            case result of Left _ -> pure (); Right _ -> assertFailure "Rejection accepted"
            current <- if mode == ConfirmThenReject then waitQueue connection "one" null else Daemon.getQueuedMessages connection "one"
            if mode == ConfirmThenReject then current @?= [] else current @?= [queued {State.queueEntryKind = State.QueuePaused}],
      testCase "a cancelled send cannot overwrite a newer authoritative queue receipt" $ bounded $ do
        let original = entry "send" State.QueuePaused "original"
            remote = State.queueEntryMessage (entry "send" State.QueueDaemonDiscardable "daemon updated")
        void $ withQueueStatePeer Held [loadValue [] [remote] True] $ \connection ready -> do
          Daemon.queueUserMessages connection "one" [original]
          withAsync (Daemon.sendQueuedUserMessage connection "one" "send") $ \pending -> do
            void (takeMVar ready)
            void (Daemon.loadSessionInfo connection "one")
            accepted <- Daemon.getQueuedMessages connection "one"
            map (userMessageText . queuedMessageInput . State.queueEntryMessage) accepted @?= ["daemon updated"]
            cancelWith pending QueueAbort
            waitCatch pending >>= \case Left cause -> fromException cause @?= Just QueueAbort; Right _ -> assertFailure "Cancelled send returned"
            current <- Daemon.getQueuedMessages connection "one"
            map State.queueEntryKind current @?= [State.QueueDaemonDiscardable]
            current @?= accepted,
      testCase "cancellation retains generated message identity and a late echo clears the paused row" $ bounded $ do
        (_, trace) <- withQueueStatePeer Held [] $ \connection ready -> do
          let queued = withInput (\input -> input {userMessageId = Nothing}) (entry "send" State.QueuePaused "text")
          Daemon.queueUserMessages connection "one" [queued]
          withAsync (Daemon.sendQueuedUserMessage connection "one" "send") $ \pending -> do
            frame <- takeMVar ready
            try @State.QueueOperationError (Daemon.sendQueuedUserMessage connection "one" "send") >>= (@?= Left State.QueuedEntryMissing)
            cancelWith pending QueueAbort
            waitCatch pending >>= \case Left cause -> fromException cause @?= Just QueueAbort; Right _ -> assertFailure "Cancelled queue send returned"
            current <- Daemon.getQueuedMessages connection "one"
            map (userMessageId . queuedMessageInput . State.queueEntryMessage) current @?= [case field "messageId" (asObject (field "params" frame)) of String value -> Just value; _ -> Nothing]
            void (Daemon.getProxyToken connection)
            waitQueue connection "one" null >>= (@?= [])
        length [() | frame <- trace, field "method" frame == String "daemon.add_user_message"] @?= 1,
      testCase "daemon-backed entries cannot be resent and accepted resolution updates the original queue ID" $ bounded $ do
        void $ withQueueStatePeer Normal [] $ \connection _ -> do
          let queued = entry "remote" State.QueueDaemonDiscardable "text"
          Daemon.queueUserMessages connection "one" [queued]
          try @State.QueueOperationError (Daemon.sendQueuedUserMessage connection "one" "remote") >>= (@?= Left State.QueuedEntryAlreadyRemote)
          void (Daemon.resolveQueuedUserMessage connection "one" (ResolveQueuedMessageParams "remote" (UpdateQueuedMessage QueueEndOfLoop) mempty))
          Daemon.getQueuedMessages connection "one" >>= (@?= [State.QueueDaemonEndOfLoop]) . map State.queueEntryKind
          void (Daemon.resolveQueuedUserMessage connection "one" (ResolveQueuedMessageParams "remote" DeleteQueuedMessage mempty))
          Daemon.restoreQueuedMessages connection "one" [queued]
          Daemon.getQueuedMessages connection "one" >>= (@?= []),
      testCase "legacy submission tracks explicit remote queue placement without fabricating confirmation" $ bounded $ do
        void $ withQueueStatePeer Legacy [] $ \connection _ -> do
          let input = (defaultUserMessageParams "later") {userMessageQueuePlacement = Just QueueEndOfLoop}
          Daemon.submitUserMessage connection "one" "queued" input >>= (@?= mempty)
          Daemon.getQueuedMessages connection "one" >>= (@?= [State.QueueDaemonEndOfLoop]) . map State.queueEntryKind
          state <- Daemon.getSessionState connection "one"
          State.optimisticSubmissions state @?= [],
      testCase "busy legacy sends queue while prepared or skip-loop sends retain their distinct behavior" $ bounded $ do
        void $ withQueueStatePeer Legacy [loadValue [] [] True] $ \connection _ -> do
          void (Daemon.loadSessionInfo connection "one")
          void (Daemon.submitUserMessage connection "one" "busy" (defaultUserMessageParams "queued"))
          Daemon.getQueuedMessages connection "one" >>= (@?= ["busy"]) . map State.queueEntryRequestId
          Daemon.registerOptimisticSubmission connection "one" "prepared" "placeholder" (defaultUserMessageParams "prepared") Nothing
          void (Daemon.submitUserMessage connection "one" "prepared" (defaultUserMessageParams "prepared"))
          void (Daemon.submitUserMessage connection "one" "skip" ((defaultUserMessageParams "skip") {userMessageSkipAgentLoop = Just True}))
          Daemon.getQueuedMessages connection "one" >>= (@?= ["busy"]) . map State.queueEntryRequestId,
      testCase "rejected resolution leaves the observed queue untouched" $ bounded $ do
        void $ withQueueStatePeer Rejected [] $ \connection _ -> do
          let queued = entry "remote" State.QueueDaemonDiscardable "original"
          Daemon.queueUserMessages connection "one" [queued]
          result <- try @RpcResultError (Daemon.resolveQueuedUserMessage connection "one" (ResolveQueuedMessageParams "remote" DeleteQueuedMessage mempty))
          case result of Left _ -> pure (); Right _ -> assertFailure "Rejected resolution succeeded"
          Daemon.getQueuedMessages connection "one" >>= (@?= [queued]),
      testCase "discard notifications preserve attachments and affect only their named session" $ bounded $ do
        void $ withQueueStatePeer Discard [] $ \connection _ -> do
          let selected = entry "selected" State.QueueDaemonEndOfLoop "restored text"
              discarded = entry "discard" State.QueueDaemonDiscardable "discarded"
              attachment = withInput (\input -> input {userMessageImagePaths = Just ["NOT_READ"]}) (entry "attached" State.QueueDaemonDiscardable "")
          Daemon.queueUserMessages connection "one" [selected, discarded, attachment]
          Daemon.queueUserMessages connection "other" [selected]
          void (Daemon.getProxyToken connection)
          current <- waitQueue connection "one" ((== ["attached"]) . map State.queueEntryRequestId)
          map State.queueEntryKind current @?= [State.QueuePaused]
          Daemon.getQueuedMessages connection "other" >>= (@?= [selected]),
      testCase "successful turn interruption pauses the existing queue through the backend owner" $ bounded $ do
        void $ withQueueStatePeer ActiveTurn [loadValue [] [] False] $ \connection _ ->
          Daemon.withResumedSessionOn connection "one" $ \session -> do
            let discard = entry "discard" State.QueueDaemonDiscardable "text"
                keep = entry "keep" State.QueueDaemonEndOfLoop "text"
            Daemon.queueUserMessages connection "one" [discard, keep]
            streamed <- newEmptyMVar
            withAsync (Daemon.sendTurn session "active" (\_ -> void (tryPutMVar streamed ()))) $ \pending -> do
              takeMVar streamed
              Daemon.interruptSession session
              result <- wait pending
              field "reason" (asObject (toJSON (resultCompletion result))) @?= String "cancelled"
              Daemon.getQueuedMessages connection "one" >>= (@?= [keep {State.queueEntryKind = State.QueuePaused}])
    ]

entry :: Text -> State.QueuedMessageKind -> Text -> State.QueueEntry
entry identifier kind text = State.QueueEntry (QueuedUserMessage identifier ((defaultUserMessageParams text) {userMessageId = Just ("message-" <> identifier)})) kind 0

withInput :: (AddUserMessageParams -> AddUserMessageParams) -> State.QueueEntry -> State.QueueEntry
withInput update queued = queued {State.queueEntryMessage = (State.queueEntryMessage queued) {queuedMessageInput = update (queuedMessageInput (State.queueEntryMessage queued))}}

data QueueAbort = QueueAbort deriving stock (Eq, Show)

instance Exception QueueAbort

data QueuePeerMode = Normal | Rejected | ConfirmThenReject | Held | Legacy | Discard | ActiveTurn | ReverseLoads | ResolveConfirmed deriving stock (Eq)

withQueueStatePeer :: QueuePeerMode -> [Value] -> (Daemon.DaemonConnection -> MVar Object -> IO a) -> IO (a, [Object])
withQueueStatePeer mode snapshots action = do
  loads <- newIORef snapshots
  trace <- newIORef []
  held <- newIORef Nothing
  ready <- newEmptyMVar
  result <- withPeer (\_ connection -> serve loads trace held ready connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
    Daemon.withConnection ((Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}) (`action` ready)
  recorded <- readIORef trace
  pure (result, recorded)
  where
    serve loads trace held ready connection = forever $ do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid queue peer RPC")) pure . eitherDecode
      modifyIORef' trace (<> [frame])
      case field "method" frame of
        String "daemon.authenticate" -> reply connection frame (object ["userId" .= String "user", "orgId" .= String "org"])
        String "daemon.list_terminals" -> reply connection frame (object ["terminals" .= ([] :: [Value])])
        String "daemon.load_session" ->
          if mode == ReverseLoads
            then do
              waiting <- readIORef held
              case waiting of
                Nothing -> writeIORef held (Just frame) >> void (tryPutMVar ready frame)
                Just older -> do
                  nextFixture loads >>= reply connection frame
                  nextFixture loads >>= reply connection older
            else nextFixture loads >>= reply connection frame
        String "daemon.add_user_message" -> do
          writeIORef held (Just frame)
          void (tryPutMVar ready frame)
          case mode of
            Normal -> echo connection frame >> reply connection frame (object ["accepted" .= True])
            Rejected -> reject connection frame
            ConfirmThenReject -> echo connection frame >> reject connection frame
            Held -> reply connection frame (object ["accepted" .= True])
            Legacy -> reply connection frame (object [])
            Discard -> assertFailure "Unexpected send in discard-only fixture"
            ReverseLoads -> assertFailure "Unexpected send during queue reload"
            ResolveConfirmed -> assertFailure "Unexpected send during queue resolution"
            ActiveTurn -> do
              echo connection frame
              reply connection frame (object ["accepted" .= True])
              notify connection (field "sessionId" (asObject (field "params" frame))) (object ["type" .= String "assistant_text_delta", "messageId" .= String "answer", "blockIndex" .= Number 0, "textDelta" .= String "running"])
        String "daemon.resolve_queued_user_message" ->
          if mode == Rejected
            then reject connection frame
            else do
              let params = asObject (field "params" frame)
              when (mode == ResolveConfirmed) $ notify connection (field "sessionId" params) (object ["type" .= String "create_message", "requestId" .= field "requestId" params, "message" .= messageValue "delivered" ""])
              notify connection (field "sessionId" params) (object ["type" .= String "create_message", "requestId" .= field "id" frame, "message" .= messageValue "resolution" ""])
              reply connection frame (object ["accepted" .= True])
        String "daemon.interrupt_session" -> do
          reply connection frame (object [])
          pending <- readIORef held
          forM_ pending $ \original -> do
            let params = asObject (field "params" original)
            notify connection (field "sessionId" params) (object ["type" .= String "agent_turn_completed", "turnId" .= field "messageId" params, "reason" .= String "cancelled", "tokenUsage" .= object ["inputTokens" .= Number 0, "outputTokens" .= Number 0, "cacheCreationTokens" .= Number 0, "cacheReadTokens" .= Number 0, "thinkingTokens" .= Number 0]])
        String "daemon.get_proxy_token" -> do
          if mode == Discard
            then notify connection (String "one") (object ["type" .= String "queued_messages_discarded", "text" .= String "restored text", "requestId" .= String "selected"])
            else do
              pending <- readIORef held
              forM_ pending (echo connection)
          reply connection frame (object ["token" .= String "OFFLINE_TOKEN"])
        _ -> assertFailure "Unexpected queue-state RPC"

nextFixture :: IORef [a] -> IO a
nextFixture ref = atomicModifyIORef' ref (\case [] -> ([], Nothing); value : rest -> (rest, Just value)) >>= maybe (assertFailure "Queue fixture exhausted") pure

waitQueue :: Daemon.DaemonConnection -> Text -> ([State.QueueEntry] -> Bool) -> IO [State.QueueEntry]
waitQueue connection identifier ready = do
  state <- Daemon.getSessionState connection identifier
  if ready (State.sessionQueue state) then pure (State.sessionQueue state) else Daemon.waitSessionStateChange connection identifier state >> waitQueue connection identifier ready

settings :: Value
settings = object ["modelId" .= String "model", "reasoningEffort" .= String "low"]

loadValue :: [Value] -> [QueuedUserMessage] -> Bool -> Value
loadValue history queued busy = object ["session" .= object ["messages" .= history], "settings" .= settings, "queuedMessages" .= queued, "isAgentLoopInProgress" .= busy]

messageValue :: Text -> Text -> Value
messageValue identifier text = object ["id" .= identifier, "role" .= String "user", "content" .= [object ["type" .= String "text", "text" .= text]], "createdAt" .= Number 0, "updatedAt" .= Number 0]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = error "Expected queue peer object"

echo :: WS.Connection -> Object -> IO ()
echo connection frame = do
  let params = asObject (field "params" frame)
  identifier <- case field "messageId" params of String value -> pure value; _ -> assertFailure "Missing persisted message ID"
  text <- case field "text" params of String value -> pure value; _ -> assertFailure "Missing message text"
  notify connection (field "sessionId" params) (object ["type" .= String "create_message", "requestId" .= field "id" frame, "message" .= messageValue identifier text])

notify :: WS.Connection -> Value -> Value -> IO ()
notify connection identifier notification = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= notification]]

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection frame result = sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "result" .= result]

reject :: WS.Connection -> Object -> IO ()
reject connection frame = sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= (-32602 :: Int), "message" .= String "Queue rejected"]]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection pairs = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> pairs)))

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
