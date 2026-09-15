{-# LANGUAGE OverloadedStrings #-}

module StreamSpec (streamTests, legacyStreamTests) where

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (cancelWith, concurrently, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException, bracket, bracket_, finally, fromException, throwIO, toException, try)
import Control.Monad (forM_, replicateM_, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Protocol (RpcChannelError (RpcChannelClosed), closeRpcChannel, requestReply, synchronizeRpcEvents)
import Factory.Droid.Protocol.Dispatch (onRpcError, onRpcNotification, withRpcDispatcher)
import Factory.Droid.Schema.Notifications
import Factory.Droid.Schema.Primitives (mkNonNegativeNumber)
import Factory.Droid.Schema.RPC
import Factory.Droid.Schema.Usage (TokenUsage (..))
import Factory.Droid.Stream
import ProcessSpec (bounded)
import ProtocolSpec qualified as Peer
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

newtype Marker = Marker Int deriving stock (Eq, Show)

instance Exception Marker

newtype CancelMarker = CancelMarker Int deriving stock (Eq, Show)

instance Exception CancelMarker where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

streamTests :: TestTree
streamTests =
  testGroup
    "Standalone stream feeds"
    [ boundedCase "terminal result is available before consumption and preserves exact wire fields" $
        withDroidStream allOptions $ \stream -> do
          atomically (getDroidStreamResult stream) >>= (@?= Nothing)
          atomically (droidStreamCompleted stream) >>= (@?= False)
          feedDroidEvent stream (delta "a" "hello") >>= (@?= True)
          feedDroidEvent stream (TurnCompletedEvent completion) >>= (@?= True)
          snapshot <- atomically (getDroidStreamResult stream) >>= maybe (assertFailure "No completed result") pure
          resultCompletion (streamTurnResult snapshot) @?= completion
          resultText (streamTurnResult snapshot) @?= "hello"
          streamDurationMs snapshot @?= 0
          atomically (droidStreamCompleted stream) >>= (@?= True)
          owner <- myThreadId
          frames <- newIORef []
          result <- consumeDroidStream stream (\frame -> myThreadId >>= (@?= owner) >> modifyIORef' frames (<> [frame]))
          result @?= snapshot
          readIORef frames >>= (@?= [delta "a" "hello", TurnCompletedEvent completion]) . map streamFrameEvent,
      boundedCase "mode filters delivery without changing accumulation" $ do
        (complete, completeFrames) <- collectMode CompleteMessages
        (partial, partialFrames) <- collectMode AllEvents
        complete @?= partial
        map streamFrameEvent completeFrames @?= [TurnCompletedEvent completion]
        map streamFrameEvent partialFrames @?= [delta "a" "hello", TurnCompletedEvent completion],
      boundedCase "all completion reasons remain observable outcomes" $
        forM_ [minBound .. maxBound] $ \reason ->
          withDroidStream allOptions $ \stream -> do
            let terminal = completion {turnCompletionReason = reason}
            void (feedDroidEvent stream (TurnCompletedEvent terminal))
            result <- consumeDroidStream stream (const (pure ()))
            resultCompletion (streamTurnResult result) @?= terminal,
      boundedCase "reported zero wins over elapsed duration and omission uses monotonic time" $
        withDroidStream allOptions $ \stream -> do
          threadDelay 1000
          void (feedDroidEvent stream (TurnCompletedEvent (completion {turnDurationMs = Nothing})))
          result <- consumeDroidStream stream (const (pure ()))
          assertBool "Missing monotonic duration" (streamDurationMs result > 0)
          turnDurationMs (resultCompletion (streamTurnResult result)) @?= Nothing,
      boundedCase "other sessions and turns are filtered before unrelated payload validation" $
        withDroidStream allOptions $ \stream -> do
          feedDroidNotification stream (note "droid.session_notification" (Just "other") (object ["type" .= String "assistant_text_delta"])) >>= (@?= False)
          feedDroidNotification stream (note "droid.session_notification" Nothing (object ["type" .= String "agent_turn_completed", "turnId" .= String "other", "reason" .= Number 4])) >>= (@?= False)
          atomically (droidStreamCompleted stream) >>= (@?= False)
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (toJSON completion)))
          consumeDroidStream stream (const (pure ())) >>= (@?= completion) . resultCompletion . streamTurnResult,
      boundedCase "empty identities remain exact and daemon routing requires a session ID" $ do
        withDroidStream (defaultDroidStreamOptions "" "") $ \stream -> do
          void (feedDroidNotification stream (note "daemon.session_notification" (Just "") (toJSON (completion {completedTurnId = Just ""}))))
          result <- consumeDroidStream stream (const (pure ()))
          resultSessionId (streamTurnResult result) @?= ""
        withDroidStream allOptions $ \stream -> do
          void (feedDroidNotification stream (note "daemon.session_notification" Nothing (toJSON completion)))
          try @DroidStreamError (consumeDroidStream stream (const (pure ()))) >>= (@?= Left DroidStreamInvalidNotification),
      boundedCase "typed missing terminal identity fails and a different identity does not finish" $
        withDroidStream allOptions $ \stream -> do
          feedDroidEvent stream (TurnCompletedEvent (completion {completedTurnId = Just "other"})) >>= (@?= False)
          feedDroidEvent stream (TurnCompletedEvent (completion {completedTurnId = Nothing})) >>= (@?= True)
          try @DroidStreamError (consumeDroidStream stream (const (pure ()))) >>= (@?= Left DroidStreamInvalidNotification),
      boundedCase "completion is final and later malformed frames or failures cannot replace it" $
        withDroidStream allOptions $ \stream -> do
          void (feedDroidEvent stream (TurnCompletedEvent completion))
          feedDroidNotification stream (note "droid.session_notification" Nothing (object ["type" .= String "assistant_text_delta"])) >>= (@?= False)
          feedDroidError stream (toException (Marker 2)) >>= (@?= False)
          feedDroidEvent stream (delta "late" "discard") >>= (@?= False)
          result <- consumeDroidStream stream (const (pure ()))
          resultText (streamTurnResult result) @?= ""
          atomically (getDroidStreamFailure stream) >>= (@?= Nothing) . fmap (fromException @Marker),
      boundedCase "known malformed events fail after the accepted prefix while extensions remain exact" $
        withDroidStream allOptions $ \stream -> do
          let extra = asObject (object ["type" .= String "future", "flag" .= False, "text" .= String "", "number" .= Number 9007199254740993])
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (Object extra)))
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (object ["type" .= String "assistant_text_delta", "textDelta" .= Number 1])))
          frames <- newIORef []
          try @DroidStreamError (consumeDroidStream stream (\frame -> modifyIORef' frames (<> [streamFrameEvent frame]))) >>= (@?= Left DroidStreamInvalidNotification)
          readIORef frames >>= (@?= [OtherNotificationEvent extra]),
      boundedCase "authoritative message corrections and retractions use the shared reducer" $
        withDroidStream allOptions $ \stream -> do
          chunks <- newIORef []
          void (feedDroidEvent stream (delta "a" "draft"))
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (created "a" [object ["type" .= String "text", "text" .= String "final"]])))
          retracted <- decoded (object ["type" .= String "assistant_message_retracted", "messageId" .= String "a"])
          void (feedDroidEvent stream (MessageRetractedEvent retracted))
          void (feedDroidEvent stream (delta "b" "kept"))
          void (feedDroidEvent stream (TurnCompletedEvent completion))
          result <- consumeDroidStream stream (\frame -> modifyIORef' chunks (<> streamFrameText frame))
          readIORef chunks >>= (@?= ["draft", "kept"])
          resultText (streamTurnResult result) @?= "kept",
      boundedCase "tool names are correlated at admission without rewriting raw events" $
        withDroidStream allOptions $ \stream -> do
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (created "a" [object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "lookup", "input" .= object []]])))
          resultEvent <- ToolResultEvent <$> decoded (object ["type" .= String "tool_result", "toolUseId" .= String "tool", "messageId" .= String "a", "content" .= String "done"])
          void (feedDroidEvent stream resultEvent)
          progress <- decoded (object ["type" .= String "tool_progress_update", "toolUseId" .= String "tool", "toolName" .= String "", "update" .= object ["type" .= String "status", "message" .= String "running", "timestamp" .= Number 0]])
          void (feedDroidEvent stream (ToolProgressEvent progress))
          void (feedDroidEvent stream (TurnCompletedEvent completion))
          frames <- newIORef []
          void (consumeDroidStream stream (\frame -> modifyIORef' frames (<> [frame])))
          values <- readIORef frames
          [streamFrameToolName frame | frame <- values, streamFrameEvent frame == resultEvent] @?= [Just "lookup"]
          [streamFrameToolName frame | frame <- values, streamFrameEvent frame == ToolProgressEvent progress] @?= [Just "lookup"]
          progressNotificationToolName progress @?= "",
      boundedCase "wire and locally adapted structured output remain independent" $
        withDroidStream allOptions $ \stream -> do
          adapter <- either throwIO pure (jsonDroidOutput @(Map.Map Text Bool) (asObject (object ["type" .= String "object"])))
          void (feedDroidEvent stream (delta "a" "{\"ok\":false}"))
          void (feedDroidEvent stream (TurnCompletedEvent (completion {turnCompletionReason = TurnCancelled})))
          result <- consumeDroidStream stream (const (pure ()))
          let adapted = adaptDroidStreamOutput adapter result
          outputValue adapted @?= Right (Map.singleton "ok" False)
          turnCompletionReason (resultCompletion (outputTurnResult adapted)) @?= TurnCancelled
          resultStructuredOutput (streamTurnResult result) @?= Nothing,
      boundedCase "explicit output outranks text and decoder failure does not rewrite receipt" $
        withDroidStream allOptions $ \stream -> do
          adapter <- either throwIO pure (jsonDroidOutput @(Map.Map Text Bool) (asObject (object ["type" .= String "object"])))
          let fields = asObject (object ["n" .= Number 9007199254740993])
          void (feedDroidEvent stream (delta "a" "{\"ok\":true}"))
          void (feedDroidEvent stream (StructuredOutputEvent (StructuredOutput "a" (Just fields) mempty)))
          void (feedDroidEvent stream (TurnCompletedEvent completion))
          result <- consumeDroidStream stream (const (pure ()))
          case outputValue (adaptDroidStreamOutput adapter result) of
            Left (DroidOutputInvalid _) -> pure ()
            other -> assertFailure (show other)
          atomically (getDroidStreamResult stream) >>= (@?= Just result)
          resultStructuredOutput (streamTurnResult result) @?= Just fields,
      boundedCase "first originating failure is retained after the accepted prefix" $
        withDroidStream allOptions $ \stream -> do
          void (feedDroidEvent stream (delta "a" "prefix"))
          feedDroidError stream (toException (Marker 7)) >>= (@?= True)
          feedDroidError stream (toException (Marker 8)) >>= (@?= False)
          chunks <- newIORef []
          try @Marker (consumeDroidStream stream (\frame -> modifyIORef' chunks (<> streamFrameText frame))) >>= (@?= Left (Marker 7))
          readIORef chunks >>= (@?= ["prefix"])
          atomically (getDroidStreamFailure stream) >>= (@?= Just (Just (Marker 7))) . fmap fromException
          try @Marker (atomically (getDroidStreamResult stream)) >>= (@?= Left (Marker 7)),
      boundedCase "feeding an async exception raises it unchanged even after completion" $
        withDroidStream allOptions $ \stream -> do
          try @CancelMarker (feedDroidError stream (toException (CancelMarker 9))) >>= (@?= Left (CancelMarker 9))
          atomically (droidStreamCompleted stream) >>= (@?= False)
          void (feedDroidEvent stream (TurnCompletedEvent completion))
          try @CancelMarker (feedDroidError stream (toException (CancelMarker 10))) >>= (@?= Left (CancelMarker 10)),
      boundedCase "callback failure preserves its identity and retires feeding" $
        withDroidStream allOptions $ \stream -> do
          void (feedDroidEvent stream (delta "a" "x"))
          try @Marker (consumeDroidStream stream (const (throwIO (Marker 11)))) >>= (@?= Left (Marker 11))
          feedDroidEvent stream (TurnCompletedEvent completion) >>= (@?= False),
      boundedCase "cancelling a blocked consumer retires feeding without changing async identity" $
        withDroidStream allOptions $ \stream -> do
          started <- newEmptyMVar
          void (feedDroidEvent stream (delta "a" "x"))
          withAsync (consumeDroidStream stream (const (putMVar started ()))) $ \consumer -> do
            takeMVar started
            cancelWith consumer (CancelMarker 12)
            waitCatch consumer >>= expectException (CancelMarker 12)
          feedDroidEvent stream (TurnCompletedEvent completion) >>= (@?= False),
      boundedCase "a second consumer cannot steal frames or close the first" $
        withDroidStream allOptions $ \stream -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          void (feedDroidEvent stream (delta "a" "x"))
          withAsync (consumeDroidStream stream (\frame -> case streamFrameEvent frame of TextDeltaEvent _ -> putMVar started () >> takeMVar release; _ -> pure ())) $ \consumer -> do
            takeMVar started
            (try @DroidStreamError (consumeDroidStream stream (const (pure ()))) >>= (@?= Left DroidStreamAlreadyConsumed)) `finally` putMVar release ()
            void (feedDroidEvent stream (TurnCompletedEvent completion))
            wait consumer >>= (@?= "x") . resultText . streamTurnResult
          try @DroidStreamError (consumeDroidStream stream (const (pure ()))) >>= (@?= Left DroidStreamAlreadyConsumed),
      boundedCase "explicit close wakes a waiter and is idempotent" $
        withDroidStream allOptions $ \stream -> do
          started <- newEmptyMVar
          void (feedDroidEvent stream (delta "a" "x"))
          withAsync (consumeDroidStream stream (const (putMVar started ()))) $ \consumer -> do
            takeMVar started
            closeDroidStream stream
            closeDroidStream stream
            waitCatch consumer >>= expectException DroidStreamClosed
          feedDroidEvent stream (delta "late" "x") >>= (@?= False),
      boundedCase "scope exit closes escaped handles without erasing an observed result" $ do
        escaped <- withDroidStream allOptions $ \stream -> feedDroidEvent stream (TurnCompletedEvent completion) >> pure stream
        atomically (droidStreamCompleted escaped) >>= (@?= True)
        feedDroidEvent escaped (delta "late" "x") >>= (@?= False)
        try @DroidStreamError (consumeDroidStream escaped (const (pure ()))) >>= (@?= Left DroidStreamClosed),
      boundedCase "pure summaries retain absent usage without fabricating a wire receipt" $ do
        let (state, pieces) = stepStream initialStreamState (delta "a" "manual")
            summary = summarizeStream "" TurnCancelled Nothing 0 state
        pieces @?= ["manual"]
        summarySessionId summary @?= ""
        summaryReason summary @?= TurnCancelled
        summaryTokenUsage summary @?= Nothing
        summaryDurationMs summary @?= 0
        summaryText summary @?= "manual"
        summaryEvents summary @?= []
        summaryText (summarizeStream "" TurnCompleted Nothing 0 initialStreamState) @?= "",
      boundedCase "pure usage fallback retains latest zero and explicit completion overrides" $ do
        let prior = turnTokenUsage completion
            zero = TokenUsage 0 0 0 0 0 (Just 0) (KeyMap.singleton "future" (Bool False))
            usage value = UsageEvent (SessionTokenUsageChanged "session" value Nothing Nothing mempty)
            state = foldl' (\current event -> fst (stepStream current event)) initialStreamState [usage prior, usage zero]
        summaryTokenUsage (summarizeStream "session" TurnCompleted Nothing 0 state) @?= Just zero
        summaryTokenUsage (summarizeStream "session" TurnCompleted (Just prior) 0 state) @?= Just prior
        summaryDurationMs (summarizeStream "session" TurnCompleted Nothing 9007199254740993 state) @?= 9007199254740993,
      boundedCase "pure decoding and accumulation share tool identity and complete artifacts" $ do
        events <- either assertFailure pure (decodeNotification "session" "turn" (note "droid.session_notification" Nothing (created "a" [object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "lookup", "input" .= object []]])))
        result <- ToolResultEvent <$> decoded (object ["type" .= String "tool_result", "toolUseId" .= String "tool", "messageId" .= String "a", "content" .= String "done"])
        let state = foldl' (\current event -> fst (stepStream current event)) initialStreamState (events <> [result])
        eventToolName state result @?= Just "lookup"
        summaryEvents (summarizeStream "session" TurnCompleted Nothing 0 state) @?= events <> [result],
      boundedCase "manual summaries reuse output decoding without inventing token usage" $ do
        adapter <- either throwIO pure (jsonDroidOutput @(Map.Map Text Bool) (asObject (object ["type" .= String "object"])))
        let state = fst (stepStream initialStreamState (delta "a" "{\"ok\":false}"))
            summary = summarizeStream "session" TurnCancelled Nothing 0 state
        adaptDroidSummaryOutput adapter summary @?= Right (Map.singleton "ok" False)
        summaryTokenUsage summary @?= Nothing
        summaryStructuredOutput summary @?= Nothing
        case adaptDroidSummaryOutput adapter (summary {summaryStructuredOutput = Just (asObject (object ["n" .= Number 1]))}) of
          Left (DroidOutputInvalid _) -> pure ()
          other -> assertFailure (show other),
      boundedCase "zero and negative budgets run no caller setup" $ do
        calls <- newIORef (0 :: Int)
        forM_ [(0, DroidStreamTimedOut), (-1, DroidStreamInvalidOptions)] $ \(budget, expected) -> do
          try @DroidStreamError (withDroidStream (allOptions {streamTimeoutMicros = Just budget}) (const (modifyIORef' calls (+ 1)))) >>= (@?= Left expected)
        readIORef calls >>= (@?= 0),
      boundedCase "deadline cancels a waiting consumer and releases caller-owned source scope" $ do
        released <- newIORef (0 :: Int)
        escaped <- newIORef Nothing
        try @DroidStreamError
          ( withDroidStream (allOptions {streamTimeoutMicros = Just 10000}) $ \stream -> do
              writeIORef escaped (Just stream)
              bracket_ (pure ()) (modifyIORef' released (+ 1)) (consumeDroidStream stream (const (pure ())))
          )
          >>= (@?= Left DroidStreamTimedOut)
        readIORef released >>= (@?= 1)
        stream <- readIORef escaped >>= maybe (assertFailure "No stream scope entered") pure
        feedDroidEvent stream (TurnCompletedEvent completion) >>= (@?= False)
        atomically (getDroidStreamFailure stream) >>= (@?= Just (Just DroidStreamTimedOut)) . fmap fromException,
      boundedCase "a deadline includes consumer callbacks but preserves a prior terminal observation" $ do
        escaped <- newIORef Nothing
        try @DroidStreamError
          ( withDroidStream (allOptions {streamTimeoutMicros = Just 10000}) $ \stream -> do
              writeIORef escaped (Just stream)
              void (feedDroidEvent stream (TurnCompletedEvent completion))
              consumeDroidStream stream (const (threadDelay 1000000))
          )
          >>= (@?= Left DroidStreamTimedOut)
        readIORef escaped >>= maybe (assertFailure "No stream scope entered") (\stream -> atomically (droidStreamCompleted stream) >>= (@?= True)),
      boundedCase "simultaneous terminal error and close retain one atomic outcome" $
        withDroidStream allOptions $ \stream -> do
          ready <- newTQueueIO
          gate <- newEmptyMVar
          let start :: IO a -> IO a
              start action = atomically (writeTQueue ready ()) >> readMVar gate >> action
          withAsync (concurrently (start (feedDroidEvent stream (TurnCompletedEvent completion))) (concurrently (start (feedDroidError stream (toException (Marker 99)))) (start (closeDroidStream stream)))) $ \workers -> do
            replicateM_ 3 (atomically (readTQueue ready))
            putMVar gate ()
            (completed, (failed, ())) <- wait workers
            assertBool "Two terminal outcomes accepted" (not (completed && failed))
            atomically (droidStreamCompleted stream) >>= (@?= completed)
            failure <- atomically (getDroidStreamFailure stream)
            fmap (fromException @Marker) failure @?= if failed then Just (Just (Marker 99)) else Nothing
            if failed
              then try @Marker (atomically (getDroidStreamResult stream)) >>= (@?= Left (Marker 99))
              else do
                result <- atomically (getDroidStreamResult stream)
                fmap (resultCompletion . streamTurnResult) result @?= if completed then Just completion else Nothing
            feedDroidEvent stream (delta "late" "ignored") >>= (@?= False),
      boundedCase "dispatcher-fed native peer retains its reader and physical ownership" $
        Peer.withMemory $ \channel incoming sent ->
          withRpcDispatcher channel (WithEnvelope Nothing Nothing mempty) $ \dispatcher -> do
            callbacks <- newIORef (0 :: Int)
            withDroidStream allOptions $ \stream ->
              bracket (onRpcNotification dispatcher (\notification -> modifyIORef' callbacks (+ 1) >> void (feedDroidNotification stream notification))) id $ \_ -> do
                Peer.feed incoming (toRpcObject (note "droid.session_notification" Nothing (toJSON (AssistantTextDelta "a" 0 "native" mempty))))
                Peer.feed incoming (toRpcObject (note "droid.session_notification" Nothing (toJSON completion)))
                consumeDroidStream stream (const (pure ())) >>= (@?= "native") . resultText . streamTurnResult
            Peer.feed incoming (toRpcObject (note "droid.session_notification" Nothing (toJSON completion)))
            synchronizeRpcEvents channel
            readIORef callbacks >>= (@?= 2)
            withAsync (requestReply channel (Just 1000000) (Peer.request "still-open")) $ \pending -> do
              atomically (readTQueue sent) >>= (@?= toRpcObject (Peer.request "still-open"))
              Peer.feed incoming (Peer.reply "still-open" Null)
              void (wait pending),
      boundedCase "dispatcher failure reaches a standalone feed through the existing error subscription" $
        Peer.withMemory $ \channel _ _ ->
          withRpcDispatcher channel (WithEnvelope Nothing Nothing mempty) $ \dispatcher ->
            withDroidStream allOptions $ \stream ->
              bracket (onRpcError dispatcher (void . feedDroidError stream . toException)) id $ \_ -> do
                closeRpcChannel channel
                try @RpcChannelError (consumeDroidStream stream (const (pure ()))) >>= (@?= Left RpcChannelClosed),
      boundedCase "stream displays do not print sensitive identities, payloads or results" $
        withDroidStream (defaultDroidStreamOptions "SECRET" "SECRET") $ \stream -> do
          void (feedDroidEvent stream (TurnCompletedEvent (completion {completedTurnId = Just "SECRET"})))
          frames <- newIORef []
          result <- consumeDroidStream stream (\frame -> modifyIORef' frames (<> [frame]))
          values <- readIORef frames
          assertBool "Sensitive Show output" (not ("SECRET" `Text.isInfixOf` Text.pack (show (defaultDroidStreamOptions "SECRET" "SECRET") <> show result <> show values)))
    ]

legacyStreamTests :: TestTree
legacyStreamTests =
  testGroup
    "Legacy idle completion"
    [ boundedCase "every non-idle state followed by Idle completes without invented usage" $
        forM_ (filter (/= WorkingIdle) [minBound .. maxBound]) $ \busy ->
          withDroidLegacyStream "session" Nothing $ \stream -> do
            feedDroidEvent stream (working WorkingIdle) >>= (@?= False)
            feedDroidEvent stream (working WorkingIdle) >>= (@?= False)
            atomically (getDroidStreamResult stream) >>= (@?= Nothing)
            feedDroidEvent stream (working busy) >>= (@?= True)
            feedDroidEvent stream (working WorkingIdle) >>= (@?= True)
            frames <- newIORef []
            consumeDroidStream stream (\frame -> modifyIORef' frames (<> [streamFrameEvent frame])) >>= (@?= DroidIdleCompletion Nothing)
            readIORef frames >>= (@?= [working busy, working WorkingIdle]),
      boundedCase "text without observed activity cannot turn an initial Idle into completion" $ do
        try @DroidStreamError
          ( withDroidLegacyStream "session" (Just 10000) $ \stream -> do
              void (feedDroidEvent stream (delta "a" "text is not activity state"))
              feedDroidEvent stream (working WorkingIdle) >>= (@?= False)
              consumeDroidStream stream (const (pure ()))
          )
          >>= (@?= Left DroidStreamTimedOut),
      boundedCase "explicit completions never terminate legacy iteration or supply its usage" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          forM_ [Nothing, Just "turn", Just "other"] $ \identifier ->
            feedDroidEvent stream (TurnCompletedEvent (completion {completedTurnId = identifier})) >>= (@?= False)
          feedDroidNotification stream (note "droid.session_notification" Nothing (toJSON (completion {completedTurnId = Nothing}))) >>= (@?= False)
          void (feedDroidEvent stream (working WorkingThinking))
          feedDroidEvent stream (TurnCompletedEvent completion) >>= (@?= False)
          atomically (droidStreamCompleted stream) >>= (@?= False)
          void (feedDroidEvent stream (working WorkingIdle))
          consumeDroidStream stream (const (pure ())) >>= (@?= DroidIdleCompletion Nothing),
      boundedCase "normal correlated streams never adopt legacy Idle completion" $
        withDroidStream allOptions $ \stream -> do
          void (feedDroidEvent stream (working WorkingThinking))
          void (feedDroidEvent stream (working WorkingIdle))
          atomically (droidStreamCompleted stream) >>= (@?= False)
          void (feedDroidEvent stream (TurnCompletedEvent completion))
          consumeDroidStream stream (const (pure ())) >>= (@?= completion) . resultCompletion . streamTurnResult,
      boundedCase "latest all-zero usage is retained with exact extensions and credits" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          let zero = TokenUsage 0 0 0 0 0 (Just 0) (KeyMap.singleton "future" (Number 9007199254740993))
          void (feedDroidEvent stream (usageEvent (turnTokenUsage completion)))
          void (feedDroidEvent stream (working WorkingExecutingTool))
          void (feedDroidEvent stream (usageEvent zero))
          void (feedDroidEvent stream (working WorkingIdle))
          consumeDroidStream stream (const (pure ())) >>= (@?= DroidIdleCompletion (Just zero)),
      boundedCase "first observed Idle is terminal and later usage cannot overwrite it" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          void (feedDroidEvent stream (working WorkingCompactingConversation))
          void (feedDroidEvent stream (working WorkingIdle))
          atomically (droidStreamCompleted stream) >>= (@?= True)
          feedDroidEvent stream (usageEvent (turnTokenUsage completion)) >>= (@?= False)
          feedDroidEvent stream (working WorkingThinking) >>= (@?= False)
          feedDroidEvent stream (working WorkingIdle) >>= (@?= False)
          atomically (getDroidStreamResult stream) >>= (@?= Just (DroidIdleCompletion Nothing)),
      boundedCase "foreign daemon activity cannot arm completion for this session" $
        withDroidLegacyStream "" Nothing $ \stream -> do
          feedDroidNotification stream (note "daemon.session_notification" (Just "other") (toJSON (DroidWorkingStateChanged WorkingThinking mempty))) >>= (@?= False)
          feedDroidNotification stream (note "daemon.session_notification" (Just "") (toJSON (DroidWorkingStateChanged WorkingIdle mempty))) >>= (@?= False)
          void (feedDroidNotification stream (note "daemon.session_notification" (Just "") (toJSON (DroidWorkingStateChanged WorkingThinking mempty))))
          void (feedDroidNotification stream (note "daemon.session_notification" (Just "") (toJSON (DroidWorkingStateChanged WorkingIdle mempty))))
          consumeDroidStream stream (const (pure ())) >>= (@?= DroidIdleCompletion Nothing),
      boundedCase "legacy filtering retains tool names without consuming unmapped updates" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (created "a" [object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "lookup", "input" .= object []]])))
          change <- decoded (object ["type" .= String "tool_call", "toolUse" .= object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "ignored", "input" .= object []]])
          feedDroidEvent stream (ToolCallDeltaEvent change) >>= (@?= False)
          feedDroidEvent stream (OtherNotificationEvent (KeyMap.singleton "type" (String "future"))) >>= (@?= False)
          result <- ToolResultEvent <$> decoded (object ["type" .= String "tool_result", "toolUseId" .= String "tool", "messageId" .= String "a", "content" .= String "done"])
          void (feedDroidEvent stream result)
          progress <- decoded (object ["type" .= String "tool_progress_update", "toolUseId" .= String "tool", "toolName" .= String "", "update" .= object ["type" .= String "status"]])
          void (feedDroidEvent stream (ToolProgressEvent progress))
          void (feedDroidEvent stream (working WorkingThinking))
          void (feedDroidEvent stream (working WorkingIdle))
          frames <- newIORef []
          void (consumeDroidStream stream (\frame -> modifyIORef' frames (<> [frame])))
          values <- readIORef frames
          [streamFrameToolName frame | frame <- values, streamFrameEvent frame == result] @?= [Just "lookup"]
          [streamFrameToolName frame | frame <- values, streamFrameEvent frame == ToolProgressEvent progress] @?= [Just "lookup"]
          length values @?= 5,
      boundedCase "malformed working state fails rather than creating activity or silently hanging" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          void (feedDroidEvent stream (delta "a" "prefix"))
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (object ["type" .= String "droid_working_state_changed", "newState" .= String "not-a-state"])))
          chunks <- newIORef []
          try @DroidStreamError (consumeDroidStream stream (\frame -> modifyIORef' chunks (<> streamFrameText frame))) >>= (@?= Left DroidStreamInvalidNotification)
          readIORef chunks >>= (@?= ["prefix"]),
      boundedCase "fatal source failure retains the accepted prefix and original exception" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          void (feedDroidEvent stream (working WorkingThinking))
          void (feedDroidError stream (toException (Marker 31)))
          feedDroidEvent stream (working WorkingIdle) >>= (@?= False)
          frames <- newIORef []
          try @Marker (consumeDroidStream stream (\frame -> modifyIORef' frames (<> [streamFrameEvent frame]))) >>= (@?= Left (Marker 31))
          readIORef frames >>= (@?= [working WorkingThinking]),
      boundedCase "error notifications remain events and do not assert a failed or successful turn" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          err <- decoded (object ["type" .= String "error", "message" .= String "reported", "errorType" .= NotifyError, "timestamp" .= String "2026-09-11T00:00:00Z"])
          void (feedDroidEvent stream (ErrorEvent err))
          atomically (droidStreamCompleted stream) >>= (@?= False)
          void (feedDroidEvent stream (working WorkingThinking))
          void (feedDroidEvent stream (working WorkingIdle))
          frames <- newIORef []
          consumeDroidStream stream (\frame -> modifyIORef' frames (<> [streamFrameEvent frame])) >>= (@?= DroidIdleCompletion Nothing)
          readIORef frames >>= (@?= [ErrorEvent err, working WorkingThinking, working WorkingIdle]),
      boundedCase "legacy cancellation preserves async identity and releases the source bracket once" $ do
        released <- newIORef (0 :: Int)
        withDroidLegacyStream "session" Nothing $ \stream -> do
          started <- newEmptyMVar
          void (feedDroidEvent stream (working WorkingThinking))
          withAsync (bracket_ (pure ()) (modifyIORef' released (+ 1)) (consumeDroidStream stream (const (putMVar started ())))) $ \consumer -> do
            takeMVar started
            cancelWith consumer (CancelMarker 32)
            waitCatch consumer >>= expectException (CancelMarker 32)
          readIORef released >>= (@?= 1)
          feedDroidEvent stream (working WorkingIdle) >>= (@?= False),
      boundedCase "legacy deadlines retain failure inspection after cleanup" $ do
        escaped <- newIORef Nothing
        try @DroidStreamError
          ( withDroidLegacyStream "session" (Just 10000) $ \stream -> do
              writeIORef escaped (Just stream)
              consumeDroidStream stream (const (pure ()))
          )
          >>= (@?= Left DroidStreamTimedOut)
        stream <- readIORef escaped >>= maybe (assertFailure "No stream scope") pure
        atomically (getDroidStreamFailure stream) >>= (@?= Just (Just DroidStreamTimedOut)) . fmap fromException
        feedDroidEvent stream (working WorkingIdle) >>= (@?= False),
      boundedCase "legacy zero and negative budgets enter no source setup" $ do
        calls <- newIORef (0 :: Int)
        forM_ [(0, DroidStreamTimedOut), (-1, DroidStreamInvalidOptions)] $ \(budget, expected) -> do
          try @DroidStreamError (withDroidLegacyStream "session" (Just budget) (const (modifyIORef' calls (+ 1)))) >>= (@?= Left expected)
        readIORef calls >>= (@?= 0),
      boundedCase "legacy mixed families and multiple tool results retain exact order" $
        withDroidLegacyStream "session" Nothing $ \stream -> do
          let tool identifier name = object ["type" .= String "tool_use", "id" .= String identifier, "name" .= String name, "input" .= object []]
              firstValue = tool "one" "first"
              secondValue = tool "two" "second"
              thinking = ThinkingDeltaEvent (ThinkingTextDelta "a" 0 "reasoning" mempty)
              usage = usageEvent (turnTokenUsage completion)
          first <- ToolCallEvent <$> decoded firstValue
          second <- ToolCallEvent <$> decoded secondValue
          result <- ToolResultEvent <$> decoded (object ["type" .= String "tool_result", "toolUseId" .= String "two", "messageId" .= String "a", "content" .= String "done"])
          progress <- ToolProgressEvent <$> decoded (object ["type" .= String "tool_progress_update", "toolUseId" .= String "one", "toolName" .= String "", "update" .= object ["type" .= String "status"]])
          void (feedDroidEvent stream (working WorkingThinking))
          void (feedDroidEvent stream thinking)
          void (feedDroidNotification stream (note "droid.session_notification" Nothing (created "a" [firstValue, secondValue])))
          forM_ [delta "a" "text", result, progress, usage, working WorkingIdle] (feedDroidEvent stream)
          frames <- newIORef []
          consumeDroidStream stream (\frame -> modifyIORef' frames (<> [frame])) >>= (@?= DroidIdleCompletion (Just (turnTokenUsage completion)))
          values <- readIORef frames
          map streamFrameEvent values @?= [working WorkingThinking, thinking, first, second, delta "a" "text", result, progress, usage, working WorkingIdle]
          [name | frame <- values, Just name <- [streamFrameToolName frame]] @?= ["first", "second", "second", "first"]
          concatMap streamFrameText values @?= ["text"],
      boundedCase "dispatcher-fed legacy response owns only its subscription" $
        Peer.withMemory $ \channel incoming _ ->
          withRpcDispatcher channel (WithEnvelope Nothing Nothing mempty) $ \dispatcher -> do
            callbacks <- newIORef (0 :: Int)
            withDroidLegacyStream "session" Nothing $ \stream ->
              bracket (onRpcNotification dispatcher (\notification -> modifyIORef' callbacks (+ 1) >> void (feedDroidNotification stream notification))) id $ \_ -> do
                forM_ [WorkingIdle, WorkingThinking, WorkingIdle] $ \state ->
                  Peer.feed incoming (toRpcObject (note "daemon.session_notification" (Just "session") (toJSON (DroidWorkingStateChanged state mempty))))
                consumeDroidStream stream (const (pure ())) >>= (@?= DroidIdleCompletion Nothing)
            Peer.feed incoming (toRpcObject (note "daemon.session_notification" (Just "session") (toJSON (DroidWorkingStateChanged WorkingThinking mempty))))
            synchronizeRpcEvents channel
            readIORef callbacks >>= (@?= 3)
    ]

working :: DroidWorkingState -> DroidEvent
working state = WorkingStateEvent (DroidWorkingStateChanged state mempty)

usageEvent :: TokenUsage -> DroidEvent
usageEvent value = UsageEvent (SessionTokenUsageChanged "session" value Nothing Nothing mempty)

boundedCase :: String -> IO () -> TestTree
boundedCase name = testCase name . bounded

allOptions :: DroidStreamOptions
allOptions = (defaultDroidStreamOptions "session" "turn") {streamMode = AllEvents}

completion :: AgentTurnCompleted
completion = AgentTurnCompleted TurnCompleted (TokenUsage 9007199254740993 0 0 0 0 (Just 0) (KeyMap.singleton "future" (Bool False))) (Just "turn") Nothing Nothing Nothing (mkNonNegativeNumber 0) (KeyMap.singleton "future" (String ""))

delta :: Text -> Text -> DroidEvent
delta identifier text = TextDeltaEvent (AssistantTextDelta identifier 0 text mempty)

collectMode :: DroidStreamMode -> IO (DroidStreamResult, [DroidStreamFrame])
collectMode mode = withDroidStream (allOptions {streamMode = mode}) $ \stream -> do
  void (feedDroidEvent stream (delta "a" "hello"))
  void (feedDroidEvent stream (TurnCompletedEvent completion))
  frames <- newIORef []
  result <- consumeDroidStream stream (\frame -> modifyIORef' frames (<> [frame]))
  (result,) <$> readIORef frames

note :: Text -> Maybe Text -> Value -> JsonRpcBaseNotification
note method identifier payload = WithEnvelope Nothing Nothing (BaseNotification method (Just (object (["notification" .= payload] <> maybe [] (\value -> ["sessionId" .= value]) identifier))) mempty)

created :: Text -> [Value] -> Value
created identifier content = object ["type" .= String "create_message", "message" .= object ["id" .= identifier, "role" .= String "assistant", "content" .= content, "createdAt" .= Number 1, "updatedAt" .= Number 1]]

decoded :: (FromJSON a) => Value -> IO a
decoded value = case fromJSON value of
  Success result -> pure result
  Error message -> assertFailure message

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = error "Expected a fixture object"

expectException :: (Eq e, Show e, Exception e) => e -> Either SomeException a -> IO ()
expectException expected = \case
  Left cause -> fromException cause @?= Just expected
  Right _ -> assertFailure "Expected an exception"
