{-# LANGUAGE OverloadedStrings #-}

module ProtocolSpec (protocolTests, PeerFailure (..), withMemory, feed, request, reply, message) where

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (Exception, bracket_, fromException, throwIO, try)
import Control.Monad (forM_, replicateM_, void)
import Data.Aeson (Object, Value (..), toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Factory.Droid.Protocol
import Factory.Droid.Schema.RPC
import Factory.Droid.Transport.Process (receiveObject, sendObject)
import GHC.Conc (BlockReason (BlockedOnMVar), ThreadStatus (ThreadBlocked, ThreadDied, ThreadFinished), threadStatus)
import ProcessSpec (bounded, withPeer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

protocolTests :: TestTree
protocolTests =
  testGroup
    "Scoped RPC channel"
    [ testCase "native peer interleaves notifications, server requests and reversed replies" $
        bounded $
          withPeer "rpc" 4096 $ \process ->
            withRpcChannel (sendObject process) (receiveObject process) $ \channel ->
              withAsync (requestReply channel Nothing (request "a")) $ \first ->
                withAsync (requestResult channel Nothing (request "b") :: IO Text) $ \second -> do
                  wait first >>= expectResult (String "a")
                  wait second >>= (@?= "b")
                  note <- receiveRpcEvent channel
                  envelopeBody note @?= NotificationBody (BaseNotification "notice" (Just (String "queued before replies")) mempty)
                  question <- receiveRpcEvent channel
                  envelopeBody question @?= RequestBody (BaseRequest "server-id" "server.question" (Just (String "fixture question")) mempty)
                  sendRpcMessage channel (message (ResponseBody (BaseResponseGeneric (Just "server-id") (Just (String "fixture answer")) Nothing mempty)))
                  answered <- receiveRpcEvent channel
                  envelopeBody answered @?= NotificationBody (BaseNotification "answered" Nothing mempty),
      testCase "concurrent duplicate IDs cannot overwrite the pending request" $ bounded $ withMemory $ \channel incoming sent ->
        withAsync (requestReply channel Nothing (request "same")) $ \first -> do
          atomically (readTQueue sent) >>= (@?= toRpcObject (request "same"))
          expectError RpcDuplicateRequestId (requestReply channel Nothing (request "same"))
          atomically (tryReadTQueue sent) >>= (@?= Nothing)
          feed incoming (reply "same" (Number 1))
          wait first >>= expectResult (Number 1),
      testCase "late, unknown and duplicate replies do not become events or settle other requests" $ bounded $ withMemory $ \channel incoming sent -> do
        withAsync (requestReply channel Nothing (request "a")) $ \first -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "unknown" Null)
          feed incoming (reply "a" (Number 1))
          feed incoming (reply "a" (Number 99))
          wait first >>= expectResult (Number 1)
        withAsync (requestReply channel Nothing (request "b")) $ \second -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "a" (Number 98))
          feed incoming (reply "b" (Number 2))
          feed incoming (toRpcObject barrier)
          wait second >>= expectResult (Number 2)
          receiveRpcEvent channel >>= (@?= barrier),
      testCase "timed observations retain the accepted receive timestamp across delayed consumption" $ bounded $ withMemory $ \channel incoming sent -> do
        observed <- newTQueueIO
        before <- getCurrentTime
        withAsync (requestReplyObservedAt channel Nothing (request "timed") (curry (writeTQueue observed))) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "timed" (Number 1))
          result <- wait worker
          acceptedBefore <- getCurrentTime
          threadDelay 20000
          feed incoming (reply "timed" (Number 99))
          feed incoming (toRpcObject barrier)
          receiveRpcEvent channel >>= (@?= barrier)
          (received, actual) <- atomically (readTQueue observed)
          actual @?= result
          assertBool "timestamp was sampled before correlation, not deferred observation" (received >= before && received <= acceptedBefore)
          atomically (tryReadTQueue observed) >>= (@?= Nothing),
      testCase "reply observations keep receive order without delaying correlation" $ bounded $ withMemory $ \channel incoming sent -> do
        observed <- newTQueueIO
        withAsync (requestReplyObserved channel Nothing (request "snapshot") (writeTQueue observed)) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (toRpcObject barrier)
          feed incoming (reply "snapshot" (String "baseline"))
          feed incoming (toRpcObject barrier)
          result <- wait worker
          atomically (tryReadTQueue observed) >>= (@?= Nothing)
          receiveRpcEvent channel >>= (@?= barrier)
          atomically (tryReadTQueue observed) >>= (@?= Nothing)
          receiveRpcEvent channel >>= (@?= barrier)
          atomically (readTQueue observed) >>= (@?= result),
      testCase "only first accepted reply is observed even while request remains pending" $ bounded $ do
        incoming <- newTQueueIO
        sent <- newEmptyMVar
        release <- newEmptyMVar
        observed <- newTQueueIO
        withRpcChannel (\_ -> putMVar sent () >> takeMVar release) (atomically (readTQueue incoming)) $ \channel ->
          withAsync (requestReplyObserved channel Nothing (request "snapshot") (writeTQueue observed)) $ \worker -> do
            takeMVar sent
            forM_ [reply "snapshot" (Number 1), reply "snapshot" (Number 2), reply "unknown" Null, toRpcObject barrier] (atomically . writeTQueue incoming)
            receiveRpcEvent channel >>= (@?= barrier)
            atomically (readTQueue observed) >>= expectResult (Number 1)
            atomically (tryReadTQueue observed) >>= (@?= Nothing)
            putMVar release ()
            wait worker >>= expectResult (Number 1),
      testCase "observations commit without a following event and are not replayed after consumer cancellation" $ bounded $ withMemory $ \channel incoming sent -> do
        observed <- newTQueueIO
        withAsync (requestReplyObserved channel Nothing (request "snapshot") (writeTQueue observed)) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "snapshot" Null)
          result <- wait worker
          withAsync (receiveRpcEvent channel) $ \consumer -> do
            atomically (readTQueue observed) >>= (@?= result)
            cancel consumer
          feed incoming (toRpcObject barrier)
          receiveRpcEvent channel >>= (@?= barrier)
          atomically (tryReadTQueue observed) >>= (@?= Nothing),
      testCase "accepted observation survives caller cancellation and terminal read failure" $ bounded $ withMemory $ \channel incoming sent -> do
        observed <- newTQueueIO
        returned <- newEmptyMVar
        blocked <- newEmptyMVar
        withAsync (requestReplyObserved channel Nothing (request "snapshot") (writeTQueue observed) >>= putMVar returned >> takeMVar blocked) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "snapshot" (Number 1))
          result <- takeMVar returned
          cancel worker
          atomically (writeTQueue incoming (Left PeerEnded))
          expectError RpcChannelReadFailure (receiveRpcEvent channel)
          atomically (readTQueue observed) >>= (@?= result),
      testCase "accepted observation survives cancellation before pending waiter returns" $ bounded $ do
        incoming <- newTQueueIO
        readStarts <- newTQueueIO
        observed <- newTQueueIO
        writing <- newEmptyMVar
        blocked <- newEmptyMVar
        let receive = atomically (writeTQueue readStarts ()) >> atomically (readTQueue incoming)
        withRpcChannel (\_ -> putMVar writing () >> takeMVar blocked) receive $ \channel ->
          withAsync (requestReplyObserved channel Nothing (request "snapshot") (writeTQueue observed)) $ \worker -> do
            takeMVar writing
            void (atomically (readTQueue readStarts))
            atomically (writeTQueue incoming (reply "snapshot" (Number 1)))
            -- Reader re-entry proves response acceptance, while sending still blocks the waiter.
            void (atomically (readTQueue readStarts))
            cancel worker
            expectError RpcChannelWriteFailure (receiveRpcEvent channel)
            atomically (readTQueue observed) >>= expectResult (Number 1)
            atomically (tryReadTQueue observed) >>= (@?= Nothing),
      testCase "cancelled pending requests and malformed replies produce no observation" $ bounded $ withMemory $ \channel incoming sent -> do
        observed <- newTQueueIO
        withAsync (requestReplyObserved channel Nothing (request "cancelled") (writeTQueue observed)) $ \worker -> do
          void (atomically (readTQueue sent))
          sendRpcMessage channel barrier
          void (atomically (readTQueue sent))
          cancel worker
        feed incoming (reply "cancelled" Null)
        withAsync (try @RpcChannelError (requestReplyObserved channel Nothing (request "bad") (writeTQueue observed))) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (KeyMap.delete "result" (reply "bad" Null))
          wait worker >>= (@?= Left RpcMalformedResponse)
        feed incoming (toRpcObject barrier)
        receiveRpcEvent channel >>= (@?= barrier)
        atomically (tryReadTQueue observed) >>= (@?= Nothing),
      testCase "well-formed remote failures are observed but channel failures are not synthesized" $ bounded $ withMemory $ \channel incoming sent -> do
        observed <- newTQueueIO
        withAsync (requestReplyObserved channel Nothing (request "error") (writeTQueue observed)) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (KeyMap.insert "error" (toJSON remoteError) (reply "error" Null))
          result <- wait worker
          feed incoming (toRpcObject barrier)
          receiveRpcEvent channel >>= (@?= barrier)
          atomically (readTQueue observed) >>= (@?= result)
        withAsync (try @RpcChannelError (requestReplyObserved channel Nothing (request "closed") (writeTQueue observed))) $ \worker -> do
          void (atomically (readTQueue sent))
          atomically (writeTQueue incoming (Left PeerEnded))
          wait worker >>= (@?= Left RpcChannelReadFailure)
        expectError RpcChannelReadFailure (receiveRpcEvent channel)
        atomically (tryReadTQueue observed) >>= (@?= Nothing),
      testCase "remote errors and both payloads retain the full response rather than imply success" $ bounded $ withMemory $ \channel incoming sent ->
        withAsync (requestReply channel Nothing (request "a")) $ \worker -> do
          void (atomically (readTQueue sent))
          let fields = KeyMap.insert "error" (toJSON remoteError) (reply "a" Null) <> KeyMap.singleton "future" (Bool True)
          feed incoming fields
          response <- wait worker
          envelopeBody response @?= BaseFailure (BaseResponseFailure (Just "a") remoteError (Just Null) (KeyMap.singleton "future" (Bool True)))
          toJSON response @?= Object fields,
      testCase "missing result and error reject only the identifiable pending response" $ bounded $ withMemory $ \channel incoming sent -> do
        withAsync (try @RpcChannelError (requestReply channel Nothing (request "a"))) $ \first -> do
          void (atomically (readTQueue sent))
          feed incoming (KeyMap.delete "result" (reply "a" Null))
          wait first >>= (@?= Left RpcMalformedResponse)
        withAsync (requestReply channel Nothing (request "b")) $ \second -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "b" Null)
          wait second >>= expectResult Null,
      testCase "invalid envelope is terminal and wakes pending and subsequent requests" $ bounded $ withMemory $ \channel incoming sent ->
        withAsync (try @RpcChannelError (requestReply channel Nothing (request "a"))) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (KeyMap.insert "jsonrpc" (String "wrong") (reply "a" Null))
          wait worker >>= (@?= Left RpcMalformedMessage)
          expectError RpcMalformedMessage (requestReply channel Nothing (request "b"))
          expectError RpcMalformedMessage (receiveRpcEvent channel),
      testCase "reader stop racing send startup preserves the terminal cause" $
        bounded $
          replicateM_ 500 $
            withRpcChannel (\_ -> pure ()) (throwIO PeerEnded) $ \channel ->
              expectError RpcChannelReadFailure (requestReply channel Nothing (request "a")),
      testCase "frames received after write failure cannot enter the event queue" $ bounded $ do
        started <- newEmptyMVar
        received <- newEmptyMVar
        input <- newEmptyMVar
        let receive = do
              myThreadId >>= putMVar started
              fields <- takeMVar input
              putMVar received ()
              pure fields
        withRpcChannel (\_ -> throwIO PeerSendFailed) receive $ \channel -> do
          reader <- takeMVar started
          try @PeerFailure (sendRpcMessage channel barrier) >>= (@?= Left PeerSendFailed)
          putMVar input (toRpcObject barrier)
          takeMVar received
          -- Wait for dispatch to finish: either reader exited or it is awaiting another frame.
          let dispatched =
                threadStatus reader >>= \case
                  ThreadFinished -> pure ()
                  ThreadDied -> pure ()
                  ThreadBlocked BlockedOnMVar -> pure ()
                  _ -> threadDelay 1000 >> dispatched
          dispatched
          expectError RpcChannelWriteFailure (receiveRpcEvent channel),
      testCase "EOF wakes all pending requests and remains sticky" $ bounded $ withMemory $ \channel incoming sent ->
        withAsync (try @RpcChannelError (requestReply channel Nothing (request "a"))) $ \first ->
          withAsync (try @RpcChannelError (requestReply channel Nothing (request "b"))) $ \second -> do
            replicateM_ 2 (void (atomically (readTQueue sent)))
            atomically (writeTQueue incoming (Left PeerEnded))
            wait first >>= (@?= Left RpcChannelReadFailure)
            wait second >>= (@?= Left RpcChannelReadFailure)
            cause <- atomically (rpcChannelFailureCause channel)
            (cause >>= fromException) @?= Just PeerEnded
            closeRpcChannel channel
            retained <- atomically (rpcChannelFailureCause channel)
            (retained >>= fromException) @?= Just PeerEnded
            expectError RpcChannelReadFailure (requestReply channel Nothing (request "c"))
            atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "completed responses survive immediately following EOF" $ bounded $ replicateM_ 50 $ withMemory $ \channel incoming sent ->
        withAsync (requestReply channel Nothing (request "a")) $ \worker -> do
          void (atomically (readTQueue sent))
          atomically $ do
            writeTQueue incoming (Right (reply "a" (Number 7)))
            writeTQueue incoming (Left PeerEnded)
          wait worker >>= expectResult (Number 7),
      testCase "queued notifications and null-ID errors drain before terminal failure" $ bounded $ withMemory $ \channel incoming _ -> do
        let diagnostic = message (ResponseBody (BaseResponseGeneric Nothing Nothing (Just remoteError) mempty))
        atomically $ do
          writeTQueue incoming (Right (toRpcObject barrier))
          writeTQueue incoming (Right (toRpcObject diagnostic))
          writeTQueue incoming (Left PeerEnded)
        receiveRpcEvent channel >>= (@?= barrier)
        receiveRpcEvent channel >>= (@?= diagnostic)
        expectError RpcChannelReadFailure (receiveRpcEvent channel),
      testCase "negative and zero deadlines send nothing" $ bounded $ withMemory $ \channel _ sent -> do
        expectError RpcInvalidTimeout (requestReply channel (Just (-1)) (request "negative"))
        expectError RpcRequestTimedOut (requestReply channel (Just 0) (request "zero"))
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "reply timeout removes one request while the channel remains usable" $ bounded $ withMemory $ \channel incoming sent -> do
        expectError RpcRequestTimedOut (requestReply channel (Just 50000) (request "timeout"))
        void (atomically (readTQueue sent))
        withAsync (requestReply channel Nothing (request "next")) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "timeout" Null)
          feed incoming (reply "next" (Number 2))
          wait worker >>= expectResult (Number 2),
      testCase "cancellation during response wait preserves identity and channel usability" $ bounded $ withMemory $ \channel incoming sent -> do
        withAsync (requestReply channel Nothing (request "cancelled")) $ \worker -> do
          void (atomically (readTQueue sent))
          -- A later serialized send proves the first write left its critical section.
          sendRpcMessage channel barrier
          void (atomically (readTQueue sent))
          cancel worker
          waitCatch worker >>= \case
            Left err -> fromException err @?= Just AsyncCancelled
            Right _ -> assertFailure "Cancelled request succeeded"
        withAsync (requestReply channel Nothing (request "next")) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "next" Null)
          wait worker >>= expectResult Null,
      testCase "deadline while queued behind another writer does not poison that writer" $ bounded $ do
        writing <- newEmptyMVar
        release <- newEmptyMVar
        input <- newEmptyMVar
        withRpcChannel (\value -> putMVar writing value >> takeMVar release) (takeMVar input) $ \channel ->
          withAsync (sendRpcMessage channel barrier) $ \holder -> do
            takeMVar writing >>= (@?= toRpcObject barrier)
            expectError RpcRequestTimedOut (requestReply channel (Just 50000) (request "queued"))
            putMVar release ()
            wait holder
            withAsync (requestReply channel Nothing (request "next")) $ \worker -> do
              takeMVar writing >>= (@?= toRpcObject (request "next"))
              putMVar release ()
              putMVar input (reply "next" Null)
              wait worker >>= expectResult Null,
      testCase "deadline covers a blocked send and poisons the potentially partial channel" $ bounded $ do
        writing <- newEmptyMVar
        blocked <- newEmptyMVar
        input <- newEmptyMVar
        withRpcChannel (\_ -> putMVar writing () >> takeMVar blocked) (takeMVar input) $ \channel -> do
          withAsync (try @RpcChannelError (requestReply channel (Just 50000) (request "a"))) $ \worker -> do
            takeMVar writing
            wait worker >>= (@?= Left RpcRequestTimedOut)
          expectError RpcChannelWriteFailure (requestReply channel Nothing (request "b")),
      testCase "read failure interrupts a blocked send even without a deadline" $ bounded $ do
        writing <- newEmptyMVar
        blocked <- newEmptyMVar
        ended <- newEmptyMVar
        withRpcChannel (\_ -> putMVar writing () >> takeMVar blocked) (takeMVar ended >> throwIO PeerEnded) $ \channel ->
          withAsync (try @RpcChannelError (requestReply channel Nothing (request "a"))) $ \worker -> do
            takeMVar writing
            putMVar ended ()
            wait worker >>= (@?= Left RpcChannelReadFailure),
      testCase "send failure is terminal without exposing its exception text to request callers" $ bounded $ do
        input <- newEmptyMVar
        withRpcChannel (\_ -> throwIO PeerSendFailed) (takeMVar input) $ \channel -> do
          expectError RpcChannelWriteFailure (requestReply channel Nothing (request "a"))
          cause <- atomically (rpcChannelFailureCause channel)
          (cause >>= fromException) @?= Just PeerSendFailed
          closeRpcChannel channel
          retained <- atomically (rpcChannelFailureCause channel)
          (retained >>= fromException) @?= Just PeerSendFailed
          expectError RpcChannelWriteFailure (requestReply channel Nothing (request "b")),
      testCase "raw sends preserve their originating exception" $ bounded $ do
        input <- newEmptyMVar
        withRpcChannel (\_ -> throwIO PeerSendFailed) (takeMVar input) $ \channel -> do
          try @PeerFailure (sendRpcMessage channel barrier) >>= (@?= Left PeerSendFailed)
          expectError RpcChannelWriteFailure (receiveRpcEvent channel),
      testCase "scope failure retains callback identity and joins the reader" $ bounded $ do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        input <- newEmptyMVar
        let receive = bracket_ (putMVar started ()) (putMVar stopped ()) (takeMVar input)
        result <- try @PeerFailure (withRpcChannel (\_ -> pure ()) receive (\_ -> takeMVar started >> throwIO CallbackFailed) :: IO ())
        result @?= Left CallbackFailed
        tryTakeMVar stopped >>= (@?= Just ()),
      testCase "escaped handles reject sends after scope closure" $ bounded $ do
        input <- newEmptyMVar
        channel <- withRpcChannel (\_ -> assertFailure "Post-close send reached transport") (takeMVar input) pure
        expectError RpcChannelClosed (requestReply channel Nothing (request "a"))
        expectError RpcChannelClosed (sendRpcMessage channel barrier)
        expectError RpcChannelClosed (receiveRpcEvent channel),
      testCase "typed result decoding distinguishes missing, null and invalid values" $ do
        decodeRpcResult @Value (rawSuccess Nothing Nothing) @?= Left RpcMissingResult
        decodeRpcResult @Value (rawSuccess (Just Null) Nothing) @?= Right Null
        decodeRpcResult @CommandAck (rawSuccess (Just Null) Nothing) @?= Left RpcInvalidResult
        let ack = CommandAck (KeyMap.singleton "future" (Number 3))
        decodeRpcResult @CommandAck (rawSuccess (Just (toJSON ack)) Nothing) @?= Right ack,
      testCase "remote errors take precedence over every result shape" $
        forM_ [Nothing, Just Null, Just (toJSON (CommandAck mempty)), Just (Bool False)] $ \result -> do
          decodeRpcResult @CommandAck (rawSuccess result (Just remoteError)) @?= Left (RpcRemoteFailure remoteError)
          let failure = WithEnvelope Nothing Nothing (BaseFailure (BaseResponseFailure Nothing remoteError result mempty))
          decodeRpcResult @CommandAck failure @?= Left (RpcRemoteFailure remoteError),
      testCase "typed result rejection does not close the channel" $ bounded $ withMemory $ \channel incoming sent -> do
        withAsync (try @RpcResultError (requestResult channel Nothing (request "bad") :: IO CommandAck)) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "bad" Null)
          wait worker >>= (@?= Left RpcInvalidResult)
        withAsync (requestResult channel Nothing (request "good") :: IO CommandAck) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "good" (toJSON (CommandAck mempty)))
          wait worker >>= (@?= CommandAck mempty),
      testCase "typed request throws a remote failure while preserving explicit details" $ bounded $ withMemory $ \channel incoming sent ->
        withAsync (try @RpcResultError (requestResult channel Nothing (request "error") :: IO CommandAck)) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (KeyMap.insert "error" (toJSON remoteError) (reply "error" (toJSON (CommandAck mempty))))
          wait worker >>= (@?= Left (RpcRemoteFailure remoteError)),
      testCase "typed result error Show excludes peer messages and data" $ do
        let err = remoteError {rpcErrorMessage = "private-message", rpcErrorData = Just (String "private-data"), rpcErrorAdditionalFields = KeyMap.singleton "private-key" (String "private-value")}
        show (RpcRemoteFailure err) @?= "RpcRemoteFailure <redacted>"
        show RpcMissingResult @?= "RpcMissingResult"
        show RpcInvalidResult @?= "RpcInvalidResult"
    ]

data PeerFailure = PeerEnded | PeerSendFailed | CallbackFailed deriving stock (Eq, Show)

instance Exception PeerFailure

withMemory :: (RpcChannel -> TQueue (Either PeerFailure Object) -> TQueue Object -> IO ()) -> IO ()
withMemory action = do
  incoming <- newTQueueIO
  sent <- newTQueueIO
  let receive = atomically (readTQueue incoming) >>= either throwIO pure
  withRpcChannel (atomically . writeTQueue sent) receive (\channel -> action channel incoming sent)

feed :: TQueue (Either PeerFailure Object) -> Object -> IO ()
feed queue = atomically . writeTQueue queue . Right

expectError :: RpcChannelError -> IO a -> IO ()
expectError expected action =
  try @RpcChannelError action >>= \case
    Left err -> err @?= expected
    Right _ -> assertFailure "Expected RPC channel failure"

expectResult :: Value -> JsonRpcBaseResponse -> IO ()
expectResult expected response = case envelopeBody response of
  BaseSuccess value -> successResponseResult value @?= Just expected
  BaseFailure _ -> assertFailure "Unexpected remote error"

request :: Text -> JsonRpcBaseRequest
request identifier = WithEnvelope (Just "1.205.0") Nothing (BaseRequest identifier "fixture.call" (Just (Object mempty)) mempty)

message :: RpcMessageBody -> JsonRpcMessage
message = WithEnvelope (Just "1.205.0") Nothing

barrier :: JsonRpcMessage
barrier = message (NotificationBody (BaseNotification "barrier" Nothing mempty))

remoteError :: JsonRpcError
remoteError = JsonRpcError RpcConflict "fixture error" (Just Null) mempty

reply :: Text -> Value -> Object
reply identifier result = KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.205.0", "type" .= String "response", "id" .= identifier, "result" .= result]

rawSuccess :: Maybe Value -> Maybe JsonRpcError -> JsonRpcBaseResponse
rawSuccess result err = WithEnvelope Nothing Nothing (BaseSuccess (BaseResponseSuccess "id" result err mempty))
