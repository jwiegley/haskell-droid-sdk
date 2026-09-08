{-# LANGUAGE OverloadedStrings #-}

module DispatchSpec (dispatchTests) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (bracket_, fromException, throwIO, try)
import Control.Monad (forM_, replicateM, replicateM_, void)
import Data.Aeson (Object, Result (..), Value (..), fromJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Factory.Droid.Protocol
import Factory.Droid.Protocol.Dispatch
import Factory.Droid.Schema.Metadata (TraceContextMeta (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Transport.Process (receiveObject, sendObject)
import ProcessSpec (bounded, withPeer)
import ProtocolSpec (PeerFailure (..), feed, message, reply, request, withMemory)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

dispatchTests :: TestTree
dispatchTests =
  testGroup
    "RPC dispatcher callbacks"
    [ testCase "native server handler and notification subscription coexist with correlated calls" $
        bounded $
          withPeer "rpc" 4096 $ \process ->
            withRpcChannel (sendObject process) (receiveObject process) $ \channel ->
              withRpcDispatcher channel context $ \dispatcher -> do
                notes <- newTQueueIO
                void (onRpcNotification dispatcher (atomically . writeTQueue notes . baseNotificationMethod . envelopeBody))
                void (registerRpcHandler dispatcher "server.question" (\_ -> pure (Right (String "fixture answer"))))
                withAsync (requestResult channel Nothing (request "a") :: IO Text) $ \first ->
                  withAsync (requestResult channel Nothing (request "b") :: IO Text) $ \second -> do
                    wait first >>= (@?= "a")
                    wait second >>= (@?= "b")
                    replicateM 2 (atomically (readTQueue notes)) >>= (@?= ["notice", "answered"]),
      testCase "listeners preserve registration order, isolate exceptions and unsubscribe idempotently" $ bounded $ withMemory $ \channel incoming _ ->
        withRpcDispatcher channel context $ \dispatcher -> do
          observed <- newTQueueIO
          removeFirst <- onRpcNotification dispatcher (\_ -> atomically (writeTQueue observed (1 :: Int)))
          void (onRpcNotification dispatcher (\_ -> throwIO CallbackFailed))
          void (onRpcNotification dispatcher (\_ -> atomically (writeTQueue observed 3)))
          feed incoming (toRpcObject notice)
          replicateM 2 (atomically (readTQueue observed)) >>= (@?= [1, 3])
          removeFirst >> removeFirst
          feed incoming (toRpcObject notice)
          atomically (readTQueue observed) >>= (@?= 3),
      testCase "raw subscribers retain all uncorrelated event variants" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          observed <- newTQueueIO
          void (onRpcEvent dispatcher (atomically . writeTQueue observed))
          let server = message (RequestBody (envelopeBody (call "server" "unknown")))
              diagnostic = message (ResponseBody (BaseResponseGeneric Nothing Nothing (Just remoteError) mempty))
          forM_ [notice, server, diagnostic] (feed incoming . toRpcObject)
          replicateM 3 (atomically (readTQueue observed)) >>= (@?= [notice, server, diagnostic])
          response <- readResponse sent
          envelopeBody response @?= BaseFailure (BaseResponseFailure (Just "server") methodMissing Nothing mempty),
      testCase "response context preserves caller metadata and cannot replace original ID" $ bounded $ withMemory $ \channel incoming sent -> do
        let meta = TraceContextMeta (Just "caller-trace") Nothing Nothing mempty
            chosen = context {envelopeMeta = Just meta, envelopeBody = KeyMap.fromList [("id", String "injected"), ("type", String "request"), ("result", Bool False), ("future", Number 7)]}
        withRpcDispatcher channel chosen $ \_ -> do
          feed incoming (toRpcObject ((call "original" "unknown") {envelopeMeta = Just (meta {traceParent = Just "peer-trace"})}))
          response <- readResponse sent
          envelopeMeta response @?= Just meta
          envelopeBody response @?= BaseFailure (BaseResponseFailure (Just "original") methodMissing Nothing (KeyMap.singleton "future" (Number 7))),
      testCase "explicit handler errors retain their wire details" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          void (registerRpcHandler dispatcher "fails" (\_ -> pure (Left remoteError)))
          feed incoming (toRpcObject (call "id" "fails"))
          response <- readResponse sent
          envelopeBody response @?= BaseFailure (BaseResponseFailure (Just "id") remoteError Nothing mempty),
      testCase "ordinary handler exceptions become content-free internal errors" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          void (registerRpcHandler dispatcher "throws" (\_ -> ioError (userError "private handler detail")))
          feed incoming (toRpcObject (call "id" "throws"))
          response <- readResponse sent
          envelopeBody response @?= BaseFailure (BaseResponseFailure (Just "id") (JsonRpcError RpcInternalError "RPC request handler failed" Nothing mempty) Nothing mempty),
      testCase "old handler unsubscribe cannot remove its replacement" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          old <- registerRpcHandler dispatcher "method" (\_ -> pure (Right (Number 1)))
          current <- registerRpcHandler dispatcher "method" (\_ -> pure (Right (Number 2)))
          old >> old
          feed incoming (toRpcObject (call "a" "method"))
          readResult sent >>= (@?= Number 2)
          current >> current
          feed incoming (toRpcObject (call "b" "method"))
          response <- readResponse sent
          envelopeBody response @?= BaseFailure (BaseResponseFailure (Just "b") methodMissing Nothing mempty),
      testCase "in-flight handlers retain their snapshot without blocking later requests" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          old <- registerRpcHandler dispatcher "method" (\_ -> putMVar started () >> takeMVar release >> pure (Right (Number 1)))
          feed incoming (toRpcObject (call "first" "method"))
          takeMVar started
          void (registerRpcHandler dispatcher "method" (\_ -> pure (Right (Number 2))))
          old
          feed incoming (toRpcObject (call "second" "method"))
          readResult sent >>= (@?= Number 2)
          putMVar release ()
          readResult sent >>= (@?= Number 1),
      testCase "duplicate active IDs do not run a second handler" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          count <- newIORef (0 :: Int)
          started <- newEmptyMVar
          release <- newEmptyMVar
          observed <- newEmptyMVar
          void (onRpcNotification dispatcher (\_ -> putMVar observed ()))
          void (registerRpcHandler dispatcher "wait" (\_ -> atomicModifyIORef' count (\n -> (n + 1, ())) >> putMVar started () >> readMVar release >> pure (Right Null)))
          feed incoming (toRpcObject (call "same" "wait"))
          takeMVar started
          feed incoming (toRpcObject (call "same" "wait"))
          feed incoming (toRpcObject notice)
          takeMVar observed
          readIORef count >>= (@?= 1)
          putMVar release ()
          readResult sent >>= (@?= Null)
          atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "notification callback can await a correlated request without blocking its reader" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          result <- newEmptyMVar
          void (onRpcNotification dispatcher (\_ -> requestResult channel Nothing (request "callback") >>= putMVar result))
          feed incoming (toRpcObject notice)
          atomically (readTQueue sent) >>= (@?= toRpcObject (request "callback"))
          feed incoming (reply "callback" (String "resolved"))
          takeMVar result >>= (@?= String "resolved"),
      testCase "callback-issued observed requests correlate before observation without deadlock" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          observed <- newTQueueIO
          returned <- newEmptyMVar
          release <- newEmptyMVar
          stop <- onRpcNotification dispatcher $ \_ -> do
            result <- requestReplyObserved channel Nothing (request "callback") (writeTQueue observed)
            pending <- atomically (tryReadTQueue observed)
            putMVar returned (result, pending)
            takeMVar release
          feed incoming (toRpcObject notice)
          void (atomically (readTQueue sent))
          feed incoming (reply "callback" (String "resolved"))
          (result, pending) <- takeMVar returned
          pending @?= Nothing
          stop
          putMVar release ()
          synchronizeRpcEvents channel
          atomically (readTQueue observed) >>= (@?= result),
      testCase "slow request handler does not stop notification delivery" $ bounded $ withMemory $ \channel incoming _ ->
        withRpcDispatcher channel context $ \dispatcher -> do
          started <- newEmptyMVar
          blocked <- newEmptyMVar
          observed <- newEmptyMVar
          void (registerRpcHandler dispatcher "wait" (\_ -> putMVar started () >> takeMVar blocked))
          void (onRpcNotification dispatcher (\_ -> putMVar observed ()))
          feed incoming (toRpcObject (call "id" "wait"))
          takeMVar started
          feed incoming (toRpcObject notice)
          takeMVar observed,
      testCase "scope failure cancels and joins handler resources without closing an idle borrowed channel" $ bounded $ withMemory $ \channel incoming sent -> do
        started <- newEmptyMVar
        cleaned <- newEmptyMVar
        blocked <- newEmptyMVar
        result <- try @PeerFailure $ withRpcDispatcher channel context $ \dispatcher -> do
          void (registerRpcHandler dispatcher "wait" (\_ -> bracket_ (putMVar started ()) (putMVar cleaned ()) (takeMVar blocked)))
          feed incoming (toRpcObject (call "id" "wait"))
          takeMVar started
          throwIO CallbackFailed :: IO ()
        result @?= Left CallbackFailed
        tryTakeMVar cleaned >>= (@?= Just ())
        atomically (tryReadTQueue sent) >>= (@?= Nothing)
        withAsync (requestResult channel Nothing (request "after") :: IO Value) $ \worker -> do
          void (atomically (readTQueue sent))
          feed incoming (reply "after" (Bool True))
          wait worker >>= (@?= Bool True),
      testCase "channel failure joins handlers before error delivery and replays its cause" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          started <- newEmptyMVar
          cleaned <- newEmptyMVar
          blocked <- newEmptyMVar
          observed <- newEmptyMVar
          void (registerRpcHandler dispatcher "wait" (\_ -> bracket_ (putMVar started ()) (putMVar cleaned ()) (takeMVar blocked)))
          void (onRpcError dispatcher (\err -> readMVar cleaned >> putMVar observed err))
          feed incoming (toRpcObject (call "id" "wait"))
          takeMVar started
          atomically (writeTQueue incoming (Left PeerEnded))
          takeMVar observed >>= (@?= RpcChannelReadFailure)
          late <- newEmptyMVar
          unsubscribe <- onRpcError dispatcher (putMVar late)
          takeMVar late >>= (@?= RpcChannelReadFailure)
          unsubscribe >> unsubscribe
          expectStopped (RpcDispatcherFailed RpcChannelReadFailure) (registerRpcHandler dispatcher "after" (\_ -> pure (Right Null)))
          expectStopped (RpcDispatcherFailed RpcChannelReadFailure) (onRpcNotification dispatcher (\_ -> pure ()))
          atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "error listeners unsubscribe and isolate ordinary exceptions" $ bounded $ withMemory $ \channel incoming _ ->
        withRpcDispatcher channel context $ \dispatcher -> do
          observed <- newEmptyMVar
          removedCalls <- newIORef (0 :: Int)
          removed <- onRpcError dispatcher (\_ -> atomicModifyIORef' removedCalls (\n -> (n + 1, ())))
          removed >> removed
          void (onRpcError dispatcher (\_ -> throwIO CallbackFailed))
          void (onRpcError dispatcher (putMVar observed))
          atomically (writeTQueue incoming (Left PeerEnded))
          takeMVar observed >>= (@?= RpcChannelReadFailure)
          readIORef removedCalls >>= (@?= 0),
      testCase "sibling handler finalizers can wait for one another during scope cleanup" $ bounded $ withMemory $ \channel incoming _ -> do
        started <- newTQueueIO
        firstStopped <- newEmptyMVar
        secondStopped <- newEmptyMVar
        firstDone <- newEmptyMVar
        secondDone <- newEmptyMVar
        blocked <- newEmptyMVar
        withRpcDispatcher channel context $ \dispatcher -> do
          let install method mine theirs done = registerRpcHandler dispatcher method (\_ -> bracket_ (atomically (writeTQueue started ())) (putMVar mine () >> readMVar theirs >> putMVar done ()) (readMVar blocked))
          void (install "first" firstStopped secondStopped firstDone)
          void (install "second" secondStopped firstStopped secondDone)
          feed incoming (toRpcObject (call "a" "first"))
          feed incoming (toRpcObject (call "b" "second"))
          replicateM_ 2 (atomically (readTQueue started))
        tryTakeMVar firstDone >>= (@?= Just ())
        tryTakeMVar secondDone >>= (@?= Just ()),
      testCase "normal scope closure neither reports a channel failure nor accepts later registrations" $ bounded $ withMemory $ \channel _ _ -> do
        errors <- newIORef (0 :: Int)
        dispatcher <- withRpcDispatcher channel context $ \value -> do
          void (onRpcError value (\_ -> atomicModifyIORef' errors (\n -> (n + 1, ()))))
          pure value
        readIORef errors >>= (@?= 0)
        expectStopped RpcDispatcherClosed (onRpcEvent dispatcher (\_ -> pure ()))
        expectStopped RpcDispatcherClosed (onRpcError dispatcher (\_ -> pure ()))
        expectStopped RpcDispatcherClosed (registerRpcHandler dispatcher "after" (\_ -> pure (Right Null))),
      testCase "event boundary completes without requiring another event or sending a frame" $ bounded $ withMemory $ \channel _ sent ->
        withRpcDispatcher channel context $ \_ -> do
          synchronizeRpcEvents channel
          atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "event boundary waits for prior callbacks even when the RPC reply has returned" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          finished <- newEmptyMVar
          void (onRpcNotification dispatcher (\_ -> putMVar started () >> takeMVar release >> putMVar finished ()))
          withAsync (requestResult channel Nothing (request "load") :: IO Text) $ \rpc -> do
            _ <- atomically (readTQueue sent)
            feed incoming (toRpcObject notice)
            takeMVar started
            feed incoming (reply "load" (String "loaded"))
            wait rpc >>= (@?= "loaded")
            withAsync (synchronizeRpcEvents channel) $ \boundary -> do
              timeout 20000 (wait boundary) >>= (@?= Nothing)
              putMVar release ()
              wait boundary
            tryTakeMVar finished >>= (@?= Just ()),
      testCase "cancelled event boundaries do not poison or block later boundaries" $ bounded $ withMemory $ \channel incoming _ ->
        withRpcDispatcher channel context $ \dispatcher -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          void (onRpcNotification dispatcher (\_ -> putMVar started () >> takeMVar release))
          feed incoming (toRpcObject notice)
          takeMVar started
          withAsync (synchronizeRpcEvents channel) $ \boundary -> do
            timeout 20000 (wait boundary) >>= (@?= Nothing)
            cancel boundary
            waitCatch boundary >>= \case
              Left err -> fromException err @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled boundary completed"
          putMVar release ()
          synchronizeRpcEvents channel,
      testCase "terminal channel failure wakes a boundary behind a blocked callback" $ bounded $ withMemory $ \channel incoming _ ->
        withRpcDispatcher channel context $ \dispatcher -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          void (onRpcNotification dispatcher (\_ -> putMVar started () >> takeMVar release))
          feed incoming (toRpcObject notice)
          takeMVar started
          withAsync (try @RpcChannelError (synchronizeRpcEvents channel)) $ \boundary -> do
            timeout 20000 (wait boundary) >>= (@?= Nothing)
            atomically (writeTQueue incoming (Left PeerEnded))
            wait boundary >>= (@?= Left RpcChannelReadFailure)
          putMVar release ()
    ]

context :: JsonRpcEnvelope
context = WithEnvelope (Just "1.205.0") Nothing mempty

call :: Text -> Text -> JsonRpcBaseRequest
call identifier method = WithEnvelope (Just "1.205.0") Nothing (BaseRequest identifier method Nothing mempty)

notice :: JsonRpcMessage
notice = message (NotificationBody (BaseNotification "notice" Nothing mempty))

methodMissing, remoteError :: JsonRpcError
methodMissing = JsonRpcError RpcMethodNotFound "No RPC handler registered" Nothing mempty
remoteError = JsonRpcError RpcInvalidParams "fixture error" (Just (Bool False)) mempty

readResponse :: TQueue Object -> IO JsonRpcBaseResponse
readResponse queue = do
  fields <- atomically (readTQueue queue)
  case fromJSON (Object fields) of
    Error _ -> assertFailure "Response failed envelope decoding"
    Success response -> pure response

readResult :: TQueue Object -> IO Value
readResult queue = readResponse queue >>= either (const (assertFailure "Unexpected RPC failure")) pure . decodeRpcResult

expectStopped :: RpcDispatcherError -> IO a -> IO ()
expectStopped expected action =
  try @RpcDispatcherError action >>= \case
    Left err -> err @?= expected
    Right _ -> assertFailure "Stopped dispatcher accepted registration"
