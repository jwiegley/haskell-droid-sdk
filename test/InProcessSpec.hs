{-# LANGUAGE OverloadedStrings #-}

module InProcessSpec (inProcessTests) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, poll, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, finally, fromException, throwIO, toException, try)
import Control.Monad (forM_, void, when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Protocol
import Factory.Droid.Transport.InProcess qualified as IP
import IpcSpec (newRegistry, rpcFrame, rpcRequest, wire)
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

inProcessTests :: TestTree
inProcessTests =
  testGroup
    "In-process channels"
    [ testCase "subscriptions precede connect and lifecycle cleanup runs once" $ bounded $ do
        peer <- newRuntime
        (observer, events) <- captureEvents
        let original = peerRuntime peer
            runtime = original {IP.inProcessConnect = Just (\url -> peerCounts peer >>= (@?= (1, 1, 1)) >> modifyIORef' (peerUrls peer) (<> [url]) >> peerMessage peer "{}")}
        IP.withInProcessChannel 4096 "in-process://exact path" runtime observer $ \connection -> do
          IP.isInProcessConnected connection >>= (@?= True)
          IP.receiveObject connection >>= (@?= mempty)
          IP.sendObject connection mempty
          atomically (readTQueue (peerSent peer)) >>= (@?= "{}")
        readIORef (peerUrls peer) >>= (@?= ["in-process://exact path"])
        readIORef (peerCloses peer) >>= (@?= 1)
        peerCounts peer >>= (@?= (0, 0, 0))
        events >>= (@?= ["open", "close"]) . map eventTag,
      testCase "connect failure preserves its cause and cleans a partially initialized runtime" $ bounded $ do
        peer <- newRuntime
        let runtime = (peerRuntime peer) {IP.inProcessConnect = Just (const (throwIO TestAbort))}
        try @TestFailure (IP.withInProcessChannel 4096 "" runtime ignore (const (pure ()))) >>= (@?= Left TestAbort)
        readIORef (peerCloses peer) >>= (@?= 1)
        peerCounts peer >>= (@?= (0, 0, 0)),
      testCase "connect cancellation keeps async identity and closes the runtime" $ bounded $ do
        peer <- newRuntime
        started <- newEmptyMVar
        hold <- newEmptyMVar
        let runtime = (peerRuntime peer) {IP.inProcessConnect = Just (\_ -> putMVar started () >> takeMVar hold)}
        withAsync (IP.withInProcessChannel 4096 "" runtime ignore (const (pure ()))) $ \connecting -> do
          takeMVar started
          cancel connecting
          waitCatch connecting >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Connect cancellation lost"
        readIORef (peerCloses peer) >>= (@?= 1)
        peerCounts peer >>= (@?= (0, 0, 0)),
      testCase "reported close during registration prevents connect and preserves exact close data" $ bounded $ do
        peer <- newRuntime
        (observer, events) <- captureEvents
        let runtime = (peerRuntime peer) {IP.inProcessOnClose = Just (\callback -> callback 1000.25 "" >> pure (pure ()))}
        try @IP.ChannelError (IP.withInProcessChannel 4096 "" runtime observer (const (pure ()))) >>= (@?= Left (IP.ChannelPeerClosed (Just 1000.25) (Just "")))
        readIORef (peerUrls peer) >>= (@?= [])
        readIORef (peerCloses peer) >>= (@?= 1)
        events >>= \case [IP.InProcessClosed code reason] -> (code, reason) @?= (1000.25, ""); _ -> assertFailure "Early close metadata lost"
        peerCounts peer >>= (@?= (0, 0, 0)),
      testCase "runtime errors are observable without disconnecting the channel" $ bounded $ do
        peer <- newRuntime
        (observer, events) <- captureEvents
        IP.withInProcessChannel 4096 "" (peerRuntime peer) observer $ \connection -> do
          peerError peer (toException TestAbort)
          IP.isInProcessConnected connection >>= (@?= True)
          IP.sendObject connection mempty
          atomically (readTQueue (peerSent peer)) >>= (@?= "{}")
          events >>= \case [IP.InProcessOpened, IP.InProcessError cause] -> fromException cause @?= Just TestAbort; _ -> assertFailure "Runtime error not observed",
      testCase "send failures report the original exception and leave raw in-process transport usable" $ bounded $ do
        peer <- newRuntime
        (observer, events) <- captureEvents
        failing <- newTVarIO True
        let original = peerRuntime peer
            runtime = original {IP.inProcessSendMessage = \message -> do failNow <- readTVarIO failing; if failNow then throwIO TestAbort else IP.inProcessSendMessage original message}
        IP.withInProcessChannel 4096 "" runtime observer $ \connection -> do
          try @TestFailure (IP.sendObject connection mempty) >>= (@?= Left TestAbort)
          IP.isInProcessConnected connection >>= (@?= True)
          atomically (writeTVar failing False)
          IP.sendObject connection mempty
          atomically (readTQueue (peerSent peer)) >>= (@?= "{}")
          events >>= (@?= ["open", "error"]) . map eventTag,
      testCase "an error observer cannot replace an initiating send exception" $ bounded $ do
        peer <- newRuntime
        let runtime = (peerRuntime peer) {IP.inProcessSendMessage = const (throwIO TestAbort)}
            observer = \case IP.InProcessError _ -> throwIO ObserverAbort; _ -> pure ()
        IP.withInProcessChannel 4096 "" runtime observer $ \connection ->
          try @TestFailure (IP.sendObject connection mempty) >>= (@?= Left TestAbort),
      testCase "peer close drains its message prefix and suppresses duplicate close events" $ bounded $ do
        peer <- newRuntime
        (observer, events) <- captureEvents
        IP.withInProcessChannel 4096 "" (peerRuntime peer) observer $ \connection -> do
          peerMessage peer "{}"
          peerClose peer 4000.125 ""
          IP.receiveObject connection >>= (@?= mempty)
          try @IP.ChannelError (IP.receiveObject connection) >>= (@?= Left (IP.ChannelPeerClosed (Just 4000.125) (Just "")))
          IP.closeInProcess connection
          IP.closeInProcess connection
        readIORef (peerCloses peer) >>= (@?= 1)
        events >>= \case [IP.InProcessOpened, IP.InProcessClosed code reason] -> (code, reason) @?= (4000.125, ""); _ -> assertFailure "Close events duplicated or changed",
      testCase "local close is idempotent and revokes subsequent operations" $ bounded $ do
        peer <- newRuntime
        (observer, events) <- captureEvents
        IP.withInProcessChannel 4096 "" (peerRuntime peer) observer $ \connection -> do
          IP.closeInProcess connection
          IP.closeInProcess connection
          try @IP.ChannelError (IP.sendObject connection mempty) >>= (@?= Left IP.ChannelClosed)
        readIORef (peerCloses peer) >>= (@?= 1)
        events >>= (@?= ["open", "close"]) . map eventTag,
      testCase "failed disconnect is attempted once and cannot replace an earlier failure" $
        bounded $
          forM_ [False, True] $ \failAction -> do
            peer <- newRuntime
            let runtime = (peerRuntime peer) {IP.inProcessDisconnect = Just (modifyIORef' (peerCloses peer) (+ 1) >> throwIO StopAbort)}
            result <- try @TestFailure (IP.withInProcessChannel 4096 "" runtime ignore (\_ -> when failAction (throwIO TestAbort)))
            result @?= Left (if failAction then TestAbort else StopAbort)
            readIORef (peerCloses peer) >>= (@?= 1)
            peerCounts peer >>= (@?= (0, 0, 0)),
      testCase "pending readiness delegates an unexecuted gate with exact identity and failure" $ bounded $ do
        peer <- newRuntime
        captured <- newEmptyMVar
        ready <- newEmptyTMVarIO
        let runtime = (peerRuntime peer) {IP.inProcessPendingSessionReady = Just (curry (putMVar captured))}
        IP.withInProcessChannel 4096 "" runtime ignore $ \connection -> do
          IP.setPendingSessionReady connection " exact " (readTMVar ready)
          (identifier, gate) <- takeMVar captured
          identifier @?= " exact "
          withAsync (atomically gate) $ \waiting -> do
            poll waiting >>= \case Nothing -> pure (); _ -> assertFailure "Readiness completed before producer"
            atomically (putTMVar ready ())
            wait waiting
          IP.setPendingSessionReady connection "failed" (throwSTM TestAbort)
          (failedId, failedGate) <- takeMVar captured
          failedId @?= "failed"
          try @TestFailure (atomically failedGate) >>= (@?= Left TestAbort),
      testCase "absent readiness hook is inert but escaped handles cannot register work" $ bounded $ do
        peer <- newRuntime
        escaped <- IP.withInProcessChannel 4096 "" (peerRuntime peer) ignore $ \connection -> do
          IP.setPendingSessionReady connection "" retry
          pure connection
        try @IP.ChannelError (IP.setPendingSessionReady escaped "" retry) >>= (@?= Left IP.ChannelClosed),
      testCase "failed event registration cleans earlier listeners without calling runtime connect" $ bounded $ do
        peer <- newRuntime
        let runtime = (peerRuntime peer) {IP.inProcessOnError = Just (\_ -> throwIO TestAbort)}
        try @TestFailure (IP.withInProcessChannel 4096 "" runtime ignore (const (pure ()))) >>= (@?= Left TestAbort)
        peerCounts peer >>= (@?= (0, 0, 0))
        readIORef (peerUrls peer) >>= (@?= [])
        readIORef (peerCloses peer) >>= (@?= 1),
      testCase "runtime teardown waits for unsubscribe even when scope exit is cancelled" $
        bounded $
          forM_ [False, True] $ \interrupted -> do
            peer <- newRuntime
            opened <- newEmptyMVar
            leave <- newEmptyMVar
            cleaning <- newEmptyMVar
            release <- newEmptyMVar
            disconnected <- newEmptyMVar
            let original = peerRuntime peer
                onMessage callback = case IP.inProcessOnMessage original of
                  Nothing -> assertFailure "Missing message fixture"
                  Just subscribe -> do
                    stop <- subscribe callback
                    pure (putMVar cleaning () >> takeMVar release >> stop)
                runtime = original {IP.inProcessOnMessage = Just onMessage, IP.inProcessDisconnect = Just (modifyIORef' (peerCloses peer) (+ 1) >> putMVar disconnected ())}
            withAsync (IP.withInProcessChannel 4096 "" runtime ignore (\_ -> putMVar opened () >> takeMVar leave)) $ \scope -> do
              takeMVar opened
              withAsync (peerClose peer 1000 "peer closed") $ \closing -> do
                takeMVar cleaning
                putMVar leave ()
                withAsync (if interrupted then cancel scope else wait scope) $ \ending -> do
                  premature <- timeout 100000 (readMVar disconnected) `finally` putMVar release ()
                  premature @?= Nothing
                  wait ending
                  when interrupted $ waitCatch scope >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Scope cancellation lost"
                wait closing
            readIORef (peerCloses peer) >>= (@?= 1)
            peerCounts peer >>= (@?= (0, 0, 0)),
      testCase "RPC remote errors are decoded by the existing shared protocol path" $ bounded $ do
        peer <- newRuntime
        IP.withInProcessChannel 4096 "" (peerRuntime peer) ignore $ \connection ->
          withRpcChannel (IP.sendObject connection) (IP.receiveObject connection) $ \rpc ->
            withAsync (requestReply rpc (Just 1000000) (rpcRequest "error")) $ \pending -> do
              void (atomically (readTQueue (peerSent peer)))
              let failure = object ["code" .= Number (-32602), "message" .= String "fixture error", "data" .= False]
              peerMessage peer (wire (rpcFrame ["type" .= String "response", "id" .= String "error", "error" .= failure]))
              response <- wait pending
              case decodeRpcResult @Value response of
                Left (RpcRemoteFailure cause) -> toJSON cause @?= failure
                _ -> assertFailure "Runtime error bypassed RPC error decoding"
    ]

data TestFailure = TestAbort | ObserverAbort | StopAbort deriving stock (Eq, Show)

instance Exception TestFailure

data RuntimePeer = RuntimePeer
  { peerRuntime :: IP.InProcessRuntime,
    peerMessage :: Text -> IO (),
    peerClose :: Scientific -> Text -> IO (),
    peerError :: SomeException -> IO (),
    peerSent :: TQueue Text,
    peerUrls :: IORef [Text],
    peerCloses :: IORef Int,
    peerCounts :: IO (Int, Int, Int)
  }

newRuntime :: IO RuntimePeer
newRuntime = do
  (onMessage, message, messages, _) <- newRegistry
  (onClose, close, closes, _) <- newRegistry
  (onError, runtimeError, errors, _) <- newRegistry
  sent <- newTQueueIO
  urls <- newIORef []
  closed <- newIORef 0
  let runtime =
        (IP.defaultInProcessRuntime (atomically . writeTQueue sent))
          { IP.inProcessConnect = Just (\url -> modifyIORef' urls (<> [url])),
            IP.inProcessDisconnect = Just (modifyIORef' closed (+ 1)),
            IP.inProcessOnMessage = Just onMessage,
            IP.inProcessOnClose = Just (onClose . uncurry),
            IP.inProcessOnError = Just onError
          }
  pure (RuntimePeer runtime message (curry close) runtimeError sent urls closed ((,,) <$> messages <*> closes <*> errors))

captureEvents :: IO (IP.InProcessEvent -> IO (), IO [IP.InProcessEvent])
captureEvents = do
  events <- newIORef []
  pure (\event -> modifyIORef' events (<> [event]), readIORef events)

eventTag :: IP.InProcessEvent -> String
eventTag = \case
  IP.InProcessOpened -> "open"
  IP.InProcessClosed _ _ -> "close"
  IP.InProcessError _ -> "error"

ignore :: IP.InProcessEvent -> IO ()
ignore _ = pure ()
