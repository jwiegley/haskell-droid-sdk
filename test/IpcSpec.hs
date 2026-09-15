{-# LANGUAGE OverloadedStrings #-}

module IpcSpec (ipcTests, newRegistry, rpcFrame, rpcRequest, wire, fixtureObject) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync, withAsyncOn)
import Control.Concurrent.STM
import Control.Exception (Exception, finally, fromException, throwIO, try)
import Control.Monad (forM_, replicateM_, void, when)
import Data.Aeson (FromJSON, Object, Value (..), encode, object, (.=))
import Data.Aeson.Types (Pair, parseEither, parseJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Unique (newUnique)
import Factory.Droid.Protocol
import Factory.Droid.Schema.RPC (JsonRpcBaseRequest)
import Factory.Droid.Transport.IPC qualified as IPC
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

ipcTests :: TestTree
ipcTests =
  testGroup
    "Host-supplied IPC"
    [ testCase "objects preserve exact data and scope owns only its subscriptions" $ bounded $ do
        peer <- newPeer
        let value = fixtureObject ["number" .= Number 9007199254740993.125, "text" .= String "سلام\n😀", "empty" .= String "", "flag" .= False, "absent" .= Null]
        escaped <- IPC.withIpcChannel 4096 (peerChannel peer) $ \transport -> do
          IPC.isIpcConnected transport >>= (@?= True)
          peerCounts peer >>= (@?= (1, 1))
          IPC.sendObject transport value
          atomically (readTQueue (peerSent peer)) >>= (@?= wire (Object value))
          peerMessage peer (wire (Object value))
          IPC.receiveObject transport >>= (@?= value)
          pure transport
        peerCounts peer >>= (@?= (0, 0))
        readTVarIO (peerAvailable peer) >>= (@?= True)
        IPC.isIpcConnected escaped >>= (@?= False)
        try @IPC.IpcError (IPC.sendObject escaped value) >>= (@?= Left IPC.IpcClosed),
      testCase "invalid limits and unavailable hosts do not publish or subscribe" $ bounded $ do
        peer <- newPeer
        try @IPC.IpcError (IPC.withIpcChannel 0 (peerChannel peer) (const (pure ()))) >>= (@?= Left IPC.InvalidIpcLimit)
        atomically (writeTVar (peerAvailable peer) False)
        try @IPC.IpcError (IPC.withIpcChannel 4096 (peerChannel peer) (const (pure ()))) >>= (@?= Left IPC.IpcUnavailable)
        peerCounts peer >>= (@?= (0, 0)),
      testCase "messages delivered during registration survive and disconnect hooks are optional" $ bounded $ do
        peer <- newPeer
        let original = peerChannel peer
            channel = original {IPC.ipcOnMessage = \callback -> do stop <- IPC.ipcOnMessage original callback; callback "{}"; pure stop, IPC.ipcOnDisconnect = Nothing}
        IPC.withIpcChannel 4096 channel $ \transport -> do
          peerCounts peer >>= (@?= (1, 0))
          IPC.receiveObject transport >>= (@?= mempty)
        peerCounts peer >>= (@?= (0, 0)),
      testCase "disconnect during subscription cannot resurrect a connection" $ bounded $ do
        peer <- newPeer
        let original = peerChannel peer
            channel = original {IPC.ipcOnDisconnect = Just (\callback -> do callback (Just "early"); maybe (pure (pure ())) ($ callback) (IPC.ipcOnDisconnect original))}
        result <- try @IPC.IpcError (IPC.withIpcChannel 4096 channel (const (assertFailure "Disconnected channel published" :: IO ())))
        result @?= Left (IPC.IpcDisconnected (Just "early"))
        peerCounts peer >>= (@?= (0, 0)),
      testCase "failed subscription releases earlier registration and preserves the exception" $ bounded $ do
        peer <- newPeer
        let channel = (peerChannel peer) {IPC.ipcOnDisconnect = Just (\_ -> throwIO TestAbort)}
        try @TestFailure (IPC.withIpcChannel 4096 channel (const (pure ()))) >>= (@?= Left TestAbort)
        peerCounts peer >>= (@?= (0, 0)),
      testCase "peer disconnect drains its prefix then retains empty reason and rejects late callbacks" $ bounded $ do
        peer <- newPeer
        IPC.withIpcChannel 4096 (peerChannel peer) $ \transport -> do
          callbacks <- peerCallbacks peer
          length callbacks @?= 1
          peerMessage peer "{}"
          peerDisconnect peer (Just "")
          peerCounts peer >>= (@?= (0, 0))
          IPC.isIpcConnected transport >>= (@?= False)
          forM_ callbacks ($ "{\"late\":true}")
          IPC.receiveObject transport >>= (@?= mempty)
          try @IPC.IpcError (IPC.receiveObject transport) >>= (@?= Left (IPC.IpcDisconnected (Just "")))
          show (IPC.IpcDisconnected (Just "private reason")) @?= "IpcDisconnected <redacted>",
      testCase "invalid and oversized messages fail without exposing contents" $
        bounded $
          forM_ [("{private", IPC.IpcInvalidObject), ("[]", IPC.IpcInvalidObject), (Text.replicate 80 "😀", IPC.IpcMessageTooLarge)] $ \(message, expected) -> do
            peer <- newPeer
            IPC.withIpcChannel 128 (peerChannel peer) $ \transport -> do
              peerMessage peer message
              peerMessage peer "{}"
              try @IPC.IpcError (IPC.receiveObject transport) >>= (@?= Left expected)
              try @IPC.IpcError (IPC.receiveObject transport) >>= (@?= Left expected)
              peerCounts peer >>= (@?= (0, 0)),
      testCase "invalid-message retirement is atomic with concurrent host delivery" $ bounded $ replicateM_ 200 $ do
        peer <- newPeer
        IPC.withIpcChannel 4096 (peerChannel peer) $ \transport -> do
          peerMessage peer "{invalid"
          withAsyncOn 0 (try @IPC.IpcError (IPC.receiveObject transport)) $ \receiving ->
            withAsyncOn 1 (replicateM_ 100 (peerMessage peer "{}")) $ \delivering -> do
              wait receiving >>= (@?= Left IPC.IpcInvalidObject)
              wait delivering
          try @IPC.IpcError (IPC.receiveObject transport) >>= (@?= Left IPC.IpcInvalidObject),
      testCase "outgoing byte limits fail before host submission" $ bounded $ do
        peer <- newPeer
        IPC.withIpcChannel 16 (peerChannel peer) $ \transport -> do
          try @IPC.IpcError (IPC.sendObject transport (fixtureObject ["text" .= Text.replicate 20 "😀"])) >>= (@?= Left IPC.IpcMessageTooLarge)
          atomically (isEmptyTQueue (peerSent peer)) >>= (@?= True)
          peerCounts peer >>= (@?= (0, 0)),
      testCase "availability cancellation retains identity without installing subscriptions" $ bounded $ do
        peer <- newPeer
        started <- newEmptyMVar
        hold <- newEmptyMVar
        let channel = (peerChannel peer) {IPC.ipcIsAvailable = putMVar started () >> takeMVar hold >> pure True}
        withAsync (IPC.withIpcChannel 4096 channel (const (pure ()))) $ \connecting -> do
          takeMVar started
          cancel connecting
          waitCatch connecting >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Availability cancellation lost"
        peerCounts peer >>= (@?= (0, 0)),
      testCase "disconnect during send is not reported as successful or replayed" $ bounded $ do
        peer <- newPeer
        let original = peerChannel peer
            channel = original {IPC.ipcSendMessage = \message -> IPC.ipcSendMessage original message >> peerDisconnect peer (Just "during send")}
        IPC.withIpcChannel 4096 channel $ \transport -> do
          try @IPC.IpcError (IPC.sendObject transport mempty) >>= (@?= Left (IPC.IpcDisconnected (Just "during send")))
          atomically (readTQueue (peerSent peer)) >>= (@?= "{}")
          atomically (isEmptyTQueue (peerSent peer)) >>= (@?= True),
      testCase "availability loss is checked before sending and retires subscriptions" $ bounded $ do
        peer <- newPeer
        IPC.withIpcChannel 4096 (peerChannel peer) $ \transport -> do
          atomically (writeTVar (peerAvailable peer) False)
          try @IPC.IpcError (IPC.sendObject transport mempty) >>= (@?= Left IPC.IpcUnavailable)
          atomically (isEmptyTQueue (peerSent peer)) >>= (@?= True)
          peerCounts peer >>= (@?= (0, 0)),
      testCase "send failures and receive cancellation preserve initiating exceptions" $ bounded $ do
        peer <- newPeer
        let channel = (peerChannel peer) {IPC.ipcSendMessage = const (throwIO TestAbort)}
        IPC.withIpcChannel 4096 channel $ \transport -> do
          try @TestFailure (IPC.sendObject transport mempty) >>= (@?= Left TestAbort)
          peerCounts peer >>= (@?= (0, 0))
        IPC.withIpcChannel 4096 (peerChannel peer) $ \transport -> do
          started <- newEmptyMVar
          withAsync (putMVar started () >> IPC.receiveObject transport) $ \waiting -> do
            takeMVar started
            cancel waiting
            waitCatch waiting >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Receive cancellation lost"
        peerCounts peer >>= (@?= (0, 0)),
      testCase "independent scopes remove only their own listeners" $ bounded $ do
        peer <- newPeer
        IPC.withIpcChannel 4096 (peerChannel peer) $ \first ->
          IPC.withIpcChannel 4096 (peerChannel peer) $ \second -> do
            peerCounts peer >>= (@?= (2, 2))
            IPC.closeIpc first
            IPC.closeIpc first
            peerCounts peer >>= (@?= (1, 1))
            peerMessage peer "{}"
            IPC.receiveObject second >>= (@?= mempty)
        peerCounts peer >>= (@?= (0, 0)),
      testCase "all unsubscribe actions run and primary callback exceptions win" $
        bounded $
          forM_ [False, True] $ \failAction -> do
            peer <- newPeer
            let original = peerChannel peer
                channel = original {IPC.ipcOnMessage = \callback -> do stop <- IPC.ipcOnMessage original callback; pure (stop >> throwIO StopAbort)}
            result <- try @TestFailure (IPC.withIpcChannel 4096 channel (\_ -> when failAction (throwIO TestAbort)))
            result @?= Left (if failAction then TestAbort else StopAbort)
            peerCounts peer >>= (@?= (0, 0)),
      testCase "scope exit joins unsubscribe already running on a host callback" $ bounded $ do
        peer <- newPeer
        opened <- newEmptyMVar
        leave <- newEmptyMVar
        cleaning <- newEmptyMVar
        release <- newEmptyMVar
        let original = peerChannel peer
            channel = original {IPC.ipcOnMessage = \callback -> do stop <- IPC.ipcOnMessage original callback; pure (putMVar cleaning () >> takeMVar release >> stop)}
        withAsync (IPC.withIpcChannel 4096 channel (\_ -> putMVar opened () >> takeMVar leave)) $ \scope -> do
          takeMVar opened
          withAsync (peerDisconnect peer Nothing) $ \closing -> do
            takeMVar cleaning
            putMVar leave ()
            before <- timeout 100000 (wait scope) `finally` putMVar release ()
            before @?= Nothing
            wait scope
            wait closing
        peerCounts peer >>= (@?= (0, 0)),
      testCase "the existing RPC reader correlates replies and reports host disconnect" $ bounded $ do
        peer <- newPeer
        IPC.withIpcChannel 4096 (peerChannel peer) $ \transport ->
          withRpcChannel (IPC.sendObject transport) (IPC.receiveObject transport) $ \rpc -> do
            withAsync (requestReply rpc (Just 1000000) (rpcRequest "one")) $ \pending -> do
              void (atomically (readTQueue (peerSent peer)))
              peerMessage peer (wire (rpcFrame ["type" .= String "response", "id" .= String "one", "result" .= object ["accepted" .= False]]))
              response <- wait pending
              decodeRpcResult @Value response @?= Right (object ["accepted" .= False])
            withAsync (requestReply rpc (Just 1000000) (rpcRequest "two")) $ \pending -> do
              void (atomically (readTQueue (peerSent peer)))
              peerDisconnect peer Nothing
              waitCatch pending >>= \case Left cause -> fromException cause @?= Just RpcChannelReadFailure; Right _ -> assertFailure "Disconnected request succeeded"
    ]

data TestFailure = TestAbort | StopAbort deriving stock (Eq, Show)

instance Exception TestFailure

data Peer = Peer
  { peerChannel :: IPC.IpcMessageChannel,
    peerMessage :: Text -> IO (),
    peerDisconnect :: Maybe Text -> IO (),
    peerSent :: TQueue Text,
    peerAvailable :: TVar Bool,
    peerCounts :: IO (Int, Int),
    peerCallbacks :: IO [Text -> IO ()]
  }

newPeer :: IO Peer
newPeer = do
  (subscribeMessage, message, messages, callbacks) <- newRegistry
  (subscribeDisconnect, disconnect, disconnects, _) <- newRegistry
  sent <- newTQueueIO
  available <- newTVarIO True
  let channel = IPC.IpcMessageChannel (readTVarIO available) (atomically . writeTQueue sent) subscribeMessage (Just subscribeDisconnect)
  pure (Peer channel message disconnect sent available ((,) <$> messages <*> disconnects) callbacks)

newRegistry :: IO ((a -> IO ()) -> IO (IO ()), a -> IO (), IO Int, IO [a -> IO ()])
newRegistry = do
  entries <- newTVarIO Map.empty
  let subscribe callback = do
        token <- newUnique
        atomically (modifyTVar' entries (Map.insert token callback))
        pure (atomically (modifyTVar' entries (Map.delete token)))
      callbacks = Map.elems <$> readTVarIO entries
  pure (subscribe, \value -> callbacks >>= mapM_ ($ value), Map.size <$> readTVarIO entries, callbacks)

fixtureObject :: [Pair] -> Object
fixtureObject fields = case object fields of Object value -> value; _ -> error "Object fixture"

wire :: Value -> Text
wire = Text.decodeUtf8 . BL.toStrict . encode

rpcFrame :: [Pair] -> Value
rpcFrame fields = object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)

rpcRequest :: Text -> JsonRpcBaseRequest
rpcRequest identifier = decodeFixture (rpcFrame ["type" .= String "request", "id" .= identifier, "method" .= String "daemon.get_proxy_token"])

decodeFixture :: (FromJSON a) => Value -> a
decodeFixture value = either error id (parseEither parseJSON value)
