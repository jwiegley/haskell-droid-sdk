{-# LANGUAGE OverloadedStrings #-}

module ConnectionHooksSpec (connectionHookTests) where

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM_, replicateM, void, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid qualified as Droid
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol
import Factory.Droid.Protocol.Dispatch
import Factory.Droid.Schema.Daemon.Cron (ListCronsParams (..), defaultListCronsParams)
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.RPC
import Factory.Droid.Transport
import Factory.Droid.Transport.Process qualified as Process
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import ProtocolSpec (PeerFailure (..), feed, reply, request, withMemory)
import System.Environment (getExecutablePath)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

connectionHookTests :: TestTree
connectionHookTests =
  testGroup
    "Connection and request hooks"
    [ testCase "only string session IDs enter the optional guard, before mandatory gates" $ bounded $ withMemory $ \channel incoming sent -> do
        calls <- newTQueueIO @(Text, Text, Text)
        caller <- myThreadId
        void $ setRpcBeforeRequest channel $ Just $ \session method -> do
          myThreadId >>= (@?= caller)
          atomically (getRpcPendingCount channel) >>= (@?= 0)
          atomically (writeTQueue calls ("guard", session, method))
        void $ registerRpcRequestBarrier channel $ \frame -> atomically (writeTQueue calls ("gate", "", baseRequestMethod (envelopeBody frame)))
        let cases = [("empty", Just (Object (KeyMap.singleton "sessionId" (String "")))), ("missing", Nothing), ("null", Just (Object (KeyMap.singleton "sessionId" Null))), ("false", Just (Object (KeyMap.singleton "sessionId" (Bool False)))), ("array", Just (Array mempty))]
        withAsync (sequence_ [atomically (readTQueue sent) >> feed incoming (reply identifier Null) | (identifier, _) <- cases]) $ \peer -> do
          forM_ cases $ \(identifier, params) -> void (requestReply channel Nothing ((request identifier) {envelopeBody = (envelopeBody (request identifier)) {baseRequestParams = params}}))
          wait peer
        atomically (replicateM 6 (readTQueue calls)) >>= (@?= [("guard", "", "fixture.call"), ("gate", "", "fixture.call"), ("gate", "", "fixture.call"), ("gate", "", "fixture.call"), ("gate", "", "fixture.call"), ("gate", "", "fixture.call")]),
      testCase "typed per-request skipping bypasses no mandatory barrier" $ bounded $ withMemory $ \channel _ sent -> do
        void (setRpcBeforeRequest channel (Just (\_ _ -> assertFailure "Skipped guard ran")))
        void (registerRpcRequestBarrier channel (const (throwIO CallbackFailed)))
        let options = Client.CallOptions "skip" context (Just 1000000)
        result <- try @PeerFailure (Client.callWithHookPolicy SkipBeforeRequest (Proxy @(WithEnvelope (MethodRequest "fixture.call" Object))) channel options (KeyMap.singleton "sessionId" (String "s")) :: IO Object)
        result @?= Left CallbackFailed
        atomically (getRpcPendingCount channel) >>= (@?= 0)
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "before-request guards can make explicitly skipped correlated calls without owning the writer" $ bounded $ withMemory $ \channel incoming sent -> do
        void $ setRpcBeforeRequest channel $ Just $ \_ _ -> void (requestReplyWithHookPolicy channel SkipBeforeRequest Nothing (scoped "inner" "s"))
        withAsync (requestReply channel Nothing (scoped "outer" "s")) $ \worker -> do
          nextId sent >>= (@?= "inner")
          feed incoming (reply "inner" Null)
          nextId sent >>= (@?= "outer")
          feed incoming (reply "outer" Null)
          void (wait worker),
      testCase "guard replacement is token-owned, failures are original, and preflight does not admit settlement" $ bounded $ withMemory $ \channel _ sent -> withIntake channel $ do
        settled <- newTQueueIO
        void (onRpcRequestSettled channel (atomically . writeTQueue settled))
        old <- setRpcBeforeRequest channel (Just (\_ _ -> assertFailure "Old guard ran"))
        current <- setRpcBeforeRequest channel (Just (\_ _ -> throwIO CallbackFailed))
        old >> old
        try @PeerFailure (requestReply channel Nothing (scoped "failure" "s")) >>= (@?= Left CallbackFailed)
        try @RpcChannelError (requestReply channel (Just 0) (scoped "zero" "s")) >>= (@?= Left RpcRequestTimedOut)
        try @RpcChannelError (requestReply channel (Just (-1)) (scoped "negative" "s")) >>= (@?= Left RpcInvalidTimeout)
        current >> current
        synchronizeRpcEvents channel
        atomically (tryReadTQueue settled) >>= (@?= Nothing)
        atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "cancelled guards have no pending request, preserve identity and leave the channel usable" $ bounded $ withMemory $ \channel incoming sent -> do
        entered <- newEmptyMVar
        never <- newEmptyMVar
        stop <- setRpcBeforeRequest channel (Just (\_ _ -> putMVar entered () >> takeMVar never))
        withAsync (requestReply channel Nothing (scoped "cancel" "s")) $ \worker -> do
          takeMVar entered
          atomically (getRpcPendingCount channel) >>= (@?= 0)
          cancel worker
          waitCatch worker >>= expectCancelled
        stop
        complete channel incoming sent (request "after"),
      testCase "settlement follows result observation, isolates failures and filters exact sessions" $ bounded $ withMemory $ \channel incoming sent -> withIntake channel $ do
        seen <- newTQueueIO
        observed <- newTVarIO False
        void (onRpcRequestSettled channel (const (throwIO CallbackFailed)))
        stop <- onRpcRequestSettled channel (\identifier -> atomically $ do ready <- readTVar observed; count <- getRpcPendingCount channel; writeTQueue seen (identifier, ready, count))
        matching <- newTQueueIO
        void (onRpcSessionRequestSettled channel "s" (atomically . writeTQueue matching))
        foreignSession <- newTQueueIO
        void (onRpcSessionRequestSettled channel "other" (atomically . writeTQueue foreignSession))
        withAsync (requestReplyObserved channel Nothing (scoped "accepted" "s") (const (writeTVar observed True))) $ \worker -> do
          void (nextId sent)
          feed incoming (reply "accepted" Null)
          feed incoming (reply "accepted" (Bool False))
          feed incoming (reply "unknown" Null)
          void (wait worker)
        atomically (readTQueue seen) >>= (@?= ("accepted", True, 0))
        atomically (readTQueue matching) >>= (@?= "accepted")
        stop >> stop
        complete channel incoming sent (request "later")
        synchronizeRpcEvents channel
        atomically (tryReadTQueue seen) >>= (@?= Nothing)
        atomically (tryReadTQueue matching) >>= (@?= Nothing)
        atomically (tryReadTQueue foreignSession) >>= (@?= Nothing),
      testCase "settled responses are no longer pending even while the sender has not returned" $ bounded $ do
        incoming <- newTQueueIO
        sent <- newEmptyMVar
        release <- newEmptyMVar
        withRpcChannel (\_ -> putMVar sent () >> takeMVar release) (atomically (readTQueue incoming)) $ \channel -> withIntake channel $ do
          settled <- newEmptyMVar
          void (onRpcRequestSettled channel (putMVar settled))
          withAsync (requestReply channel Nothing (request "early")) $ \worker -> do
            takeMVar sent
            atomically (getRpcPendingCount channel) >>= (@?= 1)
            atomically (writeTQueue incoming (reply "early" Null))
            takeMVar settled >>= (@?= "early")
            atomically (getRpcPendingCount channel) >>= (@?= 0)
            putMVar release ()
            void (wait worker),
      testCase "remote rejection, malformed response, timeout and cancellation each settle exactly once" $ bounded $ withMemory $ \channel incoming sent -> withIntake channel $ do
        seen <- newTQueueIO
        void (onRpcRequestSettled channel (atomically . writeTQueue seen))
        withAsync (requestReply channel Nothing (request "remote")) $ \worker -> do
          void (nextId sent)
          feed incoming (KeyMap.insert "error" (toJSON (JsonRpcError RpcConflict "fixture" Nothing mempty)) (reply "remote" Null))
          void (wait worker)
        withAsync (try @RpcChannelError (requestReply channel Nothing (request "malformed"))) $ \worker -> do
          void (nextId sent)
          feed incoming (KeyMap.delete "result" (reply "malformed" Null))
          wait worker >>= (@?= Left RpcMalformedResponse)
        try @RpcChannelError (requestReply channel (Just 20000) (request "timeout")) >>= (@?= Left RpcRequestTimedOut)
        void (nextId sent)
        withAsync (requestReply channel Nothing (request "cancel")) $ \worker -> do
          void (nextId sent)
          cancel worker
          waitCatch worker >>= expectCancelled
        atomically (replicateM 4 (readTQueue seen)) >>= (@?= ["remote", "malformed", "timeout", "cancel"])
        feed incoming (reply "timeout" Null)
        feed incoming (reply "cancel" Null)
        complete channel incoming sent (request "after")
        atomically (readTQueue seen) >>= (@?= "after")
        synchronizeRpcEvents channel
        atomically (tryReadTQueue seen) >>= (@?= Nothing),
      testCase "transport failure settles pending requests in admission order before error and close" $ bounded $ withMemory $ \channel incoming sent ->
        withRpcDispatcher channel context $ \dispatcher -> do
          events <- newTQueueIO
          void (onRpcRequestSettled channel (atomically . writeTQueue events))
          void (onRpcError dispatcher (const (atomically (writeTQueue events "error"))))
          void (onRpcClose dispatcher (\failure -> (failure @?= Just RpcChannelReadFailure) >> atomically (writeTQueue events "close")))
          withAsync (try @RpcChannelError (requestReply channel Nothing (request "z"))) $ \first -> do
            void (nextId sent)
            withAsync (try @RpcChannelError (requestReply channel Nothing (request "a"))) $ \second -> do
              void (nextId sent)
              atomically (getRpcPendingCount channel) >>= (@?= 2)
              atomically (writeTQueue incoming (Left PeerEnded))
              wait first >>= (@?= Left RpcChannelReadFailure)
              wait second >>= (@?= Left RpcChannelReadFailure)
          atomically (replicateM 4 (readTQueue events)) >>= (@?= ["z", "a", "error", "close"])
          atomically (getRpcPendingCount channel) >>= (@?= 0)
          void (onRpcClose dispatcher (\failure -> (failure @?= Just RpcChannelReadFailure) >> atomically (writeTQueue events "replay")))
          atomically (readTQueue events) >>= (@?= "replay"),
      testCase "normal dispatcher closure is once-only, unsubscribable and cannot replace the primary error" $
        bounded $
          forM_ [False, True] $ \asynchronous ->
            withMemory $ \channel _ _ -> do
              seen <- newTQueueIO
              result <- try @PeerFailure $ withRpcDispatcher channel context $ \dispatcher -> do
                removed <- onRpcClose dispatcher (const (atomically (writeTQueue seen False)))
                removed >> removed
                void (onRpcClose dispatcher (\failure -> (failure @?= Nothing) >> atomically (writeTQueue seen True) >> if asynchronous then throwIO AsyncCancelled else throwIO PeerSendFailed))
                throwIO CallbackFailed
              (result :: Either PeerFailure ()) @?= Left CallbackFailed
              atomically (readTQueue seen) >>= (@?= True)
              atomically (tryReadTQueue seen) >>= (@?= Nothing),
      testCase "settlement unsubscribe does not revoke an already queued snapshot" $ bounded $ withMemory $ \channel incoming sent -> do
        seen <- newEmptyMVar
        stop <- onRpcRequestSettled channel (putMVar seen)
        complete channel incoming sent (request "queued")
        stop
        withIntake channel (takeMVar seen >>= (@?= "queued")),
      testCase "normal local startup and turns execute configured settlement hooks" $ bounded $ do
        executable <- getExecutablePath
        settled <- newTQueueIO
        sent <- newTQueueIO
        Process.withJsonLinesProcess (10 * 1024 * 1024) 50000 (Process.droidProcess executable Process.StreamJsonRpc) $ \peer -> do
          let transport = (processTransport peer) {transportSendObject = \frame -> atomically (writeTQueue sent (frameId frame)) >> Process.sendObject peer frame}
              handlers = Droid.defaultDroidHandlers {Droid.onDroidRequestSettled = Just (atomically . writeTQueue settled)}
          Droid.withDroidSessionOnHandlers (Droid.defaultDroidSessionOptions ".") transport handlers $ \session -> do
            first <- atomically (readTQueue sent)
            atomically (readTQueue settled) >>= (@?= first)
            void (Droid.sendPrompt session "hello" (const (pure ())))
            next <- atomically (readTQueue sent)
            atomically (readTQueue settled) >>= (@?= next),
      testCase "normal daemon hooks expose logical identity, kind, counts and isolated scope closure" $ bounded $ withDaemonMemory $ \transport sent -> do
        closed <- newEmptyMVar
        escaped <- Daemon.withConnectionOn clientOptions transport $ \connection -> do
          identifier <- Daemon.getConnectionId connection
          case identifier of Nothing -> assertFailure "Missing generation identity"; Just _ -> pure ()
          Daemon.getTransportKind connection @?= CustomTransport
          seen <- newEmptyMVar
          void (Daemon.onRequestSettled connection (putMVar seen))
          void (Daemon.onConnectionClose connection (\case Nothing -> putMVar closed (); Just _ -> assertFailure "Invented failure on logical close"))
          void (Daemon.setBeforeRequest connection (Just (\session method -> (session @?= "s") >> (method @?= "daemon.list_crons"))))
          void (Daemon.listCrons connection filteredCrons)
          frame <- atomically (readTQueue sent)
          takeMVar seen >>= (@?= frameId frame)
          Daemon.getPendingCount connection >>= (@?= 0)
          pure connection
        takeMVar closed
        Daemon.getConnectionId escaped >>= (@?= Nothing),
      testCase "before-request runs ahead of authentication wait; cancelling the waiter does not cancel repair" $ bounded $ withDaemonMemory $ \transport _ ->
        Daemon.withConnectionOn clientOptions transport $ \connection -> do
          void (Daemon.logout connection)
          providerEntered <- newEmptyMVar
          releaseProvider <- newEmptyMVar
          guardEntered <- newEmptyMVar
          void (Daemon.setBeforeRequest connection (Just (\_ _ -> putMVar guardEntered ())))
          let authentication = Daemon.DaemonTokenProvider (putMVar providerEntered () >> takeMVar releaseProvider >> pure (Just "offline-refreshed")) Nothing
          withAsync (Daemon.ensureConnectionAuthenticated connection authentication) $ \repair -> do
            takeMVar providerEntered
            withAsync (Daemon.listCrons connection filteredCrons) $ \worker -> do
              takeMVar guardEntered
              timeout 50000 (wait worker) >>= (@?= Nothing)
              Daemon.getPendingCount connection >>= (@?= 0)
              cancel worker
              waitCatch worker >>= expectCancelled
            putMVar releaseProvider ()
            wait repair
          void (Daemon.listCrons connection filteredCrons)
          Daemon.isAuthenticated connection >>= (@?= True),
      testCase "authentication settlement can issue correlated queries without awaiting its own intake barrier" $ bounded $ withDaemonMemory $ \transport _ ->
        Daemon.withConnectionOn clientOptions transport $ \connection -> do
          void (Daemon.logout connection)
          first <- newIORef True
          observed <- newEmptyMVar
          void $ Daemon.onRequestSettled connection $ \_ -> do
            selected <- atomicModifyIORef' first (False,)
            when selected $ void (Daemon.getProxyToken connection) >> putMVar observed ()
          Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonToken "offline" Nothing))
          takeMVar observed
          Daemon.getPendingCount connection >>= (@?= 0),
      testCase "queued authentication requests retain the originating repair failure" $ bounded $ withDaemonMemory $ \transport sent ->
        Daemon.withConnectionOn clientOptions transport $ \connection -> do
          void (Daemon.logout connection)
          void (atomically (readTQueue sent))
          providerEntered <- newEmptyMVar
          releaseProvider <- newEmptyMVar
          guardEntered <- newEmptyMVar
          void (Daemon.setBeforeRequest connection (Just (\_ _ -> putMVar guardEntered ())))
          let authentication = Daemon.DaemonTokenProvider (putMVar providerEntered () >> takeMVar releaseProvider >> throwIO CallbackFailed) Nothing
          withAsync (try @PeerFailure (Daemon.ensureConnectionAuthenticated connection authentication)) $ \repair -> do
            takeMVar providerEntered
            withAsync (try @SomeException (Daemon.listCrons connection filteredCrons)) $ \worker -> do
              takeMVar guardEntered
              putMVar releaseProvider ()
              wait repair >>= (@?= Left CallbackFailed)
              wait worker >>= \case
                Left cause -> fromException cause @?= Just CallbackFailed
                Right _ -> assertFailure "Request escaped failed authentication"
          atomically (tryReadTQueue sent) >>= (@?= Nothing),
      testCase "write failure and blocked-write timeout settle once without leaking exception text" $
        bounded $
          forM_ [False, True] $ \blocked -> do
            seen <- newTQueueIO
            let send _ = if blocked then atomically retry else throwIO PeerSendFailed
            withRpcChannel send (atomically retry) $ \channel -> withIntake channel $ do
              void (onRpcRequestSettled channel (atomically . writeTQueue seen))
              result <- try @RpcChannelError (requestReply channel (if blocked then Just 20000 else Nothing) (request "write"))
              result @?= Left (if blocked then RpcRequestTimedOut else RpcChannelWriteFailure)
              atomically (readTQueue seen) >>= (@?= "write")
              atomically (getRpcPendingCount channel) >>= (@?= 0)
              atomically (tryReadTQueue seen) >>= (@?= Nothing),
      testCase "default guard reloads known sessions but skips control-plane work; attachment settlement stays scoped" $ bounded $ withDaemonMemory $ \transport sent ->
        Daemon.withConnectionOn (clientOptions {Daemon.daemonClientRestoreTerminalsOnLoad = False}) transport $ \connection -> do
          seen <- newTQueueIO
          let handlers = Droid.defaultDroidHandlers {Droid.onDroidRequestSettled = Just (atomically . writeTQueue seen)}
          Daemon.withResumedSessionOnHandlers connection handlers "s" $ \session -> do
            void (Daemon.getProxyToken connection)
            void (Daemon.listCrons connection (defaultListCronsParams {listCronsSessionId = Just "other"}))
            Daemon.markSessionNotLoaded connection "s"
            void (Daemon.listCrons connection filteredCrons)
            Daemon.getSessionReadiness connection "s" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase
            void (Daemon.listCommands session)
            frames <- atomically (replicateM 6 (readTQueue sent))
            map (KeyMap.lookup "method") frames @?= map (Just . String) ["daemon.load_session", "daemon.get_proxy_token", "daemon.list_crons", "daemon.list_crons", "daemon.load_session", "daemon.list_commands"]
            let expected = [frameId frame | frame <- frames, case KeyMap.lookup "params" frame of Just (Object params) -> KeyMap.lookup "sessionId" params == Just (String "s"); _ -> False]
            atomically (replicateM (length expected) (readTQueue seen)) >>= (@?= expected)
          void (Daemon.listCrons connection filteredCrons)
          atomically (tryReadTQueue seen) >>= (@?= Nothing),
      testCase "daemon close observers retain actual WebSocket code and reason" $ bounded $ do
        ready <- newEmptyMVar
        closed <- newEmptyMVar
        count <- newIORef (0 :: Int)
        withPeer
          ( \_ socket -> do
              bytes <- WS.receiveData socket
              frame <- either (const (assertFailure "Invalid authentication request")) pure (eitherDecode bytes)
              WS.sendTextData socket (encode (reply (frameId frame) (object ["userId" .= String "user", "orgId" .= String "org"])))
              takeMVar ready
              WS.sendCloseCode socket 4005 ("fixture close reason" :: Text)
          )
          $ \target -> do
            let options = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "offline") ".") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing, WebSocket.webSocketCloseTimeoutMicros = 100000}}
            Daemon.withConnection options $ \connection -> do
              Daemon.getTransportKind connection @?= WebSocketTransport
              void $ Daemon.onConnectionClose connection $ \cause -> do
                case cause >>= fromException of
                  Just details -> do
                    WebSocket.webSocketCloseCode details @?= 4005
                    WebSocket.webSocketCloseReason details @?= "fixture close reason"
                  Nothing -> assertFailure "Lost WebSocket close details"
                modifyIORef' count (+ 1)
                putMVar closed ()
              putMVar ready ()
              takeMVar closed
            readIORef count >>= (@?= 1)
    ]

context :: JsonRpcEnvelope
context = WithEnvelope (Just "1.205.0") Nothing mempty

scoped :: Text -> Text -> JsonRpcBaseRequest
scoped identifier session = (request identifier) {envelopeBody = (envelopeBody (request identifier)) {baseRequestParams = Just (Object (KeyMap.singleton "sessionId" (String session)))}}

withIntake :: RpcChannel -> IO a -> IO a
withIntake channel action = withRpcDispatcher channel context (const action)

complete :: RpcChannel -> TQueue (Either PeerFailure Object) -> TQueue Object -> JsonRpcBaseRequest -> IO ()
complete channel incoming sent frame = withAsync (requestReply channel Nothing frame) $ \worker -> do
  identifier <- nextId sent
  feed incoming (reply identifier Null)
  void (wait worker)

nextId :: TQueue Object -> IO Text
nextId sent = frameId <$> atomically (readTQueue sent)

frameId :: Object -> Text
frameId frame = case KeyMap.lookup "id" frame of Just (String identifier) -> identifier; _ -> error "Missing request identity"

expectCancelled :: Either SomeException a -> IO ()
expectCancelled result = case result of Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation was lost"

clientOptions :: Daemon.DaemonClientOptions
clientOptions = Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication (GetUserInfoResult "user" "org" mempty) "offline") "."

filteredCrons :: ListCronsParams
filteredCrons = defaultListCronsParams {listCronsSessionId = Just "s"}

withDaemonMemory :: (ObjectTransport -> TQueue Object -> IO a) -> IO a
withDaemonMemory action = do
  incoming <- newTQueueIO
  sent <- newTQueueIO
  let send frame = do
        atomically (writeTQueue sent frame)
        let result = case KeyMap.lookup "method" frame of
              Just (String "daemon.logout") -> object ["accepted" .= True]
              Just (String "daemon.authenticate") -> object ["userId" .= String "user", "orgId" .= String "org"]
              Just (String "daemon.list_crons") -> object ["crons" .= ([] :: [Value])]
              Just (String "daemon.get_proxy_token") -> object ["token" .= String "offline-proxy"]
              Just (String "daemon.load_session") -> object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"]]
              Just (String "daemon.list_commands") -> object ["commands" .= ([] :: [Value])]
              _ -> error "Unexpected fixture request"
        atomically (writeTQueue incoming (reply (frameId frame) result))
  action (objectTransport send (atomically (readTQueue incoming))) sent
