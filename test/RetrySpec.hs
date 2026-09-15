{-# LANGUAGE OverloadedStrings #-}

module RetrySpec (retryTests) where

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.Exception (Exception, bracket_, catch, fromException, throwIO, toException, try)
import Control.Monad (forM_, unless, void)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Retry qualified as Retry
import Factory.Droid.Transport (ObjectTransport (..))
import Factory.Droid.Transport.Relay qualified as Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

retryTests :: TestTree
retryTests =
  testGroup
    "Connection retry policies"
    [ testCase "source profiles preserve attempts, delay indexing, caps and both jitter ranges" $ do
        Retry.retryMaxAttempts (Retry.retryPolicy Retry.defaultRetryOptions) @?= 3
        Retry.retryMaxAttempts (Retry.retryPolicy Retry.connectionRetryOptions) @?= 11
        let socket = Retry.retryPolicy Retry.webSocketRetryOptions
            reconnect = Retry.retryPolicy Retry.reconnectionRetryOptions
            half = socket {Retry.retryJitter = Retry.HalfToFullJitter}
        map (\index -> Retry.retryDelayMillis socket index 0) [0 .. 5] @?= map Right [500, 1000, 2000, 4000, 5000, 5000]
        Retry.retryDelayMillis half 0 0 @?= Right 250
        Retry.retryDelayMillis half 1 0.5 @?= Right 750
        Retry.retryDelayMillis reconnect 0 0 @?= Right 1000
        Retry.retryDelayMillis reconnect 0 0.5 @?= Right 1150
        Retry.retryDelayMillis reconnect 20 0.5 @?= Right 11500
        Retry.retryDelayBeforeFirst reconnect @?= True
        Retry.retryDelayMillis half 0 1 @?= Left Retry.InvalidRetryJitterSample
        Retry.retryDelayMillis socket (-1) 0 @?= Left Retry.InvalidRetryDelay,
      testCase "invalid policies precede work and overflow does not become a short timer" $ do
        calls <- newIORef (0 :: Int)
        let policy = Retry.retryPolicy zeroOptions
        forM_ [policy {Retry.retryMaxAttempts = 0}, policy {Retry.retryDelay = Retry.FixedDelay (-1)}, policy {Retry.retryDelay = Retry.ExponentialBackoff 1 (0 / 0) Nothing}] $ \bad ->
          try @Retry.RetryError (Retry.retry (zeroOptions {Retry.retryPolicy = bad}) (modifyIORef' calls (+ 1))) >>= (@?= Left Retry.InvalidRetryPolicy)
        readIORef calls >>= (@?= 0)
        Retry.retryDelayMillis (policy {Retry.retryDelay = Retry.ExponentialBackoff 1e308 2 Nothing}) 2 0 @?= Left Retry.InvalidRetryDelay
        Retry.retryDelayMillis (policy {Retry.retryDelay = Retry.ExponentialBackoff 1e308 2 (Just 1000)}) 2 0 @?= Right 1000,
      testCase "failure callbacks, custom delay bases and success counts retain source order" $ bounded $ do
        calls <- newIORef 0
        events <- newIORef []
        let options =
              zeroOptions
                { Retry.retryPolicy = Retry.RetryPolicy 3 (Retry.ExponentialBackoff 500 2 Nothing) Retry.HalfToFullJitter False,
                  Retry.retryOnRetry =
                    Just
                      ( \cause count -> do
                          fromException cause @?= Just (AttemptFailure count)
                          modifyIORef' events (<> ["retry" <> show count])
                      ),
                  Retry.retryGetDelay =
                    Just
                      ( \cause count base -> do
                          fromException cause @?= Just (AttemptFailure count)
                          base @?= if count == 1 then 500 else 1000
                          modifyIORef' events (<> ["delay" <> show count])
                          pure 0
                      ),
                  Retry.retryOnSuccess = Just (\count -> modifyIORef' events (<> ["success" <> show count]))
                }
        Retry.retry options (next calls >>= \count -> if count < 3 then throwIO (AttemptFailure count) else pure (7 :: Int)) >>= (@?= 7)
        readIORef events >>= (@?= ["retry1", "delay1", "retry2", "delay2", "success2"]),
      testCase "exhaustion includes the last failure callback and explicit recovery sees the last cause" $ bounded $ do
        calls <- newIORef 0
        failures <- newIORef []
        let options =
              zeroOptions
                { Retry.retryOnRetry = Just (\_ count -> modifyIORef' failures (<> [count])),
                  Retry.retryOnAllError =
                    Just
                      ( \cause -> do
                          fromException cause @?= Just (AttemptFailure 3)
                          pure (42 :: Int)
                      )
                }
        Retry.retry options (next calls >>= throwIO . AttemptFailure) >>= (@?= 42)
        readIORef failures >>= (@?= [1, 2, 3]),
      testCase "a permanent predicate stops without retry notification and hook exceptions are not retried" $ bounded $ do
        calls <- newIORef 0
        let options =
              zeroOptions
                { Retry.retryPredicate = const (pure False),
                  Retry.retryOnRetry = Just (\_ _ -> assertFailure "Permanent failure notified as retry")
                }
        try @AttemptFailure (Retry.retry options (next calls >>= throwIO . AttemptFailure) :: IO ()) >>= (@?= Left (AttemptFailure 1))
        readIORef calls >>= (@?= 1)
        forM_ [zeroOptions {Retry.retryPredicate = const (throwIO HookFailure)}, zeroOptions {Retry.retryOnRetry = Just (\_ _ -> throwIO HookFailure)}, zeroOptions {Retry.retryGetDelay = Just (\_ _ _ -> throwIO HookFailure)}] $ \hooks -> do
          count <- newIORef 0
          try @HookFailure (Retry.retry hooks (next count >>= throwIO . AttemptFailure) :: IO ()) >>= (@?= Left HookFailure)
          readIORef count >>= (@?= 1),
      testCase "a success hook cannot turn completed work into another attempt" $ bounded $ do
        calls <- newIORef 0
        let options = zeroOptions {Retry.retryOnSuccess = Just (const (throwIO HookFailure)), Retry.retryOnAllError = Just (const (assertFailure "Hook failure became exhaustion"))}
        try @HookFailure (Retry.retry options (next calls)) >>= (@?= Left HookFailure)
        readIORef calls >>= (@?= 1),
      testCase "invalid custom delays fail without replay or implicit recovery" $ bounded $ do
        forM_ [-1, 0 / 0, 1 / 0] $ \delay -> do
          calls <- newIORef 0
          let options = zeroOptions {Retry.retryGetDelay = Just (\_ _ _ -> pure delay)}
          try @Retry.RetryError (Retry.retry options (next calls >>= throwIO . AttemptFailure) :: IO ()) >>= (@?= Left Retry.InvalidRetryDelay)
          readIORef calls >>= (@?= 1),
      testCase "pre-abort and abort after the last failure preserve the exact reason" $ bounded $ do
        stop <- newEmptyTMVarIO
        atomically (putTMVar stop (toException AbortReason))
        let options = zeroOptions {Retry.retryAbort = Just (readTMVar stop), Retry.retryOnAllError = Just (const (assertFailure "Aborted operation recovered"))}
        try @AbortReason (Retry.retry options (assertFailure "Pre-aborted action started") :: IO ()) >>= (@?= Left AbortReason)
        later <- newEmptyTMVarIO
        let finalOptions =
              zeroOptions
                { Retry.retryPolicy = (Retry.retryPolicy zeroOptions) {Retry.retryMaxAttempts = 1},
                  Retry.retryAbort = Just (readTMVar later),
                  Retry.retryOnRetry = Just (\_ _ -> atomically (putTMVar later (toException AbortReason))),
                  Retry.retryOnAllError = Just (const (assertFailure "Abort lost to final recovery"))
                }
        try @AbortReason (Retry.retry finalOptions (throwIO (AttemptFailure 1)) :: IO ()) >>= (@?= Left AbortReason),
      testCase "abort interrupts backoff and initial reconnect delay without a later attempt" $ bounded $ do
        forM_ [False, True] $ \beforeFirst -> do
          stop <- newEmptyTMVarIO
          calls <- newIORef 0
          started <- newEmptyMVar
          let options = zeroOptions {Retry.retryPolicy = Retry.RetryPolicy 3 (Retry.FixedDelay 60000) Retry.NoJitter beforeFirst, Retry.retryAbort = Just (readTMVar stop), Retry.retryOnRetry = Just (\_ _ -> putMVar started ())}
          withAsync (Retry.retry options (next calls >>= throwIO . AttemptFailure) :: IO ()) $ \worker -> do
            unless beforeFirst (takeMVar started)
            timeout 10000 (void (waitCatch worker)) >>= (@?= Nothing)
            atomically (putTMVar stop (toException AbortReason))
            waitCatch worker >>= \case Left cause -> fromException cause @?= Just AbortReason; Right _ -> assertFailure "Aborted retry returned"
          readIORef calls >>= (@?= if beforeFirst then 0 else 1),
      testCase "in-flight work is not rolled back but an observed abort prevents publication" $ bounded $ do
        started <- newEmptyMVar
        finish <- newEmptyMVar
        stop <- newEmptyTMVarIO
        let options = zeroOptions {Retry.retryAbort = Just (readTMVar stop), Retry.retryOnSuccess = Just (const (assertFailure "Aborted result was published"))}
        withAsync (Retry.retry options (putMVar started () >> takeMVar finish)) $ \worker -> do
          takeMVar started
          atomically (putTMVar stop (toException AbortReason))
          timeout 10000 (void (waitCatch worker)) >>= (@?= Nothing)
          putMVar finish ()
          waitCatch worker >>= \case Left cause -> fromException cause @?= Just AbortReason; Right _ -> assertFailure "Abort lost after work completed",
      testCase "standard asynchronous cancellation never enters retry callbacks" $ bounded $ do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        let options = zeroOptions {Retry.retryOnRetry = Just (\_ _ -> assertFailure "Cancellation retried")}
        withAsync (Retry.retry options (putMVar started () >> takeMVar blocked) :: IO ()) $ \worker -> do
          takeMVar started
          cancel worker
          waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled retry returned",
      testCase "fresh relay setup retries release each failed scope before the next attempt" $ bounded $ do
        attempts <- newIORef 0
        closed <- newIORef 0
        publications <- newIORef (0 :: Int)
        let options =
              zeroOptions
                { Retry.retryPredicate = \cause -> pure $ case fromException cause of Just (Relay.RelayAuthenticationRejected _) -> True; _ -> False,
                  Retry.retryOnRetry = Just (\_ count -> readIORef closed >>= (@?= count))
                }
            acquire action = do
              count <- next attempts
              withPeer (\_ socket -> setupPeer (count == 3) socket `catch` \(_ :: WS.ConnectionException) -> modifyIORef' closed (+ 1)) $ \target ->
                Relay.withRelayConnection plain target Relay.defaultRelayOptions (Relay.RelayApiKey "key") action
        Retry.withConnectionRetries options acquire (\_ -> modifyIORef' publications (+ 1))
        readIORef attempts >>= (@?= 3)
        readIORef publications >>= (@?= 1)
        readIORef closed >>= (@?= 3),
      testCase "an entered relay callback and its transmitted work cannot be replayed" $ bounded $ do
        attempts <- newIORef 0
        effects <- newIORef (0 :: Int)
        original <- myThreadId
        let options = zeroOptions {Retry.retryOnAllError = Just (const (assertFailure "Caller failure became setup recovery"))}
            acquire action = do
              void (next attempts)
              withPeer
                ( \_ socket -> do
                    void (receiveObject socket)
                    authOk socket
                    receiveObject socket >>= (@?= fields ["userWork" .= True])
                    modifyIORef' effects (+ 1)
                    WS.sendTextData socket (encode (object ["accepted" .= True]))
                    keepOpen socket `catch` \(_ :: WS.ConnectionException) -> pure ()
                )
                $ \target -> Relay.withRelayConnection plain target Relay.defaultRelayOptions (Relay.RelayApiKey "key") action
        result <- try @HookFailure $ Retry.withConnectionRetries options acquire $ \relay -> do
          myThreadId >>= (@?= original)
          transportSendObject (Relay.relayTransport relay) (fields ["userWork" .= True])
          transportReceiveObject (Relay.relayTransport relay) >>= (@?= fields ["accepted" .= True])
          throwIO HookFailure :: IO ()
        result @?= Left HookFailure
        readIORef attempts >>= (@?= 1)
        readIORef effects >>= (@?= 1),
      testCase "cleanup failure after publication never restarts acquisition" $ bounded $ do
        attempts <- newIORef 0
        callbacks <- newIORef (0 :: Int)
        let acquire action = bracket_ (void (next attempts)) (throwIO HookFailure) (action ())
        try @HookFailure (Retry.withConnectionRetries zeroOptions acquire (\() -> modifyIORef' callbacks (+ 1))) >>= (@?= Left HookFailure)
        readIORef attempts >>= (@?= 1)
        readIORef callbacks >>= (@?= 1),
      testCase "delegated reconnection selects only the external owner without local delays or retry" $ bounded $ do
        local <- newIORef (0 :: Int)
        delegated <- newIORef (0 :: Int)
        let options = Retry.reconnectionRetryOptions {Retry.retryPolicy = Retry.RetryPolicy 9 (Retry.FixedDelay 60000) Retry.PositiveReconnectJitter True}
            owned _ = modifyIORef' local (+ 1) >> assertFailure "Delegation used local reconnect"
            external action = modifyIORef' delegated (+ 1) >> action (7 :: Int)
        Retry.withReconnection options Retry.ReconnectDelegated owned external pure >>= (@?= 7)
        readIORef local >>= (@?= 0)
        readIORef delegated >>= (@?= 1)
        failures <- newIORef 0
        try @AttemptFailure (Retry.withReconnection options Retry.ReconnectDelegated owned (\_ -> next failures >> throwIO (AttemptFailure 9)) pure) >>= (@?= Left (AttemptFailure 9))
        readIORef failures >>= (@?= 1),
      testCase "normal daemon authentication retries before publishing one connection" $ bounded $ do
        attempts <- newIORef 0
        callbacks <- newIORef (0 :: Int)
        let acquire action = do
              count <- next attempts
              withPeer
                ( \_ socket -> do
                    request <- receiveObject socket
                    KeyMap.lookup "method" request @?= Just (String "daemon.authenticate")
                    let envelope = ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "response", "id" .= KeyMap.lookup "id" request]
                        outcome = if count == 1 then ["error" .= object ["code" .= (-32000 :: Int), "message" .= String "temporary"]] else ["result" .= object ["userId" .= String "user", "orgId" .= String "org"]]
                    WS.sendTextData socket (encode (object (envelope <> outcome)))
                    keepOpen socket `catch` \(_ :: WS.ConnectionException) -> pure ()
                )
                $ \target ->
                  Daemon.withConnection ((Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "key") "/workspace") {Daemon.daemonTransport = plain}) action
        Retry.withConnectionRetries zeroOptions acquire $ \connection -> do
          Daemon.isAuthenticated connection >>= (@?= True)
          toJSON (Daemon.connectionUser connection) @?= object ["userId" .= String "user", "orgId" .= String "org"]
          modifyIORef' callbacks (+ 1)
        readIORef attempts >>= (@?= 2)
        readIORef callbacks >>= (@?= 1),
      testCase "abort before scoped publication releases acquisition without entering caller work" $ bounded $ do
        stop <- newEmptyTMVarIO
        attempts <- newIORef 0
        released <- newIORef (0 :: Int)
        let options = zeroOptions {Retry.retryAbort = Just (readTMVar stop), Retry.retryOnRetry = Just (\_ _ -> assertFailure "Aborted acquisition retried")}
            acquire action = bracket_ (void (next attempts)) (modifyIORef' released (+ 1)) (atomically (putTMVar stop (toException AbortReason)) >> action ())
        try @AbortReason (Retry.withConnectionRetries options acquire (const (assertFailure "Aborted acquisition published")) :: IO ()) >>= (@?= Left AbortReason)
        readIORef attempts >>= (@?= 1)
        readIORef released >>= (@?= 1),
      testCase "scoped success-hook failure is not setup failure and cannot enter the caller" $ bounded $ do
        attempts <- newIORef 0
        released <- newIORef (0 :: Int)
        let options = zeroOptions {Retry.retryOnSuccess = Just (const (throwIO HookFailure))}
            acquire action = bracket_ (void (next attempts)) (modifyIORef' released (+ 1)) (action ())
        try @HookFailure (Retry.withConnectionRetries options acquire (const (assertFailure "Failed success hook entered caller")) :: IO ()) >>= (@?= Left HookFailure)
        readIORef attempts >>= (@?= 1)
        readIORef released >>= (@?= 1)
    ]

newtype AttemptFailure = AttemptFailure Int deriving stock (Eq, Show)

instance Exception AttemptFailure

data HookFailure = HookFailure deriving stock (Eq, Show)

instance Exception HookFailure

data AbortReason = AbortReason deriving stock (Eq, Show)

instance Exception AbortReason

zeroOptions :: Retry.RetryOptions a
zeroOptions = Retry.defaultRetryOptions {Retry.retryPolicy = Retry.RetryPolicy 3 (Retry.FixedDelay 0) Retry.NoJitter False}

next :: IORef Int -> IO Int
next counter = atomicModifyIORef' counter (\value -> let nextValue = value + 1 in (nextValue, nextValue))

plain :: WebSocket.WebSocketOptions
plain = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing, WebSocket.webSocketConnectTimeoutMicros = 1000000, WebSocket.webSocketCloseTimeoutMicros = 100000}

receiveObject :: WS.Connection -> IO Object
receiveObject socket = WS.receiveData socket >>= either (const (assertFailure "Invalid peer JSON")) pure . eitherDecode

fields :: [Data.Aeson.Types.Pair] -> Object
fields values = case object values of Object result -> result; _ -> error "Object fixture"

authOk :: WS.Connection -> IO ()
authOk socket = WS.sendTextData socket (encode (object ["type" .= String "relay.auth_ok"]))

keepOpen :: WS.Connection -> IO ()
keepOpen socket = WS.receiveDataMessage socket >> keepOpen socket

setupPeer :: Bool -> WS.Connection -> IO ()
setupPeer accepted socket = do
  void (receiveObject socket)
  if accepted then authOk socket else WS.sendTextData socket (encode (object ["type" .= String "relay.auth_error", "message" .= String "temporary", "retryable" .= True]))
  keepOpen socket
