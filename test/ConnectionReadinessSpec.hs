{-# LANGUAGE OverloadedStrings #-}

module ConnectionReadinessSpec (connectionReadinessTests) where

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.Exception (Exception, catch, fromException, throwIO, toException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid.Connection qualified as Connection
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Transport.Relay qualified as Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

connectionReadinessTests :: TestTree
connectionReadinessTests =
  testGroup
    "Connection readiness"
    [ testCase "logout and in-place authentication preserve transport and update session tokens" $ bounded $ do
        frames <- newIORef []
        withScript (serve (\socket request -> modifyIORef' frames (<> [request]) >> normalReply socket request)) $ \options ->
          Daemon.withConnection options $ \connection -> do
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True True)
            void (Daemon.logout connection)
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True False)
            Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "renewed"))
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True True)
            Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey ""))
            void (Daemon.loadSessionInfo connection "saved")
        requests <- readIORef frames
        map (field "method") requests @?= map String ["daemon.authenticate", "daemon.logout", "daemon.authenticate", "daemon.load_session"]
        case reverse requests of request : _ -> field "token" (params request) @?= String "renewed"; _ -> assertFailure "No session load",
      testCase "authentication waiters coalesce and waiter cancellation leaves the owner alive" $ bounded $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        authCalls <- newIORef 0
        let peer = serve $ \conn request ->
              if field "method" request == String "daemon.authenticate"
                then do
                  count <- next authCalls
                  when (count == 2) (putMVar started () >> takeMVar release)
                  reply conn request identity
                else normalReply conn request
        withScript peer $ \options ->
          Daemon.withConnection options $ \connection -> do
            void (Daemon.logout connection)
            let authenticate = Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "renewed"))
            withAsync authenticate $ \owner -> do
              takeMVar started
              withAsync authenticate $ \waiter -> do
                timeout 10000 (void (waitCatch waiter)) >>= (@?= Nothing)
                cancel waiter
                waitCatch waiter >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled auth waiter returned"
              putMVar release ()
              waitCatch owner >>= either throwIO pure
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True True)
        readIORef authCalls >>= (@?= 2),
      testCase "identity mismatch retires the old logical connection instead of adopting a principal" $ bounded $ do
        authCalls <- newIORef 0
        withScript
          ( serve
              ( \socket request ->
                  if field "method" request == String "daemon.authenticate"
                    then do
                      count <- next authCalls
                      reply socket request (if count == 1 then identity else object ["userId" .= String "other", "orgId" .= String "org"])
                    else normalReply socket request
              )
          )
          $ \options ->
            Daemon.withConnection options $ \connection -> do
              let original = Daemon.connectionUser connection
              void (Daemon.logout connection)
              try @Daemon.DaemonError (Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "other"))) >>= (@?= Left Daemon.DaemonIdentityMismatch)
              Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth False False)
              Daemon.connectionUser connection @?= original,
      testCase "a later logout cannot be overwritten by an earlier authentication receipt" $ bounded $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        let peer socket = do
              request <- receive socket
              reply socket request identity
              logout <- receive socket
              reply socket logout (object ["accepted" .= True])
              auth <- receive socket
              putMVar started ()
              later <- receive socket
              reply socket later (object ["accepted" .= True])
              takeMVar release
              reply socket auth identity
              keepOpen socket
        withScript peer $ \options ->
          Daemon.withConnection options $ \connection -> do
            void (Daemon.logout connection)
            withAsync (Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "renewed"))) $ \owner -> do
              takeMVar started
              void (Daemon.logout connection)
              putMVar release ()
              waitCatch owner >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonAuthenticationSuperseded; Right _ -> assertFailure "Stale authentication succeeded"
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth False False),
      testCase "failed in-place authentication leaves no usable authority" $ bounded $ do
        authCalls <- newIORef 0
        withScript
          ( serve
              ( \socket request ->
                  if field "method" request == String "daemon.authenticate"
                    then do
                      count <- next authCalls
                      if count == 1 then reply socket request identity else reject socket request (-32001)
                    else normalReply socket request
              )
          )
          $ \options ->
            Daemon.withConnection options $ \connection -> do
              void (Daemon.logout connection)
              try @RpcResultError (Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "bad"))) >>= \case Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Authentication rejection lost"
              Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth False False),
      testCase "coalesced polling exposes transport-before-auth and ignores follower options" $ bounded $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        progress <- newIORef []
        calls <- newIORef 0
        withScript
          ( serve
              ( \socket request -> do
                  void (next calls)
                  putMVar started ()
                  takeMVar release
                  reply socket request identity
              )
          )
          $ \options ->
            Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
              initial <- Connection.getConnectionStatus controller
              Connection.connectionReady initial @?= False
              let settings = fastPoll {Connection.connectionPollProgress = Just (\value -> modifyIORef' progress (<> [value]))}
              withAsync (Connection.pollUntilConnected controller settings) $ \owner -> do
                takeMVar started
                status <- Connection.getConnectionStatus controller
                Connection.connectionStatusHealth status @?= Daemon.ConnectionHealth True False
                Connection.connectionStatusPolling status @?= True
                stop <- newEmptyTMVarIO
                atomically (putTMVar stop (toException ReadinessAbort))
                withAsync (Connection.pollUntilConnected controller (fastPoll {Connection.connectionPollAbort = Just (readTMVar stop), Connection.connectionPollProgress = Just (const (assertFailure "Follower callback ran"))})) $ \follower -> do
                  timeout 10000 (void (waitCatch follower)) >>= (@?= Nothing)
                  putMVar release ()
                  waitCatch owner >>= either throwIO (@?= True)
                  waitCatch follower >>= either throwIO (@?= True)
              Connection.getConnectionStatus controller >>= (@?= True) . Connection.connectionReady
        readIORef calls >>= (@?= 1)
        readIORef progress >>= (@?= [Connection.ConnectionAttemptProgress 1 15]),
      testCase "ready state is reused and logout repairs in place without ensure-running again" $ bounded $ do
        ensured <- newIORef (0 :: Int)
        authCalls <- newIORef 0
        withScript
          ( serve
              ( \socket request -> do
                  when (field "method" request == String "daemon.authenticate") (void (next authCalls))
                  normalReply socket request
              )
          )
          $ \options -> do
            let plan = (Connection.daemonConnectionPlan options) {Connection.ensurePlannedRunning = Just (modifyIORef' ensured (+ 1))}
            Connection.withConnectionController plan $ \controller -> do
              Connection.pollUntilConnected controller fastPoll >>= (@?= True)
              Connection.pollUntilConnected controller fastPoll >>= (@?= True)
              Connection.withReadyConnection controller fastPoll Daemon.logout >>= \case Just _ -> pure (); Nothing -> assertFailure "Connection disappeared"
              status <- Connection.getConnectionStatus controller
              Connection.connectionStatusHealth status @?= Daemon.ConnectionHealth True False
              Connection.pollUntilConnected controller fastPoll >>= (@?= True)
              Connection.getConnectionStatus controller >>= (@?= True) . Connection.connectionReady
        readIORef ensured >>= (@?= 1)
        readIORef authCalls >>= (@?= 2),
      testCase "pre-spawn exemptions and first-transport auth extension preserve progress budgets" $ bounded $ do
        attempts <- newIORef 0
        ensured <- newIORef (0 :: Int)
        progress <- newIORef []
        let plan =
              Connection.ConnectionPlan
                { Connection.withPlannedConnection = \opened action -> do
                    count <- next attempts
                    if count <= 2
                      then throwIO (Connection.ConnectionFailure "daemon_not_spawned" True True Nothing)
                      else withScript (serve (\socket request -> if count < 5 then reject socket request (-32000) else normalReply socket request)) $ \options -> Daemon.withConnectionObserved options opened action,
                  Connection.repairPlannedAuthentication = \_ -> assertFailure "Unexpected repair",
                  Connection.ensurePlannedRunning = Just (modifyIORef' ensured (+ 1)),
                  Connection.classifyPlannedFailure = Connection.classifyConnectionFailure
                }
            settings = fastPoll {Connection.connectionPollAttempts = 1, Connection.connectionPollProgress = Just (\value -> modifyIORef' progress (<> [value]))}
        Connection.withConnectionController plan $ \controller -> Connection.pollUntilConnected controller settings >>= (@?= True)
        readIORef attempts >>= (@?= 5)
        readIORef ensured >>= (@?= 5)
        readIORef progress >>= (@?= [Connection.ConnectionAttemptProgress 1 1, Connection.ConnectionAttemptProgress 1 1, Connection.ConnectionAttemptProgress 1 1, Connection.ConnectionAttemptProgress 2 4, Connection.ConnectionAttemptProgress 3 4]),
      testCase "expired pre-spawn grace counts failures and stays bounded" $ bounded $ do
        attempts <- newIORef 0
        let plan = failingPlan attempts (Connection.ConnectionFailure "daemon_not_spawned" True True Nothing)
            settings = fastPoll {Connection.connectionPollAttempts = 2, Connection.connectionPreSpawnGraceMicros = 0, Connection.connectionExtraAuthAttempts = 0, Connection.connectionPollRecovery = True}
        Connection.withConnectionController plan $ \controller -> do
          Connection.pollUntilConnected controller settings >>= (@?= False)
          status <- Connection.getConnectionStatus controller
          Connection.connectionStatusPolling status @?= False
          Connection.connectionStatusRecovery status @?= Just False
          Connection.connectionRetryAllowed status @?= True
        readIORef attempts >>= (@?= 2),
      testCase "wall-clock expiry removes the pre-spawn exemption" $ bounded $ do
        attempts <- newIORef 0
        let plan = (failingPlan attempts (Connection.ConnectionFailure "daemon_not_spawned" True True Nothing)) {Connection.ensurePlannedRunning = Just (threadDelay 5000)}
        Connection.withConnectionController plan $ \controller ->
          Connection.pollUntilConnected controller (fastPoll {Connection.connectionPollAttempts = 1, Connection.connectionPreSpawnGraceMicros = 1000, Connection.connectionExtraAuthAttempts = 0}) >>= (@?= False)
        readIORef attempts >>= (@?= 1),
      testCase "permanent failures stop and progress callback errors remain observable without aborting" $ bounded $ do
        attempts <- newIORef 0
        Connection.withConnectionController (failingPlan attempts (Connection.ConnectionFailure "auth_rejected" False False Nothing)) $ \controller -> do
          Connection.pollUntilConnected controller fastPoll >>= (@?= False)
          status <- Connection.getConnectionStatus controller
          Connection.connectionRetryAllowed status @?= False
        readIORef attempts >>= (@?= 1)
        withScript (serve normalReply) $ \options ->
          Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
            Connection.pollUntilConnected controller (fastPoll {Connection.connectionPollProgress = Just (const (throwIO ObserverFailure))}) >>= (@?= True)
            status <- Connection.getConnectionStatus controller
            (Connection.connectionStatusObserverFailure status >>= fromException) @?= Just ObserverFailure,
      testCase "abort completes the shared poll without another attempt and clears recovery" $ bounded $ do
        failed <- newEmptyMVar
        stop <- newEmptyTMVarIO
        attempts <- newIORef 0
        let plan = (failingPlan attempts (Connection.ConnectionFailure "temporary" True False Nothing)) {Connection.withPlannedConnection = \_ _ -> next attempts >> putMVar failed () >> throwIO (Connection.ConnectionFailure "temporary" True False Nothing)}
            settings = fastPoll {Connection.connectionPollIntervalMicros = 60000000, Connection.connectionPollAbort = Just (readTMVar stop), Connection.connectionPollRecovery = True}
        Connection.withConnectionController plan $ \controller ->
          withAsync (Connection.pollUntilConnected controller settings) $ \owner -> do
            takeMVar failed
            atomically (putTMVar stop (toException ReadinessAbort))
            waitCatch owner >>= \case Left cause -> fromException cause @?= Just ReadinessAbort; Right _ -> assertFailure "Abort result lost"
            status <- Connection.getConnectionStatus controller
            Connection.connectionStatusPolling status @?= False
            Connection.connectionStatusRecovery status @?= Nothing
        readIORef attempts >>= (@?= 1),
      testCase "session readiness remains in the existing daemon state rather than the connection poller" $
        bounded $
          withScript (serve normalReply) $ \options ->
            Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
              result <- Connection.withReadyConnection controller fastPoll $ \connection ->
                Daemon.withResumedSessionOn connection "saved" $ \_ -> do
                  readiness <- Daemon.getSessionReadiness connection "saved"
                  Daemon.readinessPhase readiness @?= Daemon.SessionLoaded
              result @?= Just (),
      testCase "closed controller status is inert and later polling cannot reopen it" $ bounded $ do
        attempts <- newIORef 0
        escaped <- Connection.withConnectionController (failingPlan attempts (Connection.ConnectionFailure "unused" False False Nothing)) pure
        status <- Connection.getConnectionStatus escaped
        Connection.connectionReady status @?= False
        try @Connection.ConnectionError (Connection.pollUntilConnected escaped fastPoll) >>= (@?= Left Connection.ConnectionControllerClosed)
        readIORef attempts >>= (@?= 0),
      testCase "old connection teardown cannot consume or execute a queued poll twice" $ bounded $ do
        attempts <- newIORef 0
        losePeer <- newEmptyMVar
        cleanupStarted <- newEmptyMVar
        finishCleanup <- newEmptyMVar
        let plan =
              Connection.ConnectionPlan
                { Connection.withPlannedConnection = \opened action -> do
                    count <- next attempts
                    let peer socket = do
                          request <- receive socket
                          reply socket request identity
                          when (count == 1) (takeMVar losePeer >> WS.sendCloseCode socket 4001 ("lost" :: Text))
                          keepOpen socket
                    withScript peer $ \options -> do
                      result <- Daemon.withConnectionObserved options opened action
                      if count == 1 then putMVar cleanupStarted () >> takeMVar finishCleanup >> throwIO CleanupFailure else pure result,
                  Connection.repairPlannedAuthentication = \_ -> assertFailure "Unexpected repair",
                  Connection.ensurePlannedRunning = Nothing,
                  Connection.classifyPlannedFailure = \cause -> case fromException cause of Just CleanupFailure -> Connection.ConnectionFailure "cleanup" False False (Just cause); Nothing -> Connection.classifyConnectionFailure cause
                }
        Connection.withConnectionController plan $ \controller -> do
          Connection.pollUntilConnected controller fastPoll >>= (@?= True)
          putMVar losePeer ()
          takeMVar cleanupStarted
          withAsync (Connection.pollUntilConnected controller (fastPoll {Connection.connectionPollAttempts = 1, Connection.connectionExtraAuthAttempts = 0})) $ \queued -> do
            void (awaitStatus controller Connection.connectionStatusPolling)
            putMVar finishCleanup ()
            waitCatch queued >>= either throwIO (@?= True)
          readIORef attempts >>= (@?= 2),
      testCase "cancelling the first poll waiter does not cancel the controller-owned acquisition" $ bounded $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        calls <- newIORef 0
        withScript
          ( serve
              ( \socket request -> do
                  void (next calls)
                  putMVar started ()
                  takeMVar release
                  reply socket request identity
              )
          )
          $ \options ->
            Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
              withAsync (Connection.pollUntilConnected controller fastPoll) $ \owner -> do
                takeMVar started
                cancel owner
                waitCatch owner >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled waiter returned"
              withAsync (Connection.pollUntilConnected controller fastPoll) $ \follower -> do
                putMVar release ()
                waitCatch follower >>= either throwIO (@?= True)
        readIORef calls >>= (@?= 1),
      testCase "peer loss is observed and a replacement cannot silently change identity" $ bounded $ do
        attempts <- newIORef 0
        losePeer <- newEmptyMVar
        let plan =
              Connection.ConnectionPlan
                { Connection.withPlannedConnection = \opened action -> do
                    count <- next attempts
                    withScript
                      ( \socket -> do
                          request <- receive socket
                          reply socket request (if count == 1 then identity else object ["userId" .= String "other", "orgId" .= String "org"])
                          when (count == 1) (takeMVar losePeer >> WS.sendCloseCode socket 4001 ("lost" :: Text))
                          keepOpen socket
                      )
                      $ \options -> Daemon.withConnectionObserved options opened action,
                  Connection.repairPlannedAuthentication = \_ -> assertFailure "Unexpected repair",
                  Connection.ensurePlannedRunning = Nothing,
                  Connection.classifyPlannedFailure = Connection.classifyConnectionFailure
                }
        Connection.withConnectionController plan $ \controller -> do
          Connection.pollUntilConnected controller fastPoll >>= (@?= True)
          putMVar losePeer ()
          void (awaitStatus controller (not . Daemon.healthTransportConnected . Connection.connectionStatusHealth))
          Connection.pollUntilConnected controller fastPoll >>= (@?= False)
          status <- Connection.getConnectionStatus controller
          (Connection.connectionFailureReason <$> Connection.connectionStatusFailure status) @?= Just "identity_mismatch"
          Connection.connectionRetryAllowed status @?= False
        readIORef attempts >>= (@?= 2),
      testCase "relay readiness uses both authentication layers before exposing a daemon"
        $ bounded
        $ withScript
          ( \socket -> do
              relay <- receive socket
              field "method" relay @?= String "relay.authenticate"
              field "activeOrganizationId" relay @?= String "org"
              WS.sendTextData socket (encode (object ["type" .= String "relay.auth_ok"]))
              request <- receive socket
              field "method" request @?= String "daemon.authenticate"
              reply socket request identity
              keepOpen socket
          )
        $ \options -> do
          let plan = Connection.relayConnectionPlan (Daemon.daemonTransport options) (Daemon.daemonTarget options) Relay.defaultRelayOptions (Relay.RelayTokenProvider (pure (Just "token")) (pure (Just "org")) Nothing) (`Daemon.defaultDaemonClientOptions` "/workspace")
          Connection.withConnectionController plan $ \controller -> do
            Connection.attemptInitialConnection controller
            Connection.getConnectionStatus controller >>= (@?= True) . Connection.connectionReady,
      testCase "closing the controller settles a pending poll and releases its native scope" $ bounded $ do
        shared <- newEmptyMVar
        started <- newEmptyMVar
        withAsync (takeMVar shared >>= \controller -> Connection.pollUntilConnected controller fastPoll) $ \waiter -> do
          withScript (\socket -> void (receive socket) >> putMVar started () >> keepOpen socket) $ \options ->
            Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
              putMVar shared controller
              takeMVar started
          waitCatch waiter >>= \case Left cause -> fromException cause @?= Just Connection.ConnectionControllerClosed; Right _ -> assertFailure "Closed poll published a connection",
      testCase "caller work stays on its thread and is not replayed as connection setup" $ bounded $ do
        calls <- newIORef (0 :: Int)
        caller <- myThreadId
        withScript (serve normalReply) $ \options ->
          Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
            try @ObserverFailure
              ( Connection.withReadyConnection
                  controller
                  fastPoll
                  ( \_ -> do
                      myThreadId >>= (@?= caller)
                      modifyIORef' calls (+ 1)
                      throwIO ObserverFailure
                  ) ::
                  IO (Maybe ())
              )
              >>= (@?= Left ObserverFailure)
            Connection.getConnectionStatus controller >>= (@?= True) . Connection.connectionReady
        readIORef calls >>= (@?= 1),
      testCase "invalid poll options cannot enqueue work" $ bounded $ do
        calls <- newIORef 0
        Connection.withConnectionController (failingPlan calls (Connection.ConnectionFailure "unused" False False Nothing)) $ \controller ->
          forM_ [fastPoll {Connection.connectionPollAttempts = -1}, fastPoll {Connection.connectionPollIntervalMicros = -1}, fastPoll {Connection.connectionExtraAuthAttempts = -1}, fastPoll {Connection.connectionPreSpawnGraceMicros = -1}] $ \settings ->
            try @Connection.ConnectionError (Connection.pollUntilConnected controller settings) >>= (@?= Left Connection.InvalidConnectionPollOptions)
        readIORef calls >>= (@?= 0),
      testCase "a zero poll budget returns not ready without attempts or callbacks" $ bounded $ do
        calls <- newIORef 0
        Connection.withConnectionController (failingPlan calls (Connection.ConnectionFailure "unused" False False Nothing)) $ \controller -> do
          Connection.pollUntilConnected controller (fastPoll {Connection.connectionPollAttempts = 0, Connection.connectionPollProgress = Just (const (assertFailure "Zero budget notified an attempt"))}) >>= (@?= False)
          Connection.getConnectionStatus controller >>= (@?= False) . Connection.connectionStatusPolling
        readIORef calls >>= (@?= 0),
      testCase "cancelling in-place authentication retires its uncertain connection" $ bounded $ do
        started <- newEmptyMVar
        calls <- newIORef 0
        withScript
          ( serve
              ( \socket request ->
                  if field "method" request == String "daemon.authenticate"
                    then do
                      count <- next calls
                      when (count == 2) (putMVar started () >> keepOpen socket)
                      reply socket request identity
                    else normalReply socket request
              )
          )
          $ \options ->
            Daemon.withConnection options $ \connection -> do
              void (Daemon.logout connection)
              withAsync (Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "new"))) $ \owner -> do
                takeMVar started
                cancel owner
                waitCatch owner >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled auth owner returned"
              Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth False False),
      testCase "observed close status retains its permanent failure classification" $ bounded $ do
        closePeer <- newEmptyMVar
        let peer socket = do
              request <- receive socket
              reply socket request identity
              takeMVar closePeer
              WS.sendCloseCode socket 4005 ("unauthorized" :: Text)
              keepOpen socket
        withScript peer $ \options ->
          Connection.withConnectionController (Connection.daemonConnectionPlan options) $ \controller -> do
            Connection.pollUntilConnected controller fastPoll >>= (@?= True)
            putMVar closePeer ()
            status <- awaitStatus controller (\value -> case Connection.connectionStatusFailure value of Just _ -> True; Nothing -> False)
            (Connection.connectionFailureReason <$> Connection.connectionStatusFailure status) @?= Just "relay_unauthorized"
            Connection.connectionRetryAllowed status @?= False,
      testCase "close-code classification preserves known retry and permanence decisions" $
        forM_ [(4000, "computer_offline", True), (4001, "computer_disconnected", True), (4006, "computer_disconnected", True), (4007, "computer_disconnected", True), (4004, "relay_timeout", True), (4005, "relay_unauthorized", False), (4009, "relay_gateway_error", True), (4011, "relay_rate_limited", True), (4010, "daemon_conflict", False), (4012, "daemon_shutting_down", True), (4013, "daemon_updating", True), (4014, "client_orphaned", True), (4015, "grace_buffer_overflow", True), (1000, "connection_lost", True)] $ \(code, reason, retryable) -> do
          let failure = Connection.classifyConnectionFailure (toException (WebSocket.WebSocketPeerClosed code))
          Connection.connectionFailureReason failure @?= reason
          Connection.connectionFailureRetryable failure @?= retryable
    ]

data CleanupFailure = CleanupFailure deriving stock (Eq, Show)

instance Exception CleanupFailure

data ReadinessAbort = ReadinessAbort deriving stock (Eq, Show)

instance Exception ReadinessAbort

data ObserverFailure = ObserverFailure deriving stock (Eq, Show)

instance Exception ObserverFailure

fastPoll :: Connection.ConnectionPollOptions
fastPoll = Connection.defaultConnectionPollOptions {Connection.connectionPollIntervalMicros = 0}

next :: IORef Int -> IO Int
next counter = atomicModifyIORef' counter (\value -> let updated = value + 1 in (updated, updated))

failingPlan :: IORef Int -> Connection.ConnectionFailure -> Connection.ConnectionPlan
failingPlan attempts failure = Connection.ConnectionPlan (\_ _ -> next attempts >> throwIO failure) (\_ -> assertFailure "Unexpected repair") Nothing Connection.classifyConnectionFailure

withScript :: (WS.Connection -> IO ()) -> (Daemon.DaemonOptions -> IO a) -> IO a
withScript script action = withPeer (\_ socket -> script socket `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
  action ((Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "initial") "/workspace") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing, WebSocket.webSocketConnectTimeoutMicros = 1000000, WebSocket.webSocketCloseTimeoutMicros = 100000}, Daemon.daemonRestoreTerminalsOnLoad = False})

serve :: (WS.Connection -> Object -> IO ()) -> WS.Connection -> IO ()
serve handler socket = forever (receive socket >>= handler socket)

receive :: WS.Connection -> IO Object
receive socket = WS.receiveData socket >>= either (const (assertFailure "Invalid native peer JSON")) pure . eitherDecode

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

params :: Object -> Object
params request = case field "params" request of Object value -> value; _ -> error "Missing params"

identity :: Value
identity = object ["userId" .= String "user", "orgId" .= String "org"]

reply :: WS.Connection -> Object -> Value -> IO ()
reply socket request result = send socket request ["result" .= result]

reject :: WS.Connection -> Object -> Int -> IO ()
reject socket request code = send socket request ["error" .= object ["code" .= code, "message" .= String "rejected"]]

send :: WS.Connection -> Object -> [Data.Aeson.Types.Pair] -> IO ()
send socket request fields = WS.sendTextData socket (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "response", "id" .= field "id" request] <> fields)))

normalReply :: WS.Connection -> Object -> IO ()
normalReply socket request = case field "method" request of
  String "daemon.authenticate" -> reply socket request identity
  String "daemon.logout" -> reply socket request (object ["accepted" .= True])
  String "daemon.load_session" -> reply socket request (object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]])
  _ -> assertFailure "Unexpected readiness RPC or implicit mutation"

keepOpen :: WS.Connection -> IO ()
keepOpen socket = forever (void (WS.receiveDataMessage socket))

awaitStatus :: Connection.ConnectionController -> (Connection.ConnectionStatus -> Bool) -> IO Connection.ConnectionStatus
awaitStatus controller predicate = Connection.getConnectionStatus controller >>= loop
  where
    loop status | predicate status = pure status
    loop status = Connection.waitConnectionStatusChange controller status >>= loop
