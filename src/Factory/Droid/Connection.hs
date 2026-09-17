{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Scoped daemon connection readiness. The worker holds existing connection
-- scopes; it does not read protocol frames or own a second session runtime.
module Factory.Droid.Connection
  ( ConnectionPlan (..),
    ConnectionFailure (..),
    ConnectionPollOptions (..),
    ConnectionAttemptProgress (..),
    ConnectionStatus (..),
    ConnectionController,
    ConnectionError (..),
    defaultConnectionPollOptions,
    daemonConnectionPlan,
    daemonConnectionPlanWithState,
    relayConnectionPlan,
    relayConnectionPlanWithState,
    classifyConnectionFailure,
    ComputerConnectSliOptions (..),
    defaultComputerConnectSliOptions,
    ComputerConnectAttempt (..),
    computerConnectFailureReason,
    recordComputerConnectSli,
    withConnectionController,
    pollUntilConnected,
    attemptInitialConnection,
    withReadyConnection,
    getConnectionStatus,
    waitConnectionStatusChange,
    connectionReady,
    connectionRetryAllowed,
  )
where

import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM (STM, TMVar, TVar, atomically, check, modifyTVar', newEmptyTMVar, newTVarIO, orElse, readTMVar, readTVar, readTVarIO, retry, throwSTM, tryPutTMVar)
import Control.Exception (Exception, SomeAsyncException, SomeException, catch, evaluate, fromException, mask, throwIO, toException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Object, Value (String))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.Types qualified as UUID
import Data.UUID.V4 (nextRandom)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Internal.Exception (finallyPreserving, trySync)
import Factory.Droid.Internal.Wait qualified as Wait
import Factory.Droid.Observability qualified as Obs
import Factory.Droid.Protocol (RpcChannelError (RpcRequestTimedOut), RpcResultError (..))
import Factory.Droid.Schema.Discovery (GetUserInfoResult (reportedOrgId, reportedUserId))
import Factory.Droid.Schema.RPC (JsonRpcError (rpcErrorCode), JsonRpcErrorCode (RpcAuthenticationError))
import Factory.Droid.Transport.Relay qualified as Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import GHC.Clock (getMonotonicTimeNSec)

data ConnectionPlan = ConnectionPlan
  { withPlannedConnection :: forall a. IO () -> (Daemon.DaemonConnection -> IO a) -> IO a,
    repairPlannedAuthentication :: !(Daemon.DaemonConnection -> IO ()),
    ensurePlannedRunning :: !(Maybe (IO ())),
    classifyPlannedFailure :: !(SomeException -> ConnectionFailure)
  }

data ConnectionFailure = ConnectionFailure
  { connectionFailureReason :: !Text,
    connectionFailureRetryable :: !Bool,
    connectionFailureBeforeSpawn :: !Bool,
    connectionFailureCause :: !(Maybe SomeException)
  }

instance Show ConnectionFailure where show _ = "ConnectionFailure <redacted>"

instance Exception ConnectionFailure

-- | Explicit sinks and labels for one connect attempt. No global telemetry is
-- configured. The optional method is context supplied by a caller that knows
-- which RPC timed out; an anonymous native timeout cannot reveal that fact.
data ComputerConnectSliOptions = ComputerConnectSliOptions
  { computerConnectSurface :: !Text,
    computerConnectProvider :: !(Maybe Text),
    computerConnectAttemptTrigger :: !(Maybe Text),
    computerConnectRequestMethod :: !(Maybe Text),
    computerConnectMetricSink :: !(Maybe Obs.DroidMetricSink),
    computerConnectSpanSink :: !(Maybe Obs.DroidSpanAttributesSink)
  }

instance Show ComputerConnectSliOptions where show _ = "ComputerConnectSliOptions <redacted>"

defaultComputerConnectSliOptions :: Text -> ComputerConnectSliOptions
defaultComputerConnectSliOptions surface = ComputerConnectSliOptions surface Nothing Nothing Nothing Nothing Nothing

data ComputerConnectAttempt = ComputerConnectAttempt
  { connectAttemptId :: !Text,
    connectAttemptTrigger :: !Text,
    setConnectStartType :: !(Maybe Text -> IO ())
  }

instance Show ComputerConnectAttempt where show _ = "ComputerConnectAttempt <redacted>"

-- | Keep structured connection reasons, including caller-reported compute
-- limits, and reuse native transport/auth classification. RPC method identity
-- is never inferred from exception text. Standard async cancellation is aborted.
computerConnectFailureReason :: Maybe Text -> SomeException -> Text
computerConnectFailureReason method cause
  | Just failure <- fromException cause =
      if connectionFailureReason failure == "daemon_timeout" && method == Just "daemon.authenticate" && maybe False isRpcTimeout (connectionFailureCause failure)
        then "daemon_auth_timeout"
        else connectionFailureReason failure
  | isRpcTimeout cause = if method == Just "daemon.authenticate" then "daemon_auth_timeout" else "daemon_timeout"
  | Just (_ :: SomeAsyncException) <- fromException cause = "aborted"
  | otherwise = connectionFailureReason (classifyConnectionFailure cause)

isRpcTimeout :: SomeException -> Bool
isRpcTimeout cause = case fromException cause of Just RpcRequestTimedOut -> True; _ -> False

-- | Observe one operation and evaluate its success predicate. False records
-- not_fully_connected but returns the same result; action/predicate exceptions
-- are classified and rethrown unchanged. This does not retry or connect itself.
-- Duration uses a monotonic clock and includes initial span delivery, unlike
-- wall-clock Date.now arithmetic. Sinks follow the existing observability policy.
-- Bracket resources inside the action: a reporting callback can still cancel
-- after it returns. The generic observeDroidOperation remains unchanged.
recordComputerConnectSli :: ComputerConnectSliOptions -> (a -> Bool) -> (ComputerConnectAttempt -> IO a) -> IO a
recordComputerConnectSli options successful action = mask $ \restore -> do
  identifier <- UUID.toText <$> nextRandom
  startType <- newIORef Nothing
  let attempt = ComputerConnectAttempt identifier (fromMaybe "unknown" (computerConnectAttemptTrigger options)) (writeIORef startType)
  started <- getMonotonicTimeNSec
  restore (void (Obs.setDroidSpanAttributes (computerConnectSpanSink options) (computerConnectTraceAttributes attempt Nothing Nothing)))
  result <- try @SomeException $ restore $ do
    value <- action attempt
    accepted <- evaluate (successful value)
    pure (value, accepted)
  let report outcome reason = do
        selectedStartType <- readIORef startType
        finished <- getMonotonicTimeNSec
        let elapsed = fromInteger (toInteger finished - toInteger started) / 1000000
            labels = KeyMap.fromList ([("surface", String (computerConnectSurface options)), ("attemptTrigger", String (connectAttemptTrigger attempt)), ("outcome", String outcome)] <> nonemptySliField "providerType" (computerConnectProvider options) <> nonemptySliField "startType" selectedStartType <> nonemptySliField "failureReason" reason)
        void (Obs.setDroidSpanAttributes (computerConnectSpanSink options) (computerConnectTraceAttributes attempt (Just outcome) reason))
        void (Obs.recordDroidMetric (computerConnectMetricSink options) (Obs.DroidMetricEvent "factory_app_computer_connect_sli_attempt_count" Obs.MetricCounter 1 Obs.MetricCount (Just labels)))
        void (Obs.recordDroidMetric (computerConnectMetricSink options) (Obs.DroidMetricEvent "factory_app_computer_connect_sli_duration_ms" Obs.MetricHistogram elapsed Obs.MetricMilliseconds (Just labels)))
  case result of
    Left cause -> finallyPreserving (throwIO cause) (restore (report "failure" (Just (computerConnectFailureReason (computerConnectRequestMethod options) cause))))
    Right (value, accepted) -> restore (if accepted then report "success" Nothing else report "failure" (Just "not_fully_connected")) >> pure value

computerConnectTraceAttributes :: ComputerConnectAttempt -> Maybe Text -> Maybe Text -> Object
computerConnectTraceAttributes attempt outcome reason =
  KeyMap.fromList ([("factory.computer.connect.attempt_id", String (connectAttemptId attempt)), ("factory.computer.connect.attempt_trigger", String (connectAttemptTrigger attempt))] <> nonemptySliField "factory.computer.connect.outcome" outcome <> nonemptySliField "factory.computer.connect.failure_reason" reason)

nonemptySliField :: Key -> Maybe Text -> [(Key, Value)]
nonemptySliField key value = case value of Just text | not (Text.null text) -> [(key, String text)]; _ -> []

data ConnectionPollOptions = ConnectionPollOptions
  { connectionPollAttempts :: !Int,
    connectionPollIntervalMicros :: !Int,
    connectionPreSpawnGraceMicros :: !Int,
    connectionExtraAuthAttempts :: !Int,
    connectionPollAbort :: !(Maybe (STM SomeException)),
    connectionPollProgress :: !(Maybe (ConnectionAttemptProgress -> IO ())),
    connectionPollRecovery :: !Bool
  }

instance Show ConnectionPollOptions where show _ = "ConnectionPollOptions <callbacks redacted>"

defaultConnectionPollOptions :: ConnectionPollOptions
defaultConnectionPollOptions = ConnectionPollOptions 15 1000000 120000000 3 Nothing Nothing False

data ConnectionAttemptProgress = ConnectionAttemptProgress
  { connectionAttempt :: !Int,
    connectionAttemptLimit :: !Int
  }
  deriving stock (Eq, Show)

data ConnectionStatus = ConnectionStatus
  { connectionStatusVersion :: !Integer,
    connectionStatusHealth :: !Daemon.ConnectionHealth,
    connectionStatusFailure :: !(Maybe ConnectionFailure),
    connectionStatusPolling :: !Bool,
    connectionStatusRecovery :: !(Maybe Bool),
    connectionStatusProgress :: !(Maybe ConnectionAttemptProgress),
    connectionStatusObserverFailure :: !(Maybe SomeException)
  }

instance Show ConnectionStatus where show _ = "ConnectionStatus <redacted>"

data ConnectionError = InvalidConnectionPollOptions | ConnectionControllerClosed | ConnectionNotReady deriving stock (Eq, Show)

instance Exception ConnectionError

data PollRequest = PollRequest
  { requestOptions :: !ConnectionPollOptions,
    requestTicket :: !(TMVar (Either SomeException Bool)),
    requestStarted :: !Integer,
    requestFailures :: !Int,
    requestLimit :: !Int,
    requestHadConnected :: !Bool
  }

data ControllerState = ControllerState
  { controllerClosed :: !Bool,
    controllerCurrent :: !(Maybe Daemon.DaemonConnection),
    controllerTransportOpen :: !Bool,
    controllerEverConnected :: !Bool,
    controllerPending :: !(Maybe PollRequest),
    controllerActiveRequest :: !(Maybe PollRequest),
    controllerFailure :: !(Maybe ConnectionFailure),
    controllerRecovery :: !(Maybe Bool),
    controllerProgress :: !(Maybe ConnectionAttemptProgress),
    controllerObserverFailure :: !(Maybe SomeException),
    controllerIdentity :: !(Maybe (Text, Text)),
    controllerVersion :: !Integer,
    controllerFatal :: !(Maybe SomeException)
  }

data ConnectionController = ConnectionController !ConnectionPlan !(TVar ControllerState)

daemonConnectionPlan :: Daemon.DaemonOptions -> ConnectionPlan
daemonConnectionPlan options = ConnectionPlan (Daemon.withConnectionObserved options) (\connection -> Daemon.ensureConnectionAuthenticated connection (Daemon.DaemonAuthenticate (Daemon.daemonCredential options))) Nothing classifyConnectionFailure

-- | The existing controller loop with a caller-scoped logical state owner.
-- Every successful acquisition still publishes a new physical connection.
daemonConnectionPlanWithState :: Daemon.DaemonState -> Daemon.DaemonOptions -> ConnectionPlan
daemonConnectionPlanWithState shared options = (daemonConnectionPlan options) {withPlannedConnection = Daemon.withConnectionStateObserved shared options}

relayConnectionPlan :: WebSocket.WebSocketOptions -> WebSocket.WebSocketTarget -> Relay.RelayOptions -> Relay.RelayCredential -> (Daemon.DaemonAuthentication -> Daemon.DaemonClientOptions) -> ConnectionPlan
relayConnectionPlan = relayConnectionPlanUsing Nothing

relayConnectionPlanWithState :: Daemon.DaemonState -> WebSocket.WebSocketOptions -> WebSocket.WebSocketTarget -> Relay.RelayOptions -> Relay.RelayCredential -> (Daemon.DaemonAuthentication -> Daemon.DaemonClientOptions) -> ConnectionPlan
relayConnectionPlanWithState shared = relayConnectionPlanUsing (Just shared)

relayConnectionPlanUsing :: Maybe Daemon.DaemonState -> WebSocket.WebSocketOptions -> WebSocket.WebSocketTarget -> Relay.RelayOptions -> Relay.RelayCredential -> (Daemon.DaemonAuthentication -> Daemon.DaemonClientOptions) -> ConnectionPlan
relayConnectionPlanUsing shared socketOptions target relayOptions credential configure = ConnectionPlan acquire repair Nothing classifyConnectionFailure
  where
    acquire :: forall a. IO () -> (Daemon.DaemonConnection -> IO a) -> IO a
    acquire opened action = Relay.withRelayConnectionObserved socketOptions target relayOptions credential opened $ \relay ->
      case shared of
        Nothing -> Daemon.withConnectionOn (configure (Relay.relayDaemonAuthentication relay)) (Relay.relayTransport relay) action
        Just retained -> Daemon.withConnectionStateOn retained (configure (Relay.relayDaemonAuthentication relay)) (Relay.relayTransport relay) action
    authentication = case credential of
      Relay.RelayApiKey key -> Daemon.DaemonAuthenticate (Daemon.DaemonApiKey key)
      Relay.RelayTokenProvider provider _ grant -> Daemon.DaemonTokenProvider provider grant
    repair connection = Daemon.ensureConnectionAuthenticated connection (Daemon.daemonClientAuthentication (configure authentication))

classifyConnectionFailure :: SomeException -> ConnectionFailure
classifyConnectionFailure cause
  | Just failure <- fromException cause = failure
  | Just Daemon.DaemonIdentityMismatch <- fromException cause = failed "identity_mismatch" False
  | Just Daemon.DaemonAuthenticationSuperseded <- fromException cause = failed "auth_superseded" False
  | Just Daemon.DaemonStateClosed <- fromException cause = failed "state_closed" False
  | Just Daemon.DaemonStateInUse <- fromException cause = failed "state_in_use" False
  | Just Daemon.DaemonCredentialUnavailable <- fromException cause = failed "no_token" False
  | Just Daemon.InvalidDaemonCredential <- fromException cause = failed "no_token" False
  | Just (RpcRemoteFailure errorValue) <- fromException cause, rpcErrorCode errorValue == RpcAuthenticationError = failed "auth_rejected" False
  | Just Relay.RelayMissingToken <- fromException cause = failed "no_token" False
  | Just Relay.RelayAuthenticationTimedOut <- fromException cause = failed "relay_timeout" True
  | Just (Relay.RelayAuthenticationRejected rejection) <- fromException cause = case Relay.relayFailureCode rejection of
      Just Relay.RelayUnauthorized -> failed "relay_unauthorized" False
      Just Relay.RelayComputerAlreadyConnected -> failed "daemon_conflict" False
      Just Relay.RelayServiceUnavailable -> failed "relay_gateway_error" True
      Just Relay.RelayRateLimited -> failed "relay_rate_limited" True
      Nothing -> if Relay.relayFailureRetryable rejection == Just True then failed "relay_auth_rejected" True else failed "relay_unauthorized" False
  | Just Relay.InvalidRelayOptions <- fromException cause = failed "invalid_configuration" False
  | Just WebSocket.InvalidWebSocketTarget <- fromException cause = failed "invalid_configuration" False
  | Just WebSocket.InvalidWebSocketLimit <- fromException cause = failed "invalid_configuration" False
  | Just WebSocket.InvalidWebSocketTimeout <- fromException cause = failed "invalid_configuration" False
  | Just WebSocket.WebSocketConnectFailure <- fromException cause = failed "daemon_unreachable" True
  | Just WebSocket.WebSocketConnectTimeout <- fromException cause = failed "daemon_timeout" True
  | Just code <- closeCode = case code of
      4000 -> failed "computer_offline" True
      4001 -> failed "computer_disconnected" True
      4006 -> failed "computer_disconnected" True
      4007 -> failed "computer_disconnected" True
      4004 -> failed "relay_timeout" True
      4005 -> failed "relay_unauthorized" False
      4009 -> failed "relay_gateway_error" True
      4011 -> failed "relay_rate_limited" True
      4010 -> failed "daemon_conflict" False
      4012 -> failed "daemon_shutting_down" True
      4013 -> failed "daemon_updating" True
      4014 -> failed "client_orphaned" True
      4015 -> failed "grace_buffer_overflow" True
      _ -> failed "connection_lost" True
  | otherwise = failed "unknown" True
  where
    closeCode = case fromException cause of
      Just (WebSocket.WebSocketClose code _) -> Just code
      Nothing -> case fromException cause of
        Just (WebSocket.WebSocketPeerClosed code) -> Just code
        _ -> Nothing
    failed reason allowed = ConnectionFailure reason allowed False (Just cause)

withConnectionController :: ConnectionPlan -> (ConnectionController -> IO a) -> IO a
withConnectionController plan action = do
  state <-
    newTVarIO
      ControllerState
        { controllerClosed = False,
          controllerCurrent = Nothing,
          controllerTransportOpen = False,
          controllerEverConnected = False,
          controllerPending = Nothing,
          controllerActiveRequest = Nothing,
          controllerFailure = Nothing,
          controllerRecovery = Nothing,
          controllerProgress = Nothing,
          controllerObserverFailure = Nothing,
          controllerIdentity = Nothing,
          controllerVersion = 0,
          controllerFatal = Nothing
        }
  let controller = ConnectionController plan state
      failed cause = do
        atomically $ do
          current <- readTVar state
          unless (controllerClosed current) $ change state (\value -> value {controllerClosed = True, controllerFatal = Just cause, controllerFailure = Just (ConnectionFailure "controller_failed" False False (Just cause))})
          settle state (Left cause)
        throwIO cause
      close worker = do
        atomically $ do
          change state (\value -> value {controllerClosed = True, controllerCurrent = Nothing, controllerTransportOpen = False, controllerRecovery = Nothing})
          settle state (Left (toException ConnectionControllerClosed))
        cancel worker
        fatal <- controllerFatal <$> readTVarIO state
        forM_ fatal throwIO
  withAsync (connectionWorker controller `catch` failed) $ \worker -> finallyPreserving (action controller) (close worker)

-- | The first caller owns progress/abort options. Joiners wait on its ticket;
-- cancelling a waiter does not cancel the controller-owned acquisition.
pollUntilConnected :: ConnectionController -> ConnectionPollOptions -> IO Bool
pollUntilConnected controller@(ConnectionController _ state) options = do
  started <- toInteger <$> getMonotonicTimeNSec
  selected <- atomically $ do
    current <- readTVar state
    checkController current
    status <- readStatus controller
    if connectionReady status
      then pure Nothing
      else case controllerPending current of
        Just request -> pure (Just (requestTicket request))
        Nothing -> do
          either throwSTM pure (validatePollOptions options)
          ticket <- newEmptyTMVar
          if connectionPollAttempts options == 0
            then do
              void (tryPutTMVar ticket (Right False))
              change state (\value -> value {controllerRecovery = if connectionPollRecovery options then Just False else controllerRecovery value})
            else do
              let request = PollRequest options ticket started 0 (connectionPollAttempts options) (controllerEverConnected current)
              change state (\value -> value {controllerPending = Just request, controllerRecovery = if connectionPollRecovery options then Just True else controllerRecovery value})
          pure (Just ticket)
  maybe (pure True) (\ticket -> atomically (readTMVar ticket) >>= either throwIO pure) selected

attemptInitialConnection :: ConnectionController -> IO ()
attemptInitialConnection controller = do
  ready <- pollUntilConnected controller (defaultConnectionPollOptions {connectionPollAttempts = 1, connectionExtraAuthAttempts = 0, connectionPreSpawnGraceMicros = 0})
  unless ready $ getConnectionStatus controller >>= maybe (throwIO ConnectionNotReady) throwIO . connectionStatusFailure

withReadyConnection :: ConnectionController -> ConnectionPollOptions -> (Daemon.DaemonConnection -> IO a) -> IO (Maybe a)
withReadyConnection controller@(ConnectionController _ state) options action = do
  ready <- pollUntilConnected controller options
  selected <- atomically $ do
    current <- readTVar state
    checkController current
    status <- readStatus controller
    pure (if ready && connectionReady status then controllerCurrent current else Nothing)
  traverse action selected

getConnectionStatus :: ConnectionController -> IO ConnectionStatus
getConnectionStatus = atomically . readStatus

waitConnectionStatusChange :: ConnectionController -> ConnectionStatus -> IO ConnectionStatus
waitConnectionStatusChange controller@(ConnectionController _ state) previous = atomically $ do
  current <- readStatus controller
  if connectionStatusVersion current /= connectionStatusVersion previous || connectionStatusHealth current /= connectionStatusHealth previous
    then pure current
    else readTVar state >>= checkController >> retryStatus
  where
    retryStatus = Control.Concurrent.STM.retry

connectionReady :: ConnectionStatus -> Bool
connectionReady = Daemon.connectionUsable . connectionStatusHealth

connectionRetryAllowed :: ConnectionStatus -> Bool
connectionRetryAllowed = maybe True connectionFailureRetryable . connectionStatusFailure

readStatus :: ConnectionController -> STM ConnectionStatus
readStatus (ConnectionController _ state) = do
  current <- readTVar state
  health <- if controllerClosed current then pure (Daemon.ConnectionHealth False False) else maybe (pure (Daemon.ConnectionHealth (controllerTransportOpen current) False)) Daemon.readConnectionHealth (controllerCurrent current)
  pure (ConnectionStatus (controllerVersion current) health (controllerFailure current) (isJust (controllerPending current)) (controllerRecovery current) (controllerProgress current) (controllerObserverFailure current))

checkController :: ControllerState -> STM ()
checkController state = when (controllerClosed state) (maybe (throwSTM ConnectionControllerClosed) throwSTM (controllerFatal state))

change :: TVar ControllerState -> (ControllerState -> ControllerState) -> STM ()
change state update = modifyTVar' state (\value -> (update value) {controllerVersion = controllerVersion value + 1})

settle :: TVar ControllerState -> Either SomeException Bool -> STM ()
settle state outcome = do
  current <- readTVar state
  forM_ (controllerPending current) $ \request -> void (tryPutTMVar (requestTicket request) outcome)
  change state (\value -> value {controllerPending = Nothing, controllerActiveRequest = Nothing, controllerRecovery = case outcome of Left _ -> Nothing; Right _ -> controllerRecovery value})

validatePollOptions :: ConnectionPollOptions -> Either ConnectionError ()
validatePollOptions options
  | connectionPollAttempts options < 0 || connectionPollIntervalMicros options < 0 || connectionPreSpawnGraceMicros options < 0 || connectionExtraAuthAttempts options < 0 = Left InvalidConnectionPollOptions
  | toInteger (connectionPollAttempts options) + toInteger (connectionExtraAuthAttempts options) > toInteger (maxBound :: Int) = Left InvalidConnectionPollOptions
  | otherwise = Right ()

connectionWorker :: ConnectionController -> IO ()
connectionWorker (ConnectionController plan state) = next
  where
    nextRequest = do
      current <- readTVar state
      checkController current
      maybe retry pure (controllerPending current)
    next = atomically nextRequest >>= connect
    prepare request = do
      selected <- atomically $ do
        current <- readTVar state
        checkController current
        let selected = request {requestHadConnected = controllerEverConnected current}
        change state (\value -> value {controllerPending = Just selected, controllerActiveRequest = Just selected})
        pure selected
      Wait.checkAbort (connectionPollAbort (requestOptions request))
      let progress = ConnectionAttemptProgress (requestFailures request + 1) (requestLimit request)
      atomically $ do
        readTVar state >>= checkController
        change state (\value -> value {controllerProgress = Just progress})
      forM_ (connectionPollProgress (requestOptions request)) $ \observe -> do
        outcome <- trySync (observe progress)
        case outcome of
          Left cause -> atomically (change state (\value -> value {controllerObserverFailure = Just cause}))
          Right () -> pure ()
      pure selected
    markOpened = atomically $ do
      current <- readTVar state
      checkController current
      change state (\value -> value {controllerTransportOpen = True, controllerEverConnected = True})
    publish request connection = do
      Wait.checkAbort (connectionPollAbort (requestOptions request))
      health <- Daemon.getConnectionHealth connection
      unless (Daemon.connectionUsable health) (throwIO ConnectionNotReady)
      let identity = Daemon.connectionUser connection
          key = (reportedUserId identity, reportedOrgId identity)
      atomically $ do
        current <- readTVar state
        checkController current
        when (maybe False (/= key) (controllerIdentity current)) (throwSTM (ConnectionFailure "identity_mismatch" False False (Just (toException Daemon.DaemonIdentityMismatch))))
        change state (\value -> value {controllerCurrent = Just connection, controllerTransportOpen = True, controllerIdentity = Just key, controllerFailure = Nothing, controllerRecovery = Nothing})
        settle state (Right True)
    connected connection = do
      selected <-
        atomically $
          (Just <$> nextRequest) `orElse` do
            current <- readTVar state
            checkController current
            health <- Daemon.readConnectionHealth connection
            check (not (Daemon.healthTransportConnected health))
            cause <- Daemon.readConnectionFailureCause connection
            let failure = maybe (ConnectionFailure "connection_lost" True False Nothing) (classifyPlannedFailure plan) cause
            change state (\value -> value {controllerFailure = controllerFailure value `orFailure` failure})
            pure Nothing
      case selected of
        Nothing -> pure Nothing
        Just request -> do
          health <- Daemon.getConnectionHealth connection
          if not (Daemon.healthTransportConnected health)
            then pure (Just request)
            else do
              selectedRequest <- prepare request
              unless (Daemon.healthAuthenticated health) (repairPlannedAuthentication plan connection)
              publish selectedRequest connection
              connected connection
    connect request = do
      outcome <- try @SomeException $ do
        selected <- prepare request
        forM_ (ensurePlannedRunning plan) id
        withPlannedConnection plan markOpened $ \connection -> do
          publish selected connection
          connected connection
      atomically (change state (\value -> value {controllerCurrent = Nothing, controllerTransportOpen = False}))
      current <- readTVarIO state
      when (controllerClosed current) (throwIO ConnectionControllerClosed)
      case outcome of
        Right Nothing -> next
        Right (Just requestAgain) -> connect requestAgain
        Left cause -> handleFailure cause
    handleFailure cause = do
      current <- readTVarIO state
      case controllerActiveRequest current of
        Nothing -> do
          atomically (change state (\value -> value {controllerFailure = Just (classifyPlannedFailure plan cause)}))
          next
        Just request -> do
          aborted <- try @SomeException (Wait.checkAbort (connectionPollAbort (requestOptions request)))
          case (aborted, fromException cause :: Maybe SomeAsyncException) of
            (Left reason, _) -> atomically (settle state (Left reason)) >> next
            (_, Just _) -> atomically (settle state (Left cause)) >> next
            _ -> do
              now <- toInteger <$> getMonotonicTimeNSec
              let failure = classifyPlannedFailure plan cause
                  options = requestOptions request
                  beforeGrace = (now - requestStarted request) `div` 1000 < toInteger (connectionPreSpawnGraceMicros options)
                  counted = not (connectionFailureBeforeSpawn failure && beforeGrace)
                  limit = if not (requestHadConnected request) && controllerEverConnected current then max (requestLimit request) (requestFailures request + 1 + connectionExtraAuthAttempts options) else requestLimit request
                  failures = requestFailures request + if counted then 1 else 0
                  again = request {requestFailures = failures, requestLimit = limit}
              atomically (change state (\value -> value {controllerFailure = Just failure, controllerPending = Just again, controllerActiveRequest = Just again}))
              if not (connectionFailureRetryable failure) || failures >= limit
                then do
                  atomically $ do
                    change state (\value -> value {controllerRecovery = if connectionPollRecovery options then Just False else Nothing})
                    settle state (Right False)
                  next
                else do
                  waited <- try @SomeException (Wait.waitWithAbort (connectionPollAbort options) (connectionPollIntervalMicros options))
                  case waited of
                    Left reason -> atomically (settle state (Left reason)) >> next
                    Right () -> connect again
    orFailure Nothing failure = Just failure
    orFailure present _ = present
