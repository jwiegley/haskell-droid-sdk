{-# LANGUAGE OverloadedStrings #-}

-- | Scoped request correlation over caller-owned object send/receive actions.
-- Use withJsonLinesProcess for a native child transport. Requests supply their
-- complete envelopes; this layer chooses neither attribution nor CLI settings.
module Factory.Droid.Protocol
  ( RpcChannel,
    RpcChannelError (..),
    RpcResultError (..),
    withRpcChannel,
    withObservedRpcChannel,
    closeRpcChannel,
    rpcChannelFailureCause,
    RpcHookPolicy (..),
    setRpcBeforeRequest,
    registerRpcRequestBarrier,
    onRpcRequestSettled,
    onRpcSessionRequestSettled,
    getRpcPendingCount,
    requestReply,
    requestReplyWithHookPolicy,
    requestReplyObservedAtWithHookPolicy,
    requestReplyWithAdmission,
    requestReplyObservedAtWithAdmission,
    requestReplyObserved,
    requestReplyObservedAt,
    requestResult,
    decodeRpcResult,
    sendRpcMessage,
    receiveRpcEvent,
    synchronizeRpcEvents,
  )
where

import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    isEmptyTMVar,
    modifyTVar',
    newEmptyTMVarIO,
    newTQueueIO,
    newTVarIO,
    orElse,
    readTMVar,
    readTQueue,
    readTVar,
    throwSTM,
    tryPutTMVar,
    tryReadTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (Exception, SomeException, catch, finally, fromException, mask, throwIO)
import Control.Monad (filterM, forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (Object, String), fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Factory.Droid.Internal.Exception (finallyPreserving, trySync)
import Factory.Droid.Observability qualified as Obs
import Factory.Droid.Schema.RPC
import System.Timeout (timeout)

-- | Payload-free channel failures. sendRpcMessage preserves its original send
-- exception; requestReply and other pending requests use channel failures.
data RpcChannelError
  = RpcChannelClosed
  | RpcChannelReadFailure
  | RpcChannelWriteFailure
  | RpcMalformedMessage
  | RpcMalformedResponse
  | RpcDuplicateRequestId
  | RpcRequestTimedOut
  | RpcInvalidTimeout
  deriving stock (Eq, Show)

instance Exception RpcChannelError

-- | Typed result failures. Remote error details remain available explicitly,
-- but Show omits their potentially sensitive message, data and extensions.
data RpcResultError = RpcRemoteFailure !JsonRpcError | RpcMissingResult | RpcInvalidResult
  deriving stock (Eq)

instance Show RpcResultError where
  show (RpcRemoteFailure _) = "RpcRemoteFailure <redacted>"
  show RpcMissingResult = "RpcMissingResult"
  show RpcInvalidResult = "RpcInvalidResult"

instance Exception RpcResultError

-- | Valid only inside withRpcChannel. Callers own their request/event threads
-- and must finish them before leaving the callback. There is one event consumer;
-- request replies are correlated independently of event consumption.
data RpcChannel = RpcChannel
  { channelSend :: !(Object -> IO ()),
    channelWriter :: !(MVar ()),
    channelPending :: !(TVar (Map Text PendingRequest)),
    channelEvents :: !(TQueue QueuedEvent),
    channelStopped :: !(TMVar RpcChannelError),
    channelStopCause :: !(TVar (Maybe SomeException)),
    channelHooks :: !(TVar ChannelHooks),
    channelObservability :: !Obs.DroidObservability
  }

data PendingRequest = PendingRequest !Integer !(Maybe Text) !(TMVar (Either RpcChannelError JsonRpcBaseResponse)) !(Maybe (UTCTime -> JsonRpcBaseResponse -> STM ()))

data RpcHookPolicy = RunBeforeRequest | SkipBeforeRequest deriving stock (Eq, Show)

data ChannelHooks = ChannelHooks
  { hookNextToken :: !Integer,
    hookBeforeRequest :: !(Maybe (Integer, Text -> Text -> IO ())),
    hookRequestBarriers :: !(Map Integer (JsonRpcBaseRequest -> IO ())),
    hookSettled :: !(Map Integer (Maybe Text, Text -> IO ()))
  }

-- Observer construction stays lazy; only the existing consumer runs callbacks.
data QueuedEvent = QueuedMessage !JsonRpcMessage | EventBoundary !(TMVar ()) | ReplyObservation (STM ()) | RequestSettlement [IO ()]

-- | Start one reader and close the channel on scope exit. The send/receive
-- actions and their underlying transport remain caller-owned. EOF must be
-- reported by the receive action as an exception; it is terminal, not success.
-- Invalid envelopes are terminal too. Closing rejects pending requests before
-- cancelling and joining the reader, without replacing the callback exception.
withRpcChannel :: (Object -> IO ()) -> IO Object -> (RpcChannel -> IO a) -> IO a
withRpcChannel = withObservedRpcChannel Obs.defaultDroidObservability

withObservedRpcChannel :: Obs.DroidObservability -> (Object -> IO ()) -> IO Object -> (RpcChannel -> IO a) -> IO a
withObservedRpcChannel observability send receive action =
  Obs.observeDroidOperation observability "droid.rpc.connection" Nothing $ do
    channel <- RpcChannel send <$> newMVar () <*> newTVarIO Map.empty <*> newTQueueIO <*> newEmptyTMVarIO <*> newTVarIO Nothing <*> newTVarIO (ChannelHooks 0 Nothing Map.empty Map.empty) <*> pure observability
    withAsync (readMessages channel receive) $ \_ ->
      action channel `finally` atomically (stopChannel channel RpcChannelClosed Nothing)

-- | Seal the channel without closing its caller-owned transport. Pending and
-- future requests fail; queued events still drain. The scope retains reader
-- ownership and joins it on exit. Repeated calls preserve the first failure.
closeRpcChannel :: RpcChannel -> IO ()
closeRpcChannel channel = atomically (stopChannel channel RpcChannelClosed Nothing)

-- | The first original I/O failure, if one was recorded. This explicit data
-- may be sensitive even though RpcChannelError displays remain payload-free.
rpcChannelFailureCause :: RpcChannel -> STM (Maybe SomeException)
rpcChannelFailureCause = readTVar . channelStopCause

-- | Count unresolved requests, excluding already accepted responses whose
-- callers have not yet resumed. Guards run before pending admission.
getRpcPendingCount :: RpcChannel -> STM Int
getRpcPendingCount channel = do
  pending <- Map.elems <$> readTVar (channelPending channel)
  length <$> filterM (\(PendingRequest _ _ result _) -> isEmptyTMVar result) pending

nextHookToken :: RpcChannel -> STM Integer
nextHookToken channel = do
  checkOpen channel
  hooks <- readTVar (channelHooks channel)
  let token = hookNextToken hooks
  writeTVar (channelHooks channel) hooks {hookNextToken = token + 1}
  pure token

-- | Replace the optional session-scoped guard. Each request captures its own
-- admission snapshot. An old stop action cannot remove a newer replacement.
setRpcBeforeRequest :: RpcChannel -> Maybe (Text -> Text -> IO ()) -> IO (IO ())
setRpcBeforeRequest channel callback = do
  token <- atomically $ do
    token <- nextHookToken channel
    modifyTVar' (channelHooks channel) (\hooks -> hooks {hookBeforeRequest = fmap (token,) callback})
    pure token
  pure $ atomically $ modifyTVar' (channelHooks channel) $ \hooks -> case hookBeforeRequest hooks of
    Just (current, _) | current == token -> hooks {hookBeforeRequest = Nothing}
    _ -> hooks

-- | Register a mandatory gate after the optional guard. Per-request skipping
-- never skips these gates. Callbacks execute before writer/pending admission.
registerRpcRequestBarrier :: RpcChannel -> (JsonRpcBaseRequest -> IO ()) -> IO (IO ())
registerRpcRequestBarrier channel callback = do
  token <- atomically $ do
    token <- nextHookToken channel
    modifyTVar' (channelHooks channel) (\hooks -> hooks {hookRequestBarriers = Map.insert token callback (hookRequestBarriers hooks)})
    pure token
  pure (atomically (modifyTVar' (channelHooks channel) (\hooks -> hooks {hookRequestBarriers = Map.delete token (hookRequestBarriers hooks)})))

-- | Settlement is queued once at the terminal transition. The existing event
-- consumer delivers registration-order snapshots after result observations;
-- ordinary callback failures are isolated. Unsubscribe affects later snapshots.
onRpcRequestSettled :: RpcChannel -> (Text -> IO ()) -> IO (IO ())
onRpcRequestSettled channel = onRpcRequestSettledFor channel Nothing

-- | Observe only requests carrying this exact string params.sessionId.
onRpcSessionRequestSettled :: RpcChannel -> Text -> (Text -> IO ()) -> IO (IO ())
onRpcSessionRequestSettled channel identifier = onRpcRequestSettledFor channel (Just identifier)

onRpcRequestSettledFor :: RpcChannel -> Maybe Text -> (Text -> IO ()) -> IO (IO ())
onRpcRequestSettledFor channel session callback = do
  token <- atomically $ do
    token <- nextHookToken channel
    modifyTVar' (channelHooks channel) (\hooks -> hooks {hookSettled = Map.insert token (session, callback) (hookSettled hooks)})
    pure token
  pure (atomically (modifyTVar' (channelHooks channel) (\hooks -> hooks {hookSettled = Map.delete token (hookSettled hooks)})))

beforeRpcRequest :: RpcChannel -> RpcHookPolicy -> JsonRpcBaseRequest -> IO ()
beforeRpcRequest channel policy request = do
  hooks <- atomically (checkOpen channel >> readTVar (channelHooks channel))
  let body = envelopeBody request
  when (policy == RunBeforeRequest) $ forM_ (hookBeforeRequest hooks) $ \(_, callback) -> forM_ (requestSessionId request) (\identifier -> callback identifier (baseRequestMethod body))
  forM_ (Map.elems (hookRequestBarriers hooks)) ($ request)

requestSessionId :: JsonRpcBaseRequest -> Maybe Text
requestSessionId request = case baseRequestParams (envelopeBody request) of
  Just (Object params) -> case KeyMap.lookup "sessionId" params of
    Just (String identifier) -> Just identifier
    _ -> Nothing
  _ -> Nothing

queueSettlement :: RpcChannel -> Text -> Maybe Text -> STM ()
queueSettlement channel identifier session = do
  registered <- Map.elems . hookSettled <$> readTVar (channelHooks channel)
  let callbacks = [callback identifier | (scope, callback) <- registered, maybe True (\selected -> session == Just selected) scope]
  case callbacks of
    [] -> pure ()
    _ -> writeTQueue (channelEvents channel) (RequestSettlement callbacks)

-- | Send a complete request and await its raw response, retaining remote errors
-- as BaseFailure values. IDs must be connection-unique; concurrent duplicates
-- are rejected before sending. Reusing settled IDs cannot distinguish late peer
-- responses and is outside this contract.
--
-- Nothing disables the deadline; Just n bounds sending and waiting together in
-- microseconds. Guards precede this exchange budget and pending admission; bound
-- arbitrary guard I/O externally. Zero and negative budgets fail before guards.
-- Cancelling or timing out an in-progress write terminates the channel because
-- a partial frame may have been sent. Cancellation while awaiting a reply only
-- removes that pending request. Transport actions must support async cancellation.
requestReply :: RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> IO JsonRpcBaseResponse
requestReply channel = requestReplyWithHookPolicy channel RunBeforeRequest

requestReplyWithHookPolicy :: RpcChannel -> RpcHookPolicy -> Maybe Int -> JsonRpcBaseRequest -> IO JsonRpcBaseResponse
requestReplyWithHookPolicy channel policy = requestReplyWithAdmission channel policy (pure ())

-- | Check admission before hooks, pending registration and physical writer
-- admission. The STM action must be total/nonblocking (never retry). A rejected
-- queued write retains its exception and does not poison the channel.
requestReplyWithAdmission :: RpcChannel -> RpcHookPolicy -> STM () -> Maybe Int -> JsonRpcBaseRequest -> IO JsonRpcBaseResponse
requestReplyWithAdmission channel policy admit deadline request = requestReplyWith channel policy admit deadline request Nothing

-- | Correlate a reply immediately, and observe its first accepted response on
-- the single event consumer at its wire-receive position. The observer must be
-- total and nonblocking: never retry, throw, or wait for other intake work.
-- Dequeue and observation commit together. Accepted observations survive waiter
-- cancellation; late, duplicate, malformed and synthetic failure replies do not
-- create observations. A running consumer is required to execute observations,
-- not to complete the request. Use 'synchronizeRpcEvents' outside callbacks when
-- subsequent work needs the observation to have run.
requestReplyObserved :: RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> (JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyObserved channel deadline request observe = requestReplyObservedAt channel deadline request (const observe)

-- | Ordered observation with the local UTC receive timestamp, captured after
-- envelope decoding and before response acceptance. It is not peer time or a
-- monotonic ordering clock. The observation retains the same STM guarantees.
requestReplyObservedAt :: RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> (UTCTime -> JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyObservedAt channel = requestReplyObservedAtWithHookPolicy channel RunBeforeRequest

requestReplyObservedAtWithHookPolicy :: RpcChannel -> RpcHookPolicy -> Maybe Int -> JsonRpcBaseRequest -> (UTCTime -> JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyObservedAtWithHookPolicy channel policy = requestReplyObservedAtWithAdmission channel policy (pure ())

requestReplyObservedAtWithAdmission :: RpcChannel -> RpcHookPolicy -> STM () -> Maybe Int -> JsonRpcBaseRequest -> (UTCTime -> JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyObservedAtWithAdmission channel policy admit deadline request observe = requestReplyWith channel policy admit deadline request (Just observe)

requestReplyWith :: RpcChannel -> RpcHookPolicy -> STM () -> Maybe Int -> JsonRpcBaseRequest -> Maybe (UTCTime -> JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyWith channel policy admit deadline request observe = do
  forM_ deadline $ \micros -> when (micros < 0) (throwIO RpcInvalidTimeout)
  when (deadline == Just 0) (atomically (checkOpen channel) >> throwIO RpcRequestTimedOut)
  atomically (checkOpen channel >> admit)
  beforeRpcRequest channel policy request
  Obs.observeDroidOperation (channelObservability channel) "droid.rpc.exchange" (Just (KeyMap.singleton "method" (String (baseRequestMethod (envelopeBody request))))) $ mask $ \restore -> do
    reply <- newEmptyTMVarIO
    let identifier = baseRequestId (envelopeBody request)
    atomically $ do
      checkOpen channel
      admit
      pending <- readTVar (channelPending channel)
      when (Map.member identifier pending) (throwSTM RpcDuplicateRequestId)
      order <- nextHookToken channel
      modifyTVar' (channelPending channel) (Map.insert identifier (PendingRequest order (requestSessionId request) reply observe))
    let send = sendRpcMessageWhen channel admit (request {envelopeBody = RequestBody (envelopeBody request)})
        stopped = atomically (readTMVar (channelStopped channel) >> readTMVar reply)
        checkedSend =
          send `catch` \(cause :: SomeException) -> do
            failure <- atomically (tryReadTMVar (channelStopped channel))
            maybe (throwIO cause) throwIO failure
        exchange = race checkedSend stopped >>= either (const (atomically (readTMVar reply))) pure
        timed = maybe (Just <$> exchange) (`timeout` exchange) deadline
    result <-
      restore timed
        `finally` atomically
          ( do
              pending <- readTVar (channelPending channel)
              forM_ (Map.lookup identifier pending) $ \(PendingRequest _ session result _) -> do
                unresolved <- isEmptyTMVar result
                when unresolved (queueSettlement channel identifier session)
              modifyTVar' (channelPending channel) (Map.delete identifier)
          )
    response <- maybe (throwIO RpcRequestTimedOut) (either throwIO pure) result
    let remoteError = case envelopeBody response of BaseFailure failure -> Just (failureResponseError failure); BaseSuccess success -> successResponseError success
    forM_ remoteError $ \failure -> do
      let observability = channelObservability channel
          attributes = Just (KeyMap.fromList [("method", String (baseRequestMethod (envelopeBody request))), ("code", toJSON (rpcErrorCode failure))])
      void (Obs.emitDroidLog (Obs.observabilityLogger observability) (Obs.DroidLogEvent Obs.LogWarn "droid.rpc.remote_error" "Remote RPC error response" attributes Nothing))
      void (Obs.recordDroidMetric (Obs.observabilityMetrics observability) (Obs.DroidMetricEvent "droid.rpc.remote_error" Obs.MetricCounter 1 Obs.MetricCount attributes))
    pure response

-- | Request and decode a result through its FromJSON instance. Remote failures
-- become RpcRemoteFailure exceptions. The deadline applies to the RPC exchange;
-- arbitrary caller-supplied pure decoding is not given a separate CPU deadline.
requestResult :: (FromJSON a) => RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> IO a
requestResult channel deadline request = requestReply channel deadline request >>= either throwIO pure . decodeRpcResult

-- | Decode a raw response without losing remote errors to a result parser.
-- Errors take precedence even in a manually constructed BaseSuccess carrying
-- both payloads. Missing result and an explicit null remain distinct.
decodeRpcResult :: (FromJSON a) => JsonRpcBaseResponse -> Either RpcResultError a
decodeRpcResult response = case envelopeBody response of
  BaseFailure failure -> Left (RpcRemoteFailure (failureResponseError failure))
  BaseSuccess success -> case successResponseError success of
    Just err -> Left (RpcRemoteFailure err)
    Nothing -> case successResponseResult success of
      Nothing -> Left RpcMissingResult
      Just value -> case fromJSON value of
        Error _ -> Left RpcInvalidResult
        Success result -> Right result

-- | Serialize an outbound message with respect to other sends. This does not
-- register requests; use requestReply when a correlated reply is required.
-- The initiating send exception is rethrown, including async cancellation.
sendRpcMessage :: RpcChannel -> JsonRpcMessage -> IO ()
sendRpcMessage channel = sendRpcMessageWhen channel (pure ())

sendRpcMessageWhen :: RpcChannel -> STM () -> JsonRpcMessage -> IO ()
sendRpcMessageWhen channel admit message = withMVar (channelWriter channel) $ \() -> do
  atomically (checkOpen channel >> admit)
  fields <- traceMessage (Obs.observabilityTracing (channelObservability channel)) (toRpcObject message)
  atomically (checkOpen channel >> admit)
  channelSend channel fields
    `catch` \(cause :: SomeException) -> do
      atomically (stopChannel channel RpcChannelWriteFailure (Just cause))
      finallyPreserving (throwIO cause) (logChannelFailure channel RpcChannelWriteFailure)

traceMessage :: Maybe Obs.DroidTraceContextProvider -> Object -> IO Object
traceMessage Nothing fields = pure fields
traceMessage provider fields = case KeyMap.lookup "_meta" fields of
  Nothing -> inject mempty
  Just (Object carrier) -> inject carrier
  _ -> pure fields
  where
    inject carrier = do
      (updated, _) <- Obs.injectDroidTraceContext provider carrier
      pure (if updated == carrier then fields else KeyMap.insert "_meta" (Object updated) fields)

logChannelFailure :: RpcChannel -> RpcChannelError -> IO ()
logChannelFailure channel failure =
  void (Obs.emitDroidLog (Obs.observabilityLogger (channelObservability channel)) (Obs.DroidLogEvent Obs.LogWarn "droid.rpc.error" "RPC channel failed" (Just (KeyMap.singleton "failure" (String (Text.pack (show failure))))) Nothing))

-- | Read the next notification, server request or null-ID response. Unknown,
-- late and duplicate non-null-ID responses are ignored. Queued events drain
-- before a terminal error is thrown. Consumption is single-reader, not broadcast.
-- Server requests are data here: callers must handle and reply to them; no
-- permission is granted, handler executed or default response synthesized.
receiveRpcEvent :: RpcChannel -> IO JsonRpcMessage
receiveRpcEvent channel = mask $ \restore -> do
  next <- atomically $ do
    queued <-
      readTQueue (channelEvents channel)
        `orElse` (readTMVar (channelStopped channel) >>= throwSTM)
    case queued of
      QueuedMessage message -> pure (Right (Just message))
      EventBoundary reached -> void (tryPutTMVar reached ()) >> pure (Right Nothing)
      ReplyObservation observe -> observe >> pure (Right Nothing)
      RequestSettlement callbacks -> pure (Left callbacks)
  case next of
    Right (Just message) -> pure message
    Right Nothing -> restore (receiveRpcEvent channel)
    Left callbacks -> restore (mapM_ trySync callbacks) >> restore (receiveRpcEvent channel)

-- | Wait until the single event consumer has advanced past the currently
-- queued prefix. A sequential dispatcher has then finished prior callbacks;
-- this does not join separately spawned workers or wait for future wire frames.
-- Requires a running consumer. Do not call from that consumer's own callback.
synchronizeRpcEvents :: RpcChannel -> IO ()
synchronizeRpcEvents channel = do
  reached <- newEmptyTMVarIO
  atomically $ do
    checkOpen channel
    writeTQueue (channelEvents channel) (EventBoundary reached)
  atomically $
    readTMVar reached
      `orElse` (readTMVar (channelStopped channel) >>= throwSTM)

readMessages :: RpcChannel -> IO Object -> IO ()
readMessages channel receive =
  forever
    ( do
        fields <- receive
        message <- case fromJSON (Object fields) of
          Error _ -> throwIO RpcMalformedMessage
          Success value -> pure value
        case envelopeBody message of
          ResponseBody response | Just identifier <- genericResponseId response -> do
            receivedAt <- getCurrentTime
            atomically $ do
              checkOpen channel
              pending <- readTVar (channelPending channel)
              forM_ (Map.lookup identifier pending) $ \(PendingRequest _ session reply observe) -> do
                let result = responseValue message response identifier
                accepted <- tryPutTMVar reply result
                when accepted $ do
                  forM_ result $ \value -> forM_ observe $ \callback -> writeTQueue (channelEvents channel) (ReplyObservation (callback receivedAt value))
                  queueSettlement channel identifier session
          -- ponytail: event queue is unbounded; add an explicit overflow policy if consumers lag.
          _ -> atomically $ checkOpen channel >> writeTQueue (channelEvents channel) (QueuedMessage message)
    )
    `catch` \(err :: SomeException) -> do
      atomically (stopChannel channel (fromMaybe RpcChannelReadFailure (fromException err)) (Just err))
      finallyPreserving (throwIO err) (logChannelFailure channel (fromMaybe RpcChannelReadFailure (fromException err)))

responseValue :: JsonRpcMessage -> BaseResponseGeneric -> Text -> Either RpcChannelError JsonRpcBaseResponse
responseValue message response identifier =
  case genericResponseError response of
    Just err -> Right (message {envelopeBody = BaseFailure (BaseResponseFailure (Just identifier) err (genericResponseResult response) extras)})
    Nothing -> case genericResponseResult response of
      Just result -> Right (message {envelopeBody = BaseSuccess (BaseResponseSuccess identifier (Just result) Nothing extras)})
      Nothing -> Left RpcMalformedResponse
  where
    extras = genericResponseAdditionalFields response

checkOpen :: RpcChannel -> STM ()
checkOpen channel = tryReadTMVar (channelStopped channel) >>= mapM_ throwSTM

stopChannel :: RpcChannel -> RpcChannelError -> Maybe SomeException -> STM ()
stopChannel channel failure cause = do
  first <- tryPutTMVar (channelStopped channel) failure
  when first $ do
    writeTVar (channelStopCause channel) cause
    pending <- readTVar (channelPending channel)
    let order (_, PendingRequest sequenceNumber _ _ _) = sequenceNumber
    forM_ (sortOn order (Map.toList pending)) $ \(identifier, PendingRequest _ session reply _) -> do
      accepted <- tryPutTMVar reply (Left failure)
      when accepted (queueSettlement channel identifier session)
    modifyTVar' (channelHooks channel) (\hooks -> hooks {hookBeforeRequest = Nothing, hookRequestBarriers = Map.empty, hookSettled = Map.empty})
