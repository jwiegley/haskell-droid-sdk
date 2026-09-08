-- | Scoped request correlation over caller-owned object send/receive actions.
-- Use withJsonLinesProcess for a native child transport. Requests supply their
-- complete envelopes; this layer chooses neither attribution nor CLI settings.
module Factory.Droid.Protocol
  ( RpcChannel,
    RpcChannelError (..),
    RpcResultError (..),
    withRpcChannel,
    closeRpcChannel,
    requestReply,
    requestReplyObserved,
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
  )
import Control.Exception (Exception, SomeException, catch, finally, fromException, mask, onException, throwIO)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (Object), fromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
    channelStopped :: !(TMVar RpcChannelError)
  }

data PendingRequest = PendingRequest !(TMVar (Either RpcChannelError JsonRpcBaseResponse)) !(Maybe (JsonRpcBaseResponse -> STM ()))

-- Keep observer construction lazy so it runs on the consumer, not the reader.
data QueuedEvent = QueuedMessage !JsonRpcMessage | EventBoundary !(TMVar ()) | ReplyObservation (STM ())

-- | Start one reader and close the channel on scope exit. The send/receive
-- actions and their underlying transport remain caller-owned. EOF must be
-- reported by the receive action as an exception; it is terminal, not success.
-- Invalid envelopes are terminal too. Closing rejects pending requests before
-- cancelling and joining the reader, without replacing the callback exception.
withRpcChannel :: (Object -> IO ()) -> IO Object -> (RpcChannel -> IO a) -> IO a
withRpcChannel send receive action = do
  channel <- RpcChannel send <$> newMVar () <*> newTVarIO Map.empty <*> newTQueueIO <*> newEmptyTMVarIO
  withAsync (readMessages channel receive) $ \_ ->
    action channel `finally` atomically (stopChannel channel RpcChannelClosed)

-- | Seal the channel without closing its caller-owned transport. Pending and
-- future requests fail; queued events still drain. The scope retains reader
-- ownership and joins it on exit. Repeated calls preserve the first failure.
closeRpcChannel :: RpcChannel -> IO ()
closeRpcChannel channel = atomically (stopChannel channel RpcChannelClosed)

-- | Send a complete request and await its raw response, retaining remote errors
-- as BaseFailure values. IDs must be connection-unique; concurrent duplicates
-- are rejected before sending. Reusing settled IDs cannot distinguish late peer
-- responses and is outside this contract.
--
-- Nothing disables the deadline; Just n bounds sending and waiting together in
-- microseconds. Zero expires without sending; negative values are rejected.
-- Cancelling or timing out an in-progress write terminates the channel because
-- a partial frame may have been sent. Cancellation while awaiting a reply only
-- removes that pending request. Transport actions must support async cancellation.
requestReply :: RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> IO JsonRpcBaseResponse
requestReply channel deadline request = requestReplyWith channel deadline request Nothing

-- | Correlate a reply immediately, and observe its first accepted response on
-- the single event consumer at its wire-receive position. The observer must be
-- total and nonblocking: never retry, throw, or wait for other intake work.
-- Dequeue and observation commit together. Accepted observations survive waiter
-- cancellation; late, duplicate, malformed and synthetic failure replies do not
-- create observations. A running consumer is required to execute observations,
-- not to complete the request. Use 'synchronizeRpcEvents' outside callbacks when
-- subsequent work needs the observation to have run.
requestReplyObserved :: RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> (JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyObserved channel deadline request observe = requestReplyWith channel deadline request (Just observe)

requestReplyWith :: RpcChannel -> Maybe Int -> JsonRpcBaseRequest -> Maybe (JsonRpcBaseResponse -> STM ()) -> IO JsonRpcBaseResponse
requestReplyWith channel deadline request observe = do
  forM_ deadline $ \micros -> when (micros < 0) (throwIO RpcInvalidTimeout)
  mask $ \restore -> do
    reply <- newEmptyTMVarIO
    let identifier = baseRequestId (envelopeBody request)
    atomically $ do
      checkOpen channel
      pending <- readTVar (channelPending channel)
      when (Map.member identifier pending) (throwSTM RpcDuplicateRequestId)
      modifyTVar' (channelPending channel) (Map.insert identifier (PendingRequest reply observe))
    let send = sendRpcMessage channel (request {envelopeBody = RequestBody (envelopeBody request)})
        stopped = atomically (readTMVar (channelStopped channel) >> readTMVar reply)
        checkedSend =
          send `catch` \(_ :: SomeException) -> do
            failure <- atomically (tryReadTMVar (channelStopped channel))
            throwIO (fromMaybe RpcChannelWriteFailure failure)
        exchange = race checkedSend stopped >>= either (const (atomically (readTMVar reply))) pure
        timed = maybe (Just <$> exchange) (`timeout` exchange) deadline
    result <- restore timed `finally` atomically (modifyTVar' (channelPending channel) (Map.delete identifier))
    maybe (throwIO RpcRequestTimedOut) (either throwIO pure) result

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
sendRpcMessage channel message = withMVar (channelWriter channel) $ \() -> do
  atomically (checkOpen channel)
  channelSend channel (toRpcObject message)
    `onException` atomically (stopChannel channel RpcChannelWriteFailure)

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
      QueuedMessage message -> pure (Just message)
      EventBoundary reached -> void (tryPutTMVar reached ()) >> pure Nothing
      ReplyObservation observe -> observe >> pure Nothing
  maybe (restore (receiveRpcEvent channel)) pure next

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
        atomically $ do
          checkOpen channel
          case envelopeBody message of
            ResponseBody response | Just identifier <- genericResponseId response -> do
              pending <- readTVar (channelPending channel)
              forM_ (Map.lookup identifier pending) $ \(PendingRequest reply observe) -> do
                let result = responseValue message response identifier
                accepted <- tryPutTMVar reply result
                when accepted $ forM_ result $ \value ->
                  forM_ observe $ \callback -> writeTQueue (channelEvents channel) (ReplyObservation (callback value))
            -- ponytail: event queue is unbounded; add an explicit overflow policy if consumers lag.
            _ -> writeTQueue (channelEvents channel) (QueuedMessage message)
    )
    `catch` \(err :: SomeException) -> do
      atomically (stopChannel channel (fromMaybe RpcChannelReadFailure (fromException err)))
      throwIO err

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

stopChannel :: RpcChannel -> RpcChannelError -> STM ()
stopChannel channel failure = do
  first <- tryPutTMVar (channelStopped channel) failure
  when first $ do
    pending <- readTVar (channelPending channel)
    forM_ pending $ \(PendingRequest reply _) -> void (tryPutTMVar reply (Left failure))
