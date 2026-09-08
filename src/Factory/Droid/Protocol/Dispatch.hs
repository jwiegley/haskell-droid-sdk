{-# LANGUAGE OverloadedStrings #-}

-- | Generic callbacks and server-request workers over one RpcChannel intake.
-- This layer does not install Droid permission defaults or choose attribution.
module Factory.Droid.Protocol.Dispatch
  ( RpcDispatcher,
    RpcDispatcherError (..),
    RpcRequestHandler,
    withRpcDispatcher,
    onRpcEvent,
    onRpcNotification,
    onRpcError,
    registerRpcHandler,
    dispatchRpcRequest,
  )
where

import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, link, mapConcurrently_, uninterruptibleCancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, throwSTM, writeTVar)
import Control.Exception (Exception, catch, finally, mask, mask_, onException, uninterruptibleMask_)
import Control.Monad (forM_, void, when)
import Data.Aeson (Value)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Factory.Droid.Internal.Exception (trySync)
import Factory.Droid.Protocol (RpcChannel, RpcChannelError, receiveRpcEvent, sendRpcMessage)
import Factory.Droid.Schema.RPC

-- | Registration failures distinguish dispatcher closure from channel failure.
data RpcDispatcherError = RpcDispatcherClosed | RpcDispatcherFailed !RpcChannelError
  deriving stock (Eq, Show)

instance Exception RpcDispatcherError

-- | A raw handler receives the complete validated base request. Result JSON is
-- explicitly dynamic; operation-specific adapters must validate their payloads.
-- Ordinary exceptions become a fixed internal-error response without details.
type RpcRequestHandler = JsonRpcBaseRequest -> IO (Either JsonRpcError Value)

data StopReason = ScopeClosed | ChannelFailed !RpcChannelError

data DispatcherState = DispatcherState
  { stopped :: !(Maybe StopReason),
    nextToken :: !Integer,
    handlers :: !(Map Text (Integer, RpcRequestHandler)),
    listeners :: !(Map Integer (JsonRpcMessage -> IO ())),
    errorListeners :: !(Map Integer (RpcChannelError -> IO ())),
    workers :: !(Map Text (Integer, Async ()))
  }

-- | Valid only within withRpcDispatcher. It exclusively consumes the borrowed
-- channel's events. Caller threads, including registration calls, remain owned
-- by the caller and must finish before the dispatcher scope ends.
data RpcDispatcher = RpcDispatcher !RpcChannel !JsonRpcEnvelope !(TVar DispatcherState)

-- | Run one event dispatcher and own every server-request worker it creates.
-- Responses use the caller's envelope context and the original request ID.
-- Unregistered methods receive method-not-found; no permission is granted.
--
-- Scope exit seals registration before concurrently cancelling and joining the
-- loop and workers. Already-started writes may finish; none outlive the scope.
-- Cancelling a response write can poison the borrowed channel. Callbacks must
-- support asynchronous cancellation; no absolute shutdown deadline is promised.
withRpcDispatcher :: RpcChannel -> JsonRpcEnvelope -> (RpcDispatcher -> IO a) -> IO a
withRpcDispatcher channel context action = do
  state <- newTVarIO (DispatcherState Nothing 0 Map.empty Map.empty Map.empty Map.empty)
  let dispatcher = RpcDispatcher channel context state
  withAsync (runDispatcher dispatcher) $ \loop -> do
    link loop
    action dispatcher `finally` do
      (owned, _) <- atomically (seal state ScopeClosed)
      uninterruptibleMask_ (mapConcurrently_ cancel (loop : owned))

-- | Subscribe to notifications, server requests and null-ID responses. Delivery
-- uses a registration-order snapshot per message. Callbacks run serially on the
-- dispatcher: keep them brief; outbound requests are safe because correlation
-- has its own reader. Ordinary callback exceptions are isolated. The returned
-- unsubscribe action is idempotent and affects later snapshots, not work begun.
onRpcEvent :: RpcDispatcher -> (JsonRpcMessage -> IO ()) -> IO (IO ())
onRpcEvent (RpcDispatcher _ _ state) callback = do
  token <- atomically $ do
    current <- openState state
    let token = nextToken current
    writeTVar state current {nextToken = token + 1, listeners = Map.insert token callback (listeners current)}
    pure token
  pure (atomically (modifyTVar' state (\current -> current {listeners = Map.delete token (listeners current)})))

-- | Subscribe only to notifications, retaining their complete envelope.
onRpcNotification :: RpcDispatcher -> (JsonRpcBaseNotification -> IO ()) -> IO (IO ())
onRpcNotification dispatcher callback = onRpcEvent dispatcher $ \message -> case envelopeBody message of
  NotificationBody notification -> callback (message {envelopeBody = notification})
  _ -> pure ()

-- | Subscribe to terminal channel errors. An already recorded terminal error
-- is replayed immediately; normal scoped closure is not reported as a transport
-- failure. Replay runs on the registering thread. Unsubscribe is idempotent.
onRpcError :: RpcDispatcher -> (RpcChannelError -> IO ()) -> IO (IO ())
onRpcError (RpcDispatcher _ _ state) callback = do
  registration <- atomically $ do
    current <- readTVar state
    case stopped current of
      Just ScopeClosed -> throwSTM RpcDispatcherClosed
      Just (ChannelFailed err) -> pure (Left err)
      Nothing -> do
        let token = nextToken current
        writeTVar state current {nextToken = token + 1, errorListeners = Map.insert token callback (errorListeners current)}
        pure (Right token)
  case registration of
    Left err -> void (trySync (callback err)) >> pure (pure ())
    Right token -> pure (atomically (modifyTVar' state (\current -> current {errorListeners = Map.delete token (errorListeners current)})))

-- | Install or replace a method handler. Unsubscribing an older registration
-- cannot remove its successor. Active requests retain their handler snapshot;
-- removal affects later requests and does not cancel already-running work.
registerRpcHandler :: RpcDispatcher -> Text -> RpcRequestHandler -> IO (IO ())
registerRpcHandler (RpcDispatcher _ _ state) method handler = do
  token <- atomically $ do
    current <- openState state
    let token = nextToken current
    writeTVar state current {nextToken = token + 1, handlers = Map.insert method (token, handler) (handlers current)}
    pure token
  pure (atomically (modifyTVar' state (\current -> current {handlers = deleteOwned method token (handlers current)})))

runDispatcher :: RpcDispatcher -> IO ()
runDispatcher dispatcher@(RpcDispatcher channel _ state) =
  (loop `catch` failed) `finally` do
    owned <- atomically $ do
      current <- readTVar state
      case stopped current of
        Nothing -> fst <$> seal state ScopeClosed
        Just _ -> pure []
    uninterruptibleMask_ (mapConcurrently_ cancel owned)
  where
    loop = do
      message <- receiveRpcEvent channel
      callbacks <- atomically $ do
        current <- readTVar state
        pure $ case stopped current of
          Nothing -> Map.elems (listeners current)
          Just _ -> []
      forM_ callbacks $ \callback -> void (trySync (callback message))
      case envelopeBody message of
        RequestBody request -> startWorker dispatcher (message {envelopeBody = request})
        _ -> pure ()
      continue <- atomically $ do
        current <- readTVar state
        pure $ case stopped current of
          Nothing -> True
          Just _ -> False
      when continue loop
    failed err = mask $ \restore -> do
      (owned, callbacks) <- atomically (seal state (ChannelFailed err))
      uninterruptibleMask_ (mapConcurrently_ cancel owned)
      restore (forM_ callbacks (\callback -> void (trySync (callback err))))

-- | Restore an already decoded server request from a session snapshot using
-- the same owned workers and active-ID deduplication as live requests. Like
-- intake dispatch, closed dispatchers and already active IDs admit no new work.
dispatchRpcRequest :: RpcDispatcher -> JsonRpcBaseRequest -> IO ()
dispatchRpcRequest = startWorker

startWorker :: RpcDispatcher -> JsonRpcBaseRequest -> IO ()
startWorker dispatcher@(RpcDispatcher _ _ state) request = mask_ $ do
  let identifier = baseRequestId (envelopeBody request)
      method = baseRequestMethod (envelopeBody request)
  selected <- atomically $ do
    current <- readTVar state
    case stopped current of
      Just _ -> pure Nothing
      Nothing | Map.member identifier (workers current) -> pure Nothing
      Nothing -> do
        let token = nextToken current
        writeTVar state current {nextToken = token + 1}
        pure (Just (token, snd <$> Map.lookup method (handlers current)))
  forM_ selected $ \(token, handler) -> do
    ready <- newEmptyMVar
    worker <- asyncWithUnmask $ \unmask ->
      unmask (takeMVar ready >> handleRequest dispatcher request handler)
        `finally` atomically (modifyTVar' state (\current -> current {workers = deleteOwned identifier token (workers current)}))
    ( do
        admitted <- atomically $ do
          current <- readTVar state
          case stopped current of
            Just _ -> pure False
            Nothing | Map.member identifier (workers current) -> pure False
            Nothing -> do
              writeTVar state current {workers = Map.insert identifier (token, worker) (workers current)}
              pure True
        if admitted then putMVar ready () else uninterruptibleCancel worker
      )
      `onException` uninterruptibleCancel worker

handleRequest :: RpcDispatcher -> JsonRpcBaseRequest -> Maybe RpcRequestHandler -> IO ()
handleRequest (RpcDispatcher channel context state) request handler = do
  result <- case handler of
    Nothing -> pure (Left (JsonRpcError RpcMethodNotFound "No RPC handler registered" Nothing mempty))
    Just action -> fromRight (Left (JsonRpcError RpcInternalError "RPC request handler failed" Nothing mempty)) <$> trySync (action request)
  admitted <- atomically $ do
    current <- readTVar state
    pure $ case stopped current of
      Nothing -> True
      Just _ -> False
  when admitted $ do
    let identifier = Just (baseRequestId (envelopeBody request))
        extras = envelopeBody context
        response = case result of
          Left err -> BaseResponseGeneric identifier Nothing (Just err) extras
          Right value -> BaseResponseGeneric identifier (Just value) Nothing extras
    sendRpcMessage channel (context {envelopeBody = ResponseBody response})

openState :: TVar DispatcherState -> STM DispatcherState
openState state = do
  current <- readTVar state
  case stopped current of
    Nothing -> pure current
    Just ScopeClosed -> throwSTM RpcDispatcherClosed
    Just (ChannelFailed err) -> throwSTM (RpcDispatcherFailed err)

-- Claim each worker exactly once so concurrent scope/failure cleanup cannot
-- send a second cancellation while a handler's resource finalizer is running.
seal :: TVar DispatcherState -> StopReason -> STM ([Async ()], [RpcChannelError -> IO ()])
seal state reason = do
  current <- readTVar state
  case stopped current of
    Nothing -> do
      writeTVar state current {stopped = Just reason, handlers = Map.empty, listeners = Map.empty, errorListeners = Map.empty, workers = Map.empty}
      pure (map snd (Map.elems (workers current)), Map.elems (errorListeners current))
    Just _ -> do
      case reason of
        ScopeClosed -> writeTVar state current {stopped = Just ScopeClosed}
        ChannelFailed _ -> pure ()
      pure ([], [])

deleteOwned :: (Ord key) => key -> Integer -> Map key (Integer, value) -> Map key (Integer, value)
deleteOwned key token = Map.update (\entry@(owner, _) -> if owner == token then Nothing else Just entry) key
