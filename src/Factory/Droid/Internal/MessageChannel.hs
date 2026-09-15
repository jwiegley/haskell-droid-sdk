{-# LANGUAGE OverloadedStrings #-}

module Factory.Droid.Internal.MessageChannel
  ( MessageSource (..),
    SendFailurePolicy (..),
    ObjectChannel,
    ChannelError (..),
    withMessageChannel,
    closeChannel,
    finishChannel,
    isConnected,
    checkActive,
    sendObject,
    receiveObject,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, allowInterrupt, catch, finally, fromException, mask, mask_, throwIO, try, uninterruptibleMask_)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Object, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Internal.Exception (finallyPreserving)

data SendFailurePolicy = RetireOnSendFailure | RetainOnSendFailure deriving stock (Eq)

data MessageSource = MessageSource
  { sourceAvailable :: !(IO Bool),
    sourceSend :: !(Text -> IO ()),
    sourceSubscriptions :: !((Text -> IO ()) -> (Maybe Scientific -> Maybe Text -> IO Bool) -> IO Bool -> [IO (IO ())])
  }

data ChannelError = InvalidChannelLimit | ChannelUnavailable | ChannelClosed | ChannelPeerClosed !(Maybe Scientific) !(Maybe Text) | ChannelMessageTooLarge | ChannelInvalidObject | ChannelReadFailure | ChannelWriteFailure
  deriving stock (Eq)

instance Show ChannelError where
  show = \case
    InvalidChannelLimit -> "InvalidChannelLimit"
    ChannelUnavailable -> "ChannelUnavailable"
    ChannelClosed -> "ChannelClosed"
    ChannelPeerClosed _ _ -> "ChannelPeerClosed <redacted>"
    ChannelMessageTooLarge -> "ChannelMessageTooLarge"
    ChannelInvalidObject -> "ChannelInvalidObject"
    ChannelReadFailure -> "ChannelReadFailure"
    ChannelWriteFailure -> "ChannelWriteFailure"

instance Exception ChannelError

data ObjectChannel = ObjectChannel
  { source :: !MessageSource,
    sendFailurePolicy :: !SendFailurePolicy,
    messageLimit :: !Int,
    inbox :: !(TQueue Text),
    failure :: !(TMVar ChannelError),
    scoped :: !(TVar Bool),
    subscriptions :: !(TVar [IO ()]),
    cleanupLock :: !(MVar ()),
    writer :: !(MVar ()),
    reader :: !(MVar ())
  }

withMessageChannel :: Int -> SendFailurePolicy -> MessageSource -> (ObjectChannel -> IO a) -> IO a
withMessageChannel limit policy channel action = do
  when (limit <= 0) (throwIO InvalidChannelLimit)
  mask $ \restore -> do
    transport <- ObjectChannel channel policy limit <$> newTQueueIO <*> newEmptyTMVarIO <*> newTVarIO True <*> newTVarIO [] <*> newMVar () <*> newMVar () <*> newMVar ()
    finallyPreserving
      ( restore $ do
          checkAvailable transport
          forM_ (sourceSubscriptions channel (deliver transport) (\code reason -> failTransport transport (ChannelPeerClosed code reason)) (isConnected transport)) (install transport)
          checkAvailable transport
          action transport
      )
      (void (finishChannel transport))

-- Local retirement revokes access; scope exit also joins host-thread cleanup.
closeChannel :: ObjectChannel -> IO Bool
closeChannel transport = mask_ $ do
  atomically (writeTVar (scoped transport) False)
  failTransport transport ChannelClosed

-- Join subscription cleanup even if a host callback claimed retirement first.
finishChannel :: ObjectChannel -> IO Bool
finishChannel transport = finallyPreserving (closeChannel transport) (releaseSubscriptions transport)

isConnected :: ObjectChannel -> IO Bool
isConnected transport = atomically ((&&) <$> readTVar (scoped transport) <*> isEmptyTMVar (failure transport))

sendObject :: ObjectChannel -> Object -> IO ()
sendObject transport value = withMVar (writer transport) $ \() -> do
  text <- preservingFailure transport ChannelWriteFailure $ do
    checkAvailable transport
    let bytes = encode value
    when (BL.length bytes > fromIntegral (messageLimit transport)) (throwIO ChannelMessageTooLarge)
    pure (Text.decodeUtf8 (BL.toStrict bytes))
  let send = sourceSend (source transport) text
  case sendFailurePolicy transport of
    RetireOnSendFailure -> preservingFailure transport ChannelWriteFailure send
    RetainOnSendFailure -> send
  atomically (checkActive transport)

-- Peer-close prefixes drain; local close revokes them. Invalid input aborts at
-- that frame, atomically with retirement, so a later delivery cannot escape.
receiveObject :: ObjectChannel -> IO Object
receiveObject transport = withMVar (reader transport) $ \() -> preservingFailure transport ChannelReadFailure $ do
  message <- atomically $ do
    alive <- readTVar (scoped transport)
    unless alive (throwSTM ChannelClosed)
    readTQueue (inbox transport) `orElse` (readTMVar (failure transport) >>= throwSTM)
  case eitherDecodeStrict' (Text.encodeUtf8 message) of
    Right value -> pure value
    Left _ -> throwIO ChannelInvalidObject

checkActive :: ObjectChannel -> STM ()
checkActive transport = do
  alive <- readTVar (scoped transport)
  unless alive (throwSTM ChannelClosed)
  tryReadTMVar (failure transport) >>= mapM_ throwSTM

checkAvailable :: ObjectChannel -> IO ()
checkAvailable transport = do
  atomically (checkActive transport)
  available <- sourceAvailable (source transport)
  unless available (throwIO ChannelUnavailable)
  atomically (checkActive transport)

install :: ObjectChannel -> IO (IO ()) -> IO ()
install transport subscribe = mask_ $ do
  stop <- subscribe
  retained <- atomically $ do
    alive <- readTVar (scoped transport)
    active <- isEmptyTMVar (failure transport)
    when (alive && active) (modifyTVar' (subscriptions transport) (stop :))
    pure (alive && active)
  unless retained stop

deliver :: ObjectChannel -> Text -> IO ()
deliver transport message = do
  active <- isConnected transport
  when active $
    if Text.compareLength message (messageLimit transport) == GT || BS.length (Text.encodeUtf8 message) > messageLimit transport
      then void (failTransport transport ChannelMessageTooLarge)
      else atomically $ do
        alive <- readTVar (scoped transport)
        connected <- isEmptyTMVar (failure transport)
        -- ponytail: callback queue is unbounded, like RpcChannel's event queue;
        -- add an explicit overflow policy if a host can outrun its consumer.
        when (alive && connected) (writeTQueue (inbox transport) message)

failTransport :: ObjectChannel -> ChannelError -> IO Bool
failTransport transport reason = mask_ $ do
  first <- atomically $ do
    when (reason == ChannelInvalidObject) (void (flushTQueue (inbox transport)))
    tryPutTMVar (failure transport) reason
  when first (releaseSubscriptions transport)
  pure first

releaseSubscriptions :: ObjectChannel -> IO ()
releaseSubscriptions transport = mask_ $ do
  -- Only joining an existing cleanup is uninterruptible; host callbacks below
  -- remain interruptible and must be cooperative.
  uninterruptibleMask_ (takeMVar (cleanupLock transport))
  ( do
      stops <- atomically $ do
        current <- readTVar (subscriptions transport)
        writeTVar (subscriptions transport) []
        pure current
      runStops stops
    )
    `finally` putMVar (cleanupLock transport) ()
  allowInterrupt
  where
    runStops [] = pure ()
    runStops (stop : rest) = stop `finally` runStops rest

preservingFailure :: ObjectChannel -> ChannelError -> IO a -> IO a
preservingFailure transport fallback action =
  action `catch` \(cause :: SomeException) -> do
    _ <- try @SomeException (failTransport transport (fromMaybe fallback (fromException cause)))
    throwIO cause
