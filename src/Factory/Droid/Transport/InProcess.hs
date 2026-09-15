{-# LANGUAGE OverloadedStrings #-}

-- | Frame-based access to a supplied in-process runtime. RPC construction,
-- validation and correlation remain in Protocol/Client, not runtime methods.
module Factory.Droid.Transport.InProcess
  ( InProcessRuntime (..),
    defaultInProcessRuntime,
    InProcessEvent (..),
    ObjectInProcess,
    ChannelError (..),
    withInProcessChannel,
    closeInProcess,
    isInProcessConnected,
    setPendingSessionReady,
    sendObject,
    receiveObject,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch, mask_, throwIO, try)
import Control.Monad (forM_, when)
import Data.Aeson (Object)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.Exception (finallyPreserving)
import Factory.Droid.Internal.MessageChannel (ChannelError (..))
import Factory.Droid.Internal.MessageChannel qualified as Channel

data InProcessRuntime = InProcessRuntime
  { inProcessConnect :: !(Maybe (Text -> IO ())),
    inProcessDisconnect :: !(Maybe (IO ())),
    inProcessSendMessage :: !(Text -> IO ()),
    inProcessOnMessage :: !(Maybe ((Text -> IO ()) -> IO (IO ()))),
    inProcessOnClose :: !(Maybe ((Scientific -> Text -> IO ()) -> IO (IO ()))),
    inProcessOnError :: !(Maybe ((SomeException -> IO ()) -> IO (IO ()))),
    inProcessPendingSessionReady :: !(Maybe (Text -> STM () -> IO ()))
  }

instance Show InProcessRuntime where show _ = "InProcessRuntime <redacted>"

defaultInProcessRuntime :: (Text -> IO ()) -> InProcessRuntime
defaultInProcessRuntime send = InProcessRuntime Nothing Nothing send Nothing Nothing Nothing Nothing

data InProcessEvent = InProcessOpened | InProcessClosed !Scientific !Text | InProcessError !SomeException

instance Show InProcessEvent where
  show = \case
    InProcessOpened -> "InProcessOpened"
    InProcessClosed _ _ -> "InProcessClosed <redacted>"
    InProcessError _ -> "InProcessError <redacted>"

data ObjectInProcess = ObjectInProcess
  { channel :: !Channel.ObjectChannel,
    runtime :: !InProcessRuntime,
    observe :: !(InProcessEvent -> IO ()),
    closed :: !(MVar Bool)
  }

-- | Install subscriptions before connect. The observer receives lifecycle and
-- nonterminal runtime errors; it may intentionally ignore events. Registration
-- is exception-safe and teardown is cooperative and does not reenter close.
-- Observers run on the caller/runtime delivery thread; do not reenter writes
-- from a send-error observer or wait for work depending on that delivery.
-- The supplied disconnect action is scoped cleanup, attempted once even after
-- failed connect or reported peer close. It must tolerate partial/closed setup.
withInProcessChannel :: Int -> Text -> InProcessRuntime -> (InProcessEvent -> IO ()) -> (ObjectInProcess -> IO a) -> IO a
withInProcessChannel limit url supplied observer action = do
  when (limit <= 0) (throwIO InvalidChannelLimit)
  cleanup <- newMVar False
  finallyPreserving
    ( Channel.withMessageChannel limit Channel.RetainOnSendFailure source $ \transport -> do
        let connection = ObjectInProcess transport supplied observer cleanup
        finallyPreserving
          ( do
              forM_ (inProcessConnect supplied) ($ url)
              atomically (Channel.checkActive transport)
              observer InProcessOpened
              action connection
          )
          (closeInProcess connection)
    )
    (cleanupRuntime supplied cleanup)
  where
    source = Channel.MessageSource (pure True) send $ \deliver close active ->
      maybe [] (\subscribe -> [subscribe deliver]) (inProcessOnMessage supplied)
        <> maybe [] (\subscribe -> [subscribe (\code reason -> do first <- close (Just code) (Just reason); when first (observer (InProcessClosed code reason)))]) (inProcessOnClose supplied)
        <> maybe [] (\subscribe -> [subscribe (\cause -> do open <- active; when open (observer (InProcessError cause)))]) (inProcessOnError supplied)
    send message =
      inProcessSendMessage supplied message `catch` \(cause :: SomeException) -> do
        _ <- try @SomeException (observer (InProcessError cause))
        throwIO cause

-- | Idempotent local retirement and scoped runtime cleanup. Event observers
-- run outside the close lock, so they can inspect or repeat close safely.
closeInProcess :: ObjectInProcess -> IO ()
closeInProcess connection = do
  first <- finallyPreserving (Channel.finishChannel (channel connection)) (cleanupRuntime (runtime connection) (closed connection))
  when first (observe connection (InProcessClosed 1000 "In-process transport disconnect"))

cleanupRuntime :: InProcessRuntime -> MVar Bool -> IO ()
cleanupRuntime supplied cleanup = mask_ $ do
  done <- takeMVar cleanup
  if done
    then putMVar cleanup True
    else do
      result <- try @SomeException (forM_ (inProcessDisconnect supplied) id)
      putMVar cleanup True
      either throwIO pure result

isInProcessConnected :: ObjectInProcess -> IO Bool
isInProcessConnected = Channel.isConnected . channel

-- | Delegate a wait-only readiness gate without awaiting, executing or owning
-- its producer. Failure/cancellation of the producer remains in the STM gate.
setPendingSessionReady :: ObjectInProcess -> Text -> STM () -> IO ()
setPendingSessionReady connection identifier ready = do
  atomically (Channel.checkActive (channel connection))
  forM_ (inProcessPendingSessionReady (runtime connection)) (\register -> register identifier ready)

sendObject :: ObjectInProcess -> Object -> IO ()
sendObject = Channel.sendObject . channel

receiveObject :: ObjectInProcess -> IO Object
receiveObject = Channel.receiveObject . channel
