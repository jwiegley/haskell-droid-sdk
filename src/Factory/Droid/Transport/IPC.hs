{-# LANGUAGE OverloadedStrings #-}

-- | Borrow a host-owned JSON-text channel. Only subscriptions are owned here;
-- the shared callback channel supplies framing and lifetime, never an RPC reader.
module Factory.Droid.Transport.IPC
  ( IpcMessageChannel (..),
    ObjectIpc,
    IpcError (..),
    withIpcChannel,
    closeIpc,
    isIpcConnected,
    sendObject,
    receiveObject,
  )
where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (void)
import Data.Aeson (Object)
import Data.Text (Text)
import Factory.Droid.Internal.MessageChannel qualified as Channel

-- | Registration must be exception-safe; unsubscribe actions are nonblocking
-- and cooperative. Deliver messages in wire order. Send means host acceptance.
data IpcMessageChannel = IpcMessageChannel
  { ipcIsAvailable :: !(IO Bool),
    ipcSendMessage :: !(Text -> IO ()),
    ipcOnMessage :: !((Text -> IO ()) -> IO (IO ())),
    ipcOnDisconnect :: !(Maybe ((Maybe Text -> IO ()) -> IO (IO ())))
  }

instance Show IpcMessageChannel where show _ = "IpcMessageChannel <redacted>"

data IpcError = InvalidIpcLimit | IpcUnavailable | IpcClosed | IpcDisconnected !(Maybe Text) | IpcMessageTooLarge | IpcInvalidObject | IpcReadFailure | IpcWriteFailure
  deriving stock (Eq)

instance Show IpcError where
  show = \case
    InvalidIpcLimit -> "InvalidIpcLimit"
    IpcUnavailable -> "IpcUnavailable"
    IpcClosed -> "IpcClosed"
    IpcDisconnected _ -> "IpcDisconnected <redacted>"
    IpcMessageTooLarge -> "IpcMessageTooLarge"
    IpcInvalidObject -> "IpcInvalidObject"
    IpcReadFailure -> "IpcReadFailure"
    IpcWriteFailure -> "IpcWriteFailure"

instance Exception IpcError

newtype ObjectIpc = ObjectIpc Channel.ObjectChannel

-- | Borrow a positive-byte-limited channel. Finish caller I/O before leaving;
-- scope cleanup joins unsubscribe and preserves any prior callback exception.
withIpcChannel :: Int -> IpcMessageChannel -> (ObjectIpc -> IO a) -> IO a
withIpcChannel limit channel action =
  fromChannel $
    Channel.withMessageChannel limit Channel.RetireOnSendFailure source (action . ObjectIpc)
  where
    source = Channel.MessageSource (toChannel (ipcIsAvailable channel)) (toChannel . ipcSendMessage channel) $ \receive close _ ->
      [toChannel (ipcOnMessage channel receive)] <> maybe [] (\subscribe -> [toChannel (subscribe (void . close Nothing))]) (ipcOnDisconnect channel)

closeIpc :: ObjectIpc -> IO ()
closeIpc (ObjectIpc channel) = fromChannel (void (Channel.closeChannel channel))

-- | Remembered state, not a heartbeat or host-availability probe.
isIpcConnected :: ObjectIpc -> IO Bool
isIpcConnected (ObjectIpc channel) = Channel.isConnected channel

sendObject :: ObjectIpc -> Object -> IO ()
sendObject (ObjectIpc channel) = fromChannel . Channel.sendObject channel

receiveObject :: ObjectIpc -> IO Object
receiveObject (ObjectIpc channel) = fromChannel (Channel.receiveObject channel)

fromChannel :: IO a -> IO a
fromChannel action = action `catch` (throwIO . fromChannelError)

toChannel :: IO a -> IO a
toChannel action = action `catch` (throwIO . toChannelError)

fromChannelError :: Channel.ChannelError -> IpcError
fromChannelError = \case
  Channel.InvalidChannelLimit -> InvalidIpcLimit
  Channel.ChannelUnavailable -> IpcUnavailable
  Channel.ChannelClosed -> IpcClosed
  Channel.ChannelPeerClosed _ reason -> IpcDisconnected reason
  Channel.ChannelMessageTooLarge -> IpcMessageTooLarge
  Channel.ChannelInvalidObject -> IpcInvalidObject
  Channel.ChannelReadFailure -> IpcReadFailure
  Channel.ChannelWriteFailure -> IpcWriteFailure

toChannelError :: IpcError -> Channel.ChannelError
toChannelError = \case
  InvalidIpcLimit -> Channel.InvalidChannelLimit
  IpcUnavailable -> Channel.ChannelUnavailable
  IpcClosed -> Channel.ChannelClosed
  IpcDisconnected reason -> Channel.ChannelPeerClosed Nothing reason
  IpcMessageTooLarge -> Channel.ChannelMessageTooLarge
  IpcInvalidObject -> Channel.ChannelInvalidObject
  IpcReadFailure -> Channel.ChannelReadFailure
  IpcWriteFailure -> Channel.ChannelWriteFailure
