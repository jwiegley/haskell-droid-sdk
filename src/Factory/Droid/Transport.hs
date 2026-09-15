{-# LANGUAGE OverloadedStrings #-}

-- | Borrowed object I/O for normal session construction. The caller owns the
-- physical transport and must keep its scope alive around the SDK scope.
module Factory.Droid.Transport
  ( ObjectTransport (..),
    TransportLocality (..),
    TransportKind (..),
    TransportLogOptions (..),
    defaultTransportLogOptions,
    loggedObjectTransport,
    objectTransport,
    processTransport,
    ipcTransport,
    inProcessTransport,
  )
where

import Control.Concurrent.STM (STM)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Object, Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.Exception (finallyPreserving)
import Factory.Droid.Observability (DroidLogEvent (..), DroidLogLevel (..), DroidLogger, emitDroidLog)
import Factory.Droid.Transport.IPC qualified as IPC
import Factory.Droid.Transport.InProcess qualified as InProcess
import Factory.Droid.Transport.Process qualified as Process

-- | LocalHost asserts the engine can reach this process's loopback MCP servers.
-- It is not authentication, permission or folder trust.
data TransportLocality = LocalHost | UnspecifiedHost deriving stock (Eq, Show)

-- | Descriptive adapter metadata; it never selects authentication or ownership.
data TransportKind = CustomTransport | ProcessTransport | IpcTransport | InProcessTransport | WebSocketTransport | RelayTransport deriving stock (Eq, Show)

data ObjectTransport = ObjectTransport
  { transportSendObject :: !(Object -> IO ()),
    transportReceiveObject :: !(IO Object),
    transportLocality :: !TransportLocality,
    transportPendingSessionReady :: !(Maybe (Text -> STM () -> IO ())),
    transportKind :: !TransportKind
  }

instance Show ObjectTransport where show _ = "ObjectTransport <redacted>"

objectTransport :: (Object -> IO ()) -> IO Object -> ObjectTransport
objectTransport send receive = ObjectTransport send receive UnspecifiedHost Nothing CustomTransport

-- | Transport logging is explicitly enabled by decoration. Default events
-- contain direction, kind and field count only. A custom renderer explicitly
-- opts into caller-chosen content; it must avoid secrets and be cooperative.
data TransportLogOptions = TransportLogOptions
  { transportLogAttributes :: !Object,
    transportLogRenderer :: !(Maybe (Object -> Text))
  }

instance Show TransportLogOptions where show _ = "TransportLogOptions <redacted>"

defaultTransportLogOptions :: TransportLogOptions
defaultTransportLogOptions = TransportLogOptions mempty Nothing

-- | Reuse the existing send/receive actions and ownership. Sinks run on those
-- threads and must not await later traffic or reenter the same channel.
-- A failed send retains its original exception even if failure logging fails.
loggedObjectTransport :: Maybe DroidLogger -> TransportLogOptions -> ObjectTransport -> ObjectTransport
loggedObjectTransport Nothing _ transport = transport
loggedObjectTransport logger options transport =
  transport
    { transportSendObject = \fields -> do
        result <- try @SomeException (transportSendObject transport fields)
        case result of
          Right () -> void (record "out" LogDebug "Transport object sent" fields)
          Left failure -> finallyPreserving (throwIO failure) (void (record "out_failed" LogWarn "Transport object send failed" fields)),
      transportReceiveObject = do
        fields <- transportReceiveObject transport
        void (record "in" LogDebug "Transport object received" fields)
        pure fields
    }
  where
    record direction level message fields =
      emitDroidLog logger $
        DroidLogEvent
          level
          ("droid.transport." <> direction)
          (maybe message ($ fields) (transportLogRenderer options))
          (Just (KeyMap.union (KeyMap.fromList [("direction", String direction), ("transportKind", String (Text.pack (show (transportKind transport)))), ("fieldCount", Number (fromIntegral (KeyMap.size fields)))]) (transportLogAttributes options)))
          Nothing

processTransport :: Process.JsonLinesProcess -> ObjectTransport
processTransport channel = (objectTransport (Process.sendObject channel) (Process.receiveObject channel)) {transportLocality = LocalHost, transportKind = ProcessTransport}

ipcTransport :: IPC.ObjectIpc -> ObjectTransport
ipcTransport channel = (objectTransport (IPC.sendObject channel) (IPC.receiveObject channel)) {transportKind = IpcTransport}

inProcessTransport :: InProcess.ObjectInProcess -> ObjectTransport
inProcessTransport channel =
  (objectTransport (InProcess.sendObject channel) (InProcess.receiveObject channel))
    { transportLocality = LocalHost,
      transportPendingSessionReady = Just (InProcess.setPendingSessionReady channel),
      transportKind = InProcessTransport
    }
