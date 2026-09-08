{-# LANGUAGE OverloadedStrings #-}

-- | Scoped WebSocket JSON-object transport. TLS validates certificates by
-- default; authentication belongs to the protocol carried over the connection.
module Factory.Droid.Transport.WebSocket
  ( WebSocketTarget (..),
    WebSocketOptions (..),
    defaultWebSocketOptions,
    ObjectWebSocket,
    WebSocketError (..),
    withWebSocket,
    sendObject,
    receiveObject,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception, Handler (..), IOException, bracket, catches, finally, onException, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (Object, eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word16)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Connection qualified as Connection
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Network.TLS qualified as TLS
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as Stream
import System.Timeout (timeout)

-- | ASCII DNS name or unbracketed IP literal, port, and encoded origin-form
-- path (optionally with a query). No URL userinfo or credentials are read.
data WebSocketTarget = WebSocketTarget
  { webSocketHost :: !String,
    webSocketPort :: !Int,
    webSocketPath :: !String
  }
  deriving stock (Eq)

instance Show WebSocketTarget where
  show _ = "WebSocketTarget <redacted>"

-- | Explicit transport limits and TLS policy. Nothing selects plaintext;
-- it provides no confidentiality. A custom TLSSettings may supply private CAs.
-- Times are microseconds; a nonpositive close grace skips graceful closure.
data WebSocketOptions = WebSocketOptions
  { webSocketTLS :: !(Maybe Connection.TLSSettings),
    webSocketMessageLimitBytes :: !Int,
    webSocketConnectTimeoutMicros :: !Int,
    webSocketCloseTimeoutMicros :: !Int
  }

instance Show WebSocketOptions where
  show _ = "WebSocketOptions <redacted>"

defaultWebSocketOptions :: WebSocketOptions
defaultWebSocketOptions = WebSocketOptions (Just def) (10 * 1024 * 1024) 60000000 1000000

-- | Payload-free failures. Limits on received frames/assembled messages are
-- read failures; invalid JSON/UTF-8 is WebSocketInvalidObject.
data WebSocketError
  = InvalidWebSocketTarget
  | InvalidWebSocketLimit
  | InvalidWebSocketTimeout
  | WebSocketConnectFailure
  | WebSocketConnectTimeout
  | WebSocketReadFailure
  | WebSocketWriteFailure
  | WebSocketPeerClosed !Word16
  | WebSocketBinaryMessage
  | WebSocketInvalidObject
  | WebSocketMessageTooLarge
  deriving stock (Eq, Show)

instance Exception WebSocketError

-- | Borrowed only inside withWebSocket. Finish caller-owned I/O threads before
-- leaving its callback. Failed/cancelled I/O makes subsequent operations fail.
data ObjectWebSocket = ObjectWebSocket !Int !WS.Connection !(MVar ()) !(MVar ()) !(IORef Bool)

-- | One socket owner spans TCP/TLS/HTTP setup, user work and bounded graceful
-- closure. Setup shares one deadline; the user callback is outside it. The HTTP
-- response header is limited to 64 KiB. Compression is not negotiated.
withWebSocket :: WebSocketOptions -> WebSocketTarget -> (ObjectWebSocket -> IO a) -> IO a
withWebSocket options target action = do
  let limit = webSocketMessageLimitBytes options
      connectMicros = webSocketConnectTimeoutMicros options
  when (limit <= 0) (throwIO InvalidWebSocketLimit)
  when (connectMicros < 0) (throwIO InvalidWebSocketTimeout)
  unless (validTarget target) (throwIO InvalidWebSocketTarget)
  started <- toInteger <$> getMonotonicTimeNSec
  let deadline = started + toInteger connectMicros * 1000
      settings =
        WS.defaultConnectionOptions
          { WS.connectionFramePayloadSizeLimit = WS.SizeLimit (fromIntegral (max 125 limit)),
            WS.connectionMessageDataSizeLimit = WS.SizeLimit (fromIntegral limit)
          }
      hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
  addresses <- connectBoundary $ beforeDeadline deadline (Socket.getAddrInfo (Just hints) (Just (webSocketHost target)) (Just (show (webSocketPort target))))
  let attempt [] = throwIO WebSocketConnectFailure
      attempt (address : rest) = do
        outcome <- bracket (connectBoundary (Socket.socket (Socket.addrFamily address) Socket.Stream (Socket.addrProtocol address))) Socket.close $ \socket -> do
          connected <- try @IOException $ beforeDeadline deadline $ do
            Socket.setSocketOption socket Socket.NoDelay 1
            Socket.connect socket (Socket.addrAddress address)
          case connected of
            Left _ -> pure Nothing
            Right () -> do
              (stream, connection, closeTLS) <- connectBoundary (beforeDeadline deadline (openWebSocket socket target options settings))
              healthy <- newIORef True
              channel <- ObjectWebSocket limit connection <$> newMVar () <*> newMVar () <*> pure healthy
              let close = do
                    graceful <- readIORef healthy
                    writeIORef healthy False
                    when (graceful && webSocketCloseTimeoutMicros options > 0) $
                      void (timeout (webSocketCloseTimeoutMicros options) (finish connection >> closeTLS))
                  release = Socket.close socket `finally` Stream.close stream
              Just <$> (action channel `finally` (close `finally` release))
        maybe (attempt rest) pure outcome
  attempt addresses

sendObject :: ObjectWebSocket -> Object -> IO ()
sendObject (ObjectWebSocket limit connection writer _ healthy) value = withMVar writer $ \() ->
  ( do
      readIORef healthy >>= (`unless` throwIO WebSocketWriteFailure)
      let bytes = encode value
      when (BL.length bytes > fromIntegral limit) (throwIO WebSocketMessageTooLarge)
      WS.sendTextData connection bytes
        `catches` [ Handler (\(_ :: IOException) -> throwIO WebSocketWriteFailure),
                    Handler (\(_ :: WS.ConnectionException) -> throwIO WebSocketWriteFailure),
                    Handler (\(_ :: TLS.TLSException) -> throwIO WebSocketWriteFailure)
                  ]
  )
    `onException` writeIORef healthy False

receiveObject :: ObjectWebSocket -> IO Object
receiveObject (ObjectWebSocket _ connection _ reader healthy) = withMVar reader $ \() ->
  ( do
      readIORef healthy >>= (`unless` throwIO WebSocketReadFailure)
      message <-
        WS.receiveDataMessage connection
          `catches` [ Handler (\(_ :: IOException) -> throwIO WebSocketReadFailure),
                      Handler (\(_ :: TLS.TLSException) -> throwIO WebSocketReadFailure),
                      Handler
                        ( \case
                            WS.CloseRequest code _ -> throwIO (WebSocketPeerClosed code)
                            WS.UnicodeException _ -> throwIO WebSocketInvalidObject
                            _ -> throwIO WebSocketReadFailure
                        )
                    ]
      case message of
        WS.Binary _ -> throwIO WebSocketBinaryMessage
        WS.Text bytes _ -> either (const (throwIO WebSocketInvalidObject)) pure (eitherDecode bytes)
  )
    `onException` writeIORef healthy False

validTarget :: WebSocketTarget -> Bool
validTarget (WebSocketTarget host port path) =
  not (null host)
    && all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` (".-_:%" :: String)) host
    && port > 0
    && port <= 65535
    && case path of
      '/' : _ -> all (\c -> c >= '!' && c <= '~' && c /= '#' && c /= '\\') path
      _ -> False

beforeDeadline :: Integer -> IO a -> IO a
beforeDeadline deadline action = do
  now <- toInteger <$> getMonotonicTimeNSec
  let remaining = fromInteger (max 0 ((deadline - now) `div` 1000))
  timeout remaining action >>= maybe (throwIO WebSocketConnectTimeout) pure

-- The raw socket already has an owner, including when setup times out just as
-- an intermediate TLS context or WebSocket connection has been constructed.
openWebSocket :: Socket.Socket -> WebSocketTarget -> WebSocketOptions -> WS.ConnectionOptions -> IO (Stream.Stream, WS.Connection, IO ())
openWebSocket socket target options settings = do
  (readBytes, writeBytes, closeTLS) <- case webSocketTLS options of
    Nothing -> pure (SocketBytes.recv socket, SocketBytes.sendAll socket, pure ())
    Just tls -> do
      context <- Connection.initConnectionContext
      connection <- Connection.connectFromSocket context socket (Connection.ConnectionParams (webSocketHost target) (fromIntegral (webSocketPort target)) (Just tls) Nothing)
      pure (Connection.connectionGet connection, Connection.connectionPut connection, ignoreClose (Connection.connectionClose connection))
  headerBudget <- newIORef (Just (64 * 1024))
  stream <-
    Stream.makeStream
      ( do
          budget <- readIORef headerBudget
          let count = maybe 8192 (min 8192) budget
          when (count <= 0) (throwIO WebSocketConnectFailure)
          bytes <- readBytes count
          forM_ budget $ \left -> writeIORef headerBudget (Just (left - BS.length bytes))
          pure (if BS.null bytes then Nothing else Just bytes)
      )
      (mapM_ (mapM_ writeBytes . BL.toChunks))
  let host = webSocketHost target
      port = webSocketPort target
      defaultPort = maybe 80 (const 443) (webSocketTLS options)
      hostHeader = (if ':' `elem` host then "[" <> host <> "]" else host) <> if port == defaultPort then "" else ":" <> show port
  connection <- WS.newClientConnection stream hostHeader (webSocketPath target) settings []
  writeIORef headerBudget Nothing
  pure (stream, connection, closeTLS)

connectBoundary :: IO a -> IO a
connectBoundary action =
  action
    `catches` [ Handler (\(_ :: IOException) -> throwIO WebSocketConnectFailure),
                Handler (\(_ :: TLS.TLSException) -> throwIO WebSocketConnectFailure),
                Handler (\(_ :: WS.HandshakeException) -> throwIO WebSocketConnectFailure),
                Handler (\(_ :: WS.ConnectionException) -> throwIO WebSocketConnectFailure)
              ]

ignoreClose :: IO () -> IO ()
ignoreClose action = action `catches` [Handler (\(_ :: IOException) -> pure ()), Handler (\(_ :: TLS.TLSException) -> pure ())]

finish :: WS.Connection -> IO ()
finish connection =
  ignoreClose (WS.sendClose connection ("Client disconnect" :: BL.ByteString) >> forever (void (WS.receiveDataMessage connection)))
    `catches` [Handler (\(_ :: WS.ConnectionException) -> pure ())]
