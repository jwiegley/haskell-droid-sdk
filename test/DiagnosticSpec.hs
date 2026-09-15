{-# LANGUAGE OverloadedStrings #-}

module DiagnosticSpec (diagnosticTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, (>=>))
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Control (SubmitBugReportParams (..), SubmitBugReportResult)
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Schema.Sources (BugReportSource, BugReportSourceError (..), validateBugReportSource)
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

diagnosticTests :: TestTree
diagnosticTests =
  testGroup
    "Diagnostic submission"
    [ testCase "runtime-oversized source is rejected before local dispatch" $ bounded $ do
        source <- decodeValue (object ["surface" .= String "cli", "version" .= Text.replicate 51 "😀"])
        incoming <- newTQueueIO
        sent <- newIORef []
        let write frame = do
              modifyIORef' sent (<> [frame])
              atomically (writeTQueue incoming (response frame (object ["bugReportId" .= String "synthetic-report"])))
        withRpcChannel write (atomically (readTQueue incoming)) $ \channel -> do
          result <- try @BugReportSourceError (Client.submitBugReport channel (Client.CallOptions "report" (WithEnvelope Nothing Nothing mempty) (Just 1000000)) (SubmitBugReportParams "synthetic comment" Nothing (Just source) mempty))
          result @?= Left (BugReportSourceTooLong "version" 100)
        readIORef sent >>= (@?= []),
      testCase "all five runtime bounds match the reference Unicode vectors without changing the codec" $
        forM_ diagnosticFields $ \(key, limit) -> do
          let vectors = [("", True), (Text.replicate limit "a", True), (Text.replicate limit "中", True), (Text.replicate (limit `div` 2) "😀", True), ("😀" <> Text.replicate (limit - 2) "a", True), (Text.replicate (limit `div` 2) "e\x0301", True), (Text.replicate (limit `div` 2) "😀" <> "a", False), (Text.replicate (limit `div` 2 + 1) "😀", False), (Text.replicate limit "😀", False)]
          forM_ vectors $ \(text, accepted) -> do
            let wire = object ["surface" .= String "cli", key .= text, "future" .= False]
            source <- decodeValue @BugReportSource wire
            toJSON source @?= wire
            validateBugReportSource source @?= if accepted then Right source else Left (BugReportSourceTooLong (Key.toText key) (fromIntegral limit)),
      testCase "both low-level paths preflight every field before dispatch and expose value-free errors" $ bounded $ do
        incoming <- newTQueueIO
        sent <- newIORef []
        let write frame = do
              modifyIORef' sent (<> [frame])
              atomically (writeTQueue incoming (response frame reportResult))
        withRpcChannel write (atomically (readTQueue incoming)) $ \channel ->
          forM_ diagnosticFields $ \(key, limit) -> do
            source <- decodeValue (object ["surface" .= String "cli", key .= Text.replicate (limit `div` 2 + 1) "😀"])
            let params = SubmitBugReportParams "synthetic" Nothing (Just source) mempty
            forM_ reportCalls $ \(_, call) -> try @BugReportSourceError (call channel lowOptions params) >>= (@?= Left (BugReportSourceTooLong (Key.toText key) (fromIntegral limit)))
        readIORef sent >>= (@?= [])
        show (BugReportSourceTooLong "version" 100) @?= "BugReportSourceTooLong \"version\" 100",
      testCase "daemon reports preserve explicit content and source, with no inferred diagnostics or retry" $ bounded $ do
        trace <- newIORef []
        mode <- newIORef Normal
        ready <- newEmptyMVar
        rich <- richParams
        withReportPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          forM_ [minimalParams, emptyParams, rich] (Daemon.submitBugReport connection "target-session" >=> ((@?= reportResult) . toJSON))
          invalid <- decodeValue (object ["surface" .= String "cli", "platform" .= Text.replicate 17 "😀"])
          try @BugReportSourceError (Daemon.submitBugReport connection "target-session" (rich {bugReportSource = Just invalid})) >>= (@?= Left (BugReportSourceTooLong "platform" 32))
          Daemon.submitBugReport connection "target-session" minimalParams >>= (@?= reportResult) . toJSON
        frames <- readIORef trace
        map (field "method") frames @?= map String ("daemon.authenticate" : replicate 4 "daemon.submit_bug_report")
        map (field "params") (drop 1 frames) @?= [daemonBody minimalWire, daemonBody emptyWire, daemonBody richWire, daemonBody minimalWire],
      testCase "remote and malformed report results propagate and the connection remains usable" $ bounded $ do
        trace <- newIORef []
        mode <- newIORef Normal
        ready <- newEmptyMVar
        withReportPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ [Rejected, Malformed] $ \failure -> do
            writeIORef mode failure
            result <- try @RpcResultError (Daemon.submitBugReport connection "target-session" minimalParams)
            case result of
              Left (RpcRemoteFailure err) | failure == Rejected -> do
                rpcErrorCode err @?= RpcInvalidParams
                rpcErrorData err @?= Just (object ["code" .= String "report_rejected"])
              Left RpcInvalidResult | failure == Malformed -> pure ()
              _ -> assertFailure "Unexpected report result"
            writeIORef mode Normal
            Daemon.submitBugReport connection "target-session" minimalParams >>= (@?= reportResult) . toJSON,
      testCase "cancellation stops waiting without remote cancellation, retry or loss of async identity" $ bounded $ do
        trace <- newIORef []
        mode <- newIORef Held
        ready <- newEmptyMVar
        withReportPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.submitBugReport connection "target-session" minimalParams) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled report returned"
          writeIORef mode Normal
          Daemon.submitBugReport connection "target-session" emptyParams >>= (@?= reportResult) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.submit_bug_report", "daemon.submit_bug_report"]) . map (field "method"),
      testCase "channel loss rejects the report and later requests" $ bounded $ do
        trace <- newIORef []
        mode <- newIORef Disconnect
        ready <- newEmptyMVar
        withReportPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ [minimalParams, emptyParams] $ \params ->
            try @RpcChannelError (Daemon.submitBugReport connection "target-session" params) >>= \case
              Left _ -> pure ()
              Right _ -> assertFailure "Closed channel accepted report",
      testCase "local and daemon wire paths preserve caller envelopes and deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        rich <- richParams
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          forM_ reportCalls $ \(method, call) -> do
            let configured = lowOptions {Client.callRequestId = method <> "-rpc"}
            withAsync (call channel configured rich) $ \pending -> do
              frame <- atomically (readTQueue outgoing)
              field "method" frame @?= String method
              field "id" frame @?= String (method <> "-rpc")
              field "factoryProtocolVersion" frame @?= String "test-protocol"
              field "callerExtra" frame @?= Bool False
              field "params" frame @?= if method == "daemon.submit_bug_report" then daemonBody richWire else richWire
              atomically (writeTQueue incoming (response frame reportResult))
              wait pending >>= (@?= reportResult) . toJSON
            try @RpcChannelError (call channel (configured {Client.callTimeoutMicros = Just 0}) minimalParams) >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
          withAsync (try @RpcChannelError (Client.submitDaemonBugReport channel (lowOptions {Client.callTimeoutMicros = Just 20000}) "target-session" minimalParams)) $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String "daemon.submit_bug_report"
            wait pending >>= (@?= Left RpcRequestTimedOut)
    ]

response :: Object -> Value -> Object
response request result = KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "type" .= String "response", "id" .= fromMaybe Null (KeyMap.lookup "id" request), "result" .= result]

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

diagnosticFields :: [(Key, Int)]
diagnosticFields = [("version", 100), ("cliVersion", 100), ("osVersion", 100), ("platform", 32), ("arch", 32)]

reportCalls :: [(Text, RpcChannel -> Client.CallOptions -> SubmitBugReportParams -> IO SubmitBugReportResult)]
reportCalls = [("droid.submit_bug_report", Client.submitBugReport), ("daemon.submit_bug_report", \channel configured -> Client.submitDaemonBugReport channel configured "target-session")]

lowOptions :: Client.CallOptions
lowOptions = Client.CallOptions "short-report" (WithEnvelope (Just "test-protocol") Nothing (KeyMap.fromList ["method" .= String "wrong", "id" .= String "wrong", "params" .= String "wrong", "callerExtra" .= False])) (Just 1000000)

minimalParams, emptyParams :: SubmitBugReportParams
minimalParams = SubmitBugReportParams "/not/a/local/log/path" Nothing Nothing (KeyMap.fromList ["source" .= Null, "clientLogs" .= String "not inferred"])
emptyParams = SubmitBugReportParams "" (Just "") Nothing mempty

richParams :: IO SubmitBugReportParams
richParams = do
  source <- decodeValue sourceWire
  pure (SubmitBugReportParams "synthetic comment\nquoted: \"text\"" (Just "synthetic\NULlog\n") (Just source) (KeyMap.fromList ["userComment" .= String "wrong", "clientLogs" .= String "wrong", "source" .= Null, "sessionId" .= String "wrong", "future" .= False]))

minimalWire, emptyWire, richWire, sourceWire, reportResult :: Value
minimalWire = object ["userComment" .= String "/not/a/local/log/path"]
emptyWire = object ["userComment" .= String "", "clientLogs" .= String ""]
richWire = object ["userComment" .= String "synthetic comment\nquoted: \"text\"", "clientLogs" .= String "synthetic\NULlog\n", "source" .= sourceWire, "sessionId" .= String "wrong", "future" .= False]
sourceWire = object ["surface" .= String "cli", "runtime" .= String "byom", "version" .= Text.replicate 50 "😀", "cliVersion" .= String "", "platform" .= Text.replicate 16 "😀", "arch" .= String "arm64", "osVersion" .= String "  os version  ", "future" .= False]
reportResult = object ["bugReportId" .= String "synthetic-report", "future" .= False]

daemonBody :: Value -> Value
daemonBody (Object fields) = Object (KeyMap.insert "sessionId" (String "target-session") fields)
daemonBody _ = error "Expected report object fixture"

data Mode = Normal | Rejected | Malformed | Held | Disconnect deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withReportPeer :: IORef Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withReportPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      forever $ do
        request <- readFrame connection
        field "method" request @?= String "daemon.submit_bug_report"
        params <- decodeValue @Object (field "params" request)
        field "sessionId" params @?= String "target-session"
        readIORef mode >>= \case
          Rejected -> WS.sendTextData connection (encode (Object (KeyMap.insert "error" (object ["code" .= RpcInvalidParams, "message" .= String "Report rejected", "data" .= object ["code" .= String "report_rejected"]]) (KeyMap.delete "result" (response request Null)))))
          Malformed -> reply connection request (object [])
          Held -> putMVar ready ()
          Disconnect -> WS.sendClose connection ("report connection closed" :: Text)
          Normal -> reply connection request reportResult
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key
