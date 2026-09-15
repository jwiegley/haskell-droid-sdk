{-# LANGUAGE OverloadedStrings #-}

module LoggingSpec (loggingTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (link, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (AsyncException (UserInterrupt), Exception, SomeException, bracket_, fromException, throwIO, try)
import Control.Monad (unless, void)
import DaemonSpec (AckMode (AfterAck), runDaemonPeer)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DroidSpec (assertReaped)
import Factory.Droid (defaultDroidSessionOptions, droidSessionId, resultText, sendPrompt, withDroidSessionOn)
import Factory.Droid qualified as Droid
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Observability
import Factory.Droid.Protocol
import Factory.Droid.Schema.RPC (JsonRpcBaseRequest)
import Factory.Droid.Transport
import InjectedSessionSpec (withFixtureTransport)
import ProcessSpec (bounded)
import System.Environment (getExecutablePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

loggingTests :: TestTree
loggingTests = testGroup "Observability" loggingCases

loggingCases :: [TestTree]
loggingCases =
  [ testCase "disabled logging does not evaluate the event" $
      emitDroidLog Nothing (error "disabled event forced") >>= (@?= False),
    testCase "levels, explicit errors and scalar attributes preserve exact values" $ do
      map toJSON [LogDebug, LogInfo, LogWarn, LogError] @?= map String ["debug", "info", "warn", "error"]
      (logger, events) <- recording
      let attributes = KeyMap.fromList [("empty", String ""), ("false", Bool False), ("null", Null), ("large", Number 9007199254740993), ("object", object ["private" .= True]), ("array", Array mempty)]
          failure = DroidSerializedError (Just "") "explicit" (Just "")
      emitDroidLog (Just logger) (event {droidLogAttributes = Just attributes, droidLogError = Just failure}) >>= (@?= True)
      values <- events
      case values of
        [value] -> do
          droidLogAttributes value @?= Just (KeyMap.filterWithKey (\key _ -> key /= "object" && key /= "array") attributes)
          toJSON failure @?= object ["name" .= String "", "message" .= String "explicit", "code" .= String ""]
          show value @?= "DroidLogEvent <redacted>"
        _ -> assertFailure "Missing log event",
    testCase "empty attributes become absent" $ do
      sanitizeDroidAttributes Nothing @?= Nothing
      sanitizeDroidAttributes (Just mempty) @?= Nothing
      sanitizeDroidAttributes (Just (KeyMap.singleton "nested" (Object mempty))) @?= Nothing,
    testCase "SDK content attributes become exact UTF-8 lengths without changing other fields" $ do
      (logger, events) <- recording
      let attrs = KeyMap.fromList [("output", String "界"), ("outputByteLength", Number 99), ("preview", String ""), ("stderrTail", Bool False), ("count", Number 0)]
      emitDroidLog (Just logger) (sdkLogEvent LogWarn "safe" (Just attrs) Nothing) >>= (@?= True)
      values <- events
      case values of
        [value] -> do
          droidLogName value @?= "droid.sdk.warn"
          droidLogAttributes value @?= Just (KeyMap.fromList [("outputByteLength", Number 3), ("previewByteLength", Number 0), ("count", Number 0)])
        _ -> assertFailure "Missing SDK event",
    testCase "synchronous sink and reporter failures return false" $ do
      reported <- newIORef False
      let logger =
            DroidLogger
              (const (throwIO SinkFailed))
              ( Just
                  ( \cause -> do
                      fromException cause @?= Just SinkFailed
                      writeIORef reported True
                      throwIO ReporterFailed
                  )
              )
      emitDroidLog (Just logger) event >>= (@?= False)
      readIORef reported >>= (@?= True),
    testCase "event evaluation is inside the sink failure boundary" $ do
      entered <- newIORef False
      let logger = droidLogger (const (writeIORef entered True))
      emitDroidLog (Just logger) (event {droidLogMessage = error "lazy message"}) >>= (@?= False)
      readIORef entered >>= (@?= False),
    testCase "asynchronous sink cancellation propagates without reporting it as sink failure" $ do
      reported <- newIORef False
      let logger = DroidLogger (const (throwIO UserInterrupt)) (Just (const (writeIORef reported True)))
      result <- try @SomeException (emitDroidLog (Just logger) event)
      case result of Left cause -> fromException cause @?= Just UserInterrupt; Right _ -> assertFailure "Cancellation swallowed"
      readIORef reported >>= (@?= False),
    testCase "asynchronous failure-observer cancellation propagates" $ do
      let logger = DroidLogger (const (throwIO SinkFailed)) (Just (const (throwIO UserInterrupt)))
      result <- try @SomeException (emitDroidLog (Just logger) event)
      case result of Left cause -> fromException cause @?= Just UserInterrupt; Right _ -> assertFailure "Observer cancellation swallowed",
    testCase "transport logs preserve direction and objects without default payload disclosure" $ do
      (logger, events) <- recording
      sent <- newIORef mempty
      let fields = KeyMap.fromList [("token", String "private-fixture-token"), ("prompt", String "private prompt")]
          raw = (objectTransport (writeIORef sent) (pure fields)) {transportKind = IpcTransport, transportLocality = LocalHost}
          options = defaultTransportLogOptions {transportLogAttributes = KeyMap.fromList [("direction", String "spoof"), ("childPid", Number 0)]}
          transport = loggedObjectTransport (Just logger) options raw
      transportSendObject transport fields
      transportReceiveObject transport >>= (@?= fields)
      readIORef sent >>= (@?= fields)
      transportKind transport @?= IpcTransport
      transportLocality transport @?= LocalHost
      values <- events
      map droidLogName values @?= ["droid.transport.out", "droid.transport.in"]
      map (fmap (KeyMap.lookup "direction") . droidLogAttributes) values @?= [Just (Just (String "out")), Just (Just (String "in"))]
      assertBool "Default payload leak" (all (\value -> all (\secret -> not (secret `isInfixOf` show (toJSON value))) ["private-fixture-token", "private prompt"]) values),
    testCase "ordinary sink failure does not turn a successful send into failure" $ do
      sent <- newIORef False
      let transport = loggedObjectTransport (Just (droidLogger (const (throwIO SinkFailed)))) defaultTransportLogOptions (objectTransport (const (writeIORef sent True)) (pure mempty))
      transportSendObject transport mempty
      transportReceiveObject transport >>= (@?= mempty)
      readIORef sent >>= (@?= True),
    testCase "failed sends are logged and retain the primary error over logger cancellation" $ do
      directions <- newIORef []
      let logger = droidLogger (\value -> atomicModifyIORef' directions (\xs -> (droidLogName value : xs, ())) >> throwIO UserInterrupt)
          transport = loggedObjectTransport (Just logger) defaultTransportLogOptions (objectTransport (const (throwIO SendFailed)) (pure mempty))
      result <- try @SomeException (transportSendObject transport mempty)
      case result of Left cause -> fromException cause @?= Just SendFailed; Right _ -> assertFailure "Send failure lost"
      readIORef directions >>= (@?= ["droid.transport.out_failed"]),
    testCase "disabled transport logging leaves actions alone and does not evaluate options" $ do
      let transport = loggedObjectTransport Nothing (error "disabled options forced") (objectTransport (const (pure ())) (pure mempty))
      transportSendObject transport mempty
      transportReceiveObject transport >>= (@?= mempty),
    testCase "an explicit renderer supplies caller-selected content and renderer failures are isolated" $ do
      (logger, events) <- recording
      let raw = objectTransport (const (pure ())) (pure mempty)
          selected = loggedObjectTransport (Just logger) (defaultTransportLogOptions {transportLogRenderer = Just (const "chosen")}) raw
          broken = loggedObjectTransport (Just logger) (defaultTransportLogOptions {transportLogRenderer = Just (const (error "renderer"))}) raw
      transportSendObject selected mempty
      transportSendObject broken mempty
      events >>= (@?= ["chosen"]) . map droidLogMessage,
    testCase "normal injected sessions use the logged transport and retain owned cleanup" $ bounded $ do
      identifier <- withFixtureTransport $ \raw -> do
        (logger, events) <- recording
        identifier <- withDroidSessionOn (defaultDroidSessionOptions ".") (loggedObjectTransport (Just logger) defaultTransportLogOptions raw) $ \session -> do
          result <- sendPrompt session "hello" (const (pure ()))
          resultText result @?= "Hello سلام\n😀"
          pure (droidSessionId session)
        values <- events
        assertBool "No outbound RPC log" (any ((== "droid.transport.out") . droidLogName) values)
        assertBool "No inbound RPC log" (any ((== "droid.transport.in") . droidLogName) values)
        pure identifier
      assertReaped identifier,
    testCase "metrics preserve exact signed values and normalize attributes" $ do
      recorded <- newIORef []
      let sink = droidMetricSink (\value -> atomicModifyIORef' recorded (\old -> (value : old, ())))
          metric = DroidMetricEvent "count" MetricCounter 9007199254740993.25 MetricCount (Just (KeyMap.fromList [("false", Bool False), ("nested", Object mempty)]))
      recordDroidMetric Nothing (error "disabled metric forced") >>= (@?= False)
      recordDroidMetric (Just sink) metric >>= (@?= True)
      recordDroidMetric (Just sink) (metric {droidMetricValue = -0.25}) >>= (@?= True)
      values <- reverse <$> readIORef recorded
      map droidMetricValue values @?= [9007199254740993.25, -0.25]
      map droidMetricAttributes values @?= replicate 2 (Just (KeyMap.singleton "false" (Bool False)))
      recordDroidMetric (Just (droidMetricSink (const (throwIO SinkFailed)))) metric >>= (@?= False),
    testCase "trace providers are fresh and merge only present fields" $ do
      calls <- newIORef (0 :: Int)
      let provider = droidTraceContextProvider (do n <- atomicModifyIORef' calls (\old -> (old + 1, old + 1)); pure (DroidTraceContext (Just (Text.pack (show n))) Nothing))
          carrier = KeyMap.fromList [("tracestate", String "keep"), ("future", Bool False)]
      injectDroidTraceContext Nothing carrier >>= (@?= (carrier, False))
      (first, _) <- injectDroidTraceContext (Just provider) carrier
      (second, _) <- injectDroidTraceContext (Just provider) carrier
      KeyMap.lookup "traceparent" first @?= Just (String "1")
      KeyMap.lookup "traceparent" second @?= Just (String "2")
      KeyMap.lookup "tracestate" second @?= Just (String "keep")
      let empty = droidTraceContextProvider (pure (DroidTraceContext (Just "") (Just "")))
      injectDroidTraceContext (Just empty) carrier >>= (@?= (KeyMap.insert "traceparent" (String "") (KeyMap.insert "tracestate" (String "") carrier), True)),
    testCase "failed trace providers leave the original carrier and preserve cancellation" $ do
      let carrier = KeyMap.singleton "private" (String "unchanged")
      injectDroidTraceContext (Just (droidTraceContextProvider (throwIO SinkFailed))) carrier >>= (@?= (carrier, False))
      cancelled <- try @SomeException (getDroidTraceContext (Just (droidTraceContextProvider (throwIO UserInterrupt))))
      case cancelled of Left cause -> fromException cause @?= Just UserInterrupt; Right _ -> assertFailure "Trace cancellation swallowed",
    testCase "RPC observation propagates trace, records duration and never copies request secrets" $ bounded $ do
      (logger, logs) <- recording
      metrics <- newIORef []
      wires <- newIORef []
      responses <- newTQueueIO
      let observer =
            defaultDroidObservability
              { observabilityLogger = Just logger,
                observabilityMetrics = Just (droidMetricSink (\value -> atomicModifyIORef' metrics (\old -> (value : old, ())))),
                observabilityTracing = Just (droidTraceContextProvider (pure (DroidTraceContext (Just "private-trace-context") Nothing)))
              }
          send fields = do
            atomicModifyIORef' wires (\old -> (fields : old, ()))
            let response = if KeyMap.lookup "id" fields == Just (String "remote") then KeyMap.insert "error" (object ["code" .= (-32001 :: Int), "message" .= String "private-error-payload"]) (replyFor fields) else replyFor fields
            atomically (writeTQueue responses response)
      request <- requestFixture "one"
      withObservedRpcChannel observer send (atomically (readTQueue responses)) $ \channel -> do
        void (requestReply channel (Just 1000000) request)
        remote <- requestFixture "remote"
        void (requestReply channel (Just 1000000) remote)
        atomically (getRpcPendingCount channel) >>= (@?= 0)
      frames <- readIORef wires
      map (KeyMap.lookup "_meta") frames @?= replicate 2 (Just (object ["traceparent" .= String "private-trace-context", "future" .= Bool False]))
      recorded <- readIORef metrics
      assertBool "No request duration" (any (\value -> droidMetricName value == "droid.rpc.exchange.duration" && droidMetricValue value >= 0 && droidMetricUnit value == MetricMilliseconds) recorded)
      values <- logs
      let rendered = show (map toJSON values, map toJSON recorded)
      assertBool "Remote error was not observed" (any ((== "droid.rpc.remote_error") . droidLogName) values)
      assertBool "Remote error counter absent" (any (\value -> droidMetricName value == "droid.rpc.remote_error" && droidMetricValue value == 1) recorded)
      assertBool "Request telemetry leaked content" (all (\secret -> not (secret `isInfixOf` rendered)) ["private-token", "private-prompt", "private-trace-context", "private-error-payload"]),
    testCase "trace provider waits cannot admit a stale physical write" $ bounded $ do
      entered <- newEmptyMVar
      release <- newEmptyMVar
      valid <- newTVarIO True
      sent <- newIORef False
      let provider = droidTraceContextProvider (putMVar entered () >> takeMVar release >> pure (DroidTraceContext (Just "trace") Nothing))
          observer = defaultDroidObservability {observabilityTracing = Just provider}
          admission = readTVar valid >>= \allowed -> unless allowed (throwSTM AdmissionRejected)
      request <- requestFixture "stale"
      withObservedRpcChannel observer (const (writeIORef sent True)) (atomically retry) $ \channel -> do
        try @RpcChannelError (requestReply channel (Just 0) request) >>= (@?= Left RpcRequestTimedOut)
        withAsync (requestReplyWithAdmission channel RunBeforeRequest admission (Just 1000000) request) $ \worker -> do
          takeMVar entered
          atomically (writeTVar valid False)
          putMVar release ()
          waitCatch worker >>= \case
            Left cause -> fromException cause @?= Just AdmissionRejected
            Right _ -> assertFailure "Stale trace-delayed write was admitted"
        readIORef sent >>= (@?= False)
        atomically (getRpcPendingCount channel) >>= (@?= 0),
    testCase "owned session instrumentation reaches process and request lifecycles" $ bounded $ do
      executable <- getExecutablePath
      (logger, logs) <- recording
      metrics <- newIORef []
      let observer = defaultDroidObservability {observabilityLogger = Just logger, observabilityLogTransport = True, observabilityMetrics = Just (droidMetricSink (\value -> atomicModifyIORef' metrics (\old -> (value : old, ()))))}
          options = (Droid.defaultDroidOptions ".") {Droid.droidExecutable = executable}
      identifier <- Droid.withObservedDroidSession observer options Nothing Droid.defaultDroidHandlers $ \session -> do
        result <- sendPrompt session "hello" (const (pure ()))
        resultText result @?= "Hello سلام\n😀"
        pure (droidSessionId session)
      assertReaped identifier
      values <- logs
      assertBool "Process startup not observed" (any ((== "droid.process.started") . droidLogName) values)
      recorded <- readIORef metrics
      assertBool "No process startup metric" (any ((== "droid.process.startup") . droidMetricName) recorded),
    testCase "metric cancellation cannot replace a failed operation or bypass its cleanup" $ do
      closed <- newIORef False
      let observer = defaultDroidObservability {observabilityMetrics = Just (droidMetricSink (const (throwIO UserInterrupt)))}
      result <- try @SomeException (observeDroidOperation observer "fixture" Nothing (bracket_ (pure ()) (writeIORef closed True) (throwIO SendFailed :: IO ())))
      case result of Left cause -> fromException cause @?= Just SendFailed; Right _ -> assertFailure "Primary failure lost"
      readIORef closed >>= (@?= True),
    testCase "daemon authentication uses configured observability without logging credentials" $ bounded $ do
      incoming <- newTQueueIO
      outgoing <- newTQueueIO
      wires <- newIORef []
      (logger, logs) <- recording
      let observer = defaultDroidObservability {observabilityLogger = Just logger, observabilityLogTransport = True, observabilityTracing = Just (droidTraceContextProvider (pure (DroidTraceContext (Just "trace-only") Nothing)))}
          options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "OFFLINE_ONLY")) ".") {Daemon.daemonClientObservability = observer}
          transport = objectTransport (atomically . writeTQueue incoming) (atomically (readTQueue outgoing))
      withAsync (runDaemonPeer AfterAck False "1.201.1" wires False (atomically (readTQueue incoming)) (atomically . writeTQueue outgoing) (pure ())) $ \peer -> do
        link peer
        Daemon.withConnectionOn options transport (const (pure ()))
      frames <- readIORef wires
      assertBool "Authentication trace not injected" (any (\fields -> KeyMap.lookup "_meta" fields == Just (object ["traceparent" .= String "trace-only"])) frames)
      values <- logs
      assertBool "Authentication event absent" (any ((== "droid.transport.out") . droidLogName) values)
      assertBool "Credential leaked" (not ("OFFLINE_ONLY" `isInfixOf` show (map toJSON values)))
  ]

event :: DroidLogEvent
event = DroidLogEvent LogInfo "custom" "message" Nothing Nothing

recording :: IO (DroidLogger, IO [DroidLogEvent])
recording = do
  values <- newIORef []
  pure (droidLogger (\value -> atomicModifyIORef' values (\old -> (value : old, ()))), reverse <$> readIORef values)

data Failure = SinkFailed | ReporterFailed | SendFailed | AdmissionRejected deriving stock (Eq, Show)

instance Exception Failure

requestFixture :: Text -> IO JsonRpcBaseRequest
requestFixture identifier = decode (object ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "type" .= String "request", "id" .= identifier, "method" .= String "fixture.query", "_meta" .= object ["future" .= Bool False], "params" .= object ["token" .= String "private-token", "prompt" .= String "private-prompt"]])

replyFor :: Object -> Object
replyFor request = KeyMap.fromList [("jsonrpc", String "2.0"), ("factoryApiVersion", String "1.0.0"), ("type", String "response"), ("id", fromMaybe Null (KeyMap.lookup "id" request)), ("result", Bool True)]

decode :: (FromJSON a) => Value -> IO a
decode value = case fromJSON value of Error message -> assertFailure message; Success result -> pure result
