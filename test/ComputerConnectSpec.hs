{-# LANGUAGE OverloadedStrings #-}

module ComputerConnectSpec (computerConnectTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar, retry)
import Control.Exception (Exception, SomeException, bracket_, fromException, throw, throwIO, toException, try)
import Control.Monad (forM_, void)
import Data.Aeson (Object, Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.Types qualified as UUID
import Factory.Droid.Connection qualified as Connection
import Factory.Droid.Observability qualified as Obs
import Factory.Droid.Protocol (RpcChannelError (RpcRequestTimedOut))
import Factory.Droid.Transport.Relay qualified as Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

computerConnectTests :: TestTree
computerConnectTests =
  testGroup
    "Computer-connect SLI"
    ( [ testCase "success has one UUID context latest start type exact metric labels and two span updates" $ bounded $ do
          (options, capture) <- newCapture
          result <- runCaptured options capture id $ \attempt -> do
            Connection.setConnectStartType attempt (Just "cold")
            Connection.setConnectStartType attempt (Just "warm")
            pure True
          result @?= True
          assertReport capture "unknown" "success" Nothing (labels "success" Nothing (Just "warm")),
        testCase "predicate rejection records not fully connected without changing the result" $ bounded $ do
          (options, capture) <- newCapture
          runCaptured options capture id (const (pure False)) >>= (@?= False)
          assertReport capture "unknown" "failure" (Just "not_fully_connected") (labels "failure" (Just "not_fully_connected") Nothing),
        testCase "success is controlled by the predicate rather than a Boolean return value" $ bounded $ do
          (options, capture) <- newCapture
          runCaptured options capture (const True) (const (pure False)) >>= (@?= False)
          assertReport capture "unknown" "success" Nothing (labels "success" Nothing Nothing),
        testCase "trigger default differs from explicit empty and empty provider/start labels are omitted" $ bounded $ do
          (base, capture) <- newCapture
          let options = base {Connection.computerConnectSurface = "", Connection.computerConnectProvider = Just "", Connection.computerConnectAttemptTrigger = Just ""}
          runCaptured options capture id (\attempt -> Connection.setConnectStartType attempt (Just "") >> pure True) >>= (@?= True)
          assertReport capture "" "success" Nothing (fields (object ["surface" .= String "", "attemptTrigger" .= String "", "outcome" .= String "success"])),
        testCase "clearing the start type removes the eventual label" $ bounded $ do
          (options, capture) <- newCapture
          void $ runCaptured options capture id $ \attempt -> do
            Connection.setConnectStartType attempt (Just "cold")
            Connection.setConnectStartType attempt Nothing
            pure True
          assertReport capture "unknown" "success" Nothing (labels "success" Nothing Nothing),
        testCase "attempt context still exists without telemetry and is fresh for each invocation" $ bounded $ do
          let options = Connection.defaultComputerConnectSliOptions "offline"
              action attempt = Connection.setConnectStartType attempt (Just "warm") >> pure (Connection.connectAttemptId attempt)
          first <- Connection.recordComputerConnectSli options (const True) action
          second <- Connection.recordComputerConnectSli options (const True) action
          validAttemptId first
          validAttemptId second
          assertBool "Attempt identity reused" (first /= second)
          show options @?= "ComputerConnectSliOptions <redacted>",
        testCase "predicate errors are classified and rethrown without replaying the action" $ bounded $ do
          (options, capture) <- newCapture
          called <- newIORef (0 :: Int)
          result <- try @Marker (runCaptured options capture (const (throw (Marker 42))) (\_ -> atomicModifyIORef' called (\n -> (n + 1, True))))
          result @?= Left (Marker 42)
          readIORef called >>= (@?= 1)
          assertReport capture "unknown" "failure" (Just "unknown") (labels "failure" (Just "unknown") Nothing),
        testCase "caller cancellation retains its exception and records aborted" $ bounded $ do
          (options, capture) <- newCapture
          entered <- newEmptyTMVarIO
          withAsync (runCaptured options capture id (\_ -> atomically (putTMVar entered ()) >> atomically retry)) $ \worker -> do
            atomically (readTMVar entered)
            cancel worker
            waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation lost"
          assertReport capture "unknown" "failure" (Just "aborted") (labels "failure" (Just "aborted") Nothing),
        testCase "elapsed milliseconds include the initial span callback" $ bounded $ do
          (base, capture) <- newCapture
          let sink =
                Obs.droidSpanAttributesSink
                  ( \attributes -> do
                      append (capturedSpans capture) attributes
                      if KeyMap.member "factory.computer.connect.outcome" attributes then pure () else threadDelay 20000
                  )
          void (runCaptured (base {Connection.computerConnectSpanSink = Just sink}) capture id (const (pure True)))
          events <- readIORef (capturedMetrics capture)
          case events of
            [_, duration] -> assertBool "Initial span delivery was excluded" (Obs.droidMetricValue duration >= 10)
            _ -> assertFailure "Missing duration metric",
        testCase "span attributes use the existing scalar normalization and explicit delivery status" $ bounded $ do
          seen <- newIORef []
          let sink = Obs.droidSpanAttributesSink (append seen)
              input = fields (object ["text" .= String "", "number" .= Number 9007199254740993, "flag" .= False, "null" .= Null, "nested" .= object [], "array" .= ([] :: [Value])])
              expected = fields (object ["text" .= String "", "number" .= Number 9007199254740993, "flag" .= False, "null" .= Null])
          Obs.setDroidSpanAttributes Nothing input >>= (@?= False)
          Obs.setDroidSpanAttributes (Just sink) input >>= (@?= True)
          readIORef seen >>= (@?= [expected])
          show sink @?= "DroidSpanAttributesSink <redacted>",
        testCase "synchronous span and metric failures are reported without changing operation success" $ bounded $ do
          failures <- newIORef []
          attempts <- newIORef (0 :: Int)
          let report cause = case fromException cause of Just marker -> append failures (marker :: Marker); Nothing -> assertFailure "Failure identity lost"
              spanSink = (Obs.droidSpanAttributesSink (const (throwIO (Marker 1)))) {Obs.spanOnFailure = Just report}
              metricSink = (Obs.droidMetricSink (const (throwIO (Marker 2)))) {Obs.metricOnFailure = Just report}
              options = (Connection.defaultComputerConnectSliOptions "offline") {Connection.computerConnectSpanSink = Just spanSink, Connection.computerConnectMetricSink = Just metricSink}
          Connection.recordComputerConnectSli options id (\_ -> atomicModifyIORef' attempts (\n -> (n + 1, True))) >>= (@?= True)
          readIORef attempts >>= (@?= 1)
          readIORef failures >>= (@?= [Marker 1, Marker 1, Marker 2, Marker 2]),
        testCase "cancellation from initial span delivery precedes action admission" $ bounded $ do
          called <- newIORef False
          metrics <- newIORef []
          let options = (Connection.defaultComputerConnectSliOptions "offline") {Connection.computerConnectSpanSink = Just (Obs.droidSpanAttributesSink (const (throwIO AsyncCancelled))), Connection.computerConnectMetricSink = Just (Obs.droidMetricSink (append metrics))}
          try @AsyncCancelled (Connection.recordComputerConnectSli options id (\_ -> writeIORef called True >> pure True)) >>= (@?= Left AsyncCancelled)
          readIORef called >>= (@?= False)
          readIORef metrics >>= (@?= []),
        testCase "primary action failure survives cancellation in final span delivery" $ bounded $ do
          (base, capture) <- newCapture
          let sink = Obs.droidSpanAttributesSink (\attributes -> if KeyMap.member "factory.computer.connect.outcome" attributes then throwIO AsyncCancelled else append (capturedSpans capture) attributes)
          try @Marker (runCaptured (base {Connection.computerConnectSpanSink = Just sink}) capture id (const (throwIO (Marker 7)))) >>= (@?= Left (Marker 7))
          readIORef (capturedMetrics capture) >>= (@?= []),
        testCase "reporting cancellation follows caller-scoped resource cleanup and is not retried" $ bounded $ do
          alive <- newIORef False
          calls <- newIORef (0 :: Int)
          let sink = Obs.droidMetricSink (\_ -> atomicModifyIORef' calls (\n -> (n + 1, ())) >> throwIO AsyncCancelled)
              options = (Connection.defaultComputerConnectSliOptions "offline") {Connection.computerConnectMetricSink = Just sink}
              action _ = bracket_ (writeIORef alive True) (writeIORef alive False) (pure True)
          try @AsyncCancelled (Connection.recordComputerConnectSli options id action) >>= (@?= Left AsyncCancelled)
          readIORef alive >>= (@?= False)
          readIORef calls >>= (@?= 1),
        testCase "generic operation observation still means returned rather than remote success" $ bounded $ do
          metrics <- newIORef []
          let observability = Obs.defaultDroidObservability {Obs.observabilityMetrics = Just (Obs.droidMetricSink (append metrics))}
          Obs.observeDroidOperation observability "generic" Nothing (pure False) >>= (@?= False)
          events <- readIORef metrics
          length events @?= 2
          forM_ events $ \event -> fmap (KeyMap.lookup "outcome") (Obs.droidMetricAttributes event) @?= Just (Just (String "returned"))
      ]
        <> [ testCase name $ bounded $ do
               (base, capture) <- newCapture
               let options = base {Connection.computerConnectRequestMethod = method}
               Connection.computerConnectFailureReason method cause @?= expected
               result <- try @SomeException (runCaptured options capture id (const (throwIO cause)))
               case result of
                 Left actual -> Connection.computerConnectFailureReason method actual @?= expected
                 Right _ -> assertFailure "A failed action returned"
               assertReport capture "unknown" "failure" (Just expected) (labels "failure" (Just expected) Nothing)
           | (name, method, cause, expected) <- failureCases
           ]
    )

failureCases :: [(String, Maybe Text, SomeException, Text)]
failureCases =
  [ ("anonymous RPC timeout is daemon timeout", Nothing, timeoutFailure, "daemon_timeout"),
    ("known authenticate timeout is distinct", Just "daemon.authenticate", timeoutFailure, "daemon_auth_timeout"),
    ("another method is not classified as authentication", Just "daemon.load_session", timeoutFailure, "daemon_timeout"),
    ("method matching is case sensitive", Just "DAEMON.AUTHENTICATE", timeoutFailure, "daemon_timeout"),
    ("structured auth timeout retains its method context", Just "daemon.authenticate", connection "daemon_timeout" (Just timeoutFailure), "daemon_auth_timeout"),
    ("structured daemon timeout without a request cause is not guessed to be auth", Just "daemon.authenticate", connection "daemon_timeout" Nothing, "daemon_timeout"),
    ("another structured reason is not overridden by an auth timeout cause", Just "daemon.authenticate", connection "relay_timeout" (Just timeoutFailure), "relay_timeout"),
    ("caller-reported compute limit keeps its structured reason", Nothing, connection "compute_limit_exceeded" Nothing, "compute_limit_exceeded"),
    ("caller-reported relay rate limit keeps its structured reason", Nothing, connection "relay_rate_limited" Nothing, "relay_rate_limited"),
    ("native relay timeout reuses connection classification", Nothing, toException Relay.RelayAuthenticationTimedOut, "relay_timeout"),
    ("transport timeout is not an authenticate request timeout", Just "daemon.authenticate", toException WebSocket.WebSocketConnectTimeout, "daemon_timeout"),
    ("standard async marker is aborted", Nothing, toException AsyncCancelled, "aborted"),
    ("unknown error does not inherit a guessed auth reason", Just "daemon.authenticate", toException (Marker 99), "unknown")
  ]
  where
    timeoutFailure = toException RpcRequestTimedOut
    connection reason cause = toException (Connection.ConnectionFailure reason False False cause)

newtype Marker = Marker Int deriving stock (Eq, Show)

instance Exception Marker

data Capture = Capture
  { capturedMetrics :: IORef [Obs.DroidMetricEvent],
    capturedSpans :: IORef [Object],
    capturedAttempt :: IORef (Maybe Connection.ComputerConnectAttempt)
  }

newCapture :: IO (Connection.ComputerConnectSliOptions, Capture)
newCapture = do
  capture <- Capture <$> newIORef [] <*> newIORef [] <*> newIORef Nothing
  let options = (Connection.defaultComputerConnectSliOptions "offline") {Connection.computerConnectProvider = Just "fixture-provider", Connection.computerConnectMetricSink = Just (Obs.droidMetricSink (append (capturedMetrics capture))), Connection.computerConnectSpanSink = Just (Obs.droidSpanAttributesSink (append (capturedSpans capture)))}
  pure (options, capture)

runCaptured :: Connection.ComputerConnectSliOptions -> Capture -> (a -> Bool) -> (Connection.ComputerConnectAttempt -> IO a) -> IO a
runCaptured options capture predicate action = Connection.recordComputerConnectSli options predicate (\attempt -> writeIORef (capturedAttempt capture) (Just attempt) >> action attempt)

assertReport :: Capture -> Text -> Text -> Maybe Text -> Object -> Assertion
assertReport capture trigger outcome reason expectedLabels = do
  attempt <- readIORef (capturedAttempt capture) >>= maybe (assertFailure "Action did not receive a context") pure
  validAttemptId (Connection.connectAttemptId attempt)
  Connection.connectAttemptTrigger attempt @?= trigger
  show attempt @?= "ComputerConnectAttempt <redacted>"
  events <- readIORef (capturedMetrics capture)
  case events of
    [counter, duration] -> do
      Obs.droidMetricName counter @?= "factory_app_computer_connect_sli_attempt_count"
      Obs.droidMetricKind counter @?= Obs.MetricCounter
      Obs.droidMetricValue counter @?= 1
      Obs.droidMetricUnit counter @?= Obs.MetricCount
      Obs.droidMetricName duration @?= "factory_app_computer_connect_sli_duration_ms"
      Obs.droidMetricKind duration @?= Obs.MetricHistogram
      Obs.droidMetricUnit duration @?= Obs.MetricMilliseconds
      assertBool "Negative monotonic duration" (Obs.droidMetricValue duration >= 0)
      forM_ events $ \event -> Obs.droidMetricAttributes event @?= Just expectedLabels
    _ -> assertFailure "Expected exactly counter then duration"
  let initial = fields (object ["factory.computer.connect.attempt_id" .= Connection.connectAttemptId attempt, "factory.computer.connect.attempt_trigger" .= trigger])
      finished = KeyMap.union (fields (object (["factory.computer.connect.outcome" .= outcome] <> maybe [] (\value -> ["factory.computer.connect.failure_reason" .= value]) reason))) initial
  readIORef (capturedSpans capture) >>= (@?= [initial, finished])

labels :: Text -> Maybe Text -> Maybe Text -> Object
labels outcome reason start = fields (object (["surface" .= String "offline", "providerType" .= String "fixture-provider", "attemptTrigger" .= String "unknown", "outcome" .= outcome] <> maybe [] (\value -> ["failureReason" .= value]) reason <> maybe [] (\value -> ["startType" .= value]) start))

validAttemptId :: Text -> Assertion
validAttemptId identifier = do
  assertBool "Invalid UUID attempt" (isJust (UUID.fromText identifier))
  Text.length identifier @?= 36
  Text.index identifier 14 @?= '4'
  assertBool "Invalid UUID variant" (Text.index identifier 19 `elem` ("89ab" :: String))

fields :: Value -> Object
fields (Object value) = value
fields _ = error "Expected fixture object"

append :: IORef [a] -> a -> IO ()
append reference value = atomicModifyIORef' reference (\values -> (values <> [value], ()))

bounded :: IO a -> IO a
bounded action = timeout 10000000 action >>= maybe (assertFailure "SLI fixture timed out") pure
