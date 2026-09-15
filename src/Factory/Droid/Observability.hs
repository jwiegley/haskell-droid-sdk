{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Explicit observability callbacks. No global sink, environment switch,
-- background worker or mandatory telemetry backend is installed.
module Factory.Droid.Observability
  ( DroidObservability (..),
    defaultDroidObservability,
    observeDroidOperation,
    DroidMetricKind (..),
    DroidMetricUnit (..),
    DroidMetricEvent (..),
    DroidMetricSink (..),
    droidMetricSink,
    recordDroidMetric,
    DroidTraceContext (..),
    DroidTraceContextProvider (..),
    droidTraceContextProvider,
    getDroidTraceContext,
    injectDroidTraceContext,
    DroidLogLevel (..),
    DroidSerializedError (..),
    DroidLogEvent (..),
    DroidLogger (..),
    droidLogger,
    emitDroidLog,
    sdkLogEvent,
    sanitizeDroidAttributes,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, evaluate, mask, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Object, ToJSON (..), Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Factory.Droid.Internal.Exception (finallyPreserving, trySync)
import Factory.Droid.Internal.JSON (optionalField)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)

data DroidLogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

instance ToJSON DroidLogLevel where
  toJSON = String . logLevelText

logLevelText :: DroidLogLevel -> Text
logLevelText = \case
  LogDebug -> "debug"
  LogInfo -> "info"
  LogWarn -> "warn"
  LogError -> "error"

-- | Explicit error presentation, not automatic rendering of arbitrary exceptions.
-- Messages/codes can be sensitive; the caller controls what is supplied.
data DroidSerializedError = DroidSerializedError
  { serializedErrorName :: !(Maybe Text),
    serializedErrorMessage :: !Text,
    serializedErrorCode :: !(Maybe Text)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DroidSerializedError where show _ = "DroidSerializedError <redacted>"

instance ToJSON DroidSerializedError where
  toJSON value = object (["message" .= serializedErrorMessage value] <> optionalField "name" (serializedErrorName value) <> optionalField "code" (serializedErrorCode value))

data DroidLogEvent = DroidLogEvent
  { droidLogLevel :: !DroidLogLevel,
    droidLogName :: !Text,
    droidLogMessage :: !Text,
    droidLogAttributes :: !(Maybe Object),
    droidLogError :: !(Maybe DroidSerializedError)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DroidLogEvent where show _ = "DroidLogEvent <redacted>"

instance ToJSON DroidLogEvent where
  toJSON event = object (["level" .= droidLogLevel event, "name" .= droidLogName event, "message" .= droidLogMessage event] <> optionalField "attributes" (sanitizeDroidAttributes (droidLogAttributes event)) <> optionalField "error" (droidLogError event))

-- | Sinks run on the emitting thread and must be cooperative and thread-safe.
-- The optional failure observer receives the original, potentially sensitive
-- synchronous exception. It must not recursively invoke its own failing sink.
data DroidLogger = DroidLogger
  { loggerWrite :: !(DroidLogEvent -> IO ()),
    loggerOnFailure :: !(Maybe (SomeException -> IO ()))
  }

instance Show DroidLogger where show _ = "DroidLogger <redacted>"

droidLogger :: (DroidLogEvent -> IO ()) -> DroidLogger
droidLogger sink = DroidLogger sink Nothing

-- | True means the configured sink returned normally, not that an external
-- backend persisted the event. Synchronous sink/observer failures are isolated;
-- the standard asynchronous-exception family propagates unchanged.
emitDroidLog :: Maybe DroidLogger -> DroidLogEvent -> IO Bool
emitDroidLog Nothing _ = pure False
emitDroidLog (Just logger) event =
  isJust
    <$> deliver
      (loggerOnFailure logger)
      ( do
          prepared <- evaluate (force (event {droidLogAttributes = sanitizeDroidAttributes (droidLogAttributes event)}))
          loggerWrite logger prepared
      )

deliver :: Maybe (SomeException -> IO ()) -> IO a -> IO (Maybe a)
deliver onFailure action = do
  result <- trySync action
  case result of
    Right value -> pure (Just value)
    Left failure -> do
      mapM_ (\report -> void (trySync (report failure))) onFailure
      pure Nothing

data DroidMetricKind = MetricCounter | MetricHistogram
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data DroidMetricUnit = MetricCount | MetricMilliseconds
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON DroidMetricKind where
  toJSON MetricCounter = String "counter"
  toJSON MetricHistogram = String "histogram"

instance ToJSON DroidMetricUnit where
  toJSON MetricCount = String "1"
  toJSON MetricMilliseconds = String "ms"

data DroidMetricEvent = DroidMetricEvent
  { droidMetricName :: !Text,
    droidMetricKind :: !DroidMetricKind,
    droidMetricValue :: !Scientific,
    droidMetricUnit :: !DroidMetricUnit,
    droidMetricAttributes :: !(Maybe Object)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DroidMetricEvent where show _ = "DroidMetricEvent <redacted>"

instance ToJSON DroidMetricEvent where
  toJSON event = object (["name" .= droidMetricName event, "kind" .= droidMetricKind event, "value" .= droidMetricValue event, "unit" .= droidMetricUnit event] <> optionalField "attributes" (sanitizeDroidAttributes (droidMetricAttributes event)))

data DroidMetricSink = DroidMetricSink
  { metricRecord :: !(DroidMetricEvent -> IO ()),
    metricOnFailure :: !(Maybe (SomeException -> IO ()))
  }

instance Show DroidMetricSink where show _ = "DroidMetricSink <redacted>"

droidMetricSink :: (DroidMetricEvent -> IO ()) -> DroidMetricSink
droidMetricSink sink = DroidMetricSink sink Nothing

recordDroidMetric :: Maybe DroidMetricSink -> DroidMetricEvent -> IO Bool
recordDroidMetric Nothing _ = pure False
recordDroidMetric (Just sink) event =
  isJust
    <$> deliver
      (metricOnFailure sink)
      ( do
          prepared <- evaluate (force (event {droidMetricAttributes = sanitizeDroidAttributes (droidMetricAttributes event)}))
          metricRecord sink prepared
      )

data DroidTraceContext = DroidTraceContext
  { traceContextParent :: !(Maybe Text),
    traceContextState :: !(Maybe Text)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DroidTraceContext where show _ = "DroidTraceContext <redacted>"

instance ToJSON DroidTraceContext where
  toJSON context = object (optionalField "traceparent" (traceContextParent context) <> optionalField "tracestate" (traceContextState context))

data DroidTraceContextProvider = DroidTraceContextProvider
  { traceProvide :: !(IO DroidTraceContext),
    traceOnFailure :: !(Maybe (SomeException -> IO ()))
  }

instance Show DroidTraceContextProvider where show _ = "DroidTraceContextProvider <redacted>"

droidTraceContextProvider :: IO DroidTraceContext -> DroidTraceContextProvider
droidTraceContextProvider provider = DroidTraceContextProvider provider Nothing

getDroidTraceContext :: Maybe DroidTraceContextProvider -> IO (Maybe DroidTraceContext)
getDroidTraceContext Nothing = pure Nothing
getDroidTraceContext (Just provider) = deliver (traceOnFailure provider) (traceProvide provider >>= evaluate . force)

-- | Publish only a successfully obtained immutable context. Missing fields do
-- not erase the carrier; explicit empty strings remain meaningful.
injectDroidTraceContext :: Maybe DroidTraceContextProvider -> Object -> IO (Object, Bool)
injectDroidTraceContext provider carrier = do
  context <- getDroidTraceContext provider
  pure $ case context of
    Nothing -> (carrier, False)
    Just value ->
      let parent = maybe carrier (\text -> KeyMap.insert "traceparent" (String text) carrier) (traceContextParent value)
          combined = maybe parent (\text -> KeyMap.insert "tracestate" (String text) parent) (traceContextState value)
       in (combined, True)

data DroidObservability = DroidObservability
  { observabilityLogger :: !(Maybe DroidLogger),
    observabilityMetrics :: !(Maybe DroidMetricSink),
    observabilityTracing :: !(Maybe DroidTraceContextProvider),
    observabilityLogTransport :: !Bool
  }

instance Show DroidObservability where show _ = "DroidObservability <redacted>"

defaultDroidObservability :: DroidObservability
defaultDroidObservability = DroidObservability Nothing Nothing Nothing False

-- | Observe an operation through existing ownership. Bracket resources inside
-- the action; a telemetry callback can cancel even after a returned operation.
-- Duration excludes this observer's own start/finish delivery, not callbacks
-- inside the action. Returned is not a claim of remote success.
observeDroidOperation :: DroidObservability -> Text -> Maybe Object -> IO a -> IO a
observeDroidOperation observability name attributes action
  | Nothing <- observabilityLogger observability, Nothing <- observabilityMetrics observability = action
  | otherwise = mask $ \restore -> do
      restore (void (emitDroidLog (observabilityLogger observability) (DroidLogEvent LogDebug (name <> ".start") "Operation started" attributes Nothing)))
      started <- getMonotonicTimeNSec
      result <- try @SomeException (restore action)
      finished <- getMonotonicTimeNSec
      let elapsed = fromInteger (toInteger finished - toInteger started) / 1000000
          report failed = do
            let outcome = if failed then "failed" else "returned"
                attrs = Just (KeyMap.insert "outcome" (String outcome) (fromMaybe mempty attributes))
            void (recordDroidMetric (observabilityMetrics observability) (DroidMetricEvent (name <> ".duration") MetricHistogram elapsed MetricMilliseconds attrs))
            void (recordDroidMetric (observabilityMetrics observability) (DroidMetricEvent (name <> ".count") MetricCounter 1 MetricCount attrs))
            void (emitDroidLog (observabilityLogger observability) (DroidLogEvent (if failed then LogWarn else LogDebug) (name <> "." <> outcome) "Operation finished" attrs Nothing))
      case result of
        Left failure -> finallyPreserving (throwIO failure) (restore (report True))
        Right value -> restore (report False) >> pure value

-- | Only scalar JSON values are attributes; empty attributes become absent.
-- This is shape normalization, not a general secret scrubber.
sanitizeDroidAttributes :: Maybe Object -> Maybe Object
sanitizeDroidAttributes attributes = do
  values <- KeyMap.filter scalar <$> attributes
  if KeyMap.null values then Nothing else Just values
  where
    scalar (Object _) = False
    scalar (Array _) = False
    scalar _ = True

-- | Standard SDK event names and content-key reduction. Explicit messages,
-- other scalar attributes and supplied serialized errors are not redacted.
sdkLogEvent :: DroidLogLevel -> Text -> Maybe Object -> Maybe DroidSerializedError -> DroidLogEvent
sdkLogEvent level message attributes = DroidLogEvent level ("droid.sdk." <> logLevelText level) message (redact <$> attributes)
  where
    redact fields = foldl' redactKey fields ["output", "preview", "stderrTail"]
    redactKey fields key = case KeyMap.lookup key fields of
      Nothing -> fields
      Just value ->
        let without = KeyMap.delete key fields
         in case value of
              String content -> KeyMap.insert (Key.fromText (Key.toText key <> "ByteLength")) (Number (fromIntegral (BS.length (encodeUtf8 content)))) without
              _ -> without
