{-# LANGUAGE OverloadedStrings #-}

-- | Loop-state wire records for protocol 1.205.0. These codecs do not run
-- a scheduler or implement the runtime-refined LoopToolInput contract.
module Factory.Droid.Schema.Loop
  ( LoopInterval,
    mkLoopInterval,
    loopIntervalMilliseconds,
    LoopStatus (..),
    LoopStopReason (..),
    LoopState (..),
    LoopStateChanged (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    Options,
    ToJSON (..),
    Value (String),
    camelTo2,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    withObject,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, enumOptions, objectWithAdditionalFields, optionalField, requireLiteral)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | An integral interval from 5,000 through 86,400,000 milliseconds.
newtype LoopInterval = LoopInterval Integer
  deriving stock (Eq, Ord, Show)

-- | Validate the inclusive interval bounds without rounding or clamping.
mkLoopInterval :: Integer -> Maybe LoopInterval
mkLoopInterval value
  | value >= 5000 && value <= 86400000 = Just (LoopInterval value)
  | otherwise = Nothing

-- | Recover the interval in milliseconds.
loopIntervalMilliseconds :: LoopInterval -> Integer
loopIntervalMilliseconds (LoopInterval value) = value

instance FromJSON LoopInterval where
  parseJSON value = do
    integer <- parseJSON value
    maybe (fail "Loop interval is outside its allowed range") pure (mkLoopInterval integer)

instance ToJSON LoopInterval where
  toJSON = toJSON . loopIntervalMilliseconds

-- | The five declared loop-state labels.
data LoopStatus = LoopWaiting | LoopRunning | LoopDue | LoopStopped | LoopError
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON LoopStatus where
  parseJSON = genericParseJSON statusOptions

instance ToJSON LoopStatus where
  toJSON = genericToJSON statusOptions
  toEncoding = genericToEncoding statusOptions

-- | The reported reason a loop stopped, when provided.
data LoopStopReason = LoopStopUserStopped | LoopStopManualMessage | LoopStopInterrupted | LoopStopSessionClosed | LoopStopProcessExited | LoopStopError
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON LoopStopReason where
  parseJSON = genericParseJSON stopOptions

instance ToJSON LoopStopReason where
  toJSON = genericToJSON stopOptions
  toEncoding = genericToEncoding stopOptions

-- | A loop snapshot. Counters and timestamps are nonnegative integers.
-- nextRunAt is required but nullable; Nothing is emitted as null. No ordering
-- or consistency condition between timestamps, status and isDue is invented.
data LoopState = LoopState
  { loopStateId :: !Text,
    loopStateStatus :: !LoopStatus,
    loopStateInterval :: !LoopInterval,
    loopStateIteration :: !Natural,
    loopStateStartedAt :: !Natural,
    loopStateUpdatedAt :: !Natural,
    loopStateNextRunAt :: !(Maybe Natural),
    loopStateIsDue :: !Bool,
    loopStateLastRunStartedAt :: !(Maybe Natural),
    loopStateLastRunCompletedAt :: !(Maybe Natural),
    loopStateStopReason :: !(Maybe LoopStopReason),
    loopStateAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON LoopState where
  parseJSON = withObject "LoopState" $ \fields ->
    LoopState
      <$> fields .: "loopId"
      <*> fields .: "status"
      <*> fields .: "intervalMs"
      <*> fields .: "iteration"
      <*> fields .: "startedAt"
      <*> fields .: "updatedAt"
      <*> fields .: "nextRunAt"
      <*> fields .: "isDue"
      <*> fields .:! "lastRunStartedAt"
      <*> fields .:! "lastRunCompletedAt"
      <*> fields .:! "stopReason"
      <*> pure (additionalFields stateKeys fields)

instance ToJSON LoopState where
  toJSON state =
    objectWithAdditionalFields stateKeys (loopStateAdditionalFields state) $
      [ "loopId" .= loopStateId state,
        "status" .= loopStateStatus state,
        "intervalMs" .= loopStateInterval state,
        "iteration" .= loopStateIteration state,
        "startedAt" .= loopStateStartedAt state,
        "updatedAt" .= loopStateUpdatedAt state,
        "nextRunAt" .= loopStateNextRunAt state,
        "isDue" .= loopStateIsDue state
      ]
        <> optionalField "lastRunStartedAt" (loopStateLastRunStartedAt state)
        <> optionalField "lastRunCompletedAt" (loopStateLastRunCompletedAt state)
        <> optionalField "stopReason" (loopStateStopReason state)

-- | A reported loop-state change, without applying it to any live session.
data LoopStateChanged = LoopStateChanged
  { changedLoopState :: !LoopState,
    changedLoopAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON LoopStateChanged where
  parseJSON = withObject "LoopStateChanged" $ \fields -> do
    requireLiteral "type" "loop_state_changed" fields
    LoopStateChanged <$> fields .: "loopState" <*> pure (additionalFields ["type", "loopState"] fields)

instance ToJSON LoopStateChanged where
  toJSON event = objectWithAdditionalFields ["type", "loopState"] (changedLoopAdditionalFields event) ["type" .= String "loop_state_changed", "loopState" .= changedLoopState event]

statusOptions, stopOptions :: Options
statusOptions = enumOptions "Loop" (camelTo2 '_')
stopOptions = enumOptions "LoopStop" (camelTo2 '_')

stateKeys :: [Key]
stateKeys = ["loopId", "status", "intervalMs", "iteration", "startedAt", "updatedAt", "nextRunAt", "isDue", "lastRunStartedAt", "lastRunCompletedAt", "stopReason"]
