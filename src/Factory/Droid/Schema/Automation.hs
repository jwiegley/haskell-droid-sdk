{-# LANGUAGE OverloadedStrings #-}

-- | Unrefined automation list/read/delete wire inputs for protocol 1.205.0.
-- These values do not schedule jobs, access files or mutate remote resources.
module Factory.Droid.Schema.Automation
  ( AutomationExecutionLocation (..),
    AutomationListToolInput (..),
    AutomationTarget (..),
    AutomationReadToolInput,
    AutomationDeleteToolInput,
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String), withObject, withText, (.:), (.=))
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields)
import Factory.Droid.Schema.Primitives (NonEmptyText)

-- | The requested execution location, without selecting or contacting a host.
data AutomationExecutionLocation = AutomationLocal | AutomationRemote
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON AutomationExecutionLocation where
  parseJSON = withText "AutomationExecutionLocation" $ \case
    "local" -> pure AutomationLocal
    "remote" -> pure AutomationRemote
    _ -> fail "Unknown automation execution location"

instance ToJSON AutomationExecutionLocation where
  toJSON AutomationLocal = String "local"
  toJSON AutomationRemote = String "remote"

-- | List-input body. Location is required rather than defaulted.
data AutomationListToolInput = AutomationListToolInput
  { automationListLocation :: !AutomationExecutionLocation,
    automationListAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AutomationListToolInput where
  parseJSON = withObject "AutomationListToolInput" $ \fields ->
    AutomationListToolInput <$> fields .: "executionLocation" <*> pure (additionalFields ["executionLocation"] fields)

instance ToJSON AutomationListToolInput where
  toJSON input = objectWithAdditionalFields ["executionLocation"] (automationListAdditionalFields input) ["executionLocation" .= automationListLocation input]

-- | The identical read/delete input shape. The ID is nonempty but untrimmed,
-- following the supplied wire schema rather than older SDK input coercion.
data AutomationTarget = AutomationTarget
  { automationTargetLocation :: !AutomationExecutionLocation,
    automationTargetId :: !NonEmptyText,
    automationTargetAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON AutomationTarget where
  parseJSON = withObject "AutomationTarget" $ \fields ->
    AutomationTarget <$> fields .: "executionLocation" <*> fields .: "automationId" <*> pure (additionalFields ["executionLocation", "automationId"] fields)

instance ToJSON AutomationTarget where
  toJSON target = objectWithAdditionalFields ["executionLocation", "automationId"] (automationTargetAdditionalFields target) ["executionLocation" .= automationTargetLocation target, "automationId" .= automationTargetId target]

-- | The body accepted by an automation-read operation.
type AutomationReadToolInput = AutomationTarget

-- | The body accepted by an automation-delete operation; no deletion occurs here.
type AutomationDeleteToolInput = AutomationTarget
