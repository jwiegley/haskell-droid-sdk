{-# LANGUAGE OverloadedStrings #-}

-- | Daemon management wire data, not a local updater, SSH installer or relay.
-- Explicit fields/JSON remain sensitive even where record displays are redacted.
module Factory.Droid.Schema.Daemon.Management
  ( TriggerUpdateResult (..),
    InstallSshKeyParams (..),
    InstallSshKeyResult (..),
    ProxyTokenResult (..),
    RelayStartResult (..),
    RelayStopResult (..),
    RelayStatus (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), object, withObject, (.:), (.:!), (.=))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField, rejectUnknownFields)
import Factory.Droid.Schema.Primitives (NonEmptyText)

-- | Triggered is not proof that an update finished or a new version is running.
data TriggerUpdateResult = TriggerUpdateResult
  { updateTriggered :: !Bool,
    updateMessage :: !(Maybe Text),
    updateResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TriggerUpdateResult where
  show _ = "TriggerUpdateResult <redacted>"

instance FromJSON TriggerUpdateResult where
  parseJSON = withObject "TriggerUpdateResult" $ \fields -> TriggerUpdateResult <$> fields .: "triggered" <*> fields .:! "message" <*> pure (additionalFields ["triggered", "message"] fields)

instance ToJSON TriggerUpdateResult where
  toJSON value = objectWithAdditionalFields ["triggered", "message"] (updateResultAdditionalFields value) (["triggered" .= updateTriggered value] <> optionalField "message" (updateMessage value))

-- | The SDK requires nonempty input; SSH key-format validation belongs to the daemon.
data InstallSshKeyParams = InstallSshKeyParams
  { installedSshPublicKey :: !NonEmptyText,
    sshKeyAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show InstallSshKeyParams where
  show _ = "InstallSshKeyParams <redacted>"

instance FromJSON InstallSshKeyParams where
  parseJSON = withObject "InstallSshKeyParams" $ \fields -> InstallSshKeyParams <$> fields .: "publicKey" <*> pure (additionalFields ["publicKey"] fields)

instance ToJSON InstallSshKeyParams where
  toJSON value = objectWithAdditionalFields ["publicKey"] (sshKeyAdditionalFields value) ["publicKey" .= installedSshPublicKey value]

data InstallSshKeyResult = InstallSshKeyResult
  { sshKeyInstalled :: !Bool,
    sshInstallAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show InstallSshKeyResult where
  show _ = "InstallSshKeyResult <redacted>"

instance FromJSON InstallSshKeyResult where
  parseJSON = withObject "InstallSshKeyResult" $ \fields -> InstallSshKeyResult <$> fields .: "installed" <*> pure (additionalFields ["installed"] fields)

instance ToJSON InstallSshKeyResult where
  toJSON value = objectWithAdditionalFields ["installed"] (sshInstallAdditionalFields value) ["installed" .= sshKeyInstalled value]

-- | The token is opaque. No expiry, renewal or validation is inferred.
data ProxyTokenResult = ProxyTokenResult
  { proxyToken :: !Text,
    proxyTokenAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ProxyTokenResult where
  show _ = "ProxyTokenResult <redacted>"

instance FromJSON ProxyTokenResult where
  parseJSON = withObject "ProxyTokenResult" $ \fields -> ProxyTokenResult <$> fields .: "token" <*> pure (additionalFields ["token"] fields)

instance ToJSON ProxyTokenResult where
  toJSON value = objectWithAdditionalFields ["token"] (proxyTokenAdditionalFields value) ["token" .= proxyToken value]

data RelayStartResult = RelayStartResult
  { startedRelayUrl :: !Text,
    startedRelayComputerId :: !Text,
    relayStartAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show RelayStartResult where
  show _ = "RelayStartResult <redacted>"

instance FromJSON RelayStartResult where
  parseJSON = withObject "RelayStartResult" $ \fields -> RelayStartResult <$> fields .: "relayUrl" <*> fields .: "computerId" <*> pure (additionalFields ["relayUrl", "computerId"] fields)

instance ToJSON RelayStartResult where
  toJSON value = objectWithAdditionalFields ["relayUrl", "computerId"] (relayStartAdditionalFields value) ["relayUrl" .= startedRelayUrl value, "computerId" .= startedRelayComputerId value]

-- | Unlike the other management reports, relay stop has a closed empty result.
data RelayStopResult = RelayStopResult deriving stock (Eq, Show)

instance FromJSON RelayStopResult where
  parseJSON = withObject "RelayStopResult" $ \fields -> rejectUnknownFields [] fields >> pure RelayStopResult

instance ToJSON RelayStopResult where
  toJSON RelayStopResult = object []

-- | Shared by the relay status query and status_changed notification. Missing
-- optional fields are not filled from an SDK cache or inferred from connected.
data RelayStatus = RelayStatus
  { relayConnected :: !Bool,
    relayUrl :: !(Maybe Text),
    relayClientCount :: !(Maybe Scientific),
    relayComputerId :: !(Maybe Text),
    relayStatusAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show RelayStatus where
  show _ = "RelayStatus <redacted>"

instance FromJSON RelayStatus where
  parseJSON = withObject "RelayStatus" $ \fields -> RelayStatus <$> fields .: "connected" <*> fields .:! "url" <*> fields .:! "clientCount" <*> fields .:! "computerId" <*> pure (additionalFields ["connected", "url", "clientCount", "computerId"] fields)

instance ToJSON RelayStatus where
  toJSON value = objectWithAdditionalFields ["connected", "url", "clientCount", "computerId"] (relayStatusAdditionalFields value) (["connected" .= relayConnected value] <> optionalField "url" (relayUrl value) <> optionalField "clientCount" (relayClientCount value) <> optionalField "computerId" (relayComputerId value))
