{-# LANGUAGE OverloadedStrings #-}

-- | Host and computer-registration wire records for protocol 1.205.0.
-- These codecs do not read or migrate configuration files, register computers,
-- generate identifiers or infer organization/user identity.
module Factory.Droid.Schema.Host
  ( HostId,
    ComputerRegistration (..),
    HostConfig (..),
    LegacyComputerConfig (..),
    machineConnectionType,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (Number),
    withObject,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON
  ( additionalFields,
    objectWithAdditionalFields,
    optionalField,
    requireLiteral,
  )
import Factory.Droid.Schema.Primitives (NonEmptyText, UUIDText)

-- | HostIdSchema is the UUID string format, retaining its wire spelling.
type HostId = UUIDText

-- | Descriptive source-to-connection kind mapping. Unknown names have no mapping;
-- a label establishes neither transport ownership, locality nor authentication.
machineConnectionType :: Text -> Maybe Text
machineConnectionType = \case
  "computer" -> Just "computer"
  "local" -> Just "tui"
  "ephemeral" -> Just "workspace"
  _ -> Nothing

-- | A registered computer's identities and registration timestamp. IDs other
-- than computerId require nonempty text, without an added UUID constraint.
data ComputerRegistration = ComputerRegistration
  { registrationComputerId :: !UUIDText,
    registrationFirestoreOrgId :: !NonEmptyText,
    registrationUserId :: !NonEmptyText,
    registrationTimestamp :: !Scientific,
    registrationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ComputerRegistration where
  parseJSON = withObject "ComputerRegistration" $ \fields ->
    ComputerRegistration
      <$> fields .: "computerId"
      <*> fields .: "firestoreOrgId"
      <*> fields .: "userId"
      <*> fields .: "registeredAt"
      <*> pure (additionalFields registrationKeys fields)

instance ToJSON ComputerRegistration where
  toJSON registration =
    objectWithAdditionalFields
      registrationKeys
      (registrationAdditionalFields registration)
      [ "computerId" .= registrationComputerId registration,
        "firestoreOrgId" .= registrationFirestoreOrgId registration,
        "userId" .= registrationUserId registration,
        "registeredAt" .= registrationTimestamp registration
      ]

-- | Version-one host metadata. The version literal is fixed by construction;
-- an optional registration remains distinct from a null registration.
data HostConfig = HostConfig
  { hostConfigId :: !HostId,
    hostConfigCreatedAt :: !Scientific,
    hostConfigRegistration :: !(Maybe ComputerRegistration),
    hostConfigAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON HostConfig where
  parseJSON = withObject "HostConfig" $ \fields -> do
    requireLiteral "schemaVersion" (Number 1) fields
    HostConfig
      <$> fields .: "hostId"
      <*> fields .: "createdAt"
      <*> fields .:! "computerRegistration"
      <*> pure (additionalFields hostKeys fields)

instance ToJSON HostConfig where
  toJSON config =
    objectWithAdditionalFields hostKeys (hostConfigAdditionalFields config) $
      ["schemaVersion" .= Number 1, "hostId" .= hostConfigId config, "createdAt" .= hostConfigCreatedAt config]
        <> optionalField "computerRegistration" (hostConfigRegistration config)

-- | Legacy computer metadata, represented without performing a migration.
data LegacyComputerConfig = LegacyComputerConfig
  { legacyComputerId :: !UUIDText,
    legacyRegisteredAt :: !Scientific,
    legacyComputerAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON LegacyComputerConfig where
  parseJSON = withObject "LegacyComputerConfig" $ \fields ->
    LegacyComputerConfig <$> fields .: "computerId" <*> fields .: "registeredAt" <*> pure (additionalFields legacyKeys fields)

instance ToJSON LegacyComputerConfig where
  toJSON config = objectWithAdditionalFields legacyKeys (legacyComputerAdditionalFields config) ["computerId" .= legacyComputerId config, "registeredAt" .= legacyRegisteredAt config]

registrationKeys, hostKeys, legacyKeys :: [Key]
registrationKeys = ["computerId", "firestoreOrgId", "userId", "registeredAt"]
hostKeys = ["schemaVersion", "hostId", "createdAt", "computerRegistration"]
legacyKeys = ["computerId", "registeredAt"]
