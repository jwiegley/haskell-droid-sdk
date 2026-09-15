{-# LANGUAGE OverloadedStrings #-}

-- | Factory REST contracts from the baselined TypeScript SDK. Only computers
-- retain unknown response fields; the other reference records discard them.
module Factory.Droid.Schema.REST
  ( ComputerProvider (..),
    ComputerStatus (..),
    Computer (..),
    CreateComputerParams (..),
    defaultCreateComputerParams,
    UpdateComputerParams (..),
    defaultUpdateComputerParams,
    TemplateBuildState (..),
    TemplateFailureReason (..),
    TemplateBuildStatus (..),
    EnvironmentVariable (..),
    MachineTemplate (..),
    ComputerMetric (..),
    RemoteSessionStatus (..),
    RemoteSession (..),
    Pagination (..),
    Page (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, withText, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, objectWithAdditionalFields, optionalField)

data ComputerProvider = Byom | E2b deriving stock (Eq, Show)

instance FromJSON ComputerProvider where
  parseJSON = parseTag [("byom", Byom), ("e2b", E2b)]

instance ToJSON ComputerProvider where
  toJSON Byom = String "byom"
  toJSON E2b = String "e2b"

data ComputerStatus = Provisioning | Active | ComputerError deriving stock (Eq, Show)

instance FromJSON ComputerStatus where
  parseJSON = parseTag [("provisioning", Provisioning), ("active", Active), ("error", ComputerError)]

instance ToJSON ComputerStatus where
  toJSON Provisioning = String "provisioning"
  toJSON Active = String "active"
  toJSON ComputerError = String "error"

data Computer = Computer
  { computerId :: !Text,
    computerName :: !Text,
    computerHostname :: !(Maybe Text),
    computerProvider :: !ComputerProvider,
    computerStatus :: !(Maybe ComputerStatus),
    computerCreatedAt :: !Integer,
    computerRelayClientUrl :: !(Maybe Text),
    computerRemoteUser :: !(Maybe Text),
    computerAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show Computer where show _ = "Computer <redacted>"

instance FromJSON Computer where
  parseJSON = withObject "Computer" $ \v -> Computer <$> v .: "id" <*> v .: "name" <*> v .:! "hostname" <*> v .: "providerType" <*> v .:! "status" <*> v .: "createdAt" <*> v .:! "relayClientUrl" <*> v .:! "remoteUser" <*> pure (additionalFields computerFields v)

instance ToJSON Computer where
  toJSON v = objectWithAdditionalFields computerFields (computerAdditionalFields v) (["id" .= computerId v, "name" .= computerName v, "providerType" .= computerProvider v, "createdAt" .= computerCreatedAt v] <> optionalField "hostname" (computerHostname v) <> optionalField "status" (computerStatus v) <> optionalField "relayClientUrl" (computerRelayClientUrl v) <> optionalField "remoteUser" (computerRemoteUser v))

computerFields :: [Key]
computerFields = ["id", "name", "hostname", "providerType", "status", "createdAt", "relayClientUrl", "remoteUser"]

data CreateComputerParams = CreateComputerParams
  { createComputerName :: !Text,
    createComputerRemoteUser :: !Text,
    createComputerProvider :: !(Maybe ComputerProvider),
    createComputerHostId :: !(Maybe Text),
    createComputerRepos :: !(Maybe [Text]),
    createComputerAutoInstallDeps :: !(Maybe Bool),
    createComputerServiceAccountId :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show CreateComputerParams where show _ = "CreateComputerParams <redacted>"

instance ToJSON CreateComputerParams where
  toJSON v = object (["name" .= createComputerName v, "remoteUser" .= createComputerRemoteUser v] <> optionalField "provider" (createComputerProvider v) <> optionalField "hostId" (createComputerHostId v) <> optionalField "repos" (createComputerRepos v) <> optionalField "autoInstallDeps" (createComputerAutoInstallDeps v) <> optionalField "serviceAccountId" (createComputerServiceAccountId v))

defaultCreateComputerParams :: Text -> Text -> CreateComputerParams
defaultCreateComputerParams name user = CreateComputerParams name user Nothing Nothing Nothing Nothing Nothing

data UpdateComputerParams = UpdateComputerParams
  { updateComputerName :: !(Maybe Text),
    updateComputerRemoteUser :: !(Maybe Text),
    updateComputerHostId :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show UpdateComputerParams where show _ = "UpdateComputerParams <redacted>"

instance ToJSON UpdateComputerParams where
  toJSON v = object (optionalField "name" (updateComputerName v) <> optionalField "remoteUser" (updateComputerRemoteUser v) <> optionalField "hostId" (updateComputerHostId v))

defaultUpdateComputerParams :: UpdateComputerParams
defaultUpdateComputerParams = UpdateComputerParams Nothing Nothing Nothing

data TemplateBuildState = Building | BuildSuccess | BuildFailed deriving stock (Eq, Show)

instance FromJSON TemplateBuildState where
  parseJSON = parseTag [("building", Building), ("success", BuildSuccess), ("failed", BuildFailed)]

instance ToJSON TemplateBuildState where
  toJSON Building = String "building"
  toJSON BuildSuccess = String "success"
  toJSON BuildFailed = String "failed"

data TemplateFailureReason = SetupScriptError | SystemError deriving stock (Eq, Show)

instance FromJSON TemplateFailureReason where
  parseJSON = parseTag [("setup_script_error", SetupScriptError), ("system_error", SystemError)]

instance ToJSON TemplateFailureReason where
  toJSON SetupScriptError = String "setup_script_error"
  toJSON SystemError = String "system_error"

data TemplateBuildStatus = TemplateBuildStatus
  { templateBuildState :: !TemplateBuildState,
    templateFailureReason :: !(Maybe TemplateFailureReason),
    templateBuildStartedAt :: !(Maybe Integer),
    templateBuiltAt :: !(Maybe Integer),
    templateBuildLogs :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show TemplateBuildStatus where show _ = "TemplateBuildStatus <redacted>"

instance FromJSON TemplateBuildStatus where
  parseJSON = withObject "TemplateBuildStatus" $ \v -> TemplateBuildStatus <$> v .: "status" <*> v .:! "failureReason" <*> v .:! "buildStartedAt" <*> v .:! "builtAt" <*> v .:! "logs"

instance ToJSON TemplateBuildStatus where
  toJSON v = object (["status" .= templateBuildState v] <> optionalField "failureReason" (templateFailureReason v) <> optionalField "buildStartedAt" (templateBuildStartedAt v) <> optionalField "builtAt" (templateBuiltAt v) <> optionalField "logs" (templateBuildLogs v))

data EnvironmentVariable = EnvironmentVariable
  { environmentKey :: !Text,
    environmentValue :: !Text
  }
  deriving stock (Eq)

instance Show EnvironmentVariable where show _ = "EnvironmentVariable <redacted>"

instance FromJSON EnvironmentVariable where
  parseJSON = withObject "EnvironmentVariable" $ \v -> EnvironmentVariable <$> v .: "key" <*> v .: "value"

instance ToJSON EnvironmentVariable where
  toJSON v = object ["key" .= environmentKey v, "value" .= environmentValue v]

data MachineTemplate = MachineTemplate
  { machineTemplateId :: !Text,
    machineTemplateRepoUrl :: !Text,
    machineTemplateName :: !Text,
    machineTemplateDefaultBranch :: !Text,
    machineTemplateCreatedBy :: !Text,
    machineTemplateCreatedAt :: !(Maybe Integer),
    machineTemplateBuildStatus :: !(Maybe TemplateBuildStatus),
    machineTemplateLastUpdatedAt :: !(Maybe (Maybe Integer)),
    machineTemplateEnvironment :: !(Maybe [EnvironmentVariable]),
    machineTemplateUserEnvironment :: !(Maybe [EnvironmentVariable]),
    machineTemplateSetupScript :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show MachineTemplate where show _ = "MachineTemplate <redacted>"

instance FromJSON MachineTemplate where
  parseJSON = withObject "MachineTemplate" $ \v -> MachineTemplate <$> v .: "templateId" <*> v .: "repoUrl" <*> v .: "templateName" <*> v .: "defaultBranch" <*> v .: "createdBy" <*> v .:! "createdAt" <*> v .:! "buildStatus" <*> v .:! "lastUpdatedAt" <*> v .:! "environmentVariables" <*> v .:! "userEnvironmentVariablesByUser" <*> v .:! "setupScript"

instance ToJSON MachineTemplate where
  toJSON v = object (["templateId" .= machineTemplateId v, "repoUrl" .= machineTemplateRepoUrl v, "templateName" .= machineTemplateName v, "defaultBranch" .= machineTemplateDefaultBranch v, "createdBy" .= machineTemplateCreatedBy v] <> optionalField "createdAt" (machineTemplateCreatedAt v) <> optionalField "buildStatus" (machineTemplateBuildStatus v) <> optionalField "lastUpdatedAt" (machineTemplateLastUpdatedAt v) <> optionalField "environmentVariables" (machineTemplateEnvironment v) <> optionalField "userEnvironmentVariablesByUser" (machineTemplateUserEnvironment v) <> optionalField "setupScript" (machineTemplateSetupScript v))

data ComputerMetric = ComputerMetric
  { metricTimestamp :: !Text,
    metricCpuUsedPct :: !Scientific,
    metricCpuCount :: !Scientific,
    metricMemoryUsed :: !Scientific,
    metricMemoryTotal :: !Scientific,
    metricDiskUsed :: !Scientific,
    metricDiskTotal :: !Scientific
  }
  deriving stock (Eq)

instance Show ComputerMetric where show _ = "ComputerMetric <redacted>"

instance FromJSON ComputerMetric where
  parseJSON = withObject "ComputerMetric" $ \v -> ComputerMetric <$> v .: "timestamp" <*> v .: "cpuUsedPct" <*> v .: "cpuCount" <*> v .: "memUsed" <*> v .: "memTotal" <*> v .: "diskUsed" <*> v .: "diskTotal"

instance ToJSON ComputerMetric where
  toJSON v = object ["timestamp" .= metricTimestamp v, "cpuUsedPct" .= metricCpuUsedPct v, "cpuCount" .= metricCpuCount v, "memUsed" .= metricMemoryUsed v, "memTotal" .= metricMemoryTotal v, "diskUsed" .= metricDiskUsed v, "diskTotal" .= metricDiskTotal v]

data RemoteSessionStatus = RemoteIdle | RemotePending | RemoteRunning deriving stock (Eq, Show)

instance FromJSON RemoteSessionStatus where
  parseJSON = parseTag [("idle", RemoteIdle), ("pending", RemotePending), ("running", RemoteRunning)]

instance ToJSON RemoteSessionStatus where
  toJSON RemoteIdle = String "idle"
  toJSON RemotePending = String "pending"
  toJSON RemoteRunning = String "running"

data RemoteSession = RemoteSession
  { remoteSessionId :: !Text,
    remoteSessionTitle :: !(Maybe Text),
    remoteSessionStatus :: !RemoteSessionStatus,
    remoteSessionMessageCount :: !Integer,
    remoteSessionCreatedAt :: !Integer,
    remoteSessionUpdatedAt :: !Integer,
    remoteSessionCompletedAt :: !(Maybe Integer),
    remoteSessionComputerId :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show RemoteSession where show _ = "RemoteSession <redacted>"

instance FromJSON RemoteSession where
  parseJSON = withObject "RemoteSession" $ \v -> RemoteSession <$> v .: "sessionId" <*> v .:! "title" <*> v .: "status" <*> v .: "messageCount" <*> v .: "createdAt" <*> v .: "updatedAt" <*> v .:! "completedAt" <*> v .:! "computerId"

instance ToJSON RemoteSession where
  toJSON v = object (["sessionId" .= remoteSessionId v, "status" .= remoteSessionStatus v, "messageCount" .= remoteSessionMessageCount v, "createdAt" .= remoteSessionCreatedAt v, "updatedAt" .= remoteSessionUpdatedAt v] <> optionalField "title" (remoteSessionTitle v) <> optionalField "completedAt" (remoteSessionCompletedAt v) <> optionalField "computerId" (remoteSessionComputerId v))

data Pagination = Pagination
  { paginationHasMore :: !Bool,
    paginationNextCursor :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show Pagination where show _ = "Pagination <redacted>"

instance FromJSON Pagination where
  parseJSON = withObject "Pagination" $ \v -> Pagination <$> v .: "hasMore" <*> v .: "nextCursor"

instance ToJSON Pagination where
  toJSON v = object ["hasMore" .= paginationHasMore v, "nextCursor" .= paginationNextCursor v]

data Page a = Page
  { pageItems :: ![a],
    pagePagination :: !Pagination
  }
  deriving stock (Eq)

instance Show (Page a) where show _ = "Page <redacted>"

parseTag :: [(Text, a)] -> Value -> Parser a
parseTag options = withText "REST enum" $ \value -> maybe (fail "Unknown REST enum") pure (lookup value options)
