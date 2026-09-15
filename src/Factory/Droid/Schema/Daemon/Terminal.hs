{-# LANGUAGE OverloadedStrings #-}

-- | Terminal wire bodies for protocol 1.205.0, including session-scoped daemon
-- forms. No PTY, process, transport or screen restoration is operated here.
-- 'Show' instances defined here redact bodies; the shared session-ID alias
-- retains its existing instance. Explicit JSON encoding remains sensitive.
module Factory.Droid.Schema.Daemon.Terminal
  ( CreateTerminalParams (..),
    defaultCreateTerminalParams,
    WriteTerminalDataParams (..),
    ResizeTerminalParams (..),
    CloseTerminalParams (..),
    CreateTerminalResult (..),
    TerminalScreenState (..),
    TerminalInfo (..),
    ListTerminalsResult (..),
    DaemonCreateTerminalParams (..),
    DaemonWriteTerminalDataParams (..),
    DaemonResizeTerminalParams (..),
    DaemonCloseTerminalParams (..),
    DaemonListTerminalsParams,
    TerminalData (..),
    TerminalExit (..),
    TerminalNotification (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), withObject, (.:), (.:!), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, fieldsWithAdditionalFields, objectWithAdditionalFields, optionalField, requireLiteral)
import Factory.Droid.Schema.Primitives (Rfc3339Timestamp)
import Factory.Droid.Schema.Session (SessionIdParams)

-- | A terminal-creation body. Dimensions are unconstrained numbers; cwd and
-- environment are preserved without normalization, validation or execution.
data CreateTerminalParams = CreateTerminalParams
  { createdTerminalId :: !Text,
    createdTerminalCols :: !(Maybe Scientific),
    createdTerminalRows :: !(Maybe Scientific),
    createdTerminalCwd :: !(Maybe Text),
    createdTerminalEnv :: !(Maybe (KeyMap Text)),
    createTerminalAdditionalFields :: !Object
  }
  deriving stock (Eq)

-- | Only the caller-selected identifier is supplied; the daemon chooses omitted
-- dimensions, cwd and environment. This does not allocate or start a terminal.
defaultCreateTerminalParams :: Text -> CreateTerminalParams
defaultCreateTerminalParams ident = CreateTerminalParams ident Nothing Nothing Nothing Nothing mempty

instance Show CreateTerminalParams where
  show _ = "CreateTerminalParams <redacted>"

instance FromJSON CreateTerminalParams where
  parseJSON = withObject "CreateTerminalParams" $ \fields -> CreateTerminalParams <$> fields .: "terminalId" <*> fields .:! "cols" <*> fields .:! "rows" <*> fields .:! "cwd" <*> fields .:! "env" <*> pure (additionalFields createKeys fields)

instance ToJSON CreateTerminalParams where
  toJSON = Object . createObject

createObject :: CreateTerminalParams -> Object
createObject params = fieldsWithAdditionalFields createKeys (createTerminalAdditionalFields params) (["terminalId" .= createdTerminalId params] <> optionalField "cols" (createdTerminalCols params) <> optionalField "rows" (createdTerminalRows params) <> optionalField "cwd" (createdTerminalCwd params) <> optionalField "env" (createdTerminalEnv params))

-- | Exact terminal input text, including control sequences. Nothing is written.
data WriteTerminalDataParams = WriteTerminalDataParams
  { writtenTerminalId :: !Text,
    writtenTerminalData :: !Text,
    writeTerminalAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show WriteTerminalDataParams where
  show _ = "WriteTerminalDataParams <redacted>"

instance FromJSON WriteTerminalDataParams where
  parseJSON = withObject "WriteTerminalDataParams" $ \fields -> WriteTerminalDataParams <$> fields .: "terminalId" <*> fields .: "data" <*> pure (additionalFields ["terminalId", "data"] fields)

instance ToJSON WriteTerminalDataParams where
  toJSON = Object . writeObject

writeObject :: WriteTerminalDataParams -> Object
writeObject params = fieldsWithAdditionalFields ["terminalId", "data"] (writeTerminalAdditionalFields params) ["terminalId" .= writtenTerminalId params, "data" .= writtenTerminalData params]

-- | Requested resize dimensions, without rounding or applying them to a PTY.
data ResizeTerminalParams = ResizeTerminalParams
  { resizedTerminalId :: !Text,
    resizedTerminalCols :: !Scientific,
    resizedTerminalRows :: !Scientific,
    resizeTerminalAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ResizeTerminalParams where
  show _ = "ResizeTerminalParams <redacted>"

instance FromJSON ResizeTerminalParams where
  parseJSON = withObject "ResizeTerminalParams" $ \fields -> ResizeTerminalParams <$> fields .: "terminalId" <*> fields .: "cols" <*> fields .: "rows" <*> pure (additionalFields ["terminalId", "cols", "rows"] fields)

instance ToJSON ResizeTerminalParams where
  toJSON = Object . resizeObject

resizeObject :: ResizeTerminalParams -> Object
resizeObject params = fieldsWithAdditionalFields ["terminalId", "cols", "rows"] (resizeTerminalAdditionalFields params) ["terminalId" .= resizedTerminalId params, "cols" .= resizedTerminalCols params, "rows" .= resizedTerminalRows params]

-- | A close body. The identifier need not be nonempty in the wire schema.
data CloseTerminalParams = CloseTerminalParams
  { closedTerminalId :: !Text,
    closeTerminalAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CloseTerminalParams where
  show _ = "CloseTerminalParams <redacted>"

instance FromJSON CloseTerminalParams where
  parseJSON = withObject "CloseTerminalParams" $ \fields -> CloseTerminalParams <$> fields .: "terminalId" <*> pure (additionalFields ["terminalId"] fields)

instance ToJSON CloseTerminalParams where
  toJSON = Object . closeObject

closeObject :: CloseTerminalParams -> Object
closeObject params = fieldsWithAdditionalFields ["terminalId"] (closeTerminalAdditionalFields params) ["terminalId" .= closedTerminalId params]

-- | The two creation-result alternatives. Success leaves an error member
-- undeclared, so one there remains an extension rather than a failed result.
data CreateTerminalResult = TerminalCreated !Object | TerminalAlreadyExists !Object
  deriving stock (Eq)

instance Show CreateTerminalResult where
  show _ = "CreateTerminalResult <redacted>"

instance FromJSON CreateTerminalResult where
  parseJSON = withObject "CreateTerminalResult" $ \fields -> do
    success <- fields .: "success"
    if success
      then pure (TerminalCreated (additionalFields ["success"] fields))
      else do
        requireLiteral "error" "TerminalIdExists" fields
        pure (TerminalAlreadyExists (additionalFields ["success", "error"] fields))

instance ToJSON CreateTerminalResult where
  toJSON (TerminalCreated extras) = objectWithAdditionalFields ["success"] extras ["success" .= True]
  toJSON (TerminalAlreadyExists extras) = objectWithAdditionalFields ["success", "error"] extras ["success" .= False, "error" .= String "TerminalIdExists"]

-- | Reported terminal screen state. Serialized and plain text are independent
-- wire strings; the codec neither interprets escapes nor reconstructs a screen.
data TerminalScreenState = TerminalScreenState
  { screenSerialized :: !Text,
    screenPlainText :: !Text,
    screenCols :: !Scientific,
    screenRows :: !Scientific,
    screenTimestamp :: !Rfc3339Timestamp,
    screenCursorHidden :: !(Maybe Bool),
    screenAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TerminalScreenState where
  show _ = "TerminalScreenState <redacted>"

instance FromJSON TerminalScreenState where
  parseJSON = withObject "TerminalScreenState" $ \fields -> TerminalScreenState <$> fields .: "serialized" <*> fields .: "plainText" <*> fields .: "cols" <*> fields .: "rows" <*> fields .: "timestamp" <*> fields .:! "cursorHidden" <*> pure (additionalFields screenKeys fields)

instance ToJSON TerminalScreenState where
  toJSON screen = objectWithAdditionalFields screenKeys (screenAdditionalFields screen) (["serialized" .= screenSerialized screen, "plainText" .= screenPlainText screen, "cols" .= screenCols screen, "rows" .= screenRows screen, "timestamp" .= screenTimestamp screen] <> optionalField "cursorHidden" (screenCursorHidden screen))

-- | A terminal report. PID is required but nullable and retains the schema's
-- number domain. The optional screen is not inferred from terminal dimensions.
data TerminalInfo = TerminalInfo
  { terminalInfoId :: !Text,
    terminalInfoPid :: !(Maybe Scientific),
    terminalInfoCols :: !Scientific,
    terminalInfoRows :: !Scientific,
    terminalInfoCreatedAt :: !Rfc3339Timestamp,
    terminalInfoState :: !(Maybe TerminalScreenState),
    terminalInfoAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TerminalInfo where
  show _ = "TerminalInfo <redacted>"

instance FromJSON TerminalInfo where
  parseJSON = withObject "TerminalInfo" $ \fields -> TerminalInfo <$> fields .: "id" <*> fields .: "pid" <*> fields .: "cols" <*> fields .: "rows" <*> fields .: "createdAt" <*> fields .:! "state" <*> pure (additionalFields infoKeys fields)

instance ToJSON TerminalInfo where
  toJSON info = objectWithAdditionalFields infoKeys (terminalInfoAdditionalFields info) (["id" .= terminalInfoId info, "pid" .= terminalInfoPid info, "cols" .= terminalInfoCols info, "rows" .= terminalInfoRows info, "createdAt" .= terminalInfoCreatedAt info] <> optionalField "state" (terminalInfoState info))

-- | An ordered, possibly empty terminal list.
data ListTerminalsResult = ListTerminalsResult
  { listedTerminals :: ![TerminalInfo],
    listedTerminalsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListTerminalsResult where
  show _ = "ListTerminalsResult <redacted>"

instance FromJSON ListTerminalsResult where
  parseJSON = withObject "ListTerminalsResult" $ \fields -> ListTerminalsResult <$> fields .: "terminals" <*> pure (additionalFields ["terminals"] fields)

instance ToJSON ListTerminalsResult where
  toJSON result = objectWithAdditionalFields ["terminals"] (listedTerminalsAdditionalFields result) ["terminals" .= listedTerminals result]

-- | A creation body extended with required session identity. Other extensions
-- reside in the nested base record while encoding remains flat.
data DaemonCreateTerminalParams = DaemonCreateTerminalParams
  { daemonCreateSessionId :: !Text,
    daemonCreateTerminal :: !CreateTerminalParams
  }
  deriving stock (Eq)

instance Show DaemonCreateTerminalParams where
  show _ = "DaemonCreateTerminalParams <redacted>"

instance FromJSON DaemonCreateTerminalParams where
  parseJSON = parseScoped "DaemonCreateTerminalParams" DaemonCreateTerminalParams

instance ToJSON DaemonCreateTerminalParams where
  toJSON params = scopedObject (daemonCreateSessionId params) (createObject (daemonCreateTerminal params))

-- | Session-scoped terminal input, without writing any bytes.
data DaemonWriteTerminalDataParams = DaemonWriteTerminalDataParams
  { daemonWriteSessionId :: !Text,
    daemonWriteTerminal :: !WriteTerminalDataParams
  }
  deriving stock (Eq)

instance Show DaemonWriteTerminalDataParams where
  show _ = "DaemonWriteTerminalDataParams <redacted>"

instance FromJSON DaemonWriteTerminalDataParams where
  parseJSON = parseScoped "DaemonWriteTerminalDataParams" DaemonWriteTerminalDataParams

instance ToJSON DaemonWriteTerminalDataParams where
  toJSON params = scopedObject (daemonWriteSessionId params) (writeObject (daemonWriteTerminal params))

-- | Session-scoped resize body, reusing the base resize codec.
data DaemonResizeTerminalParams = DaemonResizeTerminalParams
  { daemonResizeSessionId :: !Text,
    daemonResizeTerminal :: !ResizeTerminalParams
  }
  deriving stock (Eq)

instance Show DaemonResizeTerminalParams where
  show _ = "DaemonResizeTerminalParams <redacted>"

instance FromJSON DaemonResizeTerminalParams where
  parseJSON = parseScoped "DaemonResizeTerminalParams" DaemonResizeTerminalParams

instance ToJSON DaemonResizeTerminalParams where
  toJSON params = scopedObject (daemonResizeSessionId params) (resizeObject (daemonResizeTerminal params))

-- | Session-scoped close body, without terminating a terminal.
data DaemonCloseTerminalParams = DaemonCloseTerminalParams
  { daemonCloseSessionId :: !Text,
    daemonCloseTerminal :: !CloseTerminalParams
  }
  deriving stock (Eq)

instance Show DaemonCloseTerminalParams where
  show _ = "DaemonCloseTerminalParams <redacted>"

instance FromJSON DaemonCloseTerminalParams where
  parseJSON = parseScoped "DaemonCloseTerminalParams" DaemonCloseTerminalParams

instance ToJSON DaemonCloseTerminalParams where
  toJSON params = scopedObject (daemonCloseSessionId params) (closeObject (daemonCloseTerminal params))

-- | The existing shared session-ID body, with no terminal-specific fields.
type DaemonListTerminalsParams = SessionIdParams

parseScoped :: (FromJSON a) => String -> (Text -> a -> b) -> Value -> Parser b
parseScoped name constructor = withObject name $ \fields -> constructor <$> fields .: "sessionId" <*> parseJSON (Object (KeyMap.delete "sessionId" fields))

scopedObject :: Text -> Object -> Value
scopedObject session = Object . KeyMap.insert "sessionId" (String session)

-- | Terminal output text, preserved without interpreting control sequences.
data TerminalData = TerminalData
  { terminalDataId :: !Text,
    terminalDataText :: !Text,
    terminalDataAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TerminalData where
  show _ = "TerminalData <redacted>"

instance FromJSON TerminalData where
  parseJSON = withObject "TerminalData" $ \fields -> do
    requireLiteral "type" "daemon.terminal_data" fields
    TerminalData <$> fields .: "terminalId" <*> fields .: "data" <*> pure (additionalFields ["type", "terminalId", "data"] fields)

instance ToJSON TerminalData where
  toJSON event = objectWithAdditionalFields ["type", "terminalId", "data"] (terminalDataAdditionalFields event) ["type" .= String "daemon.terminal_data", "terminalId" .= terminalDataId event, "data" .= terminalDataText event]

-- | A reported terminal exit. Signal is required text, including an empty
-- string, and the exit code is not restricted to integral or nonnegative values.
data TerminalExit = TerminalExit
  { terminalExitId :: !Text,
    terminalExitCode :: !Scientific,
    terminalExitSignal :: !Text,
    terminalExitAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show TerminalExit where
  show _ = "TerminalExit <redacted>"

instance FromJSON TerminalExit where
  parseJSON = withObject "TerminalExit" $ \fields -> do
    requireLiteral "type" "daemon.terminal_exit" fields
    TerminalExit <$> fields .: "terminalId" <*> fields .: "exitCode" <*> fields .: "signal" <*> pure (additionalFields ["type", "terminalId", "exitCode", "signal"] fields)

instance ToJSON TerminalExit where
  toJSON event = objectWithAdditionalFields ["type", "terminalId", "exitCode", "signal"] (terminalExitAdditionalFields event) ["type" .= String "daemon.terminal_exit", "terminalId" .= terminalExitId event, "exitCode" .= terminalExitCode event, "signal" .= terminalExitSignal event]

-- | The complete standalone terminal-notification union, without a dispatcher.
data TerminalNotification = TerminalDataEvent !TerminalData | TerminalExitEvent !TerminalExit
  deriving stock (Eq)

instance Show TerminalNotification where
  show _ = "TerminalNotification <redacted>"

instance FromJSON TerminalNotification where
  parseJSON = withObject "TerminalNotification" $ \fields -> do
    kind <- fields .: "type" :: Parser Text
    case kind of
      "daemon.terminal_data" -> TerminalDataEvent <$> parseJSON (Object fields)
      "daemon.terminal_exit" -> TerminalExitEvent <$> parseJSON (Object fields)
      _ -> fail "Unknown terminal notification type"

instance ToJSON TerminalNotification where
  toJSON (TerminalDataEvent event) = toJSON event
  toJSON (TerminalExitEvent event) = toJSON event

createKeys, screenKeys, infoKeys :: [Key]
createKeys = ["terminalId", "cols", "rows", "cwd", "env"]
screenKeys = ["serialized", "plainText", "cols", "rows", "timestamp", "cursorHidden"]
infoKeys = ["id", "pid", "cols", "rows", "createdAt", "state"]
