{-# LANGUAGE OverloadedStrings #-}

-- | Session catalogs, stored history, search and archive state. Numeric cursor
-- units are explicit; paths, snippets and archive timestamps are opaque data.
module Factory.Droid.Schema.Daemon.Session
  ( SessionPageLimit,
    mkSessionPageLimit,
    sessionPageLimitValue,
    SessionListFilter (..),
    defaultSessionListFilter,
    AvailableSessionFilter (..),
    ListOpenedSessionsParams (..),
    defaultListOpenedSessionsParams,
    ListAvailableSessionsParams (..),
    defaultListAvailableSessionsParams,
    SessionHistoryRole (..),
    GetSessionMessagesParams (..),
    defaultGetSessionMessagesParams,
    OpenedSessionInfo (..),
    AvailableSessionInfo (..),
    CatalogMission (..),
    ListOpenedSessionsResult (..),
    ListAvailableSessionsResult (..),
    GetSessionMessagesResult (..),
    SessionSearchKind (..),
    SessionSearchScope (..),
    SearchMessageRole (..),
    SearchSessionsParams (..),
    defaultSearchSessionsParams,
    SessionSearchHit (..),
    SessionSearchTotals (..),
    SessionSearchResult (..),
    SearchSessionsResult (..),
    ArchiveSessionParams (..),
    defaultArchiveSessionParams,
    ArchiveSessionResult (..),
    SessionArchiveStateChanged (..),
    LoadedSessionState (..),
    DaemonCloseSessionParams (..),
    defaultDaemonCloseSessionParams,
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), withObject, withScientific, withText, (.!=), (.:), (.:!), (.:?), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Internal.JSON (additionalFields, fieldsWithAdditionalFields, objectWithAdditionalFields, optionalField)
import Factory.Droid.Schema.Control (QueuedUserMessage)
import Factory.Droid.Schema.Host (HostId)
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.Schema.Mission (MissionPhase, MissionSnapshot, SubagentInvocationSummary)
import Factory.Droid.Schema.Notifications (DroidWorkingState)
import Factory.Droid.Schema.Session (SessionSnapshot, SessionTag, SessionWorktreeMetadata)
import Factory.Droid.Schema.Settings (SessionSettings)

-- | Explicit remote close. The optional empty-draft flag comes from the
-- supplied schema; omission retains the older SDK request shape.
data DaemonCloseSessionParams = DaemonCloseSessionParams
  { daemonCloseSessionId :: !Text,
    daemonClosePreserveEmptyDraft :: !(Maybe Bool),
    daemonCloseAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show DaemonCloseSessionParams where
  show _ = "DaemonCloseSessionParams <redacted>"

defaultDaemonCloseSessionParams :: Text -> DaemonCloseSessionParams
defaultDaemonCloseSessionParams identifier = DaemonCloseSessionParams identifier Nothing mempty

instance FromJSON DaemonCloseSessionParams where
  parseJSON = withObject "DaemonCloseSessionParams" $ \fields ->
    DaemonCloseSessionParams <$> fields .: "sessionId" <*> fields .:! "preserveEmptyDraft" <*> pure (additionalFields ["sessionId", "preserveEmptyDraft"] fields)

instance ToJSON DaemonCloseSessionParams where
  toJSON params = objectWithAdditionalFields ["sessionId", "preserveEmptyDraft"] (daemonCloseAdditionalFields params) (["sessionId" .= daemonCloseSessionId params] <> optionalField "preserveEmptyDraft" (daemonClosePreserveEmptyDraft params))

-- | Immutable snapshot/queue projection of a load reply, not a live store.
-- Other report fields remain extensions; this is not the entire load schema.
data LoadedSessionState = LoadedSessionState
  { loadedSessionSnapshot :: !SessionSnapshot,
    loadedSessionSettings :: !SessionSettings,
    loadedHasOlderMessages :: !(Maybe Bool),
    loadedAgentLoopInProgress :: !(Maybe Bool),
    loadedWorkingState :: !(Maybe DroidWorkingState),
    loadedQueuedMessages :: !(Maybe [QueuedUserMessage]),
    loadedMissionSnapshot :: !(Maybe MissionSnapshot),
    loadedCallingSessionId :: !(Maybe Text),
    loadedCallingToolUseId :: !(Maybe Text),
    loadedSubagentInvocations :: !(Maybe [SubagentInvocationSummary]),
    loadedSessionAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show LoadedSessionState where
  show _ = "LoadedSessionState <redacted>"

instance FromJSON LoadedSessionState where
  parseJSON = withObject "LoadedSessionState" $ \fields ->
    LoadedSessionState <$> fields .: "session" <*> fields .: "settings" <*> fields .:! "hasOlderMessages" <*> fields .:! "isAgentLoopInProgress" <*> fields .:! "workingState" <*> fields .:! "queuedMessages" <*> fields .:! "mission" <*> fields .:! "callingSessionId" <*> fields .:! "callingToolUseId" <*> fields .:! "subagentInvocations" <*> pure (additionalFields loadedStateKeys fields)

instance ToJSON LoadedSessionState where
  toJSON value = objectWithAdditionalFields loadedStateKeys (loadedSessionAdditionalFields value) (["session" .= loadedSessionSnapshot value, "settings" .= loadedSessionSettings value] <> optionalField "hasOlderMessages" (loadedHasOlderMessages value) <> optionalField "isAgentLoopInProgress" (loadedAgentLoopInProgress value) <> optionalField "workingState" (loadedWorkingState value) <> optionalField "queuedMessages" (loadedQueuedMessages value) <> optionalField "mission" (loadedMissionSnapshot value) <> optionalField "callingSessionId" (loadedCallingSessionId value) <> optionalField "callingToolUseId" (loadedCallingToolUseId value) <> optionalField "subagentInvocations" (loadedSubagentInvocations value))

loadedStateKeys :: [Key]
loadedStateKeys = ["session", "settings", "hasOlderMessages", "isAgentLoopInProgress", "workingState", "queuedMessages", "mission", "callingSessionId", "callingToolUseId", "subagentInvocations"]

-- | The wire limit permits fractional numbers in the inclusive range 1–100.
newtype SessionPageLimit = SessionPageLimit Scientific deriving stock (Eq, Ord, Show)

mkSessionPageLimit :: Scientific -> Maybe SessionPageLimit
mkSessionPageLimit value = if value >= 1 && value <= 100 then Just (SessionPageLimit value) else Nothing

sessionPageLimitValue :: SessionPageLimit -> Scientific
sessionPageLimitValue (SessionPageLimit value) = value

instance FromJSON SessionPageLimit where
  parseJSON = withScientific "SessionPageLimit" $ maybe (fail "Session limit must be between 1 and 100") pure . mkSessionPageLimit

instance ToJSON SessionPageLimit where toJSON = toJSON . sessionPageLimitValue

data SessionListFilter = SessionListFilter
  { filterMissionSessions :: !(Maybe Bool),
    filterIncludeBtwForks :: !(Maybe Bool),
    sessionFilterAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionListFilter where show _ = "SessionListFilter <redacted>"

instance FromJSON SessionListFilter where
  parseJSON = withObject "SessionListFilter" $ \v -> SessionListFilter <$> v .:! "missionSessions" <*> v .:! "includeBtwForks" <*> pure (additionalFields ["missionSessions", "includeBtwForks"] v)

instance ToJSON SessionListFilter where
  toJSON = Object . sessionFilterObject

sessionFilterObject :: SessionListFilter -> Object
sessionFilterObject v = fieldsWithAdditionalFields ["missionSessions", "includeBtwForks"] (sessionFilterAdditionalFields v) (optionalField "missionSessions" (filterMissionSessions v) <> optionalField "includeBtwForks" (filterIncludeBtwForks v))

defaultSessionListFilter :: SessionListFilter
defaultSessionListFilter = SessionListFilter Nothing Nothing mempty

data AvailableSessionFilter = AvailableSessionFilter
  { availableSessionFilter :: !SessionListFilter,
    availableMissionStates :: !(Maybe [MissionPhase])
  }
  deriving stock (Eq)

instance Show AvailableSessionFilter where show _ = "AvailableSessionFilter <redacted>"

instance FromJSON AvailableSessionFilter where
  parseJSON = withObject "AvailableSessionFilter" $ \v -> AvailableSessionFilter <$> parseJSON (Object (KeyMap.delete "missionStates" v)) <*> v .:! "missionStates"

instance ToJSON AvailableSessionFilter where
  toJSON v = objectWithAdditionalFields ["missionStates"] (sessionFilterObject (availableSessionFilter v)) (optionalField "missionStates" (availableMissionStates v))

data ListOpenedSessionsParams = ListOpenedSessionsParams
  { openedSessionsFilter :: !(Maybe SessionListFilter),
    openedSessionsParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListOpenedSessionsParams where show _ = "ListOpenedSessionsParams <redacted>"

instance FromJSON ListOpenedSessionsParams where
  parseJSON = withObject "ListOpenedSessionsParams" $ \v -> ListOpenedSessionsParams <$> v .:! "filter" <*> pure (additionalFields ["filter"] v)

instance ToJSON ListOpenedSessionsParams where
  toJSON v = objectWithAdditionalFields ["filter"] (openedSessionsParamsAdditionalFields v) (optionalField "filter" (openedSessionsFilter v))

defaultListOpenedSessionsParams :: ListOpenedSessionsParams
defaultListOpenedSessionsParams = ListOpenedSessionsParams Nothing mempty

data ListAvailableSessionsParams = ListAvailableSessionsParams
  { availableSessionsLimit :: !SessionPageLimit,
    availableEndBeforeSeconds :: !(Maybe Scientific),
    availableIncludeArchived :: !(Maybe Bool),
    availableIncludeMissionMetadata :: !(Maybe Bool),
    availableSessionsFilter :: !(Maybe AvailableSessionFilter),
    availableSessionsParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListAvailableSessionsParams where show _ = "ListAvailableSessionsParams <redacted>"

instance FromJSON ListAvailableSessionsParams where
  parseJSON = withObject "ListAvailableSessionsParams" $ \v -> ListAvailableSessionsParams <$> (v .:! "limit" .!= SessionPageLimit 50) <*> v .:! "endBefore" <*> v .:! "includeArchived" <*> v .:! "includeMissionMetadata" <*> v .:! "filter" <*> pure (additionalFields availableParamsKeys v)

instance ToJSON ListAvailableSessionsParams where
  toJSON v = objectWithAdditionalFields availableParamsKeys (availableSessionsParamsAdditionalFields v) (["limit" .= availableSessionsLimit v] <> optionalField "endBefore" (availableEndBeforeSeconds v) <> optionalField "includeArchived" (availableIncludeArchived v) <> optionalField "includeMissionMetadata" (availableIncludeMissionMetadata v) <> optionalField "filter" (availableSessionsFilter v))

defaultListAvailableSessionsParams :: ListAvailableSessionsParams
defaultListAvailableSessionsParams = ListAvailableSessionsParams (SessionPageLimit 50) Nothing Nothing Nothing Nothing mempty

availableParamsKeys :: [Key]
availableParamsKeys = ["limit", "endBefore", "includeArchived", "includeMissionMetadata", "filter"]

data SessionHistoryRole = HistoryUser | HistoryAssistant | HistoryTool deriving stock (Eq, Show)

instance FromJSON SessionHistoryRole where parseJSON = parseTag [("user", HistoryUser), ("assistant", HistoryAssistant), ("tool", HistoryTool)]

instance ToJSON SessionHistoryRole where
  toJSON HistoryUser = String "user"
  toJSON HistoryAssistant = String "assistant"
  toJSON HistoryTool = String "tool"

data GetSessionMessagesParams = GetSessionMessagesParams
  { messagesSessionId :: !Text,
    messagesLimit :: !SessionPageLimit,
    messagesCursor :: !(Maybe Text),
    messagesRole :: !(Maybe SessionHistoryRole),
    messagesParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GetSessionMessagesParams where show _ = "GetSessionMessagesParams <redacted>"

instance FromJSON GetSessionMessagesParams where
  parseJSON = withObject "GetSessionMessagesParams" $ \v -> GetSessionMessagesParams <$> v .: "sessionId" <*> (v .:! "limit" .!= SessionPageLimit 20) <*> v .:! "cursor" <*> v .:! "role" <*> pure (additionalFields messagesParamsKeys v)

instance ToJSON GetSessionMessagesParams where
  toJSON v = objectWithAdditionalFields messagesParamsKeys (messagesParamsAdditionalFields v) (["sessionId" .= messagesSessionId v, "limit" .= messagesLimit v] <> optionalField "cursor" (messagesCursor v) <> optionalField "role" (messagesRole v))

defaultGetSessionMessagesParams :: Text -> GetSessionMessagesParams
defaultGetSessionMessagesParams ident = GetSessionMessagesParams ident (SessionPageLimit 20) Nothing Nothing mempty

messagesParamsKeys :: [Key]
messagesParamsKeys = ["sessionId", "limit", "cursor", "role"]

data OpenedSessionInfo = OpenedSessionInfo
  { openedSessionId :: !Text,
    openedHostId :: !(Maybe HostId),
    openedUpdatedAtSeconds :: !Scientific,
    openedWorkingState :: !DroidWorkingState,
    openedCwd :: !(Maybe Text),
    openedRepoRoot :: !(Maybe Text),
    openedMessagesCount :: !(Maybe Scientific),
    openedCallingSessionId :: !(Maybe Text),
    openedCallingToolUseId :: !(Maybe Text),
    openedTags :: !(Maybe [SessionTag]),
    openedWorktree :: !(Maybe SessionWorktreeMetadata),
    openedSessionAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show OpenedSessionInfo where show _ = "OpenedSessionInfo <redacted>"

instance FromJSON OpenedSessionInfo where
  parseJSON = withObject "OpenedSessionInfo" $ \v -> OpenedSessionInfo <$> v .: "sessionId" <*> v .:! "hostId" <*> v .: "updatedAt" <*> v .: "workingState" <*> v .:! "cwd" <*> v .:! "repoRoot" <*> v .:! "messagesCount" <*> v .:! "callingSessionId" <*> v .:! "callingToolUseId" <*> v .:! "tags" <*> pure (optionalWorktree v) <*> pure (additionalFields openedKeys v)

instance ToJSON OpenedSessionInfo where
  toJSON v = objectWithAdditionalFields openedKeys (openedSessionAdditionalFields v) (["sessionId" .= openedSessionId v, "updatedAt" .= openedUpdatedAtSeconds v, "workingState" .= openedWorkingState v] <> optionalField "hostId" (openedHostId v) <> optionalField "cwd" (openedCwd v) <> optionalField "repoRoot" (openedRepoRoot v) <> optionalField "messagesCount" (openedMessagesCount v) <> optionalField "callingSessionId" (openedCallingSessionId v) <> optionalField "callingToolUseId" (openedCallingToolUseId v) <> optionalField "tags" (openedTags v) <> optionalField "worktree" (openedWorktree v))

openedKeys :: [Key]
openedKeys = ["sessionId", "hostId", "updatedAt", "workingState", "cwd", "repoRoot", "messagesCount", "callingSessionId", "callingToolUseId", "tags", "worktree"]

-- The selected CLI applies worktree.optional().catch(undefined), not a fallback
-- for the row or for other fields. Mission nullable optionals normalize separately.
optionalWorktree :: Object -> Maybe SessionWorktreeMetadata
optionalWorktree fields = KeyMap.lookup "worktree" fields >>= parseMaybe parseJSON

data CatalogMission = CatalogMission
  { catalogMissionState :: !MissionPhase,
    catalogMissionTitle :: !(Maybe Text),
    catalogMissionWorkingDirectory :: !(Maybe Text),
    catalogMissionCreatedAt :: !(Maybe Text),
    catalogMissionUpdatedAt :: !(Maybe Text),
    catalogMissionElapsedMs :: !(Maybe Scientific),
    catalogMissionCompletedFeatures :: !(Maybe Scientific),
    catalogMissionTotalFeatures :: !(Maybe Scientific),
    catalogMissionAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show CatalogMission where show _ = "CatalogMission <redacted>"

instance FromJSON CatalogMission where
  parseJSON = withObject "CatalogMission" $ \v -> CatalogMission <$> v .: "state" <*> v .:? "title" <*> v .:? "workingDirectory" <*> v .:? "createdAt" <*> v .:? "updatedAt" <*> v .:? "elapsedMs" <*> v .:? "completedFeatures" <*> v .:? "totalFeatures" <*> pure (additionalFields missionKeys v)

instance ToJSON CatalogMission where
  toJSON v = objectWithAdditionalFields missionKeys (catalogMissionAdditionalFields v) (["state" .= catalogMissionState v] <> optionalField "title" (catalogMissionTitle v) <> optionalField "workingDirectory" (catalogMissionWorkingDirectory v) <> optionalField "createdAt" (catalogMissionCreatedAt v) <> optionalField "updatedAt" (catalogMissionUpdatedAt v) <> optionalField "elapsedMs" (catalogMissionElapsedMs v) <> optionalField "completedFeatures" (catalogMissionCompletedFeatures v) <> optionalField "totalFeatures" (catalogMissionTotalFeatures v))

missionKeys :: [Key]
missionKeys = ["state", "title", "workingDirectory", "createdAt", "updatedAt", "elapsedMs", "completedFeatures", "totalFeatures"]

data AvailableSessionInfo = AvailableSessionInfo
  { availableSessionId :: !Text,
    availableHostId :: !(Maybe HostId),
    availableUpdatedAtSeconds :: !Scientific,
    availableTitle :: !(Maybe Text),
    availableCwd :: !(Maybe Text),
    availableRepoRoot :: !(Maybe Text),
    availableMessagesCount :: !(Maybe Scientific),
    availableArchivedAt :: !(Maybe Text),
    availableCallingSessionId :: !(Maybe Text),
    availableCallingToolUseId :: !(Maybe Text),
    availableOrganizationId :: !(Maybe Text),
    availableTags :: !(Maybe [SessionTag]),
    availableWorktree :: !(Maybe SessionWorktreeMetadata),
    availableMission :: !(Maybe CatalogMission),
    availableSessionAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show AvailableSessionInfo where show _ = "AvailableSessionInfo <redacted>"

instance FromJSON AvailableSessionInfo where
  parseJSON = withObject "AvailableSessionInfo" $ \v -> AvailableSessionInfo <$> v .: "sessionId" <*> v .:! "hostId" <*> v .: "updatedAt" <*> v .:! "title" <*> v .:! "cwd" <*> v .:! "repoRoot" <*> v .:! "messagesCount" <*> v .:! "archivedAt" <*> v .:! "callingSessionId" <*> v .:! "callingToolUseId" <*> v .:! "organizationId" <*> v .:! "tags" <*> pure (optionalWorktree v) <*> v .:! "mission" <*> pure (additionalFields availableKeys v)

instance ToJSON AvailableSessionInfo where
  toJSON v = objectWithAdditionalFields availableKeys (availableSessionAdditionalFields v) (["sessionId" .= availableSessionId v, "updatedAt" .= availableUpdatedAtSeconds v] <> optionalField "hostId" (availableHostId v) <> optionalField "title" (availableTitle v) <> optionalField "cwd" (availableCwd v) <> optionalField "repoRoot" (availableRepoRoot v) <> optionalField "messagesCount" (availableMessagesCount v) <> optionalField "archivedAt" (availableArchivedAt v) <> optionalField "callingSessionId" (availableCallingSessionId v) <> optionalField "callingToolUseId" (availableCallingToolUseId v) <> optionalField "organizationId" (availableOrganizationId v) <> optionalField "tags" (availableTags v) <> optionalField "worktree" (availableWorktree v) <> optionalField "mission" (availableMission v))

availableKeys :: [Key]
availableKeys = ["sessionId", "hostId", "updatedAt", "title", "cwd", "repoRoot", "messagesCount", "archivedAt", "callingSessionId", "callingToolUseId", "organizationId", "tags", "worktree", "mission"]

data ListOpenedSessionsResult = ListOpenedSessionsResult
  { openedSessions :: ![OpenedSessionInfo],
    openedSessionsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListOpenedSessionsResult where show _ = "ListOpenedSessionsResult <redacted>"

instance FromJSON ListOpenedSessionsResult where
  parseJSON = withObject "ListOpenedSessionsResult" $ \v -> ListOpenedSessionsResult <$> v .: "sessions" <*> pure (additionalFields ["sessions"] v)

instance ToJSON ListOpenedSessionsResult where
  toJSON v = objectWithAdditionalFields ["sessions"] (openedSessionsAdditionalFields v) ["sessions" .= openedSessions v]

data ListAvailableSessionsResult = ListAvailableSessionsResult
  { availableSessions :: ![AvailableSessionInfo],
    availableSessionsHasMore :: !Bool,
    availableSessionsNextCursorSeconds :: !(Maybe Scientific),
    availableSessionsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ListAvailableSessionsResult where show _ = "ListAvailableSessionsResult <redacted>"

instance FromJSON ListAvailableSessionsResult where
  parseJSON = withObject "ListAvailableSessionsResult" $ \v -> ListAvailableSessionsResult <$> v .: "sessions" <*> v .: "hasMore" <*> v .:! "nextCursor" <*> pure (additionalFields ["sessions", "hasMore", "nextCursor"] v)

instance ToJSON ListAvailableSessionsResult where
  toJSON v = objectWithAdditionalFields ["sessions", "hasMore", "nextCursor"] (availableSessionsAdditionalFields v) (["sessions" .= availableSessions v, "hasMore" .= availableSessionsHasMore v] <> optionalField "nextCursor" (availableSessionsNextCursorSeconds v))

data GetSessionMessagesResult = GetSessionMessagesResult
  { catalogMessages :: ![FactoryDroidMessage],
    catalogMessagesHasMore :: !Bool,
    catalogMessagesNextCursor :: !(Maybe Text),
    catalogMessagesAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show GetSessionMessagesResult where show _ = "GetSessionMessagesResult <redacted>"

instance FromJSON GetSessionMessagesResult where
  parseJSON = withObject "GetSessionMessagesResult" $ \v -> GetSessionMessagesResult <$> v .: "messages" <*> v .: "hasMore" <*> v .:! "nextCursor" <*> pure (additionalFields ["messages", "hasMore", "nextCursor"] v)

instance ToJSON GetSessionMessagesResult where
  toJSON v = objectWithAdditionalFields ["messages", "hasMore", "nextCursor"] (catalogMessagesAdditionalFields v) (["messages" .= catalogMessages v, "hasMore" .= catalogMessagesHasMore v] <> optionalField "nextCursor" (catalogMessagesNextCursor v))

data SessionSearchKind = SearchMessageText | SearchDocument | SearchToolUse | SearchToolResult deriving stock (Eq, Show)

instance FromJSON SessionSearchKind where parseJSON = parseTag [("message_text", SearchMessageText), ("document", SearchDocument), ("tool_use", SearchToolUse), ("tool_result", SearchToolResult)]

instance ToJSON SessionSearchKind where
  toJSON SearchMessageText = String "message_text"
  toJSON SearchDocument = String "document"
  toJSON SearchToolUse = String "tool_use"
  toJSON SearchToolResult = String "tool_result"

data SessionSearchScope = SearchAll | SearchOnly !SessionSearchKind deriving stock (Eq, Show)

instance FromJSON SessionSearchScope where
  parseJSON (String "all") = pure SearchAll
  parseJSON value = SearchOnly <$> parseJSON value

instance ToJSON SessionSearchScope where
  toJSON SearchAll = String "all"
  toJSON (SearchOnly kind) = toJSON kind

data SearchMessageRole = SearchUser | SearchAssistant deriving stock (Eq, Show)

instance FromJSON SearchMessageRole where parseJSON = parseTag [("user", SearchUser), ("assistant", SearchAssistant)]

instance ToJSON SearchMessageRole where
  toJSON SearchUser = String "user"
  toJSON SearchAssistant = String "assistant"

data SearchSessionsParams = SearchSessionsParams
  { searchSessionsQuery :: !Text,
    searchSessionsKind :: !(Maybe SessionSearchScope),
    searchLimitSessions :: !(Maybe Scientific),
    searchLimitHitsPerSession :: !(Maybe Scientific),
    searchContextChars :: !(Maybe Scientific),
    searchUpdatedAfterMs :: !(Maybe Scientific),
    searchUpdatedBeforeMs :: !(Maybe Scientific),
    searchSessionsParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SearchSessionsParams where show _ = "SearchSessionsParams <redacted>"

instance FromJSON SearchSessionsParams where
  parseJSON = withObject "SearchSessionsParams" $ \v -> SearchSessionsParams <$> v .: "query" <*> v .:! "kind" <*> v .:! "limitSessions" <*> v .:! "limitHitsPerSession" <*> v .:! "contextChars" <*> v .:! "updatedAfterMs" <*> v .:! "updatedBeforeMs" <*> pure (additionalFields searchParamsKeys v)

instance ToJSON SearchSessionsParams where
  toJSON v = objectWithAdditionalFields searchParamsKeys (searchSessionsParamsAdditionalFields v) (["query" .= searchSessionsQuery v] <> optionalField "kind" (searchSessionsKind v) <> optionalField "limitSessions" (searchLimitSessions v) <> optionalField "limitHitsPerSession" (searchLimitHitsPerSession v) <> optionalField "contextChars" (searchContextChars v) <> optionalField "updatedAfterMs" (searchUpdatedAfterMs v) <> optionalField "updatedBeforeMs" (searchUpdatedBeforeMs v))

defaultSearchSessionsParams :: Text -> SearchSessionsParams
defaultSearchSessionsParams query = SearchSessionsParams query Nothing Nothing Nothing Nothing Nothing Nothing mempty

searchParamsKeys :: [Key]
searchParamsKeys = ["query", "kind", "limitSessions", "limitHitsPerSession", "contextChars", "updatedAfterMs", "updatedBeforeMs"]

data SessionSearchHit = SessionSearchHit
  { searchHitDocId :: !Text,
    searchHitKind :: !SessionSearchKind,
    searchHitScore :: !(Maybe Scientific),
    searchHitToolName :: !(Maybe Text),
    searchHitMessageRole :: !(Maybe SearchMessageRole),
    searchHitSnippets :: ![Text],
    searchHitAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionSearchHit where show _ = "SessionSearchHit <redacted>"

instance FromJSON SessionSearchHit where
  parseJSON = withObject "SessionSearchHit" $ \v -> SessionSearchHit <$> v .: "docId" <*> v .: "kind" <*> v .:! "score" <*> v .:! "toolName" <*> v .:! "messageRole" <*> v .: "snippets" <*> pure (additionalFields ["docId", "kind", "score", "toolName", "messageRole", "snippets"] v)

instance ToJSON SessionSearchHit where
  toJSON v = objectWithAdditionalFields ["docId", "kind", "score", "toolName", "messageRole", "snippets"] (searchHitAdditionalFields v) (["docId" .= searchHitDocId v, "kind" .= searchHitKind v, "snippets" .= searchHitSnippets v] <> optionalField "score" (searchHitScore v) <> optionalField "toolName" (searchHitToolName v) <> optionalField "messageRole" (searchHitMessageRole v))

data SessionSearchTotals = SessionSearchTotals
  { searchTotalsByKind :: !(Maybe (KeyMap Scientific)),
    searchTotalsToolUse :: !(Maybe (KeyMap Scientific)),
    searchTotalsToolResult :: !(Maybe (KeyMap Scientific)),
    searchTotalsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionSearchTotals where show _ = "SessionSearchTotals <redacted>"

instance FromJSON SessionSearchTotals where
  parseJSON = withObject "SessionSearchTotals" $ \v -> SessionSearchTotals <$> v .:! "byKind" <*> v .:! "toolUse" <*> v .:! "toolResult" <*> pure (additionalFields ["byKind", "toolUse", "toolResult"] v)

instance ToJSON SessionSearchTotals where
  toJSON v = objectWithAdditionalFields ["byKind", "toolUse", "toolResult"] (searchTotalsAdditionalFields v) (optionalField "byKind" (searchTotalsByKind v) <> optionalField "toolUse" (searchTotalsToolUse v) <> optionalField "toolResult" (searchTotalsToolResult v))

data SessionSearchResult = SessionSearchResult
  { searchResultSessionId :: !Text,
    searchResultTitle :: !(Maybe Text),
    searchResultUpdatedAtMs :: !(Maybe Scientific),
    searchResultHits :: ![SessionSearchHit],
    searchResultTotals :: !(Maybe SessionSearchTotals),
    searchResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionSearchResult where show _ = "SessionSearchResult <redacted>"

instance FromJSON SessionSearchResult where
  parseJSON = withObject "SessionSearchResult" $ \v -> SessionSearchResult <$> v .: "sessionId" <*> v .:! "title" <*> v .:! "updatedAt" <*> v .: "hits" <*> v .:! "totals" <*> pure (additionalFields ["sessionId", "title", "updatedAt", "hits", "totals"] v)

instance ToJSON SessionSearchResult where
  toJSON v = objectWithAdditionalFields ["sessionId", "title", "updatedAt", "hits", "totals"] (searchResultAdditionalFields v) (["sessionId" .= searchResultSessionId v, "hits" .= searchResultHits v] <> optionalField "title" (searchResultTitle v) <> optionalField "updatedAt" (searchResultUpdatedAtMs v) <> optionalField "totals" (searchResultTotals v))

data SearchSessionsResult = SearchSessionsResult
  { searchedQuery :: !Text,
    searchedSessions :: ![SessionSearchResult],
    searchedAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SearchSessionsResult where show _ = "SearchSessionsResult <redacted>"

instance FromJSON SearchSessionsResult where
  parseJSON = withObject "SearchSessionsResult" $ \v -> SearchSessionsResult <$> v .: "query" <*> v .: "sessions" <*> pure (additionalFields ["query", "sessions"] v)

instance ToJSON SearchSessionsResult where
  toJSON v = objectWithAdditionalFields ["query", "sessions"] (searchedAdditionalFields v) ["query" .= searchedQuery v, "sessions" .= searchedSessions v]

data ArchiveSessionParams = ArchiveSessionParams
  { archiveSessionId :: !Text,
    archiveForce :: !(Maybe Bool),
    archiveParamsAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ArchiveSessionParams where show _ = "ArchiveSessionParams <redacted>"

instance FromJSON ArchiveSessionParams where
  parseJSON = withObject "ArchiveSessionParams" $ \v -> ArchiveSessionParams <$> v .: "sessionId" <*> v .:! "force" <*> pure (additionalFields ["sessionId", "force"] v)

instance ToJSON ArchiveSessionParams where
  toJSON v = objectWithAdditionalFields ["sessionId", "force"] (archiveParamsAdditionalFields v) (["sessionId" .= archiveSessionId v] <> optionalField "force" (archiveForce v))

defaultArchiveSessionParams :: Text -> ArchiveSessionParams
defaultArchiveSessionParams ident = ArchiveSessionParams ident Nothing mempty

data ArchiveSessionResult = ArchiveSessionResult
  { archiveSuccess :: !Bool,
    archiveTimestamp :: !Text,
    archiveResultAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show ArchiveSessionResult where show _ = "ArchiveSessionResult <redacted>"

instance FromJSON ArchiveSessionResult where
  parseJSON = withObject "ArchiveSessionResult" $ \v -> ArchiveSessionResult <$> v .: "success" <*> v .: "archivedAt" <*> pure (additionalFields ["success", "archivedAt"] v)

instance ToJSON ArchiveSessionResult where
  toJSON v = objectWithAdditionalFields ["success", "archivedAt"] (archiveResultAdditionalFields v) ["success" .= archiveSuccess v, "archivedAt" .= archiveTimestamp v]

data SessionArchiveStateChanged = SessionArchiveStateChanged
  { archiveChangedSessionId :: !Text,
    archiveChangedTimestamp :: !(Maybe Text),
    archiveChangedCwd :: !(Maybe Text),
    archiveChangedRepoRoot :: !(Maybe Text),
    archiveChangedTitle :: !(Maybe Text),
    archiveChangedWorktreeRemoved :: !(Maybe Bool),
    archiveChangedAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show SessionArchiveStateChanged where show _ = "SessionArchiveStateChanged <redacted>"

instance FromJSON SessionArchiveStateChanged where
  parseJSON = withObject "SessionArchiveStateChanged" $ \v -> SessionArchiveStateChanged <$> v .: "sessionId" <*> v .:! "archivedAt" <*> v .:! "cwd" <*> v .:! "repoRoot" <*> v .:! "title" <*> v .:! "worktreeRemoved" <*> pure (additionalFields archiveEventKeys v)

instance ToJSON SessionArchiveStateChanged where
  toJSON v = objectWithAdditionalFields archiveEventKeys (archiveChangedAdditionalFields v) (["sessionId" .= archiveChangedSessionId v] <> optionalField "archivedAt" (archiveChangedTimestamp v) <> optionalField "cwd" (archiveChangedCwd v) <> optionalField "repoRoot" (archiveChangedRepoRoot v) <> optionalField "title" (archiveChangedTitle v) <> optionalField "worktreeRemoved" (archiveChangedWorktreeRemoved v))

archiveEventKeys :: [Key]
archiveEventKeys = ["sessionId", "archivedAt", "cwd", "repoRoot", "title", "worktreeRemoved"]

parseTag :: [(Text, a)] -> Value -> Parser a
parseTag options = withText "Session catalog enum" $ \value -> maybe (fail "Unknown session catalog enum") pure (lookup value options)
