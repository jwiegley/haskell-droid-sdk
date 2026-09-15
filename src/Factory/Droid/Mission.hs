{-# LANGUAGE OverloadedStrings #-}

-- | Immutable mission-state transformations. Callers own storage and feed
-- selected events from the existing session observers. No clock, callback
-- registry or worker process is created. Association registries are immutable
-- values; the connection runtime owns any automatic observation of them.
module Factory.Droid.Mission
  ( MissionStore,
    emptyMissionStore,
    missionSnapshot,
    setState,
    setTitle,
    setFeatures,
    setProgressLog,
    setTokenUsageBySessionId,
    setSessionTokenUsage,
    addWorkerAt,
    addWorkerWithState,
    completeWorkerAt,
    hasWorkerSession,
    mergeFrom,
    restoreSnapshotAt,
    applyEventAt,
    MissionRegistry,
    MissionRegistryError (..),
    emptyMissionRegistry,
    resolveMissionId,
    lookupMissionStore,
    associateSessionWithMission,
    associateWorkerWithParentMission,
    modifyMissionStore,
    invalidateMissionStore,
    restoreRegistrySnapshotAt,
    applyRegistryEventAt,
    sharesMission,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (Scientific)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.Stream (DroidEvent (..))
import Factory.Droid.Schema.Mission
import Factory.Droid.Schema.Notifications (SessionTokenUsageChanged (..))
import Factory.Droid.Schema.Usage (TokenUsage, sumTokenUsage)
import Text.Read (readMaybe)

-- Presence, including an explicitly cleared title or empty list, is retained
-- independently of the defaults rendered by missionSnapshot.
data MissionStore = MissionStore
  { observedState :: !(Maybe MissionPhase),
    observedTitle :: !(Maybe (Maybe Text)),
    observedFeatures :: !(Maybe [MissionFeature]),
    observedProgress :: !(Maybe [ProgressLogEntry]),
    workerOrder :: !(Seq Text),
    workerIds :: !(Set Text),
    workerStates :: !(Map Text WorkerStateInfo),
    sessionUsage :: !(Map Text TokenUsage)
  }
  deriving stock (Eq)

instance Show MissionStore where
  show _ = "MissionStore <redacted>"

emptyMissionStore :: MissionStore
emptyMissionStore = MissionStore Nothing Nothing Nothing Nothing mempty mempty mempty mempty

missionSnapshot :: MissionStore -> MissionSnapshot
missionSnapshot store =
  MissionSnapshot
    (fromMaybe MissionAwaitingInput (observedState store))
    (fromMaybe [] (observedFeatures store))
    (fromMaybe [] (observedProgress store))
    (toList (workerOrder store))
    (fromMaybe Nothing (observedTitle store))
    Nothing
    Nothing
    (nonemptyMap (workerStates store))
    (sumUsage (Map.elems (sessionUsage store)))
    (nonemptyMap (sessionUsage store))
    mempty

setState :: MissionPhase -> MissionStore -> MissionStore
setState state store = store {observedState = Just state}

setTitle :: Maybe Text -> MissionStore -> MissionStore
setTitle title store = store {observedTitle = Just title}

setFeatures :: [MissionFeature] -> MissionStore -> MissionStore
setFeatures features store =
  foldl' (flip rememberWorker) (store {observedFeatures = Just features}) (concatMap (fromMaybe [] . missionFeatureWorkerSessionIds) features)

-- | Replace the log, retaining historical workers and previously observed
-- start times. Missing failure exit codes retain a prior reported exit code.
setProgressLog :: [ProgressLogEntry] -> MissionStore -> MissionStore
setProgressLog entries store =
  let derived = foldl' deriveWorker (store {observedProgress = Just entries}) entries
      previous = workerOrder store
      added = toList (Seq.drop (Seq.length previous) (workerOrder derived))
   in derived {workerOrder = previous <> Seq.fromList (sortOn propertyOrder added)}
  where
    -- Object.keys enumerates canonical array-index keys first, but existing
    -- Set members keep their positions. 2^32-1 itself is not an array index.
    propertyOrder :: Text -> Integer
    propertyOrder key
      | Text.length key > 10 = 4294967295
      | otherwise = case readMaybe (Text.unpack key) of
          Just index | index >= 0 && index < 4294967295 && Text.pack (show index) == key -> index
          _ -> 4294967295

setTokenUsageBySessionId :: Map Text TokenUsage -> MissionStore -> MissionStore
setTokenUsageBySessionId usage store = store {sessionUsage = usage}

setSessionTokenUsage :: Text -> TokenUsage -> MissionStore -> MissionStore
setSessionTokenUsage identifier usage store = store {sessionUsage = Map.insert identifier usage (sessionUsage store)}

-- | The observation timestamp is explicit, unlike the reference's local clock.
-- An existing worker state is retained, including an empty reported start time.
addWorkerAt :: Text -> Text -> MissionStore -> MissionStore
addWorkerAt observedAt identifier store = addWorkerWithState identifier (Map.findWithDefault (WorkerStateInfo observedAt Nothing Nothing mempty) identifier (workerStates store)) store

addWorkerWithState :: Text -> WorkerStateInfo -> MissionStore -> MissionStore
addWorkerWithState identifier state store = (rememberWorker identifier store) {workerStates = Map.insert identifier state (workerStates store)}

-- | Explicit completion replaces the worker report; progress-log completion
-- instead patches it, matching the distinct reference operations.
completeWorkerAt :: Text -> Text -> Scientific -> MissionStore -> MissionStore
completeWorkerAt observedAt identifier exitCode store =
  let started = maybe observedAt workerStartedAt (Map.lookup identifier (workerStates store))
   in addWorkerWithState identifier (WorkerStateInfo started (Just observedAt) (Just exitCode) mempty) store

hasWorkerSession :: Text -> MissionStore -> Bool
hasWorkerSession identifier = Set.member identifier . workerIds

-- | Source first, destination last. Only observed source fields replace the
-- destination; source worker/usage entries win collisions without clearing it.
mergeFrom :: MissionStore -> MissionStore -> MissionStore
mergeFrom source destination =
  (foldl' (flip rememberWorker) destination (workerOrder source))
    { observedState = observedState source <|> observedState destination,
      observedTitle = observedTitle source <|> observedTitle destination,
      observedFeatures = observedFeatures source <|> observedFeatures destination,
      observedProgress = observedProgress source <|> observedProgress destination,
      workerStates = Map.union (workerStates source) (workerStates destination),
      sessionUsage = Map.union (sessionUsage source) (sessionUsage destination)
    }

-- | Apply the reference load sequence. Missing title clears; missing per-session
-- usage replaces the map with empty. Aggregate usage is recomputed from that map,
-- not copied from the report. Other snapshot metadata remains in the receipt.
restoreSnapshotAt :: Text -> MissionSnapshot -> MissionStore -> MissionStore
restoreSnapshotAt observedAt snapshot store =
  let titled = setTitle (missionSnapshotTitle snapshot) store
      phased = setState (missionSnapshotState snapshot) titled
      featured = setFeatures (missionSnapshotFeatures snapshot) phased
      progressed = setProgressLog (missionSnapshotProgress snapshot) featured
      accounted = setTokenUsageBySessionId (fromMaybe mempty (missionSnapshotSessionUsage snapshot)) progressed
      restore current identifier = case missionSnapshotWorkerStates snapshot >>= Map.lookup identifier of
        Just state -> addWorkerWithState identifier state current
        Nothing -> addWorkerAt observedAt identifier current
   in foldl' restore accounted (missionSnapshotWorkers snapshot)

-- | Feed an already scoped event into a caller-owned store. The first accepted
-- entry supplies the title; per-session usage prefers the inclusive report
-- when supplied, matching the reference mission controller.
-- Heartbeats and non-mission/non-usage events leave the value unchanged.
applyEventAt :: Text -> DroidEvent -> MissionStore -> MissionStore
applyEventAt observedAt event store = case event of
  MissionStateEvent changed -> setState (changedMissionPhase changed) store
  MissionFeaturesEvent changed -> setFeatures (changedMissionFeatures changed) store
  MissionProgressEvent changed ->
    let entries = missionProgressLog changed
        next = setProgressLog entries store
     in maybe next (\title -> setTitle (Just title) next) (listToMaybe [title | ProgressLogEntry {progressEntryDetails = MissionAcceptedProgress title} <- entries])
  MissionWorkerStartedEvent started -> addWorkerAt observedAt (startedMissionWorkerId started) store
  MissionWorkerCompletedEvent completed -> completeWorkerAt observedAt (completedMissionWorkerId completed) (completedMissionWorkerExitCode completed) store
  UsageEvent changed -> setSessionTokenUsage (sessionUsageId changed) (fromMaybe (sessionUsageTokens changed) (sessionUsageInclusive changed)) store
  _ -> store

rememberWorker :: Text -> MissionStore -> MissionStore
rememberWorker identifier store
  | hasWorkerSession identifier store = store
  | otherwise = store {workerOrder = workerOrder store |> identifier, workerIds = Set.insert identifier (workerIds store)}

deriveWorker :: MissionStore -> ProgressLogEntry -> MissionStore
deriveWorker store entry = case progressWorker (progressEntryDetails entry) of
  Just identifier
    | not (Text.null identifier) ->
        let previous = Map.findWithDefault (WorkerStateInfo (progressEntryTimestamp entry) Nothing Nothing mempty) identifier (workerStates store)
            next = case progressEntryDetails entry of
              WorkerCompletedProgress completed -> previous {workerCompletedAt = Just (progressEntryTimestamp entry), workerExitCode = Just (progressCompletedExitCode completed)}
              WorkerFailedProgress failed -> previous {workerCompletedAt = Just (progressEntryTimestamp entry), workerExitCode = progressFailedExitCode failed <|> workerExitCode previous}
              _ -> previous
         in addWorkerWithState identifier next store
  _ -> store

progressWorker :: ProgressLogDetails -> Maybe Text
progressWorker = \case
  WorkerStartedProgress started -> Just (progressStartedWorkerId started)
  WorkerSelectedFeatureProgress identifier _ -> Just identifier
  WorkerCompletedProgress completed -> Just (progressCompletedWorkerId completed)
  WorkerFailedProgress failed -> progressFailedWorkerId failed
  WorkerPausedProgress identifier _ -> Just identifier
  _ -> Nothing

nonemptyMap :: Map k v -> Maybe (Map k v)
nonemptyMap values = if Map.null values then Nothing else Just values

sumUsage :: [TokenUsage] -> Maybe TokenUsage
sumUsage [] = Nothing
sumUsage values = Just (sumTokenUsage values)

-- A present Nothing is an invalid observation, not an absent/pristine store.
data MissionRegistry = MissionRegistry
  { registryStores :: !(Map Text (Maybe MissionStore)),
    registrySessions :: !(Map Text Text)
  }
  deriving stock (Eq)

instance Show MissionRegistry where
  show _ = "MissionRegistry <redacted>"

data MissionRegistryError = MissionStateInvalid
  deriving stock (Eq, Show)

emptyMissionRegistry :: MissionRegistry
emptyMissionRegistry = MissionRegistry mempty mempty

resolveMissionId :: Text -> MissionRegistry -> Maybe Text
resolveMissionId identifier = Map.lookup identifier . registrySessions

lookupMissionStore :: Text -> MissionRegistry -> Either MissionRegistryError (Maybe MissionStore)
lookupMissionStore identifier registry = case resolveMissionId identifier registry >>= (`Map.lookup` registryStores registry) of
  Nothing -> Right Nothing
  Just Nothing -> Left MissionStateInvalid
  Just (Just store) -> Right (Just store)

-- | Move a session association. Provisional source fields win, while a pristine
-- source cannot erase destination data. Other aliases keep their original store.
associateSessionWithMission :: Text -> Text -> MissionRegistry -> MissionRegistry
associateSessionWithMission identifier mission registry =
  let target = Map.findWithDefault (Just emptyMissionStore) mission (registryStores registry)
      previous = resolveMissionId identifier registry
      combined = case previous of
        Just old | old /= mission -> case Map.lookup old (registryStores registry) of
          Just source -> mergeFrom <$> source <*> target
          Nothing -> target
        _ -> target
      stores = Map.insert mission combined (registryStores registry)
      associations = Map.insert identifier mission (registrySessions registry)
      retained = case previous of
        Just old | old /= mission && old `notElem` Map.elems associations -> Map.delete old stores
        _ -> stores
   in MissionRegistry retained associations

associateWorkerWithParentMission :: Text -> Text -> MissionRegistry -> MissionRegistry
associateWorkerWithParentMission parent worker registry = associateSessionWithMission worker (fromMaybe parent (resolveMissionId parent registry)) registry

modifyMissionStore :: Text -> (MissionStore -> MissionStore) -> MissionRegistry -> MissionRegistry
modifyMissionStore identifier update registry =
  let mission = fromMaybe identifier (resolveMissionId identifier registry)
      associated = associateSessionWithMission identifier mission registry
   in associated {registryStores = Map.adjust (fmap update) mission (registryStores associated)}

invalidateMissionStore :: Text -> MissionRegistry -> MissionRegistry
invalidateMissionStore identifier registry =
  let mission = fromMaybe identifier (resolveMissionId identifier registry)
      associated = associateSessionWithMission identifier mission registry
   in associated {registryStores = Map.insert mission Nothing (registryStores associated)}

restoreRegistrySnapshotAt :: Text -> Text -> MissionSnapshot -> MissionRegistry -> MissionRegistry
restoreRegistrySnapshotAt observedAt identifier snapshot registry =
  let associated = associateSessionWithMission identifier identifier registry
      previous = fromMaybe emptyMissionStore (Map.findWithDefault Nothing identifier (registryStores associated))
      restored = associated {registryStores = Map.insert identifier (Just (restoreSnapshotAt observedAt snapshot previous)) (registryStores associated)}
   in foldl' (flip (associateWorkerWithParentMission identifier)) restored (missionSnapshotWorkers snapshot)

sharesMission :: Text -> Text -> MissionRegistry -> Bool
sharesMission first second registry = case resolveMissionId first registry of
  Nothing -> False
  Just mission -> resolveMissionId second registry == Just mission

-- | Apply an observation to its declared session, then associate the roster
-- explicitly reported by that mission. Usage alone never creates a store.
applyRegistryEventAt :: Text -> Text -> DroidEvent -> MissionRegistry -> MissionRegistry
applyRegistryEventAt observedAt identifier event registry = case event of
  UsageEvent usage -> case lookupMissionStore identifier registry of
    Right (Just store) | sessionUsageId usage == identifier || hasWorkerSession (sessionUsageId usage) store -> modifyMissionStore identifier (applyEventAt observedAt event) registry
    _ -> registry
  MissionStateEvent _ -> update
  MissionFeaturesEvent _ -> update
  MissionProgressEvent _ -> update
  MissionWorkerStartedEvent _ -> update
  MissionWorkerCompletedEvent _ -> update
  _ -> registry
  where
    update =
      let changed = modifyMissionStore identifier (applyEventAt observedAt event) registry
       in case lookupMissionStore identifier changed of
            Right (Just store) -> foldl' (flip (associateWorkerWithParentMission identifier)) changed (missionSnapshotWorkers (missionSnapshot store))
            _ -> changed
