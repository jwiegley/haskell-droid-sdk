{-# LANGUAGE OverloadedStrings #-}

-- | Request-bound interaction validation and safe RPC adapters. Handlers belong
-- to the owned CLI connection (including successor sessions), not to one turn.
module Factory.Droid.Interaction
  ( DroidHandlers (..),
    defaultDroidHandlers,
    DroidInteraction (..),
    DroidInteractionFailure (..),
    DroidMcpFailure (..),
    respondPermission,
    answerDroidQuestion,
    answerDroidQuestionMultiple,
    submitDroidAnswers,
    cancelDroidQuestions,
    permissionRpcHandler,
    questionRpcHandler,
    PendingInteractions,
    PendingInteractionId,
    PendingRequest (..),
    PendingPermission,
    PendingQuestion,
    PendingSnapshot (..),
    PendingInteractionError (..),
    newPendingInteractions,
    withDeferredResponses,
    permissionRequestsEnabled,
    closePendingInteractions,
    clearPendingInteractions,
    clearSessionPendingInteractions,
    clearInactivePendingForSession,
    inactivePendingRequestIds,
    markPendingInactive,
    hasActivePendingForSession,
    deferPendingPermission,
    deferPendingQuestion,
    getPendingSnapshot,
    waitPendingSnapshotChange,
    getPendingPermissions,
    getPendingPermissionsForSession,
    getPendingQuestions,
    onPendingPermissions,
    onPendingQuestions,
    dispatchExternalPermissionRequest,
    respondPendingPermission,
    respondPendingQuestion,
    recordPermissionResolved,
    refreshPendingPermission,
    PreparedInteraction (..),
    preparePermissionRpcHandler,
    prepareQuestionRpcHandler,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, TMVar, TVar, atomically, check, modifyTVar', newEmptyTMVarIO, newTVarIO, readTMVar, readTVar, throwSTM, tryPutTMVar, tryReadTMVar, writeTVar)
import Control.DeepSeq (force)
import Control.Exception (Exception, evaluate, finally, mask, mask_)
import Control.Monad (forM_, guard, unless, void, when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import Data.Aeson.Types (parseEither)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Scientific (Scientific)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Unique (Unique, newUnique)
import Factory.Droid.Internal.Exception (trySync)
import Factory.Droid.Internal.Stream (DroidEvent)
import Factory.Droid.Protocol (RpcChannelError)
import Factory.Droid.Protocol.Dispatch (RpcPreparedRequest (..), RpcRequestHandler)
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome)
import Factory.Droid.Schema.RPC (BaseRequest (..), JsonRpcBaseRequest, JsonRpcError, WithEnvelope (..))

-- | Permission/question callbacks use concurrent dispatcher-owned workers.
-- MCP and request-settled observers run serially on intake, including startup.
-- Settled IDs cover the local connection or the exact daemon attachment session.
-- Keep observers brief: no turns, replacement, loads or waits for later intake.
-- Ordinary callback exceptions are isolated.
data DroidHandlers = DroidHandlers
  { onDroidPermission :: Maybe (RequestPermissionParams -> IO RequestPermissionResult),
    onDroidQuestion :: Maybe (AskUserParams -> IO AskUserResult),
    onDroidInteractionFailure :: Maybe (DroidInteractionFailure -> IO ()),
    onDroidMcpEvent :: Maybe (Maybe Text -> Either DroidMcpFailure DroidEvent -> IO ()),
    onDroidRequestSettled :: Maybe (Text -> IO ())
  }

instance Show DroidHandlers where
  show _ = "DroidHandlers <redacted>"

defaultDroidHandlers :: DroidHandlers
defaultDroidHandlers = DroidHandlers Nothing Nothing Nothing Nothing Nothing

data DroidMcpFailure = DroidMcpInvalidEvent | DroidMcpConnectionFailure !RpcChannelError
  deriving stock (Eq, Show)

data DroidInteraction = PermissionInteraction | QuestionInteraction
  deriving stock (Eq, Show)

-- | Payload-free failure classifications. No question, command, answer or
-- exception text is copied into diagnostic output.
data DroidInteractionFailure
  = InvalidInteractionRequest !DroidInteraction
  | InteractionHandlerFailed !DroidInteraction
  | InvalidInteractionResponse !DroidInteraction
  deriving stock (Eq, Show)

respondPermission :: RequestPermissionParams -> ToolConfirmationOutcome -> Maybe Text -> Maybe Text -> Maybe RequestPermissionResult
respondPermission request choice comment edited = do
  guard (choice `elem` map confirmationOptionValue (permissionOptions request))
  mkRequestPermissionResult choice comment edited mempty

answerDroidQuestion :: AskUserQuestion -> Text -> AskUserCollectedAnswer
answerDroidQuestion question answer = AskUserCollectedAnswer (questionIndex question) (questionText question) answer mempty

-- | Multi-select answers use the reference comma-space text convention. Options
-- remain suggestions; free-form answers are not rejected for lacking membership.
answerDroidQuestionMultiple :: AskUserQuestion -> [Text] -> AskUserCollectedAnswer
answerDroidQuestionMultiple question = answerDroidQuestion question . Text.intercalate ", "

submitDroidAnswers :: AskUserParams -> [AskUserCollectedAnswer] -> Maybe AskUserResult
submitDroidAnswers request answers =
  let result = AskUserResult answers (Just False) mempty
   in result <$ guard (validQuestionResponse request result)

cancelDroidQuestions :: AskUserResult
cancelDroidQuestions = AskUserResult [] (Just True) mempty

-- | Safe adapter for the generic dispatcher. Unconfigured handlers cancel
-- quietly; malformed requests, invalid replies and ordinary failures are reported.
permissionRpcHandler :: DroidHandlers -> RpcRequestHandler
permissionRpcHandler handlers request =
  Right <$> handleInteraction handlers PermissionInteraction (onDroidPermission handlers) validPermissionResponse cancelPermissionResult request

questionRpcHandler :: DroidHandlers -> RpcRequestHandler
questionRpcHandler handlers request =
  Right <$> handleInteraction handlers QuestionInteraction (onDroidQuestion handlers) validQuestionResponse cancelDroidQuestions request

validPermissionResponse :: RequestPermissionParams -> RequestPermissionResult -> Bool
validPermissionResponse request response = permissionSelectedOption response `elem` map confirmationOptionValue (permissionOptions request)

validQuestionResponse :: AskUserParams -> AskUserResult -> Bool
validQuestionResponse request response =
  let answers = askUserAnswers response
      expected = Set.fromList [(questionIndex question, questionText question) | question <- askUserQuestions request]
      indices = Set.fromList (map answerIndex answers)
   in (askUserCancelled response /= Just True || null answers)
        && all (\answer -> (answerIndex answer, answerQuestion answer) `Set.member` expected) answers
        && Set.size indices == length answers

handleInteraction :: (FromJSON request, ToJSON response) => DroidHandlers -> DroidInteraction -> Maybe (request -> IO response) -> (request -> response -> Bool) -> response -> JsonRpcBaseRequest -> IO Value
handleInteraction handlers interaction handler valid fallback request = case handler of
  Nothing -> pure (toJSON fallback)
  Just action -> do
    parsed <- decodeInteraction handlers interaction request
    toJSON <$> maybe (pure fallback) (runInteractionHandler handlers interaction action valid fallback) parsed

decodeInteraction :: (FromJSON request) => DroidHandlers -> DroidInteraction -> JsonRpcBaseRequest -> IO (Maybe request)
decodeInteraction handlers interaction request = case baseRequestParams (envelopeBody request) >>= either (const Nothing) Just . parseEither parseJSON of
  Nothing -> reportInteractionFailure handlers (InvalidInteractionRequest interaction) >> pure Nothing
  Just parsed -> pure (Just parsed)

runInteractionHandler :: (ToJSON response) => DroidHandlers -> DroidInteraction -> (request -> IO response) -> (request -> response -> Bool) -> response -> request -> IO response
runInteractionHandler handlers interaction action valid fallback parsed = do
  outcome <- trySync $ do
    response <- action parsed
    if valid parsed response then evaluate (force (toJSON response)) >> pure (Just response) else pure Nothing
  case outcome of
    Left _ -> reject (InteractionHandlerFailed interaction)
    Right Nothing -> reject (InvalidInteractionResponse interaction)
    Right (Just value) -> pure value
  where
    reject failure = reportInteractionFailure handlers failure >> pure fallback

reportInteractionFailure :: DroidHandlers -> DroidInteractionFailure -> IO ()
reportInteractionFailure handlers failure = forM_ (onDroidInteractionFailure handlers) (\report -> void (trySync (report failure)))

newtype PendingInteractionId = PendingInteractionId Unique deriving stock (Eq, Ord)

instance Show PendingInteractionId where show _ = "PendingInteractionId <redacted>"

data PendingRequest request = PendingRequest
  { pendingInteractionId :: !PendingInteractionId,
    pendingRequestId :: !Text,
    pendingSessionId :: !Text,
    pendingAssociatedSessionIds :: ![Text],
    pendingRequestParams :: !request,
    pendingObservedAt :: !Scientific,
    pendingInactive :: !Bool
  }
  deriving stock (Eq)

instance Show (PendingRequest request) where show _ = "PendingRequest <redacted>"

type PendingPermission = PendingRequest RequestPermissionParams

type PendingQuestion = PendingRequest AskUserParams

data PendingSnapshot = PendingSnapshot
  { pendingPermissions :: ![PendingPermission],
    pendingQuestions :: ![PendingQuestion]
  }
  deriving stock (Eq)

instance Show PendingSnapshot where show _ = "PendingSnapshot <redacted>"

data PendingInteractionError = PendingControllerClosed | PendingControllerAlreadyManaged | PendingRequestMissing | PendingWrongSession | PendingInvalidResponse | PendingAlreadyAnswered | PendingRequestInactive | PendingCannotDefer deriving stock (Eq, Show)

instance Exception PendingInteractionError

data PendingEntry request response = PendingEntry !(PendingRequest request) !(TMVar (Maybe response)) !(Maybe Unique)

type PendingSubscribers request = Map Unique (Maybe [Text], PendingRequest request -> IO ())

-- No reader or worker is created here. Existing dispatcher-owned requests own
-- their callback race; this controller retains metadata and one reply choice.
data PendingInteractions = PendingInteractions
  { controllerOpen :: !(TVar Bool),
    controllerManualScope :: !(TVar (Maybe Unique)),
    permissionEntries :: !(TVar (Map PendingInteractionId (PendingEntry RequestPermissionParams RequestPermissionResult))),
    questionEntries :: !(TVar (Map PendingInteractionId (PendingEntry AskUserParams AskUserResult))),
    permissionSubscribers :: !(TVar (PendingSubscribers RequestPermissionParams)),
    questionSubscribers :: !(TVar (PendingSubscribers AskUserParams))
  }

newPendingInteractions :: IO PendingInteractions
newPendingInteractions = PendingInteractions <$> newTVarIO True <*> newTVarIO Nothing <*> newTVarIO mempty <*> newTVarIO mempty <*> newTVarIO mempty <*> newTVarIO mempty

-- | Explicitly defer otherwise-unconfigured requests for this scope. Existing
-- automatic requests retain their ownership; exit cancels only requests admitted
-- during this scope. The controller itself remains connection-owned.
withDeferredResponses :: PendingInteractions -> IO a -> IO a
withDeferredResponses controller action = mask $ \restore -> do
  token <- newUnique
  atomically $ do
    checkPendingOpen controller
    current <- readTVar (controllerManualScope controller)
    when (isJust current) (throwSTM PendingControllerAlreadyManaged)
    writeTVar (controllerManualScope controller) (Just token)
  restore action
    `finally` atomically
      ( do
          current <- readTVar (controllerManualScope controller)
          when (current == Just token) (writeTVar (controllerManualScope controller) Nothing)
          clearPendingWhere controller (== Just token)
      )

permissionRequestsEnabled :: PendingInteractions -> DroidHandlers -> STM Bool
permissionRequestsEnabled controller handlers = (isJust (onDroidPermission handlers) ||) . isJust <$> readTVar (controllerManualScope controller)

clearPendingInteractions :: PendingInteractions -> STM ()
clearPendingInteractions controller = clearPendingWhere controller (const True)

clearPendingWhere :: PendingInteractions -> (Maybe Unique -> Bool) -> STM ()
clearPendingWhere controller selected = do
  permissions <- readTVar (permissionEntries controller)
  questions <- readTVar (questionEntries controller)
  let matches :: PendingEntry request response -> Bool
      matches (PendingEntry _ _ scope) = selected scope
      (removedPermissions, keptPermissions) = Map.partition matches permissions
      (removedQuestions, keptQuestions) = Map.partition matches questions
  forM_ removedPermissions (\(PendingEntry _ reply _) -> void (tryPutTMVar reply (Just cancelPermissionResult)))
  forM_ removedQuestions (\(PendingEntry _ reply _) -> void (tryPutTMVar reply (Just cancelDroidQuestions)))
  writeTVar (permissionEntries controller) keptPermissions
  writeTVar (questionEntries controller) keptQuestions

closePendingInteractions :: PendingInteractions -> STM ()
closePendingInteractions controller = do
  writeTVar (controllerOpen controller) False
  writeTVar (controllerManualScope controller) Nothing
  clearPendingInteractions controller
  writeTVar (permissionSubscribers controller) mempty
  writeTVar (questionSubscribers controller) mempty

checkPendingOpen :: PendingInteractions -> STM ()
checkPendingOpen controller = readTVar (controllerOpen controller) >>= \open -> unless open (throwSTM PendingControllerClosed)

readPendingSnapshot :: PendingInteractions -> STM PendingSnapshot
readPendingSnapshot controller = do
  checkPendingOpen controller
  permissions <- readTVar (permissionEntries controller)
  questions <- readTVar (questionEntries controller)
  pure (PendingSnapshot [request | PendingEntry request _ _ <- Map.elems permissions] [request | PendingEntry request _ _ <- Map.elems questions])

getPendingSnapshot :: PendingInteractions -> IO PendingSnapshot
getPendingSnapshot = atomically . readPendingSnapshot

-- Intermediate snapshots may coalesce, unlike request subscriptions.
waitPendingSnapshotChange :: PendingInteractions -> PendingSnapshot -> IO PendingSnapshot
waitPendingSnapshotChange controller previous = atomically $ do
  current <- readPendingSnapshot controller
  check (current /= previous)
  pure current

getPendingPermissions :: PendingInteractions -> IO [PendingPermission]
getPendingPermissions controller = pendingPermissions <$> getPendingSnapshot controller

getPendingPermissionsForSession :: PendingInteractions -> Text -> IO [PendingPermission]
getPendingPermissionsForSession controller identifier = filter (elem identifier . pendingAssociatedSessionIds) <$> getPendingPermissions controller

getPendingQuestions :: PendingInteractions -> IO [PendingQuestion]
getPendingQuestions controller = pendingQuestions <$> getPendingSnapshot controller

hasActivePendingForSession :: PendingInteractions -> Text -> STM Bool
hasActivePendingForSession controller surface = do
  permissions <- activeIn (permissionEntries controller)
  questions <- activeIn (questionEntries controller)
  pure (permissions || questions)
  where
    activeIn :: TVar (Map PendingInteractionId (PendingEntry request response)) -> STM Bool
    activeIn entries = do
      current <- readTVar entries
      states <- traverse (\(PendingEntry pending reply _) -> do chosen <- tryReadTMVar reply; pure (surface `elem` pendingAssociatedSessionIds pending && not (pendingInactive pending) && isNothing chosen)) (Map.elems current)
      pure (or states)

-- One registration can name several surfaces and is invoked once per request.
-- Nothing selects all surfaces; Just [] selects none. No response is granted.
onPendingPermissions :: PendingInteractions -> Maybe [Text] -> (PendingPermission -> IO ()) -> IO (IO ())
onPendingPermissions controller = subscribePending controller (permissionSubscribers controller)

onPendingQuestions :: PendingInteractions -> Maybe [Text] -> (PendingQuestion -> IO ()) -> IO (IO ())
onPendingQuestions controller = subscribePending controller (questionSubscribers controller)

subscribePending :: PendingInteractions -> TVar (PendingSubscribers request) -> Maybe [Text] -> (PendingRequest request -> IO ()) -> IO (IO ())
subscribePending controller subscribers sessions callback = do
  token <- newUnique
  atomically (checkPendingOpen controller >> modifyTVar' subscribers (Map.insert token (sessions, callback)))
  pure (atomically (modifyTVar' subscribers (Map.delete token)))

notifyPending :: PendingInteractions -> TVar (PendingSubscribers request) -> PendingRequest request -> IO ()
notifyPending controller subscribers pending = do
  selected <- atomically $ do
    open <- readTVar (controllerOpen controller)
    callbacks <- readTVar subscribers
    pure [callback | open, (sessions, callback) <- Map.elems callbacks, maybe True (any (`elem` pendingAssociatedSessionIds pending)) sessions]
  forM_ selected (\callback -> void (trySync (callback pending)))

-- | Observation only. A foreign controller's token is never installed here;
-- its response must be routed through the owning controller.
dispatchExternalPermissionRequest :: PendingInteractions -> PendingPermission -> IO ()
dispatchExternalPermissionRequest controller = notifyPending controller (permissionSubscribers controller)

respondPendingPermission :: PendingInteractions -> Text -> PendingInteractionId -> RequestPermissionResult -> IO ()
respondPendingPermission controller = choosePending controller (permissionEntries controller) valid
  where
    valid request response = permissionSelectedOption response == permissionSelectedOption cancelPermissionResult || validPermissionResponse request response

respondPendingQuestion :: PendingInteractions -> Text -> PendingInteractionId -> AskUserResult -> IO ()
respondPendingQuestion controller = choosePending controller (questionEntries controller) validQuestionResponse

choosePending :: (ToJSON response) => PendingInteractions -> TVar (Map PendingInteractionId (PendingEntry request response)) -> (request -> response -> Bool) -> Text -> PendingInteractionId -> response -> IO ()
choosePending controller entries valid surface identifier response = do
  _ <- evaluate (force (toJSON response))
  atomically $ do
    checkPendingOpen controller
    current <- readTVar entries
    case Map.lookup identifier current of
      Nothing -> throwSTM PendingRequestMissing
      Just (PendingEntry pending reply _) -> do
        unless (surface `elem` pendingAssociatedSessionIds pending) (throwSTM PendingWrongSession)
        when (pendingInactive pending) (throwSTM PendingRequestInactive)
        unless (valid (pendingRequestParams pending) response) (throwSTM PendingInvalidResponse)
        accepted <- tryPutTMVar reply (Just response)
        unless accepted (throwSTM PendingAlreadyAnswered)

markPendingInactive :: PendingInteractions -> Text -> STM ()
markPendingInactive controller execution = do
  markEntriesInactive execution (permissionEntries controller)
  markEntriesInactive execution (questionEntries controller)

markEntriesInactive :: Text -> TVar (Map PendingInteractionId (PendingEntry request response)) -> STM ()
markEntriesInactive execution entries = do
  current <- readTVar entries
  updated <- traverse mark current
  writeTVar entries updated
  where
    mark entry@(PendingEntry pending reply scope)
      | pendingSessionId pending /= execution = pure entry
      | otherwise = do
          selected <- tryReadTMVar reply
          case selected of
            Nothing -> void (tryPutTMVar reply Nothing) >> pure (PendingEntry (pending {pendingInactive = True}) reply scope)
            Just _ -> pure entry

clearSessionPendingInteractions :: PendingInteractions -> Text -> STM ()
clearSessionPendingInteractions controller execution = do
  clearPendingEntries (permissionEntries controller) ((== execution) . pendingSessionId) cancelPermissionResult
  clearPendingEntries (questionEntries controller) ((== execution) . pendingSessionId) cancelDroidQuestions

inactivePendingRequestIds :: PendingInteractions -> Text -> STM [Text]
inactivePendingRequestIds controller execution = do
  permissions <- readTVar (permissionEntries controller)
  questions <- readTVar (questionEntries controller)
  let identifiers :: Map PendingInteractionId (PendingEntry request response) -> [Text]
      identifiers entries = [pendingRequestId pending | PendingEntry pending _ _ <- Map.elems entries, pendingSessionId pending == execution, pendingInactive pending]
  pure (identifiers permissions <> identifiers questions)

clearInactivePendingForSession :: PendingInteractions -> Text -> STM ()
clearInactivePendingForSession controller execution = do
  let selected :: PendingRequest request -> Bool
      selected pending = pendingSessionId pending == execution && pendingInactive pending
  clearPendingEntries (permissionEntries controller) selected cancelPermissionResult
  clearPendingEntries (questionEntries controller) selected cancelDroidQuestions

clearPendingEntries :: TVar (Map PendingInteractionId (PendingEntry request response)) -> (PendingRequest request -> Bool) -> response -> STM ()
clearPendingEntries entries selected fallback = do
  current <- readTVar entries
  let (removed, retained) = Map.partition (\(PendingEntry pending _ _) -> selected pending) current
  forM_ removed (\(PendingEntry _ reply _) -> void (tryPutTMVar reply (Just fallback)))
  writeTVar entries retained

-- These STM operations let the daemon atomically retain a revalidated decision
-- and retire its inactive prompt. They do not write a wire response.
deferPendingPermission :: PendingInteractions -> Text -> PendingInteractionId -> RequestPermissionResult -> STM PendingPermission
deferPendingPermission controller = deferPending controller (permissionEntries controller) (\request response -> permissionSelectedOption response == permissionSelectedOption cancelPermissionResult || validPermissionResponse request response)

deferPendingQuestion :: PendingInteractions -> Text -> PendingInteractionId -> AskUserResult -> STM PendingQuestion
deferPendingQuestion controller = deferPending controller (questionEntries controller) validQuestionResponse

deferPending :: PendingInteractions -> TVar (Map PendingInteractionId (PendingEntry request response)) -> (request -> response -> Bool) -> Text -> PendingInteractionId -> response -> STM (PendingRequest request)
deferPending controller entries valid surface identifier response = do
  checkPendingOpen controller
  current <- readTVar entries
  case Map.lookup identifier current of
    Nothing -> throwSTM PendingRequestMissing
    Just (PendingEntry pending reply _) -> do
      unless (surface `elem` pendingAssociatedSessionIds pending) (throwSTM PendingWrongSession)
      unless (pendingInactive pending) (throwSTM PendingCannotDefer)
      unless (valid (pendingRequestParams pending) response) (throwSTM PendingInvalidResponse)
      void (tryPutTMVar reply Nothing)
      writeTVar entries (Map.delete identifier current)
      pure pending

-- Association-only replays update the view, not an admitted handler's snapshot.
-- Conflicting undecided requests cancel; an already chosen reply stays chosen.
refreshPendingPermission :: PendingInteractions -> Text -> Text -> RequestPermissionParams -> STM ()
refreshPendingPermission controller execution identifier params = do
  current <- readTVar (permissionEntries controller)
  updated <- traverse refresh current
  writeTVar (permissionEntries controller) updated
  where
    refresh entry@(PendingEntry pending reply scope)
      | pendingSessionId pending /= execution || pendingRequestId pending /= identifier || pendingInactive pending = pure entry
      | otherwise = do
          selected <- tryReadTMVar reply
          case selected of
            Just _ -> pure entry
            Nothing -> do
              let previous = pendingRequestParams pending
              if (previous {permissionAssociatedSessionIds = Nothing}) == (params {permissionAssociatedSessionIds = Nothing})
                then do
                  let surfaces = nubOrd (pendingAssociatedSessionIds pending <> (execution : fromMaybe [] (permissionAssociatedSessionIds params)))
                  pure (PendingEntry (pending {pendingAssociatedSessionIds = surfaces, pendingRequestParams = previous {permissionAssociatedSessionIds = Just surfaces}}) reply scope)
                else void (tryPutTMVar reply (Just cancelPermissionResult)) >> pure entry

recordPermissionResolved :: PendingInteractions -> Text -> Text -> STM [PendingPermission]
recordPermissionResolved controller surface identifier = do
  current <- readTVar (permissionEntries controller)
  let matches (PendingEntry pending _ _) = pendingRequestId pending == identifier && surface `elem` pendingAssociatedSessionIds pending
      resolved = Map.filter matches current
      retained = Map.filter (\entry@(PendingEntry pending _ _) -> not (matches entry && pendingInactive pending)) current
  forM_ resolved $ \(PendingEntry _ reply _) -> void (tryPutTMVar reply Nothing)
  writeTVar (permissionEntries controller) retained
  pure [pending | PendingEntry pending _ _ <- Map.elems resolved]

data PreparedInteraction = PreparedInteraction
  { preparedInteractionRequest :: RpcPreparedRequest,
    cancelPreparedInteraction :: IO (Maybe (Either JsonRpcError Value))
  }

preparePermissionRpcHandler :: PendingInteractions -> STM Bool -> Text -> DroidHandlers -> JsonRpcBaseRequest -> IO PreparedInteraction
preparePermissionRpcHandler controller allowed execution handlers request = do
  enabled <- atomically (permissionRequestsEnabled controller handlers)
  if not enabled
    then pure (PreparedInteraction (RpcPreparedRequest (Just <$> permissionRpcHandler handlers request) (pure ())) (pure (Just (Right (toJSON cancelPermissionResult)))))
    else case baseRequestParams (envelopeBody request) >>= either (const Nothing) Just . parseEither parseJSON of
      Nothing -> pure (PreparedInteraction (RpcPreparedRequest (reportInteractionFailure handlers (InvalidInteractionRequest PermissionInteraction) >> pure (Just (Right (toJSON cancelPermissionResult)))) (pure ())) (pure (Just (Right (toJSON cancelPermissionResult)))))
      Just params -> do
        let surfaces = nubOrd (execution : fromMaybe [] (permissionAssociatedSessionIds params))
            automatic = (\action -> runInteractionHandler handlers PermissionInteraction action validPermissionResponse cancelPermissionResult params) <$> onDroidPermission handlers
        prepared <- preparePending controller allowed (permissionEntries controller) (notifyPending controller (permissionSubscribers controller)) (baseRequestId (envelopeBody request)) execution surfaces params automatic cancelPermissionResult
        pure (encodePreparedInteraction prepared)

prepareQuestionRpcHandler :: PendingInteractions -> STM Bool -> Text -> DroidHandlers -> JsonRpcBaseRequest -> IO PreparedInteraction
prepareQuestionRpcHandler controller allowed execution handlers request = do
  enabled <- atomically ((isJust (onDroidQuestion handlers) ||) . isJust <$> readTVar (controllerManualScope controller))
  if not enabled
    then pure (PreparedInteraction (RpcPreparedRequest (Just <$> questionRpcHandler handlers request) (pure ())) (pure (Just (Right (toJSON cancelDroidQuestions)))))
    else case baseRequestParams (envelopeBody request) >>= either (const Nothing) Just . parseEither parseJSON of
      Nothing -> pure (PreparedInteraction (RpcPreparedRequest (reportInteractionFailure handlers (InvalidInteractionRequest QuestionInteraction) >> pure (Just (Right (toJSON cancelDroidQuestions)))) (pure ())) (pure (Just (Right (toJSON cancelDroidQuestions)))))
      Just params -> do
        let automatic = (\action -> runInteractionHandler handlers QuestionInteraction action validQuestionResponse cancelDroidQuestions params) <$> onDroidQuestion handlers
        prepared <- preparePending controller allowed (questionEntries controller) (notifyPending controller (questionSubscribers controller)) (baseRequestId (envelopeBody request)) execution [execution] params automatic cancelDroidQuestions
        pure (encodePreparedInteraction prepared)

encodePreparedInteraction :: (ToJSON response) => (IO (Maybe response), IO (Maybe response), IO ()) -> PreparedInteraction
encodePreparedInteraction (action, cancel, cleanup) = PreparedInteraction (RpcPreparedRequest (fmap (fmap (Right . toJSON)) action) cleanup) (fmap (fmap (Right . toJSON)) cancel)

preparePending :: PendingInteractions -> STM Bool -> TVar (Map PendingInteractionId (PendingEntry request response)) -> (PendingRequest request -> IO ()) -> Text -> Text -> [Text] -> request -> Maybe (IO response) -> response -> IO (IO (Maybe response), IO (Maybe response), IO ())
preparePending controller allowed entries notify identifier execution surfaces params automatic fallback = mask_ $ do
  token <- PendingInteractionId <$> newUnique
  timestamp <- fromInteger . floor . (* 1000) <$> getPOSIXTime
  reply <- newEmptyTMVarIO
  let pending = PendingRequest token identifier execution surfaces params timestamp False
  admitted <- atomically $ do
    open <- readTVar (controllerOpen controller)
    current <- allowed
    scope <- readTVar (controllerManualScope controller)
    let admitted = open && current && (isJust automatic || isJust scope)
    when admitted (modifyTVar' entries (Map.insert token (PendingEntry pending reply scope)))
    pure admitted
  if not admitted
    then pure (pure (Just fallback), pure (Just fallback), pure ())
    else do
      let announce = do
            current <- atomically (Map.lookup token <$> readTVar entries)
            forM_ current (\(PendingEntry latest _ _) -> notify latest)
          cleanup = atomically (modifyTVar' entries (Map.update (\entry@(PendingEntry latest _ _) -> if pendingInactive latest then Just entry else Nothing) token))
          cancel = atomically (void (tryPutTMVar reply (Just fallback)) >> readTMVar reply)
      pure (announce >> decide reply, cancel, cleanup)
  where
    decide reply = do
      selected <- atomically (tryReadTMVar reply)
      case selected of
        Just response -> pure response
        Nothing -> case automatic of
          Nothing -> atomically (readTMVar reply)
          Just action -> either id id <$> race (atomically (readTMVar reply)) (do response <- action; atomically (void (tryPutTMVar reply (Just response)) >> readTMVar reply))
