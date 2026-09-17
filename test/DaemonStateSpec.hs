{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DaemonStateSpec (daemonStateTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (AsyncException (..), Exception, finally, fromException, throwIO, try, uninterruptibleMask_)
import Control.Monad (forM_, unless, void, when, (>=>))
import Data.Aeson (Object, Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Factory.Droid (DroidError (..))
import Factory.Droid.Connection qualified as Connection
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Interaction qualified as Interaction
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Observability qualified as Obs
import Factory.Droid.Protocol (RpcChannelError (..))
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Control (AddUserMessageParams (..), QueuePlacement (..), QueueResolution (..), QueuedUserMessage (..), ResolveQueuedMessageParams (..), defaultUserMessageParams)
import Factory.Droid.Schema.Daemon.Settings (DefaultSettings (..))
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Interaction (AskUserResult (..))
import Factory.Droid.Schema.Mission (MissionPhase (..), MissionSnapshot (..))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport (ObjectTransport, objectTransport)
import ProtocolSpec (PeerFailure (..), reply)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

newtype Marker = Marker Int deriving stock (Eq, Show)

instance Exception Marker

daemonStateTests :: TestTree
daemonStateTests =
  testGroup
    "Logical daemon state"
    [ testCase "retains observations policy cache selection and missions without reviving an old handle" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "first" identity
            atomically (writeTVar (peerLoadExtra first) (KeyMap.singleton "mission" missionWire))
            (old, oldId) <- connect shared first $ \connection -> do
              void (Daemon.setSessionCacheCapacity connection (Just 7))
              void (Daemon.setActiveSessionId connection (Just "saved"))
              void (Daemon.loadSessionInfoWithConfiguration connection "saved" (defaultSessionLoadConfiguration {loadMessageLimit = Just 7}) (defaultDaemonLoadConfiguration {daemonLoadRuntimeSettingsPath = Just "runtime"}))
              void (Daemon.registerSessionState connection "cold" "machine")
              Daemon.setSessionPreInit connection "cold" True
              old <- Daemon.withResumedSessionOn connection "saved" pure
              Daemon.storeDeferredQuestion connection "saved" (State.DeferredUserAction "deferred" "tool" (AskUserResult [] (Just True) mempty) 0 [])
              (old,) <$> Daemon.getConnectionId connection
            snapshot <- Daemon.getDaemonStateSnapshot shared
            Daemon.daemonStateGeneration snapshot @?= 1
            Daemon.daemonStateHealth snapshot @?= Nothing
            Daemon.daemonStateCacheCapacity snapshot @?= Just 7
            Daemon.daemonStateActiveSession snapshot @?= Just "saved"
            map Daemon.directorySessionId (Daemon.daemonStateDirectory snapshot) @?= ["saved", "cold"]
            map (Daemon.readinessPhase . Daemon.directoryReadiness) (Daemon.daemonStateDirectory snapshot) @?= [Daemon.SessionNotLoaded, Daemon.SessionNotLoaded]
            case Mission.lookupMissionStore "saved" (Daemon.daemonStateMissions snapshot) of
              Right (Just stored) -> do
                missionSnapshotTitle (Mission.missionSnapshot stored) @?= Just "retained mission"
                missionSnapshotState (Mission.missionSnapshot stored) @?= MissionRunning
              _ -> assertFailure "Mission observation lost"
            stateFor snapshot "saved" >>= (@?= [String "first-saved"]) . messageIds
            stateFor snapshot "saved" >>= (@?= Just (State.DeferredUserAction "deferred" "tool" (AskUserResult [] (Just True) mempty) 0 [])) . Map.lookup "tool" . State.sessionDeferredQuestions
            modelFor snapshot "saved" >>= (@?= String "first")
            second <- newPeer "second" identity
            connect shared second $ \connection -> do
              newId <- Daemon.getConnectionId connection
              assertBool "Physical generation reused its identity" (newId /= oldId)
              try @DroidError (Daemon.getSettings old) >>= (@?= Left DroidSessionUnusable)
              Daemon.getSessionMachineId connection "cold" >>= (@?= Just "machine")
              Daemon.getSessionReadiness connection "cold" >>= (@?= True) . Daemon.readinessPreInit
              Daemon.withResumedSessionOn connection "saved" (Daemon.getSettings >=> ((@?= String "second") . field "modelId" . fields . toJSON))
              request <- nextRequest second "daemon.load_session"
              field "messageLimit" (params request) @?= Number 7
              field "runtimeSettingsPath" (params request) @?= String "runtime"
            after <- Daemon.getDaemonStateSnapshot shared
            Daemon.daemonStateGeneration after @?= 2
            stateFor after "saved" >>= (@?= [String "first-saved", String "second-saved"]) . messageIds,
      testCase "the existing connection controller reuses state across a real controlled transport loss" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "first" identity
            second <- newPeer "second" identity
            count <- newIORef (0 :: Int)
            let acquire :: IO () -> (Daemon.DaemonConnection -> IO a) -> IO a
                acquire opened action = do
                  n <- atomicModifyIORef' count (\old -> (old + 1, old + 1))
                  peer <- case n of 1 -> pure first; 2 -> pure second; _ -> assertFailure "Unexpected reacquisition"
                  opened
                  connect shared peer action
                plan = Connection.ConnectionPlan acquire (const (pure ())) Nothing Connection.classifyConnectionFailure
                poll = Connection.defaultConnectionPollOptions {Connection.connectionPollAttempts = 1, Connection.connectionPollIntervalMicros = 0}
            Connection.withConnectionController plan $ \controller -> do
              original <- Connection.withReadyConnection controller poll $ \connection -> do
                void (Daemon.loadSessionInfo connection "saved")
                Daemon.getConnectionId connection
              atomically (writeTQueue (peerIncoming first) (Left PeerEnded))
              waitDisconnected controller
              retained <- Connection.withReadyConnection controller poll $ \connection -> do
                identifiers <- Daemon.getCachedSessionIds connection
                identifiers @?= ["saved"]
                Daemon.getSessionReadiness connection "saved" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase
                (,) identifiers <$> Daemon.getConnectionId connection
              case (original, retained) of
                (Just (Just old), Just (["saved"], Just new)) -> assertBool "Old physical connection retained" (old /= new)
                _ -> assertFailure "Reconnect did not publish"
              readIORef count >>= (@?= 2),
      testCase "principal changes are rejected before early queued traffic can mutate retained data" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "original" identity
            connect shared first $ \connection -> void (Daemon.loadSessionInfo connection "saved")
            before <- Daemon.getDaemonStateSnapshot shared
            forM_ [GetUserInfoResult "other-user" "org" mempty, GetUserInfoResult "user" "other-org" mempty] $ \foreignIdentity -> do
              peer <- newPeer "foreign" foreignIdentity
              atomically (writeTVar (peerBeforeAuth peer) [notification "saved" (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "foreign"]]), notification "saved" (object ["type" .= String "mission_state_changed", "state" .= String "completed"]), question "foreign-question" "saved"])
              try @Daemon.DaemonError (Daemon.withConnectionStateOn shared authenticatedOptions (peerTransport peer) (const (assertFailure "Foreign principal published" :: IO ()))) >>= (@?= Left Daemon.DaemonIdentityMismatch)
              Daemon.getDaemonStateSnapshot shared >>= (@?= before)
              atomically (isEmptyTQueue (peerReplies peer)) >>= (@?= True)
            good <- newPeer "good" identity
            connect shared good (const (pure ()))
            Daemon.getDaemonStateSnapshot shared >>= (@?= 2) . Daemon.daemonStateGeneration,
      testCase "same-principal startup deltas reach retained settings and logical STM observers" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "old" identity
            connect shared first $ \connection -> void (Daemon.loadSessionInfo connection "saved")
            second <- newPeer "second" identity
            atomically (writeTVar (peerBeforeAuth second) [notification "saved" (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "early"]])])
            withAsync (atomically (do snapshot <- Daemon.readDaemonStateSnapshot shared; check (Daemon.daemonStateGeneration snapshot == 2); pure snapshot)) $ \observer ->
              Daemon.withConnectionStateOn shared authenticatedOptions (peerTransport second) $ \_ -> do
                snapshot <- wait observer
                modelFor snapshot "saved" >>= (@?= String "early")
                case Map.lookup "saved" (Daemon.daemonStateSettings snapshot) of
                  Just (Right settings) -> field "reasoningEffort" (fields (toJSON settings)) @?= String "low"
                  _ -> assertFailure "Retained settings absent",
      testCase "a queued authenticated request sees the installed logical pending controller" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            let pending = Daemon.daemonStatePendingInteractions shared
            observed <- newTQueueIO
            _ <- Interaction.onPendingQuestions pending Nothing (atomically . writeTQueue observed)
            Interaction.withDeferredResponses pending $ do
              peer <- newPeer "early" identity
              atomically (writeTVar (peerBeforeAuth peer) [question "early-question" "saved"])
              Daemon.withConnectionStateOn shared authenticatedOptions (peerTransport peer) $ \_ -> do
                current <- atomically (readTQueue observed)
                Interaction.pendingRequestId current @?= "early-question"
                Interaction.respondPendingQuestion pending "saved" (Interaction.pendingInteractionId current) Interaction.cancelDroidQuestions
                response <- atomically (readTQueue (peerReplies peer))
                field "id" response @?= String "early-question"
                field "cancelled" (fields (field "result" response)) @?= Bool True,
      testCase "exclusive generation admission rejects another scope before it starts a reader or request" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "first" identity
            second <- newPeer "second" identity
            connect shared first $ \_ -> do
              try @Daemon.DaemonError (connect shared second (const (pure ()))) >>= (@?= Left Daemon.DaemonStateInUse)
              readIORef (peerReads second) >>= (@?= 0)
              atomically (isEmptyTQueue (peerRequests second)) >>= (@?= True)
            connect shared second (const (pure ()))
            Daemon.getDaemonStateSnapshot shared >>= (@?= 2) . Daemon.daemonStateGeneration,
      testCase "cancelled authentication releases its lease without admitting queued data" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            peer <- newPeer "held" identity
            atomically $ do
              modifyTVar' (peerHeld peer) (Set.insert "daemon.authenticate")
              writeTVar (peerBeforeAuth peer) [notification "saved" (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "unadmitted"]])]
            withAsync (Daemon.withConnectionStateOn shared authenticatedOptions (peerTransport peer) (const (assertFailure "Held authentication published"))) $ \worker -> do
              void (nextRequest peer "daemon.authenticate")
              cancel worker
              waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Auth cancellation lost"
            snapshot <- Daemon.getDaemonStateSnapshot shared
            Daemon.daemonStateGeneration snapshot @?= 0
            Daemon.daemonStateSettings snapshot @?= mempty
            replacement <- newPeer "replacement" identity
            connect shared replacement (const (pure ())),
      testCase "inactive pending metadata and subscriptions survive while fresh replies keep their owner" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            let pending = Daemon.daemonStatePendingInteractions shared
            observed <- newTQueueIO
            _ <- Interaction.onPendingQuestions pending Nothing (atomically . writeTQueue observed)
            Interaction.withDeferredResponses pending $ do
              first <- newPeer "first" identity
              atomically (writeTVar (peerBeforeProxy first) [question "question" "saved"])
              old <- connect shared first $ \connection -> do
                void (Daemon.loadSessionInfo connection "saved")
                void (Daemon.getProxyToken connection)
                atomically (readTQueue observed)
              inactive <- Interaction.getPendingQuestions pending
              map Interaction.pendingInactive inactive @?= [True]
              try @Interaction.PendingInteractionError (Interaction.respondPendingQuestion pending "saved" (Interaction.pendingInteractionId old) Interaction.cancelDroidQuestions) >>= (@?= Left Interaction.PendingRequestInactive)
              atomically (isEmptyTQueue (peerReplies first)) >>= (@?= True)
              second <- newPeer "second" identity
              atomically (writeTVar (peerBeforeProxy second) [question "question" "saved"])
              connect shared second $ \connection -> do
                void (Daemon.loadSessionInfo connection "saved")
                void (Daemon.getProxyToken connection)
                fresh <- atomically (readTQueue observed)
                assertBool "Old reply token was reactivated" (Interaction.pendingInteractionId fresh /= Interaction.pendingInteractionId old)
                try @Interaction.PendingInteractionError (Interaction.respondPendingQuestion pending "saved" (Interaction.pendingInteractionId old) Interaction.cancelDroidQuestions) >>= (@?= Left Interaction.PendingRequestMissing)
                Interaction.respondPendingQuestion pending "saved" (Interaction.pendingInteractionId fresh) Interaction.cancelDroidQuestions
                response <- atomically (readTQueue (peerReplies second))
                field "id" response @?= String "question"
                field "cancelled" (fields (field "result" response)) @?= Bool True
              atomically (isEmptyTQueue (peerReplies first)) >>= (@?= True),
      testCase "logical owner closure retires and joins a still-admitted physical scope" $ bounded $ do
        sharedSlot <- newEmptyMVar
        entered <- newEmptyMVar
        held <- newEmptyMVar
        peer <- newPeer "held" identity
        withAsync (do shared <- takeMVar sharedSlot; connect shared peer (\connection -> putMVar entered connection >> takeMVar held)) $ \worker -> do
          (shared, connection) <- Daemon.withDaemonState $ \state -> do
            putMVar sharedSlot state
            connection <- takeMVar entered
            pure (state, connection)
          waitCatch worker >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonStateClosed; Right _ -> assertFailure "Closed owner left a live scope"
          try @Daemon.DaemonError (Daemon.getDaemonStateSnapshot shared) >>= (@?= Left Daemon.DaemonStateClosed)
          Daemon.getConnectionId connection >>= (@?= Nothing)
          try @Interaction.PendingInteractionError (Interaction.getPendingSnapshot (Daemon.daemonStatePendingInteractions shared)) >>= (@?= Left Interaction.PendingControllerClosed),
      testCase "an old close callback must quiesce before another generation can attach" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "first" identity
            second <- newPeer "second" identity
            entered <- newEmptyTMVarIO
            release <- newEmptyTMVarIO
            stopped <- newTVarIO False
            let unblock = atomically (void (tryPutTMVar release ()))
                callback _ = uninterruptibleMask_ ((atomically (putTMVar entered ()) >> atomically (readTMVar release)) `finally` atomically (writeTVar stopped True))
                retiring = connect shared first $ \connection -> do
                  void (Daemon.loadSessionInfo connection "saved")
                  void (Daemon.onConnectionClose connection callback)
            withAsync retiring $ \worker ->
              ( do
                  atomically (readTMVar entered)
                  atomically $ do
                    snapshot <- Daemon.readDaemonStateSnapshot shared
                    check (Daemon.daemonStateGeneration snapshot == 1 && isNothing (Daemon.daemonStateHealth snapshot))
                  try @Daemon.DaemonError (connect shared second (const (pure ()))) >>= (@?= Left Daemon.DaemonStateInUse)
                  readIORef (peerReads second) >>= (@?= 0)
                  unblock
                  wait worker
                  readTVarIO stopped >>= (@?= True)
                  connect shared second (const (pure ()))
              )
                `finally` unblock,
      testCase "an old connection-level queue receipt cannot delete successor-generation state" $
        lateQueueMutation
          "daemon.resolve_queued_user_message"
          "same"
          Nothing
          (\connection -> Daemon.resolveQueuedUserMessage connection "saved" (ResolveQueuedMessageParams "same" DeleteQueuedMessage mempty))
          (\connection -> void (Daemon.loadSessionInfo connection "saved")),
      testCase "an old connection-level submission cannot overwrite successor-generation state" $
        lateQueueMutation
          "daemon.add_user_message"
          "new"
          Nothing
          (\connection -> Daemon.submitUserMessage connection "saved" "same" ((defaultUserMessageParams "old") {userMessageQueuePlacement = Just QueueEndOfLoop}))
          checkRetiredSubmission,
      testCase "late queued-send cancellation preserves its cause without restoring an old queue entry" $
        lateQueueMutation
          "daemon.add_user_message"
          "new"
          (Just ThreadKilled)
          ( \connection -> do
              Daemon.queueUserMessage connection "saved" State.QueuePaused (QueuedUserMessage "same" (defaultUserMessageParams "old"))
              Daemon.sendQueuedUserMessage connection "saved" "same"
          )
          checkRetiredSubmission,
      testCase "a failed physical callback preserves its exception and does not close the logical owner" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            first <- newPeer "first" identity
            result <- try @Marker $ connect shared first $ \connection -> do
              void (Daemon.loadSessionInfo connection "saved")
              throwIO (Marker 41) :: IO ()
            result @?= Left (Marker 41)
            snapshot <- Daemon.getDaemonStateSnapshot shared
            map Daemon.directorySessionId (Daemon.daemonStateDirectory snapshot) @?= ["saved"]
            second <- newPeer "second" identity
            connect shared second (const (pure ())),
      testCase "disconnect retires an unpublished creation without erasing a prior cached session" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            peer <- newPeer "peer" identity
            connect shared peer $ \connection -> do
              void (Daemon.loadSessionInfo connection "prior")
              void (atomically (flushTQueue (peerRequests peer)))
              atomically (modifyTVar' (peerHeld peer) (Set.insert "daemon.initialize_session"))
              withAsync (Daemon.withSessionOn connection (creationOptions "fresh") (const (assertFailure "Disconnected creation published"))) $ \worker -> do
                void (nextRequest peer "daemon.initialize_session")
                atomically (writeTQueue (peerIncoming peer) (Left PeerEnded))
                waitCatch worker >>= \case Left cause -> fromException cause @?= Just RpcChannelReadFailure; Right _ -> assertFailure "Disconnected creation succeeded"
                snapshot <- Daemon.getDaemonStateSnapshot shared
                map Daemon.directorySessionId (Daemon.daemonStateDirectory snapshot) @?= ["prior"]
            snapshot <- Daemon.getDaemonStateSnapshot shared
            map Daemon.directorySessionId (Daemon.daemonStateDirectory snapshot) @?= ["prior"],
      testCase "default ephemeral state stays scoped and a closed retained state starts no transport" $ bounded $ do
        peer <- newPeer "legacy" identity
        escaped <- Daemon.withConnectionOn inheritedOptions (peerTransport peer) (pure . Daemon.connectionState)
        try @Daemon.DaemonError (Daemon.getDaemonStateSnapshot escaped) >>= (@?= Left Daemon.DaemonStateClosed)
        unused <- newPeer "unused" identity
        try @Daemon.DaemonError (connect escaped unused (const (pure ()))) >>= (@?= Left Daemon.DaemonStateClosed)
        readIORef (peerReads unused) >>= (@?= 0),
      testCase "caller-owned pending and reported-default records retain their separate lifetimes" $
        bounded $
          Daemon.withDaemonState $ \shared -> do
            let pendingValue = defaultSessionConfiguration {configurationTitle = Just "pending"}
            pending <- newTVarIO pendingValue
            defaults <- newTVarIO Nothing
            first <- newPeer "first" identity
            report <- connect shared first $ \connection -> do
              reported <- Daemon.getDefaultSettings connection
              atomically (writeTVar defaults (Just reported))
              pure reported
            second <- newPeer "second" identity
            connect shared second $ \_ -> do
              readTVarIO pending >>= (@?= pendingValue)
              readTVarIO defaults >>= (@?= Just report)
              defaultsModel report @?= Just "default-first"
    ]

-- Hold the caller after the old reply was accepted, not a dispatcher callback:
-- dispatcher workers are already joined before the successor can be acquired.
lateQueueMutation :: Text -> Text -> Maybe AsyncException -> (Daemon.DaemonConnection -> IO Object) -> (Daemon.DaemonConnection -> IO ()) -> IO ()
lateQueueMutation method successorRequest interrupted mutate prepare = bounded $
  Daemon.withDaemonState $ \shared -> do
    first <- newPeer "first" identity
    second <- newPeer "second" identity
    entered <- newEmptyTMVarIO
    release <- newEmptyTMVarIO
    acquired <- newIORef (0 :: Int)
    let unblock = atomically (void (tryPutTMVar release ()))
        metric event =
          when (Obs.droidMetricName event == "droid.rpc.exchange.duration" && (Obs.droidMetricAttributes event >>= KeyMap.lookup "method") == Just (String method)) $
            uninterruptibleMask_ (atomically (putTMVar entered ()) >> atomically (readTMVar release) >> forM_ interrupted throwIO)
        firstOptions = inheritedOptions {Daemon.daemonClientObservability = Obs.defaultDroidObservability {Obs.observabilityMetrics = Just (Obs.droidMetricSink metric)}}
        acquire :: IO () -> (Daemon.DaemonConnection -> IO a) -> IO a
        acquire opened action = do
          n <- atomicModifyIORef' acquired (\old -> (old + 1, old + 1))
          opened
          case n of
            1 -> Daemon.withConnectionStateOn shared firstOptions (peerTransport first) action
            2 -> connect shared second action
            _ -> assertFailure "Unexpected reacquisition"
        plan = Connection.ConnectionPlan acquire (const (pure ())) Nothing Connection.classifyConnectionFailure
        poll = Connection.defaultConnectionPollOptions {Connection.connectionPollAttempts = 1, Connection.connectionPollIntervalMicros = 0}
    Connection.withConnectionController plan $ \controller ->
      withAsync (Connection.withReadyConnection controller poll (\connection -> void (Daemon.loadSessionInfo connection "saved") >> mutate connection)) $ \old ->
        ( do
            atomically (readTMVar entered)
            atomically (writeTQueue (peerIncoming first) (Left PeerEnded))
            waitDisconnected controller
            current <- Connection.withReadyConnection controller poll $ \connection -> do
              prepare connection
              Daemon.queueUserMessage connection "saved" State.QueueDaemonDiscardable (QueuedUserMessage successorRequest (defaultUserMessageParams "current"))
              unblock
              result <- waitCatch old
              Daemon.getQueuedMessages connection "saved" >>= (@?= ["current"]) . map (userMessageText . queuedMessageInput . State.queueEntryMessage)
              case result of
                Left cause -> case interrupted of
                  Nothing -> fromException cause @?= Just RpcChannelClosed
                  Just expected -> fromException cause @?= Just expected
                Right _ -> assertFailure "Retired queue mutation returned as current"
            current @?= Just ()
        )
          `finally` unblock

checkRetiredSubmission :: Daemon.DaemonConnection -> IO ()
checkRetiredSubmission connection = do
  state <- Daemon.getSessionState connection "saved"
  entry <- maybe (assertFailure "Retired submission observation missing") pure (State.lookupSubmission "same" state)
  State.submissionInFlight entry @?= False
  State.submissionStatus entry @?= State.SubmissionFailed State.SubmissionConnectionFailed
  State.submissionDeadline entry @?= Nothing

identity :: GetUserInfoResult
identity = GetUserInfoResult "user" "org" mempty

inheritedOptions, authenticatedOptions :: Daemon.DaemonClientOptions
inheritedOptions = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication identity "") "/offline") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
authenticatedOptions = inheritedOptions {Daemon.daemonClientAuthentication = Daemon.DaemonAuthenticate (Daemon.DaemonApiKey "OFFLINE_ONLY")}

creationOptions :: Text -> Daemon.DaemonSessionOptions
creationOptions identifier =
  let options = Daemon.defaultDaemonSessionOptions "/created"
   in options {Daemon.daemonSessionParameters = (Daemon.daemonSessionParameters options) {initializeConfiguration = defaultSessionConfiguration {configurationSessionId = Just identifier}}}

data Peer = Peer
  { peerName :: Text,
    peerIdentity :: GetUserInfoResult,
    peerIncoming :: TQueue (Either PeerFailure Object),
    peerRequests :: TQueue Object,
    peerReplies :: TQueue Object,
    peerReads :: IORef Int,
    peerHeld :: TVar (Set Text),
    peerBeforeAuth :: TVar [Object],
    peerBeforeProxy :: TVar [Object],
    peerLoadExtra :: TVar Object
  }

newPeer :: Text -> GetUserInfoResult -> IO Peer
newPeer name user = Peer name user <$> newTQueueIO <*> newTQueueIO <*> newTQueueIO <*> newIORef 0 <*> newTVarIO mempty <*> newTVarIO [] <*> newTVarIO [] <*> newTVarIO mempty

connect :: Daemon.DaemonState -> Peer -> (Daemon.DaemonConnection -> IO a) -> IO a
connect shared peer = Daemon.withConnectionStateOn shared inheritedOptions (peerTransport peer)

peerTransport :: Peer -> ObjectTransport
peerTransport peer = objectTransport send receive
  where
    receive = do
      atomicModifyIORef' (peerReads peer) (\n -> (n + 1, ()))
      atomically (readTQueue (peerIncoming peer)) >>= either throwIO pure
    send frame = case field "method" frame of
      Null -> atomically (writeTQueue (peerReplies peer) frame)
      String method -> do
        atomically (writeTQueue (peerRequests peer) frame)
        when (method == "daemon.authenticate") (emitBatch (peerBeforeAuth peer))
        held <- Set.member method <$> readTVarIO (peerHeld peer)
        unless held $ case method of
          "daemon.authenticate" -> respond frame (toJSON (peerIdentity peer))
          "daemon.load_session" -> loaded frame
          "daemon.initialize_session" -> loaded frame
          "daemon.get_proxy_token" -> emitBatch (peerBeforeProxy peer) >> respond frame (object ["token" .= String "fixture"])
          "daemon.get_default_settings" -> respond frame (object ["modelId" .= ("default-" <> peerName peer)])
          "daemon.update_session_settings" -> respond frame (object [])
          "daemon.resolve_queued_user_message" -> respond frame (object [])
          "daemon.add_user_message" -> respond frame (object [])
          _ -> assertFailure "Unexpected state fixture request"
      _ -> assertFailure "Invalid fixture method"
    emitBatch stored = do
      batch <- atomically $ do values <- readTVar stored; writeTVar stored []; pure values
      mapM_ (emit peer) batch
    respond frame value = emit peer (reply (text "id" frame) value)
    loaded frame = do
      let identifier = text "sessionId" (params frame)
          message = object ["id" .= (peerName peer <> "-" <> identifier), "role" .= String "assistant", "content" .= [object ["type" .= String "text", "text" .= peerName peer]], "createdAt" .= Number 0, "updatedAt" .= Number 0]
          base = fields (object ["sessionId" .= identifier, "session" .= object ["messages" .= [message]], "settings" .= object ["modelId" .= peerName peer, "reasoningEffort" .= String "low"], "cwd" .= ("/" <> peerName peer), "workingState" .= String "idle", "isAgentLoopInProgress" .= False])
      extra <- readTVarIO (peerLoadExtra peer)
      respond frame (Object (KeyMap.union extra base))

emit :: Peer -> Object -> IO ()
emit peer = atomically . writeTQueue (peerIncoming peer) . Right

notification :: Text -> Value -> Object
notification identifier event = envelope ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= event]]

question :: Text -> Text -> Object
question identifier session = envelope ["type" .= String "request", "id" .= identifier, "method" .= String "daemon.ask_user", "params" .= object ["sessionId" .= session, "toolCallId" .= String "tool", "questions" .= ([] :: [Value])]]

envelope :: [(Key, Value)] -> Object
envelope pairs = fields (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> pairs))

nextRequest :: Peer -> Text -> IO Object
nextRequest peer method = do
  request <- atomically (readTQueue (peerRequests peer))
  field "method" request @?= String method
  pure request

waitDisconnected :: Connection.ConnectionController -> IO ()
waitDisconnected controller = do
  status <- Connection.getConnectionStatus controller
  when (Daemon.healthTransportConnected (Connection.connectionStatusHealth status)) $
    Connection.waitConnectionStatusChange controller status >> waitDisconnected controller

stateFor :: Daemon.DaemonStateSnapshot -> Text -> IO State.SessionState
stateFor snapshot identifier = maybe (assertFailure "Retained session missing") pure (Map.lookup identifier (Daemon.daemonStateSessions snapshot))

modelFor :: Daemon.DaemonStateSnapshot -> Text -> IO Value
modelFor snapshot identifier = case Map.lookup identifier (Daemon.daemonStateSettings snapshot) of
  Just (Right settings) -> pure (field "modelId" (fields (toJSON settings)))
  _ -> assertFailure "Retained settings missing"

messageIds :: State.SessionState -> [Value]
messageIds = map (field "id" . fields . toJSON) . State.sessionMessages

missionWire :: Value
missionWire = object ["state" .= String "running", "features" .= ([] :: [Value]), "progressLog" .= ([] :: [Value]), "workerSessionIds" .= [String "worker"], "title" .= String "retained mission"]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

fields :: Value -> Object
fields (Object value) = value
fields _ = error "Expected fixture object"

params :: Object -> Object
params = fields . field "params"

text :: Key -> Object -> Text
text key objectValue = case field key objectValue of String value -> value; _ -> error "Expected fixture string"

bounded :: IO a -> IO a
bounded action = timeout 10000000 action >>= maybe (assertFailure "State fixture timed out") pure
