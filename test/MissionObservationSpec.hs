{-# LANGUAGE OverloadedStrings #-}

module MissionObservationSpec (missionObservationTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (void, (>=>))
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, getCurrentTime, parseTimeM)
import Factory.Droid (DroidError (..), DroidEvent (..), DroidStreamMode (..))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control (GetRewindInfoParams (..), GetRewindInfoResult)
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))
import Factory.Droid.Schema.Mission
import Factory.Droid.Schema.Notifications (SessionTokenUsageChanged (..))
import Factory.Droid.Schema.Settings (settingsModel)
import Factory.Droid.Schema.Usage (TokenUsage (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import MissionEventSpec (withMissionPeerState)
import MissionStateSpec (missionSnapshotWire)
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

missionObservationTests :: TestTree
missionObservationTests =
  testGroup
    "Owned mission observation"
    [ testCase "routine usage and heartbeats do not create missions; scoped getters retire" $ bounded $ do
        let events = [usageEvent "saved" 2 (Just 100), heartbeat]
        later <- withMissionPeerState Nothing [] events $ \target -> Daemon.withResumedSession (options target) "saved" $ \session -> do
          Daemon.getMissionSnapshot session >>= (@?= Nothing)
          void (Daemon.sendEvents session CompleteMessages "mission-events" (\_ -> pure ()))
          Daemon.getMissionSnapshot session >>= (@?= Nothing)
          pure (Daemon.getMissionSnapshot session)
        try @DroidError later >>= (@?= Left DroidSessionUnusable),
      testCase "load baseline is followed by ordered updates rather than re-published over them" $ bounded $ do
        before <- getCurrentTime
        withMissionPeerState (Just missionSnapshotWire) [stateEvent "paused", object ["type" .= String "mission_worker_started", "workerSessionId" .= String "post-reply"]] [] $ \target ->
          Daemon.withResumedSession (options target) "saved" $ \session -> do
            void (Daemon.sendEvents session CompleteMessages "mission-events" (\_ -> pure ()))
            after <- getCurrentTime
            snapshot <- requireSnapshot session
            missionSnapshotState snapshot @?= MissionPaused
            missionSnapshotWorkers snapshot @?= ["worker-b", "worker-a", "post-reply"]
            missionSnapshotTitle snapshot @?= Just ""
            states <- maybe (assertFailure "Missing observed workers") pure (missionSnapshotWorkerStates snapshot)
            workerStartedAt <$> Map.lookup "worker-b" states @?= Just "reported start"
            fallback <- maybe (assertFailure "Missing fallback worker") pure (Map.lookup "worker-a" states)
            timestamp <- maybe (assertFailure "Invalid local UTC observation time") pure (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (Text.unpack (workerStartedAt fallback)) :: Maybe UTCTime)
            assertBool "fallback uses a receive-time sample within the attachment exchange" (timestamp >= addUTCTime (-0.001) before && timestamp <= after)
            loaded <- maybe (assertFailure "Missing immutable receipt") pure (Daemon.daemonLoadedState (Daemon.sessionInfo session))
            missionSnapshotState <$> loadedMissionSnapshot loaded @?= Just MissionRunning,
      testCase "callback getters include their mutation and restrict usage to owner or known workers" $ bounded $ do
        let events = [stateEvent "running", object ["type" .= String "mission_worker_started", "workerSessionId" .= String "known"], usageEvent "saved" 2 (Just 11), usageEvent "known" 3 (Just 7), usageEvent "saved" 4 (Just 0), usageEvent "unrelated" 5 (Just 900), heartbeat]
        withMissionPeerState Nothing [] events $ \target -> Daemon.withResumedSession (options target) "saved" $ \session -> do
          totals <- newIORef []
          done <- newEmptyMVar
          stop <- Daemon.onSessionEvent session $ \case
            Left cause -> assertFailure (show cause)
            Right event -> do
              snapshot <- requireSnapshot session
              case event of
                MissionStateEvent _ -> missionSnapshotState snapshot @?= MissionRunning
                MissionWorkerStartedEvent _ -> missionSnapshotWorkers snapshot @?= ["known"]
                UsageEvent usage -> modifyIORef' totals (<> [(sessionUsageId usage, usageInputTokens <$> missionSnapshotTokenUsage snapshot)])
                MissionHeartbeatEvent _ -> putMVar done ()
                _ -> pure ()
          void (query session "emit")
          takeMVar done
          readIORef totals >>= (@?= [("saved", Just 11), ("known", Just 18), ("saved", Just 7), ("unrelated", Just 7)])
          snapshot <- requireSnapshot session
          fmap Map.keys (missionSnapshotSessionUsage snapshot) @?= Just ["known", "saved"]
          stop,
      testCase "malformed mutation invalidates only mission view until a fresh full baseline" $ bounded $ do
        let events = [stateEvent "running", object ["type" .= String "mission_features_changed", "features" .= False], stateEvent "paused"]
        withMissionPeerState Nothing [] events $ \target -> Daemon.withResumedSession (options target) "saved" $ \session -> do
          done <- newEmptyMVar
          failures <- newIORef []
          stop <- Daemon.onSessionEvent session $ \case
            Left DroidInvalidEvent -> try @DroidError (Daemon.getMissionSnapshot session) >>= \result -> modifyIORef' failures (<> [result])
            Left cause -> assertFailure (show cause)
            Right (MissionStateEvent state) | changedMissionPhase state == MissionPaused -> do
              try @DroidError (Daemon.getMissionSnapshot session) >>= (@?= Left DroidInvalidEvent)
              putMVar done ()
            _ -> pure ()
          void (query session "emit")
          takeMVar done
          readIORef failures >>= (@?= [Left DroidInvalidEvent])
          Daemon.getSettings session >>= (@?= "offline-model") . settingsModel
          stop
        withMissionPeerState (Just missionSnapshotWire) [] [] $ \target -> Daemon.withResumedSession (options target) "saved" (requireSnapshot >=> ((@?= MissionRunning) . missionSnapshotState))
    ]

requireSnapshot :: Daemon.DaemonSession -> IO MissionSnapshot
requireSnapshot session = Daemon.getMissionSnapshot session >>= maybe (assertFailure "Expected observed mission") pure

query :: Daemon.DaemonSession -> Text -> IO GetRewindInfoResult
query session message = Daemon.getRewindInfo (Daemon.sessionConnection session) (GetRewindInfoParams (Daemon.sessionId session) message mempty)

stateEvent :: Text -> Value
stateEvent state = object ["type" .= String "mission_state_changed", "state" .= state]

heartbeat :: Value
heartbeat = object ["type" .= String "mission_heartbeat", "timestamp" .= String "opaque heartbeat"]

usageEvent :: Text -> Int -> Maybe Int -> Value
usageEvent identifier base inclusive = object (["type" .= String "session_token_usage_changed", "sessionId" .= identifier, "tokenUsage" .= usage base] <> maybe [] (\value -> ["inclusiveTokenUsage" .= usage value]) inclusive)
  where
    usage value = object ["inputTokens" .= value, "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "/remote") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}
