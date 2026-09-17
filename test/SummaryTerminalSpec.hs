{-# LANGUAGE OverloadedStrings #-}

module SummaryTerminalSpec (summaryTerminalTests) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, try)
import Control.Monad (forM_, void, when)
import Data.Aeson (Object, Value (..), object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as D
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Control (AddUserMessageParams (..), QueuePlacement (..), defaultUserMessageParams)
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Mission (SubagentInvocationSummary (..), SubagentStatus (..))
import Factory.Droid.Schema.Notifications (AgentTurnCompletionReason (..), DroidWorkingState (..), NotificationErrorType (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcInvalidParams))
import Factory.Droid.SessionState qualified as S
import Factory.Droid.Transport (objectTransport)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

summaryTerminalTests :: TestTree
summaryTerminalTests =
  testGroup "Terminal child summaries" $
    [ testCase (show reason) $ bounded $ withFixture True False [] True $ \connection peer -> do
        emitAndSettle connection peer (completed reason)
        D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = status}))
        lastReason connection >>= (@?= Just reason)
    | (reason, status) <- expectedStatuses
    ]
      <> [ testCase "existing untagged and tagged summaries both receive observed metrics" $ bounded $ forM_ [False, True] $ \tagged ->
             withFixture True tagged messages True $ \connection peer -> do
               emitAndSettle connection peer (completed TurnCompleted)
               D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = SubagentCompleted, invocationToolUseCount = Just 2, invocationDurationMs = Just 20})),
           testCase "nonempty zero-time history reports zero tools without inventing duration" $ bounded $ withFixture True False [message "zero" "user" 0 0 []] True $ \connection peer -> do
             emitAndSettle connection peer (completed TurnCompleted)
             D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = SubagentCompleted, invocationToolUseCount = Just 0})),
           testCase "no summary is manufactured and an unregistered summary is not completed" $ bounded $ do
             withFixture True False [] False $ \connection peer -> do
               emitAndSettle connection peer (completed TurnCancelled)
               D.getSubagentInvocationSummary connection "child" >>= (@?= Nothing)
               lastReason connection >>= (@?= Just TurnCancelled)
             withFixture False False [] True $ \connection peer -> do
               emitAndSettle connection peer (completed TurnCancelled)
               D.getSubagentInvocationSummary connection "child" >>= (@?= Just initialSummary)
               lastReason connection >>= (@?= Nothing),
           testCase "unexpected errors fail summaries but expected process self-exits preserve the reason" $
             bounded $
               forM_ errorCases $ \(events, status, reason) -> withFixture True False [] True $ \connection peer -> do
                 forM_ events (emitAndSettle connection peer)
                 D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = status}))
                 lastReason connection >>= (@?= Just reason),
           testCase "all nonidle observations clear stale completion while idle preserves it" $
             bounded $
               forM_ [minBound .. maxBound] $ \working -> withFixture True False [] True $ \connection peer -> do
                 emitAndSettle connection peer (completed TurnCompleted)
                 emitAndSettle connection peer (workingEvent working)
                 lastReason connection >>= (@?= if working == WorkingIdle then Just TurnCompleted else Nothing)
                 emitAndSettle connection peer (errorEvent NotifyProcessExitError)
                 D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = if working == WorkingIdle then SubagentCompleted else SubagentFailed})),
           testCase "active load hydration clears stale reason while an idle reload preserves it" $
             bounded $
               forM_ [WorkingIdle, WorkingThinking] $ \working -> withFixture True False [] True $ \connection peer -> do
                 emitAndSettle connection peer (completed TurnCompleted)
                 atomically (writeTVar (peerWorking peer) working)
                 void (D.loadSessionInfo connection "child")
                 lastReason connection >>= (@?= if working == WorkingIdle then Just TurnCompleted else Nothing)
                 emitAndSettle connection peer (errorEvent NotifyProcessExitError)
                 D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = if working == WorkingIdle then SubagentCompleted else SubagentFailed})),
           testCase "a live observation before load capture does not block a fresh snapshot" $ bounded $ withFixture True False [] True $ \connection peer -> do
             emitAndSettle connection peer (workingEvent WorkingIdle)
             emitAndSettle connection peer (completed TurnCompleted)
             atomically (writeTVar (peerWorking peer) WorkingThinking)
             void (D.loadSessionInfo connection "child")
             D.getSessionReadiness connection "child" >>= (@?= Right (Just WorkingThinking)) . D.readinessWorkingState
             lastReason connection >>= (@?= Nothing),
           testCase "a late busy load cannot erase a newer same-valued idle observation and completion" $ bounded $ withFixture True False [] True $ \connection peer -> do
             emitAndSettle connection peer (completed TurnCompleted)
             atomically (writeTVar (peerHoldLoads peer) True)
             withAsync (D.loadSessionInfo connection "child") $ \pending -> do
               frame <- atomically (readTQueue (peerHeldLoads peer))
               emitAndSettle connection peer (workingEvent WorkingIdle)
               emitAndSettle connection peer (completed TurnCancelled)
               respond peer frame (object ["sessionId" .= String "child", "session" .= object ["messages" .= [message "receipt" "user" 2 2 []]], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"], "workingState" .= WorkingThinking])
               void (wait pending)
             D.getSessionReadiness connection "child" >>= (@?= Right (Just WorkingIdle)) . D.readinessWorkingState
             D.getSessionState connection "child" >>= (@?= ["receipt"]) . Map.keys . S.sessionMessagesById
             lastReason connection >>= (@?= Just TurnCancelled)
             emitAndSettle connection peer (errorEvent NotifyProcessExitError)
             D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = SubagentCancelled})),
           testCase "a late idle load cannot erase a newer busy working-state notification" $ bounded $ withFixture True False [] True $ \connection peer -> do
             atomically (writeTVar (peerHoldLoads peer) True)
             withAsync (D.loadSessionInfo connection "child") $ \pending -> do
               frame <- atomically (readTQueue (peerHeldLoads peer))
               emitAndSettle connection peer (workingEvent WorkingStreamingAssistantMessage)
               respond peer frame (object ["sessionId" .= String "child", "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"], "workingState" .= WorkingIdle])
               void (wait pending)
             D.getSessionReadiness connection "child" >>= (@?= Right (Just WorkingStreamingAssistantMessage)) . D.readinessWorkingState,
           testCase "availability reopens terminal status and only a seeded nonidle state clears the reason" $
             bounded $
               forM_ [False, True] $ \notLoaded -> withFixture True False [] True $ \connection peer -> do
                 emitAndSettle connection peer (completed TurnCancelled)
                 when notLoaded (D.markSessionNotLoaded connection "child")
                 notify peer "parent" availability
                 settle connection
                 D.getSubagentInvocationSummary connection "child" >>= (@?= Just initialSummary)
                 lastReason connection >>= (@?= if notLoaded then Nothing else Just TurnCancelled),
           testCase "permission resumption clears a waiting session reason but not an idle one" $
             bounded $
               forM_ [WorkingIdle, WorkingWaitingForToolConfirmation] $ \working -> withFixture True False [] True $ \connection peer -> do
                 emitAndSettle connection peer (workingEvent working)
                 emitAndSettle connection peer (completed TurnCompleted)
                 emitAndSettle connection peer permissionResolved
                 lastReason connection >>= (@?= if working == WorkingIdle then Just TurnCompleted else Nothing),
           testCase "accepted immediate submissions clear a prior reason while skipped queued and rejected sends do not" $
             bounded $
               forM_ [(False, Nothing, False, Nothing), (True, Nothing, False, Just TurnCompleted), (False, Just QueueEndOfLoop, False, Just TurnCompleted), (False, Nothing, True, Just TurnCompleted)] $ \(skip, placement, rejected, expected) ->
                 withFixture True False [] True $ \connection peer -> do
                   emitAndSettle connection peer (completed TurnCompleted)
                   let params = (defaultUserMessageParams "next") {userMessageSkipAgentLoop = Just skip, userMessageQueuePlacement = placement}
                   withAsync (try @RpcResultError (D.submitUserMessage connection "child" "request" params)) $ \pending -> do
                     frame <- atomically (readTQueue (peerSubmissions peer))
                     if rejected then reject peer frame else respond peer frame (object [])
                     wait pending >>= \case
                       Left (RpcRemoteFailure _) | rejected -> pure ()
                       Right _ | not rejected -> pure ()
                       _ -> assertFailure "Unexpected submission outcome"
                   lastReason connection >>= (@?= expected),
           testCase "an explicit message echo protects a completion observed before its request ACK" $ bounded $ withFixture True False [] True $ \connection peer -> do
             emitAndSettle connection peer (completed TurnCompleted)
             withAsync (D.submitUserMessage connection "child" "request" (defaultUserMessageParams "next")) $ \pending -> do
               frame <- atomically (readTQueue (peerSubmissions peer))
               emitAndSettle connection peer (object ["type" .= String "create_message", "requestId" .= String "request", "message" .= message "echo" "user" 1 1 []])
               emitAndSettle connection peer (completed TurnCancelled)
               respond peer frame (object [])
               void (wait pending)
             lastReason connection >>= (@?= Just TurnCancelled),
           testCase "the attached-session backend shares immediate-turn reason clearing" $ bounded $ withFixture True False [] True $ \connection peer ->
             D.withResumedSessionOn connection "child" $ \session -> do
               emitAndSettle connection peer (completed TurnCompleted)
               withAsync (D.sendPrompt session "next" (const (pure ()))) $ \pending -> do
                 frame <- atomically (readTQueue (peerSubmissions peer))
                 respond peer frame (object [])
                 waitReason connection Nothing
                 case field "params" frame of
                   Object params -> emitAndSettle connection peer (object ["type" .= String "agent_turn_completed", "reason" .= TurnCompleted, "turnId" .= field "messageId" params, "tokenUsage" .= usage])
                   _ -> assertFailure "Missing submission parameters"
                 void (wait pending)
               lastReason connection >>= (@?= Just TurnCompleted),
           testCase "terminal observations participate in summary freshness but ignored self-exits do not" $ bounded $ withFixture True False [] True $ \connection peer -> do
             let shared = D.connectionState connection
             before <- D.captureSubagentInvocationSummaryRevision shared
             emitAndSettle connection peer (completed TurnCancelled)
             D.hydrateSubagentInvocationSummariesWithRevision shared before [initialSummary]
             D.getSubagentInvocationSummary connection "child" >>= (@?= Just (initialSummary {invocationStatus = SubagentCancelled}))
             after <- D.captureSubagentInvocationSummaryRevision shared
             emitAndSettle connection peer (errorEvent NotifyProcessExitError)
             D.captureSubagentInvocationSummaryRevision shared >>= (@?= after)
             emitAndSettle connection peer (errorEvent NotifySessionError)
             failed <- D.captureSubagentInvocationSummaryRevision shared
             emitAndSettle connection peer (errorEvent NotifyProcessExitError)
             repeated <- D.captureSubagentInvocationSummaryRevision shared
             assertBool "An accepted equal failure must advance freshness" (failed /= repeated),
           testCase "malformed terminal data does not synthesize a reason or task status" $ bounded $ withFixture True False [] True $ \connection peer -> do
             emitAndSettle connection peer (object ["type" .= String "agent_turn_completed", "reason" .= String "not-a-reason", "tokenUsage" .= usage])
             lastReason connection >>= (@?= Nothing)
             D.getSubagentInvocationSummary connection "child" >>= (@?= Just initialSummary),
           testCase "pure metric refresh keeps its separate status contract and cache retirement clears only the reason view" $ do
             let before = S.setSessionTurnCompletionReason (Just TurnCompleted) (S.setInvocationSummary initialSummary S.emptySessionState)
                 refreshed = S.refreshInvocationSummary before
                 retired = S.clearCachedSessionState refreshed
             S.sessionInvocationSummary refreshed @?= Just initialSummary
             S.sessionTurnCompletionReason retired @?= Nothing
             S.sessionInvocationSummary retired @?= Just initialSummary
         ]

expectedStatuses :: [(AgentTurnCompletionReason, SubagentStatus)]
expectedStatuses =
  [ (TurnCompleted, SubagentCompleted),
    (TurnCancelled, SubagentCancelled),
    (TurnPermissionRejected, SubagentFailed),
    (TurnError, SubagentFailed),
    (TurnProcessExit, SubagentCancelled),
    (TurnSpecHandoff, SubagentCompleted),
    (TurnStructuredOutputMissing, SubagentFailed),
    (TurnStructuredOutputInvalid, SubagentFailed),
    (TurnStructuredOutputSchemaInvalid, SubagentFailed),
    (TurnModelUsageExhausted, SubagentFailed),
    (TurnModelAuthenticationFailed, SubagentFailed),
    (TurnModelRequestRejected, SubagentFailed),
    (TurnModelProviderUnreachable, SubagentFailed),
    (TurnModelProviderUnavailable, SubagentFailed),
    (TurnModelRateLimited, SubagentFailed),
    (TurnPromptRejected, SubagentFailed),
    (TurnCompletionPersistenceFailed, SubagentFailed),
    (TurnNoApproverAvailable, SubagentFailed)
  ]

errorCases :: [([Value], SubagentStatus, AgentTurnCompletionReason)]
errorCases =
  [ ([errorEvent NotifySessionError], SubagentFailed, TurnError),
    ([errorEvent NotifyProcessExitError], SubagentFailed, TurnError),
    ([completed TurnCompleted, errorEvent NotifyProcessExitError], SubagentCompleted, TurnCompleted),
    ([completed TurnCancelled, errorEvent NotifyProcessExitError], SubagentCancelled, TurnCancelled),
    ([completed TurnError, errorEvent NotifyProcessExitError], SubagentFailed, TurnError),
    ([completed TurnCompleted, errorEvent NotifySessionError], SubagentFailed, TurnError)
  ]

initialSummary :: SubagentInvocationSummary
initialSummary = SubagentInvocationSummary "child" SubagentRunning "worker" "fixture" (Just 99) (Just 999) mempty

data Peer = Peer
  { peerIncoming :: TQueue Object,
    peerSubmissions :: TQueue Object,
    peerWorking :: TVar DroidWorkingState,
    peerHeldLoads :: TQueue Object,
    peerHoldLoads :: TVar Bool
  }

withFixture :: Bool -> Bool -> [Value] -> Bool -> (D.DaemonConnection -> Peer -> IO a) -> IO a
withFixture managed tagged history hasSummary action = do
  peer <- Peer <$> newTQueueIO <*> newTQueueIO <*> newTVarIO WorkingIdle <*> newTQueueIO <*> newTVarIO False
  let send frame = case field "method" frame of
        String "daemon.load_session" -> do
          held <- readTVarIO (peerHoldLoads peer)
          if held
            then atomically (writeTQueue (peerHeldLoads peer) frame)
            else do
              working <- readTVarIO (peerWorking peer)
              respond peer frame (object ["sessionId" .= String "child", "session" .= object ["messages" .= history], "settings" .= object (["modelId" .= String "fixture", "reasoningEffort" .= String "low"] <> ["tags" .= [object ["name" .= String "subagent"]] | tagged]), "workingState" .= working])
        String "daemon.get_proxy_token" -> respond peer frame (object ["token" .= String "OFFLINE_ONLY"])
        String "daemon.add_user_message" -> atomically (writeTQueue (peerSubmissions peer) frame)
        _ -> assertFailure "Unexpected terminal-summary fixture request"
      options = (D.defaultDaemonClientOptions (D.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline") {D.daemonClientRestoreTerminalsOnLoad = False, D.daemonClientHydrateChildSessions = False}
  D.withConnectionOn options (objectTransport send (atomically (readTQueue (peerIncoming peer)))) $ \connection -> do
    when managed (void (D.loadSessionInfo connection "child"))
    when hasSummary (D.setSubagentInvocationSummary connection initialSummary)
    action connection peer

respond :: Peer -> Object -> Value -> IO ()
respond peer frame value = case field "id" frame of
  String identifier -> atomically (writeTQueue (peerIncoming peer) (reply identifier value))
  _ -> assertFailure "Missing request ID"

reject :: Peer -> Object -> IO ()
reject peer frame = case field "id" frame of
  String identifier -> atomically (writeTQueue (peerIncoming peer) (KeyMap.insert "error" (object ["code" .= RpcInvalidParams, "message" .= String "fixture"]) (KeyMap.delete "result" (reply identifier Null))))
  _ -> assertFailure "Missing request ID"

notify :: Peer -> Text -> Value -> IO ()
notify peer identifier notification = atomically (writeTQueue (peerIncoming peer) (KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= notification]]))

settle :: D.DaemonConnection -> IO ()
settle connection = do
  ready <- newEmptyTMVarIO
  bracket (D.onRequestSettled connection (const (atomically (void (tryPutTMVar ready ()))))) id $ \_ -> do
    void (D.getProxyToken connection)
    atomically (readTMVar ready)

emitAndSettle :: D.DaemonConnection -> Peer -> Value -> IO ()
emitAndSettle connection peer event = notify peer "child" event >> settle connection

lastReason :: D.DaemonConnection -> IO (Maybe AgentTurnCompletionReason)
lastReason connection = S.sessionTurnCompletionReason <$> D.getSessionState connection "child"

waitReason :: D.DaemonConnection -> Maybe AgentTurnCompletionReason -> IO ()
waitReason connection expected = atomically $ do
  snapshot <- D.readDaemonStateSnapshot (D.connectionState connection)
  check ((Map.lookup "child" (D.daemonStateSessions snapshot) >>= S.sessionTurnCompletionReason) == expected)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

completed :: AgentTurnCompletionReason -> Value
completed reason = object ["type" .= String "agent_turn_completed", "reason" .= reason, "turnId" .= String "turn", "tokenUsage" .= usage]

errorEvent :: NotificationErrorType -> Value
errorEvent kind = object ["type" .= String "error", "message" .= String "fixture", "errorType" .= kind, "timestamp" .= String "fixture"]

workingEvent :: DroidWorkingState -> Value
workingEvent working = object ["type" .= String "droid_working_state_changed", "newState" .= working]

usage, permissionResolved, availability :: Value
usage = object ["inputTokens" .= (0 :: Int), "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]
permissionResolved = object ["type" .= String "permission_resolved", "requestId" .= String "permission", "toolUseIds" .= ([] :: [Text]), "selectedOption" .= String "proceed_once"]
availability = object ["type" .= String "child_session_available", "childSessionId" .= String "child", "timestamp" .= (0 :: Int), "subagentType" .= String "worker", "description" .= String "fixture"]

messages :: [Value]
messages = [message "user" "user" 10 0 [], message "assistant" "assistant" 20 30 [tool "one", tool "two"]]

message :: Text -> Text -> Int -> Int -> [Value] -> Value
message identifier role created updated content = object ["id" .= identifier, "role" .= role, "createdAt" .= created, "updatedAt" .= updated, "content" .= content]

tool :: Text -> Value
tool identifier = object ["type" .= String "tool_use", "id" .= identifier, "name" .= String "Read", "input" .= object []]
