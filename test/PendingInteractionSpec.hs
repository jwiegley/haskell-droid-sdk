{-# LANGUAGE OverloadedStrings #-}

module PendingInteractionSpec (pendingInteractionTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (bracket, catch, finally, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Object, Result (..), ToJSON (toJSON), Value (..), eitherDecode, encode, fromJSON, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidSessionUnusable))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Interaction
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.Notifications (ToolConfirmationOutcome (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcInvalidParams))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

pendingInteractionTests :: TestTree
pendingInteractionTests =
  testGroup
    "Managed pending interactions"
    [ testCase "unconfigured requests still cancel without manual mode" $ bounded $ do
        void $ withPendingPeer [[permission "p" "worker" ["owner"], question "q" "worker"]] [] False $ \connection replies -> do
          trigger connection
          responses <- sequence [atomically (readTQueue replies), atomically (readTQueue replies)]
          map (field "result") (ordered responses) @?= [responseValue "worker" cancelPermissionResult, responseValue "worker" cancelDroidQuestions]
          getPendingSnapshot (Daemon.pendingInteractions connection) >>= (@?= PendingSnapshot [] []),
      testCase "manual permission and question responses preserve execution scope and validate their request" $ bounded $ do
        void $ withPendingPeer [[permission "p" "worker" ["owner", "worker", "owner"], question "q" "worker"]] [] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            snapshot <- awaitSnapshot controller (\view -> length (pendingPermissions view) == 1 && length (pendingQuestions view) == 1)
            let p = firstPermission snapshot
                q = case pendingQuestions snapshot of value : _ -> value; [] -> error "Missing fixture question"
            pendingAssociatedSessionIds p @?= ["worker", "owner"]
            getPendingPermissionsForSession controller "owner" >>= (@?= [p])
            try @PendingInteractionError (respondPendingPermission controller "foreign" (pendingInteractionId p) editResponse) >>= (@?= Left PendingWrongSession)
            try @PendingInteractionError (respondPendingQuestion controller "owner" (pendingInteractionId q) answers) >>= (@?= Left PendingWrongSession)
            let invalid = AskUserResult [AskUserCollectedAnswer 99 "wrong" "answer" mempty] (Just False) mempty
            try @PendingInteractionError (respondPendingQuestion controller "worker" (pendingInteractionId q) invalid) >>= (@?= Left PendingInvalidResponse)
            respondPendingPermission controller "owner" (pendingInteractionId p) editResponse
            respondPendingQuestion controller "worker" (pendingInteractionId q) answers
            responses <- sequence [atomically (readTQueue replies), atomically (readTQueue replies)]
            map (field "result") (ordered responses) @?= [responseValue "worker" editResponse, responseValue "worker" answers],
      testCase "manual scope exit cancels its pending requests and restores default policy" $ bounded $ do
        void $ withPendingPeer [[permission "first" "worker" []], [permission "later" "worker" []]] [] False $ \connection replies -> do
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            void (awaitSnapshot controller (not . null . pendingPermissions))
          atomically (readTQueue replies) >>= (@?= responseValue "worker" cancelPermissionResult) . field "result"
          trigger connection
          atomically (readTQueue replies) >>= (@?= responseValue "worker" cancelPermissionResult) . field "result"
          Daemon.isAuthenticated connection >>= (@?= True)
          Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True True),
      testCase "subscriptions deduplicate one registration across surfaces and isolate ordinary subscriber failures" $ bounded $ do
        void $ withPendingPeer [[permission "p1" "worker" ["owner", "owner"]], [permission "p2" "worker" ["owner"]]] [] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            seen <- newIORef (0 :: Int)
            _ <- onPendingPermissions controller Nothing (\_ -> ioError (userError "subscriber failed"))
            stop <- onPendingPermissions controller (Just ["owner", "worker", "owner"]) (\_ -> modifyIORef' seen (+ 1))
            trigger connection
            p <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
            respondPendingPermission controller "owner" (pendingInteractionId p) cancelPermissionResult
            void (atomically (readTQueue replies))
            readIORef seen >>= (@?= 1)
            stop
            stop
            trigger connection
            p2 <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
            respondPendingPermission controller "owner" (pendingInteractionId p2) cancelPermissionResult
            void (atomically (readTQueue replies))
            readIORef seen >>= (@?= 1),
      testCase "live and restored association replays refresh the view without a second admission" $
        bounded $
          forM_ [False, True] $ \fromLoad -> do
            let replay = permission "p" "owner" ["viewer"]
            void $ withPendingPeer [[permission "p" "owner" ["caller"]], [replay]] [replay] False $ \connection replies ->
              Daemon.withPendingInteractions connection $ \controller -> do
                requests <- newIORef (0 :: Int)
                _ <- onPendingPermissions controller Nothing (\_ -> modifyIORef' requests (+ 1))
                trigger connection
                original <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
                if fromLoad then void (Daemon.loadSessionInfo connection "owner") else trigger connection
                updated <- firstPermission <$> awaitSnapshot controller (any (elem "viewer" . pendingAssociatedSessionIds) . pendingPermissions)
                pendingInteractionId updated @?= pendingInteractionId original
                pendingAssociatedSessionIds updated @?= ["owner", "caller", "viewer"]
                respondPendingPermission controller "viewer" (pendingInteractionId updated) editResponse
                atomically (readTQueue replies) >>= (@?= responseValue "owner" editResponse) . field "result"
                readIORef requests >>= (@?= 1),
      testCase "conflicting live and restored replays cancel an undecided request" $
        bounded $
          forM_ [False, True] $ \fromLoad -> do
            let changed = (permissionParams []) {permissionOptions = [ToolConfirmationListItem "Cancel" ConfirmCancel mempty]}
                replay = IncomingPermission "p" "owner" changed
                marker = IncomingNotice "owner" (object ["type" .= String "session_working_directory_changed", "cwd" .= String "/fixture/intake-complete"])
            void $ withPendingPeer [[permission "p" "owner" []], [replay, marker]] [replay, marker] False $ \connection replies ->
              Daemon.withPendingInteractions connection $ \controller -> do
                started <- newEmptyMVar
                release <- newEmptyMVar
                _ <- onPendingPermissions controller Nothing (\_ -> putMVar started () >> takeMVar release)
                trigger connection
                takeMVar started
                pending <- firstPermission <$> getPendingSnapshot controller
                ( do
                    if fromLoad then void (Daemon.loadSessionInfo connection "owner") else trigger connection
                    let awaitIntake = do
                          snapshot <- Daemon.getSessionState connection "owner"
                          directory <- Daemon.getWorkingDirectory connection "owner"
                          if directory == Just "/fixture/intake-complete" then pure () else Daemon.waitSessionStateChange connection "owner" snapshot >> awaitIntake
                    awaitIntake
                    try @PendingInteractionError (respondPendingPermission controller "owner" (pendingInteractionId pending) editResponse) >>= (@?= Left PendingAlreadyAnswered)
                  )
                  `finally` putMVar release ()
                atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelPermissionResult) . field "result",
      testCase "external permission fanout grants no local response authority" $ bounded $ do
        void $ withPendingPeer [[permission "p" "worker" ["owner"]]] [] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \origin ->
            bracket newPendingInteractions (atomically . closePendingInteractions) $ \other -> do
              observed <- newEmptyMVar
              _ <- onPendingPermissions other (Just ["owner"]) (putMVar observed)
              trigger connection
              p <- firstPermission <$> awaitSnapshot origin (not . null . pendingPermissions)
              dispatchExternalPermissionRequest other p
              takeMVar observed >>= (@?= p)
              getPendingSnapshot other >>= (@?= PendingSnapshot [] [])
              try @PendingInteractionError (respondPendingPermission other "owner" (pendingInteractionId p) editResponse) >>= (@?= Left PendingRequestMissing)
              respondPendingPermission origin "owner" (pendingInteractionId p) editResponse
              atomically (readTQueue replies) >>= (@?= responseValue "worker" editResponse) . field "result",
      testCase "peer permission resolution retires a wait without a duplicate local response" $ bounded $ do
        let resolved = IncomingNotice "owner" (object ["type" .= String "permission_resolved", "requestId" .= String "p", "toolUseIds" .= ([] :: [Text]), "selectedOption" .= ConfirmCancel])
        (_, trace) <- withPendingPeer [[permission "p" "worker" ["owner"]], [resolved]] [] False $ \connection _ ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            void (awaitSnapshot controller (not . null . pendingPermissions))
            trigger connection
            void (awaitSnapshot controller (== PendingSnapshot [] []))
        [frame | frame <- trace, field "type" frame == String "response"] @?= [],
      testCase "binding close cannot replace an already-observed permission resolution with cancellation" $ bounded $ do
        let resolved = IncomingNotice "owner" (object ["type" .= String "permission_resolved", "requestId" .= String "p", "toolUseIds" .= ([] :: [Text]), "selectedOption" .= ConfirmCancel])
            closed = IncomingNotice "owner" (object ["type" .= String "session_closed"])
        void $ withPendingPeer [[permission "p" "owner" []], [resolved, closed]] [] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller ->
            Daemon.withResumedSessionOn connection "owner" $ \session -> do
              started <- newEmptyMVar
              hold <- newEmptyMVar
              ended <- newEmptyMVar
              _ <- onPendingPermissions controller Nothing (\_ -> (putMVar started () >> takeMVar hold) `finally` putMVar ended ())
              trigger connection
              takeMVar started
              trigger connection
              takeMVar ended
              Daemon.detachSession session
              timeout 100000 (atomically (readTQueue replies)) >>= (@?= Nothing),
      testCase "same-batch permission resolution cannot overtake pending registration" $ bounded $ do
        let resolved = IncomingNotice "owner" (object ["type" .= String "permission_resolved", "requestId" .= String "p", "toolUseIds" .= ([] :: [Text]), "selectedOption" .= ConfirmCancel])
        (_, trace) <- withPendingPeer [[permission "p" "worker" ["owner"], resolved]] [] False $ \connection _ ->
          Daemon.withPendingInteractions connection $ \controller -> do
            registered <- newEmptyMVar
            _ <- onPendingPermissions controller Nothing (const (putMVar registered ()))
            trigger connection
            takeMVar registered
            void (awaitSnapshot controller (== PendingSnapshot [] []))
        [frame | frame <- trace, field "type" frame == String "response"] @?= [],
      testCase "restored requests use the manual policy before handle publication" $ bounded $ do
        (_, trace) <- withPendingPeer [] [permission "restored" "owner" ["viewer"]] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller ->
            Daemon.withResumedSessionOn connection "owner" $ \_ -> do
              p <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
              pendingSessionId p @?= "owner"
              respondPendingPermission controller "viewer" (pendingInteractionId p) editResponse
              atomically (readTQueue replies) >>= (@?= responseValue "owner" editResponse) . field "result"
        [field "autoRejectPermissionRequests" (asObject (field "params" frame)) | frame <- trace, field "method" frame == String "daemon.load_session"] @?= [Bool False],
      testCase "resolution immediately after a load receipt cannot revive a restored permission" $ bounded $ do
        let resolved = IncomingNotice "owner" (object ["type" .= String "permission_resolved", "requestId" .= String "restored", "toolUseIds" .= ([] :: [Text]), "selectedOption" .= ConfirmCancel])
        (_, trace) <- withPendingPeer [] [permission "restored" "owner" [], resolved] False $ \connection _ ->
          Daemon.withPendingInteractions connection $ \controller -> do
            void (Daemon.loadSessionInfo connection "owner")
            void (awaitSnapshot controller (== PendingSnapshot [] []))
        [frame | frame <- trace, field "type" frame == String "response"] @?= [],
      testCase "permission resolution cannot suppress a restored question with a reused ID" $
        bounded $
          forM_ [False, True] $ \beforeLoad -> do
            let resolved = IncomingNotice "owner" (object ["type" .= String "permission_resolved", "requestId" .= String "shared", "toolUseIds" .= ([] :: [Text]), "selectedOption" .= ConfirmCancel])
                notices = [[resolved] | beforeLoad]
                restored = [question "shared" "owner"] <> [resolved | not beforeLoad]
            void $ withPendingPeer notices restored False $ \connection replies ->
              Daemon.withPendingInteractions connection $ \controller -> do
                when beforeLoad (trigger connection)
                void (Daemon.loadSessionInfo connection "owner")
                snapshot <- awaitSnapshot controller (not . null . pendingQuestions)
                let q = case pendingQuestions snapshot of value : _ -> value; [] -> error "Missing restored question"
                respondPendingQuestion controller "owner" (pendingInteractionId q) answers
                atomically (readTQueue replies) >>= (@?= responseValue "owner" answers) . field "result",
      testCase "completed live decisions cannot be replayed by a later load snapshot" $ bounded $ do
        let original = [permission "p" "owner" [], question "q" "owner"]
        void $ withPendingPeer [original, []] original False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            snapshot <- awaitSnapshot controller (\view -> length (pendingPermissions view) == 1 && length (pendingQuestions view) == 1)
            let p = firstPermission snapshot
                q = case pendingQuestions snapshot of value : _ -> value; [] -> error "Missing question"
            respondPendingPermission controller "owner" (pendingInteractionId p) editResponse
            respondPendingQuestion controller "owner" (pendingInteractionId q) answers
            sequence_ [atomically (readTQueue replies), atomically (readTQueue replies)]
            void (awaitSnapshot controller (== PendingSnapshot [] []))
            trigger connection
            void (Daemon.loadSessionInfo connection "owner")
            void (awaitSnapshot controller (== PendingSnapshot [] [])),
      testCase "restoration waits for retiring inactive ownership before admitting its replacement" $ bounded $ do
        let inactive = IncomingNotice "owner" (object ["type" .= String "session_inactivity"])
        void $ withPendingPeer [[inactive]] [permission "p" "owner" []] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            started <- newEmptyMVar
            hold <- newEmptyMVar
            retiring <- newEmptyMVar
            release <- newEmptyMVar
            let handler _ = (putMVar started () >> takeMVar hold >> pure editResponse) `finally` (putMVar retiring () >> takeMVar release)
            Daemon.withResumedSessionOnHandlers connection (defaultDroidHandlers {onDroidPermission = Just handler}) "owner" $ \_ -> do
              takeMVar started
              trigger connection
              takeMVar retiring
              withAsync (Daemon.loadSessionInfo connection "owner") $ \loading -> do
                premature <- timeout 100000 (wait loading) `finally` putMVar release ()
                case premature of Just _ -> assertFailure "Load dropped the retiring request's replacement"; Nothing -> pure ()
                void (wait loading)
              fresh <- firstPermission <$> awaitSnapshot controller (not . all pendingInactive . pendingPermissions)
              respondPendingPermission controller "owner" (pendingInteractionId fresh) cancelPermissionResult
              atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelPermissionResult) . field "result",
      testCase "handler replacement affects later requests while an admitted callback keeps its snapshot" $ bounded $ do
        void $ withPendingPeer [[permission "p1" "owner" []], [permission "p2" "owner" []]] [] False $ \connection replies -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          let initial = defaultDroidHandlers {onDroidPermission = Just (\_ -> putMVar started () >> takeMVar release >> pure editResponse)}
          Daemon.withResumedSessionOnHandlers connection initial "owner" $ \session -> do
            trigger connection
            takeMVar started
            Daemon.setSessionHandlers session defaultDroidHandlers
            putMVar release ()
            atomically (readTQueue replies) >>= (@?= responseValue "owner" editResponse) . field "result"
            trigger connection
            atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelPermissionResult) . field "result"
            Daemon.detachSession session
            try @DroidError (Daemon.setSessionHandlers session initial) >>= (@?= Left DroidSessionUnusable),
      testCase "ending manual mode does not cancel an older automatic request" $ bounded $ do
        void $ withPendingPeer [[permission "old" "owner" []], [question "manual" "owner"]] [] False $ \connection replies -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          let initial = defaultDroidHandlers {onDroidPermission = Just (\_ -> putMVar started () >> takeMVar release >> pure editResponse)}
          Daemon.withResumedSessionOnHandlers connection initial "owner" $ \_ -> do
            trigger connection
            takeMVar started
            Daemon.withPendingInteractions connection $ \controller -> do
              trigger connection
              void (awaitSnapshot controller (not . null . pendingQuestions))
            atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelDroidQuestions) . field "result"
            getPendingPermissions (Daemon.pendingInteractions connection) >>= (@?= ["old"]) . map pendingRequestId
            putMVar release ()
            atomically (readTQueue replies) >>= (@?= responseValue "owner" editResponse) . field "result",
      testCase "logout clears pending authority and deferred actions before returning" $ bounded $ do
        void $ withPendingPeer [[permission "pending" "owner" []], [permission "after" "owner" []]] [] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            void (awaitSnapshot controller (not . null . pendingPermissions))
            Daemon.storeDeferredPermission connection "owner" (State.DeferredUserAction "pending" "tool" editResponse 0 [])
            Daemon.storeDeferredQuestion connection "owner" (State.DeferredUserAction "q" "question-tool" answers 0 [])
            void (Daemon.logout connection)
            Daemon.isAuthenticated connection >>= (@?= False)
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True False)
            getPendingSnapshot controller >>= (@?= PendingSnapshot [] [])
            state <- Daemon.getSessionState connection "owner"
            State.sessionDeferredPermissions state @?= mempty
            State.sessionDeferredQuestions state @?= mempty
            try @Daemon.DaemonError (Daemon.storeDeferredPermission connection "owner" (State.DeferredUserAction "p" "tool" editResponse 0 [])) >>= (@?= Left Daemon.DaemonUnauthenticated)
            atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelPermissionResult) . field "result"
            trigger connection
            atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelPermissionResult) . field "result",
      testCase "rejected logout preserves authentication, pending requests and deferred data" $ bounded $ do
        void $ withPendingPeer [[permission "p" "owner" []]] [] True $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            p <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
            Daemon.storeDeferredPermission connection "owner" (State.DeferredUserAction "p" "tool" editResponse 0 [])
            try @RpcResultError (Daemon.logout connection) >>= \case Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Logout rejection lost"
            Daemon.isAuthenticated connection >>= (@?= True)
            Daemon.getConnectionHealth connection >>= (@?= Daemon.ConnectionHealth True True)
            Daemon.getSessionState connection "owner" >>= (@?= 1) . Map.size . State.sessionDeferredPermissions
            respondPendingPermission controller "owner" (pendingInteractionId p) editResponse
            atomically (readTQueue replies) >>= (@?= responseValue "owner" editResponse) . field "result",
      testCase "stale opaque tokens cannot answer a replacement with the same wire request ID" $ bounded $ do
        void $ withPendingPeer [[permission "same-id" "owner" []], [permission "same-id" "owner" []]] [] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            first <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
            respondPendingPermission controller "owner" (pendingInteractionId first) cancelPermissionResult
            void (atomically (readTQueue replies))
            trigger connection
            second <- firstPermission <$> awaitSnapshot controller (not . null . pendingPermissions)
            try @PendingInteractionError (respondPendingPermission controller "owner" (pendingInteractionId first) editResponse) >>= (@?= Left PendingRequestMissing)
            respondPendingPermission controller "owner" (pendingInteractionId second) editResponse
            atomically (readTQueue replies) >>= (@?= responseValue "owner" editResponse) . field "result",
      testCase "inactive decisions are retained by session and require explicit submission to fresh requests" $ bounded $ do
        let inactive = IncomingNotice "worker" (object ["type" .= String "session_inactivity"])
            resolved = IncomingNotice "owner" (object ["type" .= String "permission_resolved", "requestId" .= String "new-p", "toolUseIds" .= [String "tool-id"], "selectedOption" .= ConfirmProceedEdit])
        (_, trace) <- withPendingPeer [[permission "old-p" "worker" ["owner"], question "old-q" "worker"], [inactive], [resolved]] [permission "new-p" "worker" ["owner"], question "new-q" "worker"] False $ \connection replies ->
          Daemon.withPendingInteractions connection $ \controller -> do
            trigger connection
            initial <- awaitSnapshot controller (\view -> length (pendingPermissions view) == 1 && length (pendingQuestions view) == 1)
            let p = firstPermission initial
            try @PendingInteractionError (Daemon.deferPermissionResponse connection "owner" (pendingInteractionId p) editResponse) >>= (@?= Left PendingCannotDefer)
            Daemon.getSessionReadiness connection "owner" >>= (@?= Right True) . Daemon.sessionReadinessBusy
            trigger connection
            held <- awaitSnapshot controller (\view -> length (pendingPermissions view) == 1 && length (pendingQuestions view) == 1 && all pendingInactive (pendingPermissions view) && all pendingInactive (pendingQuestions view))
            let q = case pendingQuestions held of value : _ -> value; [] -> error "Missing inactive question"
            try @PendingInteractionError (respondPendingPermission controller "owner" (pendingInteractionId p) editResponse) >>= (@?= Left PendingRequestInactive)
            Daemon.deferPermissionResponse connection "owner" (pendingInteractionId p) editResponse
            Daemon.deferQuestionResponse connection "worker" (pendingInteractionId q) answers
            getPendingSnapshot controller >>= (@?= PendingSnapshot [] [])
            state <- Daemon.getSessionState connection "worker"
            let savedPermission = fromMaybe (error "Missing deferred permission") (Map.lookup "tool-id" (State.sessionDeferredPermissions state))
                savedQuestion = fromMaybe (error "Missing deferred question") (Map.lookup "question-tool" (State.sessionDeferredQuestions state))
            State.deferredActionRequestId savedPermission @?= "old-p"
            State.deferredActionAssociatedSessionIds savedPermission @?= ["worker", "owner"]
            Daemon.storeDeferredPermission connection "foreign" (State.DeferredUserAction "old-p" "tool-id" editResponse 0 [])
            State.deferredActionRequestId savedQuestion @?= "old-q"
            atomically (tryReadTQueue replies) >>= (@?= Nothing)
            void (Daemon.loadSessionInfo connection "worker")
            restored <- awaitSnapshot controller (\view -> length (pendingPermissions view) == 1 && length (pendingQuestions view) == 1)
            let freshPermission = firstPermission restored
                freshQuestion = case pendingQuestions restored of value : _ -> value; [] -> error "Missing restored question"
            atomically (tryReadTQueue replies) >>= (@?= Nothing)
            respondPendingPermission controller "owner" (pendingInteractionId freshPermission) (State.deferredActionResponse savedPermission)
            respondPendingQuestion controller "worker" (pendingInteractionId freshQuestion) (State.deferredActionResponse savedQuestion)
            sequence_ [atomically (readTQueue replies), atomically (readTQueue replies)]
            trigger connection
            let awaitRetirement = do
                  current <- Daemon.getSessionState connection "worker"
                  if Map.null (State.sessionDeferredPermissions current) then pure () else Daemon.waitSessionStateChange connection "worker" current >> awaitRetirement
            awaitRetirement
            Daemon.getSessionState connection "foreign" >>= (@?= 1) . Map.size . State.sessionDeferredPermissions
            Daemon.takeDeferredPermission connection "worker" "tool-id" >>= (@?= Nothing)
            Daemon.takeDeferredQuestion connection "worker" "question-tool" >>= (@?= Just savedQuestion)
        map (field "id") (ordered [frame | frame <- trace, field "type" frame == String "response"]) @?= map String ["new-p", "new-q"],
      testCase "logout cancels an already-running automatic handler without accepting its later approval" $ bounded $ do
        void $ withPendingPeer [[permission "p" "owner" []]] [] False $ \connection replies -> do
          started <- newEmptyMVar
          release <- newEmptyMVar
          ended <- newEmptyMVar
          let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> (putMVar started () >> takeMVar release >> pure editResponse) `finally` putMVar ended ())}
          Daemon.withResumedSessionOnHandlers connection handlers "owner" $ \_ -> do
            trigger connection
            takeMVar started
            void (Daemon.logout connection)
            takeMVar ended
            atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelPermissionResult) . field "result"
            Daemon.isAuthenticated connection >>= (@?= False),
      testCase "replacement installs question, failure and MCP handlers without a new attachment" $ bounded $ do
        let badMcp = IncomingNotice "owner" (object ["type" .= String "mcp_status_changed"])
        void $ withPendingPeer [[question "q1" "owner", badMcp], [question "q2" "owner"]] [] False $ \connection replies ->
          Daemon.withResumedSessionOn connection "owner" $ \session -> do
            mcp <- newEmptyMVar
            failures <- newIORef []
            let handlers = defaultDroidHandlers {onDroidQuestion = Just (const (pure answers)), onDroidMcpEvent = Just (\_ value -> putMVar mcp value), onDroidInteractionFailure = Just (\failure -> modifyIORef' failures (<> [failure]))}
            Daemon.setSessionHandlers session handlers
            trigger connection
            takeMVar mcp >>= (@?= Left DroidMcpInvalidEvent)
            atomically (readTQueue replies) >>= (@?= responseValue "owner" answers) . field "result"
            Daemon.setSessionHandlers session (handlers {onDroidQuestion = Just (const (pure (AskUserResult [AskUserCollectedAnswer 99 "unknown" "bad" mempty] (Just False) mempty)))})
            trigger connection
            atomically (readTQueue replies) >>= (@?= responseValue "owner" cancelDroidQuestions) . field "result"
            readIORef failures >>= (@?= [InvalidInteractionResponse QuestionInteraction])
    ]

data Incoming = IncomingPermission Text Text RequestPermissionParams | IncomingQuestion Text Text | IncomingNotice Text Value

permission :: Text -> Text -> [Text] -> Incoming
permission identifier execution = IncomingPermission identifier execution . permissionParams

question :: Text -> Text -> Incoming
question = IncomingQuestion

permissionParams :: [Text] -> RequestPermissionParams
permissionParams surfaces = RequestPermissionParams [toolInfo] [ToolConfirmationListItem "Edit" ConfirmProceedEdit mempty, ToolConfirmationListItem "Cancel" ConfirmCancel mempty] (Just surfaces) mempty

toolInfo :: ToolConfirmationInfo
toolInfo = case fromJSON (object ["toolUse" .= object ["type" .= String "tool_use", "id" .= String "tool-id", "input" .= object [], "name" .= String "ExitSpecMode"], "confirmationType" .= String "exit_spec_mode", "details" .= object ["type" .= String "exit_spec_mode", "plan" .= String "plan"]]) of
  Success value -> value
  Error reason -> error reason

questionParams :: AskUserParams
questionParams = AskUserParams "question-tool" [AskUserQuestion 1.5 "Topic" "Which?" ["red", "blue"] Nothing mempty, AskUserQuestion 2 "Other" "More?" [] (Just True) mempty] mempty

editResponse :: RequestPermissionResult
editResponse = fromMaybe (error "Invalid edit fixture") (mkRequestPermissionResult ConfirmProceedEdit (Just "") (Just "") mempty)

answers :: AskUserResult
answers = AskUserResult [AskUserCollectedAnswer 1.5 "Which?" "red" mempty, AskUserCollectedAnswer 2 "More?" "" mempty] (Just False) mempty

responseValue :: (ToJSON a) => Text -> a -> Value
responseValue execution = Object . KeyMap.insert "sessionId" (String execution) . asObject . toJSON

withPendingPeer :: [[Incoming]] -> [Incoming] -> Bool -> (Daemon.DaemonConnection -> TQueue Object -> IO a) -> IO (a, [Object])
withPendingPeer batches restored rejectLogout action = do
  events <- newIORef batches
  trace <- newIORef []
  responses <- newTQueueIO
  result <- withPeer (\_ connection -> serve events trace responses connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
    Daemon.withConnection ((Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}) (`action` responses)
  frames <- readIORef trace
  pure (result, frames)
  where
    serve events trace responses connection = forever $ do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid pending peer frame")) pure . eitherDecode
      modifyIORef' trace (<> [frame])
      case field "method" frame of
        String "daemon.authenticate" -> reply connection frame (object ["userId" .= String "user", "orgId" .= String "org"])
        String "daemon.load_session" -> do
          let permissions = [Object (KeyMap.insert "requestId" (String identifier) (asObject (toJSON params))) | IncomingPermission identifier _ params <- restored]
              questions = [Object (KeyMap.insert "requestId" (String identifier) (asObject (toJSON questionParams))) | IncomingQuestion identifier _ <- restored]
          reply connection frame (object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"], "pendingPermissions" .= permissions, "pendingAskUserRequests" .= questions])
          forM_ restored $ \case notice@(IncomingNotice _ _) -> emitIncoming connection notice; _ -> pure ()
        String "daemon.list_terminals" -> reply connection frame (object ["terminals" .= ([] :: [Value])])
        String "daemon.get_proxy_token" -> do
          batch <- atomicModifyIORef' events (\case value : rest -> (rest, Just value); [] -> ([], Nothing)) >>= maybe (assertFailure "Pending peer batch exhausted") pure
          forM_ batch (emitIncoming connection)
          reply connection frame (object ["token" .= String "OFFLINE_TOKEN"])
        String "daemon.logout" -> do
          field "params" frame @?= object []
          if rejectLogout
            then sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Logout rejected"]]
            else reply connection frame (object ["accepted" .= True])
        Null | field "type" frame == String "response" -> atomically (writeTQueue responses frame)
        _ -> assertFailure "Unexpected pending-interaction RPC or implicit lifecycle mutation"

emitIncoming :: WS.Connection -> Incoming -> IO ()
emitIncoming connection = \case
  IncomingPermission identifier execution params -> sendFrame connection ["type" .= String "request", "id" .= identifier, "method" .= String "daemon.request_permission", "params" .= responseValue execution params]
  IncomingQuestion identifier execution -> sendFrame connection ["type" .= String "request", "id" .= identifier, "method" .= String "daemon.ask_user", "params" .= responseValue execution questionParams]
  IncomingNotice execution value -> sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= execution, "notification" .= value]]

trigger :: Daemon.DaemonConnection -> IO ()
trigger = void . Daemon.getProxyToken

awaitSnapshot :: PendingInteractions -> (PendingSnapshot -> Bool) -> IO PendingSnapshot
awaitSnapshot controller ready = do
  snapshot <- getPendingSnapshot controller
  if ready snapshot then pure snapshot else waitPendingSnapshotChange controller snapshot >> awaitSnapshot controller ready

firstPermission :: PendingSnapshot -> PendingPermission
firstPermission snapshot = case pendingPermissions snapshot of value : _ -> value; [] -> error "Missing fixture permission"

ordered :: [Object] -> [Object]
ordered = map snd . Map.toAscList . Map.fromList . map (\frame -> (case field "id" frame of String value -> value; _ -> "", frame))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

asObject :: Value -> Object
asObject (Object fields) = fields
asObject _ = mempty

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))
