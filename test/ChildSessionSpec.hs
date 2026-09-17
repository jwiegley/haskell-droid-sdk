{-# LANGUAGE OverloadedStrings #-}

module ChildSessionSpec (childSessionTests) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (Exception, catch, fromException, throwIO, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidInvalidEvent), DroidEvent (ChildSessionAvailableEvent), DroidHandlers (..), defaultDroidHandlers)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..))
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.Schema.Mission (SubagentInvocationSummary (..), SubagentStatus (..))
import Factory.Droid.Schema.Notifications (ChildSessionAvailable)
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcEntityNotFound))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

childSessionTests :: TestTree
childSessionTests =
  testGroup
    "Child session state"
    [ testCase "availability seeds first linkage and only complete invocation metadata" $ do
        notice <- decodeValue @ChildSessionAvailable (available "child" (Just "tool") True)
        conflicting <- decodeValue @ChildSessionAvailable (available "child" (Just "different") True)
        incomplete <- decodeValue @ChildSessionAvailable (available "other" Nothing False)
        let first = State.observeChildAvailable "parent" notice State.emptySessionState
        State.sessionCallingSessionId first @?= Just "parent"
        State.sessionCallingToolUseId first @?= Just "tool"
        State.observeChildAvailable "foreign" notice first @?= first
        let missingTool = State.setCallingMetadata (Just "parent") Nothing State.emptySessionState
        State.observeChildAvailable "foreign" notice missingTool @?= missingTool
        State.sessionCallingToolUseId (State.observeChildAvailable "parent" conflicting first) @?= Just "tool"
        State.observeChildAvailable "child" notice State.emptySessionState @?= State.emptySessionState
        State.sessionInvocationSummary (State.observeChildAvailable "parent" incomplete State.emptySessionState) @?= Nothing
        let reported = State.setCallingMetadata (Just "reported") (Just "reported-tool") first
        State.sessionCallingSessionId reported @?= Just "reported"
        State.sessionCallingToolUseId (State.setCallingMetadata Nothing (Just "") reported) @?= Just "reported-tool",
      testCase "fresh availability reactivates terminal summaries without inventing missing data or metrics" $ do
        notice <- decodeValue @ChildSessionAvailable (available "child" Nothing True)
        old <- decodeValue @SubagentInvocationSummary (summaryValue "child" "completed" 0 0)
        let state = State.observeChildAvailable "parent" notice (State.setInvocationSummary old State.emptySessionState)
        State.sessionInvocationSummary state @?= Just (old {invocationStatus = SubagentRunning})
        clear <- decodeValue @ChildSessionAvailable (object ["type" .= String "child_session_available", "childSessionId" .= String "empty", "timestamp" .= Number 0, "subagentType" .= String "", "description" .= String ""])
        fmap invocationDescription (State.sessionInvocationSummary (State.observeChildAvailable "parent" clear State.emptySessionState)) @?= Just "",
      testCase "observed invocation metrics count assistant tools and preserve task status and zero" $ do
        invocation <- decodeValue @SubagentInvocationSummary (summaryValue "child" "running" 99 999)
        user <- decodeValue @FactoryDroidMessage (messageValue "user" "user" 10 0 [])
        assistant <- decodeValue @FactoryDroidMessage (messageValue "assistant" "assistant" 20 30 [toolValue "one", toolValue "two"])
        let state = State.refreshInvocationSummary (State.setInvocationSummary invocation (State.mergeLoadedMessages [user, assistant] State.emptySessionState))
        State.sessionInvocationSummary state @?= Just (invocation {invocationToolUseCount = Just 2, invocationDurationMs = Just 20})
        zero <- decodeValue @FactoryDroidMessage (messageValue "zero" "user" 0 0 [])
        let emptyTools = State.refreshInvocationSummary (State.setInvocationSummary invocation (State.mergeLoadedMessages [zero] State.emptySessionState))
        fmap invocationToolUseCount (State.sessionInvocationSummary emptyTools) @?= Just (Just 0)
        fmap invocationDurationMs (State.sessionInvocationSummary emptyTools) @?= Just (Just 999),
      testCase "load codecs retain optional tool linkage and summaries and reject malformed declared fields" $ do
        let report = loadValue [] False ["callingSessionId" .= String "parent", "callingToolUseId" .= String "", "subagentInvocations" .= [summaryValue "child" "completed" 0 0]]
        loaded <- decodeValue @LoadedSessionState report
        loadedCallingToolUseId loaded @?= Just ""
        fmap length (loadedSubagentInvocations loaded) @?= Just 1
        toJSON loaded @?= report
        forM_ ["callingToolUseId", "subagentInvocations"] $ \key -> case fromJSON @LoadedSessionState (Object (KeyMap.insert key Null (asObject report))) of
          Error _ -> pure ()
          Success _ -> assertFailure "Explicit null accepted for a declared child field",
      testCase "manual registration and summary hydration do not load or grant anything" $ bounded $ do
        (_, trace) <- withChildPeer True [] [] $ \connection _ _ -> do
          z <- decodeValue @ChildSessionAvailable (available "z-child" (Just "tool") True)
          a <- decodeValue @ChildSessionAvailable (available "a-child" (Just "tool") True)
          Daemon.registerChildSession connection "parent" z >>= (@?= True)
          Daemon.registerChildSession connection "parent" a >>= (@?= True)
          Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Just "z-child")
          Daemon.findSubagentSessionId connection "foreign" "tool" >>= (@?= Nothing)
          Daemon.getSubagentSessionIdsByParent connection >>= (@?= Map.singleton "parent" (Map.singleton "tool" "z-child"))
          try @Daemon.DaemonError (Daemon.registerChildSession connection " " z) >>= (@?= Left Daemon.InvalidChildSessionIdentity)
          Daemon.ensureChildSessionAttached connection "unknown" >>= (@?= False)
          valid <- decodeValue @SubagentInvocationSummary (summaryValue "summary-only" "completed" 0 0)
          invalid <- decodeValue @SubagentInvocationSummary (summaryValue "  " "running" 1 1)
          Daemon.hydrateSubagentInvocationSummaries connection [valid, invalid]
          Daemon.getSubagentInvocationSummary connection "summary-only" >>= (@?= Just valid)
          Daemon.getSubagentInvocationSummary connection "  " >>= (@?= Nothing)
        map (field "method") trace @?= [String "daemon.authenticate"],
      testCase "automatic hydration preserves early child messages and reported linkage supersedes discovery" $ bounded $ do
        let early = messageValue "early" "user" 0 0 []
            child = loadValue [] True ["callingSessionId" .= String "reported-parent", "callingToolUseId" .= String "reported-tool"]
            batches = [[("child", object ["type" .= String "create_message", "message" .= early]), ("parent", available "child" (Just "tool") True)]]
        (_, trace) <- withChildPeer True [("child", [Loaded child])] batches $ \connection _ _ -> do
          void (Daemon.getProxyToken connection)
          state <- waitState connection "child" ((== Just "reported-parent") . State.sessionCallingSessionId)
          map (field "id" . asObject . toJSON) (State.sessionMessages state) @?= [String "early"]
          Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Nothing)
          Daemon.findSubagentSessionId connection "reported-parent" "reported-tool" >>= (@?= Just "child")
          Daemon.ensureChildSessionAttached connection "child" >>= (@?= True)
        loadIds trace @?= ["child"],
      testCase "disabled automatic hydration still exposes metadata and explicit hydration reuses one load" $ bounded $ do
        (_, trace) <- withChildPeer False [("child", [Loaded (loadValue [] False [])])] [[("parent", available "child" (Just "tool") True)]] $ \connection _ _ -> do
          void (Daemon.getProxyToken connection)
          void (waitState connection "child" ((== Just "parent") . State.sessionCallingSessionId))
          Daemon.getSessionReadiness connection "child" >>= (@?= Daemon.SessionNotLoaded) . Daemon.readinessPhase
          Daemon.ensureChildSessionAttached connection "child" >>= (@?= True)
          Daemon.ensureChildSessionAttached connection "child" >>= (@?= True)
        loadIds trace @?= ["child"],
      testCase "duplicate availability and concurrent ensure calls share the existing child load" $ bounded $ do
        let child = loadValue [] False []
        (_, trace) <- withChildPeer True [("child", [Held child])] [[("parent", available "child" (Just "tool") True), ("parent", available "child" (Just "tool") True)], []] $ \connection loads _ -> do
          void (Daemon.getProxyToken connection)
          void (atomically (readTQueue loads))
          withAsync (Daemon.ensureChildSessionAttached connection "child") $ \one ->
            withAsync (Daemon.ensureChildSessionAttached connection "child") $ \two -> do
              void (Daemon.getProxyToken connection)
              wait one >>= (@?= True)
              wait two >>= (@?= True)
        loadIds trace @?= ["child"],
      testCase "failed automatic hydration is observable and explicit retry clears its state" $ bounded $ do
        (_, trace) <- withChildPeer True [("child", [Failed RpcEntityNotFound, Loaded (loadValue [] False [])])] [[("parent", available "child" (Just "tool") True)]] $ \connection _ _ -> do
          void (Daemon.getProxyToken connection)
          failed <- waitState connection "child" ((== Just State.ChildLoadNotFound) . State.sessionChildLoadError)
          State.sessionCallingSessionId failed @?= Just "parent"
          Daemon.ensureChildSessionAttached connection "child" >>= (@?= True)
          Daemon.getSessionState connection "child" >>= (@?= Nothing) . State.sessionChildLoadError
        loadIds trace @?= ["child", "child"],
      testCase "cancelled explicit hydration preserves cancellation and cannot overwrite a later load" $ bounded $ do
        let older = loadValue [] False ["callingToolUseId" .= String "stale"]
            newer = loadValue [] False ["callingToolUseId" .= String "new"]
        void $ withChildPeer False [("child", [Held older, Loaded newer])] [[], []] $ \connection loads _ -> do
          notice <- decodeValue @ChildSessionAvailable (available "child" (Just "tool") True)
          void (Daemon.registerChildSession connection "parent" notice)
          withAsync (Daemon.ensureChildSessionAttached connection "child") $ \pending -> do
            void (atomically (readTQueue loads))
            cancel pending
            waitCatch pending >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled hydration returned"
          Daemon.getSessionState connection "child" >>= (@?= Just State.ChildLoadInterrupted) . State.sessionChildLoadError
          void (Daemon.ensureChildSessionAttached connection "child")
          void (Daemon.getProxyToken connection)
          state <- Daemon.getSessionState connection "child"
          State.sessionCallingToolUseId state @?= Just "new"
          State.sessionChildLoadError state @?= Nothing,
      testCase "scope exit cancels pending automatic loads and does not extend connection ownership" $ bounded $ do
        (expired, trace) <- withChildPeer True [("child", [Held (loadValue [] False [])])] [[("parent", available "child" (Just "tool") True)]] $ \connection loads _ -> do
          void (Daemon.getProxyToken connection)
          void (atomically (readTQueue loads))
          pure connection
        try @RpcChannelError (Daemon.getSessionState expired "child") >>= (@?= Left RpcChannelClosed)
        loadIds trace @?= ["child"],
      testCase "child links never inherit a parent's permission handler without reported associations" $ bounded $ do
        invoked <- newIORef False
        let handlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> modifyIORef' invoked (const True) >> assertFailure "Parent handler inherited")}
            permission = object ["requestId" .= String "child-permission", "toolUses" .= ([] :: [Value]), "options" .= [object ["label" .= String "Cancel", "value" .= String "cancel"]]]
            child = loadValue [] False ["callingSessionId" .= String "parent", "pendingPermissions" .= [permission]]
        void $ withChildPeer True [("parent", [Loaded (loadValue [] False [])]), ("child", [Loaded child])] [[("parent", available "child" (Just "tool") True)]] $ \connection _ responses ->
          Daemon.withResumedSessionOnHandlers connection handlers "parent" $ \_ -> do
            void (Daemon.getProxyToken connection)
            response <- atomically (readTQueue responses)
            field "result" response @?= object ["sessionId" .= String "child", "selectedOption" .= String "cancel"]
            readIORef invoked >>= (@?= False),
      testCase "loaded invocation summaries hydrate without child loads and tagged completion refreshes metrics" $ bounded $ do
        let user = messageValue "user" "user" 10 0 []
            assistant = messageValue "assistant" "assistant" 20 30 [toolValue "one", toolValue "two"]
            parent = loadValue [] False ["subagentInvocations" .= [summaryValue "child" "completed" 99 0]]
            child = loadValue [user, assistant] True ["callingSessionId" .= String "parent", "callingToolUseId" .= String "tool"]
            batches = [[("parent", available "child" (Just "tool") True)], [("child", completedTurn)]]
        void $ withChildPeer True [("parent", [Loaded parent]), ("child", [Loaded child])] batches $ \connection _ _ -> do
          void (Daemon.loadSessionInfo connection "parent")
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just (Just 99)) . fmap invocationToolUseCount
          void (Daemon.getProxyToken connection)
          void (waitLoaded connection "child")
          void (Daemon.getProxyToken connection)
          state <- waitState connection "child" ((== Just (Just 2)) . fmap invocationToolUseCount . State.sessionInvocationSummary)
          fmap invocationDurationMs (State.sessionInvocationSummary state) @?= Just (Just 20)
          fmap invocationStatus (State.sessionInvocationSummary state) @?= Just SubagentCompleted,
      testCase "silent explicit child close retires lookup but retains its historical summary" $ bounded $ do
        void $ withChildPeer False [("child", [Loaded (loadValue [] False ["callingSessionId" .= String "parent", "callingToolUseId" .= String "tool"])])] [] $ \connection _ _ -> do
          summary <- decodeValue @SubagentInvocationSummary (summaryValue "child" "completed" 2 0)
          Daemon.setSubagentInvocationSummary connection summary
          Daemon.withResumedSessionOn connection "child" $ \session -> do
            Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Just "child")
            void (Daemon.closeAttachedSession session)
            Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Nothing)
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just summary),
      testCase "a superseded automatic load cannot overwrite newer child metadata" $ bounded $ do
        let old = loadValue [] True ["callingToolUseId" .= String "old"]
            new = loadValue [] True ["callingToolUseId" .= String "new"]
            barrier = object ["type" .= String "create_message", "message" .= messageValue "barrier" "user" 0 0 []]
        void $ withChildPeer True [("child", [Held old, Loaded new])] [[("parent", available "child" (Just "tool") True)], [("child", barrier)]] $ \connection loads _ -> do
          void (Daemon.getProxyToken connection)
          void (atomically (readTQueue loads))
          void (Daemon.loadSessionInfo connection "child")
          void (Daemon.getProxyToken connection)
          state <- waitState connection "child" (Map.member "barrier" . State.sessionMessagesById)
          State.sessionCallingToolUseId state @?= Just "new"
          State.sessionChildLoadError state @?= Nothing,
      testCase "an existing managed summary refreshes metrics without a subagent tag" $ bounded $ do
        let child = loadValue [messageValue "assistant" "assistant" 0 10 [toolValue "tool"]] False ["callingSessionId" .= String "parent"]
            barrier = object ["type" .= String "create_message", "message" .= messageValue "barrier" "user" 0 0 []]
        void $ withChildPeer False [("child", [Loaded child])] [[("child", completedTurn), ("child", barrier)]] $ \connection _ _ -> do
          report <- decodeValue @SubagentInvocationSummary (summaryValue "child" "completed" 7 0)
          Daemon.setSubagentInvocationSummary connection report
          void (Daemon.loadSessionInfo connection "child")
          void (Daemon.getProxyToken connection)
          void (waitState connection "child" (Map.member "barrier" . State.sessionMessagesById))
          Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just (report {invocationToolUseCount = Just 1})),
      testCase "invalid availability never loads a child and self-link discovery is a no-op" $ bounded $ do
        (_, trace) <- withChildPeer True [] [[("self", available "self" Nothing True)], [("parent", available "  " Nothing True)]] $ \connection _ _ -> do
          void (Daemon.getProxyToken connection)
          Daemon.getSessionState connection "self" >>= (@?= Nothing) . State.sessionCallingSessionId
          void (Daemon.getProxyToken connection)
          void (waitState connection "parent" ((== Just State.MalformedMessageEvent) . State.sessionMessageError))
        loadIds trace @?= [],
      testCase "scope exceptions remain intact while automatic hydration is pending" $ bounded $ do
        result <- try @ChildAbort $ withChildPeer True [("child", [Held (loadValue [] False [])])] [[("parent", available "child" Nothing True)]] $ \connection loads _ -> do
          void (Daemon.getProxyToken connection)
          void (atomically (readTQueue loads))
          throwIO ChildAbort
        case result of Left _ -> pure (); Right _ -> assertFailure "Scope exception was lost",
      testCase "malformed child receipts cannot publish linkage or invocation summaries" $ bounded $ do
        let bad = loadValue [] False ["callingSessionId" .= String "wrong", "subagentInvocations" .= [object ["childSessionId" .= String "other"]]]
        void $ withChildPeer False [("child", [Loaded bad])] [] $ \connection _ _ -> do
          notice <- decodeValue @ChildSessionAvailable (available "child" (Just "tool") True)
          void (Daemon.registerChildSession connection "parent" notice)
          before <- Daemon.getSubagentInvocationSummary connection "child"
          try @DroidError (Daemon.ensureChildSessionAttached connection "child") >>= (@?= Left DroidInvalidEvent)
          state <- Daemon.getSessionState connection "child"
          State.sessionCallingSessionId state @?= Just "parent"
          State.sessionInvocationSummary state @?= before
          State.sessionChildLoadError state @?= Just State.ChildLoadFailed
          Daemon.getSubagentInvocationSummary connection "other" >>= (@?= Nothing),
      testCase "silent attached close prevents a pending reload from restoring child linkage" $ bounded $ do
        let child = loadValue [] False ["callingSessionId" .= String "parent", "callingToolUseId" .= String "tool"]
            barrier = object ["type" .= String "create_message", "message" .= messageValue "barrier" "user" 0 0 []]
        void $ withChildPeer False [("child", [Loaded child, Held child])] [[("parent", barrier)]] $ \connection loads _ ->
          Daemon.withResumedSessionOn connection "child" $ \session -> do
            void (atomically (readTQueue loads))
            withAsync (Daemon.loadSessionInfo connection "child") $ \pending -> do
              void (atomically (readTQueue loads))
              void (Daemon.closeAttachedSession session)
              void (Daemon.getProxyToken connection)
              void (waitState connection "parent" (Map.member "barrier" . State.sessionMessagesById))
              state <- Daemon.getSessionState connection "child"
              State.sessionCallingSessionId state @?= Nothing
              readiness <- Daemon.getSessionReadiness connection "child"
              Daemon.readinessPhase readiness @?= Daemon.SessionNotLoaded
              Daemon.readinessKnown readiness @?= False
              waitCatch pending >>= \case Left cause -> fromException cause @?= Just Daemon.DaemonLoadSuperseded; Right _ -> assertFailure "Closed child load returned",
      testCase "duplicate tool lookup retains session registration order when linkage arrives later" $ bounded $ do
        void $ withChildPeer False [("first", [Loaded (loadValue [] False [])])] [] $ \connection _ _ -> do
          void (Daemon.loadSessionInfo connection "first")
          second <- decodeValue @ChildSessionAvailable (available "second" (Just "tool") True)
          first <- decodeValue @ChildSessionAvailable (available "first" (Just "tool") True)
          void (Daemon.registerChildSession connection "parent" second)
          void (Daemon.registerChildSession connection "parent" first)
          Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Just "first")
          Daemon.getSubagentSessionIdsForParent connection "parent" >>= (@?= Map.singleton "tool" "first"),
      testCase "typed availability callbacks observe already-published child linkage" $ bounded $ do
        let notice = Object (KeyMap.insert "future" (Bool False) (asObject (available "child" (Just "tool") True)))
        expected <- decodeValue @ChildSessionAvailable notice
        observed <- newTQueueIO
        void $ withChildPeer False [("parent", [Loaded (loadValue [] False [])])] [[("parent", notice)]] $ \connection _ _ ->
          Daemon.withResumedSessionOn connection "parent" $ \parent -> do
            stop <- Daemon.onSessionEvent parent $ \case
              Right (ChildSessionAvailableEvent event) -> do
                state <- Daemon.getSessionState connection "child"
                atomically (writeTQueue observed (event, State.sessionCallingSessionId state))
              _ -> pure ()
            void (Daemon.getProxyToken connection)
            atomically (readTQueue observed) >>= (@?= (expected, Just "parent"))
            stop
    ]

data ChildAbort = ChildAbort deriving stock (Show)

instance Exception ChildAbort

data LoadReply = Loaded Value | Held Value | Failed JsonRpcErrorCode

withChildPeer :: Bool -> [(Text, [LoadReply])] -> [[(Text, Value)]] -> (Daemon.DaemonConnection -> TQueue Object -> TQueue Object -> IO a) -> IO (a, [Object])
withChildPeer automatic replies batches action = do
  loads <- newIORef (Map.fromList replies)
  notifications <- newIORef batches
  held <- newIORef []
  trace <- newIORef []
  loadSeen <- newTQueueIO
  responses <- newTQueueIO
  result <- withPeer (\_ connection -> serve loads notifications held trace loadSeen responses connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
    Daemon.withConnection ((Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}, Daemon.daemonHydrateChildSessions = automatic}) (\connection -> action connection loadSeen responses)
  recorded <- readIORef trace
  pure (result, recorded)
  where
    serve loads notifications held trace loadSeen responses connection = forever $ do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid child peer RPC")) pure . eitherDecode
      modifyIORef' trace (<> [frame])
      case field "method" frame of
        String "daemon.authenticate" -> reply connection frame (object ["userId" .= String "user", "orgId" .= String "org"])
        String "daemon.list_terminals" -> reply connection frame (object ["terminals" .= ([] :: [Value])])
        String "daemon.load_session" -> do
          identifier <- textField "sessionId" (asObject (field "params" frame))
          atomically (writeTQueue loadSeen frame)
          selected <- atomicModifyIORef' loads $ \remaining -> case Map.lookup identifier remaining of Just (next : rest) -> (Map.insert identifier rest remaining, Just next); _ -> (remaining, Nothing)
          case selected of
            Just (Loaded value) -> reply connection frame value
            Just (Held value) -> modifyIORef' held (<> [(frame, value)])
            Just (Failed code) -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= code, "message" .= String "Child unavailable"]]
            Nothing -> assertFailure "Unexpected child load"
        String "daemon.get_proxy_token" -> do
          pending <- atomicModifyIORef' held ([],)
          forM_ pending (uncurry (reply connection))
          batch <- nextBatch notifications
          forM_ batch (uncurry (notify connection))
          reply connection frame (object ["token" .= String "OFFLINE_TOKEN"])
        String "daemon.close_session" -> reply connection frame (object [])
        Null | field "type" frame == String "response" -> atomically (writeTQueue responses frame)
        _ -> assertFailure "Unexpected child-state RPC"

nextBatch :: IORef [a] -> IO a
nextBatch ref = atomicModifyIORef' ref (\case [] -> ([], Nothing); value : rest -> (rest, Just value)) >>= maybe (assertFailure "Child event fixture exhausted") pure

waitState :: Daemon.DaemonConnection -> Text -> (State.SessionState -> Bool) -> IO State.SessionState
waitState connection identifier ready = do
  state <- Daemon.getSessionState connection identifier
  if ready state then pure state else Daemon.waitSessionStateChange connection identifier state >> waitState connection identifier ready

waitLoaded :: Daemon.DaemonConnection -> Text -> IO Daemon.SessionReadiness
waitLoaded connection identifier = do
  readiness <- Daemon.getSessionReadiness connection identifier
  if Daemon.readinessPhase readiness == Daemon.SessionLoaded then pure readiness else Daemon.waitSessionReadinessChange connection identifier readiness >> waitLoaded connection identifier

available :: Text -> Maybe Text -> Bool -> Value
available child tool complete = object (["type" .= String "child_session_available", "childSessionId" .= child, "timestamp" .= Number 0] <> maybe [] (\value -> ["toolUseId" .= value]) tool <> if complete then ["subagentType" .= String "worker", "description" .= String "offline child"] else [])

summaryValue :: Text -> Text -> Scientific -> Scientific -> Value
summaryValue child status count duration = object ["childSessionId" .= child, "status" .= status, "subagentType" .= String "worker", "description" .= String "offline child", "toolUseCount" .= count, "durationMs" .= duration]

loadValue :: [Value] -> Bool -> [Pair] -> Value
loadValue messages tagged extra = object (["session" .= object ["messages" .= messages], "settings" .= object (["modelId" .= String "model", "reasoningEffort" .= String "low"] <> ["tags" .= [object ["name" .= String "subagent"]] | tagged])] <> extra)

messageValue :: Text -> Text -> Scientific -> Scientific -> [Value] -> Value
messageValue identifier role created updated content = object ["id" .= identifier, "role" .= role, "content" .= content, "createdAt" .= created, "updatedAt" .= updated]

toolValue :: Text -> Value
toolValue identifier = object ["type" .= String "tool_use", "id" .= identifier, "name" .= String "Read", "input" .= object []]

completedTurn :: Value
completedTurn = object ["type" .= String "agent_turn_completed", "turnId" .= String "turn", "reason" .= String "completed", "tokenUsage" .= object ["inputTokens" .= Number 0, "outputTokens" .= Number 0, "cacheCreationTokens" .= Number 0, "cacheReadTokens" .= Number 0, "thinkingTokens" .= Number 0]]

loadIds :: [Object] -> [Value]
loadIds trace = [field "sessionId" (asObject (field "params" frame)) | frame <- trace, field "method" frame == String "daemon.load_session"]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

asObject :: Value -> Object
asObject (Object fields) = fields
asObject _ = error "Expected child peer object"

textField :: Key -> Object -> IO Text
textField key fields = case field key fields of String value -> pure value; _ -> assertFailure "Missing child peer text field"

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

notify :: WS.Connection -> Text -> Value -> IO ()
notify connection identifier value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= value]]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection values = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> values)))

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
