{-# LANGUAGE OverloadedStrings #-}

module DaemonCacheSpec (cacheTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (catch, finally, fromException, throwIO, try, uninterruptibleMask_)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid (DroidError (..))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError (..))
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Interaction (AskUserResult (..))
import Factory.Droid.Schema.Mission (SubagentInvocationSummary (..), SubagentStatus (..))
import Factory.Droid.Schema.Notifications (ChildSessionAvailable)
import Factory.Droid.Schema.RPC
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport (objectTransport)
import ProtocolSpec (reply)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

cacheTests :: TestTree
cacheTests =
  testGroup
    "Daemon session cache"
    [ testCase "default capacity retains the last twenty detached snapshots" $ bounded $ withCachePeer $ \connection peer -> do
        Daemon.getSessionCacheCapacity connection >>= (@?= Just 20)
        let identifiers = ["s" <> Text.pack (show n) | n <- [1 .. 25 :: Int]]
        forM_ identifiers $ \identifier -> Daemon.withResumedSessionOn connection identifier (const (pure ()))
        Daemon.getCachedSessionIds connection >>= (@?= drop 5 identifiers)
        Daemon.getSessionState connection "s1" >>= (@?= State.WorkingDirectoryUnknown) . State.sessionWorkingDirectory
        Daemon.getSessionState connection "s6" >>= (@?= State.WorkingDirectoryReported (Just "/saved/s6")) . State.sessionWorkingDirectory
        Daemon.getSessionReadiness connection "s1" >>= (@?= False) . Daemon.readinessKnown
        Daemon.getSessionState connection "s1" >>= (@?= []) . State.sessionMessages
        Daemon.getSessionState connection "s6" >>= (@?= [String "message-s6"]) . map (field "id" . asObject . toJSON) . State.sessionMessages
        frames <- drain peer
        map (field "method") frames @?= replicate 25 (String "daemon.load_session"),
      testCase "reference touch active and loading eviction cases" $ bounded $ withCachePeer $ \connection peer -> do
        Daemon.setSessionCacheCapacity connection (Just 2) >>= (@?= [])
        mapM_ (load connection) ["a", "b"]
        Daemon.touchSession connection "a" >>= (@?= True)
        load connection "c"
        Daemon.getCachedSessionIds connection >>= (@?= ["a", "c"])
        void (Daemon.setActiveSessionId connection (Just "a"))
        load connection "d"
        Daemon.getCachedSessionIds connection >>= (@?= ["a", "d"])
        void (drain peer)
        setRule peer "daemon.load_session" "loading" Hold
        withAsync (load connection "loading") $ \loading -> do
          request <- nextRequest peer "daemon.load_session" "loading"
          load connection "e"
          Daemon.getCachedSessionIds connection >>= (@?= ["a", "loading"])
          Daemon.removeCachedSession connection "loading" >>= (@?= False)
          respond peer request
          wait loading,
      testCase "plain observation does not touch but reloading does" $ bounded $ withCachePeer $ \connection _ -> do
        void (Daemon.setSessionCacheCapacity connection (Just 2))
        mapM_ (load connection) ["a", "b"]
        void (Daemon.getSessionState connection "a")
        load connection "c"
        Daemon.getCachedSessionIds connection >>= (@?= ["b", "c"])
        load connection "b"
        load connection "d"
        Daemon.getCachedSessionIds connection >>= (@?= ["b", "d"]),
      testCase "unlimited exact large and zero capacities have distinct meanings" $ bounded $ withCachePeer $ \connection _ -> do
        void (Daemon.setSessionCacheCapacity connection Nothing)
        let identifiers = ["s" <> Text.pack (show n) | n <- [1 .. 25 :: Int]]
        mapM_ (load connection) identifiers
        Daemon.getCachedSessionIds connection >>= (@?= identifiers)
        Daemon.setSessionCacheCapacity connection (Just (10 ^ (80 :: Int))) >>= (@?= [])
        Daemon.getSessionCacheCapacity connection >>= (@?= Just (10 ^ (80 :: Int)))
        Daemon.setSessionCacheCapacity connection (Just 0) >>= (@?= identifiers)
        Daemon.getCachedSessionIds connection >>= (@?= [])
        Daemon.withSessionOn connection (creationOptions "attached") $ \_ ->
          Daemon.getCachedSessionIds connection >>= (@?= ["attached"])
        Daemon.getCachedSessionIds connection >>= (@?= []),
      testCase "active attachments can exceed the soft capacity and detach independently" $ bounded $ withCachePeer $ \connection peer -> do
        void (Daemon.setSessionCacheCapacity connection (Just 0))
        Daemon.withSessionOn connection (creationOptions "one") $ \one ->
          Daemon.withSessionOn connection (creationOptions "two") $ \_ -> do
            Daemon.getCachedSessionIds connection >>= (@?= ["one", "two"])
            Daemon.removeCachedSession connection "one" >>= (@?= False)
            Daemon.detachSession one
            Daemon.getCachedSessionIds connection >>= (@?= ["two"])
        Daemon.getCachedSessionIds connection >>= (@?= [])
        drain peer >>= (@?= replicate 2 (String "daemon.initialize_session")) . map (field "method"),
      testCase "non-idle sessions are protected until an authoritative idle load" $ bounded $ withCachePeer $ \connection peer -> do
        void (Daemon.setSessionCacheCapacity connection (Just 1))
        atomically (modifyTVar' (peerWorking peer) (Map.insert "busy" "thinking"))
        load connection "busy"
        load connection "other"
        Daemon.getCachedSessionIds connection >>= (@?= ["busy"])
        Daemon.removeCachedSession connection "busy" >>= (@?= False)
        atomically (modifyTVar' (peerWorking peer) (Map.insert "busy" "idle"))
        load connection "busy"
        Daemon.removeCachedSession connection "busy" >>= (@?= True)
        Daemon.getCachedSessionIds connection >>= (@?= []),
      testCase "cancelled loading can retire and its late reply cannot restore state" $ bounded $ withCachePeer $ \connection peer -> do
        void (Daemon.setSessionCacheCapacity connection (Just 0))
        setRule peer "daemon.load_session" "held" Hold
        withAsync (load connection "held") $ \worker -> do
          request <- nextRequest peer "daemon.load_session" "held"
          Daemon.getCachedSessionIds connection >>= (@?= ["held"])
          Daemon.removeCachedSession connection "held" >>= (@?= False)
          cancel worker
          waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation lost"
          Daemon.getCachedSessionIds connection >>= (@?= [])
          respond peer request
          void (Daemon.getProxyToken connection)
          Daemon.getCachedSessionIds connection >>= (@?= [])
          Daemon.getSessionState connection "held" >>= (@?= State.WorkingDirectoryUnknown) . State.sessionWorkingDirectory,
      testCase "fresh rejected creation removes provisional state but preserves prior state" $ bounded $ withCachePeer $ \connection peer -> do
        setRule peer "daemon.initialize_session" "fresh" Reject
        rejected (Daemon.withSessionOn connection (creationOptions "fresh") (const (pure ())))
        Daemon.getCachedSessionIds connection >>= (@?= [])
        Daemon.getSessionReadiness connection "fresh" >>= (@?= False) . Daemon.readinessKnown
        Daemon.getSessionState connection "fresh" >>= (@?= State.emptySessionState)
        load connection "prior"
        setRule peer "daemon.initialize_session" "prior" Reject
        rejected (Daemon.withSessionOn connection (creationOptions "prior") (const (pure ())))
        Daemon.getCachedSessionIds connection >>= (@?= ["prior"])
        Daemon.getSessionState connection "prior" >>= (@?= State.WorkingDirectoryReported (Just "/saved/prior")) . State.sessionWorkingDirectory,
      testCase "cancelled fresh creation releases its provisional cache entry" $ bounded $ withCachePeer $ \connection peer -> do
        setRule peer "daemon.initialize_session" "fresh" Hold
        withAsync (Daemon.withSessionOn connection (creationOptions "fresh") (const (assertFailure "Cancelled creation published"))) $ \worker -> do
          void (nextRequest peer "daemon.initialize_session" "fresh")
          cancel worker
          waitCatch worker >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancellation lost"
        Daemon.getCachedSessionIds connection >>= (@?= [])
        Daemon.getSessionReadiness connection "fresh" >>= (@?= False) . Daemon.readinessKnown,
      testCase "an older failed creation cannot remove a superseding load" $ bounded $ withCachePeer $ \connection peer -> do
        setRule peer "daemon.initialize_session" "same" Hold
        withAsync (rejected (Daemon.withSessionOn connection (creationOptions "same") (const (pure ())))) $ \worker -> do
          request <- nextRequest peer "daemon.initialize_session" "same"
          load connection "same"
          reject peer request
          wait worker
        Daemon.getCachedSessionIds connection >>= (@?= ["same"])
        Daemon.getSessionState connection "same" >>= (@?= State.WorkingDirectoryReported (Just "/saved/same")) . State.sessionWorkingDirectory
        Daemon.getSessionReadiness connection "same" >>= (@?= True) . Daemon.readinessKnown,
      testCase "removal preserves independent load intent without remote teardown" $ bounded $ withCachePeer $ \connection peer -> do
        void (Daemon.loadSessionInfoWithConfiguration connection "policy" (defaultSessionLoadConfiguration {loadMessageLimit = Just 7}) (defaultDaemonLoadConfiguration {daemonLoadRuntimeSettingsPath = Just "runtime"}))
        Daemon.removeCachedSession connection "policy" >>= (@?= True)
        Daemon.removeCachedSession connection "policy" >>= (@?= False)
        Daemon.getSessionReadiness connection "policy" >>= (@?= False) . Daemon.readinessKnown
        load connection "policy"
        frames <- drain peer
        map (field "method") frames @?= replicate 2 (String "daemon.load_session")
        forM_ frames $ \request -> do
          field "messageLimit" (params request) @?= Number 7
          field "runtimeSettingsPath" (params request) @?= String "runtime",
      testCase "failed fresh creation restores independently retained earlier load intent" $ bounded $ withCachePeer $ \connection peer -> do
        void (Daemon.loadSessionInfoWithConfiguration connection "policy" (defaultSessionLoadConfiguration {loadMessageLimit = Just 7}) defaultDaemonLoadConfiguration)
        void (Daemon.removeCachedSession connection "policy")
        void (drain peer)
        setRule peer "daemon.initialize_session" "policy" Reject
        let options = (creationOptions "policy") {Daemon.daemonSessionLoadConfiguration = defaultSessionLoadConfiguration {loadMessageLimit = Just 9}}
        rejected (Daemon.withSessionOn connection options (const (pure ())))
        void (drain peer)
        load connection "policy"
        request <- nextRequest peer "daemon.load_session" "policy"
        field "messageLimit" (params request) @?= Number 7,
      testCase "removal clears cached linkage and cwd but retains child summaries" $ bounded $ withCachePeer $ \connection _ -> do
        mapM_ (load connection) ["parent", "child"]
        notice <- decodeValue @ChildSessionAvailable (object ["type" .= String "child_session_available", "childSessionId" .= String "child", "timestamp" .= Number 0, "toolUseId" .= String "tool", "subagentType" .= String "worker", "description" .= String "child"])
        Daemon.registerChildSession connection "parent" notice >>= (@?= True)
        let summary = SubagentInvocationSummary "child" SubagentCompleted "worker" "child" (Just 9) (Just 12) mempty
        Daemon.setSubagentInvocationSummary connection summary
        Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Just "child")
        Daemon.removeCachedSession connection "child" >>= (@?= True)
        Daemon.findSubagentSessionId connection "parent" "tool" >>= (@?= Nothing)
        Daemon.getSubagentInvocationSummary connection "child" >>= (@?= Just summary)
        Daemon.getSessionState connection "child" >>= (@?= State.WorkingDirectoryUnknown) . State.sessionWorkingDirectory
        Daemon.getCachedSessionIds connection >>= (@?= ["parent"]),
      testCase "deferred decisions protect entries without changing cache capacity" $ bounded $ withCachePeer $ \connection _ -> do
        load connection "deferred"
        let decision = State.DeferredUserAction "question" "tool" (AskUserResult [] (Just True) mempty) 0 []
        Daemon.storeDeferredQuestion connection "deferred" decision
        Daemon.setSessionCacheCapacity connection (Just 0) >>= (@?= [])
        Daemon.removeCachedSession connection "deferred" >>= (@?= False)
        Daemon.clearDeferredUserActions connection "deferred" >>= (@?= 1)
        Daemon.pruneSessionCache connection >>= (@?= ["deferred"]),
      testCase "cache clearing cannot revive terminal restoration or buffer acknowledgements" $ do
        let terminal = State.defaultTerminalMetadata "main" State.TerminalConnected
            initial = State.appendTerminalBufferedData "main" "old" (State.addSessionTerminal terminal State.emptySessionState)
        claim <- maybe (assertFailure "Missing buffer claim") pure (State.terminalBufferSnapshot "main" initial)
        let (ticket, pending) = State.beginTerminalRestoration initial
            cleared = State.clearCachedSessionState pending
            fresh = State.appendTerminalBufferedData "main" "old-fresh" (State.addSessionTerminal terminal cleared)
            (_, restoring) = State.beginTerminalRestoration fresh
            unsafeFresh = State.appendTerminalBufferedData "main" "old-fresh" (State.addSessionTerminal terminal State.emptySessionState)
            (_, unsafeRestoring) = State.beginTerminalRestoration unsafeFresh
        stale <- decodeValue (object ["id" .= String "stale", "pid" .= Null, "cols" .= (80 :: Int), "rows" .= (24 :: Int), "createdAt" .= String "2026-09-16T00:00:00Z"])
        State.sessionTerminals cleared @?= []
        State.acknowledgeTerminalBuffer "main" claim fresh @?= fresh
        State.restoreSessionTerminals ticket [stale] restoring @?= restoring
        assertBool "Counter reset must expose the stale buffer claim" (State.acknowledgeTerminalBuffer "main" claim unsafeFresh /= unsafeFresh)
        assertBool "Counter reset must expose the stale restoration" (State.restoreSessionTerminals ticket [stale] unsafeRestoring /= unsafeRestoring),
      testCase "unknown touches do not create entries while future active selection can pin one" $ bounded $ withCachePeer $ \connection peer -> do
        Daemon.touchSession connection "unknown" >>= (@?= False)
        Daemon.removeCachedSession connection "unknown" >>= (@?= False)
        forM_ ["", " \t\n"] $ \identifier -> do
          try @Daemon.DaemonError (Daemon.touchSession connection identifier) >>= (@?= Left Daemon.InvalidSessionCacheIdentity)
          try @Daemon.DaemonError (Daemon.removeCachedSession connection identifier) >>= (@?= Left Daemon.InvalidSessionCacheIdentity)
          try @Daemon.DaemonError (Daemon.setActiveSessionId connection (Just identifier)) >>= (@?= Left Daemon.InvalidSessionCacheIdentity)
        Daemon.getCachedSessionIds connection >>= (@?= [])
        drain peer >>= (@?= [])
        void (Daemon.setSessionCacheCapacity connection (Just 0))
        void (Daemon.setActiveSessionId connection (Just "future"))
        Daemon.getActiveSessionId connection >>= (@?= Just "future")
        load connection "future"
        Daemon.getCachedSessionIds connection >>= (@?= ["future"])
        Daemon.setActiveSessionId connection Nothing >>= (@?= ["future"]),
      testCase "a retired attachment protects admitted work even after a newer lease detaches" $ bounded $ withCachePeer $ \connection peer -> do
        void (Daemon.setSessionCacheCapacity connection (Just 0))
        Daemon.withSessionOn connection (creationOptions "same") $ \session -> do
          void (drain peer)
          entered <- newEmptyTMVarIO
          release <- newEmptyTMVarIO
          let callback _ = uninterruptibleMask_ (atomically (putTMVar entered ()) >> atomically (readTMVar release))
              unblock = atomically (void (tryPutTMVar release ()))
              reattach =
                Daemon.withResumedSessionOn connection "same" (const (pure ())) `catch` \cause ->
                  if cause == Daemon.DaemonSessionAlreadyAttached then threadDelay 1000 >> reattach else throwIO cause
          withAsync (Daemon.sendPrompt session "held" callback) $ \worker ->
            ( do
                void (nextRequest peer "daemon.add_user_message" "same")
                notify peer "same" (object ["type" .= String "assistant_text_delta", "messageId" .= String "answer", "blockIndex" .= (0 :: Int), "textDelta" .= String "held"])
                atomically (readTMVar entered)
                notify peer "same" (object ["type" .= String "session_closed"])
                reattach
                Daemon.getCachedSessionIds connection >>= (@?= ["same"])
                Daemon.removeCachedSession connection "same" >>= (@?= False)
                unblock
                waitCatch worker >>= \case Left cause -> fromException cause @?= Just DroidSessionUnusable; Right _ -> assertFailure "Closed turn completed normally"
                Daemon.getCachedSessionIds connection >>= (@?= [])
            )
              `finally` unblock
    ]

creationOptions :: Text -> Daemon.DaemonSessionOptions
creationOptions identifier =
  let options = Daemon.defaultDaemonSessionOptions "/created"
      initial = Daemon.daemonSessionParameters options
   in options {Daemon.daemonSessionParameters = initial {initializeConfiguration = defaultSessionConfiguration {configurationSessionId = Just identifier}}}

load :: Daemon.DaemonConnection -> Text -> IO ()
load connection = void . Daemon.loadSessionInfo connection

rejected :: IO () -> IO ()
rejected action =
  try @RpcResultError action >>= \case
    Left (RpcRemoteFailure failure) -> rpcErrorCode failure @?= RpcConflict
    _ -> assertFailure "Expected a remote rejection"

data Rule = Hold | Reject

data CachePeer = CachePeer
  { peerIncoming :: TQueue Object,
    peerRequests :: TQueue Object,
    peerRules :: TVar (Map (Text, Text) Rule),
    peerWorking :: TVar (Map Text Text)
  }

withCachePeer :: (Daemon.DaemonConnection -> CachePeer -> IO a) -> IO a
withCachePeer action = do
  peer <- CachePeer <$> newTQueueIO <*> newTQueueIO <*> newTVarIO mempty <*> newTVarIO mempty
  let send frame = do
        atomically (writeTQueue (peerRequests peer) frame)
        let method = text "method" frame
            identifier = case field "sessionId" (params frame) of String value -> value; _ -> ""
        case method of
          "daemon.get_proxy_token" -> feed peer (reply (text "id" frame) (object ["token" .= String "fixture"]))
          "daemon.add_user_message" -> feed peer (reply (text "id" frame) (object []))
          "daemon.update_session_settings" -> feed peer (reply (text "id" frame) (object []))
          _ | method `elem` ["daemon.load_session", "daemon.initialize_session"] -> do
            rule <- Map.lookup (method, identifier) <$> readTVarIO (peerRules peer)
            case rule of Just Hold -> pure (); Just Reject -> reject peer frame; Nothing -> respond peer frame
          _ -> assertFailure "Unexpected cache fixture request"
      options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication (GetUserInfoResult "user" "org" mempty) "") "/offline") {Daemon.daemonClientRestoreTerminalsOnLoad = False}
  Daemon.withConnectionOn options (objectTransport send (atomically (readTQueue (peerIncoming peer)))) (`action` peer)

setRule :: CachePeer -> Text -> Text -> Rule -> IO ()
setRule peer method identifier rule = atomically (modifyTVar' (peerRules peer) (Map.insert (method, identifier) rule))

respond :: CachePeer -> Object -> IO ()
respond peer request = do
  let identifier = text "sessionId" (params request)
  working <- Map.findWithDefault "idle" identifier <$> readTVarIO (peerWorking peer)
  let message = object ["id" .= ("message-" <> identifier), "role" .= String "assistant", "content" .= [object ["type" .= String "text", "text" .= ("payload-" <> identifier)]], "createdAt" .= Number 0, "updatedAt" .= Number 0]
  feed peer (reply (text "id" request) (object ["sessionId" .= identifier, "session" .= object ["messages" .= [message]], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"], "cwd" .= ("/saved/" <> identifier), "workingState" .= working, "isAgentLoopInProgress" .= (working /= "idle")]))

reject :: CachePeer -> Object -> IO ()
reject peer request = feed peer (KeyMap.insert "error" (toJSON (JsonRpcError RpcConflict "fixture rejection" Nothing mempty)) (reply (text "id" request) Null))

notify :: CachePeer -> Text -> Value -> IO ()
notify peer identifier event = feed peer (asObject (object ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= event]]))

feed :: CachePeer -> Object -> IO ()
feed peer = atomically . writeTQueue (peerIncoming peer)

drain :: CachePeer -> IO [Object]
drain = atomically . flushTQueue . peerRequests

nextRequest :: CachePeer -> Text -> Text -> IO Object
nextRequest peer method identifier = do
  request <- atomically (readTQueue (peerRequests peer))
  field "method" request @?= String method
  field "sessionId" (params request) @?= String identifier
  pure request

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

text :: Key -> Object -> Text
text key value = case field key value of String result -> result; _ -> error "Expected fixture text"

params :: Object -> Object
params = asObject . field "params"

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = mempty

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error message -> assertFailure message

bounded :: IO a -> IO a
bounded action = timeout 10000000 action >>= maybe (assertFailure "Cache fixture timed out") pure
