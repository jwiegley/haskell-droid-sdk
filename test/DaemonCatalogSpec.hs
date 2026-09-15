{-# LANGUAGE OverloadedStrings #-}

module DaemonCatalogSpec (catalogTests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Exception (catch, finally, fromException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Daemon.Session
import Factory.Droid.Schema.Enums (MessageRole (..))
import Factory.Droid.Schema.Messages (messageId, messageRole)
import Factory.Droid.Schema.Mission (MissionPhase (..))
import Factory.Droid.Schema.Notifications (DroidWorkingState (..))
import Factory.Droid.Schema.RPC (JsonRpcErrorCode (RpcInvalidParams), SuccessResult (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

catalogTests :: Value -> TestTree
catalogTests schemas = testGroup "Daemon session catalog" (catalogCases schemas)

catalogCases :: Value -> [TestTree]
catalogCases schemas =
  [ testCase "six operations use authenticated sessionless requests with exact defaults and filters" $ bounded $ do
      trace <- newIORef []
      escaped <- withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        opened <- Daemon.listOpenedSessions connection defaultListOpenedSessionsParams
        map openedSessionId (openedSessions opened) @?= ["live"]
        map openedWorkingState (openedSessions opened) @?= [WorkingIdle]
        limit <- maybe (assertFailure "Limit fixture") pure (mkSessionPageLimit 2.5)
        let filterValue = AvailableSessionFilter (SessionListFilter (Just False) (Just False) mempty) (Just [MissionPaused])
        available <- Daemon.listAvailableSessions connection (defaultListAvailableSessionsParams {availableSessionsLimit = limit, availableEndBeforeSeconds = Just 10.25, availableIncludeArchived = Just False, availableIncludeMissionMetadata = Just True, availableSessionsFilter = Just filterValue})
        map availableSessionId (availableSessions available) @?= ["saved"]
        availableSessionsHasMore available @?= False
        availableSessionsNextCursorSeconds available @?= Just 10.25
        history <- Daemon.getSessionMessages connection ((defaultGetSessionMessagesParams "other-session") {messagesRole = Just HistoryTool, messagesCursor = Just ""})
        map messageId (catalogMessages history) @?= ["message"]
        map messageRole (catalogMessages history) @?= [RoleTool]
        catalogMessagesNextCursor history @?= Just ""
        found <- Daemon.searchSessions connection ((defaultSearchSessionsParams "needle") {searchSessionsKind = Just SearchAll, searchLimitSessions = Just 0, searchLimitHitsPerSession = Just 2.5, searchContextChars = Just (-1), searchUpdatedAfterMs = Just 10001, searchUpdatedBeforeMs = Just 20002})
        searchedQuery found @?= "needle"
        map searchResultUpdatedAtMs (searchedSessions found) @?= [Just 10001.5]
        map searchHitKind (concatMap searchResultHits (searchedSessions found)) @?= [SearchToolResult]
        result <- Daemon.archiveSession connection ((defaultArchiveSessionParams "other-session") {archiveForce = Just False})
        archiveSuccess result @?= False
        archiveTimestamp result @?= "opaque-archive-time"
        Daemon.unarchiveSession connection "other-session" >>= (@?= False) . resultSuccess
        pure connection
      expectChannel RpcChannelClosed (Daemon.listOpenedSessions escaped defaultListOpenedSessionsParams)
      frames <- readIORef trace
      map (field "method") frames @?= map String ["daemon.authenticate", "daemon.list_opened_sessions", "daemon.list_available_sessions", "daemon.get_session_messages", "daemon.search_sessions", "daemon.archive_session", "daemon.unarchive_session"]
      let ids = map (field "id") frames
      length (nub ids) @?= length ids
      map params (drop 1 frames) @?= [mempty, KeyMap.fromList ["limit" .= (2.5 :: Scientific), "endBefore" .= (10.25 :: Scientific), "includeArchived" .= False, "includeMissionMetadata" .= True, "filter" .= object ["missionSessions" .= False, "includeBtwForks" .= False, "missionStates" .= [String "paused"]]], KeyMap.fromList ["sessionId" .= String "other-session", "limit" .= (20 :: Int), "cursor" .= String "", "role" .= String "tool"], KeyMap.fromList ["query" .= String "needle", "kind" .= String "all", "limitSessions" .= (0 :: Int), "limitHitsPerSession" .= (2.5 :: Scientific), "contextChars" .= (-1 :: Int), "updatedAfterMs" .= (10001 :: Int), "updatedBeforeMs" .= (20002 :: Int)], KeyMap.fromList ["sessionId" .= String "other-session", "force" .= False], KeyMap.singleton "sessionId" (String "other-session")],
    testCase "catalog defaults and bounded fractional limits match the supplied operation schemas" $ do
      decoded @ListAvailableSessionsParams (object []) >>= (@?= defaultListAvailableSessionsParams)
      decoded @GetSessionMessagesParams (object ["sessionId" .= String "saved"]) >>= (@?= defaultGetSessionMessagesParams "saved")
      forM_ [0, 100.1, -1] $ \limit -> do
        mkSessionPageLimit limit @?= Nothing
        rejects (Proxy @ListAvailableSessionsParams) (object ["limit" .= limit])
      forM_ [1, 2.5, 100] $ \limit -> case mkSessionPageLimit limit of Nothing -> assertFailure "Valid limit rejected"; Just valid -> toJSON valid @?= Number limit
      rejects (Proxy @ListAvailableSessionsParams) (object ["limit" .= Null])
      rejects (Proxy @GetSessionMessagesParams) (object ["sessionId" .= String "saved", "role" .= String "system"])
      forM_ [("DaemonListOpenedSessionsRequestSchema", "daemon.list_opened_sessions"), ("DaemonListAvailableSessionsRequestSchema", "daemon.list_available_sessions"), ("DaemonGetSessionMessagesRequestSchema", "daemon.get_session_messages"), ("DaemonSearchSessionsRequestSchema", "daemon.search_sessions"), ("DaemonArchiveSessionRequestSchema", "daemon.archive_session"), ("DaemonUnarchiveSessionRequestSchema", "daemon.unarchive_session")] $ \(name, method) ->
        (schemaAt ["definitions", name, "allOf"] schemas >>= schemaIndex 1 >>= schemaAt ["properties", "method", "const"]) @?= Right (String method)
      toJSON (defaultListAvailableSessionsParams {availableSessionsParamsAdditionalFields = KeyMap.fromList ["limit" .= (200 :: Int), "includeArchived" .= True]}) @?= object ["limit" .= (50 :: Int)]
      toJSON ((defaultGetSessionMessagesParams "saved") {messagesParamsAdditionalFields = KeyMap.fromList ["sessionId" .= String "injected", "role" .= String "system"]}) @?= object ["sessionId" .= String "saved", "limit" .= (20 :: Int)],
    testCase "catalog normalization is field-local and preserves typed metadata and extensions" $ do
      opened <- decoded @OpenedSessionInfo openedValue
      openedUpdatedAtSeconds opened @?= 10.25
      assertBool "Worktree was not decoded" (isJust (openedWorktree opened))
      available <- decoded @AvailableSessionInfo availableValue
      availableOrganizationId available @?= Just "org"
      case availableMission available of
        Just mission -> do
          catalogMissionState mission @?= MissionPaused
          catalogMissionTitle mission @?= Nothing
          catalogMissionElapsedMs mission @?= Nothing
        Nothing -> assertFailure "Missing mission metadata"
      case openedValue of
        Object values -> do
          forM_ [Null, Bool False, object ["repoRoot" .= False]] $ \bad -> decoded @OpenedSessionInfo (Object (KeyMap.insert "worktree" bad values)) >>= (@?= Nothing) . openedWorktree
          rejects (Proxy @OpenedSessionInfo) (Object (KeyMap.insert "updatedAt" Null values))
          rejects (Proxy @OpenedSessionInfo) (Object (KeyMap.insert "workingState" (String "future") values))
        _ -> assertFailure "Invalid fixture"
      case availableValue of
        Object values -> do
          rejects (Proxy @AvailableSessionInfo) (Object (KeyMap.insert "title" Null values))
          rejects (Proxy @AvailableSessionInfo) (Object (KeyMap.insert "mission" (object ["state" .= String "paused", "title" .= False]) values))
        _ -> assertFailure "Invalid fixture"
      toJSON opened @?= openedValue
      show opened @?= "OpenedSessionInfo <redacted>"
      show available @?= "AvailableSessionInfo <redacted>"
      found <- decoded @SearchSessionsResult searchValue
      show found @?= "SearchSessionsResult <redacted>"
      toJSON found @?= searchValue,
    testCase "mission null normalization does not weaken other catalog fields" $ do
      let names = ["title", "workingDirectory", "createdAt", "updatedAt", "elapsedMs", "completedFeatures", "totalFeatures"]
          normalized = object ["state" .= String "paused"]
      forM_ names $ \name -> do
        mission <- decoded @CatalogMission (object ["state" .= String "paused", name .= Null])
        toJSON mission @?= normalized
        rejects (Proxy @CatalogMission) (object ["state" .= String "paused", name .= Bool False])
      forM_ ["sessionId", "updatedAt", "workingState"] $ \name -> case openedValue of
        Object fields -> rejects (Proxy @OpenedSessionInfo) (Object (KeyMap.delete name fields))
        _ -> assertFailure "Invalid fixture"
      case availableValue of
        Object fields -> do
          normalizedWorktree <- decoded @AvailableSessionInfo (Object (KeyMap.insert "worktree" Null fields))
          availableWorktree normalizedWorktree @?= Nothing
          rejects (Proxy @AvailableSessionInfo) (Object (KeyMap.insert "hostId" (String "not-a-uuid") fields))
        _ -> assertFailure "Invalid fixture",
    testCase "search variants and archive-state absence retain their distinct contracts" $ do
      forM_ [(SearchMessageText, "message_text"), (SearchDocument, "document"), (SearchToolUse, "tool_use"), (SearchToolResult, "tool_result")] $ \(kind, literal) -> do
        hit <- decoded @SessionSearchHit (object ["docId" .= String "doc", "kind" .= String literal, "snippets" .= ([] :: [Value])])
        searchHitKind hit @?= kind
        toJSON (SearchOnly kind) @?= String literal
      rejects (Proxy @SessionSearchHit) (object ["docId" .= String "doc", "kind" .= String "all", "snippets" .= ([] :: [Value])])
      rejects (Proxy @SessionSearchHit) (object ["docId" .= String "doc", "kind" .= String "message_text", "messageRole" .= String "tool", "snippets" .= ([] :: [Value])])
      event <- decoded @SessionArchiveStateChanged (object ["sessionId" .= String "saved", "cwd" .= String "/cwd", "repoRoot" .= String "/repo", "title" .= String "title", "worktreeRemoved" .= False])
      archiveChangedTimestamp event @?= Nothing
      archiveChangedCwd event @?= Just "/cwd"
      archiveChangedRepoRoot event @?= Just "/repo"
      archiveChangedWorktreeRemoved event @?= Just False
      rejects (Proxy @SessionArchiveStateChanged) (object ["sessionId" .= String "saved", "archivedAt" .= Null]),
    testCase "available filters cannot inject an omitted mission-state restriction" $ do
      let base = defaultSessionListFilter {sessionFilterAdditionalFields = KeyMap.singleton "missionStates" (toJSON [String "running"])}
      toJSON (AvailableSessionFilter base Nothing) @?= object []
      toJSON (AvailableSessionFilter base (Just [])) @?= object ["missionStates" .= ([] :: [Value])],
    testCase "errors and malformed results leave unrelated connection queries available" $ bounded $ do
      trace <- newIORef []
      withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        failure <- try @RpcResultError (Daemon.searchSessions connection (defaultSearchSessionsParams "reject"))
        case failure of Left (RpcRemoteFailure _) -> pure (); _ -> assertFailure "Missing remote error"
        bad <- try @RpcResultError (Daemon.getSessionMessages connection (defaultGetSessionMessagesParams "bad-result"))
        bad @?= Left RpcInvalidResult
        Daemon.listOpenedSessions connection defaultListOpenedSessionsParams >>= (@?= 1) . length . openedSessions,
    testCase "archive observation is connection-wide, reentrant and explicitly unsubscribed" $ bounded $ do
      trace <- newIORef []
      withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        seen <- newIORef []
        first <- newEmptyMVar
        _ <- Daemon.onArchiveStateChanged connection (\_ -> ioError (userError "ordinary callback failure"))
        stop <- Daemon.onArchiveStateChanged connection $ \event -> do
          result <- Daemon.listOpenedSessions connection defaultListOpenedSessionsParams
          length (openedSessions result) @?= 1
          modifyIORef' seen (<> [event])
          putMVar first ()
        void (Daemon.archiveSession connection (defaultArchiveSessionParams "event"))
        takeMVar first
        values <- readIORef seen
        case values of [Right value] -> archiveChangedSessionId value @?= "foreign-saved-session"; _ -> assertFailure "Missing connection-wide event"
        stop
        stop
        barrier <- newEmptyMVar
        _ <- Daemon.onArchiveStateChanged connection (const (putMVar barrier ()))
        void (Daemon.unarchiveSession connection "event")
        takeMVar barrier
        readIORef seen >>= (@?= values),
    testCase "concurrent searches correlate reversed responses without serializing calls" $ bounded $ do
      trace <- newIORef []
      withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        ready <- newEmptyMVar
        _ <- Daemon.onArchiveStateChanged connection (const (putMVar ready ()))
        withAsync (Daemon.searchSessions connection (defaultSearchSessionsParams "pair-first")) $ \first -> do
          takeMVar ready
          withAsync (Daemon.searchSessions connection (defaultSearchSessionsParams "pair-second")) $ \second -> do
            wait second >>= (@?= "pair-second") . searchedQuery
            wait first >>= (@?= "pair-first") . searchedQuery
      frames <- readIORef trace
      length (nub (map (field "id") frames)) @?= length frames,
    testCase "malformed archive events are explicit errors without poisoning valid replies" $ bounded $ do
      trace <- newIORef []
      withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        seen <- newEmptyMVar
        _ <- Daemon.onArchiveStateChanged connection (putMVar seen)
        void (Daemon.archiveSession connection (defaultArchiveSessionParams "bad-event"))
        takeMVar seen >>= (@?= Left Daemon.InvalidDaemonEvent)
        Daemon.listAvailableSessions connection defaultListAvailableSessionsParams >>= (@?= False) . availableSessionsHasMore,
    testCase "cancelling a pending query preserves async identity and connection reuse" $ bounded $ do
      trace <- newIORef []
      withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        ready <- newEmptyMVar
        _ <- Daemon.onArchiveStateChanged connection (const (putMVar ready ()))
        withAsync (Daemon.searchSessions connection (defaultSearchSessionsParams "held")) $ \pending -> do
          takeMVar ready
          cancel pending
          waitCatch pending >>= \case Left err -> fromException err @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled query returned"
        Daemon.listOpenedSessions connection defaultListOpenedSessionsParams >>= (@?= 1) . length . openedSessions,
    testCase "connection cleanup cancels and finalizes admitted archive callbacks" $ bounded $ do
      trace <- newIORef []
      started <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      withCatalogPeer trace $ \target -> do
        Daemon.withConnection (options target) $ \connection -> do
          _ <- Daemon.onArchiveStateChanged connection (\_ -> (putMVar started () >> takeMVar hold) `finally` putMVar finished ())
          void (Daemon.archiveSession connection (defaultArchiveSessionParams "event"))
          takeMVar started
        takeMVar finished,
    testCase "channel failure is observed and rejects further catalog operations" $ bounded $ do
      trace <- newIORef []
      withCatalogPeer trace $ \target -> Daemon.withConnection (options target) $ \connection -> do
        failed <- newEmptyMVar
        _ <- Daemon.onArchiveStateChanged connection $ \case Left event -> putMVar failed event; Right _ -> pure ()
        expectChannel RpcChannelReadFailure (Daemon.searchSessions connection (defaultSearchSessionsParams "disconnect"))
        takeMVar failed >>= \case Daemon.DaemonEventConnectionError err -> err @?= RpcChannelReadFailure; _ -> assertFailure "Missing channel failure"
        expectChannel RpcChannelReadFailure (Daemon.listOpenedSessions connection defaultListOpenedSessionsParams)
  ]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withCatalogPeer :: IORef [Object] -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withCatalogPeer trace = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      value <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC JSON")) pure . eitherDecode
      field "factoryProtocolVersion" value @?= String "1.201.1"
      field "factoryApiVersion" value @?= String "1.0.0"
      field "jsonrpc" value @?= String "2.0"
      field "type" value @?= String "request"
      modifyIORef' trace (<> [value])
      pure value
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "apiKey" (params auth) @?= String "OFFLINE_ONLY"
      respond connection auth (object ["userId" .= String "fixture-user", "orgId" .= String "fixture-org"])
      forever $ do
        frame <- readFrame connection
        case field "method" frame of
          String "daemon.list_opened_sessions" -> respond connection frame (object ["sessions" .= [openedValue]])
          String "daemon.list_available_sessions" -> respond connection frame (object ["sessions" .= [availableValue], "hasMore" .= False, "nextCursor" .= (10.25 :: Scientific)])
          String "daemon.get_session_messages" -> respond connection frame (object ["messages" .= [if field "sessionId" (params frame) == String "bad-result" then object [] else messageValue], "hasMore" .= False, "nextCursor" .= String ""])
          String "daemon.search_sessions" -> case field "query" (params frame) of
            String "reject" -> sendFrame connection ["type" .= String "response", "id" .= field "id" frame, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "fixture denied"]]
            String "pair-first" -> do
              notify connection (object ["sessionId" .= String "paired"])
              second <- readFrame connection
              field "query" (params second) @?= String "pair-second"
              respond connection second (object ["query" .= String "pair-second", "sessions" .= ([] :: [Value])])
              respond connection frame (object ["query" .= String "pair-first", "sessions" .= ([] :: [Value])])
            String "held" -> notify connection (object ["sessionId" .= String "held"])
            String "disconnect" -> WS.sendClose connection ("closed" :: Text)
            _ -> respond connection frame searchValue
          String "daemon.archive_session" -> do
            case field "sessionId" (params frame) of
              String "event" -> notify connection archiveEvent
              String "bad-event" -> notify connection (Bool False)
              _ -> pure ()
            respond connection frame (object ["success" .= False, "archivedAt" .= String "opaque-archive-time"])
          String "daemon.unarchive_session" -> do
            when (field "sessionId" (params frame) == String "event") $ notify connection (object ["sessionId" .= String "foreign-saved-session"])
            respond connection frame (object ["success" .= False, "x-extra" .= True])
          _ -> assertFailure "Unexpected method or implicit session initialization"

respond :: WS.Connection -> Object -> Value -> IO ()
respond connection request result = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= result]

notify :: WS.Connection -> Value -> IO ()
notify connection value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session.archive_state_changed", "params" .= value]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

field :: Key -> Object -> Value
field key value = fromMaybe Null (KeyMap.lookup key value)

params :: Object -> Object
params frame = case field "params" frame of Object values -> values; _ -> mempty

decoded :: (FromJSON a) => Value -> IO a
decoded value = case fromJSON value of Success result -> pure result; Error _ -> assertFailure "Invalid fixture"

expectChannel :: RpcChannelError -> IO a -> IO ()
expectChannel expected action = try @RpcChannelError action >>= \case Left err -> err @?= expected; Right _ -> assertFailure "Expected channel failure"

openedValue :: Value
openedValue = object ["sessionId" .= String "live", "updatedAt" .= (10.25 :: Scientific), "workingState" .= String "idle", "hostId" .= String "550e8400-e29b-41d4-a716-446655440000", "cwd" .= String "/daemon/cwd", "repoRoot" .= String "/daemon/worktree", "messagesCount" .= (2.5 :: Scientific), "callingSessionId" .= String "parent", "callingToolUseId" .= String "tool", "tags" .= [object ["name" .= String "custom"]], "worktree" .= object ["repoRoot" .= String "/repo", "branch" .= String "topic"], "x-extra" .= False]

availableValue :: Value
availableValue = object ["sessionId" .= String "saved", "updatedAt" .= (10.25 :: Scientific), "title" .= String "SECRET_FIXTURE", "organizationId" .= String "org", "worktree" .= object ["repoRoot" .= String "/repo"], "mission" .= object ["state" .= String "paused", "title" .= Null, "elapsedMs" .= Null, "completedFeatures" .= (0 :: Int), "totalFeatures" .= (1 :: Int)], "x-extra" .= True]

messageValue :: Value
messageValue = object ["id" .= String "message", "role" .= String "tool", "content" .= [object ["type" .= String "text", "text" .= String "SECRET_FIXTURE"]], "createdAt" .= (1 :: Int), "updatedAt" .= (2 :: Int)]

searchValue :: Value
searchValue = object ["query" .= String "needle", "sessions" .= [object ["sessionId" .= String "saved", "title" .= String "SECRET_FIXTURE", "updatedAt" .= (10001.5 :: Scientific), "hits" .= [object ["docId" .= String "doc", "kind" .= String "tool_result", "score" .= (0.5 :: Scientific), "toolName" .= String "Read", "messageRole" .= String "assistant", "snippets" .= [String "SECRET_FIXTURE"]]], "totals" .= object ["byKind" .= object ["tool_result" .= (1 :: Int)], "toolUse" .= object [], "toolResult" .= object ["Read" .= (1 :: Int)]], "x-extra" .= True]]]

archiveEvent :: Value
archiveEvent = object ["sessionId" .= String "foreign-saved-session", "archivedAt" .= String "opaque-time", "title" .= String "saved", "worktreeRemoved" .= False]
