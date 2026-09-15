{-# LANGUAGE OverloadedStrings #-}

module DaemonAttachmentSpec (attachmentTests) where

import Control.Concurrent (MVar, myThreadId, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (catch, finally, fromException, try)
import Control.Monad (forM_, forever, replicateM, void)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidSessionUnusable), DroidEvent (SettingsUpdatedEvent), DroidResult (resultText))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Interaction (DroidHandlers (..), cancelDroidQuestions, defaultDroidHandlers)
import Factory.Droid.Protocol (RpcChannelError (RpcChannelClosed), RpcResultError (..))
import Factory.Droid.Schema.Control (GetRewindInfoParams (..))
import Factory.Droid.Schema.Daemon.Management (ProxyTokenResult (proxyToken))
import Factory.Droid.Schema.Daemon.Session (defaultDaemonCloseSessionParams)
import Factory.Droid.Schema.Interaction (cancelPermissionResult)
import Factory.Droid.Schema.RPC (JsonRpcError (..))
import Factory.Droid.Schema.Settings (SessionSettings (settingsModel))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

attachmentTests :: TestTree
attachmentTests =
  testGroup
    "Borrowed daemon session attachments"
    [ testCase "two sessions share authentication but retain settings and independent detach lifetimes" $ bounded $ do
        (escaped, trace) <- withAttachmentPeer $ \connection _ -> do
          Daemon.withResumedSessionOn connection "one" $ \one ->
            Daemon.withResumedSessionOn connection "two" $ \two -> do
              Daemon.getSettings one >>= (@?= "one") . settingsModel
              Daemon.getSettings two >>= (@?= "two") . settingsModel
              Daemon.detachSession one
              Daemon.detachSession one
              try @DroidError (Daemon.getSettings one) >>= (@?= Left DroidSessionUnusable)
              Daemon.getSettings two >>= (@?= "two") . settingsModel
              Daemon.getProxyToken (Daemon.sessionConnection two) >>= (@?= "OFFLINE_TOKEN") . proxyToken
          Daemon.getProxyToken connection >>= (@?= "OFFLINE_TOKEN") . proxyToken
          pure (Daemon.getProxyToken connection)
        try @RpcChannelError escaped >>= (@?= Left RpcChannelClosed)
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.load_session", "daemon.list_terminals", "daemon.get_proxy_token", "daemon.get_proxy_token"],
      testCase "duplicate attachment rejects before load and stale detach cannot remove a newer lease" $ bounded $ do
        (_, trace) <- withAttachmentPeer $ \connection _ ->
          Daemon.withResumedSessionOn connection "one" $ \old -> do
            try @Daemon.DaemonError (Daemon.withResumedSessionOn connection "one" (\_ -> pure ())) >>= (@?= Left Daemon.DaemonSessionAlreadyAttached)
            Daemon.detachSession old
            Daemon.withResumedSessionOn connection "one" $ \fresh -> do
              Daemon.detachSession old
              Daemon.getSettings fresh >>= (@?= "one") . settingsModel
              try @Daemon.DaemonError (Daemon.withResumedSessionOn connection "one" (\_ -> pure ())) >>= (@?= Left Daemon.DaemonSessionAlreadyAttached)
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.load_session", "daemon.list_terminals"],
      testCase "detaching a subscribed handle blocks later admission without removing a replacement's observer" $ bounded $ do
        void $ withAttachmentPeer $ \connection _ ->
          Daemon.withResumedSessionOn connection "one" $ \old -> do
            oldCalls <- newIORef (0 :: Int)
            stopOld <- Daemon.onSessionEvent old (\_ -> modifyIORef' oldCalls (+ 1))
            Daemon.detachSession old
            Daemon.withResumedSessionOn connection "one" $ \fresh -> do
              seen <- newEmptyMVar
              stopFresh <- Daemon.onSessionEvent fresh $ \case
                Right (SettingsUpdatedEvent _) -> Daemon.getSettings fresh >>= putMVar seen . settingsModel
                Left cause -> assertFailure (show cause)
                _ -> pure ()
              stopOld
              void (Daemon.getRewindInfo connection (GetRewindInfoParams "one" "settings" mempty))
              takeMVar seen >>= (@?= "one-updated")
              readIORef oldCalls >>= (@?= 0)
              stopFresh,
      testCase "a remote session close retires only that handle and keeps other ordered observations live" $
        bounded $
          forM_ [False, True] $ \ownedClose ->
            void $ withAttachmentPeer $ \connection _ ->
              Daemon.withResumedSessionOn connection "one" $ \one ->
                Daemon.withResumedSessionOn connection "two" $ \two -> do
                  seen <- newEmptyMVar
                  stop <- Daemon.onSessionEvent two $ \case
                    Right (SettingsUpdatedEvent _) -> Daemon.getSettings two >>= putMVar seen . settingsModel
                    Left cause -> assertFailure (show cause)
                    _ -> pure ()
                  if ownedClose
                    then void (Daemon.closeAttachedSession one)
                    else void (Daemon.closeSession connection (defaultDaemonCloseSessionParams "one"))
                  takeMVar seen >>= (@?= "two-updated")
                  try @DroidError (Daemon.getSettings one) >>= (@?= Left DroidSessionUnusable)
                  readiness <- Daemon.getSessionReadiness connection "one"
                  Daemon.readinessPhase readiness @?= Daemon.SessionNotLoaded
                  Daemon.readinessKnown readiness @?= False
                  Daemon.getSettings two >>= (@?= "two-updated") . settingsModel
                  stop,
      testCase "owned close retires its handle after a reply even without a lifecycle notice" $ bounded $ do
        (_, trace) <- withAttachmentPeer $ \connection _ ->
          Daemon.withResumedSessionOn connection "quiet" $ \quiet ->
            Daemon.withResumedSessionOn connection "two" $ \two -> do
              Daemon.closeAttachedSession quiet >>= (@?= mempty)
              try @DroidError (Daemon.getSettings quiet) >>= (@?= Left DroidSessionUnusable)
              try @DroidError (Daemon.closeAttachedSession quiet) >>= (@?= Left DroidSessionUnusable)
              Daemon.getSettings two >>= (@?= "two") . settingsModel
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.load_session", "daemon.list_terminals", "daemon.close_session"],
      testCase "turns are independent, callbacks stay on the caller thread, and detach does not interrupt remotely" $ bounded $ do
        (_, trace) <- withAttachmentPeer $ \connection fixture ->
          Daemon.withResumedSessionOn connection "one" $ \one ->
            Daemon.withResumedSessionOn connection "two" $ \two ->
              withAsync (try @DroidError (Daemon.sendPrompt one "held-turn" (\_ -> pure ()))) $ \pending -> do
                takeMVar (peerTurnStarted fixture)
                caller <- myThreadId
                result <- Daemon.sendPrompt two "complete" (\_ -> myThreadId >>= (@?= caller))
                resultText result @?= "two-result"
                Daemon.detachSession one
                wait pending >>= (@?= Left DroidSessionUnusable)
                Daemon.getSettings two >>= (@?= "two") . settingsModel
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.load_session", "daemon.list_terminals", "daemon.add_user_message", "daemon.add_user_message"],
      testCase "handler routing prefers owners, questions stay exact, and detach joins only its blocked callback" $ bounded $ do
        void $ withAttachmentPeer $ \connection fixture -> do
          entered <- newEmptyMVar
          finished <- newEmptyMVar
          never <- newEmptyMVar
          secondCalls <- newIORef (0 :: Int)
          questions <- newIORef (0 :: Int)
          let firstHandlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> (putMVar entered () >> takeMVar never) `finally` putMVar finished ()), onDroidQuestion = Just (\_ -> modifyIORef' questions (+ 1) >> pure cancelDroidQuestions)}
              secondHandlers = defaultDroidHandlers {onDroidPermission = Just (\_ -> modifyIORef' secondCalls (+ 1) >> Daemon.getProxyToken connection >> pure cancelPermissionResult)}
          Daemon.withResumedSessionOnHandlers connection firstHandlers "one" $ \one ->
            Daemon.withResumedSessionOnHandlers connection secondHandlers "two" $ \two -> do
              void (Daemon.getRewindInfo connection (GetRewindInfoParams "one" "permissions" mempty))
              takeMVar entered
              Daemon.detachSession one
              takeMVar finished
              responses <- replicateM 4 (atomically (readTQueue (peerReplies fixture)))
              forM_ [("one-permission", "one"), ("two-permission", "two"), ("worker-permission", "worker")] $ \(identifier, execution) -> do
                response <- maybe (assertFailure "Missing permission reply") pure (find ((== String identifier) . field "id") responses)
                field "result" response @?= object ["sessionId" .= String execution, "selectedOption" .= String "cancel"]
              question <- maybe (assertFailure "Missing question reply") pure (find ((== String "worker-question") . field "id") responses)
              field "result" question @?= object ["sessionId" .= String "worker", "answers" .= ([] :: [Value]), "cancelled" .= True]
              readIORef secondCalls >>= (@?= 2)
              readIORef questions >>= (@?= 0)
              Daemon.getSettings two >>= (@?= "two") . settingsModel,
      testCase "cancelled pending loads release only their lease and late replies cannot replace a fresh baseline" $ bounded $ do
        (_, trace) <- withAttachmentPeer $ \connection fixture ->
          Daemon.withResumedSessionOn connection "one" $ \one -> do
            published <- newIORef False
            withAsync (Daemon.withResumedSessionOn connection "held" (\_ -> writeIORef published True)) $ \loading -> do
              takeMVar (peerLoadStarted fixture)
              try @Daemon.DaemonError (Daemon.withResumedSessionOn connection "held" (\_ -> pure ())) >>= (@?= Left Daemon.DaemonSessionAlreadyAttached)
              cancel loading
              waitCatch loading >>= \case
                Left cause -> fromException cause @?= Just AsyncCancelled
                Right _ -> assertFailure "Cancelled load published a handle"
            readIORef published >>= (@?= False)
            Daemon.withResumedSessionOn connection "held" $ \fresh -> do
              Daemon.getSettings fresh >>= (@?= "held") . settingsModel
              Daemon.getSettings one >>= (@?= "one") . settingsModel
        methods trace @?= ["daemon.authenticate", "daemon.load_session", "daemon.list_terminals", "daemon.load_session", "daemon.load_session", "daemon.list_terminals"],
      testCase "load rejection releases the attachment key without poisoning another session" $ bounded $ do
        void $ withAttachmentPeer $ \connection _ ->
          Daemon.withResumedSessionOn connection "one" $ \one -> do
            forM_ [1 :: Int, 2] $ \_ ->
              try @RpcResultError (Daemon.withResumedSessionOn connection "rejected" (\_ -> pure ())) >>= \case
                Left (RpcRemoteFailure err) -> toJSON (rpcErrorCode err) @?= Number (-32001)
                _ -> assertFailure "Expected the original remote load rejection"
            Daemon.getSettings one >>= (@?= "one") . settingsModel
    ]

data PeerFixture = PeerFixture
  { peerTurnStarted :: !(MVar ()),
    peerLoadStarted :: !(MVar ()),
    peerReplies :: !(TQueue Object)
  }

withAttachmentPeer :: (Daemon.DaemonConnection -> PeerFixture -> IO a) -> IO (a, [Object])
withAttachmentPeer action = do
  trace <- newIORef []
  heldLoad <- newIORef Nothing
  fixture <- PeerFixture <$> newEmptyMVar <*> newEmptyMVar <*> newTQueueIO
  result <- withPeer (\_ connection -> serve trace heldLoad fixture connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \endpoint ->
    Daemon.withConnection (options endpoint) (`action` fixture)
  recorded <- readIORef trace
  pure (result, recorded)

serve :: IORef [Object] -> IORef (Maybe Object) -> PeerFixture -> WS.Connection -> IO ()
serve trace heldLoad fixture connection = forever $ do
  request <- WS.receiveData connection >>= either (const (assertFailure "Malformed request")) pure . eitherDecode
  modifyIORef' trace (<> [request])
  if field "type" request == String "response"
    then atomically (writeTQueue (peerReplies fixture) request)
    else do
      method <- textField "method" request
      case method of
        "daemon.authenticate" -> do
          field "params" request @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
          reply connection request (object ["userId" .= String "user", "orgId" .= String "org"])
        "daemon.list_terminals" -> reply connection request (object ["terminals" .= ([] :: [Value])])
        "daemon.load_session" -> do
          params <- objectField "params" request
          field "token" params @?= String "OFFLINE_ONLY"
          identifier <- textField "sessionId" params
          case identifier of
            "held" ->
              readIORef heldLoad >>= \case
                Nothing -> writeIORef heldLoad (Just request) >> putMVar (peerLoadStarted fixture) ()
                Just old -> reply connection old (loaded "late") >> reply connection request (loaded identifier)
            "rejected" -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= (-32001 :: Int), "message" .= String "Rejected load"]]
            _ -> reply connection request (loaded identifier)
        "daemon.get_proxy_token" -> reply connection request (object ["token" .= String "OFFLINE_TOKEN"])
        "daemon.close_session" -> do
          params <- objectField "params" request
          identifier <- textField "sessionId" params
          if identifier == "quiet"
            then pure ()
            else do
              notify connection identifier (object ["type" .= String "session_closed"])
              notify connection "two" (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "two-updated"]])
          reply connection request (object [])
        "daemon.add_user_message" -> do
          params <- objectField "params" request
          identifier <- textField "sessionId" params
          turn <- textField "messageId" params
          prompt <- textField "text" params
          reply connection request (object [])
          if prompt == "held-turn"
            then putMVar (peerTurnStarted fixture) ()
            else do
              notify connection identifier (object ["type" .= String "assistant_text_delta", "messageId" .= String "answer", "blockIndex" .= (0 :: Int), "textDelta" .= String "two-result"])
              notify connection identifier (object ["type" .= String "agent_turn_completed", "turnId" .= turn, "reason" .= String "completed", "tokenUsage" .= object ["inputTokens" .= (0 :: Int), "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]])
        "daemon.get_rewind_info" -> do
          params <- objectField "params" request
          if field "messageId" params == String "settings"
            then notify connection "one" (object ["type" .= String "settings_updated", "settings" .= object ["modelId" .= String "one-updated"]])
            else do
              requestPermission connection "one-permission" "one" ["two"]
              requestPermission connection "two-permission" "two" ["one"]
              requestPermission connection "worker-permission" "worker" ["two", "one"]
              sendFrame connection ["type" .= String "request", "id" .= String "worker-question", "method" .= String "daemon.ask_user", "params" .= object ["sessionId" .= String "worker", "associatedSessionIds" .= [String "one"], "toolCallId" .= String "question", "questions" .= ([] :: [Value])]]
          reply connection request (object ["availableFiles" .= ([] :: [Value]), "createdFiles" .= ([] :: [Value]), "evictedFiles" .= ([] :: [Value])])
        _ -> assertFailure "Unexpected RPC, including implicit interruption/close/logout"

loaded :: Text -> Value
loaded identifier = object ["session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= identifier, "reasoningEffort" .= String "low"]]

requestPermission :: WS.Connection -> Text -> Text -> [Text] -> IO ()
requestPermission connection identifier execution associated = sendFrame connection ["type" .= String "request", "id" .= identifier, "method" .= String "daemon.request_permission", "params" .= object ["sessionId" .= execution, "associatedSessionIds" .= associated, "toolUses" .= ([] :: [Value]), "options" .= [object ["label" .= String "Cancel", "value" .= String "cancel"]]]]

notify :: WS.Connection -> Text -> Value -> IO ()
notify connection identifier value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= value]]

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request result = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection fields = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)))

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options endpoint = (Daemon.defaultDaemonOptions endpoint (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

methods :: [Object] -> [Text]
methods trace = [method | frame <- trace, String method <- [field "method" frame]]

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

textField :: Key -> Object -> IO Text
textField key fields = case field key fields of String value -> pure value; _ -> assertFailure "Missing text field"

objectField :: Key -> Object -> IO Object
objectField key fields = case field key fields of Object value -> pure value; _ -> assertFailure "Missing object field"
