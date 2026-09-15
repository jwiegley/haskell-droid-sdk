{-# LANGUAGE OverloadedStrings #-}

module SubmissionSpec (submissionTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, cancelWith, wait, waitCatch, withAsync)
import Control.Exception (Exception, catch, fromException, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (DroidInvalidEvent), DroidResult (resultText))
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..))
import Factory.Droid.Schema.Control (AddUserMessageParams (..), defaultUserMessageParams)
import Factory.Droid.Schema.Daemon.Management (ProxyTokenResult (proxyToken))
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.Schema.Notifications (CreateMessage)
import Factory.Droid.Schema.RPC (JsonRpcError (rpcErrorCode), JsonRpcErrorCode (RpcInvalidParams))
import Factory.Droid.SessionState qualified as State
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

submissionTests :: TestTree
submissionTests =
  testGroup
    "Optimistic submissions"
    [ testCase "preparation is idempotent by placeholder and preserves original input while refreshing its deadline" $ do
        first <- registered 0 (Just 20) "request" "placeholder" (input "first" Nothing) State.emptySessionState
        refreshed <- registered 10 (Just 20) "request" "placeholder" (input "changed" Nothing) first
        entry <- requiredSubmission "request" refreshed
        userMessageText (State.submissionInput entry) @?= "first"
        State.submissionDeadline entry @?= Just 30
        State.submissionStatus entry @?= State.SubmissionPending
        length (State.optimisticSubmissions refreshed) @?= 1
        State.registerSubmissionAt 0 (Just (-1)) "request" "placeholder" (input "first" Nothing) first @?= Left State.SubmissionInvalidTimeout
        expired <- registered 30 (Just 10) "request" "placeholder" (input "ignored" Nothing) (State.expireSubmissionsAt 30 refreshed)
        renewed <- requiredSubmission "request" expired
        State.submissionStatus renewed @?= State.SubmissionPending
        State.submissionDeadline renewed @?= Just 40,
      testCase "UI cancellation does not permit replay of a request that is still on the wire" $ do
        pending <- either (assertFailure . show) pure (State.beginSubmissionAt 0 (Just 20) "request" "placeholder" (input "text" (Just "message")) State.emptySessionState)
        State.registerSubmissionAt 1 Nothing "request" "other" (input "text" Nothing) pending @?= Left State.SubmissionAlreadyInFlight
        State.beginSubmissionAt 1 Nothing "request" "other" (input "text" Nothing) (State.failSubmission "request" State.SubmissionInvalidConfirmation pending) @?= Left State.SubmissionAlreadyInFlight
        let removed = State.cancelSubmission "request" pending
        State.optimisticSubmissions removed @?= []
        State.beginSubmissionAt 0 (Just 20) "request" "other" (input "text" Nothing) removed @?= Left State.SubmissionAlreadyInFlight
        released <- either (assertFailure . show) pure (State.beginSubmissionAt 21 (Just 20) "request" "other" (input "retry" Nothing) (State.finishSubmission "request" removed))
        length (State.optimisticSubmissions released) @?= 1,
      testCase "timeout retains an error overlay, confirmation replaces it, and late rejection cannot resurrect it" $ do
        prepared <- registered 0 (Just 20) "request" "placeholder" (input "text" (Just "message")) State.emptySessionState
        let expired = State.expireSubmissionsAt 20 prepared
        entry <- requiredSubmission "request" expired
        State.submissionStatus entry @?= State.SubmissionFailed State.SubmissionTimedOut
        event <- decodeValue @CreateMessage (created "request" "message" "text")
        let confirmed = State.observeCreatedMessage event expired
        State.optimisticSubmissions confirmed @?= []
        Map.keys (State.sessionMessagesById confirmed) @?= ["message"]
        State.optimisticSubmissions (State.rejectSubmission "request" State.SubmissionRejected confirmed) @?= []
        State.registerSubmissionAt 30 Nothing "request" "again" (input "text" Nothing) confirmed @?= Left State.SubmissionAlreadyConfirmed
        show expired @?= "SessionState <redacted>"
        show entry @?= "OptimisticSubmission <redacted>",
      testCase "load reconciliation requires a fresh explicit message ID, never matching text or wall-clock proximity" $ do
        old <- decodeValue @FactoryDroidMessage (message "old" "same")
        unrelated <- decodeValue @FactoryDroidMessage (message "unrelated" "same")
        persisted <- decodeValue @FactoryDroidMessage (message "wanted" "same")
        seeded <- registered 0 Nothing "request" "placeholder" (input "same" (Just "wanted")) (State.mergeLoadedMessages [old] State.emptySessionState)
        let notConfirmed = State.mergeLoadedMessages [old, unrelated] seeded
        length (State.optimisticSubmissions notConfirmed) @?= 1
        State.optimisticSubmissions (State.mergeLoadedMessages [old, unrelated, persisted] notConfirmed) @?= []
        resubmit <- registered 0 Nothing "new-request" "other" (input "same" (Just "old")) (State.mergeLoadedMessages [old] State.emptySessionState)
        length (State.optimisticSubmissions (State.mergeLoadedMessages [old] resubmit)) @?= 1,
      testCase "confirmation can precede the RPC reply and an ACK never reinstates the overlay" $ bounded $ do
        void $ withSubmissionPeer BeforeAck $ \connection fixture ->
          withAsync (Daemon.submitUserMessage connection "one" "request" (input "text" Nothing)) $ \pending -> do
            request <- takeMVar (submissionStarted fixture)
            params <- objectField "params" request
            identifier <- textField "messageId" params
            initial <- Daemon.getSessionState connection "one"
            entry <- requiredSubmission "request" initial
            State.submissionInFlight entry @?= True
            userMessageId (State.submissionInput entry) @?= Just identifier
            void (Daemon.getProxyToken connection)
            confirmed <- waitForState connection "one" (Map.member identifier . State.sessionMessagesById)
            State.optimisticSubmissions confirmed @?= []
            void (Daemon.getProxyToken connection)
            wait pending >>= (@?= objectFields (object ["accepted" .= True]))
            Daemon.getSessionState connection "one" >>= (@?= []) . State.optimisticSubmissions,
      testCase "ACK and foreign-session or foreign-request messages do not confirm the pending request" $ bounded $ do
        void $ withSubmissionPeer AfterAck $ \connection fixture ->
          withAsync (Daemon.submitUserMessage connection "one" "request" (input "text" (Just "wanted"))) $ \pending -> do
            void (takeMVar (submissionStarted fixture))
            void (Daemon.getProxyToken connection)
            void (waitForState connection "other" (Map.member "foreign" . State.sessionMessagesById))
            void (waitForState connection "one" (Map.member "wrong" . State.sessionMessagesById))
            state <- Daemon.getSessionState connection "one"
            length (State.optimisticSubmissions state) @?= 1
            void (Daemon.getProxyToken connection)
            void (wait pending)
            confirmed <- waitForState connection "one" (Map.member "wanted" . State.sessionMessagesById)
            State.optimisticSubmissions confirmed @?= [],
      testCase "legacy success without an echo remains unconfirmed and display expiry is observable" $ bounded $ do
        void $ withSubmissionPeer Legacy $ \connection _ -> do
          Daemon.registerOptimisticSubmission connection "one" "request" "placeholder" (input "text" Nothing) (Just 200000)
          Daemon.submitUserMessage connection "one" "request" (input "text" Nothing) >>= (@?= mempty)
          failed <- waitForState connection "one" (any ((== State.SubmissionFailed State.SubmissionTimedOut) . State.submissionStatus) . State.optimisticSubmissions)
          length (State.optimisticSubmissions failed) @?= 1
          Daemon.cancelOptimisticSubmission connection "one" "request" >>= (@?= True)
          Daemon.cancelOptimisticSubmission connection "one" "request" >>= (@?= False),
      testCase "RPC rejection and malformed confirmation keep typed failures without publishing a real message" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> void $ withSubmissionPeer mode $ \connection _ -> do
            if mode == Rejected
              then
                try @RpcResultError (Daemon.submitUserMessage connection "one" "request" (input "text" Nothing)) >>= \case
                  Left (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
                  _ -> assertFailure "Expected RPC rejection"
              else try @DroidError (Daemon.submitUserMessage connection "one" "request" (input "text" Nothing)) >>= (@?= Left DroidInvalidEvent)
            state <- Daemon.getSessionState connection "one"
            entry <- requiredSubmission "request" state
            State.submissionStatus entry @?= State.SubmissionFailed (if mode == Rejected then State.SubmissionRejected else State.SubmissionInvalidConfirmation)
            Map.null (State.sessionMessagesById state) @?= True
            Daemon.getProxyToken connection >>= (@?= "OFFLINE_TOKEN") . proxyToken,
      testCase "cancelling an overlay and then the sender does not lose a later authoritative message" $ bounded $ do
        (_, trace) <- withSubmissionPeer Held $ \connection fixture ->
          withAsync (Daemon.submitUserMessage connection "one" "request" (input "text" (Just "wanted"))) $ \pending -> do
            void (takeMVar (submissionStarted fixture))
            Daemon.cancelOptimisticSubmission connection "one" "request" >>= (@?= True)
            try @State.SubmissionError (Daemon.submitUserMessage connection "one" "request" (input "duplicate" Nothing)) >>= (@?= Left State.SubmissionAlreadyInFlight)
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled sender returned"
            void (Daemon.getProxyToken connection)
            state <- waitForState connection "one" (Map.member "wanted" . State.sessionMessagesById)
            State.optimisticSubmissions state @?= []
        length [() | frame <- trace, field "method" frame == String "daemon.add_user_message"] @?= 1,
      testCase "explicit confirmation and bulk cancellation are session-scoped local transitions, not remote mutations" $ bounded $ do
        (_, trace) <- withSubmissionPeer Plain $ \connection _ -> do
          initial <- Daemon.getSessionState connection "one"
          Daemon.confirmOptimisticSubmission connection "one" "missing" >>= (@?= False)
          Daemon.getSessionState connection "one" >>= (@?= initial)
          Daemon.registerOptimisticSubmission connection "one" "first" "a" (input "one" Nothing) Nothing
          Daemon.registerOptimisticSubmission connection "one" "second" "b" (input "two" Nothing) Nothing
          Daemon.registerOptimisticSubmission connection "other" "other" "c" (input "other" Nothing) Nothing
          Daemon.confirmOptimisticSubmission connection "other" "first" >>= (@?= False)
          Daemon.confirmOptimisticSubmission connection "one" "first" >>= (@?= True)
          Daemon.confirmOptimisticSubmission connection "one" "first" >>= (@?= False)
          try @State.SubmissionError (Daemon.registerOptimisticSubmission connection "one" "first" "again" (input "one" Nothing) Nothing) >>= (@?= Left State.SubmissionAlreadyConfirmed)
          Daemon.cancelSessionOptimisticSubmissions connection "one" >>= (@?= 1)
          Daemon.cancelSessionOptimisticSubmissions connection "one" >>= (@?= 0)
          Daemon.getSessionState connection "other" >>= (@?= ["other"]) . map State.submissionRequestId . State.optimisticSubmissions
        map (field "method") trace @?= [String "daemon.authenticate"],
      testCase "custom caller cancellation preserves its exception and removes the overlay" $ bounded $ do
        void $ withSubmissionPeer Held $ \connection fixture ->
          withAsync (Daemon.submitUserMessage connection "one" "request" (input "text" Nothing)) $ \pending -> do
            void (takeMVar (submissionStarted fixture))
            cancelWith pending SubmissionCancelled
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just SubmissionCancelled
              Right _ -> assertFailure "Custom cancellation returned"
            Daemon.getSessionState connection "one" >>= (@?= []) . State.optimisticSubmissions,
      testCase "caller timeout stops waiting without retaining an error overlay or replaying the request" $ bounded $ do
        (_, trace) <- withSubmissionPeer Held $ \connection fixture ->
          withAsync (timeout 200000 (Daemon.submitUserMessage connection "one" "request" (input "text" Nothing))) $ \pending -> do
            void (takeMVar (submissionStarted fixture))
            wait pending >>= (@?= Nothing)
            Daemon.getSessionState connection "one" >>= (@?= []) . State.optimisticSubmissions
        length [() | frame <- trace, field "method" frame == String "daemon.add_user_message"] @?= 1,
      testCase "display expiry neither cancels the live RPC nor prevents a later confirmation" $ bounded $ do
        void $ withSubmissionPeer Held $ \connection fixture -> do
          Daemon.registerOptimisticSubmission connection "one" "request" "placeholder" (input "text" (Just "wanted")) (Just 0)
          withAsync (Daemon.submitUserMessage connection "one" "request" (input "text" (Just "wanted"))) $ \pending -> do
            void (takeMVar (submissionStarted fixture))
            state <- Daemon.getSessionState connection "one"
            entry <- requiredSubmission "request" state
            State.submissionStatus entry @?= State.SubmissionFailed State.SubmissionTimedOut
            State.submissionInFlight entry @?= True
            void (Daemon.getProxyToken connection)
            void (wait pending)
            confirmed <- waitForState connection "one" (Map.member "wanted" . State.sessionMessagesById)
            State.optimisticSubmissions confirmed @?= [],
      testCase "disconnect ends both a pending RPC and a state observer without exposing stale connection state" $ bounded $ do
        void $ withSubmissionPeer Disconnected $ \connection fixture ->
          withAsync (try @RpcChannelError (Daemon.submitUserMessage connection "one" "request" (input "text" Nothing))) $ \pending -> do
            void (takeMVar (submissionStarted fixture))
            initial <- Daemon.getSessionState connection "other"
            withAsync (try @RpcChannelError (Daemon.waitSessionStateChange connection "other" initial)) $ \observer -> do
              void (try @RpcChannelError (Daemon.getProxyToken connection))
              wait pending >>= (@?= Left RpcChannelReadFailure)
              wait observer >>= (@?= Left RpcChannelClosed)
              try @RpcChannelError (Daemon.getSessionState connection "one") >>= (@?= Left RpcChannelClosed),
      testCase "submission preserves false, empty, exact numbers and extensions while reserving message and session fields" $ bounded $ do
        toJSON (defaultUserMessageParams "") @?= object ["text" .= String ""]
        supplied <- decodeValue @AddUserMessageParams (object ["text" .= String "", "messageId" .= String "chosen", "skipAgentLoop" .= False, "images" .= ([] :: [Value]), "files" .= ([] :: [Value]), "future" .= Number 100000000000000001, "sessionId" .= String "wrong-session"])
        let params = supplied {userMessageAdditionalFields = KeyMap.insert "text" (String "wrong-text") (userMessageAdditionalFields supplied)}
        void $ withSubmissionPeer Legacy $ \connection fixture -> do
          void (Daemon.submitUserMessage connection "one" "request" params)
          request <- takeMVar (submissionStarted fixture)
          field "id" request @?= String "request"
          field "method" request @?= String "daemon.add_user_message"
          field "factoryProtocolVersion" request @?= String "1.201.1"
          field "params" request @?= object ["sessionId" .= String "one", "text" .= String "", "messageId" .= String "chosen", "skipAgentLoop" .= False, "images" .= ([] :: [Value]), "files" .= ([] :: [Value]), "future" .= Number 100000000000000001, "userMessageSource" .= String "api"],
      testCase "the ordinary daemon turn path uses the same optimistic state and request-ID confirmation" $ bounded $ do
        void $ withSubmissionPeer AfterAck $ \connection fixture ->
          Daemon.withResumedSessionOn connection "one" $ \session ->
            withAsync (Daemon.sendPrompt session "text" (\_ -> pure ())) $ \pending -> do
              request <- takeMVar (submissionStarted fixture)
              requestId <- textField "id" request
              state <- Daemon.getSessionState connection "one"
              void (requiredSubmission requestId state)
              void (Daemon.getProxyToken connection)
              void (Daemon.getProxyToken connection)
              wait pending >>= (@?= "done") . resultText
              Daemon.getSessionState connection "one" >>= (@?= []) . State.optimisticSubmissions,
      testCase "validated load snapshots reconcile only the explicit fresh message ID" $ bounded $ do
        void $ withSubmissionPeer LoadRecovery $ \connection _ -> do
          Daemon.registerOptimisticSubmission connection "one" "request" "placeholder" (input "same" (Just "persisted")) Nothing
          void (Daemon.loadSessionInfo connection "one")
          Daemon.getSessionState connection "one" >>= (@?= 1) . length . State.optimisticSubmissions
          void (Daemon.loadSessionInfo connection "one")
          Daemon.getSessionState connection "one" >>= (@?= []) . State.optimisticSubmissions,
      testCase "detaching an old lease cannot cancel overlays belonging to its replacement or another session" $ bounded $ do
        void $ withSubmissionPeer Plain $ \connection _ ->
          Daemon.withResumedSessionOn connection "one" $ \old ->
            Daemon.withResumedSessionOn connection "two" $ \two -> do
              Daemon.registerOptimisticSubmission connection "one" "old" "old-placeholder" (input "old" Nothing) Nothing
              Daemon.registerOptimisticSubmission connection "two" "two" "two-placeholder" (input "two" Nothing) Nothing
              Daemon.detachSession old
              Daemon.getSessionState connection "one" >>= (@?= []) . State.optimisticSubmissions
              Daemon.withResumedSessionOn connection "one" $ \fresh -> do
                Daemon.registerOptimisticSubmission connection "one" "fresh" "fresh-placeholder" (input "fresh" Nothing) Nothing
                Daemon.detachSession old
                Daemon.getSessionState connection "one" >>= (@?= 1) . length . State.optimisticSubmissions
                Daemon.getSessionState connection "two" >>= (@?= 1) . length . State.optimisticSubmissions
                void (Daemon.getSettings fresh)
                void (Daemon.getSettings two)
    ]

input :: Text -> Maybe Text -> AddUserMessageParams
input text identifier = (defaultUserMessageParams text) {userMessageId = identifier, userMessageSkipAgentLoop = Just False}

registered :: Integer -> Maybe Int -> Text -> Text -> AddUserMessageParams -> State.SessionState -> IO State.SessionState
registered now deadline requestId placeholder params = either (assertFailure . show) pure . State.registerSubmissionAt now deadline requestId placeholder params

requiredSubmission :: Text -> State.SessionState -> IO State.OptimisticSubmission
requiredSubmission requestId state = maybe (assertFailure "Missing optimistic submission") pure (State.lookupSubmission requestId state)

waitForState :: Daemon.DaemonConnection -> Text -> (State.SessionState -> Bool) -> IO State.SessionState
waitForState connection identifier ready = do
  current <- Daemon.getSessionState connection identifier
  if ready current then pure current else Daemon.waitSessionStateChange connection identifier current >> waitForState connection identifier ready

data SubmissionCancelled = SubmissionCancelled deriving stock (Eq, Show)

instance Exception SubmissionCancelled

data Mode = BeforeAck | AfterAck | Rejected | Malformed | Held | Legacy | LoadRecovery | Plain | Disconnected deriving stock (Eq)

newtype SubmissionFixture = SubmissionFixture {submissionStarted :: MVar Object}

withSubmissionPeer :: Mode -> (Daemon.DaemonConnection -> SubmissionFixture -> IO a) -> IO (a, [Object])
withSubmissionPeer mode action = do
  trace <- newIORef []
  pending <- newIORef Nothing
  stage <- newIORef (0 :: Int)
  loads <- newIORef (0 :: Int)
  fixture <- SubmissionFixture <$> newEmptyMVar
  result <- withPeer (\_ connection -> serve mode trace pending stage loads fixture connection `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \target ->
    Daemon.withConnection (options target) (`action` fixture)
  recorded <- readIORef trace
  pure (result, recorded)

serve :: Mode -> IORef [Object] -> IORef (Maybe Object) -> IORef Int -> IORef Int -> SubmissionFixture -> WS.Connection -> IO ()
serve mode trace pending stage loads fixture connection = forever $ do
  request <- WS.receiveData connection >>= either (const (assertFailure "Malformed RPC")) pure . eitherDecode
  modifyIORef' trace (<> [request])
  method <- textField "method" request
  case method of
    "daemon.authenticate" -> reply connection request (object ["userId" .= String "user", "orgId" .= String "org"])
    "daemon.list_terminals" -> reply connection request (object ["terminals" .= ([] :: [Value])])
    "daemon.load_session" -> do
      number <- atomicModifyIORef' loads (\n -> (n + 1, n + 1))
      let history = [message (if number == 1 then "old" else "persisted") "same" | mode == LoadRecovery]
      reply connection request (object ["session" .= object ["messages" .= history], "settings" .= object ["modelId" .= String "model", "reasoningEffort" .= String "low"]])
    "daemon.add_user_message" -> do
      writeIORef pending (Just request)
      putMVar (submissionStarted fixture) request
      case mode of
        Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Rejected submission"]]
        Malformed -> do
          params <- objectField "params" request
          session <- textField "sessionId" params
          notify connection session (object ["type" .= String "create_message", "requestId" .= field "id" request, "message" .= False])
          reply connection request (object ["accepted" .= True])
        AfterAck -> reply connection request (object ["accepted" .= True])
        Legacy -> reply connection request (object [])
        _ -> pure ()
    "daemon.get_proxy_token" -> do
      number <- atomicModifyIORef' stage (\n -> (n + 1, n + 1))
      waiting <- readIORef pending
      forM_ waiting $ \original -> do
        params <- objectField "params" original
        session <- textField "sessionId" params
        requestId <- textField "id" original
        identifier <- textField "messageId" params
        text <- textField "text" params
        case mode of
          BeforeAck | number == 1 -> notify connection session (created requestId identifier text)
          BeforeAck -> reply connection original (object ["accepted" .= True])
          AfterAck | number == 1 -> do
            notify connection "other" (created requestId "foreign" text)
            notify connection session (created "wrong-request" "wrong" text)
          AfterAck -> do
            notify connection session (created requestId identifier text)
            notify connection session (object ["type" .= String "assistant_text_delta", "messageId" .= String "answer", "blockIndex" .= (0 :: Int), "textDelta" .= String "done"])
            notify connection session (object ["type" .= String "agent_turn_completed", "turnId" .= identifier, "reason" .= String "completed", "tokenUsage" .= object ["inputTokens" .= (0 :: Int), "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]])
          Held -> notify connection session (created requestId identifier text) >> reply connection original (object ["accepted" .= True])
          Disconnected -> WS.sendClose connection ("offline disconnect" :: Text)
          _ -> pure ()
      if mode == Disconnected then pure () else reply connection request (object ["token" .= String "OFFLINE_TOKEN"])
    _ -> assertFailure "Unexpected optimistic-submission RPC"

created :: Text -> Text -> Text -> Value
created requestId identifier text = object ["type" .= String "create_message", "requestId" .= requestId, "message" .= message identifier text]

message :: Text -> Text -> Value
message identifier text = object ["id" .= identifier, "role" .= String "user", "content" .= [object ["type" .= String "text", "text" .= text]], "createdAt" .= (1 :: Int), "updatedAt" .= (1 :: Int)]

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

reply :: WS.Connection -> Object -> Value -> IO ()
reply connection request value = sendFrame connection ["type" .= String "response", "id" .= field "id" request, "result" .= value]

notify :: WS.Connection -> Text -> Value -> IO ()
notify connection session value = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= session, "notification" .= value]]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection values = WS.sendTextData connection (encode (object (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> values)))

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

objectFields :: Value -> Object
objectFields (Object fields) = fields
objectFields _ = error "Expected object fixture"

objectField :: Key -> Object -> IO Object
objectField key fields = case field key fields of Object value -> pure value; _ -> assertFailure "Missing object field"

textField :: Key -> Object -> IO Text
textField key fields = case field key fields of String value -> pure value; _ -> assertFailure "Missing text field"

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err
