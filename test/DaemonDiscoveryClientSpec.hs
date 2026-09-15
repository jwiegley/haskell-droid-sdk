{-# LANGUAGE OverloadedStrings #-}

module DaemonDiscoveryClientSpec (discoveryClientTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Factory.Droid (DroidError (DroidSessionUnusable), DroidEvent (TitleEvent), DroidSessionStatus (..))
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Discovery (EditableSkillSettingsLevel (..), ListSkillsResult (..), SetSkillDisabledParams (..))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), SuccessResult (..), WithEnvelope (..))
import Factory.Droid.Schema.Session (SessionIdParams (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

discoveryClientTests :: TestTree
discoveryClientTests =
  testGroup
    "Daemon discovery and context"
    [ testCase "populated reports preserve metadata, context numbers and explicit skill mutation fields" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withDiscoveryPeer Populated trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
          Daemon.listSkills session >>= (@?= skillsWire) . toJSON
          Daemon.listCommands session >>= (@?= commandsWire) . toJSON
          Daemon.getContextBreakdown session >>= (@?= contextWire) . toJSON
          forM_ [Nothing, Just EditableSkillUser, Just EditableSkillProject] $ \level -> forM_ [False, True] $ \disabled -> do
            result <- Daemon.setSkillDisabled session (skillUpdate disabled level)
            resultSuccess result @?= False
          Daemon.sessionStatus session >>= (@?= SessionReady)
        frames <- readIORef trace
        map (field "method") frames @?= map String (["daemon.authenticate", "daemon.initialize_session", "daemon.list_skills", "daemon.list_commands", "daemon.get_context_breakdown"] <> replicate 6 "daemon.set_skill_disabled")
        let expectedSettings = [KeyMap.fromList (["skillName" .= String "", "disabled" .= disabled, "future" .= False] <> maybe [] (\value -> ["settingsLevel" .= String value]) level) | level <- [Nothing, Just "user", Just "project"], disabled <- [False, True]]
        forM_ (zip expectedSettings (drop 5 frames)) $ \(expected, request) -> case field "params" request of
          Object fields -> KeyMap.delete "sessionId" fields @?= expected
          _ -> assertFailure "Skill mutation params are not an object",
      testCase "empty listings and absent project metadata remain distinct from false" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withDiscoveryPeer Empty trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
          skills <- Daemon.listSkills session
          listedSkills skills @?= []
          listedSkillsProjectAvailable skills @?= Nothing
          Daemon.listCommands session >>= (@?= object ["commands" .= ([] :: [Value])]) . toJSON,
      testCase "read and skill-setting RPC failures preserve typed errors and ordinary session usability" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withDiscoveryPeer mode trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
              forM_ [void (Daemon.listSkills session), void (Daemon.listCommands session), void (Daemon.getContextBreakdown session), void (Daemon.setSkillDisabled session (skillUpdate False Nothing))] $ \request -> do
                result <- try @RpcResultError request
                case result of
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Wrong query/control error"
                Daemon.sessionStatus session >>= (@?= SessionReady),
      testCase "read cancellation preserves the session while mutation cancellation interrupts and invalidates" $
        bounded $
          forM_ [HeldRead, HeldMutation] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withDiscoveryPeer mode trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
              let request = if mode == HeldRead then void (Daemon.listSkills session) else void (Daemon.setSkillDisabled session (skillUpdate False Nothing))
              withAsync request $ \pending -> do
                takeMVar ready
                cancel pending
                waitCatch pending >>= \case
                  Left cause -> fromException cause @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled operation returned"
              if mode == HeldRead
                then do
                  Daemon.sessionStatus session >>= (@?= SessionReady)
                  Daemon.listCommands session >>= (@?= commandsWire) . toJSON
                else do
                  Daemon.sessionStatus session >>= (@?= SessionUnavailable)
                  try @DroidError (Daemon.listCommands session) >>= (@?= Left DroidSessionUnusable)
            frames <- readIORef trace
            map (field "method") (drop 2 frames) @?= if mode == HeldRead then map String ["daemon.list_skills", "daemon.list_commands"] else map String ["daemon.set_skill_disabled", "daemon.interrupt_session"],
      testCase "ordinary context queries can run inside serial notification callbacks" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        observed <- newEmptyMVar
        withDiscoveryPeer Populated trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
          stop <- Daemon.onSessionEvent session $ \case
            Right (TitleEvent _) -> Daemon.getContextBreakdown session >>= putMVar observed . toJSON
            _ -> pure ()
          Daemon.renameSession session "query callback" >>= (@?= True) . resultSuccess
          takeMVar observed >>= (@?= contextWire)
          stop,
      testCase "low-level query/setter bindings retain explicit routing, extensions and deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "raw" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
              scoped = SessionIdParams "owned" (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])
              requests = [("daemon.list_skills", toJSON <$> Client.listDaemonSkills channel configured scoped, skillsWire), ("daemon.list_commands", toJSON <$> Client.listDaemonCommands channel configured scoped, commandsWire), ("daemon.get_context_breakdown", toJSON <$> Client.getDaemonContextBreakdown channel configured scoped, contextWire), ("daemon.set_skill_disabled", toJSON <$> Client.setDaemonSkillDisabled channel configured "owned" (skillUpdate False Nothing), object ["success" .= False])]
          forM_ requests $ \(method, request, result) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String method
            field "params" frame @?= if method == "daemon.set_skill_disabled" then object ["sessionId" .= String "owned", "skillName" .= String "", "disabled" .= False, "future" .= False] else object ["sessionId" .= String "owned", "future" .= False]
            atomically (writeTQueue incoming (response frame result))
            wait pending >>= (@?= result)
          let expired = configured {Client.callTimeoutMicros = Just 0}
          forM_ [void (Client.listDaemonSkills channel expired scoped), void (Client.listDaemonCommands channel expired scoped), void (Client.getDaemonContextBreakdown channel expired scoped), void (Client.setDaemonSkillDisabled channel expired "owned" (skillUpdate False Nothing))] $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

data PeerMode = Populated | Empty | Rejected | Malformed | HeldRead | HeldMutation deriving stock (Eq, Show)

skillUpdate :: Bool -> Maybe EditableSkillSettingsLevel -> SetSkillDisabledParams
skillUpdate disabled level = SetSkillDisabledParams "" disabled level (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withDiscoveryPeer :: PeerMode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withDiscoveryPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      initialize <- readFrame connection
      field "method" initialize @?= String "daemon.initialize_session"
      identifier <- case field "params" initialize of
        Object fields -> case field "sessionId" fields of
          String value -> pure value
          _ -> assertFailure "Missing session ID"
        _ -> assertFailure "Missing initialize params"
      reply connection initialize (object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "initial", "reasoningEffort" .= String "low"]])
      forever $ do
        request <- readFrame connection
        method <- case field "method" request of String value -> pure value; _ -> assertFailure "Missing method"
        params <- case field "params" request of Object value -> pure value; _ -> assertFailure "Missing params"
        field "sessionId" params @?= String identifier
        case method of
          "daemon.rename_session" -> do
            params @?= KeyMap.fromList ["sessionId" .= identifier, "title" .= String "query callback"]
            reply connection request (object ["accepted" .= True])
            sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= identifier, "notification" .= object ["type" .= String "session_title_updated", "requestId" .= field "id" request, "title" .= String "query callback"]]]
          "daemon.interrupt_session" -> do
            mode @?= HeldMutation
            params @?= KeyMap.singleton "sessionId" (String identifier)
            reply connection request (object [])
          _ -> do
            expected <- case method of
              "daemon.list_skills" -> do
                params @?= KeyMap.singleton "sessionId" (String identifier)
                pure (if mode == Empty then object ["skills" .= ([] :: [Value])] else skillsWire)
              "daemon.list_commands" -> do
                params @?= KeyMap.singleton "sessionId" (String identifier)
                pure (if mode == Empty then object ["commands" .= ([] :: [Value])] else commandsWire)
              "daemon.get_context_breakdown" -> do
                params @?= KeyMap.singleton "sessionId" (String identifier)
                pure contextWire
              "daemon.set_skill_disabled" -> do
                field "skillName" params @?= String ""
                case field "disabled" params of Bool _ -> pure (); _ -> assertFailure "Missing disabled flag"
                case KeyMap.lookup "settingsLevel" params of Nothing -> pure (); Just (String level) | level `elem` ["user", "project"] -> pure (); _ -> assertFailure "Wrong settings level"
                field "future" params @?= Bool False
                pure (object ["success" .= False])
              _ -> assertFailure "Unexpected request"
            if (mode == HeldRead && method == "daemon.list_skills") || (mode == HeldMutation && method == "daemon.set_skill_disabled")
              then putMVar ready ()
              else case mode of
                Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Request rejected"]]
                Malformed -> reply connection request (object [])
                _ -> reply connection request expected
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

response :: Object -> Value -> Object
response request result = envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . Object . envelope

envelope :: [Pair] -> Object
envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

skillsWire, commandsWire, contextWire :: Value
skillsWire = object ["skills" .= [object ["name" .= String "skill", "location" .= String "builtin", "filePath" .= String "/remote/skill.md", "description" .= String "", "enabled" .= False, "userInvocable" .= False, "content" .= String "NOT EXECUTED", "resources" .= [object ["name" .= String "asset", "path" .= String "/remote/data.json", "type" .= String "asset"]], "disabledBy" .= object ["kind" .= String "ledger", "sources" .= [object ["level" .= String "user"]]]]], "projectAvailable" .= False, "future" .= False]
commandsWire = object ["commands" .= [object ["name" .= String "cmd", "description" .= String "remote command", "argumentHint" .= String "", "isExecutable" .= True]], "future" .= False]
contextWire = object ["modelId" .= String "remote-model", "modelDisplayName" .= String "Remote", "contextBudget" .= Number 100.5, "usedTokens" .= Number 50.25, "freeTokens" .= Number (-0.25), "lastCallCompactionTokens" .= Number 0, "categories" .= [object ["name" .= String "system", "tokens" .= Number 1.25, "colorKey" .= String "systemPrompt"]], "skills" .= [object ["name" .= String "skill", "location" .= String "builtin", "tokens" .= Number 2.5]], "mcpServers" .= [object ["name" .= String "mcp", "toolCount" .= Number (-0.5), "tokens" .= Number 4.125]], "droids" .= [object ["name" .= String "droid", "location" .= String "personal", "tokens" .= Number 3.75]], "future" .= False]
