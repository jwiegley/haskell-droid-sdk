{-# LANGUAGE OverloadedStrings #-}

module DaemonSettingsClientSpec (settingsClientTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (SomeException, catch, fromException, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Factory.Droid (DroidError (..), DroidEvent (SettingsUpdatedEvent), DroidSessionStatus (..))
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Control (RenameSessionParams (..))
import Factory.Droid.Schema.Enums (AutonomyLevel (AutonomyOff), DroidInteractionMode (DroidAuto))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), SuccessResult (..), WithEnvelope (..))
import Factory.Droid.Schema.Settings (SessionSettings (..), UpdateSessionSettingsParams (..), emptySettingsUpdate)
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

settingsClientTests :: TestTree
settingsClientTests =
  testGroup
    "Daemon session settings controls"
    [ testCase "ACK settings update the shared observed view before getter and observer callback" $
        bounded $
          forM_ [BeforeAck, AfterAck] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            observed <- newEmptyMVar
            withSettingsPeer mode trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
              Daemon.getSettings session >>= (@?= "initial") . settingsModel
              stop <- Daemon.onSessionEvent session $ \case
                Right (SettingsUpdatedEvent _) -> Daemon.getSettings session >>= putMVar observed . settingsModel
                _ -> pure ()
              Daemon.updateSettings session patch >>= (@?= mempty)
              current <- Daemon.getSettings session
              settingsModel current @?= "resolved"
              settingsSpecModel current @?= Nothing
              takeMVar observed >>= (@?= "resolved")
              result <- Daemon.renameSession session title
              resultSuccess result @?= True
              Daemon.sessionStatus session >>= (@?= SessionReady)
              stop
            readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.initialize_session", "daemon.update_session_settings", "daemon.rename_session"]) . map (field "method"),
      testCase "legacy replies preserve false success and do not synthesize observed settings" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withSettingsPeer Legacy trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
          Daemon.updateSettings session patch >>= (@?= KeyMap.singleton "legacy" (Bool False))
          Daemon.getSettings session >>= (@?= "initial") . settingsModel
          result <- Daemon.renameSession session title
          resultSuccess result @?= False
          resultAdditionalFields result @?= KeyMap.singleton "legacy" (Bool False)
          Daemon.sessionStatus session >>= (@?= SessionReady),
      testCase "ordinary RPC errors retain policy while malformed completion invalidates the handle" $
        bounded $
          forM_ [Rejected, InvalidResult, InvalidEvent, WrongCompletion] $ \mode -> forM_ [SettingsControl, TitleControl] $ \control -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withSettingsPeer mode trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
              result <- try @SomeException (runControl control session)
              case result of
                Right _ -> assertFailure "Invalid control succeeded"
                Left cause -> case mode of
                  Rejected -> case fromException cause of
                    Just (RpcRemoteFailure err) -> rpcErrorCode err @?= RpcInvalidParams
                    _ -> assertFailure "Remote error changed type"
                  InvalidResult -> fromException cause @?= Just RpcInvalidResult
                  InvalidEvent -> fromException cause @?= Just DroidInvalidEvent
                  WrongCompletion -> fromException cause @?= Just RpcChannelReadFailure
                  _ -> assertFailure "Unexpected test mode"
              if mode `elem` [Rejected, InvalidResult]
                then do
                  Daemon.sessionStatus session >>= (@?= SessionReady)
                  Daemon.getSettings session >>= (@?= "initial") . settingsModel
                else do
                  Daemon.sessionStatus session >>= (@?= SessionUnavailable)
                  try @DroidError (Daemon.getSettings session) >>= (@?= Left DroidSessionUnusable),
      testCase "cancelled mutations preserve async identity, invalidate the handle and request interruption" $
        bounded $
          forM_ [SettingsControl, TitleControl] $ \control -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withSettingsPeer Held trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
              withAsync (runControl control session) $ \pending -> do
                takeMVar ready
                cancel pending
                waitCatch pending >>= \case
                  Left cause -> fromException cause @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled mutation returned"
              Daemon.sessionStatus session >>= (@?= SessionUnavailable)
              try @DroidError (Daemon.getSettings session) >>= (@?= Left DroidSessionUnusable)
            frames <- readIORef trace
            map (field "method") frames @?= map String ["daemon.authenticate", "daemon.initialize_session", method control, "daemon.interrupt_session"],
      testCase "raw bindings expose ACKs, scope parameters and obey caller deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "raw" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
              requests = [Client.updateDaemonSessionSettingsRaw channel configured "owned" patch, Client.renameDaemonSessionRaw channel configured "owned" (RenameSessionParams title (KeyMap.singleton "sessionId" (String "wrong")))]
          forM_ (zip [SettingsControl, TitleControl] requests) $ \(control, request) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String (method control)
            field "params" frame @?= Object (expectedParams control "owned")
            atomically (writeTQueue incoming (response frame (object ["accepted" .= True])))
            wait pending >>= (@?= KeyMap.singleton "accepted" (Bool True))
          let expired = configured {Client.callTimeoutMicros = Just 0}
          forM_ [Client.updateDaemonSessionSettingsRaw channel expired "owned" patch, Client.renameDaemonSessionRaw channel expired "owned" (RenameSessionParams title mempty)] $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

data Delivery = BeforeAck | AfterAck | Legacy | Rejected | InvalidResult | InvalidEvent | WrongCompletion | Held
  deriving stock (Eq, Show)

data Control = SettingsControl | TitleControl deriving stock (Eq, Show)

method :: Control -> Text
method SettingsControl = "daemon.update_session_settings"
method TitleControl = "daemon.rename_session"

runControl :: Control -> Daemon.DaemonSession -> IO ()
runControl SettingsControl session = void (Daemon.updateSettings session patch)
runControl TitleControl session = void (Daemon.renameSession session title)

title :: Text
title = "new \"title\"\n"

patch :: UpdateSessionSettingsParams
patch = emptySettingsUpdate {updateSettingsModel = Just "requested", updateSettingsMode = Just DroidAuto, updateSettingsAutonomy = Just AutonomyOff, updateSettingsSpecModel = Just Nothing, updateSettingsTags = Just [], updateSettingsCompactionThresholdEnabled = Just False, updateSettingsAdditionalFields = KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False]}

expectedParams :: Control -> Text -> Object
expectedParams control identifier =
  KeyMap.fromList
    ( ["sessionId" .= identifier] <> case control of
        SettingsControl -> ["modelId" .= String "requested", "interactionMode" .= String "auto", "autonomyLevel" .= String "off", "specModeModelId" .= Null, "tags" .= ([] :: [Value]), "compactionThresholdCheckEnabled" .= False, "future" .= False]
        TitleControl -> ["title" .= title]
    )

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withSettingsPeer :: Delivery -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withSettingsPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC frame")) pure . eitherDecode
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
        Object fields -> case field "sessionId" fields of String value -> pure value; _ -> assertFailure "Missing session ID"
        _ -> assertFailure "Missing initialize params"
      reply connection initialize (object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "initial", "reasoningEffort" .= String "low", "specModeModelId" .= String "old-spec"]])
      forever $ do
        request <- readFrame connection
        if field "method" request == String "daemon.interrupt_session"
          then do
            unless (mode `elem` [InvalidEvent, Held, WrongCompletion]) (assertFailure "Unexpected interruption")
            field "params" request @?= object ["sessionId" .= identifier]
            reply connection request (object [])
          else do
            control <- case field "method" request of
              String "daemon.update_session_settings" -> pure SettingsControl
              String "daemon.rename_session" -> pure TitleControl
              _ -> assertFailure "Unexpected request, getter RPC or lifecycle action"
            field "params" request @?= Object (expectedParams control identifier)
            let event = completion control True (field "id" request)
                ack = reply connection request (object ["accepted" .= True])
            case mode of
              Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Control rejected"]]
              InvalidResult -> reply connection request (if control == SettingsControl then Null else object ["success" .= String "wrong"])
              InvalidEvent -> ack >> notify connection identifier (completion control False (field "id" request))
              Legacy -> reply connection request (if control == SettingsControl then object ["legacy" .= False] else object ["success" .= False, "legacy" .= False])
              Held -> ack >> putMVar ready ()
              WrongCompletion -> do
                notify connection "foreign" event
                notify connection identifier (completion control True (String "other-rpc"))
                notify connection identifier (completion (if control == SettingsControl then TitleControl else SettingsControl) True (field "id" request))
                ack
                WS.sendClose connection ("fixture disconnect" :: Text)
              _ -> do
                notify connection "foreign" (completion control False (field "id" request))
                when (mode == BeforeAck) (notify connection identifier event)
                ack
                when (mode == AfterAck) (notify connection identifier event)
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))
    notify connection identifier event = sendFrame connection ["type" .= String "notification", "method" .= String "daemon.session_notification", "params" .= object ["sessionId" .= (identifier :: Text), "notification" .= event]]
    completion SettingsControl valid requestId = object ["type" .= String "settings_updated", "requestId" .= requestId, "settings" .= if valid then object ["modelId" .= String "resolved", "compactionThresholdCheckEnabled" .= False] else Bool False]
    completion TitleControl valid requestId = object ["type" .= String "session_title_updated", "requestId" .= requestId, "title" .= if valid then String title else Bool False]

response :: Object -> Value -> Object
response request result = envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result]

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . Object . envelope

envelope :: [Pair] -> Object
envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields)

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key
