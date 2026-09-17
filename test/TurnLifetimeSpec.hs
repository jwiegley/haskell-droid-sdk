{-# LANGUAGE OverloadedStrings #-}

module TurnLifetimeSpec (turnLifetimeTests) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, poll, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, finally, fromException, throwIO, try)
import Control.Monad (unless, void)
import Data.Aeson (Object, Result (..), Value (..), fromJSON, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as K
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Factory.Droid qualified as D
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcConflict), WithEnvelope (..))
import Factory.Droid.Transport (ObjectTransport, objectTransport, transportReceiveObject, transportSendObject)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

data ContractStop = ContractStop deriving stock (Eq, Show)

instance Exception ContractStop

turnLifetimeTests :: TestTree
turnLifetimeTests =
  testGroup "Native turn lifetime contracts" $
    [ testCase ("borrowed scope " <> label <> " leaves remote close to explicit typed composition") $ bounded $ withPeer False $ \peer -> do
        case mode of
          0 -> D.withDroidSessionOn localOptions (peerTransport peer) (const (pure ()))
          1 -> try @ContractStop (void (D.withDroidSessionOn localOptions (peerTransport peer) (const (throwIO ContractStop)))) >>= (@?= Left ContractStop)
          _ -> do
            entered <- newEmptyMVar
            hold <- newEmptyMVar
            withAsync (D.withDroidSessionOn localOptions (peerTransport peer) (const (putMVar entered () >> takeMVar hold))) $ \scope -> do
              takeMVar entered
              cancel scope
              waitCatch scope >>= \case Left cause -> fromException cause @?= Just AsyncCancelled; Right _ -> assertFailure "Cancelled scope returned normally"
        count peer "droid.close_session" >>= (@?= 0)
        closeProtocol peer
        count peer "droid.close_session" >>= (@?= 1)
        methods <- map (field "method") <$> readTVarIO (peerFrames peer)
        methods @?= [String "droid.initialize_session", String "droid.close_session"]
    | (label, mode) <- [("normal exit", 0 :: Int), ("callback error", 1), ("cancellation", 2)]
    ]
      <> [ testCase "explicit typed close rejection remains visible" $ bounded $ withPeer False $ \peer -> do
             D.withDroidSessionOn localOptions (peerTransport peer) (const (pure ()))
             atomically (writeTVar (peerRejectClose peer) True)
             try @RpcResultError (closeProtocol peer) >>= \case
               Left (RpcRemoteFailure failure) -> rpcErrorCode failure @?= RpcConflict
               _ -> assertFailure "Close rejection was hidden"
             count peer "droid.close_session" >>= (@?= 1),
           testCase "an SDK consumer waiting after ACK wakes when its borrowed scope closes" $ bounded $ withPeer False $ \peer -> do
             launch <- newEmptyTMVarIO
             observed <- newEmptyMVar
             withAsync (atomically (takeTMVar launch) >>= \session -> void (D.sendPrompt session "OFFLINE ONLY" (const (putMVar observed ())))) $ \worker -> do
               D.withDroidSessionOn localOptions (peerTransport peer) $ \session -> do
                 atomically (putTMVar launch session)
                 awaitPrompts peer 1
                 emitText peer
                 takeMVar observed
               settled <- timeout 1000000 (waitCatch worker)
               case settled of
                 Just (Left cause) -> fromException cause @?= Just D.DroidSessionUnusable
                 _ -> assertFailure "SDK consumer remained pending or succeeded after closure"
               count peer "droid.close_session" >>= (@?= 0),
           testCase "caller callback work remains caller-owned while escaped admission fails promptly" $ bounded $ withPeer False $ \peer -> do
             launch <- newEmptyTMVarIO
             entered <- newEmptyMVar
             release <- newEmptyMVar
             withAsync (atomically (takeTMVar launch) >>= \session -> void (D.sendPrompt session "OFFLINE ONLY" (const (putMVar entered () >> takeMVar release)))) $ \worker -> do
               escaped <- D.withDroidSessionOn localOptions (peerTransport peer) $ \session -> do
                 atomically (putTMVar launch session)
                 awaitPrompts peer 1
                 emitText peer
                 takeMVar entered
                 pure session
               poll worker >>= (@?= True) . isNothing
               unusable <- timeout 200000 (try @D.DroidError (void (D.sendPrompt escaped "not admitted" (const (pure ()))))) `finally` putMVar release ()
               unusable @?= Just (Left D.DroidSessionUnusable)
               settled <- timeout 1000000 (waitCatch worker)
               case settled of Just (Left cause) -> fromException cause @?= Just D.DroidSessionUnusable; _ -> assertFailure "Consumer failed to settle after callback release"
               awaitCount <- count peer "droid.add_user_message"
               awaitCount @?= 1
         ]
      <> [ testCase (label <> " overlap rejects before a second submission and leaves the incumbent usable") $ bounded $ withPeer daemon $ \peer ->
             withTurnSession daemon peer $ \send -> do
               withAsync (send (const (pure ()))) $ \first -> do
                 awaitPrompts peer 1
                 withAsync (try @D.DroidError (send (const (pure ())))) $ \second -> do
                   result <- timeout 200000 (wait second)
                   cancel second
                   emitComplete peer
                   wait first
                   result @?= Just (Left D.DroidSessionBusy)
                   promptCount peer >>= (@?= 1)
               withAsync (send (const (pure ()))) $ \next -> do
                 awaitPrompts peer 2
                 emitComplete peer
                 wait next
         | (label, daemon) <- [("local", False), ("daemon", True)]
         ]
      <> [ testCase "a nested prompt from a callback fails fast without invalidating the outer turn" $ bounded $ withPeer False $ \peer ->
             D.withDroidSessionOn localOptions (peerTransport peer) $ \session -> do
               observed <- newEmptyMVar
               withAsync (void (D.sendPrompt session "OFFLINE ONLY" (const (timeout 200000 (try @D.DroidError (void (D.sendPrompt session "nested" (const (pure ()))))) >>= putMVar observed)))) $ \worker -> do
                 awaitPrompts peer 1
                 emitText peer
                 outcome <- takeMVar observed
                 emitComplete peer
                 wait worker
                 outcome @?= Just (Left D.DroidSessionBusy)
                 promptCount peer >>= (@?= 1),
           testCase "caller-owned serialization still supports ordered multiple prompts" $ bounded $ withPeer False $ \peer ->
             withTurnSession False peer $ \send -> do
               gate <- newMVar ()
               let serialized = withMVar gate (const (send (const (pure ()))))
               withAsync serialized $ \first -> do
                 awaitPrompts peer 1
                 withAsync serialized $ \second -> do
                   poll second >>= (@?= True) . isNothing
                   emitComplete peer
                   wait first
                   awaitPrompts peer 2
                   emitComplete peer
                   wait second
                   promptCount peer >>= (@?= 2)
         ]

localOptions :: D.DroidSessionOptions
localOptions = D.defaultDroidSessionOptions "/offline"

withTurnSession :: Bool -> Peer -> (((Text -> IO ()) -> IO ()) -> IO a) -> IO a
withTurnSession daemon peer action
  | daemon = Daemon.withSessionUsing options (peerTransport peer) (\session -> action (void . Daemon.sendPrompt session "OFFLINE ONLY"))
  | otherwise = D.withDroidSessionOn localOptions (peerTransport peer) (\session -> action (void . D.sendPrompt session "OFFLINE ONLY"))
  where
    options = (Daemon.defaultDaemonClientOptions (Daemon.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline") {Daemon.daemonClientRestoreTerminalsOnLoad = False}

closeProtocol :: Peer -> IO ()
closeProtocol peer = do
  params <- case fromJSON (object ["reason" .= String "other"]) of Success value -> pure value; Error problem -> assertFailure problem
  let transport = peerTransport peer
  withRpcChannel (transportSendObject transport) (transportReceiveObject transport) $ \channel ->
    void (Client.closeSession channel (Client.CallOptions "close-contract" (WithEnvelope (Just "1.201.1") Nothing mempty) (Just 1000000)) params)

data Peer = Peer
  { peerTransport :: ObjectTransport,
    peerFrames :: TVar [Object],
    peerIncoming :: TQueue Object,
    peerSession :: TVar Text,
    peerTurn :: TVar Value,
    peerRejectClose :: TVar Bool,
    peerDaemon :: Bool
  }

withPeer :: Bool -> (Peer -> IO a) -> IO a
withPeer daemon action = do
  incoming <- newTQueueIO
  frames <- newTVarIO []
  current <- newTVarIO "fixture"
  turn <- newTVarIO Null
  rejectClose <- newTVarIO False
  let send frame = do
        let params = case field "params" frame of Object value -> value; _ -> mempty
            -- The observed-request barrier shares the turn/session publication.
            record = modifyTVar' frames (<> [frame])
            respond value = case field "id" frame of String identifier -> atomically (writeTQueue incoming (reply identifier value)); _ -> assertFailure "Missing request ID"
        case field "method" frame of
          String method | method `elem` ["droid.initialize_session", "daemon.initialize_session"] -> do
            let identifier = case field "sessionId" params of String value -> value; _ -> "fixture"
            atomically (writeTVar current identifier >> record)
            respond (object ["sessionId" .= identifier, "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"]])
          String method | method `elem` ["droid.add_user_message", "daemon.add_user_message"] -> do
            atomically (writeTVar turn (field "messageId" params) >> record)
            respond (object [])
          String "droid.close_session" -> do
            atomically record
            rejected <- readTVarIO rejectClose
            if rejected
              then case field "id" frame of String identifier -> atomically (writeTQueue incoming (K.insert "error" (object ["code" .= RpcConflict, "message" .= String "fixture rejection"]) (K.delete "result" (reply identifier Null)))); _ -> assertFailure "Missing close ID"
              else respond (object [])
          String method | method `elem` ["droid.interrupt_session", "daemon.interrupt_session"] -> atomically record >> respond (object [])
          _ -> assertFailure "Unexpected lifecycle fixture request"
  action (Peer (objectTransport send (atomically (readTQueue incoming))) frames incoming current turn rejectClose daemon)

field :: Key -> Object -> Value
field key = fromMaybe Null . K.lookup key

count :: Peer -> Text -> IO Int
count peer method = length . filter ((== String method) . field "method") <$> readTVarIO (peerFrames peer)

promptCount :: Peer -> IO Int
promptCount peer = count peer (if peerDaemon peer then "daemon.add_user_message" else "droid.add_user_message")

awaitPrompts :: Peer -> Int -> IO ()
awaitPrompts peer expected = do
  reached <- timeout 2000000 $ atomically $ do
    frames <- readTVar (peerFrames peer)
    let method = if peerDaemon peer then "daemon.add_user_message" else "droid.add_user_message"
    check (length (filter ((== String method) . field "method") frames) >= expected)
  unless (reached == Just ()) (assertFailure "Prompt not submitted")

notify :: Peer -> Value -> IO ()
notify peer event = do
  identifier <- readTVarIO (peerSession peer)
  atomically (writeTQueue (peerIncoming peer) (K.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String (if peerDaemon peer then "daemon.session_notification" else "droid.session_notification"), "params" .= object ["sessionId" .= identifier, "notification" .= event]]))

emitText :: Peer -> IO ()
emitText peer = notify peer (object ["type" .= String "assistant_text_delta", "messageId" .= String "answer", "blockIndex" .= (0 :: Int), "textDelta" .= String "piece"])

emitComplete :: Peer -> IO ()
emitComplete peer = do
  turn <- readTVarIO (peerTurn peer)
  notify peer (object ["type" .= String "agent_turn_completed", "turnId" .= turn, "reason" .= String "completed", "tokenUsage" .= object ["inputTokens" .= (0 :: Int), "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]])
