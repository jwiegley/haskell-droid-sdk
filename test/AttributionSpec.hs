{-# LANGUAGE OverloadedStrings #-}

module AttributionSpec (attributionTests) where

import Control.Concurrent.STM
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as K
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import Factory.Droid qualified as Droid
import Factory.Droid.Daemon qualified as D
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Control (AddUserMessageParams (..), ForkSessionParams (..), defaultUserMessageParams)
import Factory.Droid.Schema.Discovery (GetUserInfoResult (..))
import Factory.Droid.Schema.Enums (SessionOrigin (..))
import Factory.Droid.Schema.Metadata (SdkClientMetadata)
import Factory.Droid.Schema.Sources (SessionSource (..), SessionSourceDetails (SourceApi))
import Factory.Droid.Transport (ObjectTransport, objectTransport)
import Paths_droid_sdk (version)
import ProcessSpec (bounded)
import ProtocolSpec (reply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

attributionTests :: TestTree
attributionTests =
  testGroup "SDK attribution" $
    [ testCase (kind <> " canonical tags: " <> label) $ bounded $ withPeer $ \transport sent -> do
        tags <- traverse (mapM parsed) supplied
        let config = defaultSessionConfiguration {configurationSessionId = Just "attribution", configurationTags = tags}
        if daemon
          then D.withSessionUsing (clientOptions {D.daemonClientConfiguration = config}) transport $ \session -> void (D.sendPrompt session "OFFLINE ONLY" (const (pure ())))
          else Droid.withDroidSessionOn ((Droid.defaultDroidSessionOptions "/offline") {Droid.droidSessionConfiguration = config}) transport $ \session -> void (Droid.sendPrompt session "OFFLINE ONLY" (const (pure ())))
        frames <- readTVarIO sent
        initialized <- frameFor (prefix <> "initialize_session") frames
        submitted <- frameFor (prefix <> "add_user_message") frames
        field "tags" (params initialized) @?= toJSON expected
        field "sessionOriginHint" (params initialized) @?= String "sdk"
        field "sessionSource" (params initialized) @?= if daemon then apiSource "attribution" else Null
        field "userMessageSource" (params submitted) @?= String "sdk"
        noFalseStructuredIdentity initialized
        noFalseStructuredIdentity submitted
    | (label, supplied, expected) <-
        [ ("omitted", Nothing, [sdkTag]),
          ("empty", Just [], [sdkTag]),
          ("custom", Just [customTag], [customTag, sdkTag]),
          ("reserved duplicates", Just [falseSdkTag, customTag, falseSdkTag], [customTag, sdkTag]),
          ("case-sensitive names", Just [upperTag], [upperTag, sdkTag])
        ],
      (kind, prefix, daemon) <- [("local", "droid.", False), ("daemon", "daemon.", True)]
    ]
      <> [ testCase (kind <> " explicit creation origin and source are retained") $ bounded $ withPeer $ \transport sent -> do
             let config = defaultSessionConfiguration {configurationSessionId = Just "attribution", configurationOrigin = Just OriginAPI, configurationSource = Just (SessionSource (SourceApi "explicit") mempty)}
             if daemon
               then D.withSessionUsing (clientOptions {D.daemonClientConfiguration = config}) transport (const (pure ()))
               else Droid.withDroidSessionOn ((Droid.defaultDroidSessionOptions "/offline") {Droid.droidSessionConfiguration = config}) transport (const (pure ()))
             initialized <- readTVarIO sent >>= frameFor (prefix <> "initialize_session")
             field "sessionOriginHint" (params initialized) @?= String "api"
             field "sessionSource" (params initialized) @?= apiSource "explicit"
         | (kind, prefix, daemon) <- [("local", "droid.", False), ("daemon", "daemon.", True)]
         ]
      <> [ testCase "public local resume and replacement carry SDK origin without creation tags" $ bounded $ withPeer $ \transport sent -> do
             Droid.withResumedDroidSessionOn (Droid.defaultDroidSessionOptions "/offline") transport "saved" $ \parent -> do
               child <- Droid.forkDroidSession parent (ForkSessionParams Nothing Nothing mempty)
               void (Droid.sendPrompt child "OFFLINE ONLY" (const (pure ())))
             frames <- readTVarIO sent
             let loads = [params frame | frame <- frames, field "method" frame == String "droid.load_session"]
             length loads @?= 2
             forM_ loads $ \fields -> do
               field "sessionOriginHint" fields @?= String "sdk"
               K.member "tags" fields @?= False
               K.member "sessionSource" fields @?= False,
           testCase "local explicit resume origin and source override the defaults" $ bounded $ withPeer $ \transport sent -> do
             let config = defaultSessionLoadConfiguration {loadOrigin = Just OriginCliTui, loadSource = Just (SessionSource (SourceApi "explicit") mempty)}
             Droid.withResumedDroidSessionOn ((Droid.defaultDroidSessionOptions "/offline") {Droid.droidSessionLoadConfiguration = config}) transport "saved" (const (pure ()))
             loaded <- readTVarIO sent >>= frameFor "droid.load_session"
             field "sessionOriginHint" (params loaded) @?= String "cli_tui"
             field "sessionSource" (params loaded) @?= apiSource "explicit",
           testCase "connection-owning daemon resume shares SDK surface attribution" $ bounded $ withPeer $ \transport sent -> do
             D.withResumedSessionUsing clientOptions transport "saved" (const (pure ()))
             loaded <- readTVarIO sent >>= frameFor "daemon.load_session"
             field "sessionOriginHint" (params loaded) @?= String "sdk"
             field "sessionSource" (params loaded) @?= apiSource "saved",
           testCase "public daemon attachment adds a surface without stashing inferred attribution" $ bounded $ withPeer $ \transport sent ->
             D.withConnectionOn clientOptions transport $ \connection -> do
               D.withResumedSessionOn connection "saved" $ \session -> void (D.sendPrompt session "OFFLINE ONLY" (const (pure ())))
               loaded <- readTVarIO sent >>= frameFor "daemon.load_session"
               field "sessionOriginHint" (params loaded) @?= String "sdk"
               field "sessionSource" (params loaded) @?= apiSource "saved"
               policy <- D.getSessionLoadOptions (D.connectionState connection) "saved" >>= maybe (assertFailure "Missing retained options") pure
               loadOrigin (fst policy) @?= Nothing
               loadSource (fst policy) @?= Nothing
               submitted <- readTVarIO sent >>= frameFor "daemon.add_user_message"
               field "userMessageSource" (params submitted) @?= String "sdk",
           testCase "default daemon creation attribution does not become automatic reload intent" $ bounded $ withPeer $ \transport sent ->
             D.withConnectionOn clientOptions transport $ \connection -> do
               let options = D.defaultDaemonSessionOptions "/offline"
                   initial = (D.daemonSessionParameters options) {initializeConfiguration = defaultSessionConfiguration {configurationSessionId = Just "saved"}}
               D.withSessionOn connection (options {D.daemonSessionParameters = initial}) (const (pure ()))
               D.markSessionNotLoaded connection "saved"
               D.ensureSessionLoaded connection "saved"
               loaded <- readTVarIO sent >>= frameFor "daemon.load_session"
               field "sessionOriginHint" (params loaded) @?= Null
               field "sessionSource" (params loaded) @?= Null,
           testCase "explicit daemon attribution remains caller-owned future load intent" $ bounded $ withPeer $ \transport sent ->
             D.withConnectionOn clientOptions transport $ \connection -> do
               let config = defaultSessionLoadConfiguration {loadOrigin = Just OriginCliTui, loadSource = Just (SessionSource (SourceApi "explicit") mempty)}
               D.withResumedSessionOnConfigured connection Droid.defaultDroidHandlers "saved" config defaultDaemonLoadConfiguration (const (pure ()))
               D.markSessionNotLoaded connection "saved"
               D.ensureSessionLoaded connection "saved"
               frames <- readTVarIO sent
               let loads = [params frame | frame <- frames, field "method" frame == String "daemon.load_session"]
               length loads @?= 2
               forM_ loads $ \fields -> do
                 field "sessionOriginHint" fields @?= String "cli_tui"
                 field "sessionSource" fields @?= apiSource "explicit",
           testCase "connection-level submissions default to SDK and retain an explicit source" $ bounded $ withPeer $ \transport sent ->
             D.withConnectionOn clientOptions transport $ \connection -> do
               void (D.submitUserMessage connection "saved" "one" (defaultUserMessageParams "OFFLINE ONLY"))
               void (D.submitUserMessage connection "saved" "two" ((defaultUserMessageParams "OFFLINE ONLY") {userMessageSource = Just OriginAPI}))
               frames <- readTVarIO sent
               [field "userMessageSource" (params frame) | frame <- frames, field "method" frame == String "daemon.add_user_message"] @?= [String "sdk", String "api"],
           testCase "raw initialization and message codecs retain omission and caller tags" $ do
             let fields = asObject (toJSON (defaultInitializeSessionParams "machine" "/offline"))
             forM_ ["tags", "sessionSource", "sessionOriginHint"] $ \key -> K.member key fields @?= False
             K.member "userMessageSource" (asObject (toJSON (defaultUserMessageParams "text"))) @?= False
             tag <- parsed falseSdkTag
             let config = defaultSessionConfiguration {configurationTags = Just [tag]}
             field "tags" (initializationFields ((defaultInitializeSessionParams "machine" "/offline") {initializeConfiguration = config})) @?= toJSON [falseSdkTag],
           testCase "closed structured SDK language remains closed rather than impersonating a supported SDK" $ do
             case fromJSON (object ["language" .= String "haskell", "version" .= packageVersion]) :: Result SdkClientMetadata of
               Error _ -> pure ()
               Success _ -> assertFailure "Pinned structured SDK language domain was widened"
         ]

packageVersion :: Text
packageVersion = Text.pack (showVersion version)

sdkTag, customTag, falseSdkTag, upperTag :: Value
sdkTag = object ["name" .= String "sdk", "metadata" .= object ["language" .= String "haskell", "version" .= packageVersion]]
customTag = object ["name" .= String "custom", "metadata" .= object ["value" .= String "kept"], "extension" .= False]
falseSdkTag = object ["name" .= String "sdk", "metadata" .= object ["language" .= String "pretend", "version" .= String "9"]]
upperTag = object ["name" .= String "SDK", "metadata" .= object ["value" .= String "literal"]]

apiSource :: Text -> Value
apiSource identifier = object ["platform" .= String "api", "delegationSessionId" .= identifier]

parsed :: (FromJSON a) => Value -> IO a
parsed value = case fromJSON value of Success result -> pure result; Error problem -> assertFailure problem

field :: Key -> Object -> Value
field key = fromMaybe Null . K.lookup key

asObject :: Value -> Object
asObject (Object value) = value
asObject _ = mempty

params :: Object -> Object
params = asObject . field "params"

frameFor :: Text -> [Object] -> IO Object
frameFor method frames = case [frame | frame <- frames, field "method" frame == String method] of
  [frame] -> pure frame
  _ -> assertFailure ("Expected one controlled frame for " <> Text.unpack method)

noFalseStructuredIdentity :: Object -> IO ()
noFalseStructuredIdentity frame = K.lookup "requestAttribution" (asObject (field "_meta" frame)) @?= Nothing

clientOptions :: D.DaemonClientOptions
clientOptions = (D.defaultDaemonClientOptions (D.DaemonInheritAuthentication (GetUserInfoResult "fixture-user" "fixture-org" mempty) "OFFLINE_ONLY") "/offline") {D.daemonClientRestoreTerminalsOnLoad = False}

withPeer :: (ObjectTransport -> TVar [Object] -> IO a) -> IO a
withPeer action = do
  incoming <- newTQueueIO
  sent <- newTVarIO []
  current <- newTVarIO ("attribution" :: Text)
  let send frame = do
        atomically (modifyTVar' sent (<> [frame]))
        let fields = params frame
            respond value = case field "id" frame of String identifier -> atomically (writeTQueue incoming (reply identifier value)); _ -> assertFailure "Missing request ID"
        case field "method" frame of
          String method | method `elem` ["droid.initialize_session", "daemon.initialize_session", "droid.load_session", "daemon.load_session"] -> do
            let identifier = case field "sessionId" fields of String value -> value; _ -> "attribution"
            atomically (writeTVar current identifier)
            respond (object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "fixture", "reasoningEffort" .= String "low"], "cwd" .= String "/offline"])
          String "droid.fork_session" -> respond (object ["newSessionId" .= String "child"])
          String method | method `elem` ["droid.add_user_message", "daemon.add_user_message"] -> do
            identifier <- case field "sessionId" fields of String value -> pure value; _ -> readTVarIO current
            respond (object [])
            let notification = object ["type" .= String "agent_turn_completed", "reason" .= String "completed", "turnId" .= field "messageId" fields, "tokenUsage" .= object ["inputTokens" .= (0 :: Int), "outputTokens" .= (0 :: Int), "cacheCreationTokens" .= (0 :: Int), "cacheReadTokens" .= (0 :: Int), "thinkingTokens" .= (0 :: Int)]]
                channel = if method == "droid.add_user_message" then "droid.session_notification" else "daemon.session_notification"
            atomically (writeTQueue incoming (K.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1", "type" .= String "notification", "method" .= String channel, "params" .= object ["sessionId" .= identifier, "notification" .= notification]]))
          _ -> assertFailure "Unexpected attribution fixture request"
  action (objectTransport send (atomically (readTQueue incoming))) sent
