{-# LANGUAGE OverloadedStrings #-}

module DaemonPluginSpec (pluginTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever, void)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid (DroidError (DroidSessionUnusable), DroidSessionStatus (..))
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Plugin
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Schema.Session (SessionIdParams (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

pluginTests :: TestTree
pluginTests =
  testGroup
    "Daemon plugins and marketplaces"
    [ testCase "Git refs trim ECMAScript whitespace while SHA pins preserve either case" $ do
        fmap marketplaceGitRefText (mkMarketplaceGitRef "\xfeff\t topic with spaces \n") @?= Just "topic with spaces"
        mkMarketplaceGitRef "\xfeff\t \r\n" @?= Nothing
        fmap marketplaceGitRefText (mkMarketplaceGitRef "\x85") @?= Just "\x85"
        fmap marketplaceGitShaText (mkMarketplaceGitSha sha) @?= Just sha
        forM_ [Text.replicate 39 "a", Text.replicate 41 "a", Text.replicate 40 "g", " " <> sha, sha <> "\n"] $ \bad -> mkMarketplaceGitSha bad @?= Nothing,
      testCase "input source variants normalize refs and reported local sources discard private path data" $ do
        forM_ [(object ["source" .= String "github", "repo" .= String "org/repo", "ref" .= String "\xfeff main \t", "sha" .= sha], githubWire), (object ["source" .= String "url", "url" .= String "opaque", "ref" .= String " branch "], object ["source" .= String "url", "url" .= String "opaque", "ref" .= String "branch"]), (object ["source" .= String "local", "path" .= String "../remote"], object ["source" .= String "local", "path" .= String "../remote"]), (object ["source" .= String "git-subdir", "url" .= String "opaque", "path" .= String "subdir", "sha" .= sha], object ["source" .= String "git-subdir", "url" .= String "opaque", "path" .= String "subdir", "sha" .= sha])] $ \(input, expected) -> decodeValue @MarketplaceSource input >>= (@?= expected) . toJSON
        forM_ [object ["source" .= String "future"], object ["source" .= String "local"], object ["source" .= String "github", "repo" .= String "repo", "ref" .= Null], object ["source" .= String "url", "url" .= String "url", "sha" .= String "short"]] $ rejects (Proxy @MarketplaceSource)
        local <- decodeValue @ReportedMarketplaceSource (object ["source" .= String "local", "path" .= String "PRIVATE_PATH", "ref" .= String "PRIVATE_REF"])
        toJSON local @?= object ["source" .= String "local"]
        show local @?= "ReportedMarketplaceSource <redacted>"
        forM_ [object ["source" .= String "github", "repo" .= String "repo"], object ["source" .= String "url", "url" .= String "opaque"], object ["source" .= String "git-subdir", "url" .= String "url", "path" .= String "path"]] $ roundTrip (Proxy @ReportedMarketplaceSource),
      testCase "installed and marketplace metadata preserve unknown versus false and do not infer policy" $ do
        installed <- decodeValue @InstalledPlugin installedWire
        installedPluginActive installed @?= Just False
        installedPluginManaged installed @?= Just True
        installedPluginReason installed @?= Just PluginNotEnabled
        toJSON installed @?= installedWire
        marketplace <- decodeValue @MarketplaceInfo marketplaceWire
        marketplaceRemovable marketplace @?= Just False
        marketplaceProvisionedBy marketplace @?= Just MarketplaceProject
        toJSON marketplace @?= marketplaceWire
        let minimal = object ["name" .= String "old", "source" .= object ["source" .= String "local"], "pluginCount" .= Number (-0.5), "autoUpdate" .= False]
        old <- decodeValue @MarketplaceInfo minimal
        marketplaceRemovable old @?= Nothing
        marketplaceProvisionedBy old @?= Nothing
        toJSON old @?= minimal
        legacy <- decodeValue @InstalledPlugin (object ["id" .= String "old", "scope" .= String "", "version" .= String "", "installPath" .= String "", "installedAt" .= String "", "lastUpdated" .= String "", "source" .= String ""])
        installedPluginActive legacy @?= Nothing
        installedPluginManaged legacy @?= Nothing
        installedPluginReason legacy @?= Nothing
        forM_ ["active", "managed", "reason"] $ \key -> rejects (Proxy @InstalledPlugin) (insertField key Null installedWire)
        forM_ ["displayName", "provisionedBy", "removable"] $ \key -> rejects (Proxy @MarketplaceInfo) (insertField key Null marketplaceWire)
        rejects (Proxy @InstalledPlugin) (insertField "reason" (String "future") installedWire)
        rejects (Proxy @MarketplaceInfo) (insertField "provisionedBy" (String "user") marketplaceWire),
      testCase "input/result records retain fields, false errors and mixed update batches" $ do
        roundTrip (Proxy @PluginScopeParams) (object ["scope" .= String "", "future" .= False])
        roundTrip (Proxy @InstallPluginParams) installWire
        roundTrip (Proxy @PluginTargetParams) targetWire
        roundTrip (Proxy @SetPluginEnabledParams) enabledWire
        roundTrip (Proxy @UpdatePluginParams) (object ["pluginId" .= String "", "scope" .= String "", "future" .= False])
        toJSON defaultUpdatePluginParams @?= object []
        roundTrip (Proxy @AddMarketplaceParams) (object ["source" .= githubWire])
        roundTrip (Proxy @MarketplaceNameParams) (object ["name" .= String ""])
        roundTrip (Proxy @UpdateMarketplaceParams) (object [])
        roundTrip (Proxy @UpdateMarketplaceParams) (object ["name" .= String ""])
        roundTrip (Proxy @ListAvailablePluginsResult) availableListWire
        roundTrip (Proxy @ListInstalledPluginsResult) installedListWire
        roundTrip (Proxy @ListMarketplacesResult) marketplaceListWire
        roundTrip (Proxy @InstallPluginResult) (object ["success" .= False, "pluginId" .= String "", "error" .= String ""])
        roundTrip (Proxy @AddMarketplaceResult) (object ["success" .= False, "name" .= String "", "error" .= String ""])
        roundTrip (Proxy @UpdatePluginResult) pluginUpdatesWire
        roundTrip (Proxy @UpdateMarketplaceResult) marketplaceUpdatesWire
        rejects (Proxy @PluginScopeParams) (object ["scope" .= Null])
        rejects (Proxy @UpdatePluginParams) (object ["pluginId" .= Null])
        rejects (Proxy @UpdateMarketplaceParams) (object ["name" .= Null])
        rejects (Proxy @InstallPluginResult) (object ["success" .= False, "error" .= Null])
        let value = SetPluginEnabledParams (PluginTargetParams "plugin" "" (KeyMap.singleton "enabled" (Bool True))) False
        toJSON value @?= object ["pluginId" .= String "plugin", "scope" .= String "", "enabled" .= False],
      testCase "all ten owned operations preserve exact requests and per-operation results" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withPluginPeer Normal trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
          forM_ (zip wireCases (highCalls session)) $ \((_, _, expected), request) -> request >>= (@?= expected)
          Daemon.sessionStatus session >>= (@?= SessionReady)
        frames <- readIORef trace
        map (field "method") frames @?= map String (["daemon.authenticate", "daemon.initialize_session"] <> map (\(method, _, _) -> method) wireCases),
      testCase "RPC rejection and malformed data remain explicit without retiring ordinary-result sessions" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withPluginPeer mode trace ready $ \target -> Daemon.withSession (options target) $ \session ->
              forM_ (highCalls session) $ \request -> do
                outcome <- try @RpcResultError request
                case outcome of
                  Left (RpcRemoteFailure err) | mode == Rejected -> rpcErrorCode err @?= RpcInvalidParams
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected plugin RPC outcome"
                Daemon.sessionStatus session >>= (@?= SessionReady),
      testCase "read cancellation permits reuse but uncertain install cancellation interrupts and invalidates" $
        bounded $
          forM_ [HeldRead, HeldInstall] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            withPluginPeer mode trace ready $ \target -> Daemon.withSession (options target) $ \session -> do
              let request = if mode == HeldRead then void (Daemon.listAvailablePlugins session) else void (Daemon.installPlugin session installParams)
              withAsync request $ \pending -> do
                takeMVar ready
                cancel pending
                waitCatch pending >>= \case
                  Left cause -> fromException cause @?= Just AsyncCancelled
                  Right _ -> assertFailure "Cancelled plugin operation returned"
              if mode == HeldRead
                then do
                  Daemon.sessionStatus session >>= (@?= SessionReady)
                  Daemon.listMarketplaces session >>= (@?= marketplaceListWire) . toJSON
                else do
                  Daemon.sessionStatus session >>= (@?= SessionUnavailable)
                  try @DroidError (Daemon.listMarketplaces session) >>= (@?= Left DroidSessionUnusable)
            frames <- readIORef trace
            map (field "method") (drop 2 frames) @?= if mode == HeldRead then map String ["daemon.list_available_plugins", "daemon.list_marketplaces"] else map String ["daemon.install_plugin", "daemon.interrupt_session"],
      testCase "all low-level operations preserve scoped arguments and caller deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "raw" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
          forM_ (zip wireCases (lowCalls channel configured)) $ \((method, params, result), request) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String method
            field "params" frame @?= insertField "sessionId" (String "owned") params
            atomically (writeTQueue incoming (response frame result))
            wait pending >>= (@?= result)
          forM_ (lowCalls channel (configured {Client.callTimeoutMicros = Just 0})) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

highCalls :: Daemon.DaemonSession -> [IO Value]
highCalls session = [toJSON <$> Daemon.listAvailablePlugins session, toJSON <$> Daemon.listInstalledPlugins session (Just ""), toJSON <$> Daemon.installPlugin session installParams, toJSON <$> Daemon.uninstallPlugin session targetParams, toJSON <$> Daemon.setPluginEnabled session (SetPluginEnabledParams targetParams False), toJSON <$> Daemon.updatePlugin session defaultUpdatePluginParams, toJSON <$> Daemon.listMarketplaces session, toJSON <$> Daemon.addMarketplace session githubSource, toJSON <$> Daemon.removeMarketplace session "catalog", toJSON <$> Daemon.updateMarketplace session Nothing]

lowCalls :: RpcChannel -> Client.CallOptions -> [IO Value]
lowCalls channel configured = [toJSON <$> Client.listDaemonAvailablePlugins channel configured scoped, toJSON <$> Client.listDaemonInstalledPlugins channel configured "owned" (PluginScopeParams (Just "") mempty), toJSON <$> Client.installDaemonPlugin channel configured "owned" installParams, toJSON <$> Client.uninstallDaemonPlugin channel configured "owned" targetParams, toJSON <$> Client.setDaemonPluginEnabled channel configured "owned" (SetPluginEnabledParams targetParams False), toJSON <$> Client.updateDaemonPlugin channel configured "owned" defaultUpdatePluginParams, toJSON <$> Client.listDaemonMarketplaces channel configured scoped, toJSON <$> Client.addDaemonMarketplace channel configured "owned" (AddMarketplaceParams githubSource mempty), toJSON <$> Client.removeDaemonMarketplace channel configured "owned" (MarketplaceNameParams "catalog" mempty), toJSON <$> Client.updateDaemonMarketplace channel configured "owned" (UpdateMarketplaceParams Nothing mempty)]
  where
    scoped = SessionIdParams "owned" mempty

installParams :: InstallPluginParams
installParams = InstallPluginParams "catalog" "plugin" "" (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])

targetParams :: PluginTargetParams
targetParams = PluginTargetParams "plugin" "" (KeyMap.fromList ["sessionId" .= String "wrong", "future" .= False])

githubSource :: MarketplaceSource
githubSource = GithubMarketplace "org/repo" (mkMarketplaceGitRef "\xfeff main \t") (mkMarketplaceGitSha sha)

sha :: Text
sha = Text.replicate 20 "aB"

wireCases :: [(Text, Value, Value)]
wireCases = [("daemon.list_available_plugins", object [], availableListWire), ("daemon.list_installed_plugins", object ["scope" .= String ""], installedListWire), ("daemon.install_plugin", installWire, object ["success" .= False, "pluginId" .= String "", "error" .= String "managed"]), ("daemon.uninstall_plugin", targetWire, failureWire), ("daemon.set_plugin_enabled", enabledWire, failureWire), ("daemon.update_plugin", object [], pluginUpdatesWire), ("daemon.list_marketplaces", object [], marketplaceListWire), ("daemon.add_marketplace", object ["source" .= githubWire], object ["success" .= True, "name" .= String "catalog"]), ("daemon.remove_marketplace", object ["name" .= String "catalog"], failureWire), ("daemon.update_marketplace", object [], marketplaceUpdatesWire)]

data Mode = Normal | Rejected | Malformed | HeldRead | HeldInstall deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withPluginPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withPluginPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
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
      reply connection initialize (object ["sessionId" .= identifier, "session" .= object ["messages" .= ([] :: [Value])], "settings" .= object ["modelId" .= String "offline", "reasoningEffort" .= String "low"]])
      forever $ do
        request <- readFrame connection
        if field "method" request == String "daemon.interrupt_session"
          then do
            mode @?= HeldInstall
            field "params" request @?= object ["sessionId" .= identifier]
            reply connection request (object [])
          else case [(method, params, result) | (method, params, result) <- wireCases, field "method" request == String method] of
            [(method, params, result)] -> do
              field "params" request @?= insertField "sessionId" (String identifier) params
              if (mode == HeldRead && method == "daemon.list_available_plugins") || (mode == HeldInstall && method == "daemon.install_plugin")
                then putMVar ready ()
                else case mode of
                  Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Plugin request rejected"]]
                  Malformed -> reply connection request Null
                  _ -> reply connection request result
            _ -> assertFailure "Unexpected plugin request"
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

response :: Object -> Value -> Object
response request result = KeyMap.fromList (envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result])

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . object . envelope

envelope :: [Pair] -> [Pair]
envelope fields = ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

insertField :: Key -> Value -> Value -> Value
insertField key value (Object fields) = Object (KeyMap.insert key value fields)
insertField _ _ _ = error "Expected object fixture"

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decodeValue @a value >>= (@?= value) . toJSON

githubWire, installWire, targetWire, enabledWire, availableListWire, installedWire, installedListWire, marketplaceWire, marketplaceListWire, failureWire, pluginUpdatesWire, marketplaceUpdatesWire :: Value
githubWire = object ["source" .= String "github", "repo" .= String "org/repo", "ref" .= String "main", "sha" .= sha]
installWire = object ["marketplace" .= String "catalog", "pluginName" .= String "plugin", "scope" .= String "", "future" .= False]
targetWire = object ["pluginId" .= String "plugin", "scope" .= String "", "future" .= False]
enabledWire = insertField "enabled" (Bool False) targetWire
availableListWire = object ["plugins" .= [object ["name" .= String "plugin", "marketplace" .= String "catalog", "description" .= String ""]]]
installedWire = object ["id" .= String "plugin", "scope" .= String "future-scope", "version" .= String "1", "installPath" .= String "/remote/plugin", "installedAt" .= String "opaque", "lastUpdated" .= String "opaque", "source" .= String "catalog", "active" .= False, "managed" .= True, "reason" .= String "not enabled"]
installedListWire = object ["plugins" .= [installedWire]]
marketplaceWire = object ["name" .= String "catalog", "displayName" .= String "", "source" .= object ["source" .= String "local"], "pluginCount" .= Number 2, "autoUpdate" .= False, "provisionedBy" .= String "project", "removable" .= False]
marketplaceListWire = object ["marketplaces" .= [marketplaceWire]]
failureWire = object ["success" .= False, "error" .= String ""]
pluginUpdatesWire = object ["results" .= [object ["pluginId" .= String "one", "success" .= True], object ["pluginId" .= String "two", "success" .= False, "error" .= String ""]]]
marketplaceUpdatesWire = object ["results" .= [object ["name" .= String "one", "success" .= False, "error" .= String ""], object ["name" .= String "two", "success" .= True]]]
