{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module McpConfigSpec (mcpConfigTests, fixtureMcpOptions, fixtureMcpWire, fixtureStoredServer) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, parseJSON)
import Factory.Droid.Schema.MCP (HttpHeader (..), StdioMcp (..))
import Factory.Droid.Schema.MCP.Config
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

mcpConfigTests :: TestTree
mcpConfigTests = testGroup "MCP configuration" configurationCases

configurationCases :: [TestTree]
configurationCases =
  [ testCase "startup normalization preserves transport/header/omit/false distinctions" $ do
      normalized <- either (const (assertFailure "Invalid fixture configuration")) pure (validateMcpConfiguration fixtureMcpOptions)
      KeyMap.lookup "mcpServers" (mcpInitializeFields normalized) @?= Just (toJSON fixtureMcpWire)
      KeyMap.lookup "blockOnMcpLoad" (mcpLoadFields normalized) @?= Nothing
      KeyMap.lookup "mcpOAuthCallbackUri" (mcpLoadFields normalized) @?= Just (String "fixture:callback")
      toJSON defaultMcpSessionOptions @?= object []
      toJSON (defaultMcpSessionOptions {sessionMcpServers = Just []}) @?= object ["mcpServers" .= ([] :: [Value])],
    testCase "add conversion uses last exact header name and leaves case distinct" $ do
      let remote = McpRemoteConfig "remote" "https://mcp.invalid" (Just [HttpHeader "X-Key" "first" mempty, HttpHeader "x-key" "distinct" mempty, HttpHeader "X-Key" "last" mempty]) (Just McpOAuthDisabled) mempty
          added = mcpServerParams (McpHttpConfig remote)
      addedMcpHeaders added @?= Just (KeyMap.fromList [("X-Key", "last"), ("x-key", "distinct")])
      addedMcpOAuth added @?= Just McpOAuthDisabled,
    testCase "OAuth trims ECMAScript scopes/client ID but not the secret" $ do
      let value = object ["scopes" .= [String " read ", String "\xfeffwrite\xfeff", String "\x85"], "clientId" .= String "\xfeff client \t", "clientSecret" .= String " secret ", "authorizationServerIssuer" .= String "https://issuer.invalid", "callbackPort" .= (65535 :: Int)]
      options <- parsed @McpOAuthOptions value
      mcpOAuthScopes options @?= Just ["read", "write", "\x85"]
      mcpOAuthClientId options @?= Just "client"
      mcpOAuthClientSecret options @?= Just " secret "
      mcpOAuthCallbackPort options @?= Just 65535,
    testCase "OAuth cross-field validation requires issuer and public metadata clients" $ do
      forM_ [object ["clientId" .= String "client"], object ["clientSecret" .= String "secret"], object ["clientMetadataUrl" .= String "https://client.invalid/metadata", "clientId" .= String "client", "authorizationServerIssuer" .= String "https://issuer.invalid"], object ["clientMetadataUrl" .= String "https://client.invalid/metadata", "tokenEndpointAuthMethod" .= String "client_secret_basic"]] (rejected @McpOAuthOptions)
      _ <- parsed @McpOAuthOptions (object ["clientSecret" .= String "secret", "authorizationServerIssuer" .= String "urn:issuer"])
      _ <- parsed @McpOAuthOptions (object ["clientMetadataUrl" .= String "HTTPS://client.invalid/metadata", "tokenEndpointAuthMethod" .= String "none"])
      pure (),
    testCase "OAuth rejects blank credentials/scopes, true, null and out-of-range ports" $ do
      forM_ [object ["scopes" .= [String "\xfeff"]], object ["clientId" .= String "\t"], object ["clientSecret" .= String " \xfeff "], object ["resource" .= True], object ["callbackPort" .= (0 :: Int)], object ["callbackPort" .= (65536 :: Int)], object ["callbackPort" .= (-1 :: Int)], object ["callbackPort" .= (1.5 :: Double)]] (rejected @McpOAuthOptions)
      forM_ ["scopes", "resource", "authorizationServerIssuer", "clientMetadataUrl", "clientId", "clientSecret", "callbackPort", "tokenEndpointAuthMethod"] $ \key -> rejected @McpOAuthOptions (Object (KeyMap.singleton key Null))
      rejected @McpOAuthConfig (Bool True)
      rejected @McpOAuthConfig Null,
    testCase "metadata URIs reject roots, credentials, contents and encoded dot segments" $ do
      forM_ ["http://client.invalid/a", "https://client.invalid", "https://client.invalid/", "https://name@client.invalid/a", "https://client.invalid/a?q=1", "https://client.invalid/a#fragment", "https://client.invalid/a/../b", "https://client.invalid/a/%2e/b", "https://client.invalid/a/.%2E/b", "https://client.invalid/a/%2e%2e?x", "https://client.invalid/a b"] $ \url -> rejected @McpOAuthOptions (object ["clientMetadataUrl" .= String url])
      forM_ ["https://client.invalid/metadata", "https://client.invalid/a..b", "https://client.invalid/a?", "https://client.invalid/a#", "https://client.invalid/a%20b"] $ \url -> do
        value <- parsed @McpOAuthOptions (object ["clientMetadataUrl" .= String url])
        mcpOAuthMetadataUrl value @?= Just url,
    testCase "startup accepts generic URIs and does not impose Python-only restrictions" $ do
      _ <- parsed @McpServerConfig (object ["type" .= String "http", "name" .= String "", "url" .= String "urn:mcp:fixture", "headers" .= [object ["name" .= String "", "value" .= String "wire data"]]])
      _ <- parsed @McpServerConfig (object ["name" .= String "", "command" .= String ""])
      rejected @McpServerConfig (object ["type" .= String "http", "name" .= String "remote", "url" .= String "relative/path"])
      rejected @McpServerConfig (object ["type" .= String "http", "name" .= String "remote", "url" .= String "https://mcp.invalid", "headers" .= object []]),
    testCase "transport extensions cannot change an explicitly constructed branch" $ do
      let remote = McpRemoteConfig "remote" "https://mcp.invalid" Nothing Nothing (KeyMap.fromList ["command" .= String "not-a-stdio-command", "type" .= String "stdio", "future" .= True])
      toJSON (McpHttpConfig remote) @?= object ["type" .= String "http", "name" .= String "remote", "url" .= String "https://mcp.invalid", "future" .= True]
      let stdio = StdioMcp "local" "not-executed" Nothing Nothing (KeyMap.fromList ["type" .= String "http", "url" .= String "https://wrong.invalid", "future" .= True])
      toJSON (McpStdioConfig stdio) @?= object ["name" .= String "local", "command" .= String "not-executed", "future" .= True],
    testCase "global config reads false OAuth but update config cannot decode it" $ do
      let remote = object ["type" .= String "http", "url" .= String "https://mcp.invalid", "oauth" .= False, "disabled" .= False]
      parsed @(StoredMcpConfig McpOAuthConfig) remote >>= (@?= remote) . toJSON
      rejected @(StoredMcpConfig McpOAuthOptions) remote
      rejected @UpdateMcpConfigParams (object ["action" .= String "add", "serverNames" .= [String "remote"], "serverConfig" .= remote]),
    testCase "global stdio requires args and preserves omitted versus explicit type" $ do
      forM_ [object ["command" .= String "cmd", "args" .= ([] :: [Value])], object ["type" .= String "stdio", "command" .= String "cmd", "args" .= ([] :: [Value]), "disabled" .= False]] $ \value -> parsed @(StoredMcpConfig McpOAuthConfig) value >>= (@?= value) . toJSON
      rejected @(StoredMcpConfig McpOAuthConfig) (object ["command" .= String "cmd"]),
    testCase "configuration results retain false, error and source independently" $ do
      let value = object ["success" .= False, "servers" .= [fixtureStoredServer], "error" .= String "declined", "future" .= True]
      parsed @UpdateMcpConfigResult value >>= (@?= value) . toJSON
      rejected @McpServerInfo (object ["name" .= String "remote", "config" .= object ["command" .= String "cmd", "args" .= ([] :: [Value])], "source" .= String "unknown"]),
    testCase "programmatically constructed invalid values fail normalization without disclosure" $ do
      validateMcpConfiguration (emptyMcpOAuthOptions {mcpOAuthClientSecret = Just "fixture-secret"}) @?= Left InvalidMcpConfiguration
      show fixtureMcpOptions @?= "McpSessionOptions <redacted>"
      show (emptyMcpOAuthOptions {mcpOAuthClientSecret = Just "fixture-secret"}) @?= "McpOAuthOptions <redacted>"
  ]

parsed :: (FromJSON a) => Value -> IO a
parsed value = either (const (assertFailure "Expected valid MCP configuration")) pure (parseEither parseJSON value)

rejected :: forall a. (FromJSON a) => Value -> IO ()
rejected value = case parseEither parseJSON value :: Either String a of
  Left _ -> pure ()
  Right _ -> assertFailure "Accepted invalid MCP configuration"

fixtureMcpOptions :: McpSessionOptions
fixtureMcpOptions = McpSessionOptions (Just servers) (Just "fixture:callback") (Just True)
  where
    oauth = emptyMcpOAuthOptions {mcpOAuthScopes = Just [" read "], mcpOAuthClientId = Just " fixture-client ", mcpOAuthClientSecret = Just " fixture-secret ", mcpOAuthIssuer = Just "https://issuer.invalid"}
    headers = Just [HttpHeader "X-Fixture" "first" mempty, HttpHeader "X-Fixture" "last" mempty]
    servers = [McpStdioConfig (StdioMcp "stdio" "not-executed" (Just ["arg"]) (Just (KeyMap.singleton "FIXTURE_ENV" "value")) mempty), McpHttpConfig (McpRemoteConfig "http" "https://mcp.invalid" headers (Just (McpOAuthEnabled oauth)) mempty), McpSseConfig (McpRemoteConfig "sse" "https://sse.invalid" (Just []) (Just McpOAuthDisabled) mempty)]

fixtureMcpWire :: [Value]
fixtureMcpWire = [object ["name" .= String "stdio", "command" .= String "not-executed", "args" .= [String "arg"], "env" .= object ["FIXTURE_ENV" .= String "value"]], object ["type" .= String "http", "name" .= String "http", "url" .= String "https://mcp.invalid", "headers" .= [object ["name" .= String "X-Fixture", "value" .= String "first"], object ["name" .= String "X-Fixture", "value" .= String "last"]], "oauth" .= object ["scopes" .= [String "read"], "clientId" .= String "fixture-client", "clientSecret" .= String " fixture-secret ", "authorizationServerIssuer" .= String "https://issuer.invalid"]], object ["type" .= String "sse", "name" .= String "sse", "url" .= String "https://sse.invalid", "headers" .= ([] :: [Value]), "oauth" .= False]]

fixtureStoredServer :: Value
fixtureStoredServer = object ["name" .= String "remote", "config" .= object ["type" .= String "http", "url" .= String "https://mcp.invalid", "oauth" .= False, "disabled" .= False], "source" .= String "project", "status" .= String "disconnected", "future" .= True]
