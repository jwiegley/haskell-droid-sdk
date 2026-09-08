{-# LANGUAGE OverloadedStrings #-}

module McpPeer (handleMcpRequest, earlyMcpEvents, invokeHosted) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Object, Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.Foldable (toList)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hAccept, hContentType, statusCode)
import Test.Tasty.HUnit (assertFailure, (@?=))

-- A native offline CLI/daemon peer consumes the advertised endpoint and calls
-- the Haskell handler over real HTTP before acknowledging startup/loading.
invokeHosted :: Object -> IO ()
invokeHosted params = case KeyMap.lookup "mcpServers" params of
  Just (Array configs) -> forM_ configs $ \case
    Object fields | KeyMap.lookup "name" fields == Just (String "hosted-fixture") -> do
      url <- case KeyMap.lookup "url" fields of Just (String value) -> pure value; _ -> assertFailure "Missing hosted URL"
      suppliedHeaders <- case KeyMap.lookup "headers" fields of
        Just (Array values) -> traverse header values
        _ -> assertFailure "Missing hosted authorization"
      KeyMap.lookup "oauth" fields @?= Just (Bool False)
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      request <- HTTP.parseRequest (Text.unpack url)
      let arguments = object ["source" .= String "offline-peer"]
          payload = object ["jsonrpc" .= String "2.0", "id" .= String "hosted", "method" .= String "tools/call", "params" .= object ["name" .= String "echo", "arguments" .= arguments]]
          configured = request {HTTP.method = "POST", HTTP.requestHeaders = [(hAccept, "application/json, text/event-stream"), (hContentType, "application/json")] <> toList suppliedHeaders, HTTP.requestBody = HTTP.RequestBodyLBS (encode payload)}
      response <- HTTP.httpLbs configured manager
      statusCode (HTTP.responseStatus response) @?= 200
      result <- either (const (assertFailure "Invalid hosted result JSON")) pure (eitherDecode (HTTP.responseBody response))
      case result of
        Object envelope | Just (Object value) <- KeyMap.lookup "result" envelope -> KeyMap.lookup "structuredContent" value @?= Just arguments
        _ -> assertFailure "Missing hosted result"
    _ -> pure ()
  _ -> pure ()
  where
    header (Object fields) | Just (String name) <- KeyMap.lookup "name" fields, Just (String value) <- KeyMap.lookup "value" fields = pure (fromString (Text.unpack name), Text.encodeUtf8 value)
    header _ = assertFailure "Invalid hosted header"

earlyMcpEvents :: Object -> [Value]
earlyMcpEvents params
  | not (KeyMap.member "mcpServers" params) = []
  | KeyMap.lookup "mcpOAuthCallbackUri" params == Just (String "fixture:malformed") = [object ["type" .= String "mcp_auth_required", "serverName" .= String "early"]]
  | otherwise =
      [ object ["type" .= String "mcp_auth_required", "serverName" .= String "early", "authUrl" .= String "https://auth.invalid/early", "state" .= String "early-state", "message" .= String "Startup authorization"],
        object ["type" .= String "mcp_status_changed", "servers" .= ([] :: [Value]), "summary" .= object ["total" .= (0 :: Int), "connected" .= (0 :: Int), "connecting" .= (0 :: Int), "failed" .= (0 :: Int)]],
        object ["type" .= String "mcp_auth_completed", "serverName" .= String "early", "outcome" .= String "failed", "message" .= String "Startup completion"]
      ]

-- The same scripted MCP exchange runs behind native JSONL and WebSocket peers.
handleMcpRequest :: Text -> Text -> Object -> (Object -> [Pair] -> IO ()) -> (Text -> Value -> IO ()) -> IO Object -> IO Bool
handleMcpRequest prefix session request respond notify receive =
  case KeyMap.lookup "method" request of
    Just (String full)
      | Just method <- Text.stripPrefix prefix full,
        method `elem` methods -> do
          params <- parameters request
          let name = KeyMap.lookup "serverName" params
              result target success = respond target ["result" .= object ["success" .= success, "observedParams" .= params]]
          case method of
            "list_mcp_servers" -> do
              notify "foreign-session" (object ["type" .= String "mcp_status_changed", "servers" .= False])
              notify session (Object (KeyMap.insert "type" (String "mcp_status_changed") servers))
              respond request ["result" .= servers]
            "list_mcp_tools" -> respond request ["result" .= object ["tools" .= [object ["serverName" .= String "fixture", "name" .= String "lookup", "isEnabled" .= False, "isReadOnly" .= True]], "fixture" .= True]]
            "list_mcp_registry" -> respond request ["result" .= object ["servers" .= [object ["name" .= String "fixture", "description" .= String "Offline registry", "type" .= String "http", "url" .= String "https://mcp.invalid"]]]]
            "authenticate_mcp_server"
              | name == Just (String "rpc-error") -> respond request ["error" .= object ["code" .= (-32602 :: Int), "message" .= String "MCP fixture rejection"]]
              | name == Just (String "invalid-event") -> do
                  notify session (object ["type" .= String "mcp_auth_completed", "serverName" .= String "fixture", "outcome" .= String "success"])
                  result request False
              | name == Just (String "held") -> do
                  required "held"
                  continuation <- receive
                  continuationParams <- parameters continuation
                  unless (KeyMap.lookup "id" continuation /= KeyMap.lookup "id" request) (assertFailure "MCP RPC IDs reused")
                  case KeyMap.lookup "method" continuation of
                    Just (String methodName) | methodName == prefix <> "cancel_mcp_auth" -> do
                      KeyMap.lookup "serverName" continuationParams @?= Just (String "held")
                      respond continuation ["result" .= object ["success" .= True]]
                      result request False
                      completed "held" "cancelled"
                    Just (String methodName) | methodName == prefix <> "interrupt_session" -> respond continuation ["result" .= object []]
                    _ -> assertFailure "MCP pending authentication blocked its cancellation"
              | otherwise -> do
                  when (name == Just (String "oauth")) (required "oauth")
                  result request (name == Just (String "oauth"))
            "submit_mcp_auth_code" -> do
              when (name == Just (String "oauth")) $ do
                KeyMap.lookup "state" params @?= Just (String "fixture-state")
                KeyMap.lookup "code" params @?= Just (String "fixture-code")
              result request False
              when (name == Just (String "oauth")) (completed "oauth" "failed")
            "submit_mcp_auth_error" -> do
              result request False
              when (name == Just (String "oauth")) (completed "oauth" "cancelled")
            "remove_mcp_server" -> do
              KeyMap.lookup "settingsLevel" params @?= Just (String "user")
              result request False
            "toggle_mcp_server" -> do
              KeyMap.lookup "settingsLevel" params @?= Just (String "user")
              result request False
            "toggle_mcp_tool" -> do
              when (KeyMap.member "settingsLevel" params) (assertFailure "Tool toggle acquired a server settings level")
              result request False
            _ -> result request False
          pure True
    _ -> pure False
  where
    parameters frame = case KeyMap.lookup "params" frame of
      Just (Object params) -> do
        if prefix == "daemon."
          then KeyMap.lookup "sessionId" params @?= Just (String session)
          else when (KeyMap.member "sessionId" params) (assertFailure "Local MCP request acquired daemon routing")
        pure params
      _ -> assertFailure "MCP parameters were not an object"
    required name = notify session (object ["type" .= String "mcp_auth_required", "serverName" .= (name :: Text), "authUrl" .= String "https://auth.invalid/authorize", "message" .= String "Offline authorization", "state" .= String "fixture-state"])
    completed name outcome = notify session (object ["type" .= String "mcp_auth_completed", "serverName" .= (name :: Text), "outcome" .= (outcome :: Text), "message" .= String "Offline terminal report"])
    servers = KeyMap.fromList ["servers" .= [object ["name" .= String "fixture", "status" .= String "disconnected", "source" .= String "user", "isManaged" .= False, "serverType" .= String "http", "hasAuthTokens" .= False]], "summary" .= object ["total" .= (1 :: Int), "connected" .= (0 :: Int), "connecting" .= (0 :: Int), "failed" .= (0 :: Int), "disabled" .= (0 :: Int)], "fixture" .= True]
    methods = ["add_mcp_server", "list_mcp_servers", "list_mcp_tools", "list_mcp_registry", "remove_mcp_server", "toggle_mcp_server", "toggle_mcp_tool", "authenticate_mcp_server", "cancel_mcp_auth", "clear_mcp_auth", "submit_mcp_auth_code", "submit_mcp_auth_error"]
