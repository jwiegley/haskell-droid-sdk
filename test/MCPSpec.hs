{-# LANGUAGE OverloadedStrings #-}

module MCPSpec (mcpTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Factory.Droid.Schema.Enums (SettingsLevel (..))
import Factory.Droid.Schema.MCP
import SchemaTest (enumSchemaTest, redactedRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

mcpTests :: Value -> TestTree
mcpTests schema =
  testGroup
    "MCP wire bodies"
    [ records "McpHttpServerConfigFieldsSchema" httpFields (McpHttpServerConfigFields Nothing mempty) httpFieldsJSON mempty (\extras value -> value {mcpHttpFieldsAdditionalFields = extras}),
      records "McpStdioServerConfigFieldsSchema" stdioFields (McpStdioServerConfigFields Nothing Nothing mempty) stdioFieldsJSON mempty (\extras value -> value {mcpStdioFieldsAdditionalFields = extras}),
      records "StdioMcpSchema" stdio (StdioMcp "server" "not-executed" Nothing Nothing mempty) stdioJSON (KeyMap.fromList ["name" .= String "server", "command" .= String "not-executed"]) (\extras value -> value {stdioMcpAdditionalFields = extras}),
      records "HttpHeaderSchema" header header headerJSON headerJSON (\extras value -> value {httpHeaderAdditionalFields = extras}),
      records "McpRegistryServerSchema" registry minimalRegistry registryJSON minimalRegistryJSON (\extras value -> value {registryServerAdditionalFields = extras}),
      records "ListMcpRegistryResultSchema" registryList registryList registryListJSON registryListJSON (\extras value -> value {mcpRegistryAdditionalFields = extras}),
      redactedRecordTests "McpToolInputSchema" (schemaAt ["definitions", "McpToolInfoSchema", "properties", "inputSchema"] schema) inputSchema (McpToolInputSchema Nothing Nothing Nothing mempty) inputSchemaJSON mempty (\extras value -> value {mcpInputAdditionalFields = extras}),
      records "McpToolInfoSchema" tool minimalTool toolJSON minimalToolJSON (\extras value -> value {mcpToolAdditionalFields = extras}),
      records "ListMcpToolsResultSchema" tools tools toolsJSON toolsJSON (\extras value -> value {listedMcpToolsAdditionalFields = extras}),
      redactedRecordTests "McpConfigError" (schemaAt ["definitions", "McpStatusSummarySchema", "properties", "configError"] schema) configError configError configErrorJSON configErrorJSON (\extras value -> value {mcpConfigErrorAdditionalFields = extras}),
      records "McpStatusSummarySchema" summary (McpStatusSummary 10.5 1.25 2.5 3.75 Nothing Nothing mempty) summaryJSON (KeyMap.delete "disabled" (KeyMap.delete "configError" summaryJSON)) (\extras value -> value {mcpSummaryAdditionalFields = extras}),
      records "McpServerStatusInfoSchema" status minimalStatus statusJSON minimalStatusJSON (\extras value -> value {mcpStatusAdditionalFields = extras}),
      records "ListMcpServersResultSchema" servers servers serversJSON serversJSON (\extras value -> value {listedMcpServersAdditionalFields = extras}),
      records "McpStatusChangedNotificationSchema" (McpStatusChanged servers) (McpStatusChanged servers) changedJSON changedJSON (\extras (McpStatusChanged value) -> McpStatusChanged (value {listedMcpServersAdditionalFields = extras})),
      records "McpAuthRequiredNotificationSchema" authRequired authRequired authRequiredJSON authRequiredJSON (\extras value -> value {mcpAuthRequiredAdditionalFields = extras}),
      records "McpAuthCompletedNotificationSchema" authCompleted authCompleted authCompletedJSON authCompletedJSON (\extras value -> value {mcpAuthCompletedAdditionalFields = extras}),
      records "McpServerNameParamsSchema" nameParams nameParams nameParamsJSON nameParamsJSON (\extras value -> value {mcpNameParamsAdditionalFields = extras}),
      records "RemoveMcpServerRequestParamsSchema" removal removal removalJSON removalJSON (\extras value -> value {removeMcpAdditionalFields = extras}),
      records "ToggleMcpServerRequestParamsSchema" toggleServer toggleServer toggleServerJSON toggleServerJSON (\extras value -> value {toggleMcpServerAdditionalFields = extras}),
      records "ToggleMcpToolRequestParamsSchema" toggleTool toggleTool toggleToolJSON toggleToolJSON (\extras value -> value {toggleMcpToolAdditionalFields = extras}),
      records "SubmitMcpAuthCodeRequestParamsSchema" authCode authCode authCodeJSON authCodeJSON (\extras value -> value {submittedMcpCodeAdditionalFields = extras}),
      records "SubmitMcpAuthErrorRequestParamsSchema" authError (authError {submittedMcpErrorDescription = Nothing}) authErrorJSON (KeyMap.delete "errorDescription" authErrorJSON) (\extras value -> value {submittedMcpErrorAdditionalFields = extras}),
      enumSchemaTest "server types" (definition "McpServerTypeSchema" >>= schemaAt ["enum"]) (Proxy @McpServerType),
      enumSchemaTest "server statuses" (definition "McpServerStatusSchema" >>= schemaAt ["enum"]) (Proxy @McpServerStatus),
      enumSchemaTest "auth outcomes" (definition "McpAuthCompletedNotificationSchema" >>= schemaAt ["properties", "outcome", "enum"]) (Proxy @McpAuthOutcome),
      testCase "server-name schema is exactly the unconstrained string domain" $ do
        definition "McpServerNameSchema" @?= Right (object ["type" .= String "string"])
        forM_ ["", "  ", "server/name"] $ \name -> fromJSON (String name) @?= Success (name :: McpServerName)
        forM_ [Null, Bool False, Number 1, Array mempty, Object mempty] $ rejects (Proxy @McpServerName),
      testCase "mutation settings are the required user literal" $ do
        expected <- either assertFailure pure (definition "McpSettingsLevelSchema" >>= schemaAt ["const"])
        toJSON McpUserSettings @?= expected
        fromJSON expected @?= Success McpUserSettings
        forM_ [String "project", String "org", String "folder", Null, Bool False, Array mempty, Object mempty] $ \bad -> do
          rejects (Proxy @McpSettingsLevel) bad
          rejects (Proxy @RemoveMcpServerParams) (Object (KeyMap.insert "settingsLevel" bad removalJSON))
          rejects (Proxy @ToggleMcpServerParams) (Object (KeyMap.insert "settingsLevel" bad toggleServerJSON)),
      testCase "reported settings source remains the broader shared enum" $ do
        forM_ [minBound .. maxBound] $ \source -> fromJSON (Object (KeyMap.insert "source" (toJSON source) statusJSON)) @?= Success (status {mcpStatusSource = source})
        (definition "McpServerStatusInfoSchema" >>= schemaAt ["properties", "serverType", "enum"]) @?= (definition "McpServerTypeSchema" >>= schemaAt ["enum"]),
      testCase "status notification differs from listing only by its type literal" $ do
        listing <- either assertFailure pure (definition "ListMcpServersResultSchema")
        event <- either assertFailure pure (definition "McpStatusChangedNotificationSchema")
        props <- either assertFailure pure (schemaAt ["properties"] event)
        required <- either assertFailure pure (schemaAt ["required"] event)
        case (event, props, required) of
          (Object fields, Object properties, Array keys) ->
            Object (KeyMap.insert "properties" (Object (KeyMap.delete "type" properties)) (KeyMap.insert "required" (toJSON (filter (/= String "type") (foldr (:) [] keys))) fields)) @?= listing
          _ -> assertFailure "Expected notification record schema",
      testCase "listing extensions cannot override the notification discriminator" $ do
        let listing = servers {listedMcpServersAdditionalFields = KeyMap.singleton "type" (String "injected")}
        toJSON listing @?= Object (KeyMap.insert "type" (String "injected") serversJSON)
        toJSON (McpStatusChanged listing) @?= Object changedJSON
        fromJSON (Object changedJSON) @?= Success (McpStatusChanged servers),
      testCase "stdio defaults remain annotations and unknown type stays an extension" $ do
        (definition "StdioMcpSchema" >>= schemaAt ["properties", "args", "default"]) @?= Right (Array mempty)
        (definition "StdioMcpSchema" >>= schemaAt ["properties", "env", "default"]) @?= Right (Object mempty)
        let value = StdioMcp "" "  " (Just []) (Just mempty) (KeyMap.singleton "type" Null)
            wire = object ["name" .= String "", "command" .= String "  ", "args" .= ([] :: [Value]), "env" .= object [], "type" .= Null]
        fromJSON wire @?= Success value
        toJSON value @?= wire,
      testCase "stdio arguments and environment remain typed at their boundary" $ do
        rejects (Proxy @StdioMcp) (Object (KeyMap.insert "args" (toJSON [Number 1]) stdioJSON))
        rejects (Proxy @StdioMcp) (Object (KeyMap.insert "env" (object ["fixture" .= Null]) stdioJSON))
        rejects (Proxy @McpStdioServerConfigFields) (object ["args" .= [Bool False]]),
      testCase "registry transport does not invent command or URL requirements" $ do
        forM_ [minBound .. maxBound] $ \kind -> do
          let value = minimalRegistry {registryServerType = kind}
          fromJSON (Object (KeyMap.insert "type" (toJSON kind) minimalRegistryJSON)) @?= Success value,
      testCase "input-schema values are dynamic but declared containers are typed" $ do
        let value = McpToolInputSchema (Just "arbitrary") (Just (KeyMap.fromList ["null" .= Null, "scalar" .= Number 1, "array" .= [Bool False]])) (Just ["absent", "absent"]) mempty
        fromJSON (toJSON value) @?= Success value
        forM_ [Array mempty, Bool False, String "properties"] $ \bad -> rejects (Proxy @McpToolInputSchema) (object ["properties" .= bad])
        rejects (Proxy @McpToolInputSchema) (object ["required" .= [Number 1]])
        rejects (Proxy @McpToolInfo) (Object (KeyMap.insert "inputSchema" (Bool True) toolJSON)),
      testCase "empty catalogs and fractional signed counts remain valid" $ do
        fromJSON (object ["servers" .= ([] :: [Value])]) @?= Success (ListMcpRegistryResult [] mempty)
        fromJSON (object ["tools" .= ([] :: [Value])]) @?= Success (ListMcpToolsResult [] mempty)
        fromJSON (Object (KeyMap.insert "servers" (Array mempty) serversJSON)) @?= Success (servers {listedMcpServers = []})
        forM_ [-1.25, 0, 123456789012345678901234567890] $ \number -> fromJSON (Object (KeyMap.insert "toolCount" (Number number) statusJSON)) @?= Success (status {mcpStatusToolCount = Just number}),
      testCase "nested status, summary and catalog objects are validated" $ do
        forM_ [Null, object [], String "entry"] $ \bad -> do
          rejects (Proxy @ListMcpRegistryResult) (object ["servers" .= [bad]])
          rejects (Proxy @ListMcpToolsResult) (object ["tools" .= [bad]])
          rejects (Proxy @ListMcpServersResult) (Object (KeyMap.insert "servers" (toJSON [bad]) serversJSON))
          rejects (Proxy @McpStatusSummary) (Object (KeyMap.insert "configError" bad summaryJSON))
          rejects (Proxy @McpStatusChanged) (Object (KeyMap.insert "summary" bad changedJSON)),
      testCase "status and permission flags remain independent reports" $ do
        let value = status {mcpStatus = McpConnected, mcpStatusBlockedByPolicy = Just True, mcpStatusHasAuthTokens = Just True}
        fromJSON (toJSON value) @?= Success value
        forM_ ["status", "source", "serverType"] $ \key -> rejects (Proxy @McpServerStatusInfo) (Object (KeyMap.insert key (String "future") statusJSON)),
      testCase "header wire text is not confused with HTTP validation" $ do
        let value = HttpHeader " " "fixture\r\ntext" mempty
        fromJSON (object ["name" .= String " ", "value" .= String "fixture\r\ntext"]) @?= Success value
        toJSON value @?= object ["name" .= String " ", "value" .= String "fixture\r\ntext"]
    ]
  where
    definition name = schemaAt ["definitions", name] schema
    records :: (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = redactedRecordTests name (definition name)

httpFields :: McpHttpServerConfigFields
httpFields = McpHttpServerConfigFields (Just "unparsed URL") mempty

stdioFields :: McpStdioServerConfigFields
stdioFields = McpStdioServerConfigFields (Just "not-executed") (Just ["argument"]) mempty

stdio :: StdioMcp
stdio = StdioMcp "server" "not-executed" (Just ["argument"]) (Just (KeyMap.singleton "FIXTURE" "value")) mempty

header :: HttpHeader
header = HttpHeader "X-Fixture" "value" mempty

registry, minimalRegistry :: McpRegistryServer
minimalRegistry = McpRegistryServer "server" "description" McpHttp Nothing Nothing Nothing Nothing Nothing mempty
registry = minimalRegistry {registryServerUrl = Just "unparsed URL", registryServerCommand = Just "not-executed", registryServerArgs = Just ["argument"], registryServerNote = Just "note", registryServerLogoUrl = Just "unparsed logo"}

registryList :: ListMcpRegistryResult
registryList = ListMcpRegistryResult [registry] mempty

inputSchema :: McpToolInputSchema
inputSchema = McpToolInputSchema (Just "object") (Just (KeyMap.singleton "field" (object ["type" .= String "string"]))) (Just ["field"]) mempty

tool, minimalTool :: McpToolInfo
minimalTool = McpToolInfo "server" "tool" False Nothing Nothing Nothing mempty
tool = minimalTool {mcpToolDescription = Just "description", mcpToolReadOnly = Just False, mcpToolInputSchema = Just inputSchema}

tools :: ListMcpToolsResult
tools = ListMcpToolsResult [tool] mempty

configError :: McpConfigError
configError = McpConfigError "not-read" "fixture diagnostic" mempty

summary :: McpStatusSummary
summary = McpStatusSummary 10.5 1.25 2.5 3.75 (Just (-0.5)) (Just configError) mempty

status, minimalStatus :: McpServerStatusInfo
minimalStatus = McpServerStatusInfo "server" McpConnecting SettingsProject False McpSse Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty
status = minimalStatus {mcpStatusError = Just "fixture error", mcpStatusToolCount = Just (-1.25), mcpStatusHasAuthTokens = Just False, mcpStatusRequiresAuth = Just True, mcpStatusPendingAuthUrl = Just "unparsed callback", mcpStatusPendingAuthMessage = Just "fixture message", mcpStatusPendingAuthState = Just "fixture state", mcpStatusBlockedByPolicy = Just False}

servers :: ListMcpServersResult
servers = ListMcpServersResult [status] summary mempty

authRequired :: McpAuthRequired
authRequired = McpAuthRequired "server" "unparsed callback" "fixture message" "fixture state" mempty

authCompleted :: McpAuthCompleted
authCompleted = McpAuthCompleted "server" McpAuthCancelled "fixture message" mempty

nameParams :: McpServerNameParams
nameParams = McpServerNameParams "server" mempty

removal :: RemoveMcpServerParams
removal = RemoveMcpServerParams "server" mempty

toggleServer :: ToggleMcpServerParams
toggleServer = ToggleMcpServerParams "server" False mempty

toggleTool :: ToggleMcpToolParams
toggleTool = ToggleMcpToolParams "server" "tool" False mempty

authCode :: SubmitMcpAuthCodeParams
authCode = SubmitMcpAuthCodeParams "server" "fixture code" "fixture state" mempty

authError :: SubmitMcpAuthErrorParams
authError = SubmitMcpAuthErrorParams "server" "fixture error" "fixture state" (Just "fixture description") mempty

httpFieldsJSON, stdioFieldsJSON, stdioJSON, headerJSON, registryJSON, minimalRegistryJSON, registryListJSON, inputSchemaJSON, toolJSON, minimalToolJSON, toolsJSON, configErrorJSON, summaryJSON, statusJSON, minimalStatusJSON, serversJSON, changedJSON, authRequiredJSON, authCompletedJSON, nameParamsJSON, removalJSON, toggleServerJSON, toggleToolJSON, authCodeJSON, authErrorJSON :: Object
httpFieldsJSON = KeyMap.singleton "url" (String "unparsed URL")
stdioFieldsJSON = KeyMap.fromList ["command" .= String "not-executed", "args" .= [String "argument"]]
stdioJSON = KeyMap.fromList ["name" .= String "server", "command" .= String "not-executed", "args" .= [String "argument"], "env" .= object ["FIXTURE" .= String "value"]]
headerJSON = KeyMap.fromList ["name" .= String "X-Fixture", "value" .= String "value"]
minimalRegistryJSON = KeyMap.fromList ["name" .= String "server", "description" .= String "description", "type" .= String "http"]
registryJSON = KeyMap.union minimalRegistryJSON (KeyMap.fromList ["url" .= String "unparsed URL", "command" .= String "not-executed", "args" .= [String "argument"], "note" .= String "note", "logoUrl" .= String "unparsed logo"])
registryListJSON = KeyMap.singleton "servers" (toJSON [Object registryJSON])
inputSchemaJSON = KeyMap.fromList ["type" .= String "object", "properties" .= object ["field" .= object ["type" .= String "string"]], "required" .= [String "field"]]
minimalToolJSON = KeyMap.fromList ["serverName" .= String "server", "name" .= String "tool", "isEnabled" .= False]
toolJSON = KeyMap.union minimalToolJSON (KeyMap.fromList ["description" .= String "description", "isReadOnly" .= False, "inputSchema" .= Object inputSchemaJSON])
toolsJSON = KeyMap.singleton "tools" (toJSON [Object toolJSON])
configErrorJSON = KeyMap.fromList ["path" .= String "not-read", "message" .= String "fixture diagnostic"]
summaryJSON = KeyMap.fromList ["total" .= Number 10.5, "connected" .= Number 1.25, "connecting" .= Number 2.5, "failed" .= Number 3.75, "disabled" .= Number (-0.5), "configError" .= Object configErrorJSON]
minimalStatusJSON = KeyMap.fromList ["name" .= String "server", "status" .= String "connecting", "source" .= String "project", "isManaged" .= False, "serverType" .= String "sse"]
statusJSON = KeyMap.union minimalStatusJSON (KeyMap.fromList ["error" .= String "fixture error", "toolCount" .= Number (-1.25), "hasAuthTokens" .= False, "requiresAuth" .= True, "pendingAuthUrl" .= String "unparsed callback", "pendingAuthMessage" .= String "fixture message", "pendingAuthState" .= String "fixture state", "blockedByPolicy" .= False])
serversJSON = KeyMap.fromList ["servers" .= [Object statusJSON], "summary" .= Object summaryJSON]
changedJSON = KeyMap.insert "type" (String "mcp_status_changed") serversJSON
authRequiredJSON = KeyMap.fromList ["type" .= String "mcp_auth_required", "serverName" .= String "server", "authUrl" .= String "unparsed callback", "message" .= String "fixture message", "state" .= String "fixture state"]
authCompletedJSON = KeyMap.fromList ["type" .= String "mcp_auth_completed", "serverName" .= String "server", "outcome" .= String "cancelled", "message" .= String "fixture message"]
nameParamsJSON = KeyMap.singleton "serverName" (String "server")
removalJSON = KeyMap.fromList ["serverName" .= String "server", "settingsLevel" .= String "user"]
toggleServerJSON = KeyMap.fromList ["serverName" .= String "server", "enabled" .= False, "settingsLevel" .= String "user"]
toggleToolJSON = KeyMap.fromList ["serverName" .= String "server", "toolName" .= String "tool", "enabled" .= False]
authCodeJSON = KeyMap.fromList ["serverName" .= String "server", "code" .= String "fixture code", "state" .= String "fixture state"]
authErrorJSON = KeyMap.fromList ["serverName" .= String "server", "error" .= String "fixture error", "state" .= String "fixture state", "errorDescription" .= String "fixture description"]
