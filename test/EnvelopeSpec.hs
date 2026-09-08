{-# LANGUAGE OverloadedStrings #-}

module EnvelopeSpec (envelopeTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.Metadata (TraceContextMeta (..))
import Factory.Droid.Schema.RPC
import SchemaTest (nonNullableRecordTests, rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- These are structural schema checks and wire goldens, not a Draft-07 validator.
envelopeTests :: Value -> TestTree
envelopeTests schema =
  testGroup
    "Complete RPC envelopes"
    [ nonNullableRecordTests "JsonRpcEnvelopeSchema" (definition "JsonRpcEnvelopeSchema") fullEnvelope minimalEnvelope fullHeader minimalHeader (\extras value -> value {envelopeBody = extras}),
      wrapped "request" request requestJSON,
      wrapped "notification" notification notificationJSON,
      wrapped "raw string-ID response" success successJSON,
      wrapped "failure" failure failureJSON,
      wrapped "generic nullable-ID response" generic genericJSON,
      wrapped "message request" (RequestBody request) requestJSON,
      wrapped "message response" (ResponseBody generic) genericJSON,
      wrapped "message notification" (NotificationBody notification) notificationJSON,
      wrapped "base-response failure" (BaseFailure failure) failureJSON,
      wrapped "base-response success" (BaseSuccess (success {successResponseError = Nothing})) (KeyMap.delete "error" successJSON),
      wrapped "command acknowledgement" (ResponseResult (ResultResponse "id" (CommandAck mempty) Nothing mempty)) ackJSON,
      wrapped "open-object result" (ResponseResult (ResultResponse "id" (KeyMap.singleton "future" Null) Nothing mempty)) (resultJSON (object ["future" .= Null])),
      wrapped "Boolean-success result" (ResponseResult (ResultResponse "id" (SuccessResult False mempty) Nothing mempty)) (resultJSON (object ["success" .= False])),
      wrapped "Boolean-success/error result" (ResponseResult (ResultResponse "id" (SuccessOrErrorResult False (Just "fixture") mempty) Nothing mempty)) (resultJSON (object ["success" .= False, "error" .= String "fixture"])),
      wrapped "typed-response failure" (ResponseFailure failure :: RpcResponse CommandAck) failureJSON,
      testCase "complete base schemas reuse precisely the existing bare shapes" $
        forM_ [("JsonRpcBaseRequestSchema", "BaseRequestSchema"), ("JsonRpcBaseNotificationSchema", "BaseNotificationSchema"), ("JsonRpcBaseResponseSuccessSchema", "BaseResponseSuccessSchema"), ("JsonRpcBaseResponseFailureSchema", "BaseResponseFailureSchema")] $ \(name, base) -> do
          body <- either assertFailure pure (definition base)
          definition name @?= Right (object ["allOf" .= [object ["$ref" .= String "#/definitions/JsonRpcEnvelopeSchema"], body]]),
      testCase "typed response aliases match the supplied result/failure intersections" $
        forM_ [("CommandAckResponseSchema", "#/definitions/CommandAckSchema"), ("EmptyObjectResponseSchema", "#/definitions/EmptyObjectSchema"), ("SuccessResultResponseSchema", "#/definitions/SuccessResultSchema"), ("SuccessOrErrorResultResponseSchema", "#/definitions/SuccessOrErrorResultSchema")] $ \(name, resultRef) -> do
          let body = object ["type" .= String "object", "additionalProperties" .= True, "properties" .= object ["type" .= object ["type" .= String "string", "const" .= String "response"], "id" .= object ["type" .= String "string"], "error" .= object ["$ref" .= String "#/definitions/JsonRpcErrorSchema"], "result" .= object ["$ref" .= (resultRef :: String)]], "required" .= [String "type", String "id", String "result"]]
          definition name @?= Right (object ["anyOf" .= [object ["allOf" .= [object ["$ref" .= String "#/definitions/JsonRpcEnvelopeSchema"], body]], object ["$ref" .= String "#/definitions/JsonRpcBaseResponseFailureSchema"]]]),
      testCase "generic message and base-response domains remain distinct" $ do
        let frame = Object (minimalHeader <> KeyMap.fromList ["type" .= String "response", "id" .= Null])
            response = BaseResponseGeneric Nothing Nothing Nothing mempty
        fromJSON frame @?= Success (WithEnvelope Nothing Nothing (ResponseBody response))
        rejects (Proxy @JsonRpcBaseResponse) frame
        rejects (Proxy @JsonRpcBaseResponseSuccess) frame
        rejects (Proxy @JsonRpcBaseResponseFailure) frame
        let bare = object ["type" .= String "object", "additionalProperties" .= True, "properties" .= object ["type" .= object ["type" .= String "string", "const" .= String "response"], "id" .= object ["type" .= [String "string", String "null"]], "error" .= object ["$ref" .= String "#/definitions/JsonRpcErrorSchema"], "result" .= object []], "required" .= [String "type", String "id"]]
        (definition "JsonRpcMessageSchema" >>= schemaAt ["anyOf"] >>= schemaIndex 1) @?= Right (object ["allOf" .= [object ["$ref" .= String "#/definitions/JsonRpcEnvelopeSchema"], bare]]),
      testCase "generic response requires ID but not result or error" $ do
        forM_ [Nothing, Just ""] $ \identifier ->
          forM_ [Nothing, Just Null, Just (Bool False)] $ \result -> do
            let response = WithEnvelope Nothing Nothing (ResponseBody (BaseResponseGeneric identifier result Nothing mempty))
            eitherDecode (encode response) @?= Right response
        rejects (Proxy @JsonRpcMessage) (Object (KeyMap.delete "id" (minimalHeader <> genericJSON)))
        rejects (Proxy @JsonRpcMessage) (Object (KeyMap.insert "error" Null (minimalHeader <> genericJSON))),
      testCase "base response union follows full failure-first parsers" $ do
        (definition "JsonRpcBaseResponseSchema" >>= schemaAt ["anyOf"]) @?= Right (toJSON [object ["$ref" .= String "#/definitions/JsonRpcBaseResponseFailureSchema"], object ["$ref" .= String "#/definitions/JsonRpcBaseResponseSuccessSchema"]])
        let frame = Object (minimalHeader <> successJSON)
            expected = WithEnvelope Nothing Nothing (BaseFailure (BaseResponseFailure (Just "id") rpcError (Just Null) mempty))
        fromJSON frame @?= Success expected
        toJSON expected @?= frame,
      testCase "typed success/error overlap follows full result-first parsers" $ do
        let fields = KeyMap.insert "error" (toJSON rpcError) ackJSON
            expected = WithEnvelope Nothing Nothing (ResponseResult (ResultResponse "id" (CommandAck mempty) (Just rpcError) mempty))
        fromJSON (Object (minimalHeader <> fields)) @?= Success expected
        toJSON expected @?= Object (minimalHeader <> fields),
      testCase "invalid typed results remain valid raw failure data when error is valid" $
        forM_ [Null, Bool False, object ["accepted" .= False], String "not an acknowledgement"] $ \result -> do
          let fields = KeyMap.insert "error" (toJSON rpcError) (resultJSON result)
              expected = WithEnvelope Nothing Nothing (ResponseFailure (BaseResponseFailure (Just "id") rpcError (Just result) mempty) :: RpcResponse CommandAck)
          fromJSON (Object (minimalHeader <> fields)) @?= Success expected
          toJSON expected @?= Object (minimalHeader <> fields)
          rejects (Proxy @CommandAckResponse) (Object (minimalHeader <> resultJSON result)),
      testCase "typed result is required, but a failure may omit it" $ do
        rejects (Proxy @CommandAckResponse) (Object (minimalHeader <> KeyMap.delete "result" ackJSON))
        let response = WithEnvelope Nothing Nothing (ResponseFailure (BaseResponseFailure Nothing rpcError Nothing mempty) :: RpcResponse CommandAck)
        eitherDecode (encode response) @?= Right response
        rejects (Proxy @CommandAckResponse) (Object (KeyMap.insert "error" Null (minimalHeader <> ackJSON))),
      testCase "message union dispatch is strict despite open extension fields" $ do
        forM_ [Null, String "future", Number 1, Bool False] $ \value ->
          rejects (Proxy @JsonRpcMessage) (Object (KeyMap.insert "type" value (minimalHeader <> requestJSON)))
        rejects (Proxy @JsonRpcMessage) (Object (KeyMap.delete "type" (minimalHeader <> requestJSON)))
        let fields = KeyMap.insert "id" (Number 42) notificationJSON
        fromJSON (Object (minimalHeader <> fields)) @?= Success (WithEnvelope Nothing Nothing (NotificationBody (notification {baseNotificationAdditionalFields = KeyMap.singleton "id" (Number 42)}))),
      testCase "metadata absence and empty object are distinct, neither inferred" $ do
        let present = minimalEnvelope {envelopeMeta = Just emptyMeta}
        fromJSON (Object minimalHeader) @?= Success minimalEnvelope
        fromJSON (Object (KeyMap.insert "_meta" (object []) minimalHeader)) @?= Success present
        toJSON present @?= Object (KeyMap.insert "_meta" (object []) minimalHeader),
      testCase "version signal is optional and does not impose compatibility policy" $
        forM_ [Nothing, Just "", Just "1.192.0", Just "future-version"] $ \version -> do
          let value = minimalEnvelope {envelopeProtocolVersion = version}
          eitherDecode (encode value) @?= Right value,
      testCase "nested body and metadata extensions are retained separately" $ do
        let meta = emptyMeta {traceAdditionalFields = KeyMap.singleton "future" (String "metadata")}
            value = WithEnvelope Nothing (Just meta) (request {baseRequestAdditionalFields = KeyMap.singleton "future" (String "body")})
        toJSON value @?= Object (minimalHeader <> requestJSON <> KeyMap.fromList ["_meta" .= object ["future" .= String "metadata"], "future" .= String "body"])
        eitherDecode (encode value) @?= Right value,
      testCase "Show redacts metadata, extensions and body" $
        show (WithEnvelope (Just "private-version") (Just (emptyMeta {traceState = Just "private-trace"})) (request {baseRequestParams = Just (String "private-payload")})) @?= "WithEnvelope <redacted>"
    ]
  where
    definition name = schemaAt ["definitions", name] schema

wrapped :: forall a. (Eq a, Show a, FromJSON a, RpcObject a) => String -> a -> Object -> TestTree
wrapped label body fields =
  testGroup
    label
    [ testCase "flat full and minimal envelopes round-trip" $
        forM_ [(Just "1.205.0", Just emptyMeta, fullHeader), (Nothing, Nothing, minimalHeader)] $ \(version, meta, header) -> do
          let value = WithEnvelope version meta body
          toJSON value @?= Object (header <> fields)
          toRpcObject value @?= header <> fields
          fromJSON (Object (header <> fields)) @?= Success value
          eitherDecode (encode value) @?= Right value,
      testCase "envelope literals remain required and exact" $
        forM_ ["jsonrpc", "factoryApiVersion"] $ \key -> do
          rejects (Proxy @(WithEnvelope a)) (Object (KeyMap.delete key (fullHeader <> fields)))
          forM_ [Null, String "different", Number 2] $ \value ->
            rejects (Proxy @(WithEnvelope a)) (Object (KeyMap.insert key value (fullHeader <> fields))),
      testCase "optional envelope fields reject explicit null" $
        forM_ ["factoryProtocolVersion", "_meta"] $ \key ->
          rejects (Proxy @(WithEnvelope a)) (Object (KeyMap.insert key Null (fullHeader <> fields))),
      testCase "envelope keys never reach decoded body extensions" $
        case fromJSON (Object (fullHeader <> fields)) of
          Error err -> assertFailure err
          Success value -> envelopeBody value @?= body
    ]

emptyMeta :: TraceContextMeta
emptyMeta = TraceContextMeta Nothing Nothing Nothing mempty

fullEnvelope, minimalEnvelope :: JsonRpcEnvelope
fullEnvelope = WithEnvelope (Just "1.205.0") (Just emptyMeta) mempty
minimalEnvelope = WithEnvelope Nothing Nothing mempty

fullHeader, minimalHeader :: Object
minimalHeader = KeyMap.fromList ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0"]
fullHeader = minimalHeader <> KeyMap.fromList ["factoryProtocolVersion" .= String "1.205.0", "_meta" .= object []]

rpcError :: JsonRpcError
rpcError = JsonRpcError RpcConflict "fixture" (Just Null) mempty

request :: BaseRequest
request = BaseRequest "id" "echo" (Just Null) mempty

notification :: BaseNotification
notification = BaseNotification "notice" (Just (object [])) mempty

success :: BaseResponseSuccess
success = BaseResponseSuccess "id" (Just Null) (Just rpcError) mempty

failure :: BaseResponseFailure
failure = BaseResponseFailure Nothing rpcError (Just Null) mempty

generic :: BaseResponseGeneric
generic = BaseResponseGeneric Nothing (Just Null) (Just rpcError) mempty

requestJSON, notificationJSON, successJSON, failureJSON, genericJSON, ackJSON :: Object
requestJSON = KeyMap.fromList ["type" .= String "request", "id" .= String "id", "method" .= String "echo", "params" .= Null]
notificationJSON = KeyMap.fromList ["type" .= String "notification", "method" .= String "notice", "params" .= object []]
successJSON = KeyMap.fromList ["type" .= String "response", "id" .= String "id", "result" .= Null, "error" .= rpcError]
failureJSON = KeyMap.insert "id" Null successJSON
genericJSON = failureJSON
ackJSON = resultJSON (object ["accepted" .= True])

resultJSON :: Value -> Object
resultJSON result = KeyMap.fromList ["type" .= String "response", "id" .= String "id", "result" .= result]
