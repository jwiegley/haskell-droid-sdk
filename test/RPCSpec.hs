{-# LANGUAGE OverloadedStrings #-}

module RPCSpec (rpcTests) where

import Control.Monad (foldM, forM_)
import Data.Aeson
  ( FromJSON,
    Object,
    Result (..),
    ToJSON,
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Factory.Droid.Schema.RPC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

rpcTests :: Value -> TestTree
rpcTests schema =
  testGroup
    "RPC base schemas"
    [ records "JsonRpcErrorSchema" rpcError (rpcError {rpcErrorData = Nothing}) errorJSON (KeyMap.delete "data" errorJSON) (\extra value -> value {rpcErrorAdditionalFields = extra}),
      records "JsonRpcProtocolVersionMismatchErrorDataSchema" mismatch minimalMismatch mismatchJSON minimalMismatchJSON (\extra value -> value {mismatchAdditionalFields = extra}),
      records "BaseRequestSchema" request (request {baseRequestParams = Nothing}) requestJSON (KeyMap.delete "params" requestJSON) (\extra value -> value {baseRequestAdditionalFields = extra}),
      records "BaseNotificationSchema" notification (notification {baseNotificationParams = Nothing}) notificationJSON (KeyMap.delete "params" notificationJSON) (\extra value -> value {baseNotificationAdditionalFields = extra}),
      records "BaseResponseSuccessSchema" success (BaseResponseSuccess "request-id" Nothing Nothing mempty) successJSON (remove ["result", "error"] successJSON) (\extra value -> value {successResponseAdditionalFields = extra}),
      records "BaseResponseFailureSchema" failure (failure {failureResponseResult = Nothing}) failureJSON (KeyMap.delete "result" failureJSON) (\extra value -> value {failureResponseAdditionalFields = extra}),
      testCase "all error codes match the schema numerically" $ do
        literals <- either assertFailure pure (schemaAt ["definitions", "JsonRpcErrorSchema", "properties", "code", "enum"] schema)
        let values = [minBound .. maxBound] :: [JsonRpcErrorCode]
        toJSON values @?= literals
        forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
        forM_ [Null, String "-32700", Number (-32600.5), Number (-32000), Bool False, Object mempty, Array mempty] $ rejects (Proxy @JsonRpcErrorCode),
      testCase "message kinds match the diagnostic enum" $ do
        literals <- either assertFailure pure (schemaAt ["definitions", "JsonRpcProtocolVersionMismatchErrorDataSchema", "properties", "messageType", "enum"] schema)
        let values = [minBound .. maxBound] :: [JsonRpcMessageType]
        toJSON values @?= literals
        forM_ values $ \value -> fromJSON (toJSON value) @?= Success value
        rejects (Proxy @JsonRpcMessageType) (String "future"),
      testCase "error data and base payloads retain every JSON kind" $
        forM_ [Null, Bool False, Number 1.5, String "value", toJSON [Null, Bool True], object ["nested" .= Null]] $ \value -> do
          let err = rpcError {rpcErrorData = Just value}
              req = request {baseRequestParams = Just value}
              note = notification {baseNotificationParams = Just value}
              ok = success {successResponseResult = Just value}
              bad = failure {failureResponseResult = Just value}
          fromJSON (Object (KeyMap.insert "data" value errorJSON)) @?= Success err
          fromJSON (Object (KeyMap.insert "params" value requestJSON)) @?= Success req
          fromJSON (Object (KeyMap.insert "params" value notificationJSON)) @?= Success note
          fromJSON (Object (KeyMap.insert "result" value successJSON)) @?= Success ok
          fromJSON (Object (KeyMap.insert "result" value failureJSON)) @?= Success bad
          eitherDecode (encode err) @?= Right err
          eitherDecode (encode req) @?= Right req
          eitherDecode (encode note) @?= Right note
          eitherDecode (encode ok) @?= Right ok
          eitherDecode (encode bad) @?= Right bad,
      testCase "failure ID is required but nullable" $ do
        let noId = Object (KeyMap.delete "id" failureJSON)
            stringId = Object (KeyMap.insert "id" (String "request-id") failureJSON)
        rejects (Proxy @BaseResponseFailure) noId
        fromJSON stringId @?= Success (failure {failureResponseId = Just "request-id"})
        fromJSON (Object failureJSON) @?= Success failure
        toJSON failure @?= Object failureJSON,
      testCase "success shape permits both result and error or neither" $ do
        fromJSON (Object successJSON) @?= Success success
        fromJSON (object ["type" .= String "response", "id" .= String "request-id"]) @?= Success (BaseResponseSuccess "request-id" Nothing Nothing mempty)
        rejects (Proxy @BaseResponseSuccess) (Object (KeyMap.insert "id" Null successJSON))
        rejects (Proxy @BaseResponseSuccess) (Object (KeyMap.insert "error" Null successJSON))
        rejects (Proxy @BaseResponseFailure) (Object (KeyMap.insert "error" Null failureJSON)),
      testCase "mismatch request IDs preserve missing, null and string" $
        forM_ [(Nothing, Nothing), (Just Nothing, Just Null), (Just (Just "request-id"), Just (String "request-id"))] $ \(identifier, field) -> do
          let value = minimalMismatch {mismatchRequestId = identifier}
              fields = maybe minimalMismatchJSON (\item -> KeyMap.insert "requestId" item minimalMismatchJSON) field
          fromJSON (Object fields) @?= Success value
          toJSON value @?= Object fields
          eitherDecode (encode value) @?= Right value,
      testCase "base discriminants and typed identifiers reject invalid input" $ do
        forM_ [Null, Bool True, Number 1, String "wrong"] $ \tag -> do
          rejects (Proxy @BaseRequest) (Object (KeyMap.insert "type" tag requestJSON))
          rejects (Proxy @BaseNotification) (Object (KeyMap.insert "type" tag notificationJSON))
          rejects (Proxy @BaseResponseSuccess) (Object (KeyMap.insert "type" tag successJSON))
          rejects (Proxy @BaseResponseFailure) (Object (KeyMap.insert "type" tag failureJSON))
        forM_ [Null, Bool True, Number 1] $ \value -> do
          rejects (Proxy @BaseRequest) (Object (KeyMap.insert "id" value requestJSON))
          rejects (Proxy @BaseRequest) (Object (KeyMap.insert "method" value requestJSON))
          rejects (Proxy @BaseNotification) (Object (KeyMap.insert "method" value notificationJSON)),
      testCase "diagnostic fields retain their declared types" $ do
        forM_ ["localFactoryProtocolVersion", "peerFactoryProtocolVersion", "messageType", "method"] $ \key ->
          rejects (Proxy @ProtocolVersionMismatch) (Object (KeyMap.insert key Null mismatchJSON))
        rejects (Proxy @ProtocolVersionMismatch) (Object (KeyMap.insert "requestId" (Number 1) mismatchJSON))
        rejects (Proxy @ProtocolVersionMismatch) (Object (KeyMap.insert "messageType" (String "future") mismatchJSON)),
      testCase "bare notifications do not reject open identifier extensions" $ do
        let fields = KeyMap.insert "id" (Number 42) notificationJSON
            value = notification {baseNotificationAdditionalFields = KeyMap.singleton "id" (Number 42)}
        fromJSON (Object fields) @?= Success value
        toJSON value @?= Object fields,
      records "CommandAckSchema" (CommandAck mempty) (CommandAck mempty) (KeyMap.singleton "accepted" (Bool True)) (KeyMap.singleton "accepted" (Bool True)) (\extras ack -> ack {ackAdditionalFields = extras}),
      records "SuccessResultSchema" (SuccessResult False mempty) (SuccessResult False mempty) (KeyMap.singleton "success" (Bool False)) (KeyMap.singleton "success" (Bool False)) (\extras result -> result {resultAdditionalFields = extras}),
      records "SuccessOrErrorResultSchema" (SuccessOrErrorResult True (Just "message") mempty) (SuccessOrErrorResult True Nothing mempty) (KeyMap.fromList ["success" .= True, "error" .= String "message"]) (KeyMap.singleton "success" (Bool True)) (\extras result -> result {outcomeAdditionalFields = extras}),
      testCase "acknowledgements require actual Boolean true" $
        forM_ [Bool False, Number 1, String "true", Null] $ \value ->
          rejects (Proxy @CommandAck) (object ["accepted" .= value]),
      testCase "success flags do not impose an error-presence policy" $ do
        forM_ [False, True] $ \flag -> do
          let result = SuccessResult flag mempty
          fromJSON (object ["success" .= flag]) @?= Success result
          toJSON result @?= object ["success" .= flag]
          forM_ [Nothing, Just "", Just "message"] $ \err -> do
            let outcome = SuccessOrErrorResult flag err mempty
            fromJSON (toJSON outcome) @?= Success outcome
        rejects (Proxy @SuccessOrErrorResult) (object ["success" .= False, "error" .= Null])
        forM_ [Null, String "false", Number 0] $ \value -> do
          rejects (Proxy @SuccessResult) (object ["success" .= value])
          rejects (Proxy @SuccessOrErrorResult) (object ["success" .= value]),
      testCase "empty-object schema preserves its explicitly open fields" $ do
        definition <- either assertFailure pure (schemaAt ["definitions", "EmptyObjectSchema"] schema)
        definition @?= object ["type" .= String "object", "properties" .= object [], "additionalProperties" .= True]
        forM_ [mempty, KeyMap.singleton "future" (object ["value" .= Null])] $ \fields -> do
          fromJSON (Object fields) @?= (Success fields :: Result EmptyObject)
          toJSON (fields :: EmptyObject) @?= Object fields
        forM_ [Null, Bool True, Number 1, String "object", Array mempty] $ rejects (Proxy @EmptyObject)
    ]
  where
    records :: (Eq a, Show a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = objectTests name schema

objectTests :: forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Key -> Value -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
objectTests name schema full minimal fullJSON minimalJSON setExtra =
  testGroup
    (show name)
    [ testCase "golden covers every field in the schema" $ do
        props <- either assertFailure pure (schemaAt ["definitions", name, "properties"] schema)
        case props of
          Object fields -> sort (KeyMap.keys fields) @?= sort (KeyMap.keys fullJSON)
          _ -> assertFailure "Expected schema properties",
      testCase "full and minimal values use canonical wire fields" $
        forM_ [(full, fullJSON), (minimal, minimalJSON)] $ \(value, fields) -> do
          fromJSON (Object fields) @?= Success value
          toJSON value @?= Object fields
          eitherDecode (encode value) @?= Right value,
      testCase "schema-required fields must be present" $ do
        required <- either assertFailure pure (schemaAt ["definitions", name, "required"] schema)
        case fromJSON required :: Result [Key] of
          Error err -> assertFailure err
          Success keys -> forM_ keys $ \key -> rejects (Proxy @a) (Object (KeyMap.delete key fullJSON)),
      testCase "extensions remain distinct from known fields" $ do
        let extension = object ["nested" .= [Null, Bool True]]
            value = setExtra (KeyMap.singleton "future" extension) full
        fromJSON (Object (KeyMap.insert "future" extension fullJSON)) @?= Success value
        toJSON value @?= Object (KeyMap.insert "future" extension fullJSON)
        forM_ (KeyMap.keys fullJSON) $ \key ->
          toJSON (setExtra (KeyMap.singleton key (String "injected")) minimal) @?= Object minimalJSON,
      testCase "non-object inputs fail without pattern-match exceptions" $
        forM_ [Null, Bool True, Number 0, String "rpc", Array mempty] $
          rejects (Proxy @a)
    ]

rpcError :: JsonRpcError
rpcError = JsonRpcError RpcConflict "example" (Just (Bool False)) mempty

mismatch, minimalMismatch :: ProtocolVersionMismatch
mismatch = ProtocolVersionMismatch "1.205.0" "1.192.0" (Just RpcRequest) (Just "example") (Just (Just "request-id")) mempty
minimalMismatch = ProtocolVersionMismatch "1.205.0" "1.192.0" Nothing Nothing Nothing mempty

request :: BaseRequest
request = BaseRequest "request-id" "example" (Just (Number 7)) mempty

notification :: BaseNotification
notification = BaseNotification "notice" (Just (object [])) mempty

success :: BaseResponseSuccess
success = BaseResponseSuccess "request-id" (Just Null) (Just rpcError) mempty

failure :: BaseResponseFailure
failure = BaseResponseFailure Nothing rpcError (Just (toJSON ([] :: [Value]))) mempty

errorJSON, mismatchJSON, minimalMismatchJSON, requestJSON, notificationJSON, successJSON, failureJSON :: Object
errorJSON = KeyMap.fromList ["code" .= Number (-32006), "message" .= String "example", "data" .= Bool False]
minimalMismatchJSON = KeyMap.fromList ["localFactoryProtocolVersion" .= String "1.205.0", "peerFactoryProtocolVersion" .= String "1.192.0"]
mismatchJSON = KeyMap.union minimalMismatchJSON (KeyMap.fromList ["messageType" .= String "request", "method" .= String "example", "requestId" .= String "request-id"])
requestJSON = KeyMap.fromList ["type" .= String "request", "id" .= String "request-id", "method" .= String "example", "params" .= Number 7]
notificationJSON = KeyMap.fromList ["type" .= String "notification", "method" .= String "notice", "params" .= object []]
successJSON = KeyMap.fromList ["type" .= String "response", "id" .= String "request-id", "error" .= errorJSON, "result" .= Null]
failureJSON = KeyMap.fromList ["type" .= String "response", "id" .= Null, "error" .= errorJSON, "result" .= ([] :: [Value])]

remove :: [Key] -> Object -> Object
remove keys fields = foldr KeyMap.delete fields keys

schemaAt :: [Key] -> Value -> Either String Value
schemaAt keys value = foldM (\node key -> parseEither (withObject "schema node" (.: key)) node) value keys

rejects :: forall a. (FromJSON a) => Proxy a -> Value -> IO ()
rejects _ value = case fromJSON value :: Result a of
  Error _ -> pure ()
  Success _ -> assertFailure "Invalid JSON was accepted"
