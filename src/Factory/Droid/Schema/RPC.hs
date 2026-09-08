{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Factory protocol 1.205.0 errors, bare bodies and complete envelopes.
-- Decoding validates wire structure, not operation success or compatibility.
-- These codecs do not dispatch requests or choose SDK attribution.
module Factory.Droid.Schema.RPC
  ( JsonRpcErrorCode (..),
    JsonRpcMessageType (..),
    JsonRpcError (..),
    ProtocolVersionMismatch (..),
    RpcObject (..),
    WithEnvelope (..),
    JsonRpcEnvelope,
    JsonRpcBaseRequest,
    JsonRpcBaseNotification,
    JsonRpcBaseResponseSuccess,
    JsonRpcBaseResponseFailure,
    JsonRpcBaseResponse,
    JsonRpcMessage,
    BaseResponseGeneric (..),
    BaseResponse (..),
    RpcMessageBody (..),
    ResultResponse (..),
    RpcResponse (..),
    CommandAckResponse,
    EmptyObjectResponse,
    SuccessResultResponse,
    SuccessOrErrorResultResponse,
    BaseRequest (..),
    MethodRequest (..),
    eraseMethodRequest,
    BaseNotification (..),
    BaseResponseSuccess (..),
    BaseResponseFailure (..),
    CommandAck (..),
    SuccessResult (..),
    SuccessOrErrorResult (..),
    EmptyObject,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    withObject,
    withScientific,
    withText,
    (.:),
    (.:!),
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Internal.JSON
  ( additionalFields,
    fieldsWithAdditionalFields,
    objectWithAdditionalFields,
    optionalField,
    requireLiteral,
  )
import Factory.Droid.Schema.Metadata (TraceContextMeta)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | The nine error codes declared by the supplied Factory schema. This is
-- not an unrestricted numeric error code for every JSON-RPC implementation.
data JsonRpcErrorCode
  = RpcParseError
  | RpcInvalidRequest
  | RpcMethodNotFound
  | RpcInvalidParams
  | RpcInternalError
  | RpcAuthenticationError
  | RpcEntityNotFound
  | RpcSessionDisconnected
  | RpcConflict
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON JsonRpcErrorCode where
  parseJSON = withScientific "JsonRpcErrorCode" $ \case
    -32700 -> pure RpcParseError
    -32600 -> pure RpcInvalidRequest
    -32601 -> pure RpcMethodNotFound
    -32602 -> pure RpcInvalidParams
    -32603 -> pure RpcInternalError
    -32001 -> pure RpcAuthenticationError
    -32004 -> pure RpcEntityNotFound
    -32005 -> pure RpcSessionDisconnected
    -32006 -> pure RpcConflict
    _ -> fail "Unknown Factory JSON-RPC error code"

instance ToJSON JsonRpcErrorCode where
  toJSON RpcParseError = Number (-32700)
  toJSON RpcInvalidRequest = Number (-32600)
  toJSON RpcMethodNotFound = Number (-32601)
  toJSON RpcInvalidParams = Number (-32602)
  toJSON RpcInternalError = Number (-32603)
  toJSON RpcAuthenticationError = Number (-32001)
  toJSON RpcEntityNotFound = Number (-32004)
  toJSON RpcSessionDisconnected = Number (-32005)
  toJSON RpcConflict = Number (-32006)

-- | Factory's explicit RPC message discriminator.
data JsonRpcMessageType = RpcRequest | RpcResponse | RpcNotification
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FromJSON JsonRpcMessageType where
  parseJSON = withText "JsonRpcMessageType" $ \case
    "request" -> pure RpcRequest
    "response" -> pure RpcResponse
    "notification" -> pure RpcNotification
    _ -> fail "Unknown RPC message type"

instance ToJSON JsonRpcMessageType where
  toJSON RpcRequest = String "request"
  toJSON RpcResponse = String "response"
  toJSON RpcNotification = String "notification"

-- | Error data is explicitly unconstrained JSON. Nothing omits it, whereas
-- Just Null preserves an explicit null. This record is not an IO exception.
data JsonRpcError = JsonRpcError
  { rpcErrorCode :: !JsonRpcErrorCode,
    rpcErrorMessage :: !Text,
    rpcErrorData :: !(Maybe Value),
    rpcErrorAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \fields ->
    JsonRpcError <$> fields .: "code" <*> fields .: "message" <*> fields .:! "data" <*> pure (additionalFields errorKeys fields)

instance ToJSON JsonRpcError where
  toJSON err =
    objectWithAdditionalFields errorKeys (rpcErrorAdditionalFields err) $
      ["code" .= rpcErrorCode err, "message" .= rpcErrorMessage err] <> optionalField "data" (rpcErrorData err)

-- | Protocol mismatch diagnostics. requestId distinguishes missing, null
-- and a string. A mismatch report is not itself a compatibility policy.
data ProtocolVersionMismatch = ProtocolVersionMismatch
  { mismatchLocalVersion :: !Text,
    mismatchPeerVersion :: !Text,
    mismatchMessageType :: !(Maybe JsonRpcMessageType),
    mismatchMethod :: !(Maybe Text),
    mismatchRequestId :: !(Maybe (Maybe Text)),
    mismatchAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON ProtocolVersionMismatch where
  parseJSON = withObject "ProtocolVersionMismatch" $ \fields ->
    ProtocolVersionMismatch
      <$> fields .: "localFactoryProtocolVersion"
      <*> fields .: "peerFactoryProtocolVersion"
      <*> fields .:! "messageType"
      <*> fields .:! "method"
      <*> fields .:! "requestId"
      <*> pure (additionalFields mismatchKeys fields)

instance ToJSON ProtocolVersionMismatch where
  toJSON mismatch =
    objectWithAdditionalFields mismatchKeys (mismatchAdditionalFields mismatch) $
      ["localFactoryProtocolVersion" .= mismatchLocalVersion mismatch, "peerFactoryProtocolVersion" .= mismatchPeerVersion mismatch]
        <> optionalField "messageType" (mismatchMessageType mismatch)
        <> optionalField "method" (mismatchMethod mismatch)
        <> optionalField "requestId" (mismatchRequestId mismatch)

-- | A bare request. params is unconstrained in this base schema; a specific
-- operation's parameter codec supplies its stronger structure and validation.
data BaseRequest = BaseRequest
  { baseRequestId :: !Text,
    baseRequestMethod :: !Text,
    baseRequestParams :: !(Maybe Value),
    baseRequestAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BaseRequest where
  parseJSON = withObject "BaseRequest" $ \fields -> do
    requireLiteral "type" "request" fields
    BaseRequest <$> fields .: "id" <*> fields .: "method" <*> fields .:! "params" <*> pure (additionalFields requestKeys fields)

instance ToJSON BaseRequest where
  toJSON = Object . toRpcObject

instance RpcObject BaseRequest where
  toRpcObject request =
    fieldsWithAdditionalFields requestKeys (baseRequestAdditionalFields request) $
      ["type" .= RpcRequest, "id" .= baseRequestId request, "method" .= baseRequestMethod request]
        <> optionalField "params" (baseRequestParams request)

type role MethodRequest nominal nominal

-- | A required-parameter request with a type-level method literal. Both roles
-- are nominal: coercion cannot change its method or parameter contract. The
-- enclosing WithEnvelope supplies protocol metadata; extensions remain flat.
data MethodRequest (method :: Symbol) params = MethodRequest
  { methodRequestId :: !Text,
    methodRequestParams :: !params,
    methodRequestAdditionalFields :: !Object
  }
  deriving stock (Eq)

instance Show (MethodRequest method params) where
  show _ = "MethodRequest <redacted>"

instance (KnownSymbol method, FromJSON params) => FromJSON (MethodRequest method params) where
  parseJSON = withObject "MethodRequest" $ \fields -> do
    requireLiteral "type" "request" fields
    requireLiteral "method" (String (Text.pack (symbolVal (Proxy @method)))) fields
    MethodRequest <$> fields .: "id" <*> fields .: "params" <*> pure (additionalFields requestKeys fields)

instance (KnownSymbol method, ToJSON params) => ToJSON (MethodRequest method params) where
  toJSON = Object . toRpcObject

instance (KnownSymbol method, ToJSON params) => RpcObject (MethodRequest method params) where
  toRpcObject = toRpcObject . eraseMethodRequest

-- | Forget the method/parameter indices without changing the request's wire
-- fields. Required parameters remain present, including an explicitly null
-- value when the chosen parameter type permits it.
eraseMethodRequest :: forall method params. (KnownSymbol method, ToJSON params) => MethodRequest method params -> BaseRequest
eraseMethodRequest request =
  BaseRequest (methodRequestId request) (Text.pack (symbolVal (Proxy @method))) (Just (toJSON (methodRequestParams request))) (methodRequestAdditionalFields request)

-- | A bare notification, without a correlation identifier. Additional fields
-- are still permitted by the schema; this is not an envelope validator.
data BaseNotification = BaseNotification
  { baseNotificationMethod :: !Text,
    baseNotificationParams :: !(Maybe Value),
    baseNotificationAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BaseNotification where
  parseJSON = withObject "BaseNotification" $ \fields -> do
    requireLiteral "type" "notification" fields
    BaseNotification <$> fields .: "method" <*> fields .:! "params" <*> pure (additionalFields notificationKeys fields)

instance ToJSON BaseNotification where
  toJSON = Object . toRpcObject

instance RpcObject BaseNotification where
  toRpcObject notification =
    fieldsWithAdditionalFields notificationKeys (baseNotificationAdditionalFields notification) $
      ["type" .= RpcNotification, "method" .= baseNotificationMethod notification]
        <> optionalField "params" (baseNotificationParams notification)

-- | The raw shape named BaseResponseSuccessSchema. Despite its name, the
-- supplied schema permits an error and does not require result. This record
-- must not be used as evidence that an operation succeeded.
data BaseResponseSuccess = BaseResponseSuccess
  { successResponseId :: !Text,
    successResponseResult :: !(Maybe Value),
    successResponseError :: !(Maybe JsonRpcError),
    successResponseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BaseResponseSuccess where
  parseJSON = withObject "BaseResponseSuccess" $ \fields -> do
    requireLiteral "type" "response" fields
    BaseResponseSuccess <$> fields .: "id" <*> fields .:! "result" <*> fields .:! "error" <*> pure (additionalFields responseKeys fields)

instance ToJSON BaseResponseSuccess where
  toJSON = Object . toRpcObject

instance RpcObject BaseResponseSuccess where
  toRpcObject response =
    fieldsWithAdditionalFields responseKeys (successResponseAdditionalFields response) $
      ["type" .= RpcResponse, "id" .= successResponseId response]
        <> optionalField "result" (successResponseResult response)
        <> optionalField "error" (successResponseError response)

-- | A bare failure requires both id and error. A null id is represented by
-- Nothing and is always emitted; it is not an omitted identifier. Optional
-- result data is retained because the schema permits it alongside error.
data BaseResponseFailure = BaseResponseFailure
  { failureResponseId :: !(Maybe Text),
    failureResponseError :: !JsonRpcError,
    failureResponseResult :: !(Maybe Value),
    failureResponseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BaseResponseFailure where
  parseJSON = withObject "BaseResponseFailure" $ \fields -> do
    requireLiteral "type" "response" fields
    BaseResponseFailure <$> fields .: "id" <*> fields .: "error" <*> fields .:! "result" <*> pure (additionalFields responseKeys fields)

instance ToJSON BaseResponseFailure where
  toJSON = Object . toRpcObject

instance RpcObject BaseResponseFailure where
  toRpcObject response =
    fieldsWithAdditionalFields responseKeys (failureResponseAdditionalFields response) $
      ["type" .= RpcResponse, "id" .= failureResponseId response, "error" .= failureResponseError response]
        <> optionalField "result" (failureResponseResult response)

-- | A command acknowledgement has the literal accepted=true. Other fields
-- are retained as extensions; decoding does not establish command completion.
newtype CommandAck = CommandAck
  { ackAdditionalFields :: Object
  }
  deriving stock (Eq, Show)

instance FromJSON CommandAck where
  parseJSON = withObject "CommandAck" $ \fields -> do
    requireLiteral "accepted" (Bool True) fields
    pure (CommandAck (additionalFields ["accepted"] fields))

instance ToJSON CommandAck where
  toJSON ack = objectWithAdditionalFields ["accepted"] (ackAdditionalFields ack) ["accepted" .= True]

-- | A result with an actual Boolean success flag, not a literal true.
data SuccessResult = SuccessResult
  { resultSuccess :: !Bool,
    resultAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SuccessResult where
  parseJSON = withObject "SuccessResult" $ \fields ->
    SuccessResult <$> fields .: "success" <*> pure (additionalFields ["success"] fields)

instance ToJSON SuccessResult where
  toJSON result = objectWithAdditionalFields ["success"] (resultAdditionalFields result) ["success" .= resultSuccess result]

-- | A Boolean result and an optional, non-nullable error string. The schema
-- does not make error presence conditional on the value of success.
data SuccessOrErrorResult = SuccessOrErrorResult
  { outcomeSuccess :: !Bool,
    outcomeError :: !(Maybe Text),
    outcomeAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON SuccessOrErrorResult where
  parseJSON = withObject "SuccessOrErrorResult" $ \fields ->
    SuccessOrErrorResult <$> fields .: "success" <*> fields .:! "error" <*> pure (additionalFields ["success", "error"] fields)

instance ToJSON SuccessOrErrorResult where
  toJSON result = objectWithAdditionalFields ["success", "error"] (outcomeAdditionalFields result) (["success" .= outcomeSuccess result] <> optionalField "error" (outcomeError result))

-- | EmptyObjectSchema is an open JSON object, not a unit value. Its lack
-- of declared properties does not mean that extension fields are discarded.
type EmptyObject = Object

-- | Object-valued serialization for composing flat RPC envelopes. Instances
-- must agree with their ToJSON encoding and reserve their declared fields.
class RpcObject a where
  -- | Encode directly as an object, without a partial cast from Value.
  toRpcObject :: a -> Object

instance RpcObject Object where
  toRpcObject = id

-- | A flat JSON-RPC/Factory envelope. All extension fields reside in the body;
-- the four envelope keys are reserved even when optional fields are absent.
-- Protocol version is a signal, not an enforced compatibility decision.
-- Show redacts metadata and body; explicit fields and JSON remain sensitive.
data WithEnvelope a = WithEnvelope
  { envelopeProtocolVersion :: !(Maybe Text),
    envelopeMeta :: !(Maybe TraceContextMeta),
    envelopeBody :: !a
  }
  deriving stock (Eq)

instance Show (WithEnvelope a) where
  show _ = "WithEnvelope <redacted>"

instance (FromJSON a) => FromJSON (WithEnvelope a) where
  parseJSON = withObject "WithEnvelope" $ \fields -> do
    requireLiteral "jsonrpc" "2.0" fields
    requireLiteral "factoryApiVersion" "1.0.0" fields
    WithEnvelope <$> fields .:! "factoryProtocolVersion" <*> fields .:! "_meta" <*> parseJSON (Object (additionalFields envelopeKeys fields))

instance (RpcObject a) => ToJSON (WithEnvelope a) where
  toJSON = Object . toRpcObject

instance (RpcObject a) => RpcObject (WithEnvelope a) where
  toRpcObject envelope =
    fieldsWithAdditionalFields envelopeKeys (toRpcObject (envelopeBody envelope)) $
      ["jsonrpc" .= ("2.0" :: Text), "factoryApiVersion" .= ("1.0.0" :: Text)]
        <> optionalField "factoryProtocolVersion" (envelopeProtocolVersion envelope)
        <> optionalField "_meta" (envelopeMeta envelope)

-- | Standalone open envelope (JsonRpcEnvelopeSchema).
type JsonRpcEnvelope = WithEnvelope Object

-- | Complete request (JsonRpcBaseRequestSchema).
type JsonRpcBaseRequest = WithEnvelope BaseRequest

-- | Complete notification (JsonRpcBaseNotificationSchema).
type JsonRpcBaseNotification = WithEnvelope BaseNotification

-- | Complete raw string-ID response; result remains optional.
type JsonRpcBaseResponseSuccess = WithEnvelope BaseResponseSuccess

-- | Complete failure with required, possibly null identifier and error.
type JsonRpcBaseResponseFailure = WithEnvelope BaseResponseFailure

-- | Complete base-response union, with failure-first decoding.
type JsonRpcBaseResponse = WithEnvelope BaseResponse

-- | Complete discriminated message union (JsonRpcMessageSchema).
type JsonRpcMessage = WithEnvelope RpcMessageBody

-- | The generic response branch of JsonRpcMessageSchema permits a null ID
-- even without an error. It is wider than JsonRpcBaseResponseSchema.
data BaseResponseGeneric = BaseResponseGeneric
  { genericResponseId :: !(Maybe Text),
    genericResponseResult :: !(Maybe Value),
    genericResponseError :: !(Maybe JsonRpcError),
    genericResponseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance FromJSON BaseResponseGeneric where
  parseJSON = withObject "BaseResponseGeneric" $ \fields -> do
    requireLiteral "type" "response" fields
    BaseResponseGeneric <$> fields .: "id" <*> fields .:! "result" <*> fields .:! "error" <*> pure (additionalFields responseKeys fields)

instance ToJSON BaseResponseGeneric where
  toJSON = Object . toRpcObject

instance RpcObject BaseResponseGeneric where
  toRpcObject response =
    fieldsWithAdditionalFields responseKeys (genericResponseAdditionalFields response) $
      ["type" .= RpcResponse, "id" .= genericResponseId response]
        <> optionalField "result" (genericResponseResult response)
        <> optionalField "error" (genericResponseError response)

-- | Bare base-response union, following the schema's failure-first order.
-- Branches overlap: decoding preserves JSON, not necessarily the constructor
-- chosen when a value was built by hand.
data BaseResponse = BaseFailure !BaseResponseFailure | BaseSuccess !BaseResponseSuccess
  deriving stock (Eq, Show)

instance FromJSON BaseResponse where
  parseJSON value = BaseFailure <$> parseJSON value <|> BaseSuccess <$> parseJSON value

instance ToJSON BaseResponse where
  toJSON = Object . toRpcObject

instance RpcObject BaseResponse where
  toRpcObject (BaseFailure response) = toRpcObject response
  toRpcObject (BaseSuccess response) = toRpcObject response

-- | Discriminated bare message bodies. Each branch retains its own open
-- extensions; only the response branch declares a nullable identifier.
data RpcMessageBody
  = RequestBody !BaseRequest
  | ResponseBody !BaseResponseGeneric
  | NotificationBody !BaseNotification
  deriving stock (Eq, Show)

instance FromJSON RpcMessageBody where
  parseJSON value = withObject "RpcMessageBody" (\fields -> fields .: "type" >>= parseBody) value
    where
      parseBody RpcRequest = RequestBody <$> parseJSON value
      parseBody RpcResponse = ResponseBody <$> parseJSON value
      parseBody RpcNotification = NotificationBody <$> parseJSON value

instance ToJSON RpcMessageBody where
  toJSON = Object . toRpcObject

instance RpcObject RpcMessageBody where
  toRpcObject (RequestBody request) = toRpcObject request
  toRpcObject (ResponseBody response) = toRpcObject response
  toRpcObject (NotificationBody notification) = toRpcObject notification

-- | A string-ID response with a required typed result. The schema still
-- permits an error alongside that result; this type does not imply success.
data ResultResponse a = ResultResponse
  { resultResponseId :: !Text,
    resultResponseResult :: !a,
    resultResponseError :: !(Maybe JsonRpcError),
    resultResponseAdditionalFields :: !Object
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (ResultResponse a) where
  parseJSON = withObject "ResultResponse" $ \fields -> do
    requireLiteral "type" "response" fields
    ResultResponse <$> fields .: "id" <*> fields .: "result" <*> fields .:! "error" <*> pure (additionalFields responseKeys fields)

instance (ToJSON a) => ToJSON (ResultResponse a) where
  toJSON = Object . toRpcObject

instance (ToJSON a) => RpcObject (ResultResponse a) where
  toRpcObject response =
    fieldsWithAdditionalFields responseKeys (resultResponseAdditionalFields response) $
      ["type" .= RpcResponse, "id" .= resultResponseId response, "result" .= resultResponseResult response]
        <> optionalField "error" (resultResponseError response)

-- | A typed-result response or unrestricted failure, in schema order.
-- A malformed typed result can still match the failure branch when a valid
-- error is present. Overlapping values decode result-first, preserving JSON.
data RpcResponse a = ResponseResult !(ResultResponse a) | ResponseFailure !BaseResponseFailure
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (RpcResponse a) where
  parseJSON value = ResponseResult <$> parseJSON value <|> ResponseFailure <$> parseJSON value

instance (ToJSON a) => ToJSON (RpcResponse a) where
  toJSON = Object . toRpcObject

instance (ToJSON a) => RpcObject (RpcResponse a) where
  toRpcObject (ResponseResult response) = toRpcObject response
  toRpcObject (ResponseFailure response) = toRpcObject response

-- | Complete command acknowledgement or error; acceptance is not completion.
type CommandAckResponse = WithEnvelope (RpcResponse CommandAck)

-- | Complete open-object result or error.
type EmptyObjectResponse = WithEnvelope (RpcResponse EmptyObject)

-- | Complete Boolean-success result or error.
type SuccessResultResponse = WithEnvelope (RpcResponse SuccessResult)

-- | Complete Boolean-success/optional-error result or error.
type SuccessOrErrorResultResponse = WithEnvelope (RpcResponse SuccessOrErrorResult)

envelopeKeys, errorKeys, mismatchKeys, requestKeys, notificationKeys, responseKeys :: [Key]
envelopeKeys = ["jsonrpc", "factoryApiVersion", "factoryProtocolVersion", "_meta"]
errorKeys = ["code", "message", "data"]
mismatchKeys = ["localFactoryProtocolVersion", "peerFactoryProtocolVersion", "messageType", "method", "requestId"]
requestKeys = ["type", "id", "method", "params"]
notificationKeys = ["type", "method", "params"]
responseKeys = ["type", "id", "error", "result"]
