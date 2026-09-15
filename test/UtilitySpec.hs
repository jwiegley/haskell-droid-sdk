{-# LANGUAGE OverloadedStrings #-}

module UtilitySpec (utilityTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Schema.Content
import Factory.Droid.Schema.Enums (MessageRole (..))
import Factory.Droid.Schema.Host (machineConnectionType)
import Factory.Droid.Schema.Interaction
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.Schema.Notifications (equalToolStreamingUpdates)
import Factory.Droid.Schema.RPC
import Factory.Droid.Schema.Session (SessionTag, inspectSubagentSessionTag)
import Factory.Droid.Schema.Settings (hasDecoupledInteractionSettings)
import Factory.Droid.SessionState qualified as State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

utilityTests :: TestTree
utilityTests =
  testGroup
    "Public utility helpers"
    [ testCase "raw subagent inspection preserves non-string calling metadata without weakening its codec" $
        forM_ [(String "", Number 42), (Null, Bool False), (Number 9007199254740993, Array mempty)] $ \(caller, tool) -> do
          let metadata = KeyMap.fromList [("callingSessionId", caller), ("callingToolUseId", tool), ("future", Bool False)]
              first = KeyMap.fromList [("name", String "subagent"), ("metadata", Object metadata), ("future", Number 9007199254740993)]
              later = KeyMap.fromList [("name", String "subagent"), ("metadata", object ["callingToolUseId" .= String "ignored"])]
              selected = inspectSubagentSessionTag [first, later]
          selected @?= Just first
          case selected >>= KeyMap.lookup "metadata" of
            Just (Object values) -> (KeyMap.lookup "callingSessionId" values, KeyMap.lookup "callingToolUseId" values) @?= (Just caller, Just tool)
            _ -> assertFailure "Raw metadata was lost"
          case fromJSON @SessionTag (Object first) of
            Error _ -> pure ()
            Success _ -> assertFailure "Raw inspection weakened the string-metadata codec",
      testCase "raw first subagent tags retain missing null scalar and empty metadata" $ do
        let later = KeyMap.fromList [("name", String "subagent"), ("metadata", object ["callingSessionId" .= String "later"])]
        inspectSubagentSessionTag [] @?= Nothing
        inspectSubagentSessionTag [KeyMap.singleton "name" (String "Subagent"), KeyMap.singleton "name" (Number 1)] @?= Nothing
        forM_ [mempty, KeyMap.singleton "metadata" Null, KeyMap.singleton "metadata" (String "raw"), KeyMap.singleton "metadata" (Object mempty)] $ \fields -> do
          let first = KeyMap.insert "name" (String "subagent") fields
          inspectSubagentSessionTag [first, later] @?= Just first,
      testCase "content construction preserves source order, extensions and default empty text" $ do
        let image = Base64ImageSource "AQ==" ImagePNG (KeyMap.singleton "future" (Bool False))
            second = image {imageSourceData = ""}
            document = PlainTextDocument (PlainTextSource "document" (Just "") Nothing mempty)
        buildUserMessageContent defaultUserContentOptions (Just "") [image, second] [document]
          @?= [ContentImage (ImageBlock image Nothing base), ContentImage (ImageBlock second Nothing base), ContentDocument (DocumentBlock document base), ContentText (TextBlock "" base)],
      testCase "content construction distinguishes absent text from explicit empty text" $ do
        buildUserMessageContent defaultUserContentOptions Nothing [] [] @?= []
        buildUserMessageContent defaultUserContentOptions (Just "") [] [] @?= [ContentText (TextBlock "" base)]
        buildUserMessageContent (defaultUserContentOptions {contentIncludeEmptyText = False}) (Just "") [] [] @?= [],
      testCase "content trimming is optional ECMAScript trimming with independent empty suppression" $ do
        let whitespace = "\xfeff \t\x2028"
            trimmed = defaultUserContentOptions {contentTrimText = True}
        buildUserMessageContent defaultUserContentOptions (Just whitespace) [] [] @?= [ContentText (TextBlock whitespace base)]
        buildUserMessageContent trimmed (Just whitespace) [] [] @?= [ContentText (TextBlock "" base)]
        buildUserMessageContent (trimmed {contentIncludeEmptyText = False}) (Just whitespace) [] [] @?= []
        buildUserMessageContent trimmed (Just " \x200b ") [] [] @?= [ContentText (TextBlock "\x200b" base)],
      testCase "raw tool-result identity uses only nullish fallback without coercion" $ do
        inspectToolResultId mempty @?= Nothing
        inspectToolResultId (KeyMap.singleton "tool_use_id" Null) @?= Just Null
        inspectToolResultId (KeyMap.fromList [("toolUseId", Null), ("tool_use_id", String "legacy")]) @?= Just (String "legacy")
        forM_ [String "", Bool False, Number 0, Number 9007199254740993, Array mempty, Object mempty] $ \value ->
          inspectToolResultId (KeyMap.fromList [("toolUseId", value), ("tool_use_id", String "ignored")]) @?= Just value,
      testCase "raw identity inspection does not admit legacy spelling to the canonical codec" $ do
        let fields = KeyMap.fromList [("type", String "tool_result"), ("tool_use_id", String "legacy")]
        inspectToolResultId fields @?= Just (String "legacy")
        case fromJSON @ToolResultBlock (Object fields) of
          Error _ -> pure ()
          Success _ -> assertFailure "Legacy field weakened canonical tool-result decoding",
      testCase "pending result detection requires the exact scalar marker" $ do
        let marker = "__TOOL_RESULT_PENDING__"
            result content = ToolResultBlock "tool" content (Just False) base
        isPendingToolResult (result (Just (ResultText marker))) @?= True
        forM_ [Nothing, Just (ResultText ""), Just (ResultText (marker <> " ")), Just (ResultBlocks []), Just (ResultBlocks [ResultTextBlock (TextBlock marker base)])] $ \content ->
          isPendingToolResult (result content) @?= False,
      testCase "tool summary handles empty and zero-timestamp histories without fabricated duration" $ do
        State.summarizeSessionToolUsage [] @?= (0, Nothing)
        zero <- message "zero" RoleAssistant 0 0 [toolValue]
        State.summarizeSessionToolUsage [zero] @?= (1, Nothing),
      testCase "tool summary counts assistant blocks and honors updated-time fallback in supplied order" $ do
        first <- message "first" RoleUser 10 0 [toolValue]
        second <- message "second" RoleAssistant 9007199254740993 20 [toolValue, toolValue]
        ignored <- message "zero" RoleUser 0 0 []
        lastMessage <- message "last" RoleAssistant 40 0 []
        State.summarizeSessionToolUsage [first, second, ignored, lastMessage] @?= (2, Just 30),
      testCase "tool summary does not sort, take absolute duration or discard negative timestamps" $ do
        later <- message "later" RoleUser 20 0 []
        earlier <- message "earlier" RoleAssistant 10 0 []
        negative <- message "negative" RoleUser (-5) 0 []
        State.summarizeSessionToolUsage [later, earlier] @?= (0, Nothing)
        State.summarizeSessionToolUsage [earlier, earlier] @?= (0, Nothing)
        State.summarizeSessionToolUsage [negative, earlier] @?= (0, Just 15),
      testCase "permission display preserves nonempty and whitespace input strings" $ do
        let input = KeyMap.fromList [("plan", String " "), ("title", String "existing"), ("future", Bool False)]
        info <- permission ConfirmationTypeExitSpecMode (ConfirmationExitSpecMode "fallback" (Just "new title")) input
        permissionToolInputForDisplay info @?= input,
      testCase "permission display fills only missing empty or non-string matching fields" $ do
        forM_ [Null, Bool False, Number 0, Array mempty, String ""] $ \value -> do
          let input = KeyMap.fromList [("plan", value), ("title", value), ("future", Number 9007199254740993)]
          info <- permission ConfirmationTypeExitSpecMode (ConfirmationExitSpecMode "plan" (Just "title")) input
          permissionToolInputForDisplay info @?= KeyMap.fromList [("plan", String "plan"), ("title", String "title"), ("future", Number 9007199254740993)],
      testCase "empty permission fallbacks do not replace input but whitespace is nonempty" $ do
        let input = KeyMap.fromList [("plan", Null), ("title", String "")]
        empty <- permission ConfirmationTypeExitSpecMode (ConfirmationExitSpecMode "" (Just "")) input
        permissionToolInputForDisplay empty @?= input
        whitespace <- permission ConfirmationTypeExitSpecMode (ConfirmationExitSpecMode " " Nothing) input
        permissionToolInputForDisplay whitespace @?= KeyMap.insert "plan" (String " ") input,
      testCase "permission display requires matching outer and detail types" $ do
        forM_ [(ConfirmationTypeExec, ConfirmationExitSpecMode "plan" (Just "title")), (ConfirmationTypeExitSpecMode, ConfirmationProposeMission "proposal" (Just "title"))] $ \(kind, details) -> do
          info <- permission kind details mempty
          permissionToolInputForDisplay info @?= mempty,
      testCase "mission permission display merges proposal and title without changing other fields" $ do
        let input = KeyMap.fromList [("plan", String "unrelated"), ("title", String " ")]
        info <- permission ConfirmationTypeProposeMission (ConfirmationProposeMission "proposal" (Just "ignored")) input
        permissionToolInputForDisplay info @?= KeyMap.insert "proposal" (String "proposal") input,
      testCase "decoupled settings detection is presence, including null and invalid values" $ do
        hasDecoupledInteractionSettings mempty @?= False
        hasDecoupledInteractionSettings (KeyMap.singleton "other" Null) @?= False
        forM_ ["interactionMode", "autonomyLevel"] $ \key ->
          forM_ [Null, Bool False, Number 0, String "", Object mempty] $ \value ->
            hasDecoupledInteractionSettings (KeyMap.singleton key value) @?= True,
      testCase "streaming equality ignores only the top-level timestamp" $ do
        let common = KeyMap.fromList [("text", String ""), ("future", Bool False)]
        equalToolStreamingUpdates common (KeyMap.insert "timestamp" Null common) @?= True
        equalToolStreamingUpdates (KeyMap.insert "timestamp" (Number 0) common) (KeyMap.insert "timestamp" (String "later") common) @?= True
        equalToolStreamingUpdates (KeyMap.singleton "nested" (object ["timestamp" .= Number 1])) (KeyMap.singleton "nested" (object ["timestamp" .= Number 2])) @?= False
        equalToolStreamingUpdates (KeyMap.singleton "count" (Number 9007199254740993)) (KeyMap.singleton "count" (Number 9007199254740992)) @?= False
        equalToolStreamingUpdates common (KeyMap.delete "future" common) @?= False,
      testCase "machine connection labels are exact descriptive mappings" $ do
        map machineConnectionType ["computer", "local", "ephemeral", "", "Local", "future"] @?= [Just "computer", Just "tui", Just "workspace", Nothing, Nothing, Nothing],
      testCase "partial envelope inspection accepts missing fields without relaxing strict decoding" $ do
        inspectJsonRpcEnvelope "local" (Object mempty) @?= Just (mempty, Nothing)
        let fields = KeyMap.fromList [("type", String "request"), ("id", Null), ("method", String ""), ("future", Bool False)]
        inspectJsonRpcEnvelope "local" (Object fields) @?= Just (fields, Nothing)
        case fromJSON @JsonRpcMessage (Object fields) of
          Error _ -> pure ()
          Success _ -> assertFailure "Partial envelope entered strict RPC decoding",
      testCase "envelope inspection retains raw fields and distinguishes absent null and empty IDs" $ do
        forM_ [(Nothing, Nothing), (Just Null, Just Nothing), (Just (String ""), Just (Just ""))] $ \(rawId, expectedId) -> do
          let fields = KeyMap.fromList ([("factoryProtocolVersion", String "peer"), ("type", String "response"), ("method", String ""), ("future", Number 9007199254740993)] <> maybe [] (\value -> [("id", value)]) rawId)
              mismatch = ProtocolVersionMismatch "local" "peer" (Just RpcResponse) (Just "") expectedId mempty
          inspectJsonRpcEnvelope "local" (Object fields) @?= Just (fields, Just mismatch),
      testCase "envelope inspection validates optional known fields and nested metadata" $ do
        let malformed =
              [ ("jsonrpc", Null),
                ("jsonrpc", String "1.0"),
                ("factoryApiVersion", String "2.0"),
                ("factoryProtocolVersion", Null),
                ("factoryProtocolVersion", Number 0),
                ("_meta", Null),
                ("_meta", object ["traceparent" .= Null]),
                ("type", String "future"),
                ("type", Null),
                ("method", Null),
                ("method", Bool False),
                ("id", Number 0),
                ("id", Array mempty)
              ]
        forM_ malformed $ \(key, value) -> inspectJsonRpcEnvelope "local" (Object (KeyMap.singleton key value)) @?= Nothing
        forM_ [Null, String "message", Number 0, Bool False, Array mempty] $ \value -> inspectJsonRpcEnvelope "local" value @?= Nothing,
      testCase "equal or omitted peer versions have no mismatch and body fields remain uninterpreted" $ do
        let fields = KeyMap.fromList [("jsonrpc", String "2.0"), ("factoryApiVersion", String "1.0.0"), ("factoryProtocolVersion", String "local"), ("_meta", object ["traceparent" .= String "", "future" .= Bool False]), ("result", Bool False), ("error", String "not an RPC error"), ("params", Null)]
        inspectJsonRpcEnvelope "local" (Object fields) @?= Just (fields, Nothing)
        inspectJsonRpcEnvelope "local" (Object (KeyMap.delete "factoryProtocolVersion" fields)) @?= Just (KeyMap.delete "factoryProtocolVersion" fields, Nothing)
    ]

base :: BaseContentBlock
base = BaseContentBlock Nothing mempty

toolValue :: Value
toolValue = object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "lookup", "input" .= object []]

message :: Text -> MessageRole -> Scientific -> Scientific -> [Value] -> IO FactoryDroidMessage
message identifier role created updated content = decodeFixture (object ["id" .= identifier, "role" .= role, "content" .= content, "createdAt" .= created, "updatedAt" .= updated])

permission :: ToolConfirmationType -> ConfirmationDetails -> Object -> IO ToolConfirmationInfo
permission kind details input = do
  tool <- decodeFixture (object ["type" .= String "tool_use", "id" .= String "tool", "name" .= String "display", "input" .= Object input])
  pure (ToolConfirmationInfo tool kind (ToolConfirmationDetails details mempty) mempty)

decodeFixture :: (FromJSON a) => Value -> IO a
decodeFixture value = case fromJSON value of
  Error err -> assertFailure err
  Success result -> pure result
