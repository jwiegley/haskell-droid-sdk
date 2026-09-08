{-# LANGUAGE OverloadedStrings #-}

module TerminalSpec (terminalTests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Factory.Droid.Schema.Daemon.Terminal
import Factory.Droid.Schema.Primitives (mkRfc3339Timestamp)
import Factory.Droid.Schema.Session (SessionIdParams (..))
import SchemaTest (nonNullableRecordTests, redactedRecordTests, rejects, schemaAt, schemaIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

terminalTests :: Value -> Value -> TestTree
terminalTests shared schema = case mkRfc3339Timestamp timestampText of
  Nothing -> testCase "timestamp fixture" (assertFailure "Valid terminal timestamp rejected")
  Just timestamp ->
    let screen = TerminalScreenState "\ESC[31mtext" "plain" (-1.25) 2.5 timestamp (Just False) mempty
        info = TerminalInfo "terminal" (Just (-3.5)) 80.5 24.25 timestamp (Just screen) mempty
        minimalInfo = TerminalInfo "terminal" Nothing 80.5 24.25 timestamp Nothing mempty
        listing = ListTerminalsResult [info, minimalInfo] mempty
        listingJSON = KeyMap.singleton "terminals" (toJSON [Object infoJSON, Object minimalInfoJSON])
        resultBranch index = definition "CreateTerminalResultSchema" >>= schemaAt ["anyOf"] >>= schemaIndex index
     in testGroup
          "Terminal wire bodies"
          [ records "CreateTerminalRequestParamsSchema" create minimalCreate createJSON minimalCreateJSON (\extras value -> value {createTerminalAdditionalFields = extras}),
            records "WriteDataRequestParamsSchema" input input inputJSON inputJSON (\extras value -> value {writeTerminalAdditionalFields = extras}),
            records "ResizeRequestParamsSchema" resize resize resizeJSON resizeJSON (\extras value -> value {resizeTerminalAdditionalFields = extras}),
            records "CloseTerminalRequestParamsSchema" close close closeJSON closeJSON (\extras value -> value {closeTerminalAdditionalFields = extras}),
            records "DaemonCreateTerminalRequestParamsSchema" (DaemonCreateTerminalParams "session" create) (DaemonCreateTerminalParams "session" minimalCreate) (withSession createJSON) (withSession minimalCreateJSON) (\extras (DaemonCreateTerminalParams sid value) -> DaemonCreateTerminalParams sid (value {createTerminalAdditionalFields = extras})),
            records "DaemonWriteDataRequestParamsSchema" (DaemonWriteTerminalDataParams "session" input) (DaemonWriteTerminalDataParams "session" input) (withSession inputJSON) (withSession inputJSON) (\extras (DaemonWriteTerminalDataParams sid value) -> DaemonWriteTerminalDataParams sid (value {writeTerminalAdditionalFields = extras})),
            records "DaemonResizeRequestParamsSchema" (DaemonResizeTerminalParams "session" resize) (DaemonResizeTerminalParams "session" resize) (withSession resizeJSON) (withSession resizeJSON) (\extras (DaemonResizeTerminalParams sid value) -> DaemonResizeTerminalParams sid (value {resizeTerminalAdditionalFields = extras})),
            records "DaemonCloseTerminalRequestParamsSchema" (DaemonCloseTerminalParams "session" close) (DaemonCloseTerminalParams "session" close) (withSession closeJSON) (withSession closeJSON) (\extras (DaemonCloseTerminalParams sid value) -> DaemonCloseTerminalParams sid (value {closeTerminalAdditionalFields = extras})),
            nonNullableRecordTests "DaemonListTerminalsRequestParamsSchema" (definition "DaemonListTerminalsRequestParamsSchema") (SessionIdParams "session" mempty) (SessionIdParams "session" mempty) (KeyMap.singleton "sessionId" (String "session")) (KeyMap.singleton "sessionId" (String "session")) (\extras value -> value {sessionParamsAdditionalFields = extras}),
            redactedRecordTests "terminal-created result" (resultBranch 0) (TerminalCreated mempty) (TerminalCreated mempty) (KeyMap.singleton "success" (Bool True)) (KeyMap.singleton "success" (Bool True)) (\extras _ -> TerminalCreated extras),
            redactedRecordTests "terminal-exists result" (resultBranch 1) (TerminalAlreadyExists mempty) (TerminalAlreadyExists mempty) existsJSON existsJSON (\extras _ -> TerminalAlreadyExists extras),
            redactedRecordTests "screen state" (definition "TerminalInfoSchema" >>= schemaAt ["properties", "state"]) screen (screen {screenCursorHidden = Nothing}) screenJSON (KeyMap.delete "cursorHidden" screenJSON) (\extras value -> value {screenAdditionalFields = extras}),
            records "ListTerminalsResultSchema" listing listing listingJSON listingJSON (\extras value -> value {listedTerminalsAdditionalFields = extras}),
            records "TerminalDataNotificationSchema" output output outputJSON outputJSON (\extras value -> value {terminalDataAdditionalFields = extras}),
            records "TerminalExitNotificationSchema" exited exited exitedJSON exitedJSON (\extras value -> value {terminalExitAdditionalFields = extras}),
            testCase "scoped schemas extend the base bodies only with sessionId" $
              forM_ [("CreateTerminalRequestParamsSchema", "DaemonCreateTerminalRequestParamsSchema"), ("WriteDataRequestParamsSchema", "DaemonWriteDataRequestParamsSchema"), ("ResizeRequestParamsSchema", "DaemonResizeRequestParamsSchema"), ("CloseTerminalRequestParamsSchema", "DaemonCloseTerminalRequestParamsSchema")] $ \(baseName, scopedName) -> do
                base <- either assertFailure pure (definition baseName)
                scoped <- either assertFailure pure (definition scopedName)
                props <- either assertFailure pure (schemaAt ["properties"] scoped)
                required <- either assertFailure pure (schemaAt ["required"] scoped)
                case (scoped, props, required) of
                  (Object fields, Object properties, Array keys) ->
                    Object (KeyMap.insert "properties" (Object (KeyMap.delete "sessionId" properties)) (KeyMap.insert "required" (toJSON (filter (/= String "sessionId") (foldr (:) [] keys))) fields)) @?= base
                  _ -> assertFailure "Expected scoped record schema",
            testCase "list parameters reuse the exact shared session-ID body" $
              definition "DaemonListTerminalsRequestParamsSchema" @?= schemaAt ["definitions", "SessionIdParamsSchema"] shared,
            testCase "base extensions do not forge the scoped session ID" $ do
              let injected = create {createTerminalAdditionalFields = KeyMap.singleton "sessionId" (String "injected")}
              toJSON injected @?= Object (KeyMap.insert "sessionId" (String "injected") createJSON)
              toJSON (DaemonCreateTerminalParams "session" injected) @?= Object (withSession createJSON)
              fromJSON (Object (withSession createJSON)) @?= Success (DaemonCreateTerminalParams "session" create),
            testCase "create-result branches keep distinct extension contracts" $ do
              let value = object ["success" .= True, "error" .= Null]
              fromJSON value @?= Success (TerminalCreated (KeyMap.singleton "error" Null))
              toJSON (TerminalCreated (KeyMap.singleton "error" Null)) @?= value
              rejects (Proxy @CreateTerminalResult) (object ["success" .= False, "error" .= String "future"])
              rejects (Proxy @CreateTerminalResult) (object ["success" .= False])
              rejects (Proxy @CreateTerminalResult) (object ["success" .= Number 1]),
            testCase "terminal info fixtures cover fields and preserve full/minimal shapes" $ do
              props <- either assertFailure pure (definition "TerminalInfoSchema" >>= schemaAt ["properties"])
              case props of
                Object fields -> sort (KeyMap.keys fields) @?= sort (KeyMap.keys infoJSON)
                _ -> assertFailure "Expected terminal properties"
              forM_ [(info, infoJSON), (minimalInfo, minimalInfoJSON)] $ \(value, wire) -> do
                fromJSON (Object wire) @?= Success value
                toJSON value @?= Object wire
                eitherDecode (encode value) @?= Right value,
            testCase "PID is required nullable while other info fields reject null" $ do
              required <- either assertFailure pure (definition "TerminalInfoSchema" >>= schemaAt ["required"])
              case fromJSON required :: Result [Key] of
                Error err -> assertFailure err
                Success keys -> forM_ keys $ \key -> rejects (Proxy @TerminalInfo) (Object (KeyMap.delete key infoJSON))
              fromJSON (Object (KeyMap.insert "pid" Null infoJSON)) @?= Success (info {terminalInfoPid = Nothing})
              forM_ (filter (/= "pid") (KeyMap.keys infoJSON)) $ \key -> rejects (Proxy @TerminalInfo) (Object (KeyMap.insert key Null infoJSON))
              forM_ [Bool False, String "pid", Object mempty, Array mempty] $ \bad -> rejects (Proxy @TerminalInfo) (Object (KeyMap.insert "pid" bad infoJSON)),
            testCase "terminal timestamp formats are asserted without coercion" $ do
              (definition "TerminalInfoSchema" >>= schemaAt ["properties", "createdAt", "format"]) @?= Right (String "date-time")
              (definition "TerminalInfoSchema" >>= schemaAt ["properties", "state", "properties", "timestamp", "format"]) @?= Right (String "date-time")
              forM_ [Number 0, Null, String "2026-02-30T12:00:00Z", String "2026-09-05T12:00Z"] $ \bad -> do
                rejects (Proxy @TerminalInfo) (Object (KeyMap.insert "createdAt" bad infoJSON))
                rejects (Proxy @TerminalScreenState) (Object (KeyMap.insert "timestamp" bad screenJSON)),
            testCase "info and screen extensions preserve nested scope and redact Show" $ do
              let extra = KeyMap.singleton "fixture" (String "not-a-real-secret")
                  enriched = info {terminalInfoState = Just (screen {screenAdditionalFields = extra}), terminalInfoAdditionalFields = extra}
              fromJSON (toJSON enriched) @?= Success enriched
              show enriched @?= "TerminalInfo <redacted>"
              forM_ (KeyMap.keys infoJSON) $ \key -> toJSON (minimalInfo {terminalInfoAdditionalFields = KeyMap.singleton key Null}) @?= Object minimalInfoJSON,
            testCase "lists reject malformed terminal records and retain empty lists" $ do
              fromJSON (object ["terminals" .= ([] :: [Value])]) @?= Success (ListTerminalsResult [] mempty)
              forM_ [Null, object [], Object (KeyMap.delete "pid" infoJSON)] $ \bad -> rejects (Proxy @ListTerminalsResult) (object ["terminals" .= [bad]])
              rejects (Proxy @TerminalInfo) (Object (KeyMap.insert "state" (object []) infoJSON)),
            testCase "dimensions, PID and exit values retain exact JSON numbers" $ do
              forM_ [-1.25, 0, 123456789012345678901234567890] $ \number -> do
                fromJSON (Object (KeyMap.insert "cols" (Number number) createJSON)) @?= Success (create {createdTerminalCols = Just number})
                fromJSON (Object (KeyMap.insert "rows" (Number number) resizeJSON)) @?= Success (resize {resizedTerminalRows = number})
                fromJSON (Object (KeyMap.insert "pid" (Number number) infoJSON)) @?= Success (info {terminalInfoPid = Just number})
                fromJSON (Object (KeyMap.insert "exitCode" (Number number) exitedJSON)) @?= Success (exited {terminalExitCode = number}),
            testCase "environment maps remain string-valued and may be empty" $ do
              rejects (Proxy @CreateTerminalParams) (Object (KeyMap.insert "env" (object ["fixture" .= Null]) createJSON))
              let value = minimalCreate {createdTerminalEnv = Just mempty}
              fromJSON (Object (KeyMap.insert "env" (Object mempty) minimalCreateJSON)) @?= Success value,
            testCase "notification union covers exactly its two schema alternatives" $ do
              (definition "TerminalNotificationSchema" >>= schemaAt ["anyOf"]) @?= Right (toJSON [object ["$ref" .= String "#/definitions/TerminalDataNotificationSchema"], object ["$ref" .= String "#/definitions/TerminalExitNotificationSchema"]])
              forM_ [(TerminalDataEvent output, outputJSON), (TerminalExitEvent exited, exitedJSON)] $ \(value, wire) -> do
                fromJSON (Object wire) @?= Success value
                toJSON value @?= Object wire
                show value @?= "TerminalNotification <redacted>"
              rejects (Proxy @TerminalNotification) (object ["type" .= String "future"])
              forM_ [Null, Bool False, Number 1, String "event", Array mempty] $ rejects (Proxy @TerminalNotification),
            testCase "control strings and undeclared sibling fields survive notifications" $ do
              let value = TerminalDataEvent (output {terminalDataAdditionalFields = KeyMap.singleton "signal" Null})
              fromJSON (Object (KeyMap.insert "signal" Null outputJSON)) @?= Success value
              toJSON value @?= Object (KeyMap.insert "signal" Null outputJSON)
              fromJSON (Object (KeyMap.insert "signal" (String "") exitedJSON)) @?= Success (exited {terminalExitSignal = ""})
          ]
  where
    definition name = schemaAt ["definitions", name] schema
    records :: (Eq a, Show a, Typeable a, FromJSON a, ToJSON a) => Key -> a -> a -> Object -> Object -> (Object -> a -> a) -> TestTree
    records name = redactedRecordTests name (definition name)

withSession :: Object -> Object
withSession = KeyMap.insert "sessionId" (String "session")

timestampText :: Text
timestampText = "2026-09-05t12:30:00.123456789012345-00:00"

create, minimalCreate :: CreateTerminalParams
minimalCreate = CreateTerminalParams "terminal" Nothing Nothing Nothing Nothing mempty
create = CreateTerminalParams "terminal" (Just (-1.25)) (Just 24.5) (Just "not-accessed") (Just (KeyMap.singleton "FIXTURE" "value")) mempty

input :: WriteTerminalDataParams
input = WriteTerminalDataParams "terminal" "\ESC[31mfixture\r\n" mempty

resize :: ResizeTerminalParams
resize = ResizeTerminalParams "terminal" (-0.5) 24.25 mempty

close :: CloseTerminalParams
close = CloseTerminalParams "terminal" mempty

output :: TerminalData
output = TerminalData "terminal" "\ESC[31mfixture\r\n" mempty

exited :: TerminalExit
exited = TerminalExit "terminal" (-1.25) "signal" mempty

createJSON, minimalCreateJSON, inputJSON, resizeJSON, closeJSON, existsJSON, screenJSON, infoJSON, minimalInfoJSON, outputJSON, exitedJSON :: Object
minimalCreateJSON = KeyMap.singleton "terminalId" (String "terminal")
createJSON = KeyMap.fromList ["terminalId" .= String "terminal", "cols" .= Number (-1.25), "rows" .= Number 24.5, "cwd" .= String "not-accessed", "env" .= object ["FIXTURE" .= String "value"]]
inputJSON = KeyMap.fromList ["terminalId" .= String "terminal", "data" .= String "\ESC[31mfixture\r\n"]
resizeJSON = KeyMap.fromList ["terminalId" .= String "terminal", "cols" .= Number (-0.5), "rows" .= Number 24.25]
closeJSON = KeyMap.singleton "terminalId" (String "terminal")
existsJSON = KeyMap.fromList ["success" .= False, "error" .= String "TerminalIdExists"]
screenJSON = KeyMap.fromList ["serialized" .= String "\ESC[31mtext", "plainText" .= String "plain", "cols" .= Number (-1.25), "rows" .= Number 2.5, "timestamp" .= String timestampText, "cursorHidden" .= False]
minimalInfoJSON = KeyMap.fromList ["id" .= String "terminal", "pid" .= Null, "cols" .= Number 80.5, "rows" .= Number 24.25, "createdAt" .= String timestampText]
infoJSON = KeyMap.insert "state" (Object screenJSON) (KeyMap.insert "pid" (Number (-3.5)) minimalInfoJSON)
outputJSON = KeyMap.fromList ["type" .= String "daemon.terminal_data", "terminalId" .= String "terminal", "data" .= String "\ESC[31mfixture\r\n"]
exitedJSON = KeyMap.fromList ["type" .= String "daemon.terminal_exit", "terminalId" .= String "terminal", "exitCode" .= Number (-1.25), "signal" .= String "signal"]
