{-# LANGUAGE OverloadedStrings #-}

module HostSpec (hostTests) where

import Control.Monad (forM_)
import Data.Aeson
  ( Object,
    Result (..),
    Value (..),
    eitherDecode,
    encode,
    fromJSON,
    object,
    toJSON,
    (.=),
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Schema.Host
import Factory.Droid.Schema.Primitives
import SchemaTest (nonNullableRecordTests, rejects, schemaAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

hostTests :: Value -> TestTree
hostTests schema =
  case (mkUUIDText hostUUID, mkUUIDText computerUUID, mkNonEmptyText "organization", mkNonEmptyText "user") of
    (Just hostId, Just computerId, Just organization, Just user) ->
      let registration = ComputerRegistration computerId organization user 12345678901234567890.125 mempty
          config = HostConfig hostId (-12.5) (Just registration) mempty
          legacy = LegacyComputerConfig computerId 3.75 mempty
       in testGroup
            "Host metadata"
            [ nonNullableRecordTests
                "ComputerRegistration"
                (schemaAt ["definitions", "HostConfigSchema", "properties", "computerRegistration"] schema)
                registration
                registration
                registrationJSON
                registrationJSON
                (\extras value -> value {registrationAdditionalFields = extras}),
              nonNullableRecordTests
                "HostConfigSchema"
                (schemaAt ["definitions", "HostConfigSchema"] schema)
                config
                (config {hostConfigRegistration = Nothing})
                hostJSON
                (KeyMap.delete "computerRegistration" hostJSON)
                (\extras value -> value {hostConfigAdditionalFields = extras}),
              nonNullableRecordTests
                "LegacyComputerConfigSchema"
                (schemaAt ["definitions", "LegacyComputerConfigSchema"] schema)
                legacy
                legacy
                legacyJSON
                legacyJSON
                (\extras value -> value {legacyComputerAdditionalFields = extras}),
              testCase "UUID-bearing fields retain their schema formats" $ do
                hostIdSchema <- either assertFailure pure (schemaAt ["definitions", "HostIdSchema"] schema)
                hostIdSchema @?= object ["type" .= String "string", "format" .= String "uuid"]
                forM_ [["definitions", "HostConfigSchema", "properties", "computerRegistration", "properties", "computerId"], ["definitions", "LegacyComputerConfigSchema", "properties", "computerId"]] $ \path -> do
                  definition <- either assertFailure pure (schemaAt path schema)
                  definition @?= hostIdSchema,
              testCase "UUID text preserves case, nil and valid hexadecimal forms" $
                forM_ [hostUUID, computerUUID, "00000000-0000-0000-0000-000000000000", "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF", "123e4567-e89b-72d3-a456-426614174000"] $ \text ->
                  case mkUUIDText text of
                    Nothing -> assertFailure "Valid UUID text rejected"
                    Just value -> do
                      uuidTextValue value @?= text
                      toJSON value @?= String text
                      fromJSON (String text) @?= Success value
                      eitherDecode (encode value) @?= Right value,
              testCase "malformed UUID syntax is rejected rather than normalized" $
                forM_ ["", "not-a-uuid", "123e4567e89b12d3a456426614174000", "123e4567-e89b-12d3-a456-42661417400", "123e4567-e89b-12d3-a456-4266141740000", "123e4567-e89b-12d3-a456-42661417400g", "123e4567_e89b-12d3-a456-426614174000", "{123e4567-e89b-12d3-a456-426614174000}", "urn:uuid:123e4567-e89b-12d3-a456-426614174000", " 123e4567-e89b-12d3-a456-426614174000", "123e4567-e89b-12d3-a456-426614174000\n"] $ \text -> do
                  mkUUIDText text @?= Nothing
                  rejects (Proxy @UUIDText) (String text)
                  rejects (Proxy @HostConfig) (Object (KeyMap.insert "hostId" (String text) hostJSON)),
              testCase "UUID codecs reject non-string JSON" $
                forM_ [Null, Bool True, Number 1, Object mempty, Array mempty] $
                  rejects (Proxy @HostId),
              testCase "schema version is exactly numeric one" $
                forM_ [Number 0, Number 2, String "1", Bool True, Null] $ \value ->
                  rejects (Proxy @HostConfig) (Object (KeyMap.insert "schemaVersion" value hostJSON)),
              testCase "registration validates UUID and nonempty identity fields" $ do
                rejects (Proxy @ComputerRegistration) (Object (KeyMap.insert "computerId" (String "invalid") registrationJSON))
                forM_ ["firestoreOrgId", "userId"] $ \key -> do
                  rejects (Proxy @ComputerRegistration) (Object (KeyMap.insert key (String "") registrationJSON))
                  rejects (Proxy @HostConfig) (Object (KeyMap.insert "computerRegistration" (Object (KeyMap.insert key (String "") registrationJSON)) hostJSON))
                rejects (Proxy @HostConfig) (Object (KeyMap.insert "computerRegistration" (object []) hostJSON)),
              testCase "timestamps are exact JSON numbers, not parsed dates or integers" $ do
                eitherDecode (encode config) @?= Right config
                eitherDecode (encode registration) @?= Right registration
                rejects (Proxy @ComputerRegistration) (Object (KeyMap.insert "registeredAt" (String "1") registrationJSON))
                rejects (Proxy @HostConfig) (Object (KeyMap.insert "createdAt" (Bool False) hostJSON))
            ]
    _ -> testCase "valid host fixture values" (assertFailure "Could not construct host fixture values")

hostUUID, computerUUID :: Text
hostUUID = "123E4567-e89B-12d3-A456-426614174000"
computerUUID = "550e8400-e29b-41d4-a716-446655440000"

registrationJSON, hostJSON, legacyJSON :: Object
registrationJSON = KeyMap.fromList ["computerId" .= String computerUUID, "firestoreOrgId" .= String "organization", "userId" .= String "user", "registeredAt" .= Number 12345678901234567890.125]
hostJSON = KeyMap.fromList ["schemaVersion" .= Number 1, "hostId" .= String hostUUID, "createdAt" .= Number (-12.5), "computerRegistration" .= registrationJSON]
legacyJSON = KeyMap.fromList ["computerId" .= String computerUUID, "registeredAt" .= Number 3.75]
