{-# LANGUAGE OverloadedStrings #-}

module TimestampSpec (timestampTests) where

import Control.Monad (forM_)
import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON, toJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Factory.Droid.Schema.Primitives (Rfc3339Timestamp, mkRfc3339Timestamp, rfc3339TimestampText)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- RFC 3339 sections 5.6–5.8 and the JSON Schema draft-07 optional date-time
-- tests inform these cases. This scalar also asserts the schema's string type.
timestampTests :: TestTree
timestampTests =
  testGroup
    "RFC 3339 timestamp strings"
    [ testGroup "valid representations" (map validCase validTimestamps),
      testGroup "invalid representations" (map invalidCase invalidTimestamps),
      testCase "arbitrary fractional precision is preserved without conversion" $
        validTimestamp ("2000-01-01T00:00:00." <> Text.replicate 4096 "9" <> "Z"),
      testCase "unknown offset and alternate spellings remain distinguishable" $ do
        let values = map mkRfc3339Timestamp ["2000-01-01T00:00:00Z", "2000-01-01t00:00:00z", "2000-01-01T00:00:00+00:00", "2000-01-01T00:00:00-00:00"]
        forM_ (zip values ["2000-01-01T00:00:00Z", "2000-01-01t00:00:00z", "2000-01-01T00:00:00+00:00", "2000-01-01T00:00:00-00:00"]) $ \(value, text) -> fmap rfc3339TimestampText value @?= Just text
        case values of
          [Just utc, Just lower, Just offset, Just unknown] -> (utc /= lower && utc /= offset && offset /= unknown) @?= True
          _ -> assertFailure "Valid timestamp rejected",
      testCase "leap-second syntax does not assert an occurrence or forecast" $
        validTimestamp "9999-12-31T23:59:60Z",
      testCase "non-string JSON is rejected, not coerced to an epoch" $
        forM_ [Null, Bool False, Bool True, Number 0, Number 1.9, Object mempty, Array mempty] $
          rejects (Proxy @Rfc3339Timestamp)
    ]
  where
    validCase text = testCase (Text.unpack text) (validTimestamp text)
    invalidCase text = testCase (show text) $ do
      mkRfc3339Timestamp text @?= Nothing
      rejects (Proxy @Rfc3339Timestamp) (String text)

validTimestamp :: Text -> IO ()
validTimestamp text = case mkRfc3339Timestamp text of
  Nothing -> assertFailure "Valid RFC 3339 representation rejected"
  Just value -> do
    rfc3339TimestampText value @?= text
    toJSON value @?= String text
    fromJSON (String text) @?= Success value
    eitherDecode (encode value) @?= Right value

validTimestamps :: [Text]
validTimestamps =
  [ "1963-06-19T08:30:06.283185Z",
    "1963-06-19T08:30:06Z",
    "1937-01-01T12:00:27.87+00:20",
    "1990-12-31T15:59:50.123-08:00",
    "1998-12-31T23:59:60Z",
    "1998-12-31T15:59:60.123-08:00",
    "1999-01-01T00:59:60+01:00",
    "2017-01-01T00:29:60+00:30",
    "1963-06-19t08:30:06.283185z",
    "1985-04-12T00:59:59.999999999999999Z",
    "2021-02-28T00:00:00Z",
    "2020-02-29T00:00:00Z",
    "0400-02-29T00:00:00Z",
    "0000-01-01T00:00:00Z",
    "9999-12-31T23:59:59Z",
    "2000-01-01T00:00:00-00:00",
    "2000-01-01T00:00:00+23:59",
    "2000-01-01T00:00:00-23:59"
  ]

invalidTimestamps :: [Text]
invalidTimestamps =
  [ "",
    "1998-12-31T23:59:61Z",
    "1998-12-31T23:58:60Z",
    "1998-12-31T22:59:60Z",
    "1998-12-30T23:59:60Z",
    "1998-12-31T23:59:60+00:01",
    "1990-12-31T24:00:00Z",
    "1990-12-31T15:60:00Z",
    "1990-12-31T10:00:00+10:60",
    "1990-02-31T15:59:59.123-08:00",
    "1990-12-31T15:59:59-24:00",
    "1963-06-19T08:30:06.28123+01:00Z",
    "06/19/1963 08:30:06 PST",
    "2013-350T01:01:01",
    "1963-6-19T08:30:06.283185Z",
    "1963-06-1T08:30:06.283185Z",
    "1963-06-1৪T00:00:00Z",
    "1963-06-11T0৪:00:00Z",
    "+11963-06-19T08:30:06.283185Z",
    "1985-04-12T23:20:50+01",
    "2016-12-31T24:59:60+01:00",
    "1985-04-12T23:20:50Z\n",
    "1985-04-12T23:20Z",
    "1985-04-12T23:20:50Ztail",
    "1985-04-12T23:60:00+00:01",
    "2020-02-30T00:00:00Z",
    "2021-02-29T00:00:00Z",
    "0100-02-29T00:00:00Z",
    "2100-02-29T00:00:00Z",
    "2000-01-01 00:00:00Z",
    "2000-01-01T00:00:00",
    "2000-01-01T00:00:00.Z",
    "2000-01-01T00:00:00,5Z",
    "2000-01-01T00:00:00.5.6Z",
    "2000-01-01T00:00:00+0100",
    "2000-01-01T00:00:00+0:00",
    "2000-01-01T00:00:00.৪Z",
    "2000-01-01T00:00:00\xFEFFZ"
  ]
