{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Validated scalar types shared by wire records.
module Factory.Droid.Schema.Primitives
  ( NonEmptyText,
    mkNonEmptyText,
    nonEmptyTextValue,
    BoundedText,
    mkBoundedText,
    boundedTextValue,
    UUIDText,
    mkUUIDText,
    uuidTextValue,
    NonNegativeNumber,
    mkNonNegativeNumber,
    nonNegativeNumberValue,
    Rfc3339Timestamp,
    mkRfc3339Timestamp,
    rfc3339TimestampText,
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..), withScientific, withText)
import Data.Char (isDigit)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Calendar (addDays, fromGregorianValid, gregorianMonthLength, toGregorian)
import Data.UUID.Types qualified as UUID
import GHC.TypeNats (KnownNat, Nat, natVal)
import Text.Read (readMaybe)

-- | A string with at least one character. Whitespace is significant;
-- no normalization, trimming or identifier-format validation is implied.
newtype NonEmptyText = NonEmptyText Text
  deriving stock (Eq, Ord, Show)

-- | Reject only the empty string.
mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText value
  | Text.null value = Nothing
  | otherwise = Just (NonEmptyText value)

-- | Recover the original text.
nonEmptyTextValue :: NonEmptyText -> Text
nonEmptyTextValue (NonEmptyText value) = value

instance FromJSON NonEmptyText where
  parseJSON = withText "NonEmptyText" $ \value ->
    maybe (fail "Expected nonempty text") pure (mkNonEmptyText value)

instance ToJSON NonEmptyText where
  toJSON = toJSON . nonEmptyTextValue

type role BoundedText nominal

-- | Text containing at most the type-level number of Unicode code points.
-- The bound is nominal so coercion cannot silently change the limit.
newtype BoundedText (limit :: Nat) = BoundedText Text
  deriving stock (Eq, Ord, Show)

-- | Check the code-point limit without truncating or otherwise normalizing.
mkBoundedText :: forall limit. (KnownNat limit) => Text -> Maybe (BoundedText limit)
mkBoundedText value
  | fromIntegral (Text.length value) <= natVal (Proxy @limit) = Just (BoundedText value)
  | otherwise = Nothing

-- | Recover the original bounded text.
boundedTextValue :: BoundedText limit -> Text
boundedTextValue (BoundedText value) = value

instance (KnownNat limit) => FromJSON (BoundedText limit) where
  parseJSON = withText "BoundedText" $ \value ->
    maybe (fail "Text exceeds its maximum length") pure (mkBoundedText @limit value)

instance ToJSON (BoundedText limit) where
  toJSON = toJSON . boundedTextValue

-- | Hyphenated UUID text, validated by uuid-types without changing its
-- spelling. Equality and ordering compare the preserved text, including case.
newtype UUIDText = UUIDText Text
  deriving stock (Eq, Ord, Show)

-- | Validate the UUID text form while retaining the original input.
mkUUIDText :: Text -> Maybe UUIDText
mkUUIDText value = UUIDText value <$ UUID.fromText value

-- | Recover the original UUID spelling.
uuidTextValue :: UUIDText -> Text
uuidTextValue (UUIDText value) = value

instance FromJSON UUIDText where
  parseJSON = withText "UUIDText" $ \value ->
    maybe (fail "Expected hyphenated UUID text") pure (mkUUIDText value)

instance ToJSON UUIDText where
  toJSON = toJSON . uuidTextValue

-- | A nonnegative JSON number, including fractional values. No upper bound
-- or unit is implied; the enclosing field supplies its meaning.
newtype NonNegativeNumber = NonNegativeNumber Scientific
  deriving stock (Eq, Ord, Show)

-- | Reject negative values without rounding or clamping.
mkNonNegativeNumber :: Scientific -> Maybe NonNegativeNumber
mkNonNegativeNumber value
  | value >= 0 = Just (NonNegativeNumber value)
  | otherwise = Nothing

-- | Recover the exact numeric value.
nonNegativeNumberValue :: NonNegativeNumber -> Scientific
nonNegativeNumberValue (NonNegativeNumber value) = value

instance FromJSON NonNegativeNumber where
  parseJSON = withScientific "NonNegativeNumber" $ \value ->
    maybe (fail "Expected a nonnegative number") pure (mkNonNegativeNumber value)

instance ToJSON NonNegativeNumber where
  toJSON = toJSON . nonNegativeNumberValue

-- | A spelling-preserving RFC 3339 timestamp. Validation covers syntax,
-- Gregorian dates and the UTC month-end position of an inserted leap second;
-- it does not consult IERS announcements or predict leap-second occurrences.
-- Equality and ordering are textual, not comparisons of instants.
newtype Rfc3339Timestamp = Rfc3339Timestamp Text
  deriving stock (Eq, Ord, Show)

-- | Validate without coercion or normalization. Fractional precision is
-- unbounded, separators may use either case, and -00:00 remains distinct.
mkRfc3339Timestamp :: Text -> Maybe Rfc3339Timestamp
mkRfc3339Timestamp value = do
  let (prefix, suffix) = Text.splitAt 19 value
  case Text.unpack prefix of
    [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2, t, h1, h2, ':', n1, n2, ':', s1, s2] -> do
      guard (t == 'T' || t == 't')
      year <- digits [y1, y2, y3, y4]
      month <- digits [m1, m2]
      day <- digits [d1, d2]
      date <- fromGregorianValid (fromIntegral year) month day
      hour <- digits [h1, h2]
      minute <- digits [n1, n2]
      second <- digits [s1, s2]
      guard (hour < 24 && minute < 60 && second <= 60)
      zone <- case Text.uncons suffix of
        Just ('.', rest) -> do
          let (fraction, offset) = Text.span isDigit rest
          guard (not (Text.null fraction))
          pure offset
        _ -> pure suffix
      offset <- case Text.unpack zone of
        ['Z'] -> pure 0
        ['z'] -> pure 0
        [sign, oh1, oh2, ':', om1, om2] -> do
          guard (sign == '+' || sign == '-')
          hours <- digits [oh1, oh2]
          minutes <- digits [om1, om2]
          guard (hours < 24 && minutes < 60)
          pure ((if sign == '+' then 1 else -1) * (hours * 60 + minutes))
        _ -> Nothing
      let (dayShift, utcMinute) = (hour * 60 + minute - offset) `divMod` 1440
          (utcYear, utcMonth, utcDay) = toGregorian (addDays (fromIntegral dayShift) date)
      guard (second < 60 || (utcMinute == 1439 && utcDay == gregorianMonthLength utcYear utcMonth))
      pure (Rfc3339Timestamp value)
    _ -> Nothing
  where
    digits :: String -> Maybe Int
    digits chars = do
      guard (all isDigit chars)
      readMaybe chars

-- | Recover the exact original timestamp, including offset and precision.
rfc3339TimestampText :: Rfc3339Timestamp -> Text
rfc3339TimestampText (Rfc3339Timestamp value) = value

instance FromJSON Rfc3339Timestamp where
  parseJSON = withText "Rfc3339Timestamp" $ \value ->
    maybe (fail "Expected an RFC 3339 timestamp") pure (mkRfc3339Timestamp value)

instance ToJSON Rfc3339Timestamp where
  toJSON = toJSON . rfc3339TimestampText
