{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DateTestUtils (eachDay, timeOfDay, eachDayBetween) where

import Data.Time (Day, UTCTime (..))
import qualified Data.Time as Time hiding (Day, UTCTime (..))
import qualified Data.Time.Convenience as TC
import qualified Data.Time.LocalTime.TimeZone.Series as Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

eachDayBetween :: Day -> Day -> Day -> PropertyT IO Day
eachDayBetween epoch start end = do
  let epoch' = Time.toModifiedJulianDay epoch
      firstDay = Time.toModifiedJulianDay start
      lastDay = Time.toModifiedJulianDay end
      range = Range.constantFrom epoch' firstDay lastDay

  forAll $ Time.ModifiedJulianDay <$> Gen.integral range

eachDay :: PropertyT IO Day
eachDay =
  eachDayBetween
    (Time.fromGregorian 2000 1 1)
    (Time.fromGregorian 1500 1 1)
    (Time.fromGregorian 2500 1 1)

newtype BetterShowTZZ = BetterShowTZZ Time.TimeZoneSeries

instance Show BetterShowTZZ where
  show (BetterShowTZZ (Time.TimeZoneSeries def tzz)) = "TZZ " <> show def <> " " <> show tzz

newtype BetterShowTZ = BetterShowTZ Time.TimeZone

instance Show BetterShowTZ where
  show (BetterShowTZ (Time.TimeZone offset summerOnly acronym)) = "TZ " <> show offset <> " " <> show summerOnly <> " " <> show acronym

newtype EqualByUTCMinute = EqualByUTCMinute Time.ZonedTime

instance Show EqualByUTCMinute where
  show (EqualByUTCMinute a) = show a ++ "\n" ++ (show . Time.zonedTimeToUTC) a

instance Eq EqualByUTCMinute where
  (EqualByUTCMinute a) == (EqualByUTCMinute b) =
    (clgMinutes . utc) a == (clgMinutes . utc) b
      || (TC.flrMinutes . utc) a == (TC.flrMinutes . utc) b
    where
      utc = Time.zonedTimeToUTC

clgMinutes :: UTCTime -> UTCTime
clgMinutes (UTCTime d t) = UTCTime d (fromIntegral $ clg t)

clg :: (RealFrac a) => a -> Integer
clg i = 60 * ceiling (i / 60)

timeOfDay :: PropertyT IO Time.TimeOfDay
timeOfDay = do
  h <- forAll $ Gen.integral $ Range.constant 0 23
  m <- forAll $ Gen.integral $ Range.constant 0 59
  s <- forAll $ Gen.integral $ Range.constant 0 59
  return $ Time.TimeOfDay h m (fromInteger s)
