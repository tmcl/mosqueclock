{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Astronomy.SunRiseTimes as I
import Control.Monad (unless)
import qualified Data.Angle as A
import Data.Time (Day, UTCTime (..))
import qualified Data.Time as Time hiding (Day, UTCTime (..))
import qualified Data.Time.Convenience as TC
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.Time.LocalTime.TimeZone.Series as Time
import DateTestUtils
import Foreign.C.Types
import qualified Geography.Coordinates as I
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Iqama.Athan as I
import System.Exit
import Test.Hspec

foreign import ccall unsafe "sun_position_decl_gd"
  c_sun_position_decl_gd :: CInt -> CInt -> CInt -> CDouble

foreign import ccall unsafe "sun_position_eqt_gd"
  c_sun_position_eqt_gd :: CInt -> CInt -> CInt -> CDouble

foreign import ccall unsafe "get_julian_date"
  c_get_julian_date :: CInt -> CInt -> CInt -> CDouble

-- foreign import ccall unsafe "computeAsr" --todo
--    c_computeAsr :: CDouble -> CDouble -> CDouble -> CDouble

foreign import ccall unsafe "d"
  c_D :: CDouble -> CDouble

foreign import ccall unsafe "fe"
  c_e :: CDouble -> CDouble

foreign import ccall unsafe "deg2rad"
  c_deg2rad :: CDouble -> CDouble

foreign import ccall unsafe "darccot"
  c_darccot :: CDouble -> CDouble

foreign import ccall unsafe "pi"
  c_pi :: CDouble

foreign import ccall unsafe "dsin"
  c_dsin :: CDouble -> CDouble

foreign import ccall unsafe "dtan"
  c_dtan :: CDouble -> CDouble

foreign import ccall unsafe "fg"
  c_g :: CDouble -> CDouble

foreign import ccall unsafe "fq"
  c_q :: CDouble -> CDouble

foreign import ccall unsafe "L"
  c_L :: CDouble -> CDouble -> CDouble

timezone :: Bool -> PropertyT IO Time.TimeZone
timezone isSummer = do
  offset <- forAll $ Gen.integral $ Range.constantFrom 0 (-12 * 60) (14 * 60)
  return $ Time.TimeZone offset isSummer ""

timezoneSeries :: UTCTime -> UTCTime -> PropertyT IO (Time.DiffTime, Time.TimeZoneSeries)
timezoneSeries winterStart summerStart = do
  initialTz <- timezone False
  winterTz <- timezone False
  summerTz <- timezone True
  return
    ( fromIntegral $ 60 * Time.timeZoneMinutes summerTz,
      Time.TimeZoneSeries initialTz [(winterStart, winterTz), (summerStart, summerTz)]
    )

utcTime :: Day -> Day -> PropertyT IO UTCTime
utcTime from end = do
  diffT <- forAll $ Gen.integral $ Range.constant 0 (24 * 60 * 60)
  day <- eachDayBetween from from end
  return $ UTCTime day (fromInteger diffT)

-- summerWinterTimeCommon :: UTCTime
-- -> UTCTime
-- -> PropertyT IO (Time.DiffTime, Time.DiffTime, Time.TimeOfDay, (Time.TimeOfDay, Time.TimeOfDay))
summerWinterTimeCommon ::
  UTCTime ->
  UTCTime ->
  PropertyT
    IO
    ( TimeZoneSeries,
      TC.DiffTime,
      TC.DiffTime,
      TC.TimeOfDay,
      (TC.TimeOfDay, TC.TimeOfDay),
      ZoneSeriesTime
    )
summerWinterTimeCommon summerStart utcNow = do
  (summerOffset, tzz) <- timezoneSeries (flip UTCTime 1 $ Time.fromGregorian 1998 1 1) summerStart
  let winterTz = winterTimezone tzz
  let winterOffset :: Time.DiffTime = fromIntegral $ 60 * Time.timeZoneMinutes winterTz
  let now = Time.ZoneSeriesTime utcNow tzz
  let tod = TC.zoneSeriesTimeOfDay now
  return (tzz, winterOffset, summerOffset, tod, TC.summerAndWinter now, now)

winterTimezone :: TimeZoneSeries -> TC.TimeZone
winterTimezone (Time.TimeZoneSeries _ ((_, winterT): _)) = winterT
winterTimezone (Time.TimeZoneSeries winterTz []) = winterTz

newtype BetterShowTZZ = BetterShowTZZ Time.TimeZoneSeries

instance Show BetterShowTZZ where
  show (BetterShowTZZ (Time.TimeZoneSeries def tzz)) = "TZZ " <> show def <> " " <> show tzz

newtype BetterShowTZ = BetterShowTZ Time.TimeZone

instance Show BetterShowTZ where
  show (BetterShowTZ (Time.TimeZone offset summerOnly acronym)) = "TZ " <> show offset <> " " <> show summerOnly <> " " <> show acronym

prop_winterTime :: Property
prop_winterTime = property $ do
  summerStart <- utcTime (Time.fromGregorian 2000 1 2) (Time.fromGregorian 2200 1 1)
  utcNow <- utcTime (Time.fromGregorian 2000 1 1) (utctDay summerStart)
  (_, winterOffset, summerOffset, tod, (winter, summer), _) <- summerWinterTimeCommon summerStart utcNow
  let offsetDifference = summerOffset - winterOffset
  let trueSummer = Time.timeToTimeOfDay $ fromInteger (floor (86400 + offsetDifference + Time.timeOfDayToTime tod) `mod` 86400)
  tod === winter
  trueSummer === summer

-- i don't know what this test is trying to test, but the last time i looked at apparently i was stumped
-- prop_summerTime :: Property
-- prop_summerTime = property $ do
--    summerStart <- utcTime (Time.fromGregorian 2000 1 2) (Time.fromGregorian 2200 1 1)
--    UTCTime day time <- utcTime (utctDay summerStart) (Time.fromGregorian 2200 1 1)
--    let utcNow = UTCTime day (time+1)
--    (tzz, winterOffset, summerOffset, tod, (winter, summer), now) <- summerWinterTimeCommon summerStart utcNow
--    let offsetDifference = winterOffset - summerOffset
--    let trueWinter = Time.timeToTimeOfDay $ fromInteger (floor (86400 + offsetDifference + Time.timeOfDayToTime tod) `mod` 86400)
--    liftIO $ print (BetterShowTZZ tzz)
--    liftIO $ print (now, Time.zoneSeriesTimeToUTC now)
--    liftIO $ print (winterOffset, summerOffset)
--    liftIO $ print (winter, summer)
--    liftIO $ print (trueWinter, tod)
--    trueWinter === winter
--    tod === summer

prop_getJulianDate :: Property
prop_getJulianDate = property $ do
  day <- eachDay

  let (yearC, monthC, dateC) = toCGregorian day

  realToFrac (c_get_julian_date yearC monthC dateC) === TC.toJulianDay day

prop_D :: Property
prop_D = property $ do
  day <- eachDay

  let jd = realToFrac $ TC.toJulianDay day

  realToFrac (c_D jd) === I.small_d' day

-- This test involves undefined...
--prop_asr :: Property
--prop_asr = property $ do
--   size <- forAll $ Gen.element [1, 2]
--   lat <- forAll $ Gen.double $ Range.constant (-89) 89
--   decl <- forAll $ Gen.double $ Range.constant (-400) 400
--
--   realToFrac (c_computeAsr (fromIntegral size) (realToFrac decl) (realToFrac lat))
--      === I.calcAsr (A.Degrees decl) (I.Location (I.Latitude $ A.Degrees lat) undefined) size

prop_sine :: Property
prop_sine = property $ do
  x <- forAll $ Gen.double $ Range.constant (-400) 400

  realToFrac (c_dsin $ realToFrac x) === A.sine (A.Degrees x)

prop_tangent :: Property
prop_tangent = property $ do
  x <- forAll $ Gen.double $ Range.constant (-400) 400

  realToFrac (c_dtan $ realToFrac x) === A.tangent (A.Degrees x)

prop_darccot :: Property
prop_darccot = property $ do
  x <- forAll $ Gen.double $ Range.constant (-400) 400

  let arccotangent = A.arccotangent :: (Double -> A.Degrees Double)

  realToFrac (c_darccot $ realToFrac x) === arccotangent x

prop_g :: Property
prop_g = property $ do
  x <- forAll $ Gen.double $ Range.constant (-400) 400

  realToFrac (c_g $ realToFrac x) === I.g' x

prop_e :: Property
prop_e = property $ do
  x <- forAll $ Gen.double $ Range.constant (-400) 400

  realToFrac (c_e $ realToFrac x) === I.e' x

prop_q :: Property
prop_q = property $ do
  x <- forAll $ Gen.double $ Range.constant (-400) 400

  realToFrac (c_q $ realToFrac x) === I.q' x

prop_pi :: Property
prop_pi = property $ do
  pi === c_pi
  x' <- forAll $ Gen.double $ Range.constant (-400) 400

  let x = realToFrac x'

  c_deg2rad x === deg2rad x

deg2rad :: CDouble -> CDouble
deg2rad deg = rad
  where
    A.Radians rad = A.radians $ A.Degrees deg

prop_L :: Property
prop_L = property $ do
  q <- forAll $ Gen.double $ Range.constant (-400) 400
  g <- forAll $ Gen.double $ Range.constant (-400) 400

  realToFrac (c_L (realToFrac q) (realToFrac g))
    === unDegrees (I.lL' (realToFrac q) (realToFrac g))

unDegrees :: A.Degrees a -> a
unDegrees (A.Degrees a) = a

prop_eqT :: Property
prop_eqT = property $ do
  day <- eachDay

  let (yearC, monthC, dateC) = toCGregorian day

  realToFrac (c_sun_position_eqt_gd yearC monthC dateC) === I.equationOfTime (I.sunPosition day)

prop_decl :: Property
prop_decl = property $ do
  day <- eachDay
  let (yearC, monthC, dateC) = toCGregorian day
  realToFrac (c_sun_position_decl_gd yearC monthC dateC) === I.declinationAngle (I.sunPosition day)

toCGregorian :: Day -> (CInt, CInt, CInt)
toCGregorian day = (yearC, monthC, dateC)
  where
    (year, month, date) = Time.toGregorian day
    yearC = fromIntegral year
    monthC = fromIntegral month
    dateC = fromIntegral date

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Test.MatchesCEquivs"
      [ ("prop_decl", prop_decl),
        ("prop_eqT", prop_eqT),
        ("prop_L", prop_L),
        ("prop_D", prop_D),
        ("prop_q", prop_q),
        ("prop_g", prop_g),
        ("prop_e", prop_e),
        ("prop_darccot", prop_darccot),
        ("prop_tangent", prop_tangent),
        ("prop_sine", prop_sine),
        ("prop_pi", prop_pi),
        ("prop_getJulianDate", prop_getJulianDate),
        ("prop_winterTime", prop_winterTime)
      ]

test7 :: AthanTest
test7 =
  AthanTest
    { location = melbourne,
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 6 00 32) aest,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 14 59 11) aest
    }
  where
    testDay = Time.fromGregorian 2019 7 13

test2 :: AthanTest
test2 =
  AthanTest
    { location = melbourne,
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 5 10 0) aedt,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 17 20 1) aedt
    }
  where
    testDay = Time.fromGregorian 2019 2 13

test6 :: AthanTest
test6 =
  AthanTest
    { location = melbourne,
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 5 11 1) aedt,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 17 19 1) aedt
    }
  where
    testDay = Time.fromGregorian 2019 2 14

melbourne :: I.Location
melbourne = I.Location (I.Latitude $ -37.826) (I.Longitude 145.034)

aedt :: Time.TimeZone
aedt = Time.TimeZone (11 * 60) True "AEDT"

aest :: Time.TimeZone
aest = Time.TimeZone (10 * 60) False "AEST"

main :: IO ()
main = do
  hspec $
    describe "athan" $ do
      testFajr test1
      testFajr test2
      testFajr test3
      testFajr test4
      testFajr test5
      testFajr test6
      testFajr test7
  -- fajr melbourne day1 `shouldBe` mkZonedTime day1 fajrTime1 aedt
  -- fajr melbourne day2 `shouldBe` mkZonedTime day2 fajrTime2 aedt
  result <- tests
  -- sth <-getCurrentTimeZoneSeries
  -- now <- Time.getCurrentTime
  -- print $ BetterShowTZ $ TC.winterTimeZone (ZoneSeriesTime now sth)
  -- print $ BetterShowTZ $ TC.summerTimeZone (ZoneSeriesTime now sth)
  unless result System.Exit.exitFailure

-- getCurrentTimeZoneSeries :: IO TimeZoneSeries
-- getCurrentTimeZoneSeries = do
--    let zoneFilePath = "/usr/share/zoneinfo/Australia/Melbourne"
--    getTimeZoneSeriesFromOlsonFile zoneFilePath

testFajr :: AthanTest -> SpecWith ()
testFajr t = do
  let times = I.computeTimes I.Earlier I.mwl
      fajr a b = EqualByUTCMinute . I.fajr $ times a b
      asr1 a b = EqualByUTCMinute . I.asr $ times a b

  it (mkMessage "fajr" t) $
    fajr (location t) (athanTestDay t) `shouldBe` expectedFajr t
  it (mkMessage "asr1" t) $
    asr1 (location t) (athanTestDay t) `shouldBe` expectedAsr1 t

mkMessage :: String -> AthanTest -> String
mkMessage n t = "calculates " ++ n ++ " on " ++ (show . athanTestDay) t ++ " in " ++ (show . location $ t)

newtype EqualByUTCMinute = EqualByUTCMinute Time.ZonedTime

instance Show EqualByUTCMinute where
  show (EqualByUTCMinute a) = show a ++ "\n" ++ (show . Time.zonedTimeToUTC) a

instance Eq EqualByUTCMinute where
  (EqualByUTCMinute a) == (EqualByUTCMinute b) =
    (clgMinutes . utc) a == (clgMinutes . utc) b
      || (TC.ceilMinutes . utc) a == (TC.flrMinutes . utc) b
    where
      utc = Time.zonedTimeToUTC

clgMinutes :: UTCTime -> UTCTime
clgMinutes (UTCTime d t) = UTCTime d (fromIntegral $ clg t)

clg :: (RealFrac a) => a -> Integer
clg i = 60 * ceiling (i / 60)

mkZonedTime :: Day -> Time.TimeOfDay -> Time.TimeZone -> EqualByUTCMinute
mkZonedTime d t tz = EqualByUTCMinute $ Time.ZonedTime (Time.LocalTime d t) tz

data AthanTest = AthanTest
  { location :: I.Location,
    athanTestDay :: Day,
    expectedFajr :: EqualByUTCMinute,
    expectedAsr1 :: EqualByUTCMinute
  }

test1 :: AthanTest
test1 =
  AthanTest
    { location = I.Location (I.Latitude 0) (I.Longitude 0),
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 5 0 1) Time.utc,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 15 32 1) Time.utc
    }
  where
    testDay = Time.fromGregorian 2018 2 16

test3 :: AthanTest
test3 =
  AthanTest
    { location = I.Location (I.Latitude 0) (I.Longitude 0),
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 4 48 1) Time.utc,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 15 31 0) Time.utc
    }
  where
    testDay = Time.fromGregorian 2018 7 16

test4 :: AthanTest
test4 =
  AthanTest
    { location = I.Location (I.Latitude 10) (I.Longitude 0),
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 5 2 0) Time.utc,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 15 22 1) Time.utc
    }
  where
    testDay = Time.fromGregorian 2018 1 1

test5 :: AthanTest
test5 =
  AthanTest
    { location = I.Location (I.Latitude 0) (I.Longitude 10),
      athanTestDay = testDay,
      expectedFajr = mkZonedTime testDay (Time.TimeOfDay 4 5 0) Time.utc,
      expectedAsr1 = mkZonedTime testDay (Time.TimeOfDay 14 49 0) Time.utc
    }
  where
    testDay = Time.fromGregorian 2018 1 1
