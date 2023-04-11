{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Astronomy.SunRiseTimes
import Control.Concurrent.STM (atomically)
import Control.Exception
import Control.Monad
import qualified Data.Angle as A
import Data.Binary (Binary, get)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bool
import Data.ByteString as SBS
import Data.ByteString.Char8 as S8
import Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 (toString)
import Data.Function
import Data.Int
import Data.List
import Data.Time
import qualified Data.Time.Convenience as TC
import Data.Time.Format.ISO8601
import DateTestUtils
import Foreign.C.Types
import Geography.Coordinates
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Iqama.Athan
import Main.Utf8
import Mathematics.Fdlibm
import Numeric
import System.Environment
import System.Exit
import System.IO
import System.Process.Typed

main :: IO ()
main = withUtf8 $ do
  [kotlin] <- System.Environment.getArgs

  System.IO.putStrLn "I will begin the tests now"

  let numTests = 400

  result <- tests numTests kotlin

  unless result System.Exit.exitFailure

tests :: TestLimit -> FilePath -> IO Bool
tests n kotlin =
  checkParallel $
    Group
      "Test.Kotlin"
      [ ("prop_athanTimes", withTests n $ prop_athanTimes kotlin),
        ("prop_small_d", withTests n $ prop_small_d kotlin),
        ("prop_timezone", withTests n $ prop_timezone kotlin),
        ("prop_SunPosition", withTests n $ prop_SunPosition kotlin),
        ("prop_sine_radian", withTests n $ prop_sine_radian kotlin),
        ("prop_sine_degrees", withTests n $ prop_sine_degrees kotlin),
        ("prop_julianDate", withTests n $ prop_julianDate kotlin)
      ]

data Params = Params AsrRule Method Location Day
  deriving (Show)

instance JavaEnc Params where
  encode (Params asrRule method location day) =
    encode asrRule <> encode method <> encode location <> encode day

instance JavaEnc Location where
  encode (Location latitude lon) = encode latitude <> encode lon

instance JavaEnc Latitude where
  encode (Latitude it) = encode it

instance JavaEnc Longitude where
  encode (Longitude it) = encode it

instance JavaEnc Method where
  encode (Method fajrAngle thuhrSetting maghribSetting ishaSetting) =
    encode fajrAngle <> encode thuhrSetting <> encode maghribSetting
      <> encode ishaSetting

instance JavaEnc t => JavaEnc (A.Degrees t) where
  encode (A.Degrees x) = encode x

instance JavaEnc ThuhrSetting where
  encode Instant = encode @SBS.ByteString "Instant"
  encode LeaveZenith = encode @SBS.ByteString "Leave Zenith"

instance JavaEnc t => JavaEnc [t] where
  encode list =
    runPut (putWord8 len)
      <> mconcat (encode <$> Data.List.take (fromIntegral len) list)
    where
      len = tobound (Data.List.length list)

instance JavaEnc EventSelector where
  encode (SpecificAngle angle) = "\0" <> encode angle
  encode (FixedOffset offset) = "\1" <> encode (floor offset :: Int64)

instance JavaEnc Int64 where
  encode = runPut . putInt64be

tobound :: forall a b. (Ord a, Num a, Num b, Integral a, Integral b, Bounded a, Bounded b) => a -> b
tobound num =
  if num > fromIntegral (maxBound @b)
    then maxBound
    else fromIntegral num

instance JavaEnc AsrRule where
  encode Earlier = encode @SBS.ByteString "Earlier"
  encode Later = encode @SBS.ByteString "Later"

instance JavaEnc SBS.ByteString where
  encode bs = runPut (putWord8 len) <> partof bs
    where
      len = tobound (SBS.length bs)
      partof = LBS.fromStrict . SBS.take (fromIntegral len)

getTime :: Get UTCTime
getTime = do
  bytestr_common <- getByteString 16
  bytestr_divide <- getByteString 1
  rest <-
    if bytestr_divide == "Z"
      then return ":00Z"
      else do
        ending <- getByteString 3
        return $ ":" <> ending

  let bytestr = bytestr_common <> rest

  let r = iso8601ParseM . toString $ bytestr
  maybe
    (fail $ "cannot parse " <> show bytestr <> " as time")
    return
    r

instance JavaDec (Athans UTCTime) where
  decode = runGet $ do
    fajr <- getTime
    sunrise <- getTime
    thuhr <- getTime
    asr <- getTime
    maghrib <- getTime
    isha <- getTime
    return Athans {..}

genAsrRules :: Gen AsrRule
genAsrRules = bool Earlier Later <$> Gen.bool

genIshaEventSelector :: Gen EventSelector
genIshaEventSelector = do
  isSpecificAngle <- Gen.bool
  if isSpecificAngle
    then SpecificAngle . A.Degrees <$> Gen.realFloat (Range.linearFrac 0 20)
    else FixedOffset . (fromIntegral @Integer) <$> Gen.integral (Range.linear 0 7200)

genMethod :: Gen Method
genMethod = do
  fajrAngle <- A.Degrees <$> Gen.realFloat (Range.linearFrac 10 20)
  thuhrSetting <- bool Instant LeaveZenith <$> Gen.bool
  -- we use different Gen EventSelectors because the plausible values
  -- are quite distinct. for maghrib, we permit
  maghribDelay <- Gen.list (Range.linear 0 1) (FixedOffset . (fromIntegral @Integer) <$> Gen.integral (Range.linear 0 1800))
  let maghribSetting = SpecificAngle (5 / 6) : maghribDelay
  ishaSetting <- genIshaEventSelector
  return Method {..}

genLocation :: Gen Location
genLocation = do
  latitude <-
    Latitude . A.Degrees
      <$> Gen.realFloat (Range.linearFracFrom 0 (-90) 90)
  longitude <-
    Longitude . A.Degrees
      <$> Gen.realFloat (Range.linearFracFrom 0 (-180) 180)

  return $ Location latitude longitude

prop_small_d :: FilePath -> Property
prop_small_d kotlin = property $ do
  day <- eachDay
  evalIO $ checkValue kotlin day f "small_d"
  where
    f day =
      let small_d = small_d' day
          q = q' small_d
          g = g' small_d
          lL = lL' q g
          e = e' small_d
          cosine_e = A.cosine e
          sine_L = A.sine lL
          cos_e_times_sin_L = cosine_e * sine_L
          cosine_L = A.cosine lL
          rRA' = A.arctangent2 cos_e_times_sin_L cosine_L
       in ( small_d :: Double,
            q & unDegrees :: Double,
            toHours q :: Double,
            e & unDegrees :: Double,
            g & unDegrees :: Double,
            lL & unDegrees :: Double,
            rRA' & unDegrees,
            cosine_e,
            sine_L,
            cos_e_times_sin_L,
            cosine_L
          )

unDegrees :: A.Degrees a -> a
unDegrees (A.Degrees a) = a

prop_SunPosition :: FilePath -> Property
prop_SunPosition kotlin = property $ do
  day <- eachDay
  evalIO $ checkValue kotlin day f "sun position"
  where
    f day = let spos = sunPosition day in (spos, noon spos, extraNoon spos, extraNoonSecs spos)

prop_athanTimes :: FilePath -> Property
prop_athanTimes kotlin = property $ do
  asrRule <- forAll genAsrRules
  method <- forAll genMethod
  location <- forAll genLocation
  day <- eachDay
  let params = Params asrRule method location day
  evalIO $ checkValue kotlin params computeTimes' "compute times"
  where
    computeTimes' (Params asrRule method location day) =
      zonedTimeToUTC <$> computeTimes asrRule method location day

prop_timezone :: FilePath -> Property
prop_timezone kotlin = property $ do
  longitude <-
    forAll
      ( Longitude . A.Degrees
          <$> Gen.realFloat (Range.linearFracFrom 0 (-180) 180)
      )

  evalIO $ checkValue kotlin longitude (fromIntegral @Int @Int32 . (* 60) . timeZoneMinutes . naturalTimeZone) "naturalTimeZone"

prop_julianDate :: FilePath -> Property
prop_julianDate kotlin = property $ do
  xs <- eachDay
  evalIO $ checkValue kotlin xs TC.toJulianDay "julian date"

prop_sine_degrees :: FilePath -> Property
prop_sine_degrees kotlin = property $ do
  xs <- forAll $ Gen.realFloat $ Range.constant (-400) 400
  evalIO $ checkValue kotlin xs (A.sine . A.Degrees @Double) "sin"

prop_sine_radian :: FilePath -> Property
prop_sine_radian kotlin = property $ do
  xs <- forAll $ Gen.realFloat $ Range.constant (-400) 400
  evalIO $ checkValue kotlin xs Mathematics.Fdlibm.sin "rsin"

class JavaEnc t where
  encode :: t -> LBS.ByteString

class JavaDec t where
  decode :: LBS.ByteString -> t

instance JavaEnc CDouble where
  encode (CDouble d) = runPut . putDoublebe $ d

instance JavaEnc Double where
  encode = runPut . putDoublebe

instance JavaEnc TC.Day where
  encode day = runPut (putInt32be y >> putInt32be m >> putInt32be d)
    where
      (y', m', d') = TC.toGregorian day
      y = fromIntegral y'
      m = fromIntegral m'
      d = fromIntegral d'

instance JavaDec TC.Day where
  decode = runGet $ do
    y <- fromIntegral <$> getInt32be
    m <- fromIntegral <$> getInt32be
    d <- fromIntegral <$> getInt32be
    return $ TC.fromGregorian y m d

getSunPosition :: Get SunPosition
getSunPosition = do
  equationOfTime <- getDoublebe
  declinationAngle <- A.Degrees <$> getDoublebe
  return SunPosition {..}

instance JavaDec SunPosition where
  decode = runGet getSunPosition

instance JavaDec Double where
  decode = runGet getDoublebe

instance JavaDec (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double) where
  decode =
    runGet
      ( (,,,,,,,,,,)
          <$> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
          <*> getDoublebe
      )

instance {-# OVERLAPPABLE #-} Binary a => JavaDec a where
  decode = runGet get

instance JavaDec (SunPosition, NominalDiffTime, Double, Int64) where
  decode = runGet $ do
    spos <- getSunPosition
    nominalDiffTime <- fromIntegral <$> getInt64be
    extra_noon <- getDoublebe
    extra_noon_secs <- getInt64be
    return (spos, nominalDiffTime, extra_noon, extra_noon_secs)

checkValue ::
  forall a b.
  (Show a, Show b, JavaEnc a, JavaDec b, Eq b) =>
  FilePath ->
  a ->
  (a -> b) ->
  String ->
  IO ()
checkValue kotlin val correctFunction javaFunction = do
  -- handles <- createHandles kotlin "sin"
  -- let HandlePairs{..}  = handlesSine handles
  let encoded = encode val
  str <-
    withProcessWait_
      ( setStdin (byteStringInput encoded) $
          setStdout byteStringOutput $
            proc "bash" [kotlin, javaFunction]
      )
      $ atomically . getStdout
  let correctValue = correctFunction val
  -- let correctEncoded = encode @b correctValue
  -- let outLength = LBS.length correctEncoded
  let decodedVal = decode str
  unless (decodedVal == correctValue) $ do
    let str_enc = bytestringToHex $ LBS.toStrict str
    -- let correctEncoded_enc = bytestringToHex $ LBS.toStrict correctEncoded
    let orig_enc = bytestringToHex $ LBS.toStrict encoded
    throwIO (MismatchException (show val) (show correctValue) (show decodedVal) (show str_enc) (show orig_enc))

data MismatchException = MismatchException String String String String String

instance Show MismatchException where
  show (MismatchException val correct decoded toDecode encoded) =
    Data.List.unlines
      [ Data.List.unwords ["MismatchException", val],
        correct,
        decoded,
        toDecode,
        encoded
      ]

instance Exception MismatchException

bytestringToHex :: SBS.ByteString -> SBS.ByteString
bytestringToHex =
  SBS.concatMap $ \c -> S8.pack $ pad $ Numeric.showHex c []
  where
    pad [x] = ['0', x]
    pad s = s
