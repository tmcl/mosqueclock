{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Iqama.Athan
  ( Athans (..),
    computeTimes,
    AthanTimes,
    ours,
    mwl,
    karachi,
    jafari,
    AthanComputer,
    AsrRule (..),
    Method (..),
    AthanRec (..),
    AthanUpdater (..),
    ThuhrSetting (..),
    EventSelector (..),
    calcAsr,
    updates,
    zipA,
  )
where

import Astronomy.SunRiseTimes
import Data.Aeson
import Data.Angle hiding (Degrees)
import Data.Fixed
import Data.List (foldl')
import Data.Maybe
import qualified Data.Time as Time
import qualified Data.Time.Convenience as Time (fromHours, fromMinutes)
import GHC.Generics
import Geography.Coordinates

data EventSelector = SpecificAngle Degrees | FixedOffset Time.NominalDiffTime
  deriving (Generic, Show, Eq)

instance ToJSON EventSelector where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON EventSelector

data ThuhrSetting = Instant | LeaveZenith
  deriving (Generic, Show, Eq)

instance ToJSON ThuhrSetting where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ThuhrSetting

toDelay :: ThuhrSetting -> Time.NominalDiffTime
toDelay Instant = 0
toDelay LeaveZenith = 65 -- this could be refined to take into account the day

data Method = Method
  { fajrAngle :: Degrees,
    thuhrSetting :: ThuhrSetting,
    maghribSetting :: [EventSelector], -- after sunset
    ishaSetting :: EventSelector -- after maghrib
  }
  deriving (Generic, Show, Eq)

instance ToJSON Method where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Method

jafari :: Method
jafari =
  Method
    { fajrAngle = 16,
      thuhrSetting = Instant,
      maghribSetting = [SpecificAngle 4],
      ishaSetting = SpecificAngle 14
    }

karachi :: Method
karachi =
  Method
    { fajrAngle = 18,
      thuhrSetting = Instant,
      maghribSetting = [SpecificAngle sunSizeAngle],
      ishaSetting = SpecificAngle 18
    }

ours :: Method
ours =
  Method
    { fajrAngle = 18,
      thuhrSetting = Instant,
      maghribSetting = [SpecificAngle sunSizeAngle, FixedOffset 30],
      ishaSetting = SpecificAngle 17
    }

mwl :: Method
mwl =
  Method
    { fajrAngle = 18,
      thuhrSetting = Instant,
      maghribSetting = [SpecificAngle sunSizeAngle, FixedOffset 30],
      ishaSetting = SpecificAngle 17
    }

data Athans t = Athans
  { fajr :: t,
    sunrise :: t,
    -- zawaal :: t,
    thuhr :: t,
    asr :: t,
    -- asr1 :: t,
    -- asr2 :: t,
    -- sunset :: t,
    maghrib :: t,
    isha :: t
    -- qiyam :: t,
  }
  deriving (Eq, Generic, Show)

-- unzipA :: Athans (a, b) -> (Athans a, Athans b)
-- unzipA ath = (fst <$> ath, snd <$> ath)

zipA :: Athans a -> Athans b -> Athans (a, b)
zipA a b =
  Athans
    { fajr = (fajr a, fajr b),
      sunrise = (sunrise a, sunrise b),
      thuhr = (thuhr a, thuhr b),
      asr = (asr a, asr b),
      maghrib = (maghrib a, maghrib b),
      isha = (isha a, isha b)
    }

newtype AthanRec = AthanRec {unathan :: forall a. Athans a -> a}

newtype AthanUpdater = AthanUpdater {unathanUpdater :: forall a. Athans a -> a -> Athans a}

updates :: Athans AthanUpdater
updates =
  Athans
    { fajr = AthanUpdater $ \as a -> as {fajr = a},
      sunrise = AthanUpdater $ \as a -> as {sunrise = a},
      thuhr = AthanUpdater $ \as a -> as {thuhr = a},
      asr = AthanUpdater $ \as a -> as {asr = a},
      maghrib = AthanUpdater $ \as a -> as {maghrib = a},
      isha = AthanUpdater $ \as a -> as {isha = a}
    }

instance ToJSON t => ToJSON (Athans t) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON t => FromJSON (Athans t)

instance Functor Athans where
  fmap g a =
    Athans
      { fajr = g $ fajr a,
        sunrise = g $ sunrise a,
        thuhr = g $ thuhr a,
        asr = g $ asr a,
        maghrib = g $ maghrib a,
        isha = g $ isha a
      }

instance Applicative Athans where
  pure a =
    Athans
      { fajr = a,
        sunrise = a,
        thuhr = a,
        asr = a,
        maghrib = a,
        isha = a
      }
  g <*> a =
    Athans
      { fajr = fajr g $ fajr a,
        sunrise = sunrise g $ sunrise a,
        thuhr = thuhr g $ thuhr a,
        asr = asr g $ asr a,
        maghrib = maghrib g $ maghrib a,
        isha = isha g $ isha a
      }

instance Foldable Athans where
  foldr g z (Athans f s th a m i) = g i $ g m $ g a $ g th $ g s $ g f z

instance Traversable Athans where
  traverse g (Athans f s th a m i) =
    Athans <$> g f <*> g s <*> g th <*> g a <*> g m <*> g i

type AthanTimes = Athans Time.ZonedTime

data AsrRule = Earlier | Later
  deriving (Show, Read, Generic)

instance ToJSON AsrRule where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AsrRule

type AthanComputer = Time.Day -> AthanTimes

roundTime :: (Pico -> Int -> Int) -> Time.ZonedTime -> Time.ZonedTime
roundTime rounder time = Time.ZonedTime roundedTime tz
  where
    Time.ZonedTime unroundedTime tz = time
    Time.LocalTime day unroundedTod = unroundedTime
    Time.TimeOfDay h m s = unroundedTod
    m' = rounder s m
    roundedTime = Time.LocalTime day (Time.TimeOfDay h m' 0)

computeTimes :: AsrRule -> Method -> Location -> Time.Day -> AthanTimes
computeTimes asrRule method loc day =
  Athans
    { fajr = roundTime up $ zone fajrDefaulted,
      sunrise = roundTime down $ zone $ fromMaybe (noon' - Time.fromHours 6) (beforeNoon sunSizeAngle),
      -- zawaal = zone noon',
      thuhr = roundTime up $ zone $ noon' + (toDelay . thuhrSetting) method,
      asr = roundTime up $ zone $ case asrRule of
        Earlier -> fromMaybe (noon' + Time.fromHours 3) asr1'
        Later -> fromMaybe (noon' + Time.fromHours 4) asr2',
      -- asr1 = asr1',
      -- asr2 = asr2',
      -- sunset = sunset',
      maghrib = roundTime up $ zone maghribDefaulted,
      isha =
        roundTime up $
          zone $
            fromMaybe
              (fajrDefaulted + Time.fromMinutes (24 * 60 - 10)) -- 24 hrs less 10 min
              (delay (Just maghribDefaulted) (ishaSetting method))
    }
  where
    up 0 m = m
    up _ m = m + 1
    down _ m = m
    fajrDefaulted = fromMaybe (noon' - Time.fromHours 12) (beforeNoon $ fajrAngle method)
    maghribDefaulted = fromMaybe (noon' + Time.fromHours 6) maghrib'
    delay :: Maybe Time.NominalDiffTime -> EventSelector -> Maybe Time.NominalDiffTime
    delay _ (SpecificAngle alpha) = afterNoon alpha
    delay Nothing _ = Nothing
    delay (Just t) (FixedOffset offset) = Just (offset + t)

    maghrib' = foldl' delay (Just sunset') (maghribSetting method)
    -- maghrib' = delay sunset' (maghribSetting method)
    sunset' = fromMaybe (noon' + Time.fromHours 6) $ afterNoon sunSizeAngle
    beforeNoon alpha = fmap (noon' -) (solarAngleOffset loc spos alpha)
    afterNoon alpha = fmap (noon' +) (solarAngleOffset loc spos alpha)
    asr' = afterNoon . calcAsr decl loc
    asr1' = asr' 1
    asr2' = asr' 2
    spos = sunPosition day
    decl = declinationAngle spos
    noon' = noon spos
    midnight = Time.LocalTime day Time.midnight
    -- tz = Time.minutesToTimeZone 0
    Location _ longitude = loc
    zone time =
      Time.ZonedTime
        (Time.addLocalTime time midnight)
        (naturalTimeZone longitude)

calcAsr :: Degrees -> Location -> Int -> Degrees
calcAsr decl loc size = negate (arccotangent (fromIntegral size + tangent (absTrunc $ lat loc - decl)))

absTrunc :: Degrees -> Degrees
-- absTrunc (A.Degrees x) = A.Degrees . fromIntegral . abs . (truncate :: Double -> Integer) $ x
absTrunc = abs

sunSizeAngle :: Degrees
sunSizeAngle = 5 / 6
