{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Iqama.RunAll where

import Data.Aeson hiding (Result)
import Data.Maybe (catMaybes)
import Data.Time
import Data.Time.Convenience (toZoneSeriesTime, zoneSeriesTimeDay)
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Geography.Coordinates
import Iqama.Athan
import Iqama.Iqama
import Iqama.Jummah

data AllSettings = AllSettings
  { allS_athans :: (AsrRule, Method, Location), --todo add tuning, maybe
    allS_iqamas :: Athans Setting,
    allS_jummahs :: JummahSettings
  }
  deriving (Generic, Show)

instance ToJSON AllSettings where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AllSettings

data AllCalculations t = AllCalculations
  { allC_athans :: Athans t,
    allC_iqamas :: Athans (Maybe t),
    allC_jummahThuhr :: t,
    allC_jummahs :: [t]
  }
  deriving (Functor)

toJummahPair :: AllCalculations ZoneSeriesTime -> (ZoneSeriesTime, Maybe ZoneSeriesTime)
toJummahPair AllCalculations {..} = (start, end)
  where
    start = case allC_jummahs of
      [] -> allC_jummahThuhr
      times -> minimum times
    end = case allC_jummahs of
      [] -> Nothing
      times -> Just $ maximum times

toResultPairs :: AllCalculations a -> Athans (a, Maybe a)
toResultPairs calculations =
  Athans
    { fajr = (fajr athans, fajr iqamas),
      sunrise = (sunrise athans, sunrise iqamas),
      thuhr = (thuhr athans, thuhr iqamas),
      asr = (asr athans, asr iqamas),
      maghrib = (maghrib athans, maghrib iqamas),
      isha = (isha athans, isha iqamas)
    }
  where
    athans = allC_athans calculations
    iqamas = allC_iqamas calculations

-- timezone is a derivative of location.
-- God willing a future method will accommodate that.
-- hence the placement
calculateAll :: TimeZoneSeries -> AllSettings -> ZoneSeriesTime -> AllCalculations ZoneSeriesTime
calculateAll tzz settings now = AllCalculations athans iqamas jummahThuhr jummahs
  where
    day = zoneSeriesTimeDay now
    iqamas = applySetting <$> allS_iqamas settings <*> athans
    (jummahThuhr, mb_jummahs) = nextJummahIqamas computeAthans now tzz (allS_jummahs settings)
    jummahs = catMaybes mb_jummahs
    athans :: Athans ZoneSeriesTime
    athans = toZoneSeriesTime tzz <$> athansMeanTime
    athansMeanTime :: Athans ZonedTime
    athansMeanTime = computeAthans day
    computeAthans = computeTimes asrRule method location
    (asrRule, method, location) = allS_athans settings
