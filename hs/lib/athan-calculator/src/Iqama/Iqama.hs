{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}

module Iqama.Iqama where

import Data.Aeson hiding (Result)
import Data.Time
import Data.Time.Convenience
  ( addToZoneSeriesTime,
    setLocalTimeOfDay,
    setZoneSeriesTimeStandardTimeOfDay,
  )
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Iqama.Athan

data Setting
  = Delay DiffTime
  | FixedTime TimeOfDay
  | WinterTime TimeOfDay
  | Shown
  deriving (Generic, Show)

instance ToJSON Setting where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Setting

type Iqama = Maybe ZoneSeriesTime

-- it will be Nothing in the case of Shown

-- data Result = DelayR {dr_setting :: NominalDiffTime, dr_time :: TimeOfDay}
--     | FixedTimeR {ftr_setting :: TimeOfDay, ftr_time :: TimeOfDay}
--     | WinterTimeR {wtr_setting :: TimeOfDay, wtr_time :: TimeOfDay}
--     | ShownR
-- consider refactoring that using a gadt?

resultToD :: Iqama -> Maybe TimeOfDay
resultToD = fmap $ localTimeOfDay . zoneSeriesTimeToLocalTime

type Settings = Athans Setting

-- data Iqama = Iqama { iq_athan :: TimeOfDay, iq_iqama :: Result }

type Iqamas a = Athans (Maybe a)

applySetting :: Setting -> ZoneSeriesTime -> Iqama
applySetting (FixedTime t) zst = Just $ setLocalTimeOfDay zst t
applySetting (WinterTime t) zst = Just $ setZoneSeriesTimeStandardTimeOfDay zst t
applySetting (Delay d) zst = Just $ addToZoneSeriesTime d zst
applySetting Shown _ = Nothing

--TODO deprecated. for info only
makeIqama :: Setting -> ZoneSeriesTime -> Iqama
makeIqama = applySetting
