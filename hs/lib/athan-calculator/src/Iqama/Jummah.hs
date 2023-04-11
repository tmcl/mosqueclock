{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Iqama.Jummah (Jummah, nextJummahIqamas, JummahSettings) where

import qualified Data.Time as Time
import qualified Data.Time.Convenience as Time
  ( convertTimeZone,
    isFriday,
    zoneSeriesTimeDay,
  )
import qualified Data.Time.LocalTime.TimeZone.Series as Time
import Iqama.Athan
import Iqama.Iqama
import Prelude hiding (head)

type Jummah = Iqama

type Jummahs = [Iqama]

type JummahSetting = Setting

type JummahSettings = [JummahSetting]

fridayAfter :: Time.Day -> Time.Day
fridayAfter (Time.ModifiedJulianDay day) = Time.ModifiedJulianDay new
  where
    -- day 0 is a wednesday, so 1 is thurs and 2 is friday
    new = 2 + 7 * ceiling ((fromInteger day -1) / 7 :: Double)

nextJummah :: AthanComputer -> Time.ZoneSeriesTime -> Time.ZonedTime
nextJummah athans now = thuhr . athans $ theFriday
  where
    today = Time.zoneSeriesTimeDay now
    asrToday = asr . athans $ today
    isBeforeAsrToday = Time.zoneSeriesTimeToUTC now < Time.zonedTimeToUTC asrToday
    todayIsFriday = Time.isFriday today -- day 0 is a wednesday, so 2 is friday
    theFriday = if todayIsFriday && isBeforeAsrToday then today else fridayAfter today

-- works from any day i.e. the ZoneSeriesTime is some "now" relative to your own interest
nextJummahIqamas :: AthanComputer -> Time.ZoneSeriesTime -> Time.TimeZoneSeries -> JummahSettings -> (Time.ZoneSeriesTime, Jummahs)
nextJummahIqamas comp zst tzz settings = (jummahThuhr, findManyJummahIqamas settings jummahThuhr)
  where
    jummahThuhr = Time.convertTimeZone (nextJummah comp zst) tzz

findManyJummahIqamas :: JummahSettings -> Time.ZoneSeriesTime -> Jummahs
findManyJummahIqamas jummahSettings time = applySetting <$> jummahSettings <*> pure time
