{-# LANGUAGE TypeApplications #-}

module Data.Time.Convenience
  ( toJulianDay,
    fromHours,
    fromMinutes {- addLocalTime, -},
    addZonedTime,
    setLocalTimeOfDay,
    setZoneSeriesTimeStandardTimeOfDay,
    addToZoneSeriesTime,
    zoneSeriesTimeDay,
    convertTimeZone,
    toZoneSeriesTime,
    zoneSeriesTimeToZonedTime,
    zoneSeriesTimeOfDay,
    zonedTimeOfDay,
    flrMinutes,
    flrMinutesZst,
    ceilMinutes,
    ceilMinutesZst,
    flrBySecondsZst,
    ceilBySecondsZst,
    isFriday,
    summerAndWinter,
    summerTimeZone,
    winterTimeZone,
    module Data.Time,
    module Data.Time.LocalTime.TimeZone.Series,
  )
where

import Data.Maybe
import Data.Time
import Data.Time.LocalTime.TimeZone.Series hiding (latestNonSummer)

summerAndWinter ::
  ZoneSeriesTime ->
  (TimeOfDay, TimeOfDay)
summerAndWinter now = (zoneSeriesTimeOfDay winterTime, zoneSeriesTimeOfDay summerTime)
  where
    winterTZ = winterTimeZone now
    summerTZ = summerTimeZone now
    winterTime = convertTimeZone' now (TimeZoneSeries winterTZ [])
    summerTime = convertTimeZone' now (TimeZoneSeries summerTZ [])

toJulianDay :: Day -> Double
toJulianDay d = 2400000.5 + fromIntegral (toModifiedJulianDay d)

addToZoneSeriesTime :: DiffTime -> ZoneSeriesTime -> ZoneSeriesTime
addToZoneSeriesTime t zt = ZoneSeriesTime utct' tzz
  where
    ZoneSeriesTime (UTCTime day daytime) tzz = zt
    utct' = UTCTime day (daytime + t)

addZonedTime :: NominalDiffTime -> ZonedTime -> ZonedTime
addZonedTime t zt = zt {zonedTimeToLocalTime = incr}
  where
    incr = addLocalTime t (zonedTimeToLocalTime zt)

fromHours :: Double -> NominalDiffTime
fromHours a = fromIntegral @Int . truncate $ 60 * 60 * a

fromMinutes :: Double -> NominalDiffTime
fromMinutes a = fromIntegral @Int . truncate $ a * 60

convertTimeZone :: ZonedTime -> TimeZoneSeries -> ZoneSeriesTime
convertTimeZone t = ZoneSeriesTime (zonedTimeToUTC t)

convertTimeZone' :: ZoneSeriesTime -> TimeZoneSeries -> ZoneSeriesTime
convertTimeZone' zst = ZoneSeriesTime (zoneSeriesTimeToUTC zst)

toZoneSeriesTime :: TimeZoneSeries -> ZonedTime -> ZoneSeriesTime
toZoneSeriesTime = flip convertTimeZone

zoneSeriesTimeOfDay :: ZoneSeriesTime -> TimeOfDay
zoneSeriesTimeOfDay = localTimeOfDay . zoneSeriesTimeToLocalTime

zonedTimeOfDay :: ZonedTime -> TimeOfDay
zonedTimeOfDay = localTimeOfDay . zonedTimeToLocalTime

zoneSeriesTimeDay :: ZoneSeriesTime -> Day
zoneSeriesTimeDay = localDay . zoneSeriesTimeToLocalTime

zoneSeriesTimeToZonedTime :: ZoneSeriesTime -> ZonedTime
zoneSeriesTimeToZonedTime zst = ZonedTime (zoneSeriesTimeToLocalTime zst) (zoneSeriesTimeZone zst)

summerTimeZone :: ZoneSeriesTime -> TimeZone
summerTimeZone = filterTimeZone timeZoneSummerOnly

winterTimeZone :: ZoneSeriesTime -> TimeZone
winterTimeZone = filterTimeZone (not . timeZoneSummerOnly)

filterTimeZone :: (TimeZone -> Bool) -> ZoneSeriesTime -> TimeZone
filterTimeZone predicate (ZoneSeriesTime utct tzs) = maybe def snd nextMatch
  where
    nextMatch = listToMaybe matches
    lateTransitions = filter (flip endsAfter utct . fst) allTransitions
    matches = filter (predicate . snd) $ lateTransitions <> allTransitions
    TimeZoneSeries def allTransitions = tzs
    endsAfter a b = a > b

setZoneSeriesTimeStandardTimeOfDay :: ZoneSeriesTime -> TimeOfDay -> ZoneSeriesTime
setZoneSeriesTimeStandardTimeOfDay zst standardTimeOfDay = newTime
  where
    tz = zoneSeriesTimeSeries zst
    standardTime = TimeZoneSeries (winterTimeZone zst) []
    zstStandard = convertTimeZone' zst standardTime
    newTimeStandard = setLocalTimeOfDay zstStandard standardTimeOfDay
    newTime = convertTimeZone' newTimeStandard tz

setLocalTimeOfDay :: ZoneSeriesTime -> TimeOfDay -> ZoneSeriesTime
setLocalTimeOfDay zst tod = localTimeToZoneSeriesTime tzz dayAtTime
  where
    dayAtTime = LocalTime day tod
    LocalTime day _ = zoneSeriesTimeToLocalTime zst
    tzz = zoneSeriesTimeSeries zst

flrMinutesZst :: ZoneSeriesTime -> ZoneSeriesTime
flrMinutesZst (ZoneSeriesTime utct tzz) = ZoneSeriesTime (flrMinutes utct) tzz

flrMinutes :: UTCTime -> UTCTime
flrMinutes (UTCTime d t) = UTCTime d (fromIntegral $ flr t)

ceilMinutesZst :: ZoneSeriesTime -> ZoneSeriesTime
ceilMinutesZst (ZoneSeriesTime utct tzz) = ZoneSeriesTime (flrMinutes utct) tzz

ceilMinutes :: UTCTime -> UTCTime
ceilMinutes (UTCTime d t) = UTCTime d (fromIntegral $ ceilMin t)

flr :: (RealFrac a) => a -> Int
flr = flrBy 60

ceilMin :: (RealFrac a) => a -> Int
ceilMin = ceilBy 60

flrBySecondsZst :: Int -> ZoneSeriesTime -> ZoneSeriesTime
flrBySecondsZst rep (ZoneSeriesTime utct tzz) = ZoneSeriesTime (flrBySeconds rep utct) tzz

flrBySeconds :: Int -> UTCTime -> UTCTime
flrBySeconds rep (UTCTime d t) = UTCTime d (fromIntegral $ flrBy rep t)

ceilBySecondsZst :: Int -> ZoneSeriesTime -> ZoneSeriesTime
ceilBySecondsZst rep (ZoneSeriesTime utct tzz) = ZoneSeriesTime (ceilBySeconds rep utct) tzz

ceilBySeconds :: Int -> UTCTime -> UTCTime
ceilBySeconds rep (UTCTime d t) = UTCTime d (fromIntegral $ ceilBy rep t)

ceilBy :: (RealFrac a) => Int -> a -> Int
ceilBy rep i = rep * ceiling (i / rep')
  where
    rep' = fromIntegral rep

flrBy :: (RealFrac a) => Int -> a -> Int
flrBy rep i = rep * floor (i / rep')
  where
    rep' = fromIntegral rep

class HasDay a where
  getDay :: a -> Day

instance HasDay Day where
  getDay = id

instance HasDay ZoneSeriesTime where
  getDay (ZoneSeriesTime utct _) = getDay utct

instance HasDay UTCTime where
  getDay (UTCTime d _) = d

{-# INLINEABLE isFriday #-}
isFriday :: HasDay a => a -> Bool
isFriday day =
  -- day 0 is a wednesday, so 2 is friday
  toModifiedJulianDay (getDay day) `rem` 7 == 2
