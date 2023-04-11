{-# LANGUAGE DeriveGeneric #-}

module Geography.Coordinates where

import Data.Aeson hiding (Result)
import qualified Data.Angle as A
import qualified Data.Time.Convenience as Time
import GHC.Generics

type Degrees = A.Degrees Double

data Location = Location !Latitude !Longitude
  deriving (Generic)

instance ToJSON Location where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Location

instance Show Location where
  show loc =
    "(" ++ (show . lat) loc ++ ", "
      ++ (show . lng) loc
      ++ ")"

newtype Longitude = Longitude {unlongitude :: Degrees}
  deriving (Generic, Show)

instance ToJSON Longitude where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Longitude

newtype Latitude = Latitude {unlatitude :: Degrees}
  deriving (Generic)

instance ToJSON Latitude where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Latitude

type Elevation = Double

lng :: Location -> Degrees
lng (Location _ longitude) = unlongitude longitude

lat :: Location -> Degrees
lat (Location latitude _) = unlatitude latitude

lngDouble :: Location -> Double
lngDouble loc = res
  where
    A.Degrees res = lng loc

latDouble :: Location -> Double
latDouble loc = res
  where
    A.Degrees res = lat loc

naturalTimeZone :: Longitude -> Time.TimeZone
naturalTimeZone (Longitude long) = Time.minutesToTimeZone $ truncate $ 60 * toTimeZoneHours long

toTimeZoneHours :: A.Degrees Double -> Double
toTimeZoneHours (A.Degrees a) = fixTimeZoneHours $ a / 15.0

toHours :: A.Degrees Double -> Double
toHours (A.Degrees a) = fixHours $ a / 15.0

fixTimeZoneHours :: (Num a, Ord a) => a -> a
fixTimeZoneHours = boundAmount (-24) 24 24

fixHours :: (Num a, Ord a) => a -> a
fixHours = boundAmount 0 24 24

boundAmount :: (Num a, Ord a) => a -> a -> a -> a -> a
boundAmount minbound maxbound step = go
  where
    go d
      | d < minbound = go (d + step)
      | d > maxbound = go (d - step)
      | otherwise = d

fixDegrees :: Degrees -> Degrees
fixDegrees = boundAmount 0 360 360
