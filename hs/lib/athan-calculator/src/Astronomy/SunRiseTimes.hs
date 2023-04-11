{-# LANGUAGE TypeApplications #-}

module Astronomy.SunRiseTimes where

import Data.Angle hiding (Degrees)
import qualified Data.Angle as A
import Data.Int
import qualified Data.Time as Time
import qualified Data.Time.Convenience as Time
  ( fromHours,
    toJulianDay,
  )
import Geography.Coordinates
import Prelude

data SunPosition = SunPosition
  { equationOfTime :: Double, -- Time.NominalDiffTime
    declinationAngle :: Degrees
  }
  deriving (Show, Eq)

sunPosition :: Time.Day -> SunPosition
sunPosition day =
  SunPosition
    { equationOfTime = eqT,
      declinationAngle = d
    }
  where
    small_d = small_d' day
    g = g' small_d
    q = q' small_d
    lL = lL' q g

    e = e' small_d
    d = arcsine $ sine e * sine lL
    rRA' :: Degrees
    rRA' = arctangent2 (A.cosine e * sine lL) (A.cosine lL)
    rRA = toHours rRA'

    eqT = toHours q - rRA

e' :: Double -> Degrees
e' small_d = A.Degrees $ 23.4393 - 0.00000036 * small_d

small_d' :: Time.Day -> Double
small_d' day = Time.toJulianDay day - 2451545.0

g' :: Double -> Degrees
g' small_d = fixDegrees . A.Degrees $ 357.5291 + 0.98560028 * small_d

q' :: Double -> Degrees
q' small_d = fixDegrees . A.Degrees $ 280.459 + 0.98564736 * small_d

lL' :: Degrees -> Degrees -> Degrees
lL' q g = fixDegrees (q + 1.9148 * A.Degrees (sine g) + A.Degrees (0.020 * sine (2 * g)))

extraNoon :: SunPosition -> Double
extraNoon spos = 60 * 60 * equationOfTime spos

extraNoonSecs :: SunPosition -> Int64
extraNoonSecs spos = truncate (extraNoon spos)

noon :: SunPosition -> Time.NominalDiffTime
noon spos = Time.fromHours 12 - fromIntegral (extraNoonSecs spos)

solarAngleOffset' :: Location -> SunPosition -> Degrees -> Maybe Degrees
solarAngleOffset' loc spos alpha = timeAngle'
  where
    top = negate (sine alpha) - sine (lat loc) * sine decl
    bottom = cosine (lat loc) * cosine decl
    decl = declinationAngle spos
    timeAngle = arccosine (top / bottom)
    A.Degrees rawTimeAngle = timeAngle
    timeAngle'
      | isNaN rawTimeAngle = Nothing
      | otherwise = Just timeAngle

solarAngleOffset :: Location -> SunPosition -> Degrees -> Maybe Time.NominalDiffTime
solarAngleOffset loc spos alpha = Time.fromHours . toHours <$> timeAngle
  where
    timeAngle = solarAngleOffset' loc spos alpha
