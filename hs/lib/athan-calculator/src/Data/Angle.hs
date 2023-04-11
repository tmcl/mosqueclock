{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--  Deal with angles measured in degrees or radians.
--
--  Names are kept deliberately different from the standard prelude to avoid name clashes.
--
--  Originally by Andrew Coppin.
--    * Changed for compatibility with a c library Tristan uses
--    * Changed to use Mathematics.Fdlibm (compat with java)
module Data.Angle where

-- we will use fdlibm so obscure prelude's trig.
-- by explicitly accepting what we want, we won't
-- accidentally mix GNU libm trig with fdlibm trig.

import Data.Aeson hiding (Result)
import GHC.Generics
import Mathematics.Fdlibm
import Prelude
  ( Double,
    Eq,
    Floating,
    Fractional,
    Num,
    Ord,
    Show,
    ($),
    (*),
    (.),
    (/),
  )

-- | An angle in radians.
newtype Radians x = Radians x deriving (Eq, Ord, Show, Num, Fractional)

-- | An angle in degrees.
newtype Degrees x = Degrees x deriving (Generic, Eq, Ord, Show, Num, Fractional)

instance (ToJSON x) => ToJSON (Degrees x) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON x) => FromJSON (Degrees x)

-- | Convert from radians to degrees.
degrees :: (Floating x) => Radians x -> Degrees x
degrees (Radians x) = Degrees (x * 180 / pi)

-- | Convert from degrees to radians.
radians :: (Floating x) => Degrees x -> Radians x
radians (Degrees x) = Radians (x * pi / 180)

-- | Type-class for angles.
class Angle a where
  sine :: a Double -> Double
  cosine :: a Double -> Double
  tangent :: a Double -> Double

  arcsine :: Double -> a Double
  arccosine :: Double -> a Double
  arctangent :: Double -> a Double

  arctangent2 :: Double -> Double -> a Double

instance Angle Radians where
  sine (Radians x) = sin x
  cosine (Radians x) = cos x
  tangent (Radians x) = tan x

  arcsine x = Radians (asin x)
  arccosine x = Radians (acos x)
  arctangent x = Radians (atan x)

  arctangent2 x y = Radians $ atan2 x y

instance Angle Degrees where
  sine = sine . radians
  cosine = cosine . radians
  tangent = tangent . radians

  arcsine = degrees . arcsine
  arccosine = degrees . arccosine
  arctangent = degrees . arctangent

  arctangent2 x y = degrees $ arctangent2 x y

arccotangent :: (Angle a) => Double -> a Double
arccotangent x = arctangent $ 1.0 / x
