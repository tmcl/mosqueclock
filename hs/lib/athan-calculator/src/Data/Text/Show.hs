{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Show where

import Data.Text
import qualified Text.Read as R
import Prelude hiding (read, show)
import qualified Prelude as P

show :: (Show a) => a -> Text
show = pack . P.show

read :: (Read a) => Text -> Maybe a
read = R.readMaybe . unpack

twoDigit :: (Ord a, Num a, Show a) => a -> Text
twoDigit n = (if n < 10 then "0" else "") <> show n
