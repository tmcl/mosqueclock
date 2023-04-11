{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import System.Exit
import System.Info

main :: IO ()
main = do
  putStrLn "unimplemented on your arch:"
  putStrLn System.Info.arch
  System.Exit.exitFailure
