{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified SBVSpec
import qualified ConvertSpec
import qualified SimplifySpec

main :: IO ()
main = do
  hspec $ do
    SimplifySpec.spec    
    ConvertSpec.spec
    SBVSpec.spec


