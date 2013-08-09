{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified SBVSpec
import qualified SRichBVSpec
import qualified ConvertSpec
import qualified SimplifySpec

main :: IO ()
main = do
  hspec $ do
    ConvertSpec.spec
    SimplifySpec.spec    
    SBVSpec.spec
    SRichBVSpec.spec    


