{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified EvalSpec
import qualified SBVSpec
import qualified SRichBVSpec
import qualified ConvertSpec
import qualified SimplifySpec

import qualified ProvenSpec

main :: IO ()
main = do
  hspec $ do
    SBVSpec.spec
    {-
    EvalSpec.spec
    ConvertSpec.spec
    SimplifySpec.spec    
    SRichBVSpec.spec    
    ProvenSpec.spec        


-}