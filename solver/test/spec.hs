{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified SBVSpec
import qualified ConvertSpec

main :: IO ()
main = do
  hspec $ do
    ConvertSpec.spec
    SBVSpec.spec


