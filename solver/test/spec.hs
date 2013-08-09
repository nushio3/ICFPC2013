{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified SBVSpec

main :: IO ()
main = do
  hspec $ do
    SBVSpec.spec


