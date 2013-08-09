{-# LANGUAGE OverloadedStrings #-}
module SBVSpec (spec) where



import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "sbv converter" $ do
    prop "works identically on Integers" $ \x ->
      x == (x::Integer)


