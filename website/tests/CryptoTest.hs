{-# LANGUAGE OverloadedStrings #-}
module CryptoTest (cryptoSpecs) where


import TestImport hiding (Spec)
import qualified Data.Text as Text

import Contents.Salt
import Test.Hspec
import Test.Hspec.QuickCheck
import System.IO.Unsafe

unsafeEncode :: Show a => a -> Text.Text
unsafeEncode = unsafePerformIO . encode

cryptoSpecs :: Spec
cryptoSpecs =
  describe "crypto engine" $ do
    prop "works identically on Integers" $ \x ->
      (decode $ unsafeEncode (x::Integer)) == Just x
    prop "works identically on Strings" $ \x ->
      (decode $ unsafeEncode (x::String)) == Just x
    prop "works identically on Complex Data Types" $ \x0 ->
      let x :: String
          x = show $ show $ show (x0::[(String,Double)])
        in (decode $ unsafeEncode x) == Just x

