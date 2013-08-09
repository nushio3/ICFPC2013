{-# LANGUAGE OverloadedStrings #-}
module ConvertSpec (spec) where

import Control.Monad
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe
import Data.SBV
import System.IO.Unsafe
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.QuickCheck

import Safe

import qualified BV as B
import qualified RichBV as R
import SBV
import Convert
import TestInputs
  
spec :: Spec
spec = do
  describe "BV --> RichBV" $ do
    forM_ programs $ \src -> do
      prop ("BV.exec == RichBV.eval for " ++ src) $ \x -> 
        let 
          prog = unsafePerformIO $ do
            let ret=readProgram src 
            --print ret
            return ret
          prog' = unsafePerformIO $ do
            let ret = enrichProgram $ readProgram src 
            --print ret
            return ret
        in
        B.exec prog x == R.eval prog' x 

