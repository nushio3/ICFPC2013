{-# LANGUAGE OverloadedStrings #-}
module SimplifySpec (spec) where

import Control.Monad
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe
import Data.SBV
import System.IO.Unsafe
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.QuickCheck

import Debug.Trace (trace)
import Text.Printf

import Safe

import qualified BV as B
import qualified RichBV as R
import SBV
import Convert
import TestInputs
  


spec :: Spec
spec = do
  describe "RichBV ---> simplify RichBV" $ do
    forM_ programs $ \src -> do
      prop ("prog == simplify prog for " ++ src) $ \x -> 
        let 
          prog0 = readProgram src 
          prog = unsafePerformIO $ do
            let ret=enrichProgram $ readProgram src 
            --print ret
            return ret
          prog' = unsafePerformIO $ do
            let ret = R.simplify $  prog
            --print ret
            return ret
        in
--         trace (printf "prog0 %d = %d" x (B.exec prog0 x)) $          
--         trace (printf "prog %d = %d" x (R.eval prog x)) $ 
--         trace (printf "prog' %d = %d" x (R.eval prog' x)) $ 
        R.eval prog x == R.eval prog' x 

