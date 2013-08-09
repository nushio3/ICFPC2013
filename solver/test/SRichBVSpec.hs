{-# LANGUAGE OverloadedStrings #-}
module SRichBVSpec (spec) where

import Control.Monad
import Data.SBV
import Test.Hspec
import Test.Hspec.QuickCheck

import Safe

import qualified BV as B
import qualified SBV as B
import qualified RichBV as R
import qualified SRichBV as R
import SBVTools
import Convert
import TestInputs

import Debug.Trace(trace)
import Text.Printf

  
spec :: Spec
spec = do
  describe "RichBV --> SRichBV" $ do
    forM_ programs $ \src -> do
      prop ("exec == sExec for " ++ src) $ \x -> 
        let prog = enrichProgram $ readProgram src in
        Just (R.eval prog x) == unSymbol (R.sExec prog) x

