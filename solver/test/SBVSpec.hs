{-# LANGUAGE OverloadedStrings #-}
module SBVSpec (spec) where

import Control.Monad
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe
import Data.SBV
import System.IO.Unsafe
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.QuickCheck

import Safe

import BV
import SBV
import SBVTools (unSymbolM)
import Convert
import TestInputs

import Debug.Trace(trace)
import Text.Printf

  
spec :: Spec
spec = do
  describe "BV --> SBV" $ do
    forM_ programs $ \src -> do
      prop ("exec == sExec for " ++ src) $ \x -> 
        let prog = readProgram src in
--         trace (printf "prog %d = %d" x (exec prog x)) $
--         trace (printf "prog %d = %s" x (show $ unsafeSExec prog x)) $
        Just (exec prog x) == unSymbolM (sExec prog) x

