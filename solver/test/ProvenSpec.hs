{-# LANGUAGE OverloadedStrings #-}
module ProvenSpec (spec) where

import Control.Monad
import Data.SBV
import System.IO.Unsafe
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.QuickCheck

import Safe

import qualified BV as B
import qualified RichBV as R
import Convert
import TestInputs
  
spec :: Spec
spec = do
  describe "SBV === SRichBV" $ do
    forM_ programs $ \src -> do
      prop ("SBV === SRichBV for " ++ src) $ \x -> 
        let 
          prog  = readProgram src 
          prog' = enrichProgram $ readProgram src 
        in
        x == (x :: Word64)

