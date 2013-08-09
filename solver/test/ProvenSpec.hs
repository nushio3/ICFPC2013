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
import qualified SBV as B
import qualified SRichBV as R
import SBVTools
import Convert
import TestInputs
  
spec :: Spec
spec = do
  describe "SBV === SRichBV" $ do
    forM_ programs $ \src -> do
      it ("SBV === SRichBV for " ++ src) $ 
        let 
          prog0 = B.sExec $ readProgram src 
          prog1 = R.sExec $ enrichProgram $ readProgram src 
        in
        (Just True ==) $ unPredicate $ 
          forAll ["input"] $
            \x -> prog0 x .== prog1 x

      it ("SRichBV === simplified SRichBV for " ++ src) $ 
        let 
          prog0 = enrichProgram $ readProgram src 
          
          prog1 = R.sExec $            prog0
          prog2 = R.sExec $ R.simplify prog0
        in
        (Just True ==) $ unPredicate $ 
          forAll ["input"] $
            \x -> prog1 x .== prog2 x

