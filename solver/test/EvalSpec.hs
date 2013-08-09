module EvalSpec(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import TestInputs

import Control.Monad

import qualified BV as B
import qualified RichBV as R
import Convert

spec :: Spec
spec = do
  describe "Sever.eval == BV.exec == RichBV.eval" $ do
    forM_ evalPairs $ \(src, ioPairs) -> do
      forM_ ioPairs $ \(x,y) -> do
        it ("Server.eval == BV.eval for " ++ src ++ show (x,y)) $
          let prog  = readProgram src 
          in
          B.exec prog x  `shouldBe` y
        it ("Server.eval == RichBV.eval for " ++ src ++ show (x,y)) $
          let prog' = enrichProgram $ readProgram src 
          in
          R.eval prog' x  `shouldBe` y

