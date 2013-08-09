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

programs :: [String]
programs = unsafePerformIO $ do
  env <- lookupEnv "test_file"
  case env of
    Nothing -> return defs
    Just fn -> do
      xs <- readFile fn
      return $ filter (not . null) $ lines xs
  where
    defs = [ "(lambda (x) x)"
           , "(lambda (x) (fold x 0 (lambda (y z) (plus y z))))"
           ]  
  
spec :: Spec
spec = do
  describe "equality of enrichment" $ do
    forM_ programs $ \src -> do
      prop ("BV.exec == RichBV.eval for " ++ src) $ \x -> 
        let 
          prog = unsafePerformIO $ do
            let ret=readProgram src 
            print ret
            return ret
          prog' = unsafePerformIO $ do
            let ret = enrichProgram $ readProgram src 
            print ret
            return ret
        in
        B.exec prog x == R.eval prog' x 

