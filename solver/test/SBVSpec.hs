{-# LANGUAGE OverloadedStrings #-}
module SBVSpec (spec) where

import Control.Monad
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe
import Data.Monoid (mempty)
import Data.SBV
import System.IO.Unsafe
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Trifecta (parseString, Result(..))

import Safe

import BV
import SBV

unsafeSExec ::  Program -> BitVector -> Maybe BitVector
unsafeSExec prog i = unsafePerformIO $ do
  let rvSymbol = "returnValue"
  resp <- sat $ do
    rv0 <- sExec prog $ fromIntegral i
    rv1 <- symbolic rvSymbol
    return $ rv0 .== rv1
  let sresp :: String
      sresp = show resp
  let rcand :: [BitVector]
      rcand = do -- list monad
        let True = ("Satisfiable." `isPrefixOf` sresp)
        rvLine <- filter (isInfixOf rvSymbol) $ lines sresp
        let (_:_: val : _) = words $ rvLine
        maybeToList $ readMay val
  return $ headMay rcand

s2p :: String -> Program
s2p str = 
  case parseString parseProgram mempty str of
    Success x -> x
    Failure doc -> error $ show doc
  
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
  describe "sbv converter" $ do
    forM_ programs $ \src -> do
      prop ("exec == sExec for " ++ src) $ \x -> 
        let prog = s2p src in
        Just (exec prog x) == unsafeSExec prog x


