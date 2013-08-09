{-# LANGUAGE OverloadedStrings #-}
module SBVSpec (spec) where

import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe
import Data.SBV
import System.IO.Unsafe
import Test.Hspec
import Test.Hspec.QuickCheck

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

prog1 :: Program
prog1 = Program "x" $ Var "x"

spec :: Spec
spec = do
  describe "sbv converter" $ do
    prop "works identically on Integers" $ \x -> 
      Just (exec prog1 x) == unsafeSExec prog1 x


