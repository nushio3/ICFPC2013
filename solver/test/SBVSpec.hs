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
import Convert
import Examples

import Debug.Trace(trace)
import Text.Printf

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
  
spec :: Spec
spec = do
  describe "sbv spec" $ do
    forM_ programs $ \src -> do
      prop ("exec == sExec for " ++ src) $ \x -> 
        let prog = readProgram src in
--         trace (printf "prog %d = %d" x (exec prog x)) $
--         trace (printf "prog %d = %s" x (show $ unsafeSExec prog x)) $
        Just (exec prog x) == unsafeSExec prog x

