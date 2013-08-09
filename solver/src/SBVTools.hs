module SBVTools where

import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe
import Data.SBV
import BV(BitVector)
import SBV(SBitVector)
import Safe (readMay, headMay)
import System.IO.Unsafe

unSymbolM :: (SBitVector -> Symbolic SBitVector)
         -> BitVector -> Maybe BitVector                          
unSymbolM prog i = unsafePerformIO $ do
  let rvSymbol = "returnValue"
  resp <- sat $ do
    rv0 <- prog $ fromIntegral i
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
             
unSymbol :: (SBitVector -> SBitVector)
           -> BitVector -> Maybe BitVector                              
unSymbol prog i = unsafePerformIO $ do
  let rvSymbol = "returnValue"
  resp <- sat $ do
    let rv0 = prog $ fromIntegral i
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
    