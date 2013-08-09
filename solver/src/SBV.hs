module SBV where

import BV
import qualified Data.Map as Map
import Data.SBV

type SBitVector = SWord64

execSBV :: Program -> (SBitVector -> Symbolic SBitVector)
execSBV (Program x e) i = 
  evalSBV (Map.singleton x i) e
  
evalSBV :: Map.Map Idfr SBitVector -> Expr -> Symbolic SBitVector
evalSBV = undefined