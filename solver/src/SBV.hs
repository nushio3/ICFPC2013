module SBV where

import BV
import qualified Data.Map as Map
import Data.SBV

type SBitVector = SWord64

sExec :: Program -> (SBitVector -> Symbolic SBitVector)
sExec (Program x e) i = 
  sEval (Map.singleton x i) e
  
sEval :: Map.Map Idfr SBitVector -> Expr -> Symbolic SBitVector
sEval m (Var v) = return $ m Map.! v
sEval _ _= undefined

