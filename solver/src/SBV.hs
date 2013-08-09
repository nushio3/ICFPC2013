module SBV where

import BV
import Control.Applicative
import Control.Lens ((&), (.~), ix)
import qualified Data.Map as Map
import Data.SBV

type SBitVector = SWord64

sExec :: Program -> (SBitVector -> Symbolic SBitVector)
sExec (Program x e) i = 
  sEval (Map.singleton x i) e
  
sEval :: Map.Map Idfr SBitVector -> Expr -> Symbolic SBitVector
sEval m (If0 a b c) = do
  va <- sEval m a
  vb <- sEval m b
  vc <- sEval m c
  return $ ite (va .== 0) vb vc
sEval m (Fold a b (Reducer v w e)) = do
  va <- sEval m a
  foldr go (sEval m b) [shiftR va (i * 8) .&. 0xFF | i <- [0..7]]
  where 
    go x my = do
      y <- my
      sEval (m & ix v .~ x & ix w .~ y) e
  
sEval m (Op1 Not e) = sOp1 complement m e
sEval m (Op1 Shl1 e) = sOp1 (flip shiftL 1) m e
sEval m (Op1 Shr1 e) = sOp1 (flip shiftR 1) m e
sEval m (Op1 Shr4 e) = sOp1 (flip shiftR 4) m e
sEval m (Op1 Shr16 e) = sOp1 (flip shiftR 16) m e

sEval m (Op2 Plus a b) = sOp2 (+) m a b
sEval m (Op2 And a b) = sOp2 (.&.) m a b
sEval m (Op2 Or a b) = sOp2 (.|.) m a b
sEval m (Op2 Xor a b) = sOp2 xor m a b

sEval m (Var v) = return $ m Map.! v
sEval _ C0 = return 0
sEval _ C1 = return 1

sOp1 f m a = fmap f $ sEval m a

sOp2 f m a b = do
  va <- sEval m a 
  vb <- sEval m b
  return $ f va vb
