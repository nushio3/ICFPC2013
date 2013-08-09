module SBV where

import BV
import Control.Applicative
import Control.Lens ((&), (?~), at)
import qualified Data.Map as Map
import Data.SBV

type SBitVector = SWord64

sExecM :: Program -> (SBitVector -> Symbolic SBitVector)
sExecM (Program x e) i = 
  sEvalM (Map.singleton x i) e
  
sEvalM :: Map.Map Idfr SBitVector -> Expr -> Symbolic SBitVector
sEvalM m (If0 a b c) = do
  va <- sEvalM m a
  vb <- sEvalM m b
  vc <- sEvalM m c
  return $ ite (va .== 0) vb vc
sEvalM m (Fold a b (Reducer v w e)) = do
  va <- sEvalM m a
  foldr go (sEvalM m b) [shiftR va (i * 8) .&. 0xFF | i <- [7,6..0]]
  where 
    go x my = do
      y <- my
      sEvalM (m & at v ?~ x & at w ?~ y) e      
  
sEvalM m (Op1 Not e) = sOp1 complement m e
sEvalM m (Op1 Shl1 e) = sOp1 (flip shiftL 1) m e
sEvalM m (Op1 Shr1 e) = sOp1 (flip shiftR 1) m e
sEvalM m (Op1 Shr4 e) = sOp1 (flip shiftR 4) m e
sEvalM m (Op1 Shr16 e) = sOp1 (flip shiftR 16) m e

sEvalM m (Op2 Plus a b) = sOp2 (+) m a b
sEvalM m (Op2 And a b) = sOp2 (.&.) m a b
sEvalM m (Op2 Or a b) = sOp2 (.|.) m a b
sEvalM m (Op2 Xor a b) = sOp2 xor m a b

sEvalM m (Var v) = return $ m Map.! v
sEvalM _ C0 = return 0
sEvalM _ C1 = return 1

sOp1 f m a = fmap f $ sEvalM m a

sOp2 f m a b = do
  va <- sEvalM m a 
  vb <- sEvalM m b
  return $ f va vb


sExec :: Program -> (SBitVector -> SBitVector)
sExec (Program x e) i = 
  sEval (Map.singleton x i) e


sEval :: Map.Map Idfr SBitVector -> Expr -> SBitVector
sEval m (If0 a b c) = ite (sEval m a .== 0) (sEval m b) (sEval m c)
sEval m (Fold a b (Reducer v w e)) = foldr (\x y -> sEval (m & at v ?~ x & at w ?~ y) e) (sEval m b)
    $ [shiftR (sEval m a) (i * 8) .&. 0xFF | i <- [7,6..0]]

sEval m (Op1 Not e) = complement (sEval m e)
sEval m (Op1 Shl1 e) = shiftL (sEval m e) 1
sEval m (Op1 Shr1 e) = shiftR (sEval m e) 1
sEval m (Op1 Shr4 e) = shiftR (sEval m e) 4
sEval m (Op1 Shr16 e) = shiftR (sEval m e) 16
sEval m (Op2 Plus a b) = sEval m a + sEval m b
sEval m (Op2 And a b) = sEval m a .&. sEval m b
sEval m (Op2 Or a b) = sEval m a .|. sEval m b
sEval m (Op2 Xor a b) = sEval m a `xor` sEval m b
sEval m (Var v) = m Map.! v
sEval _ C0 = 0
sEval _ C1 = 1
