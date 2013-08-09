module SRichBV where

import Data.List (isInfixOf)
import SBV (SBitVector)
import RichBV
import Data.SBV

sExec :: Program -> SBitVector -> SBitVector
sExec (Program e) x = sEvalE e [x]

sEvalE :: Expression -> [SBitVector] -> SBitVector
sEvalE (Constant c) _ = fromIntegral c
sEvalE (Var ix) env = env !! ix 
sEvalE (If e1 e2 e3) env =
  ite (sEvalE e1 env .== 0)
      (sEvalE e2 env)
      (sEvalE e3 env)
sEvalE (Fold _ _  e1 e2 e3) env =
  let v = map (.&. 0xFF) $ reverse $ take 8 $ iterate (`shiftR` 8) $ sEvalE e1 env
  in foldr (\w a -> sEvalE e3 $ env ++ [w, a]) (sEvalE e2 env) v
  
sEvalE (Op1 op e) env =
  sEvalOp1 op $ sEvalE e env
sEvalE (Op2 op a b) env =
  sEvalOp2 op (sEvalE a env) (sEvalE b env)

sEvalOp1 Not = complement
sEvalOp1 (Shl n) = (`shiftL` n)
sEvalOp1 (Shr n) = (`shiftR` n)

sEvalOp2 And  = (.&.)
sEvalOp2 Or   = (.|.)
sEvalOp2 Xor  = xor
sEvalOp2 Plus = (+)


equiv ::  Program -> Program -> IO Bool
equiv prog1 prog2 = do
  resp <- prove $ \x -> sExec prog1 x .== sExec prog2 x
  return $ "Q.E.D." `isInfixOf` show resp
