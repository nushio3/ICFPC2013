{-# LANGUAGE LambdaCase, MultiWayIf #-}
module SRichBV where


import BV(BitVector)
import Data.List
import SBV (SBitVector)
import RichBV
import Data.SBV
import System.IO.Unsafe

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


equivE :: Expression -> Expression -> IO Bool
equivE e1 e2 = do
  res <- prove $ \x -> sEvalE e1 [x] .== sEvalE e2 [x]
  return $ "Q.E.D." `isPrefixOf` show res

ordE :: Expression -> Expression -> IO (Maybe Ordering)
ordE p1 p2  = do
  p1 `equivE` p2 >>= \case 
    True -> return $ Just EQ
    False -> search minBound maxBound
  
  where
    search a b
      | a+1 == b = return $ Just $ evalE  p1 [b] `compare` evalE  p2 [b]
      | otherwise = do
          let c = div (a+1) 2 + div b 2 
          go c >>= \case
            Just True  -> search c b
            Just False -> search a c
            Nothing    -> return Nothing

    go :: BitVector -> IO (Maybe Bool)
    go n = do
      resp <- prove $ \n' -> 
         (n' .< fromIntegral n) ==> sEvalE p1 [n']  .== sEvalE p2 [n']
      if 
        | "Q.E.D." `isInfixOf` show resp       -> return $ Just True
        | "Falsifiable." `isInfixOf` show resp -> return $ Just False
        | otherwise                            -> do
           print resp
           return Nothing
         
equiv ::  Program -> Program -> IO Bool
equiv prog1 prog2 = do
  resp <- prove $ \x -> sExec prog1 x .== sExec prog2 x
  return $ "Q.E.D." `isInfixOf` show resp

equivNeq ::  Program -> Program -> IO (Maybe BitVector)
equivNeq prog1 prog2 = do
  resp <- sat $ do
    x <- symbolic "inputVal"
    return $ sExec prog1 x ./= sExec prog2 x
  if not $ "Satisfiable." `isInfixOf` show resp
    then return Nothing
    else (do
      let atari = 
            read $
            (!!2)$
            words$
            head $
            filter (isInfixOf "inputVal") $ 
            lines $ show resp
      return $ Just atari)
