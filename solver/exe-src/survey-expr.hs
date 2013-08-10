{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.SBV
import qualified Data.Set as Set

import BV(BitVector)
import RichBV
import SRichBV (sEvalE)
import Text.Printf
import System.IO.Unsafe

newtype O = O { unO :: Expression}

instance Eq O where
  (O a) == (O b) = unsafePerformIO $ equivE a b
instance Ord O where
  O a `compare` O b = fromJust $ unsafePerformIO $ ordE a b

equivE :: Expression -> Expression -> IO Bool
equivE e1 e2 = do
  res <- prove $ \x -> sEvalE e1 [x] .== sEvalE e2 [x]
  return $ "Q.E.D." `isPrefixOf` show res

ordE :: Expression -> Expression ->  IO (Maybe Ordering)
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
        | "Q.E.D." `isInfixOf` show resp       -> return $Just True
        | "Falsifiable." `isInfixOf` show resp -> return $Just True
        | otherwise                            -> do
           print resp
           return Nothing
         




allOps = [If0 , TFold , Fold0 , Not , Shl 1 , Shr 1 , Shr 4 , Shr 16 , And , Or , Xor , Plus]

someOps = [ If0 , Shl 1 , Shr 1 , And , Or ,Xor]

main :: IO ()
main = do
  let exprs = map (^. _3) $ genExpression 6 someOps someOps 1
      simpExprs = Set.toList $ Set.fromList $ 
        map simplifyE exprs  
--      nubExprs = nubBy (\x y -> unsafePerformIO $ equivE x y) simpExprs
      nubExprs2 = 
        map unO $
        Set.toList $ Set.fromList $ 
        map O $
        simpExprs
  printf "#(expr) = %d\n" $ length exprs  
  printf "#(expr) = %d\n" $ length simpExprs
  printf "#(expr) = %d\n" $ length nubExprs2
  return ()
  
