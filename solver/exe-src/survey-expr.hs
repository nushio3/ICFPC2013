{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Lens  (_3, (^.))
import Control.Monad
import Data.List
import Data.Maybe
import Data.SBV
import qualified Data.Set as Set

import BV(BitVector)
import RichBV
import SRichBV
import Text.Printf
import System.IO.Unsafe

newtype O = O { unO :: Expression}
newtype O2 = O2 { unO2 :: Expression}

instance Eq O where
  (O a) == (O b) = unsafePerformIO $ equivE a b
instance Ord O where
  O a `compare` O b = fromJust $ unsafePerformIO $ ordE2 a b

instance Eq O2 where
  (O2 a) == (O2 b) = unsafePerformIO $ equivE a b
instance Ord O2 where
  O2 a `compare` O2 b = fromJust $ unsafePerformIO $ ordE2 a b

ordE2 :: Expression -> Expression ->  IO (Maybe Ordering)
ordE2 p1 p2  = do
  p1 `equivE` p2 >>= \case 
    True -> return $ Just EQ
    False -> do
      
      ret1 <- sat $ \x -> do       
        forAll ["y"] $ \y ->
          ((y .< x) ==> (sEvalE p1 [y]  .== sEvalE p2 [y])) &&&
          sEvalE p1 [x]  .< sEvalE p2 [x] 
      ret2 <- sat $ \x -> do       
        forAll ["y"] $ \y ->
          ((y .< x) ==> (sEvalE p1 [y]  .== sEvalE p2 [y])) &&&
          sEvalE p1 [x]  .> sEvalE p2 [x] 
      if | "Satisfiable"   `isInfixOf` show ret1 -> return $Just LT
         | "Satisfiable" `isInfixOf` show ret2   -> return $Just GT
         | otherwise -> do
             print ret1
             print ret2
             return Nothing

allOps = [If0 , TFold , Fold0 , Not , Shl 1 , Shr 1 , Shr 4 , Shr 16 , And , Or , Xor , Plus]

someOps = [ If0 , Shr 1 , And , Or ,Xor]

main :: IO ()
main = do
  let exprs = map (^. _3) $ genExpression 6 someOps someOps 1
      simpExprs = Set.toList $ Set.fromList $ 
        map simplifyE exprs  
      nubExprs0 = nubBy (\x y -> unsafePerformIO $ equivE x y) simpExprs
      nubExprs = 
        map unO $
        Set.toList $ Set.fromList $ 
        map O $
        simpExprs
      nubExprs2 = 
        map unO2 $
        Set.toList $ Set.fromList $ 
        map O2 $
        simpExprs
  print =<< ordE (exprs!!0)  (exprs!!10) 
  print =<< ordE2 (exprs!!0)  (exprs!!10) 
  printf "#(expr) = %d\n" $ length exprs  
  printf "#(expr) = %d\n" $ length simpExprs
  printf "#(expr) = %d\n" $ length nubExprs0
  printf "#(expr) = %d\n" $ length nubExprs
  printf "#(expr) = %d\n" $ length nubExprs2
  return ()
  
