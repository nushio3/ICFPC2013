{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Lens
import Control.Monad
import Data.List
import Data.SBV
import qualified Data.Set as Set

import BV(BitVector)
import RichBV
import SRichBV
import Text.Printf
import System.IO.Unsafe



allOps = [If0 , TFold , Fold0 , Not , Shl 1 , Shr 1 , Shr 4 , Shr 16 , And , Or , Xor , Plus]

someOps = [ If0 , Shl 1 , Shr 1 , And , Or ]

main :: IO ()
main = do
  let exprs = map (^. _3) $ genExpression 5 someOps someOps 1
      simpExprs = Set.toList $ Set.fromList $ 
        map simplifyE exprs  
      nubExprs = nubBy (\x y -> unsafePerformIO $ equivE x y) simpExprs

      
  printf "#(expr) = %d\n" $ length exprs  
  printf "#(expr) = %d\n" $ length simpExprs
  printf "#(expr) = %d\n" $ length nubExprs
  return ()
  
