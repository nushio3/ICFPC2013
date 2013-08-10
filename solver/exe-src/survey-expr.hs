module Main where

import Control.Lens
import Control.Monad
import Data.List
import Data.SBV
import qualified Data.Set as Set

import RichBV
import SRichBV (sEvalE)
import Text.Printf
import System.IO.Unsafe

equivE :: Expression -> Expression -> Bool
equivE e1 e2 = unsafePerformIO $ do
  res <- sat $ \x -> sEvalE e1 [x] .== sEvalE e2 [x]
  return $ "Satisfiable." `isPrefixOf` show res



allOps = [If0 , TFold , Fold0 , Not , Shl 1 , Shr 1 , Shr 4 , Shr 16 , And , Or , Xor , Plus]

someOps = [ If0 , Shl 1 , Shr 4 , And , Or , Xor]

main :: IO ()
main = do
  let exprs = map (^. _3) $ genExpression 6 someOps someOps 1
      simpExprs = Set.toList $ Set.fromList $ 
        map simplifyE exprs  
      nubExprs = nubBy equivE simpExprs

      
  printf "#(expr) = %d\n" $ length exprs  
  printf "#(expr) = %d\n" $ length simpExprs
  printf "#(expr) = %d\n" $ length nubExprs
  return ()
  
