module Convert where

import qualified BV as B
import qualified RichBV as R

import qualified Data.Map as Map
import Control.Lens (at, (&), (?~))

import Data.Monoid (mempty)
import Text.Trifecta (parseString, Result(..))

readProgram :: String -> B.Program
readProgram str = 
  case parseString B.parseProgram mempty str of
    Success x -> x
    Failure doc -> error $ show doc

enrichProgram :: B.Program -> R.Program
enrichProgram (B.Program x e) = 
  R.Program $ convE (Map.singleton x 0) e
  
convE :: Map.Map B.Idfr Int -> B.Expr -> R.Expression   
convE _ B.C0 = R.Constant 0
convE _ B.C1 = R.Constant 1
convE m (B.Var idfr) = R.Var $ m Map.! idfr
convE m (B.If0 a b c) = 
  R.If (convE m a) (convE m b) (convE m c)
convE m (B.Fold a b (B.Reducer x y e)) =                        
  let m2 = m & at x ?~ n & at y ?~ (n+1) 
      n = Map.size m2
  in
  R.Fold (convE m a) (convE m b)                                         
    (convE m2 e)
convE m (B.Op1 B.Not e) = R.Op1 R.Not (convE m e)  
convE m (B.Op1 B.Shl1 e) = R.Op1 (R.Shl 1) (convE m e)  
convE m (B.Op1 B.Shr1 e) = R.Op1 (R.Shr 1) (convE m e)  
convE m (B.Op1 B.Shr4 e) = R.Op1 (R.Shr 4) (convE m e)  
convE m (B.Op1 B.Shr16 e) = R.Op1 (R.Shr 16) (convE m e)  

convE m (B.Op2 B.Plus a b) = R.Op2 (R.Plus) (convE m a) (convE m b)  
convE m (B.Op2 B.And a b) = R.Op2 (R.And) (convE m a) (convE m b)  
convE m (B.Op2 B.Or a b) = R.Op2 (R.Or) (convE m a) (convE m b)  
convE m (B.Op2 B.Xor a b) = R.Op2 (R.Xor) (convE m a) (convE m b)  
