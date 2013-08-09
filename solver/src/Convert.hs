module Convert where

import qualified BV as B
import qualified RichBV as R

import qualified Data.Map as Map

import Data.Monoid (mempty)
import Text.Trifecta (parseString, Result(..))

readProgram :: String -> B.Program
readProgram str = 
  case parseString B.parseProgram mempty str of
    Success x -> x
    Failure doc -> error $ show doc

enrichProgram :: B.Program -> R.Program
enrichProgram (B.Program x e) = 
  R.Program $ convE (Map.singleton x 1) e
  
convE :: Map.Map B.Idfr Int -> B.Expr -> R.Expression   
convE m B.C0 = R.Constant 0
convE m B.C1 = R.Constant 1
convE m (B.Var idfr) = R.Var $ m Map.! idfr
  
  {-
data Expr = C0 | C1 | Var Idfr 
    | If0 Expr Expr Expr
    | Fold Expr Expr Reducer
    | Op1 Op1 Expr
    | Op2 Op2 Expr Expr deriving (Show, Eq, Ord)
  -}