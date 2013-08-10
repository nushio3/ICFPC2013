{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Database where
import Data.Aeson hiding ((.=))
import Control.Applicative
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Lens hiding (op)
import System.IO.Unsafe
import RichBV
import SRichBV
import qualified Data.Set as Set
type Database = Map.Map (Int, [Op], [Op], Int) [Expression]

niceGenProgram :: Int -> [Op] -> [Program]
niceGenProgram size ops = do
  e <- niceGenExpression (size - 1) ops 1 `evalStateT` (Map.empty, Set.fromList ops)
  return $ Program e

niceGenExpression :: Int -> [Op] -> Int -> StateT (Database, Set.Set Op) [] Expression
niceGenExpression size ops vars = use _2 >>= \unused -> if
    | size == 1 && Set.null unused -> lift $ [Constant 0, Constant 1] ++ map Var [0..vars-1]
    | size >= 2 -> preuse (_1 . ix (size, ops, Set.toAscList unused, vars)) >>= \case
            Just r | False -> lift $ Control.Lens.toListOf folded r
            _ -> do
                e <- op1 <|> op2 <|> ifs <|> folds
                preuse (_1 . ix (size, ops, Set.toAscList unused, vars)) >>= \case
                    Just xs
                        | any (unsafePerformIO . equivE e) xs -> lift []
                        | otherwise -> do
                            _1 . at (size, ops, Set.toAscList unused, vars) ?= e : xs
                            return e
                    Nothing -> do
                        _1 . at (size, ops, Set.toAscList unused, vars) ?= [e]
                        return e
    | otherwise -> lift []
    where
        ops' = filter (/=TFold) ops
        ops1 = filter isOp1 ops
        ops2 = filter isOp2 ops
        op1 = do
            op <- lift ops1
            _2 . contains op .= False
            e <- niceGenExpression (size - 1) ops' vars
            return $ Op1 op e
        op2 = do
            i <- lift [1..size-2]
            let j = size - i - 1
            op <- lift ops2
            _2 . contains op .= False
            l <- niceGenExpression i ops' vars
            r <- niceGenExpression j ops' vars
            return $ Op2 op l r
        ifs = do
            i <- lift [1..size-3]
            j <- lift [1..size-3]
            k <- lift [1..size-3]
            _2 . contains If0 .= False
            guard $ i + j + k + 1 == size
            If  <$> niceGenExpression i ops' vars 
                <*> niceGenExpression j ops' vars
                <*> niceGenExpression k ops' vars
        folds = do
            guard $ Fold0 `elem` ops || TFold `elem` ops
            i <- lift [1..size-4]
            j <- lift [1..size-4]
            k <- lift [1..size-4]
            _2 . contains Fold0 .= False
            _2 . contains TFold .= False
            let ops'' = filter (/=Fold0) ops'
            Fold vars (vars + 1)
                 <$> niceGenExpression i ops'' vars
                 <*> niceGenExpression j ops'' vars
                 <*> niceGenExpression k ops'' (vars + 2)