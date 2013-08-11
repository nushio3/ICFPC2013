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
import Debug.Trace
import Data.List
import qualified Data.Set as Set
import Control.Monad.Logic
type Database = Map.Map (Int, [Op], Int) [Expression]

niceGenProgram :: Int -> [Op] -> [Program]
niceGenProgram size ops = do
  let e = observeAllT (niceGenExpression (size - 1) ops 1) `evalState` (Map.empty, Set.fromList ops)
  map Program e

fromList :: [a] -> LogicT m a
fromList xs = LogicT $ \f x -> foldr f x xs

niceGenExpression :: Int -> [Op] -> Int -> LogicT (State (Database, Set.Set Op)) Expression
niceGenExpression size ops vars = use _2 >>= \unused -> preuse (_1 . ix (size, ops, vars)) >>= \case
    Just r -> fromList r
    Nothing
        | size == 1 && Set.null unused -> fromList $ [Constant 0, Constant 1] ++ map Var [0..vars-1]
        | size >= 2 -> do
            e <- op1 <|> op2 <|> ifs <|> folds
            
            preuse (_1 . ix (size, ops, vars)) >>= \case
                Just xs
                    | size < 6 && any (unsafePerformIO . equivE e) xs -> empty
                    | otherwise -> do
                        _1 . ix (size, ops, vars) .= e : xs
                        return e
                Nothing -> do
                    _1 . at (size, ops, vars) ?= [e]
                    return e
        | otherwise -> empty
    where
        ops' = filter (/=TFold) ops
        ops1 = filter isOp1 ops
        ops2 = filter isOp2 ops
        op1 = do
            op <- fromList ops1
            _2 . contains op .= False
            e <- niceGenExpression (size - 1) ops' vars
            return $ Op1 op e
        op2 = do
            op <- fromList ops2
            i <- fromList [1..size-2]
            let j = size - i - 1
            _2 . contains op .= False
            l <- niceGenExpression i ops' vars
            r <- niceGenExpression j ops' vars
            return $ Op2 op l r
        
        ifs = do
            i <- fromList [1..size-3]
            j <- fromList [1..size-3]
            let k = size - 1 - i - j
            guard $ k > 0
            _2 . contains If0 .= False
            p <- niceGenExpression i ops' vars
            t <- niceGenExpression j ops' vars
            e <- niceGenExpression k ops' vars
            return $ If p t e
        folds
            | Fold0 `elem` ops || TFold `elem` ops = do
                i <- fromList [1..size-4]
                j <- fromList [1..size-4]
                let k = size - 2 - i - j
                guard $ k > 0
                let ops'' = filter (/=Fold0) ops'
                _2 . contains Fold0 .= False
                _2 . contains TFold .= False
                a <- niceGenExpression i ops'' vars
                b <- niceGenExpression j ops'' vars
                c <- niceGenExpression k ops'' (vars + 2)
                return $ Fold vars (vars + 1) a b c
            | otherwise = empty