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
type Database = Map.Map (Int, [Op], Int) [Expression]

niceGenProgram :: Int -> [Op] -> [Program]
niceGenProgram size ops = do
  e <- niceGenExpression (size - 1) ops 1 `evalState` (Map.empty, Set.fromList ops)
  return $ Program e

niceGenExpression :: Int -> [Op] -> Int -> State (Database, Set.Set Op) [Expression]
niceGenExpression size ops vars = use _2 >>= \unused -> preuse (_1 . ix (size, ops, vars)) >>= \case
    Just r -> return r
    Nothing
        | size == 1 && Set.null unused -> return $ [Constant 0, Constant 1] ++ map Var [0..vars-1]
        | size >= 2 -> do
            es <- fmap concat $ sequence [op1, op2, ifs, folds]
            forM_ es $ \e -> preuse (_1 . ix (size, ops, vars)) >>= \case
                Just xs
                    | size < 6 && any (unsafePerformIO . equivE e) xs -> return ()
                    | otherwise -> _1 . ix (size, ops, vars) .= e : xs
                Nothing -> _1 . at (size, ops, vars) ?= [e]
            preuse (_1 . ix (size, ops, vars)) >>= \case
                Just xs -> return xs
                Nothing -> return []
        | otherwise -> return []
    where
        ops' = filter (/=TFold) ops
        ops1 = filter isOp1 ops
        ops2 = filter isOp2 ops
        op1 = do
            fmap concat $ forM ops1 $ \op -> do
                _2 . contains op .= False
                es <- niceGenExpression (size - 1) ops' vars
                return $ Op1 op <$> es
        op2 = do
            fmap concat $ forM [1..size-2] $ \i -> do
                let j = size - i - 1
                fmap concat $ forM ops2 $ \op -> do
                    _2 . contains op .= False
                    ls <- niceGenExpression i ops' vars
                    rs <- niceGenExpression j ops' vars
                    return $ Op2 op <$> ls <*> rs
        
        ifs = fmap concat $ forM [1..size-3] $ \i -> 
            fmap concat $ forM [1..size-3] $ \j ->
            fmap concat $ forM [1..size-3] $ \k -> do
                if i + j + k + 1 == size
                    then do
                        _2 . contains If0 .= False
                        ps <- niceGenExpression i ops' vars
                        ts <- niceGenExpression j ops' vars
                        es <- niceGenExpression k ops' vars
                        return $ If <$> ps <*> ts <*> es
                    else
                        return []
        folds
            | Fold0 `elem` ops || TFold `elem` ops = fmap concat $ forM [1..size-4] $ \i -> 
                fmap concat $ forM [1..size-4] $ \j ->
                fmap concat $ forM [1..size-4] $ \k -> do
                    let ops'' = filter (/=Fold0) ops'
                    _2 . contains Fold0 .= False
                    _2 . contains TFold .= False
                    as <- niceGenExpression i ops'' vars
                    bs <- niceGenExpression j ops'' vars
                    cs <- niceGenExpression k ops'' (vars + 2)
                    return $ Fold vars (vars + 1) <$> as <*> bs <*> cs 
            | otherwise = return []