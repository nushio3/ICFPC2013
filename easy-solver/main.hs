{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Lens.Aeson
import Control.Lens
import Data.List
import Data.Function
import Control.Applicative
import Control.Monad
import Data.Word
import Data.Bits
import System.Random
import qualified Data.Set as S

{-
main :: IO ()
main = do
  con <- L.getContents
  let Just objs = decode con :: Maybe [Value]
  let sorted = sortBy (compare `on` (\a -> a ^?! key "size"._Integer)) objs
  L.putStrLn $ encode sorted
  print $ length sorted
-}

main = do
  con <- L.getContents
  let Just objs = decode con :: Maybe [Value]
  let sorted = sortBy (compare `on` (^?! key "size"._Integer)) objs
  mapM_ solve sorted

solve problem = do
  L.putStrLn $ encode problem
  let ps = gen problem
      ss = S.toList $ S.fromList $ map simplify ps
  putStrLn $ "generate " ++ show (length ps) ++ " candidates"
  putStrLn $ "simplify to " ++ show (length ss)
  mapM_ print ss

{-
  let go = do
        n <- randomRIO (1, 256) 
        xs <- replicateM n randomIO
        let res = [ map (eval p) xs | p <- ps]
        if isUniq res
          then return xs
          else go

  ans <- go
  print ans
  -}
  putStrLn ""

isUniq :: [[Word64]] -> Bool
isUniq xs = S.size (S.fromList xs) == (length xs)

gen p = do
  let size = p ^?! key "size"._Integer
      ops  = p ^.. key "operators"._Array.traverse._String.to toOp
  genProgram (fromIntegral size) ops

data Program = Program Expression
  deriving (Eq, Show, Ord)

data Expression =
    Constant Word64
  | Var Int
  | If Expression Expression Expression
  | Fold Expression Expression Expression
  | Op1 Op Expression
  | Op2 Op Expression Expression
  deriving (Eq, Show, Ord)

data Op = If0 | TFold | Fold0 | Not | Shl1 | Shr1 | Shr4 | Shr16 | And | Or | Xor | Plus
  deriving (Eq, Show, Ord)

simplify :: Program -> Program
simplify (Program e) = Program $ simplifyE e

simplifyE :: Expression -> Expression
simplifyE (Constant c) = Constant c
simplifyE (Var i) = Var i
simplifyE (If p e1 e2) = case simplifyE p of
  Constant 0 -> simplifyE e1
  Constant _ -> simplifyE e2
  p' -> If p' (simplifyE e1) (simplifyE e2)
simplifyE (Fold e1 e2 e3) =
  Fold (simplifyE e1) (simplifyE e2) (simplifyE e3)
simplifyE (Op1 op e) = case simplifyE e of
  Constant v -> Constant $ evalOp1 op v
  e' -> Op1 op e'
simplifyE (Op2 op e1 e2) = case (simplifyE e1, simplifyE e2) of
  (Constant v1, Constant v2) -> Constant $ evalOp2 op v1 v2
  (e1', e2') -> Op2 op e1' e2'

-- Shl1 Shl1

eval :: Program -> Word64 -> Word64
eval (Program e) x = evalE e [x]

evalE :: Expression -> [Word64] -> Word64
evalE (Constant c) _ = c
evalE (Var ix) env = env !! (ix - 1)
evalE (If e1 e2 e3) env =
  if evalE e1 env == 0
    then evalE e2 env
    else evalE e3 env
evalE (Fold e1 e2 e3) env =
  let v = map (`mod` 0x100) $ take 8 $ iterate (`shiftR` 8) $ evalE e1 env
  in foldr (\w a -> evalE e3 $ env ++ [w, a]) (evalE e2 env) v
evalE (Op1 op e) env =
  evalOp1 op $ evalE e env
evalE (Op2 op e1 e2) env =
  evalOp2 op (evalE e1 env) (evalE e2 env)

evalOp1 Not   = complement
evalOp1 Shl1  = (`shiftL` 1 )
evalOp1 Shr1  = (`shiftR` 1 )
evalOp1 Shr4  = (`shiftR` 4 )
evalOp1 Shr16 = (`shiftR` 16)

evalOp2 And  = (.&.)
evalOp2 Or   = (.|.)
evalOp2 Xor  = xor
evalOp2 Plus = (+)

toOp "not"   = Not
toOp "shl1"  = Shl1
toOp "shr1"  = Shr1
toOp "shr4"  = Shr4
toOp "shr16" = Shr16
toOp "and"   = And
toOp "or"    = Or
toOp "xor"   = Xor
toOp "plus"  = Plus
toOp "if0"   = If0
toOp "fold"  = Fold0
toOp "tfold" = TFold

isOp1 :: Op -> Bool
isOp1 Not   = True
isOp1 Shl1  = True
isOp1 Shr1  = True
isOp1 Shr4  = True
isOp1 Shr16 = True
isOp1 _ = False

isOp2 :: Op -> Bool
isOp2 And  = True
isOp2 Or   = True
isOp2 Xor  = True
isOp2 Plus = True
isOp2 _ = False

genProgram :: Int -> [Op] -> [Program]
genProgram size ops = do
  (s, [], e) <- genExpression (size - 1) ops ops 1
  guard $ s == size - 1
  return $ Program e

genExpression :: Int -> [Op] -> [Op] -> Int -> [(Int, [Op], Expression)]
genExpression size _ _ _
  | size <= 0 = []
genExpression size ops unused vars
  | TFold `elem` unused = do
    let u0 = unused \\ [TFold, Fold0]
    (s1, u1, e1) <- genExpression (size - 2) ops u0 vars
    (s2, u2, e2) <- genExpression (size - 2 - s1) ops u1 vars
    (s3, u3, e3) <- genExpression (size - 2 - s1 - s2) ops u2 (vars + 2)
    return (2 + s1 + s2 + s3, u3, Fold e1 e2 e3)
genExpression size ops unused vars =
  [(1, unused, Constant 0), (1, unused, Constant 1)]
  ++ [(1, unused, Var x) | x <- [1..vars]]
  ++ ifs ++ folds ++ op1s ++ op2s
  where
    ifs = do
      guard $ If0 `elem` ops
      let u0 = unused \\ [If0]
      (s1, u1, e1) <- genExpression (size - 1) ops u0 vars
      (s2, u2, e2) <- genExpression (size - 1 - s1) ops u1 vars
      (s3, u3, e3) <- genExpression (size - 1 - s1 - s2) ops u2 vars
      return (1 + s1 + s2 + s3, u3, If e1 e2 e3)
    folds = do
      guard $ Fold0 `elem` ops
      let u0 = unused \\ [Fold0]
      (s1, u1, e1) <- genExpression (size - 2) ops u0 vars
      (s2, u2, e2) <- genExpression (size - 2 - s1) ops u1 vars
      (s3, u3, e3) <- genExpression (size - 2 - s1 - s2) ops u2 (vars + 2)
      return (2 + s1 + s2 + s3, u3, Fold e1 e2 e3)
    op1s = do
      opr <- filter isOp1 ops
      let u0 = unused \\ [opr]
      (s1, u1, e1) <- genExpression (size - 1) ops u0 vars
      return (1 + s1, u1, Op1 opr e1)
    op2s = do
      opr <- filter isOp2 ops
      let u0 = unused \\ [opr]
      (s1, u1, e1) <- genExpression (size - 1) ops u0 vars
      (s2, u2, e2) <- genExpression (size - 1 - s1) ops u1 vars
      return (1 + s1 + s2, u2, Op2 opr e1 e2)
