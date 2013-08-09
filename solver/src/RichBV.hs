{-# LANGUAGE OverloadedStrings #-}

module RichBV where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Lens.Aeson
import Control.Lens
import Data.List
import Data.Function
import Control.Applicative
import Control.Monad
import Debug.Trace (trace)
import Data.Word
import Data.Bits
import System.Random
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map.Strict as M

solve problem = do
  L.putStrLn $ encode problem
  let ps = gen problem
      ss = S.toList $ S.fromList $ map (canonic.simplify.moveIfP.simplify.canonic) ps
  putStrLn $ "generate " ++ show (length ps) ++ " candidates"
  putStrLn $ "simplify to " ++ show (length ss)
  -- mapM_ print ss

  let go i = do
        n <- randomRIO (1, 100) 
        vs <- replicateM n randomIO
        let xs = [0,1,2,3,4,5,15,16,17,65535,65536,65537] ++ vs
        let res  = [ map (eval p) xs | p <- ss]
            mm   = M.fromListWith (+) $ map (\x -> (x, 1)) res
            freq = maximum $ map snd $ M.toList mm
        if freq <= 60
          then return xs
          else do
            if i > 1000 then mapM_ print ss >> return [] else go (i + 1)
--            when (i > 1000) $ do
--              print $ zip xs $ snd $ maximum $ map (\(a,b)->(b,a)) $ M.toList mm
--              error "hoge"
--            go $ i + 1

  ans <- go 0
  print ans

  putStrLn ""

gen p = do
  let size = p ^?! key "size"._Integer
      ops  = p ^.. key "operators"._Array.traverse._String.to toOp
  genProgram (fromIntegral size) ops

data Program = Program Expression
  deriving (Eq, Show, Ord, Read)

data Expression =
    Constant Word64
  | Var Int
  | If Expression Expression Expression
  | Fold Int Int Expression Expression Expression
  | Op1 Op Expression
  | Op2 Op Expression Expression
  deriving (Eq, Show, Ord, Read)

data Op = If0 | TFold | Fold0 | Not | Shl Int | Shr Int | And | Or | Xor | Plus
  deriving (Eq, Show, Ord, Read)

canonic :: Program -> Program
canonic (Program e) = Program $ canonical e

canonical :: Expression -> Expression
canonical (Constant c) = Constant c
canonical (Var n) = Var n
canonical (If p e1 e2) = If p (min e1 e2) (max e1 e2)
canonical (Fold x y v e1 e2) = Fold x y v (min e1 e2) (max e1 e2)
canonical (Op1 op e) = Op1 op e
canonical (Op2 op e1 e2) = Op2 op (min e1 e2) (max e1 e2)

moveIfP :: Program -> Program
moveIfP (Program e) = Program $ moveIf e

moveIf :: Expression -> Expression
moveIf (Constant c) = Constant c
moveIf (Var x) = Var x
moveIf (If p t e) = If (moveIf p) (moveIf t) (moveIf e)
moveIf (Op1 op e) = case moveIf e of
  If p t e -> moveIf $ If p (Op1 op t) (Op1 op e)
  e' -> Op1 op e'
moveIf (Op2 op e1 e2) = case (moveIf e1, moveIf e2) of
  (If p t e, e2') -> moveIf $ If p (Op2 op t e2') (Op2 op e e2')
  (e1', If p t e) -> moveIf $ If p (Op2 op e1' t) (Op2 op e1' e)
  (e1', e2') -> Op2 op e1 e2

simplify :: Program -> Program
simplify (Program e) = Program $ simplifyE e

simplifyE :: Expression -> Expression
simplifyE (Constant c) = Constant c
simplifyE (Var i) = Var i
simplifyE (If p e1 e2) = case (simplifyE p, simplifyE e1, simplifyE e2) of
  (Constant 0, e1', _) -> e1'
  (Constant _, _, e2') -> e2'
  (_, e1', e2') | canonical e1' == canonical e2' -> e1'

  (p', e1', e2') -> If p' e1' e2'

simplifyE (Fold x y e1 e2 e3) =
  -- Fold x y (simplifyE e1) (simplifyE e2) (simplifyE e3)
  destructFold x y e1 e2 e3

simplifyE (Op1 op e) = case (op, simplifyE e) of
  (_, Constant v) -> Constant $ evalOp1 op v

  (Not, Op1 Not e') -> simplifyE e'

  (Shl n, Op1 (Shl m) e') -> simplifyE $ Op1 (Shl $ n + m) e'
  (Shl n, _) | n >= 64 -> Constant 0

  (Shr n, Op1 (Shr m) e') -> simplifyE $ Op1 (Shr $ n + m) e'
  (Shr n, _) | n >= 64 -> Constant 0
  (Shr n, e') | lessThan (2^n) e' -> Constant 0

  (_, e') -> Op1 op e'

simplifyE (Op2 op e1 e2) = case (op, simplifyE e1, simplifyE e2) of
  (_, Constant v1, Constant v2) -> Constant $ evalOp2 op v1 v2

  (And, Constant 0, _) -> Constant 0
  (And, _, Constant 0) -> Constant 0
  (And, Constant v, e1') | v == complement 0 -> e1'
  (And, e2', Constant v) | v == complement 0 -> e2'
  (And, e1', e2') | e1' == e2' -> e1'

  (Or, Constant 0, e2') -> e2'
  (Or, e1', Constant 0) -> e1'
  (Or, Constant v, _) | v == complement 0 -> Constant $ complement 0
  (Or, _, Constant v) | v == complement 0 -> Constant $ complement 0
  (Or, e1', e2') | e1' == e2' -> e1'

  (Xor, Constant 0, e2') -> e2'
  (Xor, e1', Constant 0) -> e1'
  (Xor, Constant v, e2') | v == complement 0 -> simplifyE $ Op1 Not e2'
  (Xor, e1', Constant v) | v == complement 0 -> simplifyE $ Op1 Not e1'
  (Xor, e1', e2') | e1' == e2' -> Constant 0

  (Plus, Constant 0, e2') -> e2'
  (Plus, e1', Constant 0) -> e1'
  (Plus, e1', e2') | e1' == e2' -> simplifyE $ Op1 (Shl 1) e1'

  (_, e1', e2') -> Op2 op (min e1' e2') (max e1' e2')

destructFold x y l v e = simplifyE e8    
  where
    l' = simplifyE l
    l0 = Op2 And l' (Constant 255)
    l1 = Op2 And (Op1 (Shr 8 ) l') (Constant 255)
    l2 = Op2 And (Op1 (Shr 16) l') (Constant 255)
    l3 = Op2 And (Op1 (Shr 24) l') (Constant 255)
    l4 = Op2 And (Op1 (Shr 32) l') (Constant 255)
    l5 = Op2 And (Op1 (Shr 40) l') (Constant 255)
    l6 = Op2 And (Op1 (Shr 48) l') (Constant 255)
    l7 = Op1 (Shr 56) l'
    e0 = simplifyE v
    e1 = subst x y l0 e0 e
    e2 = subst x y l1 e1 e
    e3 = subst x y l2 e2 e
    e4 = subst x y l3 e3 e
    e5 = subst x y l4 e4 e
    e6 = subst x y l5 e5 e
    e7 = subst x y l6 e6 e
    e8 = subst x y l7 e7 e

subst x y ex ey e = f e where
  f (Constant c) = Constant c
  f (Var i)
    | i == x = ex
    | i == y = ey
    | otherwise = Var i
  f (If x y z) = If (f x) (f y) (f z)
  f (Fold i j x y z) = Fold i j (f x) (f y) (f z)
  f (Op1 op e) = Op1 op (f e)
  f (Op2 op e1 e2) = Op2 op (f e1) (f e2)

lessThan ub e = case simplifyE e of
  Constant c | c < ub -> True
  If _ e1 e2 | lessThan ub e1 && lessThan ub e2 -> True
  Op1 (Shl n) e1 | lessThan (ub `shiftR` n) e1 -> True
  Op1 (Shr n) e1 | lessThan (ub `shiftL` n) e1 -> True
  Op2 And e1 e2 | lessThan ub e1 || lessThan ub e2 -> True
  Op2 Or  e1 e2 | lessThan (ub `div` 2) e1 && lessThan (ub `div` 2) e2 -> True
  Op2 Xor e1 e2 | lessThan (ub `div` 2) e1 && lessThan (ub `div` 2) e2 -> True
  Op2 Plus e1 e2 | lessThan (ub `div` 2) e1 && lessThan (ub `div` 2) e2 -> True
  _ -> False

eval :: Program -> Word64 -> Word64
eval (Program e) x = evalE e [x]

evalE :: Expression -> [Word64] -> Word64
evalE (Constant c) _ = c
evalE (Var ix) env = env !! ix
evalE (If e1 e2 e3) env =
  if evalE e1 env == 0
    then evalE e2 env
    else evalE e3 env
evalE (Fold x y e1 e2 e3) env =
  let v = map (`mod` 0x100) $ reverse $ take 8 $ iterate (`shiftR` 8) $ evalE e1 env
  in foldr (\w a -> evalE e3 $ env ++ [w, a]) (evalE e2 env) v
evalE (Op1 op e) env =
  evalOp1 op $ evalE e env
evalE (Op2 op e1 e2) env =
  evalOp2 op (evalE e1 env) (evalE e2 env)

evalOp1 Not     = complement
evalOp1 (Shl n) = (`shiftL` n)
evalOp1 (Shr n) = (`shiftR` n)

evalOp2 And  = (.&.)
evalOp2 Or   = (.|.)
evalOp2 Xor  = xor
evalOp2 Plus = (+)

toOp "not"   = Not
toOp "shl1"  = Shl 1
toOp "shr1"  = Shr 1
toOp "shr4"  = Shr 4
toOp "shr16" = Shr 16
toOp "and"   = And
toOp "or"    = Or
toOp "xor"   = Xor
toOp "plus"  = Plus
toOp "if0"   = If0
toOp "fold"  = Fold0
toOp "tfold" = TFold

isOp1 :: Op -> Bool
isOp1 Not   = True
isOp1 (Shl _) = True
isOp1 (Shr _) = True
isOp1 _ = False

isOp2 :: Op -> Bool
isOp2 And  = True
isOp2 Or   = True
isOp2 Xor  = True
isOp2 Plus = True
isOp2 _ = False

-- generating all possible candidates

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
    return (2 + s1 + s2 + s3, u3, Fold vars (vars+1) e1 e2 e3)
genExpression size ops unused vars =
  [(1, unused, Constant 0), (1, unused, Constant 1)]
  ++ [(1, unused, Var x) | x <- [0..vars-1]]
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
      return (2 + s1 + s2 + s3, u3, Fold vars (vars+1) e1 e2 e3)
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
