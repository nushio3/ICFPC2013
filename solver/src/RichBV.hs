{-# LANGUAGE OverloadedStrings #-}

module RichBV where

import Control.Applicative
import Control.Monad
import Control.Lens.Aeson
import Control.Lens

import Data.Aeson
import Data.List
import Data.Word
import Data.Text.Lens
import Data.Bits
import Data.Monoid
import System.Random
import qualified Data.Map.Strict as M
import Data.Function

-- $setup
-- >>> import Data.SBV

solve' :: Value -> IO (Maybe (Context [Word64] [Word64] [String]))
solve' p = do
  let size = p ^?! key "size"._Integer.from enum
      ops  = p ^.. key "operators"._Array.traverse._String.unpacked
  solve size ops undefined

-- >>> prove $ forAll_ $ \x -> x*2 .== x+(x :: SWord64)
-- Q.E.D.

mismatchTolerance :: Int
mismatchTolerance = 50

retryTimes :: Int
retryTimes = 10

retract :: Endo [a] -> [a]
retract (Endo f) = f []
{-# INLINE retract #-}

optimize :: Program -> Program
optimize = canonic.simplify.moveOp2P.moveIfP.simplify.moveOp2P.moveIfP.simplify.canonic -- .destructFold

solve :: Int -> [String] -> (Program -> Program -> IO Bool) -> IO (Maybe (Context [Word64] [Word64] [String]))
solve size ops equiv = do
  let ps = genProgram (fromIntegral size) $ map toOp ops
      qs = map optimize ps
      ms = M.fromList $ zip qs ps
      ss = M.keys ms
  putStrLn $ "Size: " ++ show size ++ ", " ++ show ops
  putStrLn $ "Generating " ++ show (length ps) ++ " candidates"
  putStrLn $ "Simplifies to " ++ show (length ss)

  --mapM_ (print . printProgram) ss

  let go i = do
        let n = 256
        vs <- replicateM n randomIO
        let xs = take n $ [0,1,2,3,4,5,15,16,17,65535,65536,65537] ++ vs
        let res  = [ (map (eval p) xs, Endo (p:)) | p <- ss]
            mm   = M.fromListWith mappend res
            freq = maximum $ map (length . retract) $ M.elems mm
        if freq <= mismatchTolerance
          then do
            --let tt = map (retract . snd) $ reverse $ sortBy (compare `on` (length . retract . snd)) $ M.toList mm
            --mapM_ (print . map printProgram) tt
            return $ Just $ Context (\vss -> [ printProgram $ ms M.! cand | cand <- retract $ mm M.! vss]) xs
          else do
            if i > retryTimes
              then do
                let tt = retract $ snd $ head $ reverse $ sortBy (compare `on` (length . retract . snd)) $ M.toList mm
                putStrLn "Cannot divide groups"
                putStrLn $ "Maximum group size: " ++ show (length tt)
                {-
                forM_ (zip [1::Int ..] tt) $ \(ii, x) -> forM_ (zip [1..] tt) $ \(jj, y) -> do
                  when (ii < jj) $ do
                    b <- x `equiv` y
                    when b $ do
                      putStrLn $ printProgram x
                      putStrLn $ printProgram y
                      putStrLn "==="
                -}
                return Nothing
              else go (i + 1)

  go 0 :: IO (Maybe (Context [Word64] [Word64] [String]))

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

printProgram :: Program -> String
printProgram (Program e) = "(lambda (x0) " ++ f e ++ ")" where
  f (Constant n) = show n
  f (Var ixx) = "x" ++ show ixx
  f (If c t ee) = "(if0 " ++ f c ++ " " ++ f t ++ " " ++ f ee ++ ")"
  f (Fold i j l v ee) = "(fold " ++ f l ++ " " ++ f v ++ " (lambda (x" ++ show i ++ " x" ++ show j ++ ") " ++ f ee ++ "))"
  f (Op1 opr ee) = "(" ++ g opr ++ " " ++ f ee ++ ")"
  f (Op2 opr e1 e2) = "(" ++ g opr ++ " " ++ f e1 ++ " " ++ f e2 ++ ")"

  g If0 = "if0"
  g Fold0 = "fold"
  g Not = "not"
  g (Shl n) = "shl" ++ show n
  g (Shr n) = "shr" ++ show n
  g And = "and"
  g Or = "or"
  g Xor = "xor"
  g Plus = "plus"

canonic :: Program -> Program
canonic (Program e) = Program $ canonical e

canonical :: Expression -> Expression
canonical (Constant c) = Constant c
canonical (Var n) = Var n
canonical (If p e1 e2) = If p e1 e2
canonical (Fold x y v e1 e2) = Fold x y v (min e1 e2) (max e1 e2)
canonical (Op1 opr e) = Op1 opr e
canonical (Op2 opr e1 e2) = Op2 opr (min e1 e2) (max e1 e2)

moveIfP :: Program -> Program
moveIfP (Program e) = Program $ moveIf e

moveIf :: Expression -> Expression
moveIf (Constant c) = Constant c
moveIf (Var x) = Var x
moveIf (If p t e) = If (moveIf p) (moveIf t) (moveIf e)
moveIf (Op1 opr e) = case moveIf e of
  If p t ee -> moveIf $ If p (Op1 opr t) (Op1 opr ee)
  e' -> Op1 opr e'
moveIf (Op2 opr e1 e2) = case (moveIf e1, moveIf e2) of
  (If p t e, e2') -> moveIf $ If p (Op2 opr t e2') (Op2 opr e e2')
  (e1', If p t e) -> moveIf $ If p (Op2 opr e1' t) (Op2 opr e1' e)
  (e1', e2') -> Op2 opr e1' e2'
moveIf _ = undefined

moveOp2P :: Program -> Program
moveOp2P (Program e) = Program $ moveOp2 e

moveOp2 :: Expression -> Expression
moveOp2 (If p t e) = If (moveOp2 p) (moveOp2 t) (moveOp2 e)
moveOp2 (Op2 opr e1 e2) = Op2 opr (moveOp2 e1) (moveOp2 e2)
moveOp2 (Op1 Not e) = case moveOp2 e of
  Op2 And  e1 e2 -> Op2 Or (moveOp2 $ Op1 Not e1) (moveOp2 $ Op1 Not e2)
  Op2 Or   e1 e2 -> Op2 And (moveOp2 $ Op1 Not e1) (moveOp2 $ Op1 Not e2)
  Op2 Xor  e1 e2 -> Op2 Xor (moveOp2 $ Op1 Not $ min e1 e2) (max e1 e2)
  Op2 Plus e1 e2 -> Op2 Plus (Constant 1) $ Op2 Plus (moveOp2 $ Op1 Not e1) (moveOp2 $ Op1 Not e2)
  e' -> Op1 Not e'
moveOp2 (Op1 (Shl n) e) = case moveOp2 e of
  Op2 And  e1 e2 -> Op2 And (moveOp2 $ Op1 (Shl n) e1) (moveOp2 $ Op1 (Shl n) e2)
  Op2 Or   e1 e2 -> Op2 Or  (moveOp2 $ Op1 (Shl n) e1) (moveOp2 $ Op1 (Shl n) e2)
  Op2 Xor  e1 e2 -> Op2 Xor (moveOp2 $ Op1 (Shl n) e1) (moveOp2 $ Op1 (Shl n) e2)
  Op2 Plus e1 e2 -> Op2 Plus (moveOp2 $ Op1 (Shl n) e1) (moveOp2 $ Op1 (Shl n) e2)
  e' -> Op1 (Shl n) e'
moveOp2 (Op1 (Shr n) e) = case moveOp2 e of
  Op2 And  e1 e2 -> Op2 And (moveOp2 $ Op1 (Shr n) e1) (moveOp2 $ Op1 (Shr n) e2)
  Op2 Or   e1 e2 -> Op2 Or  (moveOp2 $ Op1 (Shr n) e1) (moveOp2 $ Op1 (Shr n) e2)
  Op2 Xor  e1 e2 -> Op2 Xor (moveOp2 $ Op1 (Shr n) e1) (moveOp2 $ Op1 (Shr n) e2)
  e' -> Op1 (Shr n) e'
moveOp2 e = e

simplify :: Program -> Program
simplify (Program e) = Program $ simplifyE e

simplifyE :: Expression -> Expression
simplifyE (Constant c) = Constant c
simplifyE (Var i) = Var i
simplifyE (If p e1 e2) = case (simplifyE p, simplifyE e1, simplifyE e2) of
  (Constant 0, e1', _) -> e1'
  (Constant _, _, e2') -> e2'
  (_, e1', e2') | canonical e1' == canonical e2' -> e1'

  (c', e1', _) | isZero c' -> e1'
  (c', _, e2') | isNotZero c' -> e2'

  (Var i, e1', e2') -> If (Var i) (subst i (Constant 0) e1') e2'

  (p', e1', e2') -> If p' e1' e2'

simplifyE (Fold x y e1 e2 e3) =
  -- Fold x y (simplifyE e1) (simplifyE e2) (simplifyE e3)
  destructFold x y e1 e2 e3

simplifyE (Op1 opr e) = case (opr, simplifyE e) of
  (_, Constant v) -> Constant $ evalOp1 opr v

  (Not, Op1 Not e') -> simplifyE e'

  (Shl n, Op1 (Shl m) e') -> simplifyE $ Op1 (Shl $ n + m) e'
  (Shl n, _) | n >= 64 -> Constant 0

  (Shr n, Op1 (Shr m) e') -> simplifyE $ Op1 (Shr $ n + m) e'
  (Shr n, _) | n >= 64 -> Constant 0
  (Shr n, e') | lessThan (2^n) e' -> Constant 0

  (_, e') -> Op1 opr e'

simplifyE (Op2 opr e1 e2) = case (opr, simplifyE e1, simplifyE e2) of
  (_, Constant v1, Constant v2) -> Constant $ evalOp2 opr v1 v2

  -- And absorption
  (And, Constant 0, _) -> Constant 0
  (And, _, Constant 0) -> Constant 0
  (And, e1', e2') | e1' == e2' -> e1'
  
  -- And identity
  (And, Constant v, e1') | v == complement 0 -> e1'
  (And, e2', Constant v) | v == complement 0 -> e2'
  
  -- Or identity
  (Or, Constant 0, e2') -> e2'
  (Or, e1', Constant 0) -> e1'
  
  -- Or absorption
  (Or, Constant v, _) | v == complement 0 -> Constant $ complement 0
  (Or, _, Constant v) | v == complement 0 -> Constant $ complement 0
  (Or, e1', e2') | e1' == e2' -> e1'

  -- Xor identity
  (Xor, Constant 0, e2') -> e2'
  (Xor, e1', Constant 0) -> e1'
  
  -- Xor complement
  (Xor, Constant v, e2') | v == complement 0 -> simplifyE $ Op1 Not e2'
  (Xor, e1', Constant v) | v == complement 0 -> simplifyE $ Op1 Not e1'
  
  -- Xor cancellation
  (Xor, e1', e2') | e1' == e2' -> Constant 0

  -- Plus dentity
  (Plus, Constant 0, e2') -> e2'
  (Plus, e1', Constant 0) -> e1'
  (Plus, e1', e2') | e1' == e2' -> simplifyE $ Op1 (Shl 1) e1'

  (_, e1', e2') -> Op2 opr (min e1' e2') (max e1' e2')

destructFold :: Int -> Int -> Expression -> Expression -> Expression -> Expression
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
    e1 = subst x l0 $ subst y e0 e
    e2 = subst x l1 $ subst y e1 e
    e3 = subst x l2 $ subst y e2 e
    e4 = subst x l3 $ subst y e3 e
    e5 = subst x l4 $ subst y e4 e
    e6 = subst x l5 $ subst y e5 e
    e7 = subst x l6 $ subst y e6 e
    e8 = subst x l7 $ subst y e7 e

subst :: Int -> Expression -> Expression -> Expression
subst x ex e = f e where
  f (Constant c) = Constant c
  f (Var i)
    | i == x = ex
    | otherwise = Var i
  f (If xx yy zz) = If (f xx) (f yy) (f zz)
  f (Fold i j xx yy zz) = Fold i j (f xx) (f yy) (f zz)
  f (Op1 opr e1) = Op1 opr (f e1)
  f (Op2 opr e1 e2) = Op2 opr (f e1) (f e2)

lessThan :: Word64 -> Expression -> Bool
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


-- (Op1 (Shr 32) (Op1 (Shl 1) (Op1 Not (Var 0))))
-- (Op1 (Shr 32) (Op1 Not (Op1 (Shl 1) (Var 0))))

isZero, isNotZero :: Expression  -> Bool
isZero e = all (\i -> testB e i == Just False) [0..63]
isNotZero e = any (\i -> testB e i == Just True) [0..63]

testB :: Expression -> Int -> Maybe Bool
testB (Constant c) i = Just $ c `testBit` i
testB (Var _) _ = Nothing
testB (If c t e) i
  | isZero c = testB t i
  | isNotZero c = testB e i
  | otherwise = do
    a <- testB t i
    b <- testB e i
    if a == b then Just a else Nothing
testB (Op1 Not e) i = not <$> testB e i
testB (Op1 (Shl n) e) i
  | i < n = Just False
  | otherwise = testB e (i - n)
testB (Op1 (Shr n) e) i
  | i >= 64 - n = Just False
  | otherwise = testB e (i + n)
testB (Op2 And l r) i = case (testB l i, testB r i) of
  (Just True, Just True) -> Just True
  (Just False, _) -> Just False
  (_, Just False) -> Just False
  _ -> Nothing
testB (Op2 Or l r) i = case (testB l i, testB r i) of
  (Just True, _) -> Just True
  (_, Just True) -> Just True
  (Just False, Just False) -> Just False
  _ -> Nothing
testB (Op2 Xor l r) i =
  (/=) <$> testB l i <*> testB r i
testB (Op2 Plus _ _) _ =
  Nothing -- TODO

eval :: Program -> Word64 -> Word64
eval (Program e) x = evalE e [x]

evalE :: Expression -> [Word64] -> Word64
evalE (Constant c) _ = c
evalE (Var ixx) env = env !! ixx
evalE (If e1 e2 e3) env =
  if evalE e1 env == 0
    then evalE e2 env
    else evalE e3 env
evalE (Fold _ _ e1 e2 e3) env =
  let v = map (`mod` 0x100) $ reverse $ take 8 $ iterate (`shiftR` 8) $ evalE e1 env
  in foldr (\w a -> evalE e3 $ env ++ [w, a]) (evalE e2 env) v
evalE (Op1 opr e) env =
  evalOp1 opr $ evalE e env
evalE (Op2 opr e1 e2) env =
  evalOp2 opr (evalE e1 env) (evalE e2 env)

evalOp1 :: Op -> Word64 -> Word64
evalOp1 Not     = complement
evalOp1 (Shl n) = (`shiftL` n)
evalOp1 (Shr n) = (`shiftR` n)
evalOp1 _ = undefined

evalOp2 :: Op -> Word64 -> Word64 -> Word64
evalOp2 And  = (.&.)
evalOp2 Or   = (.|.)
evalOp2 Xor  = xor
evalOp2 Plus = (+)
evalOp2 _ = undefined

toOp :: String -> Op
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
toOp _ = undefined

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
  (_s, [], e) <- genExpression (size - 1) ops ops 1
  -- guard $ s == size - 1
  return $ Program e

genExpression :: Int -> [Op] -> [Op] -> Int -> [(Int, [Op], Expression)]
genExpression size _ _ _
  | size <= 0 = []
genExpression size ops unused vars
  | TFold `elem` unused = do
    let u0 = unused \\ [TFold, Fold0]
        o0 = ops \\ [TFold, Fold0]
    (s1, u1, e1) <- genExpression (size - 2) o0 u0 vars
    guard $ isSimple e1
    (s2, u2, e2) <- genExpression (size - 2 - s1) o0 u1 vars
    guard $ isSimple e2
    (s3, u3, e3) <- genExpression (size - 2 - s1 - s2) o0 u2 (vars + 2)
    guard $ isSimple e3
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
      guard $ isSimple e1
      (s2, u2, e2) <- genExpression (size - 1 - s1) ops u1 vars
      guard $ isSimple e2
      (s3, u3, e3) <- genExpression (size - 1 - s1 - s2) ops u2 vars
      guard $ isSimple e3
      return (1 + s1 + s2 + s3, u3, If e1 e2 e3)
    folds = do
      guard $ Fold0 `elem` ops
      let u0 = unused \\ [Fold0]
          o0 = ops \\ [Fold0]
      (s1, u1, e1) <- genExpression (size - 2) o0 u0 vars
      guard $ isSimple e1
      (s2, u2, e2) <- genExpression (size - 2 - s1) o0 u1 vars
      guard $ isSimple e2
      (s3, u3, e3) <- genExpression (size - 2 - s1 - s2) o0 u2 (vars + 2)
      guard $ isSimple e3
      return (2 + s1 + s2 + s3, u3, Fold vars (vars+1) e1 e2 e3)
    op1s = do
      opr <- filter isOp1 ops
      let u0 = unused \\ [opr]
      (s1, u1, e1) <- genExpression (size - 1) ops u0 vars
      guard $ isSimple e1
      return (1 + s1, u1, Op1 opr e1)
    op2s = do
      opr <- filter isOp2 ops
      let u0 = unused \\ [opr]
      (s1, u1, e1) <- genExpression (size - 1) ops u0 vars
      guard $ isSimple e1
      (s2, u2, e2) <- genExpression (size - 1 - s1) ops u1 vars
      guard $ isSimple e2
      return (1 + s1 + s2, u2, Op2 opr e1 e2)

isSimple e = e == simplifyE e
