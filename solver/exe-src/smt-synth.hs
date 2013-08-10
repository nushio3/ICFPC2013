{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}

import Data.SBV
import System.Random
import Control.Monad
import Data.Reflection
import qualified Data.Text as T
import Text.Printf
import System.Environment
import Data.Maybe
import Control.Applicative

import API

-- TODO: support fold
-- if0, not, shl1, shr1, shr4, shr16, and, or, xor, plus

oracleIO :: Given Token => T.Text -> [Word64] -> IO [Word64]
oracleIO ident inputs = do
  EvalResponse estat (Just eout) emsg <- API.eval $ EvalRequest (Just ident) Nothing $ map (T.pack . printf "0x%016X") inputs
  return eout

--valid :: [SWord8] -> SBool
--valid oprs = go 0 oprs (0 :: SInt8) where
--  go _ [] s = s .== 1
--  go xx (opr:rest) s = ite (s .< xx) false $ go 1 rest (select rs 0 opr) where
--    rs =
--      [ s
--      , s + 1, s + 1, s + 1
--      , s - 2
--      , s, s, s, s, s
--      , s - 1, s - 1, s - 1, s - 1
--      ]

--interp :: [SWord8] -> SWord64 -> SWord64
--interp oprs i = go oprs $ replicate maxlen 0 where
--  maxlen = length oprs
--  go [] (v:_) = v
--  go (opr:rest) ss = ite (opr .== 0) 0 $ go rest $
--    ite (opr .== 1 ) (take maxlen $ 0:ss) $ -- const 0
--    ite (opr .== 2 ) (take maxlen $ 1:ss) $ -- const 1
--    ite (opr .== 3 ) (take maxlen $ i:ss) $ -- var
--    ite (opr .== 4 ) ((case splitAt 3 ss of ([c,t,e], sss) -> ite (c.==0) t e:sss++[0,0]; _ -> ss)) $ -- if0
--    ite (opr .== 5 ) ((case splitAt 1 ss of ([x], sss) -> complement x:sss; _ -> ss)) $ -- not
--    ite (opr .== 6 ) ((case splitAt 1 ss of ([x], sss) -> x `shiftL` 1:sss; _ -> ss)) $ -- shl1
--    ite (opr .== 7 ) ((case splitAt 1 ss of ([x], sss) -> x `shiftR` 1:sss; _ -> ss)) $ -- shr1
--    ite (opr .== 8 ) ((case splitAt 1 ss of ([x], sss) -> x `shiftR` 4:sss; _ -> ss)) $ -- shr4
--    ite (opr .== 9 ) ((case splitAt 1 ss of ([x], sss) -> x `shiftR` 16:sss; _ -> ss)) $ -- shr16
--    ite (opr .== 10) ((case splitAt 2 ss of ([x, y], sss) -> (x .&. y) : sss++[0]; _ -> ss)) $ -- and
--    ite (opr .== 11) ((case splitAt 2 ss of ([x, y], sss) -> (x .|. y) : sss++[0]; _ -> ss)) $ -- or
--    ite (opr .== 12) ((case splitAt 2 ss of ([x, y], sss) -> (x `xor` y) : sss++[0]; _ -> ss)) $ -- xor
--    ite (opr .== 13) ((case splitAt 2 ss of ([x, y], sss) -> (x + y) : sss++[0]; _ -> ss)) $ -- plus
--    ss

--behave :: Int -> Int -> [(Word64, Word64)] -> IO [Word8]
--behave size opc samples = do
--  generateSMTBenchmarks True "test" $ do
--    oprs <- mkExistVars size
--    mapM_ (\opr -> constrain $ opr .<= 13) oprs
--    constrain $ valid oprs
--    mapM_ (\(i, o) -> constrain $ interp oprs (literal i) .== literal o) samples
--    return (true :: SBool)
--  fail "hoge"

--  ret <- sat $ do
--    oprs <- mkExistVars size
--    mapM_ (\opr -> constrain $ opr .<= 13) oprs
--    constrain $ valid oprs
--    mapM_ (\(i, o) -> constrain $ interp oprs (literal i) .== literal o) samples
--    return (true :: SBool)
--  case getModel ret of
--    Right (_, v) -> return v
--    Left err -> fail err

--distinct :: Int -> Int -> [(Word64, Word64)] -> [Word8] -> IO (Maybe Word64)
--distinct size opc samples l = do
--  ret <- sat $ do
--    oprs <- mkExistVars size
--    mapM_ (\opr -> constrain $ opr .<= 13) oprs
--    constrain $ valid oprs
--    mapM_ (\(i, o) -> constrain $ interp oprs (literal i) .== literal o) samples
--    i <- exists_
--    constrain $ interp oprs i ./= interp (map literal l) i
--    return (true :: SBool)
--  case getModel ret of
--    Right (_, v) -> return $ Just v
--    Left _ -> return Nothing

type Operator1 = SWord64 -> SWord64
type Operator2 = SWord64 -> SWord64 -> SWord64
type Operator3 = SWord64 -> SWord64 -> SWord64 -> SWord64

behave :: Int
          -> [T.Text] -> [Operator1]
          -> [T.Text] -> [Operator2]
          -> [T.Text] -> [Operator3]
          -> [(Word64, Word64)]
          -> IO [Word8]
behave size name1s op1s name2s op2s name3s op3s samples = do
  let total = length op1s + length op2s + length op3s
      opmax = size - 1
      progsize = total * opmax

      prog1 = concatMap (replicate opmax) op1s
      prog2 = concatMap (replicate opmax) op2s
      prog3 = concatMap (replicate opmax) op3s

  let pp = do
        args1 <- forM [0..length prog1-1] $ \i ->
          sInt16s [ "arg1-" ++ show i ++ "-" ++ show j | j <- [0..0] ]
        args2 <- forM [0..length prog2-1] $ \i ->
          sInt16s [ "arg2-" ++ show i ++ "-" ++ show j | j <- [0..1] ]
        args3 <- forM [0..length prog3-1] $ \i ->
          sInt16s [ "arg3-" ++ show i ++ "-" ++ show j | j <- [0..2] ]

        let allArgs = args1 ++ args2 ++ args3

        l <- sInt16s $ [ "line-" ++ show i | i <- [0..progsize-1] ]
        result <- sInt16 "result"
        constrain $ inRange result (0, literal $ fromIntegral $ progsize + 3 - 1)

        -- constraints for l
        constrain $ bAll (\ln -> inRange ln (3, literal (fromIntegral $ progsize + 3 - 1))) l
        constrain $ allDifferent l

        -- constraints for argix
        forM_ (zip l allArgs) $ \(lineno, arg) -> do
          forM_ arg $ \a -> constrain $ inRange a (0, lineno - 1)

        -- let go (x:xs@(_:_)) = (x .<= 2 ||| bAll (x ./=) xs) &&& go xs
        --     go _ = true
        -- constrain $ go $ concat allArgs

        -- constrain for examples
        forM_ (zip [1..] samples) $ \(ii, (i, o)) -> do
          vars <- sWord64s ["var-" ++ show ii ++ "-" ++ show i | i <- [0..progsize+3-1] ]
          let var ix = select vars 0 (ix :: SInt16)

          constrain $ (vars !! 0) .== 0
          constrain $ (vars !! 1) .== 1
          constrain $ (vars !! 2) .== literal i
          constrain $ var result  .== literal o

          -- constraints for operation
          constrain
            $ bAll (\(v,opr,[a1]) -> v .== opr (var a1))
            $ zip3 (drop 3 vars) prog1 args1
          constrain
            $ bAll (\(v,opr,[a1,a2]) -> v .== opr (var a1) (var a2))
            $ zip3 (drop (3+length prog1) vars) prog2 args2
          constrain
            $ bAll (\(v,opr,[a1,a2,a3]) -> v .== opr (var a1) (var a2) (var a3))
            $ zip3 (drop (3+length prog1+length prog2) vars) prog3 args3

        return (true :: SBool)

  generateSMTBenchmarks True "test" pp
  res <- sat pp

  let ss = [ (name, val) | (name: "=": val: _) <- map words $ lines $ show res ]
  print ss

  forM_ (zip [0..] $ concatMap (replicate opmax) name1s) $ \(i, name) -> do
    printf "v%s = %s v%s\n"
      (fromJust $ lookup (printf "line-%d" (i :: Int)) ss)
      (T.unpack name)
      (fromJust $ lookup (printf "arg1-%d-0" i) ss)
  forM_ (zip [0..] $ concatMap (replicate opmax) name2s) $ \(i, name) -> do
    printf "v%s = %s v%s v%s\n"
      (fromJust $ lookup (printf "line-%d" (length name1s + i :: Int)) ss)
      (T.unpack name)
      (fromJust $ lookup (printf "arg2-%d-0" i) ss)
      (fromJust $ lookup (printf "arg2-%d-1" i) ss)
  forM_ (zip [0..] $ concatMap (replicate opmax) name3s) $ \(i, name) -> do
    printf "v%s = %s v%s v%s v%3\n"
      (fromJust $ lookup (printf "line-%d" (length name1s + length name2s + i :: Int)) ss)
      (T.unpack name)
      (fromJust $ lookup (printf "arg3-%d-0" i) ss)
      (fromJust $ lookup (printf "arg3-%d-1" i) ss)
      (fromJust $ lookup (printf "arg3-%d-2" i) ss)

  printf "ret = v%s\n" $ fromJust $ lookup "result" ss

  undefined

distinct :: Int -> Int -> [(Word64, Word64)] -> [Word8] -> IO (Maybe Word64)
distinct = undefined

toOp1 :: T.Text -> Maybe Operator1
toOp1 "not"   = Just $ complement
toOp1 "shl1"  = Just $ \a -> a `shiftL` 1
toOp1 "shr1"  = Just $ \a -> a `shiftR` 1
toOp1 "shr4"  = Just $ \a -> a `shiftR` 4
toOp1 "shr16" = Just $ \a -> a `shiftR` 16
toOp1 _       = Nothing

toOp2 :: T.Text -> Maybe Operator2
toOp2 "and"  = Just (.&.)
toOp2 "or"   = Just (.|.)
toOp2 "xor"  = Just xor
toOp2 "plus" = Just (+)
toOp2 _      = Nothing

toOp3 :: T.Text -> Maybe Operator3
toOp3 "if0" = Just $ \c t e -> ite (c .== 0) t e
toOp3 _     = Nothing

synth :: Given Token => Int -> [T.Text] -> T.Text -> IO ()
synth size ops ident = do
  putStrLn $ "Start synthesis: " ++ show size ++ ", " ++ show ops

  let initNum = 4
  is <- zipWith (\i v -> v `div` initNum * initNum + i) [0..] <$> replicateM (fromIntegral initNum) randomIO
  os <- oracleIO ident is

  let op1s = catMaybes $ map toOp1 ops
      op2s = catMaybes $ map toOp2 ops
      op3s = catMaybes $ map toOp3 ops

  let go e = do
        putStrLn "behave..."
        l <- behave size (filter (isJust.toOp1) ops) op1s (filter (isJust.toOp2) ops) op2s (filter (isJust.toOp3) ops) op3s e
        putStrLn $ "found: " ++ show l
        a <- distinct size 14 e l
        putStrLn $ "distinct: " ++ show a
        case a of
          Nothing -> putStrLn $ "found: " ++ show l
          Just f -> do
            [g] <- oracleIO ident [f]
            go ((f, g):e)
  go $ zip is os

main :: IO ()
main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
  ("training" : level : _) -> do
    TrainingProblem prog ident size ops <- train $ TrainRequest (read level) []
    putStrLn $ "Expected answer: " ++ T.unpack prog
    synth size ops ident
