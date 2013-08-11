{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
module SMTSynth where


import Data.SBV
import System.Random
import Control.Monad
import Data.Reflection
import qualified Data.Text as T
import Text.Printf
import System.Environment
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans
import Data.List.Split

import API

-- TODO: support fold
-- if0, not, shl1, shr1, shr4, shr16, and, or, xor, plus

oracleIO :: Given Token => T.Text -> [Word64] -> IO [Word64]
oracleIO ident inputs = do
  EvalResponse _estat (Just eout) _emsg <- API.eval $ EvalRequest (Just ident) Nothing $ map (T.pack . printf "0x%016X") inputs
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

instance Applicative Symbolic where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return $ f x

--type Addr = SWord16
--type Val = SWord64

data Opr = If0 | Not | Shl Int | Shr Int | And | Or | Xor | Plus
  deriving (Eq, Show)

--argNum :: Opr -> Int
--argNum opr = case opr of
--  If0   -> 3
--  Not   -> 1
--  Shl _ -> 1
--  Shr _ -> 1
--  And   -> 2
--  Or    -> 2
--  Xor   -> 2
--  Plus  -> 2

--data Alloc a = Alloc { _aOut :: a, _aArg :: [a] }

--allocs :: Alloc a -> [a]
--allocs (Alloc a is) = a : is

--behave :: [Opr]
--       -> [Alloc Addr]
--       -> [(Word64, Word64)]
--       -> Symbolic SBool
--behave oprs locs samples = do
--  forM_ samples $ \(i, o) ->
--    phiFunc i o
--  return true

--  where
--    len = length oprs

--    phiFunc :: Word64 -> Word64 -> Symbolic ()
--    phiFunc i o = do
--      vars@(v0:v1:v2:_) <- mapM (mkAlloc $ sWord64 "var") oprs
--      constrain $ _aOut v0 .== 0
--      constrain $ _aOut v1 .== 1
--      constrain $ _aOut v2 .== literal i
--      forM_ (zip locs vars) $ \(loc, var) -> do
--        constrain $ ite (_aOut loc .== literal (fromIntegral len - 1)) (_aOut var .== literal o) true
--      forM_ (zip oprs vars) $ \(opr, var) ->
--        constrain $ oprSat opr var

--      let as = zip (concatMap allocs locs) (concatMap allocs vars)
--      liftIO $ print $ length as
--      let conss = [ (a .== c) ==> (b .== d)
--                  | (i, (a, b)) <- zip [0..] as
--                  , (j, (c, d)) <- zip [0..] as
--                  , i < j ]
--      mapM_ constrain conss

type SLoc = SWord8
type Loc = Word8

behave :: [Opr] -> Int -> [SLoc] -> [[SLoc]] -> SWord64 -> SWord64 -> Symbolic ()
behave oprs size opcs argss i o = do
  vars <- sWord64s [ printf "var-%d" ln | ln <- [0::Int ..size+3-1]]
  let var ix = select vars 0 ix

  constrain $ (vars !! 0) .== 0
  constrain $ (vars !! 1) .== 1
  constrain $ (vars !! 2) .== i
  constrain $ last vars   .== o

  let candss = flip map argss $ \[x, y, z] ->
        let vx = var x
            vy = var y
            vz = var z
        in flip map oprs $ \opr -> case opr of
          Not   -> complement vx
          Shl n -> vx `shiftL` n
          Shr n -> vx `shiftR` n
          And   -> vx .&. vy
          Or    -> vx .|. vy
          Xor   -> vx `xor` vy
          Plus  -> vx + vy
          If0   -> ite (vx .== 0) vy vz

  forM_ (zip3 [3..] candss opcs) $ \(ln, cands, opc) ->
    constrain $ vars !! ln .== select cands 0 opc

distinct :: [Opr] -> Int -> [(Word64, Word64)] -> Program -> IO (Maybe Word64)
distinct oprs size samples (oopcs, oargss) = do
  let c = do
        opcs <- sWord8s [ printf "opc-%d" i | i <- take size [3::Int ..] ]
        constrain $ bAll (`inRange` (0, fromIntegral $ length oprs-1)) opcs

        argss <- forM (take size [3::Int ..]) $ \ln -> do
          args <- sWord8s [ printf "arg-%d-%d" ln i | i <- [0::Int ..2] ]
          constrain $ bAll (.< (literal $ fromIntegral ln)) args
          return args

        forM_ samples $ \(i, o) ->
          behave oprs size opcs argss (literal i) (literal o)

        i <- sWord64 "distinctInput"
        o0 <- sWord64 "distinctOutput0"
        o1 <- sWord64 "distinctOutput1"
        constrain $ o0 ./= o1
        behave oprs size (map literal oopcs) (map (map literal) oargss) i o0
        behave oprs size opcs argss i o1

        return (true :: SBool)

  res <- sat c
  return $ fmap read $ lookup "distinctInput" $ parseRes $ show res

findProgram :: [Opr] -> Int -> [(Word64, Word64)] -> IO Program
findProgram oprs size samples = do
  print (oprs, size, samples)
  let c = do
        opcs <- sWord8s [ printf "opc-%d" i | i <- take size [3::Int ..] ]
        constrain $ bAll (`inRange` (0, fromIntegral $ length oprs-1)) opcs

        argss <- forM (take size [3::Int ..]) $ \ln -> do
          args <- sWord8s [ printf "arg-%d-%d" ln i | i <- [0::Int ..2] ]
          constrain $ bAll (.< (literal $ fromIntegral ln)) args
          return args

        forM_ samples $ \(i, o) ->
          behave oprs size opcs argss (literal i) (literal o)

        return (true :: SBool)
  -- generateSMTBenchmarks True "test" c
  res <- sat c
  return $ parseProgram $ show res

type Program = ([Loc], [[Loc]])

parseRes :: String -> [(String, String)]
parseRes ss = [ (name, val) | (name: "=": val: _) <- map words $ lines ss ]

parseProgram :: String -> Program
parseProgram ss = (opcs, argss) where
  opcs  = map read $ catMaybes $ takeWhile isJust [ lookup ("opc-" ++ show i) mm | i <- [3..] ]
  argss = chunksOf 3 $ map read $ catMaybes $ takeWhile isJust [ lookup ("arg-" ++ show i ++ "-" ++ show j) mm | i <- [3..], j <- [0..2] ]
  mm    = parseRes ss

toOp :: T.Text -> Maybe Opr
toOp "not"   = Just Not
toOp "shl1"  = Just (Shl 1)
toOp "shr1"  = Just (Shr 1)
toOp "shr4"  = Just (Shr 4)
toOp "shr16" = Just (Shr 16)
toOp "and"   = Just And
toOp "or"    = Just Or
toOp "xor"   = Just Xor
toOp "plus"  = Just Plus
toOp "if0"   = Just If0
toOp _       = Nothing

synth :: Given Token => Int -> [T.Text] -> T.Text -> IO ()
synth size ops ident = do
  putStrLn $ "Start synthesis: " ++ show size ++ ", " ++ show ops

  let initNum = 4
  -- is <- zipWith (\i v -> v `div` initNum * initNum + i) [0..] <$> replicateM (fromIntegral initNum) randomIO
  let is = [0,1,2,4]
  os <- oracleIO ident is

  let oprs = catMaybes $ map toOp ops

  let go e = do
        putStrLn "behave..."
        progn <- findProgram oprs (size-1) e
        putStrLn $ "found: " ++ show progn
        a <- distinct oprs (size-1) e progn
        putStrLn $ "distinct: " ++ show a
        case a of
          Nothing -> putStrLn $ "Answer found!!: " ++ show progn
          Just f -> do
            [g] <- oracleIO ident [f]
            go ((f, g):e)
  go $ zip is os
  undefined

