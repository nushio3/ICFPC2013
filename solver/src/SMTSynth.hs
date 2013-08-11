{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings, TemplateHaskell #-}
module SMTSynth where

import Data.SBV
import System.Random
import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH
import Control.Monad
import Data.Reflection
import qualified Data.Text as T
import Text.Printf
import Data.Maybe
import Debug.Trace
import Control.Applicative
import Data.List
import Data.List.Split
import Control.Concurrent.Async
import System.Cmd
import Control.Monad.Trans

import API
import qualified RichBV as BV

data SpecialFlags = SpecialFlags
  { _bonusMode :: Bool
  , _tfoldMode :: Bool }
  deriving (Eq, Ord, Read, Show)  

$(makeLenses ''SpecialFlags)

defaultSpecialFlags :: SpecialFlags
defaultSpecialFlags = SpecialFlags False False 

-- TODO: support fold
-- if0, not, shl1, shr1, shr4, shr16, and, or, xor, plus

oracleIO :: Given Token => T.Text -> [Word64] -> IO [Word64]
oracleIO ident inputs = do
  EvalResponse _estat (Just eout) _emsg <- API.eval $ EvalRequest (Just ident) Nothing $ map (T.pack . printf "0x%016X") inputs
  return eout

oracleGuess :: Given Token => Program -> IO Word8
oracleGuess = undefined

oracleDistinct :: Given Token => T.Text -> BV.Program -> IO (Maybe (Word64, Word64))
oracleDistinct ident p = do
  GuessResponse gstat gval gmsg <- guess $ Guess ident (T.pack $ BV.printProgram p)
  case gstat of
    GuessWin      -> return Nothing
    GuessMismatch -> case gval of
      Just [i, o, _myo] ->
        return $ Just (read $ T.unpack i, read $ T.unpack o)
      _ ->
        fail "tsurapoyo('_`)"
    GuessError -> fail $ show gmsg

instance Applicative Symbolic where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return $ f x

data Opr = If0 | Not | Shl Int | Shr Int | And | Or | Xor | Plus 
  deriving (Eq, Show)

argNum :: Opr -> Int
argNum opr = case opr of
  If0   -> 3
  Not   -> 1
  Shl _ -> 1
  Shr _ -> 1
  And   -> 2
  Or    -> 2
  Xor   -> 2
  Plus  -> 2

type SLoc = SWord8
type Loc = Word8

behave :: SpecialFlags -> [Opr] -> Int -> [SLoc] -> [[SLoc]] -> SWord64 -> SWord64 -> Symbolic ()
behave myFlags oprs size opcs argss i o = do
  let offs = if myFlags^.tfoldMode then 4 else 3 :: Int

  let candss vars = flip map argss $ \[x, y, z] ->
        let var ix = select vars 0 ix
            vx = var x
            vy = var y
            vz = var z
            vx0 = vx .== 0
        in flip map oprs $ \opr -> case opr of
          Not   -> complement vx
          Shl n -> vx `shiftL` n
          Shr n -> vx `shiftR` n
          And   -> vx .&. vy
          Or    -> vx .|. vy
          Xor   -> vx `xor` vy
          Plus  -> vx + vy
          If0   -> ite vx0 vy vz

  if not $ myFlags^.tfoldMode
    then do
      vars <- ([0,1,i]++) <$> sWord64s [ printf "var-%d" ln | ln <- [0::Int ..size-1]]
      constrain $ last vars   .== o
      forM_ (zip3 [offs..] (candss vars) opcs) $ \(ln, cands, opc) ->
        constrain $ vars !! ln .== select cands 0 opc
    else do
      let go [] acc = constrain $ acc .== o
          go (x:xs) acc = do
            vars <- ([0,1,x,acc]++) <$> sWord64s [ printf "var-%d" ln | ln <- [0::Int ..size-1]]
            forM_ (zip3 [offs..] (candss vars) opcs) $ \(ln, cands, opc) ->
              constrain $ vars !! ln .== select cands 0 opc
            go xs (last vars)

      go [ (i `shiftR` (8*s)) .&. 0xff | s <- [0..7]] 0

genProgram :: SpecialFlags -> [Opr] -> Int -> Symbolic SProgram
genProgram myFlags oprs size = do
  let offs = if myFlags ^. tfoldMode then 4 else 3 :: Int

  opcs <- sWord8s [ printf "opc-%d" i | i <- take size [offs ..] ]
  constrain $ bAll (`inRange` (0, fromIntegral $ length oprs-1)) opcs

  --let costs = map (literal . fromIntegral . argNum) oprs

  --b <- liftIO $ randomIO
  --when b $
  --  constrain $ sum [ select costs (1 :: SInt8) opc | opc <- opcs ] - (literal $ fromIntegral $ length oprs) .<= (literal $ fromIntegral size)

  argss <- forM (take size [offs ..]) $ \ln -> do
    args <- sWord8s [ printf "arg-%d-%d" ln i | i <- [0::Int ..2] ]
    constrain $ bAll (.< (literal $ fromIntegral ln)) args
    return args

  -- remove trivial cases
  let opid opr f = case findIndex (==opr) oprs of
        Just ix -> forM_ (zip opcs argss) $ \(opc, [i,j,k]) ->
          constrain $ ((opc .== (literal $ fromIntegral ix)) ==> f i j k)
        Nothing -> return ()

  -- redundant

  -- (if0 0 _) and (if0 1 _) is redundant
  opid If0 $ \i j k -> i ./= 0 &&& i ./= 1 &&& j .< k

  -- (and 0 _) (and _ 0) (and c c)
  opid And $ \i j k -> i ./= 0 &&& j ./= 0 &&& (i .> 1 ||| j .> 1) &&& i .< j

  -- (or 0 _) (or _ 0) (or c c)
  opid Or  $ \i j k -> i ./= 0 &&& j ./= 0 &&& (i .> 1 ||| j .> 1) &&& i .< j

  -- (xor 0 _) (xor _ 0) (xor c c)
  opid Xor $ \i j k -> i ./= 0 &&& j ./= 0 &&& (i .> 1 ||| j .> 1) &&& i .< j

  -- (plus 0 _) (plus _ 0) (plus i j) i >= j
  opid Plus $ \i j k -> i ./= 0 &&& j ./= 0 &&& i .<= j

  -- (shxx 0) (shrx 1)
  opid (Shl 1)  $ \i j k -> i ./= 0
  opid (Shr 1)  $ \i j k -> i ./= 0 &&& i ./= 1
  opid (Shr 4)  $ \i j k -> i ./= 0 &&& i ./= 1
  opid (Shr 16) $ \i j k -> i ./= 0 &&& i ./= 1

  when (myFlags ^. bonusMode) $ do
    let lastOpcI  = length opcs - 1
        lastOpcI2 = length opcs - 2
        
        lastAdrI  = length opcs - 1 + offs
        lastAdrI2 = length opcs - 2 + offs
        
    let    
        white = 0 :: SWord8
        red   = 1 :: SWord8
        green = 2 :: SWord8
        blue  = 4 :: SWord8
        black = 15 :: SWord8 
        
        darker a b = a .&. b .== b
    varColors <- sWord8s [ printf "color-%d" ln | ln <- [0::Int ..size+offs-1]]        
    forM_ (take offs varColors) $ (\c -> constrain $ c .== white)
    forM_ (drop offs varColors) $ 
      (\c -> constrain $ (c .== red) ||| (c.== green) ||| (c.==blue))
        
      
    let candColorThm vars = flip map argss $ \[x, y, z] ->
          let var ix = select varColors 0 ix
              vx = var x
              vy = var y
              vz = var z
          in flip map oprs $ \opr -> case opr of
            Not   -> vx
            Shl n -> vx 
            Shr n -> vx 
            And   -> vx .|. vy
            Or    -> vx .|. vy
            Xor   -> vx .|. vy
            Plus  -> vx .|. vy
            If0   -> ite (vx.==red &&& vy.==green &&& vz.==blue) red black      
    
    forM_ (zip3 [offs..] (candColorThm varColors) opcs) $ \(ln, candThms, opc) ->
         constrain $ (varColors !! ln) `darker` (select candThms 0 opc)
    
    case findIndex (==If0) oprs of
      Nothing ->  error "Bonus problem without If0 \\(>_<)/"  
      Just ifcode ->    
        constrain $ (opcs !! lastOpcI) .== fromIntegral ifcode
    case findIndex (==And) oprs of
      Nothing ->  error "Bonus problem without And \\(>_<)/"        
      Just andcode -> do
        constrain $ (opcs!! lastOpcI2) .== fromIntegral andcode
        constrain $ (argss !! lastOpcI !! 0) .== fromIntegral lastAdrI2
        constrain $ (argss !! lastOpcI2!! 0) .== 1

  return (opcs, argss)

--distinct :: [Opr] -> Int -> [(Word64, Word64)] -> Program -> IO (Maybe Word64)
--distinct oprs size samples (oopcs, oargss) = do
--  let c = do
--        (opcs, argss) <- genProgram oprs size

--        forM_ samples $ \(i, o) ->
--          behave oprs size opcs argss (literal i) (literal o)

--        i <- sWord64 "distinctInput"
--        o0 <- sWord64 "distinctOutput0"
--        o1 <- sWord64 "distinctOutput1"
--        constrain $ o0 ./= o1
--        behave oprs size (map literal oopcs) (map (map literal) oargss) i o0
--        behave oprs size opcs argss i o1

--        return (true :: SBool)

--  res <- sat c
--  generateSMTBenchmarks True "distinct" c
--  return $ fmap read $ lookup "distinctInput" $ parseRes $ show res

findProgram :: Int -> SpecialFlags -> [Opr] -> Int -> [(Word64, Word64)] -> IO Program
findProgram seed myFlags oprs size samples = do
  let c = do
        (opcs, argss) <- genProgram myFlags oprs size
        forM_ samples $ \(i, o) ->
          behave myFlags oprs size opcs argss (literal i) (literal o)
        return (true :: SBool)
  -- generateSMTBenchmarks True "find" c
  res <- satWith (z3 {solver=(solver z3) {options=options (solver z3) ++ ["smt.random_seed="++show seed]}}) c
  
  -- print res
  return $ parseProgram (myFlags^.tfoldMode) $ show res

type Program  = ([Loc],  [[Loc]])
type SProgram = ([SLoc], [[SLoc]])

parseRes :: String -> [(String, String)]
parseRes ss = [ (nam, val) | (nam: "=": val: _) <- map words $ lines ss ]

parseProgram :: Bool -> String -> Program
parseProgram isTFold ss = (opcs, argss) where
  offs = if isTFold then 4 else 3 :: Int
  opcs  = map read $ catMaybes $ takeWhile isJust [ lookup ("opc-" ++ show i) mm | i <- [offs ..] ]
  argss = chunksOf 3 $ map read $ catMaybes $ takeWhile isJust [ lookup ("arg-" ++ show i ++ "-" ++ show j) mm | i <- [offs ..], j <- [0::Int ..2] ]
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

toProgram :: SpecialFlags -> [Opr] -> Program -> BV.Program
toProgram myFlags oprs (opcs, argss) =
  if not $ myFlags^.tfoldMode
    then BV.Program $ last ee
    else BV.Program $ BV.Fold 1 2 (BV.Var 0) (BV.Constant 0) $ last ff
 where
  ee = [BV.Constant 0, BV.Constant 1, BV.Var 0] ++
       [ toExp ee (oprs !! fromIntegral opc) $ map fromIntegral args | (opc, args) <- zip opcs argss ]
  ff = [BV.Constant 0, BV.Constant 1, BV.Var 1, BV.Var 2] ++
       [ toExp ff (oprs !! fromIntegral opc) $ map fromIntegral args | (opc, args) <- zip opcs argss ]

  toExp ls opr [i,j,k] = case opr of
    Not     -> BV.Op1 BV.Not (ls !! i)
    (Shl n) -> BV.Op1 (BV.Shl n) (ls !! i)
    (Shr n) -> BV.Op1 (BV.Shr n) (ls !! i)
    And     -> BV.Op2 BV.And  (ls !! i) (ls !! j)
    Or      -> BV.Op2 BV.Or   (ls !! i) (ls !! j)
    Xor     -> BV.Op2 BV.Xor  (ls !! i) (ls !! j)
    Plus    -> BV.Op2 BV.Plus (ls !! i) (ls !! j)
    If0     -> BV.If (ls !! i) (ls !! j) (ls !! k)
  toExp _ _ _ = error "tsurapoyo"

para :: Int -> (Int -> IO a) -> IO a
para n m = foldl1 rac $ map m [0..n-1] where
  rac a b = do
    x <- race a b
    case x of
      Left v -> return v
      Right v -> return v

synth :: Given Token => Int -> Int -> [T.Text] -> T.Text -> IO ()
synth cpuNum ss ops' ident = if "fold" `elem` ops' then putStrLn "I can not use fold (>_<)" else do
  let (ops, adj, isTFold)
        | "fold" `elem` ops' || "tfold" `elem` ops' =
          (ops' \\ ["tfold"], -6, True)
        | otherwise =
          (ops', -2, False)

  i0 <- map (\x -> x `div` 2 * 2) <$> replicateM 128 randomIO
  i1 <- map (\x -> x `div` 2 * 2 + 1) <$> replicateM 128 randomIO
  let is = concatMap (\(a, b) -> [a, b]) $ zip i0 i1
  os <- oracleIO ident is
  let es = head $ chunksOf 2 $ zip is os

  let 
    myFlags = defaultSpecialFlags
      & bonusMode .~ ("bonus" `elem` ops')
      & tfoldMode .~ ("tfold" `elem` ops')
  
  let oprs = catMaybes $ map toOp ops
  let size = (max 1 $ (ss + adj - sum (map pred $ map argNum oprs)))
       + (if myFlags ^. bonusMode then 0 else 0)
  putStrLn $ "Start synthesis: " ++ T.unpack ident ++ " " ++ show ss ++ " (" ++ show size ++ "), " ++ show ops
  when isTFold $ putStrLn "TFold Mode (>_<);;"

  seeds <- replicateM cpuNum randomIO

  let go es = do
        -- putStrLn "behave..."
        putStrLn $ "inputs: " ++ show es
        progn <- para cpuNum $ \i -> findProgram ((abs $ seeds !! 1)`mod`65536) myFlags oprs (size + i `mod` 3) $ take 5 es
        system "pkill z3"
        putStrLn $ "found: " ++ (BV.printProgram $ toProgram myFlags oprs progn)
        o <- oracleDistinct ident $ toProgram myFlags oprs progn
        case o of
          Nothing -> do
            putStrLn "Accepted: yatapo-(^_^)!"
            if (myFlags ^. bonusMode)
               then system "wget http://botis.org:9999/play/AC.wav -O /dev/null"
               else system "wget http://botis.org:9999/play/meow.wav -O /dev/null"
            return ()
          Just oo -> do
            putStrLn $ "distinct: " ++ show oo
            go (oo:es)

        ---- a <- distinct oprs size e progn
        --case a of
        --  Nothing -> putStrLn $ "Answer found!!: " ++ show progn
        --  Just f -> do
        --    [g] <- oracleIO ident [f]
        --    go ((f, g):e)

  go es
