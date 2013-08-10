module Gulwani where
import Data.Bits
import Control.Monad
import Text.Printf
import Data.List

import Data.SBV
import BV (Op1, Op2)
import SBV (SBitVector)


data GInst a
  = GIf a a a
  | G1 Op1 a 
  | G2 Op2 a a

behave1 :: SBitVector -> SBitVector -> Symbolic SBool
behave1 x y = do
  inst <- (exists "inst0" :: Symbolic SWord8)
  return $ select 
    [ y .== complement x
    , y .== x `shiftL`1 
    , y .== x `shiftR`1 
    , y .== x `shiftR`4 
    , y .== x `shiftR`16
    , y .== 1
    , y .== 0
    ] 
    (false) inst


behave2 :: SBitVector -> SBitVector -> Symbolic SBool
behave2 x0 y0 = do
  inst0 <- (exists "inst0" :: Symbolic SWord8)
  tmp0 <- (exists "tmp0" :: Symbolic SWord64)
  inst1 <- (exists "inst1" :: Symbolic SWord8)
  
  let deInst inst x y = select 
        [ y .== complement x
        , y .== x `shiftL`1 
        , y .== x `shiftR`1 
        , y .== x `shiftR`4 
        , y .== x `shiftR`16
        , y .== 1
        , y .== 0
        ] 
        (false) inst
  
  return $ deInst inst0 x0 tmp0
        &&& deInst inst1 tmp0 y0 
        
        

type Adr = SWord8
type Ins = SWord8
type Val = SWord64

deInstB :: Val ->  Ins -> Val -> Val -> Val -> SBool
deInstB v  inst a b t = select
 [ t .== 0             -- 0    
 , t .== 1             -- 1
 , t .== v             -- 2 
 , t .== complement a  -- 3    
 , t .== shiftL a 1    -- 4    
 , t .== shiftR a 1    -- 5    
 , t .== shiftR a 4    -- 6    
 , t .== shiftR a 16   -- 7    
 , t .== a .&. b       -- 8    
 , t .== a .|. b       -- 9    
 , t .== a `xor` b     --10    
 , t .== a + b         --11    
 ] false inst

behaveB1 :: Val -> Val -> Symbolic SBool
behaveB1 x0 y0 = do
  let 
    load myAddr tgtAddr r 
      = ite (tgtAddr .== 0) (r .== x0) false
  
  inst0 <- (exists "inst0" :: Symbolic Ins)
  a0 <- (exists "a0" :: Symbolic Adr)
  b0 <- (exists "b0" :: Symbolic Adr)
  loadResA0 <- (exists "la0" :: Symbolic Val)
  loadResB0 <- (exists "lb0" :: Symbolic Val)
  
  return $
    load 0 a0 loadResA0 &&&
    load 0 b0 loadResB0 &&&
    deInstB x0 inst0 loadResA0 loadResB0 y0


behaveBN :: Int -> Val -> Val -> Symbolic SBool
behaveBN n x0 y0 = do
  let allAddrs = [0..n-1] 
      
  insts <- forM allAddrs 
    (\i -> exists (printf "inst%d" i) :: Symbolic Ins)
  addrAs <- forM allAddrs 
    (\i -> exists (printf "a%d" i) :: Symbolic Adr)
  addrBs <- forM allAddrs 
    (\i -> exists (printf "b%d" i) :: Symbolic Adr)
  retAs <- forM allAddrs 
    (\i -> exists (printf "ra%d" i) :: Symbolic Val)
  retBs <- forM allAddrs 
    (\i -> exists (printf "rb%d" i) :: Symbolic Val)
  tmpVals <- forM allAddrs 
    (\i -> exists (printf "tmp%d" i) :: Symbolic Val)
    

  let 
    load myAddr tgtAddr r 
      = ite (tgtAddr .>= 0 &&& tgtAddr .< myAddr) 
        (select (map (.== r) tmpVals) false tgtAddr) false

    loadOrThrow1 inst myAddr tgtAddr r
      = (inst .<= 2) ||| load myAddr tgtAddr r
    loadOrThrow2 inst myAddr tgtAddr r
      = (inst .<= 7) ||| load myAddr tgtAddr r
  
  
  return $ 
    (y0 .== tmpVals !! (n-1)) &&&
    (bAnd $ zipWith4 loadOrThrow1 insts [0..] addrAs retAs) &&&
    (bAnd $ zipWith4 loadOrThrow2 insts [0..] addrBs retBs) &&&
    (bAnd $ zipWith4 (deInstB x0) insts retAs retBs tmpVals)
      
--    load 0 a0 loadResA0 &&&
--    load 0 b0 loadResB0 &&&
--    deInstB inst0 loadResA0 loadResB0 y0
