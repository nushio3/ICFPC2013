module Gulwani where
import Data.Bits

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

deInstB :: Ins -> Val -> Val -> Val -> SBool
deInstB inst a b t = select
 [ t .== 0             -- 0    
 , t .== 1             -- 1
 , t .== complement a  -- 2    
 , t .== shiftL a 2    -- 3    
 , t .== shiftR a 1    -- 4    
 , t .== shiftR a 4    -- 5    
 , t .== shiftR a 16   -- 6    
 , t .== a .&. b       -- 7    
 , t .== a .|. b       -- 8    
 , t .== a `xor` b     -- 9    
 , t .== a + b         --10    
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
    deInstB inst0 loadResA0 loadResB0 y0
