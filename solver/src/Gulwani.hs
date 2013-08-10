module Gulwani where
import Data.Bits

import Data.SBV
import BV (Op1, Op2)
import SBV (SBitVector)

data GInst a
  = GIf a a a
  | G1 Op1 a 
  | G2 Op2 a a

behave1 :: SBitVector -> SBitVector -> SWord8 -> SBool
behave1 x y inst = 
  select 
    [ y .== complement x
    , y .== x `shiftL`1 
    , y .== x `shiftR`1 
    , y .== x `shiftR`4 
    , y .== x `shiftR`16
    , y .== 1
    , y .== 0
    ] 
    (false) inst
  
behave2 :: SBitVector -> SBitVector    
   -> SBitVector    
   -> SWord8   -> SWord8 -> SBool
behave2 x y o1 inst0 inst1 =   
  behave1 x o1 inst0 &&&
  behave1 o1 y inst1  
  
behave3 :: SBitVector -> SBitVector    
   -> SBitVector    
   -> SBitVector       
   -> SWord8   -> SWord8 -> SWord8 -> SBool
behave3 x y o1 o2 inst0 inst1 inst2 =   
  behave1 x o1 inst0 &&&
  behave1 o1 o2 inst1 &&&
  behave1 o2 y inst2    
  
  
deInstB :: SWord8 -> SBitVector -> SBitVector -> SBitVector
         -> SBool
deInstB inst a b ret =
  select 
    [ ret .== 0
    , ret .== 1
    , ret .== complement a
    , ret .== shiftL a 1
    , ret .== shiftR a 1
    , ret .== shiftR a 4
    , ret .== shiftR a 16
    , ret .== a .&. b
    , ret .== a .|. b
    , ret .== a `xor` b
    , ret .== a + b
    ] 
    (false) inst

  
behaveB1 :: SBitVector 
         -> SBitVector 
         -> SWord8 -> SWord8 -> SWord8
         -> SBool
behaveB1 x y inst a b = 
  let load :: SWord8 -> SBitVector
      load addr = select [x] 0 addr in      
  (a .== 0) &&&     
  (b .== 0) &&&     
  deInstB inst (load a) (load b) y
  
behaveB2 :: SBitVector 
         -> SBitVector 
         -> SWord8 -> SWord8 -> SWord8
         -> SWord8 -> SWord8 -> SWord8
         -> SBitVector 
         -> SBool
behaveB2 x y 
  inst0 a0 b0
  inst1 a1 b1 
  tmp0
  = 
  let load :: SWord8 -> SBitVector
      load addr = select [x,tmp0] 0 addr in      
  (a0 .<= 0) &&&     
  (b0 .<= 0) &&&     
  (a1 .<= 1) &&&     
  (b1 .<= 1) &&&     
  deInstB inst0 (load a0) (load b0) tmp0  &&&
  deInstB inst1 (load a1) (load b1) y    