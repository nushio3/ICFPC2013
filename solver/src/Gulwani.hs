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