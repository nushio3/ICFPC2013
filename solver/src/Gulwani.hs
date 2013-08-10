module Gulwani where

import Data.SBV
import BV (Op1, Op2)
import SBV (SBitVector)

data GInst a
  = GIf a a a
  | G1 Op1 a 
  | G2 Op2 a a

behave :: SWord8 -> SBitVector -> SBitVector -> Symbolic SBool
behave inst x y = do
  o1 <- symbolic
  constrain $
    select 
    [ complement o1 .== x
    , o1 `shiftLeft`1 .== x
    , o1 `shiftRight`1 .== x
    , o1 `shiftRight`4 .== x
    , o1 `shiftRight`16 .== x
    ] 
    (false) inst
  return $ o1 .== y
  