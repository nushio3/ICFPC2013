{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Instack where

import Data.SBV
import BV (BitVector)
import SBV (SBitVector)

type Val = SBitVector
type Adr = SWord8

data Z = Z deriving (Show, Eq, Ord)
data S n = S n deriving (Show, Eq, Ord)

class ToNum a where
  toNum :: Num n => a -> n

instance ToNum Z where toNum _ = 0
instance ToNum n => ToNum (S n) 
  where toNum (S n) = 1 + toNum n


data Inst
     = C0 | C1 | Var
     | Not | Shl1 | Shr1 | Shr4 | Shr16
     | And | Or | Xor | Plus
     | Ite
  deriving (Show, Eq, Ord)
           

thmInst :: SWord8 
  -> SBitVector -> SBitVector
  -> SBitVector
  -> SBool
thmInst inst a b ret =
  select 
    [ ret .== 0
    , ret .== 1
    , ret .== a      
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


type Memory = Adr -> Val

type SuccBehave x = 
  (Val -> Adr ->  Adr ->  Adr -> x)

type SuccBeh2 f x = 
  f (Val -> Adr ->  Adr ->  Adr -> x)


class (ToNum m) => Instack m where
  type ThmBehave m :: *  
  thmBehave :: m -> ThmBehave m
  withLoader :: m -> 
    (Memory -> SuccBeh2  a) -> SuccBeh2 a

instance Instack Z where
  type ThmBehave Z = Val -> SBool
  thmBehave _ = const true
  withLoader _ k = k (const 0)
  
instance (Instack n) => Instack (S n) where
  type ThmBehave (S n) = 
  thmBehave (S n) = undefined
  
  
  
  withLoader (S n) k t i a b = (withLoader n $  
    (\mem-> k (newMem mem))) t i a b
      where
        newMem mem adr =
           ite (adr .== toNum n) t (mem adr)


