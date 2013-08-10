module Gulwani3 where
import Data.Bits
import Control.Monad
import Text.Printf
import Data.List

import Data.SBV
import BV (Op1, Op2)
import SBV (SBitVector)



type Adr = SWord8
type Ins = SWord8
type Val = SWord64

deInstB :: Int -> Val -> Val -> Val -> Val -> SBool
deInstB inst v a b t = (!!inst)
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
 ] 

behaveBN :: Int -> Val -> Val -> Symbolic SBool
behaveBN size x0 y0 = do
  let allAddrs = [0..n-1] 
      
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
