{-# LANGUAGE TemplateHaskell #-}
module Gulwani4 where

import Control.Applicative hiding (Const)
import Control.Lens hiding ((.>))
import Control.Lens.TH
import Control.Monad
import Data.Bits
import Data.SBV
import Text.Printf

type Addr = SWord16
type Val = SWord64

type LProgram = (Addr, [Opaddr Addr])

data Op = Const Int | Var 
        | Not | Shl Int | Shr Int
        | And | Or | Xor | Plus
        | If0 
                           deriving (Eq, Ord, Show, Read)

symbolicOp :: Int -> Op -> Int -> Symbolic (Opaddr Addr)
symbolicOp i opr arity = do
  oaddr <- (exists (printf "oa-%d" i) :: Symbolic Addr) 
  iaddrs <- forM [0..arity-1] $ \j -> 
    (exists (printf "ia-%d-%d" i j) :: Symbolic Addr) 
  return $ Opaddr opr oaddr iaddrs
  
data Opaddr a = Opaddr 
  {_inst :: Op, _ovar :: a, _ivars :: [a]}
    
$(makeLenses ''Opaddr)  


testProg :: Symbolic LProgram
testProg = do
  ret <- symbolic "returnAddr"
  a <- symbolicOp 0 Var 0
  b <- symbolicOp 1 Plus 2
  c <- symbolicOp 2 Plus 2
  return (ret, [a,b,c])


test a b = do
  prog <- testProg
  phiFunc prog a b


phiFunc :: LProgram -> Val -> Val -> Symbolic SBool
phiFunc (retAddr, library) alpha beta = do
  let n = length library
      allAddrs = [0..n-1]  
      
  outVals <- (\k -> zipWithM k allAddrs library) $
    \i opaddr -> (exists (printf "out-%d" i) :: Symbolic Val) 

  varLib <- (\k -> zipWithM k allAddrs library) $
    \i opaddr -> do
      ivals <- 
        sequence [ exists (printf "in-%d-%d" i j) :: Symbolic Val 
                 | j <- [0..length (opaddr^.ivars)-1]]
      return $ Opaddr (opaddr^.inst) (outVals!!i) ivals                 
      
  let phiConn :: (Addr,Val) -> (Addr, Val) -> SBool
      phiConn (a1,v1) (a2,v2) = (a1 .== a2) ==> (v1 .== v2)
      
      phiAddrBound :: Addr -> SBool
      phiAddrBound a = 0 .<= a &&& a .< (fromIntegral n)

  let phiLib :: Opaddr Val -> SBool
      phiLib opaddr = 
        (opaddr^.ovar) .==
        case (opaddr^.inst, opaddr^.ivars) of
          (Const n, [])  -> fromIntegral n
          (Var, [])      -> alpha
          (Or , [a,b])   -> a .&. b
          (Xor , [a,b])  -> a .|. b
          (Plus , [a,b]) -> a + b
          (If0, [a,b,c]) -> ite (a .== 0) b c
          _              -> undefined
          
  let allAVs :: [(Addr,Val)]
      allAVs = (retAddr, beta):avLibs
      
      nAllAVs = length allAVs
      
      avLibs = concat $
        (\k -> zipWith k library varLib) $ 
          \opA opV -> (opA^.ovar, opV^.ovar) : 
                      zip (opA^.ivars) (opV^.ivars)
      outAddrs :: [Addr]
      outAddrs = map (^.ovar) library
  
      phiAcyc :: Opaddr Addr -> SBool
      phiAcyc oa = bAll (.<(oa^.ovar)) (oa^.ivars)
  
  return $ 
    bAnd (map (phiAddrBound . fst) allAVs) &&&
    bAnd [(outAddrs!!i)./= (outAddrs!!j) 
         | i <- [0..n-1], j <- [i+1..n-1]] &&& -- consistency
    bAll phiAcyc library &&&
    bAnd (map phiLib varLib) &&&
    bAnd [ phiConn (allAVs!!i) (allAVs!!j) 
         | i <- [0..nAllAVs-1], j <- [i+1 .. nAllAVs-1]]
