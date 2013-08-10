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

data Op = Const Int | Var 
        | Not | Shl Int | Shr Int
        | And | Or | Xor | Plus
        | If0 
                           deriving (Eq, Ord, Show, Read)


data Opaddr a = Opaddr 
  {_inst :: Op, _ovar :: a, _ivars :: [a]}
    
$(makeLenses ''Opaddr)  


data LVProgram = 
  LVProgram
    { _returnAddr :: Addr, 
      _library ::[Opaddr Addr]}
  
$(makeLenses ''LVProgram)  


symbolicOp :: Int -> Op -> Int -> Symbolic (Opaddr Addr)
symbolicOp i opr arity = do
  oaddr <- (exists (printf "oa-%d" i) :: Symbolic Addr) 
  iaddrs <- forM [0..arity-1] $ \j -> 
    (exists (printf "ia-%d-%d" i j) :: Symbolic Addr) 
  return $ Opaddr opr oaddr iaddrs
  


testProg :: Symbolic (SBool, LVProgram)
testProg = do
  ret <- symbolic "returnAddr"
  a <- symbolicOp 0 Var 0
  b <- symbolicOp 1 Plus 2
  c <- symbolicOp 2 Plus 2
  let 
    addrLib :: [Opaddr Addr]
    addrLib = [a,b,c]
    n = length addrLib
  let 
    allAddrs :: [Addr]
    allAddrs = map (^.ovar) addrLib
      ++ concat (map (^.ivars) addrLib)
    
    outAddrs :: [Addr]
    outAddrs = map (^.ovar) addrLib

    phiAddrBound :: Addr -> SBool
    phiAddrBound a = 0 .<= a &&& a .< (fromIntegral n)

    
    phiAcyc :: Opaddr Addr -> SBool
    phiAcyc oa = bAll (.<(oa^.ovar)) (oa^.ivars)
    
    thmWfp = 
      -- bounded
      bAll phiAddrBound allAddrs &&&
      -- consistency      
      bAnd [(outAddrs!!i)./= (outAddrs!!j) 
           | i <- [0..n-1], j <- [i+1..n-1]] &&& 
      -- acyclicity
      bAll phiAcyc addrLib
  
  return (thmWfp, LVProgram ret addrLib)

test a b = do
  (thmWfp, prog) <- testProg
  thmBeh <- phiFunc prog a b
  return $ thmWfp &&& thmBeh


phiFunc :: LVProgram -> Val -> Val -> Symbolic SBool
phiFunc lvProg alpha beta = do
  let n = length $ addrLib
      addrLib =  lvProg ^. library
      allAddrs = [0..n-1]  
      
  outVals <- (\k -> zipWithM k allAddrs addrLib) $
    \i opaddr -> (exists (printf "out-%d" i) :: Symbolic Val) 

  varLib <- (\k -> zipWithM k allAddrs addrLib) $
    \i opaddr -> do
      ivals <- 
        sequence [ exists (printf "in-%d-%d" i j) :: Symbolic Val 
                 | j <- [0..length (opaddr^.ivars)-1]]
      return $ Opaddr (opaddr^.inst) (outVals!!i) ivals                 
      
  let phiConn :: (Addr,Val) -> (Addr, Val) -> SBool
      phiConn (a1,v1) (a2,v2) = (a1 .== a2) ==> (v1 .== v2)
      

  let phiLib :: Opaddr Val -> SBool
      phiLib opaddr = 
        (opaddr^.ovar) .==
        case (opaddr^.inst, opaddr^.ivars) of
          (Const n, [])  -> fromIntegral n
          (Var, [])      -> alpha
          (Not, [a])     -> complement a
          (Shl n, [a])   -> shiftL a n
          (Shr n, [a])   -> shiftR a n
          (And , [a,b])  -> a .&. b
          (Or , [a,b])   -> a .|. b
          (Xor , [a,b])  -> a `xor` b
          (Plus , [a,b]) -> a + b
          (If0, [a,b,c]) -> ite (a .== 0) b c
          _              -> undefined
          
  let allAVs :: [(Addr,Val)]
      allAVs = (lvProg ^. returnAddr, beta):avLibs
      
      nAllAVs = length allAVs
      
      avLibs = concat $
        (\k -> zipWith k addrLib varLib) $ 
          \opA opV -> (opA^.ovar, opV^.ovar) : 
                      zip (opA^.ivars) (opV^.ivars)
      outAddrs :: [Addr]
      outAddrs = map (^.ovar) addrLib
  
      phiAddrBound :: Addr -> SBool
      phiAddrBound a = 0 .<= a &&& a .< (fromIntegral n)
  
  return $ 
    phiAddrBound (lvProg ^. returnAddr) &&&
    bAnd (map phiLib varLib) &&&
    bAnd [ phiConn (allAVs!!i) (allAVs!!j) 
         | i <- [0..nAllAVs-1], j <- [i+1 .. nAllAVs-1]]
