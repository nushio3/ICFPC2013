{-# LANGUAGE TemplateHaskell #-}
module Gulwani4 where

import Control.Applicative hiding (Const)
import Control.Lens hiding ((.>))
import Control.Lens.TH
import Control.Monad
import Data.Bits
import Data.Char
import Data.SBV
import Text.Printf

type Addr = SWord16
type Val = SWord64

data Op = Const Int | Var 
        | Not | Shl Int | Shr Int
        | And | Or | Xor | Plus
        | If0 
                           deriving (Eq, Ord, Show, Read)

arity :: Op -> Int
arity (Const _) = 0
arity Var       = 0
arity Not = 1
arity (Shl _) = 1
arity (Shr _) = 1
arity And = 2
arity Or = 2
arity Xor = 2
arity Plus = 2
arity If0 = 3

oprToKey :: Op -> String
oprToKey = filter isAlphaNum . show

data Opvar a = Opvar 
  { _strkey ::String, -- unique key for printing and searching
    _inst :: Op,      -- type of instruction
    _ovar :: a,       -- output
    _ivars :: [a]     -- inputs
  }
    
$(makeLenses ''Opvar)  


data LVProgram = 
  LVProgram
    { -- _returnAddr :: Addr, 
      _library ::[Opvar Addr]}
  
$(makeLenses ''LVProgram)  


symbolicOp :: String -> Op -> Symbolic (Opvar Addr)
symbolicOp key opr  = do
  oaddr <- (exists (printf "oa-%s" key) :: Symbolic Addr) 
  iaddrs <- forM [0..arity opr-1] $ \j -> 
    (exists (printf "ia-%s-%d" key j) :: Symbolic Addr) 
  return $ Opvar key opr oaddr iaddrs
  


progOfSize :: Int -> [Op] -> Symbolic (SBool, LVProgram)
progOfSize size0 opList0 = do
  let opList = [Const 0, Const 1, Var] ++ opList0
      n = length opList * multiplicity
      multiplicity = size0
--  ret <- symbolic "returnAddr"
  addrLib <- fmap concat $ forM opList $
    \opr -> forM [0..multiplicity-1] $ 
      \i -> symbolicOp (oprToKey opr++"#"++show i) opr 

  let 
    allAddrs :: [Addr]
    allAddrs = map (^.ovar) addrLib
      ++ concat (map (^.ivars) addrLib)
    
    outAddrs :: [Addr]
    outAddrs = map (^.ovar) addrLib

    phiAddrBound :: Addr -> SBool
    phiAddrBound a = 0 .<= a &&& a .< (fromIntegral n)

    
    phiAcyc :: Opvar Addr -> SBool
    phiAcyc oa = bAll (.<(oa^.ovar)) (oa^.ivars)
    
    thmWfp = 
      -- bounded
      bAll phiAddrBound allAddrs &&&
      -- consistency      
      bAnd [(outAddrs!!i)./= (outAddrs!!j) 
           | i <- [0..n-1], j <- [i+1..n-1]] &&& 
      -- acyclicity
      bAll phiAcyc addrLib
  
  return (thmWfp, LVProgram {-  ret -}  addrLib)

testMain a b = do
  (thmWfp, prog) <- progOfSize 20 [Plus]
  thmBeh <- phiFunc prog a b
  return $ thmWfp &&& thmBeh


phiFunc :: LVProgram -> Val -> Val -> Symbolic SBool
phiFunc lvProg alpha beta = do
  let n = length $ addrLib
      addrLib =  lvProg ^. library
      allAddrs = [0..n-1]  
      
  outVals <- forM addrLib $
    \opaddr -> (exists (printf "out-%s" (opaddr^.strkey)) :: Symbolic Val)

  varLib <- (flip.flip zipWithM) addrLib allAddrs $
    \opaddr i -> do
      ivals <- 
        sequence [ exists (printf "in-%s-%d" (opaddr^.strkey) j) :: Symbolic Val 
                 | j <- [0..length (opaddr^.ivars)-1]]
      return $ Opvar (opaddr^.strkey) (opaddr^.inst) (outVals!!i) ivals                 
      
  let phiConn :: (Addr,Val) -> (Addr, Val) -> SBool
      phiConn (a1,v1) (a2,v2) = (a1 .== a2) ==> (v1 .== v2)
      

  let phiLib :: Opvar Val -> SBool
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
      allAVs = (fromIntegral (n-1), beta):
               avLibs
      
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
--    phiAddrBound (lvProg ^. returnAddr) &&&
    bAnd (map phiLib varLib) &&&
    bAnd [ phiConn (allAVs!!i) (allAVs!!j) 
         | i <- [0..nAllAVs-1], j <- [i+1 .. nAllAVs-1]]
