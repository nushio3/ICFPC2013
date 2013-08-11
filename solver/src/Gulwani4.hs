{-# LANGUAGE TemplateHaskell #-}
module Gulwani4 where

import qualified API
import Control.Applicative hiding (Const)
import Control.Lens hiding ((.>))
import Control.Lens.TH
import Control.Monad
import Data.Bits
import Debug.Trace (trace)
import Data.Char
import Data.Tuple (swap)
import qualified Data.String.Utils as Str
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified Data.Map as Map
import Data.SBV
import qualified Data.Vector as V
import Network.HTTP.Base (urlEncode, urlDecode)
import Text.Printf

import Convert
import BV(BitVector)
import RichBV(printProgram)
import qualified BV
import SBV(SBitVector)
import SolverUtil

type Addr = SWord8
type Val = SBitVector

data Op = Const Int | Var 
        | Not | Shl Int | Shr Int
        | And | Or | Xor | Plus
        | If0 
                           deriving (Eq, Ord, Show, Read)

toOp :: String -> Op
toOp "not"   = Not
toOp "shl1"  = Shl 1
toOp "shr1"  = Shr 1
toOp "shr4"  = Shr 4
toOp "shr16" = Shr 16
toOp "and"   = And
toOp "or"    = Or
toOp "xor"   = Xor
toOp "plus"  = Plus
toOp "if0"   = If0
toOp x = error $ "unknown operator string:" ++ x


data VarEdge 
 = OutEdge { edgeKey :: String}
 | InEdge { edgeKey :: String, edgeIdx :: Int}
    deriving (Eq, Ord, Show, Read)
                                    
isOutEdge :: VarEdge -> Bool
isOutEdge (OutEdge _) = True
isOutEdge _           = False
                                    
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

oprToKey :: Simple Iso Op String
oprToKey = iso (urlEncode . show) (read . urlDecode )


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
      \i -> symbolicOp (view oprToKey opr++"#"++show i) opr 

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

testMain =  do
  satLambda 2 ["plus", "if0"] 60 $
    Map.fromList 
      [( 0 , (1.341, 1))
      ,( 3 , (1.341, 6)) ]

satLambda :: Int -> [String] ->  Double -> Map.Map BitVector (Double, BitVector) -> IO (Maybe String)
satLambda  probSize opStrs weight exampleMap = do
  sparseExamples <- 
    sampleExample (max 2 $ round $ weight / fromIntegral (probSize * length opStrs)) exampleMap
  let 
    examples :: [(SBitVector, SBitVector)]
    examples = map (both %~ fromIntegral) $ sparseExamples

  ret <- sat $ do
    (thmWfp, prog) <- progOfSize probSize $ map toOp opStrs
  
    thmBehs <- (flip. flip zipWithM) [0..] examples $ \i (a,b) -> 
      phiFunc prog i a b
    return $ thmWfp &&& bAnd thmBehs
  case ("Satisfiable." `isInfixOf` show ret) of
    False -> return Nothing
    True -> parseSBVOutput $ show ret
    
parseSBVOutput :: String -> IO (Maybe String)
parseSBVOutput outputStr = do
  return $ Just $ printProgram $ enrichProgram $ BV.Program "x" $ expand rootEdge
  where
    expand :: VarEdge -> BV.Expr
    expand (OutEdge key) = 
      expandExpr opr $
      map (expand . (oEdgeDict V.!) . (satMap Map.!) ) inEdges
      where opr = view (from oprToKey) (takeWhile (/='#') key)
            inEdges = [InEdge key i| i<- [0..arity opr-1]]
    
    expandExpr :: Op -> [BV.Expr] -> BV.Expr
    expandExpr opr args = case (opr, args) of
      (Const 0, [])-> BV.C0
      (Const 1, [])-> BV.C1
      (Var    , [])-> BV.Var "x"
      (Not    , [a])-> BV.Op1 BV.Not a
      (Shl  1 , [a])-> BV.Op1 BV.Shl1 a
      (Shr  1 , [a])-> BV.Op1 BV.Shr1 a
      (Shr  4 , [a])-> BV.Op1 BV.Shr4 a
      (Shr 16 , [a])-> BV.Op1 BV.Shr16 a
      (And    , [a,b])-> BV.Op2 BV.And a b
      (Or     , [a,b])-> BV.Op2 BV.Or a b
      (Xor    , [a,b])-> BV.Op2 BV.Xor a b
      (Plus   , [a,b])-> BV.Op2 BV.Plus a b
      (If0  , [a,b,c])-> BV.If0 a b c
      _       -> error $ printf "unexpected op and arity in SBV output: %s(%d)"  
                   (show opr) (length args)
    
    oEdgeDict :: V.Vector VarEdge
    oEdgeDict = 
      V.fromList $
      map snd $ sort $
      map swap $
      filter (isOutEdge . fst) $
      Map.toList satMap
    
    
    ret = Nothing
    
    rootEdge :: VarEdge
    rootEdge = V.last oEdgeDict
    
    satMap :: Map.Map VarEdge Int
    satMap = 
      Map.fromList $
      concat $
      map findAddrLine $
      filter ("::" `isInfixOf`)$
      lines outputStr
    findAddrLine :: String -> [(VarEdge, Int)]
    findAddrLine str = case words str of
      (key:_:val:_) | (take 3 key == "oa-")
                      -> [(OutEdge (getRaw key), read val) ]
      (key:_:val:_) | (take 3 key == "ia-")
                      -> [(InEdge (getRaw key) (getArgIdx key), read val) ]
                         
                         
      _               -> []
      
    getRaw key = Str.split "-" key !! 1
    getArgIdx key = read $ Str.split "-" key !! 2
    
phiFunc :: LVProgram -> Int -> Val -> Val -> Symbolic SBool
phiFunc lvProg exampleIdx alpha beta = do
  let n = length $ addrLib
      addrLib =  lvProg ^. library
      allAddrs = [0..n-1]  
      
  outVals <- forM addrLib $
    \opaddr -> (exists (printf "out-%d-%s" exampleIdx (opaddr^.strkey)) :: Symbolic Val)

  varLib <- (flip.flip zipWithM) addrLib allAddrs $
    \opaddr i -> do
      ivals <- 
        sequence [ exists (printf "in-%d-%s-%d" exampleIdx (opaddr^.strkey) j) :: Symbolic Val 
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
          _              -> error $ printf "unsupported operator and arity in phiLib: %s(%d)"
               (show $ opaddr^.inst) (length $ opaddr^.ivars) 
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
