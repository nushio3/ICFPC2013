module SRichBV where

import SBV (SBitVector)
import RichBV
import Data.SBV

sExec :: Program -> (SBitVector -> Symbolic SBitVector)
sExec (Program e) x = sEvalE e [x]

sEvalE :: Expression -> [SBitVector] -> Symbolic SBitVector
sEvalE (Constant c) _ = return $ fromIntegral c
