module WrapSMTSynth where

import SolverUtil
import SMTSynth
import Control.Exception
import qualified Data.Map as Map
import BV (BitVector)
import qualified Data.Text as T
import Data.Maybe
import RichBV

-- 

satLambda :: Int -> [String] -> Double -> Map.Map BitVector (Double, BitVector) -> IO (Maybe String)
satLambda size ops t example = do
    let ops' = catMaybes $ map (SMTSynth.toOp . T.pack) ops
    exs <- sampleExample (floor $ max 2 $ t / fromIntegral (length ops * size)) example
    r <- try $ findProgram False {- !!TODO!! correctly handle bonus mode -} ops' size exs :: IO (Either IOException SMTSynth.Program)
    
    return $ either (const Nothing) (Just . printProgram . toProgram ops') r