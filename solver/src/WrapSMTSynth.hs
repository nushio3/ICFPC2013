module WrapSMTSynth where

import SolverUtil
import SMTSynth
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Exception
import qualified Data.Map as Map
import BV (BitVector)
import qualified Data.Text as T
import Data.Maybe
import RichBV
import System.Random

satLambda :: SpecialFlags -> Int -> [String] -> Double -> Map.Map BitVector (Double, BitVector) -> IO (Maybe String)
satLambda flags size ops t example = do
    let ops' = catMaybes $ map (SMTSynth.toOp . T.pack) ops
    exs <- sampleExample (floor $ max 2 $ t / sqrt (fromIntegral $ length ops * size)) example
    i <- randomRIO (1, 256)
    r <- try $ findProgram (flags&randomSeed.~i) ops' size exs :: IO (Either IOException SMTSynth.Program)
    return $ either (const Nothing) (Just . printProgram . toProgram flags ops') r
