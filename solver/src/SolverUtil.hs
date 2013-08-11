module SolverUtil where

import qualified Data.Map as Map
import System.Random

import BV (BitVector)

sampleExample :: Int -> Map.Map BitVector (Double, BitVector) -> IO [(BitVector, BitVector)]
sampleExample n db = return []
  where 
    wlist :: [(Double, (BitVector, BitVector))]
    wlist = []