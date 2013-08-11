module SolverUtil where

import Control.Monad
import qualified Data.Map as Map
import System.Random
import Data.List (sort)

import BV (BitVector)

sampleExample :: Int -> Map.Map BitVector (Double, BitVector) -> IO [(BitVector, BitVector)]
sampleExample n db = 
  fmap (map snd . take n . reverse . sort) $
  mapM genRand wlist 
  where 
    genRand (w, (i,o)) = do
      r <- randomRIO (0,w)
      return $ (r, (i,o))
    
    wlist :: [(Double, (BitVector, BitVector))]
    wlist = 
      map (\(i,(w,o)) -> (w, (i,o))) $
      Map.toList db