module StupidSat where

import qualified Data.Map as Map
import BV

satLambda :: Int -> [String] ->  Double -> Map.Map BitVector (Double, BitVector) -> IO (Maybe String)
satLambda  _ _ _ exampleMap = do
  undefined
  
  where
    exs = map (\(i,(_,o)) -> (i,o)) $ Map.toList exampleMap

