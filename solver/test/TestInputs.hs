module TestInputs where

import Control.Monad
import System.IO.Unsafe
import System.Environment

import BV(BitVector)

{-# NOINLINE programs #-}
programs :: [String]
programs = unsafePerformIO $ do
  env <- lookupEnv "test_file"
  case env of
    Nothing -> return defs
    Just fn -> do
      xs <- readFile fn
      return $ filter (not . null) $ lines xs
  where
    defs = [ "(lambda (x) x)"
           , "(lambda (x) (fold x 0 (lambda (y z) (plus y z))))"
           ]  

{-# NOINLINE evalPairs #-}    
evalPairs :: [(String, [(BitVector, BitVector)])]
evalPairs = unsafePerformIO $ do
  forM epFiles readEPFiles
  
epFiles :: [FilePath]
epFiles =
  [ "../eval-data/eval1.txt"
  , "../eval-data/eval2.txt"
  , "../eval-data/eval3.txt"
  , "../eval-data/eval4.txt"
  , "../eval-data/eval5.txt"
  , "../eval-data/eval6.txt"
  , "../eval-data/eval7.txt"
  , "../eval-data/eval8.txt"
  ]
  
readEPFiles :: FilePath -> IO (String, [(BitVector, BitVector)])
readEPFiles fp = do
  con <- readFile fp
  let (lamStr: pairStr) = lines con
      pairs = map go pairStr
      go str = let (x:y:_) = words str in (read x,read y)
  return (lamStr, pairs)