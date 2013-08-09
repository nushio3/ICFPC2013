module Main where

import Control.Bool (bool)
import Control.Monad 
import System.Exit
import System.Random

import RichBV
import SRichBV

getSomeOps :: IO [Op]
getSomeOps = do
  n <- randomRIO (0, length ops-1)
  let x = ops!!n
  n2 <- randomRIO (0, 1 :: Int)      
  xs <- bool
         (return [])
         (getSomeOps) (n2 ==0) 
  return $ x:xs
  where
    ops =  [If0 , TFold , Fold0 , Not , Shl 1 , Shr 1 , And , Or , Xor , Plus]

main :: IO ()
main = do
  forM_ [3..] $ \size -> do
    replicateM_ 3 $ do
      ops <- getSomeOps
      let progs = genProgram size ops
      forM_ progs $ \prog -> do
        flag <- prog `equiv` (simplify prog)
        when (not flag) $ do 
          print prog
          print $ simplify prog
          exitFailure
    
  