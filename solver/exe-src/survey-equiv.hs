{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Bool (bool)
import Control.Monad 
import Data.List (nub, isInfixOf)
import System.Exit
import System.Random

import BV (BitVector)
import RichBV
import SRichBV
import Convert

import Data.SBV
import Text.Printf

equiv2 ::  Program -> Program -> IO Bool
equiv2 prog1 prog2 = do
  resp <- prove $ \x -> sExec prog1 x .== sExec prog2 x
  return $ "Q.E.D." `isInfixOf` show resp



getSomeOps :: Int -> IO [Op]
getSomeOps size = do
  n <- randomRIO (0, length ops-1)
  let x = ops!!n
  n2 <- randomRIO (0, size)      
  xs <- bool
         (return [])
         (getSomeOps size) (n2 /=0) 
  return $ nub $ x:xs
  where
    ops =  [If0 , TFold , Fold0 , Not , Shl 1 , Shr 1 , 
            Shl 4 , Shr 4 ,Shl 16 , Shr 16 ,And , Or , Xor , Plus]

main :: IO ()
main = do
  forM_ [3..] $ \size -> do
    replicateM_ (round $ 3^size) $ do
      ops <- getSomeOps size
      let progs = genProgram size ops
          n = length progs
      when (n>10) $ do
        i <- randomRIO (0,n-1)
        j <- randomRIO (0,n-1)
        let p1 = progs!!i
            p2 = progs!!j
        ret <- testOrder p1 p2
        printf "compare: %s\n" $ show p1
        printf "         %s\n" $ show ret
        printf "   with: %s\n" $ show p2
        printf "\n"
        
        return ()
        
testOrder :: Program -> Program -> IO (Maybe Ordering)
testOrder p1 p2  = do
  p1 `equiv` p2 >>= \case 
    True -> return $ Just EQ
    False -> search minBound maxBound

  
  where
    search a b
      | a+1 == b = return $ Just $ eval p1 b `compare` eval p2 b
      | otherwise = do
          let c = div (a+1) 2 + div b 2 
          go c >>= \case
            Just True  -> search c b
            Just False -> search a c
            Nothing    -> return Nothing

    go :: BitVector -> IO (Maybe Bool)
    go n = do
      resp <- prove $ \n' -> 
         (n' .< fromIntegral n) ==> sExec p1 n' .== sExec p2 n'
      if 
        | "Q.E.D." `isInfixOf` show resp       -> return $Just True
        | "Falsifiable." `isInfixOf` show resp -> return $Just True
        | otherwise                            -> do
           print resp
           return Nothing
         
