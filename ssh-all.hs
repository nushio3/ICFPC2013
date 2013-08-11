#!/usr/bin/env runhaskell

import Control.Monad
import System.Environment
import System.Process
import Text.Printf

main :: IO ()
main = do
  argv <- getArgs
  ec2s <- fmap lines $ readFile "ec2-instances"
  forM_ ec2s $\url -> do
    printf "ssh -i oregon-jkey.pem ubuntu@%s '%s'" url $ unwords argv
