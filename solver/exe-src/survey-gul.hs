module Main where

import Control.Monad
import Data.Time
import Data.SBV
import Gulwani2 (behaveBN)
import System.Random

main :: IO ()
main = forever $ do
  x <- randomRIO (0, 30 :: Double)
  y <- randomRIO (0, 30 :: Double)
  let ix :: Integer
      iy :: Integer
      ix = round x
      iy = round y
  return ()
  
