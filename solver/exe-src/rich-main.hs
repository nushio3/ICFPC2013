{-# LANGUAGE OverloadedStrings #-}
import RichBV

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Lens.Aeson
import Control.Lens
import Data.Aeson
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative

import SRichBV (equiv)

--main = do
--  con <- L.getContents
--  let Just objs = decode con :: Maybe [Value]
--  let sorted = sortBy (compare `on` (^?! key "size"._Integer)) objs
--  mapM_ solve sorted

main = do
  ps <- programs
  forM_ [ (i, j, x, y) | (i, x) <- zip [1..] ps, (j, y) <- zip [1..] ps, i < j ] $ \(i, j, x, y) -> do
    b <- x `equiv` y
    when b $ do
      print (i, x)
      print (j, y)
      putStrLn "====="

programs :: IO [Program]
programs = map read . lines <$> getContents
