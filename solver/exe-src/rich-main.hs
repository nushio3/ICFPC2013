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
import Data.Reflection
import qualified Data.Text as T
import Text.Printf

import SRichBV (equiv)
import API

{-}
main = do
  con <- L.getContents
  let Just objs = decode con :: Maybe [Value]
  let sorted = sortBy (compare `on` (^?! key "size"._Integer)) objs
  mapM_ solve sorted
-}

main = do
  give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ do
    TrainingProblem _prog tid tsize tops <- train $ TrainRequest 5 []
    print (_prog, tid, tsize, tops)
    Just (query, lu) <- solve tsize $ map T.unpack tops
    EvalResponse estat (Just eout) emsg <- API.eval $ EvalRequest (Just tid) Nothing $ map (T.pack . printf "0x%016X") query
    print (estat, eout, emsg)
    putStrLn $ "my guessed answer: " ++ lu eout
    resp <- guess $ Guess tid $ T.pack (lu eout)
    print resp

--main = do
--  ps <- programs
--  forM_ [ (i, j, x, y) | (i, x) <- zip [1..] ps, (j, y) <- zip [1..] ps, i < j ] $ \(i, j, x, y) -> do
--    b <- x `equiv` y
--    when b $ do
--      print (i, x)
--      print (j, y)
--      putStrLn "====="

programs :: IO [Program]
programs = map read . lines <$> getContents
