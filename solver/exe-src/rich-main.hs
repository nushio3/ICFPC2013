{-# LANGUAGE OverloadedStrings, LambdaCase #-}
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
import System.Environment
import SRichBV (equiv)
import API

import Control.Lens.Internal.Context

main = getArgs >>= \case
    (level : _) -> give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ do
        TrainingProblem _prog tid tsize tops <- train $ TrainRequest (read level) []
        print (_prog, tid, tsize, tops)
        Just (Context restore query) <- solve tsize $ map T.unpack tops
        EvalResponse estat (Just eout) emsg <- API.eval $ EvalRequest (Just tid) Nothing $ map (T.pack . printf "0x%016X") query
        print (estat, eout, emsg)
        let answer = restore eout
        putStrLn $ "My answer: " ++ answer
        resp <- guess $ Guess tid $ T.pack answer
        print resp

programs :: IO [Program]
programs = map read . lines <$> getContents
