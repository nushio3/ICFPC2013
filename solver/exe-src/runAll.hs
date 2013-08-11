{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}

import Data.SBV
import System.Random
import Control.Monad
import Data.Reflection
import qualified Data.Text as T
import Text.Printf
import System.Environment
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans
import Data.List
import Data.Function
import API
import SMTSynth

main :: IO ()
main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ do
    problems <- myproblems
    ids <- lines <$> getContents
    forM_ ids $ \tid -> case find (\p -> problemId p == T.pack tid) problems of
        Just p  -> synth 8 (problemSize p) (problemOperators p) (problemId p)
        Nothing -> fail "No such problem"
