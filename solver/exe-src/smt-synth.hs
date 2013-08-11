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
main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
  ("training": level: fld) -> do
    TrainingProblem prog ident size ops <- train $ TrainRequest (read level) $ map T.pack fld
    putStrLn $ "Problem description: " ++ T.unpack ident ++ " " ++ show size ++ " " ++ unwords (map T.unpack ops)
    putStrLn $ "Expected answer: " ++ T.unpack prog
    putStrLn ""
    synth size ops ident
  ["specify", tid] -> do
    problems <- myproblems
    case find (\p -> problemId p == T.pack tid) problems of
      Just p  -> synth (problemSize p) (problemOperators p) (problemId p)
      Nothing -> fail "No such problem"
  ["auto"] -> do
    putStrLn "You start automatic solving mode, really?"
    "y" <- getLine
    problems <- myproblems
    forM_ (sortBy (compare `on` problemSize) problems) $ \p -> do
      when (isSolved p /= Just True && timeLeft p /= Just 0) $ do
        synth (problemSize p) (problemOperators p) (problemId p)
  _ -> do
    fail "invalid input ('_`)"
