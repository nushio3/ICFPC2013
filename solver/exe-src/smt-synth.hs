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
  ("training": cpu: level: fld) -> do
    TrainingProblem prog ident size ops <- train $ TrainRequest (read level) $ map T.pack fld
    putStrLn $ "Problem description: " ++ T.unpack ident ++ " " ++ show size ++ " " ++ unwords (map T.unpack ops)
    putStrLn $ "Expected answer: " ++ T.unpack prog
    putStrLn ""
    synth (read cpu) size ops ident
  ["specify", cpu, tid] -> do
    problems <- myproblems
    case find (\p -> problemId p == T.pack tid) problems of
      Just p  -> synth (read cpu) (problemSize p) (problemOperators p) (problemId p)
      Nothing -> fail "No such problem"
  ("auto": cpu:flds) -> do
    putStrLn "You start automatic solving mode, really?"
    "y" <- getLine
    allProblems <- myproblems

    let problems = 
          (if ("bonus"`elem`flds) then filter isBonusProb else id) $
          (if ("tfold"`elem`flds) then filter isTFoldProb else id) $
          (if ("fold"`elem`flds) then filter isFoldProb else id) $
          (if ("nobonus"`elem`flds) then filter (not.isBonusProb) else id) $
          (if ("notfold"`elem`flds) then filter (not.isTFoldProb) else id) $
          (if ("nofold"`elem`flds) then filter (not.isFoldProb) else id) $
          allProblems
        isBonusProb = elem "bonus". problemOperators
        isTFoldProb = elem "tfold". problemOperators
        isFoldProb = elem "fold". problemOperators

    forM_ (sortBy (compare `on` problemSize) problems) $ \p -> do
      when (isSolved p /= Just True && timeLeft p /= Just 0) $ do
        synth (read cpu) (problemSize p) (problemOperators p) (problemId p)
  _ -> do
    fail "invalid input ('_`)"
