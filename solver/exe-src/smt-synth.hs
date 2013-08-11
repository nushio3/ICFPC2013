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

import API

import SMTSynth

main :: IO ()
main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
  ("training" : level : _) -> do
    TrainingProblem prog ident size ops <- train $ TrainRequest (read level) []
    putStrLn $ "Expected answer: " ++ T.unpack prog
    synth size ops ident
  _ -> do
    undefined
