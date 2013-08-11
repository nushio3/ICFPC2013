{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}

module SolverAPI where

import Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map

import BV(BitVector)
import qualified Gulwani4

data Input = Input
    { inputSize :: Int
    , inputOperators :: [String]
    , inputWeight :: Double
    , inputExamples :: [InputExample]
  } deriving (Show, Eq)

data InputExample = InputExample
    { exampleInput :: BitVector
    , exampleWeight :: Double
    , exampleOutput :: BitVector
    } deriving (Show, Eq)

$(deriveJSON (drop 7) ''InputExample)
$(deriveJSON (drop 5) ''Input)

-- Calls Gulwani4.satLambda using Input data.
satLambda4 :: Input -> IO (Maybe String)
satLambda4 (Input probsize ops weight examples) =
  Gulwani4.satLambda probsize ops weight (
    Map.fromList . map toTuple $ examples) where
    toTuple :: InputExample -> (BitVector, (Double, BitVector))
    toTuple (InputExample i w o) = (i, (w, o))


outputMain :: IO ()
outputMain = do
  let sample = Input 2 ["plus", "if0"] 60 [
        InputExample 0 1.361 1,
        InputExample 3 1.361 6
        ]
  BL8.putStrLn $ JSON.encode sample
