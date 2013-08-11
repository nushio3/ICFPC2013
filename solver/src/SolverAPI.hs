{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}

module SolverAPI where

import Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map

import BV(BitVector)
import SMTSynth
import qualified WrapSMTSynth

data Input = Input
    { inputFlags :: SpecialFlags
    , inputSize :: Int
    , inputOperators :: [String]
    , inputWeight :: Double
    , inputExamples :: [InputExample]
  } deriving (Show, Eq)

data InputExample = InputExample
    { exampleInput :: BitVector
    , exampleWeight :: Double
    , exampleOutput :: BitVector
    } deriving (Show, Eq)

$(deriveJSON (drop 1) ''SpecialFlags)
$(deriveJSON (drop 7) ''InputExample)
$(deriveJSON (drop 5) ''Input)

pack :: SpecialFlags -> Int -> [String] -> Double -> Map.Map BitVector (Double, BitVector) ->
        Input
pack flags size ops t example =
  (Input flags size ops t
   (map (\(x, (y, z)) -> (InputExample x y z)) $ Map.assocs example))


-- Calls WrapSMTSynth.satLambda using Input data.
satLambda :: Input -> IO (Maybe String)
satLambda (Input flags probsize ops weight examples) =
  WrapSMTSynth.satLambda flags probsize ops weight (
    Map.fromList . map toTuple $ examples) where
    toTuple :: InputExample -> (BitVector, (Double, BitVector))
    toTuple (InputExample i w o) = (i, (w, o))

sampleInput :: Input
sampleInput =
  Input (SpecialFlags False False False 0) 2 ["plus", "if0"] 60 [
    InputExample 0 1.361 1,
    InputExample 3 1.361 6
    ]

-- Outputs {"Flags":{"tfoldMode":false,"bonusMode":false},"Size":2,"Weight":60.0,"Operators":["plus","if0"],"Examples":[{"Weight":1.361,"Input":0,"Output":1},{"Weight":1.361,"Input":3,"Output":6}]}
outputMain :: IO ()
outputMain = do
  BL8.putStrLn $ JSON.encode sampleInput
