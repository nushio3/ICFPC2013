module Util where

import BV

import Data.Monoid (mempty)
import Text.Trifecta (parseString, Result(..))

readProgram :: String -> Program
readProgram str = 
  case parseString parseProgram mempty str of
    Success x -> x
    Failure doc -> error $ show doc
