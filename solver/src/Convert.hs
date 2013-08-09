module Convert where

import qualified BV

import Data.Monoid (mempty)
import Text.Trifecta (parseString, Result(..))

readProgram :: String -> BV.Program
readProgram str = 
  case parseString BV.parseProgram mempty str of
    Success x -> x
    Failure doc -> error $ show doc
