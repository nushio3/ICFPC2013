{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

type MyInt = SWord16

type Program = SWord16


execute :: Program -> (MyInt, MyInt) -> Symbolic ()
execute prog (inp, oup) = do
  constrain $ oup .== inp + prog

main = do
  let examples :: [(MyInt, MyInt)]
      examples = [(1,3), (15, 18)]
  
  res <- sat $ do
    prog <- sWord16 "x"
    mapM_ (execute prog) examples
    return $ prog .== prog 
    
  print res
