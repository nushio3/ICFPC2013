module Main where

import Data.SBV
import SRichBV (equiv)
import Convert (readProgram, enrichProgram)
import qualified BV as B

import BV 

main :: IO ()
main = do
  print $  exec (Program "x" (Fold (Var "x") C0 (Reducer "y" "z" (Op2 Plus (Var "y") (Var "z"))))) 1

  print $ prog10
  print $ B.exec prog10 1
  
  
  prog1 `equiv` prog2 >>= print
  where
    rp = enrichProgram . readProgram
    
    prog10 = readProgram "(lambda (x) (fold x 0 (lambda (y z) (plus y z) )))"

    prog1 = rp "(lambda (x) (if0 1          0 1))"
    prog2 = rp "(lambda (x) (if0 (plus x x) 0 1))"            


{-

main :: IO ()
main = do
  ret <- prove $ do
    x <- symbolic "input"
    constrain $ x .> 9223372036854775808
    r1 <- sExec prog1 x 
    r2 <- sExec prog2 x 
    return $ r1 .== r2
  print prog1 
  print ret
  
  where
--    prog1 = readProgram "(lambda (x) (shr1 1))"
--    prog2 = readProgram "(lambda (x) 0)"
--    prog1 = readProgram "(lambda (x) (fold x 0 (lambda (y z) (plus y z ))))"
--    prog2 = readProgram "(lambda (x) (fold x 0 (lambda (z y) (plus y z ))))"    
    prog1 = readProgram "(lambda (x) (if0 1          0 1))"
    prog2 = readProgram "(lambda (x) (if0 (plus x x) 0 1))"        
-}