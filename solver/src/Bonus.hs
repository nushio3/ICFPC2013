module Bonus where

import Convert

import Control.Monad
import Data.Char
import BV
import Text.Printf

main :: IO ()
main = return ()

test fn = do
  str <- readFile fn
  let sol = map readProgram $ filter (not. all isSpace)$ lines str
  forM_ sol $ putStrLn . toInfix
  
toInfix :: Program -> String  
toInfix (Program _ expr) = printf "\\x -> %s" $ go expr
  where
    go :: Expr -> String
    go C0 = "0"
    go C1 = "1"
    go (Var _) = "x"
    go (Op1 Not x) = printf "(not %s)" $ go x
    go (Op1 Shl1 x) = printf "(shl1 %s)" $ go x
    go (Op1 Shr1 x) = printf "(shr1 %s)" $ go x
    go (Op1 Shr4 x) = printf "(shr4 %s)" $ go x
    go (Op1 Shr16 x) = printf "(shr16 %s)" $ go x
    
    go (Op2 And a b) = printf "(%s & %s)"  (go b) (go a)
    go (Op2 Or a b) = printf "(%s | %s)"  (go a) (go b)
    go (Op2 Xor a b) = printf "(%s ^ %s)"  (go a) (go b)
    go (Op2 Plus a b) = printf "(%s + %s)"  (go a) (go b)

    
    go (If0 a b c) = printf "(if0 %s %s %s)" (go a) (go b) (go c)
    go _ = "?"