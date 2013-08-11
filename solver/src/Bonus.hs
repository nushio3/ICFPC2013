import Convert

import Control.Monad
import Data.Char

test = do
  str <- readFile "~/ICFPC2013/bonus.lambda"
  let sol = filter (not. all isSpace)$ lines str
  forM sol putStrLn 