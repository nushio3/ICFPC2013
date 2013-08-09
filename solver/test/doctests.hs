module Main where

import Control.Applicative
import Control.Monad
import Test.DocTest
import System.Directory
import System.FilePath

findHs :: FilePath -> IO [FilePath]
findHs dir = do
  fs <- map (dir </>) <$>
    filter (`notElem` ["..","."]) <$>
    getDirectoryContents dir
  subDirs <- filterM doesDirectoryExist fs
  files1 <-
    filter ((/='.') . head . takeFileName) <$>
    filter ((`elem` [".hs", ".lhs"]) . takeExtension) <$>
    filterM doesFileExist fs
  files2 <- concat <$> mapM findHs subDirs
  return $ files1 ++ files2

main :: IO ()
main = do
  filesP <- findHs "src/"

  let files = filesP 

  putStrLn $ "testing: " ++ unwords files
  doctest $ 
    ["--optghc=-Lcabal-dev/lib",
     "--optghc=-package-conf=cabal-dev/packages-7.6.3.conf"]
    ++ files
