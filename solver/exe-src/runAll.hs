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
import Data.List.Split
import Control.Monad.Trans
import Data.List
import Data.Function
import API
import RichBV as BV
import System.Process
import SMTSynth hiding (synth)
import Control.Lens

synth :: Given Token => Int -> Int -> [T.Text] -> T.Text -> IO ()
synth cpuNum ss ops' ident = if "fold" `elem` ops' then putStrLn "I can not use fold (>_<)" else do
  let (ops, adj, isTFold)
        | "fold" `elem` ops' || "tfold" `elem` ops' =
          (ops' \\ ["tfold"], -6, True)
        | otherwise =
          (ops', -2, False)

  i0 <- map (\x -> x `div` 2 * 2) <$> replicateM 128 randomIO
  i1 <- map (\x -> x `div` 2 * 2 + 1) <$> replicateM 128 randomIO
  let is = concatMap (\(a, b) -> [a, b]) $ zip i0 i1
  os <- oracleIO ident is
  let es = head $ chunksOf 2 $ zip is os

  let 
    myFlags = defaultSpecialFlags
      & bonusMode .~ ("bonus" `elem` ops')
      & tfoldMode .~ ("tfold" `elem` ops')
  
  let oprs = catMaybes $ map SMTSynth.toOp ops
  let size = (max 1 $ (ss + adj - sum (map pred $ map argNum oprs)))
       + (if myFlags ^. bonusMode then 0 else 0)
  putStrLn $ "Start synthesis: " ++ T.unpack ident ++ " " ++ show ss ++ " (" ++ show size ++ "), " ++ show ops
  when isTFold $ putStrLn "TFold Mode (>_<);;"

  seeds <- replicateM cpuNum randomIO

  let go es = do
        -- putStrLn "behave..."
        putStrLn $ "inputs: " ++ show es
        progn <- SMTSynth.para cpuNum $ \i -> findProgram ((abs $ seeds !! 1)`mod`65536) myFlags oprs (size + i `mod` 3) $ take 5 es
        system "pkill z3"
        putStrLn $ "found: " ++ (BV.printProgram $ toProgram myFlags oprs progn)
        o <- oracleDistinct ident $ toProgram myFlags oprs progn
        case o of
          Nothing -> do
            putStrLn "Accepted: yatapo-(^_^)!"
            if (myFlags ^. bonusMode)
               then system "wget http://botis.org:9999/play/VEC1%20FX%20011.wav -O /dev/null"
               else system "wget http://botis.org:9999/play/crash.wav -O /dev/null"
            return ()
          Just oo -> do
            putStrLn $ "distinct: " ++ show oo
            go (oo:es)

        ---- a <- distinct oprs size e progn
        --case a of
        --  Nothing -> putStrLn $ "Answer found!!: " ++ show progn
        --  Just f -> do
        --    [g] <- oracleIO ident [f]
        --    go ((f, g):e)

  go es

main :: IO ()
main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
    ("run":_) -> do
        problems <- myproblems
        ids <- lines <$> getContents
        forM_ ids $ \tid -> case find (\p -> problemId p == T.pack tid) problems of
            Just p  -> synth 8 (problemSize p) (problemOperators p) (problemId p)
            Nothing -> fail "No such problem"
    ("sect":n : m : _) -> do
        ls <- lines <$> getContents
        putStr $ unlines $ ls ^.. elements (\i -> i `mod` read m == read n)