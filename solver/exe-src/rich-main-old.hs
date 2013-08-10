{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}
import RichBV

import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Reflection
import qualified Data.Text as T
import Text.Printf
import System.Environment
import Data.List
import Data.Function
import SRichBV (equiv)
import API
import Data.Bits
import Data.Word
import qualified Data.Map as M
import System.IO.Unsafe
import Data.Monoid
import Database
import System.Random

niceSolve :: Int -> [String] -> (Program -> Program -> IO Bool) -> IO (Maybe (Context [Word64] [Word64] [String]))
niceSolve size ops equiv = do
  let ps = niceGenProgram (fromIntegral size) $ map toOp ops
      qs = map optimize ps
      ms :: M.Map Program Program
      ms = M.fromList $ zip qs ps
      ss = M.keys ms
  putStrLn $ "Size: " ++ show size ++ ", " ++ show ops
  putStrLn $ "Generating " ++ show (length ps) ++ " candidates"
  putStrLn $ "Simplifies to " ++ show (length ss)

  --mapM_ (print . printProgram) ss

  let go i = do
        let n = 256
        vs <- replicateM n randomIO
        let xs = take n $ [0,1,2,3,4,5,15,16,17,65535,65536,65537] ++ reverse ( zipWith f [0..] vs)
            f x y = x .&. 0xff .|. y .&. complement 0xff
        let res0  = [ (map (RichBV.eval p) xs, Endo (p:)) | p <- ss]
            mm0 :: M.Map [Word64] (Endo [Program])
            mm0   = M.fromListWith mappend res0
            freq0 = maximum $ map (length . retract) $ M.elems mm0            

        if freq0 <= mismatchTolerance
          then do
            return $ Just $ Context (\vss -> [ printProgram $ ms M.! cand | cand <- retract $ mm0 M.! vss]) xs
          else do
            if i > retryTimes
              then do
                let tt = retract $ snd $ head $ reverse $ sortBy (compare `on` (length . retract . snd)) $ M.toList mm0
                putStrLn "Cannot divide groups"
                putStrLn $ "Maximum group size: " ++ show (length tt)

--                 forM_ (zip [1::Int ..] tt) $ \(ii, x) -> forM_ (zip [1..] tt) $ \(jj, y) -> do
--                   when (ii < jj) $ do
--                     b <- x `equiv` y
--                     when b $ do
--                       putStrLn $ printProgram x
--                       putStrLn $ printProgram y
--                       putStrLn "==="

                putStrLn "Trying to resolve..."
                let res1  = [ (map (RichBV.eval p) xs, Endo (merger p)) | p <- ss] 
                    mm1 :: M.Map [Word64] (Endo [Program])
                    mm1   = M.fromListWith mappend res1
                    
                    merger :: Program -> [Program] -> [Program]
                    merger x xs 
                      | any (unsafePerformIO . equiv x) xs = xs
                      | otherwise        = x:xs

                    freq1 = maximum $ map (length . retract) $ M.elems mm1
                if freq1 < mismatchTolerance
                  then return $ Just $ Context (\vss -> [ printProgram $ ms M.! cand | cand <- retract $ mm1 M.! vss]) xs
                  else return Nothing
              else go (i + 1)

  go 0 :: IO (Maybe (Context [Word64] [Word64] [String]))

solveAndAnswer :: Given Token => T.Text -> Int -> [T.Text] -> IO (Maybe (String, GuessResponse))
solveAndAnswer tid size ops = do
  rr <- solve size (map T.unpack ops) equiv
  case rr of
    Just (Context restore query) -> do
      EvalResponse _ (Just eout) _ <- API.eval
          $ EvalRequest (Just tid) Nothing
          $ map (T.pack . printf "0x%016X") query
      -- putStrLn $ "query: " ++ show query
      let go [] = fail $ "failed: " ++ show tid
          go (x:xs) = do
            res <- guess $ Guess tid $ T.pack x
            if guessStatus res == GuessWin
              then return (x, res)
              else if guessStatus res == GuessMismatch
                then do
                  putStrLn "guess mismatch, retry other answer..."
                  go xs
                else do
                  fail $ "guess error: " ++ show (x, res)
      Just <$> go (restore eout)
    Nothing -> do
      putStrLn $ "cannot solve: " ++ show tid
      return Nothing

main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
  ("training" : level : _) -> do
    TrainingProblem prog ident size ops <- train $ TrainRequest (read level) []
    putStrLn $ "Expected answer: " ++ T.unpack prog
    rr <- solveAndAnswer ident size ops
    case rr of
      Just (answer, res) -> do
        putStrLn $ "My answer: " ++ answer
        putStrLn $ "Result: " ++ show res
      Nothing -> do
        return ()

  ("test" : level : ops) -> do
    _ <- solve (read level) ops equiv
    return ()

  ("submit" : _) -> do
    putStrLn "really? (y/n)"
    l <- getLine
    when (l /= "y") $ fail "bye!"

    putStrLn "really, really? (y/n)"
    l <- getLine
    when (l /= "y") $ fail "bye!"

    problems <- myproblems
    forM_ (sortBy (compare `on` problemSize) problems) $ \p -> do
      when (isSolved p /= Just True) $ do
        putStrLn $ "trying to solve: " ++ T.unpack (problemId p) ++ " " ++ show (problemSize p) ++ " " ++ show (problemOperators p)
        rr <- solveAndAnswer (problemId p) (problemSize p) (problemOperators p)
        case rr of
          Just (answer, res) -> do
            putStrLn $ "Your answer: " ++ answer
            putStrLn $ "Result: " ++ show res
          Nothing -> do
            return ()

  ("hoge" : _) -> do
    progs <- solve 9 ["if0","not","shl1","shr16"] equiv
    -- mapM_ (\p -> putStrLn $ printProgram p ++ " -> " ++ printProgram ((canonic.simplify.moveOp2P.moveIfP.simplify.canonic $ p))) progs
    return ()

-- programs :: IO [Program]
-- programs = map read . lines <$> getContents
