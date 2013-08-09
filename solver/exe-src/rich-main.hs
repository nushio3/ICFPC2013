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
                  fail $ "guess error: " ++ show tid
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
            putStrLn $ "Your answer: " ++ answer
            putStrLn $ "Result: " ++ show res
          Nothing -> do
            return ()
    ("submit" : _) -> do
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
