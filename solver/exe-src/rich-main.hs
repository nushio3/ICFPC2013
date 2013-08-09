{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}
import RichBV

import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Reflection
import qualified Data.Text as T
import Text.Printf
import System.Environment
import SRichBV (equiv)
import API

solveAndAnswer :: Given Token => T.Text -> Int -> [T.Text] -> IO (String, GuessResponse)
solveAndAnswer tid size ops = do
    Just (Context restore query) <- solve size $ map T.unpack ops
    EvalResponse _ (Just eout) _ <- API.eval
        $ EvalRequest (Just tid) Nothing
        $ map (T.pack . printf "0x%016X") query
    let answer = restore eout
    res <- guess $ Guess tid $ T.pack answer
    return (answer, res)

main = give (Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
    ("training" : level : _) -> do
        TrainingProblem prog ident size ops <- train $ TrainRequest (read level) []
        (answer, res) <- solveAndAnswer ident size ops
        putStrLn $ "Expected answer: " ++ T.unpack prog
        putStrLn $ "Your answer: " ++ answer
        putStrLn $ "Result: " ++ show res
    ("submit" : _) -> do
        putStrLn "This feature is locked"

-- programs :: IO [Program]
-- programs = map read . lines <$> getContents
