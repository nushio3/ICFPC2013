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
import Data.Vector.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Function
import Control.Lens.Aeson
import qualified Data.Aeson as JSON

solveAndAnswer :: Given Token => T.Text -> Int -> [T.Text] -> IO (String, GuessResponse)
solveAndAnswer tid size ops = do
    Just (Context restore query) <- solve size (map T.unpack ops) equiv
    EvalResponse _ (Just eout) _ <- API.eval
        $ EvalRequest (Just tid) Nothing
        $ map (T.pack . printf "0x%016X") query
    putStrLn $ "query: " ++ show query
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
    ("hoge" : _) -> do
      progs <- solve 9 ["if0","not","shl1","shr16"] equiv
      -- mapM_ (\p -> putStrLn $ printProgram p ++ " -> " ++ printProgram ((canonic.simplify.moveOp2P.moveIfP.simplify.canonic $ p))) progs
      return ()
    ("submit" : path : t : _) -> do
        Just vs <- preview (_Just . _Array . from vector) <$> (JSON.decode <$> BL.readFile path :: IO (Maybe JSON.Value))
        solveAll $ filter ((<Just (read t)) . preview (ix "size" . _Integer)) vs

solveAll :: Given Token => [JSON.Value] -> IO ()
solveAll (p:ps) = do
    putStrLn $ "Problem: " ++ T.unpack (p ^?! ix "id" . _String)
    (answer, res) <- solveAndAnswer (p ^?! ix "id" . _String) (p ^?! ix "size" . _Integer . from enum) (p ^.. ix "operators" . _Array . from vector . traverse . _String)
    case guessStatus res of
        GuessWin -> solveAll ps
        GuessMismatch -> do
            putStrLn "Mismatch!"
            putStrLn $ "Program: " ++ answer
            putStrLn $ "Input: " ++ show (guessValues res ^?! _Just . ix 0)
            putStrLn $ "Expected: " ++ show (guessValues res ^?! _Just . ix 1)
            putStrLn $ "Actual: " ++ show (guessValues res ^?! _Just . ix 2)
        GuessError -> do
            putStrLn $ T.unpack (maybe "" id $ guessMessage res)
        
-- programs :: IO [Program]
-- programs = map read . lines <$> getContents
