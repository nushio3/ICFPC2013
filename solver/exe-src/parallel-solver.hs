{-# LANGUAGE LambdaCase, MultiWayIf, FlexibleContexts, ScopedTypeVariables #-}
import qualified Data.Vector as V
import Data.Time
import Data.Reflection
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Lens
import Data.List
import Data.Function
import Convert
import RichBV
import System.Random
import System.IO.Unsafe
import qualified Data.Text as T
import SRichBV
import Control.Applicative
import BV (BitVector)
import qualified API
import qualified Data.Map as Map
type Candidate = V.Vector
data Environment = Environment
    { examples :: TVar (Map.Map BitVector (Float, BitVector))
    , guessCandidate :: TVar (Map.Map String Float)
    , evalCandidate :: TVar (Map.Map BitVector Float)
    , oracleCount :: TVar Int
    , startTime :: UTCTime
    , theProblem :: API.Problem
    }

satLambda :: API.Problem -> Map.Map BitVector (Float, BitVector) -> IO (Maybe String)
satLambda = undefined

findCounterExamples :: Given Environment => Program -> IO [BitVector]
findCounterExamples prog = do
    es <- atomically (readTVar (examples given))
    return $ [i | (i, (p, o)) <- Map.toList es, eval prog i /= o ]

genLambda :: Given Environment => IO ()
genLambda = atomically (readTVar (examples given)) >>= satLambda (theProblem given) >>= \case
    Just p -> do
        let prog = enrichProgram $ readProgram p
        findCounterExamples prog >>= \case
            [] -> do
                m <- atomically $ readTVar (guessCandidate given)
                if any (unsafePerformIO . equiv prog)
                    $ map (enrichProgram . readProgram) $ Map.keys m
                    then return ()
                    else do
                        atomically $ writeTVar (guessCandidate given) $ at p ?~ 1 $ m 
            cs -> do
                atomically $ modifyTVar (examples given) $ flip (foldr (\i -> ix i . _1 +~ 1)) cs
    Nothing -> return ()

judgement :: Given Environment => IO ()
judgement = do
    gcs <- readTVarIO (guessCandidate given)
    
    gcs' <- fmap (Map.fromAscList . concat) $ forM (Map.toAscList gcs) $ \(p, f) -> do
        findCounterExamples (enrichProgram $ readProgram p) >>= \case
            [] -> return [(p, f)]
            cs -> return []
    atomically $ writeTVar (guessCandidate given) gcs'

removeTrivial :: Given Environment => IO ()
removeTrivial = do
    es <- readTVarIO (examples given)
    forM_ (Map.keys es) $ \i -> atomically $ modifyTVar (evalCandidate given) (at i .~ Nothing)

revealDistinguisher :: Given Environment => IO ()
revealDistinguisher = do
    ps <- fmap Map.toList $ atomically $ readTVar $ guessCandidate given
    i <- randomRIO (0, length ps - 1)
    let go = do
            j <- randomRIO (0, length ps - 1)
            if i /= j
                then go
                else return j
    j <- go
    let a = ps !! i
        b = ps !! j
    equivNeq (enrichProgram $ readProgram $ fst a) (enrichProgram $ readProgram $ fst b) >>= \case 
        Just counter -> do
            atomically $ modifyTVar' (evalCandidate given) $ at counter ?~ 1
            atomically $ modifyTVar' (evalCandidate given) $ at counter ?~ 1
        Nothing -> do
            if snd a < snd b
                then atomically $ modifyTVar' (guessCandidate given) $ at (fst a) .~ Nothing
                else atomically $ modifyTVar' (guessCandidate given) $ at (fst b) .~ Nothing

remainingTime :: Given Environment => IO Float
remainingTime = do
    t <- getCurrentTime
    return $ realToFrac $ diffUTCTime t (startTime given)

manufactur :: (Given API.Token, Given Environment) => IO ()
manufactur = do
    t <- remainingTime
    n <- atomically $ readTVar (oracleCount given)
    numEvalCandidate <- fmap Map.size $ atomically $ readTVar (evalCandidate given)
    numGuessCandidate <- fmap Map.size $ atomically $ readTVar (guessCandidate given)
    if
        | t < 60, odd n -> guess
        | numEvalCandidate >= 256 , numGuessCandidate > 16 -> eval
        | numGuessCandidate == 0 -> eval
        | otherwise -> guess
    where
        eval = do
            (es, rest) <- fmap (splitAt 256 . sortBy (flip (compare `on` view _2)) . Map.toList)
                $ atomically $ readTVar (evalCandidate given)
            let is = map fst es
            API.eval (API.EvalRequest (Just $ API.problemId $ theProblem given) Nothing (map (T.pack . show) is)) >>= \case
                API.EvalResponse API.EvalOk (Just os) _ -> atomically $ modifyTVar (examples given) $ foldl (.) id (zipWith (\x y -> at x ?~ (1, y)) is os)
        guess = do
            gsc <- atomically $ readTVar (guessCandidate given)
            let (p, _) = maximumBy (compare `on` view _2) (Map.toList gsc)
            API.guess (API.Guess (API.problemId $ theProblem given) (T.pack p)) >>= \case
                API.GuessResponse API.GuessWin _ _ -> fail "Won!"
                API.GuessResponse API.GuessError _ (Just msg) -> fail (T.unpack msg)
                API.GuessResponse API.GuessMismatch (Just vs) _ -> do
                    atomically $ modifyTVar (examples given) $ at (read $ T.unpack $ vs ^?! ix 0)
                        ?~ (1000, read $ T.unpack $ vs ^?! ix 1)

oracleSummoner :: (Given API.Token, Given Environment) => IO ()
oracleSummoner = forever $ do
    manufactur
    threadDelay $ 4 * 1000 * 1000
    
trainer :: Given Environment => IO ()
trainer = forever $ do
    revealDistinguisher
    threadDelay $ 6 * 1000 * 1000

getProblem :: IO API.Problem
getProblem = undefined

main = do
    problem <- getProblem
    ves <- newTVarIO Map.empty
    vgs <- newTVarIO Map.empty
    vev <- newTVarIO Map.empty
    oc <- newTVarIO 0
    t <- getCurrentTime
    let env = Environment { examples = ves
            , guessCandidate = vgs
            , evalCandidate = vev
            , oracleCount = oc
            , startTime = t
            , theProblem = problem
            }
    give (API.Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ give env $ do
        forkIO oracleSummoner
        forkIO trainer
    