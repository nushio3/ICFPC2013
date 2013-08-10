{-# LANGUAGE LambdaCase, MultiWayIf, FlexibleContexts, ScopedTypeVariables #-}
import qualified Data.Vector as V
import Data.Time
import Data.Reflection
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Lens
import Convert
import RichBV
import System.Random
import System.IO.Unsafe
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

verifyLambda :: Given Environment => Program -> IO Bool
verifyLambda prog = do
    es <- atomically (readTVar (examples given))
    case [i | (i, (p, o)) <- Map.toList es, eval prog i /= o ] :: [BitVector] of
        [] -> return True
        cs -> do
            atomically $ modifyTVar (examples given) $ flip (foldr (\i -> ix i . _1 +~ 1)) cs
            return False

genLambda :: Given Environment => IO ()
genLambda = atomically (readTVar (examples given)) >>= satLambda (theProblem given) >>= \case
    Just p -> do
        let prog = enrichProgram $ readProgram p
        b <- verifyLambda prog
        when b $ do
            m <- atomically $ readTVar (guessCandidate given)
            if any (unsafePerformIO . equiv prog)
                $ map (enrichProgram . readProgram) $ Map.keys m
                then return ()
                else do
                    atomically $ writeTVar (guessCandidate given) $ at p ?~ 1 $ m 
    Nothing -> return ()

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

manufactur :: Given Environment => IO ()
manufactur = do
    t <- remainingTime
    n <- atomically $ readTVar (oracleCount given)
    numEvalCandidate <- fmap Map.size $ atomically $ readTVar (evalCandidate given)
    numGuessCandidate <- fmap Map.size $ atomically $ readTVar (guessCandidate given)
    if
        | t < 60, odd n -> do
            -- API.guess
            return ()
        | numEvalCandidate >= 256 , numGuessCandidate > 16 -> do
            -- API.eval
            return ()
        | numGuessCandidate == 0 -> do
            -- API.eval
            return ()
        | otherwise -> do
            -- API.guess
            return ()

oracleSummoner :: Given Environment => IO ()
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
    give env $ do
        forkIO oracleSummoner
        forkIO trainer
    