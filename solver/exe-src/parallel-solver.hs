{-# LANGUAGE LambdaCase, MultiWayIf, FlexibleContexts, ScopedTypeVariables, Rank2Types #-}
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
import System.Exit
import Text.Printf
import System.Environment
import System.Random
import System.Process
import System.IO.Unsafe
import qualified Data.Text as T
import SRichBV
import Control.Applicative
import System.Timeout
import BV (BitVector)
import qualified API
import qualified Data.Map as Map

import qualified WrapSMTSynth

data Environment = Environment
    { examples :: TVar (Map.Map BitVector (Double, BitVector))
    , guessCandidate :: TVar (Map.Map String Double)
    , evalCandidate :: TVar (Map.Map BitVector Double)
    , oracleCount :: TVar Int
    , startTime :: UTCTime
    , theId :: T.Text
    , theSatLambdas :: [Double -> Map.Map BitVector (Double, BitVector) -> IO (Maybe String)]
    , _DEATH_NOTE :: TVar [ThreadId]
    }

findCounterExamples :: Given Environment => Program -> IO [BitVector]
findCounterExamples prog = do
    es <- atomically (readTVar (examples given))
    return $ [i | (i, (p, o)) <- Map.toList es, eval prog i /= o ]

genLambda :: Given Environment => Double -> IO ()
genLambda t = atomically (readTVar (examples given)) >>= head (theSatLambdas given) t >>= \case
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

remainingTime :: Given Environment => IO Double
remainingTime = do
    t <- getCurrentTime
    return $ 60 * 5 - realToFrac (diffUTCTime t (startTime given))

manufactur :: (Given API.Token, Given Environment) => IO ()
manufactur = do
    t <- remainingTime
    n <- atomically $ readTVar (oracleCount given)
    atomically $ modifyTVar (oracleCount given) (+1)
    numEvalCandidate <- fmap Map.size $ atomically $ readTVar (evalCandidate given)
    numGuessCandidate <- fmap Map.size $ atomically $ readTVar (guessCandidate given)
    if
        | t < 60, odd n -> guess
        | numEvalCandidate >= 256, numGuessCandidate > 16 -> eval
        | numGuessCandidate == 0 -> eval
        | otherwise -> guess
    where
        eval = do
            print "eval"
            (es', rest) <- fmap (splitAt 256 . sortBy (flip (compare `on` view _2)) . Map.toList)
                $ atomically $ readTVar (evalCandidate given)
            atomically $ writeTVar (evalCandidate given) $ Map.fromAscList rest
            let es = map fst es'
            is <- (++ es) <$> replicateM (256 - length es) randomIO

            API.eval (API.EvalRequest (Just $ theId given) Nothing (map (T.pack . printf "0x%016X") is)) >>= \case
                API.EvalResponse API.EvalOk (Just os) _ -> addExample $ zip3 is (repeat 60) os
                API.EvalResponse API.EvalError _ (Just msg) -> putStrLn (T.unpack msg)
        guess = do
            print "guess"
            gsc <- atomically $ readTVar (guessCandidate given)
            let (p, _) = maximumBy (compare `on` view _2) (Map.toList gsc)
            API.guess (API.Guess (theId given) (T.pack p)) >>= \case
                API.GuessResponse API.GuessWin _ _ -> putStrLn "Won!" >> kill'em_all >> exitSuccess
                API.GuessResponse API.GuessError _ (Just msg) -> putStrLn (T.unpack msg) >> kill'em_all >> exitSuccess
                API.GuessResponse API.GuessMismatch (Just vs) _ -> do
                    addExample [(read $ T.unpack $ vs ^?! ix 0, 1000, read $ T.unpack $ vs ^?! ix 1)]
            atomically $ modifyTVar (guessCandidate given) $ at p .~ Nothing

addExample :: Given Environment => [(BitVector, Double, BitVector)] -> IO ()
addExample xs = do
    forM_ xs $ \(i, w, o) -> atomically $ modifyTVar (examples given) $ at i ?~ (w, o)
    judgement
    removeTrivial
    
oracleSummoner :: (Given API.Token, Given Environment) => IO ()
oracleSummoner = forever $ do
    manufactur
    t <- readTVarIO (examples given)
    u <- readTVarIO (evalCandidate given)
    v <- readTVarIO (guessCandidate given)
    printf "Examples: %d, Eval: %d, Guess: %d\n" (Map.size t) (Map.size u) (Map.size v)
    threadDelay $ 4 * 1000 * 1000

trainer :: Given Environment => IO ()
trainer = forever $ do
    revealDistinguisher
    threadDelay $ 6 * 1000 * 1000

trainProblem :: Given API.Token => Int -> IO (T.Text, Int, [String])
trainProblem level = do
    API.TrainingProblem prog ident size ops <- API.train $ API.TrainRequest level []
    return (ident, size, map T.unpack ops)

main = getArgs >>= \case
    (level : _) -> give (API.Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ do
        (ident, size, ops) <- trainProblem (read level)
        ves <- newTVarIO Map.empty
        vgs <- newTVarIO Map.empty
        vev <- newTVarIO $ Map.fromList $ zip [0,1,2,3,4,5,15,16,17,65535,65536,65537] (repeat 50)
        deathNote <- newTVarIO []
        oc <- newTVarIO 0
        t <- getCurrentTime
        let env = Environment { examples = ves
                , guessCandidate = vgs
                , evalCandidate = vev
                , oracleCount = oc
                , startTime = t
                , theId = ident
                , theSatLambdas = [WrapSMTSynth.satLambda size ops]
                , _DEATH_NOTE = deathNote
                }
        print (size, ops)
        (give env :: (Given Environment => IO ()) => IO ()) $ do
            replicateM_ 8 $ spawn 8
            forkKillme trainer
            oracleSummoner

forkKillme :: Given Environment => IO () -> IO ThreadId
forkKillme m = do
    i <- forkIO m
    atomically $ modifyTVar (_DEATH_NOTE given) (i:)
    return i

kill'em_all :: Given Environment => IO ()
kill'em_all = do
    is <- readTVarIO (_DEATH_NOTE given)
    forM_ is killThread

spawn :: Given Environment => Double -> IO ThreadId
spawn t = forkKillme $ forever $ do
    timeout (floor $ t * 2 * 1000 * 1000) (genLambda t) >>= \case
        Nothing -> z3Slayer
        Just _ -> return ()

z3Slayer = do
    procs <- map words <$> lines <$> readProcess "/bin/ps" [] ""
    forM_ (map (!!0) $ filter (elem "<defunct>") procs) $ \pid -> system $ "kill " ++ pid
