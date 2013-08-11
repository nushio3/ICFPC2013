{-# LANGUAGE LambdaCase, MultiWayIf, FlexibleContexts, ScopedTypeVariables, Rank2Types #-}
import Data.Time
import Data.Reflection
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
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
import Data.Aeson
import Control.Lens.Aeson
import qualified Data.Text as T
import SRichBV
import Control.Applicative
import System.Timeout
import BV (BitVector)
import qualified API
import qualified Data.Map as Map
import Network.HTTP.Conduit
import qualified RemoteSolver
import Z3Slayer
import SMTSynth hiding (Program)

data Environment = Environment
    { examples :: TVar (Map.Map BitVector (Double, BitVector))
    , guessCandidate :: TVar (Map.Map String Double)
    , evalCandidate :: TVar (Map.Map BitVector Double)
    , oracleCount :: TVar Int
    , startTime :: UTCTime
    , theId :: T.Text
    , strictness :: Double
    , theSatLambdas :: [Double -> Map.Map BitVector (Double, BitVector) -> IO (Maybe String)]
    , _DEATH_NOTE :: TVar [ThreadId]
    }

findCounterExamples :: Given Environment => Program -> IO [BitVector]
findCounterExamples prog = do
    es <- atomically (readTVar (examples given))
    return $ [i | (i, (p, o)) <- Map.toList es, eval prog i /= o ]

genLambda :: Given Environment => Double -> IO ()
genLambda t = do
    atomically (readTVar (examples given)) >>= head (theSatLambdas given) (t * strictness given) >>= \case
        Just p -> do
            let prog = enrichProgram $ readProgram p
            findCounterExamples prog >>= \case
                [] -> do
                    m <- atomically $ readTVar (guessCandidate given)
                    if any (unsafePerformIO . equiv prog)
                        $ map (enrichProgram . readProgram) $ Map.keys m
                        then putStrLn "Bad:Equivalent"
                        else do
                            putStrLn "Good"
                            atomically $ modifyTVar (guessCandidate given) $ at p ?~ 1
                            revealDistinguisher
                            return ()
                cs -> do
                    putStrLn "Bad:CounterExample"
                    atomically $ modifyTVar (examples given) $ flip (foldr (\i -> ix i . _1 +~ 2)) cs
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
    when (length ps > 2) $ do
        i <- randomRIO (0, length ps - 1)
        let go = do
                j <- randomRIO (0, length ps - 1)
                if i == j
                    then go
                    else return j
        j <- go
        let a = ps !! i
            b = ps !! j
        equivNeq (enrichProgram $ readProgram $ fst a) (enrichProgram $ readProgram $ fst b) >>= \case 
            Just counter -> do
                atomically $ modifyTVar' (evalCandidate given) $ at counter ?~ 70
                atomically $ modifyTVar' (evalCandidate given) $ at counter ?~ 70
            Nothing -> do
                if snd a < snd b
                    then atomically $ modifyTVar' (guessCandidate given) $ at (fst a) .~ Nothing
                    else atomically $ modifyTVar' (guessCandidate given) $ at (fst b) .~ Nothing

remainingTime :: Given Environment => IO Double
remainingTime = do
    t <- getCurrentTime
    return $ 60 * 5 - realToFrac (diffUTCTime t (startTime given))

notTooLarge :: Int -> [a] -> Bool
notTooLarge 0 (_:xs) = False
notTooLarge n (_:xs) = notTooLarge (n - 1) xs
notTooLarge n [] = True

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
            es' <- fmap (take 128 . sortBy (flip (compare `on` view _2)) . Map.toList)
                $ atomically $ readTVar (evalCandidate given)
            let es = map fst es'
            is <- (es++) <$> replicateM (256 - length es) randomIO

            API.eval (API.EvalRequest (Just $ theId given) Nothing (map (T.pack . printf "0x%016X") is)) >>= \case
                API.EvalResponse API.EvalOk (Just os) _ -> addExample $ zip3 is (replicate (length es) 70 ++ repeat 60) os
                API.EvalResponse API.EvalError _ (Just msg) -> putStrLn (T.unpack msg)
        guess = do
            gsc <- atomically $ readTVar (guessCandidate given)
            let (p, _) = maximumBy (compare `on` view _2) (Map.toList gsc)
            when (notTooLarge 1000 p) $ do
                putStrLn "************************************"
                putStrLn $ "     guess " ++ p
                putStrLn "************************************"
                API.guess (API.Guess (theId given) (T.pack p)) >>= \case
                    API.GuessResponse API.GuessWin _ _ -> do
                        putStrLn "Won!"
                        kill'em_all
                        simpleHttp "http://botis.org:9999/play/crash.wav"
                        exitSuccess
                    API.GuessResponse API.GuessError _ (Just msg) -> putStrLn (T.unpack msg) >> kill'em_all >> exitSuccess
                    API.GuessResponse API.GuessMismatch (Just vs) _ -> do
                        addExample [(read $ T.unpack $ vs ^?! ix 0, 1000, read $ T.unpack $ vs ^?! ix 1)]
            atomically $ modifyTVar (guessCandidate given) $ at p .~ Nothing

addExample :: Given Environment => [(BitVector, Double, BitVector)] -> IO ()
addExample xs = do
    forM_ xs $ \(i, w, o) -> atomically $ modifyTVar (examples given) $ at i ?~ (w, o)
    removeTrivial
    judgement
    return ()
    
oracleSummoner :: (Given API.Token, Given Environment) => IO ()
oracleSummoner = forever $ do
    manufactur
    t <- readTVarIO (examples given)
    u <- readTVarIO (evalCandidate given)
    v <- readTVarIO (guessCandidate given)
    printf "Examples: %d, Eval: %d, Guess: %d\n" (Map.size t) (Map.size u) (Map.size v)
    threadDelay $ 4 * 1000 * 1000

trainProblem :: Given API.Token => Int -> [T.Text] -> IO (T.Text, Int, [String])
trainProblem level ops = do
    API.TrainingProblem prog ident size ops <- API.train $ API.TrainRequest level ops
    return (ident, size, map T.unpack ops)

solveAndAnswer :: Given API.Token => T.Text -> Int -> [String] -> Double -> Double -> Double -> Double -> IO ()
solveAndAnswer ident size ops w1 w2 w3 ratio = do
    ves <- newTVarIO Map.empty
    vgs <- newTVarIO Map.empty
    vev <- newTVarIO $ Map.fromList $ zip [0,1,2,3,4,5,15,16,17,65535,65536,65537] (repeat 50)
    deathNote <- newTVarIO []
    oc <- newTVarIO 0
    t <- getCurrentTime
    let flags = defaultSpecialFlags & bonusMode .~ ("bonus" `elem` ops)
            & tfoldMode .~ ("tfold" `elem` ops)
    let env = Environment { examples = ves
            , guessCandidate = vgs
            , evalCandidate = vev
            , oracleCount = oc
            , startTime = t
            , theId = ident
            , strictness = ratio
            , theSatLambdas = [RemoteSolver.satLambdaRemote flags size ops]
            , _DEATH_NOTE = deathNote
            }
    print (size, ops)
    (give env :: (Given Environment => IO ()) => IO ()) $ do
        replicateM_ 3 $ spawn w1
        replicateM_ 3 $ spawn w2
        replicateM_ 3 $ spawn w3
        oracleSummoner

main = give (API.Token "0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H") $ getArgs >>= \case
    ("submit" : ident : w1 : w2 : w3 : ratio : _) -> do
        problems <- API.myproblems
        case find (\p -> API.problemId p == T.pack ident) problems of
            Just p -> solveAndAnswer (T.pack ident) (API.problemSize p) (map T.unpack $ API.problemOperators p) (read w1) (read w2) (read w3) (read ratio)
            Nothing -> fail ""
    (level : w1 : w2 : w3 : ratio : ops) -> do
        (ident, size, ops) <- trainProblem (read level) (map T.pack ops)
        solveAndAnswer ident size ops (read w1) (read w2) (read w3) (read ratio)

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
    s <- readTVarIO (examples given)
    if (Map.size s >= 256)
        then timeout (floor $ t * 1000 * 1000) (genLambda t) >>= \case
            Nothing -> z3Slayer >> putStrLn "Spawning: Failed."
            Just _ -> putStrLn "Spawning: Done."
        else threadDelay $ 1 * 1000 * 1000
