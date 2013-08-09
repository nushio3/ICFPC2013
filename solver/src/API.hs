{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module API where

import Network.HTTP.Conduit
import Data.Reflection
import Control.Lens
import Control.Applicative
import Network.HTTP.Types.Method
import Network
import Control.Lens.Aeson
import Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import Data.Vector.Lens
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Word
import Control.Monad
import Control.Monad.Trans
import Network.HTTP.Types.Status

newtype Token = Token { unToken :: String }

endpoint :: Given Token => String -> String
endpoint path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=" ++ unToken given

post :: Given Token => String -> Value -> IO Value
post p body = withSocketsDo $ withManager f where
    f man = do
    req <- parseUrl (endpoint p)
    res <- httpLbs req { method = "POST", requestBody = RequestBodyLBS (JSON.encode body), responseTimeout = Just (30*10^6), checkStatus = \_ _ _ -> Nothing } man
    let code = statusCode $ responseStatus res
    if code == 429
      then do
        liftIO $ putStrLn "retrying..."
        f man -- too many request, retry
      else if code >= 400 && code < 500
        then fail $ "HTTP Error " ++ show (responseStatus res) -- other response
        else maybe (fail "JSON decoding error") return . JSON.decode . responseBody $ res

myproblems :: Given Token => IO [Problem]
myproblems = do
    res' <- post "myproblems" ""                
    let vs = res' ^. _Array . from vector
    return [Problem (res ^?! ix "id" . _String)
                (res ^?! ix "size" . _Integer . from enum)
                (res ^.. ix "operators" . _Array . from vector . traverse . _String)
                (res ^? ix "solved" . _Bool)
                (res ^? ix "timeLeft" . _Integer . from enum)
        | res <- res' ^. _Array . from vector]

eval :: Given Token => EvalRequest -> IO EvalResponse
eval (EvalRequest i p as) = do
    res <- post "eval" (JSON.emptyObject
        & _Object . at "id" .~ fmap (_String #) i
        & _Object . at "program" .~ fmap (_String #) p
        & _Object . at "arguments" ?~ map (_String #) as ^. vector . re _Array)
    return $ EvalResponse
      (res ^?! ix "status" . _String . to toEvalStatus)
      (map (read . T.unpack) <$> (res ^? ix "outputs" >>= preview textList))
      (res ^? ix "message" . _String)

toEvalStatus :: Text -> EvalStatus
toEvalStatus "ok" = EvalOk
toEvalStatus "error" = EvalError

guess :: Given Token => Guess -> IO GuessResponse
guess (Guess i p) = do
    res <- post "guess" (JSON.emptyObject
        & _Object . at "id" ?~ _String # i
        & _Object . at "program" ?~ _String # p)
    return $ GuessResponse (res ^?! ix "status" . _String . to toGuessStatus) (res ^? ix "values" >>= preview textList) (res ^? ix "message" . _String)

train :: Given Token => TrainRequest -> IO TrainingProblem
train (TrainRequest size ops) = do
    res <- post "train" (JSON.emptyObject
        & _Object . at "size" ?~ _Integer # toEnum size
        & _Object . at "operators" ?~ _Array . from vector # map (_String #) ops)
    return $ TrainingProblem (res ^?! ix "challenge" . _String)
        (res ^?! ix "id" . _String)
        (res ^?! ix "size" . _Integer . from enum)
        (res ^.. ix "operators" . _Array . from vector . traverse . _String )

toGuessStatus :: Text -> GuessStatus
toGuessStatus "win" = GuessWin
toGuessStatus "mismatch" = GuessMismatch
toGuessStatus "error" = GuessError

textList = partsOf $ _Array . traverse . _String
    
data Problem = Problem
    { problemId :: Text
    , problemSize :: Int
    , problemOperators :: [Text]
    , isSolved :: Maybe Bool
    , timeLeft :: Maybe Int
    }

data EvalStatus = EvalOk | EvalError deriving (Show)

data EvalRequest = EvalRequest
    { evalId :: Maybe Text
    , evalProgram :: Maybe Text
    , evalArguments :: [Text]
    }
data EvalResponse = EvalResponse
    { evalStatus :: EvalStatus
    , evalOutputs :: Maybe [Word64]
    , evalMessage :: Maybe Text
    } deriving (Show)
data Guess = Guess
    { guessId :: Text
    , guessProgram :: Text
    }

data GuessStatus = GuessWin | GuessMismatch | GuessError deriving (Show)
data GuessResponse = GuessResponse
    { guessStatus :: GuessStatus
    , guessValues :: Maybe [Text]
    , guessMessage :: Maybe Text
    } deriving (Show)
data TrainRequest = TrainRequest
    { trainSize :: Int
    , trainOperators :: [Text]
    }
data TrainingProblem = TrainingProblem
    { trainingChallenge :: Text
    , trainingId :: Text
    , trainingSize :: Int
    , trainingOperators :: [Text]
    }
data Window = Window
    { resetsIn :: Int
    , amount :: Int
    , limit :: Int
    }

{-
data Status = Status
    { easyChairId :: Int
    , contestScore :: Int
    , lightningScore :: Int
    , trainingScore :: Int
    , mismatches :: Int
    , numRequests :: Int
    , requestWindow :: Window
    , cpuWindow :: Window
    , cpuTotalTime :: Int
    }
-}