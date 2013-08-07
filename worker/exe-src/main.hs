{-# LANGUAGE TemplateHaskell #-}

import Contents.Salt(salt)
import Control.Concurrent (threadDelay)
import Control.Lens ((%=), (.=), use)
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Spoon (spoon)
import Data.List (isPrefixOf, partition)
import Data.Maybe (fromJust)
import Data.Ratio
import Network.Curl.Download (openURIString)
import Network.HTTP.Base (urlDecode, urlEncode)
import System.IO
import Text.Printf


data WorkerState = WorkerState
  { _waitTime :: Int
  }

$(makeLenses ''WorkerState)

initState :: WorkerState
initState = WorkerState 100

collatz :: Integer -> Integer
collatz n
  | n <= 1 = 1
  | even n = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (3*n+1)

serverUrl :: FilePath
serverUrl = "http://ec2-54-250-187-247.ap-northeast-1.compute.amazonaws.com:8496"

main :: IO ()
main = flip evalStateT initState $ forever $ do
  success <- liftIO $ tryFetch
  case success of
    True  -> waitTime .= 100
    False -> waitTime %= ( min 10000000 . (*2))
  (liftIO . threadDelay) =<< use waitTime

-- return if successfully processed a task.
tryFetch :: IO Bool
tryFetch = do
  let url = printf "%s/recruit/%s" serverUrl (salt "recruit")
  hPutStrLn stderr url
  res <- openURIString $ url
  case res of
    Left errmsg -> hPutStr stderr errmsg >> return False
    Right con -> do
      case filter (isPrefixOf "your_task:") $ lines con of
        [] -> hPutStrLn stderr "task not found." >> return False
        (taskStr:_) -> do
          let protoCid :: String
              protoCid = words taskStr !! 1

              -- the use of spoon is inevitable because urlDecode is a
              -- partial function.
              maybeCid :: Maybe String
              maybeCid = spoon $ fromJust $ read $ urlDecode $ protoCid
          case maybeCid of
            Nothing -> do
                       hPutStrLn stderr $ "cannot parse commit ID string: " ++ protoCid
                       return False
            Just cidStr -> do
              processCid cidStr

processCid :: String -> IO Bool
processCid cidStr = do
  putStrLn cidStr
  let funny :: String -> Integer
      funny = collatz . product . map ((1+) . fromIntegral . fromEnum )
      (numStr, denStr) = partition ((\n -> div n 7/=0) . fromEnum) (cidStr ++ salt cidStr)
      score :: Rational
      score = funny numStr % funny denStr
      retObj = (score,cidStr)
      retObjSalted = (retObj, salt (show retObj))
  print retObjSalted
  let url = printf "%s/report/%s" serverUrl $ urlEncode $ show retObjSalted
  res <- openURIString $ url
  return True
