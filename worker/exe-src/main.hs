{-# LANGUAGE TemplateHaskell #-}

import Contents.Salt(salt)
import Control.Concurrent (threadDelay)
import Control.Lens ((%=), (.=), (.~), use, (&))
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Spoon (spoon)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (integerDigest , sha1)
import Data.List (isPrefixOf, partition)
import Data.Maybe (fromJust)
import Data.Ratio
import Network.Curl.Download (openURIString)
import Network.HTTP.Base (urlDecode, urlEncode)
import System.IO
import System.Environment (getArgs)
import Text.Printf


data WorkerState = WorkerState
  { _waitTime :: Int
  , _workerName :: String
  }

$(makeLenses ''WorkerState)

initState :: WorkerState
initState = WorkerState 100 "nameless"


serverUrl :: FilePath
serverUrl = "http://ec2-54-250-187-247.ap-northeast-1.compute.amazonaws.com:8496"

main :: IO ()
main = do
  argv <- getArgs
  let nam = unwords argv
  flip evalStateT (initState & workerName .~ nam)  $ forever $ do
    success <- liftIO $ tryFetch
    case success of
      True  -> do
        let url = printf "%s/event/%s" serverUrl $ urlEncode $ show (msg,salt msg)
            msg :: String
            msg = printf "worker %s has fetched an event." nam
        liftIO $ do
          hPutStrLn stderr url
          res <- openURIString $ url
          print res
        waitTime .= 100
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
      funny = (1+) . integerDigest . sha1 . pack
      (numStr, denStr) = partition (even . fromEnum) (cidStr ++ salt cidStr)
      score :: Rational
      score = funny numStr % oddize (funny denStr)
      retObj = (score,cidStr)
      retObjSalted = (retObj, salt (show retObj))

      oddize n
        | odd n     = n
        | otherwise = oddize $ div n 2
  print retObjSalted
  let url = printf "%s/report/%s" serverUrl $ urlEncode $ show retObjSalted
  res <- openURIString $ url
  return True
