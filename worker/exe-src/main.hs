{-# LANGUAGE TemplateHaskell #-}

import Contents.Salt(encodeString)
import Control.Concurrent (threadDelay)
import Control.Lens ((%=), (.=), (.~), use, (&))
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Spoon (spoon)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA(sha1,integerDigest)
import Data.Time (getCurrentTime)
import Data.List (isPrefixOf, partition)
import Data.Maybe (fromJust)
import Data.Ratio
import Network.Curl.Download (openURIString)
import Network.HTTP.Base (urlDecode, urlEncode)
import System.IO
import System.Process (system)
import System.Environment (getArgs)
import Text.Printf


data WorkerState = WorkerState
  { _waitTime :: Int
  , _workerName :: String
  }

$(makeLenses ''WorkerState)

initState :: WorkerState
initState = WorkerState 10000 "nameless"


serverUrl :: FilePath
serverUrl = "http://ec2-54-250-187-247.ap-northeast-1.compute.amazonaws.com:8496"

openServer :: Show a => String -> a -> IO (Either String String)
openServer subdir msg = do
  encMsg <- encodeString msg
  let url = printf "%s/%s/%s" serverUrl subdir $ encMsg
  hPutStrLn stderr url
  res <- openURIString $ url
  print res
  return res

main :: IO ()
main = do
  argv <- getArgs
  let nam = unwords argv
  flip evalStateT (initState & workerName .~ nam)  $ forever $ do
    maybeCommitId <- liftIO $ tryFetch
    case maybeCommitId of
      Nothing -> waitTime %= ( min 10000000 . (*2))

      Just cid -> do
        liftIO $ openServer "event" $ (printf "%s received a task: %s" nam cid :: String)
        success <- liftIO $ processCid cid
        let whpn
              | success   = "finished"
              | otherwise = "couldn't process"
        liftIO $ openServer "event" $ (printf "%s %s task: %s" nam whpn cid :: String)
        waitTime .= 10000
    (liftIO . threadDelay) =<< use waitTime

-- return if successfully processed a task.
tryFetch :: IO (Maybe String)
tryFetch = do
  currentTime <- getCurrentTime
  let msg :: String
      msg =  show currentTime ++ "recruit"
  res <- openServer "recruit" msg
  case res of
    Left errmsg -> hPutStr stderr errmsg >> return Nothing
    Right con -> do
      case filter (isPrefixOf "your_task:") $ lines con of
        [] -> hPutStrLn stderr "task not found." >> return Nothing
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
                       return Nothing
            Just cidStr -> do
              return $ Just cidStr

processCid :: String -> IO Bool
processCid givenStr = do
  putStrLn givenStr
  let cidStr = concat $ take 1 $ words givenStr
  
--  system $ printf "git clone git@github.com:nushio3/ICFPC2013"

  
  let num :: Integer
      num = 72
      den :: Integer
      den = 10
      retObj = (cidStr, num % den)
  print retObj
  -- openServer "report" retObj
  return True
