{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module RemoteSolver where

import Network.HTTP.Conduit
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import qualified Control.Exception as E
import Network.HTTP.Types.Method
import Network
import Control.Lens.Aeson
import Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad
import Control.Monad.Trans
import Network.HTTP.Types.Status

import qualified SolverAPI

satLambdaSingleRemote :: String -> SolverAPI.Input -> IO (Maybe String)
satLambdaSingleRemote url input = withSocketsDo $ (E.catch (withManager f) catcher) where
  f man = do
    req <- parseUrl url
    res <- httpLbs req { method = "POST", requestBody = RequestBodyLBS (JSON.encode input), responseTimeout = Just (30*10^6), checkStatus = \_ _ _ -> Nothing } man
    -- liftIO $ putStrLn $ "Answer from " ++ url
    let code = statusCode $ responseStatus res
    if code == 400
      then return Nothing
      else return . Just . BL8.unpack . responseBody $ res
  catcher (InternalIOException e) = do
    putStrLn $ "Connection to " ++ url ++ " failed."
    return Nothing


paraAny :: [IO a] -> IO a
paraAny xs = foldl1 pa xs where
  pa a b = do
    x <- race a b
    return $ either id id x

{-
paraAnyJust :: [IO (Maybe a)] -> IO (Maybe a)
paraAnyJust xs = do
  q <- atomically $ newTQueue
  let pa1 left b = do
        withAsync a $ \
  fold1 pawithAsyncFold q xs $ \ do
-}

satLambdaRemote :: [String] -> SolverAPI.Input -> IO (Maybe String)
satLambdaRemote urls input = paraAny . map callone $ urls where
  callone :: String -> IO (Maybe String)
  callone u = do
    v <- satLambdaSingleRemote u input
    case v of
      Nothing -> do
        threadDelay (30*10^6)
        return Nothing
      Just a -> return (Just a)


outputMainSingle :: IO ()
outputMainSingle = do
  p <- satLambdaSingleRemote
       "http://localhost:10203/solve" SolverAPI.sampleInput
  print p

outputMainMulti :: IO ()
outputMainMulti = do
  let urls = ["http://localhost:10203/solve",
              "http://localhost:10204/solve",
              "http://localhost:10205/solve"]
  p <- satLambdaRemote urls SolverAPI.sampleInput
  print p
