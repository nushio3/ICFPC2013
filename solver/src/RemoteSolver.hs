{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module RemoteSolver where

import Network.HTTP.Conduit
import Data.Reflection
import Control.Concurrent.Async
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
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Data.Word
import Control.Monad
import Control.Monad.Trans
import Network.HTTP.Types.Status

import qualified SolverAPI

satLambdaSingleRemote :: String -> SolverAPI.Input -> IO (Maybe String)
satLambdaSingleRemote url input = withSocketsDo $ withManager f where
  f man = do
    req <- parseUrl url
    res <- httpLbs req { method = "POST", requestBody = RequestBodyLBS (JSON.encode input), responseTimeout = Just (30*10^6), checkStatus = \_ _ _ -> Nothing } man
    -- liftIO $ putStrLn $ "Answer from " ++ url
    let code = statusCode $ responseStatus res
    if code == 400
      then return Nothing
      else return . Just . BL8.unpack . responseBody $ res

paraAny :: [IO a] -> IO a
paraAny xs = foldl1 pa xs where
  pa a b = do
    x <- race a b
    return $ either id id x

satLambdaRemote :: [String] -> SolverAPI.Input -> IO (Maybe String)
satLambdaRemote urls input =
  paraAny . map (\u -> satLambdaSingleRemote u input) $ urls

outputMainSingle :: IO ()
outputMainSingle = do
  p <- satLambdaSingleRemote "http://localhost:10203/solve" SolverAPI.sampleInput
  print p

outputMainMulti :: IO ()
outputMainMulti = do
  let urls = ["http://localhost:10203/solve",
              "http://localhost:10204/solve",
              "http://localhost:10205/solve"]
  p <- satLambdaRemote urls SolverAPI.sampleInput
  print p
