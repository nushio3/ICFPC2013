{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Control.Concurrent
import Control.Monad.Trans
import Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import Data.Maybe
import qualified Data.Text.Lazy as T
import Network.HTTP.Types (status400)
import System.Console.CmdArgs.Implicit
import Web.Scotty

-- install wai-extra if you don't have this
import Network.Wai.Middleware.RequestLogger

import SolverAPI
import Z3Slayer

-- Port: port to listen.
-- weight: sec to sleep before handling request. Pure debug purpose.
data ServerOptions = ServerOptions
  { port :: Int
  , weight :: Int
  } deriving (Show, Data, Typeable)

server_options = ServerOptions {
  port = 31940  &= help "port number",
  weight = 0 &= help "debug flag to have weight"
  } &= summary "solver server"

{-
- Things to be implemented:
--- Record and report current # of requests.
-}

main = runserver =<< cmdArgs server_options where
  runserver (ServerOptions port weight) = scotty port $ do
    middleware logStdoutDev

    -- diagnose
    get "/" $ do
      liftIO z3Slayer  -- kill stale defuncts
      text "foobar"

    post "/solve" $ do
      -- debug operation.
      liftIO $ threadDelay (weight * 1000 * 1000)
      liftIO $ z3Slayer  -- kill stale defuncts
      b <- body
      -- I forgot how to handle maybe around here. Plz get rid of those stupid cases.
      case (JSON.decode b :: Maybe SolverAPI.Input) of
        Nothing -> do
          status status400
          json $ object ["message" .= ("Bad json request"::String)]
        Just q -> do
          r <- liftIO $ SolverAPI.satLambda q
          case r of
            Nothing -> do
              status status400
              json $ object ["message" .= ("z3 died"::String)]
            Just response -> do
              json r
