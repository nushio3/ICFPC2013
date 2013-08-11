{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

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

import Gulwani4
import SolverAPI


data ServerOptions = ServerOptions { port :: Int }
                   deriving (Show, Data, Typeable)
server_options = ServerOptions {port = 10203  &= help "port number"}
                 &= summary "solver server"

{-
- Things to be implemented:
--- Record and report current # of requests.
-}

main = runserver =<< cmdArgs server_options where
  runserver (ServerOptions port) = scotty port $ do
    middleware logStdoutDev

    get "/" $ text "foobar"

    post "/solve" $ do
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
