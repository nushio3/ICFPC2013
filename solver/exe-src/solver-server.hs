{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import Data.Maybe
import qualified Data.Text.Lazy as T
import Network.HTTP.Types (status400)
import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Gulwani4
import SolverAPI

{-
- Things to be implemented:
--- Change port.
--- Record and report load.
-}

main = scotty 10203 $ do
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
        r <- liftIO $ SolverAPI.satLambda4 q
        case r of
          Nothing -> do
            status status400
            json $ object ["message" .= ("z3 died"::String)]
          Just response -> do
            json r
