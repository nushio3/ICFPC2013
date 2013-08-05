module Handler.Submit where

import Import
import Yesod.Auth
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)
import System.IO

getSubmitR :: Handler Html
getSubmitR = do
  maid <- maybeAuthId
  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitform")

postSubmitR :: Handler Html
postSubmitR = do
  maid <- maybeAuthId
  postedText <- runInputPost $ ireq textField "content"
  liftIO $ do
    hPutStrLn stderr (show postedText)
  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitted")
