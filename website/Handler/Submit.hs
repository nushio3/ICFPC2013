module Handler.Submit where

import Import
import Yesod.Auth


import Contents.Contest
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import System.IO


getSubmitR :: Handler Html
getSubmitR = do
  maid <- maybeAuthId

  defaultLayout $ do
    $(widgetFile "auth-test")
    forM_ [1..5] (\i -> renderAzunyan $ 32*i)
    $(widgetFile "submitform")
    

postSubmitR :: Handler Html
postSubmitR = do
  maid <- maybeAuthId

  postedText <- runInputPost $ ireq textField "content"
  currentTime <- liftIO getCurrentTime
 
  () <- runDB $ do
    insert (Submission postedText maid currentTime)
    return ()

  liftIO $ do
    hPutStrLn stderr (show postedText)
  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitted")

