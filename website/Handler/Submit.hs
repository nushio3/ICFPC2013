module Handler.Submit where

import Import
import qualified Data.Text as Text

getSubmitR :: Handler Html
getSubmitR = defaultLayout $(widgetFile "submitform")

postSubmitR :: Handler Html
postSubmitR = do
  postedText <- runInputPost $ ireq textField "content"
  defaultLayout $(widgetFile "submitted")
