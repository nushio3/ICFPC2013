module Handler.Event where

import Import

import           Contents.Contest (addEvent)
import           Contents.Salt (decode)
import           Control.Monad
import qualified Data.Text as Text
import           Safe (readMay)
import           System.IO

getEventR :: Text.Text -> Handler Html
getEventR arg = do
  let evtMay :: Maybe Text.Text
      evtMay = decode arg
  case evtMay of
    Nothing -> notFound
    Just evtStr -> do
      addEvent evtStr
      defaultLayout $ do
        [whamlet| AddEvent: #{evtStr} |]