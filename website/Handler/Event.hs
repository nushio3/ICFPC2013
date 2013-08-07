module Handler.Event where

import Import

import           Contents.Contest (addEvent)
import           Contents.Salt (salt)
import           Control.Monad
import qualified Data.Text as Text
import           Safe (readMay)
import           System.IO

getEventR :: String -> Handler Html
getEventR arg = do
  liftIO $ hPutStrLn stderr arg
  
  let evtMay :: Maybe (Text.Text, Text.Text)
      evtMay = readMay arg
  liftIO $ hPutStrLn stderr $ show evtMay
      
  case evtMay of
    Nothing -> notFound
    Just (evtStr, saltStr) -> do
      when (saltStr /= salt evtStr) notFound
      addEvent evtStr
      defaultLayout $ do
        [whamlet| AddEvent: #{evtStr} |]