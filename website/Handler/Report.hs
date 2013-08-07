module Handler.Report where

import Import

import           Contents.Salt (salt)
import           Control.Lens ((&),(%~))
import           Control.Monad
import qualified Data.Text as Text
import           Safe (readMay)


getReportR :: String -> Handler Html
getReportR str = do
  liftIO $ print str
  let reportMay :: Maybe ((Rational, Text.Text), String)
      reportMay = readMay str
  case reportMay of
    Nothing -> notFound
    Just (solution@(newScore, commitId) , saltStr)-> do
      when (salt (show solution) /= saltStr) notFound
      
      runDB $ do
        keyMay <- getBy (UniqueSubmission commitId)
        case (keyMay :: Maybe (Entity Submission) ) of
          Nothing -> return ()
          Just kv -> do
            repsert (entityKey kv) (entityVal kv & score %~ (newScore:))
            return ()
      defaultLayout $ do
        [whamlet| Received : #{str} . |]
