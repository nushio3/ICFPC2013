module Handler.Report where

import Import

import           Control.Lens ((&),(%~))
import qualified Data.Text as Text
import           Safe (readMay)



getReportR :: String -> Handler Html
getReportR str = do
  liftIO $ print str
  let reportMay :: Maybe (Rational, Text.Text)
      reportMay = readMay str
  case reportMay of
    Nothing -> defaultLayout [whamlet| Presentation Error. |]
    Just (newScore, commitId) -> do
      runDB $ do
        keyMay <- getBy (UniqueSubmission commitId)
        case (keyMay :: Maybe (Entity Submission) ) of
          Nothing -> return ()
          Just kv -> do
            repsert (entityKey kv) (entityVal kv & score %~ (newScore:))
            return ()
      defaultLayout $ do
        [whamlet| Received : #{str} . |]
