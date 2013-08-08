module Handler.Report where

import Import

import           Contents.Salt (decode)
import           Control.Lens ((&),(%~))
import           Control.Monad
import qualified Data.Text as Text
import           Safe (readMay)


getReportR :: Text.Text -> Handler Html
getReportR str = do
  let reportMay :: Maybe (Rational, Text.Text)
      reportMay = decode str
  case reportMay of
    Nothing -> notFound
    Just (solution@(newScore, commitId))-> do
      runDB $ do
        keyMay <- getBy (UniqueSubmission commitId)
        case (keyMay :: Maybe (Entity Submission) ) of
          Nothing -> return ()
          Just kv -> do
            repsert (entityKey kv) (entityVal kv & score %~ (newScore:))
            return ()
      defaultLayout $ do
        [whamlet| Received : #{str} . |]
