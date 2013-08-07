module Handler.Report where

import Import

getReportR :: String -> Handler Html
getReportR str = do
    defaultLayout $ do
      [whamlet| You have just entered  #{str} . |]
