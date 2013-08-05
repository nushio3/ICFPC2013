module Handler.Submit where

import Import
import Yesod.Auth


import Contents.Contest (teamMembers, renderIcon, renderSubmission)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import           Data.Conduit(($$))
import qualified Data.Conduit.List as CL
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import System.IO



getSubmitR :: Handler Html
getSubmitR = do
  maid <- maybeAuthId

  subs <- runDB $ do
    subs' <- selectSource [] [] $$ CL.foldM (\xs x -> return $ x:xs) []
    return (map entityVal subs' :: [Submission])

  liftIO $ hPutStr stderr $ show $ length subs

  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitform")

    [whamlet| <h2> Team Members |]
    forM_ teamMembers (\mem -> mem ^. renderIcon $ 64)

    [whamlet| <h2> Best Submissions |]
    let rows = foldl1 (>>) $ map renderSubmission subs 
    [whamlet| <table cellpadding=3> ^{scoreTableHeader} ^{rows} |]

    [whamlet| <h2> Recent Submissions |]
    let rows = foldl1 (>>) $ map renderSubmission subs 
    [whamlet| <table cellpadding=3> ^{scoreTableHeader} ^{rows} |]

  where
    scoreTableHeader = [whamlet|
<tr> 
   <td> icon
   <td> commit ID
   <td> score
|]    


postSubmitR :: Handler Html
postSubmitR = do
  maid <- maybeAuthId

  postedText <- runInputPost $ ireq textField "content"
  currentTime <- liftIO getCurrentTime

  () <- runDB $ do
    insert (Submission postedText maid currentTime Nothing)
    return ()
 
  liftIO $ do
    hPutStrLn stderr (show postedText)
  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitted")

