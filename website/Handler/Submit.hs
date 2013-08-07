module Handler.Submit where

import Import
import Yesod.Auth


import Contents.Contest (teamMembers, renderIcon, renderSubmission, submissionQuality, addEvent, renderEvent)
import Control.Lens ((^.), (&), (.~))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Data.Hash.MD5 (Str(..), md5i)
import Data.Function (on)
import Data.List (sortBy)
import Data.Ratio
import Data.Time (getCurrentTime)
import System.IO



getSubmitR :: Handler Html
getSubmitR = do
  maid <- maybeAuthId

  subs <- runDB $ do
    subs' <- selectList [] [] 
    return (map entityVal subs' :: [Submission])

  evts <- runDB $ do
    evts' <- selectList [] [] 
    return (map entityVal evts' :: [Event])

  let recentSubs = take 10$ reverse $ sortBy (compare `on` (^. submittedTime))subs
      bestSubs = take 10 $ reverse $ sortBy (compare `on` submissionQuality) subs

      recentEvts = take 20 $ reverse $ sortBy (compare `on` (^. eventTime)) evts
  
  liftIO $ hPutStr stderr $ show $ length subs

  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitform")

    [whamlet| <h2> Team Members |]
    forM_ teamMembers (\mem -> mem ^. renderIcon $ 64)

    [whamlet| <h2> Best Submissions |]
    let rows = foldr (>>) (return ()) $ map renderSubmission bestSubs 
    [whamlet| <table cellpadding=3> ^{scoreTableHeader} ^{rows} |]

    [whamlet| <h2> Recent Submissions |]
    let rows = foldr (>>) (return ()) $ map renderSubmission recentSubs 
    [whamlet| <table cellpadding=3> ^{scoreTableHeader} ^{rows} |]

    [whamlet| <h2> Recent Events |]
    let rows = foldr (>>) (return ()) $ map renderEvent recentEvts
    [whamlet| <table cellpadding=3> ^{eventTableHeader} ^{rows} |]

  where
    scoreTableHeader = [whamlet|
<tr> 
   <td> icon
   <td> time
   <td> commit ID
   <td> score
|]    
    eventTableHeader = [whamlet|
<tr> 
   <td> icon
   <td> time
   <td> event
|]    


postSubmitR :: Handler Html
postSubmitR = do
  maid <- maybeAuthId

  postedText <- runInputPost $ ireq textField "content"
  currentTime <- liftIO getCurrentTime
  let sub0 = Submission postedText maid currentTime 0 []

  () <- runDB $ do
    insert sub0
    return ()
 
  liftIO $ do
    hPutStrLn stderr (show postedText)
    
  case maid of
    Just addr -> addEvent $ addr <> " has suggested : " <> postedText
    _         -> return ()
    
  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitted")

