module Handler.Submit where

import Import
import Yesod.Auth


import Contents.Contest (teamMembers, renderIcon, renderSubmission, score, submittedTime)
import Control.Lens ((^.), (&), (.~))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import           Data.Conduit(($$))
import qualified Data.Conduit.List as CL
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
    subs' <- selectSource [] [] $$ CL.foldM (\xs x -> return $ x:xs) []
    return (map entityVal subs' :: [Submission])

  let recentSubs = take 10$ reverse $ sortBy (compare `on` (^. submittedTime))subs
      bestSubs = take 10$ reverse $ sortBy (compare `on` (^. score))subs

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

  where
    scoreTableHeader = [whamlet|
<tr> 
   <td> icon
   <td> time
   <td> commit ID
   <td> score
|]    


postSubmitR :: Handler Html
postSubmitR = do
  maid <- maybeAuthId

  postedText <- runInputPost $ ireq textField "content"
  currentTime <- liftIO getCurrentTime
  let sub0 = Submission postedText maid currentTime Nothing
      den = md5i $ Str $ "den" ++ show sub0
      num = md5i $ Str $ "num" ++ show sub0
      
      sub :: Submission
      sub = sub0 & score .~ (Just $ den % num)


  () <- runDB $ do
    insert sub
    return ()
 
  liftIO $ do
    hPutStrLn stderr (show postedText)
  defaultLayout $ do
    $(widgetFile "auth-test")
    $(widgetFile "submitted")

