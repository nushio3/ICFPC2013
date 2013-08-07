module Handler.Recruit where

import Import

import           Contents.Contest (submissionPriority)
import           Contents.Salt (salt)
import           Control.Lens ((^.), (%~), (&), to)
import           Data.Conduit(($$))
import qualified Data.Conduit.List as CL
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Text as Text
import           Network.HTTP.Base (urlEncode)
import           Safe (lastMay)



getRecruitR :: String -> Handler Html
getRecruitR reqStr = do
  when (reqStr /= salt "recruit") notFound
  
  subKVs <- runDB $ selectList [] [] 
    
  let nextSubKV :: Maybe (Entity Submission)
      nextSubKV = 
        lastMay $ 
        sortBy (compare `on` (submissionPriority . entityVal) ) $
        filter (null . (^. score) . entityVal) $
        subKVs
      
      nextTask :: String
      nextTask = 
        ("\nyour_task: "++) $ (++"\n") $
        urlEncode $ show $ fmap (^. to entityVal . commitId) nextSubKV
    
  case nextSubKV of
    Nothing -> return ()
    Just sub1kv -> runDB $ do
      repsert (entityKey sub1kv) $ entityVal sub1kv & aging %~ (1+)
      return ()
  
  defaultLayout $ do [whamlet| #{nextTask} |]
