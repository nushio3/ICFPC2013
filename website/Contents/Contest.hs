module Contents.Contest where

import           Import

import           Control.Lens ((^.), Iso', iso, to)
import           Control.Lens.TH (makeLenses, makeLensesFor)
import           Control.Monad
import qualified Data.Text as Text
import           Safe (headMay, maximumMay)
import           Data.Time (getCurrentTime, UTCTime)
import           Data.Time.LocalTime (utcToLocalTime, hoursToTimeZone)
import           Text.Printf (printf)
import           Prelude (head)

----------------------------------------------------------------
-- Types and Lenses for concepts in the contest.
----------------------------------------------------------------


data TeamMember = TeamMember
  { _handleName :: Text.Text 
  , _mailAddr   :: Text.Text 
  , _renderIcon :: Int -> WidgetT App IO () }

$(makeLenses ''TeamMember)


-- | the priority of the submission. The larger, the higher priority.
submissionPriority :: Submission -> (Int, UTCTime)
submissionPriority sub = (negate $ sub ^. aging, sub ^. submittedTime)

-- | larger, the better.
submissionQuality :: Submission -> (Maybe Rational, UTCTime)
submissionQuality sub = (sub ^. score . to maximumMay, sub ^. submittedTime)



renderSubmission :: Submission -> WidgetT App IO ()
renderSubmission sub = do
  let ownerMem :: TeamMember
      ownerMem = sub ^. ownerAuth . authMember
      rico = ownerMem ^. renderIcon $ 32
      subi0 = sub ^. commitId
      subi 
        | Text.length subi0 > 60 = Text.take 60 subi0 <> "..."
        | otherwise              = subi0

      sco  = maybe (printf "N/A(%d)" (sub ^. aging)) ((show :: Double -> String) . fromRational) $ sub ^. score . to maximumMay
      tims = takeWhile (/= '.')$ show $  
             utcToLocalTime (hoursToTimeZone 9) $
             sub ^. submittedTime
      tdIco  = [whamlet| <td> ^{ rico } |]
      tdComi = [whamlet| <td> #{ subi } |]
      tdTims = [whamlet| <td> #{ tims } |]

      tdSco =  [whamlet| <td align="right"> #{ sco } |]

      tds :: WidgetT App IO () 
      tds = foldr (>>) (return ()) $ [tdIco , tdTims, tdComi, tdSco]    

  [whamlet| <tr> ^{tds} |]

  
renderEvent :: Event -> WidgetT App IO ()  
renderEvent evt = do               
  let ownerMem :: TeamMember
      ownerMem = 
        head $
        filter (\mem -> (mem ^. mailAddr) `Text.isInfixOf` (evt ^. eventStr)) teamMembers ++
        [head teamMembers] 
  
      rico = ownerMem ^. renderIcon $ 32
      tims = takeWhile (/= '.')$ show $  
             utcToLocalTime (hoursToTimeZone 9) $
             evt ^. eventTime
      
      evtStr = evt ^. eventStr
      
      tdIco  = [whamlet| <td> ^{ rico } |]
      tdTims = [whamlet| <td> #{ tims } |]
      tdStr  = [whamlet| <td> #{ evtStr } |]

      tds :: WidgetT App IO () 
      tds = foldr (>>) (return ()) $ [tdIco , tdTims, tdStr]    
  
  
  [whamlet| <tr> ^{tds} |]

----------------------------------------------------------------
-- Values and functions.
----------------------------------------------------------------

authMember :: Iso' AuthToken TeamMember
authMember = iso f g
  where
    f Nothing = robot
    f (Just nameStr) = maybe robot id $ 
                       headMay $ 
                       filter ((== nameStr) . (^. mailAddr)) $ 
                       teamMembers                     
    g = undefined
    robot = teamMembers !! 0

teamMembers :: [TeamMember]
teamMembers = 
  [ TeamMember "robot" "robot has no email address"
    (\size -> [whamlet| <img src=@{StaticR img_funny_doll_png} width=#{size}> |] )
  , TeamMember "nushio" "muranushi@gmail.com"
    (\size -> [whamlet| <img src=@{StaticR img_megahomu_png} width=#{size}> |] )
  , TeamMember "tanakh" "tanaka.hideyuki@gmail.com"
    (\size -> [whamlet| <img src=@{StaticR img_azunyan_jpg} width=#{size}> |] )
  , TeamMember "gusmachine" "gusmachine@gmail.com"
    (\size -> [whamlet| <img src=@{StaticR img_celestia_png} width=#{size}> |] )
  , TeamMember "fumieval" "fumiexcel@gmail.com" 
    (\size -> [whamlet| <img src=@{StaticR img_birds_gif} width=#{size}> |] )
  ]

addEvent :: Text.Text -> Handler ()
addEvent str = do
  t <- liftIO $ getCurrentTime
  runDB $ do
    insert $ Event t str
  return ()
