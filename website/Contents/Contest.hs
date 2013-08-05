module Contents.Contest where

import           Import

import           Control.Lens ((^.), Iso', iso)
import           Control.Lens.TH (makeLenses, makeLensesFor)
import           Control.Monad
import qualified Data.Text as Text
import           Safe (headMay)

----------------------------------------------------------------
-- Types and Lenses for concepts in the contest.
----------------------------------------------------------------


data TeamMember = TeamMember
  { _handleName :: Text.Text 
  , _mailAddr   :: Text.Text 
  , _renderIcon :: Int -> WidgetT App IO () }

$(makeLenses ''TeamMember)



-- make lenses for Persistent record types defined in config/models .

$(makeLensesFor 
   [ ("submissionM_commitId"    , "commitId")
   , ("submissionM_ownerAuth"   , "ownerAuth")
   , ("submissionM_time"        , "submittedTime")
   , ("submissionM_score"       , "score") ]
   ''Submission)

renderSubmission :: Submission -> WidgetT App IO ()
renderSubmission sub = do
  let ownerMem :: TeamMember
      ownerMem = sub ^. ownerAuth . authMember
      rico = ownerMem ^. renderIcon $ 32
      subi = sub ^. commitId
      sco  = maybe (0::Double) (fromRational) $ sub ^. score
      tims = takeWhile (/= '.')$ show $ sub ^. submittedTime
      tdIco  = [whamlet| <td> ^{ rico } |]
      tdComi = [whamlet| <td> #{ subi } |]
      tdTims = [whamlet| <td> #{ tims } |]

      tdSco =  [whamlet| <td align="right"> #{ sco } |]

      tds :: WidgetT App IO () 
      tds = foldl1 (>>) [tdIco , tdTims, tdComi, tdSco]    

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
    (\size -> [whamlet| <img src=@{StaticR img_umi_png} width=#{size}> |] )
  , TeamMember "fumieval" "fumiexcel@gmail.com" 
    (\size -> [whamlet| <img src=@{StaticR img_birds_gif} width=#{size}> |] )
  ]

