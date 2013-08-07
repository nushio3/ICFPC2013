module Model where

import Prelude
import Yesod
import Data.Aeson.TH
import Data.Ratio
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

import Control.Lens.TH (makeLensesFor)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

$(deriveJSON (drop 12) ''Submission)

-- make lenses for Persistent record types defined in config/models .

$(makeLensesFor 
   [ ("submissionM_commitId"    , "commitId")
   , ("submissionM_ownerAuth"   , "ownerAuth")
   , ("submissionM_time"        , "submittedTime")
   , ("submissionM_aging"    , "aging") 
   , ("submissionM_score"       , "score") 
   ]   
   ''Submission)
