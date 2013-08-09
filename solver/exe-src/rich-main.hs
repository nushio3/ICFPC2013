{-# LANGUAGE OverloadedStrings #-}
import RichBV

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Lens.Aeson
import Control.Lens
import Data.Aeson
import Data.List
import Data.Function

main = do
  con <- L.getContents
  let Just objs = decode con :: Maybe [Value]
  let sorted = sortBy (compare `on` (^?! key "size"._Integer)) objs
  mapM_ solve sorted
