{-# LANGUAGE OverloadedStrings #-}
module Contents.Salt where

import Prelude


import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Monoid((<>))
import Data.String (IsString(..))
import Data.Digest.Pure.SHA (sha1, showDigest)


salt :: (Show a, IsString a) => a -> a
salt str = fromString $ showDigest $ sha1 $ saltString <> (pack $ show str)


-- | It is assumed that non-team member cannot read the content of our repository.

saltString :: ByteString
saltString = "Z21haWwuY29tPgpEYXRlOiAgIFNhdC%~dWcgMyAwNzo0Njo0NiAyMDEzIC0wNzAwCgogICAgSW5p"


