{-# LANGUAGE OverloadedStrings #-}
module Contents.Salt where

import Prelude


import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Monoid((<>))
import Data.String (IsString(..))
import Data.Digest.Pure.SHA (sha256, showDigest)


salt :: (Show a, IsString a) => a -> a
salt str = fromString $ showDigest $ sha256 $ saltString <> (pack $ show str)

saltString :: ByteString
saltString = "Z21haWwuY29tPgpEYXRlOiAgIFNhdC%~dWcgMyAwNzo0Njo0NiAyMDEzIC0wNzAwCgogICAgSW5p"


