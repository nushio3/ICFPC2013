{-# LANGUAGE OverloadedStrings #-}
module Contents.Salt where
---       (salt, encode, decode, encodeString, decodeString)where

import Prelude


import qualified Codec.Crypto.RSA as RSA
import           Control.Spoon (spoon)
import           Crypto.Random (newGenIO, SystemRandom)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Base64.URL.Lazy as URL
import           Data.Monoid((<>))
import           Data.String (IsString(..))
import qualified Data.Text as Text
import           Safe (readMay)

salt = id

encode :: Show a => a -> IO Text.Text
encode = fmap Text.pack . encodeString

decode :: Read a => Text.Text -> Maybe a
decode =  decodeString . Text.unpack

encodeString :: Show a => a -> IO String
encodeString x = do 
  gen <- newGenIO
  return $
    BS.unpack $ 
    URL.encode $
    fst $
    RSA.encrypt (gen :: SystemRandom) pubKey $ 
    BS.pack $ show x

decodeString :: Read a => String -> Maybe a
decodeString x = do
  decBS <- either (const Nothing) Just $ URL.decode $ BS.pack x
  decBS2 <- spoon $ RSA.decrypt priKey decBS
  readMay $ BS.unpack decBS2

-- | It is assumed that non-team member cannot read the content of our repository.

pubKey :: RSA.PublicKey
pubKey = read "PublicKey {public_size = 128, public_n = 160748802332188366509507833183517980045216992503495397344038118206754367192858879425220795224596347675651188002071063204864476932633658444461052174097137313131346743795914136931556553011480484491207904792084641396407276383761022631037121335149194152479038223316370609598385953652260078832674798253689343043397, public_e = 65537}"

priKey :: RSA.PrivateKey
priKey = read "PrivateKey {private_pub = PublicKey {public_size = 128, public_n = 160748802332188366509507833183517980045216992503495397344038118206754367192858879425220795224596347675651188002071063204864476932633658444461052174097137313131346743795914136931556553011480484491207904792084641396407276383761022631037121335149194152479038223316370609598385953652260078832674798253689343043397, public_e = 65537}, private_d = 98742153828934908075945753398368605866614286085924198710330691436124793018949904771672697153616661860913527860588290453304686633154297389240712534486297141978800415952272348557500813963936516979517849060214747373738477316291534549059396790422843050524208146947242814153760538388423789720689622128866567465293, private_p = 0, private_q = 0, private_dP = 0, private_dQ = 0, private_qinv = 0}"
