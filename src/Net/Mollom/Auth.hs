-- | Takes care of the authentication on behalf of a Mollom client.
module Net.Mollom.Auth
  ( authenticate
  , getMollomTime
  , getMollomNonce
  ) where

import Codec.Binary.Base64(encode)
import Data.ByteString.Internal(c2w)
import Data.HMAC (hmac_sha1)
import Data.List (intersperse)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Locale (defaultTimeLocale)

authenticate :: String -- ^public key
             -> String -- ^private key
             -> String -- ^timestamp
             -> String -- ^nonce
             -> String -- ^ resulting hash value, base64 encoded 
authenticate publicKey privateKey timestamp nonce =
  let hash = hmac_sha1 (map c2w privateKey) $ map c2w . concat . (intersperse ":") $ [timestamp, nonce, privateKey]
  in encode hash 


getMollomTime :: String -- ^time format
              -> String -- ^current time in the specified format
getMollomTime format = 
  let time = unsafePerformIO $ getCurrentTime
  in formatTime defaultTimeLocale format time


getMollomNonce :: String
getMollomNonce = "FIXME_NONCE"


