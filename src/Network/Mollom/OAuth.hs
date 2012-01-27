{-
 - (C) 2012, Andy Georges
 -
 - This modules takes care of preparing the OAuth 1.0 information that is to be sent 
 - along with each request to the Mollom service.
 -
 - For documentation about OAuth, see RFC 5849 (http://tools.ietf.org/html/rfc5849).
 -}

 module Network.Mollom.OAuth
    ( OAuthSignatureMethod (..) 
    , getMollomNonce
    , getMollomTime
    , oauthHeader
    , oauthHeaderValues
    , oauthSignature
    ) where

import Codec.Binary.Base64 (encode)
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.HMAC (hmac_sha1)
import Data.List (intercalate)
import Network.HTTP (RequestMethod, urlEncode)
import Network.HTTP.Headers (HasHeaders, HeaderName(HdrAuthorization), HeaderSetter, replaceHeader)
import System.IO.Unsafe (unsafePerformIO)
import System.Locale (defaultTimeLocale)
import System.Random
import System.Time

import Network.Mollom.Types

data OAuthSignatureMethod = OAuthHmacSha1 
                          | OAuthRsaSha1 
                          | OAuthPlainText 
                          deriving (Eq)

instance Show OAuthSignatureMethod where
    show OAuthHmacSha1  = "HMAC-SHA1"
    show OAuthRsaSha1   = "RSA-SHA1"
    show OAuthPlainText = "PLAINTEXT"

oauthVersion = "1.0"

-- | Obtain a nonce for use in the request
getMollomNonce :: String -- ^ unique string across requests
getMollomNonce = show . md5 . pack . show $ (+) (getMollomTime) $ (unsafePerformIO $ getStdRandom (randomR (1,100000)) :: Integer)

-- | Obtain the system time
getMollomTime :: Integer -- ^current time in the specified format
getMollomTime = let TOD s p = unsafePerformIO $ getClockTime
                in s


-- | Compute the hashed and base64 encoded value for a (key, string) pair.
hash :: String -- ^ key
     -> String -- ^ data to hash
     -> String -- ^ resulting hashed value
hash key s = encode $ hmac_sha1 (map c2w key) (map c2w s)


-- | The headers for the OAuth protocol in the HTTP
--   Authorization field. See 3.5.1
oauthHeaderValues :: String               -- ^ Public key
                  -> OAuthSignatureMethod -- ^ Signature method
                  -> [(String, String)]   -- ^ Name-value pairs (we need to be able to sort these later)
oauthHeaderValues key sig =
    [ ("oauth_consumer_key", key)
--    , ("oauth_token", "")
    , ("oauth_signature_method", show sig)
    , ("oauth_timestamp", show $ getMollomTime)
    , ("oauth_nonce", getMollomNonce)
    , ("oauth_version", "1.0")
    ]

-- | Format the OAuth header, so it can be added to the
--   HTTP request header.
oauthHeader :: HasHeaders a       -- ^ We must be applied to something that has headers
            => [(String, String)] -- ^ Name-value pairs that need to be set in the header
            -> (a -> a)           -- ^ Continuation for changing the headers
oauthHeader pairs = 
    replaceHeader HdrAuthorization $ "OAuth " ++ oas
  where oas = intercalate ","
            $ map (\(n, v) -> intercalate "=" [ urlEncode n, "\"" ++ urlEncode v ++ "\"" ]) pairs


-- | Determine the signature for the OAuth headers in the 
--   HTTP request sent to the Mollom server. See Section
--   3.4 of RFC 5849.
oauthSignature :: OAuthSignatureMethod -- ^ Signing type
               -> String         -- ^ Private key
               -> RequestMethod  -- ^ HTTP method
               -> String         -- ^ server URI
               -> String         -- ^ request path
               -> String         -- ^ request parameters
               -> String
oauthSignature sig privateKey method server path request = 
    -- The base string consists of the HTTP method (POST, GET, ...)
    -- concatenated via &-symbols with the URI of the request and 
    -- the encoded request parameters. See 3.4.1.
    let baseString = intercalate "&" 
                   $ [ show method
                     , urlEncode (server ++ "/" ++ path)
                     , urlEncode request
                     ]
    in case sig of
        -- The signature must use the token secret, even if it is empty. 
        -- See 3.4.2.
        OAuthHmacSha1 -> let key = intercalate "&"
                                 $ [ urlEncode privateKey
                                   , urlEncode ""
                                   ]
                         in hash key baseString

