{- 
 - (C) 2012, Andy Georges
 -
 - This module provides internal functions to allow using the Mollom service
 -}

module Network.Mollom.Internals 
  ( mollomApiVersion
  , mollomTimeFormat
  , MollomConfiguration(..)
  , MollomError(..)
  --, MollomValue(..)
  , MollomResponse(..)
  , boolToOneZeroString
  , catSecondMaybes
  , service
  ) where

import           Control.Arrow (second)
import           Control.Monad.Error
import           Data.List (intercalate, sort)
import           Data.Maybe(fromJust, isJust)
import           Network.HTTP (getRequest, postRequestWithBody, simpleHTTP)
import           Network.HTTP.Base (HTTPResponse, RequestMethod(..), Response(..), ResponseCode, urlEncode)
import           Network.HTTP.Headers (HeaderName (HdrContentType, HdrAccept), HasHeaders, replaceHeader)
import           Network.HTTP.Stream (ConnError(..), Result)

import Network.Mollom.OAuth
import Network.Mollom.Types

mollomServer :: String
mollomServer = "http://dev.mollom.com/v1"

-- FIXME: This should be specified in some configuration file
-- or use the system locale
mollomTimeFormat = "%Y-%m-%dT%H:%M:%S.000+0200"

catSecondMaybes :: [(k, Maybe v)] -> [(k, v)]
catSecondMaybes = map (second fromJust) . filter (isJust . snd)


boolToOneZeroString :: Bool -> String
boolToOneZeroString True = "1"
boolToOneZeroString False = "0"


-- | Encode the parameters after sorting them.
buildEncodedQuery :: [(String, String)] -- ^ The name-value pairs for the query
                  -> String
buildEncodedQuery ps = 
    let sps = sort ps
    in intercalate "&" $ map (\(n, v) -> intercalate "=" [urlEncode n, urlEncode v]) sps



-- | Process the response from the Mollom server, chekcing the return code, 
--   and fill in the associative list with name-value pairs.
--   FIXME: this should also process the HTTP return code and set the error accordingly
processResponse :: Result (HTTPResponse String) -> Either MollomError MollomResponse
processResponse result =
    case result of 
        Left ce -> Left (CMollomError ce)
        Right r -> Right MollomResponse { code = rspCode r
                                         , message = rspReason r
                                         , response = rspBody r
                                         }

-- | The service function is the common entrypoint to use the Mollom service. This
--   is where we actually send the data to Mollom.
--   FIXME: implement the maximal number of retries
service :: String             -- ^ Public key
        -> String             -- ^ Private key
        -> RequestMethod      -- ^ The HTTP method used in this request.
        -> String             -- ^ The path to the requested resource
        -> [(String, String)] -- ^ Request parameters
        -> [String]           -- ^ Expected returned values
        -> ErrorT MollomError IO MollomResponse -- :: ErrorT (IO (Either MollomError (HTTPResponse String)))
service publicKey privateKey method path params expected = do
    let oauthHVs = oauthHeaderValues publicKey OAuthHmacSha1
        oauthSig = oauthSignature OAuthHmacSha1 privateKey method mollomServer path 
                                  (buildEncodedQuery $ params ++ oauthHVs)
        oauthH   = oauthHeader (("oauth_signature", oauthSig) : oauthHVs)
        contentH = replaceHeader HdrContentType "application/x-www-form-urlencoded"
        acceptH  = replaceHeader HdrAccept "application/json;q=0.8"
    result <- liftIO $ liftM processResponse $ simpleHTTP (oauthH . contentH . acceptH 
                                                          $ case method of
                                                               POST -> let body = buildEncodedQuery params 
                                                                       in postRequestWithBody (mollomServer ++ "/" ++ path) 
                                                                          "application/x-www-form-urlencoded" 
                                                                          body
                                                               GET -> getRequest ("mollomserver" ++ "/" ++ path)
                                                          )
    ErrorT { runErrorT = return result }



