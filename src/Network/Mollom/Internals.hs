{-# LANGUAGE OverloadedStrings #-}
{- 
 - (C) 2012, Andy Georges
 -
 - This module provides internal functions to allow using the Mollom service
 -}

module Network.Mollom.Internals 
  ( mollomApiVersion
  , MollomConfiguration(..)
  , MollomError(..)
  , MollomResponse(..)
  , service
  , serviceNoAuth
  ) where

import           Control.Arrow (second)
import           Control.Monad.Error
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List (intercalate, lookup, sort)
import           Data.Maybe(fromJust, isJust)
import           Network.HTTP (getRequest, postRequestWithBody, simpleHTTP)
import           Network.HTTP.Base (HTTPResponse, RequestMethod(..), Response(..), ResponseCode, urlEncode)
import           Network.HTTP.Headers (HeaderName (HdrContentType, HdrAccept), replaceHeader)
import           Network.HTTP.Stream (ConnError(..), Result)

import Network.Mollom.OAuth
import Network.Mollom.Types

mollomServer :: String
mollomServer = "http://dev.mollom.com/v1"

catSecondMaybes :: [(k, Maybe v)] -> [(k, v)]
catSecondMaybes = map (second fromJust) . filter (isJust . snd)


-- | Encode the parameters after sorting them.
buildEncodedQuery :: [(String, String)] -- ^ The name-value pairs for the query
                  -> String
buildEncodedQuery ps = 
    let sps = sort ps
    in intercalate "&" $ map (\(n, v) -> intercalate "=" [urlEncode n, urlEncode v]) sps



-- | Process the response from the Mollom server, checking the return code.
--   If there is a connection error, we immediately return the appropriate
--   error. Otherwise, in case of an error, we check the code and return
--   the appropriate instance.
processResponse :: A.FromJSON a => [(ResponseCode, MollomError)] -> Result (HTTPResponse String) -> Either MollomError (MollomResponse a)
processResponse errors result =
    case result of 
        Left ce -> Left (ConnectionError ce)
        Right r -> let code = rspCode r
                       message = rspReason r
                   in case lookup code errors of
                          Just e  -> Left (addMessage e message)
                          Nothing -> let response = A.decode' . BS.pack $ rspBody r
                                     in case response of 
                                            Nothing -> Left JSONParseError
                                            Just r' -> Right MollomResponse { code = code
                                                                            , message = message
                                                                            , response = r'
                                                                            }

-- | The service function is the common entrypoint to use the Mollom service. This
--   is where we actually send the data to Mollom.
--   FIXME: implement the maximal number of retries
service :: A.FromJSON a
        => String                               -- ^Public key
        -> String                               -- ^Private key
        -> RequestMethod                        -- ^The HTTP method used in this request.
        -> String                               -- ^The path to the requested resource
        -> [(String, Maybe String)]             -- ^Request parameters
        -> [String]                             -- ^Expected returned values
        -> [((Int, Int, Int), MollomError)]     -- ^Possible error values
        -> ErrorT MollomError IO (MollomResponse a)-- :: ErrorT (IO (Either MollomError (HTTPResponse String)))
service publicKey privateKey method path params expected errors = do
    let params' = catSecondMaybes params
        oauthHVs = oauthHeaderValues publicKey OAuthHmacSha1
        oauthSig = oauthSignature OAuthHmacSha1 privateKey method mollomServer path 
                                  (buildEncodedQuery $ params' ++ oauthHVs)
        oauthH   = oauthHeader (("oauth_signature", oauthSig) : oauthHVs)
        contentH = replaceHeader HdrContentType "application/x-www-form-urlencoded"
        acceptH  = replaceHeader HdrAccept "application/json;q=0.8"
    --liftIO $ putStrLn $ "URI = " ++ (mollomServer ++ "/" ++ path)
    result <- liftIO $ liftM (processResponse errors) $ simpleHTTP (oauthH . contentH . acceptH 
                                                          $ case method of
                                                               POST -> let body = buildEncodedQuery params'
                                                                       in postRequestWithBody (mollomServer ++ "/" ++ path) 
                                                                          "application/x-www-form-urlencoded" 
                                                                          body
                                                               GET -> getRequest (mollomServer ++ "/" ++ path)
                                                               _ -> undefined
                                                          )
    ErrorT { runErrorT = return result }


-- | This should only be used by the createSite API call. Otherwise, it will return an error.
serviceNoAuth :: A.FromJSON a
              => RequestMethod                        -- ^The HTTP method used in this request.
              -> String                               -- ^The path to the requested resource
              -> [(String, Maybe String)]             -- ^Request parameters
              -> [String]                             -- ^Expected returned values
              -> [((Int, Int, Int), MollomError)]     -- ^Possible error values
              -> ErrorT MollomError IO (MollomResponse a)-- :: ErrorT (IO (Either MollomError (HTTPResponse String)))
serviceNoAuth method path params expected errors = do
    let params' = catSecondMaybes params
        contentH = replaceHeader HdrContentType "application/x-www-form-urlencoded"
        acceptH  = replaceHeader HdrAccept "application/json;q=0.8"
    result <- liftIO $ liftM (processResponse errors) $ simpleHTTP (contentH . acceptH
                                                          $ case method of
                                                               POST -> let body = buildEncodedQuery params'
                                                                       in postRequestWithBody (mollomServer ++ "/" ++ path) 
                                                                          "application/x-www-form-urlencoded" 
                                                                          body
                                                               GET -> getRequest (mollomServer ++ "/" ++ path)
                                                               _ -> undefined
                                                          )
    ErrorT { runErrorT = return result }
