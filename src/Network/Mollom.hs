{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Interface to the Mollom web service (http://mollom.com),
--   based on the beta REST API, see http://mollom.com/api/rest.
module Network.Mollom
  (-- getServerList
  --, checkContent
  --, sendFeedback
  --, getImageCaptcha
  --, getAudioCaptcha
  --, checkCaptcha
  --, getStatistics
  --, verifyKey
  --, detectLanguage
  --, addBlacklistText
  --, removeBlacklistText
  --, listBlacklistText
  --, addBlacklistURL
  --, removeBlacklistURL
  --, listBlacklistURL
  --, MollomConfiguration(..)
  --, MollomValue(..)
  service
  ) where

import Control.Arrow (second)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate, sort)
import Data.Maybe(fromJust, isJust)
import Network.HTTP (getRequest, postRequestWithBody, simpleHTTP)
import Network.HTTP.Base (HTTPResponse, RequestMethod(..), urlEncode)
import Network.HTTP.Headers (HeaderName (HdrContentType, HdrAccept), HasHeaders, replaceHeader)
import Network.XmlRpc.Client
import Network.XmlRpc.Internals 
--import Network.XmlRpc.THDeriveXmlRpcType (asXmlRpcStruct)

import Network.Mollom.Internals
import Network.Mollom.OAuth
import Network.Mollom.MollomMonad


mollomServer :: String
mollomServer = "http://dev.mollom.com/v1"

testPubKey = "buid5peb664h1fcezn1da61mp143wff6"
testPrivKey = "14m7q8al6ph6k1smu6qmb3mk89o435f9"

data MollomRequest = MollomRequest [(String, String)]


-- |
buildEncodedQuery :: [(String, String)] -- ^ The name-value pairs for the query
                  -> String
buildEncodedQuery ps = 
    let sps = sort ps
    in intercalate "&" $ map (\(n, v) -> intercalate "=" [urlEncode n, urlEncode v]) sps


-- | The service function is the common entrypoint to use the Mollom service. This
--   is where we actually send the data to Mollom.
--   FIXME: implement the maximal number of retries
service :: String             -- ^ Public key
        -> String             -- ^ Private key
        -> RequestMethod      -- ^ The HTTP method used in this request.
        -> String             -- ^ The path to the requested resource
        -> [(String, String)] -- ^ Request parameters
        -> [String]           -- ^ Expected returned values
        -> IO (HTTPResponse String)
service publicKey privateKey method path params expected = do
    let oauthHVs = oauthHeaderValues publicKey OAuthHmacSha1
        oauthSig = oauthSignature OAuthHmacSha1 privateKey method mollomServer path (buildEncodedQuery $ params ++ oauthHVs)

        oauthH   = oauthHeader (("oauth_signature", oauthSig) : oauthHVs)
        contentH = replaceHeader HdrContentType "application/x-www-form-urlencoded"
        acceptH  = replaceHeader HdrAccept "application/json;q=0.8"

    putStrLn $ "Generated OAuth header values: " ++ show oauthHVs
    putStrLn $ "Generated OAuth signature: " ++ show oauthSig
    putStrLn $ "Encoded query of the parameters and oauth header info: " ++ show (buildEncodedQuery $ params ++ oauthHVs)
    putStrLn $ "HTTP request: " ++ show ( oauthH . contentH . acceptH 
                                        $ postRequestWithBody (mollomServer ++ "/" ++ path) "application/x-www-form-urlencoded" (buildEncodedQuery params))

    result <- simpleHTTP (oauthH . contentH . acceptH 
                         $ case method of
                              POST -> let body = buildEncodedQuery params 
                                      in postRequestWithBody (mollomServer ++ "/" ++ path) "application/x-www-form-urlencoded" body
                              GET -> getRequest ("mollomserver" ++ "/" ++ path)
                         )
    case result of
        Left e -> undefined
        Right r -> return r


{-
-- | asks Mollom whether the specified message is legitimate.
checkContent :: Maybe String -- ^Current session ID
              -> Maybe String -- ^Title of submitted post
              -> Maybe String -- ^Body of submitted post
              -> Maybe String -- ^Submitting user's name or nick
              -> Maybe String -- ^Submitting user's URL
              -> Maybe String -- ^Submitting user's email address
              -> Maybe String -- ^Submitting user's openID
              -> Maybe String -- ^Submitting user's current IP
              -> Maybe String -- ^Submitting user's unique site ID
              -> MollomMonad [(String, MollomValue)] -- ^The monad in which the function is executing
checkContent sessionID title body authorName authorURL authorEmail authorOpenID authorIP authorSiteID =
  let kvs = catSecondMaybes [("session_id", sessionID)
                            ,("post_title", title)
                            ,("post_body", body)
                            ,("author_name", authorName)
                            ,("author_url", authorURL)
                            ,("author_mail", authorEmail)
                            ,("author_openid", authorOpenID)
                            ,("author_ip", authorIP)
                            ,("author_id", authorSiteID)]
  in checkContent' kvs
  
checkContent' :: [(String, String)] -- ^data
             -> MollomMonad [(String, MollomValue)] -- ^contains spam decision and session ID
checkContent' ds = do
  response <- ErrorT . returnStateT . runErrorT $ service "mollom.checkContent" (MollomRequest ds)
  case lookup "session_id" response of
    Nothing -> put Nothing
    Just (MString sessionID) -> put $ Just sessionID 
  return response


-- | tells Mollom that the specified message was spam or otherwise abusive.
sendFeedback :: String -- ^feedback: "spam", "profanity", "low-quality" or "unwanted"
            -> MollomMonad Bool -- ^always returns true
sendFeedback feedback = do
  sessionID <- get 
  case sessionID of
    Nothing -> throwError $ HMollomError "Mollom Error: no session ID provided"
    Just s -> do let mRequest = MollomRequest [("session_id", s), ("feedback", feedback)]
                 ErrorT . returnStateT . runErrorT $ service "mollom.sendFeedback" mRequest


-- | requests Mollom to generate a image CAPTCHA.
getImageCaptcha :: Maybe String -- ^author IP address
                -> MollomMonad [(String, MollomValue)] -- ^session ID and CAPTCHA url
getImageCaptcha authorIP = do
  sessionID <- get
  let mRequest = MollomRequest $ map (second fromJust) $ filter (isJust . snd) [("session_id", sessionID), ("author_ip", authorIP)]
  response <- ErrorT . returnStateT . runErrorT $ service "mollom.getImageCaptcha" mRequest
  case lookup "session_id" response of
    Nothing -> put Nothing
    Just (MString s) -> put $ Just s
  return response

-- | requests Mollom to generate an audio CAPTCHA
getAudioCaptcha :: Maybe String -- ^author IP address
                -> MollomMonad [(String, MollomValue)]
getAudioCaptcha authorIP = do
  sessionID <- get
  let mRequest = MollomRequest $ map (second fromJust) $ filter (isJust . snd) [("session_id", sessionID), ("author_ip", authorIP)]
  response <- ErrorT . returnStateT . runErrorT $ service "mollom.getAudioCaptcha" mRequest
  case lookup "session_id" response of
    Nothing -> put Nothing
    Just (MString s) -> put $ Just s
  return response

  

-- | requests Mollom to verify the result of a CAPTCHA.
checkCaptcha :: String -- ^solution to the CAPTCHA
             -> Maybe String -- ^author IP address
             -> MollomMonad Bool
checkCaptcha solution authorIP = do
  sessionID <- get
  case sessionID of
    Nothing -> throwError (HMollomError "Mollom Error: no session ID provided")
    Just s -> do let mRequest = MollomRequest $ catSecondMaybes [("session_id", Just s), ("solution", Just solution), ("author_ip", authorIP)]
                 ErrorT . returnStateT . runErrorT $ service "mollom.checkCaptcha" mRequest

-- | retrieves usage statistics from Mollom.
getStatistics :: String -- ^type of statistics demanded
                        -- total_days — Number of days Mollom has been used.
                        -- total_accepted — Total accepted posts.
                        -- total_rejected — Total rejected spam posts.
                        -- yesterday_accepted — Number of posts accepted yesterday.
                        -- yesterday_rejected — Number of spam posts blocked yesterday.
                        -- today_accepted — Number of posts accepted today.
                        -- today_rejected — Number of spam posts rejected today.
              -> MollomMonad Int -- ^Value of requested statistic
getStatistics statType = do
  let mRequest = MollomRequest [("type", statType)]
  ErrorT . returnStateT . runErrorT $ service "mollom.getStatistics" mRequest


-- | return a status value.
verifyKey :: MollomMonad Bool -- ^Always returns True
verifyKey = ErrorT . returnStateT . runErrorT $ service "mollom.verifyKey" (MollomRequest [])


-- | analyze text and return its most likely language code.
detectLanguage :: String -- ^text to analyse
              -> MollomMonad [[(String, MollomValue)]] -- ^list of (language, confidence) tuples
detectLanguage text = ErrorT . returnStateT . runErrorT $ service "mollom.detectLanguage" (MollomRequest [("text", text)]) 


-- | add text to your site's custom text blacklist.
addBlacklistText :: String -- ^text to blacklist
                 -> String -- ^match used to search for the text, either "exact" or "contains"
                 -> String -- ^reason: "spam", "profanity", "low-quality", or "unwanted"
                 -> MollomMonad Bool -- ^always returns True
addBlacklistText text match reason = do
  let mRequest = MollomRequest [("text", text), ("match", match), ("reason", reason)]
  ErrorT . returnStateT . runErrorT $ service "mollom.addBlacklistText" mRequest


-- | remove text from your site's custom text blacklist.
removeBlacklistText :: String -- ^text to blacklist
                    -> MollomMonad Bool -- ^always returns True
removeBlacklistText text = do
  let mRequest = MollomRequest [("text", text)]
  ErrorT . returnStateT . runErrorT $ service "mollom.removeBlacklistText" mRequest


-- | return the contents of your site's custom text blacklist.
listBlacklistText :: MollomMonad [[(String, MollomValue)]] -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
listBlacklistText = ErrorT . returnStateT . runErrorT $ service "mollom.listBlacklistText" (MollomRequest []) 


-- | add a URL to your site's custom URL blacklist.
addBlacklistURL :: String -- ^URL to be added to custom URL blacklist for the website identified by the public and private keypair
                -> MollomMonad Bool -- ^always returns True
addBlacklistURL url = do
  let mRequest = MollomRequest [("url", url)]
  ErrorT . returnStateT . runErrorT $ service "mollom.addBlacklistURL" mRequest


-- | remove a URL from your site's custom URL blacklist.
removeBlacklistURL :: String -- ^URL to be removed from the custom URL blacklist for the website identified by the public and private keypair
                   -> MollomMonad Bool -- ^always returns True
removeBlacklistURL url = do
  let mRequest = MollomRequest [("url", url)]
  ErrorT . returnStateT . runErrorT $ service "mollom.removeBlacklistURL" mRequest


-- | return the contents of your site's custom URL blacklist.
listBlacklistURL :: MollomMonad [[(String, MollomValue)]] -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
listBlacklistURL = ErrorT . returnStateT . runErrorT $ service "mollom.listBlacklistURL" (MollomRequest [])

-}
