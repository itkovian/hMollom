-- |Interface to the Mollom API
module Network.Mollom
  ( getServerList
  , checkContent
  , sendFeedback
  , getImageCaptcha
  , getAudioCaptcha
  , checkCaptcha
  , getStatistics
  , verifyKey
  , detectLanguage
  , addBlacklistText
  , removeBlacklistText
  , listBlacklistText
  , addBlacklistURL
  , removeBlacklistURL
  , listBlacklistURL
  , MollomConn(..)
  ) where

import Control.Monad.Error
import Data.Maybe(fromJust, isJust)
import Network.XmlRpc.Client
import Network.XmlRpc.Internals 
--import Network.XmlRpc.THDeriveXmlRpcType (asXmlRpcStruct)

import Network.Mollom.Auth


mollomApiVersion :: String
mollomApiVersion = "1.0"

mollomFallbackServer :: String
mollomFallbackServer = "http://xmlrpc2.mollom.com/"

-- FIXME: This should be specified in some configuration file
-- or use the system locale
mollomTimeFormat = "%Y-%m-%dT%H:%M:%S.000+0200"

-- |Describes connection with the Mollom server
data MollomConn = MollomConn
  { mcPublicKey :: String
  , mcPrivateKey :: String
  , mcSessionID :: String
  , mcServerList :: [String]
  }

-- |A computation that interacts with the Mollom server.
data MollomMonad a = MollomMonad (MollomConn -> IO (a, MollomConn))

instance Monad MollomMonad where
  return a = MollomMonad $ \conn -> return (a, conn)

  (MollomMonad m) >>= k = MollomMonad $ \conn -> do
    (a, conn') <- m conn
    let (MollomMonad m') = k a
    m' conn'


data MollomRequest = MollomRequest [(String, String)]

data MollomValue = MInt Int
                 | MBool Bool
                 | MDouble Double
                 | MString String deriving (Show, Eq)

instance XmlRpcType MollomValue where
  toValue (MInt i) = toValue i
  toValue (MBool b) = toValue b
  toValue (MDouble d) = toValue d
  toValue (MString s) = toValue s

  fromValue (ValueInt i) = maybeToM "" (Just (MInt i))
  fromValue (ValueBool b) = maybeToM "" (Just (MBool b)) 
  fromValue (ValueDouble d) = maybeToM "" (Just (MDouble d))
  fromValue (ValueString s) = maybeToM "" (Just (MString s))
  
  getType (MInt _) = TInt
  getType (MBool _) = TBool
  getType (MDouble _) = TDouble
  getType (MString _) = TString


-- | make the actual XML-RPC call to the Mollom servers
service :: XmlRpcType a 
        => MollomConn    -- ^connection to the Mollom service
        -> String        -- ^remote function name
        -> MollomRequest -- ^request specific data 
        -> IO a
service conn function (MollomRequest fields) = do
  let publicKey = mcPublicKey conn
      privateKey = mcPrivateKey conn
      timeStamp = getMollomTime mollomTimeFormat
      nonce = getMollomNonce
      hash = authenticate publicKey privateKey timeStamp nonce
      requestStruct = [("public_key", publicKey), ("time", timeStamp), ("hash", hash), ("nonce", nonce)] ++ fields
  response <- remote (mollomFallbackServer ++ mollomApiVersion ++ "/") function requestStruct
  return response
{-
-- | make the actual XML-RPC call to the Mollom server returning
--   the raw XML response, for debug purposes
--   This requires that haxr exports the post function!
service__ :: MollomConn    -- ^connection to the Mollom service
          -> String        -- ^remote function name
          -> MollomRequest -- ^request specific data
          -> IO String
service__ conn function (MollomRequest fields) = do
  let publicKey = mcPublicKey conn
      privateKey = mcPrivateKey conn
      timeStamp = getMollomTime mollomTimeFormat
      nonce = getMollomNonce
      hash = authenticate publicKey privateKey timeStamp nonce
  response <- post (mollomFallbackServer ++ mollomApiVersion ++ "/") (renderCall $ MethodCall function [toValue ([("public_key", publicKey), ("time", timeStamp), ("hash", hash), ("nonce", nonce)] ++ fields)] )
  return response
  -}


-- | request a list of Mollom servers that can handle a site's calls.
getServerList :: MollomConn -- ^connection to the Mollom service
              -> IO [String]-- ^list of servers that can be used
getServerList conn  = 
  service conn "mollom.getServerList" (MollomRequest [])


-- | asks Mollom whether the specified message is legitimate.
checkContent :: MollomConn -- ^connection to the Mollom service
             -> [(String, String)] -- ^data
             -> IO [(String, MollomValue)] -- ^contains spam decision and session ID
checkContent conn ds = 
  service conn "mollom.checkContent" (MollomRequest ds) 


-- | tells Mollom that the specifieed message was spam or otherwise abusive.
sendFeedback :: MollomConn -- ^connection to the Mollom service
             -> String -- ^session ID
             -> String -- ^feedback: "spam", "profanity", "low-quality" or "unwanted"
             -> IO Bool -- ^always returns True
sendFeedback conn sessionID feedback = do             
  service conn  "mollom.sendFeedback" (MollomRequest [("session_id", sessionID), ("feedback", feedback)])


-- | requests Mollom to generate a image CAPTCHA.
getImageCaptcha :: MollomConn -- ^connection to the Mollom service
                -> Maybe String -- ^session ID
                -> Maybe String -- ^author IP address
                -> IO [(String, MollomValue)] -- ^session ID and CAPTCHA url
getImageCaptcha conn sessionID authorIP = do
  let ds = map (\(n, v) -> (n, fromJust v)) $ filter (isJust . snd) [("session_id", sessionID), ("author_ip", authorIP)]
  service conn "mollom.getImageCaptcha" (MollomRequest ds)


-- | requests Mollom to generate an audio CAPTCHA
getAudioCaptcha :: MollomConn -- ^connection to the Mollom service
                -> Maybe String -- ^session ID
                -> Maybe String -- ^author IP address
                -> IO [(String, MollomValue)] -- ^session ID and CAPTCHA url
getAudioCaptcha conn sessionID authorIP = do
  let ds = map (\(n, v) -> (n, fromJust v)) $ filter (isJust . snd) [("session_id", sessionID), ("author_ip", authorIP)]
  service conn "mollom.getAudioCaptcha" (MollomRequest ds)


-- | requests Mollom to verify the result of a CAPTCHA.
checkCaptcha :: MollomConn -- ^connection to the Mollom service
             -> String -- ^session ID associated with the CAPTCHA
             -> String -- ^solution to the CAPTCHA
             -> IO Bool -- ^True if correct, False if wrong
checkCaptcha conn sessionID solution = do
  let ds = [("session_id", sessionID), ("solution", solution)]
  service conn "mollom.checkCaptcha" (MollomRequest ds)


-- | retrieves usage statistics from Mollom.
getStatistics :: MollomConn -- ^connection to the Mollom service
              -> String -- ^type of statistics demanded
                        -- total_days — Number of days Mollom has been used.
                        -- total_accepted — Total accepted posts.
                        -- total_rejected — Total rejected spam posts.
                        -- yesterday_accepted — Number of posts accepted yesterday.
                        -- yesterday_rejected — Number of spam posts blocked yesterday.
                        -- today_accepted — Number of posts accepted today.
                        -- today_rejected — Number of spam posts rejected today.
              -> IO Int -- ^Value of requested statistic
getStatistics conn statType = do
  service conn "mollom.getStatistics" (MollomRequest [("type", statType)])


-- | return a status value.
verifyKey :: MollomConn -- ^connection to the Mollom service
          -> IO Bool -- ^Always returns True
verifyKey conn = do
  service conn "mollom.verifyKey" (MollomRequest [])


-- | analyze text and return its most likely language code.
detectLanguage :: MollomConn -- ^connection to the Mollom service
              -> String -- ^text to analyse
              -- -> IO [[DetectLanguageResponseStruct]] -- ^list of (language, confidence) tuples
              -> IO [[(String, MollomValue)]] -- ^list of (language, confidence) tuples
detectLanguage conn text = do
  service conn "mollom.detectLanguage" (MollomRequest [("text", text)]) 


-- | add text to your site's custom text blacklist.
addBlacklistText :: MollomConn -- ^connection to the Mollom service
                 -> String -- ^text to blacklist
                 -> String -- ^match used to search for the text, either "exact" or "contains"
                 -> String -- ^reason: "spam", "profanity", "low-quality", or "unwanted"
                 -> IO Bool -- ^always returns True
addBlacklistText conn text match reason = do
  let ds = [("text", text), ("match", match), ("reason", reason)]
  service conn "mollom.addBlacklistText" (MollomRequest ds)


-- | remove text from your site's custom text blacklist.
removeBlacklistText :: MollomConn -- ^connection to the Mollom service
                 -> String -- ^text to blacklist
                 -> IO Bool -- ^always returns True
removeBlacklistText conn text = do
  let ds = [("text", text)]
  service conn "mollom.removeBlacklistText" (MollomRequest ds)


-- | return the contents of your site's custom text blacklist.
listBlacklistText :: MollomConn -- ^connection to the Mollom service
                  -- -> IO [[(String, MollomValue)]] -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
                  -> IO () -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
listBlacklistText conn = do
  r <- service conn "mollom.listBlacklistText" (MollomRequest []) 
  p <- handleError fail (parseResponse r)
  putStrLn $ show p

-- | add a URL to your site's custom URL blacklist.
addBlacklistURL :: MollomConn -- ^connection to the Mollom service
                -> String -- ^URL to be added to custom URL blacklist for the website identified by the public and private keypair
                -> IO Bool -- ^always returns True
addBlacklistURL conn url = do
  let ds = [("url", url)]
  service conn "mollom.addBlacklistURL" (MollomRequest ds)


-- | remove a URL from your site's custom URL blacklist.
removeBlacklistURL :: MollomConn -- ^connection to the Mollom service
                   -> String -- ^URL to be removed from the custom URL blacklist for the website identified by the public and private keypair
                   -> IO Bool -- ^always returns True
removeBlacklistURL conn url = do
  let ds = [("url", url)]
  service conn "mollom.removeBlacklistURL" (MollomRequest ds)


-- | return the contents of your site's custom URL blacklist.
listBlacklistURL :: MollomConn -- ^connection to the Mollom service
                 -> IO [[(String, MollomValue)]] -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
listBlacklistURL conn = do
  service conn "mollom.listBlacklistURL" (MollomRequest [])


