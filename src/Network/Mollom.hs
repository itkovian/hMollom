{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |Interface to the Mollom API
module Network.Mollom
  ( getServerList
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
  , MollomConfiguration(..)
  , MollomValue(..)
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromJust, isJust)
import Network.XmlRpc.Client
import Network.XmlRpc.Internals 
--import Network.XmlRpc.THDeriveXmlRpcType (asXmlRpcStruct)

import Network.Mollom.Internals
import Network.Mollom.Auth
import Network.Mollom.MollomMonad


mollomFallbackServer :: String
mollomFallbackServer = "http://xmlrpc2.mollom.com/"


-- |The hardcoded server list
-- XXX: this should go at the back of the state tracking the serverlist, rather than have it here. When the
-- state tracked list is empty, then we refill it next time with this list
hardCodedMollomServerList :: [String]
hardCodedMollomServerList = ["http://xmlrpc1.mollom.com", "http://xmlrpc2.mollom.com", "http://xmlrpc3.mollom.com"]

data MollomRequest = MollomRequest [(String, String)]

data MollomError = MollomInternalError
                 | MollomRefresh
                 | MollomServerBusy deriving (Eq, Ord, Show)


-- | Retrieve a new server list using the MollomMonad approach
-- This is a computation that may fail. Upon failure, we return Nothing.
-- This function should only be called from the service function. There is
-- no fall back if this fails, as we need a valid list of servers to begin with.
retrieveNewServerList :: MollomConfiguration -> IO (Maybe [String])
retrieveNewServerList config = do
  let publicKey = mcPublicKey config
      privateKey = mcPrivateKey config
      apiVersion = mcAPIVersion config
      requestStruct = getAuthenticationInformation publicKey privateKey
  newServerList <- rsl requestStruct apiVersion hardCodedMollomServerList
  case newServerList of
    Left s -> return Nothing
    Right sl -> return $ Just sl


-- | rsl does the actual polling of the service' function.
-- Keeping this as a top-level function for now. 
rsl :: [(String, String)] -> String -> [String] -> IO (Either String [String])
rsl _ _ [] = return $ Left "Error: no more servers left"
rsl rq a (server:ss) = do
   response <- service' rq server a "mollom.getServerList" 
   case response of
      Left s -> rsl rq a ss
      Right v -> return $ Right v


-- | The 'service' function implements the basic load balancing scheme from
-- <http://mollom.com/api/client-side-load-balancing>. 
--
-- FIXME: It expects the MollomConfiguration record to contain a list of valid servers,
-- obtained either through a cache or a call to getServerList with one of the
-- hardCoded server.
service :: XmlRpcType a
        => String        -- ^remote function name
        -> MollomRequest -- ^request specific data 
        -> ErrorT String MollomState a
service function (MollomRequest fields) = do
  config <- ask
  let publicKey = mcPublicKey config
      privateKey = mcPrivateKey config
      apiVersion = mcAPIVersion config
      requestStruct = (getAuthenticationInformation publicKey privateKey) ++ fields
  serviceLoop config requestStruct apiVersion function 


serviceLoop :: XmlRpcType a => MollomConfiguration -> [(String, String)] -> String -> String -> ErrorT String MollomState a
serviceLoop config r a f = serviceLoop' 
  where serviceLoop' = do serverList <- get
                          case serverList of
                            UninitialisedServerList -> refetchAndLoop
                            MollomServerList [] -> fail "No more servers"
                            MollomServerList (server:ss) -> do response <- liftIO $ service' r server a f
                                                               case response of 
                                                                  Left s -> case take 10 s of
                                                                              "Error 1100" -> refetchAndLoop 
                                                                              "Error 1000" -> fail "Mollom Error"
                                                                              _            -> put (MollomServerList ss) >> serviceLoop'
                                                                  Right v -> return v
        refetchAndLoop = do nsl <- liftIO $ retrieveNewServerList config
                            case nsl of
                              Nothing -> fail "No server available"
                              Just sl -> put (MollomServerList sl) >> serviceLoop'

-- | Make the actual call to the given Mollom server. 
-- 
-- We unwrap the ErrorT such that we get the Left error message back when Mollom gives a fault. 
service' :: XmlRpcType a => [(String, String)] -> String -> String -> String -> IO (Either String a)
service' requestStruct server api function = do
  response <- runErrorT $ call (mollomFallbackServer ++ mollomApiVersion ++ "/") function [(toValue . map (\(k, v) -> (k, toValue v)) $ requestStruct)]
  case response of 
    Left s -> return $ Left s
    Right v -> runErrorT $ fromValue v


-- | request a list of Mollom servers that can handle a site's calls.
getServerList :: MollomMonad [String]
getServerList = do
  put Nothing -- no session ID here
  let returnStateT a = StateT $ \s -> liftM (flip (,) s) a 
  response <- ErrorT . returnStateT . runErrorT $ service "mollom.getServerList" (MollomRequest [])
  lift . lift . put $ (MollomServerList response)
  return response
  

  
{-
-- | asks Mollom whether the specified message is legitimate.
checkContent :: MollomConfiguration -- ^connection to the Mollom service
             -> [(String, String)] -- ^data
             -> IO (Either String [(String, MollomValue)]) -- ^contains spam decision and session ID
checkContent conn ds = service conn "mollom.checkContent" (MollomRequest ds) 


-- | tells Mollom that the specified message was spam or otherwise abusive.
sendFeedback :: MollomConfiguration -- ^connection to the Mollom service
             -> String -- ^session ID
             -> String -- ^feedback: "spam", "profanity", "low-quality" or "unwanted"
             -> IO (Either String Bool) -- ^always returns True
sendFeedback conn sessionID feedback = service conn  "mollom.sendFeedback" (MollomRequest [("session_id", sessionID), ("feedback", feedback)])


-- | requests Mollom to generate a image CAPTCHA.
getImageCaptcha :: MollomConfiguration -- ^connection to the Mollom service
                -> Maybe String -- ^session ID
                -> Maybe String -- ^author IP address
                -> IO (Either String [(String, MollomValue)]) -- ^session ID and CAPTCHA url
getImageCaptcha conn sessionID authorIP = do
  let ds = map (\(n, v) -> (n, fromJust v)) $ filter (isJust . snd) [("session_id", sessionID), ("author_ip", authorIP)]
  service conn "mollom.getImageCaptcha" (MollomRequest ds)


-- | requests Mollom to generate an audio CAPTCHA
getAudioCaptcha :: MollomConfiguration -- ^connection to the Mollom service
                -> Maybe String -- ^session ID
                -> Maybe String -- ^author IP address
                -> IO (Either String [(String, MollomValue)]) -- ^session ID and CAPTCHA url
getAudioCaptcha conn sessionID authorIP = do
  let ds = map (\(n, v) -> (n, fromJust v)) $ filter (isJust . snd) [("session_id", sessionID), ("author_ip", authorIP)]
  service conn "mollom.getAudioCaptcha" (MollomRequest ds)


-- | requests Mollom to verify the result of a CAPTCHA.
checkCaptcha :: MollomConfiguration -- ^connection to the Mollom service
             -> String -- ^session ID associated with the CAPTCHA
             -> String -- ^solution to the CAPTCHA
             -> IO (Either String Bool) -- ^True if correct, False if wrong
checkCaptcha conn sessionID solution = do
  let ds = [("session_id", sessionID), ("solution", solution)]
  service conn "mollom.checkCaptcha" (MollomRequest ds)


-- | retrieves usage statistics from Mollom.
getStatistics :: MollomConfiguration -- ^connection to the Mollom service
              -> String -- ^type of statistics demanded
                        -- total_days — Number of days Mollom has been used.
                        -- total_accepted — Total accepted posts.
                        -- total_rejected — Total rejected spam posts.
                        -- yesterday_accepted — Number of posts accepted yesterday.
                        -- yesterday_rejected — Number of spam posts blocked yesterday.
                        -- today_accepted — Number of posts accepted today.
                        -- today_rejected — Number of spam posts rejected today.
              -> IO (Either String Int) -- ^Value of requested statistic
getStatistics conn statType = do
  service conn "mollom.getStatistics" (MollomRequest [("type", statType)])


-- | return a status value.
verifyKey :: MollomConfiguration -- ^connection to the Mollom service
          -> IO (Either String Bool) -- ^Always returns True
verifyKey conn = do
  service conn "mollom.verifyKey" (MollomRequest [])


-- | analyze text and return its most likely language code.
detectLanguage :: MollomConfiguration -- ^connection to the Mollom service
              -> String -- ^text to analyse
              -- -> IO [[DetectLanguageResponseStruct]] -- ^list of (language, confidence) tuples
              -> IO (Either String [[(String, MollomValue)]]) -- ^list of (language, confidence) tuples
detectLanguage conn text = do
  service conn "mollom.detectLanguage" (MollomRequest [("text", text)]) 


-- | add text to your site's custom text blacklist.
addBlacklistText :: MollomConfiguration -- ^connection to the Mollom service
                 -> String -- ^text to blacklist
                 -> String -- ^match used to search for the text, either "exact" or "contains"
                 -> String -- ^reason: "spam", "profanity", "low-quality", or "unwanted"
                 -> IO (Either String Bool) -- ^always returns True
addBlacklistText conn text match reason = do
  let ds = [("text", text), ("match", match), ("reason", reason)]
  service conn "mollom.addBlacklistText" (MollomRequest ds)


-- | remove text from your site's custom text blacklist.
removeBlacklistText :: MollomConfiguration -- ^connection to the Mollom service
                 -> String -- ^text to blacklist
                 -> IO (Either String Bool) -- ^always returns True
removeBlacklistText conn text = do
  let ds = [("text", text)]
  service conn "mollom.removeBlacklistText" (MollomRequest ds)


-- | return the contents of your site's custom text blacklist.
listBlacklistText :: MollomConfiguration -- ^connection to the Mollom service
                  -- -> IO [[(String, MollomValue)]] -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
                  -> IO (Either String [[(String, MollomValue)]]) -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
listBlacklistText conn = do
  service conn "mollom.listBlacklistText" (MollomRequest []) 


-- | add a URL to your site's custom URL blacklist.
addBlacklistURL :: MollomConfiguration -- ^connection to the Mollom service
                -> String -- ^URL to be added to custom URL blacklist for the website identified by the public and private keypair
                -> IO (Either String Bool) -- ^always returns True
addBlacklistURL conn url = do
  let ds = [("url", url)]
  service conn "mollom.addBlacklistURL" (MollomRequest ds)


-- | remove a URL from your site's custom URL blacklist.
removeBlacklistURL :: MollomConfiguration -- ^connection to the Mollom service
                   -> String -- ^URL to be removed from the custom URL blacklist for the website identified by the public and private keypair
                   -> IO (Either String Bool) -- ^always returns True
removeBlacklistURL conn url = do
  let ds = [("url", url)]
  service conn "mollom.removeBlacklistURL" (MollomRequest ds)


-- | return the contents of your site's custom URL blacklist.
listBlacklistURL :: MollomConfiguration -- ^connection to the Mollom service
                 -> IO (Either String [[(String, MollomValue)]]) -- ^List of the current blacklisted URLs for the website corresponding to the public and private keypair
listBlacklistURL conn = do
  service conn "mollom.listBlacklistURL" (MollomRequest [])


-}
