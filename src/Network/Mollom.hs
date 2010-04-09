{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , MollomConfiguration(..)
  , MollomValue(..)
  ) where

import Control.Arrow (second)
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
        -> ErrorT MollomError MollomState a
service function (MollomRequest fields) = do
  config <- ask
  let publicKey = mcPublicKey config
      privateKey = mcPrivateKey config
      apiVersion = mcAPIVersion config
      requestStruct = getAuthenticationInformation publicKey privateKey ++ fields
  serviceLoop config requestStruct apiVersion function 


serviceLoop :: XmlRpcType a => MollomConfiguration -> [(String, String)] -> String -> String -> ErrorT MollomError MollomState a
serviceLoop config r a f = serviceLoop' 
  where serviceLoop' = do serverList <- get
                          case serverList of
                            UninitialisedServerList -> refetchAndLoop
                            MollomServerList [] -> throwError (HMollomError "No servers available")
                            MollomServerList (server:ss) -> do response <- liftIO $ service' r server a f
                                                               case response of 
                                                                  Left s -> case take 10 s of
                                                                              "Error 1100" -> refetchAndLoop 
                                                                              "Error 1000" -> throwError MollomInternalError
                                                                              _            -> put (MollomServerList ss) >> serviceLoop'
                                                                  Right v -> return v
        refetchAndLoop = do nsl <- liftIO $ retrieveNewServerList config
                            case nsl of
                              Nothing -> throwError (HMollomError "No servers available")
                              Just sl -> put (MollomServerList sl) >> serviceLoop'

-- | Make the actual call to the given Mollom server. 
-- 
-- We unwrap the ErrorT such that we get the Left error message back when Mollom gives a fault. 
service' :: XmlRpcType a => [(String, String)] -> String -> String -> String -> IO (Either String a)
service' requestStruct server api function = do
  response <- runErrorT $ call (mollomFallbackServer ++ mollomApiVersion ++ "/") function [toValue . map (second toValue) $ requestStruct]
  case response of 
    Left s -> return $ Left s
    Right v -> runErrorT $ fromValue v

returnStateT a = StateT $ \s -> liftM (flip (,) s) a

-- | request a list of Mollom servers that can handle a site's calls.
getServerList :: MollomMonad [String]
getServerList = do
  put Nothing -- no session ID here
  response <- ErrorT . returnStateT . runErrorT $ service "mollom.getServerList" (MollomRequest [])
  lift . lift . put $ MollomServerList response
  return response
  
-- | asks Mollom whether the specified message is legitimate.
checkContent :: [(String, String)] -- ^data
             -> MollomMonad [(String, MollomValue)] -- ^contains spam decision and session ID
checkContent ds = do
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
             -> MollomMonad Bool
checkCaptcha solution = do
  --sessionID <- get
  let sessionID = Just "ll"
  case sessionID of
    Nothing -> undefined -- throwError (HMollomError "Mollom Error: no session ID provided")
    Just s -> do let mRequest = MollomRequest [("session_id", s), ("solution", solution)]
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


