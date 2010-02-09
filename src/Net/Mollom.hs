{-# LANGUAGE TemplateHaskell #-}
-- |Interface to the Mollom API
module Net.Mollom
  ( getServerList
  , checkContent
  --, sendFeedback
  --, getImageCaptcha
  --, getAudioCaptcha
  --, checkCaptcha
  --, getStatistics
  --, verifyKey
  --, verifyKey
  , detectLanguage
  --, addBlacklistText
  --, removeBlackListText
  --, listBlacklistText
  --, addBlacklistURL
  --, removeBlackListURL
  --, listBlackListURL
  ) where


import Network.XmlRpc.Client
import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType (asXmlRpcStruct)

import Net.Mollom.Auth


mollomApiVersion :: String
mollomApiVersion = "1.0"

mollomFallbackServer :: String
mollomFallbackServer = "http://xmlrpc1.mollom.com/"


data MollomRequest = MollomRequest [(String, String)]


data MollomServerListResponse = MollomServerListResponse [String] deriving (Eq, Show)

instance XmlRpcType MollomServerListResponse where
  toValue (MollomServerListResponse ss) = toValue ss
  {- FIXME: this seems ugly! -}
  fromValue vs = do 
    ss <- case vs of
            ValueArray xs -> mapM fromValue xs
            _ -> fail "Unexpected return type"
    let ms = MollomServerListResponse ss
    return ms
  getType _ = TArray



data MollomDetectLanguageResponse = MollomDetectLanguageResponse { language :: String
                                                                 , confidence :: Double
                                                                 } deriving (Eq, Show)
$(asXmlRpcStruct ''MollomDetectLanguageResponse)
  

data MollomCheckContentResponse = MollomCheckContentResponse { spam :: Int
                                                             , quality :: Double
                                                             , session_id :: String
                                                             } deriving (Eq, Show)
$(asXmlRpcStruct ''MollomCheckContentResponse)



-- | make the actual XML-RPC call to the Mollom servers
service :: XmlRpcType a 
        => String -- ^public key
        -> String -- ^timestamp
        -> String -- ^hash
        -> String -- ^nonce
        -> MollomRequest
        -> IO a
service publicKey timeStamp hash nonce (MollomRequest fields) = do
  let requestStruct = [("public_key", publicKey), ("time", timeStamp), ("hash", hash), ("nonce", nonce)] ++ fields
  response <- remote (mollomFallbackServer ++ mollomApiVersion ++ "/") "mollom.getServerList" requestStruct
  return response



-- | request a list of Mollom servers that can handle a site's calls.
getServerList :: String -- ^public key
              -> String -- ^timestamp
              -> String -- ^hash
              -> String -- ^nonce
              -> IO MollomServerListResponse -- ^list of servers that can be used
getServerList publicKey timeStamp hash nonce = do
  let requestStruct = [("public_key", publicKey), ("time", timeStamp), ("hash", hash), ("nonce", nonce)]
  response <- remote (mollomFallbackServer ++ mollomApiVersion ++ "/") "mollom.getServerList" requestStruct
  return response


-- | asks Mollom whether the specified message is legitimate.
checkContent :: String -- ^public key
             -> String -- ^timestamp
             -> String -- ^hash
             -> String -- ^nonce
             -> [(String, String)] -- ^data
             -> IO MollomCheckContentResponse -- ^contains spam decision and session ID
checkContent publicKey timeStamp hash nonce ds = do
  let requestStruct = [("public_key", publicKey), ("time", timeStamp), ("hash", hash), ("nonce", nonce)] ++ ds
  response <- remote (mollomFallbackServer ++ mollomApiVersion ++ "/") "mollom.checkContent" requestStruct
  return response


-- | tells Mollom that the specifieed message was spam or otherwise abusive. mollom.sendFeedback 
-- | requests Mollom to generate a image CAPTCHA. mollom.getImageCaptcha 
-- | requests Mollom to generate an audio CAPTCHA. mollom.getAudioCaptcha 
-- | requests Mollom to verify the result of a CAPTCHA. mollom.checkCaptcha 
-- | retrieves usage statistics from Mollom. mollom.getStatistics 
-- | return a status value. mollom.verifyKey 
-- | return a status value. mollom.verifyKey 


-- | analyze text and return its most likely language code.
detectLanguage :: String -- ^public key
              -> String -- ^timestamp
              -> String -- ^hash
              -> String -- ^nonce
              -> String -- ^text to analyse
              -> IO MollomDetectLanguageResponse -- ^list of servers that can be used
detectLanguage publicKey timeStamp hash nonce text = do
  let requestStruct = [("public_key", publicKey), ("time", timeStamp), ("hash", hash), ("nonce", nonce), ("text", text)]
  response <- remote (mollomFallbackServer ++ mollomApiVersion ++ "/") "mollom.detectLanguage" requestStruct
  return response
 



-- | add text to your site's custom text blacklist. mollom.addBlacklistText 
-- | remove text from your site's custom text blacklist. mollom.removeBlackListText 
-- | return the contents of your site's custom text blacklist. mollom.listBlacklistText 
-- | add a URL to your site's custom URL blacklist. mollom.addBlacklistURL 
-- | remove a URL from your site's custom URL blacklist. mollom.removeBlackListURL 
-- | return the contents of your site's custom URL blacklist. mollom.listBlackListURL 

