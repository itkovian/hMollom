-- |Interface to the Mollom API
module Net.Mollom
  (
  , getServerList
  --, checkContent
  --, sendFeedback
  --, getImageCaptcha
  --, getAudioCaptcha
  --, checkCaptcha
  --, getStatistics
  --, verifyKey
  --, verifyKey
  --, detectLanguage
  --, addBlacklistText
  --, removeBlackListText
  --, listBlacklistText
  --, addBlacklistURL
  --, removeBlackListURL
  --, listBlackListURL
  ) where


import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import Net.Mollom.Auth


MOLLOM_API_VERSION :: String
MOLLOM_API_VERSION = "1.0"

MOLLOM_FALLBACK_SERVER :: String
MOLLOM_FALLBACK_SERVER = "http://http://xmlrpc1.mollom.com"

-- | request a list of Mollom servers that can handle a site's calls.
getServerList :: String -- ^public key
              -> String -- ^timestamp
              -> String -- ^hash
              -> String -- ^nonce
              -> [String] -- ^list of servers that can be used
getServerList publicKey timeStamp hash nonce 

-- | asks Mollom whether the specied message is legitimate.
--checkContent 

-- | tells Mollom that the specifieed message was spam or otherwise abusive. mollom.sendFeedback 
-- | requests Mollom to generate a image CAPTCHA. mollom.getImageCaptcha 
-- | requests Mollom to generate an audio CAPTCHA. mollom.getAudioCaptcha 
-- | requests Mollom to verify the result of a CAPTCHA. mollom.checkCaptcha 
-- | retrieves usage statistics from Mollom. mollom.getStatistics 
-- | return a status value. mollom.verifyKey 
-- | return a status value. mollom.verifyKey 
-- | analyze text and return its most likely language code. mollom.detectLanguage 
-- | add text to your site's custom text blacklist. mollom.addBlacklistText 
-- | remove text from your site's custom text blacklist. mollom.removeBlackListText 
-- | return the contents of your site's custom text blacklist. mollom.listBlacklistText 
-- | add a URL to your site's custom URL blacklist. mollom.addBlacklistURL 
-- | remove a URL from your site's custom URL blacklist. mollom.removeBlackListURL 
-- | return the contents of your site's custom URL blacklist. mollom.listBlackListURL 

