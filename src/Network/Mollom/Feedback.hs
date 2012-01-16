{-
 - (C) 2012, Andy Georges
 -
 - This modules provides the interface to the Mollom feedback API.
 -
 -}

module Network.Mollom.Feedback
  ( Reason(..)
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Internals
import Network.Mollom.MollomMonad
import Network.Mollom.Types


-- | Data type representing the reasons the user can provide to
--   the Mollom service for blacklisting a string or a value.
data Reason = Approve
            | Spam 
            | Profanity 
            | Quality 
            | Unwanted
            | Delete
            deriving (Eq)

instance Show Reason where
  show Approve = "approve"
  show Spam = "spam"
  show Profanity = "profanity"
  show Quality = "quality"
  show Unwanted = "unwanted"
  show Delete = "delete"


sendFeedback :: Maybe String  -- ^Existing content ID
             -> Maybe String  -- ^Existing captcha ID
             -> Reason        -- ^Reason of the feedback
             -> Mollom MollomResponse
sendFeedback contentId captchaId reason = do
    case contentId `mplus` captchaId of
      Nothing -> undefined -- this should be an error
      Just _  -> do config <- ask
                    let pubKey = mcPublicKey config
                        privKey = mcPrivateKey config
                        path = "feedback"
                        kvs = catSecondMaybes [ ("contentId", contentId)
                                              , ("captchaId", captchaId)
                                              ]
                    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path kvs []

