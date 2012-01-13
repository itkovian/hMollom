{-
 - (C) 2012, Andy Georges
 -
 - This modules provides the interface to the Mollom CAPTCHA API.
 -
 -}

 module Network.Mollom.Captcha
  () where

import Control.Monad.Error
import Control.Monad.Reader
import Data.List (intercalate)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Internals
import Network.Mollom.MollomMonad
import Network.Mollom.Types


-- | Data type representing the CAPTCHA types Mollom can serve.
data Type = Image
          | Audio
          deriving (Eq)

instance Show Type where
  show Image = "image"
  show Audio = "audio"

createCaptcha :: Type         -- ^ The type of captcha that should be created.
              -> Maybe Bool   -- ^ Use SSL if True (only for paid subscriptions).
              -> Maybe String -- ^ Optional content ID to refer to.
              -> Mollom MollomResponse
createCaptcha t ssl id = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "captcha"
        kvs = catSecondMaybes [ ("type", Just $ show t)
                              , ("ssl", fmap boolToOneZeroString ssl)
                              , ("contentId", id)
                              ]
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path kvs []



verifyCaptcha :: String         -- ^The captcha ID to be verified.
              -> Maybe String   -- ^The name of the content author filling out the captcha.
              -> Maybe String   -- ^The website URL of the content author .
              -> Maybe String   -- ^The email address of the content author.
              -> Maybe [String] -- ^The openIDs (if any) of the content author.
              -> Maybe String   -- ^The IP-address of the content author.
              -> Maybe String   -- ^Content author's unique local site user ID.
              -> Maybe Int      -- ^The time that must have passed before the same author can post again. Defaults to 15.
              -> Maybe String   -- ^Client-side honeypot value, if any.
              -> Mollom MollomResponse
verifyCaptcha id authorName authorURL authorEmail authorOpenIds authorIP authorSiteID rateLimit honeypot = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "captcha/" ++ id
        kvs = catSecondMaybes [ ("authorName", authorName)
                              , ("authorUrl", authorURL)
                              , ("authorMail", authorEmail)
                              , ("authorOpenid", fmap (intercalate " ") authorOpenIds)
                              , ("authorIp", authorIP)
                              , ("authorId", authorSiteID)
                              , ("rateLimit", fmap show rateLimit)
                              , ("honeypot", honeypot)
                              ]
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path kvs []

