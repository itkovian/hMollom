{-
 - (C) 2012, Andy Georges
 -
 - This modules provides the interface to the Mollom CAPTCHA API.
 -
 -}

module Network.Mollom.Captcha
  ( Type(..)
  , createCaptcha
  , verifyCaptcha
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.List (intercalate)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Helper
import Network.Mollom.MollomMonad
import Network.Mollom.Types


-- | Data type representing the CAPTCHA types Mollom can serve.
data Type = Image
          | Audio
          deriving (Eq)

instance Show Type where
  show Image = "image"
  show Audio = "audio"

createCaptcha :: A.FromJSON a
              => Type         -- ^ The type of captcha that should be created.
              -> Maybe Bool   -- ^ Use SSL if True (only for paid subscriptions).
              -> Maybe String -- ^ Optional content ID to refer to.
              -> Mollom (MollomResponse a)
createCaptcha t ssl captchId = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "captcha"
        kvs = [ ("type", Just $ show t)
              , ("ssl", fmap boolToOneZeroString ssl)
              , ("contentId", captchId)
              ]
        errors = generalErrors
    mollomService pubKey privKey POST path kvs [] errors



verifyCaptcha :: A.FromJSON a
              => String         -- ^The captcha ID to be verified.
              -> Maybe String   -- ^The name of the content author filling out the captcha.
              -> Maybe String   -- ^The website URL of the content author .
              -> Maybe String   -- ^The email address of the content author.
              -> Maybe [String] -- ^The openIDs (if any) of the content author.
              -> Maybe String   -- ^The IP-address of the content author.
              -> Maybe String   -- ^Content author's unique local site user ID.
              -> Maybe Int      -- ^The time that must have passed before the same author can post again. Defaults to 15.
              -> Maybe String   -- ^Client-side honeypot value, if any.
              -> Mollom (MollomResponse a)
verifyCaptcha captchId authorName authorURL authorEmail authorOpenIds authorIP authorSiteID rateLimit honeypot = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "captcha/" ++ captchId
        kvs = [ ("authorName", authorName)
              , ("authorUrl", authorURL)
              , ("authorMail", authorEmail)
              , ("authorOpenid", fmap (intercalate " ") authorOpenIds)
              , ("authorIp", authorIP)
              , ("authorId", authorSiteID)
              , ("rateLimit", fmap show rateLimit)
              , ("honeypot", honeypot)
              ]
        errors = generalErrors
    mollomService pubKey privKey POST path kvs [] errors

