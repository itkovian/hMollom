{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-
 - (C) 2012, Andy Georges
 -
 - This modules provides the interface to the Mollom CAPTCHA API.
 -
 -}

module Network.Mollom.Captcha
  ( Type(..)
  , CaptchaResponse(..)
  , createCaptcha
  , verifyCaptcha
  ) where

import Control.Applicative
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

instance A.FromJSON Type where
    parseJSON (A.String s) = return $ case s of
                                "image" -> Image
                                "audio" -> Audio
    parseJSON _ = mzero


-- | Data type representing the response to a captcha API call
data CaptchaResponse = 
     CaptchaResponse { captchaId           :: String
                     , captchaUrl          :: String
                     , captchaSolved       :: Bool
                     , captchaReason       :: String
                     , captchaAuthorName   :: String
                     , captchaAuthorUrl    :: String
                     , captchaAuthorMail   :: String
                     , captchaAuthorIP     :: String
                     , captchaAuthorId     :: String
                     , captchaAuthorOpenId :: String
                     }


instance A.FromJSON CaptchaResponse where
    parseJSON j = do
        o <- A.parseJSON j
        s <- o A..: "captcha"
        CaptchaResponse <$>
          s A..: "captchaId" <*>
          s A..: "url" <*>
          s A..: "solved" <*>
          s A..: "reason" <*>
          s A..: "authorName" <*>
          s A..: "authorUrl" <*>
          s A..: "authorMail" <*>
          s A..: "authorIp" <*>
          s A..: "authorId" <*>
          s A..: "authorOpenId"


createCaptcha :: Type         -- ^ The type of captcha that should be created.
              -> Maybe Bool   -- ^ Use SSL if True (only for paid subscriptions).
              -> Maybe String -- ^ Optional content ID to refer to.
              -> Mollom (MollomResponse CaptchaResponse)
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



verifyCaptcha :: String         -- ^The captcha ID to be verified.
              -> Maybe String   -- ^The name of the content author filling out the captcha.
              -> Maybe String   -- ^The website URL of the content author .
              -> Maybe String   -- ^The email address of the content author.
              -> Maybe [String] -- ^The openIDs (if any) of the content author.
              -> Maybe String   -- ^The IP-address of the content author.
              -> Maybe String   -- ^Content author's unique local site user ID.
              -> Maybe Int      -- ^The time that must have passed before the same author can post again. Defaults to 15.
              -> Maybe String   -- ^Client-side honeypot value, if any.
              -> Mollom (MollomResponse CaptchaResponse)
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

