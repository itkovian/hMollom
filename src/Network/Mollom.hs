{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Interface to the Mollom web service (http://mollom.com),
--   based on the beta REST API, see http://mollom.com/api/rest.
module Network.Mollom
  ( 
    -- site API
    readSite
    , deleteSite
    -- content API
    , checkContent
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
  -- exported for testing purposes
  , service
  ) where

import           Control.Arrow (second)
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Data.Aeson as A
import           Data.Maybe(fromJust, isJust)
--import           Network.XmlRpc.Client
--import           Network.XmlRpc.Internals 
--import Network.XmlRpc.THDeriveXmlRpcType (asXmlRpcStruct)

import Network.Mollom.Blacklist
import Network.Mollom.Content
import Network.Mollom.Captcha
import Network.Mollom.Feedback
import Network.Mollom.Site
import Network.Mollom.Internals
import Network.Mollom.OAuth
import Network.Mollom.MollomMonad



-- testPubKey = "buid5peb664h1fcezn1da61mp143wff6"
-- testPrivKey = "14m7q8al6ph6k1smu6qmb3mk89o435f9"





