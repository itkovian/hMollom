{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Interface to the Mollom web service (http://mollom.com),
--   based on the beta REST API, see http://mollom.com/api/rest.
module Network.Mollom
  ( Mollom
  , runMollom
  , MollomConfiguration
  -- site API
  , readSite
  , deleteSite
  , listSites
  -- captcha API
  , createCaptcha
  , verifyCaptcha
  -- content API
  , checkContent
  ) where

import Network.Mollom.Blacklist
import Network.Mollom.Content
import Network.Mollom.Captcha
import Network.Mollom.Feedback
import Network.Mollom.Site
import Network.Mollom.MollomMonad
import Network.Mollom.Whitelist
import Network.Mollom.Types






