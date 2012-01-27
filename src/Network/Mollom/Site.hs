{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom site API.
 -}

module Network.Mollom.Site
  ( readSite
  , deleteSite
  , listSites
  ) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.List(intercalate)
import Data.Maybe(catMaybes)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.MollomMonad
import Network.Mollom.Types

type SiteLanguage = String

data SiteResponse = 
     SiteResponse { siteId :: String
                  , sitePublicKey :: String
                  , sitePrivateKey :: String
                  , siteURL :: String
                  , siteEmail :: String
                  , siteLanguages :: [SiteLanguage]
                  , siteSubscription :: String -- FIXME: I have no idea what this should be
                  , sitePlatformName :: String
                  , sitePlatformVersion :: String
                  , siteClientName :: String
                  , siteClientVersion :: String
                  }
                  deriving (Eq, Show)

instance A.FromJSON SiteResponse where
  parseJSON j = do
      o <- A.parseJSON j
      s <- o A..: "site"
      SiteResponse <$>
        s A..: "siteId"     <*>
        s A..: "publicKey"  <*>
        s A..: "privateKey" <*>
        s A..: "url"        <*>
        s A..: "email"      <*>
        s A..: "languages"  <*>
        s A..: "subscription" <*>
        s A..: "platforName" <*>
        s A..: "platformVersion" <*>
        s A..: "clientName" <*>
        s A..: "clientVersion" 

instance A.FromJSON [SiteResponse] where
  parseJSON j = do
      o <- A.parseJSON j
      ls <- o A..: "list"
      mapM A.parseJSON ls 


-- | Request the information Mollom has about a specific site
readSite :: Mollom (MollomResponse SiteResponse) -- ^ The Mollom monad in which the request is made
readSite = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "content/" ++ ( mcPublicKey config)
        errors = generalErrors
    mollomService pubKey privKey GET path [] [] errors


-- | Request that a specific site is deleted from the Mollom service
deleteSite :: A.FromJSON a
                => Mollom (MollomResponse a)
deleteSite = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "content/" ++ ( mcPublicKey config) ++ "/delete"
        errors = [((4,0,4), SiteError SiteUnknown "")]
    mollomService pubKey privKey POST path [] [] errors


-- | List all sites that can be accessed with the given authentication
--   FIXME: need to incorporate the offset and count parameters
listSites :: 
          Maybe Int -- ^ The offset from which to start listing sites. Defaults to 0 when Nothing is given as the argument.
          -> Maybe Int -- ^ The number of sites that should be returned. Defaults to all.
          -> Mollom (MollomResponse [SiteResponse])
listSites offset count = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        arguments = case offset `mplus` count of
                      Nothing -> ""
                      _       -> "/q?" ++ (intercalate "&" $ catMaybes [ fmap (\o -> "offset=" ++ show o) offset
                                                                       , fmap (\c -> "count=" ++ show c) count
                                                                       ])
        path = "site" ++ arguments
        errors = generalErrors
    mollomService pubKey privKey GET path [] [] errors

