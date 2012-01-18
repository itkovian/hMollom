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

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.List(intercalate)
import Data.Maybe(catMaybes)
--import           Control.Monad.State
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.MollomMonad
import Network.Mollom.Types

{-

readSite:
<response>
  <code>200</code>
  <message>Error message</message>
  <site>
    <id>siteId</id>
    <publicKey>publicKey</publicKey>
    <privateKey>privateKey</privateKey>
    <url>example.com</url>
    <email>mail@example.com</email>
    <languages>
      <language>en</language>
    </languages>
    <subscriptionType></subscriptionType>
    <platformName>Drupal</platformName>
    <platformVersion>7.2</platformVersion>
    <clientName>Mollom</clientName>
    <clientVersion>7.x-1.0</clientVersion>
  </site>
</response>

-}

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

instance A.FromJSON SiteResponse where
    parseJSON = undefined


-- | Request the information Mollom has about a specific site
readSite :: A.FromJSON a
                => Mollom (MollomResponse a) -- ^ The Mollom monad in which the request is made
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
listSites :: A.FromJSON a
                => Maybe Int -- ^ The offset from which to start listing sites. Defaults to 0 when Nothing is given as the argument.
          -> Maybe Int -- ^ The number of sites that should be returned. Defaults to all.
          -> Mollom (MollomResponse a)
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


