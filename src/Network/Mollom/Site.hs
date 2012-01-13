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

import           Control.Monad.Error
import           Control.Monad.Reader
--import           Control.Monad.State
import           Network.HTTP.Base (HTTPResponse, RequestMethod(..), Response(..), ResponseCode, urlEncode)

import Network.Mollom.Internals
import Network.Mollom.MollomMonad
import Network.Mollom.Types

-- | Request the information Mollom has about a specific site
readSite :: Mollom MollomResponse -- ^ The Mollom monad in which the request is made
readSite = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "content/ " ++ ( mcPublicKey config)
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey GET path [] []


-- | Request that a specific site is deleted from the Mollom service
deleteSite :: Mollom MollomResponse
deleteSite = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "content/ " ++ ( mcPublicKey config) ++ "/delete"
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path [] []


-- | List all sites that can be accessed with the given authentication
--   FIXME: need to incorporate the offset and count parameters
listSites :: Maybe Int -- ^ The offset from which to start listing sites. Defaults to 0 when Nothing is given as the argument.
          -> Maybe Int -- ^ The number of sites that should be returned. Defaults to all.
          -> Mollom MollomResponse
listSites offset count = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "site"
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey GET path [] []


