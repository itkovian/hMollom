{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom whitelisting API.
 -}

module Network.Mollom.Whitelist
  ( Reason(..)
  , Context(..)
  , createWhitelist
  , updateWhitelist
  , deleteWhitelist
  --, listWhitelist
  --, readWhitelistEntry
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Data.List(intercalate)
import Data.Maybe(catMaybes)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Internals
import Network.Mollom.MollomMonad
import Network.Mollom.Types

-- | Data type representing the reasons the user can provide to
--   the Mollom service for whitelisting a string or a value.
data Reason = Spam 
            | Profanity 
            | Quality 
            | Unwanted 
            deriving (Eq)

instance Show Reason where
  show Spam = "spam"
  show Profanity = " profanity"
  show Quality = "quality"
  show Unwanted = "unwanted"


-- | Data type representing the context in which the Mollom
--   service is allowed to look for a whitelisted term (to be 
--   provided at the creation of said term).
data Context = AuthorName
             | AuthorMail
             | AuthorIp
             | AuthorId
              deriving (Eq)

instance Show Context where
  show AuthorName = "authorName"
  show AuthorMail = "authorMail"
  show AuthorIp = "authorIp"
  show AuthorId = "authorId"

-- | Data type indicating how well a specific match should be.
data Match = Exact
           | Contains
           deriving Eq

instance Show Match where
  show Exact = "exact"
  show Contains = "contains"



-- | Create a whitelist entry for the given site.
createWhitelist :: String        -- ^ The value or string to whitelist
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom MollomResponse
createWhitelist s context status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey
        kvs = catSecondMaybes [ ("value", Just s)
                              , ("context", fmap show context)
                              , ("status", fmap boolToOneZeroString status)
                              , ("note", note)
                              ]
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path kvs []


-- | Update an existing whitelist entry. All arguments that are provided as Nothing
--   default to keeping existing values.
updateWhitelist :: String        -- ^ ID of the whitelisted entry to update
                -> Maybe String  -- ^ The whitelisted string or value.
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom MollomResponse
updateWhitelist id s context status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey ++ "/" ++ id
        kvs = catSecondMaybes [ ("value", s)
                              , ("context", fmap show context)
                              , ("status", fmap boolToOneZeroString status)
                              , ("note", note)
                              ]
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path kvs []

-- | Delete a whitelisted entry.
deleteWhitelist :: String    -- ^ ID of the whitelisted entry to delete
                -> Mollom MollomResponse
deleteWhitelist id = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey ++ "/" ++ id ++ "/delete"
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path [] []


-- | List the entries in the whitelist for a given set of credentials, 
--   identified by the site public key.
--   FIXME: the arguments determination is fugly.
listWhitelist :: Maybe Int  -- ^ The offset from which to start listing entries. Defaults to 0 when Nothing is given as the argument.
              -> Maybe Int  -- ^ The number of entries that should be returned. Defaults to all.
              -> Mollom MollomResponse
listWhitelist offset count = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        arguments = case offset `mplus` count of
                      Nothing -> ""
                      _       -> "/q?" ++ (intercalate "&" $ catMaybes [ fmap (\o -> "offset=" ++ show o) offset
                                                                       , fmap (\c -> "count=" ++ show c) count
                                                                       ])
        path = "whitelist/" ++ pubKey ++ arguments
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey GET path [] []


-- | Read the information that is stored for a given whitelist entry.
readWhitelist :: String  -- ^ ID of the whitelisted entry to read
              -> Mollom MollomResponse
readWhitelist id = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey ++ "/" ++ id
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey GET path [] []

 