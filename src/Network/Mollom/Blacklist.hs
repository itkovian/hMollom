{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom blacklisting API.
 -}

module Network.Mollom.Blacklist
  ( Reason(..)
  , Context(..)
  , createBlacklist
  , updateBlacklist
  , deleteBlacklist
  , listBlacklist
  , readBlacklistEntry
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
--   the Mollom service for blacklisting a string or a value.
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
--   service is allowed to look for a blacklisted term (to be 
--   provided at the creation of said term).
data Context = AllFields
             | AuthorName
             | AuthorMail
             | AuthorIp
             | AuthorId
             | Links
             | PostTitle
              deriving (Eq)

instance Show Context where
  show AllFields = "allFields"
  show AuthorName = "authorName"
  show AuthorMail = "authorMail"
  show AuthorIp = "authorIp"
  show AuthorId = "authorId"
  show Links = "links"
  show PostTitle = "postTitle"

-- | Data type indicating how well a specific match should be.
data Match = Exact
           | Contains
           deriving Eq

instance Show Match where
  show Exact = "exact"
  show Contains = "contains"

-- | Create a blacklist entry for the given site.
createBlacklist :: String        -- ^ The value or string to blacklist
                -> Maybe Reason  -- ^ The reason for this blacklisting
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Match   -- ^ How precise should the match be
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom MollomResponse
createBlacklist s reason context match status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey
        kvs = [ ("value", Just s)
              , ("reason", fmap show reason)
              , ("context", fmap show context)
              , ("match", fmap show match)
              , ("status", fmap boolToOneZeroString status)
              , ("note", note)
              ]
    mollomService pubKey privKey POST path kvs []


-- | Update an existing blacklist entry. All arguments that are provided as Nothing
--   default to keeping existing values.
updateBlacklist :: String        -- ^ ID of the blacklisted entry to update
                -> Maybe String  -- ^ The blacklisted string or value.
                -> Maybe Reason  -- ^ The reason for this blacklisting
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Match   -- ^ How precise should the match be
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom MollomResponse
updateBlacklist id s reason context match status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey ++ "/" ++ id
        kvs = [ ("value", s)
              , ("reason", fmap show reason)
              , ("context", fmap show context)
              , ("match", fmap show match)
              , ("status", fmap boolToOneZeroString status)
              , ("note", note)
              ]
    mollomService pubKey privKey POST path kvs []

-- | Delete a blacklisted entry.
deleteBlacklist :: String    -- ^ ID of the blacklisted entry to delete
                -> Mollom MollomResponse
deleteBlacklist id = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey ++ "/" ++ id ++ "/delete"
    mollomService pubKey privKey POST path [] []


-- | List the entries in the blacklist for a given set of credentials, 
--   identified by the site public key.
--   FIXME: the arguments determination is fugly.
listBlacklist :: Maybe Int  -- ^ The offset from which to start listing entries. Defaults to 0 when Nothing is given as the argument.
              -> Maybe Int  -- ^ The number of entries that should be returned. Defaults to all.
              -> Mollom MollomResponse
listBlacklist offset count = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        arguments = case offset `mplus` count of
                      Nothing -> ""
                      _       -> "/q?" ++ (intercalate "&" $ catMaybes [ fmap (\o -> "offset=" ++ show o) offset
                                                                       , fmap (\c -> "count=" ++ show c) count
                                                                       ])
        path = "blacklist/" ++ pubKey ++ arguments
    mollomService pubKey privKey GET path [] []


-- | Read the information that is stored for a given blacklist entry.
readBlacklistEntry :: String  -- ^ ID of the blacklisted entry to read
                   -> Mollom MollomResponse
readBlacklistEntry id = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey ++ "/" ++ id
    mollomService pubKey privKey GET path [] []

 
