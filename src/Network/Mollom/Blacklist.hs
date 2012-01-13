{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom blacklisting API.
 -}

module Network.Mollom.Blacklist
  ( Reason(..)
  , Context(..)
  , createBlackList
  --, updateBlacklist
  --, deleteBlacklist
  --, getBlackList
  --, readBlackListEntry
  ) where

import Control.Monad.Error
import Control.Monad.Reader
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
createBlackList :: String        -- ^ The value or string to blacklist
                -> Maybe Reason  -- ^ The reason for this blacklisting
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Match   -- ^ How precise should the match be
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom MollomResponse
createBlackList s reason context match status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey
        kvs = catSecondMaybes [ ("value", Just s)
                              , ("reason", fmap show reason)
                              , ("context", fmap show context)
                              , ("match", fmap show match)
                              , ("status", fmap boolToOneZeroString status)
                              , ("note", note)
                              ]
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path kvs []

