{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom blacklisting API.
 -}

module Network.Mollom.Blacklist
  ( Reason(..)
  , Context(..)
  , Match(..)
  , BlacklistResponse(..)
  , createBlacklist
  , updateBlacklist
  , deleteBlacklist
  , listBlacklist
  , readBlacklistEntry
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.List(intercalate)
import Data.Maybe(catMaybes)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Helper
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

instance A.FromJSON Reason where
    parseJSON (A.String s) = return $ case s of
                               "spam"      -> Spam
                               "profanity" -> Profanity
                               "quality"   -> Quality
                               "unwanted"  -> Unwanted
    parseJSON _ = mzero

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

instance A.FromJSON Context where
    parseJSON (A.String s) = return $ case s of
                                "allFields" -> AllFields
                                "authorName" -> AuthorName
                                "authorMail" -> AuthorMail
                                "authorIp"   -> AuthorIp
                                "authorId"   -> AuthorId
                                "links"      -> Links
                                "postTitle"  -> PostTitle
    parseJSON _ = mzero


-- | Data type indicating how well a specific match should be.
data Match = Exact
           | Contains
           deriving Eq

instance Show Match where
  show Exact = "exact"
  show Contains = "contains"

instance A.FromJSON Match where
    parseJSON (A.String s) = return $ case s of
                                "exact" -> Exact
                                "contains" -> Contains
    parseJSON _ = mzero


-- | Data type representing the response in the blacklist API.
data BlacklistResponse = 
     BlacklistResponse { blacklistId         :: String
                       , blacklistCreated    :: String -- FIXME should be datetime
                       , blacklistStatus     :: Bool
                       , blacklistLastMatch  :: String -- FIXME should be datetime
                       , blacklistMatchCount :: Int
                       , blacklistValue      :: String
                       , blacklistReason     :: Reason
                       , blacklistContext    :: Context
                       , blacklistMatch      :: Match
                       , blacklistNote       :: String
                       }

instance A.FromJSON BlacklistResponse where
    parseJSON j = do
        o <- A.parseJSON j
        e <- o A..: "entry"
        BlacklistResponse <$>
          e A..: "id"         <*>
          e A..: "created"    <*>
          e A..: "status"     <*>
          e A..: "lastMatch"  <*>
          e A..: "matchCount" <*>
          e A..: "value"      <*>
          e A..: "reason"     <*>
          e A..: "context"    <*>
          e A..: "match"      <*>
          e A..: "note"


instance A.FromJSON [BlacklistResponse] where
    parseJSON j = do
      o <- A.parseJSON j
      ls <- o A..: "list"
      mapM A.parseJSON ls


-- | Create a blacklist entry for the given site.
createBlacklist :: String        -- ^ The value or string to blacklist
                -> Maybe Reason  -- ^ The reason for this blacklisting
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Match   -- ^ How precise should the match be
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom (MollomResponse BlacklistResponse)
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
        errors = generalErrors
    mollomService pubKey privKey POST path kvs [] errors


-- | Update an existing blacklist entry. All arguments that are provided as Nothing
--   default to keeping existing values.
updateBlacklist :: String        -- ^ ID of the blacklisted entry to update
                -> Maybe String  -- ^ The blacklisted string or value.
                -> Maybe Reason  -- ^ The reason for this blacklisting
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Match   -- ^ How precise should the match be
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom (MollomResponse ())
updateBlacklist entryId s reason context match status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey ++ "/" ++ entryId
        kvs = [ ("value", s)
              , ("reason", fmap show reason)
              , ("context", fmap show context)
              , ("match", fmap show match)
              , ("status", fmap boolToOneZeroString status)
              , ("note", note)
              ]
        errors = generalErrors
    mollomService pubKey privKey POST path kvs [] errors

-- | Delete a blacklisted entry.
deleteBlacklist :: String    -- ^ ID of the blacklisted entry to delete
                -> Mollom (MollomResponse ())
deleteBlacklist entryId = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey ++ "/" ++ entryId ++ "/delete"
        errors = generalErrors
    mollomService pubKey privKey POST path [] [] errors


-- | List the entries in the blacklist for a given set of credentials, 
--   identified by the site public key.
--   FIXME: the arguments determination is fugly.
listBlacklist :: Maybe Int  -- ^ The offset from which to start listing entries. Defaults to 0 when Nothing is given as the argument.
              -> Maybe Int  -- ^ The number of entries that should be returned. Defaults to all.
              -> Mollom (MollomResponse [BlacklistResponse])
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
        errors = generalErrors
    mollomService pubKey privKey GET path [] [] errors


-- | Read the information that is stored for a given blacklist entry.
readBlacklistEntry :: String  -- ^ ID of the blacklisted entry to read
                   -> Mollom (MollomResponse BlacklistResponse)
readBlacklistEntry entryId = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "blacklist/" ++ pubKey ++ "/" ++ entryId
        errors = generalErrors
    mollomService pubKey privKey GET path [] [] errors


