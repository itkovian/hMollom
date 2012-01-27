{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom whitelisting API.
 -}

module Network.Mollom.Whitelist
  ( Reason(..)
  , Context(..)
  , WhitelistResponse(..)
  , createWhitelist
  , updateWhitelist
  , deleteWhitelist
  , listWhitelist
  , readWhitelistEntry
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

instance A.FromJSON Reason where
    parseJSON (A.String s) = return $ case s of
                               "spam"      -> Spam
                               "profanity" -> Profanity
                               "quality"   -> Quality
                               "unwanted"  -> Unwanted
    parseJSON _ = mzero


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


instance A.FromJSON Context where
    parseJSON (A.String s) = return $ case s of
                                "authorName" -> AuthorName
                                "authorMail" -> AuthorMail
                                "authorIp"   -> AuthorIp
                                "authorId"   -> AuthorId
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
data WhitelistResponse = 
     WhitelistResponse { whitelistId         :: String
                       , whitelistCreated    :: String -- FIXME should be datetime
                       , whitelistStatus     :: Bool
                       , whitelistLastMatch  :: String -- FIXME should be datetime
                       , whitelistMatchCount :: Int
                       , whitelistValue      :: String
                       , whitelistContext    :: Context
                       , whitelistNote       :: String
                       }

instance A.FromJSON WhitelistResponse where
    parseJSON j = do
        o <- A.parseJSON j
        e <- o A..: "entry"
        WhitelistResponse <$>
          e A..: "id"         <*>
          e A..: "created"    <*>
          e A..: "status"     <*>
          e A..: "lastMatch"  <*>
          e A..: "matchCount" <*>
          e A..: "value"      <*>
          e A..: "context"    <*>
          e A..: "note"


instance A.FromJSON [WhitelistResponse] where
    parseJSON j = do
      o <- A.parseJSON j
      ls <- o A..: "list"
      mapM A.parseJSON ls




-- | Create a whitelist entry for the given site.
createWhitelist :: String        -- ^ The value or string to whitelist
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom (MollomResponse WhitelistResponse)
createWhitelist s context status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey
        kvs = [ ("value", Just s)
              , ("context", fmap show context)
              , ("status", fmap boolToOneZeroString status)
              , ("note", note)
              ]
        errors = generalErrors
    mollomService pubKey privKey POST path kvs [] errors


-- | Update an existing whitelist entry. All arguments that are provided as Nothing
--   default to keeping existing values.
updateWhitelist :: String        -- ^ ID of the whitelisted entry to update
                -> Maybe String  -- ^ The whitelisted string or value.
                -> Maybe Context -- ^ Where may the entry match
                -> Maybe Bool    -- ^ Is the entry live or not
                -> Maybe String  -- ^ Note
                -> Mollom (MollomResponse ())
updateWhitelist entryId s context status note = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey ++ "/" ++ entryId
        kvs = [ ("value", s)
              , ("context", fmap show context)
              , ("status", fmap boolToOneZeroString status)
              , ("note", note)
              ]
        errors = generalErrors
    mollomService pubKey privKey POST path kvs [] errors

-- | Delete a whitelisted entry.
deleteWhitelist :: String    -- ^ ID of the whitelisted entry to delete
                -> Mollom (MollomResponse ())
deleteWhitelist entryId = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey ++ "/" ++ entryId ++ "/delete"
        errors = generalErrors
    mollomService pubKey privKey POST path [] [] errors


-- | List the entries in the whitelist for a given set of credentials, 
--   identified by the site public key.
--   FIXME: the arguments determination is fugly.
listWhitelist :: Maybe Int  -- ^ The offset from which to start listing entries. Defaults to 0 when Nothing is given as the argument.
              -> Maybe Int  -- ^ The number of entries that should be returned. Defaults to all.
              -> Mollom (MollomResponse [WhitelistResponse])
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
        errors = generalErrors
    mollomService pubKey privKey GET path [] [] errors


-- | Read the information that is stored for a given whitelist entry.
readWhitelistEntry :: String  -- ^ ID of the whitelisted entry to read
              -> Mollom (MollomResponse WhitelistResponse)
readWhitelistEntry entryId = do
    config <- ask
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        path = "whitelist/" ++ pubKey ++ "/" ++ entryId
        errors = generalErrors
    mollomService pubKey privKey GET path [] [] errors

 
