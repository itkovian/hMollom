{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Module that implements the Mollom monad stack
-- We wrap the configuration and the serverlist
-- in a Reader and a State, respectively
module Network.Mollom.MollomMonad 
  ( MollomConfiguration(..)
  , MollomMonad
--  , createMollomMonad
--  , runMollomMonad
  , MollomState
--  , runMollomState
  ) where


import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid

import Network.Mollom.Internals

type SessionID = String
type Server = String

data MollomConfiguration = MollomConfiguration 
  { mcPublicKey :: String
  , mcPrivateKey :: String
  , mcAPIVersion :: String
  } deriving (Eq, Ord, Show)


type MollomState = ReaderT MollomConfiguration (StateT MollomServerList IO)

{-
newtype Eq a => MollomState a = MollomState {
    runMollomState :: ReaderT MollomConfiguration (StateT MollomServerList IO) a
  } deriving (Monad, MonadIO, MonadReader MollomConfiguration, MonadState MollomServerList)
  -}

{-
newtype Eq a => MollomMonad a = MollomMonad { 
    runMollomMonad :: ErrorT String (StateT SessionID MollomState) a
  } deriving (Monad, MonadIO, MonadState SessionID)

-}

type MollomMonad = ErrorT String (StateT (Maybe SessionID) MollomState)

  
-- createMollomMonad :: MollomConfiguration -> MollomMonad ()
-- createMollomMonad configuration = MollomMonad $ 


