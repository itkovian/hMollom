{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Module that implements the Mollom monad stack
-- We wrap the configuration and the serverlist
-- in a Reader and a State, respectively
module Network.Mollom.MollomMonad 
  ( MollomMonad
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



type MollomState = ReaderT MollomConfiguration (StateT MollomServerList IO)
type MollomMonad = ErrorT MollomError (StateT (Maybe SessionID) MollomState)



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

-- createMollomMonad :: MollomConfiguration -> MollomMonad ()
-- createMollomMonad configuration = MollomMonad $ 


