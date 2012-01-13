{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Module that implements the Mollom monad stack
-- We wrap the configuration in a Reader
module Network.Mollom.MollomMonad 
  ( Mollom(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Network.Mollom.Types

type ContentID = String

-- | The MollomMonad type is a monad stack that can retain the content ID in its
--   state (Content, Captcha and Feedback APIs). We also need to have a configuration
--   that's towed along with the public and private keys.
newtype Mollom a = Mollom { runMollom :: ErrorT MollomError 
                                                (StateT (Maybe ContentID) 
                                                        (ReaderT MollomConfiguration IO)) a 
                          } deriving (Monad, MonadIO, MonadReader MollomConfiguration, MonadState (Maybe ContentID))

