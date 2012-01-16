{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Module that implements the Mollom monad stack
-- We wrap the configuration in a Reader
module Network.Mollom.MollomMonad 
  ( Mollom
  , MollomState
  , mollomService
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Internals
import Network.Mollom.Types

type ContentID = String
type MollomState = Maybe ContentID

-- | The MollomMonad type is a monad stack that can retain the content ID in its
--   state (Content, Captcha and Feedback APIs). We also need to have a configuration
--   that's towed along with the public and private keys.
newtype Mollom a = M { runM :: ErrorT MollomError 
                                      (StateT MollomState
                                              (ReaderT MollomConfiguration IO)) a 
                     } deriving (Monad, MonadIO, MonadReader MollomConfiguration, MonadState (Maybe ContentID))

wrapMollom :: ErrorT MollomError IO a -> Mollom a
wrapMollom = M . ErrorT . liftIO . runErrorT

mollomService :: String             -- ^ Public key
              -> String             -- ^ Private key
              -> RequestMethod      -- ^ The HTTP method used in this request.
              -> String             -- ^ The path to the requested resource
              -> [(String, Maybe String)] -- ^ Request parameters
              -> [String]           -- ^ Expected returned values
              -> Mollom MollomResponse -- :: ErrorT (IO (Either MollomError (HTTPResponse String)))
mollomService pubKey privKey method path params expected = 
    wrapMollom $ service pubKey privKey method path params expected

runMollom :: Mollom a -> MollomConfiguration -> MollomState -> IO (Either MollomError (Maybe ContentID, a))
runMollom m config state = do
    v <- runReaderT (runStateT (runErrorT $ runM m) state) config
    return $ case v of 
                 (Left err, _) -> Left err
                 (Right r, cid) -> Right (cid, r)

--mkMollom :: MollomConfiguration -> Maybe Int -> Mollom a
--mkMollom 

