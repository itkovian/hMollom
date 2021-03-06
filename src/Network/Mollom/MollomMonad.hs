{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Module that implements the Mollom monad stack
-- We wrap the configuration in a Reader
module Network.Mollom.MollomMonad 
  ( Mollom
  , MollomState
  , mollomService
  , mollomNoAuthService
  , runMollom
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error
import qualified Data.Aeson as A
import           Network.HTTP.Base (RequestMethod(..))

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
                     } deriving (Monad, MonadIO, MonadReader MollomConfiguration, MonadState (Maybe ContentID), MonadError MollomError)

wrapMollom :: ErrorT MollomError IO a -> Mollom a
wrapMollom = M . ErrorT . liftIO . runErrorT

pJSON :: A.FromJSON b => MollomResponse A.Value -> Mollom (MollomResponse b)
pJSON mr =
    case A.fromJSON (response mr) of
      A.Success r' -> return $ mr { response = r' }
      _            -> throwError JSONParseError


mollomService :: A.FromJSON a
              => String                           -- ^ Public key
              -> String                           -- ^ Private key
              -> RequestMethod                    -- ^ The HTTP method used in this request.
              -> String                           -- ^ The path to the requested resource
              -> [(String, Maybe String)]         -- ^ Request parameters
              -> [String]                         -- ^ Expected returned values
              -> [((Int, Int, Int), MollomError)] -- ^Possible error values
              -> Mollom (MollomResponse a)
mollomService pubKey privKey method path params expected errors =
    (wrapMollom $ service pubKey privKey method path params expected errors) >>= pJSON

mollomNoAuthService method path params expected errors = 
    (wrapMollom $ serviceNoAuth method path params expected errors) >>= pJSON

runMollom :: Mollom a -> MollomConfiguration -> MollomState -> IO (Either MollomError (Maybe ContentID, a))
runMollom m config s = do
    v <- runReaderT (runStateT (runErrorT $ runM m) s) config
    return $ case v of 
                 (Left err, _) -> Left err
                 (Right r, cid) -> Right (cid, r)


