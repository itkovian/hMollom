{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom site API.
 -}

module Network.Mollom.Types
  ( mollomApiVersion
  , MollomConfiguration(..)
  , MollomError(..)
  , MollomResponse(..)
  ) where

import           Control.Monad.Error
import           Network.HTTP.Base (Response(..), ResponseCode)
import           Network.HTTP.Stream (ConnError(..), Result)
import           Network.XmlRpc.Client
import           Network.XmlRpc.Internals 

mollomApiVersion :: String
mollomApiVersion = "1.0"


data MollomConfiguration = MollomConfiguration 
  { mcPublicKey :: String
  , mcPrivateKey :: String
  , mcAPIVersion :: String
  } deriving (Eq, Ord, Show)


data MollomError = Unauthorized
                 | Forbidden
                 | ResourceNotFound
                 | UnknownSite
                 | CMollomError ConnError
                 | HMollomError String deriving (Eq,Show)

instance Error MollomError where 
  noMsg = HMollomError "Unknown Error"
  strMsg str = HMollomError str


data MollomResponse = MollomResponse 
                    { code :: ResponseCode
                    , message :: String
                    , response :: String -- [(String, MollomValue)]
                    } deriving (Eq, Show)

data MollomValue = MInt Int
                 | MBool Bool
                 | MDouble Double
                 | MString String deriving (Eq, Ord, Show)

instance XmlRpcType MollomValue where
  toValue (MInt i) = toValue i
  toValue (MBool b) = toValue b
  toValue (MDouble d) = toValue d
  toValue (MString s) = toValue s

  fromValue (ValueInt i) = maybeToM "" (Just (MInt i))
  fromValue (ValueBool b) = maybeToM "" (Just (MBool b)) 
  fromValue (ValueDouble d) = maybeToM "" (Just (MDouble d))
  fromValue (ValueString s) = maybeToM "" (Just (MString s))

  getType (MInt _) = TInt
  getType (MBool _) = TBool
  getType (MDouble _) = TDouble
  getType (MString _) = TString



