-- |Internal data structures and functions to the Mollom service
module Network.Mollom.Internals 
  ( mollomApiVersion
  , mollomTimeFormat
  , MollomError(..)
  , MollomValue(..)
  , MollomServerList(..)
  ) where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals 

mollomApiVersion :: String
mollomApiVersion = "1.0"


-- FIXME: This should be specified in some configuration file
-- or use the system locale
mollomTimeFormat = "%Y-%m-%dT%H:%M:%S.000+0200"

-- |Describes connection with the Mollom server
{-data MollomConn = MollomConn
  { mcPublicKey :: String
  , mcPrivateKey :: String
  , mcSessionID :: String
  , mcServerList :: [String]
  , mcAPIVersion :: String
  } deriving (Eq, Show)
-}

data MollomServerList = UninitialisedServerList | MollomServerList [String]

data MollomError = MollomInternalError
                 | MollomRefresh
                 | MollomServerBusy deriving (Eq, Ord, Show)

data MollomValue = MInt Int
                 | MBool Bool
                 | MDouble Double
                 | MString String
                 | MFault MollomError deriving (Eq, Ord, Show)

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


