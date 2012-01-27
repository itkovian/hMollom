{- 
 - (C) 2012, Andy Georges
 -
 - This module provides the interface to the Mollom site API.
 -}

module Network.Mollom.Types
  ( mollomApiVersion
  , addMessage
  , generalErrors
  , MollomConfiguration(..)
  , MollomError(..)
  , MollomResponse(..)
  , BlacklistError(..)
  , CaptchaError(..)
  , ContentError(..)
  , FeedbackError(..)
  , SiteError(..)
  , WhitelistError(..)
  ) where

import           Control.Monad.Error
import qualified Data.Aeson as A
import           Network.HTTP.Base (ResponseCode)
import           Network.HTTP.Stream (ConnError(..))

mollomApiVersion :: String
mollomApiVersion = "1.0"


data MollomConfiguration = MollomConfiguration 
  { mcPublicKey :: String
  , mcPrivateKey :: String
  , mcAPIVersion :: String
  } deriving (Eq, Ord, Show)


generalErrors :: [(ResponseCode, MollomError)]
generalErrors = [ ((4,0,1), Unauthorised "")
                , ((4,0,3), Forbidden "")
                , ((4,0,4), NotFound "")
                ]

data MollomError = ConnectionError ConnError            -- ^HTTP connection error
                 | BlacklistError BlacklistError String 
                 | CaptchaError CaptchaError String
                 | ContentError ContentError String
                 | FeedbackError FeedbackError String
                 | SiteError SiteError String
                 | WhitelistError WhitelistError String
                 | Unauthorised String                  -- ^General unauthorised request error. 401.
                 | Forbidden String                     -- ^Unauthorised to make this request. 403.
                 | NotFound String                      -- ^Your general oops, you're making the wrong request. 404.
                 | JSONParseError
                 | Message String
                 deriving (Eq, Show)

instance Error MollomError where
    noMsg = Message ""
    strMsg s = Message s

-- | Errors returned by the blacklist API
data BlacklistError = UnknownBlacklistEntry  -- The specified entry does not exist. 404.
                    deriving (Eq, Show)

-- | Errors returned by the captcha API
data CaptchaError = CaptchaDoesNotExist      -- ^The resource was never created. 404.
                  | CaptchaAlreadyProcessed  -- ^The resource was created but was already processed and can thus no longer be solved. 409.
                  | CaptchaExpired           -- ^The resource was created but can no longer be solved since it expired. 410.
                  deriving (Eq, Show)

-- | Errors returned bu the content API
data ContentError = Whoops
                  deriving (Eq, Show)

-- | Errors returned by the feedback API
data FeedbackError = FeedbackMissingID     -- ^The request did not specify either a content or captcha ID. 400.
                   | FeedbackUnknownReason -- ^The reason is not one that is supported by Mollom. 400. FIXME: this should be a different HTTP code.
                   deriving (Eq, Show)

-- | Errors returned by the site API
data SiteError = SiteUnknown -- ^ We have no clue who you are. 404.
               deriving (Eq, Show)

-- | Errors returned by the whitelist API
data WhitelistError = UnknownWhitelistEntry  -- ^The specified entry does not exist. 404.
                    deriving (Eq, Show)



-- | Replace the String message in the error
addMessage :: MollomError -> String -> MollomError
addMessage (BlacklistError b _) s = BlacklistError b s
addMessage (CaptchaError c _) s = CaptchaError c s
addMessage (ContentError c _) s = ContentError c s
addMessage (FeedbackError f _) s = FeedbackError f s
addMessage (SiteError s' _) s = SiteError s' s 
addMessage (WhitelistError w _) s = WhitelistError w s
addMessage (Unauthorised _) s = Unauthorised s
addMessage (Forbidden _) s = Forbidden s
addMessage (NotFound _) s = NotFound s
addMessage (Message _) s = Message s





data (A.FromJSON a) => MollomResponse a = MollomResponse 
                    { code :: ResponseCode
                    , message :: String
                    , response :: a
                    } deriving (Eq, Show)




