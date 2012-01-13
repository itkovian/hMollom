{-
 - (C) 2012, Andy Georges
 -
 - This modules provides the interface to the Mollom content API.
 -
 -}

module Network.Mollom.Content
  ( Check(..)
  , Strictness(..)
  , checkContent
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Data.List (intercalate)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Internals
import Network.Mollom.MollomMonad
import Network.Mollom.Types

-- | Possible checks Mollom can perform on the provided content.
data Check = Spam | Quality | Profanity | Language | Sentiment | None deriving (Eq)

instance Show Check where
  show Spam = "spam"
  show Quality = "quality"
  show Profanity = "profanity"
  show Language = "language"
  show Sentiment = "sentiment"
  show None = "none"

-- | Strictness of the Mollom service analysis
data Strictness = Strict | Normal | Relaxed deriving (Eq)

instance Show Strictness where
  show Strict = "strict"
  show Normal = "normal"
  show Relaxed = "relaxed"



-- | Asks Mollom whether the specified message is legitimate.
checkContent :: Maybe String  -- ^Existing content ID.
             -> Maybe String  -- ^Title of submitted post.
             -> Maybe String  -- ^Body of submitted post.
             -> Maybe String  -- ^Content author's name.
             -> Maybe String  -- ^Content author's URL or website.
             -> Maybe String  -- ^Content author's email address.
             -> Maybe String  -- ^Content author's openID.
             -> Maybe String  -- ^Content author's current IP.
             -> Maybe String  -- ^Content author's unique local site user ID.
             -> Maybe [Check] -- ^The check(s) to perform. If Nothing or Just None,
                              --  this will default to Spam when the existing content ID is Nothing.
             -> Maybe Bool    -- ^ Do we want to allow unsure results, leading to a CAPTCHA or not.
             -> Maybe Strictness -- ^ How strict should Mollom be when checking this content?
             -> Maybe Int     -- ^ Rate limit imposing a bound on the time between submitted posts from the same author.
             -> Maybe String  -- ^ Value of the honeypot form-element, if any.
             -> Maybe Bool    -- ^ Was the content stored on the client-side? Should be 0 during validation of a form, 1 after a succesfull submission.
             -> Maybe String  -- ^ Absolute URL for the stored content.
             -> Maybe String  -- ^ Absolute URL to the content's parent context, e.g., the article of forum thread a comment is placed on.
             -> Maybe String  -- ^ Title of said parental context.
             -> Mollom MollomResponse -- ^The monad in which the function is executing.
checkContent contentID title body authorName authorURL authorEmail authorOpenID authorIP authorSiteID checks unsure strictness rateLimit honeypot stored storedURL storedParentURL parentTitle =
    let kvs = catSecondMaybes [ ("postTitle", title)
                              , ("postBody", body)
                              , ("authorName", authorName)
                              , ("authorUrl", authorURL)
                              , ("authorMail", authorEmail)
                              , ("authorOpenid", authorOpenID)
                              , ("authorIp", authorIP)
                              , ("authorId", authorSiteID)
                              , ("checks", fmap (intercalate "|" . map show) checks)
                              , ("unsure", fmap boolToOneZeroString unsure)
                              , ("strictness", fmap show strictness)
                              , ("rateLimit", fmap show rateLimit)
                              , ("honeypot", honeypot)
                              , ("stored", fmap boolToOneZeroString stored)
                              , ("url", storedURL)
                              , ("contextUrl", storedParentURL)
                              , ("contextTitle", parentTitle)
                              ]
        path = case contentID of
                  Just id -> "content/" ++ id
                  Nothing -> "content"
    in checkContent' path kvs


checkContent' :: String             -- ^ Path to the requested resource.
              -> [(String, String)] -- ^ Data parameters to pass to the service call
              -> Mollom MollomResponse
checkContent' path params = do
    config <- ask 
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
    Mollom $ ErrorT . liftIO . runErrorT $ service pubKey privKey POST path params []




