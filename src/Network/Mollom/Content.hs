{-# LANGUAGE OverloadedStrings #-}
{-
 - (C) 2012, Andy Georges
 -
 - This modules provides the interface to the Mollom content API.
 -
 -}

module Network.Mollom.Content
  ( Check(..)
  , ContentLanguage(..)
  , ContentResponse(..)
  , SpamClassification(..)
  , Strictness(..)
  , checkContent
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as A
import Data.List (intercalate)
import Network.HTTP.Base (RequestMethod(..))

import Network.Mollom.Helper
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


-- | Data type representing the language that was detected by 
--   the Mollom service.
data ContentLanguage =
     ContentLanguage { languageCode  :: String
                     , languageScore :: Double
                     }

instance A.FromJSON ContentLanguage where
    parseJSON (A.Object o) = ContentLanguage <$>
                              o A..: "languageCode" <*>
                              o A..: "languageScore"

-- | Data type representing the classification of the content
--   by the Mollom service.
data SpamClassification = SpamClass | HamClass | UnsureClass deriving (Eq, Show)

instance A.FromJSON SpamClassification where
    parseJSON (A.String s) = return $ case s of
                              "spam"   -> SpamClass
                              "ham"    -> HamClass
                              "unsure" -> UnsureClass
    parseJSON _ = mzero

-- | Data type representing a response in the content API.
data ContentResponse =
     ContentResponse { contentId                 :: String
                     , contentSpamScore          :: Maybe Double
                     , contentSpamClassification :: Maybe SpamClassification
                     , contentProfanityScore     :: Maybe Double
                     , contentQualityScore       :: Maybe Double
                     , contentSentimentScore     :: Maybe Double
                     , contentReason             :: Maybe String
                     , contentLanguages          :: [ContentLanguage]
                     , contentPostTitle          :: String
                     , contentPostBody           :: String
                     , contentAuthorName         :: String
                     , contentAuthorUrl          :: String
                     , contentAuthorMail         :: String
                     , contentAuthorIP           :: String
                     , contentAuthorId           :: String
                     , contentAuthorOpenId       :: [String]
                     }


instance A.FromJSON ContentResponse where
    parseJSON j = do
        o <- A.parseJSON j
        s <- o A..: "content"
        ContentResponse <$>
          s A..: "contentId" <*>
          s A..:? "spamScore" <*>
          s A..:? "spamClassification" <*>
          s A..:? "profanityScore" <*>
          s A..:? "qualityScore" <*>
          s A..:? "sentimentScore" <*>
          s A..:? "reason" <*>
          s A..: "languages" <*>
          s A..: "postTitle" <*>
          s A..: "postBody" <*>
          s A..: "authorName" <*>
          s A..: "authorUrl" <*>
          s A..: "authorMail" <*>
          s A..: "authorIp" <*>
          s A..: "authorId" <*>
          s A..: "authorOpenId"
    parseJSON _ = mzero

-- | Asks Mollom whether the specified message is legitimate.
--   FIXME: contentID should be taken from the Mollom state
checkContent :: Maybe String     -- ^Title of submitted post.
             -> Maybe String     -- ^Body of submitted post.
             -> Maybe String     -- ^Content author's name.
             -> Maybe String     -- ^Content author's URL or website.
             -> Maybe String     -- ^Content author's email address.
             -> Maybe String     -- ^Content author's openID.
             -> Maybe String     -- ^Content author's current IP.
             -> Maybe String     -- ^Content author's unique local site user ID.
             -> Maybe [Check]    -- ^The check(s) to perform. If Nothing or Just None,
                                 --  this will default to Spam when the existing content ID is Nothing.
             -> Maybe Bool       -- ^Do we want to allow unsure results, leading to a CAPTCHA or not.
             -> Maybe Strictness -- ^How strict should Mollom be when checking this content?
             -> Maybe Int        -- ^Rate limit imposing a bound on the time between submitted posts from the same author.
             -> Maybe String     -- ^Value of the honeypot form-element, if any.
             -> Maybe Bool       -- ^Was the content stored on the client-side? 
                                 --  Should be False during validation of a form, 
                                 --  True after a succesfull submission.
             -> Maybe String     -- ^Absolute URL for the stored content.
             -> Maybe String     -- ^Absolute URL to the content's parent context, e.g., 
                                 --  the article of forum thread a comment is placed on.
             -> Maybe String     -- ^Title of said parental context.
             -> Mollom (MollomResponse ContentResponse) -- ^The monad in which the function is executing.
checkContent title body authorName authorURL authorEmail authorOpenID authorIP authorSiteID checks unsure strictness rateLimit honeypot stored storedURL storedParentURL parentTitle = do
    config <- ask 
    contentID <- get
    let pubKey = mcPublicKey config
        privKey = mcPrivateKey config
        kvs = [ ("postTitle", title)
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
                  Just cid -> "content/" ++ cid
                  Nothing -> "content"
        errors = generalErrors
    ms <- mollomService pubKey privKey POST path kvs [] errors
    let contentID' = contentId . response $ ms
    put $ Just contentID'
    return ms





