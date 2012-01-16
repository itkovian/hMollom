{-
 - (C) 2012, Andy Georges
 -
 - This modules exports some helper functions.
 -
 -}

module Network.Mollom.Helper
  ( boolToOneZeroString
  ) where


boolToOneZeroString :: Bool -> String
boolToOneZeroString True = "1"
boolToOneZeroString False = "0"
