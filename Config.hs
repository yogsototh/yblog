module Config where

import Hakyll (FeedConfiguration(..))

--------------------------------------------------------------------------------
-- Important all item of lang must have a length of 2.
langs :: [String]
langs=["en","fr","de"]

--------------------------------------------------------------------------------
fstlang :: String
fstlang=head langs

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "yourdomain.com"
  , feedDescription = "Personal blog of John Doe"
  , feedAuthorName = "John Doe"
  , feedAuthorEmail = "your.mail@email.com"
  , feedRoot = "http://yourdomain.com"
  }
