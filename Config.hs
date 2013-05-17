module Config where

import Hakyll (FeedConfiguration(..))

--------------------------------------------------------------------------------
-- Important all item of lang must have a length of 2.
langs :: [String]
langs=["en","fr"]

--------------------------------------------------------------------------------
fstlang :: String
fstlang=head langs

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "yannesposito.com"
  , feedDescription = "Personal blog of Yann Esposito"
  , feedAuthorName = "Yann Esposito"
  , feedAuthorEmail = "yann.esposito@gmail.com"
  , feedRoot = "http://yannesposito.com"
  }
