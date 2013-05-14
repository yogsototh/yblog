module Config where

-- Important all item of lang must have the same length.
langs :: [String]
langs=["en","fr","de"]
fstlang=head langs
sndlang=head (tail langs)
