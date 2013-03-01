Abbreviations
=============

Declare abbreviations to be used in your content.
For example.

"%TLDR" will be transformed in "Too long; didn't read".

You simply have to add `abbreviationFilter` has a pre filter in the
rule of your markdown. See `site.hs`

> module Abbreviations
>   (abbreviationFilter)
> where
> import           Data.Map       (Map)
> import qualified Data.Map    as M
> import           Hakyll
> --------------------------------------------------------------------------------
> abbreviationFilter :: String -> String
> abbreviationFilter = replaceAll "%[a-zA-Z0-9_]*" newnaming
>   where
>     newnaming matched = case M.lookup (tail matched) abbreviations of
>                           Nothing -> matched
>                           Just v -> v
> --------------------------------------------------------------------------------
> abbreviations :: Map String String
> abbreviations = M.fromList
>     [ ("tldr",   "<span class=\"sc\"><abbr title=\"Too long; didn't read\">tl;dr</abbr>: </span>")
>     , ("tlal",   "<span class=\"sc\"><abbr title=\"Trop long; pas lu\">tlpl</abbr>: </span>")
>     , ("tlpl",   "<span class=\"sc\"><abbr title=\"Trop long; pas lu\">tlpl</abbr>: </span>")
>     , ("html", "<span class=\"sc\">html</span>")
>     , ("css", "<span class=\"sc\">css</span>")
>     , ("svg", "<span class=\"sc\">svg</span>")
>     , ("xml", "<span class=\"sc\">xml</span>")
>     , ("xslt", "<span class=\"sc\">xslt</span>")
>     , ("test", "Just a test")
>     , ("latex", "<span style=\"text-transform: uppercase\">L<sup style=\"vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em\">a</sup>T<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">e</sub>X</span>")
>     , ("xelatex", "<span style=\"text-transform: uppercase\">X<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">&#x018E;</sub>L<sup style=\"vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em\">a</sup>T<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">e</sub>X</span>")
>     , ("mailperso", "<a href=\"&#109;&#097;&#105;&#108;&#116;&#111;:&#121;&#097;&#110;&#110;&#046;&#101;&#115;&#112;&#111;&#115;&#105;&#116;&#111;&#064;&#103;&#109;&#097;&#105;&#108;&#046;&#099;&#111;&#109;\">&#121;&#097;&#110;&#110;&#046;&#101;&#115;&#112;&#111;&#115;&#105;&#116;&#111;&#064;&#103;&#109;&#097;&#105;&#108;&#046;&#099;&#111;&#109;</a>")
>     ]
