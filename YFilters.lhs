YFilters
========

> module YFilters
>   ( blogImage
>   , blogFigure
>   , frenchPunctuation
>   , highlight
>   )
> where
> import           Hakyll
> import           Control.Category    ((>>>))

Some helpers

> notin :: [Char] -> Char -> Bool
> notin [] _ = True
> notin (x:xs) c = if c == x then False else notin xs c
> notquote :: Char -> Bool
> notquote = notin "'\""

If you write

    blogimage("url","comment")
    leftblogimage("url","comment")

It will be replaced by the corresponding `img` tag.

> blogImage :: String -> String -> String
> blogImage itemName = replaceAll "(left)?blogimage\\([^)]*\\)" imgstr
>   where leftclass matched = if head matched == 'l'
>                             then " class=\"left\""
>                             else ""
>         src =     dropWhile notquote >>> tail
>               >>> takeWhile notquote
>         alt =     dropWhile notquote >>> tail
>               >>> dropWhile notquote >>> drop 3
>               >>> takeWhile notquote
>         imgstr matched = "<div>\n<img src=\"/Scratch/img/blog/" ++ itemName ++ "/" ++ src matched ++ "\" alt=\""++ alt matched ++ "\"" ++ leftclass matched ++ "/>\n</div>"

helper to draw figures

> blogFigure :: String -> String -> String
> blogFigure itemName = replaceAll "(left)?blogfigure\\([^)]*\\)" imgstr
>   where leftclass matched = if head matched == 'l'
>                             then " class=\"left\""
>                             else ""
>         src =     dropWhile notquote >>> tail
>               >>> takeWhile notquote
>         alt =     dropWhile notquote >>> tail
>               >>> dropWhile notquote >>> drop 3
>               >>> takeWhile notquote
>         imgstr matched = "<figure>\n<img src=\"/Scratch/img/blog/" ++ itemName ++ "/" ++ src matched ++ "\" alt=\""++ alt matched ++ "\"" ++ leftclass matched ++ "/>\n<figcaption>"  ++ alt matched ++ "</figcaption>\n</figure>"

The French punctuation prevent to start a line with a semicolon in French.

> frenchPunctuation :: String -> String
> frenchPunctuation = replaceAll " :</p>" (\_-> "&nbsp;:</p>")

I would like to be able to highlight some part of code when I write
`{-hi-}...{-/hi-}`. This way I will also be compatible with School of Haskell.

> highlight :: String -> String
> highlight =
>     (replaceAll "{-hi-}" (\_-> "<span class=\"highlight\">"))
>   . (replaceAll "{-/hi-}" (\_-> "</span>"))
>   . (replaceAll "<span class=\"co\">{-hi-}</span>" (\_-> "<span class=\"highlight\">"))
>   . (replaceAll "<span class=\"co\">{-/hi-}</span>" (\_-> "</span>"))
