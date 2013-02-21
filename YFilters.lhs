YFilters
========

> module YFilters
>   ( blogImage
>   , frenchPunctuation
>   )
> where
> import           Hakyll
> import           Control.Category    ((>>>))

Some helpers

> notin :: [Char] -> Char -> Bool
> notin [] _ = True
> notin (x:xs) c = if c == x then False else notin xs c
> notquote = notin "'\""

If you write

    blogimage("url","comment")
    leftblogimage("url","comment")

It will be replaced by the corresponding `img` tag.

> blogImage :: String -> String -> String
> blogImage itemName = replaceAll "(left)?blogimage\\([^)]*\\)" imgstr
>   where left = (=='l') . head
>         leftclass matched = if head matched == 'l'
>                             then " class=\"left\""
>                             else ""
>         src =     dropWhile notquote >>> tail
>               >>> takeWhile notquote
>         alt =     dropWhile notquote >>> tail
>               >>> dropWhile notquote >>> drop 3
>               >>> takeWhile notquote
>         imgstr matched = "<img src=\"/Scratch/img/blog/" ++ itemName ++ "/" ++ src matched ++ "\" alt=\""++ alt matched ++ "\"" ++ leftclass matched ++ ">"

The French punctuation prevent to start a line with a semicolon in French.

> frenchPunctuation :: String -> String
> frenchPunctuation = replaceAll " :</p>" (\_-> "&nbsp;:</p>")
