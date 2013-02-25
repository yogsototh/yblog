Multi-language contexts
=======================

This file contain all the contexts I use to handle multi-language with Hakyll.

This module provide:

1. A big dictionary :

	keyword :: String -> (french :: String, english :: String)

2. A way to detect the current Hakyll item language.
3. A way to provide links to other language corresponding item.

All these informations are only needed in the templates and not into the
content pages. Therefore, the easies way was to only provide `Context`s.

> module Multilang
>   ( multiContext
>   , tradsContext
>   , languageContext
>   , otherlanguageContext
>   , otherLanguagePathContext
>   )
> where

Some mandatory imports

> import           Hakyll
> import           Data.Map		   (Map)
> import qualified Data.Map		as  M
> import           Data.Monoid	   ((<>))

The data structure data contains the necessary informations for English and
French translation of words.

> data Trad = Trad { frTrad :: String, enTrad :: String }

The `multiContext` contains all other contexts.
For information, a Hakyll context is some kind of
"Item -> Map String String" or in other words a dictionary dependent of the
current generating item.

> --------------------------------------------------------------------------------
> multiContext :: Context a
> multiContext = tradsContext <>
>                languageContext <>
>                otherLanguagePathContext <>
>                otherlanguageContext

Let's start by the easiest. Get the current item language.
For me this is easy, the language of an item is where it is in fr or en.

`itemLang` returns the language of the current item.

> --------------------------------------------------------------------------------
> itemLang :: Item a -> Compiler String
> itemLang item = do
>   filepath <- return $ toFilePath (itemIdentifier item)
>   return $ if (languageFromPath filepath == "fr" ) then "fr" else "en"
>     where
>       languageFromPath = take 2 . drop 1
> --------------------------------------------------------------------------------
> languageContext :: Context a
> languageContext = field "language" itemLang

Next context, return the other language.

> --------------------------------------------------------------------------------
> otherlanguageContext :: Context a
> otherlanguageContext = field "otherlanguage" $ \item -> do
>   lang <- itemLang item
>   return $ if (lang == "en") then "fr" else "en"

The context containing the path of the similar element for the other language

> --------------------------------------------------------------------------------
> otherLanguagePathContext :: Context a
> otherLanguagePathContext = field "otherLanguagePath" $ \item -> do
>   itemRoute <- (getRoute . itemIdentifier) item
>   return $ maybe "/" changeLanguage itemRoute
>   where
>     changeLanguage ('/':'S':'c':'r':'a':'t':'c':'h':'/':'e':'n':'/':xs) = "/Scratch/fr/" ++ xs
>     changeLanguage ('/':'S':'c':'r':'a':'t':'c':'h':'/':'f':'r':'/':xs) = "/Scratch/en/" ++ xs
>     changeLanguage xs = xs

Next the dictionary containing all traductions of standards templates.

> --------------------------------------------------------------------------------
> trads :: Map String Trad
> trads = M.fromList $ map toTrad [
>          ("changeLanguage", ("English", "Français"))
>         ,("switchCss", ("Changer de theme","Change Theme"))
>         ,("loading", ("Chargement en cours","Loading"))
>         ,("welcome", ("Bientôt","Soon"))
>         ]
>         where
>           toTrad (k,(f,e)) = (k, Trad { frTrad = f, enTrad = e })
> --------------------------------------------------------------------------------
> tradsContext :: Context a
> tradsContext = functionField "trad" $ \args item -> do
>                 k <- getArgs args
>                 v <- getValue k trads
>                 lang <- itemLang item
>                 case lang of
>                   "en" -> return (enTrad v)
>                   "fr" -> return (frTrad v)
>                   _    -> fail $ lang ++ " is not a supported language"
>                 where
>                   getArgs [k] = return k
>                   getArgs _   = fail "Wrong arg for trad"
>                   getValue key hmap = case M.lookup key hmap of
>                                         Just value -> return value
>                                         Nothing -> fail "Traduction not found"

