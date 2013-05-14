Multi-language contexts
=======================

This file contain all the contexts I use to handle multi-language with Hakyll.

This module provide:

1. A big dictionary
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
> import           Config          (langs)
> import           Data.List       (isPrefixOf)

The data structure data contains the necessary informations for English and
French translation of words.

> data Language = L String deriving (Ord,Eq)
> data Trad = Trad (Map Language String)

The site prefix is into this file for now. I will certainly put this into a config file later.

> sitePrefix :: String
> sitePrefix = "Scratch"

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
>       languageFromPath = take 2 . drop (1 + (length sitePrefix))
> --------------------------------------------------------------------------------
> languageContext :: Context a
> languageContext = field "language" itemLang

Next context, return the other language.

> --------------------------------------------------------------------------------
> otherlanguageContext :: Context a
> otherlanguageContext = field "otherlanguage" $ \item -> do
>   lang <- itemLang item
>   return $ if (lang == head langs) then (langs !! 1) else (head langs)

The context containing the path of the similar element for the other language

> --------------------------------------------------------------------------------
> otherLanguagePathContext :: Context a
> otherLanguagePathContext = field "otherLanguagePath" $ \item -> do
>   itemRoute <- (getRoute . itemIdentifier) item
>   return $ maybe "/" changeLanguage itemRoute
>   where
>     langPrefixes = map (\l -> sitePrefix ++ "/" ++ l) langs
>     fstlang = head langPrefixes
>     sndlang = head (tail langPrefixes)
>     changeLanguage url =
>       if (isPrefixOf (head langPrefixes) url)
>          then sndlang ++ (drop (length fstlang) url)
>          else if any (\p -> isPrefixOf p url) (tail langPrefixes)
>                  then fstlang ++ (drop (length sndlang) url)
>                  else url

Next the dictionary containing all traductions of standards templates.

> --------------------------------------------------------------------------------
> trads :: Map String Trad
> trads = M.fromList $ map toTrad [
>          ("welcome",        ["Soon","Bientôt"])
>         ,("switchCss",      ["Change Theme","Changer de theme"])
>         ,("loading",        ["Loading","Chargement en cours"])
>         ,("Home",           ["Home","Accueil"])
>         ,("Blog",           ["Blog","Blog"])
>         ,("Softwares",      ["Softwares","Logiciels"])
>         ,("About",          ["About","Auteur"])
>         ,("Follow",         ["Follow","Suivre"])
>         ,("changeLanguage", ["Français","English"])
>         ,("socialPrivacy",  ["These social sharing links preserve your privacy"
>                             ,"Ces liens sociaux préservent votre vie privée"])
>         ]
>         where
>           toTrad (k,tradList) =
>               (k, Trad (M.fromList
>                 (zipWith (\lang trad  -> (L lang,trad)) langs tradList)))
>
> --------------------------------------------------------------------------------
> tradsContext :: Context a
> tradsContext = functionField "trad" $ \args item -> do
>                 k <- getArgs args
>                 Trad v <- getValue k trads
>                 lang <- itemLang item
>                 case M.lookup (L lang) v of
>                   Just translation -> return translation
>                   Nothing -> fail $ "Couldn't find translation in " ++ lang ++ " for " ++ k
>                 where
>                   getArgs [k] = return k
>                   getArgs _   = fail "Wrong arg for trad"
>                   getValue key hmap = case M.lookup key hmap of
>                                         Just value -> return value
>                                         Nothing -> fail "Traduction not found"

