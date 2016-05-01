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
>   , otherLanguageLinksContext
>   )
> where

Some mandatory imports

> import           Hakyll
> import           Data.Map	(Map)
> import qualified Data.Map     as  M
> import           Data.Monoid	((<>))
> import           Config       (langs,fstlang)
> import           Data.List    (isPrefixOf)

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
>                otherLanguageLinksContext

Let's start by the easiest. Get the current item language.
For me this is easy, the language of an item is where it is in fr or en.

`itemLang` returns the language of the current item.

> --------------------------------------------------------------------------------
> itemLang :: Item a -> Compiler String
> itemLang item = do
>   filepath <- return $ toFilePath (itemIdentifier item)
>   if length filepath > (1+(length sitePrefix))
>     then return $ languageFromPath filepath
>     else return $ fstlang
>   where
>       languageFromPath = take 2 . drop (1+(length sitePrefix))
> --------------------------------------------------------------------------------
> languageContext :: Context a
> languageContext = field "language" itemLang

Next the dictionary containing all traductions of standards templates.

> --------------------------------------------------------------------------------
> trads :: Map String Trad
> trads = M.fromList $ map toTrad [
>          ("welcome",        ["Soon","Bientôt","bald"])
>         ,("switchCss",      ["Change Theme","Changer de theme","Sie ändern Thema"])
>         ,("loading",        ["Loading","Chargement en cours","Verladung"])
>         ,("Home",           ["Home","Accueil","Willkommen"])
>         ,("Blog",           ["Blog","Blog","Blog"])
>         ,("Softwares",      ["Softwares","Logiciels","Software"])
>         ,("About",          ["About","Auteur","über"])
>         ,("Follow",         ["Follow","Suivre","folgen"])
>         ,("fr",             ["French", "Français", "Französisch"])
>         ,("en",             ["English","Anglais" , "Englisch"])
>         ,("de",             ["German", "Allemand", "Deutsch"])
>         ,("socialPrivacy",  ["These social sharing links preserve your privacy"
>                             ,"Ces liens sociaux préservent votre vie privée"
>                             ,"Diese Social Sharing Links bewahren Sie Ihre Privatsphäre"])
>         ]
>         where
>           toTrad (k,tradList) =
>               (k, Trad (M.fromList
>                 (zipWith (\lang trad  -> (L lang,trad)) langs tradList)))

This context will contain all links to the other languages

> tradsContext :: Context a
> tradsContext = mconcat (map addTrad (M.keys trads))
>   where
>     addTrad :: String -> Context a
>     addTrad name =
>       field name $ \item -> do
>           lang <- itemLang item
>           case M.lookup name trads of
>               Just (Trad lmap) -> case M.lookup (L lang) lmap of
>                           Just tr -> return tr
>                           Nothing -> return ("NO TRANSLATION FOR " ++ name)
>               Nothing -> return ("NO TRANSLATION FOR " ++ name)

> --------------------------------------------------------------------------------
> otherLanguageLinksContext :: Context a
> otherLanguageLinksContext = field "otherLanguageLinks" $ \item -> do
>   lang <- itemLang item
>   otherlangs <- return $ filter  (/=lang) langs
>   mItemRoute <- (getRoute . itemIdentifier) item
>   itemRoute <- return $ case mItemRoute of
>                             Nothing -> ""
>                             Just i -> i
>   urls <- return $ map (\l -> changeLanguageTo l itemRoute) otherlangs
>   labels <- return $ map (\l -> tradFor l lang) otherlangs
>   urlsandlabels <- return $ zip urls labels
>   return $ concat $ map linkFromUrlAndLabel urlsandlabels
>   where
>     linkFromUrlAndLabel (url,label) = "<a href=\"" ++ url ++ "\">"++label++"</a> "
>     tradFor str lang =
>         case M.lookup str trads of
>           Nothing -> error "Lack trad for: '" ++ str ++ "'"
>           Just (Trad m) -> case M.lookup (L lang) m of
>                             Nothing -> error $ "Lack trad for: '" ++ str ++ "' in '" ++ lang
>                             Just trd -> trd
>     changeLanguageTo language url =
>        if (isPrefixOf (sitePrefix ++ "/") url)
>             && (length url > preflen)
>             && (url !! (preflen-1) == '/')
>          then "/" ++ sitePrefix ++ "/" ++ language ++ "/" ++ (drop (preflen) url)
>          else "/" ++ url
>       where
>         preflen = 4+length sitePrefix
