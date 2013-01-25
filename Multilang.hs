module Multilang
  ( multiContext
  , tradsContext
  , languageContext
  , otherlanguageContext)
where

import           Hakyll
import           Data.Map      (Map)
import qualified Data.Map   as M
import           Data.Monoid   ((<>))


data Trad = Trad { frTrad :: String, enTrad :: String }

multiContext = tradsContext <>
               languageContext <>
               otherlanguageContext

--------------------------------------------------------------------------------
languageContext = field "language" itemLang

--------------------------------------------------------------------------------
trads :: Map String Trad
trads = M.fromList $ map toTrad [
         ("changeLanguage",("En Français","In English"))
        ,("switchCss",("Changer de theme","Change Theme"))
        ,("loading",("Chargement en cours","Loading"))
        ,("welcome",("Bientôt","Soon"))
        ]
        where
          toTrad (k,(f,e)) = (k, Trad { frTrad = f, enTrad = e })

--------------------------------------------------------------------------------
itemLang :: Item a -> Compiler String
itemLang item = do
  filepath <- return $ toFilePath (itemIdentifier item)
  return $ if (languageFromPath filepath == "fr" ) then "fr" else "en"
    where
      languageFromPath = take 2 . drop 1
--------------------------------------------------------------------------------
tradsContext = functionField "trad" $ \args item -> do
                k <- getArgs args
                v <- getValue k trads
                lang <- itemLang item
                case lang of
                  "en" -> return (enTrad v)
                  "fr" -> return (frTrad v)
                  _    -> fail $ lang ++ " is not a supported language"
                where
                  getArgs [k] = return k
                  getArgs _   = fail "Wrong arg for trad"
                  getValue key hmap = case M.lookup key hmap of
                                        Just value -> return value
                                        Nothing -> fail "Traduction not found"

--------------------------------------------------------------------------------
otherlanguageContext = field "otherlanguage" $ \item -> do
  lang <- itemLang item
  return $ if (lang == "en") then "fr" else "en"
