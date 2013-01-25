--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend,(<>))
import           Hakyll

import           Data.Map             (Map)
import qualified Data.Map             as M


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
          withItemBody (unixFilter "sass" ["--trace"]) >>=
          return . fmap compressCss

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" yDefaultContext
            >>= relativizeUrls

    match "posts/en/*" $ do
        route $ setExtension "html"
        compile $ do pandocCompiler
            >>= applyFilter abbreviationFilter
            >>= applyFilter blogimage
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) <>
                    constField "title" "Archives"              <>
                    yDefaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler



--------------------------------------------------------------------------------

applyFilter f str = return $ (fmap $ f) str

abbreviationFilter :: String -> String
abbreviationFilter = replaceAll "%[a-zA-Z0-9_]*" newnaming
  where
    newnaming matched = case M.lookup (tail matched) abbreviations of
                          Nothing -> matched
                          Just v -> v

--------------------------------------------------------------------------------
yDefaultContext = metaKeywordContext <>
                  languageContext <>
                  prefixContext <>
                  otherlanguageContext <>
                  tradsContext <>
                  defaultContext

--------------------------------------------------------------------------------
-- ABBREVIATIONS

--------------------------------------------------------------------------------
applyAbbrs :: Item String -> Compiler (Item String)
applyAbbrs = applyAsTemplate abbrContext

--------------------------------------------------------------------------------
abbrContext :: Context a
abbrContext = functionField "abbr" $ \args _ ->
    case args of
        [k] -> case M.lookup k abbreviations of
            Just v  -> return v
            Nothing -> fail $ "Abbreviation not found: " ++ k
        _   -> fail "Wrong args for abbreviations"


--------------------------------------------------------------------------------
abbreviations :: Map String String
abbreviations = M.fromList
    [ ("TLDR",   "Too long; didn't read")
    , ("PEBKAC", "Problem exists between keyboard and chair")
    , ("html", "<span class=\"sc\">html</span>")
    , ("css", "<span class=\"sc\">css</span>")
    , ("svg", "<span class=\"sc\">svg</span>")
    , ("test", "Just a test")
    , ("latex", "<span style=\"text-transform: uppercase\">L<sup style=\"vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em\">a</sup>T<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">e</sub>X</span>")
    , ("xelatex", "<span style=\"text-transform: uppercase\">X<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">&#x018E;</sub>L<sup style=\"vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em\">a</sup>T<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">e</sub>X</span>")
    ]

--------------------------------------------------------------------------------
data Trad = Trad { frTrad :: String, enTrad :: String }
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
prefixContext = field "webprefix" $ \_ -> return "/Scratch"

--------------------------------------------------------------------------------
otherlanguageContext = field "otherlanguage" $ \item -> do
  lang <- itemLang item
  return $ if (lang == "en") then "fr" else "en"

itemLang :: Item a -> Compiler String
itemLang item = do
  filepath <- return $ toFilePath (itemIdentifier item)
  return $ if (languageFromPath filepath == "fr" ) then "fr" else "en"
    where
      languageFromPath = take 2 . drop 1

--------------------------------------------------------------------------------
languageContext = field "language" itemLang

--------------------------------------------------------------------------------
metaKeywordContext = field "metaKeywords" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ maybe "" renderMeta $ M.lookup "tags" metadata
    where
      renderMeta tags =
        "<meta name=\"keywords\" content=\"" ++ tags ++ "\">\n"

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    yDefaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "posts/en/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
