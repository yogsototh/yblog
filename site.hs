--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend,(<>))
import           Hakyll

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
        compile $ pandocCompiler
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
yDefaultContext = metaKeywordContext <>
                  languageContext <>
                  prefixContext <>
                  otherlanguageContext <>
                  tradsContext <>
                  defaultContext

--------------------------------------------------------------------------------
trads = [
         ("change language",("En Français","In English"))
        ,("loading",("Chargement en cours","Loading"))
        ,("welcome",("Bientôt","Soon"))
        ]
tradsContext = let newfield key envalue frvalue =
                          field ("trad " ++ key) $ \item -> do
                            filepath <- return $ toFilePath (itemIdentifier item)
                            return $ if (languageFromPath filepath /= "en" )
                                     then frvalue
                                     else envalue
                   languageFromPath = take 2 . drop 1

  in
    foldr1 (<>) $
      map (\(key,(frvalue,envalue)) -> newfield key frvalue envalue) trads

--------------------------------------------------------------------------------
prefixContext = field "webprefix" $ \_ -> return "/Scratch"

--------------------------------------------------------------------------------
otherlanguageContext = field "otherlanguage" $ \item -> do
  filepath <- return $ toFilePath (itemIdentifier item)
  return $ if (languageFromPath filepath /= "en" ) then "en" else "fr"
    where
      languageFromPath = take 2 . drop 1

--------------------------------------------------------------------------------
languageContext = field "language" $ \item -> do
  filepath <- return $ toFilePath (itemIdentifier item)
  return $ if (languageFromPath filepath == "fr" ) then "fr" else "en"
    where
      languageFromPath = take 2 . drop 1

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
