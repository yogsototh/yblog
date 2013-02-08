--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend,(<>))
import           Hakyll

import           Data.Map             (Map)
import qualified Data.Map             as M

import           Abbreviations        (abbreviationFilter)
import           Multilang            (multiContext)


--------------------------------------------------------------------------------
-- Simply copy in the right place
staticBehavior :: Rules ()
staticBehavior = do
  route   idRoute
  compile copyFileCompiler

--------------------------------------------------------------------------------
-- change the extension to html
-- prefilter the markdown
-- apply pandoc (markdown -> html)
-- postfilter the html
-- apply templates posts then default then relitivize url
markdownBehavior :: Rules ()
markdownBehavior = do
  route $ setExtension "html"
  compile $ do
    body <- getResourceBody
    return $ renderPandoc (fmap preFilters body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls
  where
    applyFilter f str = return $ (fmap $ f) str
    preFilters :: String -> String
    preFilters = abbreviationFilter
    postFilters :: String -> String
    postFilters = id

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "Scratch/images/**" staticBehavior
    match "Scratch/js/**"     staticBehavior

    match "Scratch/css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
          withItemBody (unixFilter "sass" ["--trace"]) >>=
          return . fmap compressCss

    match "Scratch/posts/en/*" markdownBehavior
    match "Scratch/posts/fr/*" markdownBehavior

    match (fromList ["Scratch/about.rst", "Scratch/contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" yDefaultContext
            >>= relativizeUrls

    create ["Scratch/archive.html"] $ do
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
                  multiContext <>
                  prefixContext <>
                  defaultContext

--------------------------------------------------------------------------------
prefixContext = field "webprefix" $ \_ -> return $ "/Scratch"

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
    posts   <- sortFilter <$> loadAll "Scratch/posts/en/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
