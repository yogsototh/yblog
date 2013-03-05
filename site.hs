--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (forM)
import           Data.Monoid            ((<>),mconcat)
import           Hakyll

import           Data.List              (sortBy)
import           Data.Ord               (comparing)
import           System.Locale          (defaultTimeLocale)
import qualified Data.Map               as M

import           Abbreviations          (abbreviationFilter)
import           YFilters               (blogImage,blogFigure,frenchPunctuation)
import           Multilang              (multiContext)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match (     "Scratch/img/**"
          .||.  "Scratch/js/**"
          .||.  "Scratch/files/**"
          .||.  "Scratch/css/fonts/*"
          .||.  "Scratch/*/blog/*/**"
          .||.  "CNAME")
      staticBehavior

    -- Compressed SASS
    match "Scratch/css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
                  withItemBody (unixFilter "sass" ["--trace"]) >>=
                  return . fmap compressCss

    -- Blog posts
    match "Scratch/*/blog/*.md" markdownPostBehavior

    -- Blog posts with html extension
    match "Scratch/*/blog/*.html" htmlPostBehavior

    -- Archives
    match "Scratch/en/blog.md" (archiveBehavior "en")
    match "Scratch/fr/blog.md" (archiveBehavior "fr")

    -- RSS
    create ["Scratch/en/blog/feed/feed.xml"] (feedBehavior "en")
    create ["Scratch/fr/blog/feed/feed.xml"] (feedBehavior "fr")

    -- Basic files
    match ("Scratch/*/*.md"
          .||. "Scratch/*/about/*.md"
          .||. "Scratch/*/softwares/*.md"
          .||. "Scratch/*/softwares/ypassword/*.md" ) markdownBehavior
    match "404.md" markdownBehaviorWithSimpleRoute

    -- Homepage
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    (field "enposts" $ \_ -> homePostList "en" createdFirst) <>
                    (field "frposts" $ \_ -> homePostList "fr" createdFirst) <>
                    yContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/boilerplate.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
--
-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr str@(x:xs) | str == "/index.html" = ""
                              | otherwise = x:removeIndexStr xs
    removeIndexStr [] = []

--------------------------------------------------------------------------------
--
-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                             where p=toFilePath ident

--------------------------------------------------------------------------------
--
-- Simply copy in the right place
staticBehavior :: Rules ()
staticBehavior = do
  route   idRoute
  compile copyFileCompiler

--------------------------------------------------------------------------------
applyFilter :: (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str

--------------------------------------------------------------------------------
htmlPostBehavior :: Rules ()
htmlPostBehavior = do
  route $ niceRoute
  compile $ getResourceBody
        >>= applyFilter (abbreviationFilter . frenchPunctuation)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" yContext
        >>= loadAndApplyTemplate "templates/boilerplate.html" yContext
        >>= relativizeUrls
        >>= removeIndexHtml

--------------------------------------------------------------------------------
preFilters :: String -> String -> String
preFilters itemPath =   abbreviationFilter
                      . blogImage itemName
                      . blogFigure itemName
                      where
                        itemName = takeBaseName itemPath

--------------------------------------------------------------------------------
postFilters :: String -> String
postFilters = frenchPunctuation

--------------------------------------------------------------------------------
--
-- change the extension to html
-- prefilter the markdown
-- apply pandoc (markdown -> html)
-- postfilter the html
-- apply templates posts then default then relitivize url
markdownBehavior :: Rules ()
markdownBehavior = do
  route $ niceRoute
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc (fmap (preFilters (toFilePath identifier)) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/default.html" yContext
    >>= loadAndApplyTemplate "templates/boilerplate.html" yContext
    >>= relativizeUrls
    >>= removeIndexHtml

markdownBehaviorWithSimpleRoute :: Rules ()
markdownBehaviorWithSimpleRoute = do
  route $ setExtension "html"
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc (fmap (preFilters (toFilePath identifier)) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/default.html"    yContext
    >>= loadAndApplyTemplate "templates/boilerplate.html" yContext
    >>= relativizeUrls
    >>= removeIndexHtml

--------------------------------------------------------------------------------
-- change the extension to html
-- prefilter the markdown
-- apply pandoc (markdown -> html)
-- postfilter the html
-- apply templates posts then default then relitivize url
markdownPostBehavior :: Rules ()
markdownPostBehavior = do
  route $ niceRoute
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    prefilteredText <- return $ (fmap (preFilters (toFilePath identifier)) body)
    return $ renderPandoc prefilteredText
    >>= applyFilter postFilters
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/post.html"    yContext
    >>= loadAndApplyTemplate "templates/boilerplate.html" yContext
    >>= relativizeUrls
    >>= removeIndexHtml
  where

--------------------------------------------------------------------------------
archiveBehavior :: String -> Rules ()
archiveBehavior language = do
  route $ niceRoute
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc (fmap (preFilters (toFilePath identifier)) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    >>= loadAndApplyTemplate "templates/boilerplate.html" archiveCtx
    >>= relativizeUrls
    >>= removeIndexHtml
  where
    archiveCtx =
      field "posts" (\_ -> postList language createdFirst) <>
      yContext

--------------------------------------------------------------------------------
yContext :: Context String
yContext = metaKeywordContext <>
                  multiContext <>
                  imageContext <>
                  prefixContext <>
                  defaultContext

--------------------------------------------------------------------------------
prefixContext :: Context a
prefixContext = field "webprefix" $ \_ -> return $ "/Scratch"

--------------------------------------------------------------------------------
imageContext :: Context a
imageContext = field "image" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ maybe "/Scratch/img/presentation.png" id $ M.lookup "image" metadata

--------------------------------------------------------------------------------
metaKeywordContext :: Context a
metaKeywordContext = field "metaKeywords" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ maybe "" renderMeta $ M.lookup "tags" metadata
    where
      renderMeta tags =
        "<meta name=\"keywords\" content=\"" ++ tags ++ "\">\n"

--------------------------------------------------------------------------------
createdFirst :: [Item String] -> Compiler [Item String]
createdFirst items = do
  itemsWithTime <- forM items $ \item -> do
    utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
    return (utc,item)
  return $ map snd $ reverse $ sortBy (comparing fst) itemsWithTime

--------------------------------------------------------------------------------
feedBehavior :: String -> Rules ()
feedBehavior language = do
      route idRoute
      compile $ do
        loadAllSnapshots (fromGlob $ "Scratch/" ++ language ++ "/blog/*") "content"
        >>= (fmap (take 10)) . createdFirst
        >>= renderAtom (feedConfiguration "Yann Esposito") feedCtx
      where
        feedCtx :: Context String
        feedCtx = mconcat [bodyField "description", yContext]

--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
  { feedTitle = "yannesposito - " ++ title
  , feedDescription = "Personal blog of Yann Esposito"
  , feedAuthorName = "Yann Esposito"
  , feedAuthorEmail = "yann.esposito@gmail.com"
  , feedRoot = "http://yannesposito.com"
  }

--------------------------------------------------------------------------------
postList :: String -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList language sortFilter = do
    posts   <- loadAll (fromGlob $ "Scratch/" ++ language ++ "/blog/*") >>= sortFilter
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl yContext posts
    return list

--------------------------------------------------------------------------------
homePostList :: String -> ([Item String] -> Compiler [Item String]) -> Compiler String
homePostList language sortFilter = do
    posts   <- loadAll (fromGlob $ "Scratch/" ++ language ++ "/blog/*") >>= sortFilter
    itemTpl <- loadBody "templates/home-post-item.html"
    list    <- applyTemplateList itemTpl yContext (take 3 posts)
    return list
