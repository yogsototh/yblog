--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (forM)
import           Data.Monoid            ((<>))
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
    match "Scratch/img/**"      staticBehavior
    match "Scratch/js/**"       staticBehavior
    match "Scratch/css/fonts/*" staticBehavior

    -- SASS
    match "Scratch/css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
          withItemBody (unixFilter "sass" ["--trace"]) >>=
          return . fmap compressCss

    -- Blog posts
    match "Scratch/*/blog/*.md" markdownPostBehavior

    -- Blog posts with erb extension
    match "Scratch/fr/blog/*.erb" $ do
      route $ niceRoute
      compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx
    match "Scratch/en/blog/*.erb" $ do
      route $ niceRoute
      compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx

    -- Basic files
    match "Scratch/*/*.md" markdownBehavior
    match "Scratch/*/about/*.md" markdownBehavior
    match "Scratch/*/softwares/*.md" markdownBehavior
    match "Scratch/*/softwares/ypassword/*.md" markdownBehavior
    match "Scratch/fr/blog/code/*" staticBehavior

    -- Archives
    match "Scratch/en/blog.md" (archiveBehavior "en")
    match "Scratch/fr/blog.md" (archiveBehavior "fr")

    -- Homepage
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = (field "enposts" $ \_ -> homePostList "en" ((fmap (take 3)) . createdFirst)) <>
                           (field "frposts" $ \_ -> homePostList "fr" ((fmap (take 3)) . createdFirst)) <>
                           yDefaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/boilerplate.html" indexCtx

    match "templates/*" $ compile templateCompiler

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
-- Simply copy in the right place
staticBehavior :: Rules ()
staticBehavior = do
  route   idRoute
  compile copyFileCompiler


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
    itemPath <- getRoute identifier
    return $ renderPandoc (fmap (preFilters itemPath) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/default.html"    yDefaultContext
    >>= relativizeUrls
    >>= loadAndApplyTemplate "templates/boilerplate.html" yDefaultContext
  where
    applyFilter f str = return $ (fmap $ f) str
    preFilters :: Maybe String -> String -> String
    preFilters itemPath =   abbreviationFilter
                          . blogImage itemName
                          . blogFigure itemName
                          where
                            itemName = maybe "" (takeBaseName . takeDirectory) itemPath
    postFilters :: String -> String
    postFilters = frenchPunctuation

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
    return $ renderPandoc (fmap (preFilters (toFilePath identifier)) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= relativizeUrls
    >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx
  where
    applyFilter f str = return $ (fmap $ f) str
    preFilters :: String -> String -> String
    preFilters itemPath =   abbreviationFilter
                          . blogImage itemName
                          . blogFigure itemName
                          where
                            itemName = takeBaseName itemPath
    postFilters :: String -> String
    postFilters = frenchPunctuation

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
    >>= relativizeUrls
    >>= loadAndApplyTemplate "templates/boilerplate.html" archiveCtx
  where
    applyFilter f str = return $ (fmap $ f) str
    preFilters :: String -> String -> String
    preFilters itemPath =   abbreviationFilter
                          . blogImage itemName
                          . blogFigure itemName
                          where
                            itemName = takeBaseName itemPath
    archiveCtx =
      field "posts" (\_ -> postList language createdFirst) <>
      constField "title" "Archives"                        <>
      yDefaultContext
    postFilters :: String -> String
    postFilters = frenchPunctuation

--------------------------------------------------------------------------------
yDefaultContext :: Context String
yDefaultContext = metaKeywordContext <>
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
postCtx :: Context String
postCtx =
    yDefaultContext

--------------------------------------------------------------------------------
postList :: String -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList language sortFilter = do
    posts   <- loadAll (fromGlob $ "Scratch/" ++ language ++ "/blog/*") >>= sortFilter
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

--------------------------------------------------------------------------------
homePostList :: String -> ([Item String] -> Compiler [Item String]) -> Compiler String
homePostList language sortFilter = do
    posts   <- loadAll (fromGlob $ "Scratch/" ++ language ++ "/blog/*") >>= sortFilter
    itemTpl <- loadBody "templates/home-post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
