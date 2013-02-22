--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (forM)
import           Control.Applicative    ((<$>))
import           Data.Monoid            (mappend,(<>))
import           Hakyll

import           Data.Map               (Map)
import           Data.List              (sortBy)
import           Data.Ord               (comparing)
import           System.Locale          (defaultTimeLocale)
import qualified Data.Map               as M

import           Abbreviations          (abbreviationFilter)
import           YFilters               (blogImage,blogFigure,frenchPunctuation)
import           Multilang              (multiContext)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))


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
  route $ customRoute (\id -> let p=toFilePath id in takeDirectory p </> takeBaseName p </> "index.html" )
  compile $ do
    body <- getResourceBody
    id <- getUnderlying
    itemPath <- getRoute id
    return $ renderPandoc (fmap (preFilters itemPath) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/default.html"    postCtx
    >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx
    >>= relativizeUrls
  where
    applyFilter f str = return $ (fmap $ f) str
    preFilters :: Maybe String -> String -> String
    preFilters itemPath =   abbreviationFilter
                          . blogImage itemName
                          . blogFigure itemName
                          where
                            itemName = maybe "" takeBaseName itemPath
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
  route $ customRoute (\id -> let p=toFilePath id in takeDirectory p </> takeBaseName p </> "index.html" )
  compile $ do
    body <- getResourceBody
    id <- getUnderlying
    itemPath <- getRoute id
    return $ renderPandoc (fmap (preFilters itemPath) body)
    >>= applyFilter postFilters
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx
    >>= relativizeUrls
  where
    applyFilter f str = return $ (fmap $ f) str
    preFilters :: Maybe String -> String -> String
    preFilters itemPath =   abbreviationFilter
                          . blogImage itemName
                          . blogFigure itemName
                          where
                            itemName = maybe "" takeBaseName itemPath
    postFilters :: String -> String
    postFilters = frenchPunctuation

--------------------------------------------------------------------------------
archiveBehavior :: String -> Rules ()
archiveBehavior language = do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList language createdFirst) <>
                    constField "title" "Archives"               <>
                    yDefaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= loadAndApplyTemplate "templates/boilerplate.html" archiveCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "Scratch/img/**"      staticBehavior
    match "Scratch/js/**"       staticBehavior
    match "Scratch/css/fonts/*" staticBehavior

    match "Scratch/css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
          withItemBody (unixFilter "sass" ["--trace"]) >>=
          return . fmap compressCss

    match "Scratch/en/blog/*.md" markdownPostBehavior
    match "Scratch/fr/blog/*.md" markdownPostBehavior
    match "Scratch/fr/*.md" markdownBehavior
    match "Scratch/en/*.md" markdownBehavior

    match "Scratch/fr/blog/*.erb" $ do
      route $ customRoute (\id -> let p=toFilePath id in takeDirectory p </> takeBaseName p </> "index.html" )
      compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx
    match "Scratch/en/blog/*.erb" $ do
      route $ customRoute (\id -> let p=toFilePath id in takeDirectory p </> takeBaseName p </> "index.html" )
      compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx

    match "Scratch/fr/blog/code/*" staticBehavior
    -- TODO erb Behavior

    create ["Scratch/en/archive.html"] (archiveBehavior "en")
    create ["Scratch/fr/archive.html"] (archiveBehavior "fr")

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList "en" ((fmap (take 3)) . createdFirst)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/boilerplate.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
yDefaultContext = metaKeywordContext <>
                  multiContext <>
                  imageContext <>
                  prefixContext <>
                  defaultContext

--------------------------------------------------------------------------------
prefixContext = field "webprefix" $ \_ -> return $ "/Scratch"

--------------------------------------------------------------------------------
imageContext = field "image" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ maybe "/Scratch/img/presentation.png" id $ M.lookup "image" metadata

--------------------------------------------------------------------------------
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
