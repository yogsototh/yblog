---
kind:           article
published:      2013-03-16
image: /Scratch/img/blog/Hakyll setup/main.png
title: Hakyll setup
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">


%tlal Comment j'utilise [hakyll](http://jaspervdj.be/hakyll).
Abréviations, corrections typographiques, multi-language,
utilisation d'`index.html`, etc...

</div>

The website your are reading is done with [Hakyll][hakyll]

[hakyll]: http://jaspervdj.be/hakyll

[Hakyll][hakyll] can be considered as a minimal %cms.
But more generally it is a way to organize file generation.

My current workflow is:

1. Initialize my environment:
    a. open a Terminal, launch my script `preview.sh`.
    b. open my browser on [localhost:8000](http://localhost:8000).
2. Work:
    a. Write (customized) markdown files
    b. Time to time, I reload the browser to the corresponding file.
3. Deploy:
    a. launch the script `fastpublish.sh` (mainly do a git push)

Its main work can be abstracted as:

>creating (resp. updating) %html file when I create (resp. change) a markdown file.

While it sounds easy, there are in fact a lot of hidden details.
The work of Hakyll is to help you with these.

## The concepts and syntax

For each file you will create, you have to give, a destination path and
a list of filters.

First, let's start with the simplest case.
You simply want to deal with static files (images, fonts, etc...).
Generally, you have a source directory (here is the current directory)
and a destination directory `_site`.

You write:

``` haskell
-- for each file in the static directory
match "static/*" do
  -- don't change its name nor directory
  route   idRoute
  -- don't change its content
  compile copyFileCompiler
```

This program will copy ``static/foo.jpg`` to ``_site/static/foo.jpg``.
A bit overkill for a simple `cp`, but here is something more interesting.
We want to write a markdown file and generate an %html one.

``` haskell
-- for each file with md extension in the "posts/" directory
match "posts/*.md" do
  -- change its extension to html
  route $ setExtension "html"
  -- use pandoc library to compile the markdown content into html
  compile $ pandocCompiler
```

If you create a file ``posts/foo.md``,
it will create a file ``_site/posts/foo.html``.

If the file ``posts/foo.md``, contained

``` markdown
# Cthulhu

ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn
```

The file ``posts/foo.md``, would contains

``` html
<h1>Cthulhu</h1>
<p>ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn</p>
```

But horror! ``_site/posts/cthulhu.html`` is not a valid %html file.
It doesn't have any header nor footer, etc...
This is where you use templates.
I simply add a new directive in the compile block.

``` haskell
match "posts/*.md" do
  route $ setExtension "html"
  compile $ pandocCompiler
    -- use the template with the current content
    {-hi-}>>= loadAndApplyTemplate "templates/post.html" defaultContext{-/hi-}
```

Now if ``templates/posts.html`` contains:

``` html
<html>
  <head>
    <title>How could I get the title?</title>
  </head>
  <body>
    {-hi-}$body${-/hi-}
  </body>
</html>
```

Now our `cthulhu.html` contains (indention added for readability):

``` html
<html>
  <head>
    <title>How could I get the title?</title>
  </head>
  <body>
    {-hi-}<h1>Cthulhu</h1>
    <p>ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn</p>{-/hi-}
  </body>
</html>
```

See, its easy
But we have a problem. How could we change the title?
Or for example, add keywords?

The solution is to use `Context`s.
For this, we first need to add some metadatas to our markdown.

``` markdown
{-hi-}---
title: Cthulhu
---{-/hi-}
# Cthulhu

ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn
```

And modify slightly our template:

``` html
<html>
  <head>
    <title>{-hi-}$title${-/hi-}</title>
  </head>
  <body>
    $body$
  </body>
</html>
```

As Sir Robin said just before dying before the Bridge of Death:

> **"That's EASY!"**
>
> -- <cite>Sir Robin,
> the Not-Quite-So-Brave-As-Sir-Lancelot</cite>

## Real customization

Now that we understand the basic functionality.
How to:

- use SASS?
- add keywords?
- simplify %url?
- create an archive page?
- create an RSS feed?
- filter the content?
- add abbreviations support?
- manage two languages?

### Use SASS

That's easy.
Simply call the executable using `unixFilter`.
Of course you'll have to install SASS (`gem install sass`).
And we also use compressCss to gain some space.

``` haskell
match "css/*" $ do
    route   $ setExtension "css"
    compile $ getResourceString >>=
              withItemBody (unixFilter "sass" ["--trace"]) >>=
              return . fmap compressCss
```

### Add keywords

In order to help to reference your website on the web, it is nice
to add some keywords as meta datas to your %html page.

``` html
<meta name="keywords"
      content="Cthulhu, Yog-Sothoth, Shub-Niggurath">
```

In order to add keywords, we could not directly use the markdown metadatas.
Because, without any, there should be any meta tag in the %html.

An easy answer is to create a `Context` that will contains the meta tag.

``` haskell
-- metaKeywordContext will return a Context containing a String
metaKeywordContext :: Context String
-- can be reached using $metaKeywords$ in the templates
-- Use the current item (markdown file)
metaKeywordContext = field "metaKeywords" $ \item -> do
  -- tags contains the content of the "tags" metadata
  -- inside the item (understand the source)
  tags <- getMetadataField (itemIdentifier item) "tags"
  -- if tags is empty return an empty string
  -- in the other case return
  --   <meta name="keywords" content="$tags$">
  return $ maybe "" showMetaTags tags
    where
      showMetaTags t = "<meta name=\"keywords\" content=\""
                       ++ t ++ "\">\n"
```

Then we pass this `Context` to the `loadAndApplyTemplate` function:

``` haskell
match "posts/*.md" do
  route $ setExtension "html"
  compile $ pandocCompiler
    -- use the template with the current content
    >>= loadAndApplyTemplate "templates/post.html"
            (defaultContext {-hi-}<> metaKeywordContext{-/hi-})
```

### Simplify %url

What I mean is to use url of the form:

```
http://domain.name/post/title-of-the-post/
```

I prefer this than having to add file with `.html` extension.
We have to change the default Hakyll route behavior.
We create another function `niceRoute`.

``` haskell
-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
        takeDirectory p </> takeBaseName p </> "index.html"
    where p=toFilePath ident
```

Not too difficult. But! There might be a problem.
What if there is a `foo/index.html` link instead of a clean `foo/` in some content?

Very simple, we simply remove all '/index.html' to all our links.

``` haskell
-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr str@(x:xs) | str == "/index.html" = ""
                              | otherwise = x:removeIndexStr xs
    removeIndexStr [] = []
```

And we apply this filter at the end of our compilation

``` haskell
match "posts/*.md" do
  route $ setExtension "html"
  compile $ pandocCompiler
    -- use the template with the current content
    >>= loadAndApplyTemplate "templates/post.html" defaultContext
    {-hi-}>>= removeIndexStr{-/hi-}
```

### Create an archive page

Creating an archive start to be difficult.
There is an example in the default Hakyll example.
Unfortunately, it assumes all posts prefix their name with a date
like in `2013-03-20-My-New-Post.md`.

I migrated from an older blog and didn't want to change my %url.
Also I prefer not to use any filename convention.
Therefore, I add the date information in the metadata `published`.
And the solution is here:

``` haskell
match "archive.md" $ do
  route $ niceRoute
  compile $ do
    body <- getResourceBody
    return $ renderPandoc body
      {-hi-}>>= loadAndApplyTemplate "templates/archive.html" archiveCtx{-/hi-}
      >>= loadAndApplyTemplate {-hi-}"templates/base.html"{-/hi-} defaultContext
      >>= removeIndexHtml
```

Where `templates/archive.html` contains

``` html
$body$

<ul>
    $posts$
</ul>
```

And `base.html` is a standard template (simpler than `post.html`).

`archiveCtx` provide a context containing an %html representation
of a list of posts in the metadata named `posts`.
It will be used in the `templates/archive.html` file with `$posts$`.

``` haskell
archiveCtx =
  defaultContext <>
  metaKeywordContext <>
  {-hi-}field "posts" (\_ -> postList createdFirst){-/hi-}
```

`postList` returns an %html representation of a list of posts
given an Item sort function.
The representation will apply a minimal template on all posts.
Then it concatenate all the results.
The template is `post-item.html`:

``` html
<li><a href="$url$">$published$ - $title$</a></li>
```

Here is how it is done:

``` haskell
postList :: [Item String] -> Compiler [Item String]
            -> Compiler String
postList sortFilter = do
    -- sorted posts
    posts   <- loadAll "post/*" >>= sortFilter
    itemTpl <- loadBody "templates/post-item.html"
    -- we apply the template to all post
    -- and we concatenate the result.
    -- list is a string
    list    <- applyTemplateList itemTpl defaultContext posts
    return list
```

`createdFirst` sort a list of item and put it inside `Compiler` context.
We need to be in the `Compiler` context to access metadatas.

``` haskell
createdFirst :: [Item String] -> Compiler [Item String]
createdFirst items = do
  -- itemsWithTime is a list of couple (date,item)
  itemsWithTime <- forM items $ \item -> do
    -- getItemUTC will look for the metadata "published" or "date"
    -- then it will try to get the date from some standard formats
    utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
    return (utc,item)
  -- we return a sorted item list
  return $ map snd $ reverse $ sortBy (comparing fst) itemsWithTime
```

It wasn't so easy.
But it works pretty well.

### Create an RSS feed

To create an RSS feed, we have to:

- select only the lasts posts.
- generate partially rendered posts (no css, js, etc...)

We could then render the posts twice.
One for %html rendering and another time for %rss.
Remark we need to generate the %rss version to create the %html one.

One of the great feature of Hakyll is to be able to save snapshots.
Here is how:

``` haskell
match "posts/*.md" do
  route $ setExtension "html"
  compile $ pandocCompiler
    -- save a snapshot to be used later in RSS generation
    {-hi-}>>= saveSnapshot "content"{-/hi-}
    >>= loadAndApplyTemplate "templates/post.html" defaultContext
```

Now to each post there is a snapshot named "content" associated.
Furthermore feed don't need a source markdown file.
Then we create a new file from no one.
Instead of using `match`, we use `create`:

``` haskell
create ["feed.xml"] $ do
      route idRoute
      compile $ do
        -- load all "content" snapshots of all posts
        loadAllSnapshots "posts/*" "content"
        -- take the latest 10
        >>= (fmap (take 10)) . createdFirst
        -- renderAntom feed using some configuration
        >>= renderAtom feedConfiguration feedCtx
      where
        feedCtx :: Context String
        feedCtx =  defaultContext <>
                   -- $description$ will render as the post body
                   {-hi-}bodyField "description"{-/hi-}
```

The `feedConfiguration` contains some general informations about the feed.

``` haskell
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "Great Old Ones"
  , feedDescription = "This feed provide information about Great Old Ones"
  , feedAuthorName = "Abdul Alhazred"
  , feedAuthorEmail = "abdul.alhazred@great-old-ones.com"
  , feedRoot = "http://great-old-ones.com"
  }
```

Great idea certainly steal from [nanoc][nanoc] (my previous blog engine)!

[nanoc]: http://nanoc.ws

### Filter the content

As I just said, [nanoc][nanoc] was my preceding blog engine.
It is written in Ruby and as Hakyll, it is quite awesome.
And one thing Ruby does more naturally than Haskell is regular expressions.
I had a _lot_ of filters in nanoc.
I lost some because I don't use them much.
But I wanted to keep some.
Generally, filtering the content is just a way to apply
to the body a function of type `String -> String`.

Also we generally want prefilters (to filter the markdown)
and postfilters (to filter the %html after the pandoc compilation).

Here is how I do it:

``` haskell
markdownPostBehavior = do
  route $ niceRoute
  compile $ do
    body <- getResourceBody
    {-hi-}prefilteredText <- return $ (fmap preFilters body){-/hi-}
    {-hi-}return $ renderPandoc prefilteredText{-/hi-}
    {-hi-}>>= applyFilter postFilters{-/hi-}
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/post.html"    yContext
    >>= loadAndApplyTemplate "templates/boilerplate.html" yContext
    >>= relativizeUrls
    >>= removeIndexHtml
```

Where

``` haskell
applyFilter strfilter str = return $ (fmap $ strfilter) str
preFilters :: String -> String
postFilters :: String -> String
```

Now we have a simple way to filter the content.
Let's augment the markdown ability.

### Add abbreviations support

Comparing to %latex, a very annoying markdown limitation
is the lack of abbreviations.

Fortunately we can filter our content.
And here is the filter I use:

``` haskell
abbreviationFilter :: String -> String
abbreviationFilter = replaceAll "%[a-zA-Z0-9_]*" newnaming
  where
    newnaming matched = case M.lookup (tail matched) abbreviations of
                          Nothing -> matched
                          Just v -> v
abbreviations :: Map String String
abbreviations = M.fromList
    [ ("html", "<span class=\"sc\">html</span>")
    , ("css", "<span class=\"sc\">css</span>")
    , ("svg", "<span class=\"sc\">svg</span>")
    , ("xml", "<span class=\"sc\">xml</span>")
    , ("xslt", "<span class=\"sc\">xslt</span>") ]
```

It will search for all string starting by '%' and it will search
in the `Map` if there is a corresponding abbreviation.
If there is one, we replace the content.
Otherwise we do nothing.

Do you really believe I type
``%latex``
each time I write %latex?

### Manage two languages

Generally I write my post in English and French.
And this is more difficult than it appears.
For example, I need to filter the language in order to get
the right list of posts.
I also use some words in the templates and I want them to be translated.

A nice tip is to pass arguments to a context and use it in the template.
Typically I write:

``` html
<a href="$otherLanguagePath$"
	onclick="setLanguage('$otherlanguage$')">
	{-hi-}$trad changeLanguage${-/hi-} </a>
```

First I create a Map containing all translations.

``` haskell
data Trad = Trad { frTrad :: String, enTrad :: String }

trads :: Map String Trad
trads = M.fromList $ map toTrad [
   ("changeLanguage",
      ("English"
      , "Français"))
  ,("switchCss",
      ("Changer de theme"
      ,"Change Theme"))
  ,("socialPrivacy",
      ("Ces liens sociaux préservent votre vie privée"
      ,"These social sharing links preserve your privacy"))
  ]
  where
    toTrad (key,(french,english)) =
      (key, Trad { frTrad = french , enTrad = english })
```

Then I create a context taking an argument:

```
tradsContext :: Context a
tradsContext = functionField "trad" $ \args item -> do
  -- get the key
  k <- getArgs args
  -- get its value (a Trad object)
  v <- getValue k trads
  -- get the current item language
  lang <- itemLang item
  case lang of
    "en" -> return (enTrad v)
    "fr" -> return (frTrad v)
    _    -> fail $ lang ++ " is not a supported language"
  where
    getArgs [k] = return k
    getArgs _   = fail "Wrong arg for trad"
    -- search the Trad associated the key
    getValue key hmap =
        case M.lookup key hmap of
          Just value -> return value
          Nothing -> fail "Traduction not found"
```

In the real code source I also need more functions.
But I just wanted to show how to pass parameters to a metadata tag.

## Conclusion

The full code is [here](http://github.com/yogsototh/yblog.git).
And except from the main file, I use literate Haskell.
This way the code should be easier to understand.
