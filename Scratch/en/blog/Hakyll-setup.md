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

%tldr How I use [hakyll](http://jaspervdj.be/hakyll).
Abbreviations, typography corrections, multi-language,
use `index.html`, etc...


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

To create an archive page, things will start to become more difficult.
Fortunately, an archive page is present in the initial hakyll example.

### Create an RSS feed

### Filter the content

### Add abbreviations support

### Manage two languages
