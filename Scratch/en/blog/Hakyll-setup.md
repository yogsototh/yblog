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

