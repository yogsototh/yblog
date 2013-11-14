---
kind:           article
published:      2013-11-14
image: /Scratch/img/blog/Start-a-Haskell-Project-the-right-way/main.png
en: title: Start a Haskell Project the right way
fr: title: Start a Haskell Project the right way
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---
blogimage("main.png","Main image")

<div class="intro">

en: %tldr Starting a Haskell Project using the right tools.

fr: %tlal Commencer un projet Haskell avec les bon outils.

</div>

When you want to program Haskell, you can use the compiler manually.
But shortly you find yourself doing a lot of stuff repeatedly.
Fortunately Haskell use `cabal` to make your life easier.

Recently I read this excellent article
[How to Start a New Haskell Project](http://jabberwocky.eu/2013/10/24/how-to-start-a-new-haskell-project/).

After having tried its tutorial, I wanted to make some minor changes.
In the end I give a tool to automatize most of this work.

So, mainly, if you do it manually the steps are:

1. [Install Haskell](http://wwW.haskell.org/platform)
2. Make sure you have the latest `cabal-install`

``` bash
> cabal install cabal-install
```

This could take a while.

3. Init your project with `cabal`

``` bash
> mkdir myproject
> cd myproject
> cabal init
```

You should answer some questions.
If you don't know which answer, just choose randomly.
Any answer can be cancelled after by editing the cabal file.
Now you have the following file tree:

``` bash
LICENSE
Setup.hs
myproject.cabal
```

In my case `cabal` failed to find a license and I had to create the LICENSE file.

So the first thing to do is:

``` bash
> perl -pi -e 's/\s*$/\n/' myproject.cabal
```

Yes I dislike lost spaces at the end of lines.
These spaces are highlighted in red in my editor.

The next step is to put your project into a sandbox.
While not completely necessary, I believe it is a good practice to do that.

```
cabal sandbox init
```

Then of course use `git`:

```
> git init
> echo '.cabal-sandbox' >> .gitignore
> echo 'cabal.sandbox.config' >> .gitignore
> echo 'dist' >> .gitignore
> echo '.*.swp' >> .gitignore
> echo '*~' >> .gitignore
> cat .gitignore
.cabal-sandbox
cabal.sandbox.config
dist
*.swp
*~
```
