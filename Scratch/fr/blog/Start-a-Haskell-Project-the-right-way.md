---
kind:           article
published:      2013-11-14
image: /Scratch/img/blog/Start-a-Haskell-Project-the-right-way/main.png
title: Start a Haskell Project the right way
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---

<div class="intro">


%tlal Un outils pour commencer un nouveau projet Haskell; `cabal`, `git`, testing...

</div>

I recently read this excellent article:
[How to Start a New Haskell Project](http://jabberwocky.eu/2013/10/24/how-to-start-a-new-haskell-project/).

While the article is very good, I lacked some minor informations.
Also, I believe we could improve this a bit.
And this is also the kind of process you might repeat often.
This is why I created a simple shell script to initialize a new Haskell project.

So, mainly, if you do it manually the steps are:

1. [Install Haskell](http://wwW.haskell.org/platform)
2. Make sure you have the latest `cabal-install` (at least 1.18)

``` bash
> cabal install cabal-install
```

3. Download and run the script

``` bash
# Download the script
git clone https://github.com/yogsototh/init-haskell-project.git
# Copy the script in a directory of you PATH variable
cp init-haskell-project/init-haskell-project.sh ~/bin
# Go to the directory containing all your projects
cd my/projects/dir
# Launch the script
init-haskell-project.sh
```

What does this script do that doesn't do cabal.

- Use cabal sandbox
- It initialize `git` with the right `.gitignore` file.
- Use `tasty` to organize your tests (HUnit, QuickCheck and SmallCheck).
- Will make references to Holy Grail
