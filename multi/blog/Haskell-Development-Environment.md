---
kind:           article
published:      2014-08-16
image: /Scratch/img/blog/Haskell-Development-Environment/main.png
en: title: Haskell Development Environment
fr: title: Haskell Development Environment
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---
blogimage("main.png","Main image")

<div class="intro">

en: %tldr How to develop with Haskell

fr: %tlal

If you don't want to read all and just install Haskell (OS X and Linux only),
run the following in your terminal:

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
sudo ./install-haskell.sh
~~~

If you are on windows, just download the Haskell Platform and follow
the instruction to use stackage.

If you want to know the why and the how; you should read the entire article.

</div>

## A Haskell Weakness

The main weakness of Haskell as nothing to do with the language itself but
with its ecosystem[^1].

[^1]: By ecosystem of a language I mean, the community, the tools, the documentations, the deployment environments, the businesses using the language, etc... Mainly everything that has nothing to do with the detail of a programming language but has to do on how and why we use it.

The main problem I'll try to address is the one known as _cabal hell_.
The community is really active in fixing the issue.
I am very confident that in less than a year this problem will be one of the past.
But to do stuff today, I provide an install method that should reduce greatly
two effects of cabal hell:

- dependency error
- lost time in compilation (poor polar bears)

With my actual installation method, you should minimize your headache and almost
never hit a dependency error.

And more importantly, you should provide your code to any of your co-worker and
be sure that if he used the same install method, your code will work on his machine.

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
sudo ./install-haskell.sh
~~~

If you are a bit adventurous, you might use more libraries, but some might not
compile on your system. In this case add a parameter to the install script:

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
sudo ./install-haskell.sh yolo
~~~

## What the script is doing

You can read the script and you will see that this is quite straightforward.

It download the latest GHC binary for you system and install it.
It does the same with the `cabal` program.
It change your repository to use stackage (exclusive by default).
It installs some useful binaries that might cause compilation error.

As the version of libraries is fixed up until you update the stackage repo.
You should never use cabal sandbox.
That way, you will only compile each needed library once.
The compiled objects/binaries will be in your `~/.cabal` directory.

## Some Last Words

I'll try to update the script every 6 month or so.
That way I'll minimize the number of time bug could occurs.
And in the same time, library will continue to improve.
A bit like the Haskell platform.
What this script brings to the table Haskell platform don't is just
the use of stackage.

It is very easy to use, if you prefer you could also go that way.
Use Haskell Platform and then edit manually your `.cabal/config`.

While it comes to cabal hell, some solutions are sandboxes and `nix`.
Unfortunately, sandboxes didn't worked good enough for me after some time.
Furthermore, sandboxes forces you to re-compile everything by project.
If you have three yesod projects for example it means a lot of time and CPU.
Also, `nix` didn't worked as expected on OS X.
So fixing the list of package to a stable list of them seems to me the best
pragmatic way to handle the problem today.
