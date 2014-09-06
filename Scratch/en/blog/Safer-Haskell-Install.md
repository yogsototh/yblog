---
kind:           article
published:      2014-08-16
image: /Scratch/img/blog/Safer-Haskell-Install/main.jpg
title: Safer Haskell Install
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---
blogimage("main.jpg","to Haskell and Beyond!!!")

<div class="intro">

%tldr Install Haskell (OS X and Linux only) by pasting the following in your terminal:

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
chmod ugo+x install-haskell.sh
sudo ./install-haskell.sh $USER
~~~

If you are on windows, just download the Haskell Platform and follow
the instruction to use [stackage][stackage].

If you want to know the why and the how; you should read the entire article.

</div>

## Why?

The main weakness of Haskell as nothing to do with the language itself but
with its ecosystem[^1].

[^1]: By ecosystem of a language I mean, the community, the tools, the documentations, the deployment environments, the businesses using the language, etc... Mainly everything that has nothing to do with the detail of a programming language but has to do on how and why we use it.

The main problem I'll try to address is the one known as _cabal hell_.
The community is really active in fixing the issue.
I am very confident that in less than a year this problem will be one of the past.
But to work today, I provide an install method that should reduce greatly
two effects of cabal hell:

- dependency error
- lost time in compilation (poor polar bears)

With my actual installation method, you should minimize your headache and almost
never hit a dependency error.
But there could exists some.
If you encounter any dependency error,
ask gently to the package manager to port its package to [stackage][stackage].

So to install copy/paste the following three lines in your terminal:

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
chmod ugo+x install-haskell.sh
sudo ./install-haskell.sh $USER
~~~

## How?

You can read the script and you will see that this is quite straightforward.

1. It download the latest GHC binary for you system and install it.
2. It does the same with the `cabal` program.
3. It change your repository to use [stackage][stackage] (exclusive by default).
4. It installs some useful binaries that might cause compilation error.

As the version of libraries is fixed up until you update the [stackage][stackage] repo.
You should never use cabal sandbox.
That way, you will only compile each needed library once.
The compiled objects/binaries will be in your `~/.cabal` directory.

## Some Last Words

I'll certainly update the script once [stackage][stackage] goes from beta to production.
I'll try to update the script every 6 month or so.
That way I'll minimize the number of time bug could occurs.
And in the same time, library will continue to improve.
A bit like the Haskell platform.
What this script brings to the table Haskell platform don't is just
the use of [stackage][stackage].

It is very easy to use, if you prefer you could also go that way.
Use Haskell Platform and then edit manually your `.cabal/config`.

While it comes to cabal hell, some solutions are sandboxes and `nix`.
Unfortunately, sandboxes didn't worked good enough for me after some time.
Furthermore, sandboxes forces you to re-compile everything by project.
If you have three yesod projects for example it means a lot of time and CPU.
Also, `nix` didn't worked as expected on OS X.
So fixing the list of package to a stable list of them seems to me the best
pragmatic way to handle the problem today.

From my point of view, [stackage][stackage] is the best step in the right direction.
The actual cabal hell problem is more a human problem than a tool problem.
This is a bias in most programmer to prefer resolve social issues using tools.
There is nothing wrong with hackage and cabal.
But for a package manager to work in a static typing language as Haskell,
packages much work all together.
This is a great strength of static typed languages that they ensure that a big
part of the API between packages are compatible.
But this make the job of package managing far more difficult than in dynamic languages.

People tend not to respect the rules in package numbering[^2].
They break their API all the time.
So we need a way to organize all of that.
And this is precisely what [stackage][stackage] provide.
A set of stable packages working all together.
So if a developer break its API, it won't work anymore in stackage.
And whether the developer fix its package or all other packages upgrade their usage.
During this time, [stackage][stackage] end-users will be able to develop without dependency issues.

[^2]: I myself am guilty of such behavior. It was a beginner error.

[stackage]: http://www.stackage.org

---

<p class="small">
[The image of the cat about to jump that I slightly edited can found here](https://www.flickr.com/photos/nesster/4198442186/)
</p>


