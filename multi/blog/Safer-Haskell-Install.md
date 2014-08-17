---
kind:           article
published:      2014-08-16
image: /Scratch/img/blog/Safer-Haskell-Install/main.jpg
en: title: Safer Haskell Install
fr: title: Installer Haskell
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---
blogimage("main.jpg","About to jump")

<div class="intro">

en: %tldr Install Haskell (OS X and Linux only) by pasting the following in your terminal:
fr: %tlal Pour installer Haskell (OS X et Linux) copiez/collez les lignes suivante dans un terminal :

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
sudo ./install-haskell.sh
~~~

en: If you are on windows, just download the Haskell Platform and follow
en: the instruction to use stackage.
fr: Si vous êtes sous windows, téléchargez Haskell Platform
fr: et suivez les instructions pour utiliser stackage.

en: If you want to know the why and the how; you should read the entire article.
fr: Si vous voulez savoir le pourquoi et le comment ; lisez le reste de l'article.

</div>

en: ## Why?
fr: ## Pourquoi ?

en: The main weakness of Haskell as nothing to do with the language itself but
en: with its ecosystem[^1].
fr: La plus grande faiblesse d'Haskell n'a rien à voir avec le langage en lui-même
fr: mais avec son écosystème.

en: [^1]: By ecosystem of a language I mean, the community, the tools, the documentations, the deployment environments, the businesses using the language, etc... Mainly everything that has nothing to do with the detail of a programming language but has to do on how and why we use it.
fr: [^1]: Par l'écosystème d'un langage j'entends, la communauté, les outils, la documentation, les environnements de déploiements, les entreprises qui utilisent le langage, etc... En gros tout ce qui n'a rien à voir avec les détails du langage mais ce qui a à voir avec les comment et pourquoi on l'utilise.

The main problem I'll try to address is the one known as _cabal hell_.
The community is really active in fixing the issue.
I am very confident that in less than a year this problem will be one of the past.
But to do stuff today, I provide an install method that should reduce greatly
two effects of cabal hell:

- dependency error
- lost time in compilation (poor polar bears)

With my actual installation method, you should minimize your headache and almost
never hit a dependency error.
But there could exists some.
If you encounter a dependency error, ask gently to the package manager
to port its package to stackage.

And more importantly, you should provide your code to any of your co-worker and
be sure that if he used the same install method, your code will work on his machine.

~~~
curl -O https://raw.githubusercontent.com/yogsototh/install-haskell/master/install-haskell.sh
sudo ./install-haskell.sh
~~~

## How?

You can read the script and you will see that this is quite straightforward.

1. It download the latest GHC binary for you system and install it.
2. It does the same with the `cabal` program.
3. It change your repository to use stackage (exclusive by default).
4. It installs some useful binaries that might cause compilation error.

As the version of libraries is fixed up until you update the stackage repo.
You should never use cabal sandbox.
That way, you will only compile each needed library once.
The compiled objects/binaries will be in your `~/.cabal` directory.

## Some Last Words

I'll certainly update the script once stackage goes from beta to production.
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

From my point of view, stackage is the best and certainly only best step
in the right direction. The actual problem of cabal hell has nothing to do
with hackage or cabal. The problem is just a human organisation problem.
People tend not to respect the rules in package numbers.
They break their API all the time.
And this is not so much a problem.
The only missing part is a set of stable packages working together.

---

<p class="small">
[The image of the cat about to jump that I slightly edited can found here](https://www.flickr.com/photos/nesster/4198442186/)
</p>


