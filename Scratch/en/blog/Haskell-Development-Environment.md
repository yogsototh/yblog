---
kind:           article
published:      2014-08-16
image: /Scratch/img/blog/Haskell-Development-Environment/main.png
title: Haskell Development Environment
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---
blogimage("main.png","Main image")

<div class="intro">

%tldr How to develop with Haskell


</div>

We could discernate two Haskell faces.
Haskell the programming language which brighten far higher than most programming languages.

Haskell the ecosystem. And this is where are the Haskell weaknesses.

Haskell as a language is a very very good language.

But to make great things with a language, we don't only need a great language but a great ecosystem in which we could achieve tremendous things.

Sadly, the history has proved many times that, the ecosystem make a far better job than a great tool for accomplishing things.

I mean, the pyramids, Roma, etc... All these things could have been accomplished better if there were better tools, better ideas, etc...

The reality, is that these great things were done with very basic means.
Mostly broken tools.
But the result is here.

If we focus on our tighter domain (programming languages).
We must acknowledge that even with its horrible aspects, PHP made a great
job at producing things.
Yes, bad, things, bad tools, a lot of suffering for the doers.
But in the end, it worked good enough.

Haskell's community has a tendency to forget this.
Because Haskell is a language that does its best to make things perfect the first time.
It results a mind set that let use believe that everything should be the same.
Here are the greatest weakness of Haskell
for most Haskeller forgot that "Done is better than perfect".

And so, discuss with Haskellers and a lot will make jokes about node.js,
clojure, etc...

The reality is when it comes at doing things, a lots of things are
better in the node.js/clojure world.

One of this thing is starting.

Starting in clojure or node is extremely simple.
Let's take the time before a guy read a random tutorial on clojure or node and
start to hack on its machine.

It is generally in less than 5 minutes.
You launch (a generally insecure) command that download and install all necessary stuff and here you go.

In haskell...

Ah... In Haskell, things are not so easy.
Recently there were a new Haskell platform but this is not enough.
Because, at the second you'll want to use a library, you will certainly stumble on the cabal hell problem.

Some don't see them, but for me, it is a constant struggle.

Very few times the system worked flawlessly.

This is why I am so happy to see that stackage is coming to life!

After this rant, here we go.

Instead of just making me a bad, guy here is my small contribution to
the clean install problem on OS X.

~~~
curl http://github.com/yogsototh/install-haskell/raw/install-haskell.
~~~

Something.
