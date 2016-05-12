---
kind: article
published: 2016-05-06
image: /content/Scratch/img/blog/Haskell-Tutorials--a-tutorial/main.png
title: Haskell Tutorials, a tutorial
author: Yann Esposito
authoruri: yannesposito.com
tags: programming, tutorial, haskell, documentation
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">

%tldr Haskell is awesome! But it is not perfect yet.
We can do a better at documenting our libraries.
This document provide some hints to make it happens.

**Tutorial**:

1. Create a `Tutorial` module containing nothing except documentation.
2. Use `doctest` to check your documentation is up to date
3. For more complex real world examples, link to the test source.

**Examples**:

1. Use `doctest`
2. Use some `CI`

**Generated API Documentation**:

1. Use haddock comments

</div>

Great documentation could make the difference between people using your lib with happiness
and people not using your lib at all.

Documentation can take many different form.
Here is the preferred order (see [What to write](https://jacobian.org/writing/what-to-write/)):

1. **Tutorials** -- write some prose which friendly take a user by hand and help him
2. **Examples** -- how to use each function
3. **Generated API Documentation** -- haddock

## Tutorials

1. Create a new module named Tutorial
2. Create a link to the tutorial in the cabal description
3. Create a link to the tutorial in your README
4. Here is an example of `Tutorial`

~~~.haskell
TODO
~~~

To prevent obsolescence of your tutorial, use `doctest`.

That way when you'll do a `stack test` or `cabal test`
you'll get errors if the Tutorial examples doesn't work anymore.

## Examples (doctest)

`doctest` is a great way to provide examples in your code documenation.
These example will then be used as tests.
Apparently it comes from Python community.

To use `doctest`, this is very simple:

~~~.haskell
-- | My function description
-- 
-- >>> myFunction 3 4
-- 7
myFunction :: Int -> Int -> Int
myFunction = (+)
~~~

And to make it works simply verify you have a `test` bloc in your
`.cabal` file and in the main simply use

~~~.haskell
module Main where

import DocTest

main = docTest [ "src/MyModule/MyFile.hs"
               , "src/MyModule/AnotherFile.hs"
               ]
~~~

## Generated

So even if you do nothing, haddock should generate some API documentation for you for free.
But it will be far better if you help haddock.

~~~
-- | My function description
myFunction :: Type of function
myFunction arg1 -- ^ arg1 description
           arg2 -- ^ arg2 description
           = ...
~~~

and for data

~~~
data MyData = X Int

instance Function MyData where
  ...
~~~


-----------


## Prelude

> Who are you who are so wise in the way of science?

So I am myself largely subject to criticism.
This article isn't intented to be a bible.
More like a tour of what I feel is the most appreciated way to consume documentation.
So please, no arsh feeling.

I wouldn't want this article to be used as a pretext to
start an Holy War about the different Haskell coding style.

For example, I don't see anything wrong relative to the documentation of using

~~~ haskell
import Prelude hiding ((.))
(.) f g x = g (f x)
~~~

Is this an abomination because you consider it will break
Haskellers habit? Yes.
Does it have something to do with clarity? Yes.
Is it documentation? No.

For absolute Haskell beginner using `(f (g (h x)))`
might seems more readable than `f $ g $ h x` or `f . g . h $ x`.
But this is not about documentation.
This is a question of code clarity and readability.

## Other communities

While Haskell is great, some other languages have in my humble opinion
a far better habit concerning documentation.
Documentation shouldn't be felt like a punishment.
On the contrary it is a way of proving by example how your work
is great!

I don't want to dive in the details of the other communities
but I was slightly inspired by:

- [Elm](http://elm-lang.org) → [`guide.elm-lang.org`](http://guide.elm-lang.org) & [`docs`](http://package.elm-lang.org/packages/elm-lang/core/4.0.0)
- [Clojure](http://clojure.org) → [`clojuredocs.org`](http://clojuredocs.org)

There are a lot of thing to say about how the documentation is handled in these communities.
I don't believe I could tell everything I would want to.
But there are some big princples:

A lot of functions are accompagnied with some code example:

- [JSON decode](http://package.elm-lang.org/packages/elm-lang/core/4.0.0/Json-Decode)
- [Random list](http://package.elm-lang.org/packages/elm-lang/core/4.0.0/Random#list)


[^1]: **RANT**: Compare this documentation of core to the documentation of the `Prelude` module in `hackage`; [`Prelude`](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html)

### Clojure

In clojure when you create a new project using `lein new my-project`
a directory `doc` is created for you. It contains a file with a link
to this blog post:

- [What to write](https://jacobian.org/writing/what-to-write/)

A great deal is made about *tutorials*.

Because this is generally what most first users of your library will search for.
They just want to pass from zero to something in the minimal amount of time.

In Haskell we already have API generated documentation for free.
Hackage and Stackage both do a great job at generating your documentation.


So now the best students in class in my humble opinion:

- [`turtle`](https://www.stackage.org/package/turtle)
- [`lens`](https://www.stackage.org/package/lens)

Both library are not only aweseme for different reasons.
Their documentation contains examples, and a tutorial.
You can go deeper if you need to.

To make them even better.

1. Use `doctest` that way you will be able to *test* your tutorial and fix it
   if your API break or change. You'll be able to check it using travis CI for
   example.
2. One advantage of providing a `MyPackage.Tutorial` file is the ability to use `doctest`.

## Good Ideas

- [`clojuredocs.org`](http://clojuredocs.org)

For each symbol necessiting a documentation.
You don't only have the details and standard documentation.
You'll also get:

- Responsive Design (sometime you want to look at documentation on a mobile)
- Contributed Examples
- Contributed See Also section
- Contributed notes/comments

Clojuredocs is an independant website from the official Clojure website.

Most of the time, if you google the function you search
you end up on clojredocs for wich there are many contributions.

Imagine if we had the same functionalities in hackage/stackage.

Today a lot of information is lost on IRC or mailing list.
I know you could always find the information in the archives
but, as an end-user, it is always better to have a centralized
source of information.

Differences with existing:

- hackage has haddock
- stackage has haddock + per package comment

I believe he would be more efficient to have at least a page
by module and why not a page by *symbol*.
I mean:

- for data type definition with all their class instances
- for functions
- for typeclasses

Why?

- far less informations per page.
- Let's keep the pages we have.
- But let's just also focus more.
  So we could provide details about `foldl` for example.
  And make the design cleaner.
  As a matter of design, think about the 4 of 5 most
  important information someone want to have
  as fast as possible and provide them.
  The rest should be at the bottom, or very small in
  the navigation bar.

- function:
  1. type
  2. Documentation string
  3. Examples
  4. the version / who really care?

## How to help

There are 20k Haskell readers.
If only 1% of them pass 10 minutes adding a bit of
documentation it will certainly change a lot of
things in the percieved documenation quality.

Not too much work:

1. login
2. add/edit some example, comments, see-also section

If you pass only the next 10 minutes in adding a bit of
documentation it will certainly change a lot of things.


