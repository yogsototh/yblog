---
kind: article
published: 2016-05-06
image: /content/Scratch/img/blog/Haskell-Tutorials--a-tutorial/main.jpg
title: Haskell Tutorials, a tutorial
author: Yann Esposito
authoruri: yannesposito.com
tags: programming, tutorial, haskell, documentation
theme: scientific
---
blogimage("main.jpg","Main image")

<div class="intro">

%tldr Some hints on how to make great documentation for Haskell libraries.

1. Create a `Tutorial` module containing nothing except documentation.
2. Mention the `Tutorial` module in your `cabal` description
2. Use `doctest` to check your documentation is up to date
3. For more complex real world examples, link to the source of some test.

</div>

Great documentation make a big difference.
A bad documentation could simply make people not using your lib.

My friend was learning Haskell.
To start he tried a Haskell library to make a small application.
The documentation was deprecated to the point he wasn't able to make a basic example work.
How do you believe he felt?
What does he thought about Haskell in general?

So here are my hint on how to make a great documentation in Haskell.

Documentation can take many different form.

1. **Tutorials/Guides** -- write some prose which friendly take a user by hand and help him
2. **Examples** -- how to use each function
3. **Generated API Documentation** -- haddock

## Hints

### Tutorials/Guides

1. Create a new module named `Tutorial` (or `Guide.GuideTopic`)
2. Create a link to the tutorial in the cabal description
3. Create a link to the tutorial in your README
4. Here is an example some `Tutorial` module content:

~~~haskell
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
 Use @my-package@ if you want to ...
-}
module Data.Duration.Tutorial (

  -- * Introduction
  -- $introduction

  -- ** Subsection
  -- $subsection

  ) where

import Data.Duration

{- $introduction

So here how you use it:

 >>> humanReadableDuration 1002012.002
 "11 days 14 hours 20 min 12s 2ms"

The function is 'humanReadableDuration' and
the you'll be able to click on it to go
to its definition.

You can add images: <<path-to-image.png title>>
and links: <http://haskell-lang.org haskell>.
 
-}

{- $subsection

This is a chuck of documentation
not attached to any particular Haskell
declaration with an untested code block:

> answer = 42

-}
~~~

To prevent obsolescence of your tutorial, use `doctest`.

That way when you'll do a `stack test` or `cabal test`
you'll get errors if some example doesn't work anymore.

### Examples (doctest)

`doctest` is a great way to provide examples in your code documentation.
These example will then be used as tests.
Apparently it comes from Python community.

To use `doctest`, this is very simple:

~~~haskell
-- | My function description
-- 
-- >>> myFunction 3 4
-- 7
myFunction :: Int -> Int -> Int
myFunction = (+)
~~~

And to make it works simply verify you have a `test` bloc in your
`.cabal` file looking like this:

~~~
test-suite doctest
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: DocTest.hs
  build-depend: base >= 4.7 && < 5
              , <YOUR_LIBRARY> 
              , Glob >= 0.7
              , doctest >= 0.9.12
~~~

and in `test/DocTest.hs` simply use

~~~haskell
module Main where

import DocTest
import System.FilePath.Glob (glob)

main = glob "src/**/*.hs" >>= docTest
~~~

Now `stack test` or `cabal test` will check the validity of your documentation.

## Bonuses

### Verifying documentation coverage

1. Install haddock `stack install haddock` or `cabal install haddock`
2. Launch haddock without output format:

~~~
> haddock src/**/*.hs
Haddock coverage:
 100% ( 15 / 15) in 'Data.Duration'
 100% (  3 /  3) in 'Data.Duration.Tutorial'
~~~

### Continuous Integration

There are plenty of alternative solution.
I provide the one I believe would be used by most people.
So if you use `github` simply create an account on [`travis`](http://travis-ci.org).

Add a `.travis.yml` file in your repo containing:

~~~.yaml
language: haskell
~~~

If you want to use `stack` instead of just `cabal`:

~~~.yaml
sudo: false

addons:
  apt:
    packages:
      - libgmp-dev

# Caching so the next build will be fast too.
cache:
  directories:
  - $HOME/.stack

before_install:
# Download and unpack the stack executable
- mkdir -p ~/.local/bin
- export PATH=$HOME/.local/bin:$PATH
- travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

script:
  - stack setup && stack --no-terminal --skip-ghc-check test
~~~

Don't forget to activate your repo in travis.

For some bonus points add the build status badge in your `README.md` file:

~~~.markdown
[![Build Status](https://travis-ci.org/user-name/project-name.svg?branch=master)](https://travis-ci.org/user-name/project-name)
~~~

Congratulation!
Now if you break your documentation examples, you'll get notified.

### Badges

You could add badges to your `README.md` file.

Here is a list of some: [`shields.io`](http://shields.io)

#### Hackage

~~~.markdown
[![Hackage](https://img.shields.io/hackage/v/packagename)](https://hackage.haskell.org/package/packagename)
~~~

#### Stackage

If you didn't declared your package to `stackage`, please do it.
It isn't much work.
Just edit a file to add your package.
And you'll could be able to add another badge:

~~~.markdown
[![packagename on Stackage LTS](http://stackage.org/package/packagename/badge/lts-3)](http://stackage.org/lts/package/packagename)
~~~

See [Stackage Badges](https://www.fpcomplete.com/blog/2015/10/stackage-badges)
for more informations.

### Creating a new project with `stack`

If you use `stack` I suggest you to use the `tasty-travis` template.
It will include the boilerplate for:

- tests
- doctest
- benchmark
- travis CI
- a README file to help you start

So edit your `~/.stack/config.yaml` like this:

~~~
templates:
  params:
      author-name: Your Name
      author-email: your@mail.com
      copyright: 'Copyright: (c) 2016 Your Name'
      github-username: yourusername
      category: Development
~~~

And then you can create a new projec with:

~~~
stack new my-project tasty-travis
~~~


## Generated Documentation

Even not doing anything, if you submit your library to hackage,
haddock should generate some API documentation for free.

But to make _real_ documentation you _need_ to add some manual annotations.

**Functions**:

~~~
{-hi-}-- | My function description{-/hi-}
myFunction :: Type of function
myFunction arg1 {-hi-}-- ^ arg1 description{-/hi-}
           arg2 {-hi-}-- ^ arg2 description{-/hi-}
           = ...
~~~


**Data**:

~~~
data MyData a b
  = C1 a b -- ^ doc for constructor C1
  | C2 a b -- ^ doc for constructor C2

data MyData a b
  = C { a :: TypeA {-hi-}-- ^ field a description{-/hi-}
      , b :: TypeB {-hi-}-- ^ field b description{-/hi-}
      }
~~~

**Module**:

~~~
{-|
Module    : MyModule
Description: Short description
Copyright : (c)
License : MIT

Here is a longer description of this module.
With some code symbol @MyType@.
And also a block of code:

@
data MyData = C Int Int

myFunction :: MyData -> Int
@

-}
~~~

**Documentation Structure**:

~~~
module MyModule (
  -- * Classes
  C(..),
  -- * Types
  -- ** A data type
  T,
  -- ** A record
  R,
  -- * Some functions
  f, g
  ) where
~~~

That will generate headings.


## Other Random Ideas

In Haskell we have great tools like
[`hayoo!`](http://hayoo.fh-wedel.de)
and [`hoogle`](https://www.haskell.org/hoogle/).

And `hackage` and `stackage` provide also a lot of informations.

But generally we lack a lot of Tutorials and Guides.
This post was an attempt to help people making more of them.

But there are other good ideas to help improve the situation.

### Create a doc with link to best practices

In clojure when you create a new project using `lein new my-project`
a directory `doc` is created for you. It contains a file with a link
to this blog post:

- [What to write](https://jacobian.org/writing/what-to-write/)

### Having a page by function/symbol with comments

If you try to search for some clojure function on a search engine
there is a big chance the first result will link to:

- [`clojuredocs.org`](http://clojuredocs.org): try to search for `reduce`, `update-in` or `index` for example

For each symbol necessiting a documentation.
You don't only have the details and standard documentation.
You'll also get:

- Responsive Design (sometime you want to look at documentation on a mobile)
- Contributed Examples
- Contributed See Also section
- Contributed notes/comments

[`clojuredocs.org`](http://clojuredocs.org) is an independant website from the official Clojure website.

Most of the time, if you google the function you search
you end up on [clojuredocs](http://clojuredocs.org) for wich there are many contributions.

Currently stackage is closer to these feature than hackage.
Because on stackage you have access to the README and also some comments by package.

I believe it would be more efficient to have at least a page
by module and why not a page by *symbol* (data, functions, typeclasses...).

For example, we could provide details about `foldl` for example.
Also as there would be less information to display, it will make the design cleaner.

Today, if you want to help documenting, you need to make a PR to the source of some library.
While if we had an equivalent to clojuredocs for Haskell,
adding documentation would simply be a few clicks away:

1. login
2. add/edit some example, comments, see-also section

There are more than 23k people on `/r/haskell`.
If only 1% of them would take 10 minutes adding a bit of
documentation it will certainly change a lot of
things in the percieved documentation quality.

And last but not least, 

## **Design is important**

blogimage("design_is_important.jpg","Design is Important")

Design is a vague word.
A good design should care not only about how something look,
but also how users will interact with it.
For example by removing things to focus on the essential.

When I stumble upon some random blog post or random specification
in the Haskell community, I had too much a feeling of old fashioned design.

If you look at node.js community lot of their web page look cleaner,
easier to read and in the end, more user friendly.

Haskell is very different from node, I wouldn't like to replace all
long and precise documentation with short human unprecise concepts.
I don't want to transform scientific papers by tweets.

But like the scientific community has upgraded with the use of LaTeX,
I believe we could find something similar that would make, very clean
environment for most of us. A kind of look and feel that will be

- modern
- device friendly (either on computer, mobile, tablet)
- efficient, focus on what is most important and is helpful
