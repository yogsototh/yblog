---
kind:           article
published:      2014-07-26
image: /Scratch/img/blog/Another-Yesod-Tutorial/main.png
en: title: Another Yesod Tutorial
fr: title: Another Yesod Tutorial
author: Yann Esposito
authoruri: yannesposito.com
tags: programming, haskell, yesod
theme: modern
---
blogimage("main.png","Main image")

<div class="intro">

en: %tldr Another Yesod Tutorial. I made a tutorial on Yesod some time ago.
While I believe it is still working I believe it is time to make another version.

fr: %tlal

</div>

## Why Yesod?

1. Secure by default
2. Fast
3. Batteries included

### Secure by default

<a href="http://xkcd.com/327/"><img src="http://imgs.xkcd.com/comics/exploits_of_a_mom.png" alt="XKCD - Exploits of a Mom"></img></a>

You can't make a security error by default.
If you want to make something insecure you have to tell it.
This reverse the order from most other languages/framework.

### Fast

Haskell is a compiled language.
See benchmarks.

### Batteries included

With yesod you will have for free:

- Many logging systems and user identifications (Persona, OpenId, Facebook Login, etc...)
- Form handling
- Safe HTML/CSS/JS templating
- Persistent Library
- Access Restrictions
- Multiple Format Handling (HTML/JSON/XML...)
- ...

When you start a yesod project using the scaffolding site you start with
a lot of common features.

## Install

1. Install Haskell (I recommend downloading and compiling GHC 7.8)
2. Install cabal
3. Use stackage (http://www.stackage.org/)
4. Install yesod binary
5. Init a yesod project

## Concepts

- Routes
- Handlers
- Formats
- Forms
