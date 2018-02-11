---
kind:           article
published:      2016-10-01
image: /Scratch/img/blog/Helping-avoid-Haskell-Success/main.jpg
title: Tips in avoiding Haskell Success at all cost
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: antidesign
---
blogimage("main.jpg","Main image")

<div class="intro">

en: %tldr
Few days ago there were about 20 job offer for Haskell.
In only one day!
How is that possible?
As a **real** haskeller, I find this situation unbearable!

After all, we must *avoid success at all cost*.
And I'll help SPJ achieve this honorable goal.

</div>

## Prevent Interest from beginner

Imagine a situation were you see people demonstrating some interest in learning
Haskell.

Quick! Prevent them from going further.

If they come from the dynamic (uni-typed) languages like Python, Javascript...:

> Haskell? A statically typed language??? Hmm... You mean like C and Java?

Such a remark should immediately shut down any interest in Haskell.

If they want to produce application with them:

> Haskell? Isn't it only a language for student! I don't think it is useful for
> *REAL WORLD* applications!

If they just want to learn something new:

> Haskell? Ah yes, I remember, mostly they only have the equivalent of Java
> interfaces and they stopped there. They don't even have classes!!!! Can you
> imagine? I don't even speak about class inheritance.
> 
> We're in 2016! And they don't even support basic Object Oriented Programming.
> What a joke!

If they love low level programming:

> Haskell? Ah yes, I heard that lazyness make it impossible to think about code
> complexity and generally cause lot of space leaks.

And if it is not enough:

> Haskell? Ah yes. I'm not fan of their Stop the World GC.

If they come from LISP and the statically typed language remark wasn't enough.
Try to mention the lack of macros in Haskell. Don't mention template Haskell or
even less Generics and all recent progress in GHC.

## Make it difficult to install

Many hints there:

- Send them on another compiler than GHC
- Explain that they should never use a binary distribution of GHC! And they must
  compile it manually! It might not stop them but it will make the installation
  process much more tedious.
- Lie! Explain there is a severe security issue with latest tools. Explain they
  must use cabal-install 1.18 or older.
- Also explain them that in order to be able to handle lib dependencies
  correctly they **MUST** first learn Nix! Never talk about `stack`, `cabal
  freeze`, ... While Nix is great, forcing new user completely alien to all
  these concepts to first learn it before starting to write their first line of
  code can greatly reduce their enthusiasm. Bonus point if you make them believe
  you can only program in Haskell on NixOS.

## Make it difficult to learn

### Make new comers feel dumb

The very first thing to do is to explain how Haskell is so easy to learn. How
natural it is for everybody you know. And except someone you always considered
very dumb, everybody was very productive in Haskell in few hours.

Use vocabulary alien to them as much as possible. Here is a list of terms you
should use in the very first minutes of your description of Haskell:

- catamorphism (bonus if you mention that the word come from the Greek κατα for
  catastrophe, that way you'll look like a snob and you also use the word
  catastrophe in a Haskell context).
- Monad! Of course you should use it ASAP! And explain they are exactly like
  potatoes or bananas shaped chairs. Double bonus if you explain that monad are
  really simple as they are just a monoid in the category of endofunctors.
- GADTs
- Yoneda Lemma
- Homotopy Type Theory
- ...

Each of this term will hopefully be intimidating.

### Tutorial authors

Please don't provide an obvious first example like:

~~~~.haskell
main = putStrLn "Hello World!"
~~~~

Instead prefer a fully servant example:

~~~~.haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  getItems :<|>
  getItemById

type Handler = ExceptT ServantErr IO

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwE err404

exampleItem :: Item
exampleItem = Item 0 "example item"

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
~~~~

This nice example should overflow the number of new concepts a Haskell newcomer
should deal with:

- Language extensions. Each extension can take a lot of time to be explained.
- Strange notations:
    - `:<|>`
    - `'[]` instead of `[]`
- `Proxy`
- Immediate usage of `$`
- `deriving` ha ha! You'll need to explain typeclasses first!
- the definition for `getItemById`

Of course use the most of your energy explaining the language extensions first.
Use a great deal of details and if possible use as much as possible references
to Category Theory. You'll get bonus points if you mention HoTT! Double bonus
points if you explain that understanding all details in HoTT is essential to use
Haskell on a daily basis.

Explain that what this does is incredible but for the wrong reasons. For example
don't mention why `instance ToJSON Item` is great. But insist that we achieved
to serve a JSON with extreme elegance and simplicity. Keep insisting on the
simplicity and forgot to mention type safety which is one of the main benefit of
Servant.

If you're afraid that this example might be too close to a real world product,
you can simply use some advanced lenses examples:

~~~~.haskell
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens.TH (makePrisms)
import GHC.Generics (Generic)
import Lens.Family.Total

data Example a b c = C1 a | C2 b | C3 c deriving (Generic)

makePrisms ''Example

instance (Empty a, Empty b, Empty c) => Empty (Example a b c)

example :: Example String Char Int -> String
example = _case
    & on _C1 (\s -> s              )
    & on _C2 (\c -> replicate 3  c )
    & on _C3 (\n -> replicate n '!')
~~~~

Certainly a great example to start a new language with.

### Library authors

1. Do your best not to respect versioning number policy to maximize the
   probability to break things.
2. Don't write any documentation, type are enough!
3. Even better, add mistakes to your documentation
4. Each time you can use a meaningful notation, make it wrong. For example, if
   you have a symmetric relation use an asymmetric symbol to represent it.
5. If possible remove all function names and only use symbols of at least 5
   letters: For example you can replace your function 
   `log :: Level -> String -> IO ()` by `(<=.=$$.)`.
   
If the the trends continue toward growth, then we might need to go further at
the risk of breaking our own ecosystem:

- Split your libs as much as possible. The best would be to use one lib by
  symbol
- Use `unsafePerformIO` as much as possible
- Push to Hackage a version not accessible on your public repository
- modify the package on Hackage using the same version but with incompatible API
- Add memory leaks
- Add bugs
- Add back doors and publish how to use them

Yes we said, *at all cost*!

## Conclusion & Mistake

So with all of this I believe we should be on the right track to avoid success
at all cost!

Sorry? What?

Oh... Apparently I made a precedence mistake!

SPJ didn't asked to `avoid success $ at all cost` but
to `avoid $ success at all cost`[^1].

[^1]: A good point to use more LISP syntax.

Sorry! My bad!
Forget about all of this.
Keep the good work everybody!
Haskell is certainly one of the most awesome language in the world!
Its community is also just great.

I'm really happy to see it growth every year. Thanks to all contributors making
it possible to still have a lot of fun after many years using Haskell!

And the fact that in Haskell the *right* choice is preferred to the easiest
choice, certainly helped.
