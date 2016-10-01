---
kind:           article
published:      2016-10-01
image: /content/Scratch/img/blog/Helping-avoid-Haskell-Success/main.png
title: Tips in avoiding Haskell Success at all cost
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">

Few days ago there were about 20 job offer for Haskell.
In only one day!
How is that possible?
As a **real** haskeller, I find this situation unbearable!

After all, we must *avoid success at all cost*.
And I'll help SPJ achieve this goal.

</div>

## Prevent Interrest from beginner

Imagine a situation were you see people demonstrating some interrest in learning
Haskell.

Quick! Prevent them from going further.

If they come from the dynamic (uni-typed) languages like Python, Javascript...:

> Haskell? A statically typed language??? Hmm... You mean like C and Java?

Such a remark should immediately shut down any interrest in Haskell.

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
even less Generics and all recent progress in more advanced typing system.

## Make it difficult to install

Many hints there:

- Send them on another compiler than GHC
- Explain that they should never use binary distribution of GHC! And they must
  compile it manually! It might not stop them but it will make the installation
  process much more difficult.
- Lie! Explain there is a severe security issue with latest tools. Explain they
  must use cabal-install 1.18 or older.
- Also explain them that in order to be able to handle lib dependencies
  correctly they **MUST** first learn Nix! Never talk about `stack`, `cabal
  freeze`, ... While Nix is great, forcing new user completely alien to all
  these concepts to first learn it before starting to write their first line of
  code can greatly reduce their enthusiasm.

## Make it difficult to learn

### Make new comers feel dumb

The very first thing to do is to explain how Haskell is so easy to learn. How
natural it is for everybody you know. And except someone you always considered
very dumb, everybody was very productive in haskell in few hours.

Use vocabulary alien to them as much as possible. Here is a list of words you
must use in the very first minutes of your description of Haskell:

- catamorphism (bonus if you mention that the word come from the Greek κατα for
  catastrophe, that way you'll look like a snob and you also use the word
  catastrophe in a Haskell context).
- Monad! Of course you should use it ASAP! And explain they are exactly like
  potatoes or bananas shaped chairs. Double bonus if you explain that monad are
  really simple as they are just a monoid in the category of endofunctors.

### Tutorial authors

Please first provide the most theoretical and unpraticable example at first.
For example don't start by:

~~~~.haskell
main = putStrLn "Hello World!"
~~~~

But start by the Haskell servant example and compare it to the Node.js hello
world example!

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

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool
~~~~

Of course use the most of your time explaining the language extensions first.
With great deal of details and if possible using as much as possible references
to Category Theory. And you'll get bonus points if you mention HoTT! Double
bonus points if you explain that understand HoTT in details is essential in
using Haskell correctly.

Explain that what this does is incredible but for the wrong reasons.
For example don't mention why `instance ToJSON Item` is great.
But explain that you can with as few code as this be able to sum two numbers!

### Library authors

1. Do your best not to respect versionning number policy to maximize the
   probability to break things.
2. Don't write any documentation, type are enough!
3. Even better, add mistakes to your documentation
4. Each time you can use a meaningful notation, make it wrong. For example, if
   you have a symmetric relation use an asymetric symbol to represent it.
5. If possible remove all function names and only use symbols of at least 5
   letters: For example you can replace your function 
   `log :: Level -> String -> IO ()` by `(<=.=$$.)`.
   
If the the trends continue toward growth, then we might need to go further at
the risk of breaking our own ecosystem:

- Split your libs as much as possible. The best would be to use one lib by
  symbol
- Use `unsafePerformIO` as much as possible
- Push to hackage a version not accessible on your public repository
- modify the package on hackage using the same version but with incompatible API
- Add memory leaks
- Add bugs
- Add back doors and publish how to use them

Yes we said, *at all cost*!

## Conclusion & Mistake

So with all of this, I believe we'll all be on the right track to avoid success
at all cost!

Sorry? What?

Oh... Apparenlty I made a precedence mistake!

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
