--- kind: article published: 2015-03-27 image:
/Scratch/img/blog/Absorb-Github-Events/main.png en: title: Absorb
Github Events fr: title: Absorb Github Events author: Yann Esposito
authoruri: yannesposito.com tags: programming theme: scientific ---
blogimage("main.png","Main image")

<div class="intro">

en: %tldr How to absorb octopussies events in real time.

fr: %tlal

This article should be the first of a series of articles.
The goal of this series of articles is to display event in real time.
For this you'll need many different parts:

- You should first absorbs the events
- then you might need to enrich them,
- afterward, you'll need to aggregate,
- you also need to give a way to question the aggregates via an API,
- and finally you'll need to provide a nice dashboard using the API

So let's start by a way to get all github events in realtime.

For this series we'll use the events provided by [github][github].
The Eel ("Muraine" in French) is one of the few octopussy predator.
Which is the github mascot.

</div>

[github]: http://github.com

## The concepts

Actually all events are accessible via the <https://api.github.com/events> %url.
During standard time and if you are logged you can call this URL about 5000 times per hour.
Which is more than one times by second.

During some events, github might lower this max number of calls.
So this information is provided in the header of the HTTP response.

Mainly the algorithm will be:

Forever:
1. call `/events`
2. analyze the three Headers:
   How many call can we use?
   At which time the number of calls reset?
   Compute how much time to wait before our next call.
3. Analyze the data and put them somewhere safe where they'll could be processed next.

Apparently it seems easy.
Let's try it.

One of the goal of this series being not only to handle events in real time
but to be able to handle a tremendous number of events in real time.
The actual amount of data provided by github is quite reasonable.
But in general you'll want to optimize things to be able to absorb a full firehose of informations.

For example, twitter can provide more than 20000 events per seconds.
The twitter firehose forces you to use a single entry point.
More than that, you have to deal with a single core to download the data and parse them.
To be able to absorb such amount of data you generally don't parse completely the data.

Another provider is facebook.
Their approach is much nicer.
You declare an %https entry point and they send you the data by making `POST` calls.
It is then much easier to dispatch the packet between multiple host using `haproxy` for example.

Concerning github, I am not fond of their method to retrieve their data.
It is kind of the worse of facebook and twitter.
You have to ask yourself for data.
But if you want to receive more data you'll have to find a way to synchronize the ETAG.

TODO: explain why Haskell, why conduit and apparently difficult path.

## Initialize your environment.

...


### Now start working

The first thing to do is to add all the needed dependencies.
In a first time, we'll only need to make HTTP requests and work with the responses.

Update your `muraine.cabal` file as follow:

~~~
  ...
  build-depends:       base >=4.7 && <4.8{-hi-},{-/hi-}
                       {-hi-}http-conduit,{-/hi-}
                       {-hi-}bytestring{-/hi-}
  ...
~~~

Then edit the `Main.hs` file such that it contains this:

~~~ {.haskell}
module Main where

import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    response <- simpleHttp "http://www.google.com"
    L.putStrLn response
~~~

And now to run this code simply launch this command:

~~~
cabal run
~~~

Now you should receive some `HTML`.
It is now time to receive some github events.
Simply udpate the %url:

~~~ {.haskell}
module Main where

import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    response <- simpleHttp "https://api.github.com/events"
    L.putStrLn response
~~~

Unfortunately you should receive an error
(line wrapping added for readability):

~~~
StatusCodeException (Status {statusCode = 403, statusMessage = "Forbidden"}) [
("Cache-Control","no-cache"),("Connection","close"),("Content-Type","text/html
"),("X-Response-Body-Start","Request forbidden by administrative rules. Please
make sure your request has a User-Agent header (http://developer.github.com/v3
/#user-agent-required). Check https://developer.github.com for other possible 
causes."),("X-Request-URL","GET https://api.github.com:443/events")] (CJ {expo
se = []})
~~~

It is a bit hard to read, but in the mess you can read:

> Request forbidden by administrative rules. Please make sure your request has a User-Agent header (http://developer.github.com/v3/#user-agent-required). Check https://developer.github.com for other possible causes.


Damn, github want us to add a `User-Agent` header.
But the library we use doesn't add one by default.
We should add it manually.

~~~
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LZ

simpleHTTPWithUserAgent :: String -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
    withManager $ \manager -> httpLbs request manager

main :: IO ()
main = do
    response <- simpleHTTPWithUserAgent "https://api.github.com/events"
    LZ.putStrLn (responseBody response)
~~~

Now, that is better.
But, to start really working with the github API,
we need to manage the headers.

Here is how we display them:

~~~
import qualified Data.ByteString.Char8 as B -- strict bytestrings
import Data.Monoid ((<>)) -- for concatenating bytestrings

...

{-hi-}showHeader :: Header -> IO (){-/hi-}
{-hi-}showHeader (name, value) =
  B.putStrLn (original name <> ": " <> value){-/hi-}

main :: IO ()
main = do
    response <- simpleHTTPWithUserAgent "https://api.github.com/events"
    LZ.putStrLn (responseBody response)
    {-hi-}mapM_ showHeader (responseHeaders response){-/hi-}
~~~

The `responseHeaders response` will return a list of `Header` and
a `Header` is a couple `(header_name,value)`.

So `mapM_` will execute a function over a list, here the list of `Headers`.
For each header we execute `showHeader`.

`showHeader` take a `Header` and print it on screen.

There should be three headers beginning by `X-RateLimit`.
The number of authorized access should be quite low:

~~~
X-RateLimit-Limit: 60
X-RateLimit-Remaining: 57
X-RateLimit-Reset: 1427738616
~~~

So you have the right to connect to the github API only 60 times by hour.
It will be far from real time high volume events.
Fortunately, the limit can be greatly improved by being connected.
So you can provide a username and password.

~~~
...
{-hi-}import System.Environment (getArgs){-/hi-}
{-hi-}import System.Exit (exitFailure){-/hi-}
...


simpleHTTPWithUserAgent :: String {-hi-}-> String -> String{-/hi-} -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url {-hi-}user pass{-/hi-} = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
        {-hi-}requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) request{-/hi-}
    withManager $ \manager -> httpLbs {-hi-}requestWithAuth{-/hi-} manager


showHelpAndExit :: IO ()
showHelpAndExit = do
    putStrLn "provide your github username and password please"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
         [user,pass] -> continueWithUserAndPass user pass
         _ -> showHelpAndExit

continueWithUserAndPass :: String -> String -> IO ()
continueWithUserAndPass user pass = do
    response <- simpleHTTPWithUserAgent "https://api.github.com/events" user pass
    LZ.putStrLn (responseBody response)
    mapM_ showHeader (responseHeaders response)
~~~

Now if you run:

~~~
cabal run [nickname] [password]
~~~

Your `X-RateLimit-Limit` should have raised to `5000`!
Note it is not over `9000` thought.
