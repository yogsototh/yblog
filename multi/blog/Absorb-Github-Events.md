---
kind: article
published: 2015-03-27
image: /Scratch/img/blog/Absorb-Github-Events/main.png
en: title: Absorb Github Events
fr: title: Absorb Github Events
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---

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
- you also need to give a way to question the aggregates via an %api,
- and finally you'll need to provide a nice dashboard using the %api

So let's start by a way to get all github events in realtime.

For this series we'll use the events provided by [github][github].
The Eel ("Muraine" in French) is one of the few octopussy predator.
Which is the github mascot.

</div>

[github]: http://github.com

## The concepts

Actually all events are accessible via the [`https://api.github.com/events`](https://api.github.com/events) %url.
During standard time and if you are logged you can call this %url about 5000 times per hour.
Which is more than one times by second.

During some events, github might lower this max number of calls.
So this information is provided in the header of the %http response.

Mainly the algorithm will be:

> Forever:
> 
> 1. call `/events`
> 2. analyze the three Headers:
>    How many call can we use?
>    At which time the number of calls reset?
>    Compute how much time to wait before our next call.
> 3. Analyze the data and put them somewhere safe where they'll could be processed next.

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


## First step: %http requests

The first thing to do is to add all the needed dependencies.
In a first time, we'll only need to make %http requests and work with the responses.

Update your `muraine.cabal` file as follow:

~~~
  ...
  build-depends:       base >=4.7 && <4.8{-hi-},{-/hi-}
                       {-hi-}http-conduit,{-/hi-}
                       {-hi-}bytestring{-/hi-}
  ...
~~~

Then edit the `Main.hs` file such that it contains this:

> ☞ Haskell can be quite a strange programming language.
> It is _very_ explicit about a _lot_ of things.
> So yes.
> One of the cost to pay to have better confidence
> in the correctness of your program is to be slightly more verbose
> than some other very terse language
> (Haskell is quite very good at terseness thought)
> and be used to some strange magic notations and sometimes
> strange %api.
>
> If you are not familiar with Haskell.
> Try not be focused on details and just try to read the flow.
> Once you have to do things yourself, the Haskell compiler
> will be here to help you in your journey.
>
> The worst case scenario with Haskell is discovering
> a new library lacking some documentation and
> playing the _type tetris_ game.

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

Now you should receive some %html.
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
StatusCodeException (Status {statusCode = 403, statusMessage
 = "Forbidden"}) [("Cache-Control","no-cache"),("Connection"
,"close"),("Content-Type","text/html"),("X-Response-Body-Sta
rt","Request forbidden by administrative rules. Please make 
sure your request has a User-Agent header (http://developer.
github.com/v3/#user-agent-required). Check https://developer
.github.com for other possible causes."),("X-Request-URL","G
ET https://api.github.com:443/events")] (CJ {expose = []})
~~~

It is a bit hard to read, but in the mess you can read:


~~~
Request forbidden by administrative rules.
Please make sure your request has a User-Agent header
(http://developer.github.com/v3/#user-agent-required).
Check https://developer.github.com for other possible causes.
~~~


Damn, github want us to add a `User-Agent` header.
But the library we use doesn't add one by default.
We should add it manually.

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LZ

simpleHTTPWithUserAgent :: String -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
    withManager (httpLbs request)

main :: IO ()
main = do
    response <- simpleHTTPWithUserAgent "https://api.github.com/events"
    LZ.putStrLn (responseBody response)
~~~

Now, that is better.
But, to start really working with the github %api,
we need to manage the headers.

Here is how we display them:

~~~ {.haskell}
import qualified Data.ByteString.Char8 as B -- strict bytestrings
import Data.Monoid ((<>)) -- for concatenating bytestrings

...

{-hi-}showHeader :: Header -> IO (){-/hi-}
{-hi-}showHeader (name, value) ={-/hi-}
  {-hi-}B.putStrLn (original name <> ": " <> value){-/hi-}

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

So you have the right to connect to the github %api only 60 times by hour.
It will be far from real time high volume events.
Fortunately, the limit can be greatly improved by being connected.
So you can provide a username and password.

~~~ {.haskell}
...
{-hi-}import System.Environment (getArgs){-/hi-}
{-hi-}import System.Exit (exitFailure){-/hi-}
...


simpleHTTPWithUserAgent :: String {-hi-}-> String -> String{-/hi-}
                           -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url {-hi-}user pass{-/hi-} = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
        {-hi-}requestWithAuth = applyBasicAuth (B.pack user){-/hi-}
                                         {-hi-}(B.pack pass){-/hi-}
                                         {-hi-}request{-/hi-}
    withManager (httpLbs {-hi-}requestWithAuth{-/hi-})


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

## Waiting the right amount of time

To optimize the number of call we can make to the githup %api,
we'll have to manage %http headers.
What are the useful headers:

~~~
X-RateLimit-Remaining: 4999
X-RateLimit-Reset: 1428149482
Date: Sat, 04 Apr 2015 11:14:38 GMT
ETag: 05502a61685deb37109b648ea2107135
~~~

So, to optimize the number of call we can make we should use
the following number:

~~~
time to wait before next call in seconds:
    t = (X-RateLimit-Reset - Date in epoch) / X-RateLimit-Remaining
    max (t - time taken by last request, 0)
~~~

If we could make request instantaneously we should wait only for `t`.
But as our request has taken time, it means we already waited this time.
So if the request was longer than the time to wait,
we should make the request as soon as possible.

So most of informations we need are provided by the %http headers.
We also need to measure how much time our request took.

### Date & Time are Programmers Hell

To make things harder to use github maliciously used
different format for the header `Date` and `X-RateLimit-Reset`.

But this shouldn't be a problem.
We should use the [time](https://hackage.haskell.org/package/time) package.
And here we enter in the great world of date type translations.
From string or int to Time, etc...

    ...
    import Data.Time.Format (readTime)
    import Data.Time.Clock.POSIX (getPOSIXTime,utcTimeToPOSIXSeconds)
    ...
    rfc822DateFormat :: String
    rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"
    
    epochFromString :: String -> Int
    epochFromString = floor . utcTimeToPOSIXSeconds . readTime defaultTimeLocale rfc822DateFormat

Now we can pass from a String representation to an epoch one.
We also need to measure how much time our requests took.

~~~ {.haskell}
import System.CPUTime (getCPUTime)
...
time :: IO a -> IO (Double, a)
time action = do
    startTime <- getCPUTime
    res <- action
    endTime <- getCPUTime
    return (fromIntegral (endTime - startTime)/1000000000,res)
...
getEvents user pass = do
    (req_time, response) <- time (simpleHTTPWithUserAgent "https://api.github.com/events" user pass)
    ...
~~~

Another important aspect is to pass the ETag each of each preceding request.
For this it is time to refactor our code a bit to make it more readable.

~~~ {.haskell}
authHttpCall :: String -- ^ URL
                -> String -- ^ User
                -> String -- ^ Password
                -> RequestHeaders -- ^ Headers
                -> IO (Response LZ.ByteString)
authHttpCall url user pass headers = do
    r <- parseUrl url
    let request = r {requestHeaders = headers }
        requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) request
    withManager (httpLbs requestWithAuth)

httpGHEvents :: String -- ^ User
                -> String -- ^ Password
                -> Maybe B.ByteString -- ^ ETag if one
                -> IO (Response LZ.ByteString)
httpGHEvents user pass etag =
    authHttpCall  "https://api.github.com/events" user pass headers
    where
        headers = ("User-Agent","HTTP-Conduit"):
            maybe [] (\e -> [("If-None-Match",B.tail (B.tail e))]) etag
~~~

And then

~~~ {.haskell}
getEvents :: String             -- ^ Github username
          -> String             -- ^ Github password
          -> Maybe B.ByteString -- ^ ETag
          -> IO ()
getEvents user pass etag = do
    -- Call /events on github
    (req_time, response) <- time (httpGHEvents user pass etag)
    if statusIsSuccessful (responseStatus response)
        then do
            let headers = responseHeaders response
            -- If the server returned a date we use it
            -- otherwise we use the local current time
            serverDateEpoch <- case lookup "Date" headers of
                                Nothing -> fmap round getPOSIXTime
                                Just d -> return (epochFromString (B.unpack d))
            let etagResponded = lookup "ETag" headers
                remainingHeader = lookup "X-RateLimit-Remaining" headers
                remaining = maybe 1 (read . B.unpack) remainingHeader
                resetHeader = lookup "X-RateLimit-Reset" headers
                reset = maybe 1 (read . B.unpack) resetHeader
                timeBeforeReset = reset - serverDateEpoch
                t = 1000000 * timeBeforeReset `div` remaining
                timeToWaitIn_us = max 0 (t - floor (1000000 * req_time))
            publish (responseBody response)
            threadDelay timeToWaitIn_us
            getEvents user pass etagResponded
        else do
            putStrLn "Something went wrong"
            threadDelay 100000 -- 100ms
            getEvents user pass etag
~~~

### Pagination

parse the `Link` header:

~~~
<https://api.github.com/events?page=9>; rel="next",
<https://api.github.com/events?page=10>; rel="last",
<https://api.github.com/events?page=1>; rel="first",
<https://api.github.com/events?page=7>; rel="prev"
~~~

So we should split with `,` then with `;`
and take what is in between `<` and `>`.
