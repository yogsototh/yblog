---
kind: article
published: 2015-03-27
image: /Scratch/img/blog/Absorb-Github-Events/main.png
title: Absorb Github Events
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---

blogimage("main.png","Main image")

<div class="intro">


%tlal

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
But in general you'll want to optimize things to be able to absorb a full firehose of events.

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

So, now the choice of the weapon we'll use to handle that.
It is 2015 and it is out of question to use low-level and/or error prone technology.
But speed will still be an important factor.

If like me you don't consider Java as a high-level programming language.
And if you look at benchmarks, you'll see that Python is slow.
Then the choice belong mostly in functional programming languages:

- javascript
- Clojure
- Haskell
- Common Lisp
- OCaml

While javascript is by far the most popular choice, it is also
the worst of the list in term of language quality.

As the choice is mine, I'll then choose Haskell.
Haskell provide a really great control,
a lot of error will be discarded naturally by the language properties.
Concurrent and parallel programming will be _very_ easy to achieve.

The next article will certainly use clojure.

## Initialize your environment.

If you are on Mac or on Ubuntu you should install Haskell with
this script:

~~~
TODO
~~~

Then create a simple new project:

~~~
cabal new muraine
~~~

Or if you don't want to type things, just clone my repository.
There will be a branch for each step.

~~~
TODO: git clone ...
TODO: git checkout step1
...
~~~

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
Don't be afraid by the syntax.
It might be ugly, but it does a great job at keeping concerns separated.

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

From the github specification we should follow the %http header `Link`.
But in reality concerning all events, we should only rely on the `page` param.

So for now I won't parse the header.

Notice, parsing in Haskell is really great, so I may add a section in the end.


## Refactoring (one step back)

Now our code start to have function with more than 30 lines.
Which is really a _lot_ in Haskell.

Furthermore the way github provide its events is really awful when you
want to get them all.
Compare this to the twitter and Facebook method.

Each time we will read one page, we should take care not to add an already present event.

So we should remember the first and last id of the preceding page.
More than that we should also remember the very first id of the page 1.

~~~ {.haskell}
Data CallInfo = CallInfo { firstIdPageOne :: Maybe Text
                         , searchFirstId :: Maybe Text
                         , precedingPageFirstId :: Maybe Text
                         , precedingPageLastId :: Maybe Text
                         , currentPage :: Int
                         }
~~~

So the algorithm to read become error prone very fast.
I am not confident enough to use the date on the events as I am not sure
the are aggregated in the same time.

So we are contrived to believe there is less than 30 events between two call.

Hypothesis: between two call less than one page of events (30) occurs

Even with such an hypothesis the code is complex and bug prone.

~~~ {.haskell}
data CallInfo = CallInfo { _user            :: String             -- ^ Github username
                         , _pass            :: String             -- ^ Github password
                         , _etag            :: Maybe B.ByteString -- ^ ETag
                         , _timeToWait      :: Int                -- ^ Time to wait in micro seconds
                         , _firstId         :: Maybe Text         -- ^ First Event Id
                         , _lastId          :: Maybe Text         -- ^ Last  Event Id
                         , _searchedFirstId :: Maybe Text         -- ^ First Event Id searched
                         , _page            :: Int                -- ^ Page
                         } deriving (Show)

-- | given an HTTP response compute the next CallInfo and also publish events
-- Beware, this is an heuristic with the hypothesis that there is no more than
-- one page to download, by one page.
getCallInfoFromResponse :: CallInfo -> Response LZ.ByteString -> Double -> IO CallInfo
getCallInfoFromResponse callInfo response req_time = do
    print callInfo
    if statusIsSuccessful (responseStatus response)
        then do
            let headers = responseHeaders response
            t <- getTimeToWaitFromHeaders headers
            let
              -- Time to wait is time between two HTTP call minus the time
              -- the last HTTP call took to answer
              timeToWaitIn_us = max 0 (t - floor (1000000 * req_time))
              events = decode (responseBody response)
              nextFirstId = if _page callInfo == 1 || isNothing (_firstId callInfo)
                              then getFirstId events
                              else _firstId callInfo
              nextLastId = getLastId events
              containsSearchedFirstId = containsId (_searchedFirstId callInfo) events
              etagResponded = lookup "ETag" headers
              -- Read next pages until we reach the old first ID of the first page
              -- of the preceeding loop
              -- return a new page if the first ID wasn't found
              nextPage = if containsSearchedFirstId || (_page callInfo >= 10)
                          then 1
                          else _page callInfo + 1
              nextSearchedFirstId = if containsSearchedFirstId || (_page callInfo >= 10)
                                      then nextFirstId
                                      else _searchedFirstId callInfo
            publish events (_searchedFirstId callInfo) (_lastId callInfo)
            return (callInfo { _firstId = nextFirstId
                             , _lastId = nextLastId
                             , _page = nextPage
                             , _etag = etagResponded
                             , _timeToWait = timeToWaitIn_us
                             , _searchedFirstId = nextSearchedFirstId
                             })
        else
            putStrLn (if notModified304 == responseStatus response
                        then "Nothing changed"
                        else "Something went wrong")
            return callInfo
~~~

## Rant

So this code is working most of the time.
And if we get more than 30 events between two call, the only issue is that
we lose some events.
From experience, having more than 30 events between two call is quite a rare event.

But seriously. The github API is a pain to get all events.

> It is nearly impossible using only github event _ids_ to get all events for sure.

Even if they don't want to provide a streaming or a push API,
they could make a system using the ETAG or another system
to be sure we don't miss any events nor we don't publish an event twice in the system.

This system is completely impossible to scale.
Seriously, how could you do something serious with it?
What if the number of event double for example?

So please, github. Could you provide a better way to handle your _firehose_?


Another problem.

When twitter send a retweet, we get all necessary information to display and analyze this retweet.
It contains not only the information for the retweet but also the complete original tweet.

Here, we can't get very useful information for example.
If we want to analyze the languages on github in real time.
Our only witness is contained in the extensions in the files commited.
But for our great despair, github events don't contain them.

To get them you have to make another call to the API on the commit id itself.
Which is a shame, as we only have 5000 event per hour at our disposal.
And if we make less than 1 call by second we enter in the case
where more than 30 events will occurs between two calls.


## Exporting data to something useful

Part of the problem is to think as if everything was stream.

Functional programming fits this spirit.
So for all the system to work, everything should fit the "streaming" convention.

In particular, we should create a stream of events that will be used later.

So there are a lot of pub/sub system.
At work I use kafka.
It is great, but for this article I'll use [NATS](http://nats.io).
The first reason is that I am curious.
Another reason is to make my code more portable.
The Haskell's kafka libraries are binded to a `C` one.
On the other hand, the `nats-queue` package doesn't have such an external dependency.
Furthermore, NATS code wasn't modified for about one year.
Which can mean two things.

1. Nobody care about it anymore,
2. The code is really stable.

In the hope for the second option, I'm eager to give it a try.


So a queue system is quite simple.
Many producers write in a queue.
Many consumer read the queue.
Each message should be consumed only once between all consumer.

Don't worry, if you love Kafka, a future article will talk about it.

