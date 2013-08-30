---
kind:           article
published:      2013-08-06
image: /Scratch/img/blog/Rational-Web-Framework-Choice/battle-of-lepanto-vicentino-andrea.jpg
title: Rational Web Framework Choice
author: Yann Esposito
authoruri: yannesposito.com
tags: programming, framework, web
theme: scientific
---
blogimage("battle-of-lepanto-vicentino-andrea.jpg","Main image")

<div class="intro">

%tldr Determine with the most objectivity possible the best(s) web framework(s) depending on your needs.
[Click here to jump to the result](#the-result).


</div>

This is it.  
You've got the next big idea.  
You just need to make a very simple web application.

It sounds easy!
You just need to choose a good modern web framework, when suddenly:

blogfigure("choice_paralysis.gif","[Choice Paralysis][choice_paralysis]")

[choice_paralysis]: https://en.wikipedia.org/wiki/Analysis_paralysis

After your brain stack overflowed, you decide to use a very simple methodology.
Answer two questions:

**Which language am I familiar with?  
What is the most popular web framework for this language?**

Great! This is it.

But, you continually hear this little voice.

> **"You didn't made a bad choice, yes. But ...  
> you hadn't made the best either."**

This article try to determine the most objective and rational way
the best(s) web framework(s) depending on your needs.

I will use the following methodology:

> ### Methodology
> 
> 1. Model how to make choice 
>     a. choose important parameters
>     b. organize (hierarchize) them
>     c. write down an objective chooser
> 2. Grab objective quantified informations about web frameworks relatively to choosen parameters
> 3. Sanitize your data in order to handle imprecisions, lack of informations...
> 4. Apply the model of choice to your informations

## Model

Here are the important features (parameters) I decided to use for the choice:

1. _**Expressiveness**_, which is generally correlated to:
    - faster development
    - flexibility, adaptability

2. _**Robustness**_, which correlate with:
    - security
    - fewer bugs

3. _**Efficiency**_, which is generally correlated to:
    - how much processing power you'll need per user
    - maintenance price per user
    - how long the user will wait to see/update data

4. _**Popularity**_, which correlate with:
    - number of tested libraries
    - facility to find learning material
    - ability to find another developer to work with

Each feature is quite important and mostly independant from each other.

Unfortunately, these kind of properties are very hard to measure with precision.
In fact we could only get orders of magnitude for each.
In the end, we will have _clusters_.

For each property a framework could have one of the six possible values:
Excellent, Very Good, Good, Medium, Bad or Very Bad

So how to make a decision model from these informations?

One of the most versatile method is to give a weight for each cluster value.
And to select the framework maximizing this score:

```
score(framework) = efficiency × robustness × expressiveness × popularity
```

For example:

-------------- ---- --- --- --- --- ---
Expressiveness  100  10   1   0   0   0
Popularity       32  32   8   4   2   1
Efficiency     1024 256  64  16   4   1
Robustness     1024 256  64  16   4   1
-------------- ---- --- --- --- --- ---

Using this weighted table, that means:

- we discard the three least expressive clusters.
- We don't make any difference between excellent and very good in popularity.
- Concerning efficient framework in excellent cluster will have 4x more points than very cluster.

So for each framework we compute its score relatively to a weighted table.
And we select the best(s).

> **Example**: Using this hypothetic framework and the preceeding table.
> 
>        Expressiveness Popularity Efficiency Robustness
> ------ -------------- ---------- ---------- ----------
> yog     Excellent      Very Bad     Medium   Very Good
> 
> ```
> score(yog) = 100 × 1 × 16 × 256 = 409600
> ```


Most needs should be expressed by such a weighted table.
In the result section, we will discuss this further.

It is now time to try to get these measures.

## Objective measures

None of these property can be measured with perfect precision.
But each time we should get an order of magnitude for each.
Some properties feels easier to measure than other.

For example, popularity measure can be quite correct (at least for the most popular languages).
It is more complex to measure the efficiency, expressiveness and easiness, but
we can have good enough indicators.

As each indicator lack of precision, we will focus on giving order of magnitude.

### Popularity

[RedMonk Programming Language Rankings (January 2013)][redmonk]
provide an apparent good measure of popularity.
While not perfect the current measure feel mostly right.
They create an image using stack overflow and github data.
Vertical correspond to the number of questions on stackoverflow.
Horizontal correspond to the number of projects on github.

If you look at the image, your eye can see about four clusters.
The 1ﬆ cluster correspond to mainstream languages:

blogfigure("mainstreamlanguages.png","Mainstream Languages Cluster from [RedMonk][redmonk]")

Most developer know at least one of these language.

The second cluster is quite bigger.
It seems to correspond to languages with a solid community behind them.

blogfigure("secondtierlanguages.png","Second tier languages from [RedMonk][redmonk]")

[redmonk]: http://redmonk.com/sogrady/2013/02/28/language-rankings-1-13/

I don't get into detail, but you could also see third and fourth tier popular languages.

So:

**Mainstream**:
JavaScript, Java, PHP, Python, Ruby, `C#`, `C++`, `C`, Objective-C, Perl, Shell

**Good**: Scala, Haskell, Visual Basic, Assembly, R, Matlab, ASP, ActionScript, Coffeescript,
Groovy, Clojure, Lua, Prolog

**Medium**: Erlang, Go, Delphi, D, Racket, Scheme, ColdFusion,
F#, FORTRAN, Arduino, Tcl, Ocaml

**Bad**: third tier
**Very Bad**: fourth tier

I don't thing I could find easily web frameworks for third or fourth tier languages.

For now, I only talked about language popularity.
But what about framework popularity?
I made a test using number of question on stackoverflow only.
Then by dividing by two for each 6 cluster:


Cluster      Language    Framework          #nb        %
------------ ----------- --------------- ------ --------
Excellent    Ruby        Rails           176208     100%
Very Good    Python      Django          57385   &lt;50%
             Java        Servlet         54139
             Java        Spring          31641
             Node.js     node.js         27243
             PHP         Codeigniter     21503
             Groovy      Grails          20222
Good         Ruby        Sinatra         8631    &lt;25%
             Python      Flask           7062
             PHP         Laravel         6982
             PHP         Kohana          5959
             Node.js     Express         5009
Medium       PHP         Cake            4554    &lt;13%
             C♯          ServiceStack    3838
             Scala       Play            3823
             Java        Wicket          3819
             Dart        Dart            3753
             PHP         Slim            3361
             Python      Tornado         3321
             Scala       Lift            2844
             Go          Go              2689
Bad          Java        Tapestry        1197     &lt;6%
             C♯          aspnet          1000
             Haskell     Yesod           889
             PHP         Silex           750
             PHP         Lithium         732
             C♯          nancy           705
Very bad     Java        Grizzly         622      &lt;3%
             Erlang      Cowboy          568
             Perl        Dancer          496
             PHP         Symphony2       491
             Go          Revel           459
             Clojure     Compojure       391
             Perl        Mojolicious     376
             Scala       Scalatra        349
             Scala       Finagle         336
             PHP         Phalcon         299
             js          Ringo           299
             Java        Gemini          276
             Haskell     Snap            263
             Perl        Plack           257
             Erlang      Elli            230
             Java        Dropwizard      188
             PHP         Yaf             146
             Java        Play1           133
             Node.js     Hapi            131
             Java        Vertx           60
             Scala       Unfiltered      42
             C           onion           18
             Clojure     http-kit        17
             Perl        Kelp            16
             PHP         Micromvc        13
             Lua         Openresty       8
             C++         cpoll-cppsp     5
             Clojure     Luminus         3
             PHP         Phreeze         1

As we can see, our framework popularity indicator can be quite different from its language popularity.
There might be a better system to merge language popularity with framework popularity. For this study I will just use the number of stackoverflow question for each framework. In this sense, the popularity score can clearly be improved.
But as I said earlier. The important information here is the order of magnitude.
So we don't need much precision and certainly with a better measure there won't be much jumps from one cluster to another.

### Efficiency

Another objective measure is efficiency.
We all know benchmarks are all flawed.
But they are the only indicators concerning efficiency we have.

I used the benchmark from [benchmarksgame](http://benchmarksgame.alioth.debian.org/u64q/benchmark.php?test=all&lang=all&data=u64q). Mainly, there are five clusters:

----------- ----------------------------------------------------------
1x→2x       `C`, `C++`
2x→3x       Java 7, Scala, OCamL, Haskell, Go, Common LISP
3x→10x      C♯, Clojure, Racket, Dart
10x→30x     Erlang
30x→        PHP, Python, Perl, Ruby, JRuby
----------- ----------------------------------------------------------

Remarks concerning some very slow languages:

* PHP ; huge variations, can be about 1.5x C speed in best case.
* Python ; huge variations, can be about 1.5x C speed in best case
* Perl ; Can be about 3x C speed in best case
* Ruby, JRuby ; mostly very slow.

This is a first approach.
The speed of the language for basic benchmarks.
But, here we are interrested in web programming.
Fortunately techempower has made some tests focused on most web frameworks:

[Web framework benchmarks](http://www.techempower.com/blog/2013/07/02/frameworks-round-6/).

These benchmark doesn't fit well with our needs.
The values are certainly quite imprecise to your real usage.
The goal is just to get an order of magnitude for each framework.
Another problem is the high number of informations.

As always, we should remember these informations are also imprecise.
So I simply made some classes of efficiency.

Remark: I separated the clusters by using power of 2 relatively to the fastest.

Cluster     Language    Framework       #nb   slowness
----------  ----------- ------------- ------- ----------
Excellent   C++           cpoll-cppsp 114,711       1×
            Jav                gemini 105,204
            Lua             openresty  93,882
            Jav               servlet  90,580
            C++            cpoll-pool  89,167
             Go                    go  76,024
            Sca               finagle  68,413
             Go                 revel  66,990
            Jav          rest-express  63,209
Very Good   Jav                wicket  48,772   &gt;2×
            Sca              scalatra  48,594
            Clj              http-kit  42,703
            Jav                spring  36,643   &gt;3×
            PHP                   php  36,605
            Jav              tapestry  35,032
            Clj             compojure  32,088
             JS                 ringo  31,962
            Jav            dropwizard  31,514
            Clj               luminus  30,672
Good        Sca            play-slick  29,950   &gt;4×
            Sca            unfiltered  29,782
            Erl                  elli  28,862
            Jav                 vertx  28,075
             JS                nodejs  27,598
            Erl                cowboy  24,669
              C                 onion  23,649
            Hkl                 yesod  23,304
             JS               express  22,856   &gt;5×
            Sca            play-scala  22,372
            Jav        grizzly-jersey  20,550
             Py               tornado  20,372   &gt;6×
            PHP               phalcon  18,481
            Grv                grails  18,467
            Prl                 plack  16,647   &gt;7×
            PHP                   yaf  14,388
Medium       JS                 hapi   11,235  &gt;10×
            Jav                play1    9,979
            Hkl                 snap    9,196
            Prl                 kelp    8,250
             Py                flask    8,167
            Jav            play-java    7,905
            Jav        play-java-jpa    7,846
            PHP             micromvc    7,387
            Prl               dancer    5,040  &gt;20×
            Prl          mojolicious    4,371
             JS           ringo-conv    4,249
             Py               django    4,026
            PHP          codeigniter    3,809  &gt;30×
Bad         Rby                rails    3,445
            Sca                 lift    3,311
            PHP                 slim    3,112
            PHP               kohana    2,378  &gt;40×
            PHP                silex    2,364
Very Bad    PHP              laravel    1,639  &gt;60×
            PHP              phreeze    1,410
            PHP              lithium    1,410
            PHP                 fuel    1,410
            PHP                 cake    1,287  &gt;80×
            PHP             symfony2      879  &gt;100×
             C#           aspnet-mvc      871
            Rby              sinatra      561  &gt;200×
             C#         servicestack       51
            Dar                 dart        0
             C#                nancy        0
            Prl           web-simple        0

These are manually made clusters. But you get the idea.
Certainly, some framework could jump between two different clusters.
So this is something to remember.

# Expressiveness

Now, how to objectively measure expressiveness?

RedMonk had a very good idea to find an objective (while imprecise)
measure of each language expressiveness.
Read this [article](http://redmonk.com/dberkholz/2013/03/25/programming-languages-ranked-by-expressiveness/) for details.

After filtering languages suitable for web development,
we end up with some clusters:


---------  -----------
Cluster    Languages
---------  -----------
Excellent   Coffeescript,
            Clojure,
            Haskell

Very Good   Racket,
            Groovy, R, Scala,
            OCamL, F♯, Erlang, Lisp, Go

Medium      Perl, Python, Objective-C,
            Scheme, Tcl, Ruby

Bad         Lua, Fortran (free-format)
            PHP, Java, C++, C♯

Very Bad    Assembly, C,
            Javascript,
---------  -----------

Unfortunately there is no information about dart.
So I simply give a very fast look at the syntax.
As it looked a lot like javascript and js is quite
low. I decided to put it close to java.


<div id="toggle-expressiveness-table" class="button">Click here to show/hide the table for frameworks</div>
<div id="expressiveness-table">

Cluster   Language  Framework
--------- --------- --------------------
Excellent   Clj               luminus
            Clj              http-kit
            Clj             compojure
            Hkl                  snap
            Hkl                 yesod
Very Good   Erl                  elli
            Erl                cowboy
             Go                    go
             Go                 revel
            Grv                grails
            Sca                  lift
            Sca               finagle
            Sca              scalatra
            Sca            play-scala
            Sca            play-slick
            Sca            unfiltered
Medium      Prl                  kelp
            Prl                 plack
            Prl                dancer
            Prl            web-simple
            Prl           mojolicious
             Py                 flask
             Py                django
             Py               tornado
            Rby                 rails
            Rby               sinatra
Bad          C#                 nancy
             C#            aspnet-mvc
             C#          servicestack
            C++            cpoll-pool
            C++           cpoll-cppsp
            Dar                  dart
            Jav                 play1
            Jav                 vertx
            Jav                gemini
            Jav                spring
            Jav                wicket
            Jav               servlet
            Jav              tapestry
            Jav             play-java
            Jav            dropwizard
            Jav          rest-express
            Jav         play-java-jpa
            Jav        grizzly-jersey
            Lua             openresty
            PHP                   php
            PHP                   yaf
            PHP                  cake
            PHP                  fuel
            PHP                  slim
            PHP                 silex
            PHP                kohana
            PHP               laravel
            PHP               lithium
            PHP               phalcon
            PHP               phreeze
            PHP              micromvc
            PHP              symfony2
            PHP           codeigniter
Very Bad      C                 onion
             JS                  hapi
             JS                 ringo
             JS                nodejs
             JS               express
             JS            ringo-conv

</div>

### Robustness

I couldn't find any complete study to give the number of bug relatively
to each framework/language.

But one thing I saw from experience is the more powerful the type system the
safest your application is.
While the type system doesn't remove completely the need to test your application
a very good type system tend to remove complete classes of bug.

Typically, not using pointer help to reduce the number of bugs due to bad references.
Also, using a garbage collector, reduce greatly the probability to access unallocated space.

blogfigure("languagesafety.png","Static Type Properties from [James IRY Blog][typesanalysis]")

[typesanalysis]: http://james-iry.blogspot.fr/2010/05/types-la-chart.html

From my point of view, robustness is mostly identical to safety.
I don't think the following table is much a subject of discussion.
But if you don't trust in this table, you should just ignore it.

Here are the clusters:

----------- ----------------------
Excellent   Haskell, Scheme, Erlang
Very Good   Scala, Java, Clojure
Good        Ruby, Python, Groovy, javascript, PHP
Medium      C++, C#, Perl, Objective-C, Go, C
----------- ----------------------

So applying this to frameworks gives the following clusters:

<div id="toggle-robustness-table" class="button">Click here to show/hide the table for frameworks</div>
<div id="robustness-table">

Cluster   Language  Framework
--------- --------- --------------------
Excellent   Erl                  elli
            Erl                cowboy
            Hkl                  snap
            Hkl                 yesod
Very Good   Clj               luminus
            Clj              http-kit
            Clj             compojure
            Jav                 play1
            Jav                 vertx
            Jav                gemini
            Jav                spring
            Jav                wicket
            Jav               servlet
            Jav              tapestry
            Jav             play-java
            Jav            dropwizard
            Jav          rest-express
            Jav         play-java-jpa
            Jav        grizzly-jersey
            Sca                  lift
            Sca               finagle
            Sca              scalatra
            Sca            play-scala
            Sca            play-slick
            Sca            unfiltered
Good        Grv                grails
             JS                  hapi
             JS                 ringo
             JS                nodejs
             JS               express
             JS            ringo-conv
            Lua             openresty
            PHP                   php
            PHP                   yaf
            PHP                  cake
            PHP                  fuel
            PHP                  slim
            PHP                 silex
            PHP                kohana
            PHP               laravel
            PHP               lithium
            PHP               phalcon
            PHP               phreeze
            PHP              micromvc
            PHP              symfony2
            PHP           codeigniter
             Py                 flask
             Py                django
             Py               tornado
            Rby                 rails
            Rby               sinatra
Medium        C                 onion
             C#                 nancy
             C#            aspnet-mvc
             C#          servicestack
            C++            cpoll-pool
            C++           cpoll-cppsp
            Dar                  dart
             Go                    go
             Go                 revel
            Prl                  kelp
            Prl                 plack
            Prl                dancer
            Prl            web-simple
            Prl           mojolicious

</div>

## The result

For the result I initialized the table with my own needs.

And I am quite happy it confirms that I should continue to use my
actual preferred web framework.
I sware I didn't given it any bonus point.
I tried to be the most objective and factual as possible.

Now, it is up to you to enter your preferences.

On each line you could change how important a feature is for you.
From essential to unsignificant.
Of course you could change the matrix at will.

I just show a top 10 frameworks.
In order to give a more understandable measure I provide the log of the score.

<table id="choice-matrix">
<tr>
    <th></th>
    <th>Excellent</th>
    <th>Very good</th>
    <th>Good</th>
    <th>Medium</th>
    <th>Bad</th>
    <th>Very bad</th>
    <th>Importance</th>
</tr>
<tr id="t-expressiveness"><th>Expressiveness</th></tr>
<tr id="t-popularity"><th>Popularity</th></tr>
<tr id="t-efficiency"><th>Efficiency</th></tr>
<tr id="t-robustness"><th>Robustness</th></tr>
</table>

<div id="compute" class="button">Click to force refresh</div>
<div id="result"></div>

<script>// <![CDATA[
    function lt(x,y){return (x < y);}
// ]]></script>
<script>// <![CDATA[
    String.prototype.repeat = function(num){return new Array(num+1).join(this);};
    (function(){function run(){if (window.$){

     var languageOf={};
languageOf["elli"]="Erlang";
languageOf["cowboy"]="Erlang";
languageOf["snap"]="Haskell";
languageOf["yesod"]="Haskell";
languageOf["luminus"]="Clojure";
languageOf["http-kit"]="Clojure";
languageOf["compojure"]="Clojure";
languageOf["play1"]="Java";
languageOf["vertx"]="Java";
languageOf["gemini"]="Java";
languageOf["spring"]="Java";
languageOf["wicket"]="Java";
languageOf["servlet"]="Java";
languageOf["tapestry"]="Java";
languageOf["play-java"]="Java";
languageOf["dropwizard"]="Java";
languageOf["rest-express"]="Java";
languageOf["play-java-jpa"]="Java";
languageOf["grizzly-jersey"]="Java";
languageOf["lift"]="Scala";
languageOf["finagle"]="Scala";
languageOf["scalatra"]="Scala";
languageOf["play-scala"]="Scala";
languageOf["play-slick"]="Scala";
languageOf["unfiltered"]="Scala";
languageOf["grails"]="Groovy";
languageOf["hapi"]="javascript";
languageOf["ringo"]="javascript";
languageOf["nodejs"]="javascript";
languageOf["express"]="javascript";
languageOf["ringo-conv"]="javascript";
languageOf["openresty"]="Lua";
languageOf["php"]="PHP";
languageOf["yaf"]="PHP";
languageOf["cake"]="PHP";
languageOf["fuel"]="PHP";
languageOf["slim"]="PHP";
languageOf["silex"]="PHP";
languageOf["kohana"]="PHP";
languageOf["laravel"]="PHP";
languageOf["lithium"]="PHP";
languageOf["phalcon"]="PHP";
languageOf["phreeze"]="PHP";
languageOf["micromvc"]="PHP";
languageOf["symfony2"]="PHP";
languageOf["codeigniter"]="PHP";
languageOf["flask"]="Python";
languageOf["django"]="Python";
languageOf["tornado"]="Python";
languageOf["rails"]="Ruby";
languageOf["sinatra"]="Ruby";
languageOf["onion"]="C";
languageOf["nancy"]="C#";
languageOf["aspnet-mvc"]="C#";
languageOf["servicestack"]="C#";
languageOf["cpoll-pool"]="C++";
languageOf["cpoll-cppsp"]="C++";
languageOf["dart"]="Dart";
languageOf["go"]=" Go";
languageOf["revel"]=" Go";
languageOf["kelp"]="Perl";
languageOf["plack"]="Perl";
languageOf["dancer"]="Perl";
languageOf["web-simple"]="Perl";
languageOf["mojolicious"]="Perl";


popularityClusters=[[ "rails"
],[ "django" , "servlet" , "spring" , "nodejs" , "codeigniter" , "grails"
],[ "sinatra" , "flask" , "laravel" , "kohana" , "express"
],[ "cake" , "servicestack" , "play-java", "play-slick"
, "wicket" , "dart" , "slim" , "tornado" , "lift" , "go"
],[ "tapestry" , "aspnet-mvc" , "yesod" , "silex" , "lithium" , "nancy"
],[ "grizzly" , "cowboy" , "dancer" , "symfony2" , "revel"
, "compojure" , "mojolicious" , "scalatra" , "finagle" , "phalcon"
, "ringo" , "gemini" , "snap" , "plack" , "elli" , "dropwizard"
, "yaf" , "play1" , "hapi" , "vertx" , "unfiltered" , "onion"
, "http-kit" , "kelp" , "micromvc" , "openresty" , "cpoll-pool"
,  "cpoll-cppsp" , "luminus" , "phreeze"
]];


efficiencyClusters=[[ "cpoll-cppsp" , "gemini" , "openresty" , "servlet"
, "cpoll-pool" , "go" , "finagle" , "revel" , "rest-express"
],[ "wicket" , "scalatra" , "http-kit" , "spring" , "php" , "tapestry"
, "compojure" , "ringo" , "dropwizard" , "luminus"
],[ "play-slick" , "unfiltered" , "elli" , "vertx" , "nodejs" , "cowboy"
, "onion" , "yesod" , "express" , "play-scala" , "grizzly"
, "tornado" , "phalcon" , "grails" , "plack" , "yaf"
],[ "hapi" , "play1" , "snap" , "kelp" , "flask" , "play-java"
, "play-java-jpa" , "micromvc" , "dancer" , "mojolicious" , "ringo-conv"
, "django" , "codeigniter"
],[ "rails" , "lift" , "slim" , "kohana" , "silex"
],[ "laravel" , "phreeze" , "lithium" , "fuel" , "cake" , "symfony2"
, "aspnet-mvc" , "sinatra" , "servicestack" , "dart" , "nancy" , "web-simple"
]];


expressivenessClusters=[[
"luminus" , "http-kit" , "compojure" , "snap" , "yesod"
],[ "elli" , "cowboy" , "go" , "revel" , "grails" , "lift" , "finagle"
, "scalatra" , "play-scala" , "play-slick" , "unfiltered"
],[ "kelp" , "plack" , "dancer" , "web-simple" , "mojolicious" , "flask"
, "django" , "tornado" , "rails" , "sinatra"
],[ "nancy" , "aspnet-mvc" , "servicestack" , "cpoll-pool" , "cpoll-cppsp"
, "dart" , "play1" , "vertx" , "gemini" , "spring" , "wicket" , "servlet"
, "tapestry" , "play-java" , "dropwizard" , "rest-express" , "play-java-jpa"
, "grizzly" , "openresty" , "php" , "yaf" , "cake" , "fuel" , "slim"
, "silex" , "kohana" , "laravel" , "lithium" , "phalcon" , "phreeze"
, "micromvc" , "symfony2" , "codeigniter"
],[ "onion" , "hapi" , "ringo" , "nodejs" , "express" , "ringo-conv"
]]

robustnessClusters=[[ "elli" , "cowboy" , "snap" , "yesod"
],[ "luminus" , "http-kit" , "compojure" , "play1" , "vertx" , "gemini"
, "spring" , "wicket" , "servlet" , "tapestry" , "play-java" , "dropwizard"
, "rest-express" , "play-java-jpa" , "grizzly" , "lift" , "finagle"
, "scalatra" , "play-scala" , "play-slick" , "unfiltered"
],[ "grails" , "hapi" , "ringo" , "nodejs" , "express" , "ringo-conv"
, "openresty" , "php" , "yaf" , "cake" , "fuel" , "slim" , "silex"
, "kohana" , "laravel" , "lithium" , "phalcon" , "phreeze" , "micromvc"
, "symfony2" , "codeigniter" , "flask" , "django" , "tornado" , "rails"
, "sinatra"
],[ "onion" , "nancy" , "aspnet-mvc" , "servicestack" , "cpoll-pool"
, "cpoll-cppsp" , "dart" , "go" , "revel" , "kelp" , "plack"
, "dancer" , "web-simple" , "mojolicious"
],[
],[
]]

        var essentialVector=[100,10,1,0,0,0];
        var importantVector=[1024,256,64,16,4,1];
        var normalVector=[32,16,8,4,2,1];
        var somehowVector=[10,8,6,4,2,1];
        var whateverVector=[1,1,1,1,1,1];

        var framework=[];

        for (var i=0;lt(i,efficiencyClusters.length);i++) {
            for (var j=0;lt(j,efficiencyClusters[i].length);j++) {
                framework[efficiencyClusters[i][j]]={};
            }
        }
        $(["efficiency"
          ,"popularity"
          ,"expressiveness"
          ,"robustness"]).each(function(){
            var tab;
            eval("tab = "+this+"Clusters;");
            for (var i=0;lt(i,tab.length);i++) {
                for (var j=0;lt(j,tab[i].length);j++) {
                    eval("framework[tab[i][j]]."+this+"= i;");
                }
            }
          });

        function setLine(name,vector) {
            $('#' + name+' td input').each(function(i){
                var len = vector.length;
                if (lt(i,len)) {
                    $(this).val(vector[i]); }});}
        $(['t-expressiveness'
          ,'t-popularity'
          ,'t-efficiency'
          ,'t-robustness']).each(function(){
                    var name='#'+this;
                    var tdinput=$('<td align="right"><input style="display: inline-block;width:2em;text-align:right" type="text"></input></td>'.repeat(6));
                    $(name).append(tdinput);
                    $(name).append($('<td>'+
                        '<select style="width:6em" id="s-'+this+'">' +
                        '<option value="essential">Essential</option>' +
                        '<option value="important">Important</option>' +
                        '<option value="normal" selected="t">Normal</option>' +
                        '<option value="somehow">Somehow</option>' +
                        '<option value="whatever">Unsignificant</option>' +
                        '</select>' +
                        '</td>' ));
                    if (this == "t-expressiveness") {
                        setLine(this,essentialVector);
                        $(name+' select').val("essential");
                    } else if (this == "t-popularity") {
                        setLine(this,normalVector);
                        $(name+' select').val("normal");
                    } else if (this == "t-efficiency") {
                        setLine(this,importantVector);
                        $(name+' select').val("important");
                    } else if (this == "t-robustness") {
                        setLine(this,importantVector);
                        $(name+' select').val("important");
                    }
                    var strthis=''+this;
                    $("#s-"+this).change(function(){
                            var val=$("#s-"+strthis+" option:selected").val();
                            var tab;
                            eval('tab='+val+'Vector');
                            setLine(strthis,tab);
                            updateResult();
                        });
                });
        function updateResult(){
            var scoreMatrix=[[0,0,0,0,0,0]
                            ,[0,0,0,0,0,0]
                            ,[0,0,0,0,0,0]
                            ,[0,0,0,0,0,0]
                            ];
            $(['t-expressiveness'
              ,'t-popularity'
              ,'t-efficiency'
              ,'t-robustness']).each(function(i){
                $("#"+this+" td input").each(function(j){
                    scoreMatrix[i][j]=$(this).val(); }) });
            var result=[];
            for (key in framework) {
                framework[key].score =
                    scoreMatrix[0][framework[key].expressiveness] *
                    scoreMatrix[1][framework[key].popularity] *
                    scoreMatrix[2][framework[key].efficiency] *
                    scoreMatrix[3][framework[key].robustness];
                result.push([key,Math.log(framework[key].score)]);
            }
            result.sort(function(a,b){return lt(a[1],b[1]);});
            $('#result').html('<p style="text-align: center">The winner is<br/><strong>'+result[0][0]+'</strong> ('+languageOf[result[0][0]]+')</p><table><tr><th>position</th><th>framework</th><th>language</th><th align="right">log(score)</th></tr></table>');
            for (k=0;k<10;k++){
                $('#result table').append('<tr><td>'+(k+1)+'</td><td>'+result[k][0]+'</td><td>'+languageOf[result[k][0]]+'</td><td><code>'+result[k][1]+'</code></td></tr>');
            }
        }
        $('#compute').click(updateResult);
        $("#choice-matrix input[type='text']").change(updateResult);
        $("#choice-matrix input[type='text']").keyup(updateResult);
        updateResult();

        // Show hide tables in the article
        $('#toggle-expressiveness-table').click(function(){
                                        $('#expressiveness-table').toggle();
                                        });
        $('#toggle-robustness-table').click(function(){
                                        $('#robustness-table').toggle();
                                        });
        $('#expressiveness-table').toggle();
        $('#robustness-table').toggle();

    } else {
        setTimeout(run,50);
    }
        }
        run();
    })();
// ]]></script>

## Conclusion

Please, remember all of this is based as most as I could on objective data.
Also the choice method seems rather rational.
It is now up to you to edit the score matrix to set your needs.

Also consider it took me a while to end up with this.
And I know in the current state there are many flaws.
But I am quite happy my scoring system had chosen the framework I choosen for myself before.

Also some remarks on playing with different values.
Java frameworks score quite well, if popularity matter more than expressiveness.
Furthermore yesod score very good even if you don't consider robustness.

Of course the source code for the matrix shouldn't be too hard to read.
You could change the positionning of some frameworks if you believe I made some mistake by placing them in some bad clusters.

I didn't had the courage to make this change possible using an user interface.
So feel free to copy the js contained in this page (inline) and to make your own chooser.
