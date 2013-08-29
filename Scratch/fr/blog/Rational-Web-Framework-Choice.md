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


%tlal

</div>

This is it.  
You've got your idea.  
You know you will be rich.  
You just need to make a very simple web application.

It sounds easy!
Glory is just around the corner.
You just need to choose a good modern web framework, when suddenly:

blogfigure("choice_paralysis.gif","[Choice Paralysis][choice_paralysis]")

[choice_paralysis]: https://en.wikipedia.org/wiki/Analysis_paralysis

After your brain stack overflowed, you decide to use a very simple methodology.
Answer two questions:

**Which language am I familiar with?  
What is the most popular web framework for this language?**

Great! This is it.

But, the night when you are alone in your bed, you hear this little voice.
You know the one.

> **"You didn't made a bad choice, yes. But ...  
> you hadn't made the best either."**

The question I try to answer here is how to determine the most objectively
and rationaly possible the best(s) web framework(s) depending on your need.

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

Here are the important features (parameters) I decided to use for my choice:

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
In fact we could only get order of magnitude for each.
In the end, we will have _clusters_.

------------------ ----------- ------- -------- -------- ---------
**Efficiency**       Excellent    Good  Correct     Slow  Sluggish
**Robustness**       Excellent    Good  Correct      Bad    Unsafe
**Expressiveness**        High  Medium      Low
**Popularity**      Mainstream  Medium      Low  Obscure
------------------ ----------- ------- -------- -------- ---------

So how to make a decision model from these informations?

One of the most versatile method is to give a weight for each cluster value.
And to select the framework maximizing this score:

```
score(framework) = efficiency × robustness × expressiveness × popularity
```

For example:

-------------- ---- --- --- --- ---
Efficiency      100  80  50  30  10
Robustness      100  80  50  30  10
Expressiveness  100  50  10
Popularity      10   10   0   0
-------------- ---- --- --- --- ---

Using this weighted table, that means we discard the two last popular clusters.
And we don't make any difference between popular and mildly popular frameworks.

Also, excellent expressive framework are 10x more likely to be chosen than worst expressive class.

So for each framework we compute its score relatively to a weighted table.
And we select the best(s).

The weighted table parameter depending on your needs.
You might not need a fast framework, nor a popular one for example.
In the choice section, we will discuss this further.

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

### Efficiency

Another objective measure is efficiency.
We all know benchmarks are all flawed.
But they give an idea of magniture order:

I used the benchmark from [benchmarksgame](http://benchmarksgame.alioth.debian.org/u64q/benchmark.php?test=all&lang=all&data=u64q). Mainly, there are five clusters:

----------- ----------------------------------------------------------
1x→2x       `C`, `C++`, ADA, Fortran, [ATS](http://www.ats-lang.org/)
2x→3x       Java 7, Scala, OCamL, Haskell, Go, Common LISP
3x→10x      C♯, Clojure,, Pascal, Racket, Dart
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
The goal is just to get a magnitude order idea for each framework.
Another problem is the high number of informations.

As always, these value quite informative are also quite imprecise relatively to 
how all of this will respond for your own application.
So I simply made some classes of efficiency.
This measure can be quite important, because it will mean your
application will need more money to run.
Typically, 2x, means you'll need twice the computer power to achieve the same result.
If the framework works as expected, that will mean paying for two computer instead of only one.

Remark: I separated the clusters by using power of 2 relatively to the fastest.

Cluster     Language    Framework   #nb         slow
----------  ----------- ----------- ------- --------
Excellent   C++         cpoll-cppsp 114,711       1×
            Jav              gemini 105,204
            Lua           openresty  93,882
            Jav             servlet  90,580
            C++          cpoll-pool  89,167
             Go                  go  76,024
            Sca             finagle  68,413
             Go               revel  66,990
            Jav        rest-express  63,209
Very Good   Jav               wicket 48,772   &gt;2×
            Sca             scalatra 48,594
            Clj             http-kit 42,703
            Jav               spring 36,643   &gt;3×
            PHP                  php 36,605
            Jav             tapestry 35,032
            Clj            compojure 32,088
             JS                ringo 31,962
            Jav           dropwizard 31,514
            Clj              luminus 30,672
Good        Sca            play-slick 29,950   &gt;4×
            Sca            unfiltered 29,782
            Erl                  elli 28,862
            Jav                 vertx 28,075
             JS                nodejs 27,598
            Erl                cowboy 24,669
              C                 onion 23,649
            Hkl                 yesod 23,304
             JS               express 22,856   &gt;5×
            Sca            play-scala 22,372
            Jav        grizzly-jersey 20,550
             Py               tornado 20,372   &gt;6×
            PHP               phalcon 18,481
            Grv                grails 18,467
            Prl                 plack 16,647   &gt;7×
            PHP                   yaf 14,388
Medium       JS                 hapi  11,235  &gt;10×
            Jav                play1   9,979
            Hkl                 snap   9,196
            Prl                 kelp   8,250
             Py                flask   8,167
            Jav            play-java   7,905
            Jav        play-java-jpa   7,846
            PHP             micromvc   7,387
            Prl               dancer   5,040  &gt;20×
            Prl          mojolicious   4,371
             JS           ringo-conv   4,249
             Py               django   4,026
            PHP          codeigniter   3,809  &gt;30×
Bad         Rby                rails   3,445
            Sca                 lift   3,311
            PHP                 slim   3,112
            PHP               kohana   2,378  &gt;40×
            PHP                silex   2,364
Very Bad    PHP              laravel   1,639  &gt;60×
            PHP              phreeze   1,410
            PHP              lithium   1,410
            PHP                 fuel   1,410
            PHP                 cake   1,287  &gt;80×
            PHP             symfony2     879  &gt;100×
             C#           aspnet-mvc     871
            Rby              sinatra     561  &gt;200×
             C#         servicestack      51
            Dar                 dart       0
             C#                nancy       0
            Prl           web-simple       0

These are manually made clusters. But you get the idea.
Certainly, some framework could jump between two different clusters.
So this is something to remember.

# Expressiveness

Now, how to objectively measure expressiveness?

Here is a very good idea that helped to give an objective (while imprecise)
metrics of each language expressiveness:
[click here](http://redmonk.com/dberkholz/2013/03/25/programming-languages-ranked-by-expressiveness/).

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


<div class="showhide">

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
Also we end up with clusters:

----------- ----------------------
Excellent   Haskell, Scheme, Erlang
Very Good   Scala, Java, Clojure
Good        Ruby, Python, Groovy, javascript, PHP
Medium      C++, C#, Perl, Objective-C, Go, C
----------- ----------------------

So applying this to frameworks gives the following clusters:

<div class="showhide">

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

## The choice

<table id="#choice-matrix">
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

<input type="button" value="Compute Best(s)"/>
<div id="result"></div>

<script>// <![CDATA[
    String.prototype.repeat = function(num){return new Array(num+1).join(this);};
    (function(){

popularityClusters=[[
"Rails"
],[ "django" , "servlet" , "spring" , "node.js" , "codeigniter" , "grails"
],[ "sinatra" , "flask" , "laravel" , "kohana" , "express"
],[ "cake" , "servicestack" , "play" , "wicket" , "dart" , "slim" , "tornado" , "lift" , "go"
],[ "tapestry" , "aspnet" , "yesod" , "silex" , "lithium" , "nancy"
],[ "grizzly" , "cowboy" , "dancer" , "symphony2" , "revel"
, "compojure" , "mojolicious" , "scalatra" , "finagle" , "phalcon"
, "ringo" , "gemini" , "snap" , "plack" , "elli" , "dropwizard"
, "yaf" , "play1" , "hapi" , "vertx" , "unfiltered" , "onion"
, "http-kit" , "kelp" , "micromvc" , "openresty" , "cpoll-cppsp"
, "luminus" , "phreeze"
]];


efficiencyClusters=[[
"cpoll-cppsp" , "gemini" , "openresty" , "servlet" , "cpoll-pool" , "go"
, "finagle" , "revel" , "rest-express"
],[ "wicket" , "scalatra" , "http-kit" , "spring" , "php" , "tapestry"
, "compojure" , "ringo" , "dropwizard" , "luminus"
],[ "play-slick" , "unfiltered" , "elli" , "vertx" , "nodejs" , "cowboy"
, "onion" , "yesod" , "express" , "play-scala" , "grizzly-jersey"
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
, "grizzly-jersey" , "openresty" , "php" , "yaf" , "cake" , "fuel" , "slim"
, "silex" , "kohana" , "laravel" , "lithium" , "phalcon" , "phreeze"
, "micromvc" , "symfony2" , "codeigniter"
],[ "onion" , "hapi" , "ringo" , "nodejs" , "express" , "ringo-conv"
]]

        var essentialVector=[100,50,10,0,0,0];
        var importantVector=[100,66,44,30,20,13];
        var normalVector=[100,80,60,40,20,10];
        var somehowVector=[100,95,90,85,80,75];
        var whateverVector=[1,1,1,1,1,1];
        function setLine(name,vector) {
            $('#' + name+' td input').each(function(i){
                var len = vector.length;
                if (i < len) {
                    $(this).val(vector[i]); }});}
        function run(){
            if (window.$){
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
                    setLine(this,normalVector);
                    var strthis=''+this;
                    $("#s-"+this).change(function(){
                            var val=$("#s-"+strthis+" option:selected").val();
                            var tab;
                            eval('tab='+val+'Vector');
                            setLine(strthis,tab);
                        });
                });
            } else {
                setTimeout(run,50); } }
        run();}
    )();
// ]]></script>

Here is a very simple application helping you to decide.
I made some pre choice for you:

## Personal choice

* **Front end**: Take any reactive framework. The most popular is boostrap, so why not?
Most informations are in the %html. So could be handled on the server side.
* **Server side**: Yesod

One word about Haskell when you want to _use_ it and not to study it or play with it.

## Appendice - Tribulations

How I experienced web developement.

If you just want to know my choice without knowing why,
[click here to jump to the last section](#the-choice).

I'll try to discover what an ideal web framework might look like.

### Client side

First, creating a website necessitate a lot of different competences.
To begin, you need to know many different languages.
The bare minimum are:

  * %html
  * %css
  * javascript

For many historical reasons these languages have a _lot_ of weaknesses.
So a natural first step is to use better language for each case:

- for %html, use [HamL][haml] for example
- for %css, use [sass][sass] or [less][less]
- for javascript, use [coffeescript][cfs] or any other choice in this [huge list](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS).

Using these kind of language is a very good first step.
But after a while, we discover that this is not enough.

The problem is not only these languages are weak separately.
They also tend to not work properly together.
In fact, each language is far from independant to the other part.
This cause major difficulties in code organisation.

**Example**:
    Using the standard recommandation use as much HTML and CSS as possible.
    Don't use tables for layout and use as few javascript as possible.
    In particular, you must remember the solution must works
    at least on all modern browser (yes IE too).

> Try to create a webpage containing a single centered red box.
> The box containing a centered white text.
> 
> By centered I mean horizontally and _vertically_.
> Note the size of the box and of the text must be dynamic.
> I mean, the text can be on many lines and the box is ¼ the size of the window for example.

Whatever the solution is (if there is one), it is certainly over complicated.
Generally, vertical center things dynamically with html and css is a nightmare.

Why isn't there something like:

``` html
<div id="redblock">
    <p>The text</p>
</div>
```

``` css
#redblock,
#redblock p {
    text-align: center;
    align: center middle; // <- Why doesn't this exist?
}
#redblock {
    width: 50%;
    height: 50%;
    background: #880000;
    color: #FFF;
    overflow: hidden;
}
```

In search of minimizing the number of different languages you need to use.
The next natural step is to simply use javascript for everything.
Javascript can create %html and %css dynamically.
For example see [Cappuccino][cappuccino].
In fact, Cappuccino use [Objective-J][objj],
a language inspired from Objective-C which compile to javascript.

When I discovered [Cappuccino][cappuccino] I was really impressed.
You don't have to know %html nor %css to make highly responsive web application.
You just have to learn Objective-J.

Neat.

But this doesn't end here. For now I only talked about client side code.
If you want to create a real website you have to take care about the server side.
Here, as the layer is thinner (mostly HTTP) there are many different choices.

### Server side

Most programming languages have libraries or framework to help you create
a web server.

If you restrain yourself to the most popular for the most mainstream languages
you still need to choose between:
    Java J2EE,
    rails (ruby),
    django (python),
    Zend (PHP),
    node.js (javascript)

A more [complete list](https://en.wikipedia.org/wiki/Comparison_of_web_application_frameworks#Others) gives you more than 100 choices for about 23 different programming languages.

So, nobody could compare all of them using all the most important metrics.
I mean, we could compare their response time given a specific project
[benchmarks](http://www.techempower.com/blog/2013/05/17/frameworks-round-5/).

But it is far more difficult to measure how easy it is to create a new web app.
To measure how easy it is to change something before launch and after your
application is launched.

For the startups the current trends appear to be node.js while rails and django
continue to be highly used.
Some continue to use PHP (Zend) even if it is critized more and more.

From my point of vue, the most important is to get started and finish something.
But clearly, it is harder to do a nice job with some language than with others.

So mostly you choose your preferred language and you program your website with it.

From here, you could start to think about an ideal system:

A single high level language that handle elegantly
both user interaction and server side logic.
The client side part being compiled to javascript and interacting with the
server side using only object representation (like with a JSON API).

So we might believe in a system where the server side code doesn't handle a bit
of %html. Which is the complete opposite of the current state of the art of
many web framework.

So ok, this is the ideal. Now the question. Could we achieve it today?

The closest framework today might be node.js.
But even with node, you need to know %html, %css and of course javascript.
Of course you could certainly use one of the client side framework (such as [Cappuccino][cappuccino]).

So for now, the web technologies seems to be splitted:
client-side vs. server-side.

As the future of the web seems to go toward more interaction with the user,
it feels natural to invest in front-end development instead of backend.
So rich front-end environment, make the very minimum on the server side.

But I personnally changed my mind by discovering a server side framework with
automatic bug detection.
It is not totally magic of course.
It doesn't prevent all bugs.
But most basic bugs you face in web programming are detected before you could deploy your app.

And so, such a marvellous server side framework posses too much power no to use
it at all on the client side.
So the question is.
How could we take advantage of the server side power on the client side?

After a bit, the answer start to become mostly evident.
Write a minimal (mostly verifiable) javascript library to handle
most user interaction.
So you end up writing widgets from the server.

Here is an example of what I mean:

``` html
<form action="/wrong/path" method="post">
    <input type="text" value="Enter your text"/>
</form>
```

There is a typical error here. Typically in a standard framework, such
action will written inside a template with a code looking mostly like:


``` html
<form action="/wrong/{{target}}" method="post">
    <input type="text" value="{{initial_text}}"/>
```

The problem is that I've written `/wrong/{{target}}` instead of `/foo/{{target}}`.
And so, I couldn't discover this small error if I don't try to use this action.
And if I didn't already written the '/foo/target' handler I couldn't detect
this error right now.
Or worse, if I make a bug fix but I am a bit lazy, and send it directly without 
making a basic test the error could be unoticied.

Bad, bad, bad.

Fortunately my web framework of choice, detect these kind of errors.
Because the code is as follow:

``` html
<form action="@{Foo target}" method="post">
    <input type="text" value="#{initial_text}"/>
```

and `@{Foo target}` will be replaced by `/foo/someValue`.
Yep.

[cappuccino]: http://www.cappuccino-project.org/
[haml]: http://haml.info
[sass]: http://sass-lang.com
[less]: http://lesscss.org
[cfs]: http://coffeescript.org
[objj]: http://www.cappuccino-project.org/learn/objective-j.html
