---
kind:           article
published:      2013-08-06
image: /Scratch/img/blog/next-web-techno/main.png
title: Choosing the web techonology
author: Yann Esposito
authoruri: yannesposito.com
tags: programming, framework, web
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">

%tldr


</div>

This is it.
You've got your idea.
You know you will be rich.
You just need to make a very simple web application.

So, it sounds easy!
Glory is just around the corner.

And after the first step you are victim of the terrible
[**choice paralysis**](https://en.wikipedia.org/wiki/Analysis_paralysis) syndrom.

Which framework should you choose?
There is so many of them.
The more you get informed for hardest it is to decide.
Some framework pretend to enhance dramatically your experience.

Most people choose by answering the two following questions:

Which language am I familiar with? 
What is the most popular web framework for this language?

But behind your head you couldn't think about what if there were another
better web framework in another language?

The question I try to answer here is how to determine the most objectively
possible the best framework from a technical point of vue.
In fact, there will be different choices depending on your needs.

For example:

- if you want to develop a website for you alone.
- if you want to create a startup with 2 or 3 technical friends and know
you'll have to grow your number of developper soon.

So how to determine the _best_ possible framework being as objective as possible?

There are many different metrics.

1. _Expressiveness_, which is generally correlated to:
    - faster development
    - easier to handle specification
    - how the framework will react to specification change and feature requests?

2. _Efficiency_, which is generally correlated to:
   - how much processing power you'll need per user
   - how long the user will wait to see/update datas

3. _Popularity_, which correlate with:
   - number of tested libraries
   - facility to find learning material
   - ability to find another developper to work with

4. _Robustness_, which correlate with:
    - security
    - fewer bugs

**Remark**: I don't care much about popularity.
In fact, while a language is popular enough to have a sufficiently large
library for most usage, the language is good for me.
So popularity work more like a threshold in my model of choice.

So, you can't take a completely random one.
This is a serious question.

I too had to decide what technology to use.
Now, after a long time searching I believe I found the best answer.
It might change in some years but not tomorrow.

By best, I mean the option which appear to be the best compromise between most
important metrics.

1. Time to load the web page to the user
2. Time to create the website.
3. Quality of code
4. Adaptable to change
5. As bug free as possible
6. Easy to deploy
7. As portable as possible

# Objective measures

None of these property can be measured with perfect precision.
But each time we should get an order of magnitude for each.
For some properties it is easier than from other.

For example, popularity measure can be quite correct (at least for the most popular languages).
It is more complex to measure the efficiency, expressiveness and easiness, but
we can have good enough indicators.

As each indicator lack of precision, we will focus on giving order of magnitude.

## Popularity

[RedMonk Programming Language Rankings (January 2013)][redmonk] 
provide an apparent good measure of popularity.
While not perfect the current measure feel mostly right.
They create an image using stack overflow and github datas.
Vertical correspond to the number of questions on stackoverflow.
Horizontal correspond to the number of projects on github.

If you look at the image, your eye can see about four clusters.
The 1st cluster correspond to mainstream languages:

blogfigure("mainstreamlanguages.png","Mainstream Languages Cluster from [RedMonk][redmonk]")

Javascript, Java, PHP, Python, Ruby, C#, C++, C, Objective-C, Perl, Shell

Most developper know at least one of these language.

The second cluster is quite bigger.
It seems to correspond to languages with a solid community behind them.

blogfigure("secondtierlanguages.png","Second tier languages from [RedMonk][redmonk]")

[redmonk]: http://redmonk.com/sogrady/2013/02/28/language-rankings-1-13/

I won't talk about third and fourth tier languages.

I have the feeling that most of the third, fourth tier cluster languages haven't
reached the critical mass to have a sufficently big community to help you in
case of problem. This is to count in the _subjective_ part. For now, I believe
it would be very hard to verify the number of libraries provided for each languages.
But it doesn't feel wrong to suppose it will be closely related to the three
measures: popularity, expressiveness and easiness.

I might be very wrong for some languages thought (for example Common Lisp).

## Efficiency

Another objective measure is efficiency.
More precisely, how fast is a language?

While this is not perfect here are some benchmarks to give an idea of magniture order:

[benchmarks](http://benchmarksgame.alioth.debian.org/u64q/benchmark.php?test=all&lang=all&data=u64q)

Mainly, there are some clusters:

1. very fast languages (1x→2x):  
    * C, C++, ADA, Fortran, ATS
2. fast languages (2x→3x):  
    * Java 7, Scala, OCamL, Haskell GHC, Go, Common LISP
3. Reasonably fast languages (3x→10x):  
    * C#, Clojure,, Pascal, Racket, Dart
4. A bit slow (10x→30x):  
    * Erlang
5. Slow languages (30x→):  
    * PHP ; huge variations, can be about 1.5x C speed in best case.
    * Python ; huge variations, can be about 1.5x C speed in best case
    * Perl ; Can be about 3x C speed in best case
    * Ruby, JRuby ; mostly very slow.

This is a first approach. The speed of the language for basic benchmarks.
But, here we are interrested in web programming.
Fortunately techempower has made some tests for all web framework this time.

[Web framework benchmarks](http://www.techempower.com/blog/2013/05/17/frameworks-round-6/).

As always, these value quite informative are also quite imprecise relatively to 
how all of this will respond for your own application.

So we continue to create clusters.

Now, how to objectively measure expressiveness?

Here is a very good idea that helped to give an objective (while imprecise)
metrics of each language expressiveness:
[click here](http://redmonk.com/dberkholz/2013/03/25/programming-languages-ranked-by-expressiveness/).

## Robustness

I couldn't find any complete study to give the number of bug relatively
to each framework/language.

But on thing I saw from experience is the more powerful the type system the
safest your application is. While not removing completely the need to
test your application each progress toward a better type system tend to
remove complete classes of bug.

Typically, not using pointer in Java help reducing the number of error due to
bad reference.

blogfigure("typesystem.png","Static Type Properties from [James IRY Blog][typesanalysis]")

[typesanalysis]: http://james-iry.blogspot.fr/2010/05/types-la-chart.html

# The choice

* **Front end**: Take any reactive framework. The most popular is boostrap, so why not?
Most informations are in the %html. So could be handled on the server side.
* **Server side**: Yesod

One word about Haskell when you want to _use_ it and not to study it or play with it.

# Appendice - Tribulations

How I experienced web developement.

If you just want to know my choice without knowing why,
[click here to jump to the last section](#the-choice).

I'll try to discover what an ideal web framework might look like.

## Client side

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

# Server side

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
