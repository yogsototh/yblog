-----
isHidden:       false
menupriority:   1
kind:           article
created:     2011-10-04T10:18:59+02:00
title: Yesod excellent ideas
authorName: Yann Esposito
authorUri: yannesposito.com
tags: yesod, framework, web, haskell, ideas
-----
blogimage("main.png","Title image")

<div class="intro">

%tldr

[Yesod](http://www.yesodweb.com) is a web framework which recently reached the maturity for which you should consider to use it.
Before telling you why you should learn Haskell and use yesod, I will talk about ideas yesod introduced and I didn't saw in other frameworks before.

</div>

## Type safety
   
Let's start by an obligatory link from [xkcd](http://xkcd.com):

   ![SQL injection by a mom](http://imgs.xkcd.com/comics/exploits_of_a_mom.png)

When you create a web application, a lot of time is spent dealing with strings.
Strings for URL, HTML, JavaScript, CSS, SQL, etc...
To prevent malicious usage you have to protect each strings to be sure, no script will pass from one point to another.
Suppose a user enter this user name:

<code class="javascript">
Newton<script>alert("An apple fall")</script>
</code>

You must transform each `<` into `&lt;`.
Without this transformation alert will appear each time you try to display this user name.
Safe types associate with each string what kind of string it is.
Is it a string for URL? For javascript? For HTML?
And the right protection is made by default to prevent problems.

Yesod does its best to handle cross scripting issues. Both between the client and the server and between the server and your DB.
Here is an example:

<code class="html"><a href=@[AnotherPageR]>Go to the other page
</code>

As `AnotherPageR` is of type URL and it could not contains something nefarious.
It will be an URL safe. Not something like:

<code class="html">
falselink"><script> bad_code(); </script><a href="pipo
</code>

## Widgets

Yesod's widgets are different from javascript widget.
For yesod, widgets are sets of small parts of a web application.
If you want to use many widgets in a same page yesod do the work.
Some examples of widget are:

- the footer of a webpage,
- the header of a webpage with a menu,
- a button which appears only when scrolling down, 
- etc...

For each of this part, you might need, 

- a bit of HTML, 
- a bit of CSS and 
- a bit of javascript.

Some in the header, some in the body.

You can declare a widget as this (note I use a very high meta-language):

    htmlheader = ...
    cssheader = ...
    javascriptheader = ...
    htmlbody = ...

The real syntax is:

<code class="haskell">
toWidgetHeader cassiusFile "button.cassius"
toWidgetHeader juliusFile "button.julius"
toWidget       hamletFile "buttonTemplate.hamlet"
</code>

Note the awesome Shakespearean inspired name convention.
Another good reason to use yesod.

- Cassius _&_ Lucius of CSS (a lot similar to SASS and SCSS),
- Julius for JavaScript (note a CoffeeScript is somewhere in the source of yesod),
- Hamlet for HTML (similar to haml)

And when your page render, yesod makes it easy to render everything nicely:

<code class="haskell">
myBigWidget =  menuWidget >> contentWidget >> footerWidget
</code>

Furthermore, if you use say 10 widgets each with a bit of CSS, yesod will create a unique and compressed CSS file. Except if you expressed a need to change the header by using different CSS. 

This is just awesome!

## Optimized routing

In standard routing system you have for each entry a couple: regexp → handler

The only way to discover the right rules is to match each regexp to the current URL. Then you can see behaviour such as, if you change the order of the rules you can lose or win time.

On the other hand yesod compiles the routes. 
Therefore it can optimize it.
Of course two routes must not interfere.

<code class="html">
/blog/2003  Date2003R
/blog/$DATE DateR
</code>

is invalid by default (you can make it valid, but I don't think it is a good idea).

You'd better

<code class="html">
/blog/$DATE DateR
</code>

and test if `date = 2003` inside the handler.

## Why yesod?

1. _Speed_. This is just astounding. Look at [this](http://snapframework.com/blog/2010/11/17/snap-0.3-benchmarks) and then to [this](http://www.yesodweb.com/blog/2011/02/warp-speed-ahead).
2. _Haskell_. This is certainly hard to learn but also incredibly awesome. If you want to make you a favor. Just learn Haskell. It will be difficult, far more than you can imagine. It is very different from all other languages I used. But it will blow your mind and learn you a bunch of new programming concepts.
3. _Good ideas, excellent community_. I follow yesod from some month now and the speed at which the project progress is incredible.

If you are a haskeller, I believe you shouldn't fear the special syntax imposed by the standard yesod way of doing things.
Just try it more than the firsts basic tutorials. 

Until here I believe it goes in the right direction. 
Even if I believe the real future is by generating HTML pages from the client (using javascript) and server limited to serve JSON (or XML, or any object representation system).

To conclude, Yesod is awesome. Just overcome the difficulties about learning a bit of haskell and try it!
