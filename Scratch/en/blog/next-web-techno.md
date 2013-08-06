---
kind:           article
published:      2013-08-06
image: /Scratch/img/blog/next-web-techno/main.png
title: next web techno
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
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

So, that sounds easy!
Glory is just around the corner.

In fact, you realize just after the first step that you are confonted to the
terrible [**choice paralysis**](https://en.wikipedia.org/wiki/Analysis_paralysis).

What framework to choose?
They all claim to be the best, and there are so many of them.
The more you get informed for hardest it is to decide.
Because at first you could simply say, let's take a random one.
But many comments tells you that another framework will make the cost of the
project a tenth the price if you use another framework.

So this is a question to take seriously.

Fortunately for you, I was there too.
I believe I found the "best" current option.
By best, I mean the option which appear to be the best compromise between most
important metrics.

1. Time to load the web page to the user
2. Time to create the website.
3. Quality of code
4. Adaptable to change
5. As bug free as possible

# Tribulations

If you don't care a bit why I made the decision I made, just [skip](#the-choice)

From the idea to the creation of the apparently most basic website you need a lot of competences.
I don't even know where to begin.

To begin, you need to know many different languages.
The bare minimum being:

  * %html
  * %css
  * javascript

For many reason these languages have a _lot_ of weaknesses.
So, if you want to go to the next step you have different choices.

The first being for each language, make a better language:

- for %html, use [HamL][haml] for example
- for %css, use [sass][sass] or [less][less]
- for javascript, use [coffeescript][cfs], or any of the huge possibility to compile to javascript from another language.

This is a first try to make things better.
But this is not sufficent for many things.
The problem is not only these languages (technologies?) are weak.
They also tend to not work properly together.

Obligatory proof:

> Try to create a webpage containing a single centered red box.
> The box containing a centered white text.
> 
> By centered I mean horizontally and _vertically_.
> Note the size of the box and of the text must be dynamic.
> I mean, the text can be on many lines and the box is ¼ the size of the window for example.

Try doing this without using tables nor javascript.
Whatever the solution is (if there is one), it is certainly over complicated.
In particular, you must remember the solution must works at least on all modern
browser.

While it will certainly possible to do that, the size of the restult will be very long
compared to the sentence I used.
Knowing I expressed all necessary details in the sentence.

So on the next step, some realized you could do everything in javascript.
Javascript can create %html and %css dynamically.
For example see [Cappuccino][cappuccino]. In fact, cappuccino use [Objective-J][objj].
A language inspired from Objective-C which compile to javascript.

When I discovered [Cappuccino][cappuccino] I was really impressed.
You don't have to know %html nor %css to make highly responsive web application.
You just have to learn Objective-J.

Neat.

But this doesn't end here. For now I only talked about client side code.
If you want to create a real website you have to take care about the server side.
Here, as the layer is thinner (mostly HTTP) there are many different choices.

Most programming languages have libraries of framework to help you create
a web server.

For the entreprise Java J2EE (ugly but mostly works).
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

# The choice

* **Front end**: Take any reactive framework. The most popular is boostrap, so why not?
Most informations are in the %html. So could be handled on the server side.
* **Server side**: Yesod

One word about Haskell when you want to _use_ it and not to study it or play with it.

