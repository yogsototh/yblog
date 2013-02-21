-----
isHidden:       false
menupriority:   1
kind:           article
published: 2011-09-28
title: Programming Language Experience
authorName: Yann Esposito
authorUri: yannesposito.com
tags: programming, languages, C, C++, Java, haskell, Logo, Basic
-----
blogimage("dragon.jpg","Title image")

<div class="intro">
%tldr My short and highly subjective feelings about the programming languages I used.
</div>

### `BASIC`

leftblogimage("basic.gif","Title image")

The language of my firsts programs!
I was about 10, with an `MO5` and `Amstrad CPC 6128` and even with my `Atari STe`.
This is the language of `GOTO`s.
Ô nostalgia.
Unfortunately this might be the only interesting part of this language.

Today this language is obsolescent.
It is not even good to learn programming.
I know some compiler exists now.
But this is not enough to try to learn it.



<pre><code class="zsh">READY
10 PRINT "HELLO WORLD!"
20 GOTO 10
RUN
</code></pre>



I also remember I copied some game source code from some magazine.
Most lines were like:



<pre><code class="zsh">3110 DATA FA,01,FF,FF,FF,FF,00,23,22,43,DA,DE,EE,FF,FF,FF,00,03,4A,F2
</code></pre>



What a pleasure!

### Logo

leftblogimage("dragon.jpg","Dragon fractal")

I was about 10 when I played with logo to draw things on a screen.

I remember the Bach's music while the program loaded.

At that time we had to load the program into the memory using tapes.
This one was a rare one. It didn't made an awfull 'Krrrkrr cssssss krrr' noise.

Some years after, I used it to learn programming to my college student.
It was a really good first language.
Making fractals is like a game for children.

Here is the code to draw the dragon fractal:

<code class="zsh">
HIDETURTLE

PENUP
SETXY -200 0
RIGHT 90
PENDOWN

to dragon :degree :size
    setpensize 1
    if :size>5  [setpensize 2]
    if :size>10 [setpensize 3]
    if :size>20 [setpensize 4]
    if :size>40 [setpensize 5]
    ifelse :degree=0 [
        fd :size
    ][
        left  45 dragon (:degree-1) (size/4)
        right 90 dragon (:degree-1) (size/2)
        left  90 dragon (:degree-1) (size/4)
        right 45
    ]
end

dragon 6 3000
</code>

### Pascal

The eternal second.

I made my firsts real serious program with Pascal.
I must confess I find it inferior to C.
I made graph algorithms, sort algorithms even some IA (genetic) algorithms.
In the end I prefer C.

### C

leftblogimage("C.jpg","Pointer representation from Dancing links")

The pointer's language.

_Le_ programming language.

Once you understand loops and recursion,
it is time to do serious things.
If you want to make good quality code, knowing C is almost mandatory.

This language is close to the machine language.
So much, there is (mostly) a linear relation between the size of your code and the size of the compiled one.

In short, each time you write a C instruction there won't be anything strange that will occurs, like starting a long algorithm behind the scene.

While close to the machine, `C` keep sufficient abstractions to be fun.

I made a lot of program with it.
From sort algorithms to AI ones (SAT3), system, network programming, etc...
It is a very useful language that will help you understand how things works on your computer.
Most modern computer language hide a lot of informations on what occurs.
This is not the case with `C`.

### ADA

The "super clean" one.

I liked ADA. I must confess I didn't used it a lot.
May be one day I will try it again.
I was impressed by asynchronous programming with it.
What you need to know is this old language had certainly inspired most new object oriented languages.

## Object Oriented Languages

Until here I just described imperative languages without any object notion.

More clearly, the language didn't helped you to structure your program.

In order to limit the number of bugs, particularly for huge programs, we started to think about how to organize computer programs.
In the end, from the imperative language culture, it produced the Object Oriented programming (OOP).
Beware, the Object Oriented programming isn't a miracle. Proof? How many bug free software do you use?
Furthermore, OOP doesn't fit all problems.
But to make a bank application, an application which help to manage stock, clients or text archives or more generally
an information system, the OOP is not so bad.

Then Object Oriented Languages appeared everywhere.

### C++

leftblogimage("cplusplus.jpg","Messy router")

The ugly

Industry wanted an Object Oriented Language without losing all their old C code.
Solution, keep C and add an Object layer on it.
My main concern about C++ is: it does too many things.
I appreciated multiple inheritance and templates.
In reality I liked a lot C++ while I was working alone.
I used it to write `DEES` my main thesis software.
My only concern was about a lack in the STL.
In the doc, one could use `String<T>`.
But in reality, `T` had to be only `char` or `char16`.
Then I had to reduce my alphabet to 2<sup>16</sup> letters.
Except for some application, the alphabet must be far larger than that.
en:
To conclude, I'd say, C++ is very good if you work alone or with a fixed subset of its features.

fr:

### Eiffel

leftblogimage("eiffel.jpg","Eiffel tower construction")

Yes, it is a really nice language.
Full object in mind. Far cleaner than C++.
But it isn't so popular.
Behind C++ there is a large community to help new users and to write libraries.
Furthermore, I preferred working with C++.
When I learned Eiffel, I programmed a lot with C and liked its syntax.

### Java

leftblogimage("grail.jpg","Holy Grail from the Monty Python")

The first time I heard about Java it was _le Grail_!

Perfect portability, your program will work on all platform.
The language helps limit mistakes, and force you to use good programming habits.
But...

But It is extremely verbose.
Many limitations are quite boring if you know what you're doing.

For example, there is no multiple inheritance.
Generally it is a coherent choice when there are a way to compensate.
In Java, there are interfaces for this.
Except, interfaces can only add methods to a class.
You cannot add any attribute to a class except by subclassing.
I really lacked this feature.

I made a GUI using Java Swing and I created my own notification system between different element of the GUI.
In the beginning I only needed to send notification one to one.
After some time, I wanted to send one to many notifications.
I had to make a bunch of copy/paste inside all my subclasses!
Copy/paste are exactly what should be avoided the most by object oriented languages.

Another thing: threads.
I was forced to make my own thread management system to avoid locks and send notifications between threads (wait the end of this thread, ...).
At that time I used Java 1.5.
This problem should have been solved with Java 1.6.
I wish it is the case, but lacking such an essential feature for a language was very bad.

In the same idea, it was very long to wait for the "foreach" loops.

After my experience, I don't recommend Java.
Portability does not worth this price.

GUI portability means mediocre experience on all platforms.
Any system it might be (wxWidget, QT, etc...).

The Java ideology is "closed".
But it resolve a big problem.
It helps medium to low quality developers to work in team without the ability to make too much harm to the product.
A good programmer will be able to make very interesting things with it thought.
Please note I didn't say Java programmer are bad programmer.

### Objective-C

leftblogimage("xcode_logo.png","Xcode Logo")

The language I learned and used only to make application on Apple(c) platform.
I learned Objective-C just after Python.
It was hard to do it.
At first I didn't liked the syntax and many other details.
But it is this kind of language the more you use, the more you like.
In fact, Objective-C is a simple language, but associated with the Cocoa framework it is a really good tool.
Cocoa is very different to other framework I used before.
I find many of its idea extremely good.
Both simple and efficient.
It might seems like small details on paper, but once you start using it, it makes all the difference.

Even if Objective-C is a relatively low level language.
Its dynamic typing ability make it very good for GUI programming.
I recommend to continue working with this language.
In the end you'll certainly find it better than expected.

## Modern Scripting Languages

### PHP

leftblogimage("php.jpg","A Jacky Touch Car")

This small script language that we used all to make our website in the time of animated gifs.

Nice but no more.
Apparently there were a lot of progress since PHP5.
Maybe one day I'll use it again.
But behind it, this language has a "script kiddies only" reputation.
Also long history of easy to make security holes.

In reality PHP is just behind C for the abstraction level.
Therefore it has a lot of organisation problems and make it easier to create bugs.
For web applications it is a real problem.

PHP remains for me the SQL injection language.
I made a bit of PHP not so long ago, and it was a pain to protect my application to SQL injection.
Yep, I didn't found any standard library to make this, but I didn't searched a lot.

### Python

leftblogimage("python.jpg","Python. Do you speak it?")

Revelation!

When you were used to work with compiled languages (C++, Java) and you start learning Python, it's like a punch in the face.
Programming like it always should have been.
Everything is natural, it's _magic_.
Yes, as good as this.
But something so good must have some drawback.

And yes, like all interpreted languages, Python is _slow_.
Beware, no just a bit slow like 2 or 3 times slower than C. (like Java for example).
No, really slow, about 10 to 20 times slower than C.
Argh... Note it is completely usable for many things.

### Awk

If you have to "filter" some files and the filter is not too complicated awk is the ideal language to do this.
For example, if you want to know which words in a text file are most used.
I used it to modify hundreds of XML files in an easier manner than XSLT.

### Perl

Perl is magic, but the syntax is so hideous nobody can like to work in an environment with many different person in Perl.
Or at least, all other collaborators must be excellent programmers.
A great feature of perl is its integration with regular expression in its syntax:



<pre><code class="perl">$var =~ s/toto/titi/g
</code></pre>



This program will replace every `toto` by `titi` inside the `$var` variable.
The Perl code is often very compact and generally unreadable.
But it is a language good to know.
It is a kind of `awk` under steroids.

### Ruby

Ruby is a very good language.
It is often compared (opposed ?) to Python.
There are the regular expression operators Perl inside the langage.
But the syntax is extremely clear, like in Python.
Many feature were inspired by functional programming (as in Python).
I used it a lot.
It is the worst language I know in term of efficiency.
This is the language that lose almost all benchmarks.
But it is the perfect tool for prototypes.
If you want to make a website prototype, RoR (Ruby on Rails) is certainly one of the best system known to mankind.
From idea to realisation, few time will occur.
Make this site work for thousands of people, will, on the other hand, certainly require a lot of optimisations.

One of the greatest Ruby feature is its ability to make the program extremely readable.
It is very close to natural language.
On the other hand, I found the Object Oriented layer a bit disappointing.
The fact there is no real "class variable" but only "tree class variable" for example.

Considering the community, the ruby one feels closer to the creative than the engineer.
I am under the impression designer tends to use Ruby instead of Python.

### Javascript

It is the good surprise.
During years, javascript was considered as an annoying web experience language.
In reality, javascript has many really good qualities.
Particularly, it is easy to pass a function in parameter and to create anonymous functions (closures).
Recently, javascript became far faster than before and many frameworks and libraries appears:

- Cappuccino, Objective-J (as in objective-C but with javascript)
- Sproutcore
- Spine.js
- Backbone.js
- jQuery
- prototype.js

Particularly with jQuery we can chain functions.
It is very nice to use.
As I said, this is a good surprise.
Javascript was chosen by chance as the script inside your navigator.
Instead of the java inspired syntax, everything else is very good.
In order to compensate the syntax, you can use CoffeeScript.

## Functional Languages

### CamL

I learned CamL during the college.
It was really interesting.
Functional programming is very different to imperative programming (most of popular languages).
I had good mathematic intuitions to use this language.
But I must confess I never used it for something serious.

### Haskell

I believe I will still learning this language in many years.
I must say it is a pleasure.
Generally it takes me no more than some hours to some days to learn a new programming language.
Concerning Haskell, this is very different.
To master Haskell you need to understand very abstract concepts.
Monads and Arrows are some of them.
I didn't understood them before I read some scientific paper.
Many week will be necessary to master it perfectly (if someone does).
Also the community is very friendly and nice.
There is no "LOL! URAN00B! RTFM!"
And no concession has been made to make this language more popular (I'm looking at you C++, Java and Javascript).
This langage remain pure (I know there are two meaning).

Concerning making real product with Haskell.
In fact, Haskell is very efficient concerning code change.
Refactoring code is incredibly easy with Haskell.
And in the same time, the Haskell type system helps to make your product bug free.

Technically this language is close to perfection.
But it has some major problems:

- not so popular
- hard to learn
- I also believe there is not actually enough success stories with Haskell

On the other hand, knowing Haskell will help you learn a lot of thing about programming in general.
You should at least take a look.
[I made an Haskell introduction if you are curious](/Scratch/en/blog/Haskell-the-Hard-Way/).

## Unpopular Languages

### Metapost

Metapost is a language to program drawings.
What make metapost very good?
It contains a linear solver.
This is really useful to draw things.
For example if you write:



<pre><code class="ruby">AA=1/3[A,B]
</code></pre>



It will place the point `AA` between the point `A` and `B`.
More precisely at the barycenter `(2xA + B)/3`.



<pre><code class="ruby">X=whatever[A,B]
X=whatever[C,D]
</code></pre>



This second example, will place the point X at the intersection of the two segments `AB` and `CD`.

This feature is very helpful, and not only to draw things.
Most programming language should think about adding it.

### zsh

Yes, zsh is a shell.
But it is also a script language very well suited to file management.
For now, it is the best shell I used.
I prefer zsh to bash.

### Prolog

I never made something serious with Prolog, but I really loved to use and learn it.
I had the chance to learn Prolog with [Alain Colmerauer](http://alain.colmerauer.free.fr/) himself.
This language try to resolve constraints as much as it can.
It has a magic feeling when you use it.
We only write constraints, we never put order.
A bit like functional programming but far more powerful.

## Languages to discover

Many languages and framework remains to be learnt and tried.
Actually I believe I will stay a while with Haskell.
Maybe tomorrow I will look at LISP, Scala or Erlang.
I also certainly look at clojure to make web application.

Tell me if you have any other experience with these programming languages.
Of course, my feelings are highly subjectives.
But I used all of these languages.

*[STL]: Standard Tempate Library
*[GUI]: Graphic User Interface
