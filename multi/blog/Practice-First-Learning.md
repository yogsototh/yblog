---
kind:           article
published:      2016-03-22
image: /Scratch/img/blog/Practice-First-Learning/main.png
en: title: Practice First Learning
fr: title: Practice First Learning
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">

%tldr Theory First Learning vs Practice First Learning and their consequences
relative to the programming/frameworks/technologies.

</div>

Let say you are wanting to deliver something using some `tool`.
The `tool` here being a programming language, a framework
or any piece of non trivial technology.

Here are two opposite methods to achieve your result:

1. **Theory first**:
   You read _the_ book and it explain you all the gory details about
   this language / framework.
   It will start with basic concepts, and then will send you to
   more and more complex concepts.
2. **Practice first**:
   You don't read _the_ book, you start to use the system, and
   each time you encounter a problem, you learn how to solve it.

Generally you don't use completely one or another method.
In practice, you end up, doing a bit of both at different level.

For example you could read only an introduction book, or
start directly using examples but read blog posts at the same time, etc...

During my programming life I generally used different level of both method.

- C: Method 1
- C++: Method 1
- Perl: Method 2
- HTML/CSS: Method 2
- AngularJS: Method 2
- MongoDB: Method 2
- Clojure: Method 2 (80%) + Method 1 (20%)
- Haskell: Method 1 (80%) + Method 2 (20%)

Here I'm not interrested in which method is superior.
But I observed something interresting.

When using methodology 1, you tend to use _all_ possible feature of
a framework/language. Because you all know them and use them.
The problem with that, is generally to enter your code, another
dev should know at least everything you used.

If you use methodolgy "Coding first", then your code tend to be
cleaner, simpler and easier to read.
Simply because you tend to use far less concepts.
Generally, very few concepts are enough to have sufficient expressive power.

For example things I didn't need to use to finish a complete real time
data analysis in Clojure:

- macros
- parameter deconstruction
- defrecord
- defmulti
- defprotocol
- extendprotocol
- core.async channels
- ...

What did we used? Only functions, primitive data types and atoms.

Should we have needed to use more complex or more optimized functions.
Yes, we would have used some of these Clojure _features_.

The point is. We finished a complete, complex distributed product
without any of these feature. We never needed them.
The code is fast enough, and it is _very_ clear to read.

Because, once you understood *functions*, *primitive data types* and *atoms*.
YUep, that was all you needed to know about the language to understand our code.

Of course, *functions* are already something a bit difficult to grasp
when you're not used to functional code. For example, you need to 
understand how to use `reduce`, `map`, `fmap`, `into`, `merge`, etc...

But compared to all the features of Clojure.
We only used about 20% of Clojure.
We didn't tried to optimize our code.
We tried to make it clear first.

More important than that.
Now we completely understand how all of these concepts works.
But we still don't use them.
Simply because it make our code cleaner and easier to read.
And the optimization benefits aren't really clear here.
We prefer, from far, a cleaner code that to use
harder to read but slightly optimized one.

### Macros

We used libraries that used macros extensively.
But generally any bug inside these library tended to be very complex to fix.
In practice, macros break composability.
And this is *bad*. By *bad* I mean, code using macros tend to become
dead members of your code body.
You are afrait to touch them.

### `defmulti`

These are *BAD*.

You can provide the ability that an external file change a local behaviour silently.
For example, you could change the behaviour of your code
depending on the libraries you imported.

How clever! How deadly clever!

### `defprotocol`

OK acceptable, but still, hard to read

### Schema

Schemas are great to provide interface between your program and the
external world.
But somehow I am not at complete ease with them.
For me, if you start to use schema extensively in Clojure, you 
don't use the right programming language.
If you use schemas a lot, in reality the chances that you want to program
in Haskell are really high.

More than that, schema were a real chance to be able to use
compile time type checking.
But they took the decision to remove `either` and to replace it by
`conditional`.
As conditional take a function which could be executed only at runtime.
That close completly the door open to the ability to have
compile time checked ADT in Clojure.

Furthermore here the kind of things you can do with schemas.

~~~ {.clojure}
;; infinite look causing Stack Overlow
(defn mk-wrong []
 {:wrong (mk-wrong)})

(defschema Wrong
  (conditional #(< (t/year (t/now)) 2042) String
               (mk-wrong)))
~~~

Your program will run just fine until January the 1st of 2042 where
there will be stack overflow!
Yooohoo side effects in the system that is here
to help you make things more secure.

OK, I know this example is contrived, but, there is clearly a problem
with runtime type checks.
I am sure, one day, somebody will believe to be very clever
by using a DB request in the conditional.
And this is just a disaster waiting to occurs.

## Minimal Core Language Usage

We generally don't use our language fully.
For example, Victor Hugo had a vocabulary about 10x larger than which
of most French people only know about 2000 words.

### Conclusion

What should be the best method to learn programming?

Theory first? or Practice first?

I would believe it depends on your needs and your environment.
If you want to achieve very elegant code using high level concepts.
Certainly Theory first is the way to go.

On the other hand if you really need to make an idea of a language / framework.
Nothing beat real world usage.
Another language is out, try to make a non trivial program with it.

Each language / framework is generally the best for some things and not the best for other things.
The right tool for the job.

But mainly, you should *do* things with them to understand all the pain points
of a technology.

Nevertheless, I would be really happy to find a language for which
the number of concepts is minimal and still expressive enough
to do a lot of things with.

Typically, I believe Scheme is better in this domain than Haskell
or Clojure for example.
