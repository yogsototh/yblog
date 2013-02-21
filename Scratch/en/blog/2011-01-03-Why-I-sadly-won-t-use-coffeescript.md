-----
isHidden:       false
menupriority:   1
kind:           article
published: 2011-01-03
title: Why I won't use CoffeeScript (sadly)
author: Yann Esposito
authoruri: yannesposito.com
tags:  Coffeescript
-----
blogimage("main.png","Title image")

begindiv(encadre)

*Update*: I might change my mind now. Why?
I just discovered a [js2coffee converter](https://github.com/rstacruz/js2coffee). Furthermore Denis Knauf told me about a `CoffeeScript.eval` function. 
And as Denis said: "it is time to use Coffeescript as a javascript with Ruby-like syntax not a Ruby-like programming language".

enddiv

begindiv(intro)

%tldr I would have loved to program client side using a Ruby-like syntax. But in the end, CoffeScript raised more disavantages than advantages.

enddiv

Recently I read [this entry](http://news.ycombinator.com/item?id=2053956) on HackerNews.
The most upvoted comment praised (within other) [CoffeeScript][cf].
Recently I used _a lot_ of javascript. After trying
[Sproutcore](http://sproutcore.com),
[Cappuccino](http://cappuccino.org), looking at
[backbone.js](documentcloud.github.com/backbone/) _&_
[javascriptMVC](javascriptmvc.com),
I've finally decided to make my own minimal javascript MVC framework.[^1]

[cf]: http://coffeescript.org

[^1]: I know it may not be the best nor productive decision, but I'd like to start from scratch and understand how things works under the hood.

I had to fight the horrible syntax of javascript. It was like experiencing a back-in-time travel: 

- Verbose Java-like syntax, 
- Strange and insanely Verbose Object Oriented Programming,
- No easy way to refer to current instance of a class (`this` doesn't work really well),
- etc... 

It was so annoying at a point, I had thinked about creating my _own_ CoffeeScript.

I'd finished a first draft of my MVC javascript framework. 
Just after I learned about the existence of CoffeeScript, I immediately created a new git branch to try it.

Here is my experience:

1. I had to install `node.js` and use `npm` just to use CoffeeScript. It wasn't a big deal but it wasn't as straightfoward as I expected either.
2. Existing javascript file are not coffee compatible. 
    I _had_ to translate them by hand. 
    There were no script to help me in this process.
    Thanks to [vim](http://vim.org), it wasn't too hard to translate 90% of the javascript using some regexp. 
    The `--watch` option of coffee was also really helpful to help in the translation. 
    But I had to write my own shell script in order to follow an entire directory tree.
4. An unexpected event. I made some meta-programming in javascript using `eval`. But in order to work, the string in the eval must be written in pure javascript not in coffee. It was like writing in two different languages. Really not so good.

## Conclusion

Advantages:

- Readability: clearly it resolved most of javascript syntax problems
- Verbosity: I gained 14% line, 22% words, 14% characters

Disadvantages:

- Added another compilation step to see how my code behave on the website.
- I had to launch some script to generate on change every of my javascript file
- I have to learn another Ruby-like language,
- meta-programming become a poor experience,
- I must convince people working with me to: 
    - install `node.js`, `npm` and CoffeeScript,
    - remember to launch a script at each code session,
    - learn and use another ruby-like language

The last two point were definitively really problematic for me.

But even if I'll have to work alone, I certainly won't use CoffeeScript either. 
CoffeeScript is a third party and any of their update can break my code. 
I experienced this kind of situation many times, and it is very annoying. 
Far more than coding with a bad syntax.

## Digression

I am sad. 
I wanted so much to program on Web Client with a Ruby-like syntax. 
But in the end I think it is not for me. 
I have to use the _horrible_ javascript syntax for now. 
At least I would have preferred a complete `ruby2js` script for example[^2]. 
But I believe it would be a really hard task just to simulate the access of current class for example.

[^2]: I know there is `rb2js`, but it doesn't handle the problem I talk about.

Typically `@x` translate into `this.x`. But the following code will not do what I should expect. Call the foo function of the current class.

<code class="ruby">
-> 
class MyClass
  foo: ->
    alert('ok')

  bar: ->
    $('#content').load( '/content.html', ( -> @foo(x) ) )
    # That won't call MyClass.foo
</code>

The only way to handle this is to make the following code:

<code class="ruby">
-> 
class MyClass
  foo: ->
    alert('ok')

  bar: ->
    self=this
    $('#content').load( '/content.html', ( -> self.foo(x) ) )
</code>

Knowing this, `@` notation lose most of its interrest for me.
