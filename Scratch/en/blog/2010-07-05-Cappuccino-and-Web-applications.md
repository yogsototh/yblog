-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-07-05
title: Cappuccino vs jQuery
author: Yann Esposito
authoruri: yannesposito.com
tags:  Cappuccino, iPhone, web, javascript, jQuery, Cocoa, programming
-----

<div class="intro">

<abbr class="sc" title="Too long; didn't read">tl;dr</abbr>:

* Tried to make [YPassword](http://yannesposito.com/Softwares/YPassword.html) in jQuery and with Cappuccino.
* Cappuccino nice in desktop browser but 1.4MB, not compatible with iPhone.
* jQuery not as nice as the Cappuccino version but 106KB. iPhone compatible.
* I'll give a try to Dashcode 3.

</div>

---

<div class="intro">

Before start, I must say I know Cappuccino and jQuery are no more comparable than Cocoa and the C++ standard library. One is oriented for user interface while the other is and helper for low level programming.
Nonetheless I used these two to make the same web application. This is why I compare the experience I had with each of them for this specific task.

</div>

I made a web version of my dashboard widget [YPassword](http://yannesposito.com/Softwares/YPassword.html).
It is a simple widget to manage your online password with a *strong* security and with a totally portable way. It is not intended to replace a *keychain*.
It is more a password generator.

The first was made from the code of my dashboard widget and with some jQuery.
You can try it [here](http://yannesposito.com/YPassword.old).
I then made a second version with the [Cappuccino](http://cappuccino.org). You can try it [here](http://yannesposito.com/YPassword).

## What this widget do?

<div class="intro">

If you don't mind about what does my widget and just want to know how the two frameworkcompare, you should go
directly to the [next part](#cappuccino).

</div>

I manage my password on many site with a simple method. 
I remember a strong master password. And my password is mainly
<code class="ruby">hash(masterPassword+domainName)</code>

In reality I need a bit more informations to create a password:

* A master password,
* an URL,
* a maximal password length,
* the kind of output base64 or hexadecimal,
* how many times my password could have leaked.

The result password is this:

<code class="ruby">
domainName=domaine_Name_Of_URL(url)
hash=sha1( masterPassword + leakedTimes + domainName )
if ( kind == 'base64' )
    hash=base64(hash)
end
return hash[0..maxlength]
</code></pre>

In fact depending of websites, some give some strange constraint to your password: 

* minimal length, 
* maximal length,
* must not contain a special character, 
* must contain a special character, 
* etc...

And if you want to change your password the *leak number* is here for that.
All informations such as user name, maximal length can be stored in a public file. The only real secret is the master password.

If you want to know even more details you can always look at some of my old blog entries: 

* [simple portable password management](http://yannesposito.com/YBlog/Computer/Entr%C3%A9es/2008/7/30_Easy%2C_secure_and_portable_password_management_system.html)
* [base64](http://yannesposito.com/YBlog/Computer/Entrées/2009/3/15_Shorter_Password_with_the_same_efficiency.html)
* [change your password](http://yannesposito.com/YBlog/Computer/Entr%C3%A9es/2009/4/11_Be_able_to_change_your_password.html)

## Cappuccino

First, I'd like to say Cappuccino applications look simply awesome. 
It is like having a Cocoa application in your web browser. 
And this is *great*.

I also must admit I enjoyed making my application with Cappuccino. 
It is like programming for an iPhone application. 
If you are a bit familiar with Cocoa, you feel at home. 
If you don't know anything about Cocoa, I suggest you to look at it. 
This is a really _great_ framework to make User Interface.
I am not a specialist, but I have done some MFC, java Swing[^1] and WXWindows User Interfaces (some years ago).
And I must say, Cocoa is far better than those.

[^1]: If you are interested you can take a look at [SEDiL](http://labh-curien.univ-st-etienne.fr/informatique/SEDiL/). I am proud of the tree drawing view made from scratch.

Cappuccino is a great web application oriented development.
But there was also some drawbacks

Things I liked:

* It looks great
* It was fun to program
* It was like programming a Mac application
* I could have done the User Interface using [Interface Builder](http://developer.apple.com/technologies/tools/xcode.html).

Some things I didn't like:

* I made some time to understand how to handle the `onChange` on the text fields.
* Documentation lacked a bit of organisation.
* It doesn't work on iPhone.
* It weighted 11MB to deploy.
* It weight 1.3MB to load.

I didn't use bindings because I believe they are not ready by now.

## jQuery

The jQuery version of YPassword is not as finished as the Cappuccino one. Because, there is no *slider* directly with jQuery. I'd have to use jQueryUI. And I believe, using it will make the application weight far more than the today 106KB.

To make this version I simply copied my widget source code and adapted it. It was straightforward. But jQuery is not an *application oriented framework*. It is more a "*dark side* javascript animation framework"[^2].

[^2]: I don't want to feel like a *troll* I use jQuery to make some *dark side* animation on this blog. But the javascript on my blog is not needed except for commenting.

I don't have too much to say about the jQuery version. But this was way more *low level* programming than Cappuccino.

## My conclusion

If you want to make an iPhone compatible web application just don't use Cappuccino yet.
If you want to make *simple* application like mine, I also believe, Cappuccino is a bit too much.

If you want to make a complex web oriented application, Cappuccino is a great choice. 
But you may have some difficulties to begin programming with it.

Finally, to terminate my web version of my widget, I'll give a try to Dashcode 3. 
It seems to be a good alternative to create web widgets.
I don't know if Dashcode 3 is portable on non webkit browser.
But if it is, it could be the end of projects like Cappuccino and Sproutcore.
