-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-06-14
title: multi language choices
authorName: Yann Esposito
authorUri: yannesposito.com
tags: multi-language, blog
-----

I translate most of my blog entries in French and English.
Most people advice me to have one file per language. Generally it ends with:

<pre class="twilight">
Bonjour, 

voici un exemple de texte en français.
[image](url)
</pre>

<pre class="twilight">
Hello, 

here is an example of english text.
[image](url)
</pre>

This way of handling translations force you to write completely an article in one language, copy it, and translate it.

However, most of time, there are common parts like images, source code, etc...
When I want to correct some mistake on these parts, I have to make twice the work. With sometimes adding another mistake in only one language.

This is why I preferred to handle it differently.
I use *tags* on a single file.
Finally my files looks like:

<pre class="twilight">
 fr:   Bonjour, 
 en:   Hello, 

 en:   here is an example of english text.
 fr:   voici un exemple de texte en français.
[image](url)
</pre>

As I edit my files with [vim](http://vim.org), it is really easy to add `fr:` or `en:` at some line's beginning using the useful `C-v`.
However [nanoc](http://nanoc.stoneship.org) was conceived to be used for one language only. Or to be used with the first method. I tried to adapt nanoc to my usage. But after a while, I found it easier to pre-filter the nanoc work by a simple script. My script transform my file into two new files. And all work like a charm.

You can get my blog code source (without most of articles) at [github.com/yogsototh/Scratch](http://github.com/yogsototh/Scratch). 
