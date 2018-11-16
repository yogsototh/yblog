-----
isHidden:       false
theme: brutalist
menupriority:   1
kind:           article
published: 2010-06-15
title: Get my blog engine
author: Yann Esposito
authoruri: yannesposito.com
tags:  blog, nanoc
-----

I published a *light* version of my blog engine based on [nanoc](http://nanoc.stoneship.org) yesterday night. By *light*, I mean a lighter, more portable CSS (without round border).
You can get it on [github.com](http://github.com/yogsototh/nanoc3_blog).

What this system provide?

* All [nanoc](http://nanoc.stoneship.org) advantages,
* Easy multi-language handling,
* Syntax Coloration for most languages,
* [intenseDebate](http://intensedebate.org) comments integration (asynchronous) ;
* Portable with and without javascript, XHTML Strict 1.0 / CSS3,
* Write in markdown format (no HTML editing needed),
* Typographic ameliorations (no ':' starting a line in French for example),
* [Graphviz](http://graphviz.org) graph generation integration.

---

# Main Documentation Page

# Use It NOW!

Once installed (follow the README.md instructions).

~~~~~~ {.zsh}
$ cd /root/of/nanoc3_blog
$ ./task/new_blog_entry Title of the blog
$ vi latest.md
$ ./task/recompile
~~~~~~

Now your website reside into the `output` directory.

---

# Documentation

## Useful things to know

### Multi-language

All files in `multi` are processed and copied in the `content` directory.
For each file in multi, each line starting by '`fr: `' are copied (without the `fr: ` into the `content/html/fr/` tree, but not into the `content/html/en` tree. File not starting by `fr: ` or `en: ` are copied in each destinations.

If you want to add another language, you'll have to modify `tasks/config`, and `config.yaml`, create a `content/html/xx` where `xx` is the language code.

### Edition & Rendering

#### additional keywords

You can separate multi content div using the: `n``ewcorps` directive (see examples).

You can create div using `b``egindiv(classname)`, `e``nddiv`. (See some existing blog entries for example). Use the class `intro` for the abstract part.

You can create nice description table using `<``desc>` (See source code for example).

#### Typography

In French all ':', ';', '!' and '?' are preceded automatically by `&nbsp`. This enable not to have a line starting by a single special character.

You can use small caps using `<sc>` tags. 

* `(c``)` is replaced by (c).
* `(r``)` is replaced by (r).
* `<``-` is replaced by <-.
* `-``>` is replaced by ->.

#### source code

To write source code you should use the following format:

~~~~~~ {.html}
~~~~~~ {.ruby}
The code
</cOde>
~~~~~~

The `file` attribute is not required.

### blog

If you want to make really long blog post, you can separate them into many files. To accomplish that, you simply have to make your files like:

<pre class="twilight">
multi/blog/2010-06-01-the-title.md
multi/blog/2010-06-01-the-title/second_part.md
multi/blog/2010-06-01-the-title/third_part.md
</pre>

### mobileme

All files are intended to be generated into the `output/Scratch` directory.
This was made like that to work nicely with iWeb organisation of websites.

### menu

The order of post is done using the `menupriority` meta-data in the header of the files.

You can hide some file from the menu by setting: `isHidden: true` in the header.

## Details

To know more about this blog engine, you should look at
[nanoc](http://nanoc.stoneship.org) project.

Then look at the files inside your project:

<desc>
README.md   : readme for the project (used by github) ::
latest.md   : symbolic link to the last blog entry ::
multi/      : Directory containing multi-language articles ::
tasks/      : scripts for website live ::
config.yaml : global configuration file ::
Rules       : generation rules ::
content/    : content files processed by nanoc ::
layouts/    : erb templates ::
lib/        : ruby libraries used to process files ::
output/     : website ::
Rakefile    : not mandatory for this blog ::
</desc>
