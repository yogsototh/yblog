-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-06-15
title: Récupérez mon système de blog
author: Yann Esposito
authoruri: yannesposito.com
tags:  blog, nanoc
-----

J'ai publié une version *light* de mon système de blog hier soir. Par *light* il faut comprendre avec un CSS plus épuré et plus portable (sans les bords ronds).
Vous pouvez le récupérer sur [github.com](http://github.com/yogsototh/nanoc3_blog).

Que pouvez-vous attendre de ce système de blog ?

* Tous les avantages liés à [nanoc](http://nanoc.stoneship.org) ;
* Facilité de la gestion de plusieurs langues ;
* coloration syntaxique des codes sources pour la plupart des languages ;
* commentaires gérés avec [intenseDebate](http://intensedebate.org) de façon asynchrone ;
* très portable avec ou sans javascript, XHTML Strict 1.0 / CSS3 ;
* écrivez vos entrées au format Markdown (pas de HTML) ;
* des améliorations typographiques (pas de ':' en début de ligne en Français par exemple),
* entrez directement le code de graphes qui se génèreront automatiquement en image à l'aide de [Graphviz](http://graphviz.org).

Pour vous donner une idée plus précise, voici la documentation que j'ai faite (en anglais) pour accompagner le code.

---

# Main Documentation Page

Cette page est seulement en anglais désolé.

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
