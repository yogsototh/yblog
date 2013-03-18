---
kind:           article
published:      2013-03-16
image: /Scratch/img/blog/Hakyll setup/main.png
en: title: Hakyll setup
fr: title: Hakyll setup
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">

en: %tldr How I use [hakyll](http://jaspervdj.be/hakyll).
en: Abbreviations, typography corrections, multi-language,
en: use `index.html`, etc...

fr: %tlal Comment j'utilise [hakyll](http://jaspervdj.be/hakyll).
fr: Abréviations, corrections typographiques, multi-language,
fr: utilisation d'`index.html`, etc...

</div>

The website your are reading is done with [Hakyll][hakyll]

[hakyll]: http://jaspervdj.be/hakyll

[Hakyll][hakyll] can be considered as a minimal %cms.
But more generally it is a way to organize file generation.

My current workflow is:

1. open a Terminal, launch my script `preview.sh`.
2. open my browser on [localhost:8000](http://localhost:8000).
3. Write (customized) markdown files
4. Time to time, I reload the browser to the corresponding file.
5. Return to 3 up until I am satisfied with the result.

Mainly, when I modify a markdown file, it generates an %html file.

It sounds easy, but there are a lot of decisions and parameters to think about.
I will show you which they are.
