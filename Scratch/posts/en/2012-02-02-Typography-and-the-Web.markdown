-----
isHidden:       false
menupriority:   1
kind:           article
created:     2012-02-02T11:53:00+02:00
title: Typography and the Web
authorName: Yann Esposito
authorUri: yannesposito.com
tags: web, design, typography
macros: latex: '<span style="text-transform: uppercase">L<sup style="vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em">a</sup>T<sub style="vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em">e</sub>X</span>' xelatex: '<span style="text-transform: uppercase">X<sub style="vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em">&#x018E;</sub>L<sup style="vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em">a</sup>T<sub style="vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em">e</sub>X</span>' css: <span class="sc">css</span>
-----
blogimage("first_sc_screenshot.png", "Screenshot of first in small caps with and without ligatures.")

<div class="intro">

%tldr Web typography sucks and we'll have to wait forever before it will be fixed.

</div>

I stumbled upon [open typography](http://opentypography.org/). Their main message is:

> «There is no reason to wait for browser development to catch up.
> We can all create better web typography ourselves, today.»

As somebody who tried to make my website using some nice typography features and in particular _ligatures_, I believe this is wrong.

I already made an automatic system which will detect and replace text by their ligatures in my blog. But this I never published this on the web and this is why.

First, what is a ligatures?

blogimage("ligatures.png", "A ligature example")

What is the problem between the Web and ligatures?
The first one is: you cannot search them. For example, try to search the word "first":

- first ← No ligature, no problem[^1]
- <span style="color: #800">ﬁ</span>r<span style="color: #800">ﬆ </span> ← ligature nice but unsearchable

[^1]: In fact, you might see a ligature and the search works because I now use some CSS ninja skills: `text-rendering: optimizelegibility`. But it also works because I use the right font; Computer Modern. Steal my CSS at will.

The second one is the rendering, for example, try to use a ligature character with small caps:

- <sc>first</sc>
- <sc><span style="color:#800">ﬁ</span>r<span style="color:#800">ﬆ</span></sc>

Here is a screenshot of what I see:

blogimage("first_sc_screenshot.png", "Screenshot of first in small caps with and without ligatures.")

The browser isn't able to understand that the ligature character "<span style="color:#800">ﬁ</span>" should render as <sc>fi</sc> when rendered in small caps. And one part of the problem is you should choose to display a character in small caps using %css.

This way, how could you use a ligature Unicode character on a site on which you could change the %css?

Let's compare to %latex.

blogimage("first_latex_screenshot.png", "LaTeX screenshot")

If you take attention to detail, you'll see the first "first" contains a ligature. Of course the second render nicely. The code I used were:

<code class="latex">
\item first
\item {\sc first}
</code>

%latex was intelligent enough to create himself the ligatures when needed.

The "<span style="color:#800">ﬆ</span>" ligature is rare and not rendered in %latex by default. But if you want you could also render rare ligature using %xelatex:

blogimage("xelatex_ligatures.jpg","XeLaTeX ligatures")

I took this image from the excellent article of [Dario Taraborelli](http://nitens.org/taraborelli/latex#rare).

Clearly fix the rendering of ligature in a browser is a difficult task.
Simply imagine the number of strange little exceptions:

- The text is rendered in small caps, I cannot use ligature.
- The current word contains a ligature unicode character, I should search for ligature in this one.
- The current font does not defined the ligature unicode character, we shouldn't use it, etc
- A javascript command changed the CSS, I should verify if I had to revert the insertion of ligatures characters
- etc...

Nonetheless if someone has a solution, I would be happy to hear about it.
