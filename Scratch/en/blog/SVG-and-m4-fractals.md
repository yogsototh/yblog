-----
isHidden:       false
image: /Scratch/img/blog/SVG-and-m4-fractals/main.png
menupriority:   1
kind:           article
published: 2011-10-20
title: Increase the power of deficient languages.
subtitle: Fractals with SVG and m4
author: Yann Esposito
authoruri: yannesposito.com
tags:  m4, SVG, XSLT, XML, design, programming, fractal
-----
blogimage("main.png","Yesod logo made in SVG and m4")

<div class="intro">

%tldr How to use m4 to increase the power of deficient languages. Two examples: improve %xslt syntax and make fractal with %svg.

</div>

%xml was a very nice idea about structuring data.
Some people where so enthusiastic about %xml they saw it everywhere.
The idea was: the future is %xml.
Then some believed it would be a good idea to invent many %xml compatible format and even programming languages with %xml syntax.

Happy! Happy! Joy! Joy! 

Unfortunately, %xml was made to transfert structured data.
Not a format a human should see or edit directly.
The sad reality is %xml syntax is simply verbose and ugly.
Most of the time it shouldn't be a problem, as nobody should see it.
In a perfect nice world, we should never deal directly with %xml but only use software which deal with it for us.
Guess what?
Our world isn't perfect. Too sad, a bunch of developer have to deal directly with this ugly %xml.

Unfortunately %xml isn't the only case of misused format I know. 
You have many format for which it would be very nice to add variables, loops, functions...

If like me you hate with passion %xslt or writing %xml,
I will show you how you could deal with this bad format
or language.

## The %xslt Example

Let's start by the worst case of misused %xml I know: %xslt.
Any developer who had to deal with %xslt know how horrible it is.

In order to reduce the verbosity of such a bad languages, there is a way.
**`m4`**. Yes, the preprocessor you use when you program in `C` and `C++`.

Here are some example: 

- Variable, instead of writing the natural `myvar = value`, here is the <sc>xslt</sc> way of doing this:

<code class="xml">
<xsl:variable name="myvar" select="value"/>
</code></pre>

- Printing something. Instead of `print "Hello world!"` here is the <sc>xslt</sc> equivalent:

<code class="xml">
<xsl:text 
    disable-output-escaping="yes"><![CDATA[Hello world!
]]></xsl:text>
</code></pre>

- printing the value of a variable, instead of `print myvar` the <sc>xslt</sc> is:

<code class="xml">
<xslt:value-of select="myvar"/>
</code></pre>

- Just try to imagine how verbose it is to declare a function with this language.

## The cure (m4 to the rescue)

<code class="xml">
<?xml version="1.0" standalone="yes"?> <!-- YES its %xml -->
<!-- ← start a comment, then write some m4 directives:

define(`ydef',`<xsl:variable name="$1" select="$2"/>')
define(`yprint',`<xsl:text disable-output-escaping="yes"><![CDATA[$1]]></xsl:text>')
define(`yshow',`<xsl:value-of select="$1"/>')

-->
<!-- Yes, %xml sucks to be read -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- And it sucks even more to edit -->
<xsl:template match="/">
    ydef(myvar,value)
    yprint(Hello world!)
    yshow(myvar)
</xsl:template>
</code></pre>

Now just compile this file:

<code class="zsh">
m4 myfile.m4 > myfile.xslt
</code></pre>

Profit! Now <sc>xslt</sc> is more readable and easier to edit!

## The cool part: Fractals!

%svg is an %xml format used to represent vector graphics, it even support animations.
At its beginning some people believed it would be the new Flash. Apparently, it will be more canvas + js.

Let me show you the result:

<a href="blogimagedirmain.svg">
blogimage("main.png","Yesod logo made in SVG and m4")
Click to view directly the %svg. It might slow down your computers if you have an old one.
</a>

The positionning of the "esod" text with regards to the reversed "λ" was done by changing position in firebug. I didn't had to manually regenerate to test.

Making such a fractal is mostly:

1. take a root element
2. duplicate and transform it (scaling, translating, rotate)
3. the result is a sub new element.
4. repeat from 2 but by taking the sub new element as new root.
5. Stop when recursion is deep enough.

If I had to do this for each step, I had make a lot of copy/paste in my %svg, because the transformation is always the same, but I cannot say, use transformation named "titi". Then instead of manually copying some %xml, I used m4

and here is the commented code:

<code class="xml" file="yesodlogo.m4">
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!--
     M4 Macros
define(`YTRANSFORMONE', `scale(.43) translate(-120,-69) rotate(-10)')
define(`YTRANSFORMTWO', `scale(.43) translate(-9,-67.5) rotate(10)')
define(`YTRANSFORMTHREE', `scale(.43) translate(53,41) rotate(120)')
define(`YGENTRANSFORM', `translate(364,274) scale(3)')
define(`YTRANSCOMPLETE', `
    <g id="level_$1">
        <use style="opacity: .8" transform="YTRANSFORMONE" xlink:href="#level_$2" />
        <use style="opacity: .8" transform="YTRANSFORMTWO" xlink:href="#level_$2" />
        <use style="opacity: .8" transform="YTRANSFORMTHREE" xlink:href="#level_$2" />
    </g>
    <use transform="YGENTRANSFORM" xlink:href="#level_$1" />
')
 -->
<svg 
    xmlns="http://www.w3.org/2000/svg" 
    xmlns:xlink="http://www.w3.org/1999/xlink"
    x="64" y="64" width="512" height="512" viewBox="64 64 512 512"
    id="svg2" version="1.1">
    <g id="level_0"> <!-- some group, if I want to add other elements -->
        <!-- the text "λ" -->
        <text id="lambda" 
            fill="#333" style="font-family:Ubuntu; font-size: 100px"
            transform="rotate(180)">λ</text>
    </g>
    <!-- the text "esod" -->
    <text 
        fill="#333" 
        style="font-family:Ubuntu; font-size: 28px; letter-spacing: -0.10em" 
        x="-17.3" 
        y="69" 
        transform="YGENTRANSFORM">esod</text>
    <!-- ROOT ELEMENT -->
    <use transform="YGENTRANSFORM" xlink:href="#level_0" />

    YTRANSCOMPLETE(1,0) <!-- First recursion -->
    YTRANSCOMPLETE(2,1) <!-- deeper -->
    YTRANSCOMPLETE(3,2) <!-- deeper -->
    YTRANSCOMPLETE(4,3) <!-- even deeper -->
    YTRANSCOMPLETE(5,4) <!-- Five level seems enough -->
</svg>
</code></pre>

and I compiled it to <sc>svg</sc> and then to <sc>png</sc> with:

<code class="zsh">
m4 yesodlogo.m4 > yesodlogo.svg && convert yesodlogo.svg yesodlogo.png
</code></pre>

The main λ is duplicated 3 times. Each transformation is named by: `YTRANSFORMONE`, `YTRANSFORMTWO` and `YTRANSFORMTHREE`.

Each transformation is just a similarity (translate + rotation + scale).

Once fixed, we should now simply copy and repeat for each new level.

Now it is time to talk about where the magic occurs: `YTRANSCOMPLETE`.
This macro takes two arguments.
The current depth and the preceding one.
It duplicates using the three transformations the preceding level.

- At level 0 there is only one λ,
- at level 1 there is 3 λ,
- at level 2 there is 9 λ
- etc... 

At the final 5th level there is 3<sup>5</sup>=243 λ.
All level combined have 3<sup>6</sup>-1 / 2 = 364 λ.

I could preview the final result easily. 
Without the macro system, I would have to make 5 copy/paste + modifications for each try.

## Conclusion

It was fun to make a fractal in <sc>svg</sc>, but the interesting part is how to augment the power of a language using this preprocessor method. 
I used the <sc>xslt</sc> trick at work for example.
I also used it to make include inside obscure format.
If all you want is to generate  a minimal static website withou using nanoc, jekyll or hakyll (ther are plenty other alternatives). You can consider using m4 to generate your <sc>html</sc> instead of copy/paste the menu and the footer, or using AJAX.

Another usage I thouhgt about is to use m4
to organize languages such as brainfuck.
