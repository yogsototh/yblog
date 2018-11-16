-----
isHidden:       false
theme: brutalist
menupriority:   1
kind:           article
published: 2010-07-07
title: Do not use CSS gradient with Chrome
author: Yann Esposito
authoruri: yannesposito.com
tags:  CSS, web, programming, Chrome, Safari, Firefox
-----

Some [Reddit](http://reddit.com) users reported my website was really long to load and to scroll.
They thinks it was because of the '1px shadow' I apply on all the text.
I was a bit surprised, because I make some test into a really *slow* virtual machine. And all have always worked fine. In fact, what slow down so much are by order of importance:

1. Radial gradient on Chrome (not in Safari on Mac)
2. Box shadows on Firefox and Chrome

## Gradient

On Safari on Mac there is absolutely no rendering time problem. But when I use Chrome under Linux it is almost unusable.

Safari and Chrome use webkit, when you access my website with javascript enabled, an additionnal browser specific CSS is loaded. Until now I switched only between: IE, Mozilla and Webkit. Now I added one more special case for Chrome. Now I continue to use gradient for Safari but no more on Chrome.

I didn't tried to verify the efficiency of all new CSS 3 features. But I advise you not to use **`-webkit-gradient`** on Chrome. At least when the host is a Linux.

## Box Shadows

I also detected that **`-moz-box-shadow`** elements slow down the rendering on Firefox under Linux. But there was very few time rendering issue with Safari on Mac.

## Text Shadows

Many tell me to use text-shadows sparingly. But I believe it was not the real reason of the slow down. This is why I'll get them back.

## Conclusion

Do not use **`-webkit-gradient`** on Chrome browser yet.
Try to use **`-moz-box-shadow`** sparingly.
