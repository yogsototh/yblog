-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2009-09-28
title: Disqus versus Intense Debate (Why I switched)
author: Yann Esposito
authoruri: yannesposito.com
tags:  disqus, intense debate, web, blog
-----

# [Disqus](http://disqus.com/) *vs.* [Intense Debate](http://intensedebate.com/)

I made a blog entry about how I tried to integrate [Disqus](http://disqus.com). I had to wait Disqus comment to be displayed before loading correctly my page. This is why I tried to include it in a "non-blocking" way. Unfortunately, I had [difficulties to make it works correctly](/Scratch/en/blog/11_Load_Disqus_Asynchronously/). 

Furthermore, it was not trivial to make comment to be shared between multiple version of the same page (each page has three differents representations, one for each language and one more for the multi-language version).

I am a bit sad to quit [Disqus](http://disqus.com) because I must confess [giannii](http://giannii.com) had helped me has efficiently as he could. But the problem I had with disqus are inherent to some design choice not simply technical ones.

During the time I tried to integrate [Disqus](http://disqus.com/) I never tried [Intense Debate](http://intensedebate.com). Now that I have tried, i must confess it does exactly what I needed. 

In order to make it fully asynchronous, you've just to download their common js and replace the following line:

<div>
~~~~~~ {.javascript}
document.getElementsByTagName("head")[0].appendChild(commentScript);
~~~~~~
</div>

by: 

<div>
~~~~~~ {.javascript}
$(document).ready( function() {
    document.getElementsByTagName("head")[0].appendChild(commentScript);
});
~~~~~~
</div>

## And the Winner is: [Intense Debate](http://intensedebate.com/)

 To conclude, main advantages (for me) of [Intense Debate](http://intensedebate.com/) over [Disqus](http://disqus.com/): 

  - Load Asynchronously ; don't block my website
  - Add for free buttons like "share to any" and load them **asynchronously**.

Voilà.
