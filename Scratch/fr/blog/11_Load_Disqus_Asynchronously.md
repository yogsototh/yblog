-----
menupriority:   1
kind:           article
published: 2009-09-17
title: Load Disqus Asynchronously [en]
author: Yann Esposito
authoruri: yannesposito.com
tags:  disqus, web, javascript, intense debate, commentaires
-----

# Update

In fact this method works for old threads. But it fails to create new post threads. This is why I tried and be conquered by [intensedebate](http://intensedebate.com), as you can see in the bottom of this page.

Remark I didn't have any comment on my blog when I switched. Therefore my lack of influence was a good thing :-).

---

Before begining, I must state that I **love** Disqus. 

I know there is a similar blog entry at [Trephine.org](http://trephine.org/t/index.php?title=Site_improvements_-_fighting_with_Disqus). Here I just add a straight and easy way to load disqus asynchronously using jQuery. 

I also know there is a jQuery plugin to make just that. Unfortunately I had some issue with CSS.

*Now let's begin.*

---

# Why?

Why should I want to load the disqus javascript asynchronously?

  - Efficiency: I don't want my page to wait the complete execution of disqus script to load.
  - More independance: when disqus is down, my page is blocked!

---

# How?

I give a solution with jQuery, but I'm certain it will work with many other js library.

### Javascript

replace:

<div>
~~~~~~ {.javascript}
<script type="text/javascript" src="http://disqus.com/forums/YOUR_DISQUS_ID/embed.js"></script>
~~~~~~
</div>

by

<div>
~~~~~~ {.javascript}
window.disqus_no_style=true;
$(document).ready(function(){
    $.getScript("http://disqus.com/forums/YOUR_DISQUS_ID/embed.js");
});
~~~~~~
</div>

If you forget the `window.disqus_no_style=true;` then your page will be blank. Simply because without this option, the javascript use a `document.write` action after the document was closed, which cause a complete erasing of it.

### CSS

But with this option you still need to provide a CSS. This is why you have to copy the css code from the `embed.js` file and rewrite it in a CSS file. You can [download the CSS](/Scratch/en/blog/11_Load_Disqus_Asynchronously/code/original_disqus.css) I obtained.

---

Now it's done. I believe all should be fine but I just finished the manip for my own site only 1 hour ago. Therefore there should be some error, tell me if it is the case.
