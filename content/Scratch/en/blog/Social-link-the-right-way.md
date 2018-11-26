---
kind:           article
published:      2013-03-14
image: /Scratch/img/blog/Social-link-the-right-way/main.png
title: Social link the right way
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: brutalist
---
blogimage("main.png","Main image")

<div class="intro">

%tldr
Default share buttons track your users,
aren't uniform with your design,
use computer ressources
and slow down your web page loading.

Do it right.
Use static links instead.


If you don't want to read, just copy/paste this in your %html:

``` html
<div id="sociallinks">
  <a href="https://twitter.com/home?status=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Tweet this</a> -
  <a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Like this</a> -
  <a href="https://plus.google.com/share?url=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Share on G+</a>
</div>
<script>
(function(){window.addEventListener("DOMContentLoaded",function(){
    var url=document.location;
    var links=document.getElementById("sociallinks")
                      .getElementsByTagName('a');
    for (var i=0;i!=links.length;i++){
        links[i].setAttribute("href",
            links[i].href.replace('$url$',url));}})})();
</script>
```

</div>

## The problem

Ever been on a website and want to tweet about it?
Fortunately, the website might have a button to help you.
But do you really know what this button do?

The "Like", "Tweet" and "+1" buttons will call a javascript.
It will get access to your cookies.
It helps the provider of the button to know who you are.

In plain English, the "+1" button will inform Google
you are visiting the website, **even if you don't click on "+1"**.
The same is true for the "like" button for facebook and the "tweet this" button for twitter.

The problem is not only a privacy issue.
In fact (sadly %imho) this isn't an issue for most people.
These button consume computer ressources.
Far more than a simple link.
It thus slow down a bit the computer and consume energy.
These button could also slow down the rendering of your web page.

Another aspect is their design.
Their look and feel is mostly imposed by the provider.

The most problematic aspect in my opinion is to use a third party js on your website.
What if tomorrow twitter update their tweet button?
If the upgrade break something for only a minority of people, they won't fix it.
This could occur anytime without any notification.
They just have to add a `document.write` in their `js` you call asynchronously and **BAM!**
Your website is just an empty blank page.
And as you call many external ressources, it can be very difficult to find the origin of the problem.

**Using social network buttons:**

- Pros:
    - help user share your website,
    - can provide a popularity indicator to your users.
- Cons:
    - you help tracking your users,
    - generally doesn't follow the design of your website,
    - use more computer ressources,
    - slow down your website,
    - executing third party js can break things silently.

## Solutions

I will provide you two solutions with the following properties:

- Pros:
    - help user share your website,
    - doesn't follow your user,
    - use almost no computer ressource,
    - doesn't slow down your website,
    - doesn't execute any third party js on your website.
- Cons:
    - doesn't provide any popularity information.

**Solution 1 (no js):**

``` html
<a href="https://twitter.com/home?status=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   >Tweet this</a>

<a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   >Like this</a>

<a href="https://plus.google.com/share?url=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   >Share on G+</a>
```
But you have to replace `$url$` by the current %url.

**Solution 2 (Just copy/paste):**

If you don't want to write the %url yourself, you could use some minimal js:

``` html
<div id="sociallinks">
  <a href="https://twitter.com/home?status=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Tweet this</a> -
  <a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Like this</a> -
  <a href="https://plus.google.com/share?url=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Share on G+</a>
</div>
<script>
(function(){window.addEventListener("DOMContentLoaded",function(){
    var url=document.location;
    var links=document.getElementById("sociallinks")
                      .getElementsByTagName('a');
    for (var i=0;i!=links.length;i++){
        links[i].setAttribute("href",
            links[i].href.replace('$url$',url));}})})();
</script>
```

Here is the result:

<div style="text-align:center" class="nostar">

<div id="sociallinks">
  <a href="https://twitter.com/home?status=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Tweet this</a> -
  <a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Like this</a> -
  <a href="https://plus.google.com/share?url=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
     >Share on G+</a>
</div>
<script>
(function(){window.addEventListener("DOMContentLoaded",function(){
    var url=document.location;
    var links=document.getElementById("sociallinks")
                      .getElementsByTagName('a');
    for (var i=0;i!=links.length;i++){
        links[i].setAttribute("href",
            links[i].href.replace('$url$',url));}})})();
</script>

</div>

## Good looking solutions

If you don't want just text but nice icons.
You have many choices:

- Use images `<img src="..."/>` in the links.
- Use icon fonts

As the first solution is pretty straightforward, I'll explain the second one.

1. Download the icon font [here](http://blog.martianwabbit.com/post/4344642365.html)
2. put the font file(s) at some place (here 'fonts/social_font.ttf' relatively to your %css file)
3. Add this to your %css

``` css
@font-face
  font-family: 'social'
  src: url('fonts/social_font.ttf') format('truetype')
  font-weight: normal
  font-style: normal
.social
  font-family: social
```

Now add this to your %html:

**Solution 1 (without js):**

``` html
<a href="https://twitter.com/home?status=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#116;</a>
·
<a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#0096;</a>
·
<a href="https://plus.google.com/share?url=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#0103;</a>
```

**Solution 2 (same with a bit more js):**

``` html
<div id="sociallinksunicode">
<a href="https://twitter.com/home?status=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#116;</a>
·
<a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#0096;</a>
·
<a href="https://plus.google.com/share?url=$url$"
   target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#0103;</a>
</div>
<script>
(function(){window.addEventListener("DOMContentLoaded",function(){
    var url=document.location;
    var links=document.getElementById("sociallinksunicode")
                      .getElementsByTagName('a');
    for (var i=0;i!=links.length;i++){
        links[i].setAttribute("href",
            links[i].href.replace('$url$',url));}})})();
</script>
```

Here is the result:

<div style="font-size: 2em; text-align: center;" class="nostar">
<div id="sociallinksunicode">
<a href="https://twitter.com/home?status=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
    class="social">&#116;</a>
·
<a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
   class="social">&#0096;</a>
·
<a href="https://plus.google.com/share?url=$url$"
     target="_blank" rel="noopener noreferrer nofollow"
    class="social">&#0103;</a>
</div>
<script>
(function(){window.addEventListener("DOMContentLoaded",function(){
    var url=document.location;
    var links=document.getElementById("sociallinksunicode")
                      .getElementsByTagName('a');
    for (var i=0;i!=links.length;i++){
        links[i].setAttribute("href",
            links[i].href.replace('$url$',url));}})})();
</script>
</div>

## Conclusion

1. You get your design back,
2. You stop to help tracking people,
3. You use less computer ressources and more generally power ressources which is good for the planet,
4. Your web pages will load faster.

_ps_: On my personal website I continue to use Google analytics.
Therefore, Google (and only Google, not facebook nor twitter) can track you here.
But I might change this in the future.

