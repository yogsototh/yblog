---
kind:           article
published:      2013-03-14
image: /Scratch/img/blog/Social link the right way/main.png
title: Social link the right way
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">


%tlal Les réseaux sociaux utilisent leur boutons pour vous traquer.
Vous leur donnez cette possibilité gratuitement.
Celà va à l'encontre de l'intéret de vos utilisateurs tout en ralentissant votre site.
Ajoutez des boutons sociaux correctement en utilisant des liens statiques.

</div>

Ever been on a website and want to tweet about it?
Fortunately, the website might have a button to help you.
But do you really know what this button do?

The "Like", "Tweet" and "+1" buttons will call a javascript.
This javascript will then get access to your cookies.
It helps the provider of the button to know who you are.

In plain English, the "+1" button will inform Google
you are visiting the website, **even if you don't click on "+1"**.

The problem is not only a privacy issue.
In fact (sadly %imho) this isn't an issue for mos people.
There are more problems.
These button consume more computer ressource than a simple link.
It thus slow down a bit the computer.
These button could also slow down the rendering of your web page.

Another aspect is their design.
I generally prefer to master the look and feel of the objects on my pages.

The most problematic aspect in my opinion is the third party js on your website.
What if tomorrow twitter update their tweet button?
They could broke something.
And if you are in a minority for whom the upgrade broke something.
They wouldn't change it for you.
This could occur anytime without any notification.

**Using social network buttons:**

- Pros:
    - help user share your website
    - can provide a popularity indicator to your users
- Cons:
    - you help tracking your users,
    - generally doesn't follow the design of your website,
    - use more computer ressources,
    - slow down your website
    - executing third party js, a silent update could break your website.

I will provide you two solutions with the following properties:

- Pros:
    - help user share your website
    - doesn't follow your user
    - use almost no computer ressource
    - doesn't slow down your website
    - doesn't execute any third party js on your website
- Cons:
    - doesn't provide any popularity information

**Solution 1 (no js):**

``` html
<a href="https://twitter.com/home?status=$url$"
    target="_blank">Tweet this</a>

<a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
    target="_blank">Like this</a>

<a href="https://plusone.google.com/_/+1/confirm?hl=en&url=$url$"
    target="_blank">+1 this</a>
```
But you have to replace `$url$` by the current %url.

**Solution 2 (Just copy/paste):**

If you don't want to write the %url yourself, you could use some minimal js:

``` js
<script>
var url=document.location;
document.write(
  '<'+'a href="https://twitter.com/home?status='+url+'" '
    +'target="_blank">Tweet this<'+'/a> - '

+ '<'+'a href="http://www.facebook.com/sharer/sharer.php?u='+url+'" '
    +'target="_blank">Like this<'+'/a> - '

+ '<'+'a href="https://plusone.google.com/_/+1/confirm?hl=en&url='+url+'" '
    +'target="_blank">+1 this<'+'/a>');
</script>
```

Here is the result:

<div style="text-align:center">
<script>
var url=document.location;
document.write(
  '<'+'a href="https://twitter.com/home?status='+url+'" '
    +'target="_blank">Tweet this<'+'/a> - '

+ '<'+'a href="http://www.facebook.com/sharer/sharer.php?u='+url+'" '
    +'target="_blank">Like this<'+'/a> - '

+ '<'+'a href="https://plusone.google.com/_/+1/confirm?hl=en&url='+url+'" '
    +'target="_blank">+1 this<'+'/a>');
</script>
</div>

If you don't want just text but nice icons.
You have many choices:

- Use images `<img src="..."/>` in the links.
- Use icon fonts

As the first solution is pretty straightforward, I'll explain the second one.

1. Download the icon font [here →](http://blog.martianwabbit.com/post/4344642365.html)
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
    target="_blank"
    class="social">&#116;</a>
·
<a href="http://www.facebook.com/sharer/sharer.php?u=$url$"
   target="_blank"
   class="social">&#0096;</a>
·
<a href="https://plusone.google.com/_/+1/confirm?hl=en&url=$url$"
    target="_blank"
    class="social">&#0103;</a>
```

**Solution 2 (just copy/paste):**

``` html
<script>
var url=document.location;
document.write(
    '<a href="https://twitter.com/home?status='+url+'"'
        + ' target="_blank"'
        + ' class="social">&#116;</a>'
    + '·'
    + '<' + 'a href="http://www.facebook.com/sharer/sharer.php?u='+url+'"'
       + ' target="_blank"'
       + ' class="social">&#0096;<'+'/a>'
    + '·'
    + '<a href="https://plusone.google.com/_/+1/confirm?hl=en&url='+url+'"'
        + ' target="_blank"'
        + ' class="social">&#0103;<'+'/a>');
</script>
```

Here is the result: <span style="font-size: 2em"><script>
document.write(
    '<a href="https://twitter.com/home?status='+url+'"'
        + ' target="_blank"'
        + ' class="social">&#116;</a>'
    + '·'
    + '<' + 'a href="http://www.facebook.com/sharer/sharer.php?u='+url+'"'
       + ' target="_blank"'
       + ' class="social">&#0096;<'+'/a>'
    + '·'
    + '<a href="https://plusone.google.com/_/+1/confirm?hl=en&url='+url+'"'
        + ' target="_blank"'
        + ' class="social">&#0103;<'+'/a>');
</script></span>

You will be satisfied knowing you are making something good.

1. You get back your design.
2. Your users are a bit less tracked.
3. You use less computer ressources and then generally power ressources which is good for the planet.
4. Your web pages will load faster.

On my personal website I continue to use Google analytics.
Therefore, Google (and only Google, not facebook nor twitter) can track you here.
But I might change this as well in the future.

