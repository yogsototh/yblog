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

The "Like", "Tweet" or "+1" button will call a javascript.
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

Another more problematic aspect is, you launch a third party js on your website.
What if tomorrow twitter update their tweet button?

Using social network buttons:

- Pros:
    - help user share your website
    - give an indication of the popularity of the page she's looking at.
- Cons:
    - follow your user, privacy issue,
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

**Solution 1 (no javascript):**

``` html
<a target="_blank" href="https://twitter.com/home?status=$url$" class="social">Tweet this</a>
<a target="_blank" href="http://www.facebook.com/sharer/sharer.php?u=$url$" class="social">Like this</a>
<a target="_blank" href="https://plusone.google.com/_/+1/confirm?hl=en&url=$url$" class="social">+1 this</a>
```
Where `$url$` is the current url.

**Solution 2:**

If you don't want to write the url yourself, you could use some minimal js:

``` js
<script>
var url=document.location;
document.write(
  '<'+'a target="_blank" href="https://twitter.com/home?status='+url+'">Tweet this<'+'/a> - '
+ '<'+'a target="_blank" href="http://www.facebook.com/sharer/sharer.php?u='+url+'">Like this<'+'/a> - '
+ '<'+'a target="_blank" href="https://plusone.google.com/_/+1/confirm?hl=en&url='+url+'">+1 this<'+'/a>');
</script>
```

Here is the result:

<div style="text-align:center">
<script>
var url=document.location;
document.write(
  '<'+'a target="_blank" href="https://twitter.com/home?status='+url+'">Tweet this<'+'/a> - '
+ '<'+'a target="_blank" href="http://www.facebook.com/sharer/sharer.php?u='+url+'">Like this<'+'/a> - '
+ '<'+'a target="_blank" href="https://plusone.google.com/_/+1/confirm?hl=en&url='+url+'">+1 this<'+'/a>');
</script>
</div>

If you read below, you see nice icons.
I just used some nice font-face with icons.
Steal my %css at will.

I would really like people to use link instead of the full provided buttons.
