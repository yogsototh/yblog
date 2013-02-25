-----
isHidden:       false
menupriority:   1
kind:           article
published: 2009-10-22
title: Focus &gt; Minimalism
author: Yann Esposito
authoruri: yannesposito.com
tags:  jQuery, design, web
-----

I believe the goal researched by minimalism is <strong>Focus</strong>.  But I don't believe minimalism should be the goal. Focus should be the goal, and I believe minimalism isn't necessary to reach it.

This is why my design is not minimalist, but I decided to remove most of the navigation stuff of all pages of my website. May be I'll prefer to hide the menu only when you are on blog article. For now, I hide the menu everywhere on the website.

---

## technical details

For those who want the technical details behind the show/hide menu, here is the simple jQuery code.

The HTML: 

<div>
<pre><code class="html"><div id="menuButton"></div>
<div id="entete">#content of the menu</div>
</code></pre>
</div>

The CSS: 

<div><code class="css">
#menuButton {
  font-size: 2em;
  height: 2em;
  line-height: 1.8em;
  width: 2em;
  position: fixed;
  left: 0;
  top: 0; 
  z-index: 9001 }

#menuButton:hover {
  cursor: pointer; }

#entete {
  top: 5em;
  left: 0;
  position: fixed;
  width: 10em;
  z-index: 9000; }
</code></pre>
</div>

The javascript code (using jQuery)

<div>
<pre><code class="javascript">function hideMenu() {
    $('#entete').animate({left:"-10em"}, 500 );
    $('#menuButton').html('&rarr;');
}
function showMenu() {
    $('#entete').animate({left:"0em"}, 500 );
    $('#menuButton').html('&larr;');
}
function toggleMenu() {
    if ( $('#entete').css('left')=='-10em' ) {
        showMenu();
    } else {
        hideMenu();
    }
}
</code></pre>
</div>

And the result is shown in the top left corner of this website. 

