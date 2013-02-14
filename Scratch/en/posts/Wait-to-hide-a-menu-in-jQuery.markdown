-----
isHidden:       false
menupriority:   1
kind:           article
published: 2009-10-26
title: Menu waiting to hide himself
authorName: Yann Esposito
authorUri: yannesposito.com
tags: jQuery, web, javascript, design 
-----

I discussed [earlier why I prefer to hide my navigation menu](/Scratch/en/blog/2009-10-Focus-vs-Minimalism). I finally decided to hide it only after a short time. Just the time needed for a user to see it. But how make it disappear only when it is not used for some time?

Here is how to accomplish that easily.

HTML: 

<pre><code class="html">    &lt;div id="menuButton"&gt;&lt;/div&gt;
    &lt;div id="entete"&gt;
        &lt;ul&gt;
            &lt;li&gt; menu item 1 &lt;/li&gt;
            ...
            &lt;li&gt; menu item n &lt;/li&gt;
        &lt;/ul&gt;
    &lt;/div&gt;
</code></pre>

CSS: 

<pre><code class="css">    #entete {
      top: 1em;
      left: 0;
      position: fixed;
      width: 10em;
      z-index: 2000; }
    
    #entete {
      top: 1em;
      height: 22em;
      left: 0;
      position: fixed;
      width: 10em; }
</code></pre>

Javascript: 

<div><code class="javascript">
var last=0;

// will hide the menu in 5 seconds
// if the variable 'last' has not changed its value
function autoHideMenu(value) {
    setTimeout(function(){
        if ( last == value ) { hideMenu(); }
    },5000);
}

$(document).ready( function() {
    // show the menu when the mouse is on
    // the good area
    $('#menuButton').hover(showMenu);

    // If the mouse is on the menu change the
    // value of 'last'
    // try to hide the menu when the mouse 
    // go out off the menu.
    $('#entete').hover(
        function(){last+=1;}, 
        function(){autoHideMenu(last);} );
    autoHideMenu(0);
});

// show / hide menu functions details

// move to the left
function hideMenu() { 
    $('#entete').animate({left:"-10em"}, 500 ); 
}

// move to right and will try to hide in 5 sec.
function showMenu() { 
    $('#entete').animate({left:"0em"}, 500 );
    last+=1;
    autoHideMenu(last);
}

</code></div>

Simple and lightweight. No timer (almost), no memory leak, no Date...
