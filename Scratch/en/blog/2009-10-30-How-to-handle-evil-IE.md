-----
isHidden:       false
menupriority:   1
kind:           article
published: 2009-10-30
title: How to handle evil IE
author: Yann Esposito
authoruri: yannesposito.com
tags:  web, webdesign, jQuery
-----

For developer IE is a nightmare. This is why, I use a method to disable my standard CSS and enable a IE only CSS. I use jQuery to accomplish that.

<div><code class="javascript">
$(document).ready( function() {
    if ($.browser["msie"]) {
        // include the ie.js file
        $('head').append('<script type="text/javascript" src="/js/ie.js"></scr' + 'ipt>');
    }
});
</code></div>

<div><code class="javascript" file="ie.js">
// Remove all CSS I don't want to use on IE
$('link[rel=stylesheet]').each(function(i)
{
    if (this.getAttribute('href') == '/css/layout.css') 
        this.disabled = true;
    if (this.getAttribute('href') == '/css/shadows.css') 
        this.disabled = true;
    if (this.getAttribute('href') == '/css/gen.css')    
        this.disabled = true;
}) ;

// Append the CSS for IE only
$('head').append('<link rel="stylesheet" type="text/css" href="/css/ie.css"/>');

// I also add a message on top of the page
$('body').prepend('<div id="iemessage"><p><span class="fr"><em>Avec <a href="http://www.firefox.com"> Firefox </a> et <a href="http://www.apple.com/safari">Safari</a> cette page est bien plus jolie !</em></span><span class="en"><em>This page is far nicer with <a href="http://www.firefox.com"> Firefox </a> and <a href="http://www.apple.com/safari">Safari</a>!</em></span></p>.</div>');

</code></div>

That's it.
