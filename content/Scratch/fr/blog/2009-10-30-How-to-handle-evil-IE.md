-----
menupriority:   1
theme: scientific
kind:           article
published: 2009-10-30
title: Une CSS pour IE seulement
author: Yann Esposito
authoruri: yannesposito.com
tags:  web, webdesign, jQuery
-----

Pour les développeur de site web Internet Explorer est un cauchemar. C'est pourquoi j'utilise un style complètement différent pour ce navigateur. Avec la librairie jQuery.

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

Voilà.
