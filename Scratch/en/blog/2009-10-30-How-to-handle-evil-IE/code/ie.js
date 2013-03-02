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

