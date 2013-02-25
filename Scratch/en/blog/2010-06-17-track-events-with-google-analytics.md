-----
isHidden:       false
image: /Scratch/img/blog/2010-06-17-track-events-with-google-analytics/GA_events.png
menupriority:   1
kind:           article
published: 2010-06-17
title: Track Events with Google Analytics
subtitle: Asynchronous Complete Google Analytics with jQuery
author: Yann Esposito
authoruri: yannesposito.com
tags:  blog, javascript, jQuery, Google, analytics, analyser, User, Asynchronous
-----

Here is how to track all clicks on your website using google analytics asynchronously.

First in your <sc>html</sc> you need to use [jQuery](http://jquery.com) and a javscript file I named `yga.js`:

<pre><code class="html">    <script type="text/javascript" src="jquery.js"></script>
    <script type="text/javascript" src="yga.js"></script>
</code></pre>

And here is the `yga.js` file:

<pre><code class="javascript" file="yga.js">$(document).ready( function() {
    // add an event to all link for google analytics
    $('a').click(function () {
        // tell analytics to save event
        try {
            var identifier=$(this).attr('id') ;
            var href=$(this).attr('href')
            var label="";
            if ( typeof( identifier ) != 'undefined' ) {
                label=label+'[id]:'+identifier
                category='JSLink'
            }
            if ( typeof( href ) != 'undefined' ) {
                label=label+' [href]:'+href
                if ( href[0] == '#' ) {
                    category='Anchor';
                } else {
                    category='Link';
                }
            }
            _gaq.push(['_trackEvent', category, 'clicked', label]);
            // console.log('[tracked]: ' + category + ' ; clicked ; ' + label );
        }
        catch (err) {
            console.log(err);
        }

        // pause to allow google script to run
        var date = new Date();
        var curDate = null;
        do {
            curDate = new Date();
        } while(curDate-date < 300);
    });
});

var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-XXXXXXXX-1']);
_gaq.push(['_trackPageview']);

(function() {
 var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
 ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
 var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
 })();
</code></pre>

Replace the: `UA-XXXXXXXX-1` by your google analytics code and you're done.

To see what occurs, simply go in `Content` and `Event Tracking` as shown in the following screenshot:

blogimage("GA_events.png", "Where to find events tracking in google analytics interface")

Happy tracking!
