$(document).ready( function() {
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
