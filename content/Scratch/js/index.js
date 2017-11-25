function useCssAnimations() {
    if ( ! /ip(od|hone)/.test(userAgent) ) {
        if (! /chrome/.test(userAgent) ) {
            // Disable animation in chrome
            // It seems they have some progress to do :(
            $('head').append('<link rel="stylesheet" href="/Scratch/css/dynamic.css" type="text/css" />');
        }
    }
}

// --- Google analytics ---
function analytics() {
    var admin = $.cookie('admin');
    if (! admin) {
        // console.log("you're logged by google analytics");
        // add an event to all link for google analytics
        $('a').click(function () {
            // tell analytics to save event
            try {
                var identifier=$(this).attr('id') ;
                var href=$(this).attr('href');
                var label="";
                if ( typeof( identifier ) != 'undefined' ) {
                    label=label+'[id]:'+identifier;
                    category='JSLink';
                }
                if ( typeof( href ) != 'undefined' ) {
                    label=label+' [href]:'+href;
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
    } else {
        console.log("[WARNING] you're HIDDEN to analytics");
    }
}

var userAgent;

function detectClient() {
    userAgent = navigator.userAgent.toLowerCase();
    if (/ip(od|hone)/.test(userAgent)) {
        $('head').append('<meta name="viewport" content="width=device-width; initial-scale=1.0;">');
    }
    return userAgent;
}

// -----------------------------------
var pref='Scratch/css';
var styleindex=0;
var styles=[ pref+'/scientific.css'
           , pref+'/brutalist.css'
		       , pref+'/modern.css'
		       , pref+'/darkmodern.css'
           ];

// -- fix for ie (I HATE YOU Internet Explorer!)
if (!('indexOf' in Array.prototype)) {
    Array.prototype.indexOf = function(find, i /*opt*/) {
        if (i===undefined) i=0;
        if (i<0) i+= this.length;
        if (i<0) i= 0;
        for (var n= this.length; i<n; i++)
            if (i in this && this[i]===find)
                return i;
        return -1;};}
if (!('forEach' in Array.prototype)) {
    Array.prototype.forEach = function(action, that /*opt*/) {
        for (var i=0, n=this.length; i<n; i++)
            if (i in this)
                action.call(that, this[i], i, this);};}

function badPref() {
    var badPrefix=true;
    styles.forEach(function(trystyle){
		if ($('link[href="'+trystyle+'"]').length > 0) {
            badPrefix=false; }});
    return badPrefix;
}

var maxDepth=10;
while (badPref() && (maxDepth-->0)) {
    pref="../" + pref;
    styles=[ pref+'/scientific.css'
           , pref+'/brutalist.css'
		       , pref+'/modern.css'
		       , pref+'/darkmodern.css'
           ];
}
function reloadStylesheets() {
	var queryString = '?reload=' + new Date().getTime();
	$('link[rel="stylesheet"]').each(function () {
		this.href = this.href.replace(/\?.*|$/, queryString);
	});
}
function switchCssTo(style) {
	// try each style
	styles.forEach(function(trystyle){
		if ($('link[href="'+trystyle+'"]').length > 0) {
			$('link[href="'+trystyle+'"]').attr('href',style);
			styleindex=styles.indexOf(style);
		}
	});
	// save the preference
	$.cookie('css',style);
}
// Ability to switch css by clicking on #swtichcss
function switchcss() {
	// If the user has saved a preference
	// load its preferred style
	if ( $.cookie('css') !== null ) {
		setTimeout(function(){switchCssTo($.cookie('css'));}, 1000);
	}
	$('#switchcss').click(function(){
		switchCssTo(styles[ (styleindex+1) % styles.length ]);
		});
}

function animatedToTop() {
    $('#totop a').click(function(){
        $('body,html').animate({scrollTop:0},800);});
}
function animatedToMenu() {
    $('.tomenu a').click(function(){
        $('body,html').animate({
            scrollTop: $('#navigation').offset().top - ($(window).height()/2)},800);});
}

// Ce que l'on va lancer à l'init.
$(document).ready(function() {
    var client=detectClient();
    if ( ! /msie/.test(client) ) { useCssAnimations(); }
    $('#blackpage').fadeOut('slow',function(){ $('#blackpage').remove(); });
    animatedToTop();
    animatedToMenu();
    analytics();
    switchcss();
    $.scrollDepth();
});

$(window).bind("load", function() {
	// lorsque toutes les ressources ont ete chargees
    if (/windows/.test(navigator.userAgent.toLowerCase())) {
        $('head').append('<link rel="stylesheet" type="text/css" href="/Scratch/css/windows.css"/>');
	}
});


// --- Google Analytics ---
if ( ! $.cookie('admin') ) {
    var _gaq = _gaq || [];
    _gaq.push(['_setAccount', 'UA-10612400-1']);
    _gaq.push(['_trackPageview']);

    (function() {
     var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
     ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
     var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
     })();
} else {
    console.log("[WARNING] you're HIDDEN to analytics");
}

