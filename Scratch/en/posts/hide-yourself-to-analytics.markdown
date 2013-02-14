-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-06-17
title: Hide Yourself to your Analytics
authorName: Yann Esposito
authorUri: yannesposito.com
tags: analytics, statistics, hide, blog, jQuery, javascript
-----

This is a way not to count your own visits to your blog.
First you should look on how [I handle analytics](/Scratch/en/blog/2010-06-17-track-events-with-google-analytics). All analytics are handled in one javascript file, this make things really convenient.

Then you need to know my method use the `jquery-cookie`.

I check if the key `admin` is not set in the cookie before adding the visit.



<pre><code class="javascript">    var admin = $.cookie('admin');
    if (! admin) {
        // put your analytics code here
    } else {
        console.log("[WARNING] you're HIDDEN to analytics");
    }
</code></pre>



then create two <sc>html</sc> files. One to hide:



<pre><code class="html">&lt;?xml version="1.0" encoding="utf-8"?&gt;
&lt;!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"&gt;
&lt;html xmlns="http://www.w3.org/1999/xhtml" lang="fr" xml:lang="fr"&gt;
    &lt;head&gt;
        &lt;meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /&gt;
        &lt;script type="text/javascript" src="jquery.js"&gt;&lt;/script&gt;
        &lt;script type="text/javascript" src="jquery.cookie.js"&gt;&lt;/script&gt;
        &lt;script&gt;
            $(document).ready(function(){
                $.cookie('admin',1);
                $('#info').html('Analytics can no more see you.')
            });
        &lt;/script&gt;
        &lt;title&gt;Hide to analytics&lt;/title&gt;
    &lt;/head&gt;
    &lt;body&gt;
        &lt;div id="info"&gt;&lt;/div&gt; 
    &lt;/body&gt;
&lt;/html&gt;
</code></pre>



the other to be visible again (it can be useful):



<pre><code class="html">&lt;?xml version="1.0" encoding="utf-8"?&gt;
&lt;!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"&gt;
&lt;html xmlns="http://www.w3.org/1999/xhtml" lang="fr" xml:lang="fr"&gt;
    &lt;head&gt;
        &lt;meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /&gt;
        &lt;script type="text/javascript" src="jquery.js"&gt;&lt;/script&gt;
        &lt;script type="text/javascript" src="jquery.cookie.js"&gt;&lt;/script&gt;
        &lt;script&gt;
            $(document).ready(function(){
                $.cookie('admin',null);
                $('#info').html('Analytics can see you.')
            });
        &lt;/script&gt;
        &lt;title&gt;Hide to analytics&lt;/title&gt;
    &lt;/head&gt;
    &lt;body&gt;
        &lt;div id="info"&gt;&lt;/div&gt; 
    &lt;/body&gt;
&lt;/html&gt;
</code></pre>



Now accessing these files with you browser you can *hide* or *appear* in your statistics. You just have to think to access these file from all you browser.

