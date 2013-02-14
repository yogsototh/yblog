-----
isHidden:       false
menupriority:   1
kind:           article
published:           2009-10-03T14:34:11+02:00
title: How to preload your site with style
authorName: Yann Esposito
authorUri: yannesposito.com
tags: web, jQuery, webdesign 
-----

## Example

Here is a live example of what appear while loading my pages.

<div id="demo" style="width:45%; position: relative; height: 8em; background: #333; background-position: 50% 50%; color: #fff; text-align: center; padding-top: 1em; margin-left: auto; margin-right: auto; border: solid 10px rgba(255,255,255,0.7); -webkit-border-radius: 1em; -moz-border-radius: 1em; border-radius: 1em; cursor: pointer; ">
    <p>Hello! I've finished loading!</p>
    <p>Click me to see me disapear again.</p>
    <div id="todisapear" style="color: #000; position:absolute;top:0;left:0;text-align: center; padding-top: 1em; width: 100%; background-color: #eee; height: 8em;">
    Loading...
    <img style="border: none; background-color: none; background: none" src="/Scratch/img/loading.gif" alt="loading logo"/>
    </div>
    <script>
    function Rabbit(){
        $('#todisapear')
            .show()
            .animate({opacity: 1.0},3000)
            .fadeOut();
    }
    $(document).ready(function(){
        $('#todisapear').animate({opacity: 1.0},3000).fadeOut();
        $('#demo').click(Rabbit);
    });
    </script>
</div>

I first tried to integrate [queryLoader](http://www.gayadesign.com/diy/queryloader-preload-your-website-in-style/), but it didn't fill my needs.

The plugin add a black div to hide all the content. But as the script had to be launched at the end of the source code my website show for a small time.

In order to hide this small artefact, here is how I do that.

## Code

In a first time, I added at the top of the body the div hiding all the content.

<div>
<code class="html">
...
<body>
<div id="blackpage">
    content to display during the loading.
</div>
...
</code>
</div>

and here is the associated CSS to `#blackpage`: 

<div>
<code class="css">
#blackpage
  top: 0 
  left: 0 
  width: 100%
  height: 100%
  margin-left: 0
  margin-right: 0
  margin-top: 0
  margin-bottom: 0
  position: absolute
  text-align: center
  color: #666
  padding-top: 10em
  background-color: #eee
  z-index: 9000
</code>
</div>

and the associated jQuery code: 

<div>
<code class="javascript">
$(document).ready(function(){
    $('#blackpage').fadeOut();
});
</code>
</div>

Yes, it is as simple as that. And, putting the `#blackpage` div at the top of my page, I ensure to hide anything while loading.

I hope it had helped you!
