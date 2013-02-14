-----
isHidden:       false
menupriority:   1
kind:           article
published:     2010-06-19T00:44:50+02:00
title: jQuery popup the easy way
authorName: Yann Esposito
authorUri: yannesposito.com
tags: jQuery, javascript, popup, blog, web
-----

Here is a fast and easy way to create jQuery popup.

<code class="javascript" file="essai.js">
// --- code popup ---
function openPopup() {
    $(this).clone(false).appendTo($("#_code"));
    $("#_code").show();
}

function closePopup() {
    $("#_code").html("");
    $("#_code").hide();
}

function initCode() {
    $(".code").click(openPopup);
    $(".code").css({cursor: "pointer"});
    $('body').append('<div id="_code"></div>');
    $('#_code').css( { 'text-align': "justify", position: "fixed", 
                        left:0, top:0, width: "100%", height: "100%", 
                        "background-color": "rgba(0, 0, 0, 0.8)", 'z-index':2000, 'padding':'3px'} );
    $('#_code').hide();
    $('#_code').click(closePopup);
}
// --- end of code popup section ---
</code>

What does this code do?

At the loading of the page, I create a `div` as wide as the window. 
This `div` is a bit transparent. Then I hide it. I also take care to its `z-index` value to be sure it is behind all elements.

Then when we click on a `div` of class `code`, I copy the content into this new wide `div`, and I show it. 
Really simple but really efficient. 
No need to use a `jQuery` plugin.
