-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-06-19
title: Comment faire des popups en jQuery rapidement
author: Yann Esposito
authoruri: yannesposito.com
tags:  jQuery, javascript, popup, blog, web
-----

Voici une façon simple et rapide pour faire des popups avec jQuery.

~~~~~~ {.javascript}
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
~~~~~~

Que fait ce code ?

Au chargement de la page je crée un `div` grand comme toute la page avec un fond légèrement transparent que je cache. 
Je fais bien attention à son `z-index` pour qu'il soit devant tout le reste.

Puis lorsque l'on clique sur un `div` de class `code`, 
je recopie le contenu de celui-ci dans le grand `div` que je rend visible. 
Très simple mais très efficace. 
Pas besoin d'utiliser un plugin `jQuery`.
