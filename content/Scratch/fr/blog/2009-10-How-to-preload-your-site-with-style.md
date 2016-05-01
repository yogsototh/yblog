-----
menupriority:   1
theme: scientific
image: /Scratch/img/loading.gif
kind:           article
published: 2009-10-03
title: Charger une page web avec style
author: Yann Esposito
authoruri: yannesposito.com
tags:  web, jQuery, webdesign
-----

## Exemple

Voici comment apparaissent mes pages pendant leur chargement.

<div id="demo" style="width:45%; position: relative; height: 8em; background: #333; background-position: 50% 50%; color: #fff; text-align: center; padding-top: 1em; margin-left: auto; margin-right: auto; border: solid 10px rgba(255,255,255,0.7); -webkit-border-radius: 1em; -moz-border-radius: 1em; border-radius: 1em; cursor: pointer; ">
    <p>Voilà ! Je suis chargée !</p>
    <p>Cliquez-moi dessus pour recommencer.</p>
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

J'ai d'abord essayé d'intégrer [queryLoader](http://www.gayadesign.com/diy/queryloader-preload-your-website-in-style/), mais il ne comblait pas mes besoins.

Ce plugin ajoutait un 'div' noir pour cacher le contenu du site. Cependant, comme le script doit être lancé à la fin du code source. Pendant un petit moment, on peut voir mon site en train de se mettre à jour.

Pour cacher ce petit 'artefact', voici comment je m'y suis pris.

## Code

D'abort il faut ajouter tout en haut du body cette fois un div qui va être le voile noir qui va tout cacher.

<div>
~~~~~~ {.html}
...
<body>
<div id="blackpage">
    content to display during the loading.
</div>
...
~~~~~~
</div>

et le CSS correspondant au div `#blackpage` : 

<div>
~~~~~~ {.css}
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
~~~~~~
</div>

ainsi que le code jQuery associé : 

<div>
~~~~~~ {.javascript}
$(document).ready(function(){
    $('#blackpage').fadeOut();
});
~~~~~~
</div>

Oui, c'est aussi simple que ça. Maintenant ajouter le `#blackpage` tout en haut de ma page me permet d'être certain de tout cacher pendant le chargement de la page.

J'espère que ça a pu vous être utile !

