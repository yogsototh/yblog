-----
menupriority:   1
kind:           article
published: 2009-10-22
title: <em>Focus</em> &gt; Minimalisme
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Cacher la navigation pour une meilleure concentration
tags:  jQuery, design, web
-----

 Je crois que le but du minimalisme est de facilité le <em><strong>Focus</strong></em> c'est-à-dire la concentration sur le contenu.   Je crois que le minimalisme doit être un moyen et pas une fin. Le <em>Focus</em> devrait être le but, et je pense que le minimalisme n'est pas obligatoire pour l'atteindre.

C'est pourquoi mon design n'est pas minimaliste. Mais j'ai décidé d'enlever la majorité des objets servant à la navigation pour améliorer l'attention sur l'article. Peut-être que plus tard, je préfèrerai laisser le menu dans les pages normales du site pour ne le cacher que dans les articles de blog.
Pour l'instant je le cache partout.

---

## Détails techniques

Pour ceux qui souhaitent connaître les détails techniques derrière le menu apparaissant/disparaissant, voici le code utilisant jQuery.

L'HTML : 

<div>
<pre><code class="html"><div id="menuButton"></div>
<div id="entete">#content of the menu</div>
</code></pre>
</div>

La CSS : 

<div><code class="css">
#menuButton {
  font-size: 2em;
  height: 2em;
  line-height: 1.8em;
  width: 2em;
  position: fixed;
  left: 0;
  top: 0; 
  z-index: 9001 }

#menuButton:hover {
  cursor: pointer; }

#entete {
  top: 5em;
  left: 0;
  position: fixed;
  width: 10em;
  z-index: 9000; }
</code></pre>
</div>

Le code javascript (utilisant jQuery)

<div>
<pre><code class="javascript">function hideMenu() {
    $('#entete').animate({left:"-10em"}, 500 );
    $('#menuButton').html('&rarr;');
}
function showMenu() {
    $('#entete').animate({left:"0em"}, 500 );
    $('#menuButton').html('&larr;');
}
function toggleMenu() {
    if ( $('#entete').css('left')=='-10em' ) {
        showMenu();
    } else {
        hideMenu();
    }
}
</code></pre>
</div>

Le résultat est visible dans le coin en haut à droite de cet article. 
