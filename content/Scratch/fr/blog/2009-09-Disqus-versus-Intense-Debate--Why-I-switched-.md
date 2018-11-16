-----
menupriority:   1
theme: brutalist
kind:           article
published: 2009-09-28
title: Disqus contre Intense Debate (pourquois j'ai changé)
author: Yann Esposito
authoruri: yannesposito.com
tags:  disqus, intense debate, web, blog
-----

# [Disqus](http://disqus.com/) *vs.* [Intense Debate](http://intensedebate.com/)

J'ai écrit un article sur la façon dont j'ai essayé d'intégrer [Disqus](http://disqus.com). Mon problème majeur avec [Disqus](http://disqus.com/) c'était que ma page ne s'affichait pas correctement tant que les commentaire n'avait pas fini de s'afficher. Ça m'est arrivé plusieurs fois d'avoir ma page complètement bloquée parce que les serveurs de [Disqus](http://disqus.com/) ne répondait pas.
C'est pourquoi j'ai essayer de l'inclure de manière asynchrone. Cependant j'ai eu des [difficultés pour le faire fonctionner correctement](/Scratch/fr/blog/11_Load_Disqus_Asynchronously/).

De plus il n'a pas été trivial de faire en sorte que les commentaires soient commun à plusieurs pages différentes (chaque page à trois représentations différentes, une par language plus une version multi-langue).

Je dois reconnaître que je suis un peu triste de quitter [Disqus](http://disqus.com) parce que pour chacun de mes problèmes [giannii](http://giannii.com)  m'a aidé du mieux qu'il a pu. Cependant les problèmes que j'ai eu étaient inhérents à des choix de conceptions plus que de simples petits problèmes techniques.

Lorsque j'ai commencé à intégrer [Disqus](http://disqus.com/) je n'ai jamais essayé [Intense Debate](http://intensedebate.com). Maintenant que j'ai essayé je doit dire que je suis conquis. Il correspond exactement à ce que j'espérais de ce type de service. 

Pour le rendre complètement asynchrone il suffit de récupérer leur js commun et de remplacer la ligne suivante :

<div>
~~~~~~ {.javascript}
document.getElementsByTagName("head")[0].appendChild(commentScript);
~~~~~~
</div>

par (si vous utilisez jQuery) : 

<div>
~~~~~~ {.javascript}
$(document).ready( function() {
    document.getElementsByTagName("head")[0].appendChild(commentScript);
});
~~~~~~
</div>

## And the Winner is: [Intense Debate](http://intensedebate.com/)

 Pour conclure les avantages majeurs (pour moi) d'[Intense Debate](http://intensedebate.com/) par rapport à [Disqus](http://disqus.com/): 

  - Se charge de façon asynchrone ; ne bloque pas mon site web
  - Permet d'ajouter sans rien de plus des boutons comme "share to any" et les charge eux aussi de façon asynchrone.

Voilà.
