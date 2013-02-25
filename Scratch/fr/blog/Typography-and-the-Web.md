-----
isHidden:       false
image: /Scratch/img/blog/Typography-and-the-Web/first_sc_screenshot.png
menupriority:   1
kind:           article
published: 2012-02-02
title: La typography et le Web
author: Yann Esposito
authoruri: yannesposito.com
tags:  web, design, typography
-----
blogimage("first_sc_screenshot.png", "Screenshot of first in small caps with and without ligatures.")

<div class="intro">

%tlal La typography sur le web est pourrie et nous ne somme pas près de voir ce problème réparé.

</div>

Je suis tombé sur ce site: [open typography](http://opentypography.org/). Leur message principal est :

> «There is no reason to wait for browser development to catch up.
> We can all create better web typography ourselves, today.»

ou en français :

> «Nous ne somme pas obligé d'attendre le développement des navigateurs.
> Nous pouvons créer un web avec une meilleure typographie aujourd'hui.»

Comme quelqu'un qui a déjà essayé d'améliorer la typographie de son site web, et en particulier des _ligatures_, je crois que c'est faux.

J'ai déjà écrit un système automatique qui détecte et ajoute des ligatures en utilisant des caractères unicode.
Cependant je n'ai jamais publié cette amélioration sur le web et voilà pourquoi :

Tout d'abord, qu'est-ce qu'un ligature ?

blogimage("ligatures.png", "A ligature example")

Quel est le problème des ligatures sur le web ?
Le premier c'est que vous ne pouvez pas chercher les mots qui contiennent ces ligatures. Par exemple essayez de chercher le mot "first".

- first ←  Pas de ligature, pas de problème[^1].
- <span class="red">ﬁ</span>r<span class="red">ﬆ </span> ← Une jolie ligature, mais introuvable avec une recherche (<code>C-f</code>).

[^1]: En réalité, vous devriez pouvoir voir une ligature. Maintenant j'utilise : `text-rendering: optimizelegibility`. Le rendu est correct parce que j'utilise une fonte correct, à savoir Computer Modern de Donald Knuth.

Le second problème est le rendu. Par exemple, essayer d'utiliser un charactère de ligature en petites capitales :

- <sc>first</sc>
- <sc><span class="red">ﬁ</span>r<span class="red">ﬆ</span></sc>

Voici une capture d'écran pour que vous voyez ce que je vois :

blogimage("first_sc_screenshot.png", "Screenshot of first in small caps with and without ligatures.")

Le navigateur est incapable de comprendre que le caractère de ligature "<span class="red">ﬁ</span>" doit être rendu comme <sc>fi</sc> lorsqu'il est en petites capitales. 
Et une part du problème est que l'on peut décider d'écrire en petite majuscule dans le %css.

Comment par exemple utiliser un charactère de ligature unicode sur un site qui possède différents rendus via différentes %css ?

Comparons à %latex

blogimage("first_latex_screenshot.png", "LaTeX screenshot")

Si vous faites attention au détail, vous constaterez que le premier "first" contient une ligature. Bien entendu la deuxième ligne est affichée correctement. Le code que j'ai utilisé pour avoir ce rendu est simplement :

<pre><code class="latex">\item first
\item {\sc first}
</code></pre>

%latex a été suffisamment intelligent pour créer les ligatures si nécessaire.

La ligature "<span class="red">ﬆ</span>" est rare et n'est pas rendu par défaut par %latex. 
Si vous voulez voir des ligatures rares, vous pouvez utiliser %xelatex:

blogimage("xelatex_ligatures.jpg","XeLaTeX ligatures")

J'ai copié cette image de l'excellent article de [Dario Taraborelli](http://nitens.org/taraborelli/latex#rare).

Clairement il sera difficile aux navigateurs de corriger ces problèmes.
Imaginez le nombre de petites exceptions.

- Le texte est en petites capitales, je ne dois pas utiliser de ligatures.
- Le mot courant contient un caractère de ligature, je ne dois pas chercher d'autre ligature dans ce mot.
- La fonte n'a pas défini de caractère unicode pour la ligature, je ne dois pas l'utiliser.
- Une commande javascript a modifé le CSS, je dois vérifier si je dois remplacer les ligatures par les deux caractères.
- etc...

Dans tous les cas, si quelqu'un possède une solution je suis preneur !
