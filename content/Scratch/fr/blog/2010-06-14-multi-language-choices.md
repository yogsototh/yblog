-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-06-14
title: choix liés à l'écriture dans plusieurs langues
author: Yann Esposito
authoruri: yannesposito.com
tags:  langues, blog
-----

Je traduis la plupart de mes articles pour qu'ils soient disponibles en français et en anglais. 
La façon que l'on m'a conseillé était d'avoir un fichier par langue. En général ça donne ça.

<pre class="twilight">
Bonjour, 

voici un exemple de texte en français.
[image](url)
</pre>

<pre class="twilight">
Hello, 

here is an example of english text.
[image](url)
</pre>

Cette façon de traduire vous impose une certaine façon de traduire.
D'abord écrire entièrement le texte dans une langue, 
puis copier le fichier et enfin retraduire dans une nouvelle langue.

Le problème, c'est que très souvent, les articles ont des parties communes non négligeables. Par exemple, les images, les codes sources, etc...
Lorsque je m'aperçoit que j'ai fait une erreur dans ces parties communes
ça m'oblige à refaire deux fois la même manipulation. Sauf que comme il m'arrive d'être distrait, il peut y avoir pas mal d'aller-retours.

C'est pourquoi, j'ai plutôt opté pour une autre solution. 
J'utilise des *tags* sur un seul fichier. 
En fin de compte, mes fichiers ressemblent à :

<pre class="twilight">
 fr:   Bonjour, 
 en:   Hello, 

 en:   here is an example of english text.
 fr:   voici un exemple de texte en français.
[image](url)
</pre>

Comme j'édite mes fichier avec [vim](http://vim.org), il m'est très facile d'ajouter ces `fr:` ou `en:` en début de ligne à l'aide du très utile `C-v`.
Par contre [nanoc](http://nanoc.stoneship.org) a été conçu pour être utilisé par une seule langue. Précédemment, j'avais utilisé les capacité de nanoc pour séparer les langues. Mais finalement, il s'avère bien plus simple de faire un *pré-traitement* qui nettoie mes fichiers et en fait deux copie qui seront ensuite gérées par [nanoc](http://nanoc.stoneship.org).

Vous pouvez récupérer les sources de mon blog (sans tous les articles) à l'adresse suivante [github.com/yogsototh/Scratch](http://github.com/yogsototh/Scratch). J'écrirais un article pour savoir comment l'utiliser facilement. J'ai en effet ajouté beaucoup de scripts et de librairies.
