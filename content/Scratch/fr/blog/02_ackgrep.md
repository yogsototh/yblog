-----
published: 2009-07-22
theme: scientific
kind: article
title: Mieux que grep
author: Yann Esposito
authoruri: yannesposito.com
menupriority: 1
-----

# Mise à jour

Comme [Andy Lester](http://www.theworkinggeek.com) me l'a fait remarqué. [`ack`](http://betterthangrep.com) est un simple fichier perl qu'il suffit de copier dans son répertoire personnel `~/bin`. Maintenant j'ai `ack` sur mon serveur professionnel.

Il suffit d'aller sur [http://betterthangrep.com](http://betterthangrep.com) pour le télécharger.

Sincèrement, je ne comprend pas qu'`ack` ne soit pas une commande implémentée par défaut sur les systèmes UNIX. Je ne peux vraiment plus m'en passer, il m'est devenu aussi essentiel qu'un `which` ou un `find`.

---

Mieux que grep
=============================================

Un des mes usages principaux de `grep` est

<div>
    <code class="zsh">
grep 'pattern' **/*(.)</code>
</div>

La plupart du temps c'est suffisant, mais ajouter de la coloration
améliore beaucoup l'utilité de cette commande. Il existe déjà un outil
pour ça : il s'appelle `ack-grep` sous Ubuntu.
Comme je ne peux pas l'installer sur le serveur de mon entreprise,
j'en ai créé un moi-même en quelques lignes :

<div>
    <code class="zsh" file="ack">
#!/usr/bin/env zsh
(($#<1)) && { print 'usage: ack "regexp"' >&2; exit 1 }

listeFic=( **/*(.) )
autoload zargs
zargs -- $listeFic -- grep $1 | perl -ne 'use Term::ANSIColor;
if (m/([^:]*)(:.*)('$1')(.*)/) {
    print color("green").$1;
    print color("reset").$2;
    print color("black","on_yellow").$3;
    print color("reset").$4."\n";
} '
    </code>
</div>

Pour mon utilisation personnelle et celle de mon équipe
c'est suffisant. J'espère que ça pourra vous aider.

