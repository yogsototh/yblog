-----
isHidden:       false
theme: brutalist
menupriority:   1
kind:           article
published: 2010-07-07
title: N'utilisez pas de gradients avec Chrome
author: Yann Esposito
authoruri: yannesposito.com
tags:  CSS, web, programming, Chrome, Safari, Firefox
-----

Beaucoup d'utilisateurs de [Reddit](http://reddit.com) m'ont rapporté que mon site était très long à charger et à *scroller*.
Ils pensaient qu'il s'agissait d'un problème dû aux ombres que j'applique sur le texte.
J'étais un peu surpris puisque je fais mes tests sur une machine vraiment très lente et je n'avais jamais détecté ces problèmes.
En réalité, ce qui ralenti le rendu de ce site est par ordre d'importance :

1. Les dégradés sur Chrome (pas dans Safari sur Mac)
2. les *box shadows* sur Firefox

## les dégradés

Sur Safari il n'y a absolument aucun problème avec les dégradés. Par contre sur Chrome sous Linux le site devient quasiment inutilisable.

Safari et Chrome utilisent *webkit* tous les deux. Lorsque vous accéder à ce blog avec javascript activé, un CSS spécifique à votre navigateur est ajouté. Jusqu'à maintenant je faisais un tri entre : IE, Mozilla et Webkit. Maintenant j'ai rajouté un cas particulier pour Chrome.
Maintenant j'ai supprimé les gradients lorsque vous naviguer sur ce site en utilisant Chrome.

Je n'ai pas vérifier la vitesse de rendu de toutes les propriétés de CSS 3. Mais je vous conseille de ne pas utiliser **`-webkit-gradient`** avec Chrome. Au moins sous Linux.

## Les ombres (box-shadow)

J'ai aussi remarqué que **`-moz-box-shadow`** ralenti le rendu sous Firefox (et sous Linux). Alors que l'équivalent webkit ne pose aucun problème à Safari sous Mac.

## Ombres de texte

Beaucoup d'utilisateurs mon dit d'utiliser text-shadows avec parcimonie. Mais je pense qu'il ne s'agissait pas là du problème du ralentissement du site. C'est pourquoi je vais les remettre.

## en conclusion

N'utilisez pas **`-webkit-gradient`** avec google Chrome pour l'instant.
Utilisez **`-moz-box-shadow`** avec parcimonie.
