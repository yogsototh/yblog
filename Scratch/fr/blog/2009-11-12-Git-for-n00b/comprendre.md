-----
isHidden:       false
menupriority:   10
kind:           article
published: 2009-11-12
title: Git pour les nuls
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Comprendre
tags:  git
-----

# Pourquoi Git est cool ?

Parce que grace à [Git][git] vous pouvez travailler sur plusieurs partie du projet de façon complètement isolée les unes des autres. Ça c'est la partie décentralisée de [Git][git].

Toutes les branches locales utilisent le même répertoire. Ainsi on peu changer de branche très aisément et rapidement. On peut aussi changer de branche alors que certains fichier sont en cours de modifications. On peut même pousser le vice jusqu'à modifier un fichier, changer de branche, commiter une partie seulement des modifications de ce fichier dans la branche courante. Revenir dans l'ancienne branche et commiter à nouveau les modifications restantes. Et merger dans une troisième branche les deux modifications.

Avec la command `git rebase` on peut après coup, décider que certaines modifications devaient aller dans certaines branches, que d'autres ne servaient à rien. C'est une commande vraiment très puissante pour organiser l'historique.

En pratique, qu'est-ce que ça signifie ? Mieux qu'avec tous les autres systèmes de versions, vous pouvez utiliser [Git][git] pour vous concentrer sur votre code. En effet, on peut envoyer les commits après avoir coder. Par exemple, vous pouvez coder sur la résolution du bug b01, du bug b02 et de la feature f03. Puis ensuite, vous pouvez créer une branche par bug et par feature. Puis commiter les modifications pour chaque branche et chaque feature. Puis finalement merger tous les modifications dans la branche principale.

Tout a été pensé pour vous permettre de coder d'abord, puis de vous occuper du système de version plus tard. Bien entendu, faire des commit atomique au fur et à mesure du code permet de gagner du temps et de ne pas trop s'embêter pour organiser les branches. Mais rien ne vous y oblige. Par contre faire la même chose dans d'autres systèmes de versions n'est absolument pas naturel.

Avec [Git][git] vous pouvez aussi dépendre de plusieurs sources. Ainsi, plutôt que d'avoir un serveur centralisé, vous pouvez avoir plusieurs sources. Vous pouvez définir ce genre de chose très finement.

Ce qui change le plus avec [Git][git] c'est la vision d'un projet centralisé sur un serveur avec plusieurs personnes qui travaillent dessus. Avec [Git][git] plusieurs personnes peuvent travailler sur le même projet, mais sans nécessairement avoir un *repository* de référence. On peut très facilement résoudre un bug et envoyer le patch à plein d'autres versions du projet.

[git]: http://git-scm.org "Git"
