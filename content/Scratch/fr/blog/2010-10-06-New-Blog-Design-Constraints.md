-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-10-06
title: Contraintes du design de ce blog
author: Yann Esposito
authoruri: yannesposito.com
tags:  programming, blog
-----

Vous avez pu constater que j'ai modifié le design de mon blog.
Maintenant il doit être beaucoup plus léger qu'avant.
Je n'utilise plus de CSS3 et beaucoup moins de javascript.
Bien entendu, même avant, mes pages étaient parfaitement lisibles sans javascript.
Mais, je me suis aperçu que les systèmes de CSS3 sont loin d'être au point.
J'utilisait des gradient en CSS3, ainsi que des ombres sous le texte. Ça avait un rendu très sympa. Sauf...
Ce n'était pas compatible ie6, sous Chrome le rendu était d'une lenteur incroyable.
J'ai donc décidé de faire un site à minima. 
Je voulais qu'il soit joli _et_ le plus simple possible pour assurer sa compatibilité.
Les règles que je me suis fixées sont donc:

- pas d'élément CSS qui commence par `-moz` ou `-webkit`, etc... ;
- pas d'ombre sous le texte pour donner une impression de profondeur ;
- nettoyer pas mal le code et enlever tout ce que je peux ;

J'espère que ce nouveau design vous plaît.
