-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-08-11
title: Indécidabilités (partie 1)
author: Yann Esposito
authoruri: yannesposito.com
tags:  mathématiques, science, philosophy, indecidability
-----

<% # toremove_ %>

begindiv(intro)

%tlal Je crée un mode mathématique simple pour parler de différents types d'_indécidabilités_ :

- indécidabilité due aux erreurs d'observation ;
- grandes erreurs résultant de petites erreurs de mesure ;
- indécidabilité fractales ;
- indécidabilité logique.

enddiv

newcorps

# Les indécidabilités

begindiv(intro)

Si le monde a été fabriqué par un démiurge, on peut dire que celui-ci devait avoir le sens de l'humour. 
Et le récit que je vais faire va vous en fournir la preuve.
Je vais me mettre à sa place. 
Je vais créer un monde simplifié.
Un monde régi exactement par nos règles mathématiques.
Puis je vais vous parler du mal qui touche cet Univers semblable au notre ; l'*indécidabilité*. 
L'incapacité de savoir si nous avons trouvé la vérité, ou seulement une approximation de celle-ci.
L'incapacité de prédire certaines choses qui semblent pourtant aller de soi.
Voilà comment tout aurait pu commencer.

enddiv

leftblogimage("genesis.png")

Au début, il n'y avait rien. 
Puis un article de blog commença à prendre forme.
J'inspire profondément pour sentir la pesanteur de ce que je vais accomplir.
Attention, une dernier moment de tension et je crée l'_Univers_.
Un *Univers* qui n'existera que le temps de la lecture de cet article.
Me voici le _démiurge_ de cet Univers et te voilà son observateur privilégié.

Comme j'aime bien tout contrôler, je fabrique ce monde avec quelques règles simples.
Je décide que les _vrais_ règles de ce monde sont celles que nous pensons qui régissent notre monde.
Notez qu'il y a une grande différence. 
Pour leur monde, ce que l'on _croit_ vrai aujourd'hui, est vraiment vrai pour eux.
Leur monde est donc plus _simple_ à priori que le notre. 
En particulier, on peut le décrire avec des axiomes et des règles mathématiques.
Alors qu'il est possible que ce ne soit pas le cas de notre Univers. 
Mais nous reviendront là-dessus plus tard.

Bon au travail maintenant, je crée une *Terre*.
J'y ajoute des habitants intelligents, les _Ys_.
Bien entendu ils se posent des questions.
En particulier, ils se demandent quelles sont les lois qui régissent leur monde. 
Ils pensent que connaître toutes ces règles leur permettrait de connaître l'avenir. 
Leur naïveté est touchante. 
Ah, si seulement ils savaient.
Mais je suis là pour les aider à apprendre.

Comme je suis un Dieu un peu facétieux, je vais leur jouer quelques tours.
Sinon on s'ennuierai à mourir.
Le premier est de leur donner des sens imparfaits. 
De plus il leur est impossible d'avoir des mesures parfaites.
Je leur laisse cependant toutes libertés pour améliorer leur technologie et diminuer ces erreurs de mesures.

Les habitants de ce monde pensent que celui-ci est plat.
Certains d'entre eux pensent qu'il est possible de découvrir les règles du monde que j'ai créé.
Et bien que le jeu commence.

Commençons par leur première leçon, _les erreurs causent de l'indécidabilité_.

## Indécidabilité dues aux erreurs de mesures

Voici ce que pense l'un de ces individus.

> Tous les triangles que j'observe semble avoir une propriété commune.
> La somme de leurs angles est toujours π radiants (180°).
> Il s'agit certainement d'une loi de mon Univers.
> Mais comment être certain que tous les triangles de mon Univers possèdent cette propriété ?

leftblogimage("triangle_3_angles.png","three triangles")

Certain d'entre eux commencent à formaliser un petit peu le problème
et ils finissent faire une preuve mathématique.
Magnifique !
La preuve est correcte, mais il reste un petit problème.
La preuve s'appuie sur des axiomes et des règles.
Comment être certain que ces règles et ces axiomes sont vrai dans leur monde?
Ils auront beau faire des mesures de plus en plus précises qui conforteront cette formule,
ils n'auront que l'_espoir_ et  _jamais_ la certitude que la formule est vrai.
Simplement parce que le seul moyen de vérifier la véracité des axiomes est par l'observation.
Hors en tant que dieu facétieux, j'ai interdit les observation avec des mesures parfaites.

Bien entendu, ils prient, ils m'appellent à l'aide.
Et comme tout Dieu qui se respecte, je ne réponds pas.
Ah ah ah ! J'ai toujours aimé faire ce genre de chose.
Ensuite je ferai comme si je n'existe pas.
Encore un bonne blague !

Si certains se sentent accablés, il leur reste un espoir :

> _Espoir_
>
> Si nous faisons de faibles erreurs de mesure, nous aurons de faibles erreurs dans nos prédictions.

## Indécidabilité avec erreurs croissantes

leftblogimage("3_corps.png","Three bodies")

Malheureusement pour eux, il y a le  _problème des 3 corps_. 
Prenons les formules de la gravitation Universelle et appliquons la à deux corps célestes. 
Si on connait la position de ces corps avec un grande précision, on pourra aussi connaître la position future de ces corps avec une grande précision.
L'hypothèse selon laquelle de petite erreurs de mesures impliquent de petites erreurs prédictive est confortée.
Cependant, il y a un problème.
Reprenons le même problème mais avec trois corps. Par exemple, avec le Soleil, la Terre et la Lune.
Dans ce cas, les erreurs de mesures initiales vont s'amplifier. 
S'amplifier au point de rendre toute prédiction inutilisable.

Là encore une voix d'espoir s'élève :

> Peut-être pouvons nous calculer l'erreur maximale acceptable pour prédire quelque chose.
> Et nous pourrions au moins savoir ce que nous pouvons prédire ou pas.

Une fois encore, ça ne va pas très bien se passer.

## Indécidabilité fractale

Considérons la question suivante :

leftblogimage("mandelbrot.png","Mandelbrot set")

Soit des coordonnées GPS précises à 1m près.
Les coordonnées sont proches des côtes de la Bretagne.
Ce point va-t-il tomber dans la mer ou sur la terre ferme ?

Et bien, pour certaines coordonnées, c'est impossible de le savoir. 
Même si je réduis l'erreur à une valeur infinitésimale. 
Simplement parce que certains voisinages autour d'un point contiennent toujours à la fois de l'eau et de la terre.
Et ce quelque soit la taille du voisinage.

On peut même imaginer une structure ou *tous* les points sont au bord de celle-ci, on ne peut donc pas se permettre d'erreur[^2].

[^2]: Pensez aux deux ensembles R\Q et Q.

Mais que vois-je ? 
Un petit malin essaye de trouver la vérité en s'extrayant de mon Monde et en faisant un article sur un blog ?
Ça ne va pas se passer comme ça ! Croyez moi !
> Faire des prédictions précises à partir des données observées semble être une quête vouée à l'échec. 
> Mais je suis persuadé que l'on peut aller au delà.
> Au diable ce Dieu qui nous empêche d'avoir des mesures précises !
> Inventons notre propre Univers mathématique.
> Un monde qui se suffit à lui-même.
> Un monde dans lequel il n'y aura plus d'erreur de mesure. 
> Un monde entièrement contrôlé par des règles que nous aurons choisi.
> Un monde similaire au notre mais où tout pourra être prédit.

## Indécidabilité logique

leftblogimage("stackOverflow.png","recursive stack overflow")

Jusqu'ici, tous les problèmes d'indécidabilités étaient dûs aux _erreurs_.
Maintenant peut-être que privé d'erreur de mesure, on pourrait enfin résoudre tous les problèmes.  
Et bien non.
Même dans un monde mathématique complètement contrôlé.
On peut créer un objet pour lequel on ne pourra pas décider à l'avance ce qu'il fait.

Il s'agit du problème de l'arrêt. 

Le Théorème stipule qu'il n'existe pas de programme permettant de _décider_ si un autre programme s'arrête.
La preuve est suffisamment simple pour rentrer dans ce post, donc je me fais un petit plaisir en la donnant.

> Supposons qu'il existe un programme qui puisse dire si un autre programme s'arrête. Plus précisément :
> 
> Hypothèse: Il existe un programme `P` tel que: 
>
> - `P(x,y)` réponde "s'arrête" en un temps fini si et seulement si `x(y)`[^1] s'arrête effectivement en temps fini et 
> - `P(x,y)` réponde "ne s'arrête pas" en un temps fini dans le cas contraire.
>
> Remarque: Tout code de programme est une chaîne de caractère qui peut être utilisée aussi comme entrée d'un autre programme. 
> Ainsi écrire `P(x,x)` est autorisé.
> 
> Soit le programme Q que j'écris comme suit :
> 
> <pre class="twilight">
> Q(x) :
>     si P(x,x)="s'arrête" alors je fais une boucle infinie.
>     si P(x,x)="ne s'arrête pas" alors je m'arrête.
> </pre>
> 
> Maintenant que répond `P(Q,Q)`?
>
> - si `P(Q,Q)` répond "s'arrête" ça implique que `P(Q,Q)`="ne s'arrête pas"
> - si `P(Q,Q)` répond "ne s'arrête pas" ça implique que `P(Q,Q)`="s'arrête"
> 
> Il y a donc une contradiction que le seul moyen de régler est par la non existence du programme P.

[^1]: C'est-à-dire le programme `x` prenant l'entrée `y`.

C'est simple, je suis le démiurge de ce monde imaginaire. 
Et même moi, je dois me soumettre à cette règle.
Comme quoi, avoir la possibilité de créer le monde et la toute puissance sont deux choses différentes.

newcorps

Après tout ceci, il peut sembler difficile de savoir en quoi nous pouvons croire.
Mais ce serait une erreur de jeter le bébé avec l'eau du bain.
Dans une seconde partie, j'expliquerai ce que nous pouvons espérer et qu'elle attitude nous devons adopter une fois que l'on a réalisé que beaucoup de vérité nous sont inaccessibles.
