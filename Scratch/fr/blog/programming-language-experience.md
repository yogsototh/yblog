-----
isHidden:       false
theme: scientific
image: /Scratch/img/blog/programming-language-experience/dragon.jpg
menupriority:   1
kind:           article
published: 2011-09-28
title: Expérience avec les languages de programmations
author: Yann Esposito
authoruri: yannesposito.com
tags:  programming, languages, C, C++, Java, haskell, Logo, Basic
-----
blogimage("dragon.jpg","Title image")

<div class="intro">
%tlal Mon avis succinct et hautement subjectif concernant les différents languages de programmation que j'ai utilisé.
</div>

### `BASIC`

leftblogimage("basic.gif","Title image")

Ah ! Le language de mes premiers programmes !
Je devais avoir 10-11 ans.
Sous `MO5`, `Amstrad CPC 6128` et même `Atari STe`.
Le langage des `GOTO`s.
Je suis empleint de nostalgie rien que d'y penser.
C'est à peu prêt le seul intérêt de ce langage.

Aujourd'hui ce langage est tombé en désuétude.
Ce n'est ni un bon langage pour apprendre, ni un bon langage pour faire de vrai programmes.
Même si quelques années plus tard, je me remettais à programmer dans un basic avec un compilateur qui pourrait lui redonner vie.
Je m'en était servi pour faire un livre dont vous êtes le héro :-).

~~~~~~ {.zsh}
READY
10 PRINT "HELLO WORLD!"
20 GOTO 10
RUN
~~~~~~

Je m'en souviens aussi pour avoir copier des codes de jeux vidéo à partir de magasines.
La plupart des lignes ressemblaient à

~~~~~~ {.zsh}
3110 DATA FA,01,FF,FF,FF,FF,00,23,22,43,DA,DE,EE,FF,FF,FF,00,03,4A,F2
~~~~~~

Quel plaisir c'était !

### Logo

leftblogimage("dragon.jpg","Dragon fractal")

Toujours lors que j'avais 10 ans, on pouvait faire de petits programmes sympathiques.

Je me souviens que lors du chargement de l'application logo on avait droit à de la musique de Bach.

Oui, il fallait charger le programme en mémoire avec une cassette.
Et elle ne faisait pas les 'Krrrkrr csssss krrrr'.

Je l'avais utilisé sans les boucles.
Des années plus tard, je le réutiliser pour faire de l'initiation à l'informatique à mes étudiants de DEUG MIAS première année.
Il s'est en fait révélé très utile.
Grace à lui, faire des fractales se révèle être un jeu d'enfant, au sens litéral.
Je ne peux que conseiller ce langage pour apprendre à programmer et aussi pour le fun.

Voici un exemple de code et le résultat est la jolie fractale 'dragon'.

~~~~~~ {.zsh}
HIDETURTLE

PENUP
SETXY -200 0
RIGHT 90
PENDOWN

to dragon :degree :size
    setpensize 1
    if :size>5  [setpensize 2]
    if :size>10 [setpensize 3]
    if :size>20 [setpensize 4]
    if :size>40 [setpensize 5]
    ifelse :degree=0 [
        fd :size
    ][
        left  45 dragon (:degree-1) (size/4)
        right 90 dragon (:degree-1) (size/2)
        left  90 dragon (:degree-1) (size/4)
        right 45
    ]
end

dragon 6 3000
~~~~~~

### Pascal

L'éternel numéro 2.

J'ai dû apprendre à programmer en Pascal aux alentour de 15 ans et je l'ai aussi réutiliser un peit peu en faculté.
Je dois avouer, que je le trouve inférieur au C en tous points.
J'ai fait pas mal de chose avec ça, comme des algorithmes de graphes, des algorithmes de tri, et même un peu d'intelligence artificielle comme des algorithmes génétiques.
Mais je préfère largement le C.

### C

leftblogimage("C.jpg","Pointer representation from Dancing links")

Le langage des pointeurs

Ah, _le_ langage de programmation par excellence.

Une fois que vous avez compris les boucles et la récursivité.
Il est temps de passer aux choses sérieuses.
Si vous voulez avoir du code de bonne qualité, alors apprendre le C est quasi-obligatoire.

Ce langage est très proche du langage machine.
En particulier, (la majorité du temps).
Il y a une relation linéaire entre la taille du code en C et de son résultat compilé en assembleur.

Ça signifie qu'à chaque fois que vous écrivez une ligne de C, il ne va pas se passer de choses toutes bizarres comme lancer un algorithme qui va prendre deux plombes.

Il est très proche de la machine tout en ayant une abstraction suffisante pour ne pas être "trop"  désagréable.

J'ai fait beaucoup de choses avec.
Tous les algorithmes de tri, des algorithmes d'intelligence artificielle (résolution de SAT3), du système, du réseau etc...
Bref il est versatile, et on ne peut pas dire que l'on sait programmer si on ne s'est jamais mis à programmer sérieusement en `C`.

### ADA

Le langage "super propre".

J'avais bien aimé ADA, mais j'avoue que ça n'a duré que le temps d'un semestre de cours.
Peut-être qu'un jour je m'y remettrai.
Disons qu'il est assez vieux et qu'il a inspiré la plupart des concepts objets.

## Les langages orientés objets

Bon, oui, le Pascal, le C, le Basic (fortran, Cobol et autres) étaient tous des langages impératifs, sans notion d'objets.

En gros, il n'y avait pas d'aide pour structurer votre code.

Alors, pour aider à limiter le nombre de bug, en particulier pour la création de très gros programmes, on s'est mis à réfléchir à la meilleure façon d'organiser du code d'ordinateur.
À la fin, ça à donné la programmation orienté objet. Et donc les langages comme le C manquaient de système pour aider au développement orienté objet.
Attention, la programmaiton orienté objet n'est pas la panacée. Combien de programme utilisez-vous qui n'ont pas de bug ?
Et ça ne convient pas à tous les type de problème.
Mais pour faire une application banquaire, un système de gestion des stocks, des clients ou des archives.
C'est-à-dire un système d'information, c'est pas trop mal.

Donc les langages orientés objets se sont mis à fleurir.

### C++

leftblogimage("cplusplus.jpg","Messy router")

Le malpropre

en:

Et oui l'industrie voulait un langage objet, mais elle n'était pas prête à mettre à la poubelle tout ses codes en C.
La solution, prendre C et lui rajouter une couche objet.
Le problème avec C++ c'est qu'il fait trop de choses.
L'héritage multiple, des templates, etc...
Bon, je l'ai quand même choisi pour faire le plus gros programme que j'ai jamais fais lors de ma thèse.
Et je dois avouer que l'expérience m'a plu.
Le seul reproche que j'ai à faire, c'est que la STL n'était pas aussi complète que l'on aurait pu l'espérer pour un détail.
On ne peut pas faire de `String<T>` pour autre chose que des `char16`.
Du coup, mon alphabet était limité à 2<sup>16</sup> lettres.
Hors, pour certaines application, l'alphabet doit être gigantesque.
fr:
En conclusion je dirai que C++ est un très bon langage si vous vous fixez à l'avance un sous ensemble de ses fonctionnalités.

### Eiffel

leftblogimage("eiffel.jpg","Eiffel tower construction")

Eiffel est un très beau langage objet.
Bien plus propre que C++.
Mais, à moins que les choses aient changées, il n'est pas très populaire.
Derrière lui il n'a pas la communauté de C++.
Pour être franc, j'ai préféré travailler en C++.
J'ai menti à mes profs de l'époque pour leur faire plaisir.
Lorsqu'on viens du C, il est désagréable de changer ses habitudes.

### Java

leftblogimage("grail.jpg","Holy Grail from the Monty Python")

On continue vers les langages objets. Alors, à une époque où j'en ai entendu parler, c'était _le Graal_ !

La portabilité, votre programme marchera partout. Il était orienté objet. Incrusté à l'intérieur il y avait des concepts d'architecture qui empêchent de faire n'importe quoi... Sauf que.

Sauf qu'il est incroyablement verbeux.
Et que les limitations sont très désagréables si on sait ce que l'on fait.

Par exemple, il n'y a pas d'héritage multiple en Java.
Ce qui est en général un choix que je trouve cohérent s'il est bien appuyé par des systèmes qui compensent ce manque.
En java, il existe les interfaces.
Les interfaces permettent d'ajouter des méthodes à une classe.
En aucun cas on ne peut rajouter un attribut autrement qu'en héritant.
Cet état de fait m'a vraiment géné.

Typiquement je faisais une GUI en Java Swing.
J'avais créé mon propre système de notification entre objets.
Au début je considérais qu'un objet ne devait envoyer des notifications qu'à un seul objet.
Ô quelle erreur lorsque je réalisais qu'il fallait non plus gérer un seul objet mais parfois plusieurs.
Je changeais mon implémentation d'interface partout, conséquence, des copier/coller dans tous les sens pour mes classes.
Les copier/coller qui sont justement un problème censé être évité par les langages orientés objets.

De plus toujours pour ma GUI, je devais évidemment gérer des threads.
Hors, il m'a fallu faire mon propre système de gestion de threads pour éviter les locks, pour les notifications (ce thread à fini, etc...).
À l'époque j'utilisais Java 1.5.
Normallement ce problème devait être réglé sur Java 1.6.
J'espère que c'est le cas, mais avoir ce type de "feature" essentielle oubliée par le langage était assez grave.

De même, il a fallu attendre très longtemps avant d'avoir des boucles foreach qui rendent le code bien plus lisible.

Bon, après cette expérience je déconseillerai Java.
La portabilité, n'est pas si intéressante que ce qu'on pourrait croire.

En ce qui concerne les GUI, portable signifie interface fonctionnelle mais médiocre sur toutes les plateformes.
Quelque soit le système d'ailleurs (wxWidget, QT, etc...).
Donc, pour des applications à distribuer à des tiers, c'est à éviter.

Le système de Java est très clos.
Par contre il résout un très bon problème.
Il permet à des développeurs médiocres de travailler en groupe sans faire trop de mal.
Et un bon programmeur sera tout de même capable d'y faire des choses très intéressantes.
Veuillez noter que je n'ai pas dit que les programmeurs Java sont de mauvais programmeurs, ce n'est pas ce que je pense.

### Objective-C

leftblogimage("xcode_logo.png","Xcode Logo")

Le langage que je n'ai appris et utilisé que pour faire des applications sur les plateformes d'Apple(c).
J'ai appris Objective-C après Python.
Et je dois avouer que j'ai eu du mal à m'y mettre.
Je n'ai pas du tout aimé la syntaxe et pas mal d'autres détails.
Mais ça fait parti de ces langages que plus on utilise, plus on aime.
En réalité, il y a quelque chose dans ce langage qui fait que tout est bien pensé.
Mais surtout, ici, ce n'est pas le langage qui est la meilleure partie, c'est plutôt le framework Cocoa qui lui est le plus souvent associé qui est une merveille.
Par rapport à tous les autres framework permettant de fabriquer des GUI, Cocoa est de très loin supérieur.
Même si ça semble être des détails sur le papier, en pratique cela fait une grande différence.

Vraiment jusqu'ici, même si Objective-C reste assez bas niveau, le fait que le typage de ce langage soit dynamique est un vrai plus pour l'interface graphique.
Je ne peux que vous encourager à vous accrocher à ce langage et de faire un vrai programme avec. Vous en serez certainement plus ravi qu'il n'y parrait eu début.

## Les langages interprétés modernes

### PHP

leftblogimage("php.jpg","A Jacky Touch Car")

Le petit langage de script que nous utilisions tous pour faire des sites web à l'époque des gifs animées !

Sympatique, mais sans plus. Apparemment il y a eu pas mal de progrès depuis PHP5, un jour peut-être que j'y reviendrai. Mais, il a derrière lui une réputation de langage pour les "scripts kiddies".
En gros ceux qui ne savent pas coder.
Des trous de sécurité de tous les cotés, etc...

En réalité, PHP est au niveau d'abstration à peine supérieur au C.
Et donc, il est beaucoup moins bien organisé que des langages objets, favorisant ainsi la création de bug.
Pour les applications web, c'est un vrai problème.

PHP, reste pour moi le langage de l'injection SQL. J'en fait encore un peu de temps en temps. Et j'ai moi-même dû protéger les accès au SQL pour éviter les injections. Oui, je n'ai pas trouvé de librairie toute prête pour protéger les entrées SQL. Je n'ai pas beaucoup cherché non plus.

### Python

leftblogimage("python.jpg","Python. Do you speak it?")

Alors là, attention ! Révélation !

Lorsqu'on avait l'habitude de travailler avec des langages compilé, type C++, Java et qu'on passe à Python, on se prend une claque magistrale.
La programmation comme elle doit être faite.
Tout est si naturel, c'est _magique_.
Oui, c'est si bien que ça.
Mais quelque chose d'aussi incroyablement bien doit avoir des inconvénients me dirais-vous.

Et bien, oui, comme tous les langages de scripts de haut niveau, Python est _lent_.
Attention pas juste un peu lent, comme 2 fois plus lent que du C.
Non, de l'ordre de 10 à 20 fois plus lent que le C.
Argh... Bon ça reste utilisable pour beaucoup de choses.
Mais certaines application lui sont donc interdites.

### Awk

Des filtres de fichiers à faire.
Si ce n'est pas trop compliqué, c'est le langage idéal.
Vous avez un fichier et vous voulez savoir quels sont les mots les plus utilisés.
Savoir combien de fois un mot est utilisé.
Filtrer sous des condition un peu plus compliquées qu'un grep.
Super outils. Je l'ai utilisé pour modifier en masse des centaines de fichier XML plus facilement qu'avec du XSLT.

### Perl

Perl c'est assez magique, mais la syntaxe est tellement désagréable à lire que personne ne peut vraiment aimer programmer dans un environnement de plusieurs personnes en Perl.
A moins que tous les autres soient des cadors du Perl.
Mais la feature qui tue, les expressions régulières :

~~~~~~ {.perl}
$var =~ s/toto/titi/g
~~~~~~

Va remplacer toto par titi dans la valeur de la variable `$var`.
Et oui, les expressions régulière y sont intégrées directement comme avec `sed` et `awk`.
Et ça rend le code beacoup plus compact (et parfois illisible).
Mais c'est vraiment pas mal.
C'est une sorte de `awk` sous stéroides.

### Ruby

C'est une sorte de Perl en plus propre.
Un mélange de Perl et de Python.
Les notion objets y sont plus fortes qu'en Python.
Je l'ai beaucoup utilisé, je reste quand même un Pythoniste de préférence.
Mais Ruby est vraiment très bien.
Par contre en terme d'efficacité, c'est le pire langage utilisé par beaucoup de monde de ce point de vue.
C'est le langage qui perd quasiment tous les benchmarks.
Par contre c'est un outil parfait pour faire des prototypes.
Et si vous voulez faire un prototype de site web, RoR est ce qui se fait de mieux.
De l'idée au site, il ne se passera que peu de temps.

### Javascript

C'est la bonne surprise.
Pendant des années, javascript était considéré comme un langage tout bon à vous embéter dans votre navigation web.
En réalité, javascript possède beaucoup de qualité des langages de haut niveau.
En particulier, il est facille de passer une fonction en paramèter ou de créer des fonctions anonymes (closures).
Récemment, il est devenu très rapide et beaucoup de frameworks et de librairies naissent un peu partout.

- Il y a Cappuccino, Objective-J (comme de l'objective-C mais avec du javascript)
- Sproutcore
- Spine.js
- Backbone.js
- jQuery
- prototype.js

En particulier avec jQuery, on peut faire des appels chainés, très agréables à utiliser.
Comme je le disais, c'est une bonne surprise, javascript a été choisi un peu au hasard lors de la création des navigateurs web comme langage de script.
Et il s'avère qu'à part sa syntaxe, tout le reste est bien.
Heureusement, en ce qui concerne la syntaxe, on peu pallier à ce problème en utilisant CoffeeScript.

## Les langages fonctionnels

### CamL

J'ai appris CamL à la fac, j'avais trouvé cette expérience très interressante.
J'étais plutôt bon, et j'avais les bonnes intuitions mathématiques qui vont avec la programmation fonctionnelle.
Mais je dois avouer que je ne l'ai plus jamais utilisé.
Simplement, ce type de langage semble si loin de ce qui se fait pour fabriquer des produits que ça me donnais vraiment l'impression d'être un langage pour chercheurs.

### Haskell

Je suis en train d'apprendre ce langage.
Et je dois dire que c'est un vrai plaisir.
En général les concepts derrière tous les langages de programmation  sont assez limités.
Chaque langage y va de son petit lot de nouveau concepts, et en général en une après-midi, c'est appris.
Pour haskell, c'est très différent.
Je sens bien qu'il va me falloir plusieurs semaines pour maîtriser la bête.
Ça doit faire quatre semaines que j'apprend haskell un peut tous les jours et je sais qu'il y a des notions que j'ai juste survollées et qui sont assez incroyables.
Les Monades par exemple, est un concept que je n'avais jamais rencontré ailleurs.
C'est un super concept.
De plus le design du langage en fait un parfait système pour paralléliser les calculs naturellement.
haskell sépare la partie "pure" de la partie "impure" de la programmation.
À ma connaissance, c'est le seul langage de programmation qui fait ça.
Enfin, je prend beaucoup de plaisir à apprendre ce langage.
La communauté est aussi très acceuillante.
Pas de "L0L! URAN00B!".
Et aussi pas de concession du langage pour devenir populaire.
Le langage est bon, voilà tout.
Alors qu'en Java et C++, typiquement certain choix ont été fait en dépis du bon sens pour "faire plaisir".

## Langages originaux

### Metapost

Metapost est un langage qui permet de programmer des dessins.
Le gros plus de metapost, c'est sa capacité de résoudre automatiquement les systèmes d'équations linéaires.
Par exemple, si vous écrivez :

~~~~~~ {.ruby}
AA=1/3[A,B]
~~~~~~

Il va position le point `AA` entre `A` et `B`.
Plus précisément, au barycentre `(2A + B)/3`.

~~~~~~ {.ruby}
X=whatever[A,B]
X=whatever[C,D]
~~~~~~

Ce deuxième exemple positionne `X` à l'intersection des deux segments `AB` et `CD`.
Vous pouvez aussi voir pas mal d'[exemples ici](http://tex.loria.fr/prod-graph/zoonekynd/metapost/metapost.html).
You could see [more example there](http://tex.loria.fr/prod-graph/zoonekynd/metapost/metapost.html).

Cette fonction est très utile.
Et à mon avis pas seulement pour afficher des choses.
De mon point de vue, les autres langages de programmation devraient penser à rajouter les résolutions automatiques simples.

### zsh

Oui, zsh est un shell.
Mais c'est aussi un langage de script très bien adapté aux traitement de fichiers.
Je le recommande chaudement.
C'est pour l'instant le meilleur shell que j'ai utilisé. Je le préfère au bash.

### Prolog

Je n'ai jamais rien fait de conséquent avec Prolog, mais j'ai adoré l'apprendre et l'utiliser.
J'ai eu la chance d'apprendre Prolog par [Alain Colmerauer](http://alain.colmerauer.free.fr/) lui-même.
C'est un langage qui essaye de résoudre les contraintes autant qu'il le peut pour vous.
Il en ressort un impression de magie.
On ne fait que décrire ce qu'il faut et on ne donne pas d'ordre.
Un peu comme la programmation fonctionnelle mais en beaucoup plus puissant.

## Les langages à découvrir

Il reste encore pas mal de langages et de framework à essayer.
Actuellement je pense que je vais passer un moment avec Haskell.
Peut-être demain que j'irai apprendre LISP, Scala ou Erlang.
Comme je suis plus dans la création de site web, j'irai certainement jeter un coup d'œil à clojure aussi.
Et certainement beaucoup d'autres choses.

Dites moi si vous avez une autre expérience avec ces langages de programmation.
Évidement mes impression sont hautement subjectives.
Cependant, j'ai utilisé tous les langages dont j'ai parlé.

*[STL]: Standard Tempate Library
*[GUI]: Graphic User Interface
