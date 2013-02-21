-----
menupriority:   1
kind:           article
published: 2009-10-13
title: Usages non dits de Git
author: Yann Esposito
authoruri: yannesposito.com
tags:  git, dcvs, programming
-----

<small> <em>Je décris pourquoi j'ai eu tant de mal à me faire à Git. Il y a en effet une partie "non dite" qui m'a bloqué pendant un bon moment. Jusqu'à ce que je découvre le bon document. </em></small>

<small> <em> Le fait est que les *branches légères* ne sont pas destinée à être des branches isolées. Ainsi, il y a un </em>"workflow standard"<em> qui s'il n'est pas suivi rend l'utilisation de Git inappropriée.  </em> </small>

newcorps

# La décentralisation en action

### De SVN à Bazaar

J'étais un fervent utilisateur de [subversion (svn)](http://subversion.tigris.org). Lorsqu'un jour, comme beaucoup de gens je découvris ce qu'en pensais [Linus Torvald sur une vidéo](http://www.youtube.com/watch?v=4XpnKHJAok8). Ventant les mérites d'un *système de versions concurrentes décentralisé*.

En effet une fois qu'on s'y intéresse un peu, on voit tous les avantages pratiques qu'apporteraient en théorie un tel système.

J'ai alors eu besoin d'un système de version dans mon équipe.
Ils n'étaient pas familier avec les systèmes de versions. À par ceux possédant une <abbr title="Interface Utilisateur">GUI</abbr>, qui sont lourds et administrés par un responsable<sup><a href="#note1">&dagger;</a></sup>.

Après quelques recherches trois choix se dessinent : 

  - [Git](http://git-scm.com)
  - [Bazaar](http://bazaar-vcs.org)
  - [Mercurial](http://selenic.com/mercurial)

En me renseignant un peu sur les forums et en essayant les trois, je me suis vite rendu compte que celui possédant l'interface utilisateur la plus simple était Bazaar<sup><a href="#note2">&#42;</a></sup>. Mon choix était fait.


### De bazaar à Git

Je me suis alors familiarisé avec Bazaar. Et je dois dire que c'était vraiment naturel en venant de subversion. La commande `pull` correspond au `update`, la commande `push` correspond au `commit`. Puis les commandes `commit` et `update` existent toujours si on en a besoin et qu'on veut utiliser un *workflow* identique à celui de subversion. 

Mais plus le temps passe et plus de partout sur les blog, c'est surtout Git qui a le vent en poupe.

Je décide alors d'utiliser Git en particulier pour *versionner* le site que vous êtes en train de lire. Sauf que je le trouve vraiment difficile d'utilisation et surtout complètement contre intuitif (j'y reviendrai plus tard).

Alors que j'essaye de trouver de l'aide et que je dis qu'il est plus difficile à utiliser que Bazaar, beaucoup me répliquent que c'est : 

> &mdash; *Super-tellement-trop-simple que même ma fille de 12 ans qui n'y comprend rien en informatique l'utilise pour versionner ses documents. Elle s'en sert très facilement en créant des branches et tout et tout...*

Bon alors si une gamine 12 ans trouve ça très naturel et que moi (avec mon Doctorat en informatique) j'ai du mal à faire ce que je veux, c'est un peu frustrant et humiliant. Mais qu'est-ce qui fait que Git est naturel aux uns (comme pour [CocoaSamurai](http://cocoasamurai.blogspot.com) ) et très confus pour moi ?

C'est en lisant un article j'ai enfin compris ce qu'il me manquait. C'est la partie **non dite** de la conception. Celle que tous les développeurs et les concepteurs trouvaient comme *aller de soi*. Sauf que pour moi, ce n'était pas du tout le cas.

<small><a name="note1">&dagger;</a> - Je parle de *ClearCase(c)*. Et oui, je sais qu'il existe des commandes en lignes pour ClearCase(c), mais ce n'est pas comme ça qu'ils étaient habitués à travailler avec des systèmes de "versionning".</small>

<small><a name="note2">&#42;</a> - Je n'ai pas vraiment donné sa chance à Mercurial, la terminologie qu'ils utilisaient était trop éloignée de celle de svn à laquelle je m'étais habituée.</small>

newcorps

Lorsqu'on voit les présentations autour de la notion de *branche* et de <abbr title="Système de Version Concurentes Décentralisé"><sc>dcvs</sc></abbr>, on s'imagine dans un monde où chaque branche est totalement isolée des autres sauf au moment de "merger" les différences les unes des autres. 
Tout est magique. C'est la façon de voir "*Mondes Parallèles*". Cette façon de voir est expliquée dans le [très bon article sur les branches](http://betterexplained.com/articles/a-visual-guide-to-version-control/) sur betterexplained.

Sauf que les concepteurs de Git (conçu pour le noyau Linux) ont plutôt imaginé un système basé non pas autour des mondes parallèles, mais sur la notion de *Patch*.

D'un côté *Mondes Parallèles*, de l'autre *Patchs*. Il y a beaucoup de notions équivalentes dans les deux cas, mais aussi quelques différences. 

* Bazaar est complètement basé sur la notion de *Mondes Parallèles* qui va impliquer un phénomène de *Patch*. 
* Alors que Git est basé sur la notion de *Patch* qui va impliquer la création de *Mondes Parallèles*.

Je ne vais pas argumenté pour savoir si une façon de voir est meilleure que l'autre. Disons simplement que ma façon d'entrer dans l'explication des DCVS était par le biais des *Mondes Parallèles* alors que Git est conçut selon l'autre notion<sup><a href="#note3">&Dagger;</a></sup>.

##  De la théorie à la pratique 

Bien que je pense avoir bien compris les mécanismes conceptuels de Git, la mise en pratique posait problème. Et le point noir, celui qui m'empêchait de comprendre Git comme je le souhaitais était dû à la notion de *branche légère*.

Une *branche légère* qu'est-ce que c'est me demanderez-vous ? Si comme moi on vient de Bazaar, c'est une notion complètement nouvelle. Il s'agit simplement de la capacité de créer une nouvelle branche en réutilisant le répertoire dans lequel on se trouve.

En pratique pour changer de branche, il faut lancer une commande. Tous les fichiers locaux non modifiés depuis le dernier commit seront alors modifiés pour correspondre à la version de la branche.

En théorie, les *branches légères* sont des branches tout comme avec bazaar. D'ailleurs le mot utilisé n'est pas *branche légère* mais *branche* tout court.

Sauf que contrairement à une branche standard résidant dans son propre répertoire, une branche légère est destinée à n'être qu'un **patch** de la branche principale du répertoire dans lequel elle réside.

Bien entendu on pourra m'objecter que l'on peut tout à fait utiliser ces branches légères comme des branches normales. Mais elles n'ont pas été conçues pour ça. Et donc, en pratique, c'est gênant de les utiliser de la sorte.

Voici comment Git est censé être utilisé (pour plus de détails vous pouvez lire [Git for Designers](http://hoth.entp.com/output/git_for_designers.html) en anglais) :

* récupération ou création d'un "repository" central **<sc>Le Grand Repository</sc>**
* Création d'un *branche légère* locale qui contient les différences qui vont devoir être "patché" dans **<sc>LE GRAND REPOSITORY</sc>**.

Voici comment n'est **<sc>pas</sc>** censé être utilisé Git : 

  * Récupération ou création d'un "repository" quelconque
  * Création d'un *branche légère* locale qui n'a pas pour vocation de mettre à jour le "repository" d'origine, mais de vivre sa vie de façon autonome et de récupérer les mises à jour des autres du "repository" d'origine.

En effet cette petite notion m'a empêché de fonctionner correctement.

### En pratique

Maintenant que j'ai compris ça, je peux enfin comprendre pourquoi Git a tant de défenseurs qui continue de trouver que Git est meilleur que les autres.

La notion de branche légère est essentielle à Git et vraiment utile en pratique. Notamment pour la gestion de ce site. Par contre, elle m'empêche d'utiliser les branches comme je le souhaiterai.

Mais dans ce cas-là, je n'ai qu'à utiliser des *clônes* et pas des *branches légères*.

### Des exemples

Je trouve toujours que les terminologies de bazaar sont plus claires et plus concises.

<div><code class="zsh">bzr revert</code></div>

est quand même plus clair que

<div><code class="zsh">git reset --hard HEAD</code></div>

De la même façon

<div><code class="zsh">bzr revert -r -3</code></div>

je trouve ça mieux que

<div><code class="zsh">git reset --hard HEAD~3</code></div>

Là ça va commencer à se compliquer. Si on veut revenir dans le temps sur toute l'arborescence, avec Git on utilise `reset`. 

<center>OK</center>

Maintenant si je veux revenir dans le temps sur un seul fichier. Naturellement on se dit :

<div><code class="zsh">git reset --hard FILE</code></div>

<center>**ET BIEN NON !**</center>

La solution c'est :

<div><code class="zsh">git checkout FILE</code></div>

Quoi ? **`checkout`** !? Bon, d'accord, j'accepte, pourquoi pas après tout ?
En plus quand on est habitué à Bazaar c'est :

<div><code class="zsh">git revert FILE</code></div>

Ce que je trouve quand même bien plus naturel.

Mais là où ça devient vraiment difficile de s'y faire c'est pour changer de branche.  
Avec Bazaar ça donne : 

<div><code class="zsh">cd ../branch</code></div>

Bon ok, il faut changer de répertoire, un répertoire par branche. Ça consomme de l'espace disque mais au moins on voit où on est. Avec Git voilà comment on change de branche (*branche légère*) :

<div><code class="zsh">git checkout branch</code></div>

Alors là, on se dit "<abbr title="What the Fuck?">WTF?</abbr>" ; en français : mais qu'est-ce que c'est que ça ? Je croyais que `checkout` c'était pour récupérer l'état d'un fichier ?

En fait le mot `checkout` sert à la fois à revenir en arrière sur un fichier (MAIS PAS TOUTE UNE ARBORESCENCE où là ça sera `reset --hard`) et à **changer de branche** !

Je trouve ça carrément contre nature.  Même si c'est totalement justifié du point de vue théorique voir le très bon article (en anglais) [Git for Computer Scientist](http://eagain.net/articles/git-for-computer-scientists/). Du point de vue interface utilisateur, on peut difficilement faire pire. On dirait que les mots clés sont utilisés pour piéger l'utilisateur.

> - &mdash; Alors, essaye de deviner ce qu'il va falloir écrire pour faire cette opération ? 
> - &mdash; Perdu. Essaye encore, cherche sur Internet (blaireau).
> - &mdash; Non, c'est toujours pas bon, recommence (sale nul).

Bon alors, voilà, les défauts de Git. Mais, il a par contre beaucoup d'avantages. Une fois qu'on a compris le principe des branches légères. Tout devient plus clair. Même si en pratique, l'édition du fichier `.git/config` peut s'avérer un peu fastidieuse et surtout contre intuitive.

<small><a name="note3">&Dagger;</a> - Il faut aussi préciser qu'ayant travaillé sur les logiques multi-modales et en particulier sur les logiques temporelles (linéaires ou non), j'étais plus enclin à adhérer à cette vision des choses. "Ah mes premiers amours dans la recherche scientifique !"</small>

newcorps

# Conclusion

### DCVS vs. CVS ?

Est-ce que ça valait la peine d'utiliser un système de "versionning" décentralisé ? Indéniablement la réponse est oui. On peut très bien vivre avec des systèmes de versions centralisés, mais la souplesse apportée par la facilité de "merger" différentes branches. De travailler de façon autonomes sur différentes parties d'un projets sont vraiment des plus appréciables. J'aurai vraiment du mal à revenir en arrière.

### Est-ce que Git est meilleurs que Bazaar ?

En terme de *fonctionnalités* je dirai que Git est meilleurs. 
Par contre, je dois avouer qu'il s'agit d'un CVS qui s'est mis dans mes pattes. Or c'est exactement ce que je ne souhaitait pas lors de mon premier choix.

Je n'aurai pas dû avoir du mal à comprendre cette notion de *branche légère* qui doit être un patch sinon tu reçois des messages t'expliquant que tu es en retard. En réalité, Git différencie la notion d'arbre de la notion de branche. Ce qui n'est pas le cas dans Bazaar. Conceptuellement, c'est beaucoup plus simple de comprendre avec Bazaar.

### Finalement ?

Pour conclure, j'utilise plus souvent Git que Bazaar et je dois dire que je préfère utiliser Git. Cependant, les commandes comme `revert` manquent cruellement avec Git. Pour l'instant je n'ai pas encore fait d'alias pour renommer les commandes Git comme je le souhaite.

