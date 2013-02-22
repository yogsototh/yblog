-----
isHidden:       false
menupriority:   1
kind:           article
published: 2009-11-12
title: Git pour les nuls 
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Git pour quoi faire ?
-----

# [Git][git] pour quoi faire ?

<div class="intro">

Si tout ce qui vous intéresse c'est d'utiliser [Git][git] **tout de suite**. Lisez simplement les parties sur fond noir. Je vous conseille aussi de revenir relire tout ça un peu plus tard, pour mieux comprendre les fondements des systèmes de versions et ne pas faire de bêtises quand vous les utilisez.

</div>

[Git][git] est un <abbr title="Decentralized Concurent Versions System">DCVS</abbr>, c'est-à-dire un système de versions concurrentes décentralisé. Analysons chaque partie de cette appellation compliquée.

### Système de versions

Tout d'abord, les systèmes de versions gèrent des fichiers.
Quand on travaille avec des fichiers sans système de version voilà ce qui arrive souvent :

Lorsqu'on modifie un fichier un peu critique et qu'on a pas envie de perdre, on se retrouve souvent à le recopier sous un autre nom. Par exemple

<div><code class="zsh">$ cp fichier_important.c fichier_important.c.bak</code></div>

Du coups, ce nouveau fichier joue le rôle de *backup*. Si on casse tout, on peut toujours écraser les modifications que nous avons faites. Évidemment le problème avec cette façon de faire c'est que ce n'est pas très professionnel. Et puis c'est un peu limité. Si on veut faire trois ou quatre modifications on se retrouve avec plein de fichiers. Parfois avec des nom bizarres comme :

<div>
<code class="zsh">
fichier_important.c.bak
fichier_important.c.old
fichier_important.c.Bakcup
fichier_important.c.BAK.2009-11-14
fichier_important.c.2009.11.14
fichier_important.c.12112009
old.fichier_important.c
</code></pre>
</div>

Bon alors si on veut que ça marche il faut se fixer des conventions de nommage. Les fichiers prennent beaucoup de place alors que souvent il n'y a que quelques lignes différentes entre le fichier et son backup...

*Heureusement les systèmes de version viennent à la rescousse.*

Il suffit de signaler que l'on va faire une nouvelle version d'un fichier et le système de version se débrouille pour l'enregistrer quelque part où on pourra facilement le retrouver. Et en général, le système de version fait les choses bien. C'est-à-dire qu'il n'utilise que très peu d'espace disque pour faire ces backups.

Il fut un temps où les versions étaient gérées fichier par fichier. Je pense à CVS. Puis on s'est vite aperçu qu'un projet c'est un ensemble de fichiers cohérents. Et donc il ne suffit pas de pouvoir revenir en arrière par fichier, mais plutôt dans le temps. Les numéros de versions sont donc passé d'un numéro par fichier à un numéro par projet tout entier. 

Ainsi on peut dire, «je veux revenir trois jours en arrière», et tous les fichiers se remettent à jour.

<div class="black">

*Qu'apportent les systèmes de versions ?* (je n'ai pas tout mentionné)

- backup automatique de tous les fichiers: *Revenir dans le temps.* ;
- donne la possibilité de voir les différences entre chaque version et les différences entre la version en cours et les modifications locales ;
- permet de poser un *tag* sur certaines versions et ainsi pouvoir s'y référer facilement ;
- permet d’avoir un historique des modifications. Car en général il est demandé aux utilisateurs d’ajouter un petit commentaire à chaque nouvelle version.

</div>

### concurrentes

Les systèmes de versions sont déjà intéressants pour gérer ses projets personnels. Car ils permettent de mieux organiser celui-ci. De ne (presque) plus se poser de questions à propos des backups. Je dis presque parce qu'il faut quand même penser à protéger par backup son repository. Mais là où les systèmes de versions deviennent vraiment intéressants, c'est pour la gestion de projets à plusieurs.

Commençons par un exemple avec un projet fait par deux personnes ; Alex et Béatrice.
Sur un fichier contenant une liste de dieux *Lovecraftiens* :

<div style="width: 10em; margin-left: auto; margin-right: auto">
<code class="zsh">
Cthulhu
Shubniggurath
Yogsototh
</code></div>

Disons que Alex est chez lui, il modifie le fichier :

<div style="width: 10em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Cthulhu
Shubniggurath
<span class="StringConstant"><strong>Soggoth</strong></span>
Yogsototh
</pre>
</div>

puis il envoi ce fichier sur le serveur du projet. Ainsi sur le serveur, il y a le fichier d'Alex.

Ensuite c'est Béatrice qui n'a pas récupéré le fichier d'Alex sur le serveur qui fait une modification.

<div style="width: 10em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Cthulhu
<span class="Constant"><strong>Dagon</strong></span>
Shubniggurath
Yogsototh
</pre>
</div>

Puis Béatrice envoi son fichier sur le serveur.

La modification d'Alex est *perdue*. Encore une fois les systèmes de versions sont là pour résoudre ce type de soucis.

Un système de version aurait *mergé* les deux fichiers au moment où Béatrice voulait envoyer la modification sur le serveur. Et comme par magie, sur le serveur le fichier deviendra :

<div style="width: 10em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Cthulhu
<span class="Constant"><strong>Dagon</strong></span>
Shubniggurath
<span class="StringConstant"><strong>Soggoth</strong></span>
Yogsototh
</pre>
</div>

En pratique, au moment où Béatrice veut envoyer ses modifications, le système de version la préviens qu'une modification a eu lieu sur le serveur. Elle utilise la commande qui rapatrie les modifications localement et qui va mettre à jour le fichier. Ensuite Béatrice renvoie le nouveau fichier sur le serveur.

<div class="black">

**Qu'apportent les Systèmes de Versions Concurrentes ?**

- récupérer sans problème les modifications des autres ;
- envoyer sans problème ses modifications aux autres ;
- permet de gérer les conflits. Je n'en ai pas parlé, mais quand un conflit arrive (ça peut arriver si deux personnes modifient la même ligne avec deux contenus différents), les <abbr title="Systèmes de versions concurrentes">SVC</abbr> proposent leur aide pour les résoudre. J'en dirai un mot plus loin.
- permet de savoir qui a fait quoi et quand

</div>

### décentralisé

Ce mot n'est devenu populaire que très récemment dans le milieu des systèmes de version. Et bien ça veut dire principalement deux choses.

Tout d'abord, jusqu'à très récemment (SVN) il fallait être connecté sur un serveur distant pour avoir des informations sur un projet. Comme avoir l'historique. Les nouveaux systèmes décentralisés permettent de travailler avec un *REPOSITORY* (le répertoire contenant tous les backups, et les différentes info nécessaires au fonctionnement du système de versions) local au projet. Ainsi on peut avoir l'historique du projet sans avoir à se connecter au serveur.

Toutes les instances de projets peuvent vivre de façon indépendantes.

Pour préciser, les systèmes de versions concurrentes décentralisés sont basés sur la notion de **branche**.

Et la signification pratique est très importante. Ça veut dire que tout les utilisateurs travaillent de façon complètement indépendante les uns des autres. Et c'est l'outil de version qui se charge de mettre tout ça ensemble.

Ça va même encore plus loin. Ça permet de développer plusieurs features de manière complètement indépendantes. Sous les autres systèmes c'était plus difficile.

L'exemple type :

> Je développe mon projet. Je suis en train de l'améliorer. Lorsqu'un bug urgent est reporté.
> 
> Je peux très facilement avec un système décentralisé, revenir sur la version qui pose problème. Résoudre le bug. Renvoyer les modifications. Puis revenir à ma version avec les améliorations en cours. Et même récupérer la correction de bug dans ma nouvelle version avec les améliorations.
> 
> Dans un système non décentralisé, cela est possible, mais fastidieux. Les systèmes décentralisés rendent ce type de comportement très naturels. Ainsi, il devient naturel de tirer des *branches* pour toutes les features, les bug...

<div class="black">

**Avantages donnés par la décentralisation des systèmes de versions concurrentes : **

- Possibilité de travailler sans être connecté au serveur de version ;
- Possibilité de créer beaucoup de `patches` atomiques ;
- Grande facilité de maintenance de plusieurs versions différentes de la même application.

</div>

## Pour résumer

Résumons l'ensemble des choses que l'on peut faire facilement avec un <abbr title="Decentralized Concurrent Versions System">DCVS</abbr> :

**Systèmes de versions**

- revenir dans le temps ;
- lister les différences entre chaque version ;
- nommer certaines versions pour s'y référer facilement ;
- afficher l'historique des modifications.

**Concurrentes**

- récupérer les modifications des autres ;
- envoyer ses modifications aux autres ;
- permet de savoir qui a fait quoi et quand ;
- gestion des conflits.

**Décentralisé**

- manipuler facilement des branches

Maintenant voyons comment obtenir toutes ces choses facilement avec [Git][git].

[git]: http://git-scm.org "Git"
