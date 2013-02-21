-----
isHidden:       false
menupriority:   30
kind:           article
published: 2009-11-12
title: Git pour les nuls
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Liste de commandes
tags:  git
-----

# Liste de commandes

## Les commandes pour chaque choses

Dans la première partie, nous avons vu la liste des problèmes résolus par [Git][git]. En résumé Git doit pouvoir :

- récupérer les modifications des autres ;
- envoyer ses modifications aux autres ;
- revenir dans le temps ;
- lister les différences entre chaque version ;
- nommer certaines versions pour s'y référer facilement ;
- afficher l'historique des modifications ;
- savoir qui a fait quoi et quand ;
- gérer des conflits ;
- manipuler facilement des branches.

### récupérer les modifications des autres

<div><code class="zsh">
$ git pull
</code></div>

### envoyer ses modifications aux autres

<div><code class="zsh">
$ git push
</code></div>

ou plus généralement

<div><code class="zsh">
$ git pull
$ git push
</code></div>

### revenir dans le temps

#### Pour toute l'arborescence

<div><code class="zsh">
$ git checkout
</code></div>

<div><code class="zsh">
$ git revert
</code></div>

revenir trois versions en arrière

<div><code class="zsh">
$ git uncommit 3
</code></div>

Revenir avant le dernier merge (s'il s'est mal passé).

<div><code class="zsh">
$ git revertbeforemerge
</code></div>

#### Pour un seul fichier

<div><code class="zsh">
$ git checkout file
$ git checkout VersionHash file
$ git checkout HEAD~3 file
</code></div>

### lister les différences entre chaque version

liste les fichiers en cours de modifications

<div><code class="zsh">
$ git status
</code></div>

différences entre les fichiers de la dernière version et les fichiers locaux.

<div><code class="zsh">
$ git diff
</code></div>

liste les différences entre les fichier d'une certaine version et les fichiers locaux.

<div><code class="zsh">
$ git diff VersionHash fichier
</code></div>

### nommer certaines versions pour s'y référer facilement

<div><code class="zsh">
$ git tag 'toto'
</code></div>

### afficher l'historique des modifications

<div><code class="zsh">
$ git log
$ git lg
$ git logfull
</code></div>

### savoir qui a fait quoi et quand

<div><code class="zsh">
$ git blame fichier
</code></div>

### gérer des conflits

<div><code class="zsh">
$ git conflict
</code></div>

### manipuler facilement des branches

Pour créer une branche : 

<div><code class="zsh">
$ git branch branch_name
</code></div>

Pour changer de branche courante : 

<div><code class="zsh">
$ git checkout branch_name
</code></div>

[git]: http://git-scm.org "Git"
