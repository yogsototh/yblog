-----
isHidden:       false
menupriority:   2
kind:           article
published: 2009-11-12
title: Git pour les nuls
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Avant l'utilisation, la configuration
-----

# Avant l'utilisation, la configuration

## installation

Sous Linux Ubuntu ou Debian :

<div><code class="zsh">$ sudo apt-get install git</code></div>

Sous Mac OS X :

* installez [MacPorts](http://macports.org/install.php)
* installez [Git][git]

<div><code class="zsh">
$ sudo port selfupdate

$ sudo port install git-core
</code></div>

## Configuration globale

Enregistrez le fichier suivant comme le fichier `~/.gitconfig`.

<div><code class="zsh" file="gitconfig">
[color]
    branch = auto
    diff   = auto
    status = auto
[alias]
    st        = status
    co        = checkout
    br        = branch
    lg        = log --pretty=oneline --graph
    logfull   = log --pretty=fuller --graph --stat -p
    unstage   = reset HEAD
    # there should be an article on what this command do
    uncommit = !zsh -c '"if (($0)); then nb=$(( $0 - 1 )); else nb=0; fi; i=0; while ((i<=nb)); do git revert -n --no-edit HEAD~$i; ((i++)); done; git commit -m \"revert to $0 version(s) back\""'
    undomerge = reset --hard ORIG_HEAD
	conflict  = !gitk --left-right HEAD...MERGE_HEAD
    # under Mac OS X, you should use gitx instead
	# conflict    = !gitx --left-right HEAD...MERGE_HEAD
[branch]
	autosetupmerge = true
</code></div>

Vous pouvez obtenir le même résultat en utilisant pour chaque entrée la commande `git config --global`.
Configurez ensuite votre nom et votre email. Par exemple si vous vous appelez John Doe et que votre email est `john.doe@email.com`. Lancez les commandes suivantes :

<div><code class="zsh">
$ git config --global user.name John Doe

$ git config --global user.email john.doe@email.com
</code></div>

Voilà, la configuration de base est terminée. J'ai créé dans le fichier de configuration global des *alias* qui vont permettre de taper des commandes un peu plus courtes.

## Récupération d'un projet déjà versionné

Si un projet est déjà versionné avec [Git][git] vous devez avoir une `URL` pointant vers les sources du projet. La commande a exécuter est alors très simple.

<div><code class="zsh">
$ cd ~/Projets
$ git clone git://main.server/path/to/file
</code></div>

S'il n'y a pas de serveur git sur le serveur distant, mais que vous avez un accès `ssh`, il suffit de remplacer le `git` de l'url par `ssh`. Pour ne pas avoir à entrer votre mot de passe à chaque fois le plus simple est de procéder comme suit :

<div><code class="zsh">
$ ssh-keygen -t rsa
</code></div>

Répondez aux question et n'entrez **surtout PAS** de mot de passe. Ensuite copiez les clés sur le serveur distant. Ce n'est pas la façon la plus sûre de procéder. L'idéal étant d'écrire quand même un mot de passe et d'utiliser `ssh-agent`.

Ensuite le plus simple, si vous possédez `ssh-copy-id` (sous Ubuntu par exemple) :

<div><code class="zsh">
me@locahost$ ssh-copy-id -i ~/.ssh/id_rsa.pub me@main.server
</code></div>

ou manuellement :

<div><code class="zsh">
me@locahost$ scp ~/.ssh/id_rsa.pub me@main.server:
me@locahost$ ssh me@main.server
password:
me@main.server$ cat id_rsa.pub >> ~/.ssh/authorized_keys
me@main.server$ rm id_rsa.pub
me@main.server$ logout
</code></div>

Maintenant vous n'avez plus besoin de taper votre mot de passe pour accéder à `main.server`. Et donc aussi pour les commandes `git`.

## Créer un nouveau projet

Supposons que vous avez déjà un projet avec des fichiers. Alors il est très facile de le versionner.

<div><code class="zsh">
$ cd /path/to/project
$ git init
$ git add .
$ git commit -m "Initial commit"
</code></div>

Une petite précision. Si vous ne souhaitez pas *versionner* tous les fichiers. Par exemple, les fichiers de compilations intermédiaires. Alors il faut les exclure. Pour cela, avant de lancer la commande `git add .`. Il faut créer un fichier `.gitignore` qui va contenir les *pattern* que git doit ignorer. Par exemple :

<div><code class="zsh">
*.o
*.bak
*.swp
*~
</code></div>

Maintenant si vous voulez créer un repository sur un serveur distant, il faut absolument qu'il soit en  mode `bare`. C'est-à-dire que le repository ne contiendra que la partie contenant les informations utile à la gestion de git, mais pas les fichiers du projet. Sans rentrer dans les détails, il suffit de lancer :

<div><code class="zsh">
$ cd /path/to/local/project
$ git clone --bare . ssh://server/path/to/project
</code></div>

Les autres pourront alors récupérer les modifications via la commande vue précédemment :

<div><code class="zsh">
git clone ssh://server/path/to/project
</code></div>

## Résumé de la seconde étape

Vous avez maintenant un répertoire sur votre ordinateur local. Il est versionné. Vous pouvez vous en rendre compte parcequ'à la racine (et à la racine seulement), il y a un répertoire `.git`. Ce répertoire contient tous les fichiers nécessaires au bon fonctionnement de [Git][git].

Il ne reste plus qu'à savoir comment s'en servir maintenant pour obtenir toutes les jolies promesses faites dans la première partie.

[git]: http://git-scm.org "Git"
