-----
menupriority:   1
theme: scientific
image: /Scratch/img/blog/06_How_I_use_git/central_architecture.png
kind:           article
published: 2009-08-18
title: Git en solo
author: Yann Esposito
authoruri: yannesposito.com
tags:  git, svn, workflow
-----

blogimage("central_architecture.png","central architecture")

<div class="encadre">

_Màj_ : Actuellement j'utilise github avec des repository privés. Je paye une somme très raisonnable pour ce service. Si vous voulez être complètement autonome, je vous conseille d'utiliser [gitolite](https://github.com/sitaramc/gitolite) sur votre propre serveur accessible sur le web.

</div>

J'utilise [Git](http://www.git-scm.org/) pour gérer mes projets personnels.
J'ai un *repository* centralisé et tous mes ordinateurs se synchronisent avec lui.
Cependant, dans la documentation officielle, je n'ai pas trouvé clairement ce que je souhaitais.

En d'autres termes, si vous souhaitez utiliser le type de *workflow* que SVN proposait avec Git (et ses avantages), voici comment procéder.

---

## Initialisation

Disons que j'ai déjà un projet et que je veuille en créer un nouveau.

<div>
~~~~~~ {.zsh}
cd to/project/directory/
git init
git add
git commit
~~~~~~
</div>

Maintenant tous les fichiers du répertoire <code>to/project/directory/</code> sont *versionnés*. Si vous voulez ignorer certains fichiers il suffit de modifier le fichier <code>.gitignore</code>.

Par exemple voici le mien : 
<div>
~~~~~~ {.zsh}
*.swp
.DS_Store
ikog.py.bak
output/Scratch/assets
output/Scratch/en
output/Scratch/fr
output/Scratch/multi
~~~~~~
</div>

Ensuite, il faut placer ce projet dans un répertoire accessible via Internet.

<div>
~~~~~~ {.zsh}
git clone --bare . /path/to/repository
~~~~~~
</div>

<div class="encadre"><em>
Màj: La meilleure solution est d'installer <a href="https://github.com/sitaramc/gitolite">gitolite</a> pour installer un serveur git sur sa machine. Gitolite permet de gérer la gestion des droits d'utilisateurs, ceux-ci n'ayant pas accès à un shell sur la machine.
</em>
</div>

Maintenant à partir de n'importe quel ordinateur, voici ce que vous pouvez faire : 

<div>
~~~~~~ {.zsh}
git clone protocol://path/to/repository local_directory
~~~~~~
</div>

et <code>local_directory</code> contiendra un projet à jour.

<div class="encadre"><em>

Je vous conseille de faire la même opération sur l'ordinateur qui à servi à créer le projet de façon à vérifier que tout fonctionne correctement.
</em>
</div>

---

## L'utilisation courante

Pour résumer vous avez maintenant un repository sur Internet et un ou plusieurs ordinateurs lui sont associés. Maintenant il faut que tout soit toujours synchronisé.

Avant de commencer à travailler, la première chose à faire est de récupérer les modification à partir d'Internet vers votre poste local : 

<div>
~~~~~~ {.zsh}
git pull
~~~~~~
</div>

Ensuit vous pouvez travailler en faisant (plusieurs fois) : 

<div>
~~~~~~ {.zsh}
hack, hack, hack...
git add some files
git commit
~~~~~~
</div>

Quang vous voulez envoyez les modifications locales sur Internet, il suffit de faire :

<div>
~~~~~~ {.zsh}
git push
~~~~~~
</div>

Tout devrait être bon.

Si vous avez des problèmes avec le <code>push</code> et le <code>pull</code> ; vérifiez votre fichier <code>.git/config</code>. Il devrait contenir les lignes suivantes :

<div>
~~~~~~ {.zsh}
...
[remote "origin"]
	url = protocol://url/of/the/repository
	fetch = +refs/heads/*:refs/remotes/origin/*
[branch "master"]
	remote = origin
	merge = refs/heads/master
...
~~~~~~
</div>

## Synchronisation des branches

Bien, maintenant que tout semble bon, il faut encore s'occuper de quelques petites choses (sinon, SVN suffirait).
Git est complètement orienté sur la décentralisation et la création de nouvelles branches sur le même poste. Synchroniser des branches sur plusieurs serveurs différent n'est pas une opération naturelle.

C'est pourquoi j'ai créé deux simples scripts pour automatiser cette opération. Un script pour créer un branche localement et en ligne. Un autre script pour récupérer les branches en lignes qui ne sont pas présente localement.

Ainsi, lorsque je veux créer une nouvelle branche (localement et ligne) ; je lance le script :

<div><code class="zsh">git-create-new-branch branch_name</code></div>

et quand je suis sur un autre ordinateur et que je veux récupérer les branches crées sur un autre poste, j'exécute :

<div><code class="zsh">git-get-remote-branches</code></div>

Voici le code des deux script (en zsh) : 

<div>
~~~~~~ {.zsh}
#!/usr/bin/env zsh

if (($#<1)); then
    print -- "usage: $0:t branch_name" >&2
    exit 1
fi

branch=$1
git br ${branch}
git co ${branch}
git config branch.${branch}.remote origin
git config branch.${branch}.merge refs/heads/${branch}
~~~~~~
</div>

<div>
~~~~~~ {.zsh}
#!/usr/bin/env zsh

# recup branches not on local
localbranches=( $(git br | sed 's/\*/ /') )
remoteMissingBranches=( $(git br -r | \
    egrep -v "origin/HEAD|(${(j:|:)localbranches})" ) )
for br in $remoteMissingBranches; do
  branch=${br#origin/}
  print "get remote branch $branch"
  git br ${branch}
  git config branch.${branch}.remote origin
  git config branch.${branch}.merge refs/heads/${branch}
done
~~~~~~
</div>
