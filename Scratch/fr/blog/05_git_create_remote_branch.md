-----
kind: article
menupriority: 1
published: 2009-08-17
title: Création de branches externe avec Git
author: Yann Esposito
authoruri: yannesposito.com
tags:  git, branch, local, remote
-----

## Créer une branche Git externe facilement

J'utilise Git pour synchroniser des projets personnels. 
C'est pourquoi quand je crée une branche locale je souhaite quasiment toujours qu'elle soit aussi créée en *externe* (remote).

Voici le script que j'utilise pour accomplir cette tâche : 

<div>
    <code class="zsh" file="git-create-new-branch.sh">
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
    </code>
</div>

Bien sûr, je suppose qu'<code>origin</code> est déjà configurée.

