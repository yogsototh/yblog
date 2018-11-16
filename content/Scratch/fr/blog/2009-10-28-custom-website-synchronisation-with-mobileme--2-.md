-----
menupriority:   1
theme: brutalist
kind:           article
published: 2009-10-28
title: Synchronisation avec mobileme (2)
author: Yann Esposito
authoruri: yannesposito.com
tags:  Apple, mobileme, WebDav, synchronisation, zsh, script
-----

J'ai déjà discuté de la façon dont je [synchronise mon site web sur mobileme](/Scratch/fr/blog/10_Synchronize_Custom_WebSite_with_mobileMe). J'ai amélioré mon script pour le rendre incrémental.

Voici mon script, il créé tout d'abord un fichier qui contient la liste des fichiers avec leur *hash*. Afin de les comparer avec ceux qui sont en ligne sans avoir à les parcourir. Ensuite pour chaque fichier qui semble différent, je met à jour le contenu.

Cependant même avec ce script j'ai encore des problèmes. Dû à webdav. En particulier le renommage de répertoire. Par exemple :

<div><code class="zsh">
mv folder folder2
</code></div>

Retourne OK et pourtant : 

<div><code class="zsh">
$ ls 
folder folder2
</code></div>

Bouuhh...

Pour résoudre ce type de problèmes j'utilise un *framework* en zsh. Il résout presque tous les problèmes liés à webdav à l'exception du renommage de répertoire.

<div><code class="zsh" file="webdav-framework">
#!/usr/bin/env zsh

function samelineprint {
    print -n -P -- "\r$*"
}

# avec 1 essai par seconde: 300 = 5 minutes
maxessais=300

# try to create a directory until success
function trymkdir {
    target="$1"
    print -- mkdir -p $target
    local essai=1
    while ! mkdir -p $target; do
        samelineprint "Echec: essai n°$essai"
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    print
}

# try to copy until success
function trycp {
    element="$1"
    target="$2"
    if [[ ! -d ${target:h} ]]; then
        trymkdir ${target:h}
    fi
    local essai=1
    print -- cp $element $target
    while ! \cp $element $target; do
        samelineprint "Echec: essai n°$essai"
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    print
}

# try to remove until success
function tryrm {
    target="$1"
    local essai=1
    local options=''
    [[ -d $target ]] && options='-rf'
    print -- rm $options $target
    while ! rm $options $target; do
        samelineprint "Echec: essai n°$essai"
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    essai=1
    while [[ -e $element ]]; do
        samelineprint "rm reussi mais fichier source non disparu n°$essai"
        sleep 1
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    print
}

# try to rename until success
function tryrename {
    element="$1"
    target="$2"
    local essai=1
    while [[ -e $target ]]; do
        samelineprint "Echec n°$essai le fichier $target existe déjà"
        ((essai++))
        ((essai>maxessais)) && exit 5
        sleep 1
    done
    print -- mv $element $target
    while ! mv $element $target; do
        samelineprint "Echec: essai n°$essai"
        ((essai++))
        ((essai>maxessais)) && exit 4
    done
    essai=1
    while [[ -e $element ]]; do
        samelineprint "mv reussi mais fichier source non disparu n°$essai"
        sleep 1
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    print
}

# try to move until success
function trymv {
    element="$1"
    target="$2"
    local essai=1
    print -- mv $element $target
    while ! mv $element $target; do
        samelineprint "Echec: essai n°$essai"
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    essai=1
    while [[ -e $element ]]; do
        samelineprint "mv reussi mais fichier source non disparu n°$essai"
        sleep 1
        ((essai++))
        ((essai>maxessais)) && exit 5
    done
    print
}
</code></div>

Et voici le code qui me permet de synchroniser mon site web. Il y a une partie un peu incompréhensible. C'est pour enlever les mail réencodés par le filtre bluecloth qui est une implémentation de markdown. Mes mails, sont encodés à chaque fois de façon différente à chaque réengendrement de page html. C'est pourquoi je les enlève pour ne pas les *uploadés* inutilement à chaque fois.

<div><code class="zsh" file="publish">
#!/usr/bin/env zsh

# Script synchronisant le site sur me.com
# normalement, le site est indisponible le moins de temps possible
# le temps de deux renommages de répertoire

# get configuration
# mostly directories
source $0:h/config

# get trycp function (copy until success)
source $0:h/webdav-framework

if [[ $1 == '-h' ]]; then
    print -- "usage : $0:h [-h|-s|-d]"
    print -- "  -a sychronise aussi l'index"
    print -- "  -h affiche l'aide"
    print -- "  -d modification directe (pas de swap)"
    print -- "  -s swappe simplement les répertoires"
fi

# publication incrementale
function incrementalPublish {
    local ydestRep=$destRep$suffix
    localRef="$srcRep/map.yrf"
    print -- "Creation du fichier de references"
    create-reference-file.sh > $localRef
    remoteRef="/tmp/remoteSiteMapRef.$$.yrf"
    if [[ ! -e "$ydestRep/map.yrf" ]]; then
        # pas de fichier de reference sur la cible
        print -- "pas de fichier de reference sur la cible, passage en mode rsync"
        rsyncPublish
        swap
    else
        trycp "$ydestRep/map.yrf" "$remoteRef"
        typeset -U filesToUpdate
        filesToUpdate=( $(diff $localRef $remoteRef | awk '/^[<>]/ {print $2}' ) )
        if ((${#filesToUpdate} == 1)); then
            print -- "Seul le fichier ${filesToUpdate} sera téléversé"
        elif ((${#filesToUpdate}<10)); then
            print -- "${#filesToUpdate} fichiers seront téléversés :"
            print -- "${filesToUpdate}"
        else
            print -- "${#filesToUpdate} fichiers seront téléversés"
        fi
        # copy all file with some differences
        # except the map in case of error
        for element in $filesToUpdate; do
            if [[ $element == "/map.yrf" ]]; then
                continue
            fi
            if [[ -e $srcRep$element ]]; then
                trycp $srcRep$element $ydestRep$element
            else
                tryrm $ydestRep$element
            fi
        done
        # if all went fine, copy the map file
        trycp $srcRep/map.yrf $ydestRep/map.yrf
        # remove the temporary file
        \rm $remoteRef
        # if we have used the tmp directory we swap
        if [[ "$suffix" != "" ]]; then
            swap
        fi
    fi
}

# publication via rsync
function rsyncPublish {
    result=1
    essai=1
    while (( $result > 0 )); do
        print -- rsync -arv $srcRep/ $destRep.tmp
        if ((!testmode)); then
            rsync -arv $srcRep/ $destRep.tmp
        fi
        result=$?
        if (( $result > 0 )); then
            print -P -- "%BEchec du rsync%b (essai n°$essai)" >&2
        fi
        ((essai++))
    done
}

# swap
function swap {
    print -P -- "%B[Directory Swap (tmp <=> target)]%b"
    [[ -e $destRep.old ]] && tryrm $destRep.old

    print -- "  renommage du repertoire sandard vers le .old"
    tryrename $destRep $destRep.old 

    print -- "  renommage du repertoire tmp (nouveau) vers le standard"
    print -P -- "%B[Site Indisponible]%b $(date)"
    tryrename $destRep.tmp $destRep
    print -P -- "%B[Site Disponible]%b $(date)"

    print -- "  renommage du repertoire old vers le tmp"
    tryrename $destRep.old $destRep.tmp

    print -P -- "  publication terminée"
}

print -- "Root = $webroot"
print -- "Dest = $destRep"

if [[ "$1" = "-s" ]]; then
    swap
else 
    print -P "Copie de l'init"
    \cp -f $webroot/Scratch/multi/index.html $webroot/index.html

    if [[ "$1" = "-d" ]]; then
        suffix=""
    else
        suffix=".tmp"
    fi
    print -P -- "%BSync%b[${Root:t} => ${destRep:t}$suffix]"
    incrementalPublish
fi
</code></div>

C'est ma façon de remplacer `rsync` avec des filesystem qui ne permettent pas de l'utiliser. J'espère que ça pourra vous être utile. Je serai heureux de savoir si quelqu'un à une idée sur comment gérer le problème de renommage de répertoire avec webdav.

