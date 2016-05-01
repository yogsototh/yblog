-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2009-10-28
title: custom website synchronisation with mobileme (2)
author: Yann Esposito
authoruri: yannesposito.com
tags:  Apple, mobileme, WebDav, synchronisation, zsh, script
-----
I already talked about how [I synchronized my website with mobileme](/Scratch/en/blog/10_Synchronize_Custom_WebSite_with_mobileMe). I ameliorated this script in order to make it incremental.

Here is my new script, it first create a map which associate to each file its hash. After that it compare this file to the remote one. Then for each different file, update the content.

Even with this script I also have some problem. Mostly due to 'webdav' issues. For example, renaming a folder work really badly (on Linux at least). I use webdavfs. For example:

<div><code class="zsh">
mv folder folder2
</code></div>

It returns OK and I've got: 

<div><code class="zsh">
$ ls 
folder folder2
</code></div>

Booh....

In order to handle most webdav issues I use a *framework* in zsh. It handle almost all except the correct renaming of folder. Working on it... Anyway here is the code I use.

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

And here is the code on how I synchronize my website. There is a little cryptic code. It correspond a problem caused by the bluecloth filter which is a markdown program made in ruby. Each time my email is written it is transformed differently. This is why I remove this part from the content of each html file. Without it, all my files containing email are different at each regeneration of my website.

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
    if [[ "$1" = "-d" ]]; then
        suffix=""
    else
        suffix=".tmp"
    fi
    print -P -- "%BSync%b[${Root:t} => ${destRep:t}$suffix]"
    incrementalPublish
fi
</code></div>

This is my way to replace `rsync` with filesystem not handling it.
Hope it is usefull. I'll be happy to hear a way to handle the webdav rename folder problem. This is really annoying.

