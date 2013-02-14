-----
isHidden:       false
menupriority:   1
kind:           article
published: 2009-09-11
title: Synchronize Custom WebSite with mobileMe
authorName: Yann Esposito
authorUri: yannesposito.com
tags: Apple, mobileme, WebDav, synchronisation, zsh, script 
-----

# Update <small>(2012/01/11)</small>

iDisk should soon disapear. This entry is mainly obsolescent now.

# Update <small>(2009/10/28)</small>

I [updated my script](/Scratch/en/blog/2009-10-28-custom-website-synchronisation-with-mobileme--2-) which is now incremental. Since the writing of this article, Apple(c) had made many efforts about the bandwith of its European servers.

# WebDav terror

I live in France and iDisk upload is just terrible. Upload speed remind me the old 56k modem. Most operations such as list the content of a directory take at least 30 seconds (for 15 elements). Renaming a directory fail most of time.

Apple(c) use a WebDav server to host files. It works on port 80 (like http). I realized WebDav via https work better (2 to 3 times faster with far less errors). But even https is too slow.

I upload from my Mac and sometimes from an Ubuntu PC (iDisk mounted with webdavfs).

# Synchronize safely the website

Here is the script I use in order to synchronize my website with maximum safety. It try each operations until it works.

The idea are: 

  - synchronize to a temporary folder then swap the name therefore the website isn't accessible only during the swap time. It takes only the time of two rename.
  - reiterate all operations until they work (for example, renaming).

For now I use `rsync` which in fact is no more efficient than a simple `cp` with WebDav. And I should use a method to keep track of elements who have changed. before the publication.

In fact when I'm on a Mac, I use [Transmit](http://www.panic.com/transmit) which is very cool and far more efficient than the Finder to synchronize files. After the synchronization, I swap the directories.

My script take a `-s` option in order to make only the swap option. It also take a `-a` in order to put the new `index.html` which should point to the new homepage (not the iWeb one).

In order to keep this script working for you, just modify the username by yours (the value of the `mobileMeUser`).

<div class="fr">
<code class="zsh" file="publish">
#!/usr/bin/env zsh

# Script synchronisant le site sur me.com
# normalement, le site est indisponible le moins de temps possible
# le temps de deux renommages de répertoire

mobileMeUser="yann.esposito"
siteName="siteName"

# Depending of my hostname the 
if [[ $(hostname) == 'ubuntu' ]]; then
    iDisk='/mnt/iDisk'
else
    iDisk="/Volumes/$mobileMeUser"
fi

root=$HOME/Sites/$siteName
destRep=$iDisk/Web/Sites/$siteName

[[ ! -d $root ]] && { 
    print -- "$root n'existe pas ; vérifiez la conf" >&2; 
    exit 1 
}

[[ ! -d $destRep ]] && { 
    print -- "$destRep n'existe pas, veuillez remonter le FS" >&2; 
    exit 1 
}

if [[ $1 == '-h' ]]; then
    print -- "usage: $0:h [-h|-a|-s]"
    print -- "  -a sychronise aussi l'index"
    print -- "  -h affiche l'aide"
    print -- "  -s swappe simplement les répertoires"
fi

if [[ $1 == '-a' ]]; then
    print -- "Synchronisation de l'index (${destRep:h})"
    rsync -av $root/index.html ${destRep:h}/index.html
fi

print -- "Root = $root"
print -- "Dest = $destRep"

if [[ ! $1 = '-s' ]]; then
    [[ ! -d $destRep.tmp ]] && mkdir $destRep.tmp
    print -P -- "%B[Sync => tmp]%b"
    result=1
    essai=1
    while (( $result > 0 )); do
        rsync -arv $root/Scratch/ $destRep.tmp
        result=$?
        if (( $result > 0 )); then
            print -P -- "%BEchec du rsync%b (essai n°$essai)" >&2
        fi
        ((essai++))
    done
fi

# SWAP
print -P -- "%B[Swap des Répertoires (tmp <=> target)]%b"
essai=1
while [[ -e $destRep.old ]]; do
    print -n -- "suppression de $destRep.old"
    if ((essai>1)); then 
        print " (essai n°$essai)"
    else
        print
    fi
    ((essai++))
    \rm -rf $destRep.old
done

print -- "  renommage du repertoire sandard vers le .old"
essai=1
while [[ -e $destRep ]]; do
    mv $destRep $destRep.old 
    (($?)) && print -- "Echec du renommage (essai n°$essai)" >&2
    ((essai++))
done

print -- "  renommage du repertoire tmp (nouveau) vers le standard"
print -P -- "  %BSite Indisponible%b $(date)"
essai=1
while [[ ! -e $destRep ]]; do
    mv $destRep.tmp $destRep
    (($?)) && print -P -- "%B[Site Indisponible]%b(essai n°$essai) Echec du renommage (mv $destRep.tmp $destRep)" >&2
    ((essai++))
done

print -P -- "\t===\t%BSITE DISPONIBLE%b\t==="

print -- "  renommage du repertoire old vers le tmp"
essai=1
while [[ ! -e $destRep ]]; do
    mv $destRep.old $destRep.tmp
    (($?)) && print -P -- "Echec du renommage n°$essai" >&2
    ((essai++))
done

print -P -- "  publication terminée"
</code>
</div>

<div class="en">
<code class="zsh" file="publish">
#!/usr/bin/env zsh

# Author: Yann Esposito
#   Mail: yann.esposito@gmail.com
# Synchronize with "mobileMe" iDisk account.

mobileMeUser="firstname.lastname"
siteName="siteName"

# Depending of my hostname the 
if [[ $(hostname) == 'ubuntu' ]]; then
    iDisk='/mnt/iDisk'
else
    iDisk="/Volumes/$mobileMeUser"
fi

root=$HOME/Sites/$siteName
destRep=$iDisk/Web/Sites/$siteName

[[ ! -d $root ]] && { 
    print -- "$root does not exist ; please verify the configuration ($0)" >&2; 
    exit 1 
}

[[ ! -d $destRep ]] && { 
    print -- "$destRep does not exist, please mount the filesystem" >&2; 
    exit 1 
}

if [[ $1 == '-h' ]]; then
    print -- "usage: $0:h [-h|-a|-s]"
    print -- "  -a sychronize primary index"
    print -- "  -h show this help"
    print -- "  -s only swap directories"
fi

if [[ $1 == '-a' ]]; then
    print -- "Index synchronisation (${destRep:h})"
    rsync -av $root/index.html ${destRep:h}/index.html
fi

print -- "Root = $root"
print -- "Dest = $destRep"

if [[ ! $1 = '-s' ]]; then
    [[ ! -d $destRep.tmp ]] && mkdir $destRep.tmp
    print -P -- "%B[Sync => tmp]%b"
    result=1
    essai=1
    while (( $result > 0 )); do
        rsync -arv $root/Scratch/ $destRep.tmp
        result=$?
        if (( $result > 0 )); then
            print -P -- "%Brsync failed%b (try n°$essai)" >&2
        fi
        ((essai++))
    done
fi

# SWAP
print -P -- "%B[Directory Swap (tmp <=> target)]%b"
essai=1
while [[ -e $destRep.old ]]; do
    print -n -- "remove $destRep.old"
    if ((essai>1)); then 
        print " (try n°$essai)"
    else
        print
    fi
    ((essai++))
    \rm -rf $destRep.old
done

print -- "  renommage du repertoire sandard vers le .old"
essai=1
while [[ -e $destRep ]]; do
    mv $destRep $destRep.old 
    (($?)) && print -- "Failed to rename (try n°$essai)" >&2
    ((essai++))
done

print -- "  renaming folder tmp (new) to the standard one"
print -P -- "  %BThe WebSite isn't working%b $(date)"
essai=1
while [[ ! -e $destRep ]]; do
    mv $destRep.tmp $destRep
    (($?)) && print -P -- "%B[WebSite not working]%b(try n°$essai) Failed to rename (mv $destRep.tmp $destRep)" >&2
    ((essai++))
done

print -P -- "\t===\t%BWEBSITE SHOULD WORK NOW%b\t==="

print -- "  rename old folder to tmp folder"
essai=1
while [[ ! -e $destRep ]]; do
    mv $destRep.old $destRep.tmp
    (($?)) && print -P -- "Failed to rename n°$essai" >&2
    ((essai++))
done

print -P -- "  Publish terminated"
</code>
</div>
