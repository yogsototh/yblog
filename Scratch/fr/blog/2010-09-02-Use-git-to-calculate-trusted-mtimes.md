-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-09-02
title: Utilisation de git pour calculer les mtimes
author: Yann Esposito
authoruri: yannesposito.com
tags:  nanoc, web, git
-----

Vous pouvez remarquer qu'à la fin de chaque page je donne une date de dernière modification.
Précédemment cette date était calculée en utilisant la date du fichier.
Mais il arrive fréquemment que je fasse un `touch` d'un fichier pour engendrer tout le site de nouveau.
Donc la date n'est pas nécessairement la _vraie_ de modification du contenue.

J'utilise [git](http://git-scm.org) pour _versionner_ mon site web.
Et cet outil me permet de récupérer la dernière date de _vraie_ modification d'un fichier.
Voici comment je m'y prend avec [nanoc](http://nanoc.stoneship.org) :

<code class="ruby" file="gitmtime.rb">
def gitmtime
    filepath=@item.path.sub('/Scratch/','content/html/').sub(/\/$/,'')
    ext=%{.#{@item[:extension]}}
    filepath<<=ext
    if not FileTest.exists?(filepath)
        filepath.sub!(ext,%{#{@item.raw_filename}#{ext}})
    end
    str=`git log -1 --format='%ci' -- #{filepath}`
    if str.nil? or str.empty?
        return Time.now
    else
        return DateTime.parse( str )
    end
end
</code>

Bien entendu je sais que c'est très lent et absolument pas optimisé.
Mais ça fonctionne comme prévu.
Maintenant la date que vous voyez en bas de la page correspond exactement à la dernière date de modification de son contenu.

_Mise à jour_:
Je tiens à remercier Eric Sunshine et Kris pour leurs conseils sur ce problème.
