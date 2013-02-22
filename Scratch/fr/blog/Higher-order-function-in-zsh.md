-----
isHidden:       false
image: /Scratch/img/blog/Higher-order-function-in-zsh/main.jpg
menupriority:   1
kind:           article
published: 2011-09-28
title: Fonctions d'ordre supérieur en zsh
author: Yann Esposito
authoruri: yannesposito.com
tags:  zsh, map, foldr, filter, functional, programming, higher order functions
-----
blogimage("main.jpg","Title image")

<div class="intro">

UPDATE: [Nicholas Sterling a découvert un moyen de faire des fonctions anonymes](http://nicholassterling.wordpress.com/2012/03/30/a-zsh-map-function/) 
Merci!

Avec cette dernière version vous pouvez utiliser `map` si vous utilisez
des fonctions déclarées. `mapl` pour les fonctions anonymes 
et `mapa` pour les fonctions arithmétiques.

Exemple :

<code class="zsh">
$ filterl 'echo $1|grep a >/dev/null' ab cd ef ada
ab
ada

$ folda '$1+$2' {1..5}
15

$ folda '$1*$2' {1..20}
2432902008176640000

$ mapl 'echo X $1:t Y' ~/.zsh/functional/src/*
X each Y
X filter Y
X fold Y
X map Y

$ mapa '$1*2' {1..3}
2
4
6

$ mapl 'echo result $1' $(mapa '$1+5' $(mapa '$1*2' {1..3}))
result 7
result 9
result 11

</code></pre>

%tlal des fonctions d'ordres supérieurs en zsh.

</div>

Tout d'abord, pourquoi c'est important d'avoir ces fonctions. 
Plus je programmais avec zsh plus j'essayais d'avoir un style fonctionnel.

Le minimum pour pouvoir avoir du code plus lisible c'est de posséder les fonctions `map`, `filter` et `fold`.

Voici pourquoi avec une comparaison.
Commençons par un programme qui converti tous les gif en png dans plusieurs répertoires projets contenant tous des répertoires resources.
Avant :

Avant ⇒

<code class="zsh">
# for each directory in projects dir
for toProject in /path/to/projects/*(/N); do
    # toProject is /path/to/projects/foo
    # project become foo (:t for tail)
    project=${toProject:t}
    for toResource in $toProject/resources/*.gif(.N); do
        convert $toResource ${toResource:r}.png && \
        \rm -f $toResource
    done
done
</code></pre>

- Le `(/N)` permet de sélectionner seulement les répertoires sans casser la boucle s'il n'y a pas de "match".
- Le `(.N)` permet de sélection seulement les fichiers, aussi sans tout arréter s'il ne trouve rien.
- Le `:t` signfie "tail" ; si `toto=/path/to/file.ext` alors `${toto:t}=file.ext`.

Après

<code class="bash">
gif_to_png() { convert $1 ${1:r}.png && \rm -f $1 }

handle_resources() { map gif_to_png $1/resources/*.gif(.N) }

map handle_resources /path/to/projects/*(/N)
</code></pre>

Plus de bloc ! 
Oui, c'est un poil plus difficile à lire pour les non initiés. 
Mais c'est à la fois plus concis et plus robuste.

Et encore ce code ne possède pas de test.
Recommençons sur le même principe.

Trouver les fichiers des projets qui ne contiennent pas de s dans leur nom qui ont le même nom que leur projet.

Before ⇒

<code class="zsh">
for toProject in Projects/*; do
    project=$toProject:t
    if print -- project | grep -v s >/dev/null
    then
        print $project
        for toResource in $toProject/*(.N); do
            if print -- ${toResource:t} | grep $project >/dev/null; then
                print -- "X $toResource"
            fi
        done
    fi
done
</code></pre>

After ⇒

<code class="zsh">
contain_no_s() { print $1 | grep -v s }

function verify_file_name {                               
    local project=$1:t
    contains_project_name() { print $1:t | grep $project }
    map "print -- X" $(filter contains_project_name $1/*(.N))
}

map verify_file_name $( filter contain_no_s Projects/* )
</code></pre>

La première version peu paraître plus facile à lire.
Mais la seconde est plus bien supérieure en terme d'architecture.
Je ne veux pas discuster ici pourquoi c'est mieux.
Je vous demande simplement de me croire quand je dis que l'approche fonctionnelle est supérieure.

Vous pouvez télécharger [une version à jour du code (merci à Arash Rouhani)](https://github.com/Tarrasch/zsh-functional). 
Une ancienne version est [ici](https://github.com/yogsototh/zsh_functional).
Voici le code source (de la première version) :

<code class="zsh" file="functional.sh">
#!/usr/bin/env zsh

# Provide higer-order functions 

# usage:
#
# $ foo(){print "x: $1"}
# $ map foo a b c d
# x: a
# x: b
# x: c
# x: d
function map {
    local func_name=$1
    shift
    for elem in $@; print -- $(eval $func_name $elem)
}

# $ bar() { print $(($1 + $2)) }
# $ fold bar 0 1 2 3 4 5
# 15
# -- but also
# $ fold bar 0 $( seq 1 100 )
function fold {
    if (($#<2)) {
        print -- "ERROR fold use at least 2 arguments" >&2
        return 1
    }
    if (($#<3)) {
        print -- $2
        return 0
    } else {
        local acc
        local right
        local func_name=$1
        local init_value=$2
        local first_value=$3
        shift 3
        right=$( fold $func_name $init_value $@ )
        acc=$( eval "$func_name $first_value $right" )
        print -- $acc
        return 0
    }
}

# usage:
#
# $ baz() { print $1 | grep baz }
# $ filter baz titi bazaar biz
# bazaar
function filter {
    local predicate=$1
    local result
    typeset -a result
    shift
    for elem in $@; do
        if eval $predicate $elem >/dev/null; then
            result=( $result $elem )
        fi
    done
    print $result
}
</code></pre>
