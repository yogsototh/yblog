-----
isHidden:       false
menupriority:   1
kind:           article
published: 2011-08-17
title: Un diff plus pratique
author: Yann Esposito
authoruri: yannesposito.com
tags:  diff, git, couleurs
-----

`diff` est un utilitaire très pratique, mais il n'est pas facile à lire pour nous, les Hommes.

C'est pourquoi, lorsque vous utilisez `git`, il vous montre un formatage plus agréable avec des couleurs.

Voici le script que j'utilise lorsque je veux avoir un `diff` à la git.

<pre><code class="zsh" file="ydiff">#!/usr/bin/env zsh

# Load colors helpers
autoload -U colors && colors

function colorize_diff {
    while read line; do
    case ${line[0]} in
    +) print -n $fg[green];;
    -) print -n $fg[red];;
    @) # Display in cyan the @@ positions @@
       if [[ ${line[1]} = '@' ]]; then
           line=$(print $line | perl -pe 's#(\@\@[^\@]*\@\@)(.*)$#'$fg[cyan]'$1'$reset_color'$2#')
       fi;;

    esac
        print -- $line
        print -n $reset_color
        done
}

diff -u $* | colorize_diff
</code></pre>
