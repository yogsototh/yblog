-----
isHidden:       false
theme: scientific
image: /Scratch/img/blog/2010-03-23-Encapsulate-git/dynamic_branching.png
menupriority:   1
kind:           article
published: 2010-03-23
title: Encapsuler git
author: Yann Esposito
authoruri: yannesposito.com
tags:  git, protection, branches
-----

<span class="intro">
Voici une solution pour conserver des branches divergentes avec `git`.
Parce qu'il est facile de *merger* par erreur, je propose un script qui encapsule le comportement de `git` pour interdire certains *merges* dangereux. Mais qui permet aussi de faire des merges en cascades de la racines vers les autres branches.
</span>

## Se prémunir contre les erreurs

Je travaille sur un projet dans lequel certaines de mes branches `git` doivent rester divergentes. Et les divergences devraient aller en s'accentuant.

J'utilise aussi certaines branches qui contiennent la partie commune de ces projets.

Disons que j'ai les branches :

- master: commune à toutes les branches
- dev:    branche instable pour le développement
- client: Branche commune à plusieurs clients
- clientA: le projet spécialisé pour le client A
- clientB: le projet spécialisé pour le client B

Voilà comment je souhaiterai que ça fonctionne

blogimage("dynamic_branching.png","Dynamic branching")

Et plus précisément la hiérarchie des branches :

blogimage("branch_hierarchy.png","Branch hierarchy")

Une flèche de A vers B signifie que l'on peut merger A dans B. S'il n'y a pas de flèche de A vers B cela signifie qu'il est *interdit* de merger A dans B. Voici le code ruby correspondant :

<div><code class="ruby">
$architecture={ 
    :master => [ :dev, :client ],
    :dev => [ :master ],
    :client => [ :clientA, :clientB ] }
</code></div>

`:master => [ :dev, :client ]` signifie que l'on peut merger la branche `master` dans la branche `dev` et la branche `client`.

Je fait une erreur si je tape `git checkout master && git merge clientA`.
C'est pour éviter ça que j'ai fait un script pour encapsuler le comportement de `git`.

Mais ce script fait bien plus que ça. Il fait des merges en cascade de la racine vers les feuilles avec l'acion `allmerges`.

<div><code class="zsh">
git co dev && git merge master
git co client && git merge master
git co clientA && git merge client
git co clientB && git merge client
</code></div>

Je peux ainsi mettre à jour toutes les branches. L'algorithme ne boucle pas même s'il y a des cycles dans la structure des branches.  
Le voici :

<div class="small"><code class="ruby" file="eng">
#!/usr/bin/env ruby
# encoding: utf-8

# architecture
#
# master <-> dev
# master -> client
# clien -> clientA | clientB
#
# merge using two of these branches should be 
#   restricted to these rules
# merge to one of these branch and an unknown one should
#   raise a warning, and may the option to add this new branch
#   to the hierarchy

$architecture={ 
    :master => [ :dev, :client ],
    :dev => [ :master ],
    :client => [ :clientA, :clientB ] }

def get_current_branch()
    (`git branch --no-color | awk '$1 == "*" {print $2}'`).chop.intern
end

if ARGV.length == 0
    puts %{usage: $0:t [git_command or local_command]

local commands:
    allmerges: merge from top to down}
    exit 0
end

require 'set'
$known_branches=Set.new
$architecture.each do |k,v| 
    $known_branches.add(k)
    v.each { |b| $known_branches.add(b) }
end

def rec_merge(branch)
    if $architecture[branch].nil?
        return
    end
    $architecture[branch].each do |b|
        if $flag.has_key?(b.to_s + branch.to_s)
            next
        end
        flagname=branch.to_s + b.to_s
        if $flag.has_key?(flagname)
            next
        end
        if system %{eng checkout #{b}}
            if get_current_branch != b
                puts "Can't checkout to #{b}"
                exit 2
            end
            if system %{eng merge #{branch}}
                $flag[flagname]=true
                rec_merge(b)
            else
                exit 1
            end
        else
            exit 1
        end
    end
end

def do_all_merges
    puts 'Will merge from father to sons'
    current_branch=get_current_branch
    $flag={}
    rec_merge(:master)
    system %{git co #{current_branch}}
end

def do_merge
    current_branch=get_current_branch
    src_branch=ARGV[1].intern
    puts %{do_merge: #{src_branch} => #{current_branch}}
    if $known_branches.include?(current_branch)
        if $known_branches.include?(src_branch)
            if $architecture.has_key?(src_branch) and 
                $architecture[src_branch].include?(current_branch)
                system %{git merge #{src_branch}}
            else
                puts %{Forbidden merge: #{src_branch} => #{current_branch}}
            end
        else
            puts %{Warning! #{src_branch} not mentionned in rb configuration}
            sleep 2
            f system %{git merge #{src_branch}}
            puts %{Warning! #{src_branch} not mentionned in rb configuration}
        end
    end
end

case ARGV[0] 
    when 'allmerges' then do_all_merges
    when 'merge' then do_merge
    else system %{git #{ARGV.join(' ')}}
end
</code></div>

Pour que ça fonctionne il vous suffit de copier `eng` dans un répertoire présent dans votre `PATH`.

Bien entendu, il faut essayer de faire aussi peu que possible des `cherry-pick` et des `rebase`. Ce script a pour but de s'intégrer avec les workflow basé sur les `pull` et `merge`.
