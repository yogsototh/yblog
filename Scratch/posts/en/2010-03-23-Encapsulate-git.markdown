----- 
isHidden:       false
menupriority:   1
kind:           article
created:           2010-03-23T22:37:36+02:00
title: Encapsulate git
authorName: Yann Esposito
authorUri: yannesposito.com
tags: git, protection, branches, diverged 
-----

<div class="intro">
Here is a solution to maintain divergent branches in git. Because it is easy to merge by mistake. I give a script that encapsulate git in order to forbid some merge and warn you some merge should be dangerous.
</div>

## how to protect against your own dumb

I work on a project in which some of my git branches should remain divergent. And divergences should grow.

I also use some branch to contain what is common between projects.

Say I have some branches:

master: common to all branches
dev:    branch devoted to unstable development
client: branch with features for all client but not general enough for master
clientA: project adapted for client A
clientB: project adapted for client B

Here how I want to work: 

blogimage("dynamic_branching.png","Dynamic branching")

And more precisely the branch hierarchy: 

blogimage("branch_hierarchy.png","Branch hierarchy")

An arrow from A to B means, you can merge A in B. If there is no arrow from A to B that means it is *forbidden* to merge A in B. Here is the corresponding rubycode:

<div><code class="ruby">
$architecture={ 
    :master => [ :dev, :client ],
    :dev => [ :master ],
    :client => [ :clientA, :clientB ] }
</code></div>

Having a `:master => [ :dev, :client ]` means you can merge `master` branch into `dev` and `client`.

If by mistake I make a `git checkout master && git merge clientA`, I made a mistake. This is why I made a script which encapsulate the git behaviour to dodge this kind of mistake.

But this script do far more than that. It also merge from top to down. The action `allmerges` will do:

<div><code class="zsh">
git co dev && git merge master
git co client && git merge master
git co clientA && git merge client
git co clientB && git merge client
</code></div>

That means, I can update all branches. The algorithm will not make loop even if there is a cycle in the branch hierarchy.

Here it is:

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
            system %{git merge #{src_branch}}
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

All you need to do to make it work is simply to copy eng in a directory contained in your PATH.

Of course try to use as few as possible `cherry-pick` and `rebase`. This script was intended to work with workflow using `pull` and `merge`.
