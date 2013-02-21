-----
isHidden:       false
menupriority:   30
kind:           article
published: 2009-11-12
title: Git for n00b
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Command List
tags:  git
-----

# Command List

## Command for each functionality

In the first part, we saw the list of resolved problem by [Git][git]. To resume [Git][git] should do:

- get others modifications,
- send modifications to others,
- get back in time,
- list differences between each version,
- name some versions in order to refer easily to them,
- write an historic of modifications,
- know who did what and when,
- manage conflicts,
- easily manage branches.

### get others modifications

<div><code class="zsh">
$ git pull
</code></div>

### send modifications to others

<div><code class="zsh">
$ git push
</code></div>

or more generally

<div><code class="zsh">
$ git pull
$ git push
</code></div>

### get back in time

#### For all tree

<div><code class="zsh">
$ git checkout
</code></div>

<div><code class="zsh">
$ git revert
</code></div>

revert three version before (see my `.gitconfig` file).

<div><code class="zsh">
$ git uncommit 3
</code></div>

Undo the las merge (if something goes wrong)

<div><code class="zsh">
$ git revertbeforemerge
</code></div>

#### For one file

<div><code class="zsh">
$ git checkout file
$ git checkout VersionHash file
$ git checkout HEAD~3 file
</code></div>

### list differences between each version

list files being modified

<div><code class="zsh">
$ git status
</code></div>

differences between last version files and local files

<div><code class="zsh">
$ git diff
</code></div>

differences between some version and local files

<div><code class="zsh">
$ git diff VersionHash fichier
</code></div>

### name some version to refer to them in the future

<div><code class="zsh">
$ git tag 'toto'
</code></div>

### show historic of modifications

<div><code class="zsh">
$ git log
$ git lg
$ git logfull
</code></div>

### know who did what and when

<div><code class="zsh">
$ git blame fichier
</code></div>

### handle conflicts

<div><code class="zsh">
$ git conflict
</code></div>

### manage branches

To create a branch: 

<div><code class="zsh">
$ git branch branch_name
</code></div>

To change the current branch: 

<div><code class="zsh">
$ git checkout branch_name
</code></div>

[git]: http://git-scm.org "Git"
