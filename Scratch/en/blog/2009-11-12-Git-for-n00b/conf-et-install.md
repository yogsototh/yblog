-----
isHidden:       false
menupriority:   2
kind:           article
published: 2009-11-12
title: Git for n00b
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Configure before Use
tags:  git
-----

# Before usage, configuration

## install

Under Linux Ubuntu or Debian:

<div><code class="zsh">$ sudo apt-get install git</code></div>

Under Mac OS X:

* install [MacPorts](http://macports.org/install.php)
* install [Git][git]

<div><code class="zsh">
$ sudo port selfupdate

$ sudo port install git-core
</code></div>

## Global configuration

Save the following file as your `~/.gitconfig`.

<div><code class="zsh" file="gitconfig">
[color]
    branch = auto
    diff   = auto
    status = auto
[alias]
    st        = status
    co        = checkout
    br        = branch
    lg        = log --pretty=oneline --graph
    logfull   = log --pretty=fuller --graph --stat -p
    unstage   = reset HEAD
    # there should be an article on what this command do
    uncommit = !zsh -c '"if (($0)); then nb=$(( $0 - 1 )); else nb=0; fi; i=0; while ((i<=nb)); do git revert -n --no-edit HEAD~$i; ((i++)); done; git commit -m \"revert to $0 version(s) back\""'
    undomerge = reset --hard ORIG_HEAD
	conflict  = !gitk --left-right HEAD...MERGE_HEAD
    # under Mac OS X, you should use gitx instead
	# conflict    = !gitx --left-right HEAD...MERGE_HEAD
[branch]
	autosetupmerge = true
</code></div>

You can achieve the same result using for each entry the command: `git config --global`. Next, configure your name and your email. For example, if your name is John Doe and your email is `john.doe@email.com`. Launch the following commands:

<div><code class="zsh">
$ git config --global user.name John Doe

$ git config --global user.email john.doe@email.com
</code></div>

Here it is. Base configuration is over. The file containing alias will help to type shorter commands.

## Get a project

If a project is already versionned with [Git][git] you should have an `URL` of the sources. Then use the following command:

<div><code class="zsh">
$ cd ~/Projets
$ git clone git://main.server/path/to/file
</code></div>

If there is no git server but you've got an `ssh` access. Just replace the `git://host` by `ssh://user@host`. In order not to type your password each time, use:

<div><code class="zsh">
$ ssh-keygen -t rsa
</code></div>

Reply to question and **do not enter* a password. Then copy your keys to the distant server. This is not the safest way to do this. The safest being, using `ssh-agent`.

The easiest way if you have `ssh-copy-id`:
<div><code class="zsh">
me@locahost$ ssh-copy-id ~/.ssh/id_rsa.pub me@main.server
</code></div>

or manually

<div><code class="zsh">
me@locahost$ scp ~/.ssh/id_rsa.pub me@main.server:
me@locahost$ ssh me@main.server
password:
me@main.server$ cat id_rsa.pub >> ~/.ssh/authorized_keys
me@main.server$ rm id_rsa.pub
me@main.server$ logout
</code></div>

Now you don't need to write your password to access the `main.server`.

## Creating a new project

Suppose you already have a project with files. Then it is really easy to version it.

<div><code class="zsh">
$ cd /path/to/project
$ git init
$ git add .
$ git commit -m "Initial commit"
</code></div>

Let do a small remark. If you don't want to *version* every file. Typically intermediate compilation file, swap files... Then you need to exclude them. Just before launching the `git add .` command. You need to create a `.gitignore` file in the root directory of your project. This file will contain all exclude *pattern*. For example:

<div><code class="zsh">
*.o
*.bak
*.swp
*~
</code></div>

Now, if you want to create a repository on a distant server, it *must* not be in `bare` mode. The repository will contain only versionning informations, but not the files of the project. To achieve that:

<div><code class="zsh">
$ cd /path/to/local/project
$ git clone --bare . ssh://server/path/to/project
</code></div>

Others will be able to get your modifications.

<div><code class="zsh">
git clone ssh://server/path/to/project
</code></div>

## Abstract of the second step

You now have a local directory on your computer. It is versionned and you can say it is, because there is a `.git` directory at the root (and the root only) of your project. This directory contain all necessary informations for [Git][git] to version your project.

Now you only need to know how to use it.

[git]: http://git-scm.org "Git"
