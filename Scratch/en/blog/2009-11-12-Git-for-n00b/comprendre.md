-----
isHidden:       false
menupriority:   10
kind:           article
published: 2009-11-12
title: Git for n00b
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Understanding
tags:  git
-----

# Why Git is cool?

Because with [Git][git] you can work on many part of some project totally independently. This is the true efficiency of decentralisation.

Each branch use the same directory. Then you can easily change your branch. You can also change branch when some files are modified. You can then dispatch your work on many different branches and merge them on one master branch at will.

Using the `git rebase` you can decide which modifications should be forget or merged into only one modification.

What does it mean for real usage? You can focus on coding. For example, you can code, a fix for bug b01 and for bug b02 and code a feature f03. Once finished you can create a branch by bug and by feature. And finally you can merge these modifications on a main branch.

All was done to code and decide how to organize your versions after. In other VCS it is not as natural as in [Git][git].

With [Git][git] you can depend of many different sources. Then, there is not necessarily a 'master' repository where everybody puts its modifications.

What changes the most with [Git][git] when you come from SVN, it's the idea of a centralized project on one server. With [Git][git] many people could work on the same project but not necessarily on the same *repository* as main reference. One can easily fix a bug and send a patch to many different versions of a project.

[git]: http://git-scm.org "Git"
