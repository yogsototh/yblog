-----
isHidden:       false
menupriority:   1
kind:           article
published: 2009-11-12
title: Git for n00b
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Git for what?
tags:  git
-----

# [Git][git] for what?

<div class="intro">

If you just want to use [Git][git] **immediately**, just read dark part. You read this part later to understand correctly foundations of version systems and not doing strange things.

</div>

[Git][git] is a <abbr title="Decentralized Concurent Versions System">DCVS</abbr>, which means a Decentralized Concurrent Versions System. Let's analyze each part of this long term:

### Versions System

Firstly, versions system manage files.
When somebody work with files without a versions system, the following happens frequently:

When you modify a somehow critical file you don't want to loose. You copy naturally this file with another name. For example:

<div><code class="zsh">$ cp fichier_important.c fichier_important.c.bak</code></div>

In consequence of what, the new file, play the role of *backup*. If you break everything, you can always return in the last state by overwriting your modifications.
Of course, this method is not very professional and is a bit limited. If you make many modifications, you'll end with many files with strange names like:

<div>
<code class="zsh">
fichier_important.c.bak
fichier_important.c.old
fichier_important.c.Bakcup
fichier_important.c.BAK.2009-11-14
fichier_important.c.2009.11.14
fichier_important.c.12112009
old.fichier_important.c
</code></pre>
</div>

If you want to make it works correctly, you'll have to use naming convention. Files take many place even if you modify most of time only some lines.

*Fortunately, versions system are here to help.*

You only have to signal you want a new version of a file and the versions system will do the job for you. It will record the backup where it could be easily recovered. Generally, systems version do it better than you, making the backup only of the modified lines and not the total file.

Once upon a time versions were managed for each file separately. I think about CVS. Then it naturally appears projects are a coherent set of files. Recover each file separately was a tedious work. This is why versions number passed from files to the entire project.

It is therefore possible to say, "I want to get back three days earlier".

<div class="black">

*What gives versions system?* (I didn't mention everything at all)

- automatic backups: *back in time*,
- gives the ability to see differences between each version,
- put a *tag* on some version to be able to refer to them easily,
- gives the ability to see an historic of all modifications. Generally the user must add a comment for each new version.

</div>

### concurrent:

Version Systems are already useful to manage its own projects. They help to organize and resolve partially backup problems. I say partially because you have to backup your repository on a decent file system. But versions system are really interesting is on projects done by many people.

Let's begin by an example, a two person project ; Alex and Beatrice. On a file containing a *Lovecraft*'s gods list:

<div style="width: 10em; margin-left: auto; margin-right: auto">
<code class="zsh">
Cthulhu
Shubniggurath
Yogsototh
</code></div>

Say Alex is home and modify the file: 
<div style="width: 10em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Cthulhu
Shubniggurath
<span class="StringConstant"><strong>Soggoth</strong></span>
Yogsototh
</pre>
</div>

after that he send the file on the project server. Then on the server there is the Alex file:

A bit later, Beatrice who had not get the Alex file on the server make the modification:

<div style="width: 10em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Cthulhu
<span class="Constant"><strong>Dagon</strong></span>
Shubniggurath
Yogsototh
</pre>
</div>

Beatrice send her file on the server

Alex modification is *lost*. One more time, versions system are here to help.

A version system would had *merge* the two files at the time Beatrice send the file on the server. And like by magic, on the server the file would be:

<div style="width: 10em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Cthulhu
<span class="Constant"><strong>Dagon</strong></span>
Shubniggurath
<span class="StringConstant"><strong>Soggoth</strong></span>
Yogsototh
</pre>
</div>

In real life, at the moment Beatrice want to send her modifications, the versions system alert her a modification had occurred on the server. Then she uses a command which pull the modification from the server to her local computer. And this command update her file. After that, Beatrice send again the new file on the server.

<div class="black">

**In what Concurrent Versions System help?**

- get without any problem others modifications,
- send without any problem its own modifications to others,
- manage conflicts. I didn't speak about it, but sometimes a conflict can occur (when two different people modify the same line on a file for example). SVC help to resolve such problem. More on that later,
- help to know who done what and when.

</div>

### decentralized

This word became popular only recently about CVS. And it mainly means two things:

First, until really recently (SVN), you'll have to be connected to the distant server to get informations about a project. Like get the history. New decentralized systems work with a local *REPOSITORY* (directory containing backups and many informations linked to the versions system functionalities). Hence, one can view the history of a project without the need of being connected.

All instances of a project can live *independently*.

To be more precise, DCVS are base on the *branch* notion.

Practically, it has great importance. It means, everybody work separately, and the system help to glue all their work.

It is even more than just that. It help to code independently each feature and bug fixes. Under other system it was far more difficult.

Typical example:

> I develop my project. I'm ameliorating something. An urgent bug is reported.
> 
> With a DCVS I can easily, get back to the version with the bug. Fix it. Send the fix. Get back to my feature work. And even, use the fix for the new version with my new feature.
> 
> In a not decentralized version system, doing such a thing is possible but not natural. Decentralization means it become natural to use a branch for each separable work.

<div class="black">

**Advantages given by DCVS: **

- Ability to work offline,
- Ability to create many *atomic* patches,
- Help the maintenance of many different versions of the same application.

</div>

## To resume

Let's resume what we can easily do with DCVS:

**Versions Systems**

- back in time,
- list differences between versions,
- name some versions to refer to them easily
- show history of modifications

**Concurrent**

- get others modifications,
- send its modifications to others,
- know who done what and when,
- conflicts management.

**Decentralized**

- Easily manipulate branches

Now let's see how to obtain all these things easily with [Git][git].

[git]: http://git-scm.org "Git"
