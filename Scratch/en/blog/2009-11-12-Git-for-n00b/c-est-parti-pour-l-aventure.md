-----
isHidden:       false
menupriority:   3
kind:           article
published: 2009-11-12
title: Git for n00b
author: Yann Esposito
authoruri: yannesposito.com
subtitle: The Adventure Begins
tags:  git
-----

# Here we go!

Here is one from many way to use [Git][git]. This method is sufficient to work on a project. Not there is many other *workflows*.

## Basic usage

Work with [Git][git] immediately:

+ Get modification done by others `git pull`,
+ See details of these modifications `git log`,
+ Many times:
  + *Make an atomic modification*
  + Verify details of this modification: `git status` and `git diff`,
  + Add some file to be versionned if necessary:<br/>`git add [file]`,
  + Save you modifications <br/>`git commit -a -m "message"`,
  + Send your modifications to others: `git push` (redo a `git pull` if push return an error).

With these few commands you can use [Git][git]. Even if it is sufficient, you need to know one more thing before really begin ; How to manage *conflicts*.

### Conflicts management

Conflicts can arise when you change the same line of code on the same file from another branch you're merging. It can seems a bit intimidating, but with [Git][git] this kind of thing is really simple to handle.

#### example

You start from the following file

<div style="width: 18em; margin-left: auto; margin-right: auto">
<code class="zsh">
Zoot 
</code></pre>
</div>

and you modify one line

<div style="width: 18em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Zoot <span class="Constant"><strong>the pure</strong></span>
</pre>
</div>

except during this time, another user had also modified the same line and had done a `push`.

<div style="width: 18em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Zoot<span class="StringConstant"><strong>, just Zoot</strong></span>
</pre>
</div>

Now when you do a:

<div>
<code class="zsh">
$ git pull
remote: Counting objects: 5, done.
remote: Total 3 (delta 0), reused 0 (delta 0)
Unpacking objects: 100% (3/3), done.
From /home/yogsototh/tmp/conflictTest
   d3ea395..2dc7ffb  master     -> origin/master
Auto-merging foo
CONFLICT (content): Merge conflict in foo
Automatic merge failed; fix conflicts and then commit the result.
</code></pre>
</div>

Our file `foo` now contains: 

<div>
<pre class="twilight">
<<<<<<< HEAD:foo
Zoot <span class="Constant"><strong>the pure</strong></span>
=======
<span>Zoot<span class="StringConstant"><strong>, just Zoot</strong></span></span>
>>>>>>> 2dc7ffb0f186a407a1814d1a62684342cd54e7d6:foo
</pre>
</div>

#### Conflict resolution

To resolve the conflict you only have to edit the file for example, writing:

<div style="width: 18em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Zoot <span class="Constant"><strong>the not so pure</strong></span>
</pre>
</div>

and to commit

<div>
<code class="zsh">
git commit -a -m "conflict resolved"
</code></pre>
</div>

Now you're ready to use [Git][git].
[Git][git] provide many other functionnalities. Now we'll see some [Git][git] usages older CVS couldn't handle.

[git]: http://git-scm.org "Git"
