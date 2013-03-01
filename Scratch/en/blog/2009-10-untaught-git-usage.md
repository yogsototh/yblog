-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2009-10-13
title: Untaught Git usage
author: Yann Esposito
authoruri: yannesposito.com
tags:  git, dcvs, programming
-----

<small> <em>I explain why I had so much difficulties to use Git. There is an "untaught rule" that make hard to work without. Until I read the good document. </em></small>

<small> <em> "Cheap branches" aren't designed to be totally isolated branches but rather should follow a "Master Branch". There is a </em>Standard Workflow<em> to follow. If you don't follow it, you prepare yourself with some hard time with Git.</em> </small>

---

# My way to decentralisation

### From SVN to Bazaar

I was a huge user of [subversion (svn)](http://subversion.tigris.org). Until the day I saw this [video of Linus Torvald](http://www.youtube.com/watch?v=4XpnKHJAok8). Where he explain Git and all advantages of *Decentralized Concurrent Versioning System*(<abbr title="Decentralized Concurrent Versioning System">DCVS</abbr>)

I must say I was completely convinced. And the more you learn about <abbr title="Decentralized Concurrent Versioning System">DCVS</abbr> the more you see good reason to use them.

I then needed a versioning system for my team. As they were not used to open source versioning system except those heavy, with a GUI and with and administrator<sup><a href="#note1">&dagger;</a></sup>

After some web searches, I founded three main choices:

  - [Git](http://git-scm.com)
  - [Bazaar](http://bazaar-vcs.org)
  - [Mercurial](http://selenic.com/mercurial)

After trying each other I chosen Bazaar. It has the simplest User Interface<sup><a href="#note2">&#42;</a></sup>. My choice was done.

### From Bazaar to Git

It was really natural to learn when coming from *subversion*. The `pull` command corresponding to `update`, `push` command to `commit`. Commands like `commit` and `update` are still there if you want to use an SVN *workflow*.

After some times, reading on many blogs, I realize Git is far more popular and by influent people. 

I then decide to use Git in particular to *version* this current website.
But after trying it, I found it difficult and couter intuitive (I'll speak a work about it later).

After calling for some help, when I say Bazaar is much simpler to learn, some people answer me that Git:

> &mdash; *SO-MUCH-EASY my 12 year old daughter uses it to version its school documents. She has no difficulties at all, creating branches, blah, blah, blah...*

If a 12 years old girl has no problem with Git and I (with my Computer Science Ph.D.) have difficulties to uses it like I want, it is frustrating and humiliating. But what makes Git natural for some people and confusing for me? 

I finally understood why reading a document I didn't read before. It was the *untaught* part of the conception. The part every developer found so *natural* it is not necessary to say a word about it. But it was not *natural* for me.

<small><a name="note1">&dagger;</a> - I speak about *ClearCase(c)*. I know there exists command line tools. But it was not the way my team used it.</small>

<small><a name="note2">&#42;</a> - I never really given its chance to Mercurial. The terminology they chosen was too far from the svn one. And I was used to it.</small>

---

When you see explanation about *branches* and <abbr title="Decentralized Concurrent Versioning System">DCVS</abbr> we imagine each branch is totally uncorrelated to each other, except when *merging*. Everything is magic. This is the "*Parallel World*" explanation. This way of seeing is explained more in depth in the [real good article about branches](http://betterexplained.com/articles/a-visual-guide-to-version-control/) on betterexplained. 

Git was designed to manage the Linux Kernel. Git was designed using the concept of *Patch* instead of *Parallel Worlds*.

From one site *Parallel World* and *Patches* from the other. There is many equivalent notions in the two point of vue, but also some differences.

* Bazaar seems base on the *Parallel World* vision which implies *Patches* 
* While Git seem base on the *Patch* model which will implie the creation of *Parallel Worlds*.

I will not argument about which is the best. Just tell my vision of DCVS come from the *Parallel World* vision and Git was designed the other way<sup><a href="#note3">&Dagger;</a></sup>.

##  From Theory to Real Life Usage 

I believe I understood conceptual mechanism under Git. But I had some difficulties with real usage. The worst point, the one I didn't get before long was because I didn't get really well the notion of *Cheap Branching*.

What is a *Cheap Branch*? If like me you come from Bazaar, it is a totally new notion. It is in fact the ability to create a branches all of them using the same directory.

You just have to launch a Git command and the local directory reflect the state of the branch you selected.

In theory, *Cheap Branches* are exactly like Bazaar branches. The word used is *Branch* and not *Cheap Branch*. But there is a slight difference between them. A slight difference between a *Cloned Branch* and a *Cheap Branch*.

A "Standard branch" is what is theoretically a kind of new *Parallel World*.
But *Cheap branch* was designed to be future *Patch* for the main branch of the directory/Cloned branch.

Of course, I know anybody can state you can totally use *Cheap branches* as *Cloned branches*. But they weren't designed for that. On daily usage, it is a bit uneasy to use it like this.

Here how Git *cheap branches* should be used (for more details see [Git for Designers](http://hoth.entp.com/output/git_for_designers.html)):

* get or creation of a main repositoy **<sc>The Great Repository</sc>**
*  creation of a *Cheap branch* containing differences which **have** to be patched somewhere in the future into **<sc>The Great Repository</sc>**

Here's how you should **not** use Git:

  * Get or creation of a repository
  * Create a *cheap branch* which will never push it's modification to the main repository.

This simple minor difference of point of view confused me a lot.

### Real Life Usage

Now I have understood all that. I understand why Git has some many people claiming it is the best DCVS.

Cheap branching notion is essential in Git and is a really useful feature. Particularly for this website. But, there are not exactly, completely parallel line of development. Because they are designed to path the main branch. Of course, it is not an obligation, but there are slight messages which tell you this should be used like that.

If I want to separate in a better way some branches I just have to *Clone* them.
And I return exactly in branches Bazaar provided me.

### Examples

For now, I prefer (from far) Bazaar terminology. They are cleaner and more understandable.

<div><code class="zsh">bzr revert</code></div>

Is clearer than

<div><code class="zsh">git reset --hard HEAD</code></div>

We can tell the same thing about

<div><code class="zsh">bzr revert -r -3</code></div>

which seems preferable to

<div><code class="zsh">git reset --hard HEAD~3</code></div>

Until now, it is not big business. But now, things will go worse.
If we want to revert time on all the tree we use the keyword `reset`.

<center>OK</center>

Now, if I want to revert time on on file. We should naturally imagine the command will be:

<div><code class="zsh">git reset --hard FILE</code></div>

<center>**OF COURSE NOT!**</center>

The solution is:

<div><code class="zsh">git checkout FILE</code></div>

What? **`checkout`** !? Well, ok. I accept. why not?
With Bazaar it is:

<div><code class="zsh">git revert FILE</code></div>

What I personally found far more natural.

But the command to change the current *cheap branch* is really hard to be accepted (from the User Interface point of view).
With Bazaar it is:

<div><code class="zsh">cd ../branch</code></div>

Well yes. With Bazaar you have to change your directory to change your branch. It needs more disk resources but it is really clear. Which is my current branch, is just a `pwd` away. For Git here is the command:

<div><code class="zsh">git checkout branch</code></div>

**WTF?** I believed `checkout` was the key to get a file in some state (not the entire tree).

Then `checkout` is the same keyword used to get back in time on a file (BUT NOT ON ALL THE TREE where you have to use `reset --hard`) and to **change current branch**!

It is totally unnatural. Even if it is theoretically totally justified like you can see in the really good article [Git for Computer Scientist](http://eagain.net/articles/git-for-computer-scientists/). From the user point of vue, it is difficult to do worse than that. It is like somebody made it on purpose to make it the hardest possible to learn and understand.

> - &mdash;  Try to find the good keyword for this operation
> - &mdash; Wrong! Try again!
> - &mdash; False, it is not yet right!

That were the Git bad side. But It has many advantages. Once you've understood the *cheap branching* paradigm. All became clearer for me after. Even if there is also some difficulties with the edit of the `.git/config` files (not user friendly at all).

<small><a name="note3">&Dagger;</a> I must precise that I worked a lot with multi-modal logic and particularly about "Temporal Logics" (linear or not). This is why I was more inclined to see things this way. "Ah ! Just to remember my firsts love with computer science !"</small>

---

# Conclusion

### DCVS vs. CVS ?

Was it a good idea to change to a *decentralised* versionning system? Clearly yes. Decentralisation give far much great possibilities.
Such as working on a fix on a totally isolated branches.

### Is Git better than Bazaar?

Speaking about *features* I'll tell Git is the best.
But Git was too much in my way. Is was exactly what I didn't want for my first DCVS.

I shouldn't have had those difficulties about understanding *cheap branching which must be a patch*. In reality, Git make a difference between the Tree and the Branch. Which is obviously not the case for Bazaar. Conceptually, bazaar is simpler to understand.

### Finally

In conclusion, I use Git more often than Bazaar and I must say, that I have some preferences for Git. However, Git lack hardly clear commands name like `revert`.
For now I don't made alias to correct that. But may be one day I should do that.

