-----
isHidden:       false
menupriority:   3
kind:           article
published: 2009-11-12
title: Git pour les nuls
author: Yann Esposito
authoruri: yannesposito.com
subtitle: Utiliser git avec quelques commandes simples
-----

# Et c'est parti !

Voici une parmi de nombreuses autres façon d'utiliser [Git][git]. Cette méthode est nécessaire et suffisante pour travailler seul ou en collaboration sur un projet commun. Cependant, on peut faire beaucoup mieux avec [Git][git] que ce *workflow* (en langage anglo-saxon).

## Utilisation basique

La façon immédiate de travailler avec [Git][git] :

+ récupérer les modifications des autres <span class="black">`git pull`</span>
+ voir les détails de ces modifications <span class="black">`git log`</span>
+ Plusieurs fois:
  + *Faire une modification atomique*
  + verifier le details de ses modifications <span class="black">`git status`</span> et <span class="black">`git diff`</span>
  + indiquer si nécessaire que de nouveaux fichiers doivent être *versionnés* <span class="black">`git add [file]`</span>
  + enregistrer ses modifications <br/><span class="black">`git commit -a -m "message"`</span>
  + envoyer ses modifications aux autres <span class="black">`git push`</span> (refaire un `git pull` si le push renvoie une erreur).

Voilà, avec ces quelques commandes vous pouvez utiliser [Git][git] sur un projet avec d'autres personnes. Même si c'est suffisant, il faut quand même connaître une chose avant de se lancer ; la gestion des *conflits*.

### Gestion des conflits

Les conflits peuvent survenir lorsque vous modifiez les même lignes de codes sur le même fichier d'une autre branche que vous *mergez*. Ça peut sembler un peu intimidant, mais avec [Git][git] ce genre de chose est très facile a régler.

#### exemple

Vous partez du fichier suivant : 

<div style="width: 18em; margin-left: auto; margin-right: auto">
~~~~~~ {.zsh}
Zoot 
~~~~~~
</div>

et vous modifiez une ligne

<div style="width: 18em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Zoot <span class="Constant"><strong>the pure</strong></span>
</pre>
</div>

sauf que pendant ce temps, un autre utilisateur a aussi modifié cette ligne et a fait un `push` de sa modification. 

<div style="width: 18em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Zoot<span class="StringConstant"><strong>, just Zoot</strong></span>
</pre>
</div>

Maintenant quand vous lancez la commande

<div>
~~~~~~ {.zsh}
$ git pull
remote: Counting objects: 5, done.
remote: Total 3 (delta 0), reused 0 (delta 0)
Unpacking objects: 100% (3/3), done.
From /home/e640846/tmp/conflictTest
   d3ea395..2dc7ffb  master     -> origin/master
Auto-merging foo
CONFLICT (content): Merge conflict in foo
Automatic merge failed; fix conflicts and then commit the result.
~~~~~~
</div>

Notre fichier `foo` contient alors : 

<div>
<pre class="twilight">
<<<<<<< HEAD:foo
Zoot <span class="Constant"><strong>the pure</strong></span>
=======
<span>Zoot<span class="StringConstant"><strong>, just Zoot</strong></span></span>
>>>>>>> 2dc7ffb0f186a407a1814d1a62684342cd54e7d6:foo
</pre>
</div>

#### Résolution du conflit

Régler le conflit, il suffit d'éditer le fichier, par exemple en écrivant :

<div style="width: 18em; margin-left: auto; margin-right: auto">
<pre class="twilight">
Zoot <span class="Constant"><strong>the not so pure</strong></span>
</pre>
</div>

et de 'commiter' tout simplement : 

<div>
~~~~~~ {.zsh}
git commit -a -m "conflict resolved"
~~~~~~
</div>

Maintenant vous êtes fin prêt pour utiliser [Git][git].
Sauf que [Git][git], c'est un outil qui permet de faire beaucoup plus que juste ça. Nous allons maintenant voir comment utiliser les fonctionnalités de Git qui n'étaient pas disponibles avec CVS et consorts.

[git]: http://git-scm.org "Git"
