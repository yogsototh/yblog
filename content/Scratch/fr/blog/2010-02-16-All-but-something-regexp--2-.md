-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-02-16
title: Tout sauf quelquechose en expression régulière.
author: Yann Esposito
authoruri: yannesposito.com
tags:  regexp, regular expression
-----

Dans mon [précédent article][previouspost] j'ai donné certaines astuces pour matcher 'tout sauf quelque chose'. De la même manière, un truc pour matcher la chaine de caractère la plus petite possible.
Disons que vous voulez matcher la chaine de caractère entre 'a' et 'b'. Par exemple, vous voulez matcher :

<pre class="twilight">
a.....<strong class="blue">a......b</strong>..b..a....<strong class="blue">a....b</strong>...
</pre>

Voici les deux erreurs communes et une solution :

<pre>
/a.*b/
<strong class="red">a.....a......b..b..a....a....b</strong>...
</pre>

La première erreur vient de l'utilisation du *terrible* `.*`. Parce que vous allez matcher la chaîne de caractère la plus longue possible.

<pre>
/a.*?b/
<strong class="red">a.....a......b</strong>..b..<strong class="red">a....a....b</strong>...
</pre>

 L'autre manière naturelle de répondre à ce problème est de changer la *greediness*. Mais ce n'est pas assez parce que vous allez matcher du premier `a` au premier `b` après celui-ci. On peut alors constater que votre chaine de caractère ne devrait comprendre ni la lettre `a` ni la lettre `b`. Ce qui emène à la dernière solution élégante.

<pre>
/a[^ab]*b/
a.....<strong class="blue">a......b</strong>..b..a....<strong class="blue">a....b</strong>...
</pre>

Jusqu'ici, c'était facile. Maintenant comment fait vous quand au lieu de `a` vous avez une chaine de caractère ?

Par exemple, vous voulez matcher:

~~~ {.html}
<li>...<li>
~~~

C'est un peu difficile. Vous devez matcher

~~~ {.html}
<li>[anything not containing <li>]</li>
~~~

La première méthode serait d'utiliser le même rainsonnement que dans mon [article précédent][previouspost]. Ici un premier essai :

~~~ {.perl}
<li>([^<]|<[^l]|<l[^i]|<li[^>])*</li>
~~~

Mais il y a encore une erreur. Pensez à la chaine de caractère suivante :

~~~ {.html}
<li>...<li</li>
~~~

Cette chaine ne matchera pas. C'est pourquoi si on veut vraiment la matcher correctement<sup><a href="#note1">&dagger;</a></sup> nous devons ajouter :

~~~ {.perl}
<li>([^<]|<[^l]|<l[^i]|<li[^>])*(|<|<l|<li)</li>
~~~

Oui, c'est un peu compliqué. Mais que se passe t'il lorsque la chaine de caractère que vous voulez matcher est encore plus longue que `<li>` ?

Voici un algorithme qui permet de résoudre ce problème aisément. Vous devez réduire ce problème au premier. C'est-à-dire celui avec une seule lettre :

~~~
# transforme un simple caractère choisi aléatoirement
# en un identifiant unique
# (vous devez vérifier que l'identifier est VRAIMENT unique)
# attention l'identifiant unique ne doit pas 
# contenir le caractère choisi.
s/X/_was_x_/g
s/Y/_was_y_/g

# transforme la longue chaine de caractère
# en un seul caractère
s/<li>/X/g
s/<\/li>/Y/g

# Utilisation de la première méthode
s/X([^X]*)Y//g

# Retransformation des lettres en chaines
# de caractères
s/X/<li>/g
s/Y/<\/li>/g

# retour des anciens caractères.
s/_was_x_/X/g
s/_was_y_/Y/g
~~~

Et ça fonctionne en seulement 9 lignes pour toute chaine de début et de fin.
Cette solution fait un peu moins *I AM THE GREAT REGEXP M45T3R, URAN00B*, mais elle est mieux adaptée à mon avis. De plus, utiliser cette dernière solution prouve que vous maitrisez les expressions régulières. Simplement parce que vous savez qu'il est difficile de résoudre des problèmes de cette forme en utilisant seulement des expressions régulières.

---

<small><a name="note1"><sup>&dagger;</sup></a> Je sais que j'ai utilisé une syntaxe HTML dans mon exemple. Mais dans l'utilisation réelle que j'en ai faite, je devais matcher entre `en:` et `::`, sachant que parfois les chaines pouvaient se terminer par `e::`. </small>

[previouspost]: /Scratch/fr/blog/2010-02-15-All-but-something-regexp
