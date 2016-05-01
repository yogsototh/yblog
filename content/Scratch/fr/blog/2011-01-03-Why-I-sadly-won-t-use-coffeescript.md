-----
isHidden:       false
theme: scientific
image: /Scratch/img/blog/2011-01-03-Why-I-sadly-won-t-use-coffeescript/main.png
menupriority:   1
kind:           article
published: 2011-01-03
title: Pas de CoffeeScript pour moi (malheureusement)
author: Yann Esposito
authoruri: yannesposito.com
tags:  Coffeescript
-----
blogimage("main.png","Title image")

<div class="encadre">

*Mise à jour :* Je pense que je vais finallement changer d'avis.
Pourquoi ?
Tout d'abord, je viens de découvrir un convertisseur javascript vers coffeescript, ensuite Denis Knauf m'a laissé un commentaire et m'a appris l'existence d'une fonction `CoffeeScript.eval`. De plus, il faut voir CoffeeScript comme javascript avec une syntaxe similaire à Ruby et pas comme un langage similaire à Ruby.

</div>

<div class="intro">

%tlal Qu'est-ce qui n'allait pas avec Coffeescript? La meta-programmation, il faut le "vendre" aux autres, une nouvelle étape de compilation intermédiaire sans fournir les avantages de Cappuccino, la sensation que c'est un peu instable.

</div>

Le commentaire le mieux classé de [la question suivante](http://news.ycombinator.com/item?id=2053956) posée sur HackerNews mentionnait [CoffeeScript][cf].
Récemment j'ai beaucoup programmé en javascript.
Après avoir essayé
[Sroutcore](http://sproutcore.com),
[Cappuccino](http://cappuccino.org),
[backbone.js](http://documentcloud.github.com/backbone/) _&_
[javascriptMVC](http://javascriptmvc.com),
Je me suis décidé à créer mon propre framework MVC minimal pour client javascript.[^1]

[cf]: http://coffeescript.org

[^1]: Je sais que ce n'est certainement ni la meilleure ni la plus productive des décisions. Mais j'aime bien fabriquer les choses pour savoir comment tout fonctionne dans le détail.

Je me suis battu avec l'horrible syntaxe de javascript. C'était comme revenir des années dans le passé :

- une syntaxe à la Java très verbeuse ;
- une syntaxe follement verbeuse et étrange pour la programmation orientée objet ;
- pas de manière naturelle de se référer à l'instance d'une classe ;
- etc... 

J'étais tellement ennuyé par tous ces point qu'il était arrivé un moment où je commençais à vouloir faire mon propre CoffeeScript.

J'ai fini une première version de mon framework MVC en javascript et j'ai appris l'existence de CoffeeScript. Merci à git, j'ai immédiatement créé une nouvelle branche dans le seul but d'essayer CoffeeScript.

Voici mon expérience :

1. J'ai dû installer `node.js` et utiliser `npm` simplement pour utiliser CoffeeScript. Ce n'était pas très difficile, mais pas aussi facile que ce que j'aurai aimé.
2. Les fichier javascript existants ne sont pas compatible avec coffee.
3. Il n'y a pas script pour aider à transformer les anciens fichiers javascripts en fichier coffee. Du coups j'ai dû faire ça manuellement.
    Merci à [vim](http://vim.org), il ne fut pas très difficile de transformer 90% des fichiers avec des expressions régulières.
    L'option `--watch` de coffee était très utile pour debugger cette transformation.
    Cependant, il m'a fallu écrire mon propre script pour que tous mes fichiers soient _watchés_ dans tous les sous-répertoires.
4. Quelque chose à laquelle je n'avais pas pensé. J'ai fait un peu de meta-programmation en javascript en utilisant `eval`. Mais pour que celà fonctionne correctement, il faut que la chaîne de caractère que je passe à `eval` soit codée en javascript et pas en coffee. C'est un peu comme écrire dans deux langages différents au même endroit. Ça ne me parraissait vraiment pas agréable.

## Conclusion

Avantages :

- Code plus lisible : CoffeeScript résoud la majorité des problèmes de syntaxes de javascript
- Concision : j'ai gagné 14% de lignes, 22% de mots et 14% de caractères.

Inconvénients :

- Ajout d'une nouvelle étape de compilation avant de pouvoir vérifier le comportement de mon site
- Facilité d'utilisation : il m'a fallu créer un script pour gérer la génératio automatique des fichiers
- Il faut apprendre un autre langage proche de ruby
- La meta-programmation devient une expérience désagréable
- Je dois convaincre les personnes travaillant avec moi : 
    - d'installer `node.js`, `npm` et CoffeeScript ;
    - de se souvenir de lancer un script à chaque session de codage ;
    - d'apprendre un autre language proche de ruby.

Les deux derniers points étant de mon point de vue les plus problématiques.

Mais même si j'avais à travailler seul, je n'utiliserai certainement pas CoffeeScript. 
Il s'agit d'un tier dont la moindre mise à jour pourrait rendre mon code inutilisable. 
Cette situation m'est déjà arrivée plusieurs fois et c'est très désagrable. 
Beaucoup plus que coder avec une mauvaise syntaxe.

## Digression

Je suis attristé.
J'espérais tant pouvoir programmer Javascript avec une touche de Ruby.
En fin de compte, cette solution n'est pas pour moi.
Je vais devoir utiliser l'_horrible_ syntaxe javascript pour l'instant.
À la limite j'aurai préféré un script Ruby2Js par exemple[^2]. 
Mais il me semble que ça serait un travail très difficile rien que pour simuler l'accès à la classe courante. 

[^2]: Je sais qu'il existe un projet `rb2js`, mais il ne résoud pas le problème dont je parle.

Typiquement `@x` est transformé en `this.x`. Mais le code suivant ne fait pas ce que j'attendrai de lui.

~~~~~~ {.ruby}
-> 
class MyClass
  foo: ->
    alert('ok')

  bar: ->
    $('#content').load( '/content.html', ( -> @foo(x) ) )
    # Ça n'appellera pas MyClass.foo
~~~~~~

La seule façon de résoudre ce problème est avec le code suivant :

~~~~~~ {.ruby}
-> 
class MyClass
  foo: ->
    alert('ok')

  bar: ->
    self=this
    $('#content').load( '/content.html', ( -> self.foo(x) ) )
~~~~~~

Sachant celà, la notation `@` perd tout son intérêt pour moi.
