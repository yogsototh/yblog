-----
isHidden:       false
menupriority:   1
kind:           article
published: 2011-10-04
title: Les idées de yesod
author: Yann Esposito
authoruri: yannesposito.com
tags:  yesod, framework, web, haskell, ideas
-----
blogimage("main.png","Title image")

<div class="intro">

%tlal

Cela fait un moment que je suis la progression du [framework yesod](http://www.yesodweb.com). 
À mon humble avis on peut commencer à l'utiliser pour des applications sérieuses (comprendre en prod).
Avant de vous dire pourquoi vous devriez aussi le considérer, je préfère vous parler de bonnes idées (parmi d'autres) introduites par yesod que je n'avais jamais vu ailleurs.

</div>

## Types saufs

Commençons par une BD d'[xkcd](http://xkcd.com) :

   ![SQL injection by a mom](http://imgs.xkcd.com/comics/exploits_of_a_mom.png)

Lorsque vous créez une application web, beaucoup de temps est passé à s'occuper de chaînes de caractères.
Des chaînes de caractère pour les URL, le HTML, le Javascript, les CSS, les requêtes SQL, etc...
Pour éviter des utilisation malicieuses vous devez protéger chaque chaîne de caractère entre chaque étape.
Par exemple supposons que vous entriez comme nom :

<pre><code class="javascript">Newton<script>alert("An apple fall")</script>
</code></pre>

Sans une protection correcte, le message "An apple fall" sera affiché à chaque fois que quelqu'un essayera d'accéder au nom de cet utilisateur.
Les "types saufs" sont le [tonyglandil du web](https://www.youtube.com/watch?v=1IWF3IsEPBE).
A chaque chaine de caractère, on lui associe un "type". 
A quoi sert cette chaîne de caractère ? Est-ce une URL ? Du javascript ? De l'HTML ?
Entre chaque passage d'une représentation à une autre, un transformation is faite par défaut.

Yesod fait de son mieux pour typer les objets manipulés et ainsi il fera ce qu'il faut pour ne pas mettre du script dans une URL par exemple.

<code class="html"><a href=@[AnotherPageR]>Go to the other page
</code></pre>

Comme `AnotherPageR` est une URL elle ne pourra contiendra pas (par défaut) de caractère dangereux comme par exemple :

<pre><code class="html">falselink"><script> bad_code(); </script><a href="pipo
</code></pre>

## Les widgets

Les widgets de yesod sont différents des widgets Javascripts (ou java).
Pour yesod un widget est un ensemble de morceaux d'appli web. 
Et si dans une page on veut utiliser plusieurs widgets, alors yesod s'occupe de tout.
Des exemples de widgets (au sens yesod) sont :

- Le «footer» d'une page web,
- Le «header» d'une page web,
- un bouton qui apparaît lorsque l'on «scrolle» vers le bas,
- etc...

Pour chacun de ces widgets vous pourriez avoir besoin d'

- un peu d'HTML,
- un peu de CSS et
- un peu de javascript.

Certain morceau doivent être placés dans le «header» de la page et d'autre dans le «body».

Vous pouvez déclarer un widget comme suit (je n'utilise pas la vrai syntaxe) :

    htmlheader = ...
    cssheader = ...
    javascriptheader = ...
    htmlbody = ...

La vraie syntaxe est :

<pre><code class="haskell">toWidgetHeader cassiusFile "button.cassius"
toWidgetHeader juliusFile "button.julius"
toWidget       hamletFile "buttonTemplate.hamlet"
</code></pre>

Veuillez aussi noté la convention Shakespearienne des noms.
Encore une bonne raison d'utiliser yesod.

- Cassius _&_ Lucius pour le CSS (très similaire à SASS et SCSS)
- Julius pour le javascript (notons qu'il existe aussi un CoffeeScript qui traîne dans les sources de yesod)
- Hamlet pour l'HTML (similaire à haml)

Lorsque vous générez votre page, yesod se débrouille pour que tout fonctionne ensemble:

<pre><code class="haskell">myBigWidget =  menuWidget >> contentWidget >> footerWidget
</code></pre>

De plus, si vous utilisez 10 widgets avec un peu de CSS, yesod fabriquera un unique fichier CSS pour vous. Bien entendu si vous préférez avoir une dizaine de fichier CSS vous pouvez aussi le faire.

C'est juste génial !

## Routage optimisé

Dans un système de routage standard (à la ruby on rails par exemple) vous avez pour chaque entrée un couple: regexp → handler

La seule façon de découvrir la bonne règle est d'essayer de matcher l'url demandée à chaque expression régulière.

Au lieu d'essayer chaque expression régulière, yesod regroupe et compile les routes pour les optimiser.
Bien entendu pour pouvoir profiter de cet avantage au mieux, il ne faut pas que deux routes interfèrent entres elles.

<pre><code class="html">/blog/2003  Date2003R
/blog/$DATE DateR
</code></pre>

Cette définition de route est invalide par défaut dans yesod.
Si vous voulez vraiment vous pouvez le faire foncionner quand même, mais il me semble que ça doit être quasiment toujours une mauvaise idée.

Il vaut mieux faire :

<pre><code class="html">/blog/$DATE DateR
</code></pre>

et faire le test "est-ce que date = 2003 ?" dans le «handler».

## Pourquoi yesod?

1. _La vitesse_. Simplement incroyable, je ne pense pas qu'il existe quelque chose de plus rapide aujourd'hui. Regardez d'abord cet [article](http://snapframework.com/blog/2010/11/17/snap-0.3-benchmarks) puis [celui-ci](http://www.yesodweb.com/blog/2011/02/warp-speed-ahead).
2. _Haskell_. C'est certainement le langage de programmation le plus difficile à apprendre que j'ai jamais rencontré. Mais aussi l'un des plus incroyables. Si vous voulez rencontrer tout un tas de notions que vous n'avez jamais croisées avant et faire exploser votre cerveau avec de nouvelles idées, alors apprenez Haskell.
3. _Bonnes idées et communauté excellente_. Cela fait quelques mois que je suis la progression de yesod. Et la vitesse à laquelle tout s'est déroulé est simplement incroyable. De plus les développeurs sont intelligents et super sympa.

Si vous êtes un "haskeller", je pense que vous ne devriez pas avoir peur de la syntaxe particulière imposée par la façon standard de faire les choses avec yesod.
Il faut essayer un peu plus loin que les premiers tutoriaux du livre.

Je pense que yesod va dans la bonne direction d'un web plus sûr et plus rapide. Même si je pense que l'avenir sera que les serveurs devront être limités à faire serveur d'API (JSON ou XML ou n'importe quel autre mode de représentation d'objets).

Yesod est juste incroyable. Dépassez les difficultés liées à l'apprentissage d'haskell et essayez le !
