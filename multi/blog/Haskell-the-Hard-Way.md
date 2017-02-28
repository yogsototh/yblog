-----
theme: scientific
image: /Scratch/img/blog/Haskell-the-Hard-Way/magritte_pleasure_principle.jpg
menupriority:   1
kind: article
published: 2012-02-08
en: title: Learn Haskell Fast and Hard
en: subtitle: Blow your mind with Haskell
fr: title: Haskell comme un vrai!
fr: subtitle: Haskell à s'en faire griller les neurones
author: Yann Esposito
authoruri: yannesposito.com
tags: Haskell, programming, functional, tutorial
-----
blogimage("magritte_pleasure_principle.jpg","Magritte pleasure principle")

<div class="intro">

en: %tldr A very short and dense tutorial for learning Haskell.

fr: %tlal Un tutoriel très court mais très dense pour apprendre Haskell.

en: Thanks to:
en: 
en: - [Oleg Taykalo](https://plus.google.com/u/0/113751420744109290534)
en:   you can find a Russian translation here:
en:   [Part 1](http://habrahabr.ru/post/152889/) _&_
en:   [Part 2](http://habrahabr.ru/post/153383/),
en: - [Daniel Campoverde](http://silly-bytes.blogspot.fr)
en:   for the Spanish translation here:
en:   [Aprende Haskell rápido y difícil](http://silly-bytes.blogspot.fr/2016/06/aprende-haskell-rapido-y-dificil_29.html),
en: - [Joomy Korkut](http://github.com/joom) for the Turkish translation here:
en:   [Zor Yoldan Haskell](https://github.com/joom/zor-yoldan-haskell).

fr: Merci à :
fr: 
fr: - [Oleg Taykalo](https://plus.google.com/u/0/113751420744109290534)
fr:   vous pouvez trouver une traduction russe ici: [Partie 1](http://habrahabr.ru/post/152889/) _&_
fr:   [Partie 2](http://habrahabr.ru/post/153383/) ;
fr: - [Daniel Campoverde](http://silly-bytes.blogspot.fr) pour la version Espagnole :
fr:   [Aprende Haskell rápido y difícil](http://silly-bytes.blogspot.fr/2016/06/aprende-haskell-rapido-y-dificil_29.html) ;
fr: - [Joomy Korkut](http://github.com/joom) pour sa traduction en Turc:
fr:   [Zor Yoldan Haskell](https://github.com/joom/zor-yoldan-haskell)
fr: - [lepereceval](https://github.com/lepereceval)
fr:   pour sa traduction française que je n'ai pas eu le courage de faire !
fr: - [Younesse Kaddar](https://github.com/youqad)
fr:   pour toutes ses corrections de style et d'orthographe.


> <center><hr style="width:30%;float:left;border-color:#CCCCD0;margin-top:1em"/><span class="sc"><b>Table of Content</b></span><hr style="width:30%;float:right;border-color:#CCCCD0;margin-top:1em"/></center>
>
> <div class="toc">
>
> * <a href="#introduction">Introduction</a>
en: >   * <a href="#install">Install</a>
fr: >   * <a href="#install">Installation</a>
en: >   * <a href="#don-t-be-afraid">Don't be afraid</a>
fr: >   * <a href="#don-t-be-afraid">Ne soyez pas effrayés!</a>
en: >   * <a href="#very-basic-haskell">Very basic Haskell</a>
fr: >   * <a href="#very-basic-haskell">Les bases de Haskell</a>
en: >     * <a href="#function-declaration">Function declaration</a>
fr: >     * <a href="#function-declaration">Déclaration de fonctions</a>
en: >     * <a href="#a-type-example">A Type Example</a>
fr: >     * <a href="#a-type-example">Un exemple de type</a>
en: > * <a href="#essential-haskell">Essential Haskell</a>
fr: > * <a href="#essential-haskell">Notions essentielles</a>
>   * <a href="#notations">Notations</a>
en: >       * <a href="#arithmetic">Arithmetic</a>
fr: >       * <a href="#arithmetic">Arithmétique</a>
en: >       * <a href="#logic">Logic</a>
fr: >       * <a href="#logic">Logique</a>
en: >       * <a href="#powers">Powers</a>
fr: >       * <a href="#powers">Puissances</a>
en: >       * <a href="#lists">Lists</a>
fr: >       * <a href="#lists">Listes</a>
en: >       * <a href="#strings">Strings</a>
fr: >       * <a href="#strings">Chaînes de caractères</a>
>       * <a href="#tuples">Tuples</a>
en: >       * <a href="#deal-with-parentheses">Deal with parentheses</a>
fr: >       * <a href="#deal-with-parentheses">Traiter avec les parenthèses</a>
en: >   * <a href="#useful-notations-for-functions">Useful notations for functions</a>
fr: >   * <a href="#useful-notations-for-functions">Notations utiles pour les fonctions</a>
en: > * <a href="#hard-part">Hard Part</a>
fr: > * <a href="#hard-part">La Partie Difficile</a>
en: >   * <a href="#functional-style">Functional style</a>
fr: >   * <a href="#functional-style">Le style fonctionnel</a>
en: >     * <a href="#higher-order-functions">Higher Order Functions</a>
fr: >     * <a href="#higher-order-functions">Fonctions d'ordre supérieur</a>
en: >   * <a href="#types">Types</a>
fr: >   * <a href="#types">Les types</a>
en: >     * <a href="#type-inference">Type inference</a>
fr: >     * <a href="#type-inference">Inférence de type</a>
en: >     * <a href="#type-construction">Type construction</a>
fr: >     * <a href="#type-construction">Construction de types</a>
en: >     * <a href="#recursive-type">Recursive type</a>
fr: >     * <a href="#recursive-type">Type récursif</a>
en: >     * <a href="#trees">Trees</a>
fr: >     * <a href="#trees">Les arbres</a>
en: >   * <a href="#infinite-structures">Infinite Structures</a>
fr: >   * <a href="#infinite-structures">Structures infinies</a>
en: > * <a href="#hell-difficulty-part">Hell Difficulty Part</a>
fr: > * <a href="#hell-difficulty-part">Partie de difficulté infernale</a>
en: >   * <a href="#deal-with-io">Deal With IO</a>
fr: >   * <a href="#deal-with-io">S'occuper de l'E/S (IO)</a>
fr: >   * <a href="#io-trick-explained">Le truc des IO révélé</a>
en: >   * <a href="#io-trick-explained">IO trick explained</a>
fr: >   * <a href="#monads">Les monades</a>
en: >   * <a href="#monads">Monads</a>
fr: >     * <a href="#maybe-monad">Maybe est une monade</a>
en: >     * <a href="#maybe-monad">Maybe is a monad</a>
fr: >     * <a href="#the-list-monad">La monade List</a>
en: >     * <a href="#the-list-monad">The list monad</a>
fr: > * <a href="#appendix">Appendice</a>
en: > * <a href="#appendix">Appendix</a>
fr: >   * <a href="#more-on-infinite-tree">Revenons sur les arbres infinis</a>
en: >   * <a href="#more-on-infinite-tree">More on Infinite Tree</a>
>
> </div>

</div>
<div class="intro">

en: I really believe all developers should learn Haskell.
en: I don't think everyone needs to be super Haskell ninjas,
en: but they should at least discover what Haskell has to offer.
en: Learning Haskell opens your mind.
fr: Je pense vraiment que
fr: tous les développeurs devraient apprendre Haskell.
fr: Peut-être pas devenir des ninjas d'Haskell,
fr: mais au moins savoir ce que ce langage a de particulier.
fr: Son apprentissage ouvre énormément l'esprit.

en: Mainstream languages share the same foundations:
fr: La plupart des langages partagent les mêmes fondements :

en: - variables
en: - loops
en: - pointers[^0001]
en: - data structures, objects and classes (for most)
fr: - les variables
fr: - les boucles
fr: - les pointeurs[^0001]
fr: - les structures de données, les objets et les classes

en: [^0001]: Even if most recent languages try to hide them, they are present.
fr: [^0001]: Même si tous les langages récents essayent de les cacher, ils restent présents.

en: Haskell is very different.
en: The language uses a lot of concepts I had never heard about before.
en: Many of those concepts will help you become a better programmer.
fr: Haskell est très différent.
fr: Ce langage utilise des concepts dont je n'avais jamais entendu parler avant.
fr: Beaucoup de ces concepts pourront vous aider à devenir un meilleur développeur.

en: But learning Haskell can be hard.
en: It was for me.
en: In this article I try to provide what I lacked during my learning.
fr: Plier son esprit à Haskell peut être difficile.
fr: Ce le fut pour moi.
fr: Dans cet article, j'essaye de fournir les informations qui m'ont manquées lors de mon apprentissage.

en: This article will certainly be hard to follow.
en: This is on purpose.
en: There is no shortcut to learning Haskell.
en: It is hard and challenging.
en: But I believe this is a good thing.
en: It is because it is hard that Haskell is interesting.
fr: Cet article sera certainement difficile à suivre.
fr: Mais c'est voulu.
fr: Il n'y a pas de raccourci pour apprendre Haskell.
fr: C'est difficile.
fr: Mais je pense que c'est une bonne chose.
fr: C'est entre autres parce qu'Haskell est difficile qu'il est intéressant.

en: The conventional method to learning Haskell is to read two books.
en: First ["Learn You a Haskell"](http://learnyouahaskell.com) and just after ["Real World Haskell"](http://www.realworldhaskell.org).
en: I also believe this is the right way to go.
en: But to learn what Haskell is all about, you'll have to read them in detail.
fr: La manière conventionnelle d'apprendre Haskell est de lire deux livres.
fr: D'abord ["Learn You a Haskell"](http://haskell.fr/lyah/)
fr: et ensuite ["Real World Haskell"](http://www.realworldhaskell.org).
fr: Je pense aussi que c'est la bonne manière de s'y prendre.
fr: Mais apprendre même un tout petit peu d'Haskell est presque impossible sans se plonger réellement dans ces livres.

en: In contrast, this article is a very brief and dense overview of all major aspects of Haskell.
en: I also added some information I lacked while I learned Haskell.
fr: Cet article fait un résumé très dense et rapide des aspects majeurs d'Haskell.
fr: J'y ai aussi rajouté des informations qui m'ont manqué pendant l'apprentissage de ce langage.

fr: Pour les francophones : je suis désolé.
fr: Je n'ai pas eu le courage de tout retraduire en français.
fr: Sachez cependant que si vous êtes plusieurs à insister, je ferai certainement l'effort de traduire l'article en entier.
fr: Et si vous vous sentez d'avoir une bonne âme je ne suis pas contre un peu d'aide.
fr: Les sources de cet article sont sur [github](http://github.com/yogsototh/learn_haskell.git).

en: The article contains five parts:
fr: Cet article contient cinq parties :

en: - Introduction: a short example to show Haskell can be friendly.
en: - Basic Haskell: Haskell syntax, and some essential notions.
en: - Hard Difficulty Part:
en:     - Functional style; a progressive example, from imperative to functional style
en:     - Types; types and a standard binary tree example
en:     - Infinite Structure; manipulate an infinite binary tree!
en: - Hell Difficulty Part:
en:     - Deal with IO; A very minimal example
en:     - IO trick explained; the hidden detail I lacked to understand IO
en:     - Monads; incredible how we can generalize
en: - Appendix:
en:     - More on infinite tree; a more math oriented discussion about infinite trees

fr: - Introduction : un exemple rapide pour montrer qu'Haskell peut être facile.
fr: - Les bases d'Haskell : La syntaxe et des notions essentielles
fr: - Partie difficile :
fr:     - Style fonctionnel : un exemple progressif, du style impératif au style fonctionnel ;
fr:     - Types : la syntaxe et un exemple d'arbre binaire ;
fr:     - Structure infinie : manipulons un arbre infini !
fr: - Partie de difficulté infernale :
fr:     - Utiliser les IO : un exemple très minimal ;
fr:     - Le truc des IO révélé : les détails cachés d'IO qui m'ont manqués
fr:     - Les monades : incroyable à quel point on peut généraliser
fr: - Appendice :
fr:     - Revenons sur les arbres infinis : une discussion plus mathématique sur la manipulation d'arbres infinis.

en:  > Note: Each time you see a separator with a filename ending in `.lhs`
en:  > you can click the filename to get this file.
en:  > If you save the file as `filename.lhs`, you can run it with
en:  > <pre>
en:  > runhaskell filename.lhs
en:  > </pre>
en:  >
en:  > Some might not work, but most will.
en:  > You should see a link just below.

fr:  > Note: Chaque fois que vous voyez un séparateur avec un nom de fichier se terminant par `lhs`, vous pouvez cliquer sur le nom de fichier et télécharger le fichier.
fr:  > Si vous sauvegardez le fichier sour le nom `filename.lhs`, vous pouvez l'exécuter avec :
fr:  > <pre>
fr:  > runhaskell filename.lhs
fr:  > </pre>
fr:  >
fr:  > Certains ne marcheront pas, mais la majorité vous donneront un résultat.
fr:  > Vous devriez voir un lien juste en dessous.

</div>

<hr/><a href="code/01_basic/10_Introduction/00_hello_world.lhs" class="cut">01_basic/10_Introduction/<strong>00_hello_world.lhs</strong></a>

<h2 id="introduction">Introduction</h2>

en: <h3 id="install">Install</h3>
fr: <h3 id="install">Installation</h3>

blogimage("Haskell-logo.png", "Haskell logo")

en: - [Haskell Platform](http://www.haskell.org/platform) is the standard way to install Haskell.
fr: - La principale façon d'installer Haskell est [Haskell Platform](http://www.haskell.org/platform).

en: Tools:
fr: Outils:

en: - `ghc`: Compiler similar to gcc for `C`.
en: - `ghci`: Interactive Haskell (REPL)
en: - `runhaskell`: Execute a program without compiling it. Convenient but very slow compared to compiled programs.
fr: - `ghc`: Compilateur similaire à gcc pour le langage `C`.
fr: - `ghci`: Console Haskell interactive (Read-Eval-Print Loop)
fr: - `runhaskell`: Exécuter un programme sans le compiler. Pratique mais très lent comparé aux programmes compilés.

en: <h3 id="don-t-be-afraid">Don't be afraid</h3>
fr: <h3 id="don-t-be-afraid">Ne soyez pas effrayés!</h3>

blogimage("munch_TheScream.jpg","The Scream")

en: Many books/articles about Haskell start by introducing some esoteric formula (quick sort, Fibonacci, etc...).
en: I will do the exact opposite.
en: At first I won't show you any Haskell super power.
en: I will start with similarities between Haskell and other programming languages.
en: Let's jump to the mandatory "Hello World".
fr: Beaucoup de livres/articles sur Haskell commencent par présenter des formules ésotériques (Algorithmes de tri rapide, suite de Fibonacci, etc...).
fr: Je ferai l'exact opposé
fr: En premier lieu je ne vous montrerai pas les super-pouvoirs d'Haskell.
fr: Je vais commencer par les similarités avec les autres langages de programmation.
fr: Commençons par l'indispensable "Hello World!".

<div class="codehighlight">
~~~~~~ {.haskell}
main = putStrLn "Hello World!"
~~~~~~
</div>
en: To run it, you can save this code in a `hello.hs` and:
fr: Pour l'exécuter, vous pouvez enregistrer ce code dans un fichier `hell.hs` et:

~~~~~~ {.zsh}
~ runhaskell ./hello.hs
Hello World!
~~~~~~

en: You could also download the literate Haskell source.
en: You should see a link just above the introduction title.
en: Download this file as `00_hello_world.lhs` and:
fr: Vous pouvez également télécharger la source Haskell littérale.
fr: Vous devriez voir un lien juste au dessus du titre de l'introduction.
fr: Téléchargez ce fichier en tant que `00_hello_world.lhs` et:

~~~~~~ {.zsh}
~ runhaskell 00_hello_world.lhs
Hello World!
~~~~~~
<a href="code/01_basic/10_Introduction/00_hello_world.lhs" class="cut">01_basic/10_Introduction/<strong>00_hello_world.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/10_hello_you.lhs" class="cut">01_basic/10_Introduction/<strong>10_hello_you.lhs</strong></a>

en: Now, a program asking your name and replying "Hello" using the name you entered:
fr: Maintenant, un programme qui demande votre nom et répond "Hello" suivit du nom que vous avez entré:

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
    print "What is your name?"
    name <- getLine
    print ("Hello " ++ name ++ "!")
~~~~~~
</div>
en: First, let us compare this with similar programs in a few imperative languages:
fr: Premièrement, comparons ce code avec ceux de quelques langages de programmation impératif:

~~~~~~ {.python}
# Python
print "What is your name?"
name = raw_input()
print "Hello %s!" % name
~~~~~~

~~~~~~ {.ruby}
# Ruby
puts "What is your name?"
name = gets.chomp
puts "Hello #{name}!"
~~~~~~

~~~~~~ {.c}
// In C
#include <stdio.h>
int main (int argc, char **argv) {
    char name[666]; // <- An Evil Number!
    // What if my name is more than 665 character long?
    printf("What is your name?\n");
    scanf("%s", name);
    printf("Hello %s!\n", name);
    return 0;
}
~~~~~~

en: The structure is the same, but there are some syntax differences.
en: The main part of this tutorial will be dedicated to explaining why.
fr: La structure est la même, mais il y a quelques différences de syntaxe.
fr: La partie principale de ce tutoriel sera consacrée à expliquer cela.

en: In Haskell there is a `main` function and every object has a type.
en: The type of `main` is `IO ()`.
en: This means `main` will cause side effects.
fr: En Haskell il y a une fonction `main` tous les objets ont un type.
fr: Le type de `main` est `IO ()`.
fr: Cela veut dire que `main` causera des effets secondaires.

en: Just remember that Haskell can look a lot like mainstream imperative languages.
fr: Rappelez-vous just que Haskell peut ressembler énormément aux principaux langages impératifs.

<a href="code/01_basic/10_Introduction/10_hello_you.lhs" class="cut">01_basic/10_Introduction/<strong>10_hello_you.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/20_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>20_very_basic.lhs</strong></a>

en: <h3 id="very-basic-haskell">Very basic Haskell</h3>
fr: <h3 id="very-basic-haskell">Les bases de Haskell</h3>

blogimage("picasso_owl.jpg","Picasso minimal owl")

en: Before continuing you need to be warned about some essential properties of Haskell.
fr: Avant de continuer, vous devez êtres avertis à propos de propriétés essentielles de Haskell.

en: _Functional_
fr: _Fonctionnel_

en: Haskell is a functional language.
en: If you have an imperative language background, you'll have to learn a lot of new things.
en: Hopefully many of these new concepts will help you to program even in imperative languages.
fr: Haskell est un langage fonctionnel
fr: Si vous avez déjà travaillé avec un langage impératif, vous devrez apprendre beaucoup de nouvelles choses.
fr: Heureusement beaucoup de ces nouveaux concepts vous aidera à programmer même dans un langage impératif.

en: _Smart Static Typing_
fr: _Typage Statique Intelligent_

en: Instead of being in your way like in `C`, `C++` or `Java`, the type system is here to help you.
fr: Au lieu de bloquer votre chemin comme en `C`, `C++` ou `Java`, le système de typage est ici pour vous aider.

en: _Purity_
fr: _Pureté_

en: Generally your functions won't modify anything in the outside world.
en: This means they can't modify the value of a variable, can't get user input, can't write on the screen, can't launch a missile.
en: On the other hand, parallelism will be very easy to achieve.
en: Haskell makes it clear where effects occur and where your code is pure.
en: Also, it will be far easier to reason about your program.
en: Most bugs will be prevented in the pure parts of your program.
fr: Généralement vos fonctions ne modifieront rien du le monde extérieur.
fr: Cela veut dire qu'elles ne peuvent pas modifier la valeur d'une variable,
fr: lire du texte entré par un utilisateur,
fr: écrire sur l'écran, lancer un missile.
fr: D'un autre coté, avoir un code parallèle devient très facile.
fr: Haskell rend très clair où les effets apparaissent et où le code est pur.
fr: De plus, il devient beaucoup plus aisé de raisonner sur son programme.
fr: La majorité des bugs seront évités dans les parties pures de votre programme.

en: Furthermore, pure functions follow a fundamental law in Haskell:
fr: En outre, les fonctions pures suivent une loi fondamentale en Haskell:

en: > Applying a function with the same parameters always returns the same value.
fr: > Appliquer une fonction avec les mêmes paramètres retourne toujours la même valeur.

en: _Laziness_
fr: _Paresse_

en: Laziness by default is a very uncommon language design.
en: By default, Haskell evaluates something only when it is needed.
en: In consequence, it provides a very elegant way to manipulate infinite structures, for example.
fr: La paresse par défaut est un choix de conception de langage très rare.
fr: Par défaut, Haskell évalue quelque chose seulement lorsque cela est nécessaire.
fr: En conséquence, cela fournit un moyen très élégant de manipuler des structures infinies, par exemple.

en: A last warning about how you should read Haskell code.
en: For me, it is like reading scientific papers.
en: Some parts are very clear, but when you see a formula, just focus and read slower.
en: Also, while learning Haskell, it _really_ doesn't matter much if you don't understand syntax details.
en: If you meet a `>>=`, `<$>`, `<-` or any other weird symbol, just ignore them and follows the flow of the code.
fr: Un dernier avertissement sur comment vous devriez lire le code Haskell.
fr: Pour moi, c'est comme lire des papiers scientifiques.
fr: Quelques parties sont très claires, mais quand vous voyez une formule, concentrez-vous dessus et lisez plus lentement.
fr: De plus, lorsque vous apprenez Haskell, cela n'importe _vraiment_ pas si vous ne comprenez pas les détails syntaxiques.
fr: Si vous voyez un `>>=`, `<$>`, `<-` ou n'importe quel symbole bizarre, ignorez-les et suivez le déroulement du code.

en: <h4 id="function-declaration">Function declaration</h4>
fr: <h4 id="function-declaration">Déclaration de fonctions</h4>

en: You might be used to declaring functions like this:
fr: Vous avez déjà dû déclarer des fonctions comme cela:

en: In `C`:
fr: En `C`:

~~~~~~ {.c}
int f(int x, int y) {
    return x*x + y*y;
}
~~~~~~

en: In JavaScript:
fr: En JavaScript:

~~~~~~ {.javascript}
function f(x,y) {
    return x*x + y*y;
}
~~~~~~

en: in Python:
fr: En Python:

~~~~~~ {.python}
def f(x,y):
    return x*x + y*y
~~~~~~

en: in Ruby:
fr: En Ruby:

~~~~~~ {.ruby}
def f(x,y)
    x*x + y*y
end
~~~~~~

en: In Scheme:
fr: En Scheme:

~~~~~~ {.scheme}
(define (f x y)
    (+ (* x x) (* y y)))
~~~~~~

en: Finally, the Haskell way is:
fr: Finalement, la manière de faire de Haskell est:

~~~~~~ {.haskell}
f x y = x*x + y*y
~~~~~~

en: Very clean. No parenthesis, no `def`.
fr: Très propre. Aucune parenthèse, aucun `def`.

en: Don't forget, Haskell uses functions and types a lot.
en: It is thus very easy to define them.
en: The syntax was particularly well thought out for these objects.
fr: N'oubliez pas, Haskell utilise beaucoup les fonctions et les types.
fr: C'est très facile de les définir.
fr: La syntaxe a été particulièrement réfléchie pour ces objets.

en: <h4 id="a-type-example">A Type Example</h4>
fr: <h4 id="a-type-example">Un exemple de type</h4>

en: Although it is not mandatory, type information for functions is usually made
en: explicit. It's not mandatory because the compiler is smart enough to discover
en: it for you. It's a good idea because it indicates intent and understanding.
fr: Même si ce n'est pas obligatoire, les informations de type pour les fonctions sont habituellement déclarées
fr: explicitement. Ce n'est pas indispensable car le compilateur est suffisamment intelligent pour le déduire
fr: à votre place. Cependant, c'est une bonne idée car cela montre bien l'intention du développeur et facilite la compréhension.

en: Let's play a little.
en: We declare the type using `::`
fr: Jouons un peu.
fr: On déclare le type en utilisant `::`

<div class="codehighlight">
~~~~~~ {.haskell}
 f :: Int -> Int -> Int
 f x y = x*x + y*y

 main = print (f 2 3)
~~~~~~
</div>
~~~
~ runhaskell 20_very_basic.lhs
13
~~~

<a href="code/01_basic/10_Introduction/20_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>20_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/21_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>21_very_basic.lhs</strong></a>

en: Now try
fr: Maintenant essayez

<div class="codehighlight">
~~~~~~ {.haskell}
f :: Int -> Int -> Int
f x y = x*x + y*y

main = print (f 2.3 4.2)
~~~~~~
</div>
en: You should get this error:
fr: Vous devriez avoir cette erreur:

~~~
21_very_basic.lhs:6:23:
    No instance for (Fractional Int)
      arising from the literal `4.2'
    Possible fix: add an instance declaration for (Fractional Int)
    In the second argument of `f', namely `4.2'
    In the first argument of `print', namely `(f 2.3 4.2)'
    In the expression: print (f 2.3 4.2)
~~~

en: The problem: `4.2` isn't an Int.
fr: Le problème est que `4.2` n'est pas de type `Int` (_NDT: Il n'est pas un entier_)

<a href="code/01_basic/10_Introduction/21_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>21_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/22_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>22_very_basic.lhs</strong></a>

en: The solution: don't declare a type for `f` for the moment and let Haskell infer the most general type for us:
fr: La soulution: ne déclarez pas de type pour `f` pour le moment et laissez Haskell inférer le type le plus général pour nous:

<div class="codehighlight">
~~~~~~ {.haskell}
f x y = x*x + y*y

main = print (f 2.3 4.2)
~~~~~~
</div>
en: It works!
en: Luckily, we don't have to declare a new function for every single type.
en: For example, in `C`, you'll have to declare a function for `int`, for `float`, for `long`, for `double`, etc...
fr: Maintenant, ça marche!
fr: Heureursement, nous n'avons pas à déclarer un nouvelle fonction pour chaque type différent.
fr: Par exemple, en `C`, vous auriez dû déclarer un fonction pour `int`, pour `float`, pour `long`, pour `double`, etc...

en: But, what type should we declare?
en: To discover the type Haskell has found for us, just launch ghci:
fr: Mais quel type devons nous déclarer?
fr: Pour découvrir le type que Haskell a trouvé pour nous, lançons ghci:

<pre><span class="low">
%</span> ghci<span class="low"><code>
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
Prelude></code></span> let f x y = x*x + y*y
<span class="low"><code>Prelude></code></span> :type f
<code>f :: Num a => a -> a -> a</code>
</pre>

en: Uh? What is this strange type?
fr: Hein? Quel ce type étrange?

~~~
Num a => a -> a -> a
~~~

en: First, let's focus on the right part `a -> a -> a`.
en: To understand it, just look at a list of progressive examples:
fr: Premièrement, concentrons-nous sur la partie de droite: `a -> a -> a`.
fr: Pour le comprendre, regardez cette liste d'exemples progressifs:

en: --------------------------------------------------------------------------------------------------
en: The&nbsp;written&nbsp;type Its meaning
en: -------------------------- -----------------------------------------------------------------------
en: `Int`                      the type `Int`
en: 
en: `Int -> Int`               the type function from `Int` to `Int`
en: 
en: `Float -> Int`             the type function from `Float` to `Int`
en: 
en: `a -> Int`                 the type function from any type to `Int`
en: 
en: `a -> a`                   the type function from any type `a` to the same type `a`
en: 
en: `a -> a -> a`              the type function of two arguments of any type `a` to the same type `a`
en: --------------------------------------------------------------------------------------------------
fr: --------------------------------------------------------------------------------------------------------------------------------------
fr: Le&nbsp;type&nbsp;écrit    Son sens
fr: -------------------------- -----------------------------------------------------------------------------------------------------------
fr: `Int`                      Le type `Int`
fr: 
fr: `Int -> Int`               Le type de la fonction qui prend un `Int` et retourne un `Int`
fr: 
fr: `Float -> Int`             Le type de la fonction qui prend un `Float` et retourne un `Int`
fr: 
fr: `a -> Int`                 Le type de la fonction qui prend n'importe quel type de variable et retourne un `Int`
fr: 
fr: `a -> a`                   Le type de la fonction qui prend n'importe quel type `a` et retourne une variable du même type `a`
fr: 
fr: `a -> a -> a`              Le type de la fonction qui prend de arguments de n'importe quel type`a` et retourne une variable de type `a`
fr: --------------------------------------------------------------------------------------------------------------------------------------

en: In the type `a -> a -> a`, the letter `a` is a _type variable_.
en: It means `f` is a function with two arguments and both arguments and the result have the same type.
en: The type variable `a` could take many different type values.
en: For example `Int`, `Integer`, `Float`...
fr: Dans le type `a -> a -> a`, la lettre `a` est une _variable de type_.
fr: Cela signifie que `f` est une fonction avec deux arguments et que les deux arguments et le résultat ont le même type.
fr: La variable de type `a` peut prendre de nombreuses valeurs différentes
fr: Par exemple `Int`, `Integer`, `Float`...

en: So instead of having a forced type like in `C` and having to declare a function
en: for `int`, `long`, `float`, `double`, etc., we declare only one function like
en: in a dynamically typed language.
fr: Donc à la place d'avoir un type forcé comme en `C` et de devoir déclarer une fonction
fr: pour `int`, `long`, `float`, `double`, etc., nous déclarons une seule fonction comme
fr: dans un langage typé de façon dynamique.

en: This is sometimes called parametric polymorphism. It's also called having your
en: cake and eating it too.
fr: C'est parfois appelé le polymorphisme paramétrique. C'est aussi appelé avoir un
fr: gâteau et le manger.

en: Generally `a` can be any type, for example a `String` or an `Int`, but also
en: more complex types, like `Trees`, other functions, etc. But here our type is
en: prefixed with `Num a => `.
fr: Généralement `a` peut être de n'importe quel type, par exemple un `String` ou un `Int`, mais aussi
fr: des types plus complexes comme `Trees`, d'autres fonctions, etc. Mais ici notre type est
fr: préfixé par `Num a => `.

en: `Num` is a _type class_.
en: A type class can be understood as a set of types.
en: `Num` contains only types which behave like numbers.
en: More precisely, `Num` is class containing types which implement a specific list of functions, and in particular `(+)` and `(*)`.
fr: `Num` est une _classe de type_.
fr: Une classe de type peut être comprise comme un ensemble de types
fr: `Num` contient seulement les types qui se comportent comme des nombres.
fr: Plus précisement, `Num` est une classe qui contient des types qui implémentent une liste spécifique de fonctions,
fr: en particulier `(+)` et `(*)`.

en: Type classes are a very powerful language construct.
en: We can do some incredibly powerful stuff with this.
en: More on this later.
fr: Les classes de types sont une structure de langage très puissante.
fr: Nous pouvons faire des trucs incroyablement puissants avec.
fr: Nous verrons cela plus tard.

en: Finally, `Num a => a -> a -> a` means:
fr: Finalement, `Num a => a -> a -> a` signifie:

en: Let `a` be a type belonging to the `Num` type class.
en: This is a function from type `a` to (`a -> a`).
fr: soit `a` un type qui appartient à la classe `Num`.
fr: C'est une fonction qui prend une variable de type `a` et retourne une fonction de type `(a -> a)`

en: Yes, strange.
en: In fact, in Haskell no function really has two arguments.
en: Instead all functions have only one argument.
en: But we will note that taking two arguments is equivalent to taking one argument and returning a function taking the second argument as a parameter.
fr: Oui, c'est étrange.
fr: En fait, en Haskell aucune fonction ne prend réellement deux arguments.
fr: Au lieu de cela toutes les fonctions n'ont qu'un argument unique.
fr: Mais nous retiendrons que prendre deux arguments est équivalent à n'en prendre qu'un et à retourner une fonction qui prend le second argument en paramètre.

en: More precisely `f 3 4` is equivalent to `(f 3) 4`.
en: Note `f 3` is a function:
fr: Plus précisement `f 3 4` est équivalent à `(f 3) 4 `.
fr: Remarque: `f 3` est une fonction:

~~~
f :: Num a => a -> a -> a

g :: Num a => a -> a
g = f 3

g y ⇔ 3*3 + y*y
~~~

en: Another notation exists for functions.
en: The lambda notation allows us to create functions without assigning them a name.
en: We call them anonymous functions.
en: We could also have written:
fr: Une autre notation existe pour les fonctions.
fr: La notation lambda nous autorise à créer des fonctions sans leur assigner un nom.
fr: On les appelle des fonctions anonymes.
fr: nous aurions donc pu écrire:

~~~
g = \y -> 3*3 + y*y
~~~

en: The `\` is used because it looks like `λ` and is ASCII.
fr: Le `\` esst utilisé car il ressemble à un `λ` et est un caractère ASCII.

en: If you are not used to functional programming your brain should be starting to heat up.
en: It is time to make a real application.
fr: Si vous n'êtes pas habitué à la programmation fonctionnelle, votre cerveau devrait commencer à chauffer
fr: Il est temps de faire une vraie application.

<a href="code/01_basic/10_Introduction/22_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>22_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/23_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>23_very_basic.lhs</strong></a>

en: But just before that, we should verify the type system works as expected:
fr: Mais juste avant cela, nous devrions vérifier que le système de type marche comme nous le supposons:

<div class="codehighlight">
~~~~~~ {.haskell}
f :: Num a => a -> a -> a
f x y = x*x + y*y

main = print (f 3 2.4)
~~~~~~
</div>
en: It works, because, `3` is a valid representation both for Fractional numbers like Float and for Integer.
en: As `2.4` is a Fractional number, `3` is then interpreted as being also a Fractional number.
fr: Cela fonctionne, car `3` est une représentation valide autant pour les nombres fractionnaires comme Float que pour les entiers.
fr: Comme `2.4` est un nombre fractionnaire, `3` est interprété comme une autre nombre fractionnaire

<a href="code/01_basic/10_Introduction/23_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>23_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/24_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>24_very_basic.lhs</strong></a>

en: If we force our function to work with different types, it will fail:
fr: Si nous forçons notre fonction à travailler avec des types différents, le test échouera:

<div class="codehighlight">
~~~~~~ {.haskell}
f :: Num a => a -> a -> a
f x y = x*x + y*y

x :: Int
x = 3
y :: Float
y = 2.4
-- won't work because type x ≠ type y
main = print (f x y)
~~~~~~
</div>
en: The compiler complains.
en: The two parameters must have the same type.
fr: Le compilateur se plaint.
fr: Les deux paramètres doivent avoir le même type.

en: If you believe that this is a bad idea, and that the compiler should make the transformation
en: from one type to another for you, you should really watch this great (and funny) video:
en: [WAT](https://www.destroyallsoftware.com/talks/wat)
fr: Si vous pensez que c'est une mauvaise idée et que le compilateur devrait faire la transformation
fr: depuis un type à un autre pour vous, vous devriez vraiment regarder cette vidéo géniale (et amusante):
fr: [WAT](https://www.destroyallsoftware.com/talks/wat) (_NDT: En Anglais_)

<a href="code/01_basic/10_Introduction/24_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>24_very_basic.lhs</strong> </a>

en: <h2 id="essential-haskell">Essential Haskell</h2>
fr: <h2 id="essential-haskell">Notions essentielles</h2>

blogimage("kandinsky_gugg.jpg","Kandinsky Gugg")

en: I suggest that you skim this part.
en: Think of it as a reference.
en: Haskell has a lot of features.
en: A lot of information is missing here.
en: Come back here if the notation feels strange.
fr: Je vous suggère de seulement survoler cette partie
fr: Pensez-y seulement comme à une référence.
fr: Haskell a beaucoup de caractèristiques
fr: Il manque beaucoup d'informations ici.
fr: Revenz ici si la notation vous semble étrange.

en: I use the `⇔` symbol to state that two expression are equivalent.
en: It is a meta notation, `⇔` does not exists in Haskell.
en: I will also use `⇒` to show what the return value of an expression is.
fr: J'utilise le symbole `⇔` pour signifier que deux expressions sont équivalentes.
fr: C'est une notation extérieure, `⇔` n'existe pas en Haskell.
fr: Je vais aussi utiliser le symoble `⇒` quelle est la valeur que retourne une fonction.

<h3 id="notations">Notations</h3>

en: <h5 id="arithmetic">Arithmetic</h5>
fr: <h5 id="arithmetic">Arithmétique</h5>

~~~
3 + 2 * 6 / 3 ⇔ 3 + ((2*6)/3)
~~~

en: <h5 id="logic">Logic</h5>
fr: <h5 id="logic">Logique</h5>

~~~
True || False ⇒ True
True && False ⇒ False
True == False ⇒ False
en: True /= False ⇒ True  (/=) is the operator for different
fr: True /= False ⇒ True  (/=) est l'opérateur pour "différent de"
~~~

en: <h5 id="powers">Powers</h5>
fr: <h5 id="powers">Puissances</h5>

~~~
en: x^n     for n an integral (understand Int or Integer)
en: x**y    for y any kind of number (Float for example)
fr: x^n     pour n un entier (comprenez Int ou Integer)
fr: x**y    pour y tout type de nombre (Float par exemple)
~~~

en: `Integer` has no limit except the capacity of your machine:
fr: `Integer` n'a aucune limite à part la capacité de votre machine:

~~~
4^103
102844034832575377634685573909834406561420991602098741459288064
~~~

Yeah!
en: And also rational numbers FTW!
en: But you need to import the module `Data.Ratio`:
fr: Et aussi les nombres rationnels!
fr: Mais vous avez besoin d'importer le module `Data.Ratio`

~~~
$ ghci
....
Prelude> :m Data.Ratio
Data.Ratio> (11 % 15) * (5 % 3)
11 % 9
~~~

en: <h5 id="lists">Lists</h5>
fr: <h5 id="lists">Listes</h5>

~~~
en: []                      ⇔ empty list
en: [1,2,3]                 ⇔ List of integral
en: ["foo","bar","baz"]     ⇔ List of String
en: 1:[2,3]                 ⇔ [1,2,3], (:) prepend one element
en: 1:2:[]                  ⇔ [1,2]
en: [1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concatenate
en: [1,2,3] ++ ["foo"]      ⇔ ERROR String ≠ Integral
en: [1..4]                  ⇔ [1,2,3,4]
en: [1,3..10]               ⇔ [1,3,5,7,9]
en: [2,3,5,7,11..100]       ⇔ ERROR! I am not so smart!
en: [10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
fr: []                      ⇔ liste vide
fr: [1,2,3]                 ⇔ Liste d'entiers
fr: ["foo","bar","baz"]     ⇔ Liste de chaînes de caractères
fr: 1:[2,3]                 ⇔ [1,2,3], (:) ajoute un élément au début
fr: 1:2:[]                  ⇔ [1,2]
fr: [1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concaténation de deux listes
fr: [1,2,3] ++ ["foo"]      ⇔ ERREUR String ≠ Integral
fr: [1..4]                  ⇔ [1,2,3,4]
fr: [1,3..10]               ⇔ [1,3,5,7,9]
fr: [2,3,5,7,11..100]       ⇔ ERREUR! Je ne suis pas si intelligent!
fr: [10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
~~~

en: <h5 id="strings">Strings</h5>
fr: <h5 id="strings">Chaînes de caractères</h5>

en: In Haskell strings are list of `Char`.
fr: En Haskell les chaînes de caractères sont des listes de `Char`.

~~~
'a' :: Char
"a" :: [Char]
""  ⇔ []
"ab" ⇔ ['a','b'] ⇔  'a':"b" ⇔ 'a':['b'] ⇔ 'a':'b':[]
"abc" ⇔ "ab"++"c"
~~~

en:  > _Remark_:
en:  > In real code you shouldn't use list of char to represent text.
en:  > You should mostly use `Data.Text` instead.
en:  > If you want to represent a stream of ASCII char, you should use `Data.ByteString`.
fr:  > _Remarque_:
fr:  > Dans un vrai code vous n'utiliserez pas des listes de char pour représenter du texte.
fr:  > Vous utiliserez plus souvent `Data.Text` à la place.
fr:  > Si vous voulez représenter un chapelet de caractères ASCII, vous utiliserez `Data.ByteString`.

<h5 id="tuples">Tuples</h5>

en: The type of couple is `(a,b)`.
en: Elements in a tuple can have different types.
fr: Le type d'un couple est `(a,b)`.
fr: Les éléments d'un tuple peuvent avoir des types différents.

~~~
en: -- All these tuples are valid
fr: -- tous ces tuples sont valides
(2,"foo")
(3,'a',[2,3])
((2,"a"),"c",3)

fst (x,y)       ⇒  x
snd (x,y)       ⇒  y

fst (x,y,z)     ⇒  ERROR: fst :: (a,b) -> a
snd (x,y,z)     ⇒  ERROR: snd :: (a,b) -> b
~~~

en: <h5 id="deal-with-parentheses">Deal with parentheses</h5>
fr: <h5 id="deal-with-parentheses">Traiter avec les parenthèses</h5>

en: To remove some parentheses you can use two functions: `($)` and `(.)`.
fr: Pour enlever des parenthèses vous pouvez utiliser deux fonctions: `($)` et `(.)`.

~~~
en: -- By default:
fr: -- Par défaut:
f g h x         ⇔  (((f g) h) x)

en: -- the $ replace parenthesis from the $
en: -- to the end of the expression
fr: -- le $ remplace les parenthèses depuis le $
fr: -- jusqu'à la fin de l'expression.
f g $ h x       ⇔  f g (h x) ⇔ (f g) (h x)
f $ g h x       ⇔  f (g h x) ⇔ f ((g h) x)
f $ g $ h x     ⇔  f (g (h x))

en: -- (.) the composition function
fr: -- (.) permet de faire des compositions de fonctions
(f . g) x       ⇔  f (g x)
(f . g . h) x   ⇔  f (g (h x))
~~~

<hr/><a href="code/01_basic/20_Essential_Haskell/10a_Functions.lhs" class="cut">01_basic/20_Essential_Haskell/<strong>10a_Functions.lhs</strong></a>

en: <h3 id="useful-notations-for-functions">Useful notations for functions</h3>
fr: <h3 id="useful-notations-for-functions">Notations utiles pour les fonctions</h3>

en: Just a reminder:
fr: Juste un mémo:

~~~
en: x :: Int            ⇔ x is of type Int
en: x :: a              ⇔ x can be of any type
en: x :: Num a => a     ⇔ x can be any type a
en:                       such that a belongs to Num type class 
en: f :: a -> b         ⇔ f is a function from a to b
en: f :: a -> b -> c    ⇔ f is a function from a to (b→c)
en: f :: (a -> b) -> c  ⇔ f is a function from (a→b) to c
fr: x :: Int            ⇔ x est de type Int
fr: x :: a              ⇔ x peut être de n'importe quel type
fr: x :: Num a => a     ⇔ x peut être de n'importe quel type a
fr:                       tant qu' a appartient à la classe de type Num 
fr: f :: a -> b         ⇔ f est une fonction qui prend un a et retourne un b
fr: f :: a -> b -> c    ⇔ f est une fonction qui prend un a et retourne un (b→c)
fr: f :: (a -> b) -> c  ⇔ f est une fonction qui prend un (a→b) et retourne un c
~~~

en: Remember that defining the type of a function before its declaration isn't mandatory.
en: Haskell infers the most general type for you.
en: But it is considered a good practice to do so.
fr: Rappelez-vous que définir le type d'une fonction avant sa déclaration n'est pas obligatoire.
fr: Haskell infère le type le plus général pour vous.
fr: Mais c'est considéré comme une bonne pratique.

en: _Infix notation_
fr: _Notation Infixée_

<div class="codehighlight">
~~~~~~ {.haskell}
square :: Num a => a -> a
square x = x^2
~~~~~~
</div>
en: Note `^` uses infix notation.
en: For each infix operator there its associated prefix notation.
en: You just have to put it inside parenthesis.
fr: Remarquez que `^` utilise une notation infixée.
fr: Pour chaque opérateur infixe il y a une notation préfixée associée.
fr: Vous devz juste l'écrire entre parenthèses.

<div class="codehighlight">
~~~~~~ {.haskell}
square' x = (^) x 2

square'' x = (^2) x
~~~~~~
</div>
en: We can remove `x` in the left and right side!
en: It's called η-reduction.
fr: Nous pouvons enlever le `x` dans les parties de gauche et de droite!
fr: On appelle cela la η-réduction

<div class="codehighlight">
~~~~~~ {.haskell}
square''' = (^2)
~~~~~~
</div>
en: Note we can declare functions with `'` in their name.
en: Here:
fr: Rmarquez qu nous pouvons déclarer des fonctions avec `'` dans leur nom.
fr: Exemples:

 > `square` ⇔  `square'` ⇔ `square''` ⇔ `square'''`

_Tests_

en: An implementation of the absolute function.
fr: Une implémentation de la fonction absolue.

<div class="codehighlight">
~~~~~~ {.haskell}
absolute :: (Ord a, Num a) => a -> a
absolute x = if x >= 0 then x else -x
~~~~~~
</div>
en: Note: the `if .. then .. else` Haskell notation is more like the
en: `¤?¤:¤` C operator. You cannot forget the `else`.
fr: Remarque: la notation de Haskell pour le `if .. then .. else` ressemble plus
fr: à l'opérateur `¤?¤:¤` en C. Le `else` est obligatoire.

en: Another equivalent version:
fr: Une version équivalente:

<div class="codehighlight">
~~~~~~ {.haskell}
absolute' x
    | x >= 0 = x
    | otherwise = -x
~~~~~~
</div>
en:  > Notation warning: indentation is _important_ in Haskell.
en:  > Like in Python, bad indentation can break your code!
fr:  > Avertissement: l'indentation est _importante_ en Haskell.
fr:  > Comme en Python, une mauvaise indentation peut détruire votre code!

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
      print $ square 10
      print $ square' 10
      print $ square'' 10
      print $ square''' 10
      print $ absolute 10
      print $ absolute (-10)
      print $ absolute' 10
      print $ absolute' (-10)
~~~~~~
</div>
</div>

<a href="code/01_basic/20_Essential_Haskell/10a_Functions.lhs" class="cut">01_basic/20_Essential_Haskell/<strong>10a_Functions.lhs</strong> </a>

en: <h2 id="hard-part">Hard Part</h2>
fr: <h2 id="hard-part">La Partie Difficile</h2>

en: The hard part can now begin.
fr: La partie difficile peut maintenant commencer.

en: <h3 id="functional-style">Functional style</h3>
fr: <h3 id="functional-style">Le style fonctionnel</h3>

blogimage("hr_giger_biomechanicallandscape_500.jpg","Biomechanical Landscape by H.R. Giger")

en: In this section, I will give a short example of the impressive refactoring ability provided by Haskell.
en: We will select a problem and solve it in a standard imperative way.
en: Then I will make the code evolve.
en: The end result will be both more elegant and easier to adapt.
fr: Dans cette section, je vais vous donner un court exemple de l'impressionante capacité de remaniement de Haskell.
fr: Nous allons sélectionner un problème et le résoudre à la manière d'un langage impératif standard.
fr: Ensuite, je ferais évoluer le code.
fr: Le résultat final sera plus élégant et plus facile à adapter.

en: Let's solve the following problem:
fr: résolvons les problèmes suivants:

en:  > Given a list of integers, return the sum of the even numbers in the list.
fr:  > Soit une liste d'entiers, retourner la somme des nombres pairs de cette liste.
 >
en:  > example:
fr:  > exemple:
 > `[1,2,3,4,5] ⇒  2 + 4 ⇒  6`

en: To show differences between functional and imperative approaches,
en: I'll start by providing an imperative solution (in JavaScript):
fr: Pour montrer les différences entre les approches fonctionnelle et impérative,
fr: je vais commencer par donner la solution impérative (en JavaScript):

~~~~~~ {.javascript}
function evenSum(list) {
    var result = 0;
    for (var i=0; i< list.length ; i++) {
        if (list[i] % 2 ==0) {
            result += list[i];
        }
    }
    return result;
}
~~~~~~

en: In Haskell, by contrast, we don't have variables or a for loop.
en: One solution to achieve the same result without loops is to use recursion.
fr: En Haskell, en revanche, nous n'avons pas de variables ou un boucle `for`.
fr: Une des solutions pour parvenir au même résultat sans boucles est d'utiliser la récursion.

en:  > _Remark_:
en:  > Recursion is generally perceived as slow in imperative languages.
en:  > But this is generally not the case in functional programming.
en:  > Most of the time Haskell will handle recursive functions efficiently.
fr:  > _Remarque_:
fr:  > La récursion est souvent perçue comme lente dans les langages impératifs.
fr:  > Mais ce n'est généralement pas le cas en programmation fonctionnelle.
fr:  > La plupart du temps Haskell gérera les fonctions récursives efficacement.

en: Here is a `C` version of the recursive function.
en: Note that for simplicity I assume the int list ends with the first `0` value.
fr: Voici la version `C` de la fonction récursive.
fr: Remarquez que je suppose que la liste d'int fini avec la première valeur `0`.

~~~~~~ {.c}
int evenSum(int *list) {
    return accumSum(0,list);
}

int accumSum(int n, int *list) {
    int x;
    int *xs;
    if (*list == 0) { // if the list is empty
        return n;
    } else {
        x = list[0]; // let x be the first element of the list
        xs = list+1; // let xs be the list without x
        if ( 0 == (x%2) ) { // if x is even
            return accumSum(n+x, xs);
        } else {
            return accumSum(n, xs);
        }
    }
}
~~~~~~

en: Keep this code in mind. We will translate it into Haskell.
en: First, however, I need to introduce three simple but useful functions we will use:
fr: Gardez ce code à l'esprit. Nous allons le traduire en Haskell.
fr: Premièrement,

~~~~~~ {.haskell}
even :: Integral a => a -> Bool
head :: [a] -> a
tail :: [a] -> [a]
~~~~~~

en: `even` verifies if a number is even.
fr: `even` vérifie si un nombre est pair.

~~~~~~ {.haskell}
even :: Integral a => a -> Bool
even 3  ⇒ False
even 2  ⇒ True
~~~~~~

en: `head` returns the first element of a list:
fr: `head` retourne le premier élément d'une liste:

~~~~~~ {.haskell}
head :: [a] -> a
head [1,2,3] ⇒ 1
head []      ⇒ ERROR
~~~~~~

en: `tail` returns all elements of a list, except the first:
fr: `tail` retourne tous les éléments d'une liste, sauf le premier:

~~~~~~ {.haskell}
tail :: [a] -> [a]
tail [1,2,3] ⇒ [2,3]
tail [3]     ⇒ []
en: tail []      ⇒ ERROR
fr: tail []      ⇒ ERREUR
~~~~~~

en: Note that for any non empty list `l`,
en:  `l ⇔ (head l):(tail l)`
fr: Remarquez que pour toute liste non-vide `l`,
fr:  `l ⇔ (head l):(tail l)`

<hr/><a href="code/02_Hard_Part/11_Functions.lhs" class="cut">02_Hard_Part/<strong>11_Functions.lhs</strong></a>

en: The first Haskell solution.
en: The function `evenSum` returns the sum of all even numbers in a list:
fr: La première solution en Haskell.
fr: La fonction `evenSum` retourne la somme de tous les nombres pairs d'une liste:

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 1
evenSum :: [Integer] -> Integer

evenSum l = accumSum 0 l

accumSum n l = if l == []
                  then n
                  else let x = head l
                           xs = tail l
                       in if even x
                              then accumSum (n+x) xs
                              else accumSum n xs
~~~~~~
</div>
en: To test a function you can use `ghci`:
fr: Pour tester une fonction nous pouvons utiliser `ghci`:

<pre>
% ghci
<span class="low">GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude&gt;</span> :load 11_Functions.lhs
<span class="low">[1 of 1] Compiling Main             ( 11_Functions.lhs, interpreted )
Ok, modules loaded: Main.
*Main&gt;</span> evenSum [1..5]
6
</pre>

en: Here is an example of execution[^2]:
fr: Voici un exemple d'exécution[^2]:

en: [^2]: I know I'm cheating. But I will talk about non-strictness later.
fr: [^2]: Je sais que je triche. Mais je parlerais de la non-rigueur plus tard. <!-- IL FAUDRA TROUVER UNE AUTRE TRADUCTION POUR NON-STRICTNESS -->

<pre>
*Main> evenSum [1..5]
accumSum 0 [1,2,3,4,5]
en: <span class="yellow">1 is odd</span>
fr: <span class="yellow">1 est impair</span>
accumSum 0 [2,3,4,5]
en: <span class="yellow">2 is even</span>
fr: <span class="yellow">2 est pair</span>
accumSum (0+2) [3,4,5]
en: <span class="yellow">3 is odd</span>
fr: <span class="yellow">3 est impair</span>
accumSum (0+2) [4,5]
en: <span class="yellow">2 is even</span>
fr: <span class="yellow">4 est pair</span>
accumSum (0+2+4) [5]
en: <span class="yellow">5 is odd</span>
fr: <span class="yellow">5 est impair</span>
accumSum (0+2+4) []
<span class="yellow">l == []</span>
0+2+4
0+6
6
</pre>

en: Coming from an imperative language all should seem right.
en: In fact, many things can be improved here.
en: First, we can generalize the type.
fr: En venant d'un langage impératif, tout devrait vous sembler juste.
fr: En fait, beaucoup de choses peuvent être améliorées ici.
fr: Tout d'abord, nous pouvons généraliser le type.

~~~~~~ {.haskell}
evenSum :: Integral a => [a] -> a
~~~~~~

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = do print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/11_Functions.lhs" class="cut">02_Hard_Part/<strong>11_Functions.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/12_Functions.lhs" class="cut">02_Hard_Part/<strong>12_Functions.lhs</strong></a>

en: Next, we can use sub functions using `where` or `let`.
fr: Ensuite, nous pouvons utiliser des sous-fonctions grâce à `where` et `let`.
en: This way our `accumSum` function won't pollute the namespace of our module.
fr: Ansi, notre fonction `accumSum` ne polluera pas le _namespace_ de notre module

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 2
evenSum :: Integral a => [a] -> a

evenSum l = accumSum 0 l
    where accumSum n l =
            if l == []
                then n
                else let x = head l
                         xs = tail l
                     in if even x
                            then accumSum (n+x) xs
                            else accumSum n xs
~~~~~~
</div>
<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/12_Functions.lhs" class="cut">02_Hard_Part/<strong>12_Functions.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/13_Functions.lhs" class="cut">02_Hard_Part/<strong>13_Functions.lhs</strong></a>

en: Next, we can use pattern matching.
fr: Puis on utilise le _pattern matching_

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 3
evenSum l = accumSum 0 l
    where
        accumSum n [] = n
        accumSum n (x:xs) =
             if even x
                then accumSum (n+x) xs
                else accumSum n xs
~~~~~~
</div>
en: What is pattern matching?
en: Use values instead of general parameter names[^021301].
fr: Qu'est ce que le _pattern matching_ ?
fr: Il s'agit d'utiliser des valeurs au lieu de noms de paramètres généraux.

en: [^021301]: For the brave, a more complete explanation of pattern matching can be found [here](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html).
fr: [^021301]: Pour les plus courageux, une explication plus complète du _pattern matching_ peut être trouvée [ici](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html) (_NdT: En anglais_)

en: Instead of saying: `foo l = if l == [] then <x> else <y>`
en: You simply state:
fr: Au lieu d'écrire: `foo l = if l == [] then <x> else <y>`
fr: Vous écrivez tout simplement :

~~~~~~ {.haskell}
foo [] =  <x>
foo l  =  <y>
~~~~~~

en: But pattern matching goes even further.
en: It is also able to inspect the inner data of a complex value.
en: We can replace
fr: Mais le _pattern matching_ peut aller encore plus loin.
fr: Il est également capable d'inspect les données internes d'un valeur complexe.
fr: Nous pouvons ainsi remplacer

~~~~~~ {.haskell}
foo l =  let x  = head l
             xs = tail l
         in if even x
             then foo (n+x) xs
             else foo n xs
~~~~~~

en: with
fr: par

~~~~~~ {.haskell}
foo (x:xs) = if even x
                 then foo (n+x) xs
                 else foo n xs
~~~~~~

en: This is a very useful feature.
en: It makes our code both terser and easier to read.
fr: C'est une caractéristique très utile.
fr: Notre code est ainsi plus concis et plus facile à lire.

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/13_Functions.lhs" class="cut">02_Hard_Part/<strong>13_Functions.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/14_Functions.lhs" class="cut">02_Hard_Part/<strong>14_Functions.lhs</strong></a>

en: In Haskell you can simplify function definitions by η-reducing them.
en: For example, instead of writing:
fr: Avec Haskell, nous pouvons simplifier les défitions des fonctions en les _η-réduisant_ .
fr: Par exemple, au lieu d'écrire:

~~~~~~ {.haskell}
en: f x = (some expresion) x
fr: f x = (expression) x
~~~~~~

en: you can simply write
fr: Nous pouvons écrire

~~~~~~ {.haskell}
en: f = some expression
fr: f = expression
~~~~~~

en: We use this method to remove the `l`:
fr: Utilisons cette méthode pour retirer le `l`:

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 4
evenSum :: Integral a => [a] -> a

evenSum = accumSum 0
    where
        accumSum n [] = n
        accumSum n (x:xs) =
             if even x
                then accumSum (n+x) xs
                else accumSum n xs
~~~~~~
</div>
<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/14_Functions.lhs" class="cut">02_Hard_Part/<strong>14_Functions.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/15_Functions.lhs" class="cut">02_Hard_Part/<strong>15_Functions.lhs</strong></a>

en: <h4 id="higher-order-functions">Higher Order Functions</h4>
fr: <h4 id="higher-order-functions">Fonctions d'ordre supérieur</h4>

blogimage("escher_polygon.png","Escher")

en: To make things even better we should use higher order functions.
en: What are these beasts?
en: Higher order functions are functions taking functions as parameters.
fr: Pour rendre les choses plus faciles, nous devrions utiliser des fonctions d'ordre supérieur.
fr: Ce sont des fonctions qui prennent des fonctions en paramètres

en: Here are some examples:
fr: Voici quelques exemples:

~~~~~~ {.haskell}
filter :: (a -> Bool) -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
foldl :: (a -> b -> a) -> a -> [b] -> a
~~~~~~

en: Let's proceed by small steps.
fr: Procédons par étapes.

~~~~~~ {.haskell}
-- Version 5
evenSum l = mysum 0 (filter even l)
    where
      mysum n [] = n
      mysum n (x:xs) = mysum (n+x) xs
~~~~~~

en: where
fr: où

~~~~~~ {.haskell}
filter even [1..10] ⇔  [2,4,6,8,10]
~~~~~~

en: The function `filter` takes a function of type (`a -> Bool`) and a list of type `[a]`.
en: It returns a list containing only elements for which the function returned `true`.
fr: La fonction `filter` prend une fonction du type (`a -> Bool`) et une liste de type `[a]`.
fr: Elle retourne une liste qui contient seulement les élements pour qui la fonction  a retourné `True`.

en: Our next step is to use another technique to accomplish the same thing as a loop.
en: We will use the `foldl` function to accumulate a value as we pass through the list.
en: The function `foldl` captures a general coding pattern:
fr: La prochaine étape est d'utiliser une autre technique pour accomplir la même chose qu'une boucle.
fr: Nous allons utiliser la fonction `foldl` pour accumuler une valeur au fur et à mesure que l'on parcoure la liste.
fr: La fonction `foldl` capture un modèle de code général:

<pre>
    myfunc list = foo <span class="blue">initialValue</span> <span class="green">list</span>
    foo accumulated []     = accumulated
    foo tmpValue    (x:xs) = foo (<span class="yellow">bar</span> tmpValue x) xs
</pre>

en: Which can be replaced by:
fr: Qui peut être remplacé par:

<pre>
myfunc list = foldl <span class="yellow">bar</span> <span class="blue">initialValue</span> <span class="green">list</span>
</pre>

en: If you really want to know how the magic works, here is the definition of `foldl`:
fr: Si vous souhaitez vraiment savoir comment la magie se produit, voici la définition de `foldl`:

~~~~~~ {.haskell}
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
~~~~~~

~~~~~~ {.haskell}
foldl f z [x1,...xn]
⇔  f (... (f (f z x1) x2) ...) xn
~~~~~~

en: But as Haskell is lazy, it doesn't evaluate `(f z x)` and  simply pushes it onto the stack.
en: This is why we generally use `foldl'` instead of `foldl`;
en: `foldl'` is a _strict_ version of `foldl`.
en: If you don't understand what lazy and strict means,
en: don't worry, just follow the code as if `foldl` and `foldl'` were identical.
fr: Mais comme Haskell est paresseux, il n'évalue pas `(f z x)` et le met simplement dans la pile.
fr: C'est pourquoi on utilise généralement `foldl'`, une version _stricte_ de `foldl`,
fr: Si vous ne comprenez pas encore ce que _paresseux_ ou _strict_ signifie,
fr: ne vous inquiétez pas, suivez le code comme si `foldl'` et `foldl` étaient identiques

en: Now our new version of `evenSum` becomes:
fr: Maintenant notre version de `evenSum` devient:

~~~~~~ {.haskell}
-- Version 6
en: -- foldl' isn't accessible by default
en: -- we need to import it from the module Data.List
fr: -- foldl' n'est pas accessible par défaut
fr: -- nous devons l'importer depuis le module Data.List
import Data.List
evenSum l = foldl' mysum 0 (filter even l)
  where mysum acc value = acc + value
~~~~~~

en: We can also simplify this by using directly a lambda notation.
en: This way we don't have to create the temporary name `mysum`.
fr: Nous pouvons aussi simplifier cela en utilisant une _lambda-notation_.
fr: Ainsi nous n'avons pas besoin de créer le nom temporaire `mySum`.

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 7
-- Generally it is considered a good practice
-- to import only the necessary function(s)
import Data.List (foldl')
evenSum l = foldl' (\x y -> x+y) 0 (filter even l)
~~~~~~
</div>
en: And of course, we note that
fr: Et bien sûr, nous remarquons que

~~~~~~ {.haskell}
(\x y -> x+y) ⇔ (+)
~~~~~~

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/15_Functions.lhs" class="cut">02_Hard_Part/<strong>15_Functions.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/16_Functions.lhs" class="cut">02_Hard_Part/<strong>16_Functions.lhs</strong></a>

en: Finally
fr: Finalement

~~~~~~ {.haskell}
-- Version 8
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum l = foldl' (+) 0 (filter even l)
~~~~~~

en: `foldl'` isn't the easiest function to grasp.
en: If you are not used to it, you should study it a bit.
fr: `foldl'` n'est pas la fonction la plus facile à prendre en main.
fr: Si vous n'y êtes pas habitué, vous devriez l'étudier un peu.

en: To help you understand what's going on here, let's look at a step by step evaluation:
fr: Pour mieux comprendre ce qui se passe ici, étudions une évaluation étape par étape:

<pre>
  <span class="yellow">evenSum [1,2,3,4]</span>
⇒ foldl' (+) 0 (<span class="yellow">filter even [1,2,3,4]</span>)
⇒ <span class="yellow">foldl' (+) 0 <span class="blue">[2,4]</span></span>
⇒ <span class="blue">foldl' (+) (<span class="yellow">0+2</span>) [4]</span>
⇒ <span class="yellow">foldl' (+) <span class="blue">2</span> [4]</span>
⇒ <span class="blue">foldl' (+) (<span class="yellow">2+4</span>) []</span>
⇒ <span class="yellow">foldl' (+) <span class="blue">6</span> []</span>
⇒ <span class="blue">6</span>
</pre>

en: Another useful higher order function is `(.)`.
en: The `(.)` function corresponds to mathematical composition.
fr: Une autre fonction d'ordre supérieur utile est `(.)`.
fr: Elle correspond à une composition en mathématiques.

~~~~~~ {.haskell}
(f . g . h) x ⇔  f ( g (h x))
~~~~~~

en: We can take advantage of this operator to η-reduce our function:
fr: Nous pouvons profiter de cet opérateur pour η-réduire notre fonction:

~~~~~~ {.haskell}
-- Version 9
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum = (foldl' (+) 0) . (filter even)
~~~~~~

en: Also, we could rename some parts to make it clearer:
fr: Nous pouvons maintenant renommer certaines parties pour rendre le tout plus clair:

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 10
import Data.List (foldl')
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0
evenSum :: Integral a => [a] -> a
evenSum = sum' . (filter even)

~~~~~~
</div>
en: It is time to discuss the direction our code has moved as we introduced more functional idioms.
en: What did we gain by using higher order functions?
fr: Il est temps de discuter de la direction qu'a pris notre code depuis que nous avons introduit plus d'idiomes fonctionnels.
fr: Que gagnons-nous à utiliser des fonctions d'ordre supérieur?

en: At first, you might think the main difference is terseness. But in fact, it has
en: more to do with better thinking. Suppose we want to modify our function
en: slightly, for example, to get the sum of all even squares of elements of the list.
fr: D'abord, vous pourriez penser que la principale différence est la brièveté. Mais en réalité,
fr: il s'agit d'une meilleure façon de penser. Supposons que nous voulons modifier légèrement notre fonction,
fr: par exemple, pour qu'elle renvoie la somme de tous les carrés pairs des éléments de la liste.

~~~
[1,2,3,4] ▷ [1,4,9,16] ▷ [4,16] ▷ 20
~~~

en: Updating version 10 is extremely easy:
fr: Mettre la version 10 à jour est très facile:

<div class="codehighlight">
~~~~~~ {.haskell}
squareEvenSum = sum' . (filter even) . (map (^2))
squareEvenSum' = evenSum . (map (^2))
~~~~~~
</div>
en: We just had to add another "transformation function"[^0216].
fr: Nous avons juste eu à ajouter une autre "fonction de trabsformation"[^0216].

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

en: The `map` function simply applies a function to all the elements of a list.
fr: La fonction `map` applique simplementune fonction à tous les élements d'une liste.

en: We didn't have to modify anything _inside_ the function definition.
en: This makes the code more modular.
en: But in addition you can think more mathematically about your function.
en: You can also use your function interchangably with others, as needed.
en: That is, you can compose, map, fold, filter using your new function.
fr: Nous n'avons rien modifié _à l'intérieur_ de notre définition de fonction.
fr: Cela rend le code plus modulaire.
fr: En plus de cela, vous pouvez penser à votre fonction plus mathématiquement.
fr: Vous pouvez aussi utilier votre fonction avec d'autres, au besoin:
fr: vous pouvez utiliser `compose`, `map`, `fold` ou `filter` sur notre nouvelle fonction.

en: Modifying version 1 is left as an exercise to the reader ☺.
fr: Modifier la version 1 est laissé comme un exercice pour le lecteur ☺.

en: If you believe we have reached the end of generalization, then know you are very wrong.
en: For example, there is a way to not only use this function on lists but on any recursive type.
en: If you want to know how, I suggest you to read this quite fun article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf).
fr: Si vous croyez avoir atteint le bout de la généralisation, vous avez tout faux.
fr: Par example, il y a un moyen d'utiliser cette fonction non seulement sur les listes mais aussi sur n'importe quel type récursif.
fr: Si vous voulez savoir comment, je vous suggère de lire cet article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf) (_NDT: en anglais, mais là vous vous en seriez douté je pense ☺_)

en: This example should show you how great pure functional programming is.
en: Unfortunately, using pure functional programming isn't well suited to all usages.
en: Or at least such a language hasn't been found yet.
fr: Cet exemple montre à quel point la programmation fonctionnelle pure est géniale.
fr: Malheureusement, utiliser cet outil n'est pas adapté à tous les besoins.
fr: Ou alors un langage qui le premettrait n'a pas encore été trouvé.

en: One of the great powers of Haskell is the ability to create DSLs
en: (Domain Specific Language)
en: making it easy to change the programming paradigm.
fr: Une des grands pouvoirs de Haskell est sa capacité à créer des DSLs
fr: (_Domain Specific Language_, en français : _langage spécifique à un domaine_)
fr: Il est ainsi facile de changer le pardigme de programmation

en: In fact, Haskell is also great when you want to write imperative style
en: programming. Understanding this was really hard for me to grasp when first
en: learning Haskell. A lot of effort tends to go into explaining the superiority
en: of the functional approach. Then when you start using an imperative style with
en: Haskell, it can be hard to understand when and how to use it.
fr: En fait, Haskell peut très bien vous permettre d'écrire des programmes impératifs.
fr: Comprendre cela a été très difficile pour moi lorsque j'apprenais Haskell.
fr: Beaucoup d'efforts tendent à expliquer la supériorité de l'approche fonctionnele.
fr: Puis lorsque vous commencez à utliser le style impératif en Haskell,
fr: Il peut être difficile de comprendre quand et où l'utliser.

en: But before talking about this Haskell super-power, we must talk about another
en: essential aspect of Haskell: _Types_.
fr: Mais avant de parler de ce super-pouvoir de Haskell, nous devons parler
fr: d'un autre aspet essentiel: les _Types_.

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/16_Functions.lhs" class="cut">02_Hard_Part/<strong>16_Functions.lhs</strong> </a>

en: <h3 id="types">Types</h3>
fr: <h3 id="types">Les types</h3>

blogimage("salvador-dali-the-madonna-of-port-lligat.jpg","Dali, the madonna of port Lligat")

 > %tldr
 >
 > - `type Name = AnotherType` is just an alias and the compiler doesn't mark any difference between `Name` and `AnotherType`.
 > - `data Name = NameConstructor AnotherType` does mark a difference.
 > - `data` can construct structures which can be recursives.
 > - `deriving` is magic and creates functions for you.

en: In Haskell, types are strong and static.
fr: En Haskell, les types sont forts et statiques.

en: Why is this important? It will help you _greatly_ to avoid mistakes.
en: In Haskell, most bugs are caught during the compilation of your program.
en: And the main reason is because of the type inference during compilation.
en: Type inference makes it easy to detect where you used the wrong parameter at the wrong place, for example.
fr: Pourquoi est-ce important? Cela vous aidera a éviter _beaucoup_ d'erreurs.
fr: En Haskell, la majorité des bugs est repérée durant la compilation de votre programme.
fr: Et la raison principale de cela est l'inférence de type durant la compilation.
fr: L'inférence de type permet de détecter plus facilement lorsque vous utilisez le mauvais paramètre au mauvais endroit, par exemple

en: <h4 id="type-inference">Type inference</h4>
fr: <h4 id="type-inference">Inférence de type</h4>

en: Static typing is generally essential for fast execution.
en: But most statically typed languages are bad at generalizing concepts.
en: Haskell's saving grace is that it can _infer_ types.
fr: Le typage statique est généralement essentiel pour une exécution rapide.
fr: Mais la plupart des langages typés statiquement ont du mal à généraliser des concepts.
fr: La "grâce salvatrice" de Haskell est qu'il peut _inférer_ des types.

en: Here is a simple example, the `square` function in Haskell:
fr: Voici un exemple simple, la fonction `square` en Haskell:

~~~~~~ {.haskell}
square x = x * x
~~~~~~

en: This function can `square` any Numeral type.
en: You can provide `square` with an `Int`, an `Integer`, a `Float` a `Fractional` and even `Complex`. Proof by example:
fr: Cette fonction peut mettre au carré n'importe quel type `Numeral`.
fr: Vous pouvez l'utilser avec un `Int`, un `Integer`, un `Float`, un `Fractional` ou même un `Complex`. Preuve par l'exemple:

~~~
% ghci
GHCi, version 7.0.4:
...
Prelude> let square x = x*x
Prelude> square 2
4
Prelude> square 2.1
4.41
en: Prelude> -- load the Data.Complex module
fr: Prelude> -- charge le module Data.Complex
Prelude> :m Data.Complex
Prelude Data.Complex> square (2 :+ 1)
3.0 :+ 4.0
~~~

en: `x :+ y` is the notation for the complex (<i>x + iy</i>).
fr: `x :+ y` est la notation pour le complexe (<i>x + iy</i>)

en: Now compare with the amount of code necessary in C:
fr: Comparons maintenant avec la quantité de code nécessaire pour le faire en C:

~~~~~~ {.c}
int     int_square(int x) { return x*x; }

float   float_square(float x) {return x*x; }

complex complex_square (complex z) {
    complex tmp;
    tmp.real = z.real * z.real - z.img * z.img;
    tmp.img = 2 * z.img * z.real;
}

complex x,y;
y = complex_square(x);
~~~~~~

en: For each type, you need to write a new function.
en: The only way to work around this problem is to use some meta-programming trick, for example using the pre-processor.
en: In C++ there is a better way, C++ templates:
fr: Pour chaque type, vous avez besoin d'écrire une nouvelle fonction.
fr: Le seul moyen de se débarrasser de ce problème est d'utiliser des astuces de méta-programmation, par exemple en utilisant le pré-processeur.
fr: en C++ il y a un meilleur moyen, les _templates_:

~~~~~~ {.c++}
#include <iostream>
#include <complex>
using namespace std;

template<typename T>
T square(T x)
{
    return x*x;
}

int main() {
    // int
    int sqr_of_five = square(5);
    cout << sqr_of_five << endl;
    // double
    cout << (double)square(5.3) << endl;
    // complex
    cout << square( complex<double>(5,3) )
         << endl;
    return 0;
}
~~~~~~

en: C++ does a far better job than C in this regard.
en: But for more complex functions the syntax can be hard to follow:
en: see [this article](http://bartoszmilewski.com/2009/10/21/what-does-haskell-have-to-do-with-c/) for example.
fr: C++ fait un bien meilleur travail que C ici.
fr: Mais pour des fonctions plus complexes, la syntaxe sera difficile à suivre.
fr: Voyez [cet article](http://bartoszmilewski.com/2009/10/21/what-does-haskell-have-to-do-with-c/) pour quelques exemples. (_NDT: toujours en anglais)

en: In C++ you must declare that a function can work with different types.
en: In Haskell, the opposite is the case.
en: The function will be as general as possible by default.
fr: En C++ vous devez déclarer qu'une fonction peut marcher avec différents types.
fr: En Haskell, c'est le contraire.
fr: La fonction sera aussi générale que possible par défaut.

en: Type inference gives Haskell the feeling of freedom that dynamically typed languages provide.
en: But unlike dynamically typed languages, most errors are caught before run time.
en: Generally, in Haskell:
fr: L'inférence de type donne à Haskell le sentiment de liberté que les langages dynamiquement typés proposent.
fr: Mais contrairement aux langages dynamiquement typés, la majorité des erreurs est détectée avant de lancer le programme.
fr: Généralement, en Haskell:

en:  > "if it compiles it certainly does what you intended"
fr:  > "Si ça compile, ça fait certainement ce que vous attendiez."

<hr/><a href="code/02_Hard_Part/21_Types.lhs" class="cut">02_Hard_Part/<strong>21_Types.lhs</strong></a>

en: <h4 id="type-construction">Type construction</h4>
fr: <h4 id="type-construction">Construction de types</h4>

en: You can construct your own types.
en: First, you can use aliases or type synonyms.
fr: Vous pouvez construire vos propres types.
fr: D'abord, vous pouvez utiliser des alias ou des synonymes de types.

<div class="codehighlight">
~~~~~~ {.haskell}
type Name   = String
type Color  = String

showInfos :: Name ->  Color -> String
showInfos name color =  "Name: " ++ name
                        ++ ", Color: " ++ color
name :: Name
name = "Robin"
color :: Color
color = "Blue"
main = putStrLn $ showInfos name color
~~~~~~
</div>
<a href="code/02_Hard_Part/21_Types.lhs" class="cut">02_Hard_Part/<strong>21_Types.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/22_Types.lhs" class="cut">02_Hard_Part/<strong>22_Types.lhs</strong></a>

en: But it doesn't protect you much.
en: Try to swap the two parameter of `showInfos` and run the program:
fr: Mais cela ne vous protège pas tellement.
fr: Essayez d'inverser les deux paramètres de `showInfos` et lancez le programme:

~~~~~~ {.haskell}
    putStrLn $ showInfos color name
~~~~~~

en: It will compile and execute.
en: In fact you can replace Name, Color and String everywhere.
en: The compiler will treat them as completely identical.
fr: Le code sera compilé et exécuté.
fr: En fait vous pouvez remplace Name, Color et String n'importe où.
fr: Le compilateur les traitera comme si ils était complétement identiques.

en: Another method is to create your own types using the keyword `data`.
fr: Une autre méthode est de créer vos propres type avec le mot-clé `data`.

<div class="codehighlight">
~~~~~~ {.haskell}
data Name   = NameConstr String
data Color  = ColorConstr String

showInfos :: Name ->  Color -> String
showInfos (NameConstr name) (ColorConstr color) =
      "Name: " ++ name ++ ", Color: " ++ color

name  = NameConstr "Robin"
color = ColorConstr "Blue"
main = putStrLn $ showInfos name color
~~~~~~
</div>
en: Now if you switch parameters of `showInfos`, the compiler complains!
en: So this is a potential mistake you will never make again and the only price is to be more verbose.
fr: Maintenant, si vous échangez les paramètres de `showInfos`, le compilateur se plaint!
fr: Au seul prix d'être plus verbeux, vous écartez définitivement cette erreur potentielle.

en: Also notice that constructors are functions:
fr: Remarquez aussi que les constructeurs sont des fonctions :

~~~~~~ {.haskell}
NameConstr  :: String -> Name
ColorConstr :: String -> Color
~~~~~~

en: The syntax of `data` is mainly:
fr: La syntaxe de `data` est principalement:

~~~~~~ {.haskell}
data TypeName =   ConstructorName  [types]
                | ConstructorName2 [types]
                | ...
~~~~~~

en: Generally the usage is to use the same name for the
en: DataTypeName and DataTypeConstructor.
fr: Généralement on utilise le même nom pour le DatatTypeName et le DataTypeConstructor.

en: Example:
fr: Exemple :

~~~~~~ {.haskell}
data Complex a = Num a => Complex a a
~~~~~~

en: Also you can use the record syntax:
fr: Vous pouvez également utiliser cette syntaxe :

~~~~~~ {.haskell}
data DataTypeName = DataConstructor {
                      field1 :: [type of field1]
                    , field2 :: [type of field2]
                    ...
                    , fieldn :: [type of fieldn] }
~~~~~~

en: And many accessors are made for you.
en: Furthermore you can use another order when setting values.
fr: Et de nombreux accesseurs sont définis pour vous.
fr: En outre, vous pouvez utiliser une autre ordre lorsque vous définissez des valeurs.

en: Example:
fr: Exemple :

~~~~~~ {.haskell}
data Complex a = Num a => Complex { real :: a, img :: a}
c = Complex 1.0 2.0
z = Complex { real = 3, img = 4 }
real c ⇒ 1.0
img z ⇒ 4
~~~~~~

<a href="code/02_Hard_Part/22_Types.lhs" class="cut">02_Hard_Part/<strong>22_Types.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/23_Types.lhs" class="cut">02_Hard_Part/<strong>23_Types.lhs</strong></a>

en: <h4 id="recursive-type">Recursive type</h4>
fr: <h4 id="recursive-type">Type récursif</h4>

en: You already encountered a recursive type: lists.
en: You can re-create lists, but with a more verbose syntax:
fr: Nous avons déjà rencontré un type récursif : les listes.
fr: Nous pourrions re-créer les listes, avec une syntaxe plus bavarde:

~~~~~~ {.haskell}
data List a = Empty | Cons a (List a)
~~~~~~

en: If you really want to use an easier syntax you can use an infix name for constructors.
fr: Si vous voulez réellement utiliser une syntxe plus simple, utilisez un nom infixe pour les constructeurs.

~~~~~~ {.haskell}
infixr 5 :::
data List a = Nil | a ::: (List a)
~~~~~~

en: The number after `infixr` gives the precedence.
fr: Le nombre après `infixr` donne la priorité.

en: If you want to be able to print (`Show`), read (`Read`), test equality (`Eq`) and compare (`Ord`) your new data structure you can tell Haskell to derive the appropriate functions for you.
fr: Si vous voulez pouvoir écrire (`Show`), lire (`Read`), tester l'égalité (`Eq`) et comparer (`Ord`) votre nouvelle structure, vous pouvez demander à Haskell de dériver les fonctions appropriées pour vous.

<div class="codehighlight">
~~~~~~ {.haskell}
infixr 5 :::
data List a = Nil | a ::: (List a)
              deriving (Show,Read,Eq,Ord)
~~~~~~
</div>
en: When you add `deriving (Show)` to your data declaration, Haskell creates a `show` function for you.
en: We'll see soon how you can use your own `show` function.
fr: Quand vous ajoutez `deriving (Show)` à votre déclaration, Haskell crée une fonction `show` pour vous.
fr: Nous verrons bientôt comment utiliser sa propre fonction `show`.

<div class="codehighlight">
~~~~~~ {.haskell}
convertList [] = Nil
convertList (x:xs) = x ::: convertList xs
~~~~~~
</div>
<div class="codehighlight">
~~~~~~ {.haskell}
main = do
      print (0 ::: 1 ::: Nil)
      print (convertList [0,1])
~~~~~~
</div>
en: This prints:
fr: Ceci donne :

~~~
0 ::: (1 ::: Nil)
0 ::: (1 ::: Nil)
~~~

<a href="code/02_Hard_Part/23_Types.lhs" class="cut">02_Hard_Part/<strong>23_Types.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/30_Trees.lhs" class="cut">02_Hard_Part/<strong>30_Trees.lhs</strong></a>

en: <h4 id="trees">Trees</h4>
fr: <h4 id="trees">Les arbres</h4>

blogimage("magritte-l-arbre.jpg","Magritte, l'Arbre")

en: We'll just give another standard example: binary trees.
fr: Voici une autre exemple standard : les arbres binaires.

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.List

data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                              deriving (Show)
~~~~~~
</div>
en: We will also create a function which turns a list into an ordered binary tree.
fr: Créons aussi une fonctions qui transforme une liste en un arbre binaire ordonné.

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
~~~~~~
</div>
en: Look at how elegant this function is.
en: In plain English:
fr: Remarquez à quel point cette fonction est élégante.
fr: En français :

en: - an empty list will be converted to an empty tree.
en: - a list `(x:xs)` will be converted to a tree where:
en:   - The root is `x`
en:   - Its left subtree is the tree created from members of the list `xs` which are strictly inferior to `x` and
en:   - the right subtree is the tree created from members of the list `xs` which are strictly superior to `x`.
fr: - une liste vide est convertie en un arbre vide
fr: - une liste `(x:xs)` sera convertie en un arbre où :
fr:   - La racine est `x`
fr:   - Le "sous-arbre" de gauche est l'arbre créé à partir des membres de la liste `xs` strictement inférieurs à `x`
fr:   - Le "sous-arbre" de droite est l'arbre créé à partir des membres de la liste `xs` strictement superieurs à `x`

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ treeFromList [7,2,4,8]
~~~~~~
</div>
en: You should obtain the following:
fr: Vous devriez obtenir :

~~~
Node 7 (Node 2 Empty (Node 4 Empty Empty)) (Node 8 Empty Empty)
~~~

en: This is an informative but quite unpleasant representation of our tree.
fr: C'est une représentation de notre arbre informative mais plutôt déplaisante.

<a href="code/02_Hard_Part/30_Trees.lhs" class="cut">02_Hard_Part/<strong>30_Trees.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/31_Trees.lhs" class="cut">02_Hard_Part/<strong>31_Trees.lhs</strong></a>

en: Just for fun, let's code a better display for our trees.
en: I simply had fun making a nice function to display trees in a general way.
en: You can safely skip this part if you find it too difficult to follow.
fr: Juste pour le plaisir, codons un meilleur affichage pour nos arbres.
fr: Je me suis simplement amusé à faire une belle fonction pour afficher les arbres de façon générale.
fr: Vous pouvez passer cette partie si vous la trouvez difficile à suivre.

en: We have a few changes to make.
en: We remove the `deriving (Show)` from the declaration of our `BinTree` type.
en: And it might also be useful to make our BinTree an instance of (`Eq` and `Ord`) so we will be able to test equality and compare trees.
fr: Nous avons quelques changements à faire.
fr: Enlevons le `deriving (Show)` de la déclaration de notre type `BinTree`.
fr: Il serait aussi utile de faire de BinTree une instance de (`Eq` et `Ord`), nous serons ainsi capable de tester l'égalité et de comparer des arbres.

<div class="codehighlight">
~~~~~~ {.haskell}
data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                  deriving (Eq,Ord)
~~~~~~
</div>
en: Without the `deriving (Show)`, Haskell doesn't create a `show` method for us.
en: We will create our own version of `show`.
en: To achieve this, we must declare that our newly created type `BinTree a`
en: is an instance of the type class `Show`.
en: The general syntax is:
fr: Sans le `deriving (Show)`, Haskell ne crée pas de méthode `show` pour nous.
fr: Nous allons créer notre propre version.
fr: Pour accomplir cela, nous devons déclarer que notre type `BinTree a`
fr: est une instance de la classe de type `Show`.
fr: La syntaxe générale est :

~~~~~~ {.haskell}
instance Show (BinTree a) where
en:    show t = ... -- You declare your function here
fr:    show t = ... -- Déclarez votre fonction ici
~~~~~~

en: Here is my version of how to show a binary tree.
en: Don't worry about the apparent complexity.
en: I made a lot of improvements in order to display even stranger objects.
fr: Voici ma version pour afficher un arbre binaire.
fr: Ne vous inquiétez pas de sa complexité apparente.
fr: J'ai fait beaucoup d'améliorations pour afficher même les objets les plus étranges.

<div class="codehighlight">
~~~~~~ {.haskell}
-- declare BinTree a to be an instance of Show
instance (Show a) => Show (BinTree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    -- treeshow pref Tree
    --   shows a tree and starts each line with pref
    -- We don't display the Empty tree
    treeshow pref Empty = ""
    -- Leaf
    treeshow pref (Node x Empty Empty) =
                  (pshow pref x)

    -- Right branch is empty
    treeshow pref (Node x left Empty) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    -- Left branch is empty
    treeshow pref (Node x Empty right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- Tree with left and right children non empty
    treeshow pref (Node x left right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- shows a tree using some prefixes to make it nice
    showSon pref before next t =
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replaces "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (show x)

    -- replaces one char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x
              | x == c = new
              | otherwise = x:[] -- "x"
~~~~~~
</div>
en: The `treeFromList` method remains identical.
fr: La méthode `treeFromList` reste identique.

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
~~~~~~
</div>
en: And now, we can play:
fr: Et maintenant, nous pouvons jouer :

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
  putStrLn "Int binary tree:"
  print $ treeFromList [7,2,4,8,1,3,6,21,12,23]
~~~~~~
</div>
~~~
en: Int binary tree:
fr: Arbre binaire d'Int:
< 7
: |--2
: |  |--1
: |  `--4
: |     |--3
: |     `--6
: `--8
:    `--21
:       |--12
:       `--23
~~~

en: Now it is far better!
en: The root is shown by starting the line with the `<` character.
en: And each following line starts with a `:`.
en: But we could also use another type.
fr: Maintenant c'est beaucoup mieux !
fr: La racine est montrée en commençant la ligne avec le caractère `<`.
fr: Et chaque ligne suivante est commence par `:`.
fr: Mais nous pourrions aussi utiliser un autre type.

<div class="codehighlight">
~~~~~~ {.haskell}
  putStrLn "\nString binary tree:"
  print $ treeFromList ["foo","bar","baz","gor","yog"]
~~~~~~
</div>
~~~
en: String binary tree:
fr: Arbre binaire de chaînes de caractères
< "foo"
: |--"bar"
: |  `--"baz"
: `--"gor"
:    `--"yog"
~~~

en: As we can test equality and order trees, we can
en: make tree of trees!
fr: Commme nous pouvons tester l'égalité et ordonner des arbres,
fr: nous pouvons aussi faire des arbres d'arbres!

<div class="codehighlight">
~~~~~~ {.haskell}
  putStrLn "\nBinary tree of Char binary trees:"
  print ( treeFromList
           (map treeFromList ["baz","zara","bar"]))
~~~~~~
</div>
~~~
en: Binary tree of Char binary trees:
fr: Arbre binaire d'arbres binaires de Char :
< < 'b'
: : |--'a'
: : `--'z'
: |--< 'b'
: |  : |--'a'
: |  : `--'r'
: `--< 'z'
:    : `--'a'
:    :    `--'r'
~~~

en: This is why I chose to prefix each line of tree display by `:` (except for the root).
fr: C'est pour cela que j'ai choisi de préfixer chaque ligne par un `:` (sauf pour la racine).

blogimage("yo_dawg_tree.jpg","Yo Dawg Tree")

<div class="codehighlight">
~~~~~~ {.haskell}
  putStrLn "\nTree of Binary trees of Char binary trees:"
  print $ (treeFromList . map (treeFromList . map treeFromList))
             [ ["YO","DAWG"]
             , ["I","HEARD"]
             , ["I","HEARD"]
             , ["YOU","LIKE","TREES"] ]
~~~~~~
</div>
en: Which is equivalent to
fr: Qui est équivalent à

~~~~~~ {.haskell}
print ( treeFromList (
          map treeFromList
             [ map treeFromList ["YO","DAWG"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["YOU","LIKE","TREES"] ]))
~~~~~~

en: and gives:
fr: et donne :

~~~
en: Binary tree of Binary trees of Char binary trees:
fr: Arbre d'arbres d'arbres de Char :
< < < 'Y'
: : : `--'O'
: : `--< 'D'
: :    : |--'A'
: :    : `--'W'
: :    :    `--'G'
: |--< < 'I'
: |  : `--< 'H'
: |  :    : |--'E'
: |  :    : |  `--'A'
: |  :    : |     `--'D'
: |  :    : `--'R'
: `--< < 'Y'
:    : : `--'O'
:    : :    `--'U'
:    : `--< 'L'
:    :    : `--'I'
:    :    :    |--'E'
:    :    :    `--'K'
:    :    `--< 'T'
:    :       : `--'R'
:    :       :    |--'E'
:    :       :    `--'S'
~~~

en: Notice how duplicate trees aren't inserted;
en: there is only one tree corresponding to `"I","HEARD"`.
en: We have this for (almost) free, because we have declared Tree to be an instance of `Eq`.
fr: Remarquez que les arbres en double ne sont pas insérés.
fr: Il n'y a qu'un seul arbre correspondant à `"I","HEARD"`.
fr: Nous avons ceci presque gratuitement, car nous avons déclaré Tree comme instance de `Eq`.

en: See how awesome this structure is:
en: We can make trees containing not only integers, strings and chars, but also other trees.
en: And we can even make a tree containing a tree of trees!
fr: Voyez à quel point cette structure est formidable :
fr: Nous pouvons faire des arbres contenant seulement des entiers, des chaînes de caractères, mais aussi d'autres arbres.
fr: Et nous pouvons même faire un arbre contenant un arbre d'arbres!

<a href="code/02_Hard_Part/31_Trees.lhs" class="cut">02_Hard_Part/<strong>31_Trees.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/40_Infinites_Structures.lhs" class="cut">02_Hard_Part/<strong>40_Infinites_Structures.lhs</strong></a>

en: <h3 id="infinite-structures">Infinite Structures</h3>
fr: <h3 id="infinite-structures">Structures infinies</h3>

blogimage("escher_infinite_lizards.jpg","Escher")

en: It is often said that Haskell is _lazy_.
fr: On dit souvent que Haskell est _paresseux_.

en: In fact, if you are a bit pedantic, you should say that [Haskell is _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict).
en: Laziness is just a common implementation for non-strict languages.
fr: En fait, si vous êtes un petit peu pédant, vous devriez dire que [Haskell est _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict) (_NDT: En anglais, pour changer_).
fr: La paresse est juste une implémentation commune aux langages non-stricts.

en: Then what does "not-strict" mean? From the Haskell wiki:
fr: Alors que signifie "non-strict"? D'après le wiki de Haskell :

en:  > Reduction (the mathematical term for evaluation) proceeds from the outside in.
en:  >
en:  > so if you have `(a+(b*c))` then you first reduce `+` first, then you reduce the inner `(b*c)`
fr:  > La réduction (terme mathématique pour "évaluation") procède depuis l'extérieur.
fr:  >
fr:  > Donc si vous avez `(a+(b*c))`, alors vous réduisez `+` d'abord, puis vous réduisez `(b*c)`

en: For example in Haskell you can do:
fr: Par exemple en Haskell vous pouvez faire :

<div class="codehighlight">
~~~~~~ {.haskell}
-- numbers = [1,2,..]
numbers :: [Integer]
numbers = 0:map (1+) numbers

take' n [] = []
take' 0 l = []
take' n (x:xs) = x:take' (n-1) xs

main = print $ take' 10 numbers
~~~~~~
</div>
en: And it stops.
fr: Et ça s'arrête.

en: How?
fr: Comment ?

en: Instead of trying to evaluate `numbers` entirely,
en: it evaluates elements only when needed.
fr: Au lieu d'essayer d'évaluer `numbers` entièrement,
fr: Haskell évalue les éléments seulement lorsque c'est nécessaire.

en: Also, note in Haskell there is a notation for infinite lists
fr: Remarquez aussi qu'en Haskell, il y a une notation pour les listes infinies

~~~
[1..]   ⇔ [1,2,3,4...]
[1,3..] ⇔ [1,3,5,7,9,11...]
~~~

en: and most functions will work with them.
en: Also, there is a built-in function `take` which is equivalent to our `take'`.
fr: et que la majorité des fonctions fonctionnera avec ces listes.
fr: Il y a aussi une fonction `take` équivalente à notre `take'`.

<a href="code/02_Hard_Part/40_Infinites_Structures.lhs" class="cut">02_Hard_Part/<strong>40_Infinites_Structures.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/41_Infinites_Structures.lhs" class="cut">02_Hard_Part/<strong>41_Infinites_Structures.lhs</strong></a>

<div style="display:none">

This code is mostly the same as the previous one.

<div class="codehighlight">
~~~~~~ {.haskell}
import Debug.Trace (trace)
import Data.List
data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                  deriving (Eq,Ord)
~~~~~~
</div>
<div class="codehighlight">
~~~~~~ {.haskell}
-- declare BinTree a to be an instance of Show
instance (Show a) => Show (BinTree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    treeshow pref Empty = ""
    treeshow pref (Node x Empty Empty) =
                  (pshow pref x)

    treeshow pref (Node x left Empty) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    treeshow pref (Node x Empty right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    treeshow pref (Node x left right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- show a tree using some prefixes to make it nice
    showSon pref before next t =
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replace "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (" " ++ show x)

    -- replace on char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x
              | x == c = new
              | otherwise = x:[] -- "x"

~~~~~~
</div>
</div>

en: Suppose we don't mind having an ordered binary tree.
en: Here is an infinite binary tree:
fr: Supposons que nous ne nous préoccupions pas d'avoir une arbre ordonné.
fr: Voici un arbre binaire infini :

<div class="codehighlight">
~~~~~~ {.haskell}
nullTree = Node 0 nullTree nullTree
~~~~~~
</div>
en: A complete binary tree where each node is equal to 0.
en: Now I will prove you can manipulate this object using the following function:
fr: Un arbre complet où chaque noeud est égal à 0.
fr: Maintenant je vais vous prouver que nous pouvons manipuler cet arbre avec la fonction suivante :

<div class="codehighlight">
~~~~~~ {.haskell}
-- take all element of a BinTree
-- up to some depth
treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) = let
          nl = treeTakeDepth (n-1) left
          nr = treeTakeDepth (n-1) right
          in
              Node x nl nr
~~~~~~
</div>
en: See what occurs for this program:
fr: Regardez ce qui se passe avec ce programme :

~~~~~~ {.haskell}
main = print $ treeTakeDepth 4 nullTree
~~~~~~

en: This code compiles, runs and stops giving the following result:
fr: Le code compile, se lance et s'arrête en donnant ce résultat :

~~~
<  0
: |-- 0
: |  |-- 0
: |  |  |-- 0
: |  |  `-- 0
: |  `-- 0
: |     |-- 0
: |     `-- 0
: `-- 0
:    |-- 0
:    |  |-- 0
:    |  `-- 0
:    `-- 0
:       |-- 0
:       `-- 0
~~~

en: Just to heat up your neurones a bit more,
en: let's make a slightly more interesting tree:
fr: Pour nous chauffer encore un peu les neurones,
fr: faisons un arbre plus intéressant :

<div class="codehighlight">
~~~~~~ {.haskell}
iTree = Node 0 (dec iTree) (inc iTree)
        where
           dec (Node x l r) = Node (x-1) (dec l) (dec r)
           inc (Node x l r) = Node (x+1) (inc l) (inc r)
~~~~~~
</div>
en: Another way to create this tree is to use a higher order function.
en: This function should be similar to `map`, but should work on `BinTree` instead of list.
en: Here is such a function:
fr: Un autre moyen de créer cet arbre est d'utiliser une fonction d'ordre supérieur.
fr: Cette fonction devrait être similaire à `map` n, mais devrait travailler sur un `BinTree` au lieu d'une liste.
fr: Voici cette fonction :

<div class="codehighlight">
~~~~~~ {.haskell}
-- apply a function to each node of Tree
treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f Empty = Empty
treeMap f (Node x left right) = Node (f x)
                                     (treeMap f left)
                                     (treeMap f right)
~~~~~~
</div>
en: _Hint_: I won't talk more about this here.
en: If you are interested in the generalization of `map` to other data structures,
en: search for functor and `fmap`.
fr: _NB_: Je ne parlerai pas plus de cette fonction ici.
fr: Si vous vous intéressez à la généralisation de `map` à d'autres structures de données,
fr: cherchez des informations sur les foncteurs et `fmap`.

en: Our definition is now:
fr: Notre définition est maintenant :

<div class="codehighlight">
~~~~~~ {.haskell}
infTreeTwo :: BinTree Int
infTreeTwo = Node 0 (treeMap (\x -> x-1) infTreeTwo)
                    (treeMap (\x -> x+1) infTreeTwo)
~~~~~~
</div>
en: Look at the result for
fr: Regardez le résultat pour

~~~~~~ {.haskell}
main = print $ treeTakeDepth 4 infTreeTwo
~~~~~~

~~~
<  0
: |-- -1
: |  |-- -2
: |  |  |-- -3
: |  |  `-- -1
: |  `-- 0
: |     |-- -1
: |     `-- 1
: `-- 1
:    |-- 0
:    |  |-- -1
:    |  `-- 1
:    `-- 2
:       |-- 1
:       `-- 3
~~~

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
  print $ treeTakeDepth 4 nullTree
  print $ treeTakeDepth 4 infTreeTwo
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/41_Infinites_Structures.lhs" class="cut">02_Hard_Part/<strong>41_Infinites_Structures.lhs</strong> </a>

en: <h2 id="hell-difficulty-part">Hell Difficulty Part</h2>
fr: <h2 id="hell-difficulty-part">Partie de difficulté infernale</h2>

en: Congratulations for getting so far!
en: Now, some of the really hardcore stuff can start.
fr: Félicitations pour être allé si loin!
fr: Maitenant, les choses vraiment extrêmes peuvent commencer.

en: If you are like me, you should get the functional style.
en: You should also understand a bit more the advantages of laziness by default.
en: But you also don't really understand where to start in order to make a real
en: program.
en: And in particular:
fr: Si vous êtes comme moi, vous êtes déjà familier avec le style fonctionnel.
fr: Vous devriez également comprendre les avantages de la paresse par défaut.
fr: Mais vous ne comprenez peut-être pas vraiment par où commencer pour faire un vrai
fr: programme.
fr: Et en particulier :

en: - How do you deal with effects?
en: - Why is there a strange imperative-like notation for dealing with IO?
fr: - Comment s'occuper des effets ?
fr: - Pourquoi y a t-il une étrange notation impérative lorsque l'on s'occupe de l'Entrée/Sortie? (E/S, _IO_ pour _Input/Output_ en anglais)

en: Be prepared, the answers might be complex.
en: But they are all very rewarding.
fr: Accrochez-vous, les réponses risquent d'être compliquées.
fr: Mais elles en valent la peine.

<hr/><a href="code/03_Hell/01_IO/01_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>01_progressive_io_example.lhs</strong></a>

en: <h3 id="deal-with-io">Deal With IO</h3>
fr: <h3 id="deal-with-io">S'occuper de l'E/S (IO)</h3>

blogimage("magritte_carte_blanche.jpg","Magritte, Carte blanche")

 > %tldr
 >
en:  > A typical function doing `IO` looks a lot like an imperative program:
fr:  > Une fonction typique qui fait de l'`IO` ressemble à un programme impératif:
 >
 > ~~~
 > f :: IO a
 > f = do
 >   x <- action1
 >   action2 x
 >   y <- action3
 >   action4 x y
 > ~~~
 >
en:  > - To set a value to an object we use `<-` .
en:  > - The type of each line is `IO *`;
en:  >   in this example:
fr:  > - Pour définir la valeur d'un objet on utilise `<-` .
fr:  > - Le type de chaque ligne est `IO *`;
fr:  >   dans cet exemple:
 >   - `action1     :: IO b`
 >   - `action2 x   :: IO ()`
 >   - `action3     :: IO c`
 >   - `action4 x y :: IO a`
 >   - `x :: b`, `y :: c`
en:  > - Few objects have the type `IO a`, this should help you choose.
en:  >   In particular you cannot use pure functions directly here.
en:  >   To use pure functions you could do `action2 (purefunction x)` for example.
fr:  > - Quelques objets ont le type `IO a`, cela devrait vous aider à choisir.
fr:  >   En particulier vous ne pouvez pas utiliser de fonctions pures directement ici.
fr:  >   Pour utiliser des fonctions pures vous pourriez faire `action2 (pureFunction x)` par exemple.

en: In this section, I will explain how to use IO, not how it works.
en: You'll see how Haskell separates the pure from the impure parts of the program.
fr: Dans cette section, je vais expliquer comment utiliser l'IO, pas comment ça marche.
fr: Vous verrez comment Haskell sépare les parties pures et impures du programme.

en: Don't stop because you're trying to understand the details of the syntax.
en: Answers will come in the next section.
fr: Ne vous arrêtez pas sur les détails de la syntaxe
fr: Les réponses viendront dans la section suivante.

en: What to achieve?
fr: Que cherchons-nous à faire?

en:  > Ask a user to enter a list of numbers.
en:  > Print the sum of the numbers
fr:  > Demander une liste de nombres à l'utilisateur.
fr:  > Afficher la somme de ces nombres.

<div class="codehighlight">
~~~~~~ {.haskell}
toList :: String -> [Integer]
toList input = read ("[" ++ input ++ "]")

main = do
  putStrLn "Enter a list of numbers (separated by comma):"
  input <- getLine
  print $ sum (toList input)
~~~~~~
</div>
en: It should be straightforward to understand the behavior of this program.
en: Let's analyze the types in more detail.
fr: Il devrait être simple de comprendre le comportement de ce programme.
fr: Analysons les types en détails.

~~~
putStrLn :: String -> IO ()
getLine  :: IO String
print    :: Show a => a -> IO ()
~~~

en: Or more interestingly, we note that each expression in the `do` block has a type of `IO a`.
fr: Ou, de manièree plus intéressante, on remarque que chaque expression dans le bloc `do` est de type `IO a`.

<pre>
main = do
  putStrLn "Enter ... " :: <span class="high">IO ()</span>
  getLine               :: <span class="high">IO String</span>
  print Something       :: <span class="high">IO ()</span>
</pre>

en: We should also pay attention to the effect of the `<-` symbol.
fr: Nous devrions aussi prêter attention à l'effet du symbole `<-`.

~~~
do
 x <- something
~~~

en: If `something :: IO a` then `x :: a`.
fr: Si `something :: IO a` alors `x :: a`.

en: Another important note about using `IO`:
en: All lines in a do block must be of one of the two forms:
fr: Une autre remarque importante sur l'`IO`:
fr: Toutes les lignes d'un bloc `do` doivent être d'une des deux formes suivantes :

~~~
action1             :: IO a
                    -- in this case, generally a = ()
~~~

ou

~~~
value <- action2    -- where
                    -- action2 :: IO b
                    -- value   :: b
~~~

en: These two kinds of line will correspond to two different ways of sequencing actions.
en: The meaning of this sentence should be clearer by the end of the next section.
fr: Ces deux types de ligne correspondent à deux types différents de séquençage d'action.
fr: La signification de cette phrase devrait être plus claire à la fin de la prochaine section.

<a href="code/03_Hell/01_IO/01_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>01_progressive_io_example.lhs</strong> </a>

<hr/><a href="code/03_Hell/01_IO/02_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>02_progressive_io_example.lhs</strong></a>

en: Now let's see how this program behaves.
en: For example, what happens if the user enters something strange?
en: Let's try:
fr: Maintenant voyons comment ce programme se comporte.
fr: Par exemple, que ce passe-t-il si l'utilisateur entre une mauvaise valeur?
fr: Essayons :

~~~
    % runghc 02_progressive_io_example.lhs
    Enter a list of numbers (separated by comma):
    foo
    Prelude.read: no parse
~~~

en: Argh! An evil error message and a crash!
fr: Argh! Un message d'erreur effrayant et un crash !
en: Our first improvement will simply be to answer with a more friendly message.
fr: Notre première amélioration sera de répondre avec un message plus amical.

en: In order to do this, we must detect that something went wrong.
fr: Pour faire cela, nous devons détecter que quelque chose s'est mal passé.
en: Here is one way to do this: use the type `Maybe`.
fr: Voici un moyen de le faire : utiliser le type `Maybe`.
en: This is a very common type in Haskell.
fr: C'est un type très utilisé en Haskell.

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.Maybe
~~~~~~
</div>
en: What is this thing? `Maybe` is a type which takes one parameter.
fr: Mais qu'est-ce que c'est ? `Maybe` est un type qui prend un paramètre.
en: Its definition is:
fr: Sa définition est :

~~~~~~ {.haskell}
data Maybe a = Nothing | Just a
~~~~~~

fr: C'est un bon moyen de dire qu'il y a eu une erreur en essayant de créer/évaluer
en: This is a nice way to tell there was an error while trying to create/compute
fr: une valeur.
en: a value.
fr: La fonction `maybeRead` en est un bon exemple.
en: The `maybeRead` function is a great example of this.
fr: C'est une fonction similaire à `read`[^1],
en: This is a function similar to the function `read`[^1],
fr: mais s'il y a un problème, la valeur retournée est `Nothing`.
en: but if something goes wrong the returned value is `Nothing`.
fr: Si la valeur est bonne, la valeur retournée est `Just <la valeur>`.
en: If the value is right, it returns `Just <the value>`.
fr: Ne vous efforcez pas trop de comprendre cette fonction.
en: Don't try to understand too much of this function.
fr: J'utilise une fonction de plus bas niveau que `read` : `reads`.
en: I use a lower level function than `read`: `reads`.

fr: [^1]: Qui est elle-même très similaire à la fonction `eval` de javascript, appliquée sur une chaîne contenant du code au format JSON.
en: [^1]: Which is itself very similar to the javascript `eval` function, that is applied to a string containing JSON.

<div class="codehighlight">
~~~~~~ {.haskell}
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing
~~~~~~
</div>
fr: Maintenant, pour être plus lisible, on définit une fonction comme ceci :
en: Now to be a bit more readable, we define a function which goes like this:
fr: Si la chaîne a un mauvais format, elle retournera `Nothing`.
en: If the string has the wrong format, it will return `Nothing`.
fr: Sinon, par exemple pour "1,2,3", cela retournera `Just [1,2,3]`.
en: Otherwise, for example for "1,2,3", it will return `Just [1,2,3]`.

<div class="codehighlight">
~~~~~~ {.haskell}
getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"
~~~~~~
</div>
en: We simply have to test the value in our main function.
fr: Nous avons juste à tester la valeur dans notre fonction principale.

<div class="codehighlight">
~~~~~~ {.haskell}
main :: IO ()
main = do
  putStrLn "Enter a list of numbers (separated by comma):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> print (sum l)
          Nothing -> error "Bad format. Good Bye."
~~~~~~
</div>
fr: En cas d'erreur, on affiche un joli message.
en: In case of error, we display a nice error message.

fr: Notez que le type de chaque expression dans le bloc `do` de `main` reste de la forme `IO a`.
en: Note that the type of each expression in the main's `do` block remains of the form `IO a`.
fr: La seule construction étrange est `error`.
en: The only strange construction is `error`.
fr: Disons juste que `error msg` prend le type nécessaire (ici, `IO ()`).
en: I'll just say here that `error msg` takes the needed type (here `IO ()`).

fr: Une chose très importante à noter est le type de toutes les fonctions définies jusqu'ici.
en: One very important thing to note is the type of all the functions defined so far.
fr: Il n'y a qu'une seule fonction qui contient `IO` dans son type : `main`.
en: There is only one function which contains `IO` in its type: `main`.
fr: Cela signifie que `main` est impure.
en: This means main is impure.
fr: Mais `main` utilise `getListFromString`, qui, elle, est pure.
en: But main uses `getListFromString` which is pure.
fr: Nous pouvons donc facilement repérer quelles fonctions sont pures
en: So it's clear just by looking at declared types which functions are pure and
fr: et lesquelles sont impures, seulement en regardant leur type.
en: which are impure.

fr: Pourquoi la pureté a-t-elle de l'importance?
en: Why does purity matter?
fr: Parmi ses nombreux avantages, en voici trois :
en: Among the many advantages, here are three:

fr: - Il est beaucoup plus facile de penser à du code pur qu'à du code impur.
en: - It is far easier to think about pure code than impure code.
fr: - La pureté vous protège de tous les bugs difficiles à reproduire dûs aux [effets de bord](https://fr.wikipedia.org/wiki/Effet_de_bord_(informatique)).
en: - Purity protects you from all the hard-to-reproduce bugs that are due to side effects.
fr: - Vous pouvez évaluer des fonctions pures dans n'importe quel ordre ou en parallèle, sans prendre de risques.
en: - You can evaluate pure functions in any order or in parallel without risk.

fr: C'est pourquoi vous devriez mettre le plus de code possible dans des fonctions pures.
en: This is why you should generally put as most code as possible inside pure functions.

<a href="code/03_Hell/01_IO/02_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>02_progressive_io_example.lhs</strong> </a>

<hr/><a href="code/03_Hell/01_IO/03_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>03_progressive_io_example.lhs</strong></a>

fr: La prochaine étape sera de demander la liste de nombres à l'utilisateur encore et encore jusqu'à ce qu'il entre une réponse valide.
en: Our next iteration will be to prompt the user again and again until she enters a valid answer.

fr: Nous gardons la première partie :
en: We keep the first part:

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing
getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"
~~~~~~
</div>
fr: Maintenant nous créons la fonction qui demandera une liste d'entiers à l'utilisateur
en: Now we create a function which will ask the user for an list of integers
fr: jusqu'à ce que l'entrée soit correcte
en: until the input is right.

<div class="codehighlight">
~~~~~~ {.haskell}
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by comma):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser
~~~~~~
</div>
fr: Cette fonction est de type `IO [Integer]`.
en: This function is of type `IO [Integer]`.
fr: Cela signifie que la valeur récupérée est de type `[Integer`] et est le résultat d'actions d'E/S.
en: Such a type means that we retrieved a value of type `[Integer]` through some IO actions.
fr: D'aucuns diront avec enthousiasme :
en: Some people might explain while waving their hands:

fr:  > «C'est un `[Integer]` dans un `IO` !»
en:  > «This is an `[Integer]` inside an `IO`»

fr: Si vous voulez comprendre les détails derrière tout cela, vous devrez lire la prochaine section.
en: If you want to understand the details behind all of this, you'll have to read the next section.
fr: Mais si vous voulez seulement _utiliser_ l'E/S, contentez-vous pratiquer un peu et rappelez-vous de penser aux types.
en: But really, if you just want to _use_ IO just practice a little and remember to think about the type.

fr: Finalement, notre fonction `main`est bien plus simple :
en: Finally our main function is much simpler:

<div class="codehighlight">
~~~~~~ {.haskell}
main :: IO ()
main = do
  list <- askUser
  print $ sum list
~~~~~~
</div>
fr: Nous avons fini notre introduction à l'`IO`.
en: We have finished with our introduction to `IO`.
fr: C'était plutôt rapide. Voici les principales choses à retenir :
en: This was quite fast. Here are the main things to remember:

fr: - Dans le bloc `do`, chaque expression doit avoir le type `IO a`.
en: - in the `do` block, each expression must have the type `IO a`.
fr: Vous êtes donc limité quant au panel d'expression disponibles.
en: You are then limited with regard to the range of expressions available.
fr: Par exemple, `getLine`, `print`, `putStrLn`, etc...
en: For example, `getLine`, `print`, `putStrLn`, etc...
fr: - Essayez d'externaliser le plus possible les fonctions pures.
en: - Try to externalize the pure functions as much as possible.
fr: - le type `IO a` signifie : une _action_ d'E/S qui retourne un élément de type a.
en: - the `IO a` type means: an IO _action_ which returns an element of type `a`.
fr: L'`IO` représente des actions; sous le capot, `IO a` est le type d'une fonction.
en: `IO` represents actions; under the hood, `IO a` is the type of a function.
fr: Lisez la prochaine section si vous êtes curieux.
en: Read the next section if you are curious.

fr: Si vous pratiquez un peu, vous devriez être capable d'_utiliser_ l'`IO`.
en: If you practice a bit, you should be able to _use_ `IO`.

fr:  > -Exercices_:
en:  > _Exercises_:
 >
fr:  > - Écrivez un programme qui additionne tous ses arguments. Utilisez la fonction `getArgs`.
en:  > - Make a program that sums all of its arguments. Hint: use the function `getArgs`.

<a href="code/03_Hell/01_IO/03_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>03_progressive_io_example.lhs</strong> </a>

fr: <h3 id="io-trick-explained">Le truc des IO révélé</h3>
en: <h3 id="io-trick-explained">IO trick explained</h3>

blogimage("magritte_pipe.jpg","Magritte, ceci n'est pas une pipe")

fr:  > Voici un %tlal pour cette section.
en:  > Here is a %tldr for this section.
 >
fr:  > Pour séparer les parties pures et impures,
en:  > To separate pure and impure parts,
fr:  > `main` est définie comme une fonction.
en:  > `main` is defined as a function
fr:  > qui modifie l'état du monde.
en:  > which modifies the state of the world.
 >
 > ~~~
 > main :: World -> World
 > ~~~
 >
fr:  > Une fonction aura des effets de bord si elle a ce type.
en:  > A function is guaranteed to have side effects only if it has this type.
fr:  > Mais regardez cette fonction `main` typique:
en:  > But look at a typical main function:
 >
 > ~~~
 > 
 > main w0 =
 >     let (v1,w1) = action1 w0 in
 >     let (v2,w2) = action2 v1 w1 in
 >     let (v3,w3) = action3 v2 w2 in
 >     action4 v3 w3
 > ~~~
 >
fr:  > Nous avons beaucoup d'élements temporaires (ici, `w1`, `w2` et `w3`) 
en:  > We have a lot of temporary elements (here `w1`, `w2` and `w3`)
fr:  > qui doivent être passés à l'action suivante.
en:  > which must be passed on to the next action.
 >
fr:  > Nous créons une fonction `bind` ou `(>>=)`.
en:  > We create a function `bind` or `(>>=)`.
fr:  > Avec `bind` nous n'avons plus besoin de noms temporaires.
en:  > With `bind` we don't need temporary names anymore.
 >
 > ~~~
 > main =
 >   action1 >>= action2 >>= action3 >>= action4
 > ~~~
 >
fr:  > Bonus: Haskell a du sucre syntaxique :
en:  > Bonus: Haskell has syntactical sugar for us:
 >
 > ~~~
 > main = do
 >   v1 <- action1
 >   v2 <- action2 v1
 >   v3 <- action3 v2
 >   action4 v3
 > ~~~

fr: Pourquoi avons-nous utilisé cette syntaxe étrange, et quel est exactement le type `IO`?
en: Why did we use this strange syntax, and what exactly is this `IO` type?
fr: Cela peut sembler un peu magique.
en: It looks a bit like magic.

fr: Pour l'instant, oublions les parties pures de notre programme, et concentrons-nous
en: For now let's just forget all about the pure parts of our program, and focus
fr: sur les parties impures:
en: on the impure parts:

~~~~~~ {.haskell}
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by commas):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser

main :: IO ()
main = do
  list <- askUser
  print $ sum list
~~~~~~

fr: Première remarque : on dirait de l'impératif.
en: First remark: this looks imperative.
fr: Haskell est assez puissant pour faire sembler impératif du code impur.
en: Haskell is powerful enough to make impure code look imperative.
fr: Par exemple, si vous le vouliez vous pourriez créer une boucle `while` en Haskell.
en: For example, if you wish you could create a `while` in Haskell.
fr: En fait, pour utiliser les `IO`, le style impératif est en général plus approprié.
en: In fact, for dealing with `IO`, an imperative style is generally more appropriate.

fr: Mais vous devriez avoir remarqué que la notation est inhabituelle.
en: But you should have noticed that the notation is a bit unusual.
fr: Voici pourquoi, en détail.
en: Here is why, in detail.

fr: Dans un langage impur, l'état du monde peut être vu comme une énorme variable globale cachée.
en: In an impure language, the state of the world can be seen as a huge hidden global variable.
fr: Cette variable cachée est accessible par toutes les fonctions du langage.
en: This hidden variable is accessible by all functions of your language.
fr: Par exemple, vous pouvez lire et écrire dans un fichier avec n'importe quelle fonction.
en: For example, you can read and write a file in any function.
fr: Le fait que le fichier putatif existe ou non est une éventualité qui relève des états possibles que le monde courant peut prendre.
en: Whether a file exists or not is a difference in the possible states that the world can take.

fr: En Haskell l'état courant du monde n'est pas caché.
en: In Haskell the current state of the world is not hidden.
fr: Au contraire, il est dit _explicitement_ que `main` est une fonction qui change _potentiellement_ l'état du monde.
en: Rather, it is _explicitly_ said that `main` is a function that _potentially_ changes the state of the world.
fr: Son type est donc quelque chose comme :
en: Its type is then something like:

~~~~~~ {.haskell}
main :: World -> World
~~~~~~

fr: Les fonctions ne sont pas toutes susceptibles de modifier cette variable.
en: Not all functions may access this variable.
fr: Celle qui peuvent la modifier sont impures.
en: Those which have access to this variable are impure.
fr: Les fonctions qui ne peuvent pas agir sur la variable sont pures[^032001].
en: Functions to which the world variable isn't provided are pure[^032001].

fr: [^032001]: Il y a quelques exceptions _peu sûres_ à cette règle. Mais vous ne devriez pas en voir en application réelle, sauf pour le _debugging_.
en: [^032001]: There are some _unsafe_ exceptions to this rule. But you shouldn't see such use in a real application except maybe for debugging purposes.

fr: Haskell considère l'état du monde comme une variable à passer à `main`.
en: Haskell considers the state of the world as an input variable to `main`.
fr: Mais son type réel est plus proche de celui ci[^032002] :
en: But the real type of main is closer to this one[^032002]:

fr: [^032002]: Pour les curieux, le vrai type est `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. Tous les `#` ont rapport avec l'optimisation et j'ai échangé quelques champs dans mon exemple. Mais c'est l'idée de base.
en: [^032002]: For the curious ones, the real type is `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. All the `#` has to do with optimisation and I swapped the fields in my example. But this is the basic idea.

~~~~~~ {.haskell}
main :: World -> ((),World)
~~~~~~

fr: Le type `()` est le type "unit".
en: The `()` type is the unit type.
fr: Rien à voir ici.
en: Nothing to see here.

fr: Maintenant réécrivons notre fonction `main` avec cela à l'esprit :
en: Now let's rewrite our main function with this in mind:

~~~~~~ {.haskell}
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (sum list,w1) in
    x
~~~~~~

fr: D'abord, on remarque que toutes les fonctions avec des effets de bord doivent avoir le type :
en: First, we note that all functions which have side effects must have the type:

~~~~~~ {.haskell}
World -> (a,World)
~~~~~~

fr: où `a` est le type du résultat.
en: where `a` is the type of the result.
fr: Par exemple, une fonction `getChar` aura le type `World -> (Char, World).
en: For example, a `getChar` function should have the type `World -> (Char, World)`.

fr: Une autre chose à noter est l'astuce pour corriger l'ordre d'évaluation.
en: Another thing to note is the trick to fix the order of evaluation.
fr: En Haskell, pour évaluer `f a b`, vous avez l'embarras du choix :
en: In Haskell, in order to evaluate `f a b`, you have many choices:

fr: - évaluer d'abord `a` puis `b` puis `f a b`
en: - first eval `a` then `b` then `f a b`
fr: - évaluer d'abord `b` puis `a` puis `f a b`
en: - first eval `b` then `a` then `f a b`.
fr: - évaluer `a` et `b` parallèlement, puis `f a b`
en: - eval `a` and `b` in parallel then `f a b`

fr: Cela vient du fait que nous avons recours à une partie pure du langage.
en: This is true because we're working in a pure part of the language.

fr: Maintenant, si vous regardez la fonction `main`, vous voyez tout de suite qu'il faut évaluer la première
en: Now, if you look at the main function, it is clear you must eval the first
fr: ligne avant la seconde, car pour évaluer la seconde ligne vous devez
en: line before the second one since to evaluate the second line you have
fr: utliser un paramètre donné suite à l'évaluation de la première ligne.
en: to get a parameter given by the evaluation of the first line.

fr: Cette astuce fonctionne très bien.
en: This trick works like a charm.
fr: Le compilateur donnera à chaque étape un pointeur sur l'id du nouveau monde courant.
en: The compiler will at each step provide a pointer to a new real world id.
fr: En réalité, `print` sera évaluée comme suit :
en: Under the hood, `print` will evaluate as:

fr: - Écrit quelque chose sur l'écran
en: - print something on the screen
fr: - Modifie l'id du monde
en: - modify the id of the world
fr: - renvoyer `((), id du nouveau monde)`.
en: - evaluate as `((),new world id)`.

fr: Maintenant, si jetez un oeil au style de la fonction `main`, vous remarquerez qu'il est clairement peu commode.
en: Now, if you look at the style of the main function, it is clearly awkward.
fr: Essayons de faire la même chose avec la fonction `askUser` :
en: Let's try to do the same to the `askUser` function:

~~~~~~ {.haskell}
askUser :: World -> ([Integer],World)
~~~~~~

fr: Avant :
en: Before:

~~~~~~ {.haskell}
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers:"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser
~~~~~~

fr: Après :
en: After:

~~~~~~ {.haskell}
askUser w0 =
    let (_,w1)     = putStrLn "Enter a list of numbers:" in
    let (input,w2) = getLine w1 in
    let (l,w3)     = case getListFromString input of
                      Just l   -> (l,w2)
                      Nothing  -> askUser w2
    in
        (l,w3)
~~~~~~

fr: C'est similaire, mais peu commode.
en: This is similar, but awkward.
fr: Voyez-vous toutes ces variables temporaires `w?`.
en: Look at all these temporary `w?` names.

fr: Voici la leçon : une implémentation naïve des IO dans les langages fonctionnels purs serait maladroite !
en: The lesson is: naive IO implementation in Pure functional languages is awkward!

fr: Heureusement, il y a un meilleur moyen de résoudre ce problème.
en: Fortunately, there is a better way to handle this problem.
fr: Nous voyons un motif.
en: We see a pattern.
fr: Chaque ligne est de la forme :
en: Each line is of the form:

~~~~~~ {.haskell}
let (y,w') = action x w in
~~~~~~

fr: Même si pour certaines lignes l'argument `x` n'est pas nécessaire.
en: Even if for some lines the first `x` argument isn't needed.
fr: La sortie est un couple, `(answer, newWorldValue)`.
en: The output type is a couple, `(answer, newWorldValue)`.
fr: Chaque fonction `f` doit avoir un type similaire à :
en: Each function `f` must have a type similar to:

~~~~~~ {.haskell}
f :: World -> (a,World)
~~~~~~

fr: Et ce n'est pas fini, nous pouvons aussi remarquer que nous suivons toujours le même motif :
en: Not only this, but we can also note that we always follow the same usage pattern:

~~~~~~ {.haskell}
let (y,w1) = action1 w0 in
let (z,w2) = action2 w1 in
let (t,w3) = action3 w2 in
...
~~~~~~

fr: Chaque action peut prendre de 0 à n paramètres.
en: Each action can take from 0 to n parameters.
fr: Et en particulier, chaque action prend comme paramètre le résultat de la ligne précédente.
en: And in particular, each action can take a parameter from the result of a line above.

fr: Par exemple, nous pourrions aussi avoir :
en: For example, we could also have:

~~~~~~ {.haskell}
let (_,w1) = action1 x w0   in
let (z,w2) = action2 w1     in
let (_,w3) = action3 z w2 in
...
~~~~~~

fr: Avec, bien entendu, `actionN w :: (World) -> (a,World)`.
en: With, of course: `actionN w :: (World) -> (a,World)`.

fr:  > IMPORTANT: Il y a seulement 2 schémas importants à considérer :
en:  > IMPORTANT: there are only two important patterns to consider:
 >
 > ~~~
 > let (x,w1) = action1 w0 in
 > let (y,w2) = action2 x w1 in
 > ~~~
 >
fr:  > et 
en:  > and
 >
 > ~~~
 > let (_,w1) = action1 w0 in
 > let (y,w2) = action2 w1 in
 > ~~~

leftblogimage("jocker_pencil_trick.jpg","Jocker pencil trick")

fr: Maintenant, préparez-vous pour un petit tour de magie !
en: Now, we will do a magic trick.
fr: Faisons disparaître les variables temporaires de monde courant.
en: We will make the temporary world symbols "disappear".
fr: Nous allons `attacher` (_NDT: `bind` en anglais_) les deux lignes.
en: We will `bind` the two lines.
fr: Définissons la fonction `bind`.
en: Let's define the `bind` function.
fr: Son type est assez intimidant au début :
en: Its type is quite intimidating at first:

~~~~~~ {.haskell}
bind :: (World -> (a,World))
        -> (a -> (World -> (b,World)))
        -> (World -> (b,World))
~~~~~~

fr: Mais gardez en tête que `(World -> (a,World))` est le type d'une action d'IO.
en: But remember that `(World -> (a,World))` is the type for an IO action.
fr: Renommons-le pour plus de clarté :
en: Now let's rename it for clarity:

~~~~~~ {.haskell}
type IO a = World -> (a, World)
~~~~~~

fr: Quelques exemples de fonctions :
en: Some examples of functions:

~~~~~~ {.haskell}
getLine :: IO String
print :: Show a => a -> IO ()
~~~~~~

fr: `getLine` est une action d'E/S qui prend le monde en paramètre et retourne un couple `(String, World)`.
en: `getLine` is an IO action which takes world as a parameter and returns a couple `(String, World)`.
fr: Cela peut être résumé par : `getLine` est de type `IO String`, que nous pouvons voir comme une action d'E/S qui retournera une chaîne de caractères "dans une E/S".
en: This can be summarized as: `getLine` is of type `IO String`, which we also see as an IO action which will return a String "embeded inside an IO".

fr: La fonction `print` est elle aussi intéressante.
en: The function `print` is also interesting.
fr: Elle prend un argument qui peut être montré avec `show`.
en: It takes one argument which can be shown.
fr: En fait, elle prend deux arguments.
en: In fact it takes two arguments.
fr: Le premier est la valeur et le deuxième est l'état du monde.
en: The first is the value to print and the other is the state of world.
fr: Elle retourne un couple de type `((), World)`.
en: It then returns a couple of type `((), World)`.
fr: Cela signifie qu'elle change l'état du monde, mais ne produit pas d'autre donnée.
en: This means that it changes the state of the world, but doesn't yield any more data.

fr: Ce nouveau type `IO a` nous aide à simplifier le type de `bind` :
en: This new `IO a` type helps us simplify the type of `bind`:

~~~~~~ {.haskell}
bind :: IO a
        -> (a -> IO b)
        -> IO b
~~~~~~

fr: Cela dit que `bind` prend deux actions d'E/S en paramètres et retourne une autre action d'E/S.
en: It says that `bind` takes two IO actions as parameters and returns another IO action.

fr: Maintenant, rappelez-vous des motifs _importants_. Le premier était :
en: Now, remember the _important_ patterns. The first was:

~~~~~~ {.haskell}
pattern1 w0 = 
 let (x,w1) = action1 w0 in
 let (y,w2) = action2 x w1 in
 (y,w2)
~~~~~~

fr: Voyez les types :
en: Look at the types:

~~~~~~ {.haskell}
action1  :: IO a
action2  :: a -> IO b
pattern1 :: IO b
~~~~~~

fr: Cela ne vous semble-t-il pas familier ?
en: Doesn't it seem familiar?

~~~~~~ {.haskell}
(bind action1 action2) w0 =
    let (x, w1) = action1 w0
        (y, w2) = action2 x w1
    in  (y, w2)
~~~~~~

fr: L'idée est de cacher l'argument `World` avec cette fonction. Allons-y !
en: The idea is to hide the World argument with this function. Let's go:
fr: Par exemple si nous avions voulu simuler :
en: As an example imagine if we wanted to simulate:

~~~~~~ {.haskell}
let (line1, w1) = getLine w0 in
let ((), w2) = print line1 in
((), w2)
~~~~~~

fr: Maintenant, en utilisant la fonction `bind` :
en: Now, using the `bind` function:

~~~~~~ {.haskell}
(res, w2) = (bind getLine print) w0
~~~~~~

fr: Comme `print` est de type `Show a => a -> (World -> ((), World))`, nous savons que `res = ()` (type `unit`)
en: As print is of type `Show a => a -> (World -> ((), World))`, we know `res = ()` (`unit` type).
fr: Si vous ne voyez pas ce qui est magique ici, essayons avec trois lignes cette fois.
en: If you didn't see what was magic here, let's try with three lines this time.

~~~~~~ {.haskell}
let (line1,w1) = getLine w0 in
let (line2,w2) = getLine w1 in
let ((),w3) = print (line1 ++ line2) in
((),w3)
~~~~~~

fr: Qui est équivalent à :
en: Which is equivalent to:

~~~~~~ {.haskell}
(res,w3) = (bind getLine (\line1 ->
             (bind getLine (\line2 ->
               print (line1 ++ line2))))) w0
~~~~~~

fr: Avez-vous remarqué quelque chose ?
en: Didn't you notice something?
fr: Oui, aucune variable `World` temporaire n'est utilisée !
en: Yes, no temporary World variables are used anywhere!
fr: C'est _MA_._GIQUE_.
en: This is _MA_. _GIC_.

fr: Nous pouvons utiliser une meilleure notation.
en: We can use a better notation.
fr: Utilisons `(>>=)` au lieu de `bind`.
en: Let's use `(>>=)` instead of `bind`.
fr: `(>>=)` est une fonction infixe, comme
en: `(>>=)` is an infix function like
fr: `(+)`; pour mémoire : `3 + 4 ⇔ (+) 3 4`
en: `(+)`; reminder `3 + 4 ⇔ (+) 3 4`

~~~~~~ {.haskell}
(res,w3) = (getLine >>=
           (\line1 -> getLine >>=
           (\line2 -> print (line1 ++ line2)))) w0
~~~~~~

fr: Ho Ho Ho! Joyeux Noël !
fr; Haskell a confectionné du sucre syntaxique pour vous :
en: Ho Ho Ho! Merry Christmas Everyone!
en: Haskell has made syntactical sugar for us:

~~~~~~ {.haskell}
do
  x <- action1
  y <- action2
  z <- action3
  ...
~~~~~~

fr: Est remplacé par :
en: Is replaced by:

~~~~~~ {.haskell}
action1 >>= (\x ->
action2 >>= (\y ->
action3 >>= (\z ->
...
)))
~~~~~~

fr: Remarquez que vous pouvez utliser `x` dans `action2` et `x` et `y` dans `action3`.
en: Note that you can use `x` in `action2` and `x` and `y` in `action3`.

fr: Mais que se passe-t-il pour les lignes qui n'utilisent pas le `<-` ?
en: But what about the lines not using the `<-`?
fr: Facile, une autre fonction `blindBind` :
en: Easy, another function `blindBind`:

~~~~~~ {.haskell}
blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 w0 =
    bind action (\_ -> action2) w0
~~~~~~

fr: Je n'ai pas simplifié cette définition pour plus de clarté.
en: I didn't simplify this definition for the purposes of clarity.
fr: Bien sûr, nous pouvons utiliser une meilleure notation avec l'opérateur `(>>)`.
en: Of course, we can use a better notation: we'll use the `(>>)` operator.

fr: Et
en: And

~~~~~~ {.haskell}
do
    action1
    action2
    action3
~~~~~~

fr: Devient
en: Is transformed into

~~~~~~ {.haskell}
action1 >>
action2 >>
action3
~~~~~~

fr: Enfin, une autre fonction est plutôt utile.
en: Also, another function is quite useful.

~~~~~~ {.haskell}
putInIO :: a -> IO a
putInIO x = IO (\w -> (x,w))
~~~~~~

fr: D'une manière générale, c'est une façon de mettre des valeurs pures dans le "contexte d'E/S".
en: This is the general way to put pure values inside the "IO context".
fr: Le nom général pour `putInIO` est `return`.
en: The general name for `putInIO` is `return`.
fr: C'est un plutôt un mauvais nom lorsque vous commencez à programmer en Haskell. `return` est très différent de ce à quoi vous pourriez être habitué.
en: This is quite a bad name when you learn Haskell. `return` is very different from what you might be used to.

<hr/><a href="code/03_Hell/01_IO/21_Detailled_IO.lhs" class="cut">03_Hell/01_IO/<strong>21_Detailled_IO.lhs</strong></a>

fr: Pour finir, traduisons notre exemple :
en: To finish, let's translate our example:

~~~~~~ {.haskell}

askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by commas):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser

main :: IO ()
main = do
  list <- askUser
  print $ sum list
~~~~~~

fr: Se traduit en :
en: Is translated into:

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing
getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"
askUser :: IO [Integer]
askUser = 
    putStrLn "Enter a list of numbers (sep. by commas):" >>
    getLine >>= \input ->
    let maybeList = getListFromString input in
      case maybeList of
        Just l -> return l
        Nothing -> askUser

main :: IO ()
main = askUser >>=
  \list -> print $ sum list
~~~~~~
</div>
fr: Vous pouvez compiler ce code pour vérifier qu'il marche.
en: You can compile this code to verify that it works.

fr: Imaginez à quoi il ressemblerait sans le `(>>)` et `(>>=)`.
en: Imagine what it would look like without the `(>>)` and `(>>=)`.

<a href="code/03_Hell/01_IO/21_Detailled_IO.lhs" class="cut">03_Hell/01_IO/<strong>21_Detailled_IO.lhs</strong> </a>

<hr/><a href="code/03_Hell/02_Monads/10_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>10_Monads.lhs</strong></a>

fr: <h3 id="monads">Les monades</h3>
en: <h3 id="monads">Monads</h3>

blogimage("dali_reve.jpg","Dali, reve. It represents a weapon out of the mouth of a tiger, itself out of the mouth of another tiger, itself out of the mouth of a fish itself out of a grenade. I could have choosen a picture of the Human centipede as it is a very good representation of what a monad really is. But just to think about it, I find this disgusting and that wasn't the purpose of this document.")

fr: Maintenant le secret peut être dévoilé : `IO` est une _monade_.
en: Now the secret can be revealed: `IO` is a _monad_.
fr: Être une monade signifie que vous avez accès à du sucre syntaxique avec la notation `do`.
en: Being a monad means you have access to some syntactical sugar with the `do` notation.
fr: Mais principalement, vous avez accès à un motif de codage qui tempérera le flux de votre code.
en: But mainly, you have access to a coding pattern which will ease the flow of your code.

fr:  > **Remarques importantes** :
en:  > **Important remarks**:
 >
fr:  > - Le monades n'ont pas forcément quoi que ce soit à voir avec les effets de bord !
en:  > - Monad are not necessarily about effects!
fr:  >   Il y a beaucoup de monades _pures_.
en:  >   There are a lot of _pure_ monads.
fr:  > - Les monades concernent plus le séquençage.
en:  > - Monad are more about sequencing

fr: En Haskell, `Monad` est une classe de type.
en: In Haskell, `Monad` is a type class.
fr: Pour être une instance d'une classe de type, vous devez fournir les fonctions `(>>=)` et `return`.
en: To be an instance of this type class, you must provide the functions `(>>=)` and `return`.
fr: La fonction `(>>)` est dérivée de `(>>=)`.
en: The function `(>>)` is derived from `(>>=)`.
fr: Voici commment la classe de type `Monad` est déclarée (grosso modo) :
en: Here is how the type class `Monad` is declared (basically):

~~~~~~ {.haskell}
class Monad m  where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

  (>>) :: m a -> m b -> m b
  f >> g = f >>= \_ -> g

fr:   -- Vous pouvez ignorer cette fonction généralement,
en:   -- You should generally safely ignore this function
fr:   -- je crois qu'elle existe pour des raisons historiques
en:   -- which I believe exists for historical reasons
  fail :: String -> m a
  fail = error
~~~~~~

fr:  > Remarques :
en:  > Remarks:
 >
fr:  > - le mot-clé `class` n'est pas votre ami.
en:  > - the keyword `class` is not your friend.
fr:  >   Une classe en Haskell _n'est pas_ du même genre que celle des langages orientés-objet.
en:  >   A Haskell class is _not_ a class of the kind you will find in object-oriented programming.
fr:  >   Elles ont beaucoup de similarités avec les interfaces de Java.
en:  >   A Haskell class has a lot of similarities with Java interfaces.
fr:  >   Un meilleur mot aurait été `typeClass`, ce qui signifierait un ensemble de types. 
en:  >   A better word would have been `typeclass`, since that means a set of types.
fr:  >   Pour qu'un type appartienne à une classe, toutes les fonctions de cette classe doivent être fournies pour ce type.
en:  >   For a type to belong to a class, all functions of the class must be provided for this type.
fr:  > - Dans cet exemple particulier de classe de type, le type `m` doit être un type qui prend un argument.
en:  > - In this particular example of type class, the type `m` must be a type that takes an argument.
fr:  >   par exemple `IO a`, mais aussi `Maybe a`, `[a]`, etc...
en:  >   for example `IO a`, but also `Maybe a`, `[a]`, etc...
fr:  > - Pour être une monade utile, votre fonction doit obéir à quelques règles.
en:  > - To be a useful monad, your function must obey some rules.
fr:  >   Si votre construction n'obéit pas à ces règles, des choses étranges pourraient se produire :
en:  >   If your construction does not obey these rules strange things might happens:
 
 >   ~~~
 >   return a >>= k  ==  k a
 >   m >>= return  ==  m
 >   m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 >   ~~~

fr: <h4 id="maybe-monad">Maybe est une monade</h4>
en: <h4 id="maybe-monad">Maybe is a monad</h4>

fr: Il y a beaucoup de types différents qui sont des instances de `Monad`.
en: There are a lot of different types that are instances of `Monad`.
fr: L'un des plus faciles à décrire est `Maybe`.
en: One of the easiest to describe is `Maybe`.
fr: Si vous avez une séquence de valeurs `Maybe`, vous pouvez utiliser les monades pour les manipuler.
en: If you have a sequence of `Maybe` values, you can use monads to manipulate them.
fr: C'est particulièrement utile pour enlever des constructions `if..then..else..` trop nombreuses.
en: It is particularly useful to remove very deep `if..then..else..` constructions.

fr: Imaginez une opération bancaire complexe. Vous êtes éligible pour gagner 700€ seulement si
en: Imagine a complex bank operation. You are eligible to gain about 700€ only
fr: vous pouvez effectuer une liste d'opérations sans tomber en dessous de zéro.
en: if you can afford to follow a list of operations without your balance dipping below zero.

<div class="codehighlight">
~~~~~~ {.haskell}
deposit  value account = account + value
withdraw value account = account - value

eligible :: (Num a,Ord a) => a -> Bool
eligible account =
  let account1 = deposit 100 account in
    if (account1 < 0)
    then False
    else
      let account2 = withdraw 200 account1 in
      if (account2 < 0)
      then False
      else
        let account3 = deposit 100 account2 in
        if (account3 < 0)
        then False
        else
          let account4 = withdraw 300 account3 in
          if (account4 < 0)
          then False
          else
            let account5 = deposit 1000 account4 in
            if (account5 < 0)
            then False
            else
              True

main = do
  print $ eligible 300 -- True
  print $ eligible 299 -- False
~~~~~~
</div>
<a href="code/03_Hell/02_Monads/10_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>10_Monads.lhs</strong> </a>

<hr/><a href="code/03_Hell/02_Monads/11_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>11_Monads.lhs</strong></a>

fr: Maintenant, améliorons cela en utilisant le fait que `Maybe` est une Monade.
en: Now, let's make it better using Maybe and the fact that it is a Monad

<div class="codehighlight">
~~~~~~ {.haskell}
deposit :: (Num a) => a -> a -> Maybe a
deposit value account = Just (account + value)

withdraw :: (Num a,Ord a) => a -> a -> Maybe a
withdraw value account = if (account < value) 
                         then Nothing 
                         else Just (account - value)

eligible :: (Num a, Ord a) => a -> Maybe Bool
eligible account = do
  account1 <- deposit 100 account 
  account2 <- withdraw 200 account1 
  account3 <- deposit 100 account2 
  account4 <- withdraw 300 account3 
  account5 <- deposit 1000 account4
  Just True

main = do
  print $ eligible 300 -- Just True
  print $ eligible 299 -- Nothing
~~~~~~
</div>
<a href="code/03_Hell/02_Monads/11_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>11_Monads.lhs</strong> </a>

<hr/><a href="code/03_Hell/02_Monads/12_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>12_Monads.lhs</strong></a>

fr: Pas mauvais, mais nous pouvons faire encore mieux :
en: Not bad, but we can make it even better:

<div class="codehighlight">
~~~~~~ {.haskell}
deposit :: (Num a) => a -> a -> Maybe a
deposit value account = Just (account + value)

withdraw :: (Num a,Ord a) => a -> a -> Maybe a
withdraw value account = if (account < value) 
                         then Nothing 
                         else Just (account - value)

eligible :: (Num a, Ord a) => a -> Maybe Bool
eligible account =
  deposit 100 account >>=
  withdraw 200 >>=
  deposit 100  >>=
  withdraw 300 >>=
  deposit 1000 >>
  return True

main = do
  print $ eligible 300 -- Just True
  print $ eligible 299 -- Nothing
~~~~~~
</div>
fr: Nous avons prouvé que les monades sont un bon moyen de rendre notre code plus élégant.
en: We have proven that Monads are a good way to make our code more elegant.
fr: Remarquez que cette idée d'organisation de code, en particulier pour `Maybe`, peut être utilisée
en: Note this idea of code organization, in particular for `Maybe` can be used
fr: dans la plupart des langages impératifs.
en: in most imperative languages.
fr: En fait, c'est le type de construction que nous faisons naturellement.
en: In fact, this is the kind of construction we make naturally.

fr:  > Une remarque importante :
en:  > An important remark:
 > 
fr:  > Le premier élement de la séquence qui sera évalué comme `Nothing` stoppera
en:  > The first element in the sequence being evaluated to `Nothing` will stop
fr:  > l'évaluation.
en:  > the complete evaluation. 
fr:  > Cela signifie que vous n'exécutez pas toutes les lignes.
en:  > This means you don't execute all lines.
fr:  > Cela découle du caractère paresseux de Haskell.
en:  > You get this for free, thanks to laziness.

fr: Vous pourriez aussi revoir ces exemples avec la définition de `(>>=)` pour `Maybe`
en: You could also replay these example with the definition of `(>>=)` for `Maybe`
fr: en tête :
en: in mind:

~~~~~~ {.haskell}
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing  >>= _  = Nothing
    (Just x) >>= f  = f x

    return x = Just x
~~~~~~

fr: La monade `Maybe` a prouvé par un simple exemple qu'elle est utile.
en: The `Maybe` monad proved to be useful while being a very simple example.
fr: Nous avons vu l'utilité de la monade `IO`.
en: We saw the utility of the `IO` monad.
fr: Mais maintenant, voici un exemple encore plus cool : les listes.
en: But now for a cooler example, lists.

<a href="code/03_Hell/02_Monads/12_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>12_Monads.lhs</strong> </a>

<hr/><a href="code/03_Hell/02_Monads/13_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>13_Monads.lhs</strong></a>

fr: <h4 id="the-list-monad">La monade List</h4>
en: <h4 id="the-list-monad">The list monad</h4>

blogimage("golconde.jpg","Golconde de Magritte")

fr: La monade `List` nous aide à simuler des calculs non-déterministes.
en: The list monad helps us to simulate non-deterministic computations.
fr: C'est parti :
en: Here we go:

<div class="codehighlight">
~~~~~~ {.haskell}
import Control.Monad (guard)

allCases = [1..10]

resolve :: [(Int,Int,Int)]
resolve = do
              x <- allCases
              y <- allCases
              z <- allCases
              guard $ 4*x + 2*y < z
              return (x,y,z)

main = do
  print resolve
~~~~~~
</div>
fr: Ma. GIQUE. :
en: MA. GIC. :

~~~
[(1,1,7),(1,1,8),(1,1,9),(1,1,10),(1,2,9),(1,2,10)]
~~~

fr: Pour la monade `List`, il y a aussi un sucre syntaxique :
en: For the list monad, there is also this syntactic sugar:

<div class="codehighlight">
~~~~~~ {.haskell}
  print $ [ (x,y,z) | x <- allCases,
                      y <- allCases,
                      z <- allCases,
                      4*x + 2*y < z ]
~~~~~~
</div>
fr: Je ne listerai pas toutes les monades, car il y en a beaucoup.
en: I won't list all the monads, since there are many of them.
fr: Utiliser les monades simplifie la manipulations de plusieurs notions dans les langages purs.
en: Using monads simplifies the manipulation of several notions in pure languages.
fr: Les monades sont très utiles, en particulier pour :
en: In particular, monads are very useful for:

fr: - L'E/S (IO),
en: - IO,
fr: - les calculs non-déterministes,
en: - non-deterministic computation,
fr: - générer des nombres pseudo-aléatoires,
en: - generating pseudo random numbers,
fr: - garder un état de configuration,
en: - keeping configuration state,
fr: - écrire un état,
en: - writing state,
- ...

fr: Si vous m'avez suivi jusqu'ici, alors vous avez terminé !
en: If you have followed me until here, then you've done it!
fr: Vous connaissez les monades[^03021301] !
en: You know monads[^03021301]!

fr: [^03021301]: Vous aurez quand même besoin de pratiquer un peu pour vous habituer à elles et pour comprendre quand les utiliser et créer les vôtres. Mais vous avez déjà fait un grand pas dans cette direction.
en: [^03021301]: Well, you'll certainly need to practice a bit to get used to them and to understand when you can use them and create your own. But you already made a big step in this direction.

<a href="code/03_Hell/02_Monads/13_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>13_Monads.lhs</strong> </a>

fr: <h2 id="appendix">Appendice</h2>
en: <h2 id="appendix">Appendix</h2>

fr: Cette section n'est pas vraiment sur l'apprentissage d'Haskell.
en: This section is not so much about learning Haskell.
fr: Elle est ici pour discuter de quelques détails.
en: It is just here to discuss some details further.

<hr/><a href="code/04_Appendice/01_More_on_infinite_trees/10_Infinite_Trees.lhs" class="cut">04_Appendice/01_More_on_infinite_trees/<strong>10_Infinite_Trees.lhs</strong></a>

fr: <h3 id="more-on-infinite-tree">Revenons sur les arbres infinis</h3>
en: <h3 id="more-on-infinite-tree">More on Infinite Tree</h3>

fr: Dans la section sur [les structures infinies](#infinite-structures) nous avons vu quelques 
en: In the section [Infinite Structures](#infinite-structures) we saw some simple
fr: constructions simples.
en: constructions.
fr: Malheureusement, nous avons enlevé deux propriétés de notre arbre:
en: Unfortunately we removed two properties from our tree:

fr: 1. Pas de valeurs identiques
en: 1. no duplicate node value
fr: 2. Arbre bien ordonné
en: 2. well ordered tree

fr: Dans cette section nous allons tenter de garder la première propriété.
en: In this section we will try to keep the first property.
fr: Concernant la seconde, nous ne devons pas nous en préoccuper ici mais nous discuterons
en: Concerning the second one, we must relax it but we'll discuss how to
fr: de comment la garder le plus possible.
en: keep it as much as possible.

<div style="display:none">

This code is mostly the same as the one in the [tree section](#trees).

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.List
data BinTree a = Empty 
                 | Node a (BinTree a) (BinTree a) 
                  deriving (Eq,Ord)

-- declare BinTree a to be an instance of Show
instance (Show a) => Show (BinTree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    treeshow pref Empty = ""
    treeshow pref (Node x Empty Empty) = 
                  (pshow pref x)

    treeshow pref (Node x left Empty) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    treeshow pref (Node x Empty right) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    treeshow pref (Node x left right) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- show a tree using some prefixes to make it nice
    showSon pref before next t = 
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replace "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (show x)

    -- replace on char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x 
              | x == c = new
              | otherwise = x:[] -- "x"

~~~~~~
</div>
</div>

fr: Notre première étape est de créer une liste de nombres pseudo-aléatoires:
en: Our first step is to create some pseudo-random number list:

<div class="codehighlight">
~~~~~~ {.haskell}
shuffle = map (\x -> (x*3123) `mod` 4331) [1..]
~~~~~~
</div>
fr: Pour mémoire, voici la définition de `treeFromList`
en: Just as a reminder, here is the definition of `treeFromList`

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList []    = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
~~~~~~
</div>
fr: et 
en: and 
`treeTakeDepth`:

<div class="codehighlight">
~~~~~~ {.haskell}
treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) = let
          nl = treeTakeDepth (n-1) left
          nr = treeTakeDepth (n-1) right
          in
              Node x nl nr
~~~~~~
</div>
fr: Voyez le résultats de:
en: See the result of:

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
      putStrLn "take 10 shuffle"
      print $ take 10 shuffle
      putStrLn "\ntreeTakeDepth 4 (treeFromList shuffle)"
      print $ treeTakeDepth 4 (treeFromList shuffle)
~~~~~~
</div>
~~~
% runghc 02_Hard_Part/41_Infinites_Structures.lhs
take 10 shuffle
[3123,1915,707,3830,2622,1414,206,3329,2121,913]
treeTakeDepth 4 (treeFromList shuffle)

< 3123
: |--1915
: |  |--707
: |  |  |--206
: |  |  `--1414
: |  `--2622
: |     |--2121
: |     `--2828
: `--3830
:    |--3329
:    |  |--3240
:    |  `--3535
:    `--4036
:       |--3947
:       `--4242
~~~

fr: Le code fonctionne!
en: Yay! It ends! 
fr: Attention cependant, cela marchere seulement si vous avez toujours quelque chose à mettre dans une branche.
en: Beware though, it will only work if you always have something to put into a branch.

fr: Par exemple
en: For example 

~~~~~~ {.haskell}
treeTakeDepth 4 (treeFromList [1..]) 
~~~~~~

fr: tournera en boucle pour toujours.
en: will loop forever. 
fr: Simplement parce que le code essayera d'accéder à première valeur de `filter (<1) [2..]`.
en: Simply because it will try to access the head of `filter (<1) [2..]`.
fr: Mais `filter` n'est pas assez intelligent pour comprendre que le résultat est une liste vide.
en: But `filter` is not smart enought to understand that the result is the empty list.

fr: Toutefois, cela reste un exemple sympa de ce qu'un programme non-stricit a à offrir.
en: Nonetheless, it is still a very cool example of what non strict programs have to offer.

fr: Laissé pour exercice au lecteur:
en: Left as an exercise to the reader:

fr: - Prouver l'existence d'un nombre `n` tel que `treeTakeDepth n (treeFromList shuffle)` provoquera une boucle infinie.
en: - Prove the existence of a number `n` so that `treeTakeDepth n (treeFromList shuffle)` will enter an infinite loop.
fr: - Trouver une borne supérieur `n`.
en: - Find an upper bound for `n`.
fr: - Prouver qu'il n(y a pas de liste `shuffle` qui termine le programme pour n'importe quelle profondeur.
en: - Prove there is no `shuffle` list so that, for any depth, the program ends.

<a href="code/04_Appendice/01_More_on_infinite_trees/10_Infinite_Trees.lhs" class="cut">04_Appendice/01_More_on_infinite_trees/<strong>10_Infinite_Trees.lhs</strong> </a>

<hr/><a href="code/04_Appendice/01_More_on_infinite_trees/11_Infinite_Trees.lhs" class="cut">04_Appendice/01_More_on_infinite_trees/<strong>11_Infinite_Trees.lhs</strong></a>

<div style="display:none">

This code is mostly the same as the preceding one.

<div class="codehighlight">
~~~~~~ {.haskell}
import Debug.Trace (trace)
import Data.List
data BinTree a = Empty 
                 | Node a (BinTree a) (BinTree a) 
                  deriving (Eq,Ord)
~~~~~~
</div>
<div class="codehighlight">
~~~~~~ {.haskell}
-- declare BinTree a to be an instance of Show
instance (Show a) => Show (BinTree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    treeshow pref Empty = ""
    treeshow pref (Node x Empty Empty) = 
                  (pshow pref x)

    treeshow pref (Node x left Empty) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    treeshow pref (Node x Empty right) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    treeshow pref (Node x left right) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- show a tree using some prefixes to make it nice
    showSon pref before next t = 
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replace "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (" " ++ show x)

    -- replace on char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x 
              | x == c = new
              | otherwise = x:[] -- "x"

treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) = let
          nl = treeTakeDepth (n-1) left
          nr = treeTakeDepth (n-1) right
          in
              Node x nl nr
~~~~~~
</div>
</div>

fr: Pour résoudre ces problèmes nous allons modifier légèrement nos
en: In order to resolve these problem we will modify slightly our
fr: fonctions `treeFromList` et `shuffle`.
en: `treeFromList` and `shuffle` function.

fr: Un premier problème est le manque de nombres différents dans notre immlémentation de `shuffle`.
en: A first problem, is the lack of infinite different number in our implementation of `shuffle`.
fr: Nous avons  généré seulement `4331` nombres différents.
en: We generated only `4331` different numbers.
fr: Pour résoudre cela nous allons faire un meilleure fonction `shuffle`.
en: To resolve this we make a slightly better `shuffle` function.

<div class="codehighlight">
~~~~~~ {.haskell}
shuffle = map rand [1..]
          where 
              rand x = ((p x) `mod` (x+c)) - ((x+c) `div` 2)
              p x = m*x^2 + n*x + o -- some polynome
              m = 3123    
              n = 31
              o = 7641
              c = 1237
~~~~~~
</div>
fr: Cette fonction à la propriété de ne pas avoir de bornes supérieure ou inférieure.
en: This shuffle function has the property (hopefully) not to have an upper nor lower bound.
fr: Mais avoir une meilleure list `shuffle` n'est pas assez pour entrer dans une boucle infinie.
en: But having a better shuffle list isn't enough not to enter an infinite loop.

fr: Généralement,  nous ne pouvons pas décider que `filter (<x) xs` est vide.
en: Generally, we cannot decide whether `filter (<x) xs` is empty.
fr: Donc pour résoudre le problème, je vais autoriser quelques erreurs dans la création de notre arbre binaire.
en: Then to resolve this problem, I'll authorize some error in the creation of our binary tree.
fr: Cette nouvelle version du code peut créer des arbres binaires qui n'ont pas à suivre les propriétés suivantes pour quelque uns de leurs noeuds:
en: This new version of code can create binary tree which don't have the following property for some of its nodes: 

fr:  > Tous les élements de la branche de gauche doit être strictement inférieur au la valeur racine.
en:  > Any element of the left (resp. right) branch must all be strictly inferior (resp. superior) to the label of the root.

fr: Remarquez que cela donnera _souvent_ un arbre ordonné.
en: Remark it will remains _mostly_ an ordered binary tree.
fr: En outre, avec cette construction, chaque noeud est unique dans l'arbre.
en: Furthermore, by construction, each node value is unique in the tree.

fr: Voici notre nouvelle version de `treeFromList`. Nous avons simplement remplacé `filter` par `safefilter`.
en: Here is our new version of `treeFromList`. We simply have replaced `filter` by `safefilter`.

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a, Show a) => [a] -> BinTree a
treeFromList []    = Empty
treeFromList (x:xs) = Node x left right
          where 
              left = treeFromList $ safefilter (<x) xs
              right = treeFromList $ safefilter (>x) xs
~~~~~~
</div>
fr: Cette nouvelle fonction `safefilter` est presque équivalente à `filter` mais n'entre pas dans des boucles infinies si le résultat est une liste finie.
en: This new function `safefilter` is almost equivalent to `filter` but don't enter infinite loop if the result is a finite list.
fr: Si elle ne peut pas trouver un élément pour lequel le test est vrai après 10000 étapes consécutives, alors elle considère que la recherche est finie.
en: If it cannot find an element for which the test is true after 10000 consecutive steps, then it considers to be the end of the search.

<div class="codehighlight">
~~~~~~ {.haskell}
safefilter :: (a -> Bool) -> [a] -> [a]
safefilter f l = safefilter' f l nbTry
  where
      nbTry = 10000
      safefilter' _ _ 0 = []
      safefilter' _ [] _ = []
      safefilter' f (x:xs) n = 
                  if f x 
                     then x : safefilter' f xs nbTry 
                     else safefilter' f xs (n-1) 
~~~~~~
</div>
fr: Maintenant faites tourner le programme et soyez heureux:
en: Now run the program and be happy:

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
      putStrLn "take 10 shuffle"
      print $ take 10 shuffle
      putStrLn "\ntreeTakeDepth 8 (treeFromList shuffle)"
      print $ treeTakeDepth 8 (treeFromList $ shuffle)
~~~~~~
</div>
fr: Vous devriez réaliser que le temps nécessaire pour afficher chaque valeur est différent.
en: You should realize the time to print each value is different.
fr: C'est parce que Haskell calcule chaque valeur lorsqu'il en a besoin.
en: This is because Haskell compute each value when it needs it.
fr: Et dans ce cas, il est demandé de l'afficher à l'écran.
en: And in this case, this is when asked to print it on the screen.

fr: Vous pouvez même essayer de remplacer la profondeur de `8` par `100`.
en: Impressively enough, try to replace the depth from `8` to `100`.
fr: Cela marchera sans tuer votre RAM!
en: It will work without killing your RAM! 
fr: La gestion de la mémoire est faite naturellement par Haskell.
en: The flow and the memory management is done naturally by Haskell.

fr: Laissé comme exercices au lecteur:
en: Left as an exercise to the reader:

fr: - Même avec une grande valeur constante pour `deep` et `nbTry`, cela semble marcher correctement. Mais dans le pire des cas, cela peut devenir exponentiel. 
en: - Even with large constant value for `deep` and `nbTry`, it seems to work nicely. But in the worst case, it can be exponential.
fr:   Créez la pire liste à donner comme paramètre à `treeFromList`.
en:   Create a worst case list to give as parameter to `treeFromList`.  
fr:   _indice_: pensez à (`[0,-1,-1,....,-1,1,-1,...,-1,1,...]`).
en:   _hint_: think about (`[0,-1,-1,....,-1,1,-1,...,-1,1,...]`).
fr: - J'ai commencé à implémenter `safefilter` comme ceci:
en: - I first tried to implement `safefilter` as follow:
  <pre>
  safefilter' f l = if filter f (take 10000 l) == []
                    then []
                    else filter f l
  </pre>
fr:   Expliquer pourquoi cela ne fonctionne pas et peut entrer dans une boucle infinie.
en:   Explain why it doesn't work and can enter into an infinite loop.
fr: - Supposez que `shuffle` est une liste de nombre réellement aléatoires avec de plus en plus de bornes.
en: - Suppose that `shuffle` is real random list with growing bounds.
fr:   Si vous étudiez un peu cette structure, vous découvrirez qu'elle a toutes les chances
en:   If you study a bit this structure, you'll discover that with probability 1,
fr:   d'être finie.
en:   this structure is finite.
fr:   En utilisant le code suivant
en:   Using the following code 
fr:   (supposez que nous pouvons utliser `safefilter'` directement comme si cela n'était pas dans le `where` de `safefilter`.
en:   (suppose we could use `safefilter'` directly as if was not in the where of safefilter)
fr:   trouvez une définition de `f` telle que, avec une probabilité de `1`,
en:   find a definition of `f` such that with probability `1`, 
fr:   `treeFromList' shuffle` est infinie?. Et prouvez-le.
en:   `treeFromList' shuffle` is infinite. And prove it.
fr:   Avertissement, ce n'est qu'une conjecture.
en:   Disclaimer, this is only a conjecture.

~~~~~~ {.haskell}
treeFromList' []  n = Empty
treeFromList' (x:xs) n = Node x left right
    where
        left = treeFromList' (safefilter' (<x) xs (f n)
        right = treeFromList' (safefilter' (>x) xs (f n)
        f = ???
~~~~~~

<a href="code/04_Appendice/01_More_on_infinite_trees/11_Infinite_Trees.lhs" class="cut">04_Appendice/01_More_on_infinite_trees/<strong>11_Infinite_Trees.lhs</strong> </a>

en: ## Thanks
fr: ## Remerciements

fr: Merci à [`/r/haskell`](http://reddit.com/r/haskell) et 
en: Thanks to [`/r/haskell`](http://reddit.com/r/haskell) and 
[`/r/programming`](http://reddit.com/r/programming).
fr: Vos commentaires étaient plus que bienvenus.
en: Your comment were most than welcome.

fr: Particulièrement, je voudrais remercier mille fois [Emm](https://github.com/Emm)
en: Particularly, I want to thank [Emm](https://github.com/Emm) a thousand times 
fr: pour le temps qu'il a consacré à corriger mon anglais.
en: for the time he spent on correcting my English. 
fr: Merci beaucoup.
en: Thank you man.
