-----
theme: scientific
image: /Scratch/img/blog/Haskell-the-Hard-Way/magritte_pleasure_principle.jpg
menupriority:   1
kind: article
published: 2012-02-08
title: Haskell comme un vrai!
subtitle: Haskell à s'en faire griller les neurones
author: Yann Esposito
authoruri: yannesposito.com
tags: Haskell, programming, functional, tutorial
-----
blogimage("magritte_pleasure_principle.jpg","Magritte pleasure principle")

<div class="intro">


%tlal Un tutoriel très court mais très dense pour apprendre Haskell.

Merci à [Oleg Taykalo](https://plus.google.com/u/0/113751420744109290534) vous pouvez trouver une traduction russe ici: [Partie 1](http://habrahabr.ru/post/152889/) _&_ [Partie 2](http://habrahabr.ru/post/153383/) ; 
Un grand merci à [lepereceval](https://github.com/lepereceval) pour sa traduction française que je n'ai pas eu le courage de faire moi-même ! (_NDT: Si vous trouvez une erreurs ou même plusieurs dans la traduction française, vous pouvez m'en faire part à le_point_pere_point_ceval_arobase_gmail_point_com_. N'hésitez pas!_)

> <center><hr style="width:30%;float:left;border-color:#CCCCD0;margin-top:1em"/><span class="sc"><b>Table of Content</b></span><hr style="width:30%;float:right;border-color:#CCCCD0;margin-top:1em"/></center>
>
> <div class="toc">
>
> * <a href="#introduction">Introduction</a>
>   * <a href="#install">Installation</a>
>   * <a href="#don-t-be-afraid">Ne soyez pas effrayés!</a>
>   * <a href="#very-basic-haskell">Les bases de Haskell</a>
>     * <a href="#function-declaration">Déclaration de fonctions</a>
>     * <a href="#a-type-example">Un exemple de type</a>
> * <a href="#essential-haskell">Notions essentielles</a>
>   * <a href="#notations">Notations</a>
>       * <a href="#arithmetic">Arithmétique</a>
>       * <a href="#logic">Logique</a>
>       * <a href="#powers">Puissances</a>
>       * <a href="#lists">Listes</a>
>       * <a href="#strings">Chaînes de caractères</a>
>       * <a href="#tuples">Tuples</a>
>       * <a href="#deal-with-parentheses">Traiter avec les parenthèses</a>
>   * <a href="#useful-notations-for-functions">Notations utiles pour les fonctions</a>
> * <a href="#hard-part">La Partie Difficile</a>
>   * <a href="#functional-style">Le style fonctionnel</a>
>     * <a href="#higher-order-functions">Fonctions d'ordre supérieur</a>
>   * <a href="#types">Les types</a>
>     * <a href="#type-inference">Inférence de type</a>
>     * <a href="#type-construction">Construction de types</a>
>     * <a href="#recursive-type">Type récursif</a>
>     * <a href="#trees">Les arbres</a>
>   * <a href="#infinite-structures">Structures infinies</a>
> * <a href="#hell-difficulty-part">Partie de difficulté infernale</a>
>   * <a href="#deal-with-io">S'occuper de l'E/S (IO)</a>
>   * <a href="#io-trick-explained">Le truc des IO révélé</a>
>   * <a href="#monads">Les monades</a>
>     * <a href="#maybe-monad">Maybe est une monade</a>
>     * <a href="#the-list-monad">La monade List</a>
> * <a href="#appendix">Appendice</a>
>   * <a href="#more-on-infinite-tree">Revenons sur les arbres infinis</a>
>
> </div>

</div>
<div class="intro">

Je pense vraiment que
tous les développeurs devraient apprendre Haskell.
Peut-être pas devenir des ninjas d'Haskell,
mais au moins savoir ce que ce langage a de particulier.
Son apprentissage ouvre énormément l'esprit.

La plupart des langages partagent les mêmes fondamentaux :

- les variables
- les boucles
- les pointeurs[^0001]
- les structures de données, les objets et les classes

[^0001]: Même si tous les langages récents essayent de les cacher, ils restent présents.

Haskell est très différent.
Ce langage utilise des concepts dont je n'avais jamais entendu parlé avant.
Beaucoup de ces concepts pourront vous aider à devenir un meilleur développeur.

Plier son esprit à Haskell peut être difficile.
Ce le fût pour moi.
Dans cet article, j'essaye de fournir les informations qui m'ont manquées lors de mon apprentissage.

Cet article sera certainement difficile à suivre.
Mais c'est voulu.
Il n'y a pas de raccourci pour apprendre Haskell.
C'est difficile.
Mais je pense que c'est une bonne chose.
C'est parce qu'Haskell est difficile qu'il est intéressant.

La manière conventionnelle d'apprendre Haskell est de lire deux livres.
En premier ["Learn You a Haskell"](http://learnyouahaskell.com)
et ensuite ["Real World Haskell"](http://www.realworldhaskell.org).
Je pense aussi que c'est la bonne manière de s'y prendre.
Mais apprendre même un tout petit peu d'Haskell est presque impossible sans se plonger réellement dans ces livres.

Cet article fait un résumé très dense et rapide des aspect majeurs d'Haskell.
J'y ai aussi rajouté des informations qui m'ont manqué pendant l'apprentissage de ce langage.

Pour les francophones ; je suis désolé.
Je n'ai pas eu le courage de tout retraduire en français.
Sachez cependant que si vous êtes plusieurs à insister, je ferai certainement l'effort de traduire l'article en entier.
Et si vous vous sentez d'avoir une bonne âme je ne suis pas contre un peu d'aide.
Les sources de cet article sont sur [gihub](http://github.com/yogsototh/learn_haskell.git).

Cet article contient cinq parties :


- Introduction : un exemple rapide pour montrer qu'Haskell peut être facile.
- Les bases d'Haskell : La syntaxe et des notions essentielles
- Partie difficile :
    - Style fonctionnel : un exemple progressif, du style impératif au style fonctionnel ;
    - Types : la syntaxe et un exemple d'arbre binaire ;
    - Structure infinie : manipulons un arbre infini !
- Partie de difficulté infernale :
    - Utiliser les IO : un exemple très minimal ;
    - Le truc des IO révélé : les détails cachés d'IO qui m'ont manqués
    - Les monades : incroyable à quel point on peut généraliser
- Appendice :
    - Revenons sur les arbres infinis : une discussion plus mathématique sur la manipulation d'arbres infinis.


 > Note: Chaque fois que vous voyez un séparateur avec un nom de fichier se terminant par `lhs`, vous pouvez cliquer sur le nom de fichier et télécharger le fichier.
 > Si vous sauvegardez le fichier sour le nom `filename.lhs`, vous pouvez l'exécuter avec :
 > <pre>
 > runhaskell filename.lhs
 > </pre>
 >
 > Certains ne marcheront pas, mais la majorité vous donneront un résultat.
 > Vous devriez voir un lien juste en dessous.

</div>

<hr/><a href="code/01_basic/10_Introduction/00_hello_world.lhs" class="cut">01_basic/10_Introduction/<strong>00_hello_world.lhs</strong></a>

<h2 id="introduction">Introduction</h2>

<h3 id="install">Installation</h3>

blogimage("Haskell-logo.png", "Haskell logo")

- La principale façon d'installer Haskell est [Haskell Platform](http://www.haskell.org/platform).

Outils:

- `ghc`: Compilateur similaire à gcc pour le langage `C`.
- `ghci`: Console Haskell interactive (Read-Eval-Print Loop)
- `runhaskell`: Exécuter un programme sans le compiler. Pratique mais très lent comparé aux programmes compilés.

<h3 id="don-t-be-afraid">Ne soyez pas effrayés!</h3>

blogimage("munch_TheScream.jpg","The Scream")

Beaucoup de livres/articles sur Haskell commencent par présenter des formules ésotériques (Algorithmes de tri rapide, suite de Fibonacci, etc...).
Je ferai l'exact opposé
En premier lieu je ne vous montrerai pas les super-pouvoirs d'Haskell.
Je vais commencer par les similarités avec les autres langages de programmation.
Commençons par l'indispensable "Hello World!".

<div class="codehighlight">
~~~~~~ {.haskell}
main = putStrLn "Hello World!"
~~~~~~
</div>
Pour l'exécuter, vous pouvez enregistrer ce code dans un fichier `hell.hs` et:

~~~~~~ {.zsh}
~ runhaskell ./hello.hs
Hello World!
~~~~~~

Vous pouvez également télécharger la source Haskell littérale.
Vous devriez voir un lien juste au dessus du titre de l'introduction.
Téléchargez ce fichier en tant que `00_hello_world.lhs` et:

~~~~~~ {.zsh}
~ runhaskell 00_hello_world.lhs
Hello World!
~~~~~~
<a href="code/01_basic/10_Introduction/00_hello_world.lhs" class="cut">01_basic/10_Introduction/<strong>00_hello_world.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/10_hello_you.lhs" class="cut">01_basic/10_Introduction/<strong>10_hello_you.lhs</strong></a>

Maintenant, un programme qui demande votre nom et répond "Hello" suivit du nom que vous avez entré:

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
    print "What is your name?"
    name <- getLine
    print ("Hello " ++ name ++ "!")
~~~~~~
</div>
Premièrement, comparons ce code avec ceux de quelques langages de programmation impératif:

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

La structure est la même, mais il y a quelques différences de syntaxe.
La partie principale de ce tutoriel sera consacrée à expliquer cela.

En Haskell il y a une fonction `main` tous les objets ont un type.
Le type de `main` est `IO ()`.
Cela veut dire que `main` causera des effets secondaires.

Rappelez-vous just que Haskell peut ressembler énormément aux principaux langages impératifs.

<a href="code/01_basic/10_Introduction/10_hello_you.lhs" class="cut">01_basic/10_Introduction/<strong>10_hello_you.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/20_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>20_very_basic.lhs</strong></a>

<h3 id="very-basic-haskell">Les bases de Haskell</h3>

blogimage("picasso_owl.jpg","Picasso minimal owl")

Avant de continuer, vous devez êtres avertis à propos de propriétés essentielles de Haskell.

_Fonctionnel_

Haskell est un langage fonctionnel
Si vous avez déjà travaillé avec un langage impératif, vous devrez apprendre beaucoup de nouvelles choses.
Heureusement beaucoup de ces nouveaux concepts vous aidera à programmer même dans un langage impératif.

_Typage Statique Intelligent_

Au lieu de bloquer votre chemin comme en `C`, `C++` ou `Java`, le système de typage est ici pour vous aider.

_Pureté_

Généralement vos fonctions ne modifieront rien du le monde extérieur.
Cela veut dire qu'elles ne peuvent pas modifier la valeur d'une variable,
lire du texte entré par un utilisateur,
écrire sur l'écran, lancer un missile.
D'un autre coté, avoir un code parallèle devient très facile.
Haskell rend très clair où les effets apparaissent et où le code est pur.
De plus, il devient beaucoup plus aisé de raisonner sur son programme.
La majorité des bugs seront évités dans les parties pures de votre programme.

En outre, les fonctions pures suivent une loi fondamentale en Haskell:

> Appliquer une fonction avec les mêmes paramètres retourne toujours la même valeur.

_Paresse_

La paresse par défaut est un choix de conception de langage très rare.
Par défaut, Haskell évalue quelque chose seulement lorsque cela est nécessaire.
En conséquence, cela fournit un moyen très élégant de manipuler des structures infinies, par exemple.

Un dernier avertissement sur comment vous devriez lire le code Haskell.
Pour moi, c'est comme lire des papiers scientifiques.
Quelques parties sont très claires, mais quand vous voyez une formule, concentrez-vous dessus et lisez plus lentement.
De plus, lorsque vous apprenez Haskell, cela n'importe _vraiment_ pas si vous ne comprenez pas les détails syntaxiques.
Si vous voyez un `>>=`, `<$>`, `<-` ou n'importe quel symbole bizarre, ignorez-les et suivez le déroulement du code.

<h4 id="function-declaration">Déclaration de fonctions</h4>

Vous avez déjà dû déclarer des fonctions comme cela:

En `C`:

~~~~~~ {.c}
int f(int x, int y) {
    return x*x + y*y;
}
~~~~~~

En JavaScript:

~~~~~~ {.javascript}
function f(x,y) {
    return x*x + y*y;
}
~~~~~~

En Python:

~~~~~~ {.python}
def f(x,y):
    return x*x + y*y
~~~~~~

En Ruby:

~~~~~~ {.ruby}
def f(x,y)
    x*x + y*y
end
~~~~~~

En Scheme:

~~~~~~ {.scheme}
(define (f x y)
    (+ (* x x) (* y y)))
~~~~~~

Finalement, la manière de faire de Haskell est:

~~~~~~ {.haskell}
f x y = x*x + y*y
~~~~~~

Très propre. Aucune parenthèse, aucun `def`.

N'oubliez pas, Haskell utilise beaucoup les fonctions et les types.
C'est très facile de les définir.
La syntaxe a été particulièrement réfléchie pour ces objets.

<h4 id="a-type-example">Un exemple de type</h4>

Même si ce n'est pas obligatoire, les informations de type pour les fonctions sont habituellement déclarées
explicitement. Ce n'est pas indispensable car le compilateur est suffisamment intelligent pour le déduire
à votre place. Cependant, c'est une bonne idée car cela montre bien l'intention du développeur et facilite la compréhension.

Jouons un peu.
On déclare le type en utilisant `::`

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

Maintenant essayez

<div class="codehighlight">
~~~~~~ {.haskell}
f :: Int -> Int -> Int
f x y = x*x + y*y

main = print (f 2.3 4.2)
~~~~~~
</div>
Vous devriez avoir cette erreur:

~~~
21_very_basic.lhs:6:23:
    No instance for (Fractional Int)
      arising from the literal `4.2'
    Possible fix: add an instance declaration for (Fractional Int)
    In the second argument of `f', namely `4.2'
    In the first argument of `print', namely `(f 2.3 4.2)'
    In the expression: print (f 2.3 4.2)
~~~

Le problème est que `4.2` n'est pas de type `Int` (_NDT: Il n'est pas un entier_)

<a href="code/01_basic/10_Introduction/21_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>21_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/22_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>22_very_basic.lhs</strong></a>

La soulution: ne déclarez pas de type pour `f` pour le moment et laissez Haskell inférer le type le plus général pour nous:

<div class="codehighlight">
~~~~~~ {.haskell}
f x y = x*x + y*y

main = print (f 2.3 4.2)
~~~~~~
</div>
Maintenant, ça marche!
Heureursement, nous n'avons pas à déclarer un nouvelle fonction pour chaque type différent.
Par exemple, en `C`, vous auriez dû déclarer un fonction pour `int`, pour `float`, pour `long`, pour `double`, etc...

Mais quel type devons nous déclarer?
Pour découvrir le type que Haskell a trouvé pour nous, lançons ghci:

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

Hein? Quel ce type étrange?

~~~
Num a => a -> a -> a
~~~

Preumièrement, concentrons-nous sur la partie de droite: `a -> a -> a`.
Pour le comprendre, regardez cette liste d'exemples progressifs:

--------------------------------------------------------------------------------------------------------------------------------------
Le&nbsp;type&nbsp;écrit    Son sens
-------------------------- -----------------------------------------------------------------------------------------------------------
`Int`                      Le type `Int`

`Int -> Int`               Le type de la fonction qui prend un `Int` et retourne un `Int`

`Float -> Int`             Le type de la fonction qui prend un `Float` et retourne un `Int`

`a -> Int`                 Le type de la fonction qui prend n'importe quel type de variable et retourne un `Int`

`a -> a`                   Le type de la fonction qui prend n'importe quel type `a` et retourne une variable du même type `a`

`a -> a -> a`              Le type de la fonction qui prend de arguments de n'importe quel type`a` et retourne une variable de type `a`
--------------------------------------------------------------------------------------------------------------------------------------

Dans le type `a -> a -> a`, la lettre `a` est une _variable de type_.
Cela signifie que `f` est une fonction avec deux arguments et que les deux arguments et le résultat ont le même type.
La variable de type `a` peut prendre de nombreuses valeurs différentes
Par exemple `Int`, `Integer`, `Float`...

Donc à la place d'avoir un type forcé comme en `C` et de devoir déclarer une fonction
pour `int`, `long`, `float`, `double`, etc., nous déclarons une seule fonction comme
dans un langage typé de façon dynamique.

C'est parfois appelé le polymorphisme paramétrique. C'est aussi appelé avoir un
gâteau et le manger.

Généralement `a` peut être de n'importe quel type, par exemple un `String` ou un `Int`, mais aussi
des types plus complexes comme `Trees`, d'autres fonctions, etc. Mais ici notre type est
préfixé par `Num a => `.

`Num` est une _classe de type_.
Une classe de type peut être comprise comme un ensemble de types
`Num` contient seulement les types qui se comportent comme des nombres.
Plus précisement, `Num` est une classe qui contient des types qui implémentent une liste spécifique de fonctions,
en particulier `(+)` et `(*)`.

Les classes de types sont une structure de langage très puissante.
Nous pouvons faire des trucs incroyablement puissants avec.
Nous verrons cela plus tard.

Finalement, `Num a => a -> a -> a` signifie:

soit `a` un type qui appartient à la classe `Num`.
C'est une fonction qui prend une variable de type `a` et retourne une fonction de type `(a -> a)`

Oui, c'est étrange.
En fait, en Haskell aucune fonction ne prend réellement deux arguments.
Au lieu de cela toutes les fonctions n'ont qu'un argument unique.
Mais nous retiendrons que prendre deux arguments est équivalent à n'en prendre qu'un et à retourner une fonction qui prend le second argument en paramètre.

Plus précisement `f 3 4` est équivalent à `(f 3) 4 `.
Remarque: `f 3` est une fonction:

~~~
f :: Num a => a -> a -> a

g :: Num a => a -> a
g = f 3

g y ⇔ 3*3 + y*y
~~~

Une autre notation existe pour les fonctions.
La notation lambda nous autorise à créer des fonctions sans leur assigner un nom.
On les appelle des fonctions anonymes.
nous aurions donc pu écrire:

~~~
g = \y -> 3*3 + y*y
~~~

Le `\` esst utilisé car il ressemble à un `λ` et est un caractère ASCII.

Si vous n'êtes pas habitué à la programmation fonctionnelle, votre cerveau devrait commencer à chauffer
Il est temps de faire une vraie application.

<a href="code/01_basic/10_Introduction/22_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>22_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/23_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>23_very_basic.lhs</strong></a>

Mais juste avant cela, nous devrions vérifier que le système de type marche comme nous le supposons:

<div class="codehighlight">
~~~~~~ {.haskell}
f :: Num a => a -> a -> a
f x y = x*x + y*y

main = print (f 3 2.4)
~~~~~~
</div>
Cela fonctionne, car `3` est une représentation valide autant pour les nombres fractionnaires comme Float que pour les entiers.
Comme `2.4` est un nombre fractionnaire, `3` est interprété comme une autre nombre fractionnaire

<a href="code/01_basic/10_Introduction/23_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>23_very_basic.lhs</strong> </a>

<hr/><a href="code/01_basic/10_Introduction/24_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>24_very_basic.lhs</strong></a>

Si nous forçons notre fonction à travailler avec des types différents, le test échouera:

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
Le compilateur se plaint.
Les deux paramètres doivent avoir le même type.

Si vous pensez que c'est une mauvaise idée et que le compilateur devrait faire la transformation
depuis un type à un autre pour vous, vous devriez vraiment regarder cette vidéo géniale (et amusante):
[WAT](https://www.destroyallsoftware.com/talks/wat) (_NDT: En Anglais_)

<a href="code/01_basic/10_Introduction/24_very_basic.lhs" class="cut">01_basic/10_Introduction/<strong>24_very_basic.lhs</strong> </a>

<h2 id="essential-haskell">Notions essentielles</h2>

blogimage("kandinsky_gugg.jpg","Kandinsky Gugg")

Je vous suggère de seulement survoler cette partie
Pensez-y seulement comme à une référence.
Haskell a beaucoup de caractèristiques
Il manque beaucoup d'informations ici.
Revenz ici si la notation vous semble étrange.

J'utilise le symbole `⇔` pour signifier que deux expressions sont équivalentes.
C'est une notation extérieure, `⇔` n'existe pas en Haskell.
Je vais aussi utiliser le symoble `⇒` quelle est la valeur que retourne une fonction.

<h3 id="notations">Notations</h3>

<h5 id="arithmetic">Arithmétique</h5>

~~~
3 + 2 * 6 / 3 ⇔ 3 + ((2*6)/3)
~~~

<h5 id="logic">Logique</h5>

~~~
True || False ⇒ True
True && False ⇒ False
True == False ⇒ False
True /= False ⇒ True  (/=) est l'opérateur pour "différent de"
~~~

<h5 id="powers">Puissances</h5>

~~~
x^n     pour n un entier (comprenez Int ou Integer)
x**y    pour y tout type de nombre (Float par exemple)
~~~

`Integer` n'a aucune limite à part la capacité de votre machine:

~~~
4^103
102844034832575377634685573909834406561420991602098741459288064
~~~

Yeah!
Et aussi les nombres rationnels!
Mais vous avez besoin d'importer le module `Data.Ratio`

~~~
$ ghci
....
Prelude> :m Data.Ratio
Data.Ratio> (11 % 15) * (5 % 3)
11 % 9
~~~

<h5 id="lists">Listes</h5>

~~~
[]                      ⇔ liste vide
[1,2,3]                 ⇔ Liste d'entiers
["foo","bar","baz"]     ⇔ Liste de chaînes de caractères
1:[2,3]                 ⇔ [1,2,3], (:) ajoute un élément au début
1:2:[]                  ⇔ [1,2]
[1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concaténation de deux listes
[1,2,3] ++ ["foo"]      ⇔ ERREUR String ≠ Integral
[1..4]                  ⇔ [1,2,3,4]
[1,3..10]               ⇔ [1,3,5,7,9]
[2,3,5,7,11..100]       ⇔ ERREUR! Je ne suis pas si intelligent!
[10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
~~~

<h5 id="strings">Chaînes de caractères</h5>

En Haskell les chaînes de caractères sont des listes de `Char`.

~~~
'a' :: Char
"a" :: [Char]
""  ⇔ []
"ab" ⇔ ['a','b'] ⇔  'a':"b" ⇔ 'a':['b'] ⇔ 'a':'b':[]
"abc" ⇔ "ab"++"c"
~~~

 > _Remarque_:
 > Dans un vrai code vous n'utiliserez pas des listes de char pour représenter du texte.
 > Vous utiliserez plus souvent `Data.Text` à la place.
 > Si vous voulez représenter un chapelet de caractères ASCII, vous utiliserez `Data.ByteString`.

<h5 id="tuples">Tuples</h5>

Le type d'un couple est `(a,b)`.
Les éléments d'un tuple peuvent avoir des types différents.

~~~
-- tous ces tuples sont valides
(2,"foo")
(3,'a',[2,3])
((2,"a"),"c",3)

fst (x,y)       ⇒  x
snd (x,y)       ⇒  y

fst (x,y,z)     ⇒  ERROR: fst :: (a,b) -> a
snd (x,y,z)     ⇒  ERROR: snd :: (a,b) -> b
~~~

<h5 id="deal-with-parentheses">Traiter avec les parenthèses</h5>

Pour enlever des parenthèses vous pouvez utiliser deux fonctions: `($)` et `(.)`.

~~~
-- Par défaut:
f g h x         ⇔  (((f g) h) x)

-- le $ remplace les parenthèses depuis le $
-- jusqu'à la fin de l'expression.
f g $ h x       ⇔  f g (h x) ⇔ (f g) (h x)
f $ g h x       ⇔  f (g h x) ⇔ f ((g h) x)
f $ g $ h x     ⇔  f (g (h x))

-- (.) permet de faire des compositions de fonctions
(f . g) x       ⇔  f (g x)
(f . g . h) x   ⇔  f (g (h x))
~~~

<hr/><a href="code/01_basic/20_Essential_Haskell/10a_Functions.lhs" class="cut">01_basic/20_Essential_Haskell/<strong>10a_Functions.lhs</strong></a>

<h3 id="useful-notations-for-functions">Notations utiles pour les fonctions</h3>

Juste un mémo:

~~~
x :: Int            ⇔ x est de type Int
x :: a              ⇔ x peut être de n'importe quel type
x :: Num a => a     ⇔ x peut être de n'importe quel type a
                      tant qu' a appartient à la classe de type Num 
f :: a -> b         ⇔ f est une fonction qui prend un a et retourne un b
f :: a -> b -> c    ⇔ f est une fonction qui prend un a et retourne un (b→c)
f :: (a -> b) -> c  ⇔ f est une fonction qui prend un (a→b) et retourne un c
~~~

Rappelez-vous que définir le type d'une fonction avant sa déclaration n'est pas obligatoire.
Haskell infère le type le plus général pour vous.
Mais c'est considéré comme une bonne pratique.

_Notation Infixée_

<div class="codehighlight">
~~~~~~ {.haskell}
square :: Num a => a -> a
square x = x^2
~~~~~~
</div>
Remarquez que `^` utilise une notation infixée.
Pour chaque opérateur infixe il y a une notation préfixée associée.
Vous devz juste l'écrire entre parenthèses.

<div class="codehighlight">
~~~~~~ {.haskell}
square' x = (^) x 2

square'' x = (^2) x
~~~~~~
</div>
Nous pouvons enlever le `x` dans les parties de gauche et de droite!
On appelle cela la η-réduction

<div class="codehighlight">
~~~~~~ {.haskell}
square''' = (^2)
~~~~~~
</div>
Rmarquez qu nous pouvons déclarer des fonctions avec `'` dans leur nom.
Exemples:

 > `square` ⇔  `square'` ⇔ `square''` ⇔ `square'''`

_Tests_

Une implémentation de la fonction absolue.

<div class="codehighlight">
~~~~~~ {.haskell}
absolute :: (Ord a, Num a) => a -> a
absolute x = if x >= 0 then x else -x
~~~~~~
</div>
Remarque: la notation de Haskell pour le `if .. then .. else` ressemble plus
à l'opérateur `¤?¤:¤` en C. Le `else` est obligatoire.

Une version équivalente:

<div class="codehighlight">
~~~~~~ {.haskell}
absolute' x
    | x >= 0 = x
    | otherwise = -x
~~~~~~
</div>
 > Avertissement: l'indentation est _importante_ en Haskell.
 > Comme en Python, une mauvaise indentation peut détruire votre code!

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

<h2 id="hard-part">La Partie Difficile</h2>

La partie difficile peut maintenant commencer.

<h3 id="functional-style">Le style fonctionnel</h3>

blogimage("hr_giger_biomechanicallandscape_500.jpg","Biomechanical Landscape by H.R. Giger")

Dans cette section, je vais vous donner un court exemple de l'impressionante capacité de remaniement de Haskell.
Nous allons sélectionner un problème et le résoudre à la manière d'un langage impératif standard.
Ensuite, je ferais évoluer le code.
Le résultat final sera plus élégant et plus facile à adapter.

résolvons les problèmes suivants:

 > Soit une liste d'entiers, retourner la somme des nombres pairs de cette liste.
 >
 > exemple:
 > `[1,2,3,4,5] ⇒  2 + 4 ⇒  6`

Pour montrer les différences entre les approches fonctionnelle et impérative,
je vais commencer par donner la solution impérative (en JavaScript):

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

En Haskell, en revanche, nous n'avons pas de variables ou un boucle `for`.
Une des solutions pour parvenir au même résultat sans boucles est d'utiliser la récursion.

 > _Remarque_:
 > La récursion est souvent perçue comme lente dans les langages impératifs.
 > Mais ce n'est généralement pas le cas en programmation fonctionnelle.
 > La plupart du temps Haskell gérera les fonctions récursives efficacement.

Voici la version `C` de la fonction récursive.
Remarquez que je suppose que la liste d'int fini avec la première valeur `0`.

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

Gardez ce code à l'esprit. Nous allons le traduire en Haskell.
Premièrement,

~~~~~~ {.haskell}
even :: Integral a => a -> Bool
head :: [a] -> a
tail :: [a] -> [a]
~~~~~~

`even` vérifie si un nombre est pair.

~~~~~~ {.haskell}
even :: Integral a => a -> Bool
even 3  ⇒ False
even 2  ⇒ True
~~~~~~

`head` retourne le premier élément d'une liste:

~~~~~~ {.haskell}
head :: [a] -> a
head [1,2,3] ⇒ 1
head []      ⇒ ERROR
~~~~~~

`tail` retourne tous les éléments d'une liste, sauf le premier:

~~~~~~ {.haskell}
tail :: [a] -> [a]
tail [1,2,3] ⇒ [2,3]
tail [3]     ⇒ []
tail []      ⇒ ERREUR
~~~~~~

Remarquez que pour toute liste non-vide `l`,
 `l ⇔ (head l):(tail l)`

<hr/><a href="code/02_Hard_Part/11_Functions.lhs" class="cut">02_Hard_Part/<strong>11_Functions.lhs</strong></a>

La première solution en Haskell.
La fonction `evenSum` retourne la somme de tous les nombres pairs d'une liste:

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
Pour tester une fonction nous pouvons utiliser `ghci`:

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

Voici un exemple d'exécution[^2]:

[^2]: Je sais que je triche. Mais je parlerais de la non-rigueur plus tard. <!-- IL FAUDRA TROUVER UNE AUTRE TRADUCTION POUR NON-STRICTNESS -->

<pre>
*Main> evenSum [1..5]
accumSum 0 [1,2,3,4,5]
<span class="yellow">1 est impair</span>
accumSum 0 [2,3,4,5]
<span class="yellow">2 est pair</span>
accumSum (0+2) [3,4,5]
<span class="yellow">3 est impair</span>
accumSum (0+2) [4,5]
<span class="yellow">4 est pair</span>
accumSum (0+2+4) [5]
<span class="yellow">5 est impair</span>
accumSum (0+2+4) []
<span class="yellow">l == []</span>
0+2+4
0+6
6
</pre>

En venant d'un langage impératif, tout devrait vous sembler juste.
En fait, beaucoup de choses peuvent être améliorées ici.
Tout d'abord, nous pouvons généraliser le type.

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

Ensuite, nous pouvons utiliser des sous-fonctions grâce à `where` et `let`.
Ansi, notre fonction `accumSum` ne polluera pas le _namespace_ de notre module

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

Puis on utilise le _pattern matching_

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
Qu'est ce que le _pattern matching_ ?
Il s'agit d'utiliser des valeurs au lieu de noms de paramètres généraux.

[^021301]: Pour les plus courageux, une explication plus complète du _pattern matching_ peut être trouvée [ici](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html) (_NdT: En anglais_)

Au lieu d'écrire: `foo l = if l == [] then <x> else <y>`
Vous écrivez tout simplement :

~~~~~~ {.haskell}
foo [] =  <x>
foo l  =  <y>
~~~~~~

Mais le _pattern matching_ peut aller encore plus loin.
Il est également capable d'inspect les données internes d'un valeur complexe.
Nous pouvons ainsi remplacer

~~~~~~ {.haskell}
foo l =  let x  = head l
             xs = tail l
         in if even x
             then foo (n+x) xs
             else foo n xs
~~~~~~

par

~~~~~~ {.haskell}
foo (x:xs) = if even x
                 then foo (n+x) xs
                 else foo n xs
~~~~~~

C'est une caractéristique très utile.
Notre code est ainsi plus concis et plus facile à lire.

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/13_Functions.lhs" class="cut">02_Hard_Part/<strong>13_Functions.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/14_Functions.lhs" class="cut">02_Hard_Part/<strong>14_Functions.lhs</strong></a>

Avec Haskell, nous pouvons simplifier les défitions des fonctions en les _η-réduisant_ .
Par exemple, au lieu d'écrire:

~~~~~~ {.haskell}
f x = (expression) x
~~~~~~

Nous pouvons écrire

~~~~~~ {.haskell}
f = expression
~~~~~~

Utilisons cette méthode pour retirer le `l`:

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

<h4 id="higher-order-functions">Fonctions d'ordre supérieur</h4>

blogimage("escher_polygon.png","Escher")

Pour rendre les choses plus faciles, nous devrions utiliser des fonctions d'ordre supérieur.
Ce sont des fonctions qui prennent des fonctions en paramètres

Voici quelques exemples:

~~~~~~ {.haskell}
filter :: (a -> Bool) -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
foldl :: (a -> b -> a) -> a -> [b] -> a
~~~~~~

Procédons par étapes.

~~~~~~ {.haskell}
-- Version 5
evenSum l = mysum 0 (filter even l)
    where
      mysum n [] = n
      mysum n (x:xs) = mysum (n+x) xs
~~~~~~

où

~~~~~~ {.haskell}
filter even [1..10] ⇔  [2,4,6,8,10]
~~~~~~

La fonction `filter` prend une fonction du type (`a -> Bool`) et une liste de type `[a]`.
Elle retourne une liste qui contient seulement les élements pour qui la fonction  a retourné `True`.

La prochaine étape est d'utiliser une autre technique pour accomplir la même chose qu'une boucle.
Nous allons utiliser la fonction `foldl` pour accumuler une valeur au fur et à mesure que l'on parcoure la liste.
La fonction `foldl` capture un modèle de code général:

<pre>
    myfunc list = foo <span class="blue">initialValue</span> <span class="green">list</span>
    foo accumulated []     = accumulated
    foo tmpValue    (x:xs) = foo (<span class="yellow">bar</span> tmpValue x) xs
</pre>

Qui peut être remplacé par:

<pre>
myfunc list = foldl <span class="yellow">bar</span> <span class="blue">initialValue</span> <span class="green">list</span>
</pre>

Si vous souhaitez vraiment savoir comment la magie se produit, voici la définition de `foldl`:

~~~~~~ {.haskell}
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
~~~~~~

~~~~~~ {.haskell}
foldl f z [x1,...xn]
⇔  f (... (f (f z x1) x2) ...) xn
~~~~~~

Mais comme Haskell est paresseux, il n'évalue pas `(f z x)` et le met simplement dans la pile.
C'est pourquoi on utilise généralement `foldl'`, une version _stricte_ de `foldl`,
Si vous ne comprenez pas encore ce que _paresseux_ ou _strict_ signifie,
ne vous inquiétez pas, suivez le code comme si `foldl'` et `foldl` étaient identiques

Maintenant notre version de `evenSum` devient:

~~~~~~ {.haskell}
-- Version 6
-- foldl' n'est pas accessible par défaut
-- nous devons l'importer depuis le module Data.List
import Data.List
evenSum l = foldl' mysum 0 (filter even l)
  where mysum acc value = acc + value
~~~~~~

Nous pouvons aussi simplifier cela en utilisant une _lambda-notation_.
Ainsi nous n'avons pas besoin de créer le nom temporaire `mySum`.

<div class="codehighlight">
~~~~~~ {.haskell}
-- Version 7
-- Generally it is considered a good practice
-- to import only the necessary function(s)
import Data.List (foldl')
evenSum l = foldl' (\x y -> x+y) 0 (filter even l)
~~~~~~
</div>
Et bien sûr, nous remarquons que

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

Finalement

~~~~~~ {.haskell}
-- Version 8
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum l = foldl' (+) 0 (filter even l)
~~~~~~

`foldl'` n'est pas la fonction la plus facile à prendre en main.
Si vous n'y êtes pas habitué, vous devriez l'étudier un peu.

Pour mieux comprendre ce qui se passe ici, étudions une évaluation étape par étape:

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

Une autr fonction d'ordre supérieur utile est `(.)`.
Elle correspond à une composition en mathématiques.

~~~~~~ {.haskell}
(f . g . h) x ⇔  f ( g (h x))
~~~~~~

Nous pouvons profiter de cet opérateur pour η-réduire notre fonction:

~~~~~~ {.haskell}
-- Version 9
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum = (foldl' (+) 0) . (filter even)
~~~~~~

Nous pouvons maintenant renommer certaines parties pour rendre le tout plus clair:

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
Il est temps de discuter de la direction qu'a pris notre code depuis que nous avons introduit plus d'idiomes fonctionnels.
Que gagnons-nous à utiliser des fonctions d'ordre supérieur?

D'abord, vous pourriez penser que la principale différence est la brièveté. Mais en réalité,
il s'agit d'une meilleure façon de penser. Supposons que nous voulons modifier légèrement notre fonction,
par exemple, pour qu'elle renvoie la somme de tous les carrés pairs des éléments de la liste.

~~~
[1,2,3,4] ▷ [1,4,9,16] ▷ [4,16] ▷ 20
~~~

Mettre la version 10 à jour est très facile:

<div class="codehighlight">
~~~~~~ {.haskell}
squareEvenSum = sum' . (filter even) . (map (^2))
squareEvenSum' = evenSum . (map (^2))
~~~~~~
</div>
Nous avons juste eu à ajouter une autre "fonction de trabsformation"[^0216].

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

La fonction `map` applique simplementune fonction à tous les élements d'une liste.

Nous n'avons rien modifié _à l'intérieur_ de notre définition de fonction.
Cela rend le code plus modulaire.
En plus de cela, vous pouvez penser à votre fonction plus mathématiquement.
Vous pouvez aussi utilier votre fonction avec d'autres, au besoin:
vous pouvez utiliser `compose`, `map`, `fold` ou `filter` sur notre nouvelle fonction.

Modifier la version 1 est laissé comme un exercice pour le lecteur ☺.

Si vous croyez avoir atteint le bout de la généralisation, vous avez tout faux.
Par example, il y a un moyen d'utiliser cette fonction non seulement sur les listes mais aussi sur n'importe quel type récursif.
Si vous voulez savoir comment, je vous suggère de lire cet article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf) (_NDT: en anglais, mais là vous vous en seriez douté je pense ☺_)

Cet exemple montre à quel point la programmation fonctionnelle pure est géniale.
Malheureusement, utiliser cet outil n'est pas adapté à tous les besoins.
Ou alors un langage qui le premettrait n'a pas encore été trouvé.

Une des grands pouvoirs de Haskell est sa capacité à créer des DSLs
(_Domain Specific Language_, en français : _langage spécifique à un domaine_)
Il est ainsi facile de changer le pardigme de programmation

En fait, Haskell peut très bien vous permettre d'écrire des programmes impératifs.
Comprendre cela a été très difficile pour moi lorsque j'apprenais Haskell.
Beaucoup d'efforts tendent à expliquer la supériorité de l'approche fonctionnele.
Puis lorsque vous commencez à utliser le style impératif en Haskell,
Il peut être difficile de comprendre quand et où l'utliser.

Mais avant de parler de ce super-pouvoir de Haskell, nous devons parler
d'un autre aspet essentiel: les _Types_.

<div style="display:none">

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ evenSum [1..10]
~~~~~~
</div>
</div>

<a href="code/02_Hard_Part/16_Functions.lhs" class="cut">02_Hard_Part/<strong>16_Functions.lhs</strong> </a>

<h3 id="types">Les types</h3>

blogimage("salvador-dali-the-madonna-of-port-lligat.jpg","Dali, the madonna of port Lligat")

 > %tldr
 >
 > - `type Name = AnotherType` is just an alias and the compiler doesn't mark any difference between `Name` and `AnotherType`.
 > - `data Name = NameConstructor AnotherType` does mark a difference.
 > - `data` can construct structures which can be recursives.
 > - `deriving` is magic and creates functions for you.

En Haskell, les types sont forts et statiques.

Pourquoi est-ce important? Cela vous aidera a éviter _beaucoup_ d'erreurs.
En Haskell, la majorité des bugs est repérée durant la compilation de votre programme.
Et la raison principale de cela est l'inférence de type durant la compilation.
L'inférence de type permet de détecter plus facilement lorsque vous utilisez le mauvais paramètre au mauvais endroit, par exemple

<h4 id="type-inference">Inférence de type</h4>

Le typage statique est généralement essentiel pour une exécution rapide.
Mais la plupart des langages typés statiquement ont du mal à généraliser des concepts.
La "grâce salvatrice" de Haskell est qu'il peut _inférer_ des types.

Voici un exemple simple, la fonction `square` en Haskell:

~~~~~~ {.haskell}
square x = x * x
~~~~~~

Cette fonction peut mettre au carré n'importe quel type `Numeral`.
Vous pouvez l'utilser avec un `Int`, un `Integer`, un `Float`, un `Fractional` ou même un `Complex`. Preuve par l'exemple:

~~~
% ghci
GHCi, version 7.0.4:
...
Prelude> let square x = x*x
Prelude> square 2
4
Prelude> square 2.1
4.41
Prelude> -- charge le module Data.Complex
Prelude> :m Data.Complex
Prelude Data.Complex> square (2 :+ 1)
3.0 :+ 4.0
~~~

`x :+ y` est la notation pour le complex (<i>x + iy</i>)

Comparons maintenant avec la quantité de code nécessaire pour le faire en C:

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

Pour chaque type, vous avfez besoin d'écrire une nouvelle fonction.
Le seul moyen de se débarsser de se problème est d'utiliser des trucs de méta-programmation, par exemple via le pré-processeur.
en C++ il y a un meilleur moyen, les _templates_:

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

C++ fait un bien meilleur travail que C ici.
Mais pour des fonctions plus complexes, la syntaxe sera difficile à suivre.
Voyez [cet article](http://bartoszmilewski.com/2009/10/21/what-does-haskell-have-to-do-with-c/) pour quelques exemples. (_NDT: toujours en anglais)

En C++ vous devez déclarer qu'un fonction peut marcher avec différents types.
En Haskell, c'est le cas contraire.
La fonction sera aussi générale que possible par défaut.

L'inférence de type donne à Haskell le sentiment de liberté que les langages dyumaniquement typés proposent.
Mais contrairement aux langages dynamiquement typés, la majorité des erreurs est détectée avant de lancer le programme.
Généralement, en Haskell:

 > "Si ça compile, ça fera certainement ce à quoi vous vous attendiez."

<hr/><a href="code/02_Hard_Part/21_Types.lhs" class="cut">02_Hard_Part/<strong>21_Types.lhs</strong></a>

<h4 id="type-construction">Construction de types</h4>

Vous pouvez construire vos propres types.
D'abord, vous pouvez utiliser des alias ou des synonymes de types.

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

Mais cela ne vous protège pas tellement.
Essayez d'inverser les deux paramètres de `showInfos` et lancez le programme:

~~~~~~ {.haskell}
    putStrLn $ showInfos color name
~~~~~~

Le code sera compilé et exécuté.
En fait vous pouvez remplace Name, Color et String n'importe où.
Le compilateur les traitera comme si ils était complétement identiques.

Une autre méthode est de créer vos propres type avec le mot-clé `data`.

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
Maintenant, si vous échangez les paramètres de`showInfos`, le compilateur se plaint!
C'est donc une erreur que vous ne refairez jamais, au suel prix d'être plus bavard.

Remarquez aussi que les constructeurs sont des fonctions:

~~~~~~ {.haskell}
NameConstr  :: String -> Name
ColorConstr :: String -> Color
~~~~~~

La syntaxe de `data` est principalement:

~~~~~~ {.haskell}
data TypeName =   ConstructorName  [types]
                | ConstructorName2 [types]
                | ...
~~~~~~

Généralement on utilise le même nom pour le DatatTypeName et le DataTypeConstructor.

Exemple:

~~~~~~ {.haskell}
data Complex a = Num a => Complex a a
~~~~~~

Vous pouvez également utiliser cette syntaxe:

~~~~~~ {.haskell}
data DataTypeName = DataConstructor {
                      field1 :: [type of field1]
                    , field2 :: [type of field2]
                    ...
                    , fieldn :: [type of fieldn] }
~~~~~~

Et les accesseurs sont définis automatiquement.
En outre, vous pouvez utiliser une autre ordre lorsque vous définissez des valeurs.

Exemple:

~~~~~~ {.haskell}
data Complex a = Num a => Complex { real :: a, img :: a}
c = Complex 1.0 2.0
z = Complex { real = 3, img = 4 }
real c ⇒ 1.0
img z ⇒ 4
~~~~~~

<a href="code/02_Hard_Part/22_Types.lhs" class="cut">02_Hard_Part/<strong>22_Types.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/23_Types.lhs" class="cut">02_Hard_Part/<strong>23_Types.lhs</strong></a>

<h4 id="recursive-type">Type récursif</h4>

Nous avons déjà rencontré un type récursif : les listes.
Nous pourrions re-créer les listes, avec une syntaxe plus bavarde:

~~~~~~ {.haskell}
data List a = Empty | Cons a (List a)
~~~~~~

Si vous voulez réellement utiliser une syntxe plus simple, utilisez un nom infixe pour les constructeurs.

~~~~~~ {.haskell}
infixr 5 :::
data List a = Nil | a ::: (List a)
~~~~~~

Le nombre après `infixr` donne la priorité.

Si vous voulez pouvoir écrire (`Show`), lire (`Read`), tester l'égalite (`Eq`) et comparer (`Ord`) votre nouvelle structure, vous pouvez demander à Haskell de dériver les fonctions appropriées pour vous.

<div class="codehighlight">
~~~~~~ {.haskell}
infixr 5 :::
data List a = Nil | a ::: (List a)
              deriving (Show,Read,Eq,Ord)
~~~~~~
</div>
Quand vous ajoutez `deriving (Show)` à votre déclaration, Haskell crée une fonction `show` pour vous.
Nous verrons bientôt comment utiliser sa propre fonction `show`.

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
Ceci écrit:

~~~
0 ::: (1 ::: Nil)
0 ::: (1 ::: Nil)
~~~

<a href="code/02_Hard_Part/23_Types.lhs" class="cut">02_Hard_Part/<strong>23_Types.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/30_Trees.lhs" class="cut">02_Hard_Part/<strong>30_Trees.lhs</strong></a>

<h4 id="trees">Les arbres</h4>

blogimage("magritte-l-arbre.jpg","Magritte, l'Arbre")

Voici une autre exemple standard : les arbres binaires.

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.List

data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                              deriving (Show)
~~~~~~
</div>
Créons aussi une fonctions qui transforme une liste en un arbre binaire ordonné.

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
~~~~~~
</div>
Remarquez à quel point cette fonction est élégante.
En français:

- une liste vide est convertie en un arbre vide
- une liste `(x:xs)` sera convertie en un arbre où:
  - La racine est `x`
  - Le "sous-arbre" de gauche est l'arbre créé à partir des membres de la liste `xs` strictement inférieurs à `x`
  - Le "sous-arbre" de droite est l'arbre créé à partir des membres de la liste `xs` strictement superieurs à `x`

<div class="codehighlight">
~~~~~~ {.haskell}
main = print $ treeFromList [7,2,4,8]
~~~~~~
</div>
Vious devriez obtenir:

~~~
Node 7 (Node 2 Empty (Node 4 Empty Empty)) (Node 8 Empty Empty)
~~~

C'est représentation de notre arbre informative mais déplaisante.

<a href="code/02_Hard_Part/30_Trees.lhs" class="cut">02_Hard_Part/<strong>30_Trees.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/31_Trees.lhs" class="cut">02_Hard_Part/<strong>31_Trees.lhs</strong></a>

Juste pour le plaisir, codons un meilleur affichage pour nos arbres.
Je me suis simplement amusé à faire une belle fonction pour afficher les arbres de façon générale.
Vous pouvez passer cette partie si vous la trouvez difficile à suivre.

Nous avons quelques changements à faire.
Enlevons le `deriving (Show)` de la déclaration de notre type `BinTree`.
Il serait aussi utile de faire de BinTree une instance de (`Eq` et `Ord`), nous serons ainsi capable de tester l'égalité et de comparer des arbres.

<div class="codehighlight">
~~~~~~ {.haskell}
data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                  deriving (Eq,Ord)
~~~~~~
</div>
Sans le `deriving (Show)`, Haskell ne crée pas de méthode `show` pour nous.
Nous allons créer notre propre version.
Pour accomplir cela, nous devons déclarer que notre type `BinTree a`
est une instance de la classe de type `Show`.
La syntaxe générale est:

~~~~~~ {.haskell}
instance Show (BinTree a) where
   show t = ... -- Déclarez votre fonction ici
~~~~~~

Voici ma version pour afficher un arbre binaire.
Ne vous inquiétez pas de sa complexité apparente.
J'ai fais beaucoup d'améliorations pour afficher même les objets les plus étranges.

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
La méthode `treeFromList` reste identique.

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
~~~~~~
</div>
Et maintenant, nous pouvons jouer:

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
  putStrLn "Int binary tree:"
  print $ treeFromList [7,2,4,8,1,3,6,21,12,23]
~~~~~~
</div>
~~~
Arbre binaire d'Int:
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

Maintenant c'est beaucoup mieux!
La racine est montré en commençant la ligne avec le caractère `<`.
Et chaqeue ligne suivante commence avec un `:`.
Mais nous pourrions aussi utiliser un autre type.

<div class="codehighlight">
~~~~~~ {.haskell}
  putStrLn "\nString binary tree:"
  print $ treeFromList ["foo","bar","baz","gor","yog"]
~~~~~~
</div>
~~~
Arbre binaire de chaïnes de caractères
< "foo"
: |--"bar"
: |  `--"baz"
: `--"gor"
:    `--"yog"
~~~

Commme nous pouvons tester l'égalité et ordonner des arbres,
nous pouvons aussi faire des arbres d'arbres!

<div class="codehighlight">
~~~~~~ {.haskell}
  putStrLn "\nBinary tree of Char binary trees:"
  print ( treeFromList
           (map treeFromList ["baz","zara","bar"]))
~~~~~~
</div>
~~~
Arbre binaire d'arbres binaires de Char:
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

C'est pour cela que j'ai choisi de préfixer chaque ligne par un `:` (sauf pour la racine).

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
Qui est équivalent à

~~~~~~ {.haskell}
print ( treeFromList (
          map treeFromList
             [ map treeFromList ["YO","DAWG"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["YOU","LIKE","TREES"] ]))
~~~~~~

et donne:

~~~
Arbre d'arbres d'arbres de char:
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

Remarquez que les arbres en double ne sont pas insérés.
Il n'y a qu'un seul arbre correspondant à `"I","HEARD"`.
Nous avons ceci presque gratuitement, car nous avons déclaré Tree comme instance de `Eq`.

fr:Voyez à quel point cette structure est formidable:
Nous pouvons faire des arbres contenant seulement des entiers, des chaînes de caractères, mais aussi d'autres arbres.
Et nous pouvons même faire un arbre contenant un arbre d'arbres!

<a href="code/02_Hard_Part/31_Trees.lhs" class="cut">02_Hard_Part/<strong>31_Trees.lhs</strong> </a>

<hr/><a href="code/02_Hard_Part/40_Infinites_Structures.lhs" class="cut">02_Hard_Part/<strong>40_Infinites_Structures.lhs</strong></a>

<h3 id="infinite-structures">Structures infinies</h3>

blogimage("escher_infinite_lizards.jpg","Escher")

On dit souvent que Haskell est _paresseux_.

En fait, si vous êtes un petit peu pédant, vous devriez dire que [Haskell est _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict) (_NDT: En anglais, pour changer_).
La paresse est juste une implémentation commune aux langages non-stricts.

Alors que signifie "non-strict"? D'après le wiki de Haskell:

 > Réduction (terme mathématique pour évaluation) procède depuis l'extérieur.
 >
 > donc si vous avez `(a+(b*c))` alors vous réduisez `+` d'abord, puis vous réduisez `(b*c)`

Par exemple en Haskell vous pouvez faire:

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
Et ça s'arrête.

Comment?

Au lieu d'essayer d'évaluer `numbers` entièrement,
Haskell évalue les éléments seulement lorsque c'est nécessaire.

Remarquez aussi qu'en Haskell, il y a une notation pour les listes infinies

~~~
[1..]   ⇔ [1,2,3,4...]
[1,3..] ⇔ [1,3,5,7,9,11...]
~~~

et que la majorité des fonctions fonctionnera avec ces listes.
Il y a aussi une fonction `take` équivalente à notre `take'`.

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

Supposons que nous ne nous préoccupions pas d'avoir une arbre ordonné.
Voici un arbre binaire infini:

<div class="codehighlight">
~~~~~~ {.haskell}
nullTree = Node 0 nullTree nullTree
~~~~~~
</div>
Un arbre complet où chaque noeud est égal à 0.
Maintenant je vais vous prouver que nous pouvons manipuler cet arbre avec la fonction suivante:

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
Regardez ce qui se passe avec ce programme:

~~~~~~ {.haskell}
main = print $ treeTakeDepth 4 nullTree
~~~~~~

Le code compile, se lance et s'arrête en donnant ce résultat:

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

Pour nous chauffer encore un peu les neurones,
faisons un arbre plus intéressant:

<div class="codehighlight">
~~~~~~ {.haskell}
iTree = Node 0 (dec iTree) (inc iTree)
        where
           dec (Node x l r) = Node (x-1) (dec l) (dec r)
           inc (Node x l r) = Node (x+1) (inc l) (inc r)
~~~~~~
</div>
Un autre moyen de créer cet arbre est d'utiliser une fonction d'ordre supérieur.
Cette fonction devrait être similaire à `map` n, mais devrais travailler sur un `BinTree` au lieu d'une liste.
Voici cette fonction:

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
_NB_: Je ne parlerais pas plus de cette fonction ici.
Si vous vous intéressez à la généralisation de `map`à d'autre structures de données,
cherchez des informations sur les foncteurs et `fmap`.

Notre définition est maintenant:

<div class="codehighlight">
~~~~~~ {.haskell}
infTreeTwo :: BinTree Int
infTreeTwo = Node 0 (treeMap (\x -> x-1) infTreeTwo)
                    (treeMap (\x -> x+1) infTreeTwo)
~~~~~~
</div>
Regardez le résultat pour

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

<h2 id="hell-difficulty-part">Partie de difficulté infernale</h2>

Félicitations pour être allé si loin!
Maitenant, les choses vraiment extrêmes peuvent commencer.

Si vous êtes comme moi, vous avez déjà acqis le style fonctionnel.
Vous devriez également comprendre les avantages de la paresse par défaut.
Mais vous ne comprenez pas vraiment par où commencer pour faire un vrai
programme.
et en particulier:

- Comment s'occuper des effets?
- Pourquoi y a t-il une étrange notation impérative lorsque l'on s'occupe de l'Entrée/Sortie? (E/S, _IO_ en anglais.)

Soyez préparés,  les réponses risquent d'être compliquées.
Mais elles en valent la peine.

<hr/><a href="code/03_Hell/01_IO/01_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>01_progressive_io_example.lhs</strong></a>

<h3 id="deal-with-io">S'occuper de l'E/S (IO)</h3>

blogimage("magritte_carte_blanche.jpg","Magritte, Carte blanche")

 > %tldr
 >
 > Une fonction typique qui fait de l'`IO` ressemble à un programme impératif:
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
 > - Pour définir la valeur d'un objet on utilise `<-` .
 > - Le type de chaque ligne est `IO *`;
 >   dans cet exemple:
 >   - `action1     :: IO b`
 >   - `action2 x   :: IO ()`
 >   - `action3     :: IO c`
 >   - `action4 x y :: IO a`
 >   - `x :: b`, `y :: c`
 > - Quelques objets ont le type `IO a`, cela devrait vous aider à choisir.
 >   En particulier vous ne pouvez pas utiliser de fonctions pures directement ici.
 >   Pour utiliser des fonctions pures vous pourriez faire `action2 (pureFunction x)` par exemple.

Dans cette section, je vais expliquer comment utiliser l'IO, pas comment ça marche.
Vous verrez comment Haskell sépare les parties pures et impures du programme.

Ne vous arrêtez pas sur les détails de la syntaxe
Les réponses viendront dans la section suivante.

Que cherchons-nous à faire?

 > Demander une liste de nombres à l'utilisateur.
 > Afficher la somme de ces nombres.

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
Il devrait être simple de comprendre le comportement de ce programme.
Analysons les types en détails.

~~~
putStrLn :: String -> IO ()
getLine  :: IO String
print    :: Show a => a -> IO ()
~~~

Ou, plus intéressant, on remarque que chaque expression dans le bloc `do` est de type `IO a`.

<pre>
main = do
  putStrLn "Enter ... " :: <span class="high">IO ()</span>
  getLine               :: <span class="high">IO String</span>
  print Something       :: <span class="high">IO ()</span>
</pre>

Nous devrions aussi prêter attention à l'effet du symbole `<-`.

~~~
do
 x <- something
~~~

Si `something :: IO a` alors `x :: a`.

Une autre remarque important sur l'`IO`:
Toutes les lignes d'un bloc `do` doivent être d'une des deux formes:

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

Ces deux types de ligne correspondent à deux différents types de séquençage d'action.
La signification de cette phrase devrait être plus claire à la fin de la prochaine section.

<a href="code/03_Hell/01_IO/01_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>01_progressive_io_example.lhs</strong> </a>

<hr/><a href="code/03_Hell/01_IO/02_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>02_progressive_io_example.lhs</strong></a>

Maintenant voyons comment ce programme se comporte.
Par exemple, que ce passe-t-il si l'utilisateur entre une mauvaise valeur?
Essayons:

~~~
    % runghc 02_progressive_io_example.lhs
    Enter a list of numbers (separated by comma):
    foo
    Prelude.read: no parse
~~~

Argh! Un message d'erreur et un crash!
Notre première amélioration sera de répondre avec un message plus amical.

Pour faire cela, nous devons détecter que quelque chose s'est mal passé.
Voici un moyen de le faire: utiliser le type `Maybe`.
C'est un type très commun en Haskell.

<div class="codehighlight">
~~~~~~ {.haskell}
import Data.Maybe
~~~~~~
</div>
Quelle est cette chose? `Maybe` est un type qui prend un paramètre.
Sa définition est:

~~~~~~ {.haskell}
data Maybe a = Nothing | Just a
~~~~~~

C'est un bon moyen de dire qu'il y a eu une erreur en essayant de créer/évaluer
une valeur.
La fonction `maybeRead` est un bon exemple de cela.
C'est une fonction similaire à `read`[^1],
mais si quelque chose ne va pas la valeur retournée est `Nothing`.
Si la valeur est bonne, la valeur retournée est `Just <la valeur>`.
N'essayez pas trop de comprendre cette fonction.
J'utilise un fonction de plus bas niveau que `read`; `reads`.

[^1]: Qui est elle-même très similaire à la fonction `eval` de javascript, appliquée sur une chaîne contenant du code au format JSON.

<div class="codehighlight">
~~~~~~ {.haskell}
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing
~~~~~~
</div>
Maintenant, pour être plus lisible, on définie une fonction comme ceci:
Si la chaîne a un mauvais format, elle retournera `Nothing`.
Sinon, par exemple pour "1,2,3", cela retournera `Just [1,2,3]`.

<div class="codehighlight">
~~~~~~ {.haskell}
getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"
~~~~~~
</div>
Nous avons juste à tester la valeur dans notre fonction principale.

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
En cas d'erreur, on affiche un joli message.

Remarquez que le type de chaque expresstion dans le bloc `do` de `main` reste de la forme `IO a`.
La seule construction étrange est `error`.
Je dirais juste que `error msg` prend le type nécessaire (ici, `IO ()`).

Un chose très importante à noter est le type de toutes les fonctions définies jusqu'ici.
Il n'y a qu'une seule fonction qui contient `IO` dans son type: `main`.
Cela signifie que `main` est impure.
Mais `main` utilise `getListFromString`, qui est pure.
Nous pouvons donc facilement repérer quelles fonctions sont pures
et lesquelles sont impures, seulement en regardant leurs type.

Pourquoi la pureté a-t-elle de l'importance?
Parmis ses nombreux avantages, en voici trois:

- Il est beaucoup plus facile de penser à du code pur qu'à du code impur.
- La pureté vous protège de tous les bugs difficiles à reproduire qui sont dûs aux effets collatéraux.
- Vous pouvez évaluer des fonctions pures dans n'importe quell ordre ou en parallèle, sans prendre de risques.

C'est pourquoi vous devriez mettre le plus de code possible dans des fonctions pures.

<a href="code/03_Hell/01_IO/02_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>02_progressive_io_example.lhs</strong> </a>

<hr/><a href="code/03_Hell/01_IO/03_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>03_progressive_io_example.lhs</strong></a>

La prochaine étape sera de demader la liste de nombre à l'utilisateur encore et encore jusqu'à ce qu'il entre une réponse valide

Nous gardons la première partie:

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
Maintenant nous créons la fonction qui demandera une liste d'entiers à l'utilisateur
jusqu'à ce que l'entrée soit correcte

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
Cette fonction est de type `IO [Integer]`.
cela signifie que la valeur trouvée est de type `[Integer`] et est le résultat d'actions d'E/S.
Certaines personnes expliqueraient en agitant leurs mains:

 > «C'est un `[Integer]` dans un `IO`»

Si vous voulez comprendre les détails derrière tout cela, vous devrez lire la prochaine section.
MAis si vous voulez seulement _utiliser_ l'E/S, pratiquer juste une peu et rappelez-vous de penser aux types.

Finalement notre fonction `main`est plus simple:

<div class="codehighlight">
~~~~~~ {.haskell}
main :: IO ()
main = do
  list <- askUser
  print $ sum list
~~~~~~
</div>
Nous avons fini notre introduction à l'`IO`.
C'était rapide. Voici les principales choses à se rappeler:

- Dans le bloc `do`, chaque expression doit avoir le type `IO a`.
Vous êtes donc limité dans le nombre d'expression disponibles.
Par exemple, `getLine`, `print`, `putStrLn`, etc...
- Essayez d'externaliser le plus possible les fonctions pures.
- le type `IO a` signifie: un _action_ d'E/S qui retourne un élément de type a.
L'`IO` représente des actions; `IO a` est le type d'une fonction.
Lisez la prochaine section si vous êtes curieux.

Si vous pratiquez un peu, vous devriez être capable d'_utiliser_ l'`IO`.

 > -Exercices_:
 >
 > - Faites un programme qui additionne tous ses arguments. Utilisez la fonction `getArgs`.

<a href="code/03_Hell/01_IO/03_progressive_io_example.lhs" class="cut">03_Hell/01_IO/<strong>03_progressive_io_example.lhs</strong> </a>

<h3 id="io-trick-explained">Le truc des IO révélé</h3>

blogimage("magritte_pipe.jpg","Magritte, ceci n'est pas une pipe")

 > Voici un %tlal pour cette section.
 >
 > Pour séparer les parties pures et impures,
 > `main` est définie comme une fonction.
 > qui modifie l'état du monde.
 >
 > ~~~
 > main :: World -> World
 > ~~~
 >
 > Une fonction aura des effets collatéraux si elle a ce type.
 > Mais regardez cette fonction `main` typique:
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
 > Nous avons beaucoup d'élements temporaires (ici, `w1`, `w2` et `w3`) 
 > qui doivent être passés à la prochauine action.
 >
 > Nous créons une fonction `bind` ou `(>>=)`.
 > Avec `bind` nous n'avons plus besoin de noms temporaires.
 >
 > ~~~
 > main =
 >   action1 >>= action2 >>= action3 >>= action4
 > ~~~
 >
 > Bonus: Haskell a un sucre syntaxique:
 >
 > ~~~
 > main = do
 >   v1 <- action1
 >   v2 <- action2 v1
 >   v3 <- action3 v2
 >   action4 v3
 > ~~~

Pourquoi avons-nous utilisé cette syntaxe étrange, et quel est exactement le type `IO`?
Cela peut sembler un peu magique.

Pour l'instant, oublions les parties pures de notre programme, et concentrons-nous
sur les parties impures:

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

Première remarque: on dirait de l'impératif.
Haskell est assez puissant pour faire sembler impératif du code impur.
Par exemple, si vous le vouliez vous pourriez créer `while` en Haskell.
En fait, pour utiliser les `IO`, le style impératif est généralement plus approprié.

Mais vous devriez avoir remarqué que la notation est inhabituelle.
Voici pourquoi, en détail.

Dans un langage impur, l'état du monde peut être vu comme une énorme variable globale cachée.
Cette variable cachée est accessible par toutes les fonctions du langage/
Par exemple, vous pouvez lire et écrire un fichier dans n'importe quelle fonction.
L'existence hypothétique du fichier est une différence dans les états possibles que le monde peut prendre.

En Haskell cet état n'est pas caché.
Au contraire, il est dit _explicitement_ que `main` est une fonction qui change _potentiellement_ l'état du monde.
Son type est donc quelque chose comme:

~~~~~~ {.haskell}
main :: World -> World
~~~~~~

Les fonctions n'ont pas toutes accès à cette variable.
Celle qui y ont accès sont impures.
Les fonctions qui ne peuvent pas agir sur la variable sont pures[^032001].

[^032001]: Il y a quelques exceptions _peu sûres_ à cette règle. Mais vous ne devriez pas en voir en application réelle, sauf pour le _debugging_.

Haskell considère l'état u monde comme une variable à passer à `main`.
Mais son type réel est plus proche de celui ci[^032002]:

[^032002]: Pour les curieux, le vrai type est `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. Tous les `#` ont rapport avec l'optimisation et j'ai échangé quelques champs dans mon exemple. Mais c'est l'idée de base.

~~~~~~ {.haskell}
main :: World -> ((),World)
~~~~~~

Le type `()` est le type unité.
Rien à voir ici.

Maintenant reécrivons notre fonction `main` avec cela à l'esprit:

~~~~~~ {.haskell}
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (sum list,w1) in
    x
~~~~~~

D'abord, on remarque que toutes les fonctions avec des effets collatéraux doivent avoir le type:

~~~~~~ {.haskell}
World -> (a,World)
~~~~~~

ou `a  est le type du résultat.
Par exemple, un fonction `getChar` aura le type `World -> (Char, World).

Une autre chose à noter est le truc pour corriger l'ordre d'évaluation.
En Haskell, pour évaluer `f a b`, vous avez beaucoup de choix:

- évaluer d'abord `a` puis `b` puis `f a b`
- évaluer d'abord `b` puis `a` puis `f a b`
- évaluer `a` et `b` en même temps puis `f a b`

Ceci est vrai car nous travaillons sur une partie pure du langage.

Maintenant, si vous regardez la fonction `main`, vous voyez tout de suite qu'il faut évaluer la première
ligne avant la seconde, car pour évaluer la seconde ligne vous devez
utliser un parmètre donné par l'évaluation de la première ligne.

Ce truc marche très bien.
Le compilateur donnera à chaque étape un pointeur sur l'id d'un vrai monde.
En réalité, `print` sera évaluée comme:

- Écrit quelque chose sur l'écran
- Modifie l'id du monde
- renvoyer `((), id du nouveau monde)`.

Maintenant, si vous regardez le style de la fonction `main`, c'est clairement peu maniable.
Essayons de fair la même chose avec la fonction `askUser`:

~~~~~~ {.haskell}
askUser :: World -> ([Integer],World)
~~~~~~

Avant:

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

Après:

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

C'est similaire, mais malaisé.
Voyez ces noms temporaires `w?`.

Voici la leçon: l'implémentation naïve des IO dans les langages fonctionnels purs est maladroite!

Heureusement, il ya un meilleur moyen de résoudre ce problème.
Nous voyons un motif.
Chaque ligne est de la forme:

~~~~~~ {.haskell}
let (y,w') = action x w in
~~~~~~

Même si pour quelques lignes l'argument `x` n'est pas nécessaire.
La sortie est un couple, `(answer, newWorldValue)`.
Chaque fonction `f` doit avoir un type similaire à:

~~~~~~ {.haskell}
f :: World -> (a,World)
~~~~~~

Il n'y apas seulement ça, nous pouvons aussi remarquer que nous suivaons toujours le même motif:

~~~~~~ {.haskell}
let (y,w1) = action1 w0 in
let (z,w2) = action2 w1 in
let (t,w3) = action3 w2 in
...
~~~~~~

Chaque action peut prendre de 0 à n paramètres.
Et en particulier, chaque action peut prendre comme paramètre le résultat de la ligne du dessus.

Par exemple, nous pourrions aussi avoir:

~~~~~~ {.haskell}
let (_,w1) = action1 x w0   in
let (z,w2) = action2 w1     in
let (_,w3) = action3 x z w2 in
...
~~~~~~

Et, bien entendu, `actionN w :: (World) -> (a,World)`.

 > IMPORTANT: Il y a seulement 2 patterns importants à considérer:
 >
 > ~~~
 > let (x,w1) = action1 w0 in
 > let (y,w2) = action2 x w1 in
 > ~~~
 >
 > et 
 >
 > ~~~
 > let (_,w1) = action1 w0 in
 > let (y,w2) = action2 w1 in
 > ~~~

leftblogimage("jocker_pencil_trick.jpg","Jocker pencil trick")

Maintenant nous allons faire un tour de magie.
Nous allons faire disparaître le symbole temporaire du monde.
Nous allons `attacher` (_NDT: `bind` en anglais_) les deux lignes.
Définissons la fonction `bind`.
Son type est intimidant au départ:

~~~~~~ {.haskell}
bind :: (World -> (a,World))
        -> (a -> (World -> (b,World)))
        -> (World -> (b,World))
~~~~~~

Mais rappelez-vous que `(World -> (a,World))` est le type d'un action des IO.
Renommons-le pour plus de clareté:

~~~~~~ {.haskell}
type IO a = World -> (a, World)
~~~~~~

Quelques exemples de fonctions:

~~~~~~ {.haskell}
getLine :: IO String
print :: Show a => a -> IO ()
~~~~~~

`getLine` est une action d'E/S qui prend le monde en paramètre et retourne un couple `(String, World)`.
Cela peut être résumé par: `getLine` est de type `IO String`, que nous pouvons voir comme une action 'E/S qui retournera une chaîne de caractères "dans une E/S".

La fonction `print` est elle aussi intéressante.
Elle prend un argument qui peut être montré.
En réalité elle prend deux arguments.
LE premier est la valeur et le deuxième est l'état du monde.
Elle retourne un couple de type `((), World)`.
Cela signifie qu'elle change l'état du monde, mais ne produit pas d'autre donnée.

Ce type nous aide à simplifier le type de `bind`:

~~~~~~ {.haskell}
bind :: IO a
        -> (a -> IO b)
        -> IO b
~~~~~~

Cela dit que `bind` prend deux action d'E/S comme paramètres et retourne une autre action d'E/S.

Maintenant, rappelez-vous des motifs _importants_. Le premier était:

~~~~~~ {.haskell}
let (x,w1) = action1 w0 in
let (y,w2) = action2 x w1 in
(y,w2)
~~~~~~

Voyez les types:

~~~~~~ {.haskell}
action1  :: IO a
action2  :: a -> IO b
(y,w2)   :: IO b
~~~~~~

Cela ne vous semble pas familier?

~~~~~~ {.haskell}
(bind action1 action2) w0 =
    let (x, w1) = action1 w0
        (y, w2) = action2 x w1
    in  (y, w2)
~~~~~~

L'idée est de cacher l'argument `World` avec cette fonction. Allons-y:
Par exemple si nous avions voulu simuler:

~~~~~~ {.haskell}
let (line1,w1) = getLine w0 in
let ((),w2) = print line1 in
((),w2)
~~~~~~

Maintenant, en utilisant la fonction `bind`:

~~~~~~ {.haskell}
(res,w2) = (bind getLine (\l -> print l)) w0
~~~~~~

Comme `print` est de type `(World -> ((),World))`, nous savons que `res = ()` (type nul)
Si vous ne voyez pas ce qui est magique ici, essayons avec trois lignes cette fois.

~~~~~~ {.haskell}
let (line1,w1) = getLine w0 in
let (line2,w2) = getLine w1 in
let ((),w3) = print (line1 ++ line2) in
((),w3)
~~~~~~

Qui est équivalent à:

~~~~~~ {.haskell}
(res,w3) = (bind getLine (\line1 ->
             (bind getLine (\line2 ->
               print (line1 ++ line2))))) w0
~~~~~~

Avez-vous remarqué quelque chose ?
Oui, aucune variable `World` temporaire n'est utilisée!
C'est _MA_._GIQUE_.

Nous pouvons utiliser une meilleure notation.
Utilisons `(>>=)` au lieu de `bind`.
`` est une fonction infixe, comme
`(+)`; pour mémoire: `3 + 4 ⇔ (+) 3 4`

~~~~~~ {.haskell}
(res,w3) = (getLine >>=
           (\line1 -> getLine >>=
           (\line2 -> print (line1 ++ line2)))) w0
~~~~~~

Ho Ho Ho! Joyeux Noël!
fr; HAskell a un sucre syntaxique pour nous:

~~~~~~ {.haskell}
do
  x <- action1
  y <- action2
  z <- action3
  ...
~~~~~~

Est remplacé par:

~~~~~~ {.haskell}
action1 >>= (\x ->
action2 >>= (\y ->
action3 >>= (\z ->
...
)))
~~~~~~

Remarquez que vous pouvez utliser `x` dans `action2` et `x` et `y` dans `action3`

Mais que se passe-t-il pour les lignes qui n'utilisent pas le `<-`?
FAcile, une autre fonction `blindBind`:

~~~~~~ {.haskell}
blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 w0 =
    bind action (\_ -> action2) w0
~~~~~~

Je n'ai pas simplifié cette définition pour plus de clareté.
Bien sûr, nous pouvons utiliser une meilleure notation avec l'opérateur `(>>)`.

Et

~~~~~~ {.haskell}
do
    action1
    action2
    action3
~~~~~~

Est transformé en

~~~~~~ {.haskell}
action1 >>
action2 >>
action3
~~~~~~

Aussi, une autre fonction est réellement utile.

~~~~~~ {.haskell}
putInIO :: a -> IO a
putInIO x = IO (\w -> (x,w))
~~~~~~

C'est le moyen énéral de mettre des valeurs pures dans le "contexte d'E/S".
Le nom général pour `putInIO` est `return`.
C'est un très mauvais nom lorsque vous commencer à programmer en Haskell. `return` est très différent de ce que vous pourriez être habitué à utliser.

<hr/><a href="code/03_Hell/01_IO/21_Detailled_IO.lhs" class="cut">03_Hell/01_IO/<strong>21_Detailled_IO.lhs</strong></a>

Pour finir, traduisons notre exemple:

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

Est traduis en:

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
vous pouvez compiler ce code pour vérifier qu'il marche.

Imaginez à quoi il ressemblerait sans le `(>>)` et `(>>=)`.

<a href="code/03_Hell/01_IO/21_Detailled_IO.lhs" class="cut">03_Hell/01_IO/<strong>21_Detailled_IO.lhs</strong> </a>

<hr/><a href="code/03_Hell/02_Monads/10_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>10_Monads.lhs</strong></a>

<h3 id="monads">Les monades</h3>

blogimage("dali_reve.jpg","Dali, reve. It represents a weapon out of the mouth of a tiger, itself out of the mouth of another tiger, itself out of the mouth of a fish itself out of a grenade. I could have choosen a picture of the Human centipede as it is a very good representation of what a monad really is. But just to think about it, I find this disgusting and that wasn't the purpose of this document.")

Maintenant le secret peut être révélé: `IO` est une _monade_.
Être une monade signifie que vous avez accès à du sucre syntaxique avec la notation `do`.
Mais principalement, vous avez accès à un motif de codage qui tempérera le flux de votre code.

 > **Remarques importantes**:
 >
 > - Le monades n'ont pas forcément quelque chose à voir avec les effets!
 >   Il y a beaucoup de monades _pures_.
 > - Les monades ont plus à faire avec du séquençage.

En Haskell, `Monad` est une classe de type.
Pour être une instance d'une classe de type, vous devez fournir les fonctions `(>>=)` et `return`.
La fonction `(>>)` est dérivée de `(>>=)`.
Voici commment la classe de typr `Monad` est déclarée (basiquement):

~~~~~~ {.haskell}
class Monad m  where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

  (>>) :: m a -> m b -> m b
  f >> g = f >>= \_ -> g

  -- Vous pouvez ignorer cette fonction généralement,
  -- je crois qu'elle existe pour des raisons historiques
  fail :: String -> m a
  fail = error
~~~~~~

 > Remarques:
 >
 > - le mot-clé `class` n'est pas votre ami.
 >   Une classe en Haskell _n'est pas_ du même genre que celle des langages orientés-objet.
 >   Elles ont beaucoup de similarités avec les interfaces de Java.
 >   Un meilleur mot aurait été `typeClass`, ce qui signifierait un ensemble de types. 
 >   Pour qu'un type appartiennent à une classe, toutes les fonctions de cette classe doivent être fournies pour ce type.
 > - Dans cet exemple particulier de classe de type, le type `m` doit être un type qui prend un argument.
 >   par exemple `IO a`, mais aussi `Maybe a`, `[a]`, etc...
 > - Pour être une monade utile, votre fonction doit obéir à quelques règles.
 >   Si votre construction n'obéit pas à ces règles, des choses étranges pourrait se produire:
 >
 >   ~~~
 >   return a >>= k  ==  k a
 >   m >>= return  ==  m
 >   m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 >   ~~~

<h4 id="maybe-monad">Maybe est une monade</h4>

Il y a beaucoup de types différents qui sont des instances de `Monad`.
L'un des plus faciles à décrire est `Maybe`.
Si vous avez une séquence de valeurs `Maybe`, vous pouvez utiliser les monades pour les manipuler.
C'est particulièrement utile pour enlever des constructions `if..then..else..` trop nombreuses.

Imaginez une opération bancaire complexe. Vous êtes éligible pour gagner 700€ seulement si
vous pouvez effectuer une liste d'opérations sans tomber en dessous de zéro.

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

Maintenant, améliorons cela en utilisant le fait que `Maybe` est une Monade.

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

Pas mauvais, mais nous pouvons faire encore mieux:

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
Nous avons prouvé que les monades sont un bon moyen de rendre notre code plus élégant.
Remarquez que cette idée d'organisation de code, en particulier pour `Maybe`, peut être utilisée
dans la plupart des langages impératifs.
En fait, c'est le type de construction que nous faisons naturellement.

 > Un remarque importante:
 > 
 > Le premier élement de la séquence qui sera évalué comme `Nothing` stoppera
 > l'évaluation.
 > Cela signifie que vous n'exécutez pas toutes les lignes.
 > Vous avgfez cela gratuitement, grâce à la paresse.

Vous pourriez aussi revoir ces exemples avec la définition de `(>>=)` pour `Maybe`
en tête:

~~~~~~ {.haskell}
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing  >>= _  = Nothing
    (Just x) >>= f  = f x

    return x = Just x
~~~~~~

La monade `Maybe` a prouvé par un simple exemple qu'elle est utile.
Nous avons vu l'utilité de la monade `IO`.
Mais maintenant, voici un exemple plus cool, les listes.

<a href="code/03_Hell/02_Monads/12_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>12_Monads.lhs</strong> </a>

<hr/><a href="code/03_Hell/02_Monads/13_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>13_Monads.lhs</strong></a>

<h4 id="the-list-monad">La monade List</h4>

blogimage("golconde.jpg","Golconde de Magritte")

La monade List nouis aide à simuler des calculs non-détertiministe.
C'est parti:

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
Ma. GIQUE. :

~~~
[(1,1,7),(1,1,8),(1,1,9),(1,1,10),(1,2,9),(1,2,10)]
~~~

Pour la monade List, il y a aussi un sucre syntaxique:

<div class="codehighlight">
~~~~~~ {.haskell}
  print $ [ (x,y,z) | x <- allCases,
                      y <- allCases,
                      z <- allCases,
                      4*x + 2*y < z ]
~~~~~~
</div>
Je ne listerais pas toutes les monades, mais il y en a beaucoup.
Utiliser les monades simplifie la manipulations de plusieurs notions dans les langages purs.
Les monades sont très utiles, en particulier pour:

- L'E/S;
- calculs non-déterministes,
- générer des nombres pseudo-aléatoires,
- garder un état de configuration,
- écrire un état,
- ...

Si vous m'avez suivi jusqu'ici, alors vous avez terminé!
Vous connaissez les monades[^03021301]!

[^03021301]: Vous aurez quand même besoin de pratiquer un peu pour vous habituer à elles et pour comprendre quand les utiliser ou créer les vôtres. Mais vous avez déjà fait un grand pas dans cette direction.

<a href="code/03_Hell/02_Monads/13_Monads.lhs" class="cut">03_Hell/02_Monads/<strong>13_Monads.lhs</strong> </a>

<h2 id="appendix">Appendice</h2>

Cette section n'est pas vraiment sur l'apprentissage d'Haskell.
Elle est ici pour discuter de quelques détails.

<hr/><a href="code/04_Appendice/01_More_on_infinite_trees/10_Infinite_Trees.lhs" class="cut">04_Appendice/01_More_on_infinite_trees/<strong>10_Infinite_Trees.lhs</strong></a>

<h3 id="more-on-infinite-tree">Revenons sur les arbres infinis</h3>

Dans la section sur [les structures infinies](#infinite-structures) nous avons vu quelques 
constructions simples.
Malheureusement, nous avons enlevé deux propriétés de notre arbre:

1. Pas de valeurs identiques
2. Arbre bien ordonné

Dans cette section nous allons tenter de garder la première propriété.
Concernant la seconde, nous ne devons pas nous en préoccuper ici mais nous discuterons
de comment la garder le plus possible.

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

Notre première étape est de créer une liste de nombres pseudo-aléatoires:

<div class="codehighlight">
~~~~~~ {.haskell}
shuffle = map (\x -> (x*3123) `mod` 4331) [1..]
~~~~~~
</div>
Pour mémoire, voici la définition de `treeFromList`

<div class="codehighlight">
~~~~~~ {.haskell}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList []    = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
~~~~~~
</div>
et 
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
Voyez le résultats de:

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

Le code fonctionne!
Attention cependant, cela marchere seulement si vous avez toujours quelque chose à mettre dans une branche.

Par exemple

~~~~~~ {.haskell}
treeTakeDepth 4 (treeFromList [1..]) 
~~~~~~

tournera en boucle pour toujours.
Simplement parce que le code essayera d'accéder à première valeur de `filter (<1) [2..]`.
Mais `filter` n'est pas assez intelligent pour comprendre que le résultat est une liste vide.

Toutefois, cela reste un exemple sympa de ce qu'un programme non-stricit a à offrir.

Laissé pour exercice au lecteur:

- Prouver l'existence d'un nombre `n` tel que `treeTakeDepth n (treeFromList shuffle)` provoquera une boucle infinie.
- Trouver une borne supérieur `n`.
- Prouver qu'il n(y a pas de liste `shuffle` qui termine le programme pour n'importe quelle profondeur.

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

Pour résoudre ces problèmes nous allons modifier légèrement nos
fonctions `treeFromList` et `shuffle`.

Un premier problème est le manque de nombres différents dans notre immlémentation de `shuffle`.
Nous avons  généré seulement `4331` nombres différents.
Pour résoudre cela nous allons faire un meilleure fonction `shuffle`.

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
Cette fonction à la propriété de ne pas avoir de bornes supérieure ou inférieure.
Mais avoir une meilleure list `shuffle` n'est pas assez pour entrer dans une boucle infinie.

Généralement,  nous ne pouvons pas décider que `filter (<x) xs` est vide.
Donc pour résoudre le problème, je vais autoriser quelques erreurs dans la création de notre arbre binaire.
Cette nouvelle version du code peut créer des arbres binaires qui n'ont pas à suivre les propriétés suivantes pour quelque uns de leurs noeuds:

 > Tous les élements de la branche de gauche doit être strictement inférieur au la valeur racine.

Remarquez que cela donnera _souvent_ un arbre ordonné.
En outre, avec cette construction, chaque noeud est unique dans l'arbre.

Voici notre nouvelle version de `treeFromList`. Nous avons simplement remplacé `filter` par `safefilter`.

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
Cette nouvelle fonction `safefilter` est presque équivalente à `filter` mais n'entre pas dans des boucles infinies si le résultat est une liste finie.
Si elle ne peut pas trouver un élément pour lequel le test est vrai après 10000 étapes consécutives, alors elle considère que la recherche est finie.

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
Maintenant faites tourner le programme et soyez heureux:

<div class="codehighlight">
~~~~~~ {.haskell}
main = do
      putStrLn "take 10 shuffle"
      print $ take 10 shuffle
      putStrLn "\ntreeTakeDepth 8 (treeFromList shuffle)"
      print $ treeTakeDepth 8 (treeFromList $ shuffle)
~~~~~~
</div>
Vous devriez réaliser que le temps nécessaire pour afficher chaque valeur est différent.
C'est parce que Haskell calcule chaque valeur lorsqu'il en a besoin.
Et dans ce cas, il est demandé de l'afficher à l'écran.

Vous pouvez même essayer de remplacer la profondeur de `8` par `100`.
Cela marchera sans tuer votre RAM!
La gestion de la mémoire est faite naturellement par Haskell.

Laissé comme exercices au lecteur:

- Même avec une grande valeur constante pour `deep` et `nbTry`, cela semble marcher correctement. Mais dans le pire des cas, cela peut devenir exponentiel. 
  Créez la pire liste à donner comme paramètre à `treeFromList`.
  _indice_: pensez à (`[0,-1,-1,....,-1,1,-1,...,-1,1,...]`).
- J'ai commencé à implémenter `safefilter` comme ceci:
  <pre>
  safefilter' f l = if filter f (take 10000 l) == []
                    then []
                    else filter f l
  </pre>
  Expliquer pourquoi cela ne fonctionne pas et peut entrer dans une boucle infinie.
- Supposez que `shuffle` est une liste de nombre réellement aléatoires avec de plus en plus de bornes.
  Si vous étudiez un peu cette structure, vous découvrirez qu'elle a toutes les chances
  d'être finie.
  En utilisant le code suivant
  (supposez que nous pouvons utliser `safefilter'` directement comme si cela n'était pas dans le `where` de `safefilter`.
  trouvez une définition de `f` telle que, avec une probabilité de `1`,
  `treeFromList' shuffle` est infinie?. Et prouvez-le.
  Avertissement, ce n'est qu'une conjecture.

~~~~~~ {.haskell}
treeFromList' []  n = Empty
treeFromList' (x:xs) n = Node x left right
    where
        left = treeFromList' (safefilter' (<x) xs (f n)
        right = treeFromList' (safefilter' (>x) xs (f n)
        f = ???
~~~~~~

<a href="code/04_Appendice/01_More_on_infinite_trees/11_Infinite_Trees.lhs" class="cut">04_Appendice/01_More_on_infinite_trees/<strong>11_Infinite_Trees.lhs</strong> </a>

## Remerciements

Merci à [`/r/haskell`](http://reddit.com/r/haskell) et 
[`/r/programming`](http://reddit.com/r/programming).
Vos commentaires étaient plus que bienvenus.

Particulièrement, je voudrais remercier mille fois [Emm](https://github.com/Emm)
pour le temps qu'il a consacré à corriger mon anglais.
Merci beaucoup.
