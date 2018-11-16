-----
isHidden:       false
theme: brutalist
image: /Scratch/img/blog/SVG-and-m4-fractals/main.png
menupriority:   1
kind:           article
published: 2011-10-20
title: Accroître le pouvoir des languages déficients.
subtitle: Fractales en SVG avec m4
author: Yann Esposito
authoruri: yannesposito.com
tags:  m4, SVG, XSLT, XML, design, programmation, fractales
-----
blogimage("main.png","Yesod logo made in SVG and m4")

<div class="intro">

%tlal Utiliser m4 pour accroître le pouvoir d'%xslt et d'%svg. Example cool, les fractales.

</div>

Lorsqu'%xml fût inventé beaucoup pensaient que c'était l'avenir.
Passer de fichiers plat à des fichiers structurés standardisés fût un grand progrès dans beaucoup de domaines.
Cerain se mirent à voir du %xml de partout.
À tel point que les les format compatibles %xml naquirent de toute part.
Non seulement comme format de fichier, mais aussi comme format pour un langage de programmation.

Ô joie !

Malheureusement, %xml fût fabriquer pour le transfert de données.
Pas du tout pour être vu ou édité directement.
La triste vérité est qu'%xml est verbeux et laid.
Dans un monde parfait, nous ne devrions avoir des programmes qui s'occupent de nous afficher correctement le %xml pour nous épargner la peine de les voir directement.
Mais devinez quoi ?
Notre monde n'est pas parfait. 
Beaucoup de programmeurs sont ainsi forcé de travailler
directement avec de l'%xml.

%xml, n'est pas le seul cas de format mal utilisé que je connaisse.
Vous avez d'autres formats dans lesquels il serait très agréable d'ajouter des variables, des boucles, des fonctions...

Mais je suis là pour vous aider.
Si comme moi vous détestez %xslt ou écrire de l'%xml.
Je vais vous montrer une façon d'améliorer tout ça.

## Un exemple avec %xslt

Commençons avec le pire cas de langage %xml que je connaisse : %xslt.
Tous les développeurs qui ont déjà dû écrire du %xslt savent à quel point ce langage est horrible.

Pour réduire la "verbosité" de tels langages, il y a un moyen.
**`m4`**.
Oui, le préprocesseur utilisé par `C` et `C++`.

Voici certains exemples :

- Les variables, au lieu d'écrire `myvar = value`, voici la version <sc>xslt</sc> :

~~~~~~ {.xml}
<xsl:variable name="myvar" select="value"/>
~~~~~~

- Afficher quelquechose. Au lieu de `print "Hello world!"`, <sc>xslt</sc> nous offre :

~~~~~~ {.xml}
<xsl:text 
    disable-output-escaping="yes"><![CDATA[Hello world!
]]></xsl:text>
~~~~~~

- afficher la valeur d'une variable, au lieu de `print myvar`, nous avons droit à :

~~~~~~ {.xml}
<xslt:value-of select="myvar"/>
~~~~~~

- Essayez d'imaginer à quel point il est verbeux de déclarer une fonction dans ce langage.

## La solution (m4 à la rescousse)

~~~~~~ {.xml}
<?xml version="1.0" standalone="yes"?> <!-- YES its %xml -->
<!-- ← start a comment, then write some m4 directives:

define(`ydef',`<xsl:variable name="$1" select="$2"/>')
define(`yprint',`<xsl:text disable-output-escaping="yes"><![CDATA[$1]]></xsl:text>')
define(`yshow',`<xsl:value-of select="$1"/>')

-->
<!-- Yes, %xml sucks to be read -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- And it sucks even more to edit -->
<xsl:template match="/">
    ydef(myvar,value)
    yprint(Hello world!)
    yshow(myvar)
</xsl:template>
~~~~~~

Maintenant compilons simplement ce fichier :

~~~~~~ {.zsh}
m4 myfile.m4 > myfile.xslt
~~~~~~

Et vous pouvez profitez ! Maintenant <sc>xslt</sc> devient plus lisible et plus facile à éditer.

## La partie la plus cool: les fractales !

À ses débuts, beaucoup pensaient que ce serait le nouveau Flash. Apparemment, ce devrait plutôt être `canvas` avec du javascript qui occupera cette place.

Tout d'abord, laissez moi vous montrer le résultat :

<a href="/Scratch/img/blog/SVG-and-m4-fractals/main.svg">
blogimage("main.png","Yesod logo made in SVG and m4")
Cliquez sur l'image pour voir le %svg directement. Attention, si vous n'avez pas un ordinateur récent, ça risque de ramer.
</a>

Le positionnement du texte "esod" par rapport au "λ" renversé a été en jouant avec firebug. De cette façon je n'avais pas à regénérer pour tester.

Faire une telle fractale revient à :

1. Choisir un élément racine ;
2. le dupliquer et le transformer ;
3. le résultat est un nouveau sous-élément ;
4. répéter à partir de 2 mais en utilisant le sous-élément comme nouvelle racine.
5. Arréter lorsque la récursion est assez profonde.

Si j'avais dû faire ça manuellement, il m'aurait fallu faire beaucoup de copier/coller dans mon %svg.
Simplement parce que la transformation est toujours la même, mais je ne pouvais pas dire, utiliser la transformation appelée "titi".
Plutôt que copier du %xml, j'ai utilisé m4.

Et voici le code commenté :

~~~~~~ {.xml}
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!--
     M4 Macros
define(`YTRANSFORMONE', `scale(.43) translate(-120,-69) rotate(-10)')
define(`YTRANSFORMTWO', `scale(.43) translate(-9,-67.5) rotate(10)')
define(`YTRANSFORMTHREE', `scale(.43) translate(53,41) rotate(120)')
define(`YGENTRANSFORM', `translate(364,274) scale(3)')
define(`YTRANSCOMPLETE', `
    <g id="level_$1">
        <use style="opacity: .8" transform="YTRANSFORMONE" xlink:href="#level_$2" />
        <use style="opacity: .8" transform="YTRANSFORMTWO" xlink:href="#level_$2" />
        <use style="opacity: .8" transform="YTRANSFORMTHREE" xlink:href="#level_$2" />
    </g>
    <use transform="YGENTRANSFORM" xlink:href="#level_$1" />
')
 -->
<svg 
    xmlns="http://www.w3.org/2000/svg" 
    xmlns:xlink="http://www.w3.org/1999/xlink"
    x="64" y="64" width="512" height="512" viewBox="64 64 512 512"
    id="svg2" version="1.1">
    <g id="level_0"> <!-- some group, if I want to add other elements -->
        <!-- the text "λ" -->
        <text id="lambda" 
            fill="#333" style="font-family:Ubuntu; font-size: 100px"
            transform="rotate(180)">λ</text>
    </g>
    <!-- the text "esod" -->
    <text 
        fill="#333" 
        style="font-family:Ubuntu; font-size: 28px; letter-spacing: -0.10em" 
        x="-17.3" 
        y="69" 
        transform="YGENTRANSFORM">esod</text>
    <!-- ROOT ELEMENT -->
    <use transform="YGENTRANSFORM" xlink:href="#level_0" />

    YTRANSCOMPLETE(1,0) <!-- First recursion -->
    YTRANSCOMPLETE(2,1) <!-- deeper -->
    YTRANSCOMPLETE(3,2) <!-- deeper -->
    YTRANSCOMPLETE(4,3) <!-- even deeper -->
    YTRANSCOMPLETE(5,4) <!-- Five level seems enough -->
</svg>
~~~~~~

et je l'ai compile en <sc>svg</sc> et ensuite en <sc>png</sc> avec :

~~~~~~ {.zsh}
m4 yesodlogo.m4 > yesodlogo.svg && convert yesodlogo.svg yesodlogo.png
~~~~~~

Le λ est dupliqué avec trois "transformations" différentes. Les transformations sont : `YTRANSFORMONE`, `YTRANSFORMTWO` et `YTRANSFORMTHREE`.

Chaque transformation est une similarité (translation + rotation + zoom, ce qui est équivalent à juste rotation + zoom, mais bon).

Une fois fixée chaque transformation peut ensuite être réutilisée pour chaque nouveau niveau.

Maintenant `YTRANSCOMPLETE` entre en jeu.
Cette macro prend deux arguments.
Le niveau courant et le niveau précédent.
Cette macro va dupliquer le niveau précédent en lui appliquant chacune des 3 transformations.
Au niveau 0, le contenu est un seul grand λ, le niveau 1 en contient 3. Le niveau 2 en contient 9, etc...
Le niveau 5 contient 3<sup>5</sup>=243 λ.
Tous les niveaux combinés représentent 3<sup>6</sup>-1 / 2 = 364 λ.

L'avantage principal c'est que je pouvais visualiser le résultat final facilement.
Sans ce système de macro, pour faire une preview il m'aurait fallu faire des copier/coller + quelques modifications à chaque essai.

## Conclusion

Ce fut très amusant de faire une fractale en <sc>svg</sc>, mais la partie la plus intéressante était d'augmenter la puissance d'expressivité du langage en utilise un préprocesseur.
J'ai utilisé cette méthode avec <sc>xslt</sc> pour une vrai application par exemple.
On peut aussi utiliser m4 pour faire des includes d'autres fichiers.
Typiquement je l'ai utiliser pour les includes dans un format obscur.
Mais vous pouvez aussi le considérer pour des includes dans du HTML.
Par exemple pour fabriquer un site statique rapidement, m4 peut se révéler utile pour inclure un footer ou un menu sur toutes les pages par exemple.
J'ai aussi pensé que l'on pouvait utiliser m4 pour structurer des programmes comme brainfuck.
