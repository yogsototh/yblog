-----
isHidden:       false
theme: brutalist
image: /img/img.png
menupriority:   1
kind:           article
published: 2010-05-19
title: Comment réparer un XML coupé ?
subtitle: et comment s'en sortir sans parseur ?
author: Yann Esposito
authoruri: yannesposito.com
tags:  arbre, HTML, script, ruby
-----

Sur ma page d'accueil vous pouvez voir la liste des mes derniers articles avec le début de ceux-ci. Pour arriver à faire ça, j'ai besoin de couper le code XHTML de mes pages en plein milieu. Il m'a donc fallu trouver un moyen de les réparer.

Prenons un exemple :

~~~~~~ {.html}
<div class="corps">
    <div class="intro">
        <p>Introduction</p>
    </div>
    <p>The first paragraph</p>
    <img src="/img/img.png" alt="an image"/>
    <p>Another long paragraph</p>
</div>
~~~~~~

Après avoir coupé, j'obtiens :

~~~~~~ {.html}
<div class="corps">
    <div class="intro">
        <p>Introduction</p>
    </div>
    <p>The first paragraph</p>
    <img src="/img/im
~~~~~~

En plein milieu d'un tag `<img>` !

En réalité, ce n'est pas si difficile que celà peut paraître au premier abord. Le secret réside dans le fait de comprendre que l'on n'a pas besoin de conserver la structure complète de l'arbre pour le réparer, mais seulement la liste des parents non fermés.

Pour notre exemple, juste après le paragraphe `first paragraph` nous n'avons qu'à fermer un `div` pour la classe `corps` et le XML est réparé. Bien entendu, quand on est dans le cas où un tag est coupé au milieu, on a qu'à remonté juste avant le début de ce tag corrompu.

Donc, tout ce que nous avons à faire, c'est d'enregistrer la liste des parents dans une pile. Supposons que nous traitions le premier exemple complètement. La pile passera par les états suivants :

~~~~~~ {.html}
[]           
[div]           <div class="corps">
[div, div]          <div class="intro">
[div, div, p]           <p>
                            Introduction
[div, div]              </p>
[div]               </div>
[div, p]            <p>
                        The first paragraph
[div]               </p>
[div]               <img src="/img/img.png" alt="an image"/>
[div, p]            <p>
                        Another long paragraph
[div]               </p>
[]              </div>
~~~~~~

L'algorithme est alors très simple :
~~~~~~ {.html}
let res be the XML as a string ; 
read res and each time you encouter a tag: 
    if it is an opening one: 
        push it to the stack
    else if it is a closing one: 
        pop the stack.

remove any malformed/cutted tag in the end of res
for each tag in the stack, pop it, and write:
    res = res + closed tag

return res
~~~~~~

Et `res` contiend le XML réparé.

Finallement, voici le code en ruby que j'utilise. La variable `xml` contient le XML coupé.

~~~~~~ {.ruby}
# repair cutted XML code by closing the tags
# work even if the XML is cut into a tag.
# example:
#    transform '<div> <span> toto </span> <p> hello <a href="http://tur'
#    into      '<div> <span> toto </span> <p> hello </p></div>'
def repair_xml( xml )
    parents=[]
    depth=0
    xml.scan( %r{<(/?)(\w*)[^>]*(/?)>} ).each do |m|
        if m[2] == "/"
            next
        end
        if m[0] == "" 
            parents[depth]=m[1]
            depth+=1
        else
            depth-=1
        end
    end
    res=xml.sub(/<[^>]*$/m,'')
    depth-=1
    depth.downto(0).each { |x| res<<= %{</#{parents[x]}>} }
    res
end
~~~~~~

Je ne sais pas si ce code pourra vous être utile. Par contre le raisonnement pour y parvenir mérite d'être connu.
