-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-05-24
title: Arbres ; Pragmatisme et Formalisme
subtitle: Quand la théorie est plus pratique que la pratique
author: Yann Esposito
authoruri: yannesposito.com
tags:  XML, Perl, programmation, arbre, théorie, mathématiques, regexp, script
-----

<div class="intro">

%tlal :

- J'ai essayé de programmer un simple filtre ;
- J'ai été bloqué pendant deux jours ;
- J'ai arrêté de penser comme un robot ;
- J'ai utilisé un papier et un stylo ;
- J'ai fait un peu de maths ;
- J'ai résolu le problème en 10 minutes ;
- Conclusion: Pragmatisme n'est pas : &laquo;n'utilisez jamais la théorie&raquo;.
</div>

## Résumé (plus long que le  %tlal)

Je devais résoudre un problème à mon travail. Au début cela
semblait assez facile. J'ai donc commencé à programmer
tout de suite. Je suis alors entré dans un cercle infernal d'essais
et de réparations. Voilà à quoi ressemblait cet étrange état
de boucle infini :

>   -- Plus que ça a réparer et ça devrait être bon.  
>   -- Très bien, maintenant ça doit marcher.  
>   -- Oui !!  
>   -- Ah mince! J'ai oublié ce détail...  
>   `répéter jusqu'à la mort`

Après deux jours à me prendre pour [Sisyphe](http://fr.wikipedia.org/wiki/Sisyphe), je me suis arrêté pour repenser le problème.
J'ai pris un stylo et une feuille de papier. Je me suis souvenu de de ce que j'avais appris sur les arbres pendant mon doctorat.
Finalement, le problème fut résolu en moins de 20 minutes.

Je pense que la leçon à retenir de cette expérience est de se souvenir que la méthodologie la plus efficace pour résoudre ce problème *pragamtique* était la méthode *théorique*. 
Ça ne signifie pas que la méthode théorique est toujours la meilleure, mais en tout cas, il ne faut pas l'écarter.

---

# L'anecdote

Apparemment 90% des programmeurs sont incapable de programmer une recherche binaire sans faire de bug.
L'algorithme est pourtant connu et facile à comprendre.
Cependant, il est difficile à programmer sans bug.
J'ai participé à [ce concours](http://reprog.wordpress.com/2010/04/19/are-you-one-of-the-10-percent/).
Vous pouvez voir les [résultats ici](http://reprog.wordpress.com/2010/04/21/binary-search-redux-part-1/)[^1].
J'ai dû faire face à un problème similaire à mon travail.
Il paraissait simple au départ.
Transformer un <sc>xml</sc> d'un format à un autre.

[^1]: Normalement, je fais parti des 10% qui ont fourni une implémentation sans bug.

Voici le format général du <sc>xml</sc> source :

~~~~~~ {.xml}
<rubrique>
    <contenu>
        <tag1>value1</tag1>
        <tag2>value2</tag2>
        ...
    </contenu>
    <enfant>
        <rubrique>
            ...
        </rubrique>
        ...
        <rubrique>
            ...
        </rubrique>
    </enfant>
</menu>
~~~~~~

et le format d'arrivé est celui-ci :

~~~~~~ {.xml}
<item name="Menu0">
    <value>
        <item name="menu">
            <value>
                <item name="tag1">
                    <value>value1</value>
                </item>
                <item name="tag2">
                    <value>value2</value>
                </item>
                ...
                <item name="menu">
                    <value>
                        ...
                    </value>
                    <value>
                        ...
                    </value>
                </item>
            </value>
        </item>
    </value>
</item>
~~~~~~

À première vue, cela m'a paru simple. J'étais certain de pouvoir y arriver en me fixant les règles suivantes :

1. ne pas utiliser <sc>xslt</sc> ;
2. ne pas utiliser de parseur <sc>xml</sc> ;
3. résoudre le problème en utilisant un simple script perl

Vous pouvez essayer si vous le souhaitez. Si vous attaquez ce problème directement en écrivant le programme, ce ne sera certainement pas si simple.
Je peux le dire, parce que c'est ce que j'ai fait.
Et je dois dire que j'ai perdu une journée de travail complète en m'y prenant de la sorte.
En réalité, il y avait pas mal de petits détails dont je ne parle pas qui m'ont induis en erreur et qui m'ont fait perdre encore plus de temps.

Pourquoi étais-je incapable de résoudre ce problème si simple en aparence ?

Voici comment je m'y suis pris :

1. Réfléchir
2. Écrire le programme
3. Essayer le programme
4. Vérifier les résultats
5. Trouver un bug
6. Résoudre le bug
7. Reprendre à l'étape 3

Il s'agissait d'une méthode de travail standard pour un ingénieur en informatique. L'erreur venait de la première étape.
J'ai d'abord pensé à comment résoudre le problème mais avec des yeux d'*ingéinieur pragmatique*. Je me suis simplement dit :

> Ça à l'air de pouvoir se résouvre avec un petit script de *search&amp;replace* en perl
> Commençons à écrire le code maintenant.

C'est la deuxième phrase qui est complètement fausse. Parce que j'avais mal commencé et que cette méthodologie de travail ne fonctionne pas lorsque l'on part vraiment mal.

## Réfléchir

Après un certain temps, j'ai arrêté de programmer et je me suis dit : &laquo;Maintenant, ça suffit !&raquo;.
J'ai pris une feuille et un stylo et j'ai commencé à dessiner des arbres.

J'ai commencer par simplifier un peu en enlevant le maximum de verbiage.
Tout d'abord en renommant `<item name="Menu">` par un simple `M` par exemple.
J'ai obtenu quelque chose comme :

<graph title="The source tree">
    subgraph cluster_x {
        node [label="C"] C_x ;
        node [label="E"] E_x ;
        node [label="a1"] tag1_x ;
        node [label="a2"] tag2_x ;
        node [label="R", color="#333333", fillcolor="#333333", fontcolor="white"] R_x ;
        R_x -> C_x;
        C_x -> tag1_x ;
        C_x -> tag2_x ;
        R_x -> E_x ;
    }
    subgraph cluster_y {
        node [label="C"] C_y ;
        node [label="E"] E_y ;
        node [label="a1"] tag1_y ;
        node [label="a2"] tag2_y ;
        node [label="R", color="#333333", fillcolor="#333333", fontcolor="white"] R_y ;
        R_y -> C_y;
        C_y -> tag1_y ;
        C_y -> tag2_y ;
        R_y -> E_y ;
    }
    subgraph cluster_z {
        node [label="C"] C_z ;
        node [label="E"] E_z ;
        node [label="a1"] tag1_z ;
        node [label="a2"] tag2_z ;
        node [label="R", color="#333333", style="filled", fillcolor="#333333", fontcolor="white"] R_z ;
        R_z -> C_z;
        C_z -> tag1_z ;
        C_z -> tag2_z ;
        R_z -> E_z ;
    }
    E_x -> R_y ;
    E_x -> R_z ;

</graph>

et

<graph title="The destination tree">
    subgraph cluster_x {
        node [label="M"] E_x ;
        node [label="a1"] tag1_x ;
        node [label="V"] value_tag1_x ;
        node [label="a2"] tag2_x ;
        node [label="V"] value_tag2_x ;
        node [label="V", color="#333333", fillcolor="#333333", fontcolor="white"] R_x ;
        R_x -> value_tag1_x -> tag1_x ;
        R_x -> value_tag2_x -> tag2_x ;
        R_x -> E_x ;
    }
    subgraph cluster_y {
        node [label="M"] E_y ;
        node [label="a1"] tag1_y ;
        node [label="V"] value_tag1_y ;
        node [label="a2"] tag2_y ;
        node [label="V"] value_tag2_y ;
        node [label="V", color="#333333", fillcolor="#333333", fontcolor="white"] R_y ;
        R_y -> value_tag1_y -> tag1_y ;
        R_y -> value_tag2_y -> tag2_y ;
        R_y -> E_y ;
    }
    subgraph cluster_z {
        node [label="M"] E_z ;
        node [label="a1"] tag1_z ;
        node [label="V"] value_tag1_z ;
        node [label="a2"] tag2_z ;
        node [label="V"] value_tag2_z ;
        node [label="V", color="#333333", fillcolor="#333333", fontcolor="white"] R_z ;
        R_z -> value_tag1_z -> tag1_z ;
        R_z -> value_tag2_z -> tag2_z ;
        R_z -> E_z ;
    }
    E_x -> R_y ;
    E_x -> R_z ;

</graph>

Puis, je me suis fait la réflexion suivante :

Dans les distances d'éditions sur les arbres, chaque opération atomique correspond à un simple *search and replace* sur mon fichier <sc>xml</sc> source[^nb].
On considère trois opérations atomiques sur les arbres :

 - *substitution*: renommer un nœud
 - *insertion*: ajouter un nœud
 - *délétion*: supprimer un nœud

[^nb]: J'ai programmé un outil qui calcule automatiquement le poids de chaque élément des matrices d'édition à partir de données.

Une des particularité avec les transformations sur les arbres est celle-ci : 
supprimer un nœud et tous ses enfants deviendront les enfants du père de ce nœud.

Un exemple:

<pre class="twilight">
r - x - a
  \   \
   \    b
    y - c   
</pre>

Si vous supprimez le nœud `x`, vous obtenez

<pre class="twilight">
    a
  /
r - b
  \
    y - c   
</pre>

Et regardez ce que ça implique quand on l'écrit en <sc>xml</sc> :

~~~~~~ {.xml}
<r>
  <x>
    <a>value for a</a>
    <b>value for b</b>
  </x>
  <y>
    <c>value for c</c>
  </y>
</r>
~~~~~~

Alors supprimer tous les nœuds `x` revient à faire passer le <sc>xml</sc> à travers le filtre suivant :

~~~~~~ {.perl}
s/<\/?x>//g
~~~~~~

Par conséquent, s'il existe un transducteur déterministe à un état qui permet de transformer mes arbres ; 
je suis capable de transformer le <sc>xml</sc> d'un format à l'autre en utilisant une simple liste de *search and replace*.

# Solution

Transformer cet arbre :

<pre class="twilight">
R - C - tag1
  \   \
   \    tag2
    E -- R - C - tag1
      \   \    \
       \   \     tag2
        \    E ...
         R - C - tag1 
           \    \
            \     tag2
             E ...
</pre>

en celui-ci :

<pre class="twilight">
                tag1
              /
M - V - M - V - tag2      tag1
              \         / 
                M --- V - tag2
                  \     \ 
                   \      M
                    \     tag1
                     \  / 
                      V - tag2
                        \ 
                          M
</pre>

peut-être fait en utilisant le transducteur déterministe à un état suivant: 

>    C -> &epsilon;  
>    E -> M  
>    R -> V  

Ce qui peut-être traduit par les simples directives Perl suivantes :

~~~~~~ {.perl}
s/C//g
s/E/M/g
s/R/V/g
~~~~~~

Une fois adapté au <sc>xml</sc> cela devient :

~~~~~~ {.perl}
s%</?contenu>%%g
s%<enfant>%<item name="menu">%g
s%</enfant>%</item>%g
s%<rubrique>%<value>%g
s%</rubrique>%</value>%g
~~~~~~

Et c'est tout.

# Conclusion

Même si cela peut sembler paradoxal, parfois la solution la plus efficace à un problème pragmatique est d'utiliser une méthodologie théorique.
