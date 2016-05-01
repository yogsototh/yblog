-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-02-15
title: Expression régulière pour tout sauf quelquechose
author: Yann Esposito
authoruri: yannesposito.com
tags:  regexp, regular expression
-----

Parfois vous ne pouvez simplement pas écrire :

<div><code class="ruby">
if str.match(regexp) and 
    not str.match(other_regexp)
        do_something
</code></div>

et vous devez obtenir le même comportement avec seulement une expression régulière. Le problème c'est que le complémentaire des régulier n'est pas régulier. Celà peut s'avérer impossible.

Cependant, pour certaines expressions ce peut être possible<sup><a href="#note1">&dagger;</a></sup>. Disons que vous souhaitez *matcher* toutes les lignes contenant le mot `bull`, mais que vous ne souhaitez pas matcher `bullshit`. Voici une façon sympa d'y arriver :

<div><code class="ruby">
# matcher toute les chaines qui 
# matchent 'bull' (bullshit compris)
/bull/

# matcher toutes les chaines qui 
# contiennent 'bull' sauf 'bullshit'
/bull([^s]|$)|
bulls([^h]|$)|
bullsh([^i]|$)|
bullshi([^t]|$)/

# une autre façon de l'écrire serait
/bull([^s]|$|s([^h]|$)|sh([^i]|$)|shi([^t]|$))/
</code></div>

Regardons de plus près. Dans la première ligne, l'expression est :
`bull([^s]|$)`, pourquoi avons nous besoin du `$` ?
Parce que sans lui, le mot `bull` ne serait pas matché. Cette expression signifie :

> La chaine finie par `bull`    
> ou,   
> contient `bull` suivi d'une lettre différente de `s`.

Et voilà. J'espère que ça a pu vous aider.

Notez que cette méthode n'est pas toujours la meilleure. Par exemple essayons d'écrire une expression régulière équivalente à l'expression conditionnelle suivante :
<div><code class="ruby">
# Commence avec 'a': ^a
# Se finit par 'a': c$
# Contient 'b': .*b.*
# Mais n'est pas 'axbxc'
if str.match(/^a.*b.*c$/) and 
        not str.match(/^axbxc$/)
    do_something
end
</code></div>

Une solution est :

<div><code class="ruby">
/abc|           # longueur 3
a.bc|           # longueur 4
ab.c|
a[^x]b[^x]c|    # longueur 5
a...*b.*c|      # longueur >5
a.*b...*c/
</code></div>

Cette solution utilise la longueur maximale de la chaine qui ne doit pas être matchée. Il existe certainement d'autres méthodes. Mais la leçon importante c'est qu'il n'est pas naturel d'exclure des solutions d'un expression régulière.

---

<small><a name="note1">&dagger;</a>
Il peut être démontré que tout ensemble régulier privé d'un ensemble fini est aussi régulier.
</small>
