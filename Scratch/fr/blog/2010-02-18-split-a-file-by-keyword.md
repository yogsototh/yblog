-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-02-18
title: découper un fichier par mots clés
author: Yann Esposito
authoruri: yannesposito.com
tags:  awk, shell, script
-----

Assez bizarrement, je n'ai trouvé aucun outil UNIX pour découper un fichier par mot clé. 
Alors j'en ai fait un en `awk`. Je le met ici principalement pour moi, mais ça peut toujours servir à quelqu'un d'autre.
Le code suivant découpe un fichier pour chacune de ses ligne contenant le mot `UTC`.

<div><code class="perl">
#!/usr/bin/env awk
BEGIN{i=0;}
/UTC/ { 
    i+=1;
    FIC=sprintf("fic.%03d",i); 
} 
{print $0>>FIC}
</code></div>

En réalité, j'avais besoin de cet outils pour avoir un fichier par jour. Chaque ligne contenant UTC ayant le format suivant :

<pre class="twilight">
Mon Dec  7 10:32:30 UTC 2009
</pre>

J'en suis finallement arrivé au code suivant :

<div><code class="perl">
#!/usr/bin/env awk
BEGIN{i=0;}
/UTC/ {
    date=$1$2$3; 
    if ( date != olddate ) {
        olddate=date;
        i+=1;
        FIC=sprintf("fic.%03d",i); 
    }
} 
{print $0>>FIC}
</code></div>
