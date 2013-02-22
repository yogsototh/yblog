-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-08-31
title: Envoyer un mail en ligne de commande avec un fichier attaché
author: Yann Esposito
authoruri: yannesposito.com
tags:  email, shell, web
-----

J'ai dû envoyer un mail en ligne de commande récemment. 
Quelle ne fût pas ma surprise lorsque je constatais que ce n'était vraiment pas évident.
Je n'avais ni `pine` ni `mutt`. Seulement `mail` et `mailx`.

Ce qu'on trouve sur internet pour envoyer un mail avec fichier attaché c'est ça :

<pre><code class="zsh">uuencode fic.jpg fic.jpg | mail -s 'Subject'
</code></pre>

Bon, alors, bête et discipliné j'ai essayé. 
Et bien, ça marche _presque_ tout le temps. 
Pour mon fichier ça n'a pas marché du tout. 
Je l'ai compressé au format `.gz`, `.bz2` et `.zip`.
Avec le format `.bz2` le mail reçu avait bien un fichier attaché. 
Mais avec les formats `.gz` et `.zip`, ça ne fonctionnait pas. 
Au lieu d'avoir un fichier attaché j'avais un message qui contenait quelque chose comme :

<pre>
begin 664 fic.jpg
M(R$O=7-R+V)I;B]E;G8@>G-H"GAL<STD,0H*9F]R(&QI;F4@:6X@)"@\("1X
M;',@*0H@("`@9&-R/20H96-H;R`D;&EN92!\(&%W:R`M1EP[("=[<')I;G0@
...
M93U<(FUO='-<(CX\=F%L=64^/&ET96T@;F%M93U<(F-T>%]M8UPB/BD\=F%L
M=64O/B@\+VET96T^*2-<)#$\=F%L=64^)&ME>7=O<F1S/"]V86QU93Y<)#(C
end
</pre>

Pas très lisible. 
Après pas mal de recherche j'ai trouvé la solution.
Le problème c'est `uuencode` qui est une méthode qui devrait devenir obsolète pour envoyer les fichiers. 
Il vaut mieux utiliser le format MIME pour envoyer des fichiers attachés.

Donc finalement le mieux est de faire ça "à la main" avec `sendmail`.
Je n'ai quand même pas utilisé `telnet`.
La commande à lancer est :

<pre><code class="zsh">sendmail -t -oi < mailcontent.txt
</code></pre>

Bien entendu il faut créer le fichier `mailcontent.txt` qui contient :

<pre>
From: from@mail.com
To: to@mail.com
Subject: View the attached file
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary="-"

This is a MIME encoded message. Decode it with "Decoder"
or any other MIME reading software. Decoder is available
at <http://www.etresoft.com>.
---
Content-Type: image/jpeg; name="fic.jpg"
Content-Transfer-Encoding: base64
Content-Disposition: inline; filename="fic.jpg"

H4sICB6Ke0wAA2Rjcl93aXRob3V0X2tleXdvcmQuY3N2ANSdW5ubOJPH7/e7
7Brw+dmrTk8yk7yTSTaZeWd2b/TIIGy6MRAE7ng+/VaJgwF3g522SsxN2+3T
/4eOJamqmARP+yibvI8ykUYim+x5EE2euBfIyd3byZ+fvvzr7svbu8ndTx/f
...
</pre>

Et pour avoir le code il suffit de lancer la commande :

<code classs="zsh">
uuencode -m fic.jpg fic.jpg
</code></pre>

Et voilà. 
Parfois la technique c'est tellement simple.
Si j'en ai besoin encore quelques fois, je pense que j'écrirai un émetteur de mail en shell.
