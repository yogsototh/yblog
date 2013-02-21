-----
menupriority:   1
kind:           article
published: 2009-10-23
title: lancer un démon en ligne de commande
author: Yann Esposito
authoruri: yannesposito.com
tags:  zsh, shell, script, tip
-----

Une petite astuce dont je ne me souvient jamais (je ne sais pas pourquoi).

Lorsque que vous souhaitez lancer une commande qui ne soit pas tuée après la fermeture du terminal voici comment s'y prendre : 

<div><code class="zsh">
nohup cmd &
</code>
<small>où <code>cmd</code> est la commande que vous souhaitez lancer.</small>
</div>

Je laisse cette astuce ici pour moi et dans l'espoir que ça pourra aussi être utile à quelqu'un d'autre.
