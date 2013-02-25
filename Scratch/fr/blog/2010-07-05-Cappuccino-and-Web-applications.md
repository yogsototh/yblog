-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-07-05
title: Cappuccino ou jQuery ?
author: Yann Esposito
authoruri: yannesposito.com
tags:  Cappuccino, iPhone, web, javascript, jQuery, Cocoa, programming
-----

<div class="intro">

<abbr title="Trop long à lire">tlàl</abbr>:

* J'ai essayé de faire une version de [YPassword](http://yannesposito.com/Softwares/YPassword.html) en jQuery et avec Cappuccino.
* Cappuccino est très bien sur les navigateurs non mobile mais l'application pèse 1.4Mo et n'est pas compatible avec l'iPhone.
* la version jQuery n'est pas aussi jolie que la version réalisée avec Cappuccino mais elle pèse seulement 106Ko et est compatible avec l'iPhone.
* J'essayerai Dashcode 3

</div>

---

<div class="intro">

Avant de commencer, je dois dire que je sais que Cappuccino et jQuery ne sont pas plus comparable que Cocoa et la *standard library* en C++. L'un est fait pour créer des interfaces utilisateurs tandis que l'autre est plus une librairie qui aide aux tâches de bas niveaux.
Par contre je les ai utilisé tous les deux pour faire la même application. C'est pourquoi je compare l'expérience que j'ai retenu de chacun pour cette tâche.

</div>

J'ai fait une version web de mon widget [YPassword](http://yannesposito.com/Softwares/YPassword.html).
C'est un simple widget qui permet d'organiser ses mots de passes simplement avec une grande sécurité et de façon portable. 
Ce n'est pas un widget créé pour remplacer le *trousseau d'accès*, mais
plus un générateur de mots de passe.

Le premier a été élaboré à partir du code de mon widget Mac.
Vous pouvez l'essayer [ici](http://yannesposito.com/YPassword.old).
J'ai ensuite fait une version avec [Cappuccino](http://cappuccino.org), que vous pouvez  essayer [ici](http://yannesposito.com/YPassword).

## Que fait ce widget ?

<div class="intro">

Si vous vous moquez de savoir ce que fait mon widget, vous pouvez allez directement à la [section suivante](#cappuccino).

</div>

J'organise mes mots de passe avec une méthode simple.
Je mémorise un mot de passe *maître*. Et mon mot de passe est alors (principalement) : 
<code class="ruby">hash(motDePasseMaitre+NomDeDomaine)</code>

En réalité j'ai besoin d'un plus d'informations pour créer mon mot de passe :

* Un mot de passe maître ;
* une URL ;
* une longeur maximale de mot de passe ;
* le type de sortie (hexadécimale ou base64) ;
* Combien de fois mon mot de passe a dû être changé.

Le mot de passe résultant est calculé comme suit :

<pre><code class="ruby">domainName=domaine_Name_Of_URL(url)
hash=sha1( masterPassword + leakedTimes + domainName )
if ( kind == 'base64' )
    hash=base64(hash)
end
return hash[0..maxlength]
</code></pre>

En fait, selon le site web, on peut avoir des contraintes très différentes :

* longueur minimale ;
* longueur maximale ;
* ne doit pas contenir de caractères spéciaux ;
* doit contenir des caractères spéciaux ;
* etc...

Et si vous souhaitez changer votre mot de passe, le nombre de changement sert à ça.
Toutes les informations peuvent rester publiques sans trop de danger à l'exception du mot de passe principal.

Si vous souhaitez avoir encore plus de détails vous pouvez toujours lire certaines de mes anciens articles de blog (en anglais) :

* [simple portable password management](http://yannesposito.com/YBlog/Computer/Entr%C3%A9es/2008/7/30_Easy%2C_secure_and_portable_password_management_system.html)
* [base64](http://yannesposito.com/YBlog/Computer/Entrées/2009/3/15_Shorter_Password_with_the_same_efficiency.html)
* [change your password](http://yannesposito.com/YBlog/Computer/Entr%C3%A9es/2009/4/11_Be_able_to_change_your_password.html)

## Cappuccino

Tout d'abord je voudrais dire que les applications réalisées avec Cappuccino sont tout simplement incroyables.
C'est comme avoir une application Mac dans son navigateur.

Je dois aussi admettre que j'ai pris du plaisir a écrire mon application avec Cappuccino.
C'est comme programmer une application Mac ou iPhone.
Si vous connaissez bien Cocoa, vous vous sentirez comme *à la maison*.
Si vous ne connaissez pas Cocoa, je vous conseille de vous y intéresser.
Il s'agit vraiment d'un framework excellent pour faire des interfaces utilisateur.
Je ne suis pas un spécialiste de tous les frameworks.
Mais j'ai réalisé des Interfaces Utilisateurs avec les MFC, Java Swing[^1] et WXWindows il y a quelques années.
Et je dois dire que Cocoa est bien meilleurs que tous ces framework.

[^1]: Si ça vous intéresse vous pouvez jeter un coup d'œil à [SEDiL](http://labh-curien.univ-st-etienne.fr/informatique/SEDiL/). Je suis assez fier de la vue automatique des arbres que j'ai programmé sans librairie de départ.

Cappuccino est un framework spécialisé dans le développement d'application web vraiment exceptionnel. Mais il a aussi quelques défauts qui ont surgit lors de l'écriture de mon widget.

Les choses qui m'ont plu :

* Le résultat est vraiment très beau
* C'était très agréable de programmer
* Comme programmer une application Mac
* J'aurai pu utiliser [Interface Builder](http://developer.apple.com/technologies/tools/xcode.html) pour créer l'interface.

Les choses qui ne m'ont pas plu :

* J'ai mis un bon moment avant de comprendre comment récupérer le `onChange` des champs textuels.
* La documentation manquait d'organisation.
* Ça ne marche pas sous iPhone.
* Il a fallu déployer 11Mo.
* Il faut télécharger 1,3Mo pour que l'application se charge dans le navigateur.

Je n'ai pas utilisé les `bindings` parce qu'il me semble qu'ils ne sont pas prêts.

## jQuery

La version jQuery de YPassword n'est pas aussi bien finie que celle de Cappuccino. Simplement parce qu'il n'y a pas de *slider* directement avec jQuery. Il faudrait que j'utilise jQueryUI. Et je pense que l'application deviendrait beaucoup plus lourde pour le coups. En tout cas largement au dessus des 106Ko actuels.

J'ai utilisé le code de mon widget mac en l'adaptant un peu pour faire cette version. C'était relativement facile. Mais jQuery n'est pas un *framework orienté application*. Il s'agit plus d'un *framework pour faire des animations qui la pète*.

[^2]: I don't want to feel like a *troll* I use jQuery to make some *dark side* animation on this blog. But the javascript on my blog is not needed except for commenting.

Je n'ai pas beaucoup plus à dire sur la version jQuery, sinon que programmer avec jQuery était de la programmation de niveau beaucoup plus bas qu'avec Cappuccino.

## En conclusion

Si vous voulez faire une application compatible iPhone n'utilisez pas Cappuccino. Du moins pas encore.
Si vous souhaitez faire un application très simple (comme la mienne), je pense que Cappuccino est un peu trop lourd pour ça.

Si vous souhaitez faire des applications web complexes qui ressemblent à des applications de bureau alors clairement Cappuccino est un très bon choix.
Notez cependant qu'il peut être un peu difficile de débuter.

Finallement, pour terminer la version web de mon widget, j'essayerai Dashcode 3.
Il semblerai que ce soit une bonne alternative pour créer des widget sur le web compatible iPhone.
Je ne sais pas si les applications réalisées avec Dashcode 3 sont compatibles pour les browser n'utilisant pas webkit. Mais si c'est le cas, alors ça pourrait sonner le glas des projets comme Cappuccino et Sproutcore.
