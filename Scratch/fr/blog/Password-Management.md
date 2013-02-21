-----
isHidden:       false
menupriority:   1
kind:           article
published: 2011-05-18
title: Password Management
author: Yann Esposito
authoruri: yannesposito.com
tags:  password, security
-----
blogimage("main.png","Title image")

begindiv(intro)

%tlal Une méthode de gestion des mots de passes que j'utilise avec succès depuis quelques années.  
**`sha1( mot_de_passe + nom_de_domaine )`**  
Je ne mémorise qu'un seul mot de passe de très bonne qualité.
J'utilise des mots de passe différents sur tous les sites.

enddiv

Avant de commencer, je tiens à préciser qu'il s'agit d'une tentative de vous vendre mon appli iPhone ;-).

Vous êtes toujours là ?
Bon, d'accord, même si vous ne téléchargez pas mon application vous pouvez quand même utiliser ma méthode.
Elle est à la fois très sûre et simple à utiliser.

Si vous souhaitez simplement _utiliser_ le système sans essayer de comprendre comment ça marche derrière vous pouvez [aller à la fin de cet article en cliquant ici](#en-pratique-).

## Pourquoi devriez-vous utiliser un gestionnaire de mot de passe ?

> Même les paranoïaques peuvent avoir des ennemis.

Imaginez que vous trouviez un très bon mot de passe. Vous l'utilisez sur gmail, amazon, PayPal, twitter, facebook...
Plus tard, vous découvrez un super petit jeu en ligne très sympa.
Vous devez vous enregistrer pour y jouer.
Le site vous demande votre email et un mot de passe.
Quelques semaines/mois se passent.
La machine qui héberge le jeu en ligne se fait attaquer.
Maintenant, l'attaquant du site web possède votre email avec ce mot de passe.
Il peut donc essayer votre mot de passe un peu partout. 
Sur PayPal par exemple.

Bien, maintenant comment pouvons nous régler ce problèmes ?

## Quelle méthodologie ?

> Le bon, la brute et le truand

La méthode la plus courante est de se souvenir de plusieurs mot de passes différents. 
En général, si vous avez bonne mémoire vous pouvez mémoriser jusqu'à 13 mots de passes. Certain de bonne qualité, d'autre moins.

Que faire si vous utilisez plus de services que vous pouvez mémoriser de mots de passe ?

Un _mauvaise_ solution peut être de choisir ses 
mots de passes de la façon suivante :

- twitter: `P45sW0r|)Twitter`
- gmail: `P45sW0r|)gmail`
- badonlinegame: `P45sW0r|)badonlinegame`

Malheureusement, si quelqu'un récupère votre mot de passe sur 
`badonlinegame`, il peut facilement retrouvez vos autres mots de passe.
Bien sûr, on peut imaginer des transformation de mots de passe de meilleure qualité. 
Mais il est très difficile d'en trouver une suffisamment bonne.

Fort heureusement, il existe une des fonctions bien connues dans le milieu de la sécurité informatique et qui résolvent précisément ce problème.
Il s'agit des _fontions de hachages_.
Il est difficile de retrouver le paramètre d'entrée d'une fonction de hachage à partir de son résultat.
Prenons un exemple : 

Si quelqu'un possède `9f00fd5dbba232b7c03afd2b62b5fce5cdc7df63`,
il va avoir de grande difficulté pour retrouver `P45sW0r|)`.

Choisisson la fonction de hashage `sha1`.
Connaissant celà, le mot de passe d'un site donné doit avoir la forme :

Où :

- `master_password` est votre unique mot de passe _maître_ ;
- `domain_name` est le nom de domaine du site pour lequel vous voulez le mot de passe.

---

Il faut aussi penser à certaines contraintes. 
Certains site web veulent des mots de passe d'une certaine longueur, ni trop longs ni trop courts.
Que faire si vous voulez changez votre mot de passe ? Soit parce qu'il est compromis ou simplement parce qu'on vous impose de le changer.
C'est pouquoi pour chaque site on a besoin de quelques paramètres supplémentaires.

- le nom de login ;
- la longueur du mot de passe ;
- le numéro du mot de passe (pour le changer au cas où) ;
- le format du mot de passe : hexadécimal ou base64.

## En pratique ?

Selon ma situation, voici les outils que j'ai fait et que j'utilise :

- Sur mon Mac : 
  - J'utilise le widget [YPassword](http://yannesposito.com/Scratch/files/YPassword-1.7.zip)
  - Parfois, certains champs de mots passe interdisent la copie.  Dans ce cas, j'utilise un petit utilitaire en AppleScript : [ForcePaste](http://yannesposito.com/Scratch/files/forcePaste.app.zip). 
- Sous mon Linux : J'utilise le script [ypassword](http://github.com/yogsototh/getpass)
- Sur mon iPhone : J'utilise l'application [YPassword](http://itunes.apple.com/WebObjects/MZStore.woa/wa/viewSoftware?id=436268354&mt=8)
- Sur tous les autres ordinateurs :
  - L'application Web Cappuccino [YPassword](http://yannesposito.com/Scratch/en/softwares/ypassword/web/)
  - L'application Web jQuery [YPassword](http://yannesposito.com/Scratch/en/softwares/ypassword/iphoneweb/)

Quelquesoit mon environnement de travail, tous mes mots de passes sont à un copier/coller.
Pour certain services, j'utilise des mots de passe de 40 caractères.
Actuellement j'utilise plutôt des mots de passes de 10 caractères.
Avec des mots de passes plus petit, il est encore plus difficile pour un attaquant de retrouver mon mot de passe principal.

Je serai heureux de savoir ce que vous pensez de cette méthode. Alors n'hésitez pas à laisser un commentaire ou à m'envoyer un mail.
