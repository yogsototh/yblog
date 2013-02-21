-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-10-10
title: Sécurisez vos emails
author: Yann Esposito
authoruri: yannesposito.com
tags:  securité, courriel, S/MIME, email, mac
-----

blogimage("main.png","Title image","clean")

begindiv(intro)

%tlal _avec un Mac_ 

- Récupérez un certificat signé par une AC: [cliquez ici pour un certificat gratuit](http://www.instantssl.com/ssl-certificate-products/free-email-certificate.html) ;
- ouvrez le fichier ;
- supprimer le fichier en mode sécurisé ;
- utilisez Mail plutôt que l'interface web de gmail.

enddiv

J'ai (re)découvert comment adoptez la norme S/MIME. 
J'ai été surpris de voir à quel point ce fut aisé.
Il y a seulement quelques années c'était bien plus difficile à accomplir.
Maintenant je peux signer et chiffrer mes mails.

## Pourquoi est-ce important ?

Signer : cela permet de certifier avec une absolue certitude que la personne qui a écrit le mail est _vous_ ou au moins qu'elle a utilisé votre ordinateur.

Chiffrer : parce que parfois il est nécessaire d'être certain qu'une conversation reste privée.

## Comment procéder ?

- Récupérez un certificat signé par une authorité de certification : [cliquez ici pour récupérer un certificat gratuit](http://www.instantssl.com/ssl-certificate-products/free-email-certificate.html) ;
- ouvrez le fichier ;
- supprimer le fichier en mode sécurisé ;
- utilisez Mail plutôt que l'interface web de gmail.
  Maintenant vous devriez voir ces icônes : 
  blogimage("sign_icon.png","icône signature")

_n.b._ : si vous utilisez gmail et que vous ne travaillez pas toujours avec un Mac, vous devriez considérer d'utiliser le [module gmail S/MIME](https://addons.mozilla.org/firefox/addon/592) de firefox.
