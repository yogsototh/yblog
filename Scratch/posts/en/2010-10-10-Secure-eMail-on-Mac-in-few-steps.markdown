-----
isHidden:       false
menupriority:   1
kind:           article
created:     2010-10-10T18:39:00+02:00
title: Secure eMail on Mac in few steps
authorName: Yann Esposito
authorUri: yannesposito.com
tags: security, S/MIME, email, mac
-----

blogimage("main.png","Title image","clean")

<div class="intro">

%tldr _on Mac_

- Get a certificate signed by a CA: [click here for a free one](http://www.instantssl.com/ssl-certificate-products/free-email-certificate.html),
- open the file,
- delete securely the file,
- use Mail instead of online gmail.
- ???
- Profit

</div>

I've (re)discovered how to become S/MIME compliant. 
I am now suprised how easy it was. 
Some years ago it was far more difficult.
Now I'm able to sign and encrypt my emails.

## Why is it important?

Signing: it tell the other with an aboslute certitude the writer of the mail is _you_ or at least used your computer.

Encrypt: because sometimes you need to be 100% sure a conversation remains private.

## How to proceed?

- Get a certificate signed by a CA: [click here to get a free one](http://www.instantssl.com/ssl-certificate-products/free-email-certificate.html),
- open the file,
- empty your trash, put the file in the trash, secure empty trash,
- use Mail instead of online gmail.
  Now you should see these icons: 
  blogimage("sign_icon.png","Sign icon")

_n.b._: if you use gmail, you and work not alway with a Mac, you should consider to try the [gmail S/MIME firefox addon](https://addons.mozilla.org/firefox/addon/592).
