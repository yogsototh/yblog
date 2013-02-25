-----
menupriority:   1
kind:           article
published: 2009-09-07
title: ssh sur le port 443 avec Snow Leopard
author: Yann Esposito
authoruri: yannesposito.com
tags:  Apple, mac, ssh, securité
-----

# Surfez partout comme si vous étiez chez vous

Que ce soit pour surfer en toute sécurité depuis un accès <sc>wifi</sc> non sécurisé ou pour contourner les parefeux diaboliques des entreprises. J'ai configuré un serveur ssh écoutant sur le port 443 chez moi.

Ensuite de mon portable ou de mon ordinateur local, je dois simplement lancé la merveilleuse commande :

<div>
<pre><code class="zsh">ssh -p 443 -D 9050 username@host
</code></pre>
</div>

et un proxy <sc>socks</sc> écoute sur le port 9050. Ce proxy <sc>socks</sc> transférera toutes les requêtes locales via le tunnel ssh. Ainsi je peux surfer en local comme si je naviguais depuis mon ordinateur à la maison. Je peux écrire mon numéro de carte bleu sans avoir peur que le <sc>wifi</sc> local soit *sniffé*. Je dois simplement configurer mon navigateur web pour utiliser le proxy <sc>socks</sc> sur  `localhost` écoutant le port 9050.

J'ai eu cette information à partir de [cet article](http://dltj.org/article/ssh-as-socks-proxy/).

# Ssh et Snow Leopard(c)

J'ai un Mac avec Snow Leopard(c) à la maison. 
Il ne suffit pas de modifier le fichier `/etc/sshd.config` pour changer le port d'écoute d'`sshd`.
Le système utilise `launchd` pour lancer les démons.

J'ai posé cette question sur [Apple Discussions](discussions.apple.com) dans ce [fil de discussion](http://discussions.apple.com/thread.jspa?messageID=10141032). 
Merci à tous ceux qui m'ont aidé. Et la solution est :

Créer un fichier <tt>/Library/LaunchDaemons/ssh-443.plist</tt> contenant :

<div>
<pre><code class="xml" file="ssh-443.plist"><?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>Disabled</key>
	<false/>
	<key>Label</key>
	<string>local.sshd</string>
	<key>Program</key>
	<string>/usr/libexec/sshd-keygen-wrapper</string>
	<key>ProgramArguments</key>
	<array>
		<string>/usr/sbin/sshd</string>
		<string>-i</string>
	</array>
	<key>Sockets</key>
	<dict>
		<key>Listeners</key>
		<dict>
			<key>SockServiceName</key>
			<string>https</string>
		</dict>
	</dict>
	<key>inetdCompatibility</key>
	<dict>
		<key>Wait</key>
		<false/>
	</dict>
	<key>StandardErrorPath</key>
	<string>/dev/null</string>
        <key>SHAuthorizationRight</key>
        <string>system.preferences</string>
</dict>
</plist>
</code></pre>
</div>

C'est une copie de `/System/Library/LaunchDaemons/ssh.plist` avec quelques modifications :

  - le `SockServiceName` est devenu `https` au lieu de `ssh`
  - le `Label` est passé de `com.openssh.sshd` à quelque chose qui n'existait pas comme `local.sshd`

Encore une fois j'espère que ça a pu être utile.
