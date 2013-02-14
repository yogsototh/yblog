-----
isHidden:       false
menupriority:   1
kind:           article
published:           2009-09-07T20:25:56+02:00
title: ssh to Listen 443 on Snow Leopard
authorName: Yann Esposito
authorUri: yannesposito.com
tags: Apple, mac, ssh, security 
-----
# Surf everywhere as if you were at home

In order to bypass *evil* company firewall and to surf safely on unsafe <sc>wifi</sc>. I keep an ssh server listening on the port 443.

Then from my laptop or my local computer I just have to launch the marvelous

<div>
<code class="zsh">
ssh -p 443 -D 9050 username@host
</code>
</div>

and a local <sc>socks</sc> proxy listening on port 9050 is launched. The <sc>socks</sc> proxy will transfer local requests via the ssh tunnel. Therefore I can surf locally as if I was on my own computer. I can put password and card number without fear the local <sc>wifi</sc> network to be *sniffed*. I simply need to configure my web browser to user the <sc>socks</sc> proxy on localhost and port 9050.

I get this information from [this post](http://dltj.org/article/ssh-as-socks-proxy/).

# Ssh and Snow Leopard(c)

Here I don't want to talk about how great <sc>socks</sc> proxy via ssh tunneling is but how to configure my local server.

I have Mac with Snow Leopard(c) at home and it is far from enough to modify the `/etc/sshd.config` file. The system use `launchd` to launch starting daemons.

I posted the question on [Apple Discussions](discussions.apple.com) in this [discussion thread](http://discussions.apple.com/thread.jspa?messageID=10141032). Thanks to all guys who helped me. And the solution is:

Create the file <tt>/Library/LaunchDaemons/ssh-443.plist</tt> containing:

<div>
<code class="xml" file="ssh-443.plist">
<?xml version="1.0" encoding="UTF-8"?>
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
</code>
</div>

It is a copy of `/System/Library/LaunchDaemons/ssh.plist` with some modifications:

  - the `SockServiceName` from `ssh` to `https`.
  - the `Label` from `com.openssh.sshd` to something not existing as `local.sshd`

Tell me if it was helpfull or if you have any question.
