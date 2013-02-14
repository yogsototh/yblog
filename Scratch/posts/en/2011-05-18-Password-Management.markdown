-----
isHidden:       false
menupriority:   1
kind:           article
created:     2011-05-18T13:14:28+02:00
title: 40 character's passwords
authorName: Yann Esposito
authorUri: yannesposito.com
tags: password, security
-----
blogimage("main.png","Title image")

<div class="intro">

%tldr How I manage safely my password with success for some years now.  
**`sha1( password + domain_name )`**  
I memorize only one password.
I use a different password on all website.

</div>

Disclamer, this is an unashamed attempt to make you download my iPhone app ;-). 
You're always here?
Even if you won't download my app, you should read more.
My method doesn't necessitate my app.
It is both safe and easy to use everyday.

If you just want to _use_ the tools without searching to understand why it is safe, just jump at the [end of this article by clicking here](#in-practice).

## Why you should use a Password Manager?

> Even paranoid could have ennemies.

Imagine you find a really good password. You use it on GMail, Amazon, PayPal, Twitter, Facebook...
One day you see a nice online game you want to try. 
They ask you your email and a password.
Some week passes, and the host machine of this online game is hacked.
Your mail and password is now in bad hands.
Unfortunately for you, you use the same password everywhere. 
Then, the attacker can simply try your password everywhere. 
On PayPal for example.

Well now, how could we fix that?

## Which methodology?

> the good, the bad _&_ the ugly

The mostly used method is to remember a subset of different passwords.
In the best cases, your remember about 13 password.
Some strong, some weak.

What to do if you use more online services 
than your memory can handle?

A _bad_ solution would be to
chose passwords like this:

- twitter: `P45sW0r|)Twitter`
- gmail: `P45sW0r|)gmail`
- badonlinegame: `P45sW0r|)badonlinegame`

Unfortunately, if someone get your password on 
badonlinegame, he could easily find your other passwords.
Of course you can imagine some better transformation. But it is hard to find a very good one.

Fortunately, there exists functions which handle exactly this problem. 
_Hash Function_.
Knowing the result of a hash function, it is difficult to know what was their input.
For example:

<code class="zsh">
hash("P45sW0r|)") = 9f00fd5dbba232b7c03afd2b62b5fce5cdc7df63
</code>

If someone has `9f00fd5dbba232b7c03afd2b62b5fce5cdc7df63`,
he will have hard time to recover `P45sW0r|)`.

Let choose SHA1 as hash function. 
Now the password for any website should 
of the form:

<code lang="zsh">
sha1( master_password + domain_name )
</code>

Where:

- `master_password` is your unique master password,
- `domain_name` is the domain name of the website you want the password for,

---

But what about some website constraint?
For example regarding the length of the password?
What to do if you want to change your password?
What to do if you want number or special characters?
This is why, for each website I need some other parameters:

- the login name
- the password's length,
- the password number (in order to change it),
- The output format: hexadecimal or base64.

## In practice?

Depending on my situation here are the tools I made _&_ use:

- On my Mac: 
  - I use the dashboard widget [YPassword](http://yannesposito.com/Scratch/files/YPassword-1.7.zip)
  - Sometimes, some password field are forbidden to paste into. For time like this, I use this AppleScript made tool: [ForcePaste](http://yannesposito.com/Scratch/files/forcePaste.app.zip). 
- On my Linux Box: I use the script [ypassword](http://github.com/yogsototh/getpass)
- On my iPhone: I use the [YPassword app](http://itunes.apple.com/WebObjects/MZStore.woa/wa/viewSoftware?id=436268354&mt=8)
- On any other computer:
  - [Cappuccino Made YPassword](http://yannesposito.com/Scratch/en/softwares/ypassword/web/) Web application
  - [jQuery Made YPassword](http://yannesposito.com/Scratch/en/softwares/ypassword/iphoneweb/) Web application

My password are at a copy/paste on all environment I use. I have some services for which I have password of 40 characters. 
Now I use 10 character for most of my passwords.
Further more using shorter password make it even harder for an attaquer to retrieve my master password.

I would be happy to hear your thoughts on using this methodology.
