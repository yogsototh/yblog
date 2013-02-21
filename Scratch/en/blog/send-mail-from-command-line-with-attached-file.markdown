-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-08-31
title: send mail from command line with attached file
authorName: Yann Esposito
authorUri: yannesposito.com
tags: email, shell, web
-----

I had to send a mail using only command line. 
I was surprised it isn't straightforward at all.
I didn't had `pine` nor `mutt` or anything like that.
Just `mail` and `mailx`.

What Internet say (via google) is



<pre><code class="zsh">uuencode fic.jpg fic.jpg | mail -s 'Subject'
</code></pre>



I tried it.
And it works almost each times.
But for my file, it didn't worked.
I compressed it to `.gz`, `.bz2` and `.zip`.
Using `.bz2` format it worked nicely, but not with other formats.
Instead of having an attached file I saw this in my email.

<pre>
begin 664 fic.jpg
M(R$O=7-R+V)I;B]E;G8@>G-H"GAL<STD,0H*9F]R(&QI;F4@:6X@)"@\("1X
M;',@*0H@("`@9&-R/20H96-H;R`D;&EN92!\(&%W:R`M1EP[("=[<')I;G0@
...
M93U<(FUO='-<(CX\=F%L=64^/&ET96T@;F%M93U<(F-T>%]M8UPB/BD\=F%L
M=64O/B@\+VET96T^*2-<)#$\=F%L=64^)&ME>7=O<F1S/"]V86QU93Y<)#(C
end
</pre>

Not really readable.
After some research I found the solution.
Use MIME instead of `uuencode`.

Finally I made it manually using `sendmail`.
I didn't dare to use `telnet`.
The command to use is:



<pre><code class="zsh">sendmail -t -oi < mailcontent.txt
</code></pre>



Of course you need to create the `mailcontent.txt` file.
It should contains:

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

And to obtain the "encoded" file in base64 I used:

<code classs="zsh">
uuencode -m fic.jpg fic.jpg
</code>

That is all.
Sometimes technology is so easy to use.
If I need it another time I should consider to make a shell script to automatize this.
