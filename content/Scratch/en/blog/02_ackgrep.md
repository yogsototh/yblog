-----
published: 2009-07-22
theme: brutalist
kind: article
title: Better than Grep
author: Yann Esposito
authoruri: yannesposito.com
menupriority: 1
-----

# update

As [Andy Lester](http://www.theworkinggeek.com) told me [`ack`](http://betterthangrep.com) is a simple file you only have to copy in your `~/bin` folder. Now I've got `ack` on my professional server.

Go on [http://betterthangrep.com](http://betterthangrep.com) to download it.

Sincerely, I don't understand `ack` don't become a common command on all UNIX systems. I can no more live without. For me it is as essential as `which` or `find`.

---

Better than grep
=============================================

One of the my main usage of `grep` is

~~~ {.zsh}
grep 'pattern' **/*(.)
~~~

Most of time it is enough. 
But it is far better with colored output. 
`ack-grep` in Ubuntu does that. 
As I couldn't install it on my 'Evil Company Server',
I had done one myself in very few lines:

~~~
#!/usr/bin/env zsh
(($#<1)) && { print 'usage: ack "regexp"' >&2; exit 1 }

listeFic=( **/*(.) )
autoload zargs
zargs -- $listeFic -- grep $1 | perl -ne 'use Term::ANSIColor;
if (m/([^:]*)(:.*)('$1')(.*)/) {
    print color("green").$1;
    print color("reset").$2;
    print color("black","on_yellow").$3;
    print color("reset").$4."\n";
} '
~~~

For my team  and I it is usable enough.
I hope it could help.

