-----
isHidden:       false
menupriority:   1
kind:           article
created:           2010-02-18T15:29:14+02:00
title: split a file by keyword
authorName: Yann Esposito
authorUri: yannesposito.com
tags: awk, shell, script 
-----

Strangely enough, I didn't find any built-in tool to split a file by keyword. I made one myself in `awk`. I put it here mostly for myself. But it could also helps someone else.
The following code split a file for each line containing the word `UTC`.

<div><code class="perl">
#!/usr/bin/env awk
BEGIN{i=0;}
/UTC/ { 
    i+=1;
    FIC=sprintf("fic.%03d",i); 
} 
{print $0>>FIC}
</code></div>

In my real world example, I wanted one file per day, each line containing UTC being in the following format:

<pre class="twilight">
Mon Dec  7 10:32:30 UTC 2009
</pre>

I then finished with the following code:

<div><code class="perl">
#!/usr/bin/env awk
BEGIN{i=0;}
/UTC/ {
    date=$1$2$3; 
    if ( date != olddate ) {
        olddate=date;
        i+=1;
        FIC=sprintf("fic.%03d",i); 
    }
} 
{print $0>>FIC}
</code></div>
