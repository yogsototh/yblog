-----
menupriority:   1
kind:           article
published: 2009-09-22
title: Remplacer tout sauf une partie
author: Yann Esposito
authoruri: yannesposito.com
tags:  ruby, regexp, regular expression
-----

My problem is simple:

I want to filter a text except some part of it. I can match easily the part I don't want to be filtered. For example

<div>
<pre><code class="html">...
text
...
BEGIN not to filter
...
text
...
END not to filter
...
text
...
</code></pre>
</div>

I searched a better way to do that, but the best I can do is using `split` and `scan`.

<div>
<pre><code class="ruby">def allExceptCode( f, content )
    # Beware the behaviour will change if you add
    # parenthesis (groups) to the regexp!
    regexp=/<code[^>]*>.*?<\/code>|<pre[^>]*>.*?<\/pre>/m
    tmp=""
    mem=[]
    content.scan(regexp).each do |c|
        mem <<= c
    end
    i=0
    content.split(regexp).each do |x|
        tmp <<= send(f,x) 
        if not mem[i].nil? 
            tmp <<= mem[i]
            i+=1
        end
    end
    tmp
end
</code></pre>
</div>

An usage is:

<div>
<pre><code class="ruby">def filter(content)
    content.gsub(/e/,'X')
end
...
allExceptCode(:filter, content)
...
</code></pre>
</div>

A better syntax would be:

<div>
<pre><code class="ruby"># !!!!!!!!!! THIS SYNTAX DOES NOT WORK !!!!!!! #
def allExceptCode( f, content )
    regexp=/<code[^>]*>.*?<\/code>/m
    tmp=""
    content.split(regexp).each do |x|
        separator=$&
        tmp <<= send(f,x) 
        if not separator.nil?
            tmp <<= separator
        end
    end
    tmp
end
</code></pre>
</div>

I would expect the split make a search on a regular expression and then give the matched expression into the `$&` variable. But it is not the case.

If someone know a nicer way to do that I will be happy to know how.
