-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-05-19
title: How to repair a cutted XML?
subtitle: and how to do it without any parsor?
authorName: Yann Esposito
authorUri: yannesposito.com
tags: tree, HTML, script, ruby
-----

For my main page, you can see, a list of my latest blog entry. And you have the first part of each article. To accomplish that, I needed to include the begining of the entry and to cut it somewhere. But now, I had to repair this cutted HTML.

Here is an example:



<pre><code class="html">&lt;div class="corps"&gt;
    &lt;div class="intro"&gt;
        &lt;p&gt;Introduction&lt;/p&gt;
    &lt;/div&gt;
    &lt;p&gt;The first paragraph&lt;/p&gt;
    &lt;img src="/img/img.png" alt="an image"/&gt;
    &lt;p&gt;Another long paragraph&lt;/p&gt;
&lt;/div&gt;
</code></pre>



After the cut, I obtain:



<pre><code class="html">&lt;div class="corps"&gt;
    &lt;div class="intro"&gt;
        &lt;p&gt;Introduction&lt;/p&gt;
    &lt;/div&gt;
    &lt;p&gt;The first paragraph&lt;/p&gt;
    <img src="/img/im
</code></pre>



Argh! In the middle of an `<img>` tag.

In fact, it is not as difficult as it should sound first. The secret is, you don't need to keep the complete tree structure to repair it, but only the list of not closed parents.

Given with our example, when we are after the first paragraph. we only have to close the `div` for class `corps` and the XML is repaired. Of course, when you cut inside a tag, you sould go back, as if you where just before it. Delete this tag and all is ok.

Then, all you have to do, is not remember all the XML tree, but only the heap containing your parents. Suppose we treat the complete first example, the stack will pass through the following state, in order:



<pre><code class="html">[]           
[div]           &lt;div class="corps"&gt;
[div, div]          &lt;div class="intro"&gt;
[div, div, p]           &lt;p&gt;
                            Introduction
[div, div]              &lt;/p&gt;
[div]               &lt;/div&gt;
[div, p]            &lt;p&gt;
                        The first paragraph
[div]               &lt;/p&gt;
[div]               &lt;img src="/img/img.png" alt="an image"/&gt;
[div, p]            &lt;p&gt;
                        Another long paragraph
[div]               &lt;/p&gt;
[]              &lt;/div&gt;
</code></pre>



The algorihm, is then really simple: 
<code class="html">
let res be the XML as a string ; 
read res and each time you encouter a tag: 
    if it is an opening one: 
        push it to the stack
    else if it is a closing one: 
        pop the stack.

remove any malformed/cutted tag in the end of res
for each tag in the stack, pop it, and write:
    res = res + closed tag

return res
</code>

And `res` contain the repaired XML.

Finally, this is the code in ruby I use. The `xml` variable contain the cutted XML.



<pre><code class="ruby"># repair cutted XML code by closing the tags
# work even if the XML is cut into a tag.
# example:
#    transform '&lt;div&gt; &lt;span&gt; toto &lt;/span&gt; &lt;p&gt; hello &lt;a href="http://tur'
#    into      '<div&gt; &lt;span&gt; toto &lt;/span&gt; &lt;p&gt; hello &lt;/p&gt;&lt;/div&gt;'
def repair_xml( xml )
    parents=[]
    depth=0
    xml.scan( %r{&lt;(/?)(\w*)[^&gt;]*(/?)>} ).each do |m|
        if m[2] == "/"
            next
        end
        if m[0] == "" 
            parents[depth]=m[1]
            depth+=1
        else
            depth-=1
        end
    end
    res=xml.sub(/&lt;[^&gt;]*$/m,'')
    depth-=1
    depth.downto(0).each { |x| res&lt;<= %{</#{parents[x]}&gt;} }
    res
end
</code></pre>



I don't know if the code can help you, but the raisonning should definitively be known.
