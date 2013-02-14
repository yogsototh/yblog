-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-05-19T22:20:34+02:00
title: How to repair a cutted XML?
subtitle: and how to do it without any parsor?
authorName: Yann Esposito
authorUri: yannesposito.com
tags: tree, HTML, script, ruby
-----

For my main page, you can see, a list of my latest blog entry. And you have the first part of each article. To accomplish that, I needed to include the begining of the entry and to cut it somewhere. But now, I had to repair this cutted HTML.

Here is an example:

<code class="html">
<div class="corps">
    <div class="intro">
        <p>Introduction</p>
    </div>
    <p>The first paragraph</p>
    <img src="/img/img.png" alt="an image"/>
    <p>Another long paragraph</p>
</div>
</code>

After the cut, I obtain:

<code class="html">
<div class="corps">
    <div class="intro">
        <p>Introduction</p>
    </div>
    <p>The first paragraph</p>
    <img src="/img/im
</code>

Argh! In the middle of an `<img>` tag.

In fact, it is not as difficult as it should sound first. The secret is, you don't need to keep the complete tree structure to repair it, but only the list of not closed parents.

Given with our example, when we are after the first paragraph. we only have to close the `div` for class `corps` and the XML is repaired. Of course, when you cut inside a tag, you sould go back, as if you where just before it. Delete this tag and all is ok.

Then, all you have to do, is not remember all the XML tree, but only the heap containing your parents. Suppose we treat the complete first example, the stack will pass through the following state, in order:

<code class="html">
[]           
[div]           <div class="corps">
[div, div]          <div class="intro">
[div, div, p]           <p>
                            Introduction
[div, div]              </p>
[div]               </div>
[div, p]            <p>
                        The first paragraph
[div]               </p>
[div]               <img src="/img/img.png" alt="an image"/>
[div, p]            <p>
                        Another long paragraph
[div]               </p>
[]              </div>
</code>

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

<code class="ruby" file="repair_xml.rb">
# repair cutted XML code by closing the tags
# work even if the XML is cut into a tag.
# example:
#    transform '<div> <span> toto </span> <p> hello <a href="http://tur'
#    into      '<div> <span> toto </span> <p> hello </p></div>'
def repair_xml( xml )
    parents=[]
    depth=0
    xml.scan( %r{<(/?)(\w*)[^>]*(/?)>} ).each do |m|
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
    res=xml.sub(/<[^>]*$/m,'')
    depth-=1
    depth.downto(0).each { |x| res<<= %{</#{parents[x]}>} }
    res
end
</code>

I don't know if the code can help you, but the raisonning should definitively be known.
