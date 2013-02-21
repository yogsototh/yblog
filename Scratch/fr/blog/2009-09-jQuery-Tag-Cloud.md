-----
menupriority:   1
kind:           article
published: 2009-09-23
title: jQuery Tag Cloud [en]
author: Yann Esposito
authoruri: yannesposito.com
tags:  jQuery, javascript, web, ruby
-----
Here is how I done the tag cloud of my blog. It is done mostly in jQuery.
All my site is static and pages are generated with [nanoc](nanoc.stoneship.org).
It is (in my humble opinion) the modern geek way to make a website.

This is why I'll give only a Ruby Generator, not a full javascript generator. 
But you can easily translate from Ruby to Javascript.

Here is what you should obtain:

newcorps

<div>
<%= tagCloud %>
</div>

newcorps

# jQuery

Here is the simple jQuery code:

<div>
<code class="javascript">
    $(document).ready( function(){$('.list').hide();} );
    function tagSelected(id) {
        $('.list').hide();
        $('#'+id).fadeIn();
        $('.tag.selected').removeClass('selected');
        $('#tag_'+id).addClass('selected');
    }
</code>
</div>

This code will hide all the div containing links to articles containing the tag. And create a function do show the div containing the tag.

For each tag I create a span element:

<div>
<code class="html">
    <span   style="font-size: 1.0em;" 
            class="tag" 
            onClick="tagSelected('[TAG]')" 
            id="tag_[TAG]">
        [TAG]
    </span> 
</code>
</div>

and a div containing links associtated to this tag:

<div>
<code class="html">
    <div id="[TAG]">
        <h4>[TAG]</h4>
        <ul>
            <li> LINK 1 </li>
            <li> LINK 2 </li>
        </ul>
    </div> 
</code>
</div>

newcorps

# nanoc

Here is how I generate this using nanoc 2.

If you want to make it fully jQuery one, it shouldn't be
too difficult, to use my ruby code and translate it into javascript.

In a first time `tags` correpond of the list of all tags.

<div>
<code class="ruby">
def tags
    return @page.tags.join(', ')
end
</code>
</div>

A function to create a data structure associating to each 
tag its occurence. 

<div>
<code class="ruby">
# generate an hash tag => number of occurence of tag
def tagNumber
    tags={}
    @pages.each do |p|
        if p.tags.nil?
            next
        end
        p.tags.each do |t|
            if tags[t]
                tags[t]+=1
            else
                tags[t]=1
            end
        end
    end
    return tags
end
</code>
</div>

I also need a data structure who associate to each
tag a list of pages (at least url and title).

<div>
<code class="ruby">
# generate an hash tag => [ page1, page2 ... ]
def tagRefs
    tagLinks={}
    @pages.each do |p|
        if p.tags.nil?
            next
        end
        p.tags.each do |t|
            if tagLinks[t].nil?
                tagLinks[t]=[ p ]
            else
                tagLinks[t] <<= p
            end
        end
    end
    return tagLinks
end
</code>
</div>

Calculate the real size of each tag to be displayed.

I choosen not to use the full range of size for all the tag. Because if no
tag has more than `n` (here 10) occurences, then it doesn't deserve to be
of the maximal size.

<div>
<code class="ruby">
def tagRealSize
    tags=tagNumber
    max=tags.values.max
    min=tags.values.min
    # size in CSS em.
    minSize=1.0
    maxSize=2.5
    tagSize={}
    tags.each do |t,n|
        if ( max == min )
            tagSize[t]=minSize
        else
            # normalized value between 0 and 1
            # if not tag appear more than 10 times, 
            # then it cannot have the maximal size
            tagSize[t]=[ ( n - min + 0.0 ) / ( max - min ) , 
                         (n - min) / 10.0 ].min
            # from normalized size to real size
            tagSize[t]=( tagSize[t] ) * (maxSize - minSize) + minSize
        end
    end
    return tagSize
end
</code>
</div>

Finaly a function to generate the XHTML/jQuery code

<div>
<code class="ruby">
# generate an XHTML/jQuery code for tag cloud
def tagCloud
    tagLinks=tagRefs
    tagSize=tagRealSize

    # begin to write the code
    tagCloud=%{<script type="text/javascript">
        $(document).ready( function(){$('.list').hide();} );
        function tagSelected(id) {
            $('.list').hide();
            $('#'+id).fadeIn();
            $('.tag.selected').removeClass('selected');
            $('#tag_'+id).addClass('selected');
        }
    </script><div id="tagcloud">}
    # Creation of the tags <span>
    tagSize.sort{|a,b| a[0].downcase <=> b[0].downcase}.each do |t,s|
        tag_in_id=t.gsub(/\W/,'_')
        # HTML protected version of the tag
        # for example, replace ' ' by '&nbsp;'
        protected=t.gsub(/&/,'&amp;').gsub(/ /,'&nbsp;').gsub(/</,'&lt;').gsub(/>/,'&gt;')
        tagCloud <<= %{
            <span style="font-size: #{s}em;" 
                  class="tag" 
                  onClick="tagSelected('#{tag_in_id}')" 
                  id="tag_#{tag_in_id}">
                #{protected}
            </span> }
    end
    tagCloud <<= %{</div><div id="hiddenDivs" >}
    # Creation of the divs containing links associated to a tag.
    tagLinks.each do |t,l|
        tag_in_id=t.gsub(/\W/,'_')
        tagCloud <<= %{
            <div id="#{tag_in_id}" class="list">
                <h4>#{t}</h4><ul>}
        # generate the link list
        l.each do |p|
            tagCloud <<= %{<li><a href="#{p.path}">#{p.title}</a></li>}
        end
        tagCloud <<= %{</ul></div>}
    end
    tagCloud <<= %{</div>}
    return tagCloud # yeah I know it is not necessary
end
</code>
</div>

You can [download the complete file](/Scratch/en/blog/2009-09-jQuery-Tag-Cloud/code/tag.rb) to put in your 'lib' directory. **Beware, it is a nanoc 2 version, you'll have to make some small changes like replace `@pages` by `@items` to be nanoc3 compatible.**

Of course to be nice you need the associated CSS

<div>
<code class="css">

// Change the color when mouse over
.tag:hover {
  color: #cc0000; }

// Change the color when tag selected
.tag.selected {
  color: #6c0000; }

// a bit of space and pointer cursor
.tag {
  cursor: pointer;
  margin-left: .5em;
  margin-right: .5em; }
</code>
</div>

That's all folks.
