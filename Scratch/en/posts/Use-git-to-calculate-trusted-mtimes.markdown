-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-09-02
title: Use git to calculate trusted mtimes
authorName: Yann Esposito
authorUri: yannesposito.com
tags: nanoc, web, git
-----

You can remark at the bottom of each page I provide a last modification date.
This label was first calculated using the `mtime` of the file on the file system.
But many times I modify this date just to force some recompilation. 
Therefore the date wasn't a date of _real_ modification.

I use [git](http://git-scm.org) to version my website.
And fortunately I can know the last date of _real_ change of a file.
This is how I do this with [nanoc](http://nanoc.stoneship.org):



<pre><code class="ruby">def gitmtime
    filepath=@item.path.sub('/Scratch/','content/html/').sub(/\/$/,'')
    ext=%{.#{@item[:extension]}}
    filepath<<=ext
    if not FileTest.exists?(filepath)
        filepath.sub!(ext,%{#{@item.raw_filename}#{ext}})
    end
    str=`git log -1 --format='%ci' -- #{filepath}`
    if str.nil? or str.empty?
        return Time.now
    else
        return DateTime.parse( str )
    end
end
</code></pre>



Of course I know it is really slow and absolutely not optimized.
But it works as expected.
Now the date you see at the bottom is exactly the date I modified the _content_ of the page.

_Edit_:
Thanks to Eric Sunshine and Kris to provide me some hints at cleaning my code.
