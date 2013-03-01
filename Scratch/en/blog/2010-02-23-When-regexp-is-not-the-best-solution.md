-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-02-23
title: When regexp is not the best solution
author: Yann Esposito
authoruri: yannesposito.com
tags:  programming, regexp, regular expression, extension, file
-----

Regular expression are really useful. Unfortunately, they are not always the best way of doing things.
Particularly when transformations you want to make are easy.

I wanted to know how to get file extension from filename the fastest way possible. There is 3 natural way of doing this:

<div><code class="ruby">
# regexp
str.match(/[^.]*$/); 
ext=$&

# split
ext=str.split('.')[-1]

# File module
ext=File.extname(str)
</code></div>

At first sight I believed that the regexp should be faster than the split because it could be many `.` in a filename. But in reality, most of time there is only one dot and I realized the split will be faster. But not the fastest way. There is a function dedicated to this work in the `File` module.

Here is the Benchmark ruby code:

<div><code class="ruby" file="regex_benchmark_ext.rb">
#!/usr/bin/env ruby
require 'benchmark'
n=80000
tab=[ '/accounts/user.json',
      '/accounts/user.xml',
      '/user/titi/blog/toto.json',
      '/user/titi/blog/toto.xml' ]

puts "Get extname"
Benchmark.bm do |x|
    x.report("regexp:") { n.times do 
        str=tab[rand(4)]; 
        str.match(/[^.]*$/); 
        ext=$&; 
    end  }
    x.report(" split:") { n.times do 
        str=tab[rand(4)]; 
        ext=str.split('.')[-1] ; 
    end }
    x.report("  File:") { n.times do 
        str=tab[rand(4)]; 
        ext=File.extname(str); 
    end  }
end
</code></div>

And here is the result

<pre class="twilight">
Get extname
            user     system      total        real
regexp:  2.550000   0.020000   2.570000 (  2.693407)
 split:  1.080000   0.050000   1.130000 (  1.190408)
  File:  0.640000   0.030000   0.670000 (  0.717748)
</pre>

Conclusion of this benchmark, dedicated function are better than your way of doing stuff (most of time).

## file path without the extension.

<div><code class="ruby" file="regex_benchmark_strip.rb">
#!/usr/bin/env ruby
require 'benchmark'
n=80000
tab=[ '/accounts/user.json',
      '/accounts/user.xml',
      '/user/titi/blog/toto.json',
      '/user/titi/blog/toto.xml' ]

puts "remove extension"
Benchmark.bm do |x|
    x.report(" File:") { n.times do 
        str=tab[rand(4)]; 
        path=File.expand_path(str,File.basename(str,File.extname(str))); 
    end }
    x.report("chomp:") { n.times do 
        str=tab[rand(4)]; 
        ext=File.extname(str); 
        path=str.chomp(ext); 
    end }
end
</code></div>

and here is the result:

<pre class="twilight">
remove extension
          user     system      total        real
 File:  0.970000   0.060000   1.030000 (  1.081398)
chomp:  0.820000   0.040000   0.860000 (  0.947432)
</pre>

Conclusion of the second benchmark. One simple function is better than three dedicated functions. No surprise, but it is good to know.
