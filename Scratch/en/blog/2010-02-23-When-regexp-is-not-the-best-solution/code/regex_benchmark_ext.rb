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
