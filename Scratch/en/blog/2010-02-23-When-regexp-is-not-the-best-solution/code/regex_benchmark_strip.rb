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
