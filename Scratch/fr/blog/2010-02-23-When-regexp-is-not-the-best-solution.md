-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-02-23
title: Quand se passer des expressions régulières ?
author: Yann Esposito
authoruri: yannesposito.com
tags:  programmation, regexp, expressions régulières, extension, fichier
-----

Les expressions régulières sont très utiles. Cependant, elles ne sont pas toujours la meilleure manière d'aborder certain problème autour des chaines de caractères.
Et surtout quand les transformations que vous voulez accomplir sont simples.

Je voulais savoir comment récupérer le plus vite possible l'extension d'un nom de fichier. Il y a trois manière naturelle d'accomplir celà :

<div><code class="ruby">
# regexp
str.match(/[^.]*$/); 
ext=$&

# split
ext=str.split('.')[-1]

# File module
ext=File.extname(str)
</code></div>

A première vue, je pensais que l'expression régulière serait plus rapide que le `split` parce qu'il pouvait y avoir plusieurs de `.` dans un nom de fichier. Mais la majorité du temps il n'y a qu'un seul point par nom de fichier. C'est pourquoi j'ai réalisé que le `split` serait plus rapide. Mais pas le plus rapide possible. Il y a une fonction qui est dédiée à faire ce travail dans un module standard de ruby ; le module `File`.

Voici le code pour faire un benchmark :

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

Et voici les résultats :

<pre class="twilight">
Get extname
            user     system      total        real
regexp:  2.550000   0.020000   2.570000 (  2.693407)
 split:  1.080000   0.050000   1.130000 (  1.190408)
  File:  0.640000   0.030000   0.670000 (  0.717748)
</pre>

En conclusion, les fonction dédiées sont meilleures que votre façon de faire (la plupart du temps).

## Chemin complet d'un fichier sans l'extension

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

et voici les résultats :

<pre class="twilight">
remove extension
          user     system      total        real
 File:  0.970000   0.060000   1.030000 (  1.081398)
chomp:  0.820000   0.040000   0.860000 (  0.947432)
</pre>

En conclusion du ce second benchmark. Un fonction simple est meilleure que trois fonctions dédiées. Pas de surprise, mais c'est toujours bien de savoir.
