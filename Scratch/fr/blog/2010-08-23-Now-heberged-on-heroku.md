-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-08-23
title: Maintenant sur Heroku
subtitle: Héberger un site web statique sur heroku
author: Yann Esposito
authoruri: yannesposito.com
tags:  blog
-----
# Maintenant sur [Heroku](http://heroku.com)

J'ai changé mon hébergeur. Mobileme n'est absolument pas adapté à la diffusion de mon blog. C'est pourquoi je suis passé à [Heroku](http://heroku.com).

Mais comme vous devez le savoir mon blog est un site complètement statique.
J'utilise [nanoc](http://nanoc.stoneship.org/) pour l'engendrer.
Avoir un site statique amène beaucoup d'avantages par rapport à un site dynamique. Surtout en terme de sécurité.
Voici comment configurer un site statique sur heroku.

La racine de mes fichiers est '/output'. Vous devez simplement créer deux fichiers. Un fichier `config.ru`[^1] :

[^1]: Je me suis complètement inspiré de cet [article](http://gmarik.info/blog/2010/05/10/blogging-with-jekyll-and-heroku-for-free).

<code class="ruby" file="config.ru">
require 'rubygems'
require 'rack'
require 'rack/contrib'
require 'rack-rewrite'
require 'mime/types'

use Rack::ETag
module ::Rack
    class TryStatic < Static

        def initialize(app, options)
            super
            @try = ([''] + Array(options.delete(:try)) + [''])
        end

        def call(env)
            @next = 0
            while @next < @try.size && 404 == (resp = super(try_next(env)))[0] 
                @next += 1
            end
            404 == resp[0] ? @app.call : resp
        end

        private
        def try_next(env)
            env.merge('PATH_INFO' => env['PATH_INFO'] + @try[@next])
        end

    end
end

use Rack::TryStatic, 
    :root => "output",                              # static files root dir
    :urls => %w[/],                                 # match all requests 
    :try => ['.html', 'index.html', '/index.html']  # try these postfixes sequentially

errorFile='output/Scratch/en/error/404-not_found/index.html'
run lambda { [404, {
                "Last-Modified"  => File.mtime(errorFile).httpdate,
                "Content-Type"   => "text/html",
                "Content-Length" => File.size(errorFile).to_s
            }, File.read(errorFile)] }
</code>

et un fichier `.gems` qui liste les gems nécessaires.

<code class="ruby" file=".gems">
rack
rack-rewrite
rack-contrib
</code>

Maintenant il suffit de suivre l'introduction rapide d'heroku pour créer une nouvelle application :

<code class="zsh">
git init
git add .
heroku create
git push heroku master
</code>

Maintenant je peux rediriger correctement mes erreurs 404.
J'espère que ça a pu vous être utile.
