-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-08-23
title: Now hosted by heroku
subtitle: Host static website on Heroku
authorName: Yann Esposito
authorUri: yannesposito.com
tags: blog
-----
# Now on [Heroku](http://heroku.com)

I now changed my hosting to [Heroku](http://heroku.com). 
I believe it will be far more reliable.

But as you should know my website is completely static.
I use [nanoc](http://nanoc.stoneship.org/) to generate it.
But here is the conf to make it work on heroku.

The root of my files is `/output`. You only need to create a `config.ru`[^1] file:

[^1]: I was inspired by this [article](http://gmarik.info/blog/2010/05/10/blogging-with-jekyll-and-heroku-for-free). 

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

and the `.gems` file needed to install `rack` middlewares.

<code class="ruby" file=".gems">
rack
rack-rewrite
rack-contrib
</code>

Now, just follow the heroku tutorial to create an application :

<code class="zsh">
git init
git add .
heroku create
git push heroku master
</code>

Now I'll should be able to redirect properly to my own 404 page for example.
I hope it is helpful.
