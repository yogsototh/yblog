-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-10-26
title: LaTeX like macro for markdown
author: Yann Esposito
authoruri: yannesposito.com
tags:  LaTeX, macros, markdown, nanoc, ruby
-----

<div class="intro">

%tldr I made a simple macro system for my blog. Now I juste have to write %<span></span>latex and it show as %latex.

</div>

I added a macro system for my blog system.
When we are used to %latex this lack can be hard to handle.
Particularly when using mathematical notations.
In the header of my files I simply write:

~~~~~~ {.yaml}
~~~~~~

In the body it will replace every occurrence of:

- %<span></span>test by *%test*,
- and %<span></span>latex by *%latex*.

The source code is really simple.
For `nanoc` user, simply put this file in your `lib` directory.

~~~~~~ {.ruby}
# usage:
# ---
# ...
# macros:
#   test: "passed test"
# ---
# ...
# Here is a %test.
#
class Macros < Nanoc3::Filter
    identifier :falacy
    attr_accessor :macro
    def initialize(arg)
        super
        @macro={}
        @macro[:tlal] = %{<span class="sc"><abbr title="Trop long à lire">tlàl</abbr> : </span>}
        @macro[:tldr] = %{<span class="sc"><abbr title="Too long; didn't read">tl;dr</abbr>: </span>}
        if @item.nil?
            if not arg.nil?
                @macro.merge!( arg )
            end
        else
            if not @item[:macros].nil?
                @macro.merge!( @item[:macros] )
            end
        end
    end
    def macro_value_for(macro_name)
        if macro_name.nil? or macro_name=="" or @macro[macro_name.intern].nil?
            return %{%#{macro_name}} 
        end
        return @macro[macro_name.intern]
    end
    def run(content, params={})
        content.gsub(/%(\w*)/) do |m| 
            if m != '%'
                macro_value_for($1)
            else
                m
            end
        end
    end
end
~~~~~~

Macros could be very useful, read [this article](http://adam.gomaa.us/blog/2007/oct/22/markdown-doesnt-scale/index.html) for example.
