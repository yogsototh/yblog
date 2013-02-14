-----
isHidden:       false
menupriority:   1
kind:           article
published:     2010-10-26T14:30:58+02:00
title: LaTeX like macro for markdown
authorName: Yann Esposito
authorUri: yannesposito.com
macroTest: "This is a macro test"
macroLatex: '<span style="text-transform: uppercase">L<sup style="vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em">a</sup>T<sub style="vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em">e</sub>X</span>'
tags: LaTeX, macros, markdown, nanoc, ruby
-----

<div class="intro">

%tldr I made a simple macro system for my blog. Now I juste have to write %<span></span>latex and it show as %latex.

</div>

I added a macro system for my blog system.
When we are used to %latex this lack can be hard to handle.
Particularly when using mathematical notations.
In the header of my files I simply write:

<code class="yaml">
macros:
  test: "This is a macro test"
  latex: '<span style="text-transform: uppercase">L<sup style="vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em">a</sup>T<sub style="vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em">e</sub>X</span>'
</code>

In the body it will replace every occurrence of:

- %<span></span>test by *%test*,
- and %<span></span>latex by *%latex*.

The source code is really simple.
For `nanoc` user, simply put this file in your `lib` directory.

<code class="ruby" file="macros.rb">
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
</code>

Macros could be very useful, read [this article](http://adam.gomaa.us/blog/2007/oct/22/markdown-doesnt-scale/index.html) for example.
