---
kind:           article
published:      2014-12-07
image: /Scratch/img/blog/Vim-as-IDE/main.png
title: Vim as IDE
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: scientific
---
blogimage("main.png","Main image")

<div class="intro">

%tldr How to use vim as a _very_ efficient IDE



Vim is great for editing text and navigating in the same file (buffer).
But, how to deal with multiple organized files.
To play with REPL, to lint or compile?

In this article I provide my configuration for dealing with all of this.

</div>

## Plugins

There are a _lot_ of Vim plugins.
To manage them I use [`vim-plug`][vim-plug].

To install it:

``` {.bash}
mkdir -p ~/.vim/autoload
curl -fLo ~/.vim/autoload/plug.vim \
             https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

[vim-plug]: https://github.com/junegunn/vim-plug


## Survival

### Colorscheme

Before anything, you should protect your eyes using a readable and low
contrast colorscheme.

For this I use [solarized][solarized] dark.
For this, you only have to write this in your `~/.vimrc` file:

```
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'

call plug#end()

" -- solarized personal conf
set background=dark
try
    colorscheme solarized
catch
endtry
```

[solarized]: http://ethanschoonover.com/solarized

### Minimal hygiene

You should also see and be able to clean trailing whitespace.

```
Plug 'bronson/vim-trailing-whitespace'
```

And also you should see your 80th column.

```
if (exists('+colorcolumn'))
    set colorcolumn=80
    highlight ColorColumn ctermbg=9
endif
```

## File Management

Whatever your task, you really want to be able to search and find files easily.

Generally people tend to use `NERDTree`.
This plugin show you the standard left column with file tree.

I prefer to use unite.
It doesn't lost the left column.

First install [`ag` (the silver search)][ag]

[ag]: https://github.com/ggreer/the_silver_searcher

```
" Unite
"   depend on vimproc
"   you have to go to .vim/plugin/vimproc.vim and do a ./make
Plug 'Shougo/vimproc.vim'
Plug 'Shougo/unite.vim'

...

let g:unite_source_history_yank_enable = 1
try
  let g:unite_source_rec_async_command='ag --nocolor --nogroup -g ""'
  call unite#filters#matcher_default#use(['matcher_fuzzy'])
catch
endtry
" search a file in the filetree
nnoremap <space><space> :split<cr> :<C-u>Unite -start-insert file_rec/async<cr>
" reset not it is <C-l> normally
:nnoremap <space>r <Plug>(unite_restart)
```

So how does it works?
Type space twice.
A list of files appears.
Start to type some letters of the file you are searching for.
Select it, type return and bingo the file opens in a new horizontal split.

If something goes wrong just type `<space>r` to reset the unite cache.

Now you are able to search file by name easily and efficiently.

Now search text in many files.
For this you use [`ag`][ag]:

```
Plug 'rking/ag.vim'
...
" --- type ° to search the word in all files in the current dir
nmap ° :Ag <c-r>=expand("<cword>")<cr><cr>
nnoremap <space>/ :Ag 
```

These are two of the most powerful shortcut for working in a project.
using `°` which is nicely positionned on my `azerty` keyboard.
You should use a key close to `*`.

So what `°` is doing? It read the string under the cursor and search for it
in all files. Really useful to search where a function is used.

If you type `<space>/` followed by a string, it will search for all
occurences of this string in the project files.

So with this you should already be able to navigate between files very easily.
