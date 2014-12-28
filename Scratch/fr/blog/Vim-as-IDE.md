---
kind:           article
published:      2014-12-07
image: /Scratch/img/blog/Vim-as-IDE/vim_spock.jpg
title: Vim as IDE
author: Yann Esposito
authoruri: yannesposito.com
tags: programming, vi, vim, ide, haskell, clojure
theme: scientific
---
blogimage("vim_spock.jpg","Main image")

<div class="intro">


%tlal Comment utiliser vim comme une IDE très efficace

In [Learn Vim Progressively](/Scratch/en/blog/Learn-Vim-Progressively/)
I've show how Vim is great for editing text,
and navigating in the same file (buffer).
In this short article you'll see how I use Vim as an IDE.
Mainly by using some great plugins.

</div>

## Vim Plugin Manager

There are a _lot_ of Vim plugins.
To manage them I use [`vim-plug`][vim-plug].

To install it:

``` {.zsh}
mkdir -p ~/.vim/autoload
curl -fLo ~/.vim/autoload/plug.vim \
             https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

<div class="small">

☞ Note I have two parts in my `.vimrc`.
The first part contains the list of all my plugins.
The second part contains the personal preferences I setted for each plugin.
I'll separate each part by `...` in the code.

</div>

[vim-plug]: https://github.com/junegunn/vim-plug


## Survival

### Colorscheme

blogimage("solarized.png","Solarized theme")

Before anything, you should protect your eyes using a readable and low
contrast colorscheme.

For this I use [solarized dark][solarized].
To add it, you only have to write this in your `~/.vimrc` file:

``` {.vim}
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

You should be able to see and destroy trailing whitespaces.

blogimage("trim.gif","Trim whitespaces")

``` {.vim}
Plug 'bronson/vim-trailing-whitespace'
```

You can clean trailing whitespace with `:FixWhitespace`.

And also you should see your 80th column.

``` {.vim}
if (exists('+colorcolumn'))
    set colorcolumn=80
    highlight ColorColumn ctermbg=9
endif
```

blogimage("80thcolumn.png","80th column")

## File Management

One of the most important hidden skills in programming is the ability
to search and find files in your projects.

The majority of people use something like `NERDTree`.
This is the classical left column with a tree of files of your project.
_I stopped to use this_.
And you should probably too.

I switched to _unite_.
No left column lost.
Faster to find files.
Mainly it works like Spotlight on OS X.

First install [`ag` (the silver search)][ag].
If you don't know `ack` or `ag` your life is going to be upgraded.
This is a simple but essential tool.
It is mostly a `grep` on steroids.

[ag]: https://github.com/ggreer/the_silver_searcher

``` {.vim}
" Unite
"   depend on vimproc
"   ------------- VERY IMPORTANT ------------
"   you have to go to .vim/plugin/vimproc.vim and do a ./make
"   -----------------------------------------
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

Now type space twice.
A list of files appears.
Start to type some letters of the file you are searching for.
Select it, type return and bingo the file opens in a new horizontal split.

blogimage("unite.gif","Unite example")

If something goes wrong just type `<space>r` to reset the unite cache.

Now you are able to search file by name easily and efficiently.

Now search text in many files.
For this you use [`ag`][ag]:

``` {.vim}
Plug 'rking/ag.vim'
...
" --- type ° to search the word in all files in the current dir
nmap ° :Ag <c-r>=expand("<cword>")<cr><cr>
nnoremap <space>/ :Ag
```

Don't forget to add a space after the `:Ag`.

These are two of the most powerful shortcut for working in a project.
using `°` which is nicely positioned on my `azerty` keyboard.
You should use a key close to `*`.

So what `°` is doing? It read the string under the cursor and search for it
in all files. Really useful to search where a function is used.

If you type `<space>/` followed by a string, it will search for all
occurrences of this string in the project files.

So with this you should already be able to navigate between files very easily.

## Language Agnostic Plugins

### Git

blogimage("git-gutter.png","Show modified lines")

Show which line changed since your last commit.

``` {.vim}
Plug 'airblade/vim-gitgutter'
```

And the "defacto" git plugin:

``` {.vim}
Plug 'tpope/vim-fugitive'
```

You can reset your changes from the latest git commit with `:Gread`.
You can stage your changes with `:Gwrite`.

blogimage("Gread.gif","Reset changes")

### Align things

``` {.vim}
Plug 'junegunn/vim-easy-align'

...

" Easy align interactive
vnoremap <silent> <Enter> :EasyAlign<cr>
```

Just select and type `Return` then `space`.
Type `Return` many type to change the alignments.

If you want to align the second column, `Return` then `2` then `space`.

blogimage("easy-align.gif","Easy align example")

### Basic Completion `C-n` &amp; `C-p`

Vim has a basic completion system.
The keywords are `C-n` and `C-p` while you are in insert mode.
These are completion using keywords in current open buffers.
But it is already useful in a lot of cases.
For example when I open a file not in my configured languages.

## Haskell

My current Haskell programming environment is the best I could ever dreamed of for any language.

Seriously, each time I save a file,
I get a comment pointing to my errors or proposing me how to improve my code.

So here we go:


``` {.vim}
" ---------- VERY IMPORTANT -----------
" Don't forget to install ghc-mod with:
" cabal install ghc-mod
" -------------------------------------

Plug 'scrooloose/syntastic'             " syntax checker
" --- Haskell
Plug 'yogsototh/haskell-vim'            " syntax indentation / highlight
Plug 'enomsg/vim-haskellConcealPlus'    " unicode for haskell operators
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
Plug 'Twinside/vim-hoogle'
Plug 'pbrisbin/html-template-syntax'    " Yesod templates

...

" -------------------
"       Haskell
" -------------------
let mapleader="-"
let g:mapleader="-"
set tm=2000
nmap <silent> <leader>ht :GhcModType<CR>
nmap <silent> <leader>hh :GhcModTypeClear<CR>
nmap <silent> <leader>hT :GhcModTypeInsert<CR>
nmap <silent> <leader>hc :SyntasticCheck ghc_mod<CR>:lopen<CR>
let g:syntastic_mode_map={'mode': 'active', 'passive_filetypes': ['haskell']}
let g:syntastic_always_populate_loc_list = 1
nmap <silent> <leader>hl :SyntasticCheck hlint<CR>:lopen<CR>

" Auto-checking on writing
autocmd BufWritePost *.hs,*.lhs GhcModCheckAndLintAsync

"  neocomplcache (advanced completion)
autocmd BufEnter *.hs,*.lhs let g:neocomplcache_enable_at_startup = 1
function! SetToCabalBuild()
    if glob("*.cabal") != ''
        set makeprg=cabal\ build
    endif
endfunction
autocmd BufEnter *.hs,*.lhs :call SetToCabalBuild()

" -- neco-ghc
let $PATH=$PATH.':'.expand("~/.cabal/bin")
```

Just enjoy!

blogimage("vim-lint.gif","hlint on save")


I use `-` for my leader because I use `,` a lot for its native usage.

- `-ht` will highlight and show the type of the block under the cursor.
- `-hT` will insert the type of the current block.
- `-hh` will unhighlight the selection.

blogimage("auto-typing.gif","Auto typing on save")

## Clojure

blogimage("clojure.gif","Rainbow parenthesis")

My main language at work is Clojure.
And my current vim environment is quite good.
I lack the automatic integration to `lein-kibit` thought.
If I have the courage I might do it myself one day.
But due to the very long startup time of clojure,
I doubt I'll be able to make a useful vim plugin.

So mainly you'll have real rainbow-parenthesis
(the default values are broken for solarized).

I used the vim `paredit` plugin before.
But it is too restrictive.
Now I use `sexp` which feel more coherent with the spirit of vim.

``` {.vim}
" " -- Clojure
Plug 'kien/rainbow_parentheses.vim'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fireplace'

...

autocmd BufEnter *.cljs,*.clj,*.cljs.hl RainbowParenthesesActivate
autocmd BufEnter *.cljs,*.clj,*.cljs.hl RainbowParenthesesLoadRound
autocmd BufEnter *.cljs,*.clj,*.cljs.hl RainbowParenthesesLoadSquare
autocmd BufEnter *.cljs,*.clj,*.cljs.hl RainbowParenthesesLoadBraces
autocmd BufEnter *.cljs,*.clj,*.cljs.hl setlocal iskeyword+=?,-,*,!,+,/,=,<,>,.,:
" -- Rainbow parenthesis options
let g:rbpt_colorpairs = [
	\ ['darkyellow',  'RoyalBlue3'],
	\ ['darkgreen',   'SeaGreen3'],
	\ ['darkcyan',    'DarkOrchid3'],
	\ ['Darkblue',    'firebrick3'],
	\ ['DarkMagenta', 'RoyalBlue3'],
	\ ['darkred',     'SeaGreen3'],
	\ ['darkyellow',  'DarkOrchid3'],
	\ ['darkgreen',   'firebrick3'],
	\ ['darkcyan',    'RoyalBlue3'],
	\ ['Darkblue',    'SeaGreen3'],
	\ ['DarkMagenta', 'DarkOrchid3'],
	\ ['Darkblue',    'firebrick3'],
	\ ['darkcyan',    'SeaGreen3'],
	\ ['darkgreen',   'RoyalBlue3'],
	\ ['darkyellow',  'DarkOrchid3'],
	\ ['darkred',     'firebrick3'],
	\ ]
```

So now Clojure should look really nice.
You can eval any part of your code, you must launch a Clojure REPL manually in another terminal thought.

## Last words

I hope it will be useful.

Last but not least, if you want to use my vim configuration you can get it here:

[`github.com/yogsototh/vimrc`](http://github.com/yogsototh/vimrc)
