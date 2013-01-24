-----
isHidden:       false
menupriority:   1
kind:           article
created:     2011-09-28T15:15:23+02:00
title: Higher order function in zsh
authorName: Yann Esposito
authorUri: yannesposito.com
tags: zsh, map, foldr, filter, functional, programming, higher order functions
-----
blogimage("main.jpg","Title image")

begindiv(intro)

UPDATE: [Nicholas Sterling had discovered a way to implement anonymous functions](http://nicholassterling.wordpress.com/2012/03/30/a-zsh-map-function/)
Thanks!

With this last version you should use `map` if you use external function.
`mapl` to use lambda function. And `mapa` for arithmetic operations.

Example: 

<code class="zsh">
$ filterl 'echo $1|grep a >/dev/null' ab cd ef ada
ab
ada

$ folda '$1+$2' {1..5}
15

$ folda '$1*$2' {1..20}
2432902008176640000

$ mapl 'echo X $1:t Y' ~/.zsh/functional/src/*
X each Y
X filter Y
X fold Y
X map Y

$ mapa '$1*2' {1..3}
2
4
6

$ mapl 'echo result $1' $(mapa '$1+5' $(mapa '$1*2' {1..3}))
result 7
result 9
result 11

</code>

%tldr some simple implementation of higher order function for zsh.

enddiv

Why is it important to have these functions?
Simply because, the more I programmed with zsh the more I tended to work using functional programming style.

The minimal to have better code are the functions `map`, `filter` and `fold`.

Let's compare.
First a program which convert all gif to png in many different directories of different projects.

Before ⇒

<code class="zsh">
# for each directory in projects dir
for toProject in /path/to/projects/*(/N); do
    # toProject is /path/to/projects/foo
    # project become foo (:t for tail)
    project=${toProject:t}
    for toResource in $toProject/resources/*.gif(.N); do
        convert $toResource ${toResource:r}.png && \
        \rm -f $toResource
    done
done
</code>

- The `(/N)` means to select only directory and not to crash if there isn't any.
- The `(.N)` means to select only files and not to crash if there isn't any.
- The `:t` means tail; if `toto=/path/to/file.ext` then `${toto:t}=file.ext`.

After ⇒

<code class="zsh">
gif_to_png() { convert $1 ${1:r}.png && \rm -f $1 }

handle_resources() { map gif_to_png $1/resources/*.gif(.N) }

map handle_resources /path/to/projects/*(/N)
</code>

No more bloc!
It might be a little bit harder to read if you're not used to functional programming notation.
But it is more concise and robusts.

Another example with some tests.

Find all files in project not containing an `s` which their name contains their project name:

Before ⇒

<code class="zsh">
for toProject in Projects/*; do
    project=$toProject:t
    if print -- project | grep -v s >/dev/null
    then
        print $project
        for toResource in $toProject/*(.N); do
            if print -- ${toResource:t} | grep $project >/dev/null; then
                print -- "X $toResource"
            fi
        done
    fi
done
</code>

After ⇒

<code class="zsh">
contain_no_s() { print $1 | grep -v s }

function verify_file_name {                               
    local project=$1:t
    contains_project_name() { print $1:t | grep $project }
    map "print -- X" $(filter contains_project_name $1/*(.N))
}

map verify_file_name $( filter contain_no_s Projects/* )
</code>

Also, the first verstion is a bit easier to read. 
But the second one is clearly far superior in architecture.
I don't want to argue why here. 
Just believe me that the functional programming approach is superior.

You can find an [updated version of the code (thanks to Arash Rouhani)](https://github.com/Tarrasch/zsh-functional). 
An older version is [here thought](https://github.com/yogsototh/zsh_functional).
Here is the (first version) source code:

<code class="zsh" file="functional.sh">
#!/usr/bin/env zsh

# Provide higer-order functions 

# usage:
#
# $ foo(){print "x: $1"}
# $ map foo a b c d
# x: a
# x: b
# x: c
# x: d
function map {
    local func_name=$1
    shift
    for elem in $@; print -- $(eval $func_name $elem)
}

# $ bar() { print $(($1 + $2)) }
# $ fold bar 0 1 2 3 4 5
# 15
# -- but also
# $ fold bar 0 $( seq 1 100 )
function fold {
    if (($#<2)) {
        print -- "ERROR fold use at least 2 arguments" >&2
        return 1
    }
    if (($#<3)) {
        print -- $2
        return 0
    } else {
        local acc
        local right
        local func_name=$1
        local init_value=$2
        local first_value=$3
        shift 3
        right=$( fold $func_name $init_value $@ )
        acc=$( eval "$func_name $first_value $right" )
        print -- $acc
        return 0
    }
}

# usage:
#
# $ baz() { print $1 | grep baz }
# $ filter baz titi bazaar biz
# bazaar
function filter {
    local predicate=$1
    local result
    typeset -a result
    shift
    for elem in $@; do
        if eval $predicate $elem >/dev/null; then
            result=( $result $elem )
        fi
    done
    print $result
}
</code>
