#!/usr/bin/env zsh

src="$HOME/dev/mkdocs/resume"
dst="content/Scratch/files"

err(){
   print -P "%B$*%b"  >&2
   exit 1
}

[[ -e $src ]] || err "Can't find $src"
[[ -e $dst ]] || err "Can't find $dst"

print "Removing $dst/resume"
[[ -e $dst/resume ]] && rm -rf $dst/resume

print "Copying css"
cp -f $src/../styling.css $dst/
print "Copying css"
cp -r $src $dst
