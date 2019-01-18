#!/usr/bin/env zsh
if (($#==0))
then files=( **/*.{png,jpg,jpeg} )
else files=( $@ )
fi

du -h -c $files

maxsize=512
mogrify -resize ${maxsize}x${maxsize}\> $files
imageoptim $files

du -h -c $files

