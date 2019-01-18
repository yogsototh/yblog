#!/usr/bin/env zsh
if (($#==0))
then files=( **/*.{png,jpg,jpeg} )
else files=( $@ )
fi

du -h -c $files

# max size will be 512px and slightly pixelated
maxsize=1024
mogrify -resize ${maxsize}x${maxsize}\> $files
# optim compression with minimal quality lost
imageoptim $files

du -h -c $files

