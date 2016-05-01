#!/usr/bin/env zsh
(($#==0)) && {
    print -- "usage: ${0:t} [file ...]"
    exit 1
}
for fic in $@; do
	newname="${fic:r}-web.${fic:e}"
	# convert -thumbnail 1600x1200\> -adaptive-sharpen 0 -quality 75% $fic $newname
	convert -adaptive-sharpen 0 -quality 75% $fic $newname
	mv $fic $fic.old
	mv $newname $fic
done
