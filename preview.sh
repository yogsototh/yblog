#!/usr/bin/env zsh

# Launch a child that will recompile and regenerate the website
compileAndLaunch() {
	{
		print -- "COMPILING"
		ghc -O2 -Wall -odir _comp -hidir _comp site.hs && \
			print -- "REBUILDING" && \
			./site clean && \
			print -- "SERVING" && \
			./site preview
	} &
	son=$!
}

compileAndLaunch

print -- "JOBS = $son"

md5() {
	if [[ $md5 = "" ]]; then
		if which md5sum >/dev/null; then
			md5=$(which md5sum)
		else
			md5="openssl md5"
		fi
	fi
	eval $md5 $*
}

getState(){
	cat $filesToWatch | md5 | awk '{print $1}'
}

# what the Haskell files
filesToWatch=( *.{,l}hs(N) )
curids=$(getState)
oldids=$curids

while true; do
	sleep 1
	curids=$(getState)
	if [[ "$curids" != "$oldids" ]]; then
		oldids=$curids
		print "KILLING $son"
		kill $son
		compileAndLaunch
	fi
done
