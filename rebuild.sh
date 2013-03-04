#!/usr/bin/env zsh

print -- "Compiling and building"
ghc -O2 -Wall -odir _comp -hidir _comp site.hs && \
./site clean && \
./site build
