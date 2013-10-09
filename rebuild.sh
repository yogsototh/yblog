#!/usr/bin/env zsh

print -- "Compiling and building"
[[ ! -e .cabal-sandbox ]] && cabal sandbox init
cabal install && \
./site clean && \
./site build
