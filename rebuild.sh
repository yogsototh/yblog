#!/usr/bin/env zsh

print -- "Compiling and building"
cabal-dev install && \
./site clean && \
./site build
