#!/usr/bin/env zsh

stack exec yblog clean && \
stack exec yblog build && \
./fastpublish.sh
