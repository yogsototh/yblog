#!/usr/bin/env zsh

./update-from-multi && \
stack exec yblog clean && \
stack exec yblog build && \
./fastpublish.sh
