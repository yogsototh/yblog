#!/usr/bin/env zsh

./tools/update-from-multi && \
cd content/ && \
stack exec yblog clean && \
stack exec yblog build && \
cd .. && \
./tools/fastpublish.hs
