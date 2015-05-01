#!/usr/bin/env zsh

./.cabal-sandbox/bin/yblog clean && \
./.cabal-sandbox/bin/yblog build && \
./fastpublish.sh
