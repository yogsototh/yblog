#!/usr/bin/env zsh

hakylldir=$HOME/Sites/yblog
pubdir=$hakylldir/_publish

[[ ! -e $pubdir ]] && git clone github.com:yogsototh/yannesposito.com.git _publish

cd $hakylldir       && \
git pull            && \
./rebuild.sh        && \
cd $pubdir          && \
git checkout gh-pages && \
print -- "get latest modif from github" && \
git pull && \
print -- "Remove all files except .git" && \
\rm -rf * && \
print -- "Copy _site" && \
\cp -rf ../_site/* . && \
print -- "Adding files to repository" && \
git add . && \
git add -u && \
print -- "Commit and publish" && \
git commit -m "publishing"
git push
