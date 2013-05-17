#!/usr/bin/env zsh

githubURI="git@github.com:yogsototh/yannesposito.com.git"
hakylldir=$HOME/Sites/yblog
pubdir=$hakylldir/_publish

[[ ! -e $pubdir ]] && git clone -b gh-pages $githubURI _publish

cd $hakylldir       && \
git pull            && \
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
