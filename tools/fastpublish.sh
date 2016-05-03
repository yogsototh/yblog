#!/usr/bin/env zsh

hakylldir=$0/..
pubdir=$hakylldir/content/_publish

[[ ! -e $pubdir ]] && \
    cd ${pubdir:h} && \
    {
        git clone -b gh-pages git@github.com:yogsototh/yannesposito.com.git _publish || exit 1
    }

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
