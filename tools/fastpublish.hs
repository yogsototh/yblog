#!/usr/bin/env stack
{- stack
   --resolver lts-5.15
   --install-ghc
   runghc
   --package turtle
   --package ansi-terminal
   --verbosity s
-}

 {-# LANGUAGE OverloadedStrings #-}
import Turtle

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Data.Maybe (fromMaybe)
import System.Console.ANSI
import Control.Exception (catches,Handler(..))

main = mainProc `catches` [ Handler handleShellFailed
                          , Handler handleProcFailed
                          ]

handleShellFailed :: ShellFailed -> IO ()
handleShellFailed (ShellFailed cmdLine _) = do
  setSGR [SetColor Foreground Dull Red]
  echo $ ("[FAILED]: " <> cmdLine)
  setSGR [Reset]
handleProcFailed :: ProcFailed -> IO ()
handleProcFailed (ProcFailed procCommand procArgs _) = do
  setSGR [SetColor Foreground Dull Red]
  echo $ ("[FAILED]: " <> procCommand <> (mconcat procArgs))
  setSGR [Reset]


mainProc :: IO ()
mainProc = do
  -- So we can't have access to $0 in Haskell via stack.
  -- Too bad.
  -- So instead, I'll check I'm in the right directory.
  debug "Checking directory"
  (hakylldir,pubdir) <- checkDir
  debug "Retrieving revision number"
  rev <- fold (inshell "git rev-parse --short HEAD" empty) Fold.head
  debug ("Revision number retrieved: " <> fromMaybe "unknow" rev)
  debug $ "cd" <> (format fp pubdir)
  cd pubdir
  pwd >>= echo . format fp
  dshells "git init ."
  dshell ("git remote add upstream " <> mainRepository)
  dshells "git fetch upstream"
  dshells "git reset upstream/gh-pages"
  dshells "git add -A ."
  echo "Commit and publish"
  dshells ("git commit -m \"publishing at rev " <> (fromMaybe "unknow" rev) <> "\"")
  echo "Don't `git push` this time"
  dshells "git push -q upstream HEAD:gh-pages"

debug txt = do
  setSGR [SetColor Foreground Dull Yellow]
  echo txt
  setSGR [Reset]

dshells x = do
  debug x
  shells x empty

dshell x = do
  debug x
  shell x empty

checkDir :: IO (FilePath,FilePath)
checkDir = do
  toolsExists <- testdir "tools"
  if (not toolsExists)
     then exit (ExitFailure 1)
     else return (".","content/_site")

mainRepository = "git@github.com:yogsototh/yannesposito.com.git"

cloneIfNeeded :: FilePath -> IO ()
cloneIfNeeded pubdir = do
  contentExists <- testdir pubdir
  when (not contentExists) $
       procs "git"
             [ "clone"
             , "-b", "gh-pages"
             , mainRepository
             , format fp pubdir]
             empty
