---
kind:           article
published:      2013-11-14
image: /Scratch/img/blog/Start-a-Haskell-Project-the-right-way/main.png
en: title: Start a Haskell Project the right way
fr: title: Start a Haskell Project the right way
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---

<div class="intro">

en: %tldr Starting a Haskell Project tool with `cabal`, `git`, testing.

fr: %tlal Un outils pour commencer un nouveau projet Haskell; `cabal`, `git`, testing...

</div>

I recently read this excellent article:
[How to Start a New Haskell Project](http://jabberwocky.eu/2013/10/24/how-to-start-a-new-haskell-project/).

While the article is very good, I lacked some minor informations.
Also, I believe we could improve this a bit.
And this is also the kind of process you might repeat often.
This is why I created a simple shell script to initialize a new Haskell project.

So, mainly, if you do it manually the steps are:

1. [Install Haskell](http://wwW.haskell.org/platform)
2. Make sure you have the latest `cabal-install` (at least 1.18)

``` bash
> cabal install cabal-install
```

3. Download and run the script

``` bash
# Download the script
git clone https://github.com/yogsototh/init-haskell-project.git
# Copy the script in a directory of you PATH variable
cp init-haskell-project/holy-project.sh ~/bin
# Go to the directory containing all your projects
cd my/projects/e idi:
# Launch thcript
holy-haskell.sh
```

What does this script do that doesn't do cabal.

- Use cabal sandbox
- It initialize `git` with the right `.gitignore` file.
- Use `tasty` to organize your tests (HUnit, QuickCheck and SmallCheck).
- Use `-Wall` for GHC compilation.
- Will make references to Holy Grail
- Search your default github username via [github api](http://developer.github.com/v3/search/#search-users).

## `zsh` really?

Developing the script in `zsh` was easy.
And while `zsh` is my favorite shell script, the size of this script
make it worth to write it in a more secure language.
Furthermore it will be a good exercise to translate
this script from `zsh` to Haskell.


### Patricide

So to make our development, let us initialize a new Haskell project with
`holy-haskell`.
Here is what its execution look like:

<pre>
> ./holy-haskell.sh
<span class="green">Bridgekeeper: Stop!
Bridgekeeper: Who would cross the Bridge of Death
Bridgekeeper: must answer me these questions three,
Bridgekeeper: ere the other side he see.</span>
<span class="yellow">You: Ask me the questions, bridgekeeper, I am not afraid.</span>

<span class="green">Bridgekeeper: What is the name of your project?</span>
> Holy project
<span class="green">Bridgekeeper: What is your name?</span> (Yann Esposito (Yogsototh))
>
<span class="green">Bridgekeeper: What is your email?</span> (Yann.Esposito@gmail.com)
>
<span class="green">Bridgekeeper: What is your github user name?</span> (yogsototh)
>
<span class="green">Bridgekeeper: What is your project in less than ten words?</span>
> Start your Haskell project with cabal, git and tests.
Initialize git
Initialized empty Git repository in .../holy-project/.git/
Create files
    .gitignore
    holy-project.cabal
    Setup.hs
    LICENSE (MIT)
    test/Test.hs
    test/HolyProject/Swallow/Test.hs
    src/HolyProject/Swallow.hs
    test/HolyProject/Coconut/Test.hs
    src/HolyProject/Coconut.hs
    src/HolyProject.hs
    src/Main.hs
Cabal sandboxing, install and test
...
  many compilations lines
...
Running 1 test suites...
Test suite Tests: RUNNING...
Test suite Tests: PASS
Test suite logged to: dist/test/holy-project-0.1.0.0-Tests.log
1 of 1 test suites (1 of 1 test cases) passed.
All Tests
  Swallow
    swallow test:     <span class="green">OK</span>
  coconut
    coconut:          <span class="green">OK</span>
    coconut property: <span class="green">OK</span>
      148 tests completed

<span class="green">All 3 tests passed</span>



<span class="green">Bridgekeeper: What... is the air-speed velocity of an unladen swallow?</span>
<span class="yellow">You: What do you mean? An African or European swallow?</span>
<span class="green">Bridgekeeper: Huh? I... I don't know that.</span>
[the bridgekeeper is thrown over]
<span class="green">Bridgekeeper: Auuuuuuuuuuuugh</span>
Sir Bedevere: How do you know so much about swallows?
<span class="yellow">You: Well, you have to know these things when you're a king, you know.</span>
</pre>

The different steps are:

- small introduction quotes
- ask five questions -- _three question sir..._
- create the directory for the project
- init git
- create files
- sandbox cabal
- cabal install and test
- run the test directly in the terminal
- small goodbye quotes

Things to note:

- color in the terminal
- check some rules on the project name
- random message if error
- use `~/.gitconfig` file in order to provide a default name and email.
- use the github API which returns JSON to get the default github user name.

So, apparently nothing too difficult to achieve.

### The dialogs

First lets us write a function which show the introduction text:

in `zsh`:

``` bash
# init colors
autoload colors
colors
for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
    eval $COLOR='$fg_no_bold[${(L)COLOR}]'
    eval BOLD_$COLOR='$fg_bold[${(L)COLOR}]'
done
eval RESET='$reset_color'
# functions
bk(){print -- "${GREEN}Bridgekeeper: $*${RESET}"}
bkn(){print -n -- "${GREEN}Bridgekeeper: $*${RESET}"}
you(){print -- "${YELLOW}You: $*${RESET}"}
...
# the introduction dialog
bk "Stop!"
bk "Who would cross the Bridge of Death"
bk "must answer me these questions three,"
bk "ere the other side he see."
you "Ask me the questions, bridgekeeper, I am not afraid.\n"
...
# the final dialog
print "\n\n"
bk "What... is the air-speed velocity of an unladen swallow?"
you "What do you mean? An African or European swallow?"
bk "Huh? I... I don't know that."
log "[the bridgekeeper is thrown over]"
bk "Auuuuuuuuuuuugh"
log "Sir Bedevere: How do you know so much about swallows?"
you "Well, you have to know these things when you're a king, you know."
```

In the first Haskell version I dont use colors.
We see we can almost copy/paste.
I just added the types.

``` haskell
bk :: String -> IO ()
bk str = putStrLn $ "Bridgekeeper: " ++ str

bkn :: String -> IO ()
bkn str = pustStr $ "Bridgekeeper: " ++ str

you :: String -> IO ()
you str = putStrLn $ "You: " ++ str

intro :: IO ()
intro = do
    bk "Stop!"
    bk "Who would cross the Bridge of Death"
    bk "must answer me these questions three,"
    bk "ere the other side he see."
    you "Ask me the questions, bridgekeeper, I am not afraid.\n"

end :: IO ()
end = do
    putStrLn "\n\n"
    bk "What... is the air-speed velocity of an unladen swallow?"
    you "What do you mean? An African or European swallow?"
    bk "Huh? I... I don't know that."
    putStrLn "[the bridgekeeper is thrown over]"
    bk "Auuuuuuuuuuuugh"
    putStrLn "Sir Bedevere: How do you know so much about swallows?"
    you "Well, you have to know these things when you're a king, you know."
```

Now let's just add the colors using the
[`ansi-term`](http://hackage.haskell.org/package/ansi-terminal) package.
So we have to add `ansi-term` as a build dependency in our cabal file.

Edit `holy-project.cabal` to add it.

```
...
build-depends:  base >=4.6 && <4.7
                , ansi-terminal
...
```

Now look at the modified Haskell code:

``` haskell
import System.Console.ANSI

colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
    setSGR  [ SetColor Foreground Dull color
            , SetConsoleIntensity NormalIntensity
            ]
    putStr str
    setSGR []


bk :: String -> IO ()
bk str = colorPutStr Green ("Bridgekeeper: " ++ str ++ "\n")
bkn :: String -> IO ()
bkn str = colorPutStr Green ("Bridgekeeper: " ++ str)
you :: String -> IO ()
you str = colorPutStr Yellow ("Bridgekeeper: " ++ str ++ "\n")

intro :: IO ()
intro = do
    bk "Stop!"
    bk "Who would cross the Bridge of Death"
    bk "must answer me these questions three,"
    bk "ere the other side he see."
    you "Ask me the questions, bridgekeeper, I am not afraid.\n"

end :: IO ()
end = do
    putStrLn "\n\n"
    bk "What... is the air-speed velocity of an unladen swallow?"
    you "What do you mean? An African or European swallow?"
    bk "Huh? I... I don't know that."
    putStrLn "[the bridgekeeper is thrown over]"
    bk "Auuuuuuuuuuuugh"
    putStrLn "Sir Bedevere: How do you know so much about swallows?"
    you "Well, you have to know these things when you're a king, you know."
```

For now we could put this code inside `src/Main.hs`.
Declare a main function:

``` haskell
main :: IO ()
main = do
    intro
    end
```

Make `cabal install` and run `./.cabal-sandbox/bin/holy-project`.
It works!



## Questions

In order to ask questions, here is how we do it in shell script:

``` bash
print -- "What is your name?"
read name
```

If we want to abstract things a bit, the easiest way in shell is to use
a global variable[^1] which will get the value of the user input like this:

``` bash
answer=""
ask(){
    local info="$1"
    bk "What is your $info?"
    print -n "> "
    read answer
}
...
ask name
name="$answer"
```

[^1]: There is now easy way to do something like `name=$(ask name)`.
      Simply because `$(ask name)` run in another process which
      doesn't get access to the standard input

In Haskell we won't need any global variable:

``` haskell
import System.IO (hFlush, stdout)
...
ask :: String -> IO String
ask info = do
    bk $ "What is your " ++ info ++ "?"
    putStr "> "
    hFlush stdout -- Because we want to ask on the same line.
    getLine
```

Now our main function might look like:

``` haskell
main = do
    intro
    _ <- ask "project name"
    _ <- ask "name"
    _ <- ask "email"
    _ <- ask "github account"
    _ <- ask "project in less than a dozen word"
    end
```

You could test it with `cabal install` and
then `./.cabal-sandbox/bin/holy-project`.

We will see later how to guess the answer using the `.gitconfig` file and
the github API.

## Using answers

### Create the project name

I don't really like the ability to use capital letter in a package name.
So in shell I transform the project name like this:

``` bash
# replace all spaces by dashes then lowercase the string
project=${${project:gs/ /-/}:l}
```

In order to achieve the same result in Haskell:

``` haskell
import Data.List (instersperse)
...
projectNameFromString :: String -> String
projectNameFromString str = concat $ intersperse "-"
    (splitOneOf " -" (map toLower str))
```

One important thing to note is that in zsh the transformation occurs
on strings but in haskell we use list as intermediate representation:

```
zsh:
"Holy grail" ==( ${project:gs/ /-/} )=> "Holy-grail"
             ==( ${project:l}       )=> "holy-grail"

haskell
"Holy grail" ==( map toLower     )=> "holy grail"
             ==( splitOneOf " -" )=> ["holy","grail"]
             ==( intersperse "-" )=> ["holy","-","grail"]
             ==( concat          )=> "holy-grail"

```


### Create the module name

``` bash
# Capitalize a string
capitalize(){
    local str="$(print -- "$*" | sed 's/-/ /g')"
    print -- ${(C)str} | sed 's/ //g'
}
```

``` haskell
-- | transform a chain like "Holy project" in "HolyProject"
capitalize :: String -> String
capitalize str = concat (map capitalizeWord (splitOneOf " -" str))
    where
        capitalizeWord :: String -> String
        capitalizeWord (x:xs)   = (toUpper x):map toLower xs
        capitalizeWord  _       = []
```

The haskell version is made by hand where zsh already had a capitalize
operation on string with many words.
Here is the difference between the shell and haskell way:

```
shell:
"Holy-grail" ==( sed 's/-/ /g' )=> "Holy grail"
             ==( ${(C)str}     )=> "Holy Grail"
             ==( sed 's/ //g'  )=> "HolyGrail"

haskell:
"Holy-grail" ==( splitOneOf " -"    )=> ["Holy","grail"]
             ==( map capitalizeWord )=> ["Holy","Grail"]
             ==( concat             )=> "HolyGrail"
```

Note how the representation is always strings in shell and use lists with
haskell.

## Create the project

Now we should have had all the informations we need to create a new project.
Mainly, making a project will consists in creating files and directories whose
name and content depends on the answer we had until now.

In shell we used something like:

``` bash
> file-to-create cat <<END
file content here.
We can use $variables here
END
```

What we need in haskell is something that will takes some
string with some "identifier" and will replace the identifier by its value
at runtime. This is a templating system.
And I choose to use [`hastache`](http://hackage.haskell.org/package/hastache),
an haskell implementation of Mustache templates.

``` haskell
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy.Char8 as LZ

genFile :: Context -> String -> ByteString
genFile context template =
    hastacheStr defaultConfig (encodeStr template) context
...
createMITLicense = do
    content = genFile projectContext mitTemplate
    LZ.writeFile "LICENSE" content
    where
        mitTemplate = concat [
              "MIT ."
            , "Author {{author}}"
            ]
```


<div style="display:none">
``` bash
#!/usr/bin/env zsh

# --- Function declaration and global variables
answer=""
# Capitalize a string
capitalize(){
    local str="$(print -- "$*" | sed 's/-/ /g')"
    print -- ${(C)str} | sed 's/ //g'
}
err(){
    {
    case $(( $RANDOM % 1 )); in
    0)  bk "What... is your favourite colour?"
        you "Blue. No, yel..."
        log "[You are thrown over the edge into the volcano]"
        you "You: Auuuuuuuuuuuugh"
        bk " Hee hee heh."
        ;;
    1)  bk "What is the capital of Assyria?"
        you "I don't know that!"
        log "[You are thrown over the edge into the volcano]"
        you "Auuuuuuuuuuuugh"
        ;;
    esac

    print -- "\n$*"
    }>&2
    exit 1
}
# Ask the user for some information
# Search in .gitconfig some hints to provide default value
ask(){
    local elem=$1
    local gitconfigelem
    bkn "What is your $elem?"
    [[ -e ~/.gitconfig ]] && {
        gitconfigelem="$(< ~/.gitconfig| awk '$1 == "'$elem'" {$1="";$2="";gsub("^ *","");print}')"
        [[ $gitconfigelem = "" ]] || {
            print -n -- " ($gitconfigelem)"
        }
    }
    print -n "\n> ";read answer
    [[ $answer = "" ]] && answer=$gitconfigelem
}

# Delete the project directory and show an error message
reinit(){
    cd ..
    [[ -e $project ]] && \rm -rf $project
    err "Something went wrong, I removed the project directory"
}


# --- Start asking questions
bk "Stop!"
bk "Who would cross the Bridge of Death"
bk "must answer me these questions three,"
bk "ere the other side he see."
you "Ask me the questions, bridgekeeper, I am not afraid.\n"

# project name
bk "What is the name of your project?"
print -n "> ";read project
project=${${project:gs/ /-/}:l} # use lowercase and replace spaces by dashes

# Verify project has the right format
if perl -e 'exit("'$project'" =~ /^[a-z][a-z0-9-]*$/)'; then
    err "The project must start with a letter and contains only letter, number, spaces or dashes"
fi
[[ $project = "" ]] && err "Can't use empty project name"
[[ -e $project ]] && err "$project directory already exists"

# Find the main module name from the project name
module=$(capitalize $project)

# author name
ask name
name="$answer"
# email
ask email
email="$answer"
# github
bkn "What is your github user name?"
githubname=( $( curl -sH 'Accept: application/vnd.github.v3.text-match+json' 'https://api.github.com/search/users?q='$email|grep '"login":'|perl -pe 's/.*"([^"]*)",/$1/' ) )
(( ${#githubname} == 1 )) && print -- " ($githubname)"
print -n "> ";read github
[[ $github = "" ]] && github=$githubname
# synopsis
bk "What is your project in less than ten words?"
print -n "> ";read description

# --- We start the creation of the project files here
mkdir $project
cd $project

print -- "Initialize git"
git init .

print -- "Create files"

print -- "\t.gitignore"
>.gitignore cat <<END
.cabal-sandbox
cabal.sandbox.config
dist
*.swp
*~
.ghci
END

print -- "\t$project.cabal"
> $project.cabal cat <<END
-- Initial $project.cabal generated by cabal init.  For further documentation,
-- see http://haskell.org/cabal/users-guide/

name:                   $project
version:                0.1.0.0
synopsis:               $description
-- description:
homepage:               http://github.com/$github/$project
license:                MIT
license-file:           LICENSE
author:                 $name
maintainer:             $email
-- copyright:
category:               Unknown
build-type:             Simple
-- extra-source-files:
cabal-version:          >=1.10

executable $project
  main-is:              Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:        base >=4.6 && <4.7
  hs-source-dirs:       src
  ghc-options:          -Wall
  default-language:     Haskell2010

library
  exposed-modules:      $module
                        , $module.Swallow
                        , $module.Coconut
  -- other-modules:
  -- other-extensions:
  build-depends:        base >=4.6 && <4.7
  ghc-options:          -Wall
  hs-source-dirs:       src
  default-language:     Haskell2010

executable test-$project
  hs-source-dirs:       test
  ghc-options:          -Wall
  main-is:              Test.hs
  default-language:     Haskell2010
  build-depends:        base ==4.6.*, Cabal >= 1.16.0
                        , $project
                        , HUnit
                        , QuickCheck
                        , smallcheck
                        , tasty
                        , tasty-hunit
                        , tasty-quickcheck
                        , tasty-smallcheck
test-suite Tests
  hs-source-dirs:       test
  ghc-options:          -Wall
  main-is:              Test.hs
  Type:                 exitcode-stdio-1.0
  default-language:     Haskell2010
  build-depends:        base ==4.6.*, Cabal >= 1.16.0
                        , $project
                        , HUnit
                        , QuickCheck
                        , smallcheck
                        , tasty
                        , tasty-hunit
                        , tasty-quickcheck
                        , tasty-smallcheck
END

print -- "\tSetup.hs"
> Setup.hs cat <<END
import Distribution.Simple
main = defaultMain
END

print -- "\tLICENSE (MIT)"
> LICENSE cat<<END
The MIT License (MIT)

Copyright (c) $(date +%Y) $name

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
END

mkdir src test

print -- "\ttest/Test.hs"
> test/Test.hs cat <<END
module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import $module.Swallow.Test
import $module.Coconut.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ swallowSuite
            , coconutSuite
            ]
END

mkdir -p src/$module
mkdir -p test/$module/{Swallow,Coconut}

print -- "\ttest/$module/Swallow/Test.hs"
> test/$module/Swallow/Test.hs cat <<END
module $module.Swallow.Test
    (swallowSuite)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import $module.Swallow

swallowSuite :: TestTree
swallowSuite = testGroup "Swallow"
    [testCase "swallow test" testSwallow]

testSwallow :: Assertion
testSwallow = "something" @=? swallow "some" "thing"
END

print -- "\tsrc/$module/Swallow.hs"
> src/$module/Swallow.hs cat <<END
module $module.Swallow (swallow) where

swallow :: String -> String -> String
swallow prefix suffix = prefix ++ suffix
END

print -- "\ttest/$module/Coconut/Test.hs"
> test/$module/Coconut/Test.hs cat <<END
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module $module.Coconut.Test
    (coconutSuite)
where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import $module.Coconut

-- Make instance of CoconutDataStruct
-- we simply use consN Constr where N is the arity of Constr (SmallCheck)
-- we also needed the
-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.SmallCheck.Series
instance Monad m => Serial m CoconutDataStruct  where series = cons1 CoconutConstr
-- Now we could test properties with smallcheck on CoconutDataStruct type.

coconutSuite :: TestTree
coconutSuite = testGroup "coconut"
    [ testCase "coconut" testCoconut
    , SC.testProperty "coconut property" prop_coconut
    ]

testCoconut :: Assertion
testCoconut = coconut @=? 10

prop_coconut :: Property IO
prop_coconut = forAll $ \coconutStruct -> coconutfunc coconutStruct >= 0
END

print -- "\tsrc/$module/Coconut.hs"
> src/$module/Coconut.hs cat <<END
module $module.Coconut (coconut,coconutfunc,CoconutDataStruct(..)) where
data CoconutDataStruct = CoconutConstr [Integer] deriving (Show)

coconut :: Integer
coconut = 10

coconutfunc :: CoconutDataStruct -> Int
coconutfunc (CoconutConstr l) = length l
END

print -- "\tsrc/$module.hs"
> "src/$module.hs" cat <<END
module $module where
import $module.Swallow ()
import $module.Coconut ()
END

print -- "\tsrc/Main.hs"
> "src/Main.hs" cat <<END
module Main where
main :: IO ()
main = do
    putStrLn "You fight with the strength of many men sir Knight..."
    putStrLn "You have proved yourself worthy; will you join me?"
    putStrLn "You make me sad. So be it. Come, Patsy."
END

print -- "Cabal sandboxing, install and test"

cabal sandbox init || reinit
cabal install
cabal test
testfile="./.cabal-sandbox/bin/test-$project"
[[ -x $testfile ]] && $testfile

# -- Final touch
print "\n\n"
bk "What... is the air-speed velocity of an unladen swallow?"
you "What do you mean? An African or European swallow?"
bk "Huh? I... I don't know that."
log "[the bridgekeeper is thrown over]"
bk "Auuuuuuuuuuuugh"
log "Sir Bedevere: How do you know so much about swallows?"
you "Well, you have to know these things when you're a king, you know."
```
</div>
