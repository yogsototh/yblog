---
kind:           article
published:      2013-11-14
image: /Scratch/img/blog/Holy-Haskell-Starter/holy-grail-monty-python.jpg
title: Holy Haskell Project Starter
author: Yann Esposito
authoruri: yannesposito.com
tags: programming
theme: modern
---

blogimage("holy-grail-monty-python.jpg","Monty Python Holy Grail")

<div class="intro">

%tldr A Haskell project starter rewritten from zsh to Haskell.



In order to work properly with Haskell you need to initialize your environment.
Typically, you need to use a cabal file, create some test for your code.
Both, unit test and propositional testing
(random and exhaustive up to a certain depth).
You need to use `git` and generally hosting it on github.
Also, it is recommended to use cabal sandboxes.
And as bonus, an auto-update tool that recompile and retest on each file save.

In this article, we will create such an environment using a zsh script.
Then we will write a Haskell project which does the same work as the zsh script.
You will then see how to work in such an environment.

If you are starting to understand Haskell but consider yourself a beginner,
this tutorial will show you how to make a real application using
quite surprisingly a lot of features:

- use colorized output
- interact with a user in command line
- read/write files
- kind of parse a file (in fact, simply split it)
- use a templating system (mustache: fill a data structure, write files)
- make a wget then parse the JSON answer and use it
- use random
- create a cabal package
- add and use non source files to a cabal package
- compare Haskell to zsh
- Test your code (both unit testing and property testing)

**☞** zsh is by its nature more suitable to file manipulation.
But the Haskell code is clearly more organized while quite terse
for a multi-purpose language.

</div>


I recently read this excellent article:
[How to Start a New Haskell Project](http://jabberwocky.eu/2013/10/24/how-to-start-a-new-haskell-project/).

While the article is very good, I lacked some minor informations[^1].
As this is a process you might repeat often,
I created a simple script to initialize a new Haskell project.
During the process I improved some things a bit:

- use `Tasty` instead of `test-framework`
- compile with `-Wall` option
- use cabal sandbox
- initialize a `.gitignore`

And certainly other minor things. You should get the idea.

[^1]: For example, you have to install the test libraries manually to use `cabal test`.

If you do it manually the steps are:

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
cd my/projects/directory
# Launch thcript
holy-haskell.sh
```

What does this script do that doesn't do `cabal init`.

- Use cabal sandbox
- It initialize `git` with the right `.gitignore` file.
- Use `tasty` to organize your tests (HUnit, QuickCheck and SmallCheck).
- Use `-Wall` for `ghc` compilation.
- Will make references to Holy Grail
- Search your default github username via [github api](http://developer.github.com/v3/search/#search-users).

## `zsh` really?


blogimage("french-insult.jpg","French insult")

Developing the script in `zsh` was easy.
And while `zsh` is my favorite shell script, the size of this script
make it worth to write it in a more secure language.
Furthermore it will be a good exercise to translate
this script from `zsh` to Haskell.

### Patricide

So to make our development, let us initialize a new Haskell project with
`holy-haskell.sh`.
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

blogimage("bridge-of-death.jpg","Bridge of Death")

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
[`ansi-terminal`](http://hackage.haskell.org/package/ansi-terminal) package.
So we have to add `ansi-terminal` as a build dependency in our cabal file.

Edit `holy-project.cabal` to add it.

```
...
build-depends:  base >=4.6 && <4.7
                {-hi-}, ansi-terminal{-/hi-}
...
```

Now look at the modified Haskell code:

``` haskell
{-hi-}import System.Console.ANSI{-/hi-}

colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
    setSGR  [ SetColor Foreground Dull color
            , SetConsoleIntensity NormalIntensity
            ]
    putStr str
    setSGR []


bk :: String -> IO ()
bk str = {-hi-}colorPutStr Green{-/hi-} ("Bridgekeeper: " ++ str ++ "\n")
bkn :: String -> IO ()
bkn str = {-hi-}colorPutStr Green{-/hi-} ("Bridgekeeper: " ++ str)
you :: String -> IO ()
you str = {-hi-}colorPutStr Yellow{-/hi-} ("Bridgekeeper: " ++ str ++ "\n")

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



## Five Questions -- Three questions Sir!

In order to ask questions, here is how we do it in shell script:

``` bash
print -- "What is your name?"
read name
```

If we want to abstract things a bit, the easiest way in shell is to use
a global variable[^2] which will get the value of the user input like this:

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

[^2]: There is no easy way to do something like `name=$(ask name)`.
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

In order to achieve the same result in Haskell
(don't forget to add the `split` package):

``` haskell
import Data.List        (instersperse)
import Data.List.Split  (splitOneOf)
...
projectNameFromString :: String -> String
projectNameFromString str = concat $ intersperse "-"
    (splitOneOf " -" (map toLower str))
```

One important thing to note is that in zsh the transformation occurs
on strings but in haskell we use list as intermediate representation:

```
zsh:
"Holy grail" ==( ${project:gs/ /-/} )=> "Holy{-hi-}-{-/hi-}grail"
             ==( ${project:l}       )=> "{-hi-}h{-/hi-}oly-grail"

haskell
"Holy grail" ==( map toLower     )=> "{-hi-}h{-/hi-}oly grail"
             ==( splitOneOf " -" )=> {-hi-}[{-/hi-}"holy"{-hi-},{-/hi-}"grail"{-hi-}]{-/hi-}
             ==( intersperse "-" )=> ["holy",{-hi-}"-"{-/hi-},"grail"]
             ==( concat          )=> "holy-grail"

```


### Create the module name

The module name is a capitalized version of the project name where we remove
dashes.

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
"Holy-grail" ==( sed 's/-/ /g' )=> "Holy{-hi-} {-/hi-}grail"
             ==( ${(C)str}     )=> "Holy {-hi-}G{-/hi-}rail"
             ==( sed 's/ //g'  )=> "HolyGrail"

haskell:
"Holy-grail" ==( splitOneOf " -"    )=> {-hi-}[{-/hi-}"Holy"{-hi-},{-/hi-}"grail"{-hi-}]{-/hi-}
             ==( map capitalizeWord )=> ["Holy","{-hi-}G{-/hi-}rail"]
             ==( concat             )=> "HolyGrail"
```

As the preceding example, in shell we work on strings while Haskell use temporary lists representations.

### Check the project name

Also I want to be quite restrictive on the kind of project name we can give.
This is why I added a check function.

``` haskell
ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = error str

main :: IO ()
main = do
  intro
  project <- ask "project name"
  ioassert (checkProjectName project)
       "Use only letters, numbers, spaces ans dashes please"
  let projectname = projectNameFromString project
      modulename = capitalize project
```

Which verify the project name is not empty and use only letter, numbers and dashes:

``` haskell
-- | verify if project name is conform
checkProjectName :: String -> Bool
checkProjectName [] = False
checkProjectName str = all (\c -> (isLetter c)||(isNumber c)||(c=='-')||(c==' ')) str
```

## Create the project

blogimage("giant-three-head.jpg","Giant with three heads and mustaches")

Making a project will consists in creating files and directories whose
name and content depends on the answer we had until now.

In shell, for each file to create, we used something like:

``` bash
> file-to-create cat <<END
file content here.
We can use $variables here
END
```

In Haskell, while possible, we shouldn't put the file content in the source code.
We have a relatively easy way to include external file in a cabal package.
This is what we will be using.

Furthermore, we need a templating system to replace small part of the
static file by computed values.
For this task, I choose to use
[`hastache`](http://hackage.haskell.org/package/hastache),
an haskell implementation of Mustache templates.

### Add external files in a cabal project

Cabal provides a way to add files which are not source files to a package.
You simply have to add a `Data-Files:` entry in the header.

```
data-files: scaffold/LICENSE
            , scaffold/gitignore
            , scaffold/project.cabal
            , scaffold/Setup.hs
            , scaffold/src/Main.hs
            , scaffold/src/ModuleName/Coconut.hs
            , scaffold/src/ModuleName.hs
            , scaffold/src/ModuleName/Swallow.hs
            , scaffold/test/ModuleName/Coconut/Test.hs
            , scaffold/test/ModuleName/Swallow/Test.hs
            , scaffold/test/Test.hs
```

Now we simply have to create our files at the specified path.
Here is for example the first lines of the LICENSE file.

``` mustache
The MIT License (MIT)

Copyright (c) {-hi-}{{year}}{-/hi-} {-hi-}{{author}}{-/hi-}

Permission is hereby granted, free of charge, to any person obtaining a copy
...
```

It will be up to our program to replace the `{{year}}` and `{{author}}` at runtime.
Now we have to find them, and in fact, cabal will create a module named
`Paths_holy_project`.
If we import this module we have the function `genDataFileName` at our disposal.
We then are able to read the files at runtime like this:

``` haskell
  ...
  do
    pkgFilePath     <- {-hi-}getDataFileName "scaffold/LICENSE"{-/hi-}
    templateContent <- readFile pkgFilePath
    ...
```


### Create files and directories

A first remark is for portability purpose we shouldn't use String for file path.
For example on Windows `/` isn't considered as a subdirectory character.
To resolve this problem we will use `FilePath`:

``` haskell
import System.Directory
import System.FilePath.Posix        (takeDirectory,(</>))
...
createProject ... = do
      ...
      {-hi-}createDirectory{-/hi-} projectName
      {-hi-}setCurrentDirectory{-/hi-} projectName
      genFile "LICENSE" "LICENSE"
      genFile "gitignore" ".gitignore"
      genFile "src/Main.hs" ("src" </> "Main.hs")

genFile dataFilename outputFilename = do
    pkgfileName <- getDataFileName ("scaffold/" ++ filename)
    template <- readFile pkgfileName
    transformedFile <- ??? -- hastache magic here
    {-hi-}createDirectoryIfMissing{-/hi-} True (takeDirectory outputFileName)
    {-hi-}writeFile{-/hi-} outputFileName transformedFile
```

### Use Hastache

In order to use hastache we can either create a context manually or use
generics to create a context from a record. This is the last option
we will show here.
So in a first time, we need to import some modules and declare a
record containing all necessary informations to create our project.

``` haskell
{-# LANGUAGE DeriveDataTypeable #-}
...
import Data.Data
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LZ
```

``` haskell
data Project = Project {
    projectName   :: String
    , moduleName    :: String
    , author        :: String
    , mail          :: String
    , ghaccount     :: String
    , synopsis      :: String
    , year          :: String
    } deriving (Data, Typeable)
```

Once we have declared this, we should populate our Project record with
the data provided by the user. So our main should now look like:

``` haskell
main :: IO ()
main = do
    intro
    project <- ask "project name"
    ioassert (checkProjectName project)
             "Use only letters, numbers, spaces ans dashes please"
    let projectname = projectNameFromString project
        modulename  = capitalize project
    {-hi-}in_author{-/hi-}       <- ask "name"
    {-hi-}in_email{-/hi-}        <- ask "email"
    {-hi-}in_ghaccount{-/hi-}    <- ask "github account"
    {-hi-}in_synopsis{-/hi-}     <- ask "project in less than a dozen word?"
    {-hi-}current_year{-/hi-}    <- getCurrentYear
    createProject $ {-hi-}Project projectname modulename in_author in_email{-/hi-}
                            {-hi-}in_ghaccount in_synopsis current_year{-/hi-}
    end
```

Finally we could use hastache this way:

``` haskell
createProject :: {-hi-}Project{-/hi-} -> IO ()
createProject {-hi-}p{-/hi-} = do
    let {-hi-}context{-/hi-} = {-hi-}mkGenericContext p{-/hi-}
    createDirectory ({-hi-}projectName p{-/hi-})
    setCurrentDirectory ({-hi-}projectName p{-/hi-})
    genFile {-hi-}context{-/hi-} "gitignore"      $ ".gitignore"
    genFile {-hi-}context{-/hi-} "project.cabal"  $ (projectName p) ++ ".cabal"
    genFile {-hi-}context{-/hi-} "src/Main.hs")   $ "src" </> "Main.hs"
    ...

genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context filename outputFileName = do
    pkgfileName <- getDataFileName ("scaffold/"++filename)
    template <- {-hi-}BS.{-/hi-}readFile pkgfileName
    transformedFile <- {-hi-}hastacheStr defaultConfig template context{-/hi-}
    createDirectoryIfMissing True (takeDirectory outputFileName)
    {-hi-}LZ.{-/hi-}writeFile outputFileName transformedFile
```

So now, we use external files in mustache format.
We ask question to our user to fill a data structure.
Hastache use this filled data structure with the external files to initialize
the project.

## Git and Cabal

We need to initialize git and cabal.
For this we simply call external command with the `system` function.

``` haskell
import System.Cmd

...
main = do
    ...
    _ <- system "git init ."
    _ <- system "cabal sandbox init"
    _ <- system "cabal install"
    _ <- system "cabal test"
    _ <- system $ "./.cabal-sandbox/bin/test-" ++ projectName
```

## Ameliorations

Our job is almost finished.
Now, we only need to add some nice feature to make the application more
enjoyable.

### Better error message

The first one would be to add a better error message.

``` haskell
import System.Random

holyError :: String -> IO ()
holyError str = do
    r <- randomIO
    if r
        then
            do
                bk "What... is your favourite colour?"
                you "Blue. No, yel..."
                putStrLn "[You are thrown over the edge into the volcano]"
                you "You: Auuuuuuuuuuuugh"
                bk " Hee hee heh."
        else
            do
                bk "What is the capital of Assyria?"
                you "I don't know that!"
                putStrLn "[You are thrown over the edge into the volcano]"
                you "Auuuuuuuuuuuugh"
    error ('\n':str)
```

And also update where this can be called

``` haskell
ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = holyError str
```

### Use `.gitconfig` and `github` API

We want to retrieve the `~/.gitconfig` file content and see if it
contains a name and email information.
We will need to access to the `HOME` environment variable.
Also, as we use bytestring package for hastache, let's take advantage of
this library.

``` haskell
import Data.Maybe           (fromJust)
import System.Environment   (getEnv)
import Control.Exception
import System.IO.Error
import Control.Monad        (guard)

safeReadGitConfig :: IO LZ.ByteString
safeReadGitConfig = do
    e <- tryJust (guard . isDoesNotExistError)
                 (do
                    home <- getEnv "HOME"
                    LZ.readFile $ home ++ "/.gitconfig" )
    return $ either (const (LZ.empty)) id e
...
main = do
    gitconfig <- safeReadGitConfig
    let (name,email) = {-hi-}getNameAndMail{-/hi-} gitconfig
    project <- ask "project name" Nothing
    ...
    in_author       <- ask "name" name
    ...
```

We could note I changed the ask function slightly to take a maybe parameter.

``` haskell
ask :: String {-hi-}-> Maybe String{-/hi-} -> IO String
ask info hint = do
    bk $ "What is your " ++ info ++ "?" ++ {-hi-}(maybe "" (\h -> " ("++h++")") hint){-/hi-}
    ...
```

Concerning the parsing of `.gitconfig`, it is quite minimalist.

``` haskell
getNameAndMail :: LZ.ByteString -> (Maybe String,Maybe String)
getNameAndMail gitConfigContent = (getFirstValueFor splitted "name",
                                   getFirstValueFor splitted "email")
    where
        -- make lines of words
        splitted :: [[LZ.ByteString]]
        splitted = map LZ.words (LZ.lines gitConfigContent)

-- Get the first line which start with
-- 'elem =' and return the third field (value)
getFirstValueFor :: [[LZ.ByteString]] -> String -> Maybe String
getFirstValueFor splitted key = firstJust (map (getValueForKey key) splitted)

-- return the first Just value of a list of Maybe
firstJust :: (Eq a) => [Maybe a] -> Maybe a
firstJust l = case dropWhile (==Nothing) l of
    [] -> Nothing
    (j:_) -> j

-- Given a line of words ("word1":"word2":rest)
-- getValue will return rest if word1 == key
-- 'elem =' or Nothing otherwise
getValueForKey :: String            -- key
                  -> [LZ.ByteString] -- line of words
                  -> Maybe String    -- the value if found
getValueForKey el (n:e:xs) = if (n == (LZ.pack el)) && (e == (LZ.pack "="))
                        then Just (LZ.unpack (LZ.unwords xs))
                        else Nothing
getValueForKey _ _ = Nothing
```

We could notice, `getNameAndMail` doesn't read the full file and stop at the
first occurrence of name and mail.

### Use the github API

The task seem relatively easy, but we'll see there will be some complexity hidden.
Make a request on `https://api.github.com/search/users?q=<email>`.
Parse the JSON and get the `login` field of the first item.

So the first problem to handle is to connect an URL.
For this we will use the `http-conduit` package.

In the general case, for something simple as this we should have used:

``` haskell
do
    body <- simpleHTTP "https://api.github.com/search/users?q="++email
    ...
```

But, after some research, I discovered we must declare a User-Agent in the HTTP header
to be accepted by the github API. So we have to change the HTTP Header, and
our code became slightly more complex:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
...
simpleHTTPWithUserAgent url = do
    r  <- parseUrl url
    let request = r { requestHeaders =  [ ("User-Agent","HTTP-Conduit") ] }
    body <- withManager $ \manager -> do
                response <- httpLbs request manager
                return $ responseBody response
    let str = LZ.unpack body
    return $ Just str

getGHUser :: String -> IO (Maybe String)
getGHUser email = do
            let url = "https://api.github.com/search/users?q=" ++ email
            body <- simpleHTTPWithUserAgent url
            ...
```

So now, we have a String containing a JSON representation.
In javascript we would have used `login=JSON.parse(body).items[0].login`.
How does Haskell will handle it (knowing the J in JSON is for Javascript)?

First we will need to add the `lens-aeson` package and use it that way:

``` haskell
getGHUser :: String -> IO (Maybe String)
getGHUser email = do
            body <- simpleHTTPWithUserAgent $ "https://api.github.com/search/users?q=" ++ email
            login <- return $ {-hi-}body ^? key "items" . nth 0 . key "login"{-/hi-}
            return $ fmap jsonValueToString login
            where
                jsonValueToString = TLZ.unpack . TLB.toLazyText . fromValue
```

Ugly, but terse enough.
Also I didn't found anything better than `jsonValueToString`, I hope it exists.

Read [this article on `lens-aeson` and prisms](https://www.fpcomplete.com/user/tel/lens-aeson-traversals-prisms) if you want to understand how this works.

Now, we have something as good as the zsh script shell. But wouldn't it be
better to launch the API request sooner. Because, actually you have to wait during
answering the questions.

Wouldn't it be better to launch the request in parallel?
Let's do it:


