-----
isHidden:       false
theme: scientific
image: /Scratch/img/blog/Yesod-tutorial-for-newbies/flying_neo.jpg
menupriority:   1
kind:           article
published: 2012-01-15
title: Haskell web programming
subtitle: A Yesod tutorial
author: Yann Esposito
authoruri: yannesposito.com
tags:  yesod, haskell, programming, web
-----
blogimage("flying_neo.jpg","Neo Flying at warp speed")

<div class="intro">

_update_: updated for Yesod 1.2

%tldr A simple Yesod tutorial.
Yesod is a Haskell web framework.
You shouldn't need to know Haskell.

</div>

Why Haskell?

blogimage("haskell-benchmark.png","Impressive Haskell Benchmark")

Its efficiency (see [Snap Benchmark][snapbench] _&_
[Warp Benchmark][warpbench][^benchmarkdigression]).
Haskell is an order of magnitude faster than interpreted languages
like [Ruby][haskellvsruby] and [Python][haskellvspython][^speeddigression].

Haskell is a high level language that makes it harder to shoot yourself in the foot
than `C`, `C++` or `Java`, for example.
One of the best properties of Haskell is:

> "If your program compiles it will be
>  very close to what the programmer intended".

Haskell web frameworks handle parallel tasks perfectly---even better than
node.js[^nodejstroll], for example.

leftblogimage("thousands_smiths.jpg","Thousands of Agent Smith")

From a purely technical point of view,
Haskell seems to be the perfect web development tool.
Weaknesses of Haskell certainly won't be technical:

- Hard to grasp Haskell
- Hard to find a Haskell programmer
- The Haskell community is smaller than the community for `/.*/`
- <strike>There is not yet a [heroku](http://heroku.com) for Haskell.
  Even In fact, I use heroku to host my websites but this isn't straightforward
  (see the [how to](https://github.com/yesodweb/yesod/wiki/Deploying-Yesod-Apps-to-Heroku)).</strike>
  [FPComplete](http://fpcomplete.com) has now filled this hole.
  And they provide not only cloud hosting but also a
  complete IDE and Haskell environment to work with.

I won't say these are not important drawbacks. But with Haskell your web
application will be able to both absorb an impressive number of parallel
requests securely and to adapt to change.

Actually there are three main Haskell web frameworks:

1. [Happstack](http://happstack.com)
2. [Snap](http://snapframework.com)
3. [Yesod](http://yesodweb.com)

I don't think there is a real winner between these three framework.
The choice I made for Yesod is highly subjective.
I just lurked a bit and tried some tutorials.
I had the feeling Yesod did a better job at helping newcomers.
Furthermore, the Yesod team seems the most active.
Of course I might be wrong since this is just an impression.

blogimage("owl_draw.png","1. Draw some circles. 2. Draw the rest of the fucking owl")

Why did I write this article?
The Yesod documentation and particularly the book are excellent.
But I missed an intermediate tutorial.
This tutorial won't explain all details.
I tried to give a step by step of how to start from a five minute tutorial
to an almost production ready architecture.
Furthermore explaining something to others is a great way to learn.
If you are used to Haskell and Yesod, this tutorial won't teach you much.
If you are completely new to Haskell and Yesod, hopefully it will help you.
Also if you find yourself too confused by the syntax, it might helps to read this
[article](http://blog.ezyang.com/2011/11/how-to-read-haskell/)

During this tutorial you'll install, initialize, and configure your first Yesod project.
Then there is a very minimal 5 minute Yesod tutorial to get us warmed up and confirm the awesomeness of Yesod.
Then we will clean up the 5 minute tutorial to use some "best practices".
Finally there will be a more standard real world example: a minimal blog system.

[snapbench]: http://snapframework.com/blog/2010/11/17/snap-0.3-benchmarks

[warpbench]: http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks

[^benchmarkdigression]: One can argue these benchmark contains many problems.  But the benchmarks are just here to give the basic idea, namely that Haskell is very fast.

[^speeddigression]: Generally _high level_ Haskell is slower than C, but _low level_ Haskell is equivalent to C speed. It means that even if you can easily link C code with Haskell, this is not needed to reach the same speed. Furthermore writing a web service in C/C++ seems to be a very bad idea. You can take a look at a [discussion on HN about this](http://news.ycombinator.com/item?id=3449388).

[^nodejstroll]: If you are curious, you can search about [the Fibonacci node.js troll](http://www.unlimitednovelty.com/2011/10/nodejs-has-jumped-shark.html). Without any tweaking, [Haskell handled this problem perfectly](http://mathias-biilmann.net/posts/2011/10/is-haskell-the-cure). I tested it myself using Yesod instead of Snap.

[haskellvsruby]: http://shootout.alioth.debian.org/u64q/benchmark.php?test=all&lang=ghc&lang2=yarv

[haskellvspython]: http://shootout.alioth.debian.org/u64q/benchmark.php?test=all&lang=ghc&lang2=python3

## Before the real start

### Install

The recommended way to install [Haskell][haskell]
is to download the [Haskell Platform][haskellplatform].

If you want optimal performances I suggest you to download and compile
the latest [GHC Release][ghc] (at least 7.8).
It is generally very easy to install from source.
If you go this way, you'll need to install [cabal][cabal].
You should use at least cabal 1.18.
Don't forget to add cabal binaries to your PATH.
Generally:

~~~
echo 'export PATH=$HOME/.cabal/bin:$PATH' >> ~/.profile
~~~

And to be sure you have the latest cabal release:

~~~~~~ {.bash}
cabal update
cabal install cabal-install
~~~~~~

[haskell]: http://www.haskell.org

[haskellplatform]: http://www.haskell.org/platform

[ghc]: http://ghc.haskell.org

[cabal]: http://www.haskell.org/cabal/download.html

Once done, you need to install Yesod.

Then open a terminal session and do:

~~~~~~ {.bash}
cabal install yesod-bin
~~~~~~

There are few steps but it should take some time to finish.

### Initialize

You are now ready to initialize your first Yesod project.
Open a terminal and type:

~~~~~~ {.bash}
yesod init
~~~~~~

Enter your name, choose `yosog` for the project name and enter `Yosog` for the name of the Foundation.
Finally choose `sqlite`.
Now, start the development cycle:

~~~~~~ {.bash}
cd yosog
cabal sandbox init
cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals
yesod devel
~~~~~~

This will compile the entire project. Be patient, since it will take a while the first time.
Once finished a server is launched and you can visit it by clicking this link:

[`http://localhost:3000`](http://localhost:3000)

Congratulation! Yesod works!

<blockquote>

Note: if something is messed up use the following command at the command line inside the project directory.

~~~~~~ {.bash}
\rm -rf .cabal-sandbox cabal.sandbox.config
cabal sandbox init && cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals && yesod devel

~~~~~~

</blockquote>

For the rest of the tutorial, use another terminal and keep this one open
in a corner to see what happens.

### Configure git

> Of course this step is not mandatory for the tutorial
> but it is a good practice.

Fortunately, there is already a `.gitignore` file into the `yosog` folder.
You just have to initialize your git repository:

~~~~~~ {.bash}
git init .
git add .
git commit -a -m "Initial yesod commit"
~~~~~~

We are almost ready to start.

### A few words before we start

Up until here, we have a directory containing a bunch of files
and a local web server listening the port 3000.
If we modify a file inside this directory, Yesod should try
to recompile as fast as possible the site.
Instead of explaining the role of every file,
let's focus only on the important files/directories for this tutorial:

1. `config/routes`
2. `Handler/`
3. `templates/`
4. `config/models`

Obviously:

<table><tr><td> `config/routes`      </td><td>is where you'll configure the map %url → Code.
</td></tr><tr><td> `Handler/`        </td><td> contains the files that will contain the code called when a %url is accessed.
</td></tr><tr><td> `templates/`      </td><td> contains %html, js and %css templates.
</td></tr><tr><td> `config/models`   </td><td> is where you'll configure the persistent objects (database tables).
</td></tr></table>

During this tutorial we'll modify other files as well,
but we won't explore them in detail.

Also note, shell commands are executed in the root directory of your project unless specified otherwise.

We are now ready to start!

## Echo

To verify the quality of the security of the Yesod framework,
let's make a minimal echo application.

> Goal:
>
> Make a server that when accessed `/echo/[some text]` should return a web page containing "some text" inside an `h1` bloc.

~~~ {.no-highlight}
~/Sites/yosog $ {-hi-}yesod add-handler{-/hi-}
Name of route (without trailing R): {-hi-}Echo{-/hi-}
Enter route pattern (ex: /entry/#EntryId): {-hi-}/echo/#String{-/hi-}
Enter space-separated list of methods (ex: GET POST): {-hi-}GET{-/hi-}
~~~

Almost all the work is done for us. The `add-handler` does the following:

Updates the `config/route` file by appending:

~~~
/echo/#String EchoR GET
~~~

This line contains three elements: the %url pattern, a handler name, an %http method.

- creates a `Handler/Echo.hs` file
- imports `Handler.Echo` in the main `Application.hs` file
- declares `Handler.Echo` in the cabal file for building the application

Now try to go to [`localhost:3000/echo/foo`](http://localhost:3000/echo/foo).
You should get a message explaining that `getEchoR` is not yet implemented.

So let's take a look at `Handler/Echo.hs`:

~~~ {.haskell}
module Handler.Echo where

import Import

getEchoR :: String -> Handler Html
getEchoR = error "Not yet implemented: getEchoR"
~~~

This should be straightforward.
Now we can replace it with this:

~~~ {.haskell}
module Handler.Echo where

import Import

getEchoR :: String -> Handler Html
getEchoR theText = defaultLayout [whamlet|<h1>#{theText}|]
~~~

Don't worry if you find all of this a bit cryptic.
In short it just declares a function named `getEchoR` with one argument (`theText`) of type `String`.
When this function is called, it returns a `Handler Html` whatever it is.
But mainly this will encapsulate our expected result inside an %html text.

After saving the file, you should see Yesod recompile the application.
When the compilation is finished you'll see the message: `Starting devel application`.

Now you can visit: [`http://localhost:3000/echo/Yesod%20rocks!`](http://localhost:3000/echo/Yesod%20rocks!)

TADA! It works!

### Bulletproof?

blogimage("neo_bullet_proof.jpg","Neo stops a myriad of bullets")

Even this extremely minimal web application has some impressive properties.
For example, imagine an attacker entering this %url:

<div class="small">

[`http://localhost:3000/echo/I'm <script>alert(\"Bad!\");</script>`][bad]

</div>

[bad]: http://localhost:3000/echo/I'm%20%3Cscript%3Ealert(%22Bad!%22);%3C%2Fscript%3E

You can click on it to test it.

The special characters are protected for us, preventing a malicious user from
hiding a bad script within the url.

This behavior is a direct consequence of _type safety_.
The %url string is put inside a %url type.
Then the interesting part in the %url is put inside a String type.
To pass from %url type to String type some transformations are made.
For example, all instances of "`%20`" are replaced by space characters.
Then to show the String inside an %html document, the string is put inside an %html type.
Some transformations occurs like replace "<code><</code>" by "`&lt;`".
Thanks to Yesod, this tedious job is done for us.

~~~~~~ {.bash}
"http://localhost:3000/echo/some%20text%3Ca%3E" :: URL
                    ↓
              "some text<a>"                    :: String
                    ↓
          "some text &amp;lt;a&amp;gt;"         :: Html
~~~~~~

Yesod is not only fast, it helps us to remain secure.
It protects us from many common errors in other paradigms.
Yes, I am looking at you, PHP!

### Cleaning up

Even this very minimal example should be enhanced.
We will clean up many details:

- Use `Data.Text` instead of `String`
- Put our "views"[^explainviewwidget] inside the `template` directory

[^explainviewwidget]: By view I mean yesod widget's hamlet, lucius and julius files.

#### `Data.Text`

It is a good practice to use `Data.Text` instead of `String`.

To declare it, add this import directive to `Foundation.hs` (just after the last one):

~~~~~~ {.diff}
import Data.Text
~~~~~~

We have to modify `config/routes` and our handler accordingly.
Replace `#String` by `#Text` in `config/routes`:

<pre>
/echo/{-hi-}#Text{-/hi-} EchoR GET
</pre>

And do the same in `Handler/Echo.hs`:

~~~~~~ {.haskell}
module Handler.Echo where

import Import

getEchoR :: {-hi-}Text{-/hi-} -> Handler Html
getEchoR theText = defaultLayout [whamlet|<h1>#{theText}|]
~~~~~~

#### Use templates

Some %html (more precisely hamlet) is written directly inside our handler.
We should put this part inside another file.
Create the new file `templates/echo.hamlet` containing:

~~~~~~ {.haskell}
<h1> #{theText}
~~~~~~

and modify the handler `Handler/Echo.hs`:

~~~~~~ {.haskell}
module Handler.Echo where

import Import

getEchoR :: Text -> Handler Html
getEchoR theText = defaultLayout {-hi-}$(widgetFile "echo"){-/hi-}
~~~~~~

At this point, our web application is nicely structured.
We use `Data.Text` and our views are in templates.
It is the time to make a slightly more complex example.

## Mirror

leftblogimage("mirror.jpg","Neo touching a mirror")

Let's make another minimal application.
You should see a form containing a text field and a validation button.
When you enter some text (for example "Jormungad") and validate it,
the next page presents the content to you with its reverse appended to it.
In our example, it should return "JormungaddagnumroJ".

First, add a new handler:

~~~ {.no-highlight}
 ~/Sites/yosog (master) $ {-hi-}yesod add-handler{-/hi-}
Name of route (without trailing R): {-hi-}Mirror{-/hi-}
Enter route pattern (ex: /entry/#EntryId): {-hi-}/mirror{-/hi-}
Enter space-separated list of methods (ex: GET POST): {-hi-}GET POST{-/hi-}
~~~

This time the path `/mirror` will accept GET and POST requests.
Update the corresponding new Handler file (`Handler/Mirror.hs`):

~~~~~~ {.haskell}
module Handler.Mirror where

import Import
import qualified Data.Text as T

getMirrorR :: Handler Html
getMirrorR = defaultLayout $(widgetFile "mirror")

postMirrorR :: Handler Html
postMirrorR =  do
        postedText <- runInputPost $ ireq textField "content"
        defaultLayout $(widgetFile "posted")
~~~~~~

We will need to use the `reverse` function provided by `Data.Text`
which explains the additional import.

The only new thing here is the line that gets the POST parameter named "content".
If you want to know more detail about it and forms in general you can take a
look at [the Yesod book](http://www.yesodweb.com/book/forms).

Create the two corresponding templates
(`templates/mirror.hamlet` and `templates/posted.hamlet`):

~~~~~~ {.html}
<h1> Enter your text
<form method=post action=@{MirrorR}>
    <input type=text name=content>
    <input type=submit>
~~~~~~

~~~~~~ {.html}
<h1>You've just posted
<p>#{postedText}#{T.reverse postedText}
<hr>
<p><a href=@{MirrorR}>Get back
~~~~~~

And that is all.
This time, we won't need to clean up.
We might have generated the form a different way,
but we'll see how to do this in the next section.

Just try it by [clicking here](http://localhost:3000/mirror).

Also you can try to enter strange values (like `<script>alert('Bad');</script>`).
As before, your application is quite secure.

## A Blog

We saw how to retrieve %http parameters.
It is the time to save things into a database.

This example will be very minimal:

- `GET`  on `/blog` should display the list of articles,
- `POST` on `/blog` should create a new article,
- `GET`  on `/blog/<article id>` should display the content of the article.

As before, we'll start by adding some handlers:

~~~
~/Sites/yosog (master) $ yesod add-handler
Name of route (without trailing R): Blog
Enter route pattern (ex: /entry/#EntryId): /blog
Enter space-separated list of methods (ex: GET POST): GET POST

~/Sites/yosog (master) $ yesod add-handler
Name of route (without trailing R): Article
Enter route pattern (ex: /entry/#EntryId): /blog/#ArticleId
Enter space-separated list of methods (ex: GET POST): GET
~~~

Then we declare another model object.
Append the following content to `config/models`:

~~~
Article
    title   Text
    content Html
    deriving
~~~

As `Html` is not an instance of `Read`, `Show` and `Eq`,
we had to add the `deriving` line.
If you forget it, there will be an error.

After the route and the model, we write the handler.
Let's write the content of `Handler/Blog.hs`.
We start by declaring the module and by importing some block necessary to
handle Html in forms.

~~~~~~ {.haskell}
module Handler.Blog
    ( getBlogR
    , postBlogR
    )
where

import Import

-- to use Html into forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App
~~~~~~

_Remark_: it is a best practice to add the YesodNic instance inside `Foundation.hs`.
I put this definition here to make things easier but you should see a warning about this orphan instance.
To put the include inside Foundation.hs is left as an exercice to the reader.

_Hint_: Do not forget to put `YesodNic` and `nicHtmlField` inside the exported objects of the module.

~~~~~~ {.haskell}
entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq   textField "Title" Nothing
    <*> areq   nicHtmlField "Content" Nothing
~~~~~~

This function defines a form for adding a new article.
Don't pay attention to all the syntax.
If you are curious you can take a look at Applicative Functor.
You just have to remember `areq` is for required form input.
Its arguments being: `areq type label default_value`.

~~~~~~ {.haskell}
-- The view showing the list of articles
getBlogR :: Handler Html
getBlogR = do
    -- Get the list of articles inside the database.
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")
~~~~~~

This handler should display a list of articles.
We get the list from the DB and we construct the form.
Just take a look at the corresponding template:

~~~~~~ {.html}
<h1> Articles
$if null articles
    <p> There are no articles in the blog
$else
    <ul>
        $forall Entity articleId article <- articles
            <li>
                <a href=@{ArticleR articleId} > #{articleTitle article}
<hr>
  <form method=post enctype=#{enctype}>
    ^{articleWidget}
    <div>
        <input type=submit value="Post New Article">
~~~~~~

Notice we added some logic inside the template.
There is a test and a "loop".

Another very interesting part is the creation of the form.
The `articleWidget` was created by Yesod.
We have given it the right parameters
(input required or optional, labels, default values),
and now we have a protected form made for us.
But now we have to create the submit button.

You can take a first look by [clicking here](localhost:3000/blog).
Of course, you can't post something yet.

Get back to `Handler/Blog.hs`.

~~~~~~ {.haskell}
postBlogR :: Handler Html
postBlogR = do
    ((res,articleWidget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")
~~~~~~

This function should be used to create a new article.
We handle the form response.
If there is an error we display an error page, for example if we left some required value blank.
If things go well:

- we add the new article inside the DB (`runDB $ insert article`)
- we add a message to be displayed (`setMessage $ ...`)
- we are redirected to the article web page.

Here is the content of the error Page:

~~~~~~ {.haskell}
<form method=post enctype=#{enctype}>
    ^{articleWidget}
    <div>
        <input type=submit value="Post New Article">
~~~~~~

Finally we need to display an article.
For this we will modify `Handler/Article.hs`

~~~~~~ {.haskell}
getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
~~~~~~

The `get404` function tries to do a get on the DB.
If it fails, it returns a 404 page.
The rest should be clear.
Here is the content of `templates/article.hamlet`:

~~~~~~ {.html}
<h1> #{articleTitle article}
<article> #{articleContent article}
<hr>
<a href=@{BlogR}>
    Go to article list.
~~~~~~

The blog system is finished.
You can jump to it by clicking [here](http://localhost:3000/blog).

Just for fun, you can try to create an article with the following content:

~~~~~~ {.html}
Cross Script:
   <script>alert("You loose");</script>

SQL injection: "); DROP TABLE ARTICLE;;
~~~~~~

## Conclusion

This is the end of this tutorial.
I made it very minimal.

If you already know Haskell and you want to go further,
you should take a look at the
recent [i18n blog tutorial](http://yesodweb.com/blog/2012/01/blog-example).
It will be obvious I based my own tutorial on it.
You'll learn in a very straightforward way how easy it is to use authorization,
Time and internationalization.

If, on the other hand, you don't know Haskell, then you shouldn't jump directly
to web programming with it.
Haskell is a very complex and unusual language.
My advice to go as fast as possible in using Haskell for web programming is:

1. Start by [trying Haskell in your browser](http://tryhaskell.org)
2. Read my tutorial [Learn Haskell Fast and Hard on School of Haskell](https://www.fpcomplete.com/school/haskell-fast-hard) or directly [on this blog](/Scratch/en/blog/Haskell-the-Hard-Way/)
3. Then read the excellent [Learn you a Haskell for Great Good](http://learnyouahaskell.com)
4. If you have difficulty understanding concepts like monads, you should really read [these articles](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html). For me they were enlightening.
5. If you feel confident, you should be able to follow the
   [Yesod book](http://yesodweb.com/book) but if you find it difficult to follow the Yesod book, you should read [real world Haskell](http://book.realworldhaskell.org) first.

Also, note that:

- [haskell.org](http://haskell.org) is full of excellent resources.
- [hoogle](http://www.haskell.org/hoogle/) will be very useful
- Use [hlint](http://community.haskell.org/~ndm/hlint/) as soon as possible to get good habits.

As you should see, if you don't already know Haskell,
the path is long but I guarantee you it will be very rewarding!

_ps:_ You can download the source of this Yesod blog tutorial at
[github.com/yogsototh/yosog](http://github.com/yogsototh/yosog).

