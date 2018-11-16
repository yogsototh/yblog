---
title: Resume
author: Yann Esposito
abstract: Yann Esposito's Resume
theme: brutalist
highlight-style: solarized-dark
date: 26 July 2016
---


\newpage

# Yann Esposito

\iffalse

[PDF Version](./resume.pdf)

\fi

-------- ---------------------------------------------------------------
name     Yann Esposito
mail     <yann.esposito@gmail.com>
port     (+33)650845271
address  Bât 9, Résidence Saint Marc
         591, avenue Jean Aicard
         06700, Saint Laurent du Var
-------- ---------------------------------------------------------------

## Professional Background

--------- ------ -------- -----------------------------------------------------------------------------------------
   _2016_  &rarr;         Clojure Software Engineer for Cisco (Threatgrid), _Remote_

   _2013_  &rarr; _2016_  Machine Learning Scientist &amp; Software Engineer at Vigiglobe,
                          _Sophia Antipolis, France_

   _2010_ &rarr;          Co-Founder of GridPocket, _Sophia Antipolis, France_

   _2007_ &rarr; _2013_   AirFrance, _Sophia Antipolis, France_

_10/2006_ &rarr; _3/2007_ Post Ph.D., Hubert Curien Laboratory, _St-Etienne, France_

_10/2004_ &rarr; _9/2006_ ATER (College Degree Teach _&_ Research), _Marseille, France_

_10/2001_ &rarr; _9/2004_ University Monitor (College Degree Teach _&_ Research),
                          _Marseille, France_

   _1995_ &rarr; _2000_   Miscellaneous summer jobs
--------- ------ -------- -----------------------------------------------------------------------------------------

## Education

----- ---------------------------------------------
_2004_  CS Ph.D. in Machine Learning at Université de Provence
_2001_  D.E.A. (Equivalent to Master in Computer science)
_2000_  Maîtrise in Computer Science
_1999_  Licence in Computer Science
_1998_  DEUG MIAS (Math)
_1995_  BAC S (Math)
----- ---------------------------------------------

## Research Activies: Publications

-------------------------- ---------------------------------------------
_International Journal_    [Fundamenta Informaticæ, 2008]
                           [Pattern&nbsp;Recognition,&nbsp;2004]
_Internation Conferences_  [ECML 2008] [ICGI 2006] [COLT 2006]
                           [COLT 2004] [ICALP 2003] [ICGI 2002]
_National Journal_         [JEDAI 2002]
_National Conferences_     [CAp'06] [CAp'04] [CAp'03]
-------------------------- ---------------------------------------------

\newpage

# Presentation

I am French with a Post Ph.D in Machine Learning[^10].
Furthermore I love web programming and design.

I am currently working remotely for Cisco Security team
as a Clojure Software Engineer.

Previously I worked for Vigiglobe.
The first six months I worked with `node.js` (API/MongoDB/Web).
Then we upgraded our stack to _Clojure, Haskell, Mesos, Kafka, Druid_, etc...
At that time we were two to make all technical decisions.
In the end we made a real time analytics of social media content on a scalable architecture.
Actually our architecture is able to manage (Aggregation &amp; Machine Learning) thousands of messages per second.[^7]
In particular, I've written an Haskell twitter stream absorber able to handle thousands of tweets per seconds. And I coded myself a real time sentiment analysis module taking algebraic properties into account to optimize its efficiency.

- During my Ph.D. I made a C++ program (github[^1] and resume[^2]).
  I coded most of standard HMM learning algorithms.
  I developed an algorithm which I invented during my Ph.D. which use some operational optimization algorithm.
  During this period I published articles in international conferences and
  I taught Computer Science to college students.

- At the Hubert Curien Laboratory I made my post Ph.D.
  I developed a scientific application in Java/applet/JWS that should be used by biologists.
  The code has been updated a bit since my 6 month post Ph.D[^3].

- I worked in the web industry for Airfrance.
  My work environment was quite heterogeneous.
  From shell scripting to manage huge amount of data, web design
  and production environment.

- I worked for GridPocket (I am a co-founder).
  This is a French startup specialized in Electric Grid.
  I created a private[^6] web application.

- I've also written an iOS application to manage passwords[^4].

- I am the author of some quite popular blog posts[^5].

For an almost exhaustive list of my projects, you could check my
github account: [github.com/yogsototh](https://github.com/yogsototh)

[^10]: To be more precise in Grammatical Inference
[^1]: <https://github.com/yogsototh/DEES>
[^2]: <http://yann.esposito.free.fr/dees.php?lang=en>
[^3]: <http://labh-curien.univ-st-etienne.fr/SEDiL/faq.php?lang=en> (I like to believe I became a better designer ☺)
[^4]: <http://ypassword.espozito.com>
[^5]: <http://yannesposito.com/Scratch/en/blog/>
[^6]: Sorry the code is private I can't show it :(.
[^7]: <http://vigiglobe.com>

\newpage

# Public things done

- [Cisco (threatgrid)](http://cisco.com) Security & Threat Management.
- [Vigiglobe](http://vigiglobe.com) architecture able to analyze thousands of social media messages in realtime. In particular, real time Machine Learning &amp; Statistics.
- [Gridpocket](http://gridpocket.com) web services (from conception to realization, works in correlation with a mobile app)
- [DEES](https://github.com/yogsototh/DEES): a 10.000 line C++ command line program. This program implement most [HMM](http://en.wikipedia.org/wiki/Hidden_Markov_model) standard algorithms _&_ inference algorithms.
- [SeDiL](http://labh-curien.univ-st-etienne.fr/SEDiL/): a Java application using Swing UI. The goal is to provide biologist an easy way to use an algorithm that generate Similarity Matrices for strings but also for Tree structures. Most graphics was done by me, including the drawing of trees. I didn't used a library for that purpose.
- YPassword iOS application
- [YPassword](http://yannesposito/YPassword) web interface in elm
- Also relative to YPassword ; a Dashboard Widget, a command line tool.
- Some websites: [yannesposito.com](http://yannesposito.com)
- Written a thesis in Machine Learning and published in major international conferences:
  [ICALP&nbsp;2003], [COLT&nbsp;2004] _&_ [COLT&nbsp;2006].
- A full javascript web application which display Electric consumption in real time.
- [mkdocs](http://yogsototh.github.io/mkdocs) (the engine I use to create this document. I exported it in HTML, PDF (using \LaTeX) and SVG.
- some Mac OS X screensaver, a MetaPost plugin to draw Automata, an RFC-like document to help my student to make a TOR like network, etc...
- a bunch of other projects see [http://github.com/yogsototh](http://github.com/yogsototh)

\newpage

# Technical Competences

------------------ -----------------------------------------------------------------
Languages          __Haskell__, __Clojure__, __Javascript__,
                   scheme, C, camL, C++, Ruby, Perl, Java, Python, PHP
Web frontend       __elm__, __Clojurescript__, __Reagent__, __Angular.js__, __sass__, etc...
Web frameworks     __compojure-api__, __Yesod__, __servant__, actionhero
ML Tools           __weka__, SVMlight
Stream Computing   __kafka__, __druid__, storm (with clojure)
UNIX               Shell scripts (zsh, bash), awk, \LaTeX, ConTeXt, metapost
VCS                __git__, Bazaar (DCVS), subversion (svn), CVS
Mac/iOS            __Objective-C Cocoa (Mac &amp; iOS)__, Dahsboard widget,
                   Quartz Composer
------------------ -----------------------------------------------------------------

\newpage

# Jobs

## Clojure Software Engineer for Cisco _2016 &rarr;_

- _Remote_

------- --------------------------
Product Security Threat Management
Role    Clojure Software Engineer
------- --------------------------

## Machine Learning Scientist _&_ Software Engineer for Vigiglobe _2013 &rarr; 2016_

- _Sophia Antipolis, France_

---------- -------------------------------------------------
Product    Scalable Real Time Social Media Analytics
           Sentiment Analysis
           Many client side web applications (Angular.js & reagent)
Role       Machine Learning Scientist
           (fast sentiment analysis, learning protocols, etc..)
           Full stack engineer (backend to frontend  architecture)
Keywords   Clojure, Haskell, node.js, reagent, Angular.js, Stream computing
---------- -------------------------------------------------

## Co-Founder _&_ freelance for GridPocket _2010 &rarr;_

_Sophia Antipolis, France_

---------- -------------------------------------------------
Product    Two API server (one for client, another for administration)
           A private client side web application
           An iPhone Application
           Some Linux boxes to send data to the servers
           A Linux driver
Role       Full technical responsibilities
Keywords   Ruby, REST, JSON, HTML, CSS, Javascript, AJAX,
           jQuery, Objective-C, ASIHTTPRequest, CorePlot, CoreData, C
---------- -------------------------------------------------

## Consultant, AirFrance _2007 &rarr;_

_Sophia Antipolis, France_

---------- -------------------------------------------------
Role       In charge of the Airfrance CMS for their website.
Keywords   TeamSite, Perl, XML, XHTML, CSS, javascript, JSP,
           Unix (Solaris/Linux), Bazaar
---------- -------------------------------------------------

## Post Ph.D _10/2006 &rarr; 3/2007_

_Université Jean Monet, Laboratoire Hubert Curien, Saint-Etienne_


---------- -------------------------------------------------
Product    [SeDiL](http://labh-curien.univ-st-etienne.fr/SEDiL/)
Role       Java Developer
Research   Similarity measure between strings or XML trees
Contact    [Marc Sebban](mailto://marc.sebban@univ-st-etienne.fr)
Keywords   UML, Java 1.5, Swing, Java 2D, Java Web Start, Applet,
           subversion, XML, XHTML, PHP
---------- -------------------------------------------------

Details:

> Java application: _11 000 lines with javadoc_
>
> Main functionalities
>
> - learn edit matrices
> - compute edit distances between trees or strings
> - visualize trees or sequences (JAVA 2D)
> - classification using K means
> - Generate random tree couple from an edit distance matrice
>
> Web: [http://labh-curien.univ-st-etienne.fr/SEDiL/](http://labh-curien.univ-st-etienne.fr/SEDiL/)


## ATER _10/2004 &rarr; 9/2006_

Research _&_ Teacher, Université de Provence, Marseille

_teach 1/2, research 1/6, C++ development 1/3_

DEES ; a C++ software

> _7500 lines of C++ code,  10.000 with comments_
>
> Main functionalities:
>
> - Mulitiplicity Automata, HMM _&_ PDA Inference,
> - Baum Welch _&_ Viterbi Algorithms,
> - GraphViz export,
> - String Generation from many Models,
>
> ------------ ------------------------------
> Languages    C++
> API          STL
> Environment  Linux (Debian) _&_ Windows XP
> ------------ ------------------------------

## Moniteur des Universités _10/2001 &rarr; 9/2004_

Université de Provence, Marseille

_teach 1/3, research 1/3, C++ Development 1/3_

Creation of DEES (see preceeding entry).

\newpage

# Diploma

------ -----------------------------------------------------
_2004_ Ph.D. degree in Machine Learning
_2001_ D.E.A. in Computer Science (equivalent to master)
_2000_ Maîtrise d’Informatique
_1999_ Licence in Computer Science
_1998_ DEUG MIAS (math)
_1995_ BAC S (math)
------ -----------------------------------------------------

\newpage

# Scientific Publications

## International

-------------- ----------------------------------------------------
Journals       [Fundamenta&nbsp;Inforamticæ&nbsp;vol.86&nbsp;2008]
               [Pattern&nbsp;Recognition,&nbsp;2004]

Conferences    [ECML&nbsp;2008] [COLT&nbsp;2006] [ICGI&nbsp;2006]
               [COLT&nbsp;2004] [ICALP&nbsp;2003] [ICGI&nbsp;2002]

Workshop       [TAGI05]
-------------- ----------------------------------------------------

## National (French)

-------------- ----------------------------------------------------
Journals       [JEDAI,&nbsp;2003]
Conferences    [CAP&nbsp;2006] [CAP&nbsp;2004] [CAP&nbsp;2003]
Thesis         [Université&nbsp;de&nbsp;Provence&nbsp;2004]
-------------- ----------------------------------------------------

\newpage

# Projects

Most of my latest programming activities are publicly available at [github.com/yogsototh](http://github.com/yogsototh)

## Haskell libraries

- [Link to list of packages](http://hackage.haskell.org/user/yogsototh)

- `holy-project`
- `human-readable-duration`
- `wai-middleware-caching-lru`
- `wai-middleware-caching-redis`
- `wai-middleware-caching`

## YPassword _2008 &rarr;_

Mainly an iOS application:

- [YPassword, `http://ypassword.espozito.com`](http://ypassword.espozito.com)

I've done fully the website from scratch. Also there are some javascript implementation of YPassword method:

> - a Mac OS X dashboard widget,
> - a Cappuccino Web application,
> - a jQuery Web application,
> - a command line tool,
> - an Applescript helper

## Anonymous Network Project _02/2006 &rarr; 06/2006_

Made a protocol similar to [TOR](http://www.torproject.org) for student.

## Other projects

- Web Application used for private team usage at AirFrance _2008 &rarr;_
  This application is just done _[for teh lulz](http://cache.ohinternet.com/images/thumb/f/fa/4tehlulz.jpg/618px-4tehlulz.jpg)_.
  Not related to the Airfrance work. But still pleasant.
  _Javascript(Prototype.js, Scriptaculous), CSS, PHP/MySQL, Google Talk_
- [metapost package](https://github.com/yogsototh/metautomata) to draw Automata _2003 &rarr; 2004_
  <em>metapost</em>
- Mac OS X Screensavers ([YClock](https://github.com/yogsototh/YClock) _&_ YAquaBubbles) _2003 &rarr; 2004_
  _Objective-C,Quartz Composer,Cocoa_

You could find even more information by looking at:

- My personnal website: [`http://yannesposito.com`](http://yannesposito.com)
- My github account: [`http://github.com/yogsototh`](http://github.com/yogsototh)
