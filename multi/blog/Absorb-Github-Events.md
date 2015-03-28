--- kind: article published: 2015-03-27 image:
/Scratch/img/blog/Absorb-Github-Events/main.png en: title: Absorb
Github Events fr: title: Absorb Github Events author: Yann Esposito
authoruri: yannesposito.com tags: programming theme: scientific ---
blogimage("main.png","Main image")

<div class="intro">

en: %tldr How to absorb octopussies events in real time.

fr: %tlal

This article should be the first of a series of articles.
The goal of this series of articles is to display event in real time.
For this you'll need many different parts:

- You should first absorbs the events
- then you might need to enrich them,
- afterward, you'll need to aggregate,
- you also need to give a way to question the aggregates via an API,
- and finally you'll need to provide a nice dashboard using the API

So let's start by a way to get all github events in realtime.

For this series we'll use the events provided by [github][github].
The Eel ("Muraine" in French) is one of the few octopussy predator.
Which is the github mascot.

</div>

[github]: http://github.com

## The concepts

Actually all events are accessible via the <https://api.github.com/events> %url.
During standard time and if you are logged you can call this URL about 5000 times per hour.
Which is more than one times by second.

During some events, github might lower this max number of calls.
So this information is provided in the header of the HTTP response.

Mainly the algorithm will be:

Forever:
1. call `/events`
2. analyze the three Headers:
   How many call can we use?
   At which time the number of calls reset?
   Compute how much time to wait before our next call.
3. Analyze the data and put them somewhere safe where they'll could be processed next.

Apparently it seems easy.
Let's try it.

But another very interesting detail.
While github provide quite 

## Initialize your environment.

