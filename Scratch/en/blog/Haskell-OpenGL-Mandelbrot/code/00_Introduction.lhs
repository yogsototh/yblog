 ## Introduction

In my
[preceding article](/Scratch/en/blog/Haskell-the-Hard-Way/) I introduced Haskell. 

This article goes further.
It will show how to use functional programming with interactive programs.
But more than that, it will show how to organize your code in a functional way.
This article is more about functional paradigm than functional language.
The code organization can be used in most imperative language.

As Haskell is designed for functional paradigm, it is easier to use in this context.
In reality, the firsts sections will use an imperative paradigm.
As you can use functional paradigm in imperative language, 
you can also use imperative paradigm in functional languages.

This article is about creating an useful and clean program.
It can interact with the user in real time.
It uses OpenGL, a library with imperative programming foundations.
Despite this fact, 
most of the final code will remain in the pure part (no `IO`).

I believe the main audience for this article are:

- Haskell programmer looking for an OpengGL tutorial.
- People interested in program organization (programming language agnostic).
- Fractal lovers and in particular 3D fractal.
- People interested in user interaction in a functional paradigm.

I had in mind for some time now to make a Mandelbrot set explorer.
I had already written a [command line Mandelbrot set generator in Haskell](http://github.com/yogsototh/mandelbrot.git).
This utility is highly parallel; it uses the `repa` package[^001].

[^001]: Unfortunately, I couldn't make this program to work on my Mac. More precisely, I couldn't make the [DevIL](http://openil.sourceforge.net/) library work on Mac to output the image. Yes I have done a `brew install libdevil`. But even a minimal program who simply write some `jpg` didn't worked. I tried both with `Haskell` and `C`.

This time, we will not parallelize the computation.
Instead, we will display the Mandelbrot set extended in 3D using OpenGL and Haskell.
You will be able to move it using your keyboard.
This object is a Mandelbrot set in the plan (z=0),
and something nice to see in 3D.

Here are some screenshots of the result:

blogfigure("GoldenMandelbulb.png","The entire Mandelbulb")
blogfigure("3DMandelbulbDetail.png","A Mandelbulb detail")
blogfigure("3DMandelbulbDetail2.png","Another detail of the Mandelbulb")

And you can see the intermediate steps to reach this goal:

blogimage("HGL_Plan.png","The parts of the article")

From the 2nd section to the 4th it will be _dirtier_ and _dirtier_.
We start cleaning the code at the 5th section.
