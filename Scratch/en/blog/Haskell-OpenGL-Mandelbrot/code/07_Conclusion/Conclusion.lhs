 ## Conclusion

As we can use imperative style in a functional language,
know you can use functional style in imperative languages.
This article exposed a way to organize some code in a functional way.
I'd like to stress the usage of Haskell made it very simple to achieve this.

Once you are used to pure functional style,
it is hard not to see all advantages it offers.

The code in the two last sections is completely pure and functional.
Furthermore I don't use `GLfloat`, `Color3` or any other OpenGL type.
If I want to use another library in the future, 
I would be able to keep all the pure code and simply update the YGL module.

The `YGL` module can be seen as a "wrapper" around 3D display and user interaction. 
It is a clean separator between the imperative paradigm and functional paradigm.

If you want to go further, it shouldn't be hard to add parallelism.
This should be easy mainly because most of the visible code is pure.
Such an optimization would have been harder by using directly the OpenGL library.

You should also want to make a more precise object. Because, the Mandelbulb is
clearly not convex. But a precise rendering might be very long from 
O(n².log(n)) to O(n³).
