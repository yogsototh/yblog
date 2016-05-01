 ## First version

We can consider two parts.
The first being mostly some boilerplate[^011].
And the second part more focused on OpenGL and content.

[^011]: Generally in Haskell you need to declare a lot of import lines.
      This is something I find annoying.
      In particular, it should be possible to create a special file, Import.hs
      which make all the necessary import for you, as you generally need them all.
      I understand why this is cleaner to force the programmer not to do so,
      but, each time I do a copy/paste, I feel something is wrong.
      I believe this concern can be generalized to the lack of namespace in Haskell.

 ### Let's play the song of our people

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef

For efficiency reason[^010001], I will not use the default Haskell `Complex` data type.

[^010001]: I tried `Complex Double`, `Complex Float`, this current data type with `Double` and the actual version `Float`. For rendering a 1024x1024 Mandelbrot set it takes `Complex Double` about 6.8s, for `Complex Float` about 5.1s, for the actual version with `Double` and `Float` it takes about `1.6` sec. See these sources for testing yourself: [https://gist.github.com/2945043](https://gist.github.com/2945043). If you really want to things to go faster, use `data Complex = C {-# UNPACK #-} !Float {-# UNPACK #-} !Float`. It takes only one second instead of 1.6s.

> data Complex = C (Float,Float) deriving (Show,Eq)


> instance Num Complex where
>     fromInteger n = C (fromIntegral n,0.0)
>     C (x,y) * C (z,t) = C (z*x - y*t, y*z + x*t)
>     C (x,y) + C (z,t) = C (x+z, y+t)
>     abs (C (x,y))     = C (sqrt (x*x + y*y),0.0)
>     signum (C (x,y))  = C (signum x , 0.0)

We declare some useful functions for manipulating complex numbers:

> complex :: Float -> Float -> Complex
> complex x y = C (x,y)
> 
> real :: Complex -> Float
> real (C (x,y))    = x
> 
> im :: Complex -> Float
> im   (C (x,y))    = y
> 
> magnitude :: Complex -> Float
> magnitude = real.abs


 ### Let us start

We start by giving the main architecture of our program:

> main :: IO ()
> main = do
>   -- GLUT need to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We will use the double buffered mode (GL constraint)
>   initialDisplayMode $= [DoubleBuffered]
>   -- We create a window with some title
>   createWindow "Mandelbrot Set with Haskell and OpenGL"
>   -- Each time we will need to update the display
>   -- we will call the function 'display'
>   displayCallback $= display
>   -- We enter the main loop
>   mainLoop

Mainly, we initialize our OpenGL application.
We declared that the function `display` will be used to render the graphics:

> display = do
>   clear [ColorBuffer] -- make the window black
>   loadIdentity -- reset any transformation
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen

Also here, there is only one interesting line;
the draw will occur in the function `drawMandelbrot`.

This function will provide a list of draw actions.
Remember that OpenGL is imperative by design.
Then, one of the consequence is you must write the actions in the right order.
No easy parallel drawing here.
Here is the function which will render something on the screen:

> drawMandelbrot =
>   -- We will print Points (not triangles for example) 
>   renderPrimitive Points $ do
>     mapM_ drawColoredPoint allPoints
>   where
>       drawColoredPoint (x,y,c) = do
>           color c -- set the current color to c
>           -- then draw the point at position (x,y,0)
>           -- remember we're in 3D
>           vertex $ Vertex3 x y 0 

The `mapM_` function is mainly the same as map but inside a monadic context.
More precisely, this can be transformed as a list of actions where the order is important:

~~~
drawMandelbrot = 
  renderPrimitive Points $ do
    color color1
    vertex $ Vertex3 x1 y1 0
    ...
    color colorN
    vertex $ Vertex3 xN yN 0
~~~

We also need some kind of global variables. 
In fact, global variable are a proof of a design problem. 
We will get rid of them later.

> width = 320 :: GLfloat
> height = 320 :: GLfloat

And of course our list of colored points.
In OpenGL the default coordinate are from -1 to 1.

> allPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
> allPoints = [ (x/width,y/height,colorFromValue $ mandel x y) | 
>                   x <- [-width..width], 
>                   y <- [-height..height]]
>

We need a function which transform an integer value to some color:

> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
>   in
>     Color3 (t n) (t (n+5)) (t (n+10))

And now the `mandel` function. 
Given two coordinates in pixels, it returns some integer value:

> mandel x y = 
>   let r = 2.0 * x / width
>       i = 2.0 * y / height
>   in
>       f (complex r i) 0 64

It uses the main Mandelbrot function for each complex \\(c\\).
The Mandelbrot set is the set of complex number \\(c\\) such that the following sequence does not escape to infinity.

Let us define \\(f_c: \mathbb{C} \to \mathbb{C}\\)

$$ f_c(z) = z^2 + c $$

The sequence is: 

$$ 0 \rightarrow f_c(0) \rightarrow f_c(f_c(0)) \rightarrow \cdots \rightarrow f^n_c(0) \rightarrow \cdots $$

Of course, instead of trying to test the real limit, we just make a test after a finite number of occurrences.

> f :: Complex -> Complex -> Int -> Int
> f c z 0 = 0
> f c z n = if (magnitude z > 2 ) 
>           then n
>           else f c ((z*z)+c) (n-1)

Well, if you download this file (look at the bottom of this section), compile it and run it this is the result:

blogimage("hglmandel_v01.png","The mandelbrot set version 1")

A first very interesting property of this program is that the computation for all the points is done only once.
It is a bit long before the first image appears, but if you resize the window, it updates instantaneously.
This property is a direct consequence of purity.
If you look closely, you see that `allPoints` is a pure list.
Therefore, calling `allPoints` will always render the same result and Haskell is clever enough to use this property.
While Haskell doesn't garbage collect `allPoints` the result is reused for free.
We did not specified this value should be saved for later use. 
It is saved for us.

See what occurs if we make the window bigger:

blogimage("hglmandel_v01_too_wide.png","The mandelbrot too wide, black lines and columns")

We see some black lines because we have drawn less point than there is on the surface.
We can repair this by drawing little squares instead of just points.
But, instead we will do something a bit different and unusual.
