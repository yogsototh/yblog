 ## Only the edges

<div style="display:none">

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef
> -- Use UNPACK data because it is faster
> -- The ! is for strict instead of lazy
> data Complex = C  {-# UNPACK #-} !Float 
>                   {-# UNPACK #-} !Float 
>                deriving (Show,Eq)
> instance Num Complex where
>     fromInteger n = C (fromIntegral n) 0.0
>     (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
>     (C x y) + (C z t) = C (x+z) (y+t)
>     abs (C x y)     = C (sqrt (x*x + y*y)) 0.0
>     signum (C x y)  = C (signum x) 0.0
> complex :: Float -> Float -> Complex
> complex x y = C x y
> 
> real :: Complex -> Float
> real (C x y)    = x
> 
> im :: Complex -> Float
> im   (C x y)    = y
> 
> magnitude :: Complex -> Float
> magnitude = real.abs
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
> display = do
>    -- set the background color (dark solarized theme)
>   clearColor $= Color4 0 0.1686 0.2117 1
>   clear [ColorBuffer] -- make the window black
>   loadIdentity -- reset any transformation
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen
> 
> width = 320 :: GLfloat
> height = 320 :: GLfloat


</div>

This time, instead of drawing all points, 
we will simply draw the edges of the Mandelbrot set.
The method I use is a rough approximation. 
I consider the Mandelbrot set to be almost convex.
The result will be good enough for the purpose of this tutorial.

We change slightly the `drawMandelbrot` function.
We replace the `Points` by `LineLoop`

> drawMandelbrot =
>   -- We will print Points (not triangles for example) 
>   renderPrimitive LineLoop $ do
>     mapM_ drawColoredPoint allPoints
>   where
>       drawColoredPoint (x,y,c) = do
>           color c -- set the current color to c
>           -- then draw the point at position (x,y,0)
>           -- remember we're in 3D
>           vertex $ Vertex3 x y 0 

And now, we should change our list of points.
Instead of drawing every point of the visible surface, 
we will choose only point on the surface.

> allPoints = positivePoints ++ 
>       map (\(x,y,c) -> (x,-y,c)) (reverse positivePoints)

We only need to compute the positive point.
The Mandelbrot set is symmetric relatively to the abscisse axis.

> positivePoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
> positivePoints = do
>      x <- [-width..width]
>      let y = maxZeroIndex (mandel x) 0 height (log2 height)
>      if y < 1 -- We don't draw point in the absciss
>         then []
>         else return (x/width,y/height,colorFromValue $ mandel x y)
>      where
>          log2 n = floor ((log n) / log 2)

This function is interesting. 
For those not used to the list monad here is a natural language version of this function:

<code class="no-highlight">
positivePoints =
    for all x in the range [-width..width]
    let y be smallest number s.t. mandel x y > 0
    if y is on 0 then don't return a point
    else return the value corresonding to (x,y,color for (x+iy))
</code>

In fact using the list monad you write like if you consider only one element at a time and the computation is done non deterministically.
To find the smallest number such that `mandel x y > 0` we use a simple dichotomy:

> -- given f min max nbtest,
> -- considering 
> --  - f is an increasing function
> --  - f(min)=0
> --  - f(max)≠0
> -- then maxZeroIndex f min max nbtest returns x such that
> --    f(x - ε)=0 and f(x + ε)≠0
> --    where ε=(max-min)/2^(nbtest+1) 
> maxZeroIndex func minval maxval 0 = (minval+maxval)/2
> maxZeroIndex func minval maxval n = 
>   if (func medpoint) /= 0 
>        then maxZeroIndex func minval medpoint (n-1)
>        else maxZeroIndex func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2

No rocket science here. See the result now:

blogimage("HGLMandelEdges.png","The edges of the mandelbrot set")

<div style="display:none">

> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
>   in
>     Color3 (t n) (t (n+5)) (t (n+10))

> mandel x y = 
>   let r = 2.0 * x / width
>       i = 2.0 * y / height
>   in
>       f (complex r i) 0 64

> f :: Complex -> Complex -> Int -> Int
> f c z 0 = 0
> f c z n = if (magnitude z > 2 ) 
>           then n
>           else f c ((z*z)+c) (n-1)

</div>

