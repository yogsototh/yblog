 ## 3D Mandelbrot?

Now we will we extend to a third dimension.
But, there is no 3D equivalent to complex.
In fact, the only extension known are quaternions (in 4D).
As I know almost nothing about quaternions, I will use some extended complex,
instead of using a 3D projection of quaternions.
I am pretty sure this construction is not useful for numbers.
But it will be enough for us to create something that look nice.

This section is quite long, but don't be afraid,
most of the code is some OpenGL boilerplate.
If you just want to skim this section,
here is a high level representation:

 > - OpenGL Boilerplate
 >  
 >   - set some IORef (understand variables) for states  
 >   - Drawing: 
 > 
 >      - set doubleBuffer, handle depth, window size...
 >      - Use state to apply some transformations
 >  
 >   - Keyboard: hitting some key change the state of IORef
 > 
 > - Generate 3D Object
 >
 >   ~~~ 
 >   allPoints :: [ColoredPoint]  
 >   allPoints =
 >       for all (x,y), -width<x<width, 0<y<height
 >       Let z be the minimal depth such that
 >           mandel x y z > 0
 >       add the points 
 >              (x, y, z,color) 
 >              (x,-y, z,color) 
 >              (x, y,-z,color) 
 >              (x,-y,-z,color) 
 >           + neighbors to make triangles
 >   ~~~



<div style="display:none">

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef
> type ColoredPoint = (GLfloat,GLfloat,GLfloat,Color3 GLfloat)

</div>

We declare a new type `ExtComplex` (for extended complex). 
An extension of complex numbers with a third component:

> data ExtComplex = C (GLfloat,GLfloat,GLfloat) 
>                   deriving (Show,Eq)
> instance Num ExtComplex where
>     -- The shape of the 3D mandelbrot 
>     -- will depend on this formula
>     C (x,y,z) * C (x',y',z') = C (x*x' - y*y' - z*z', 
>                                   x*y' + y*x' + z*z', 
>                                   x*z' + z*x' )
>     -- The rest is straightforward
>     fromInteger n = C (fromIntegral n, 0, 0)
>     C (x,y,z) + C (x',y',z') = C (x+x', y+y', z+z')
>     abs (C (x,y,z))     = C (sqrt (x*x + y*y + z*z), 0, 0)
>     signum (C (x,y,z))  = C (signum x, signum y, signum z)

The most important part is the new multiplication instance.
Modifying this formula will change radically the shape of the result.
Here is the formula written in a more mathematical notation.
I called the third component of these extended complex _strange_.

$$ \mathrm{real}      ((x,y,z) * (x',y',z')) = xx' - yy' - zz' $$

$$ \mathrm{im}        ((x,y,z) * (x',y',z')) = xy' - yx' + zz' $$

$$ \mathrm{strange}   ((x,y,z) * (x',y',z')) = xz' + zx' $$

Note how if `z=z'=0` then the multiplication is the same to the complex one.

<div style="display:none">

> extcomplex :: GLfloat -> GLfloat -> GLfloat -> ExtComplex
> extcomplex x y z = C (x,y,z)
> 
> real :: ExtComplex -> GLfloat
> real (C (x,y,z))    = x
> 
> im :: ExtComplex -> GLfloat
> im   (C (x,y,z))    = y
>
> strange :: ExtComplex -> GLfloat
> strange (C (x,y,z)) = z
> 
> magnitude :: ExtComplex -> GLfloat
> magnitude = real.abs

</div>

 ### From 2D to 3D 

As we will use some 3D, we add some new directive in the boilerplate.
But mainly, we simply state that will use some depth buffer.
And also we will listen the keyboard.

> main :: IO ()
> main = do
>   -- GLUT need to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We will use the double buffered mode (GL constraint)
>   -- We also Add the DepthBuffer (for 3D)
>   initialDisplayMode $= 
>       [WithDepthBuffer,DoubleBuffered,RGBMode]
>   -- We create a window with some title
>   createWindow "3D HOpengGL Mandelbrot"
>   -- We add some directives
>   depthFunc  $= Just Less
>   windowSize $= Size 500 500
>   -- Some state variables (I know it feels BAD)
>   angle   <- newIORef ((35,0)::(GLfloat,GLfloat))
>   zoom    <- newIORef (2::GLfloat)
>   campos  <- newIORef ((0.7,0)::(GLfloat,GLfloat))
>   -- Function to call each frame
>   idleCallback $= Just idle
>   -- Function to call when keyboard or mouse is used
>   keyboardMouseCallback $= 
>           Just (keyboardMouse angle zoom campos)
>   -- Each time we will need to update the display
>   -- we will call the function 'display'
>   -- But this time, we add some parameters
>   displayCallback $= display angle zoom campos
>   -- We enter the main loop
>   mainLoop

The `idle` is here to change the states.
There should never be any modification done in the `display` function.

> idle = postRedisplay Nothing

We introduce some helper function to manipulate
standard `IORef`.
Mainly `modVar x f` is equivalent to the imperative `x:=f(x)`,
`modFst (x,y) (+1)` is equivalent to `(x,y) := (x+1,y)`
and `modSnd (x,y) (+1)` is equivalent to `(x,y) := (x,y+1)`

> modVar v f = do
>   v' <- get v
>   v $= (f v')
> mapFst f (x,y) = (f x,  y)
> mapSnd f (x,y) = (  x,f y)

And we use them to code the function handling keyboard.
We will use the keys `hjkl` to rotate, 
`oi` to zoom and `sedf` to move.
Also, hitting space will reset the view.
Remember that `angle` and `campos` are pairs and `zoom` is a scalar.
Also note `(+0.5)` is the function `\x->x+0.5` 
and `(-0.5)` is the number `-0.5` (yes I share your pain).

> keyboardMouse angle zoom campos key state modifiers position =
>   -- We won't use modifiers nor position
>   kact angle zoom campos key state
>   where 
>     -- reset view when hitting space
>     kact a z p (Char ' ') Down = do
>           a $= (0,0) -- angle 
>           z $= 1     -- zoom
>           p $= (0,0) -- camera position
>     -- use of hjkl to rotate
>     kact a _ _ (Char 'h') Down = modVar a (mapFst (+0.5))
>     kact a _ _ (Char 'l') Down = modVar a (mapFst (+(-0.5)))
>     kact a _ _ (Char 'j') Down = modVar a (mapSnd (+0.5))
>     kact a _ _ (Char 'k') Down = modVar a (mapSnd (+(-0.5)))
>     -- use o and i to zoom
>     kact _ z _ (Char 'o') Down = modVar z (*1.1)
>     kact _ z _ (Char 'i') Down = modVar z (*0.9)
>     -- use sdfe to move the camera
>     kact _ _ p (Char 's') Down = modVar p (mapFst (+0.1))
>     kact _ _ p (Char 'f') Down = modVar p (mapFst (+(-0.1)))
>     kact _ _ p (Char 'd') Down = modVar p (mapSnd (+0.1))
>     kact _ _ p (Char 'e') Down = modVar p (mapSnd (+(-0.1)))
>     -- any other keys does nothing
>     kact _ _ _ _ _ = return ()

Note `display` takes some parameters this time.
This function if full of boilerplate:

> display angle zoom position = do
>    -- set the background color (dark solarized theme)
>   clearColor $= Color4 0 0.1686 0.2117 1
>   clear [ColorBuffer,DepthBuffer]
>   -- Transformation to change the view
>   loadIdentity -- reset any transformation
>   -- tranlate
>   (x,y) <- get position
>   translate $ Vector3 x y 0 
>   -- zoom
>   z <- get zoom
>   scale z z z
>   -- rotate
>   (xangle,yangle) <- get angle
>   rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
>   rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
>   
>   -- Now that all transformation were made
>   -- We create the object(s)
>   preservingMatrix drawMandelbrot
>   
>   swapBuffers -- refresh screen

Not much to say about this function.
Mainly there are two parts: apply some transformations, draw the object.

 ### The 3D Mandelbrot

We have finished with the OpenGL section, let's talk about how we 
generate the 3D points and colors.
First, we will set the number of details to 200 pixels in the three dimensions.

> nbDetails = 200 :: GLfloat
> width  = nbDetails
> height = nbDetails
> deep   = nbDetails

This time, instead of just drawing some line or some group of points,
we will show triangles.
The function `allPoints` will provide a multiple of three points.
Each three successive point representing the coordinate of each vertex of a triangle.


> drawMandelbrot = do
>   -- We will print Points (not triangles for example) 
>   renderPrimitive Triangles $ do
>     mapM_ drawColoredPoint allPoints
>   where
>       drawColoredPoint (x,y,z,c) = do
>           color c
>           vertex $ Vertex3 x y z

In fact, we will provide six ordered points. 
These points will be used to draw two triangles.

blogimage("triangles.png","Explain triangles")

The next function is a bit long. 
Here is an approximative English version:

~~~
forall x from -width to width
  forall y from -height to height
    forall the neighbors of (x,y)
      let z be the smalled depth such that (mandel x y z)>0
      let c be the color given by mandel x y z 
      add the point corresponding to (x,y,z,c)
~~~

Also, I added a test to hide points too far from the border.
In fact, this function show points close to the surface of the modified mandelbrot set. But not the mandelbrot set itself.

<code class="haskell">
depthPoints :: [ColoredPoint]
depthPoints = do
  x <- [-width..width]
  y <- [-height..height]
  let 
      depthOf x' y' = maxZeroIndex (mandel x' y') 0 deep logdeep 
      logdeep = floor ((log deep) / log 2)
      z1 = depthOf    x     y
      z2 = depthOf (x+1)    y
      z3 = depthOf (x+1) (y+1)
      z4 = depthOf    x  (y+1)
      c1 = mandel    x     y  (z1+1)
      c2 = mandel (x+1)    y  (z2+1)
      c3 = mandel (x+1) (y+1) (z3+1)
      c4 = mandel    x  (y+1) (z4+1)
      p1 = (   x /width,   y /height, z1/deep, colorFromValue c1)
      p2 = ((x+1)/width,   y /height, z2/deep, colorFromValue c2)
      p3 = ((x+1)/width,(y+1)/height, z3/deep, colorFromValue c3)
      p4 = (   x /width,(y+1)/height, z4/deep, colorFromValue c4)
  if (and $ map (>=57) [c1,c2,c3,c4])
  then []
  else [p1,p2,p3,p1,p3,p4]
</code>

If you look at the function above, you see a lot of common patterns.
Haskell is very efficient to make this better.
Here is a harder to read but shorter and more generic rewritten function:

> depthPoints :: [ColoredPoint]
> depthPoints = do
>   x <- [-width..width]
>   y <- [-height..height]
>   let 
>     neighbors = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]
>     depthOf (u,v) = maxZeroIndex (mandel u v) 0 deep logdeep
>     logdeep = floor ((log deep) / log 2)
>     -- zs are 3D points with found depth
>     zs = map (\(u,v) -> (u,v,depthOf (u,v))) neighbors
>     -- ts are 3D pixels + mandel value
>     ts = map (\(u,v,w) -> (u,v,w,mandel u v (w+1))) zs
>     -- ps are 3D opengl points + color value
>     ps = map (\(u,v,w,c') -> 
>         (u/width,v/height,w/deep,colorFromValue c')) ts
>   -- If the point diverged too fast, don't display it
>   if (and $ map (\(_,_,_,c) -> c>=57) ts)
>   then []
>   -- Draw two triangles
>   else [ps!!0,ps!!1,ps!!2,ps!!0,ps!!2,ps!!3]

If you prefer the first version, then just imagine how hard it will be to change the enumeration of the point from (x,y) to (x,z) for example.

Also, we didn't searched for negative values. 
This modified Mandelbrot is no more symmetric relatively to the plan `y=0`.
But it is symmetric relatively to the plan `z=0`.
Then I mirror these values. 

> allPoints :: [ColoredPoint]
> allPoints = planPoints ++ map inverseDepth  planPoints
>   where 
>       planPoints = depthPoints
>       inverseDepth (x,y,z,c) = (x,y,-z+1/deep,c)

The rest of the program is very close to the preceding one.

<div style="display:none">

> -- given f min max nbtest,
> -- considering 
> --  - f is an increasing function
> --  - f(min)=0
> --  - f(max)≠0
> -- then maxZeroIndex f min max nbtest returns x such that
> --    f(x - ε)=0 and f(x + ε)≠0
> --    where ε=(max-min)/2^(nbtest+1) 
> maxZeroIndex :: (Fractional a,Num a,Num b,Eq b) => 
>                  (a -> b) -> a -> a -> Int -> a
> maxZeroIndex func minval maxval 0 = (minval+maxval)/2
> maxZeroIndex func minval maxval n = 
>   if (func medpoint) /= 0 
>        then maxZeroIndex func minval medpoint (n-1)
>        else maxZeroIndex func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2

I made the color slightly brighter

> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.7 + 0.3*cos( fromIntegral i / 10 )
>   in
>     Color3 (t n) (t (n+5)) (t (n+10))

We only changed from `Complex` to `ExtComplex` of the main `f` function.

> f :: ExtComplex -> ExtComplex -> Int -> Int
> f c z 0 = 0
> f c z n = if (magnitude z > 2 ) 
>           then n
>           else f c ((z*z)+c) (n-1)

</div>

We simply add a new dimension to the `mandel` function
and change the type signature of `f` from `Complex` to `ExtComplex`.

> mandel x y z = 
>   let r = 2.0 * x / width
>       i = 2.0 * y / height
>       s = 2.0 * z / deep
>   in
>       f (extcomplex r i s) 0 64


Here is the result:

blogimage("mandelbrot_3D.png","A 3D mandelbrot like")
