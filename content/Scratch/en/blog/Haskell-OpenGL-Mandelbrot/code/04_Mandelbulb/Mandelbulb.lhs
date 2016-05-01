 ## Naïve code cleaning

The first approach to clean the code is to separate the GLUT/OpenGL 
part from the computation of the shape.
Here is the cleaned version of the preceding section.
Most boilerplate was put in external files.

- [`YBoiler.hs`](code/04_Mandelbulb/YBoiler.hs), the 3D rendering
- [`Mandel`](code/04_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/04_Mandelbulb/ExtComplex.hs), the extended complexes

> import YBoiler -- Most the OpenGL Boilerplate
> import Mandel -- The 3D Mandelbrot maths

The `yMainLoop` takes two arguments:
the title of the window 
and a function from time to triangles

> main :: IO ()
> main = yMainLoop "3D Mandelbrot" (\_ -> allPoints)

We set some global constant (this is generally bad).

> nbDetails = 200 :: GLfloat
> width  = nbDetails
> height = nbDetails
> deep   = nbDetails

We then generate colored points from our function.
This is similar to the preceding section.

> allPoints :: [ColoredPoint]
> allPoints = planPoints ++ map inverseDepth  planPoints
>   where 
>       planPoints = depthPoints ++ map inverseHeight depthPoints
>       inverseHeight (x,y,z,c) = (x,-y,z,c)
>       inverseDepth (x,y,z,c) = (x,y,-z+1/deep,c)

> depthPoints :: [ColoredPoint]
> depthPoints = do
>   x <- [-width..width]
>   y <- [0..height]
>   let 
>     neighbors = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]
>     depthOf (u,v) = maxZeroIndex (ymandel u v) 0 deep 7
>     -- zs are 3D points with found depth
>     zs = map (\(u,v) -> (u,v,depthOf (u,v))) neighbors
>     -- ts are 3D pixels + mandel value
>     ts = map (\(u,v,w) -> (u,v,w,ymandel u v (w+1))) zs
>     -- ps are 3D opengl points + color value
>     ps = map (\(u,v,w,c') -> 
>         (u/width,v/height,w/deep,colorFromValue c')) ts
>   -- If the point diverged too fast, don't display it
>   if (and $ map (\(_,_,_,c) -> c>=57) ts)
>   then []
>   -- Draw two triangles
>   else [ps!!0,ps!!1,ps!!2,ps!!0,ps!!2,ps!!3]
> 
> 
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
> 
> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.7 + 0.3*cos( fromIntegral i / 10 )
>   in
>     ((t n),(t (n+5)),(t (n+10)))
> 
> ymandel x y z = mandel (2*x/width) (2*y/height) (2*z/deep) 64

This code is cleaner but many things doesn't feel right.
First, all the user interaction code is outside our main file.
I feel it is okay to hide the detail for the rendering.
But I would have preferred to control the user actions.

On the other hand, we continue to handle a lot rendering details.
For example, we provide ordered vertices.
