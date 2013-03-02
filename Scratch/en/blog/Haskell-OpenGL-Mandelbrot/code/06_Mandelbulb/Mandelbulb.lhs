 ## Optimization

Our code architecture feel very clean.
All the meaningful code is in our main file and all display details are
externalized.
If you read the code of `YGL.hs`, you'll see I didn't made everything perfect. 
For example, I didn't finished the code of the lights.
But I believe it is a good first step and it will be easy to go further.
Unfortunately the program of the preceding session is extremely slow.
We compute the Mandelbulb for each frame now.

Before our program structure was:

<code class="no-highlight">
Constant Function -> Constant List of Triangles -> Display
</code>

Now we have 

<code class="no-highlight">
Main loop -> World -> Function -> List of Objects -> Atoms -> Display
</code>

The World state could change. 
The compiler can no more optimize the computation for us. 
We have to manually explain when to redraw the shape.

To optimize we must do some things in a lower level.
Mostly the program remains the same, 
but it will provide the list of atoms directly.

<div style="display:none">

> import YGL -- Most the OpenGL Boilerplate
> import Mandel -- The 3D Mandelbrot maths
> 
> -- Centralize all user input interaction
> inputActionMap :: InputMap World
> inputActionMap = inputMapFromList [
>      (Press ' ' , switchRotation)
>     ,(Press 'k' , rotate xdir 5)
>     ,(Press 'i' , rotate xdir (-5))
>     ,(Press 'j' , rotate ydir 5)
>     ,(Press 'l' , rotate ydir (-5))
>     ,(Press 'o' , rotate zdir 5)
>     ,(Press 'u' , rotate zdir (-5))
>     ,(Press 'f' , translate xdir 0.1)
>     ,(Press 's' , translate xdir (-0.1))
>     ,(Press 'e' , translate ydir 0.1)
>     ,(Press 'd' , translate ydir (-0.1))
>     ,(Press 'z' , translate zdir 0.1)
>     ,(Press 'r' , translate zdir (-0.1))
>     ,(Press '+' , zoom 1.1)
>     ,(Press '-' , zoom (1/1.1))
>     ,(Press 'h' , resize 2.0)
>     ,(Press 'g' , resize (1/2.0))
>     ]

</div>

> data World = World {
>       angle       :: Point3D
>     , anglePerSec :: Scalar
>     , scale       :: Scalar
>     , position    :: Point3D
>     , box         :: Box3D
>     , told        :: Time 
>     -- We replace shape by cache
>     , cache       :: [YObject]
>     } 


> instance DisplayableWorld World where
>   winTitle _ = "The YGL Mandelbulb"
>   camera w = Camera {
>         camPos = position w, 
>         camDir = angle w,
>         camZoom = scale w }
>   -- We update our objects instanciation
>   objects = cache

<div style="display:none">

> xdir :: Point3D
> xdir = makePoint3D (1,0,0)
> ydir :: Point3D
> ydir = makePoint3D (0,1,0)
> zdir :: Point3D
> zdir = makePoint3D (0,0,1)
> 
> rotate :: Point3D -> Scalar -> World -> World
> rotate dir angleValue world = 
>   world {
>      angle = angle world + (angleValue -*< dir) }
>
> switchRotation :: World -> World
> switchRotation world = 
>   world {
>      anglePerSec = if anglePerSec world > 0 then 0 else 5.0 }
> 
> translate :: Point3D -> Scalar -> World -> World
> translate dir len world = 
>   world {
>     position = position world + (len -*< dir) }
> 
> zoom :: Scalar -> World -> World
> zoom z world = world {
>     scale = z * scale world }

> main :: IO ()
> main = yMainLoop inputActionMap idleAction initialWorld

</div>

Our initial world state is slightly changed:

> -- We initialize the world state
> -- then angle, position and zoom of the camera
> -- And the shape function
> initialWorld :: World
> initialWorld = World {
>    angle = makePoint3D (30,30,0)
>  , anglePerSec = 5.0
>  , position = makePoint3D (0,0,0)
>  , scale = 1.0
>  , box = Box3D { minPoint = makePoint3D (0-eps, 0-eps, 0-eps)
>                , maxPoint = makePoint3D (0+eps, 0+eps, 0+eps)
>                , resolution =  0.02 }
>  , told = 0
>  -- We declare cache directly this time
>  , cache = objectFunctionFromWorld initialWorld
>  }
>  where eps=2

The use of `eps` is a hint to make a better zoom by computing with the right bounds.

We use the `YGL.getObject3DFromShapeFunction` function directly.
This way instead of providing `XYFunc`, we provide directly a list of Atoms.

> objectFunctionFromWorld :: World -> [YObject]
> objectFunctionFromWorld w = [Atoms atomList]
>   where atomListPositive = 
>           getObject3DFromShapeFunction
>               (shapeFunc (resolution (box w))) (box w)
>         atomList = atomListPositive ++ 
>           map negativeTriangle atomListPositive
>         negativeTriangle (ColoredTriangle (p1,p2,p3,c)) = 
>               ColoredTriangle (negz p1,negz p3,negz p2,c)
>               where negz (P (x,y,z)) = P (x,y,-z)

We know that resize is the only world change that necessitate to 
recompute the list of atoms (triangles). 
Then we update our world state accordingly.

> resize :: Scalar -> World -> World
> resize r world = 
>   tmpWorld { cache = objectFunctionFromWorld tmpWorld }
>   where 
>       tmpWorld = world { box = (box world) {
>               resolution = sqrt ((resolution (box world))**2 * r) }}

All the rest is exactly the same.

<div style="display:none">

> idleAction :: Time -> World -> World
> idleAction tnew world = 
>       world {
>         angle = angle world + (delta -*< zdir)
>       , told = tnew
>       }
>   where 
>       delta = anglePerSec world * elapsed / 1000.0
>       elapsed = fromIntegral (tnew - (told world))
> 
> shapeFunc :: Scalar -> Function3D
> shapeFunc res x y = 
>   let 
>       z = maxZeroIndex (ymandel x y) 0 1 20
>   in
>   if and [ maxZeroIndex (ymandel (x+xeps) (y+yeps)) 0 1 20 < 0.000001 |
>               val <- [res], xeps <- [-val,val], yeps<-[-val,val]]
>       then Nothing 
>       else Just (z,colorFromValue 0)
> 
> colorFromValue :: Point -> Color
> colorFromValue n =
>   let 
>       t :: Point -> Scalar
>       t i = 0.0 + 0.5*cos( i /10 )
>   in
>     makeColor (t n) (t (n+5)) (t (n+10))
> 
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
> maxZeroIndex _ minval maxval 0 = (minval+maxval)/2
> maxZeroIndex func minval maxval n = 
>   if func medpoint /= 0 
>        then maxZeroIndex func minval medpoint (n-1)
>        else maxZeroIndex func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2
> 
> ymandel :: Point -> Point -> Point -> Point
> ymandel x y z = fromIntegral (mandel x y z 64) / 64

</div>

And you can also consider minor changes in the `YGL.hs` source file.

- [`YGL.hs`](code/06_Mandelbulb/YGL.hs), the 3D rendering framework
- [`Mandel`](code/06_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/06_Mandelbulb/ExtComplex.hs), the extended complexes
