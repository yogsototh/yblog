 ## Functional organization?

Some points:

1. OpenGL and GLUT is done in C.
   In particular the `mainLoop` function is a direct link to the C library (FFI).
   This function is clearly far from the functional paradigm.
   Could we make this better?
   We will have two choices: 

   - create our own `mainLoop` function to make it more functional.
   - deal with the imperative nature of the GLUT `mainLoop` function.

   As one of the goal of this article is to understand how to deal with existing libraries and particularly the one coming from imperative languages, we will continue to use the `mainLoop` function.
2. Our main problem come from user interaction.
   If you ask "the Internet", 
   about how to deal with user interaction with a functional paradigm, 
   the main answer is to use _functional reactive programming_ (FRP).
   I won't use FRP in this article.
   Instead, I'll use a simpler while less effective way to deal with user interaction.
   But The method I'll use will be as pure and functional as possible.

Here is how I imagine things should go.
First, what the main loop should look like if we could make our own:

<code class="no-highlight">
functionalMainLoop =
    Read user inputs and provide a list of actions
    Apply all actions to the World
    Display one frame 
    repetere aeternum
</code>

Clearly, ideally we should provide only three parameters to this main loop function:

- an initial World state
- a mapping between the user interactions and functions which modify the world
- a function taking two parameters: time and world state and render a new world without user interaction.

Here is a real working code, I've hidden most display functions.
The YGL, is a kind of framework to display 3D functions.
But it can easily be extended to many kind of representation.

> import YGL -- Most the OpenGL Boilerplate
> import Mandel -- The 3D Mandelbrot maths

We first set the mapping between user input and actions.
The type of each couple should be of the form
`(user input, f)` where (in a first time) `f:World -> World`.
It means, the user input will transform the world state.

> -- Centralize all user input interaction
> inputActionMap :: InputMap World
> inputActionMap = inputMapFromList [
>      (Press 'k' , rotate xdir   5)
>     ,(Press 'i' , rotate xdir (-5))
>     ,(Press 'j' , rotate ydir   5)
>     ,(Press 'l' , rotate ydir (-5))
>     ,(Press 'o' , rotate zdir   5)
>     ,(Press 'u' , rotate zdir (-5))
>     ,(Press 'f' , translate xdir   0.1)
>     ,(Press 's' , translate xdir (-0.1))
>     ,(Press 'e' , translate ydir   0.1)
>     ,(Press 'd' , translate ydir (-0.1))
>     ,(Press 'z' , translate zdir   0.1)
>     ,(Press 'r' , translate zdir (-0.1))
>     ,(Press '+' , zoom    1.1)
>     ,(Press '-' , zoom (1/1.1))
>     ,(Press 'h' , resize    1.2)
>     ,(Press 'g' , resize (1/1.2))
>     ]

And of course a type design the World State. 
The important part is that it is our World State type.
We could have used any kind of data type.

> -- I prefer to set my own name for these types
> data World = World {
>       angle       :: Point3D
>     , scale       :: Scalar
>     , position    :: Point3D
>     , shape       :: Scalar -> Function3D
>     , box         :: Box3D
>     , told        :: Time -- last frame time
>     } 

The important part to glue our own type to the framework
is to make our type an instance of the type class `DisplayableWorld`.
We simply have to provide the definition of some functions.

> instance DisplayableWorld World where
>   winTitle _ = "The YGL Mandelbulb"
>   camera w = Camera {
>         camPos = position w, 
>         camDir = angle w,
>         camZoom = scale w }
>   -- objects for world w
>   -- is the list of one unique element
>   -- The element is an YObject
>   --   more precisely the XYFunc Function3D Box3D
>   --   where the Function3D is the type
>   --             Point -> Point -> Maybe (Point,Color)
>   --   and its value here is ((shape w) res)
>   --   and the Box3D value is defbox
>   objects w = [XYFunc ((shape  w) res) defbox]
>               where
>                   res = resolution $ box w
>                   defbox = box w

The `camera` function will retrieve an object of type `Camera` which contains
most necessary information to set our camera.
The `objects` function will returns a list of objects. 
Their type is `YObject`. Note the generation of triangles is no more in this file.
Until here we only used declarative pattern.

We also need to set all our transformation functions.
These function are used to update the world state.

> xdir :: Point3D
> xdir = makePoint3D (1,0,0)
> ydir :: Point3D
> ydir = makePoint3D (0,1,0)
> zdir :: Point3D
> zdir = makePoint3D (0,0,1)

Note `(-*<)` is the scalar product (`α -*< (x,y,z) = (αx,αy,αz)`).
Also note we could add two Point3D. 

> rotate :: Point3D -> Scalar -> World -> World
> rotate dir angleValue world = 
>   world {
>      angle = (angle world) + (angleValue -*< dir) }
> 
> translate :: Point3D -> Scalar -> World -> World
> translate dir len world = 
>   world {
>     position = (position world) + (len -*< dir) }
> 
> zoom :: Scalar -> World -> World
> zoom z world = world {
>     scale = z * scale world }
> 
> resize :: Scalar -> World -> World
> resize r world = world {
>     box = (box world) {
>      resolution = sqrt ((resolution (box world))**2 * r) }}

The resize is used to generate the 3D function.
As I wanted the time spent to generate a more detailed view 
to grow linearly I use this not so straightforward formula.

The `yMainLoop` takes three arguments.

- A map between user Input and world transformation
- A timed world transformation
- An initial world state

> main :: IO ()
> main = yMainLoop inputActionMap idleAction initialWorld

Here is our initial world state.

> -- We initialize the world state
> -- then angle, position and zoom of the camera
> -- And the shape function
> initialWorld :: World
> initialWorld = World {
>    angle = makePoint3D (-30,-30,0)
>  , position = makePoint3D (0,0,0)
>  , scale = 0.8
>  , shape = shapeFunc 
>  , box = Box3D { minPoint = makePoint3D (-2,-2,-2)
>                , maxPoint =  makePoint3D (2,2,2)
>                , resolution =  0.16 }
>  , told = 0
>  }

We will define `shapeFunc` later.
Here is the function which transform the world even without user action.
Mainly it makes some rotation.

> idleAction :: Time -> World -> World
> idleAction tnew world = world {
>     angle = (angle world) + (delta -*< zdir)
>   , told = tnew
>   }
>   where 
>       anglePerSec = 5.0
>       delta = anglePerSec * elapsed / 1000.0
>       elapsed = fromIntegral (tnew - (told world))

Now the function which will generate points in 3D.
The first parameter (`res`) is the resolution of the vertex generation.
More precisely, `res` is distance between two points on one direction.
We need it to "close" our shape.

The type `Function3D` is `Point -> Point -> Maybe Point`.
Because we consider partial functions
(for some `(x,y)` our function can be undefined).

> shapeFunc :: Scalar -> Function3D
> shapeFunc res x y = 
>   let 
>       z = maxZeroIndex (ymandel x y) 0 1 20
>   in
>   if and [ maxZeroIndex (ymandel (x+xeps) (y+yeps)) 0 1 20 < 0.000001 |
>               val <- [res], xeps <- [-val,val], yeps<-[-val,val]]
>       then Nothing 
>       else Just (z,colorFromValue ((ymandel x y z) * 64))

With the color function.

> colorFromValue :: Point -> Color
> colorFromValue n =
>   let 
>       t :: Point -> Scalar
>       t i = 0.7 + 0.3*cos( i / 10 )
>   in
>     makeColor (t n) (t (n+5)) (t (n+10))

The rest is similar to the preceding sections.

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
>   if (func medpoint) /= 0 
>        then maxZeroIndex func minval medpoint (n-1)
>        else maxZeroIndex func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2
> 
> ymandel :: Point -> Point -> Point -> Point
> ymandel x y z = fromIntegral (mandel x y z 64) / 64

I won't explain how the magic occurs here.
If you are interested, just read the file [`YGL.hs`](code/05_Mandelbulb/YGL.hs).
It is commented a lot.

- [`YGL.hs`](code/05_Mandelbulb/YGL.hs), the 3D rendering framework
- [`Mandel`](code/05_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/05_Mandelbulb/ExtComplex.hs), the extended complexes

