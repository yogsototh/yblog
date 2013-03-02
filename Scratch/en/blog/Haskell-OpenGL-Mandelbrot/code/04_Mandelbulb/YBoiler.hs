-- An OpenGL boilerplate
module YBoiler (GLfloat,yMainLoop,ColoredPoint,Color3) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef


type ColorRGB = (GLfloat,GLfloat,GLfloat)
type YAngle = (GLfloat,GLfloat,GLfloat)
type ColoredPoint = (GLfloat,GLfloat,GLfloat,ColorRGB)

yMainLoop :: String -> (Int -> [ColoredPoint]) -> IO ()
yMainLoop windowTitle triangles = do
  -- GLUT need to be initialized
  (progname,_) <- getArgsAndInitialize
  -- We will use the double buffered mode (GL constraint)
  -- We also Add the DepthBuffer (for 3D)
  initialDisplayMode $= 
      [WithDepthBuffer,DoubleBuffered,RGBMode]
  -- We create a window with some title
  createWindow windowTitle
  -- We add some directives
  depthFunc  $= Just Less
  -- matrixMode $= Projection
  windowSize $= Size 500 500
  -- Some state variables (I know it feels BAD)
  angle   <- newIORef ((35,0,0)::YAngle)
  zoom    <- newIORef (2::GLfloat)
  campos  <- newIORef ((0.7,0)::(GLfloat,GLfloat))
  -- Action to call when waiting
  idleCallback $= Just idle
  -- We will use the keyboard
  keyboardMouseCallback $= 
          Just (keyboardMouse angle zoom campos)
  -- Each time we will need to update the display
  -- we will call the function 'display'
  -- But this time, we add some parameters
  displayCallback $= display angle zoom campos triangles
  -- We enter the main loop
  mainLoop

idle = postRedisplay Nothing

-- modify IORef cleanly
modVar :: IORef a -> (a -> a) -> IO ()
modVar v f = do
  v' <- get v
  v $= (f v')
-- modify IORef (a,b) using f:a->a
mapFst f (x,y) = (f x,  y)
-- modify IORef (a,b) using f:b->b
mapSnd f (x,y) = (  x,f y)
mapFst3 f (x,y,z) = (f x, y, z)
mapSnd3 f (x,y,z) = (x, f y, z)
mapThi3 f (x,y,z) = (x, y, f z)

-- Get User Input
keyboardMouse angle zoom pos key state modifiers position =
  kact angle zoom pos key state
  where 
    -- reset view when hitting space
    kact a z p (Char ' ') Down = do
          a $= (0,0,0)
          z $= 1
          p $= (0,0)
    -- use of hjkl to rotate
    kact a _ _ (Char 'j') Down = modVar a (mapFst3 (+0.5))
    kact a _ _ (Char 'l') Down = modVar a (mapFst3 (+(-0.5)))
    kact a _ _ (Char 'i') Down = modVar a (mapSnd3 (+0.5))
    kact a _ _ (Char 'k') Down = modVar a (mapSnd3 (+(-0.5)))
    kact a _ _ (Char 'o') Down = modVar a (mapThi3 (+0.5))
    kact a _ _ (Char 'u') Down = modVar a (mapThi3 (+(-0.5)))
    -- use o and i to zoom
    kact _ s _ (Char '+') Down = modVar s (*1.1)
    kact _ s _ (Char '-') Down = modVar s (*0.9)
    -- use sdfe to move the camera
    kact _ _ p (Char 's') Down = modVar p (mapFst (+0.1))
    kact _ _ p (Char 'f') Down = modVar p (mapFst (+(-0.1)))
    kact _ _ p (Char 'd') Down = modVar p (mapSnd (+0.1))
    kact _ _ p (Char 'e') Down = modVar p (mapSnd (+(-0.1)))
    -- any other keys does nothing
    kact _ _ _ _ _ = return ()

-- The function that will display datas
display angle zoom position triangles = do
   -- set the background color (dark solarized theme)
  clearColor $= Color4 0 0.1686 0.2117 1
  clear [ColorBuffer,DepthBuffer]
  -- Transformation to change the view
  loadIdentity -- reset any transformation
  -- tranlate
  (x,y) <- get position
  translate $ Vector3 x y 0 
  -- zoom
  z <- get zoom
  scale z z z
  -- rotate
  (xangle,yangle,zangle) <- get angle
  rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
  rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
  rotate zangle $ Vector3 0.0 0.0 (1.0::GLfloat)
  -- Now that all transformation were made
  -- We create the object(s)
  t <- get elapsedTime
  preservingMatrix $ drawObject (triangles t)
  swapBuffers -- refresh screen

red   (r,_,_) = r
green (_,g,_) = g
blue  (_,_,b) = b

drawObject triangles = do
  -- We will print Points (not triangles for example) 
  renderPrimitive Triangles $ do
    mapM_ drawColoredPoint triangles
  where
      drawColoredPoint (x,y,z,c) = do
          color $ Color3 (red c) (green c) (blue c)
          vertex $ Vertex3 x y z
