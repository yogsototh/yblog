module ExtComplex where

import Graphics.Rendering.OpenGL

-- This time I use unpacked strict data type
-- Far faster when compiled.
data ExtComplex = C {-# UNPACK #-} !GLfloat
                    {-# UNPACK #-} !GLfloat
                    {-# UNPACK #-} !GLfloat
                  deriving (Show,Eq)

instance Num ExtComplex where
    -- The shape of the 3D mandelbrot 
    -- will depend on this formula
    (C x y z) * (C x' y' z') = C (x*x' - y*y' - z*z') 
                                 (x*y' + y*x' + z*z') 
                                 (x*z' + z*x' )
    -- The rest is straightforward
    fromInteger n = C (fromIntegral n) 0 0
    (C x y z) + (C x' y' z') = C (x+x') (y+y') (z+z')
    abs (C x y z)     = C (sqrt (x*x + y*y + z*z)) 0 0
    signum (C x y z)  = C (signum x) (signum y) (signum z)

extcomplex :: GLfloat -> GLfloat -> GLfloat -> ExtComplex
extcomplex x y z = C x y z

real :: ExtComplex -> GLfloat
real (C x _ _)    = x

im :: ExtComplex -> GLfloat
im   (C _ y _)    = y

strange :: ExtComplex -> GLfloat
strange (C _ _ z) = z

magnitude :: ExtComplex -> GLfloat
magnitude = real.abs
