-- The Mandelbrot function
module Mandel (mandel) where

import ExtComplex

mandel :: Float -> Float -> Float -> Int -> Int
mandel r i s nbIterations = 
    f (extcomplex r i s) 0 nbIterations
    where
        f :: ExtComplex -> ExtComplex -> Int -> Int
        f _ _ 0 = 0
        f c z n = if (magnitude z > 2 ) 
                  then n
                  else f c ((z*z)+c) (n-1)
