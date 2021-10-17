module Transformation where

import Data.Geometry
import Data.Geometry.Matrix
import Data.Geometry.Transformation

-- | Create an identity transformation.
identity :: Num r => Transformation 2 r
identity = Transformation . Matrix $ Vector3 (Vector3 1 0 0)
                                             (Vector3 0 1 0)
                                             (Vector3 0 0 1)

-- | Create a matrix that corresponds to a rotation by 'a' radians counter-clockwise 
--   around the origin.
rotation :: Floating r => r -> Transformation 2 r
rotation a = Transformation . Matrix $ Vector3 (Vector3 (cos a) (- sin a) 0)
                                               (Vector3 (sin a) (  cos a) 0)
                                               (Vector3 0       0         1)

-- | Create a matrix that corresponds to a reflection in a line through the origin
--   which makes an angle of 'a' radians with the positive 'x'-asis, in counter-clockwise
--   orientation.
reflection :: Floating r => r -> Transformation 2 r
reflection a = rotation a |.| reflectionV |.| rotation (-a)

reflectionV :: Num r => Transformation 2 r
reflectionV = Transformation . Matrix $ Vector3 (Vector3 1   0  0)
                                                (Vector3 0 (-1) 0)
                                                (Vector3 0   0  1)

reflectionH :: Num r => Transformation 2 r
reflectionH = Transformation . Matrix $ Vector3 (Vector3 (-1) 0  0)
                                                (Vector3   0  1  0)
                                                (Vector3   0  0  1)

