module PingPong.Simulation.Collision (CollisionChecker, defaultCollisionChecker, verify) where

import Data.Geometry hiding (init, head, zero, replicate)
import Data.List hiding (intersect)
import Data.Foldable
import Data.Ext

import Control.Lens

import Data.Geometry.Transformation
import Transformation

-- * Implementation of collision checking.

-- | A collision checker takes as input two time stamps, with for each time stamp
--   the location of a point and a segment.
--   Assuming linear motion between the two time stamps, the checker should test
--   if a collision between the point and the segment takes place.
--   If so, it should report the time of the collision, as well as location and
--   velocity of the point as a result of the collision.
type CollisionChecker = (Float, Point 2 Float, LineSegment 2 () Float) 
                     -> (Float, Point 2 Float, LineSegment 2 () Float) 
                     -> IO (Maybe (Float, Point 2 Float, Vector 2 Float))

-- The collision checker that will be used when running the simulator through the play function.
defaultCollisionChecker :: CollisionChecker
defaultCollisionChecker = modelChecker

-- Make sure a collision checker returns something with a sensible time stamp.
verify :: CollisionChecker -> CollisionChecker
verify checker st1 st2 = do
  result <- checker st1 st2
  return $ verifyResult st1 st2 result

verifyResult :: (Float, Point 2 Float, LineSegment 2 () Float) 
             -> (Float, Point 2 Float, LineSegment 2 () Float) 
             -> Maybe (Float, Point 2 Float, Vector 2 Float)
             -> Maybe (Float, Point 2 Float, Vector 2 Float)
verifyResult _ _ Nothing = Nothing
verifyResult (t1, p1, s1) (t2, p2, s2) (Just (t, p, v)) | t <= t1 = Nothing
                                                        | t >= t2 = Nothing
                                                        | otherwise = Just (t, p, v)

-- A simple collision checker which ignores the segment, and only checks for collision with the floor.
floorChecker :: CollisionChecker
floorChecker (t1, Point2 x1 y1, _) (t2, Point2 x2 y2, _)
  | y2 >= 0   = return Nothing
  | y1 == y2  = error "Ball was already under the floor!?"
  | otherwise = let tc = t1 + (t2 - t1) * y1 / (y1 - y2)
                    xc = x1 + (x2 - x1) * y1 / (y1 - y2)
                    yc = 0
                    dx = (x2 - x1) / (t2 - t1)
                    dy = (y1 - y2) / (t2 - t1)
                in return $ Just (tc, Point2 xc yc, Vector2 dx dy)
  
-- The model solution.
modelChecker :: CollisionChecker
modelChecker (t1, p1, s1) (t2, p2, s2) =
  let collisions = collisionPoints (t1, p1, s1) (t2, p2, s2)
      outputs    = sort $ map (collisionOutput (t1, p1, s1) (t2, p2, s2)) collisions
      future     = filter (\(t, _, _) -> t >= t1) outputs
  in case future of [] -> return Nothing
                    (o : _) -> return $ Just o

-- | For a given collision point and time, compute the velocity of the ball
--   at that point and time.
collisionOutput :: (Float, Point 2 Float, LineSegment 2 () Float) 
                -> (Float, Point 2 Float, LineSegment 2 () Float) 
                -> (Point 2 Float, Float, Float)
                -> (Float, Point 2 Float, Vector 2 Float)
collisionOutput (t0, b0, s0) (t1, b1, s1) (p, s, t) =
  (t, p, collisionVelocity (t0, b0, s0) (t1, b1, s1) (p, s, t))

-- | For a given collision point and time, compute the velocity of the ball
--   at that point and time.
collisionVelocity :: (Float, Point 2 Float, LineSegment 2 () Float) 
                  -> (Float, Point 2 Float, LineSegment 2 () Float) 
                  -> (Point 2 Float, Float, Float)
                  -> Vector 2 Float
collisionVelocity (t0, b0, s0) (t1, b1, s1) (p, s, t') = 
  let t = (t' - t0) / (t1 - t0)
      c0 = s0 ^. start ^. core
      d0 = s0 ^. end   ^. core
      c1 = s1 ^. start ^. core
      d1 = s1 ^. end   ^. core
      vl =   ((1-t) *^ (d0 .-. origin) ^+^ t *^ (d1 .-. origin)) 
         ^-^ ((1-t) *^ (c0 .-. origin) ^+^ t *^ (c1 .-. origin))
      vs =   (((1-s) *^ (c1 .-. origin) ^+^ s *^ (d1 .-. origin)) 
         ^-^ ((1-s) *^ (c0 .-. origin) ^+^ s *^ (d0 .-. origin)))
         ^/  (t1 - t0)
      vb = (b1 .-. b0) ^/ (t1 - t0)
      vr = vb ^-^ vs
      vm = reflectVector vr vl
  in vm ^+^ vs

-- | Collect all points where collisions happen, and also report the fraction
--   of the segment and the time of each collision.
--   May return 0, 1, or 2 points.
collisionPoints :: (Float, Point 2 Float, LineSegment 2 () Float) 
                -> (Float, Point 2 Float, LineSegment 2 () Float) 
                -> [(Point 2 Float, Float, Float)]
collisionPoints (t0, b0, s0) (t1, b1, s1) = 
  let c0 = s0 ^. start ^. core
      d0 = s0 ^. end   ^. core
      c1 = s1 ^. start ^. core
      d1 = s1 ^. end   ^. core
      Point2 xb0 yb0 = b0
      Point2 xb1 yb1 = b1
      Point2 xc0 yc0 = c0
      Point2 xc1 yc1 = c1
      Point2 xd0 yd0 = d0
      Point2 xd1 yd1 = d1
      xa = xd0 - xc0
      ya = yd0 - yc0
      xb = xb0 - xc0 + xc1 - xb1
      yb = yb0 - yc0 + yc1 - yb1 
      xc = xc0 - xd0 + xd1 - xc1
      yc = yc0 - yd0 + yd1 - yc1
      xd = xb0 - xc0
      yd = yb0 - yc0
      i = xd * ya - yd * xa
      j = xd * yc - xb * ya - yd * xc + yb * xa
      k = yb * xc - xb * yc
      ts = solveQuadraticEquation k j i
      s t | zero $ xa + xc * t = (yd - yb * t) / (ya + yc * t)
          | zero $ ya + yc * t = (xd - xb * t) / (xa + xc * t)
          | otherwise = let s1 = (xd - xb * t) / (xa + xc * t)
                            s2 = (yd - yb * t) / (ya + yc * t)
                        in if 0 <= s1 && s1 <= 1 then s1 else s2 -- checkAlmostEqual s1 s2 
      ss = map s ts
      p (s, t) = origin .+^ (1-t) * (1-s) *^ (c0 .-. origin) 
                        .+^ (1-t) * s     *^ (d0 .-. origin) 
                        .+^ t     * (1-s) *^ (c1 .-. origin)
                        .+^ t     * s     *^ (d1 .-. origin)
      ps = map p $ zip ss ts
      ts' = map (\t -> (1 - t) * t0 + t * t1) ts
      psts = zip3 ps ss ts'
  in filter (\(p, s, t) -> 0 < s && s <= 1 && t0 < t && t <= t1) psts


checkAlmostEqual :: (Ord r, Floating r, Show r) => r -> r -> r
checkAlmostEqual a b | a == 0 && abs b > treshold = error message
                     | b == 0 && abs a > treshold = error message
                     | a == 0 || b == 0           = 0
                     | a / b > 1 + treshold       = error message
                     | b / a > 1 + treshold       = error message
                     | otherwise                  = a -- trace ("checking " ++ show a ++ " " ++ show b) a
  where treshold = 100
        message  = error $ "checkAlmostEqual: " ++ show a ++ " /= " ++ show b


-- | Solve equation of the form ax^2 + bx + c = 0.
--   Attempt at a somewhat robust implementation.
solveQuadraticEquation :: (Ord r, Enum r, Floating r, Show r) => r -> r -> r -> [r]
solveQuadraticEquation 0 0 0 = [0] -- [0..]
solveQuadraticEquation a 0 0 = [0]
solveQuadraticEquation 0 b 0 = [0]
solveQuadraticEquation 0 0 c = []
solveQuadraticEquation a b 0 = sort [0, -b / a]
solveQuadraticEquation a 0 c | (-c / a) <  0 = []
                             | (-c / a) == 0 = [0]
                             | (-c / a) >  0 = [sqrt (-c / a)]
solveQuadraticEquation 0 b c = [-c / b]
solveQuadraticEquation a b c | zero a || zero (a / b) || zero (a / c) = solveQuadraticEquation 0 b c
solveQuadraticEquation a b c = 
  let d = b^2 - 4 * a * c
      result | d == 0 = [-b / (2 * a)]
             | d >  0 = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
             | otherwise = []
  in result
  -- trace ("soving equation " ++ show a ++ "x^2 + " ++ show b ++ "x + " ++ show c ++ " = 0") $ result  


-- | Test whether a floating point number is zero, taking rounding errors into account.
zero :: (Floating r, Ord r) => r -> Bool
zero x = abs x < epsilon

-- | Treshold for rounding errors in zero tests
--   TODO: Should be different depending on the type.
epsilon :: Floating r => r
epsilon = 0.0001




-- | Reflect vector 'a' in a line with direction vector 'b'.
reflectVector :: Vector 2 Float -> Vector 2 Float -> Vector 2 Float
reflectVector a b = reflection (angle (Vector2 1 0) b) `transformBy` a

-- | Find the angle between two vectors, in counter-clockwise order, from the first to the second.
angle :: Vector 2 Float -> Vector 2 Float -> Float
angle (Vector2 x1 y1) (Vector2 x2 y2) = atan2 (x1 * y2 - y1 * x2) (x1 * x2 + y1 * y2)
